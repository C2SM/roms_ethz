#include "cppdefs.h"
#ifdef BIOLOGY_BEC2

      subroutine get_dip_river(ierr)
      implicit none
# include "param.h"
# include "forces.h"
# include "scalars.h"
# include "netcdf.inc"
# include "ncvars.h"
      real cff
      integer i,ierr, lstr,lenstr, nf_fread, ifrc, ncid
      character*(max_name_size) frcname
      save ncid

!
! Initialization: Check, whether forcing netCDF file is already
!===============  opened, an if not, open it. Find and save netCDF
! IDs for relevant variables, and determine whether sst data exists
! as 2D field or scalar. After that determine whether there is
! cycling to reuse the input data and what is cycling period
! "dip_river_cycle", set initial cycling index "dip_river_ncycle", record index
! "dip_river_rec", and time index "itdip_river" and both time record bounds to
! large negative artificial values, so that it will trigger logic in
! readipg part below.
!
      if (iic.eq.ntstart) then
        ifrc=1
 100    ncid=ncidfrc(ifrc)
        frcname=frcfile(ifrc)
        ierr=nf_noerr

        lstr=lenstr(frcname)
        if (ncid.eq.-1) then
          ierr=nf_open(frcname(1:lstr), nf_nowrite, ncid)
          if (ierr.eq.nf_noerr) then
            ncidfrc(ifrc)=ncid
            MPI_master_only print*
     &            , 'get_dip_river: Opened forcing data file "'
     &            , frcname(1:lstr),'"'
          else
            MPI_master_only print*, '### ERROR: '
     &            , 'get_dip_river: Cannot open forcing data file "'
     &            , frcname(1:lstr),'"', ', netCDF error no = ',ierr
          endif
        endif
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varid (ncid, 'dip_river_time', dip_river_tid)
          if (ierr .ne. nf_noerr) then
            MPI_master_only
     &      print*, 'get_din:river: Error getting ID dip_river_time in '
     &             , frcname(1:lstr),', nc error no = ',ierr
          else
            MPI_master_only
     &      print*,  'get_dip_river: got ID of dip_river_time in '
     &             , frcname(1:lstr)
          endif
          if (ierr .eq. nf_noerr) then
            ierr=nf_inq_varid (ncid, 'dip_river_flux', dip_river_id)
            if (ierr .eq. nf_noerr) then
              call set_cycle (ncid, dip_river_tid,ntdip_river,dip_river_cycle,
     &                                 dip_river_ncycle,dip_river_rec,ierr)
              itdip_river=1
              dip_river_time(1)=-1.E+20
              dip_river_time(2)=-1.E+20
            else
              MPI_master_only print*,'At 1st WARNING in get_dip_river'
              MPI_master_only
     &        write(stdout,1) 'dip_river_flux', frcname(1:lstr)
              print*,' 1st WARNING: ierr = ',ierr
            endif
          else
            MPI_master_only
     &        print*,'At 2nd WARNING in get_dip_river'
            MPI_master_only
     &      write(stdout,1) 'dip_river_time', frcname(1:lstr)
            MPI_master_only print*,' 2nd WARNING: ierr = ',ierr
            if (ifrc .lt. max_frc_files) then
              ifrc=ifrc+1
              goto 100
            endif
          endif
        endif
      endif
  1   format(/1x,'WARNING: get_dip_river cannot find variable ''',
     &     A, ''''/7x, 'in forcing netCDF file ''', A, '''.')

!
! Readipg data from the forcing file:  Get out, if model time is
!======== ==== ==== === ======= =====  already within the interval
! set by the past and future data times. Otherwise flip the time
! index, increment record and cyclin indices and read a new portion
! of data. Repeat it until model time is between the two times from
! data.
!
      do while (dip_river_time(itdip_river).lt.time+dt .and. ierr.eq.nf_noerr)
        call advance_cycle (dip_river_cycle, ntdip_river, dip_river_ncycle,
     &                                 dip_river_rec,     ierr)
        if (ierr .eq. nf_noerr) then
          ierr=nf_get_var1_FTYPE (ncid, dip_river_tid, dip_river_rec, cff)
          if (ierr .eq. nf_noerr) then
            itdip_river=3-itdip_river
            dip_river_time(itdip_river)=cff*day2sec + dip_river_cycle*dip_river_ncycle
            ierr=nf_fread (dip_riverg(START_2D_ARRAY,itdip_river), ncid,
     &                                 dip_river_id, dip_river_rec, r2dvar)
            if (ierr .eq. nf_noerr) then
              MPI_master_only write(stdout,
     &          '(6x,2A,G12.4,1x,A,I4)') 'get_dip_river    --',
     &          ' read dip_river                       for time =',
     &                   cff MYID
            else
              write(stdout,2) 'dip_river', dip_river_rec
            endif
          else
            write(stdout,2) 'dip_river_time', dip_river_rec
          endif
        else
          write(stdout,'(/1x,A,I4,1x,A,I4/7x,4A/7x,2(A,G12.4)/)')
     &   'ERROR in get_dip_river: requested time record ',      dip_river_rec,
     &   'exeeds the last record', ntdip_river, 'available in forcing ',
     &   'netCDF file ''',   frcname(1:lstr),   '''',  'tdays = ',
     &    tdays,        '  but the last available  dip_river_time =',
     &                                    dip_river_time(itdip_river)*sec2day
        endif
      enddo
  2   format(/1x,'ERROR in get_dip_river: cannot read variable ''', A,
     &                                    ''' for record ', I4/)
      return
      end
 
      subroutine set_dip_river_tile (Istr,Iend,Jstr,Jend,ierr)
!
! Set-up sea surface dip_river data for current tile.
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j, it1,it2,ierr
      real cff, cff1,cff2, cff3,cff4, val1

# include "param.h"
# include "forces.h"
# include "scalars.h"
!
# include "compute_extended_bounds.h"
 

! Set coefficients for interpolation. Check that for the next time
! step [when time=time+dt] both weights will still be positive, and
! if not, set synchro_flag to signal that new data should be read
! from an appropriate netCDF input file (master thread only).
! After that either load time-invariant data, or interpolate in time
! or complain about error and signal to quit, if interpolation is
! needed, but not possible.
!
 
      it1=3-itdip_river
      it2=itdip_river
      cff1=dip_river_time(it2)-time
      cff2=time-dip_river_time(it1)


!      print*,'dip_river(istrR:iendR,jstrR:jendR)',dip_river(istrR:iendR,jstrR:jendR)
      if (ZEROTH_TILE  .and. ( cff1.lt.dt )) synchro_flag=.true.
 
      if (cff1.ge.0. .and. cff2.ge.0. ) then

        cff=1./(cff1+cff2)
        cff1=cff1*cff                ! Interpolate Din_River
        cff2=cff2*cff                ! data to model time.

        do j=jstrR,jendR
          do i=istrR,iendR
              dip_river(i,j)=(cff1*dip_riverg(i,j,it1)+cff2*dip_riverg(i,j,it2))
          enddo
        enddo

      elseif (ZEROTH_TILE) then
        write(stdout,'(/1x,2A/3(1x,A,F16.10)/)')
     &       'SET_DIN_RIVER_TILE - current model time is outside bounds ',
     &       'of ''dip_river_time''.', 'DIN_RIVER_TSTART=',dip_river_time(it1)*sec2day,
     &                           'TDAYS=',     tdays,
     &                           'DIN_RIVER_TEND=',  dip_river_time(it2)*sec2day
        ierr=ierr+1
      endif
      return
      end
#else /* BIOLOGY_BEC2 */
      subroutine get_dip_river_empty
      end
#endif /* BIOLOGY_BEC2 */
