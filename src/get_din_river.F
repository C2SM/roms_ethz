#include "cppdefs.h"
#ifdef BIOLOGY_BEC2

      subroutine get_din_river(ierr)
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
! "din_river_cycle", set initial cycling index "din_river_ncycle", record index
! "din_river_rec", and time index "itdin_river" and both time record bounds to
! large negative artificial values, so that it will trigger logic in
! reading part below.
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
     &            , 'get_din_river: Opened forcing data file "'
     &            , frcname(1:lstr),'"'
          else
            MPI_master_only print*, '### ERROR: '
     &            , 'get_din_river: Cannot open forcing data file "'
     &            , frcname(1:lstr),'"', ', netCDF error no = ',ierr
          endif
        endif
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varid (ncid, 'din_river_time', din_river_tid)
          if (ierr .ne. nf_noerr) then
            MPI_master_only print*
     &             , 'get_din:river: Error getting ID din_river_time in '
     &             , frcname(1:lstr),', nc error no = ',ierr
          else
            MPI_master_only print*
     &             ,  'get_din_river: got ID of din_river_time in '
     &             , frcname(1:lstr)
          endif
          if (ierr .eq. nf_noerr) then
            ierr=nf_inq_varid (ncid, 'din_river_flux', din_river_id)
            if (ierr .eq. nf_noerr) then
              call set_cycle (ncid, din_river_tid, ntdin_river,din_river_cycle,
     &                                 din_river_ncycle, din_river_rec,ierr)
              itdin_river=1
              din_river_time(1)=-1.E+20
              din_river_time(2)=-1.E+20
            else
              MPI_master_only print*,'At 1st WARNING in get_din_river'
              MPI_master_only write(stdout,1)
     &           'din_river_flux', frcname(1:lstr)
              print*,' 1st WARNING: ierr = ',ierr
            endif
          else
            MPI_master_only print*
     &         ,'At 2nd WARNING in get_din_river'
            MPI_master_only write(stdout,1)
     &       'din_river_time', frcname(1:lstr)
            MPI_master_only print*,' 2nd WARNING: ierr = ',ierr
            if (ifrc .lt. max_frc_file) then
              ifrc=ifrc+1
              goto 100
            endif
          endif
        endif
      endif
  1   format(/1x,'WARNING: get_din_river cannot find variable ''',
     &     A, ''''/7x, 'in forcing netCDF file ''', A, '''.')

!
! Reading data from the forcing file:  Get out, if model time is
!======== ==== ==== === ======= =====  already within the interval
! set by the past and future data times. Otherwise flip the time
! index, increment record and cyclin indices and read a new portion
! of data. Repeat it until model time is between the two times from
! data.
!
      do while (din_river_time(itdin_river).lt.time+dt .and. ierr.eq.nf_noerr)
        call advance_cycle (din_river_cycle, ntdin_river, din_river_ncycle,
     &                                 din_river_rec,     ierr)
        if (ierr .eq. nf_noerr) then
          ierr=nf_get_var1_FTYPE (ncid, din_river_tid, din_river_rec, cff)
          if (ierr .eq. nf_noerr) then
            itdin_river=3-itdin_river
            din_river_time(itdin_river)=cff*day2sec + din_river_cycle*din_river_ncycle

            ierr=nf_fread (din_riverg(START_2D_ARRAY,itdin_river), ncid,
     &                                 din_river_id, din_river_rec, r2dvar)
            if (ierr .eq. nf_noerr) then
              MPI_master_only write(stdout,
     &          '(6x,2A,G12.4,1x,A,I4)') 'get_din_river    --',
     &          ' read din_river                       for time =',
     &                   cff MYID
            else
              write(stdout,2) 'din_river', din_river_rec
            endif
          else
            write(stdout,2) 'din_river_time', din_river_rec
          endif
        else
          write(stdout,'(/1x,A,I4,1x,A,I4/7x,4A/7x,2(A,G12.4)/)')
     &   'ERROR in get_din_river: requested time record ',      din_river_rec,
     &   'exeeds the last record', ntdin_river, 'available in forcing ',
     &   'netCDF file ''',   frcname(1:lstr),   '''',  'tdays = ',
     &    tdays,        '  but the last available  din_river_time =',
     &                                    din_river_time(itdin_river)*sec2day
        endif
      enddo
  2   format(/1x,'ERROR in get_din_river: cannot read variable ''', A,
     &                                    ''' for record ', I4/)
      return
      end
 
      subroutine set_din_river_tile (Istr,Iend,Jstr,Jend,ierr)
!
! Set-up sea surface din_river data for current tile.
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
 
      it1=3-itdin_river
      it2=itdin_river
      cff1=din_river_time(it2)-time
      cff2=time-din_river_time(it1)


!      print*,'din_river(istrR:iendR,jstrR:jendR)',din_river(istrR:iendR,jstrR:jendR)
      if (ZEROTH_TILE  .and. ( cff1.lt.dt )) synchro_flag=.true.
 
      if (cff1.ge.0. .and. cff2.ge.0. ) then

        cff=1./(cff1+cff2)
        cff1=cff1*cff                ! Interpolate Din_River
        cff2=cff2*cff                ! data to model time.

        do j=jstrR,jendR
          do i=istrR,iendR
              din_river(i,j)=(cff1*din_riverg(i,j,it1)+cff2*din_riverg(i,j,it2))
          enddo
        enddo

      elseif (ZEROTH_TILE) then
        write(stdout,'(/1x,2A/3(1x,A,F16.10)/)')
     &       'SET_DIN_RIVER_TILE - current model time is outside bounds ',
     &       'of ''din_river_time''.', 'DIN_RIVER_TSTART=',din_river_time(it1)*sec2day,
     &                           'TDAYS=',     tdays,
     &                           'DIN_RIVER_TEND=',  din_river_time(it2)*sec2day
        ierr=ierr+1
      endif
      return
      end
#else /* BIOLOGY_BEC2 */
      subroutine get_din_river_empty
      end
#endif /* BIOLOGY_BEC2 */
