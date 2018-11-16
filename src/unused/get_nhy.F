#include "cppdefs.h"
#ifdef BIOLOGY_BEC2

      subroutine get_nhy(ierr)
      implicit none
# include "param.h"
# include "forces.h"
# include "scalars.h"
# include "netcdf.inc"
# include "ncvars.h"
      real cff
      integer i,ierr, lstr,lenstr, nf_fread, ifrc, ncid
      character*(max_name_size) frcname

!
! Initialization: Check, whether forcing netCDF file is already
!===============  opened, an if not, open it. Find and save netCDF
! IDs for relevant variables, and determine whether sst data exists
! as 2D field or scalar. After that determine whether there is
! cycling to reuse the input data and what is cycling period
! "nhy_cycle", set initial cycling index "nhy_ncycle", record index
! "nhy_rec", and time index "itnhy" and both time record bounds to
! large negative artificial values, so that it will trigger logic in
! reading part below.
!
      ifrc=1
 100  ncid=ncfrc(ifrc)
      frcname=frcfile(ifrc)
      ierr=nf_noerr

      if (iic.eq.ntstart) then
        lstr=lenstr(frcname)
        if (ncid.eq.-1) then
          ierr=nf_open(frcname(1:lstr), nf_nowrite, ncfrc)
          if (ierr.eq.nf_noerr) then
            ncfrc(ifrc)=ncid
          else
            write(*,'(/1x,4A/)') '### ERROR ',
     &                   'in get_nhy: can not open netCDF file ''',
     &                                       frcname(1:lstr), '''.'
          endif
        endif
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varid (ncid, 'nhy_time', nhy_tid)
          if (ierr .eq. nf_noerr) then
            ierr=nf_inq_varid (ncid,'nhy_flux',nhy_id)
            if (ierr .eq. nf_noerr) then
              call set_cycle (ncid, nhy_tid, ntnhy, nhy_cycle,
     &                                 nhy_ncycle, nhy_rec, ierr)
              itnhy=1
              nhy_time(1)=-1.E+20
              nhy_time(2)=-1.E+20
            else
              write(*,1) 'nhy_flux', frcname(1:lstr)
            endif
          else
            write(*,1) 'nhy_time', frcname(1:lstr)
            if (ifrc .lt. max_frc) then
              ifrc=ifrc+1
              goto 100
            endif
          endif
        endif
      endif
  1   format(/1x,'WARNING: get_nhy cannot find variable ''',
     &     A, ''''/7x, 'in forcing netCDF file ''', A, '''.')
!
! Reading data from the forcing file:  Get out, if model time is
!======== ==== ==== === ======= =====  already within the interval
! set by the past and future data times. Otherwise flip the time
! index, increment record and cyclin indices and read a new portion
! of data. Repeat it until model time is between the two times from
! data.
!
      do while (nhy_time(itnhy).lt.time+dt .and. ierr.eq.nf_noerr)
        call advance_cycle (nhy_cycle, ntnhy, nhy_ncycle,
     &                                 nhy_rec,     ierr)
        if (ierr .eq. nf_noerr) then
          ierr=nf_get_var1_FTYPE (ncid, nhy_tid, nhy_rec, cff)
          if (ierr .eq. nf_noerr) then
            itnhy=3-itnhy
            nhy_time(itnhy)=cff*day2sec + nhy_cycle*nhy_ncycle
            ierr=nf_fread (nhyg(START_2D_ARRAY,itnhy), ncid,
     &                                 nhy_id, nhy_rec, r2dvar)
            if (ierr .eq. nf_noerr) then
              MPI_master_only write(*,
     &             '(6x,2A,G12.4,1x,A,I4)') 'get_nhy    --',
     &                 ' read nhy                       for time =',
     &                   cff MYID
            else
              write(*,2) 'nhy', nhy_rec
            endif
          else
            write(*,2) 'nhy_time', nhy_rec
          endif
        else
          write(*,'(/1x,A,I4,1x,A,I4/7x,4A/7x,2(A,G12.4)/)')
     &   'ERROR in get_nhy: requested time record ',      nhy_rec,
     &   'exeeds the last record', ntnhy, 'available in forcing ',
     &   'netCDF file ''',   frcname(1:lstr),   '''',  'tdays = ',
     &    tdays,        '  but the last available  nhy_time =',
     &                                    nhy_time(itnhy)*sec2day
        endif
      enddo
  2   format(/1x,'ERROR in get_nhy: cannot read variable ''', A,
     &                                    ''' for record ', I4/)
      return
      end

      subroutine set_nhy_tile (Istr,Iend,Jstr,Jend,ierr)
!
! Set-up sea surface nhy data for current tile.
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

      it1=3-itnhy
      it2=itnhy
      cff1=nhy_time(it2)-time
      cff2=time-nhy_time(it1)


      if (ZEROTH_TILE  .and. ( cff1.lt.dt )) synchro_flag=.true.

      if (cff1.ge.0. .and. cff2.ge.0. ) then

        cff=1./(cff1+cff2)
        cff1=cff1*cff                ! Interpolate NHy
        cff2=cff2*cff                ! data to model time.

        do j=jstrR,jendR
          do i=istrR,iendR
              nhy(i,j)=(cff1*nhyg(i,j,it1)+cff2*nhyg(i,j,it2))
          enddo
        enddo
      elseif (ZEROTH_TILE) then
        write(*,'(/1x,2A/3(1x,A,F16.10)/)')
     &       'SET_NHY_TILE - current model time is outside bounds ',
     &       'of ''nhy_time''.', 'NHY_TSTART=',nhy_time(it1)*sec2day,
     &                           'TDAYS=',     tdays,
     &                           'NHY_TEND=',  nhy_time(it2)*sec2day
        ierr=ierr+1
      endif
      return
      end
#else /* BIOLOGY_BEC2 */
      subroutine get_nhy_empty
      end
#endif /* BIOLOGY_BEC2 */
