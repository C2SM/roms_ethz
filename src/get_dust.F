#include "cppdefs.h"
#if defined BIOLOGY_BEC || defined BIOLOGY_BEC2

      subroutine get_dust(ierr)
      implicit none
# include "param.h"
# include "forces.h"
# include "scalars.h"
# include "netcdf.inc"
# include "ncvars.h"
      real cff
      integer i,ierr, lstr,lvar,lenstr, nf_fread, ifrc, ncid
      character*(max_name_size) frcname

!
! Initialization: Check, whether forcing netCDF file is already
!===============  opened, an if not, open it. Find and save netCDF
! IDs for relevant variables, and determine whether sst data exists
! as 2D field or scalar. After that determine whether there is
! cycling to reuse the input data and what is cycling period
! "dust_cycle", set initial cycling index "dust_ncycle", record index
! "dust_rec", and time index "itdust" and both time record bounds to
! large negative artificial values, so that it will trigger logic in
! reading part below.
!
      ifrc=1
 100  ncid=ncidfrc(ifrc)
      frcname=frcfile(ifrc)
      ierr=nf_noerr

      if (iic.eq.ntstart) then
        lstr=lenstr(frcname)
        if (ncid.eq.-1) then
          ierr=nf_open(frcname(1:lstr), nf_nowrite, ncid)
          if (ierr.eq.nf_noerr) then
            ncidfrc(ifrc)=ncid
          else
            write(stdout,'(/1x,4A/)') '### ERROR ',
     &                   'in get_dust: can not open netCDF file ''',
     &                                       frcname(1:lstr), '''.'
          endif
        endif
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varid (ncid, 'dust_time', dust_tid)
          if (ierr .eq. nf_noerr) then
            lvar=lenstr(vname(1,indxdust))
            ierr=nf_inq_varid (ncid, vname(1,indxdust)(1:lvar),
     &                                                   dust_id)
            if (ierr .eq. nf_noerr) then
              call set_cycle (ncid, dust_tid, ntdust, dust_cycle,
     &                                 dust_ncycle, dust_rec, ierr)
              itdust=1
              dust_time(1)=-1.E+20
              dust_time(2)=-1.E+20
            else
              write(stdout,1) vname(1,indxSST)(1:lvar),
     &                                  frcname(1:lstr)
            endif
          else
            write(stdout,1) 'dust_time', frcname(1:lstr)
            if (ifrc .lt. max_frc_file) then
              ifrc=ifrc+1
              goto 100
            endif
          endif
        endif
      endif
  1   format(/1x,'WARNING: get_dust cannot find variable ''',
     &     A, ''''/7x, 'in forcing netCDF file ''', A, '''.')
!
! Reading data from the forcing file:  Get out, if model time is
!======== ==== ==== === ======= =====  already within the interval
! set by the past and future data times. Otherwise flip the time
! index, increment record and cyclin indices and read a new portion
! of data. Repeat it until model time is between the two times from
! data.
!
      do while (dust_time(itdust).lt.time+dt .and. ierr.eq.nf_noerr)
        call advance_cycle (dust_cycle, ntdust, dust_ncycle,
     &                                 dust_rec,     ierr)
        if (ierr .eq. nf_noerr) then
          ierr=nf_get_var1_FTYPE (ncid, dust_tid, dust_rec, cff)
          if (ierr .eq. nf_noerr) then
            itdust=3-itdust
            dust_time(itdust)=cff*day2sec + dust_cycle*dust_ncycle
            ierr=nf_fread (dustg(START_2D_ARRAY,itdust), ncid,
     &                                 dust_id, dust_rec, r2dvar)
            if (ierr .eq. nf_noerr) then
              MPI_master_only write(stdout,
     &             '(6x,2A,G12.4,1x,A,I4)') 'get_dust    --',
     &                 ' read dust                       for time =',
     &                   cff MYID
            else
              write(stdout,2) 'dust', dust_rec
            endif
          else
            write(stdout,2) 'dust_time', dust_rec
          endif
        else
          write(stdout,'(/1x,A,I4,1x,A,I4/7x,4A/7x,2(A,G12.4)/)')
     &   'ERROR in get_dust: requested time record ',      dust_rec,
     &   'exeeds the last record', ntdust, 'available in forcing ',
     &   'netCDF file ''',   frcname(1:lstr),   '''',  'tdays = ',
     &    tdays,        '  but the last available  dust_time =',
     &                                    dust_time(itdust)*sec2day
        endif
      enddo
  2   format(/1x,'ERROR in get_dust: cannot read variable ''', A,
     &                                    ''' for record ', I4/)
      return
      end
 
      subroutine set_dust_tile (Istr,Iend,Jstr,Jend,ierr)
!
! Set-up sea surface dust data for current tile.
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
 
      it1=3-itdust
      it2=itdust
      cff1=dust_time(it2)-time
      cff2=time-dust_time(it1)


      if (ZEROTH_TILE  .and. ( cff1.lt.dt )) synchro_flag=.true.
 
      if (cff1.ge.0. .and. cff2.ge.0. ) then

        cff=1./(cff1+cff2)
        cff1=cff1*cff                ! Interpolate SST and dQdSST
        cff2=cff2*cff                ! data to model time.  Convert
                                     ! units of dQdSST from

        do j=jstrR,jendR             !
          do i=istrR,iendR
              dust(i,j)=cff1*dustg(i,j,it1)+cff2*dustg(i,j,it2)
          enddo
        enddo
      elseif (ZEROTH_TILE) then
        write(stdout,'(/1x,2A/3(1x,A,F16.10)/)')
     &       'SET_DUST_TILE - current model time is outside bounds ',
     &       'of ''dust_time''.', 'DUST_TSTART=',dust_time(it1)*sec2day,
     &                           'TDAYS=',     tdays,
     &                           'DUST_TEND=',  dust_time(it2)*sec2day
        ierr=ierr+1
      endif
      return
      end
#else /* BIOLOGY_BEC || BIOLOGY_BEC2 */
      subroutine get_dust_empty
      end
#endif /* BIOLOGY_BEC  || BIOLOGY_BEC2 */
