#include "cppdefs.h"
#if defined SOLVE3D && defined QCORRECTION && !defined ANA_SST
 
      subroutine get_sss (ierr)     ! Read sea surface salinity
      implicit none
# define SST_DATA
# include "param.h"
# include "forces.h"
# include "scalars.h"
# include "netcdf.inc"
# include "ncvars.h"
      real cff
      integer ncid,ifrc, ierr, lfrc,lvar,lenstr, nf_fread
      character*(max_name_size) frcname
!
! Initialization: Check, whether forcing netCDF file is already
!===============  opened, an if not, open it. Find and save netCDF
! IDs for relevant variables, and determine whether sss data exists
! as 2D field or scalar. After that determine whether there is
! cycling to reuse the input data and what is cycling period
! "sss_cycle", set initial cycling index "sss_ncycle", record index
! "sss_rec", and time index "itsss" and both time record bounds to
! large negative artificial values, so that it will trigger logic in
! reading part below.
!
      ifrc=max(sss_file_id,1)
 100  ncid=ncidfrc(ifrc)
      frcname=frcfile(ifrc)
      lfrc=lenstr(frcname)
      ierr=nf_noerr

      if (iic.eq.ntstart) then
        if (ncid.eq.-1) then
          ierr=nf_open(frcname(1:lfrc), nf_nowrite, ncid)
          if (ierr.eq.nf_noerr) then
            ncidfrc(ifrc)=ncid
          else
            write(stdout,'(/1x,4A/)') '### ERROR in get_sss: can ',
     &            'not open netCDF file ''', frcname(1:lfrc), '''.'
          endif
        endif
        if (ierr .eq. nf_noerr) then
          lvar=lenstr(vname(1,indxSSS))
          ierr=nf_inq_varid (ncid, vname(1,indxSSS)(1:lvar), sss_id)
          if (ierr .eq. nf_noerr) then
            sss_file_id=ifrc
# ifdef DEBUG
            MPI_master_only write(*,*) 'Found ''', 
     &        vname(1,indxSSS)(1:lvar),
     &        ''' in ''', frcname(1:lfrc), ''', ifrc =', ifrc
# endif

            ierr=nf_inq_varid (ncid, 'sss_time', sss_tid)
            if (ierr .eq. nf_noerr) then
              call set_cycle (ncid, sss_tid, ntsss, sss_cycle,
     &                                 sss_ncycle, sss_rec, ierr)
              itsss=1
              sss_time(1)=-1.E+20
              sss_time(2)=-1.E+20
            else
              write(stdout,1) 'sss_time', frcname(1:lfrc)
            endif
          else
            write(stdout,1) vname(1,indxSSS)(1:lvar), frcname(1:lfrc)
            if (ifrc .lt. max_frc) then
              ifrc=ifrc+1
              goto 100
            endif
          endif
        endif
      endif
  1   format(/1x,'WARNING: get_sss cannot find variable ''', A,
     &          ''''/7x, 'in forcing netCDF file ''', A, '''.')
!
! Reading data from the forcing file:  Get out, if model time is
!======== ==== ==== === ======= =====  already within the interval
! set by the past and future data times. Otherwise flip the time
! index, increment record and cycling indices and read a new portion
! of data. Repeat it until model time is between the two times from
! data.
!
      do while (sss_time(itsss).lt.time+0.5*dt .and.ierr.eq.nf_noerr)
        call advance_cycle (sss_cycle, ntsss, sss_ncycle,
     &                                 sss_rec,     ierr)
        if (ierr .eq. nf_noerr) then
          ierr=nf_get_var1_FTYPE (ncid, sss_tid, sss_rec, cff)
          if (ierr .eq. nf_noerr) then
            itsss=3-itsss
            sss_time(itsss)=cff*day2sec + sss_cycle*sss_ncycle
            ierr=nf_fread (sssg(START_2D_ARRAY,itsss), ncid,
     &                                 sss_id, sss_rec, r2dvar)
            if (ierr .eq. nf_noerr) then
              MPI_master_only write(stdout,
     &        '(6x,2A,F12.4,2(1x,A,I4))') 'get_sss   ',
     &              '--  read SSS                       for time =',
     &               cff, 'rec =', sss_rec MYID
            else
              write(stdout,2) 'SSS', sss_rec, nf_strerror(ierr)
            endif
          else
            write(stdout,2) 'sss_time', sss_rec, nf_strerror(ierr)
          endif
        else
          write(stdout,'(/1x,A,I4,1x,A,I4/7x,4A/7x,2(A,G12.4)/)')
     &   'ERROR in get_sss: requested time record ',      sss_rec,
     &   'exeeds the last record', ntsss, 'available in forcing ',
     &   'netCDF file ''',   frcname(1:lfrc),   '''',  'tdays = ',
     &    tdays,        '  but the last available  sss_time =',
     &                                    sss_time(itsss)*sec2day
        endif
      enddo
  2   format(/1x,'### ERROR: get_sss: cannot read variable ''',
     &                         A, ''' for record ', I4/12x,A/)
      return
      end
#else
      subroutine get_sss_empty
      end
#endif /* QCORRECTION && !ANA_SST */
