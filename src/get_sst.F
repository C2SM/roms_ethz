#include "cppdefs.h"
#if defined SOLVE3D && defined QCORRECTION && !defined ANA_SST
 
      subroutine get_sst (ierr) ! Read sea surface temperature
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
! IDs for relevant variables, and determine whether sst data exists
! as 2D field or scalar. After that determine whether there is
! cycling to reuse the input data and what is cycling period
! "sst_cycle", set initial cycling index "sst_ncycle", record index
! "sst_rec", and time index "itsst" and both time record bounds to
! large negative artificial values, so that it will trigger logic in
! reading part below.
!
      ifrc=max(sst_file_id,1)
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
            write(stdout,'(/1x,4A/)') '### ERROR in get_sst: can ',
     &           'not open netCDF file ''', frcname(1:lfrc), '''.'
          endif
        endif
        if (ierr .eq. nf_noerr) then
          lvar=lenstr(vname(1,indxSST))
          ierr=nf_inq_varid (ncid, vname(1,indxSST)(1:lvar), sst_id)
          if (ierr .eq. nf_noerr) then
            sst_file_id=ifrc
            write(*,*) 'Found ''', vname(1,indxSST)(1:lvar),
     &          ''' in ''', frcname(1:lfrc), ''', ifrc =', ifrc

            ierr=nf_inq_varid (ncid, 'sst_time', sst_tid)
            if (ierr .eq. nf_noerr) then
              call set_cycle (ncid, sst_tid, ntsst, sst_cycle,
     &                                 sst_ncycle, sst_rec, ierr)
              itsst=1
              sst_time(1)=-1.E+20
              sst_time(2)=-1.E+20
            else
              write(stdout,1) 'sst_time', frcname(1:lfrc)
            endif
          else
            write(stdout,1) vname(1,indxSST)(1:lvar), frcname(1:lfrc)
            if (ifrc .lt. max_frc) then
              ifrc=ifrc+1
              goto 100
            endif
          endif
        endif
      endif
  1   format(/1x,'WARNING: get_sst cannot find variable ''',
     &     A, ''''/7x, 'in forcing netCDF file ''', A, '''.')
!
! Reading data from the forcing file:  Get out, if model time is
!======== ==== ==== === ======= =====  already within the interval
! set by the past and future data times. Otherwise flip the time
! index, increment record and cyclin indices and read a new portion
! of data. Repeat it until model time is between the two times from
! data.
!
      do while (sst_time(itsst).lt.time+0.5*dt .and.ierr.eq.nf_noerr)
        call advance_cycle (sst_cycle, ntsst, sst_ncycle,
     &                                 sst_rec,     ierr)
        if (ierr .eq. nf_noerr) then
          ierr=nf_get_var1_FTYPE (ncid, sst_tid, sst_rec, cff)
          if (ierr .eq. nf_noerr) then
            itsst=3-itsst
            sst_time(itsst)=cff*day2sec + sst_cycle*sst_ncycle
            ierr=nf_fread (sstg(START_2D_ARRAY,itsst), ncid,
     &                                 sst_id, sst_rec, r2dvar)
            if (ierr .eq. nf_noerr) then
              write(stdout,'(6x,2A,F12.4,2(1x,A,I4))') 'get_sst    ',
     &                '-- read SST                       for time =',
     &                   cff, 'rec =', sst_rec MYID
            else
              write(stdout,2) 'SST', sst_rec
            endif
          else
            write(stdout,2) 'sst_time', sst_rec
          endif
        else
          write(stdout,'(/1x,A,I4,1x,A,I4/7x,4A/7x,2(A,G12.4)/)')
     &   'ERROR in get_sst: requested time record ',      sst_rec,
     &   'exeeds the last record', ntsst, 'available in forcing ',
     &   'netCDF file ''',   frcname(1:lfrc),   '''',  'tdays = ',
     &    tdays,        '  but the last available  sst_time =',
     &                                    sst_time(itsst)*sec2day
        endif
      enddo
  2   format(/1x,'ERROR in get_sst: cannot read variable ''', A,
     &                                    ''' for record ', I4/)
      return
      end
#else
      subroutine get_sst_empty
      end
#endif /* QCORRECTION && !ANA_SST */
