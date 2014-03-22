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
      integer ncid,ifrc, ierr, lfrc, lenstr, nf_fread
      character(len=max_name_size) frcname
      character(len=3), parameter :: sst_var='SST'
      character(len=8), parameter :: sst_time_var='sst_time'


! Initialization: Check, whether forcing netCDF file is already
!----------------  opened, an if not, open it. Find and save netCDF
! IDs for relevant variables, and determine whether sst data exists
! as 2D field or scalar. After that determine whether there is
! cycling to reuse the input data and what is cycling period
! "sst_cycle", set initial cycling index "sst_ncycle", record index
! "sst_rec", and time index "itsst" and both time record bounds to
! large negative artificial values, so that it will trigger logic in
! reading part below.
#ifdef VERBOSE
      write(*,*) 'enter get_sst' MYID
#endif

      ifrc=max(sst_file_id,1)
 100  ncid=ncidfrc(ifrc)
      frcname=frcfile(ifrc)
      lfrc=lenstr(frcname)
      ierr=nf_noerr

      if (iic==ntstart) then
        if (ncid==-1) then
          ierr=nf_open(frcname(1:lfrc), nf_nowrite, ncid)
          if (ierr==nf_noerr) then
            ncidfrc(ifrc)=ncid
          else
            write(*,'(/1x,4A/)')   '### ERROR: get_sst :: Cannot ',
     &               'open netCDF file ''', frcname(1:lfrc), '''.'
          endif
        endif
        if (ierr == nf_noerr) then
          ierr=nf_inq_varid (ncid, sst_var, sst_id)
          if (ierr == nf_noerr) then
            sst_file_id=ifrc
            mpi_nonexit_warn write(*,'(1x,5A,I3)') 'Found ''', sst_var,
     &                 ''' in ''', frcname(1:lfrc), ''', ifrc =', ifrc
            ierr=nf_inq_varid (ncid, sst_time_var, sst_tid)
            if (ierr == nf_noerr) then
              call set_cycle (ncid, sst_tid, ntsst, sst_cycle,
     &                                 sst_ncycle, sst_rec, ierr)
              itsst=1
              sst_time(1)=-1.E+20
              sst_time(2)=-1.E+20
            else
              write(*,2) sst_time_var, frcname(1:lfrc)
            endif
          elseif (ifrc < max_frc) then
            mpi_nonexit_warn write(*,1) sst_var, frcname(1:lfrc)
            ifrc=ifrc+1
            goto 100
          else
            write(*,2) sst_var,  frcname(1:lfrc)
          endif
        endif
      endif
  1   format(1x,  'WARNING: get_sst :: Cannot find variable ''',
     &                             A, ''' in file ''', A, '''.')
  2   format(1x,'### ERROR: get_sst :: Cannot find variable ''',
     &                             A, ''' in file ''', A, '''.')


! Read data from the file: Check if model time is bounded by the past
!----- ---- ---- --- ----- and the future data times: if not, increment
! record and cycling indices, flip time index and read a new portion of
! the data. Repeat until model time falls between the two data times.

      do while (sst_time(itsst)<time+0.5*dt .and.ierr==nf_noerr)
        call advance_cycle (sst_cycle, ntsst, sst_ncycle,
     &                                 sst_rec,     ierr)
        if (ierr == nf_noerr) then
          ierr=nf_get_var1_FTYPE (ncid, sst_tid, sst_rec, cff)
          if (ierr == nf_noerr) then
            itsst=3-itsst
            sst_time(itsst)=cff*day2sec + sst_cycle*sst_ncycle
            ierr=nf_fread (sstg(START_2D_ARRAY,itsst), ncid,
     &                                 sst_id, sst_rec, r2dvar)
            if (ierr == nf_noerr) then
            mpi_nonexit_warn write(*,'(7x,A,27x,A,F12.4,2(1x,A,I4))')
     &                      'get_sst :: read SST', 'time =',cff,
     &                      'rec =', sst_rec MYID
            else
              write(*,3) 'SST', sst_rec
            endif
          else
            write(*,3) sst_time_var, sst_rec
          endif
        else
          write(*,'(/1x,A,I4,1x,A,I4/7x,4A/7x,2(A,G12.4)/)')
     &   '### ERROR: get_sst :: requested time record ',  sst_rec,
     &   'exeeds the last record', ntsst, 'available in forcing ',
     &   'netCDF file ''',   frcname(1:lfrc),   '''',  'tdays = ',
     &    tdays,        '  but the last available  sst_time =',
     &                                    sst_time(itsst)*sec2day
        endif
      enddo
  3   format(/1x,'### ERROR: get_sst :: Cannot read variable ''', A,
     &                                       ''' for record ', I4/)
# ifdef VERBOSE
      write(*,*) 'return from get_sst' MYID
# endif
      return
      end
#else
      subroutine get_sst_empty
      end
#endif /* QCORRECTION && !ANA_SST */
