#include "cppdefs.h"
#if defined SOLVE3D && defined QCORRECTION && !defined ANA_SST

      subroutine get_dqdt (ierr)
      implicit none
# define SST_DATA
# include "param.h"
# include "forces.h"
# include "scalars.h"
# include "netcdf.inc"
# include "ncvars.h"
      real cff
      integer ierr, ncid, ifrc, lfrc, lenstr, nf_fread
      character(len=max_name_size) frcname
      character(len=6), parameter :: dqdt_var='dQdSST'

! Initialization:  Check, whether forcing netCDF file is already
!----------------  opened, an if not, open it. Find and save netCDF
! IDs for relevant variables, and determine whether dqdt data exists
! as 2D field or scalar. After that determine whether there is
! cycling to reuse the input data and what is cycling period
! "dqdt_cycle", set initial cycling index "dqdt_ncycle", record index
! "dqdt_rec", and time index "itdqdt" and both time record bounds to
! large negative artificial values, so that it will trigger logic in
! the reading part below.
# ifdef VERBOSE
      write(*,*) 'enter get_dqdt' MYID
# endif

      ifrc=max(dqdt_file_id,1)
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
            write(*,'(/1x,4A/)') '### ERROR: get_dqdt :: Cannot ',
     &              'open netCDF file ''', frcname(1:lfrc), '''.'
          endif
        endif
        if (ierr == nf_noerr) then
          ierr=nf_inq_varid (ncid, dqdt_var, dqdt_id)
          if (ierr == nf_noerr) then
            dqdt_file_id=ifrc
            mpi_nonexit_warn write(*,'(1x,5A,I3)')        'Found ''',
     &      dqdt_var, ''' in ''', frcname(1:lfrc), ''', ifrc =',ifrc

! WARNING: Here dQdSST is presumed to have its own time variable,
! or, if not, found it is slaved to one of the existing fields, such
! as "sss" or "sst", in this priority. This policy, however is unsafe
! because it does not rule out possibility of wrong clocking of
! dQdSST, if SST does from dayly input, while dQdSST is computed
! from climatology and placed into the same file.

            ierr=nf_inq_varid (ncid, 'dqdt_time', dqdt_tid)
            if (ierr /= nf_noerr) then
c              ierr=nf_inq_varid (ncid, 'sst_time', dqdt_tid)
c              if (ierr == nf_noerr) then
c                mpi_nonexit_warn write(*,*) 'WARNING: timing of ''',
c     &                             dqdt_var, ''' is slaved to SST.'
c              else
                ierr=nf_inq_varid (ncid, 'sss_time', dqdt_tid)
                if (ierr == nf_noerr) then
                  mpi_nonexit_warn write(*,*)    'WARNING: timing ',
     &                    'of ''', dqdt_var, ''' is slaved to SSS.'
                endif
c              endif
            endif
            if (ierr == nf_noerr) then
              call set_cycle (ncid, dqdt_tid, ntdqdt,dqdt_cycle,
     &                              dqdt_ncycle, dqdt_rec, ierr)
              itdqdt=1
              dqdt_time(1)=-1.E+20
              dqdt_time(2)=-1.E+20
            else
              write(*,2) 'dqdt_time', frcname(1:lfrc)
            endif
          elseif (ifrc < max_frc) then
            mpi_nonexit_warn write(*,1) dqdt_var, frcname(1:lfrc)
            ifrc=ifrc+1
            goto 100
          else
            write(*,2) dqdt_var, frcname(1:lfrc)
          endif
        endif
      endif
  1   format(1x,  'WARNING: get_dqdt :: Cannot find variable ''',
     &                               A, ''' in file ''', A, '''.')
  2   format(1x, '### ERROR: get_dqdt :: Cannot find variable ''',
     &                               A, ''' in file ''', A, '''.')

! Read data from the file: Check if model time is bounded by the past
!----- ---- ---- --- ----- and the future data times: if not, increment
! record and cycling indices, flip time index and read a new portion of
! the data. Repeat until model time falls between the two data times.

      do while (dqdt_time(itdqdt)<time+0.5*dt .and.
     &                                   ierr==nf_noerr)
        call advance_cycle (dqdt_cycle, ntdqdt, dqdt_ncycle,
     &                                 dqdt_rec,     ierr)
        if (ierr == nf_noerr) then
          ierr=nf_get_var1_FTYPE (ncid, dqdt_tid, dqdt_rec, cff)
          if (ierr == nf_noerr) then
            itdqdt=3-itdqdt
            dqdt_time(itdqdt)=cff*day2sec + dqdt_cycle*dqdt_ncycle
            ierr=nf_fread (dqdtg(START_2D_ARRAY,itdqdt), ncid,
     &                                 dqdt_id, dqdt_rec, r2dvar)
            if (ierr == nf_noerr) then
#ifdef MPI_SILENT_MODE
              if (mynode==0) then
#endif
                write(*,'(6x,2A,F12.4,2(1x,A,I4))')  'get_dqdt :: ',
     &                  'read dQdT                      for time =',
     &                   cff, 'rec =', dqdt_rec MYID
#ifdef MPI_SILENT_MODE
              endif
#endif
            else
              write(*,3) 'dQdT', dqdt_rec
            endif
          else
            write(*,3) 'dqdt_time', dqdt_rec
          endif
        else
          write(*,'(/1x,A,I4,1x,A,I4/7x,4A/7x,2(A,G12.4)/)')
     &   '### ERROR: get_dqdt :: requested time record ', dqdt_rec,
     &   'exeeds the last record', ntdqdt, 'available in forcing ',
     &   'netCDF file ''',   frcname(1:lfrc),   '''',   'tdays = ',
     &    tdays,          '  but the last available  dqdt_time =',
     &                                 dqdt_time(itdqdt)*sec2day
        endif
      enddo
  3   format(/1x,'### ERROR: get_dqdt :: Cannot read variable ''',
     &                                   A, ''' for record ', I4/)
# ifdef VERBOSE
      write(*,*) 'return from get_dqdt' MYID
# endif
      return
      end
#else
      subroutine get_dqdt_empty
      end
#endif /* QCORRECTION && !ANA_SST */
