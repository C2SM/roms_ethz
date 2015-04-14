#include "cppdefs.h"

      subroutine get_init (req_rec, tindx)    ! Read initial conditions
      implicit none                           ! from netCDF file.
#include "param.h"
#include "ncvars.h"
#include "scalars.h"
#include "netcdf.inc"

      integer req_rec, tindx, max_rec,  record,   ncid, varid,
     &        ierr,    lstr,  start(2), count(2), ibuff(iaux),
     &        lvar,    latt,  lenstr,   nf_fread, checkdims
#ifdef SOLVE3D
     &                                          , itrc
#endif
      real time_scale
      integer init_type
      integer, parameter :: init_run=1,  rst_run=2
#ifdef EXACT_RESTART
     &                    , apprx_rst=3, exact_rst=4
      real time_bak
      integer vid_DU, vid_DV
#endif
      character(len=64) units
#include "ocean2d.h"
#ifdef EXACT_RESTART
# include "coupling.h"
#endif
#include "ocean3d.h"
#ifdef LMD_KPP
# include "mixing.h"
#endif
#define time illegal
#define tdays illegal
#define nrrec illegal
c--#define VERBOSE
#ifdef VERBOSE
      write(*,'(3(2x,A,I3))') 'enter get_init: req_rec =', req_rec,
     &            'tindx =', tindx, 'may_day_flag =', may_day_flag
#endif
      if (may_day_flag/=0) return      !-->  EXIT

! Open conditions netCDF file for reading.  Check that all spatial
! dimensions in that file are consistent with the model, determine how
! many time records are available in the file and determine the number
! of the record from which the data will be read. The record is set as
! follows:
!          (1) if only one time record is available in the file, then
!              that record is used REGARDLESS of value of "nrrec"
!              supplied from the input parameter file;
!
!          (2) if the file has multiple records and "nrrec" is
!              positive, then "nrrec" is used, provided that "nrrec"
!              is within the available records;
!
!          (3) if the file has multiple records and nrrec<0, then
!              THE LAST available record is used.

      init_type=0
      lstr=lenstr(ininame)
      ierr=nf_open(ininame(1:lstr), nf_nowrite, ncid)
      if (ierr == nf_noerr) then
        ierr=checkdims (ncid, ininame, max_rec)
        if (ierr. eq. nf_noerr) then
          if (max_rec>0) then
            if (req_rec>0) then
              if (req_rec<=max_rec) then
                record=req_rec
              else
                write(*,'(/1x,2A,I4,1x,A/12x,A,I4,1x,3A/)')
     &              '### ERROR: get_init :: requested restart time ',
     &              'record',req_rec, 'exceeds number', 'of records',
     &                       max_rec,  'available in netCDF file ''',
     &                                        ininame(1:lstr), '''.'
              endif
            else
              record=max_rec
            endif
          else
            record=1
          endif
        endif
      else
        write(*,'(/1x,4A/12x,A/)')  '### ERROR: get_init :: Cannot ',
     &                 'open netCDF file ''', ininame(1:lstr), '''.',
     &                                             nf_strerror(ierr)
      endif
      if (ierr /= nf_noerr) goto 99                     !--> ERROR

! Read in evolving model variables:
!----- -- -------- ----- ----------
! Time: find netCDF id, read value, read attribute 'units' and set
! starting time index and time clock in days.
!
! Note that if EXACT_RESTART CPP-switch is defined, make a "soft"
! attempt to do exact restart, where "soft" means that exact restart
! is done only when file of initial conditions contains sufficient
! data, i.e. two consecutive time records of evolving fields one time
! step apart from each other.  Thus, in order to accept the file for
! exact restart, it must pass two consecutive checks: (i) ocean_time
! values in two consecutive values must differ by "dt" of current run,
! and (ii) "ntstart" from two consecutive records of netCDF structure
! "time_step" must differ by one.  If either check fails, forward step
! is used as the initial time step.  "get_init" is expected to be
! called twice consecutively as
!
!           call get_init (req_rec=rec-1, tindx=2)
!           call get_init (req_rec=rec,   tindx=1)
!
! where "rec" is record number in netCDF file which contains fields
! corresponding to time step "n" while "rec-1" corresponds to "n-1"
! (hence, making it possible to start time stepping with regular LF
! predictor step rather than forward), both checks are performed
! during the first call, tindx=2. If either check fails, the exact
! restart is cancelled and no reading of 2D and 3D fields will be
! performed for tindx=2.
!
! The possibility of exact restart is communicated with the rest of
! the code via integer variable "forw_start" which is set exclussively
! by this routine and is used as part of CPP-macro
!
!             FIRST_TIME_STEP iic==forw_start
!
! where the possibilities are as follows:
!
!       forw_start=1   means that "exact" restart, i.e., forward
!                      step is to be performed only during absolutely
!                      first time step, iic=1, and
!
!       forw_start=ntstart, means that restarted time stepping should
!                           also begin with forward step (approximate
!                           restart).
!
! This mechanism of exact restart is designed to handle essentially
! three situations: (1) initial run using a 3rd-party file which
! contain initial time, but does not contain "time_step". In this
! case ntstart is set to 1, and forward step is assumed at the first
! step; (2) restart from restart file generated by this code, but
! with deactivated CPP-switch EXACT_RESTART.  This file contains both
! both time variable and "time_step" structure, but only one
! consecutive record. This situation is identified automatically and
! approximate restart is assumed. This is compatibility mode. This
! also incldes restart from a history file generated by this code.
! (3) restart from a file created by this code with activated
! EXACT_RESTART.

      lvar=lenstr(vname(1,indxTime))
      ierr=nf_inq_varid (ncid, vname(1,indxTime)(1:lvar), varid)

! The following is done for backward compatibility: normally time
! variable is named "ocean_time", but legacy startup files may name
! it either "roms_time" or "scrum_time".

      if (ierr /= nf_noerr) then
        ierr=nf_inq_varid (ncid, 'roms_time', varid)
      endif
      if (ierr /= nf_noerr) then
        ierr=nf_inq_varid (ncid, 'scrum_time', varid)
      endif

      if (ierr == nf_noerr) then
        ierr=nf_get_var1_FTYPE (ncid, varid, record, start_time)
        if (ierr == nf_noerr) then
          ierr=nf_get_att_text(ncid, varid, 'units', units)
          if (ierr == nf_noerr) then
            latt=lenstr(units)
            if (units(1:6)=='second') then
              time_scale=1.
            elseif (units(1:3)=='day') then
              time_scale=day2sec
            else
              write(*,'(/1x,4A/12x,3A/)')    '### ERROR: get_init :',
     &                          ': unknown units of for variable ''',
     &                               vname(1,indxTime)(1:lvar), '''',
     &                   'in netCDF file ''', ininame(1:lstr), '''.'
              goto 99                                    !--> ERROR
            endif
            start_time=start_time*time_scale


#ifdef EXACT_RESTART
            if (tindx==2) then
              forw_start=0
              if (record<max_rec) then
                time_bak=start_time
                ierr=nf_get_var1_FTYPE (ncid, varid, record+1,
     &                                             start_time)
                if (ierr == nf_noerr) then
                  start_time=start_time*time_scale
# ifdef VERBOSE
                  write(*,'(3(1x,A,F16.6))') 'time_bak =', time_bak,
     &                    'start_time =', start_time,    'dt =', dt
# endif

! Here expression "abs(start_time-time_bak-dt)<0.001*dt" below is
! a roundoff-error tolerant version of "start_time==time_bak+dt".

                 if (abs(start_time-time_bak-dt) < 0.01*dt) then
                    forw_start=1
                  else
            mpi_nonexit_warn write(*,'(1x,2A,2I4/10x,4A/10x,A/)')
     &                'WARNING: Exact restart is requested, but ',
     &                'is not possible: records', record,record+1,
     &                'in ''',   ininame(1:lstr),   ''' are not ',
     &                'consecutive time steps ==> proceeding ',
     &                          'with forward initial time step.'
                  endif
                else
                  write(*,2) vname(1,indxTime)(1:lvar), record,
     &                       ininame(1:lstr), nf_strerror(ierr)
                  goto 99
                endif
              else
                mpi_nonexit_warn write(*,'(1x,2A/10x,4A)')
     &           'WARNING: Exact restart is requested, but is not ',
     &           'possible: initial',  'file ''',   ininame(1:lstr),
     &                    ''' does not contain sufficient records.'
              endif
              if (forw_start/=1) return
              forw_start=0
            endif
#endif
          else
            write(*,'(/1x,4A/12x,3A/12x,A/)')  '### ERROR: get_init ',
     &           ':: Cannot read attribute ''units'' for variable ''',
     &          vname(1,indxTime)(1:lvar),'''', 'from netCDF file ''',
     &                    ininame(1:lstr),   '''.', nf_strerror(ierr)
          endif
        else
          write(*,2) vname(1,indxTime)(1:lvar), record,
     &               ininame(1:lstr), nf_strerror(ierr)
        endif
      else
        write(*,1) vname(1,indxTime)(1:lvar), ininame(1:lstr),
     &                                      nf_strerror(ierr)
      endif
      if (ierr /= nf_noerr)  goto 99                    !--> ERROR


#ifdef PACIFIC
c**  shift back by 20 years for Xavier's experiment with dayly winds,
c**  hence "day 15" corresponds to Jan 15 1990, rather than 1970.
c**      start_time=start_time - 20.*360. *day2sec
#endif


! Check presence of variable 'time_step', which is a structure of
! either four or eight integer numbers storing time step number and
! the corresponding records number in the output files.  If found,
! use them to restart time step and record counting (i.e., restart),
! otherwise initialise all counters from zero.

      ierr=nf_inq_varid (ncid, 'time_step', varid)
      if (ierr == nf_noerr) then
        ierr=nf_inq_vardimid (ncid, varid, ibuff)
        if (ierr == nf_noerr) then
          ierr=nf_inq_dimlen (ncid, ibuff(1), count(1))
          if (ierr == nf_noerr) then
            start(1)=1
            start(2)=record
            count(2)=1
            ibuff(1:iaux)=0
            ierr=nf_get_vara_int (ncid, varid, start, count, ibuff)
            if (ierr == nf_noerr) then
              ntstart=ibuff(1)+1
              nrecrst=ibuff(2)
              nrechis=ibuff(3)
#ifdef AVERAGES
              nrecavg=ibuff(4)
#endif
#ifdef STATIONS
              if (count(1)>=5) nrecstn=ibuff(5)
#endif
#ifdef FLOATS
              if (count(1)>=6) nrecflt=ibuff(6)
#endif
#ifdef EXACT_RESTART
              if (tindx==2 .and. record<max_rec) then
                start(2)=record+1
                ierr=nf_get_vara_int (ncid, varid, start, count, ibuff)
                if (ierr == nf_noerr) then
# ifdef VERBOSE
                  write(*,*) 'ibuff(1),ntstart =', ibuff(1), ntstart
# endif
                  if (ibuff(1) == ntstart) then
                    forw_start=1
                  else
                  mpi_nonexit_warn write(*,'(1x,2A,2I4/10x,4A/10x,A)')
     &             'WARNING: Exact restart is requested, but is not ',
     &               'possible: records',  record, record+1,  'in ''',
     &                ininame(1:lstr), ''' are not consecutive time ',
     &                                    'steps ==> proceeding with',
     &                                   'forward initial time step.'

                    return   !--> no need to read preliminary record
                  endif
                else
                  write(*,2) 'time_step', record, ininame(1:lstr),
     &                                            nf_strerror(ierr)
                endif
              elseif (tindx==1) then
                if (forw_start==1) then
                  init_type=exact_rst
                else
                  init_type=apprx_rst
                endif
              endif
#else
              init_type=rst_run
#endif
            else
              write(*,2)  'time_step',  record,  ininame(1:lstr),
     &                                           nf_strerror(ierr)
            endif
          else
             write(*,'(/1x,3A/16x,A/)')     '### ERROR: get_init :: ',
     &            'Cannot determine size of the first dimension for ',
     &                  'variable ''time_step'':', nf_strerror(ierr)
          endif
        else
          write(*,'(/1x,3A/16x,A/)') '### ERROR: get_init :: Cannot ',
     &      'determine dimensions for netCDF variable ''time_step'':',
     &                                             nf_strerror(ierr)
        endif
        if (ierr /= nf_noerr)  goto 99                  !--> ERROR
      else
        init_type=init_run
        ntstart=1               ! netCDF variable "time_step" not
        nrecrst=0               ! found: proceed with initializing
        nrechis=0               ! all counters to zero (initial run).
#ifdef AVERAGES
        nrecavg=0
#endif
#ifdef STATIONS
        nrecstn=0
#endif
#ifdef FLOATS
        nrecflt=0
#endif
      endif
#ifdef EXACT_RESTART
      if (tindx==1 .and. forw_start==0) forw_start=ntstart
#endif
#ifdef AVERAGES
      if (tindx==1) then
        if (ntstart>1) then
          mpi_master_only write(*,'(/4x,A,I5,A,I8/)')
     &         'Adjusting averaging phase ntsavg =',  ntsavg,
     &                          ' ---> ', ntsavg + ntstart-1
          ntsavg=ntsavg + ntstart-1
        endif
      endif
#endif
#ifdef VERBOSE
      write(*,'(1x,2A,F12.4,1x,A,I4)')   'get_init: reading initial ',
     &   'fields for time =', start_time*sec2day, 'record =', record
#endif

! Read initial fields:
!---------------------

! Free-surface

      lvar=lenstr(vname(1,indxZ))
      ierr=nf_inq_varid (ncid, vname(1,indxZ)(1:lvar), varid)
      if (ierr == nf_noerr) then
        ierr=nf_fread (zeta(START_2D_ARRAY,1), ncid, varid,
     &                                         record, r2dvar)
        if (ierr /= nf_noerr) then
          write(*,2) vname(1,indxZ)(1:lvar),   record,
     &               ininame(1:lstr), nf_strerror(ierr)
          goto 99                                         !--> ERROR
        endif
      else
        write(*,1) vname(1,indxZ)(1:lvar), ininame(1:lstr),
     &                                   nf_strerror(ierr)
        goto 99                                           !--> ERROR
      endif

! 2D momentum component in the XI- and ETA-directions

      lvar=lenstr(vname(1,indxUb))
      ierr=nf_inq_varid (ncid, vname(1,indxUb)(1:lvar), varid)
      if (ierr == nf_noerr) then
        ierr=nf_fread (ubar(START_2D_ARRAY,1), ncid, varid,
     &                                         record, u2dvar)
        if (ierr /= nf_noerr) then
          write(*,2) vname(1,indxUb)(1:lvar),   record,
     &               ininame(1:lstr), nf_strerror(ierr)
          goto 99                                        !--> ERROR
        endif
      else
        write(*,1) vname(1,indxUb)(1:lvar), ininame(1:lstr),
     &                                    nf_strerror(ierr)
        goto 99                                           !--> ERROR
      endif

      lvar=lenstr(vname(1,indxVb))
      ierr=nf_inq_varid (ncid, vname(1,indxVb)(1:lvar), varid)
      if (ierr == nf_noerr) then
        ierr=nf_fread (vbar(START_2D_ARRAY,1), ncid, varid,
     &                                         record, v2dvar)
        if (ierr /= nf_noerr) then
          write(*,2) vname(1,indxVb)(1:lvar),   record,
     &               ininame(1:lstr), nf_strerror(ierr)
          goto 99                                          !--> ERROR
        endif
      else
        write(*,1) vname(1,indxVb)(1:lvar), ininame(1:lstr),
     &                                    nf_strerror(ierr)
        goto 99                                           !--> ERROR
      endif

#ifdef SOLVE3D
# ifdef EXACT_RESTART
      ierr=nf_inq_varid (ncid, 'DU_avg2', vid_DU)
      if (ierr == nf_noerr) then
        ierr=nf_inq_varid (ncid, 'DV_avg2', vid_DV)
        if (ierr == nf_noerr) then
          ierr=nf_fread (DU_avg2, ncid, vid_DU, record, u2dvar)
          if (ierr == nf_noerr) then
            ierr=nf_fread (DV_avg2, ncid, vid_DV, record, v2dvar)

            if (ierr /= nf_noerr) then
              write(*,2) 'DV_avg2',  record, ininame(1:lstr),
     &                                       nf_strerror(ierr)
              goto 99                                    !--> ERROR
            endif
          else
            write(*,2)   'DU_avg2',  record, ininame(1:lstr),
     &                                       nf_strerror(ierr)
            goto 99                                      !--> ERROR
          endif
        else
          forw_start=ntstart    !--> cancel exact restart
        endif
      else
        forw_start=ntstart      !--> cancel exact restart
      endif
# endif

! 3D momentum components in XI- and ETA-directions

      lvar=lenstr(vname(1,indxU))
      ierr=nf_inq_varid (ncid, vname(1,indxU)(1:lvar), varid)
      if (ierr == nf_noerr) then
        ierr=nf_fread (u(START_2D_ARRAY,1,tindx), ncid, varid,
     &                                         record, u3dvar)
        if (ierr /= nf_noerr) then
          write(*,2) vname(1,indxU)(1:lvar),    record,
     &               ininame(1:lstr), nf_strerror(ierr)
          goto 99                                         !--> ERROR
        endif
      else
        write(*,1) vname(1,indxU)(1:lvar), ininame(1:lstr),
     &                                   nf_strerror(ierr)
        goto 99                                           !--> ERROR
      endif

      lvar=lenstr(vname(1,indxV))
      ierr=nf_inq_varid (ncid, vname(1,indxV)(1:lvar), varid)
      if (ierr == nf_noerr) then
        ierr=nf_fread (v(START_2D_ARRAY,1,tindx), ncid, varid,
     &                                         record, v3dvar)
        if (ierr /= nf_noerr) then
          write(*,2) vname(1,indxV)(1:lvar),    record,
     &               ininame(1:lstr), nf_strerror(ierr)
          goto 99                                        !--> ERROR
        endif
      else
        write(*,1) vname(1,indxV)(1:lvar), ininame(1:lstr),
     &                                   nf_strerror(ierr)
        goto 99                                           !--> ERROR
      endif

! Tracer variables.

      do itrc=1,NT
        lvar=lenstr(vname(1,indxT+itrc-1))
        ierr=nf_inq_varid (ncid, vname(1,indxT+itrc-1)(1:lvar),
     &                                                    varid)
        if (ierr == nf_noerr) then
          ierr=nf_fread (t(START_2D_ARRAY,1,tindx,itrc), ncid,
     &                                    varid, record, r3dvar)
          if (ierr /= nf_noerr) then
            write(*,2) vname(1,indxT+itrc-1)(1:lvar), record,
     &                     ininame(1:lstr), nf_strerror(ierr)
            goto 99                                       !--> ERROR
          endif
        else
          write(*,1) vname(1,indxT+itrc-1)(1:lvar), ininame(1:lstr),
     &                                            nf_strerror(ierr)
          goto 99                                         !--> ERROR
        endif
      enddo
# ifdef LMD_KPP
      lvar=lenstr(vname(1,indxHbls))
      ierr=nf_inq_varid (ncid, vname(1,indxHbls)(1:lvar), varid)
      if (ierr == nf_noerr) then
        ierr=nf_fread (hbls(START_2D_ARRAY,tindx), ncid, varid,
     &                                           record, r2dvar)
        if (ierr /= nf_noerr) then
          write(*,2) vname(1,indxHbls)(1:lvar), record,
     &               ininame(1:lstr), nf_strerror(ierr)
          goto 99                                        !--> ERROR
        endif
      else
        mpi_nonexit_warn write(*,'(1x,6A)')        'WARNING: netCDF ',
     & 'variable ''', vname(1,indxHbls)(1:lvar), ''' not found in ''',
     &           ininame(1:lstr), ''' ==> initialized to zero state.'

      endif

# endif
# ifdef LMD_BKPP
      lvar=lenstr(vname(1,indxHbbl))
      ierr=nf_inq_varid (ncid, vname(1,indxHbbl)(1:lvar), varid)
      if (ierr == nf_noerr) then
        ierr=nf_fread (hbbl(START_2D_ARRAY,tindx), ncid, varid,
     &                                           record, r2dvar)
        if (ierr /= nf_noerr) then
          write(*,2) vname(1,indxHbbl)(1:lvar), record,
     &               ininame(1:lstr), nf_strerror(ierr)
          goto 99                                        !--> ERROR
        endif
      else
        mpi_nonexit_warn write(*,'(1x,6A)')        'WARNING: netCDF ',
     & 'variable ''', vname(1,indxHbbl)(1:lvar), ''' not found in ''',
     &           ininame(1:lstr), ''' ==> initialized to zero state.'

      endif
# endif

#endif

! Close input NetCDF file.

      ierr=nf_close(ncid)

! Write greeting message depending on type of initial conditions.

#ifdef MPI_SILENT_MODE
      if (mynode==0) then
#endif
       if (tindx==1) then
        if (init_type == init_run) then
          write(*,'(6x,2A,F12.4,1x,A,I4)')   'get_init: Read initial ',
     &   'conditions for day =', start_time*sec2day, 'record =',record
#ifdef EXACT_RESTART
        elseif (init_type == exact_rst) then
          write(*,'(6x,A,F12.4,1x,A,I4,A,I8,A,I4,A,I4,A)')
     &   'get_init: Exact restart from day =',  start_time*sec2day,
     &   'rec =', record, '(',ntstart,',', nrecrst,',', nrechis,').'
        elseif (init_type == apprx_rst) then
          write(*,'(6x,A,F12.4,1x,A,I4,A,I8,A,I4,A,I4,A)')
     &        'get_init: Approximate, single-step restart from day =',
     &         start_time*sec2day,  'rec =', record, '(',ntstart, ',',
     &                                     nrecrst, ',', nrechis,').'
#else
        elseif (init_type == rst_run) then
          write(*,'(6x,A,F12.4,1x,A,I4,A,I8,A,I4,A,I4,A)')
     &          'get_init: Restarted from day =', start_time*sec2day,
     &     'rec =', record, '(',ntstart,',', nrecrst,',', nrechis,').'
#endif
        else
          write(*,'(/1x,A/)') '### ERROR: get_init :: Unknown Error.'
          goto 99
        endif
       endif  !<-- tindex==1
#ifdef MPI_SILENT_MODE
      endif
#endif
#ifdef VERBOSE
      write(*,'(1x,3(1x,A,I6))') 'return from get_init, ntstart =',
     &                                                  ntstart
# ifdef EXACT_RESTART
     &                              ,   'forw_start =', forw_start
# endif
#endif
      return
  99  may_day_flag=2
      return
  1   format(/1x,'### ERROR: get_init :: Cannot find variable ''', A,
     &                 '''' /12x, 'in netCDF file ''', A, ''''/12x,A)
  2   format(/1x,'### ERROR: get_init :: Cannot read variable ''', A,
     &               '''' /12x, 'from netCDF file ''', A, ''''/12x,A)
      end
