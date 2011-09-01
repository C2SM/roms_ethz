#include "cppdefs.h"
                                                ! Read initial
      subroutine get_initial (req_rec, tindx)   ! conditions from
                                                ! netCDF file.
      implicit none
      integer req_rec, tindx,     ncid,  max_rec, record,
     &        varid, ierr,    start(2), count(2), ibuff(4),
     &        lstr, lvar, latt, lenstr, nf_fread, checkdims
#ifdef SOLVE3D
     &                                          , itrc
#endif
      real time_scale
#ifdef EXACT_RESTART
      real time_bak
#endif
      character units*64
#include "param.h"
#include "scalars.h"
#include "ncvars.h"
#include "netcdf.inc"
#include "ocean2d.h"
#include "ocean3d.h"
#ifdef BIOLOGY_BEC
# include "ecosys_bec.h"
!# include "ncroms_bgcflux.h"
!HF not needed, only for debugging:
# include "param_bec.h"
#endif
#ifdef PHYS_FLUX_ANALYSIS
#  include "physflux.h"
#  include "ncroms_physflux.h"
#endif
#ifdef BGC_FLUX_ANALYSIS
# include "bgcflux.h"
# include "ncroms_bgcflux.h"
#endif
#if defined LMD_KPP || defined LMD_BKPP
# include "mixing.h"
#endif
#define time illegal
#define tdays illegal
#define nrrec illegal
#if defined BIOLOGY_NPZDOC && !defined BGC_FLUX_ANALYSIS
# include "bgcflux.h"
#endif
!
! Open initial conditions netCDF file for reading. Check that all
! spatial dimensions in that file are consistent with the model
! arrays, determine how many time records are available in the file
! and determine number of record from which the data will be read.
! The record is set as follows: (1) if only one time record is
! available in the file, then that record is used REGARDLESS of value
! of "nrrec" supplied from the parameter file; (2) if the file has
! multiple records and "nrrec" is positive, then "nrrec" is used,
! provided that "nrrec" is within the available records; (3) if the
! file has multiple records and nrrec<0, then THE LAST available
! record is used.
!
      if (may_day_flag.ne.0) return      !-->  EXIT
      lstr=lenstr(ininame)
      ierr=nf_open(ininame(1:lstr), nf_nowrite, ncid)
      if (ierr .eq. nf_noerr) then
        ierr=checkdims (ncid, ininame(1:lstr), lstr, max_rec)
        if (ierr. eq. nf_noerr) then
          if (max_rec.gt.0) then
            if (req_rec.gt.0) then
              if (req_rec.le.max_rec) then
                record=req_rec
              else
                write(stdout,'(/1x,2A,I4,1x,A/12x,A,I4,1x,3A/)')
     &           '### ERROR: get_initial :: requested restart time ',
     &           'record',  req_rec, 'exceeds number',  'of records',
     &                       max_rec,  'available in netCDF file ''',
     &                                        ininame(1:lstr), '''.'

              goto 99                                      !--> ERROR
              endif
            else
              record=max_rec
            endif
          else
            record=1
          endif
        else
          goto 99
        endif
      else
        write(stdout,'(/1x,4A/12x,A/)') '### ERROR: get_initial :: ',
     &               'Cannot open netCDF file ''',   ininame(1:lstr),
     &                                  '''.',     nf_strerror(ierr)
        goto 99                                           !--> ERROR
      endif
!
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
! values in two consecutive values must differ by "dt" of current
! run, and (ii) "ntstart" from two consecutive records of netCDF
! structure "time_step" must differ by one.  If either check
! is fails, forward step is used as the initial time step.  Since  
! "get_initial" is expected to be called twice consecutively as
!
!           call get_initial (req_rec=rec-1, tindx=2)
!           call get_initial (req_rec=rec,   tindx=1)
!
! where "rec" is record number in netCDF file which contains fields 
! corresponding to time step "n" while "rec-1" corresponds to "n-1"
! (hence, making it possible to start time stepping with regular LF
! predictor step rather than forward), both checks are performed
! during the first call, tindx=2. If either check fails, the exact
! restart is cancelled and no reading of 2D and 3D fields will be
! performed for tindx=2.
!
! The possibility of exact restart is communicated with the rest
! of the code via integer variable "forw_start"  which is set
! exclussively by this routine and is used as part of CPP-macro
!
!             FIRST_TIME_STEP iic.eq.forw_start
!
! where the possibilities are as follows:
!
!       forw_start=1 means that "exact" restart, i.e., forward step
!                    is to be performed only during absolutely first
!                    time step, iic=1, and
!
!       forw_start=ntstart, means that restarted time stepping
!                           should also begin with forward step
!                           (this is approximate restart). 
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
!
      lvar=lenstr(vname(1,indxTime))
      ierr=nf_inq_varid (ncid, vname(1,indxTime)(1:lvar), varid)
!
! The following is done for backward compatibility: normally time
! variable is named "ocean_time", but legacy startup files may name
! it either "roms_time" or "scrum_time".
!
      if (ierr .ne. nf_noerr) then
        ierr=nf_inq_varid (ncid, 'roms_time', varid)
      endif
      if (ierr .ne. nf_noerr) then
        ierr=nf_inq_varid (ncid, 'scrum_time', varid)
      endif

      if (ierr .eq. nf_noerr) then
        ierr=nf_get_var1_FTYPE (ncid, varid, record, start_time)
        if (ierr .eq. nf_noerr) then
          ierr=nf_get_att_text(ncid, varid, 'units', units)
          if (ierr .eq. nf_noerr) then
            latt=lenstr(units)
            if (units(1:6).eq.'second') then
              time_scale=1.
            elseif (units(1:3).eq.'day') then
              time_scale=day2sec
            else
              write(*,'(/1x,4A/12x,3A/)') '### ERROR: get_initial :',
     &                          ': unknown units of for variable ''',
     &                               vname(1,indxTime)(1:lvar), '''',
     &                   'in netCDF file ''', ininame(1:lstr), '''.'
              goto 99                                    !--> ERROR
            endif
            start_time=start_time*time_scale


#ifdef EXACT_RESTART
            if (tindx.eq.2) then
              forw_start=0
              if (record.lt.max_rec) then
                time_bak=start_time
                ierr=nf_get_var1_FTYPE (ncid, varid, record+1,
     &                                             start_time)
                if (ierr .eq. nf_noerr) then
                  start_time=start_time*time_scale
!
! Here expression "abs(start_time-time_bak-dt).lt.0.001*dt" below is
! a roundoff-error tolerant version of "start_time.eq.time_bak+dt".
!
                  if (abs(start_time-time_bak-dt) .lt. 0.001*dt) then
                    forw_start=1
                  else
                    write(*,'(1x,2A,2I4/10x,4A/10x,A/)')
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
                write(*,'(1x,2A/10x,4A)')  'WARNING: Exact restart ',
     &                  'is requested, but is not possible: initial',
     &                  'file ''',  ininame(1:lstr),  ''' does not ',
     &                                 'contain sufficient records.'
              endif
              if (forw_start.ne.1) return
              forw_start=0
            endif
#endif
          else
            write(*,'(/1x,5A/12x,3A/12x,A/)')     '### ERROR: ',
     &             'get_initial :: Cannot read attribute ''units'' ',
     &             'for variable ''', vname(1,indxTime)(1:lvar),'''',
     &             'from netCDF file ''',   ininame(1:lstr),   '''.',
     &                                            nf_strerror(ierr)
            goto 99                                       !--> ERROR
          endif
        else
          write(*,2) vname(1,indxTime)(1:lvar), record,
     &               ininame(1:lstr), nf_strerror(ierr)
          goto 99                                         !--> ERROR
        endif
      else
        write(*,1) vname(1,indxTime)(1:lvar), ininame(1:lstr),
     &                                      nf_strerror(ierr)
        goto 99                                           !--> ERROR
      endif
 

#ifdef PACIFIC
c**  shift back by 20 years for Xavier's experiment with dayly winds,
c**  hence "day 15" corresponds to Jan 15 1990, rather than 1970.
c**      start_time=start_time - 20.*360. *day2sec
#endif
 

      ierr=nf_inq_varid (ncid, 'time_step', varid)
      if (ierr .eq. nf_noerr) then
        start(1)=1
        start(2)=record
        count(1)=4
        count(2)=1
        ierr=nf_get_vara_int (ncid, varid, start, count, ibuff)
        if (ierr .eq. nf_noerr) then
          ntstart=ibuff(1)
          nrecrst=ibuff(2)
          nrechis=ibuff(3)
#ifdef AVERAGES
          nrecavg=ibuff(4)
#endif
#ifdef EXACT_RESTART
          if (tindx.eq.2 .and. record.lt.max_rec) then
            start(2)=record+1
            ierr=nf_get_vara_int (ncid, varid, start, count, ibuff)
            if (ierr .eq. nf_noerr) then
              write(*,*) ibuff(1), ntstart
              if (ibuff(1) .eq. ntstart+1) then
                forw_start=1
              else
                write(*,'(1x,3A,2I4/10x,4A/10x,A)')      'WARNING: ',
     &           'Exact restart is requested, but is not possible: ',
     &           'records', record,record+1, 'in ''',ininame(1:lstr),
     &           ''' are not consecutive time steps ==> proceeding ',
     &                          'with', 'forward initial time step.'
              endif
            else
              write(*,2) 'time_step', record, ininame(1:lstr),
     &                                      nf_strerror(ierr)
              goto 99
            endif
          endif 
          if (tindx.eq.2 .and. forw_start.ne.1) return
          if (tindx.eq.1) then


            if (forw_start.eq.1) then
              write(*,'(6x,A,F12.4,A,I4,A,I8,A,I4,A,I4,A)')
     &                'get_initial: Exact restart from day =',
     &                 start_time*sec2day,  ' rec =', record,
     &          '(', ntstart, ',', nrecrst, ',', nrechis, ').'
            else
#endif
              write(*,'(6x,A,F12.4,A,I4,A,I8,A,I4,A,I4,A)')
     &                'get_initial: Restarted from day =',
     &                 start_time*sec2day,  ' rec =', record,
     &          '(', ntstart, ',', nrecrst, ',', nrechis, ').'
#ifdef EXACT_RESTART
            endif
          endif
#endif
        else
          write(*,2) 'time_step', record, ininame(1:lstr), 
     &                                  nf_strerror(ierr)
          goto 99                                         !--> ERROR
        endif
      else
        ntstart=1
        nrecrst=0
        nrechis=0
#ifdef AVERAGES
        nrecavg=0
#endif
        write(*,'(6x,2A,F12.4,1x,A,I4)')  'get_initial: Processing ',
     &    'data for time =', start_time*sec2day, 'record =', record 
      endif
#ifdef EXACT_RESTART
      if (tindx.eq.1 .and. forw_start.eq.0) forw_start=ntstart
#endif

#ifdef AVERAGES
      if (tindx.eq.1) then
        if (ntstart.gt.1) then
          MPI_master_only write(stdout,'(/4x,A,I5,A,I5/)')
     &         'Adjusting averaging phase ntsavg =',  ntsavg,
     &                          ' ---> ', ntsavg + ntstart-1
          ntsavg=ntsavg + ntstart-1
        endif
      endif
#endif

#if defined BGC_FLUX_ANALYSIS || defined PHYS_FLUX_ANALYSIS
      ierr=nf_inq_varid (ncid, 'fa_time_step', varid)
      if (ierr .eq. nf_noerr) then
        start(1)=1
        start(2)=record
        count(1)=4
        count(2)=1
        ierr=nf_get_vara_int (ncid, varid, start, count, ibuff)
        if (ierr .eq. nf_noerr) then
# ifdef BGC_FLUX_ANALYSIS
          nrec_bgc_flux_his=ibuff(1)
          write(stdout,'(6x,A,I4)')
     &         'GET_INITIAL: bgc_flux_his rec =',
     &         nrec_bgc_flux_his
#  ifdef AVERAGES
          nrec_bgc_flux_avg=ibuff(2)
          write(stdout,'(6x,A,I4)')
     &         'GET_INITIAL: bgc_flux_avg rec =',
     &         nrec_bgc_flux_avg
#  endif
# endif
# ifdef PHYS_FLUX_ANALYSIS
          nrec_phys_flux_his=ibuff(3)
          write(stdout,'(6x,A,I4)')
     &         'GET_INITIAL: phys_flux_his rec =',
     &         nrec_phys_flux_his
#  ifdef AVERAGES
          nrec_phys_flux_avg=ibuff(4)
          write(stdout,'(6x,A,I4)')
     &         'GET_INITIAL: phys_flux_avg rec =',
     &         nrec_phys_flux_avg
#  endif
# endif
        else
          write(stdout,'(/1x,2A/)') 'GET_INITIAL WARNING: Cannot ',
     &          'read record indices for flux analysis.'
!!!          goto 99                                         !--> ERROR
        endif
! no "else" needed, initialization to 0 in init_scalars...F routines
      endif
#endif /* BGC_FLUX_ANALYSIS || PHYS_FLUX_ANALYSIS */

!
! Free-surface.
!
      lvar=lenstr(vname(1,indxZ))
      ierr=nf_inq_varid (ncid, vname(1,indxZ)(1:lvar), varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fread (zeta(START_2D_ARRAY,1), ncid, varid,
     &                                         record, r2dvar)
        if (ierr .ne. nf_noerr) then
          write(*,2) vname(1,indxZ)(1:lvar),   record,
     &               ininame(1:lstr), nf_strerror(ierr)
          goto 99                                         !--> ERROR
        endif
      else
        write(*,1) vname(1,indxZ)(1:lvar), ininame(1:lstr),
     &                                   nf_strerror(ierr)
        goto 99                                           !--> ERROR
      endif
!
! 2D momentum component in the XI-direction.
!
      lvar=lenstr(vname(1,indxUb))
      ierr=nf_inq_varid (ncid, vname(1,indxUb)(1:lvar), varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fread (ubar(START_2D_ARRAY,1), ncid, varid,
     &                                         record, u2dvar)
        if (ierr .ne. nf_noerr) then
          write(*,2) vname(1,indxUb)(1:lvar),   record,
     &               ininame(1:lstr), nf_strerror(ierr)
          goto 99                                        !--> ERROR
        endif
      else
        write(*,1) vname(1,indxUb)(1:lvar), ininame(1:lstr),
     &                                    nf_strerror(ierr)
        goto 99                                           !--> ERROR
      endif
!
! 2D momentum component in the ETA-direction.
!
      lvar=lenstr(vname(1,indxVb))
      ierr=nf_inq_varid (ncid, vname(1,indxVb)(1:lvar), varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fread (vbar(START_2D_ARRAY,1), ncid, varid,
     &                                         record, v2dvar)
        if (ierr .ne. nf_noerr) then
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
!
! 3D momentum component in the XI-direction.
!
      lvar=lenstr(vname(1,indxU))
      ierr=nf_inq_varid (ncid, vname(1,indxU)(1:lvar), varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fread (u(START_2D_ARRAY,1,tindx), ncid, varid,
     &                                         record, u3dvar)
        if (ierr .ne. nf_noerr) then
          write(*,2) vname(1,indxU)(1:lvar),    record,
     &               ininame(1:lstr), nf_strerror(ierr)
          goto 99                                         !--> ERROR
        endif
      else
        write(*,1) vname(1,indxU)(1:lvar), ininame(1:lstr),
     &                                   nf_strerror(ierr)
        goto 99                                           !--> ERROR
      endif
!
! 3D momentum component in the ETA-direction.
!
      lvar=lenstr(vname(1,indxV))
      ierr=nf_inq_varid (ncid, vname(1,indxV)(1:lvar), varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fread (v(START_2D_ARRAY,1,tindx), ncid, varid,
     &                                         record, v3dvar)
        if (ierr .ne. nf_noerr) then
          write(*,2) vname(1,indxV)(1:lvar),    record,
     &               ininame(1:lstr), nf_strerror(ierr)
          goto 99                                        !--> ERROR
        endif
      else
        write(*,1) vname(1,indxV)(1:lvar), ininame(1:lstr),
     &                                   nf_strerror(ierr)
        goto 99                                           !--> ERROR
      endif
!
! Tracer variables.
!
      do itrc=1,NT
        lvar=lenstr(vname(1,indxT+itrc-1))
        ierr=nf_inq_varid (ncid, vname(1,indxT+itrc-1)(1:lvar),
     &                                                    varid)
        if (ierr .eq. nf_noerr) then
          ierr=nf_fread (t(START_2D_ARRAY,1,tindx,itrc), ncid,
     &                                    varid, record, r3dvar)
          if (ierr .ne. nf_noerr) then
            write(*,2) vname(1,indxT+itrc-1)(1:lvar), record,
     &                     ininame(1:lstr), nf_strerror(ierr)
            goto 99                                       !--> ERROR
          endif
        else
          write(*,1) vname(1,indxT+itrc-1)(1:lvar), ininame(1:lstr),
     &                                            nf_strerror(ierr)
# ifdef PASSIVE_TRACER
! HF: assume a special case: if number of passive tracers equals number
! of sigma levels, use each passive tracer to track advection/diffusion
! from one sigma level
          if (ntrc_pas .eq. N .and.
     &         (itrc .ge. itpas(1) .and. itrc .le. itpas(N)) ) then
             write(stdout,'(3a)') 'Start with unit concentration for ',
     &            'variable ', vname(1,indxT+itrc-1)(1:lvar)
!HF concentration is 1 in layer equal to index, 0 otherwise
             t(:,:,itrc-itpas(1)+1,tindx,itrc) = 1.0
          else
# endif
CDL Don't stop if no initial conditions are found. Start with 0 conc.
CDL instead:
             write(stdout,'(3a)') 'Start with zero concentration for ',
     &            'variable ', vname(1,indxT+itrc-1)(1:lvar)
CDL          goto 99                                         !--> ERROR
# ifdef PASSIVE_TRACER
          end if
# endif
        endif
      enddo
# ifdef SEDIMENT_BIOLOGY
!
! Sediment tracer variables.
!
      do itrc=1,NT_sed
        lvar=lenstr(vname(1,indxSedOrgN+itrc-1))
        ierr=nf_inq_varid (ncid, 
     &       vname(1,indxSedOrgN+itrc-1)(1:lvar), varid)
        if (ierr .eq. nf_noerr) then
          ierr=nf_fread (t_sed(START_2D_ARRAY,itrc), ncid,  varid,
     &                                               record, r2dvar)
          if (ierr .ne. nf_noerr) then
            write(stdout,2) vname(1,indxSedOrgN+itrc-1)(1:lvar),
     &            record,  ininame(1:lstr)
            goto 99                                       !--> ERROR
          endif
        else
          write(stdout,1) vname(1,indxSedOrgN+itrc-1)(1:lvar),
     &                                    ininame(1:lstr)
          write(stdout,'(a,a)') 'Note: Previous versions of ROMS did ',
     &         'not include sediment biology.'
          write(stdout,'(a)') 'Start with 0 concentration.'
CHF          goto 99                                         !--> ERROR
        endif
      enddo
# endif /* SEDIMENT_BIOLOGY */
# if defined BIOLOGY_BEC || (defined BIOLOGY_NPZDOC && \
    defined OXYGEN && defined CARBON)

      ! pH and PAR can be treated for both BEC and NPZD.
      ! if pH field is present in the restart file (only recently added), 
      ! these values will be used instead of the zero initialization
#  if defined CH_CARBON_DEPTH

      !
      ! pH_DEPTH:
      !
      lvar=lenstr(vname(1,indxPHd_rst))
      ierr=nf_inq_varid (ncid, vname(1,indxPHd_rst)(1:lvar), varid)
      if (ierr .eq. nf_noerr) then
         ierr=nf_fread (PHd_hist(START_2D_ARRAY,1), ncid,  varid,
     &                                               record, r3dvar)
         if (ierr .ne. nf_noerr) then
            write(stdout,2) vname(1,indxPHd_rst)(1:lvar), record,
     &           ininame(1:lstr)
            goto 99             !--> ERROR
         endif
      else
         MPI_master_only write(stdout,*)
     &        'pH_depth field not in init/rst file, initialize to 0'
      endif

#endif /*CH_CARBON_DEPTH*/


      !
      !pH
      !
      lvar=lenstr(vname(1,indxPH_rst))
      ierr=nf_inq_varid (ncid, vname(1,indxPH_rst)(1:lvar), varid)
      if (ierr .eq. nf_noerr) then
         ierr=nf_fread (ph_hist(START_2D_ARRAY), ncid,  varid,
     &                                               record, r2dvar)
         if (ierr .ne. nf_noerr) then
            write(stdout,2) vname(1,indxPH_rst)(1:lvar), record,
     &           ininame(1:lstr)
            goto 99             !--> ERROR
         endif
      else
         MPI_master_only write(stdout,*)
     &        'pH field not in init/rst file, initialize to 0'
      endif

      !
      ! pCO2:
      !
      lvar=lenstr(vname(1,indxPCO2_rst))
      ierr=nf_inq_varid (ncid, vname(1,indxPCO2_rst)(1:lvar), varid)
      if (ierr .eq. nf_noerr) then
         ierr=nf_fread (pCO2sw(START_2D_ARRAY), ncid,  varid,
     &                                               record, r2dvar)
         if (ierr .ne. nf_noerr) then
            write(stdout,2) vname(1,indxPCO2_rst)(1:lvar), record,
     &           ininame(1:lstr)
            goto 99             !--> ERROR
         endif
      else
         MPI_master_only write(stdout,*) 
     &        'PCO2 not in init/rst file, initialize to 0'
      endif

      ! pCO2 in the air:
      lvar=lenstr(vname(1,indxPCO2air_rst))
      ierr=nf_inq_varid (ncid, vname(1,indxPCO2air_rst)(1:lvar), varid)
      if (ierr .eq. nf_noerr) then
         ierr=nf_fread (pCO2air(START_2D_ARRAY), ncid,  varid,
     &                                               record, r2dvar)
         if (ierr .ne. nf_noerr) then
            write(stdout,2) vname(1,indxPCO2air_rst)(1:lvar), record,
     &           ininame(1:lstr)
            goto 99             !--> ERROR
         endif
      else
         MPI_master_only write(stdout,*) 
     &        'PCO2air not in init/rst file, initialize to 0'
      endif
# endif

# if defined BIOLOGY_BEC || defined BIOLOGY_NPZDOC
      ! Photosynthetically available radiation:
      lvar=lenstr(vname(1,indxPAR_rst))
      ierr=nf_inq_varid (ncid, vname(1,indxPAR_rst)(1:lvar), varid)
      if (ierr .eq. nf_noerr) then
         ierr=nf_fread (PAR(START_2D_ARRAY,1), ncid,  varid,
     &                                               record, r2dvar)
         if (ierr .ne. nf_noerr) then
            write(stdout,2) vname(1,indxPAR_rst)(1:lvar), record,
     &           ininame(1:lstr)
            goto 99             !--> ERROR
         endif
      else
         MPI_master_only write(stdout,*) 
     &        'PAR not in init/rst file, initialize to 0'
      endif
# endif /* BIOLOGY_BEC || BIOLOGY_NPZDOC */
# ifdef LMD_KPP
      lvar=lenstr(vname(1,indxHbl))
      ierr=nf_inq_varid (ncid, vname(1,indxHbl)(1:lvar), varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fread (hbls(START_2D_ARRAY,tindx), ncid, varid,
     &                                           record, r2dvar)
        if (ierr .ne. nf_noerr) then
          write(*,2) vname(1,indxHbl)(1:lvar), record,
     &               ininame(1:lstr), nf_strerror(ierr)
          goto 99                                        !--> ERROR
        endif
      else
        write(*,'(1x,5A)')        'WARNING: netCDF variable ''',
     &     vname(1,indxHbl)(1:lvar),       ''' not found in ''',
     &     ininame(1:lstr), ''' ==> initialized to zero state.' 

      endif

# endif

# ifdef LMD_BKPP
      lvar=lenstr(vname(1,indxHbbl))
      ierr=nf_inq_varid (ncid, vname(1,indxHbbl)(1:lvar), varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fread (hbbls(START_2D_ARRAY,tindx), ncid, varid,
     &                                           record, r2dvar)
        if (ierr .ne. nf_noerr) then
          write(*,2) vname(1,indxHbbl)(1:lvar), record,
     &               ininame(1:lstr), nf_strerror(ierr)
          goto 99                                        !--> ERROR
        endif
      else
        write(stdout,3) vname(1,indxHbbl)(1:lvar), ininame(1:lstr)
      endif
# endif
#endif /* SOLVE3D */
  1   format(/1x,'### NOTE: get_initial :: Cannot find variable ''',
     &               A,'''' /12x, 'in netCDF file ''', A, ''''/12x,A)
  2   format(/1x,'### ERROR: get_initial :: Cannot read variable ''',
     &             A,'''' /12x, 'from netCDF file ''', A, ''''/12x,A)
  3   format(/1x,'### WARNING: get_initial :: Cannot find variable ''',
     &             A,'''' /12x, ' from netCDF file ''',A, '''' /12x,
     &             ' ==> initialized to zero state.')
!
!  Close input NetCDF file.
!

      ierr=nf_close(ncid)
      return
  99  may_day_flag=2
      return
      end
