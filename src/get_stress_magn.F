#include "cppdefs.h"
#if defined WIND_STRESS_AMP && !defined ANA_SMFLUX

      subroutine get_stress_magn (ierr)   ! Read magnitude of wind
      implicit none                       ! stress from netCDF file.
# define SMFLUX_DATA
# include "param.h"
# include "scalars.h"
# include "forces.h"
# include "ncvars.h"
# include "netcdf.inc"
      real cff
      integer ncid,ifrc, ierr, lfrc,lvar, lenstr, nf_fread
      character(len=max_name_size) frcname
      character(len=4), parameter :: var='atau'

! Initialization:  Check, whether forcing netCDF file is already
!----------------  opened, an if not, open it. Find and save netCDF
! IDs for relevant variables. Determine whether there is cycling to
! reuse the input data and what is cycling period "atau_cycle", find
! initial cycling index "atau_ncycle" and record index "atau_rec".
! Set initial value for time index "itatau" and set both time record
! bounds to large negative artificial values to trigger the logic
! in reading part below.

      ifrc=max(atau_file_id,1)
 100  ncid=ncfrc(ifrc)
      frcname=frcfile(ifrc)
      lfrc=lenstr(frcname)
      ierr=nf_noerr
      lvar=lenstr(var)

      if (iic==ntstart) then
        if (ncid==-1) then
          ierr=nf_open (frcname(1:lfrc), nf_nowrite, ncid)
          if (ierr==nf_noerr) then
            ncfrc(ifrc)=ncid
          else
            write(*,'(/1x,4A/12x,A)') '### ERROR: get_stress_magn ',
     &             ':: Cannot open netCDF file ''', frcname(1:lfrc),
     &                                     '''.', nf_strerror(ierr)
          endif
        endif
        if (ierr == nf_noerr) then
          ierr=nf_inq_varid (ncid, var, atauid)
          if (ierr == nf_noerr) then
            atau_file_id=ifrc
            write(*,'(1x,5A,I3)')           'Found ''', var(1:lvar),
     &              ''' in ''', frcname(1:lfrc), ''', ifrc =', ifrc
            ierr=nf_inq_varid (ncid, var(1:lvar)/ /'_time', atau_tid)
            if (ierr == nf_noerr) then
# ifdef VERBOSE
              write(*,'(1x,3A)')         'Found timing variable ''',
     &                                 var(1:lvar)/ /'_time', '''.'
# endif
            else
              ierr=nf_inq_varid (ncid, 'sms_time', atau_tid)
              if (ierr /= nf_noerr) then
                write(*,'(1x,4A)')          'No timing variable ''',
     &                        var(1:lvar)/ /'_time', ''' is found.',
     &                                        'Using ''sms_time''.'
              else
                write(*,'(1x,4A)') '### ERROR: get_stress_magn :: ',
     &                         'Cannot find timing variable for ''',
     &                                           var(1:lvar), '''.'
              endif
            endif
            if (ierr == nf_noerr) then
              call set_cycle (ncid, atau_tid, ntatau, atau_cycle,
     &                               atau_ncycle, atau_rec, ierr)
              itatau=1
              atau_time(1)=-1.E+20
              atau_time(2)=-1.E+20
            else
              write(*,1) , frcname(1:lfrc)
            endif
          elseif (ifrc < max_frc) then
            ifrc=ifrc+1
            goto 100
          else
            write(*,1) var(1:lvar), frcname(1:lfrc)
          endif
        endif
      endif
  1   format(1x,'WARNING: get_stress_magn :: Cannot find variable ''',
     &                                  A, ''' in file ''', A, '''.')

! Read data from the file: Check if model time is bounded by the past
!----- ---- ---- --- ----- and the future data times: if not, increment
! record and cycling indices, flip time index and read a new portion of
! the data. Repeat until model time falls between the two data times.

      if (ntatau>1) then
        do while (atau_time(itatau)<time+0.5*dt   .and.
     &                                      ierr==nf_noerr)
          call advance_cycle (atau_cycle, ntatau, atau_ncycle,
     &                                         atau_rec, ierr)
          if (ierr==nf_noerr) then
            ierr=nf_get_var1_FTYPE (ncid, atau_tid, atau_rec, cff)
            if (ierr == nf_noerr) then
              itatau=3-itatau
              atau_time(itatau)=atau_cycle*atau_ncycle + cff*day2sec

              ierr=nf_fread (amptaug(START_2D_ARRAY,itatau), ncid,
     &                                  atauid, atau_rec, r2dvar)
              if (ierr == nf_noerr) then
# ifdef MPI_SILENT_MODE
                if (mynode==0) then
# endif
                  write(*,'(1x,A,3x,A,F12.4,2(1x,A,I4))')
     &           'get_stress_magn :: read wind stress amplitude',
     &           var(1:lvar)/ /'_time =',cff,'rec =',atau_rec MYID
# ifdef MPI_SILENT_MODE
                endif
# endif
              else
                write(*,2) var(1:lvar), atau_rec, nf_strerror(ierr)
              endif
            else
              write(*,2) 'atau_time', atau_rec,  nf_strerror(ierr)
            endif
          else
            write(*,'(/2(1x,A,I4)/12x,3A/12x,A,G12.4,A,G12.4/)')
     &        '### ERROR: get_stress_magn :: Requested time record ',
     &         atau_rec,      'exeeds the last record',     ntatau,
     &        'available in netCDF file ''',  frcname(1:lfrc),  '''',
     &        'tdays = ',   tdays,  '   while the last ',
     &        'available  atau_time =', atau_time(itatau)*sec2day
          endif
        enddo
      elseif (iic==ntstart) then
        atau_rec=1
        ierr=nf_fread (amptaug(START_2D_ARRAY,itatau), ncid,
     &                             atauid, atau_rec, r2dvar)
        if (ierr == nf_noerr) then
          write(*,'(6x,2A,3x,A,I4)') 'get_stress_magn -- read ',
     &            'time-invarient wind stress amplitude' MYID
        else
          write(*,2) var(1:lvar), atau_rec, nf_strerror(ierr)
        endif
      endif
  2   format(/1x,'### ERROR: get_stress_magn :: Cannot read ',
     &           'variable ''', A, ''' for rec =', I4/12x,A/)
      return
      end
#else
      subroutine get_stress_magn_empty
      end
#endif
