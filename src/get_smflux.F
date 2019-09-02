#include "cppdefs.h"
#ifndef ANA_SMFLUX
                                     ! Read surface momentum flux
      subroutine get_smflux(ierr)    ! (wind stress) at appropriate
      implicit none                  ! time from forcing netCDF file
# define SMFLUX_DATA
# include "param.h"
# include "scalars.h"
# include "forces.h"
# include "ncvars.h"
# include "netcdf.inc"
      real cff
      integer ncid,ifrc, ierr, lfrc, lenstr, nf_fread
      character(len=max_name_size) :: frcname

! Initialization: Check, whether forcing netCDF file is already in
!---------------- opened state, open it if not. Find and save netCDF
! IDs for relevant variables. Determine whether there is cycling to
! reuse the input data and what is cycling period "sms_cycle", find
! initial cycling index "sms_ncycle" and record index "sms_rec".
! Set initial value for time index "itsms" and set both time record
! bounds to large negative artificial values to trigger the logic
! in the reading part below.
# ifdef VERBOSE
      write(*,*) 'enter get_smflux' MYID
# endif

      ifrc=max(sms_file_id,1)
 100  ncid=ncfrc(ifrc)
      frcname=frcfile(ifrc)
      lfrc=lenstr(frcname)
      ierr=nf_noerr

      if (iic==ntstart) then
        if (ncid==-1) then
          ierr=nf_open (frcname(1:lfrc), nf_nowrite, ncid)
          if (ierr==nf_noerr) then
            ncfrc(ifrc)=ncid
          else
            write(*,'(/1x,4A,1x,2A,I4 /)') '### ERROR: get_smflux :: ',
     &            'Cannot open netCDF file ''', frcname(1:lfrc), '''.',
     &             nf_strerror(ierr) MYID
          endif
        endif
        if (ierr == nf_noerr) then
          ierr=nf_inq_varid (ncid, 'sustr', susid)
          if (ierr == nf_noerr) then
            sms_file_id=ifrc
            mpi_nonexit_warn write(*,'(1x,4A,I3)')  'Found ''sustr''',
     &                  ' in ''', frcname(1:lfrc), ''', ifrc =', ifrc
            ierr=nf_inq_varid (ncid, 'svstr', svsid)
            if (ierr == nf_noerr) then
              ierr=nf_inq_varid (ncid, 'sms_time', sms_tid)
              if (ierr == nf_noerr) then
                call set_cycle (ncid, sms_tid, ntsms, sms_cycle,
     &                                sms_ncycle, sms_rec, ierr)
                itsms=1
                sms_time(1)=-1.E+20
                sms_time(2)=-1.E+20
              else
                write(*,2) 'sms_time', frcname(1:lfrc)
              endif
            else
              write(*,2) 'svstr', frcname(1:lfrc)
            endif
          elseif (ifrc < max_frc) then
            mpi_nonexit_warn write(*,1) 'sustr', frcname(1:lfrc)
            ifrc=ifrc+1
            goto 100
          else
            write(*,2) 'sustr', frcname(1:lfrc)
          endif
        endif
      endif
  1   format(1x,   'WARNING: get_smflux :: Cannot find variable ''',
     &                                A, ''' in file ''', A, '''.')
  2   format(1x, '### ERROR: get_smflux :: Cannot find variable ''',
     &                                A, ''' in file ''', A, '''.')

! Read data from the file: Check if model time is bounded by the past
!----- ---- ---- --- ----- and the future data times: if not, increment
! record and cycling indices, flip time index and read a new portion of
! the data. Repeat until model time falls between the two data times.

      if (ntsms>1) then
        do while (sms_time(itsms)<time+0.5*dt  .and.
     &                                    ierr==nf_noerr)
          call advance_cycle (sms_cycle, ntsms, sms_ncycle,
     &                                       sms_rec, ierr)
          if (ierr==nf_noerr) then
            ierr=nf_get_var1_FTYPE (ncid, sms_tid, sms_rec, cff)
            if (ierr == nf_noerr) then
              itsms=3-itsms
              sms_time(itsms)=sms_cycle*sms_ncycle + cff*day2sec

              ierr=nf_fread (sustrg(START_2D_ARRAY,itsms), ncid,
     &                                    susid, sms_rec, u2dvar)
              if (ierr == nf_noerr) then
                ierr=nf_fread (svstrg(START_2D_ARRAY,itsms), ncid,
     &                                    svsid, sms_rec, v2dvar)
                if (ierr == nf_noerr) then
          mpi_nonexit_warn write(*,'(4x,A,3x,A,F12.4,2(1x,A,I4))')
     &                'get_smflux :: read surface momentum stress',
     &                'sms_time =', cff, 'rec =',sms_rec MYID
                else
                  write(*,3) 'svstr', sms_rec, nf_strerror(ierr)
                endif
              else
                write(*,3) 'sustr',  sms_rec,  nf_strerror(ierr)
              endif
            else
              write(*,3) 'sms_time', sms_rec,  nf_strerror(ierr)
            endif
          else
            write(*,'(/2(1x,A,I4)/12x,3A/12x,A,G12.4,A,G12.4/)')
     &            '### ERROR: get_smflux :: Requested time record ',
     &             sms_rec,    'exeeds the last record',     ntsms,
     &            'available in netCDF file ''',    frcname(1:lfrc),
     &            '''',  'tdays = ',   tdays,  '   while the last ',
     &            'available  sms_time =', sms_time(itsms)*sec2day
          endif
        enddo
      elseif (iic==ntstart) then
        sms_rec=1
        ierr=nf_fread (sustrg(START_2D_ARRAY,itsms), ncid,
     &                              susid, sms_rec, u2dvar)
        if (ierr == nf_noerr) then
          ierr=nf_fread (svstrg(START_2D_ARRAY,itsms), ncid,
     &                              svsid, sms_rec, v2dvar)
          if (ierr == nf_noerr) then
            mpi_nonexit_warn write(*,'(6x,2A,3x,1x,A,I4)')
     &         'get_smflux :: read time-invariant surface ',
     &                             'momentum stress' MYID
          else
            write(*,3) 'svstr', sms_rec, nf_strerror(ierr)
          endif
        else
          write(*,3) 'sustr',  sms_rec,  nf_strerror(ierr)
        endif
      endif
  3   format(/1x,'### ERROR: get_smflux :: Cannot read variable ''',
     &                                A, ''' for rec =', I4/12x,A/)
# ifdef VERBOSE
      write(*,*) 'leaving get_smflux' MYID
# endif
      end


      subroutine set_smflux_tile(istr,iend,jstr,jend, ierr)
      implicit none
      integer istr,iend,jstr,jend, ierr, it1,it2, i,j
      real cff, cff1,cff2
# ifdef WIND_STRESS_AMP
      real cff3,cff4
      integer it3,it4
# endif
# define SMFLUX_DATA
# include "param.h"
# ifdef CURVGRID
#  include "grid.h"
# endif
# include "forces.h"
# include "scalars.h"
!
# include "compute_extended_bounds.h"

!
! Set coefficients for interpolation. Check that for the next time
! step [when time=time+dt] both weights will still be positive, and
! if not, set synchro_flag to signal that new data should be read
! from an appropriate netCDF input file (master thread only).
! After that either load time-invariant data, or interpolate in time
! or complain about error and signal to quit, if interpolation is
! needed, but not possible.

      cff=time+0.5*dt

      it1=3-itsms
      it2=itsms
      cff1=sms_time(it2)-cff
      cff2=cff-sms_time(it1)

      if (cff1<dt .and. ntsms>1) synchro_flag=.true.

# ifdef WIND_STRESS_AMP
      it3=3-itatau
      it4=itatau
      cff3=atau_time(it4)-cff
      cff4=cff-atau_time(it3)

      if (cff3<dt .and. ntatau>1) synchro_flag=.true.
# endif

      if (ntsms==1) then                     ! Load time-invariant
        if (iic==ntstart) then               ! wind-stress components
          cff=1./rho0
          do j=jstrR,jendR
            do i=IstR,iendR
              sustr(i,j)=cff*sustrg(i,j,itsms)
              svstr(i,j)=cff*svstrg(i,j,itsms)
            enddo
          enddo
        endif                                  ! Interpolate in time
      else                                     ! with changing units
        cff=1./(rho0*(cff1+cff2))              ! from [Newton/m^2]
        cff1=cff1*cff                          ! to [m^2/s^2], i.e
        cff2=cff2*cff                          ! convert to kinematic
        do j=jstrR,jendR                       ! momentum flux.
          do i=istrR,iendR
            sustr(i,j)=cff1*sustrg(i,j,it1)+cff2*sustrg(i,j,it2)
            svstr(i,j)=cff1*svstrg(i,j,it1)+cff2*svstrg(i,j,it2)
          enddo
        enddo
        if (cff1<0. .or. cff2<0.) then
          if (ZEROTH_TILE) then
            write(*,'(/1x,3A/3(1x,A,F16.10)/)')      '### WARNING: ',
     &    'set_smflux_tile :: Current model time is outside bounds ',
     &    'of ''sms_time''.', 'sms_tstart =', sms_time(it1)*sec2day,
     &                        'tdays =',      tdays,
     &                        'sms_tend =',   sms_time(it2)*sec2day
          endif
          ierr=ierr+1
        endif
      endif


# ifdef WIND_STRESS_AMP
      if (ntatau==1) then                    ! Load time-invariant
        if (iic==ntstart) then               ! wind-stress amplitude
          cff=1./rho0
          do j=jstrR,jendR
            do i=IstR,iendR
              amptau(i,j)=cff*amptaug(i,j,itatau)
            enddo
          enddo
        endif
      else
        cff=1./(rho0*(cff3+cff4))              ! Interpolate in time.
        cff3=cff3*cff
        cff4=cff4*cff
        do j=jstrR,jendR
          do i=istrR,iendR
            amptau(i,j)=cff3*amptaug(i,j,it3)+cff4*amptaug(i,j,it4)
          enddo
        enddo
        if (cff3<0. .or. cff4<0.) then
          if (ZEROTH_TILE) then
            write(*,'(/1x,3A/3(1x,A,F16.10)/)')      '### WARNING: ',
     &    'set_smflux_tile :: Current model time is outside bounds ',
     &    'of ''atau_time''.','atau_tstart =',atau_time(it3)*sec2day,
     &                        'tdays =',      tdays,
     &                        'atau_tend =',  atau_time(it4)*sec2day
          endif
          ierr=ierr+1
        endif
      endif
# endif
      end
#else
      subroutine get_smflux_empty
      end
#endif /* !ANA_SMFLUX */

