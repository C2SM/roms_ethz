#include "cppdefs.h"
#if defined BIOLOGY_BEC2 && defined BEC2_INCLUDE_GHOSTCELLS && !defined ANA_SMFLUX
                                     ! Read surface momentum flux
      subroutine get_smflux_rho (ierr)   ! (wind stress) on rho grid at appropriate
      implicit none                  ! time from forcing netCDF file
# define SMFLUX_DATA
# include "param.h"
# ifdef MPI
#  include "mpif.h"
# endif
# include "scalars.h"
# include "forces.h"
# include "ncvars.h"
# include "netcdf.inc"
      real cff
      integer ncid,ifrc, ierr, lfrc,lvar, lenstr, nf_fread
      character*(max_name_size) frcname 
!
! Initialization: Check, whether forcing netCDF file is already
!===============  opened, an if not, open it. Find and save netCDF
! IDs for relevant variables. Determine whether there is cycling to
! reuse the input data and what is cycling period "sms_cycle", find
! initial cycling index "sms_ncycle" and record index "sms_rec".
! Set initial value for time index "itsms" and set both time record
! bounds to large negative artificial values to trigger the logic
! in reading part below.
!
      ifrc=max(sms_file_id,1)
 100  ncid=ncidfrc(ifrc)
      frcname=frcfile(ifrc)
      lfrc=lenstr(frcname)
      ierr=nf_noerr

      if (iic.eq.ntstart) then
        if (ncid.eq.-1) then
          ierr=nf_open (frcname(1:lfrc), nf_nowrite, ncid)
          if (ierr.eq.nf_noerr) then
            ncidfrc(ifrc)=ncid
          else
            write(stdout,'(/1x,4A/)') '### ERROR in get_smflux_rho: ',
     &      'can not open netCDF file ''', frcname(1:lfrc), '''.'
          endif
        endif
        if (ierr .eq. nf_noerr) then
          lvar=lenstr(vname(1,indxSUSTR_rho))
          ierr=nf_inq_varid (ncid, vname(1,indxSUSTR_rho)(1:lvar), susid_rho)
          if (ierr .eq. nf_noerr) then
            sms_file_id=ifrc
CR            MPI_master_only write(*,*) 'Found ''', 
CR     &        vname(1,indxSUSTR)(1:lvar), 
CR     &        ''' in ''', frcname(1:lfrc), ''', ifrc =', ifrc
            lvar=lenstr(vname(1,indxSVSTR_rho))
            ierr=nf_inq_varid (ncid,vname(1,indxSVSTR_rho)(1:lvar),svsid_rho)
            if (ierr .eq. nf_noerr) then
              ierr=nf_inq_varid (ncid, 'sms_time', sms_tid)
              if (ierr .eq. nf_noerr) then
                call set_cycle (ncid, sms_tid, ntsms, sms_cycle,
     &                                   sms_ncycle, sms_rec, ierr)
                itsms=1
                sms_time(1)=-1.E+20
                sms_time(2)=-1.E+20
              else
                write(stdout,1) 'sms_time', frcname(1:lfrc)
              endif
            else
              write(stdout,1) vname(1,indxSVSTR_rho)(1:lvar),
     &                                   frcname(1:lfrc)
            endif
          else
            write(stdout,1) vname(1,indxSUSTR_rho)(1:lvar),
     &                                 frcname(1:lfrc)
            if (ifrc .lt. max_frc) then
              ifrc=ifrc+1
              goto 100
            endif 
          endif
        endif
      endif
  1   format(/1x,'WARNING: get_smflux_rho cannot find variable ''',
     &              A, ''''/7x, 'in netCDF file ''', A, '''.')
!
! Read data from the file:  Check if model time is bounded by past
!===== ==== ==== === =====  and future data times: if not, increment
! record and cycling indices, flip time index and read a new portion
! of data. Repeat until model time falls between the two data times.
!
      do while (sms_time(itsms).lt.time+0.5*dt .and.ierr.eq.nf_noerr)
        call advance_cycle (sms_cycle, ntsms, sms_ncycle,
     &                                        sms_rec, ierr)
        if (ierr.eq.nf_noerr) then
          ierr=nf_get_var1_FTYPE (ncid, sms_tid, sms_rec, cff)
          if (ierr .eq. nf_noerr) then
            itsms=min(3-itsms,ntsms)
            sms_time(itsms)=sms_cycle*sms_ncycle + cff*day2sec
            ierr=nf_fread (sustrg_rho(START_2D_ARRAY,itsms), ncid,
     &                                      susid_rho, sms_rec, u2dvar)
            if (ierr .eq. nf_noerr) then
              ierr=nf_fread (svstrg_rho(START_2D_ARRAY,itsms), ncid,
     &                                      svsid_rho, sms_rec, v2dvar)
              if (ierr .eq. nf_noerr) then
                MPI_master_only write(stdout,
     &          '(6x,A,3x,A,F12.4,2(1x,A,I4))')
     &          'get_smflux_rho -- read surface momentum stress',
     &          'sms_time =', cff, 'rec =',sms_rec MYID
                 if (ntsms.eq.1) return
              else
                write(stdout,2) 'svstr_rho', sms_rec
              endif
            else
              write(stdout,2) 'sustr_rho', sms_rec
            endif
          else
            write(stdout,2) 'sms_time', sms_rec
          endif
        else
          write(stdout,'(/1x,A,I4,1x,A,I4/7x,4A/7x,2(A,G12.4)/)')
     &      'ERROR in get_smflux_rho: requested time record ', sms_rec,
     &      'exeeds the last record', ntsms,  'available in netCDF ',
     &      'file ''',   frcname(1:lfrc),   '''',  'tdays = ', tdays,
     &      '  but the last available  sms_time =',
     &                                       sms_time(itsms)*sec2day
        endif
      enddo
  2   format(/1x,'ERROR in get_smflux_rho: cannot read variable ''',
     &                                 A, ''' for record ', I4/)
      return
      end
 
 
 
 
      subroutine set_smflux_rho_tile (istr,iend,jstr,jend,ierr)
      implicit none
      integer istr,iend,jstr,jend, ierr, i,j, it1,it2
      real cff, cff1,cff2
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
!
      it1=3-itsms
      it2=itsms
      cff1=sms_time(it2)-time
      cff2=time-sms_time(it1)
      if (cff1.lt.dt) synchro_flag=.true.
 
      if (ntsms.eq.1) then                ! Load time-invariant
        if (iic.eq.ntstart) then          ! wind stress components
          cff=1./rho0
          do j=jstrR,jendR
            do i=IstR,iendR
              sustr_rho(i,j)=cff*sustrg_rho(i,j,itsms)
              svstr_rho(i,j)=cff*svstrg_rho(i,j,itsms)
            enddo                                ! Interpolate wind
          enddo                                  ! stress in time,
        endif                                    ! changing its units
      elseif (cff1.ge.0. .and. cff2.ge.0.) then  ! from [Newton/m^2]
        cff=1./(rho0*(cff1+cff2))                ! to [m^2/s^2], i.e.
        cff1=cff1*cff                            ! converting it into
        cff2=cff2*cff                            ! kinematic momentum
        do j=jstrR,jendR                         ! flux.
          do i=istrR,iendR
            sustr_rho(i,j)=cff1*sustrg_rho(i,j,it1)+cff2*sustrg_rho(i,j,it2)
            svstr_rho(i,j)=cff1*svstrg_rho(i,j,it1)+cff2*svstrg_rho(i,j,it2)
          enddo
        enddo
      elseif (ZEROTH_TILE) then
        write(stdout,'(/1x,2A/3(1x,A,F16.10)/)')
     &     'SET_SMFLUX_RHO_TILE - current model time is outside bounds ',
     &     'of ''sms_time''.',  'SMS_TSTART=', sms_time(it1)*sec2day,
     &                          'TDAYS=',      tdays,
     &                          'SMS_TEND=',   sms_time(it2)*sec2day
        ierr=ierr+1
      endif
      return
      end
#else
      subroutine get_smflux_rho_empty
      end
#endif /* !ANA_SMFLUX && BIOLOGY_BEC2 && BEC2_INCLUDE_GHOSTCELLS */
 
