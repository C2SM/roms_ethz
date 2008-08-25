#include "cppdefs.h"
#if defined SOLVE3D && !defined ANA_SRFLUX
 
                                      ! Read shortwave radiation flux
      subroutine get_srflux (ierr)    ! at the appropriate time from
      implicit none                   ! forcing netCDF file.
# define SRFLUX_DATA
# include "param.h"
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
! IDs for relevant variables. After that determine whether there is
! cycling to reuse the input data, find cycling period "srf_cycle",
! set initial cycling index "srf_ncycle" and record index "srf_rec".
! Set initial value for time index "itsrf" and both time record
! bounds to large negative artificial values to trigger logic in
! reading part below.
!
      ifrc=max(srf_file_id,1)
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
            write(stdout,'(/1x,4A/)') '### ERROR  in getrflux: can ',
     &             'not open netCDF file ''', frcname(1:lfrc), '''.'
          endif
        endif
        if (ierr .eq. nf_noerr) then
          lvar=lenstr(vname(1,indxSWRad))
          ierr=nf_inq_varid (ncid, vname(1,indxSWRad), srf_id)
          if (ierr .eq. nf_noerr) then
            srf_file_id=ifrc
            write(*,*) 'Found ''', vname(1,indxSWRad)(1:lvar),
     &           ''' in ''', frcname(1:lfrc), ''', ifrc =', ifrc

            ierr=nf_inq_varid (ncid, 'srf_time', srf_tid)
            if (ierr .eq. nf_noerr) then
              call set_cycle (ncid, srf_tid, ntsrf, srf_cycle,
     &                                 srf_ncycle, srf_rec, ierr)
              itsrf=1
              srf_time(1)=-1.E+20
              srf_time(2)=-1.E+20
            else
              write(stdout,1) 'srf_time', frcname(1:lfrc)
            endif
          else
            write(stdout,1) vname(1,indxSWRad)(1:lvar),
     &                                 frcname(1:lfrc)
            if (ifrc .lt. max_frc) then
              ifrc=ifrc+1
              goto 100
            endif
          endif
        endif
      endif
  1   format(/1x,'WARNING: get_srflux cannot find variable ''',
     &       A, ''''/7x, 'in forcing netCDF file ''', A, '''.')
 
!
! Read data from the forcing file:  Get out, if model time is already
!===== ==== ==== === ======= =====  within the interval set by past
! and future data times. Otherwise increment record, flip the time
! index, and read a new portion of data. Repeat it until model time
! is between the two times from data.
!
      do while (srf_time(itsrf).lt.time+0.5*dt .and.ierr.eq.nf_noerr)
        call advance_cycle (srf_cycle, ntsrf, srf_ncycle, srf_rec,
     &                                                       ierr)
        if (ierr .eq. nf_noerr) then
          ierr=nf_get_var1_FTYPE(ncid, srf_tid, srf_rec, cff)
          if (ierr .eq. nf_noerr) then
            itsrf=min(3-itsrf,ntsrf)
            srf_time(itsrf)=cff*day2sec + srf_cycle*srf_ncycle
            ierr=nf_fread (srflxg(START_2D_ARRAY,itsrf), ncid,
     &                                   srf_id, srf_rec, r2dvar)
            if (ierr .eq. nf_noerr) then
              write(stdout,'(6x,2A,F12.4,2(1x,A,I4))') 'get_srflux ',
     &                '-- read solar shortwave radiation srf_time =',
     &                 cff, 'rec =', srf_rec MYID
              if (ntsrf.eq.1) return
            else
              write(stdout,2) 'srflux', srf_rec
            endif
          else
            write(stdout,2) 'srf_time', srf_rec
          endif
        else
          write(stdout,'(/1x,A,I4,1x,A,I4/7x,4A/7x,2(A,G12.4)/)')
     &    'ERROR in get_srflux: requested time record ',   srf_rec,
     &    'exeeds the last record ',  ntsrf, 'available in forcing ',
     &    'netCDF file ''',   frcname(1:lfrc),   '''',    'tdays = ',
     &     tdays,             '  but the last available srf_time = ',
     &                                     srf_time(itsrf)*sec2day
        endif
      enddo
  2   format(/1x,'ERROR in get_srflux: cannot read variable ''',
     &                                 A, ''' for record ', I4/)
      return
      end
 
 
 
      subroutine set_srflux_tile (istr,iend,jstr,jend, ierr)
!
! Set-up shortwave radiation flux for current tile.
!
      implicit none
      integer istr,iend,jstr,jend, ierr, i,j, it1,it2
      real cff, cff1, cff2
# ifdef DIURNAL_SRFLUX
      real phase
!cc      real cos_h, dec, cos_d, sin_d, tan_d, phi, h0
#endif
# define SRFLUX_DATA
# include "param.h"
# include "scalars.h"
# include "grid.h"
# include "forces.h"
!
# include "compute_extended_bounds.h"
!

# ifdef DIURNAL_SRFLUX
      cff=2.*dt*sec2day
      phase=4.*(tdays-int(tdays))-2.
      cff1=max(-1., min(1., phase-cff))
      cff2=max(-1., min(1., phase+cff))
      phase=(cff2-cff1)/cff + (sin(pi*cff2)-sin(pi*cff1))/(pi*cff)

c     if (ZEROTH_TILE) write(*,*) 'swrad modulation', tdays, phase
# endif


! Set coefficients for interpolation. Check that for the next time
! step [when time=time+dt] both weights will still be positive, and
! if not, set synchro_flag to signal that new data should be read
! from an appropriate netCDF input file (master thread only).
! After that either load time-invariant data or interpolate in time.
! In either case apply factor 1./(rho0*Cp) to convert units from
! [Watts/m^2, data units] to [degC*m/sec, units of kinematic flux].
! Complain about error and signal to quit, if interpolation is
! needed, but not possible.
!
      it1=3-itsrf
      it2=itsrf
      cff1=srf_time(it2)-time
      cff2=time-srf_time(it1)
      if (cff1.lt.dt) synchro_flag=.true.
 
      if (ntsrf.eq.1) then                  ! Load time-invariant
        if (iic.eq.ntstart) then            ! shortwave radiation
          cff=1/(rho0*Cp)                   ! flux
          do j=jstrR,jendR
            do i=istrR,iendR
              srflx(i,j)=cff*srflxg(i,j,itsrf)
            enddo
          enddo
        endif
      elseif (cff1.ge.0. .and. cff2.ge.0.) then     ! Interpolate
        cff=1./(rho0*Cp*(cff1+cff2))                ! shortwave
        cff1=cff1*cff                               ! radiation
        cff2=cff2*cff                               ! flux in time.
        do j=jstrR,jendR
          do i=istrR,iendR
            srflx(i,j)=cff1*srflxg(i,j,it1)+cff2*srflxg(i,j,it2)
c : zl 
# ifdef DIURNAL_SRFLUX
            cff=stflx(i,j,itemp)-srflx(i,j)  ! subtract shortwave
                                             ! radiatin from the net, 
# if defined DAILYPAR_PHOTOINHIBITION || defined DAILYPAR_BEC
! save the daily average value for the evaluation of photoinhibited 
! nitrification
            srflx_dailyavg(i,j)=srflx(i,j)
# endif 
            srflx(i,j)=srflx(i,j)*phase      ! modulate 
            stflx(i,j,itemp)=cff+srflx(i,j)  ! and add back
# endif
          enddo
        enddo
      elseif (ZEROTH_TILE) then
        write(stdout,'(/1x,2A/3(1x,A,F16.10)/)')
     &     'SET_SRFLUX_TILE - current model time is outside bounds ',
     &     'of ''srf_time''.',  'SRF_TSTART=', srf_time(it1)*sec2day,
     &                          'TDAYS=',      tdays,
     &                          'SRF_TEND=',   srf_time(it2)*sec2day
        ierr=ierr+1
      endif
 
# ifdef XXXXX 
!
! DIURNAL CYCLE - USED IN BOTH PHYSICAL AND ECOSYSTEM MODELS
! Patrick Marchesiello - 1999: Modulate average dayly insolation
! to get diurnal cycle by:
!
!              cos(h)*cos(d)*cos(phi) + sin(d)*sin(phi)
!       pi * ---------------------------------------------
!             sin(h0)*cos(d)*cos(phi) + h0*sin(d)sin(phi)
!
! where: h, d, phi -- are hour, declination, latitude angles;
!        h0 is hour angle at sunset and sunrise
!
      cos_h=cos(2.*pi*(tdays-int(tdays)))
      dec=-0.406*cos(deg2rad*(tdays-int(tdays/360.)*360.))
      cos_d=cos(dec)
      sin_d=sin(dec)
      tan_d=tan(dec)
      do j=jstrR,jendR
        do i=istrR,iendR
          phi=latr(i,j)*deg2rad
          h0=acos(-tan(phi)*tan_d)
          cff1=cos_d*cos(phi)
          cff2=sin_d*sin(phi)
#if defined DAILYPAR_PHOTOINHIBITION || defined DAILYPAR_BEC
! save the daily average value for the biogeochemical model
          srflx_dailyavg(i,j)=srflx(i,j)
#endif /* DAILYPAR_PHOTOINHIBITION || DAILYPAR_BEC */
          srflx(i,j)=srflx(i,j)*pi*(cos_h*cff1+cff2)
     &                        /(sin(h0)*cff1+h0*cff2)
          srflx(i,j)=max(0.,srflx(i,j))
        enddo
      enddo
# endif
      return
      end
#else
      subroutine get_srflux_empty
      end
#endif /* SOLVE3D && !ANA_SRFLUX */
 
