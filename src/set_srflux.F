#include "cppdefs.h"
#if defined SOLVE3D && !defined ANA_SRFLUX

c--#define DIURNAL_SRFLUX
c--#define ROBUST_DIURNAL_SRFLUX


      subroutine set_srflux_tile (istr,iend,jstr,jend, ierr)

! Set-up shortwave radiation flux for current tile.

      implicit none
      integer istr,iend,jstr,jend, ierr, i,j, it1,it2
      real cff, cff1, cff2, eps
# define SWRAD_DATA
# include "param.h"
# include "scalars.h"
# include "grid.h"
# include "forces.h"
# ifdef DIURNAL_SRFLUX
      real dec, utc, sin_d, cos_d
      real, parameter :: yrlen=360.0
      real ::  twopi=2.0*pi
# elif defined DIURNAL_SRFLUX_OLD
      real phase, cos_h, dec, cos_d, sin_d, tan_d, phi, h0, utc
      real, parameter :: year2day=365.25, day2year=1.D0/year2day,
     &                             utc_cor=1.D0/360.D0
# endif

# include "compute_extended_bounds.h"

c# ifdef DIURNAL_SRFLUX
c      cff=2.*dt*sec2day
c      phase=4.*(tdays-int(tdays))-2.
c      cff1=max(-1., min(1., phase-cff))
c      cff2=max(-1., min(1., phase+cff))
c      phase=(cff2-cff1)/cff + (sin(pi*cff2)-sin(pi*cff1))/(pi*cff)
c
ccc     if (ZEROTH_TILE) write(*,*) 'swrad modulation', tdays, phase
c# endif


! Set coefficients for interpolation. Check that for the next time
! step [when time=time+dt] both weights will still be positive, and
! if not, set synchro_flag to signal that new data should be read
! from an appropriate netCDF input file (master thread only).
! After that either load time-invariant data or interpolate in time.
! In either case apply factor 1./(rho0*Cp) to convert units from
! [Watts/m^2, data units] to [degC*m/sec, units of kinematic flux].
! Complain about error and signal to quit, if interpolation is
! needed, but not possible.

      it1=3-itsrf
      it2=itsrf
      cff=time+0.5*dt
      cff1=srf_time(it2)-cff
      cff2=cff-srf_time(it1)

      if (cff1 < dt .and. ntsrf > 1) synchro_flag=.true.

      if (ntsrf == 1) then                  ! Load time-invariant
        if (iic == ntstart) then            ! shortwave radiation
          cff=1/(rho0*Cp)                   ! flux
          do j=jstrR,jendR
            do i=istrR,iendR
              srflx(i,j)=cff*swradg(i,j,itsrf)
            enddo
          enddo
        endif
      else                                          ! Interpolate
        cff=1./(rho0*Cp*(cff1+cff2))                ! shortwave
        cff1=cff1*cff                               ! radiation
        cff2=cff2*cff                               ! flux in time.
        do j=jstrR,jendR
          do i=istrR,iendR
            srflx(i,j)=cff1*swradg(i,j,it1)+cff2*swradg(i,j,it2)
c# ifdef DIURNAL_SRFLUX
c            cff=stflx(i,j,itemp)-srflx(i,j)  ! subtract shortwave
c            srflx(i,j)=srflx(i,j)*phase      ! radiatin from the net,
c            stflx(i,j,itemp)=cff+srflx(i,j)  ! modulate and add back
c# endif
# if defined DAILYPAR_PHOTOINHIBITION || defined DAILYPAR_BEC
! save the daily average value for the evaluation of photoinhibited 
! nitrification
            srflx_dailyavg(i,j)=srflx(i,j)
# endif
          enddo
        enddo
        if (cff1 < 0. .or. cff2 < 0.) then
          if (ZEROTH_TILE) then
            write(*,'(/1x,3A/3(1x,A,F16.10)/)')       '### WARNING: ',
     &     'set_srflux_tile :: Current model time is outside bounds ',
     &     'of ''srf_time''.', 'srf_tstart =', srf_time(it1)*sec2day,
     &                         'tdays =',      tdays,
     &                         'srf_tend =',   srf_time(it2)*sec2day
          endif
          ierr=ierr+1
        endif
      endif

# ifdef DIURNAL_SRFLUX
! Estimate diurnal cycle of srflux from its daily mean
! and lat,lon,time. Assumes tdays is in UTC and starts at midnight on a Jan 1st.
! This code is based on the Rutgers ROMS version of DIURNAL_SRFLUX
!
! Notation: 
! yrlen     :: length of year (e.g., 360, 365, 365.25, or 365.2425)
! dec       :: solar declination (-23.439 degree at winter solstice,
!                            night of Dec. 21/22 or tdays = yrlen-9)
! utc       :: hour angle at 0 Merdian
! h         :: local hour angle
! cos_h     :: cos(h)
! theta_sol :: solar zenith angle
!
! Formula for theta_sol (Wikipedia)
!  cos(theta_sol) = sin(lat)sin(dec) + cos(lat)cos(dec)cos(h)
!                 =        cff1      +        cff2     cos_h
! 3 cases: 
!   1) |cff1| < |cff2|:          with sunrise
!   2) cff1 > 0 and cff1 > |cff2|:  permanent day
!   3) cff1 < 0 and -cff1 > |cff2|: permanent night => no modulation
!             Normalization for sunrise case:
!               = (cff1*arccos(-cff1/cff2)+sqrt(cff2**2-cff1**2))/pi

      dec=-23.439*deg2rad*cos(twopi*(tdays-9.)/yrlen) ! at noon
      sin_d=sin(dec)
      cos_d=cos(dec)
      utc=twopi*(mod(tdays,1.0)-0.5)                    ! h at 0 Meridian
      eps=0.000001 ! Don't modulate very small solar elevation

      do j=jstrR,jendR
        do i=istrR,iendR
          cff=latr(i,j)*deg2rad
          cff1=sin(cff)*sin_d
          cff2=cos(cff)*cos_d
          if ( abs(cff2) > abs(cff1)+eps ) then              ! Case 1: With sunrise
            stflx(i,j,itemp)=stflx(i,j,itemp)-srflx(i,j)     ! Remove mean solar from total
            cff=pi/(cff1*acos(-cff1/cff2)+sqrt(cff2**2-cff1**2)) ! 1/Normalisation
            srflx(i,j)=max( 0.0,                             ! Night
     &                     srflx(i,j)*cff*(cff1+cff2         ! Day
     &                               *cos(lonr(i,j)*deg2rad-utc)) ) ! cos_h
            stflx(i,j,itemp)=stflx(i,j,itemp)+srflx(i,j)     ! Add current solar back to total
          elseif (cff1 > eps .and. cff1 > abs(cff2) ) then   ! Case 2: Permanent day
            stflx(i,j,itemp)=stflx(i,j,itemp)-srflx(i,j)     ! Remove mean solar from total
            srflx(i,j)=srflx(i,j)*( 1.0+cff2/cff1
     &                               *cos(lonr(i,j)*deg2rad-utc) ) ! cos_h
            stflx(i,j,itemp)=stflx(i,j,itemp)+srflx(i,j)     ! Add current solar back to total
          endif                                              ! Else case 3: No modulation
        enddo
      enddo

# elif defined DIURNAL_SRFLUX_OLD
! mm: updated to work when now sunrise occurrs
!
! DIURNAL CYCLE - USED IN BOTH PHYSICAL AND ECOSYSTEM MODELS
! Patrick Marchesiello - 1999: Modulate average daily insolation
! to get diurnal cycle by:
!
!              cos(h)*cos(d)*cos(phi) + sin(d)*sin(phi)
!       pi * ---------------------------------------------
!             sin(h0)*cos(d)*cos(phi) + h0*sin(d)*sin(phi)
!
! where: h, d, phi -- are hour, declination, latitude angles;
!        h0 is hour angle at sunset and sunrise
!
! Yusuke Uchiyama, 2009: UTC correction based on lonr is added.
!                      [ocean_]time should be in UTC.
      eps = 0.001 ! Don't modulate for solar elevations smaller that this

# define UTC_CORRECTION

# ifndef UTC_CORRECTION
      cos_h=cos(2.*pi*(tdays+.5-int(tdays+.5)))
# endif
      dec=-0.406*cos(deg2rad*(tdays-int(tdays*day2year)*year2day))
      ! dec=-23.44*deg2rad*cos(twopi*(floor(tdays)-9.5)/yrlen) ! at noon
      cos_d=cos(dec)
      sin_d=sin(dec)
      tan_d=tan(dec)
      do j=jstrR,jendR
        do i=istrR,iendR
# ifdef UTC_CORRECTION
          utc=lonr(i,j)*utc_cor
          cos_h=cos(2.*pi*(tdays+.5-int(tdays+.5)+utc))
# endif
          phi=deg2rad*latr(i,j)
          cff1=cos_d*cos(phi) ; cff2=sin_d*sin(phi)
          if (abs(cff2)+eps < abs(cff1)) then
            h0=acos(-tan(phi)*tan_d)
            cff=pi*(cos_h*cff1+cff2)/(sin(h0)*cff1+h0*cff2)
            stflx(i,j,itemp)=stflx(i,j,itemp)-srflx(i,j)
            srflx(i,j)=max(0.,cff*srflx(i,j))
            stflx(i,j,itemp)=stflx(i,j,itemp)+srflx(i,j)
          elseif (cff2 > eps .and. cff2 > abs(cff1) )  then ! permanent day
            cff=( 1.0+cff1/cff2*cos_h )
            stflx(i,j,itemp)=stflx(i,j,itemp)-srflx(i,j)
            srflx(i,j)=cff*srflx(i,j)
            stflx(i,j,itemp)=stflx(i,j,itemp)+srflx(i,j)
          endif
        enddo
      enddo
# endif
      end
#else
      subroutine set_srflux_empty
      end
#endif /* SOLVE3D && !ANA_SRFLUX */

