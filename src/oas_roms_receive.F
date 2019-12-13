#include "cppdefs.h"
      subroutine oas_roms_receive(oas_step)                 
      !**** *oas_roms_receive*  - 
!
!     Purpose.
!     --------
!     Receive coupling fields from Atmospheric model and convert to ocean forcing
!
!**   Interface.
!     ----------
!       *CALL*  *oas_roms_receive*
!
!     Input:
!     -----
!
!     Output:
!     ------
!      
!
!     Method:
!     ------
!       
!
!     Externals:
!     ---------
!      oas_roms_vardef
!
!
!     Author:
!     -------
!       D. Byrne - ETHZ
!     
!     Modifications:
!     --------------
!       Matthieu Leclair - ETHZ
!

      USE oas_roms_exchange, ONLY: oas_roms_rcv, oas_roms_dbg_rcv

      USE oas_roms_data, ONLY: srcv, cpl_grd, k_rho, k_u, k_v,
     &                         oas_UST_U, oas_VST_U,
     &                         oas_UST_V, oas_VST_V,
     &                         oas_NHF, oas_SWR, oas_TEP,
     &                         alpha_rho, alpha_u, alpha_v,
     &                         u_cos_proj_u, v_cos_proj_u,
     &                         u_cos_proj_v, v_cos_proj_v

      implicit none
      
#include "param.h"
#include "scalars.h"
#include "forces.h"
#include "ocean3d.h"
      
      integer oas_dt, oas_step, jn, i, j
      real cff_wstr, cff_hflx, cff_fwflx
      real zflx_1, zflx_2

# ifdef DIURNAL_SRFLUX
      real phase, year2day, day2year, cff, cff1,cff2
      real cos_h, dec, cos_d, sin_d, tan_d, phi, h0, utc, utc_cor
      parameter (year2day=365.25, day2year=1.D0/year2day,
     &           utc_cor=1.D0/360.D0)
# endif

! Receive fields when necessary
! -----------------------------
      oas_dt=int((oas_step)*dt)
      call oas_roms_rcv(oas_dt)

! Set conversion coefficients
! ---------------------------    
      cff_wstr=1./rho0
      cff_hflx=1./(rho0*Cp)
      cff_fwflx=0.01/86400

! Momentum Flux
! -------------
      do j=cpl_grd(k_u)%jmin, cpl_grd(k_u)%jmax
        do i=cpl_grd(k_u)%imin, cpl_grd(k_u)%imax
          zflx_1 = u_cos_proj_u(i,j) * srcv(oas_UST_U)%pdata(i,j)
     &         + v_cos_proj_u(i,j) * srcv(oas_VST_U)%pdata(i,j)
          sustr(i,j) = (1.0-alpha_u(i,j)) * sustr(i,j)
     &                  + alpha_u(i,j) * cff_wstr * zflx_1
        enddo
      enddo
      
      do j=cpl_grd(k_v)%jmin, cpl_grd(k_v)%jmax
        do i=cpl_grd(k_v)%imin, cpl_grd(k_v)%imax
          zflx_1 = u_cos_proj_v(i,j) * srcv(oas_UST_V)%pdata(i,j)
     &         + v_cos_proj_v(i,j) * srcv(oas_VST_V)%pdata(i,j)
          svstr(i,j) = (1.0-alpha_v(i,j)) * svstr(i,j)
     &                  + alpha_v(i,j) * cff_wstr * zflx_1
        enddo
      enddo

! Solar Radiative Flux
! --------------------
# ifdef DIURNAL_SRFLUX
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
! Yusuke Uchiyama, 2009: UTC correction based on lonr is added.
!                        (ocean_)time should be in UTC.
!
#  define UTC_CORRECTION
!
#  ifndef UTC_CORRECTION
      cos_h=cos(2.*pi*(tdays+.5-int(tdays+.5)))
#  endif
      dec=-0.406*cos(deg2rad*(tdays-
     &               int(tdays*day2year)*year2day))
      cos_d=cos(dec)
      sin_d=sin(dec)
      tan_d=tan(dec)
# endif
      do j=cpl_grd(k_rho)%jmin, cpl_grd(k_rho)%jmax
         do i=cpl_grd(k_rho)%imin, cpl_grd(k_rho)%imax
          zflx_1=cff_hflx*srcv(oas_SWR)%pdata(i,j)
# ifdef DIURNAL_SRFLUX
#  ifdef UTC_CORRECTION
          utc=lonr(i,j)*utc_cor
          cos_h=cos(2.*pi*(tdays+.5-int(tdays+.5)+utc))
#  endif
          phi=latr(i,j)*deg2rad
          h0=acos(-tan(phi)*tan_d)
          cff1=cos_d*cos(phi)
          cff2=sin_d*sin(phi)
          cff=pi*(cos_h*cff1+cff2)/(sin(h0)*cff1+h0*cff2)
          zflx_1=max(0.,cff*zflx_1)
# endif
          srflx(i,j) = (1.0-alpha_rho(i,j)) * srflx(i,j)
     &                 + alpha_rho(i,j) * zflx_1

! Net Heat Flux
! -------------
          zflx_2 = cff_hflx * srcv(oas_NHF)%pdata(i,j) + zflx_1
!
! Restrict stflx to prevent surface temperature to go below -2
! degrees C.
!
#  if defined PACIFIC || defined STFLX_LIM
          zflx_2 = max( zflx_2,
#   ifdef LMD_KPP
     &                max(hbls(i,j,nstp), Hz(i,j,N))
#   else
     &                                    Hz(i,j,N)
#   endif
     &                  *(-2.D0-t(i,j,N,nrhs,itemp))/dt )
#  endif
          stflx(i,j,itemp) = (1.0-alpha_rho(i,j)) * stflx(i,j,itemp)
     &                       + alpha_rho(i,j) * zflx_2


! Fresh Water Flux
! ----------------
          zflx_1 = cff_fwflx * srcv(oas_TEP)%pdata(i,j)
     &                     * t(i,j,N,nrhs,isalt)
          stflx(i,j,isalt) = (1.0-alpha_rho(i,j)) * stflx(i,j,isalt)
     &                       + alpha_rho(i,j) * zflx_1
        enddo
      enddo

! Write Debug File
! ----------------
      call oas_roms_dbg_rcv(oas_step,
     &     "SUSTR", sustr           , k_u  ,
     &     "SVSTR", svstr           , k_v  ,
     &     "NHF"  , stflx(:,:,itemp), k_rho,
     &     "SWR"  , srflx           , k_rho,
     &     "FRESH", stflx(:,:,isalt), k_rho )

      end

      
