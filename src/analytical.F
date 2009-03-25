#include "cppdefs.h"
!
! ANALYTICAL PACKAGE:
!=========== ========
!
! This package is used to provide various analytical fields to the
! model when appropriate.
!
! Routines:
!
! ana_bsedim_tile   Analytical bottom sediment grain size
!                          and density.
! ana_meanRHO_tile  Analytical mean density anomaly.
! ana_smflux_tile   Analytical kinematic surface momentum flux
!                          (wind stress).
! ana_srflux_tile   Analytical kinematic surface shortwave
!                          radiation.
! ana_ssh_tile      Analytical sea surface height climatology.
! ana_sst_tile      Analytical sea surface temperature and dQdSST
!                         which are used during heat flux correction.
! ana_stflux_tile   Analytical kinematic surface flux of tracer type
!                          variables.
! ana_tclima_tile   Analytical tracer climatology fields.
! ana_uclima_tile   Analytical tracer climatology fields.
! ana_wwave_tile    Analytical wind induced wave amplitude,
!                         direction and period.
!-------------------------------------------------------------------
!
#if defined ANA_BSEDIM && defined SG_BBL96
      subroutine ana_bsedim_tile (Istr,Iend,Jstr,Jend)
!
! Set initial bottom sediment grain diameter size [m]  and density
! used in the bottom boundary formulation [kg/m^3].
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j
# include "param.h"
# include "bblm.h"
# include "grid.h"
# include "scalars.h"
!
# include "compute_auxiliary_bounds.h"
!
      do j=JstrR,JendR
        do i=IstrR,IendR
          Ssize(i,j)=???
          Sdens(i,j)=???
        enddo
      enddo
      return
      end
#endif /* ANA_BSEDIM && SG_BBL96 */







#if defined SOLVE3D && defined ANA_MEANRHO
      subroutine ana_meanRHO_tile (Istr,Iend,Jstr,Jend)
!
!--------------------------------------------------------------------
!  This subroutine sets mean density anomaly [kg/m^3] using an
!  analytical expression.
!--------------------------------------------------------------------
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k
      real cff1, cff2 , cff3, cff4
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
!
# include "compute_auxiliary_bounds.h"
!
!  Set mean density anomaly (kg/m^3) at horizontal and vertical
!  RHO-points.
!
# if defined BASIN
c     cff1=(44.690/39.382)**2
c     cff2=cff1*(rho0*800./g)*(5.0e-5/((42.689/44.690)**2))
c-song:
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
c           rhobar(i,j,k)=R0-cff2*exp(z_r(i,j,k)/800.)*
            rhobar(i,j,k)=R0-
     &  (44.690/39.382)**2
     &  *(rho0*800./g)*(5.0e-5/((42.689/44.690)**2))
     &                           *exp(z_r(i,j,k)/800.)*
     &                    (0.6-0.4*tanh(z_r(i,j,k)/800.))
           enddo
        enddo
      enddo
# elif defined CANYON_A || defined GRAV_ADJ || defined OVERFLOW
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            rhobar(i,j,k)=R0
          enddo
        enddo
      enddo
# elif defined CANYON_B
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            rhobar(i,j,k)=R0-3.488*exp(z_w(i,j,k)/800.)*
     &                  (1.-(2./3.)*tanh(z_r(i,j,k)/800.))
          enddo
        enddo
      enddo
# elif defined NJ_BIGHT
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            if (z_r(i,j,k).ge.-15.0 ) then
              cff1=2.049264257728403e+01-z_r(i,j,k)*(
     &               2.640850848793918e-01+z_r(i,j,k)*(
     &               2.751125328535212e-01+z_r(i,j,k)*(
     &               9.207489761648872e-02+z_r(i,j,k)*(
     &               1.449075725742839e-02+z_r(i,j,k)*(
     &               1.078215685912076e-03+z_r(i,j,k)*(
     &               3.240318053903974e-05+
     &                1.262826857690271e-07*z_r(i,j,k)
     &                                           ))))))
              cff2=3.066489149193135e+01-z_r(i,j,k)*(
     &               1.476725262946735e-01+z_r(i,j,k)*(
     &               1.126455760313399e-01+z_r(i,j,k)*(
     &               3.900923281871022e-02+z_r(i,j,k)*(
     &               6.939014937447098e-03+z_r(i,j,k)*(
     &               6.604436696792939e-04+z_r(i,j,k)*(
     &               3.191792361954220e-05+
     &               6.177352634409320e-07*z_r(i,j,k)
     &                                           ))))))
            else
               cff1=14.6+6.7 *tanh(1.1*z_r(i,j,k)+15.9)
               cff2=31.3-0.55*tanh(1.1*z_r(i,j,k)+15.9)
            endif
            rhobar(i,j,k)=R0+Tcoef*cff1+Scoef*cff2
          enddo
        enddo
      enddo
# elif defined SEAMOUNT
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            rhobar(i,j,k)=28.-2.*exp(z_r(i,j,k)/1000.)
          enddo
        enddo
      enddo
# elif defined SHELFRONT
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            rhobar(i,j,k)=R0
          enddo
        enddo
      enddo
# elif defined UPWELLING
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            rhobar(i,j,k)=26.459472-2.24*exp(z_r(i,j,k)/50.)
          enddo
        enddo
      enddo
# elif defined JET
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
           cff1=TL1+
     &          TL2*z_r(i,j,k)+
     &          TL3*exp(z_r(i,j,k)/ZL1)
           cff2=TC1+
     &          TC2*z_r(i,j,k)+
     &          TC3*exp((z_r(i,j,k)-ZC1)/ZC2)
           cff3=YC1+YC2*
     &          tanh(YC3*xr(i,j)+YC4)
!           cff4=0.5*(
!     &          cff2*(1+tanh((cff3-yr(i,j))
!     &           /LCUR))+
!     &          cff1*(1+tanh((yr(i,j)-cff3)
!     &           /LCUR)))
           cff4=cff1
            rhobar(i,j,k)=R0+Tcoef*cff4
          enddo
        enddo
      enddo
# else
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            rhobar(i,j,k)=???
          enddo
        enddo
      enddo
# endif
# ifdef MASKING
!
!  Apply Land/Sea mask.
!
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            rhobar(i,j,k)=rhobar(i,j,k)*rmask(i,j)
          enddo
        enddo
      enddo
# endif
      return
      end
#endif /* ANA_MEANRHO && SOLVE3D */
 
 
#ifdef ANA_SMFLUX
      subroutine ana_smflux_tile (Istr,Iend,Jstr,Jend)
!
! Sets kinematic surface momentum flux (wind stress) "sustr"
! and "svstr"  [m^2/s^2] using an analytical expression.
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j
# include "param.h"
# include "grid.h"
# include "forces.h"
# include "scalars.h"
      real Ewind, Nwind, dircoef, windamp
      data Ewind, Nwind, dircoef /0., 0., 0./
!
# include "compute_auxiliary_bounds.h"
!
                          ! Set kinematic surface momentum flux
      do j=JstrR,JendR    ! (wind stress) components in XI- and ETA-
        do i=IstrR,IendR  ! directions [m^2/s^2] at horizontal U-
          sustr(i,j)=0.   ! and V-points respectively.  For the sake 
          svstr(i,j)=0.   ! of code compactness, both components are
        enddo             ! set to zero at first (default values)
      enddo               ! and then overwritten, when appropriate. 

# ifdef BASIN
      cff1=0.0001 * 0.5*(1.+tanh((time-6.*86400.)/(3.*86400.)))
      cff2=2.*pi/el
      do j=JstrR,JendR
        do i=Istr,IendR
          sustr(i,j)=-cff1*cos(cff2*yr(i,j))
        enddo
      enddo
# elif defined CANYON_A || defined CANYON_B
      do j=JstrR,JendR
        do i=Istr,IendR
          sustr(i,j)=0.0001*0.5*sin(2.*pi*tdays/10.)*
     &               (1.-tanh((yr(i,j)-0.5*el)/10000.))
        enddo
      enddo
# elif defined NJ_BIGHT
      windamp=0.086824313
      dircoef=0.5714286
      if (tdays.le.0.5) then
        Ewind=windamp*dircoef*sin(pi*tdays)/rho0
        Nwind=windamp*sin(pi*tdays)/rho0
      else
        Ewind=windamp*dircoef/rho0
        Nwind=windamp/rho0
      endif
      do j=JstrR,JendR
        do i=Istr,IendR
          cff1=0.5*(angler(i-1,j)+angler(i,j))
          sustr(i,j)=Ewind*cos(cff1)+Nwind*sin(cff1)
        enddo
      enddo
      do j=Jstr,JendR
        do i=IstrR,IendR
          cff1=0.5*(angler(i,j)+angler(i,j-1))
          svstr(i,j)=-Ewind*sin(cff1)+Nwind*cos(cff1)
        enddo
      enddo

# elif defined TASMAN_SEA
      windamp=0.5*(1.+tanh((time-6.*86400.)/(3.*86400.)))
      do j=JstrR,JendR
        do i=Istr,IendR
          cff1=yr(i,j)/el
          sustr(i,j)=windamp*cff1*(1.-cff1)*( 1.7580e-3 +cff1*(
     &                       -2.7907e-3+cff1*( -4.0764e-3+cff1*(
     &                                  9.1903e-3-cff1*4.7915e-3
     &                                                      ))))
        enddo
      enddo

# elif defined UPWELLING
      if (tdays.le.2.) then
        windamp=-0.1*sin(pi*tdays/4.)/rho0
      else
        windamp=-0.1/rho0
      endif
c??
c??
c      windamp=0.
c??
c??
      do j=JstrR,JendR
        do i=Istr,IendR
          sustr(i,j)=windamp
        enddo
      enddo
# elif defined USWEST
      do j=JstrR,JendR
        do i=Istr,IendR
c         cff1=(latr(i,j)-latr(Lm/2,Mm/2))/20.
c         sustr(i,j)=1.d-4.*cff1
c         sustr(i,j)=-1.d-4
          sustr(i,j)=0.
        enddo
      enddo
      do j=Jstr,JendR
        do i=IstrR,IendR
c          svstr(i,j)=-1.0d-4
          svstr(i,j)=0.
        enddo
      enddo
# endif
      return
      end
#endif /* ANA_SMFLUX */

#if defined ANA_SRFLUX && defined SOLVE3D

      subroutine ana_srflux_tile (Istr,Iend,Jstr,Jend)
!
! Set kinematic surface solar shortwave radiation flux "srflx"
! [degC m/s] using an analytical expression.
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j
# include "param.h"
# include "grid.h"
# include "forces.h"
# include "scalars.h"
!
# include "compute_extended_bounds.h"
!
      do j=JstrR,JendR
        do i=IstrR,IendR
          srflx(i,j)=0.
        enddo
      enddo
      return
      end
#endif /* ANA_SRFLUX */


#if defined ANA_SSH
      subroutine ana_ssh (tile)                ! Set analytical
      implicit none                            ! sea-surface height
      integer tile                             ! climatology [m]
#include "param.h"
#include "compute_tile_bounds.h"
      call ana_ssh_tile   (Istr,Iend,Jstr,Jend)
      return
      end

      subroutine ana_ssh_tile (Istr,Iend,Jstr,Jend)
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j
#ifdef JET
      real TL0, TC0, cff1, cff2, cff3, cff4
#endif
# include "param.h"
# include "grid.h"
# include "climat.h"
# include "scalars.h"
!
# include "compute_auxiliary_bounds.h"
!
!  Set sea surface height (meters).
!
# if defined JET
      TL0=-TL1 - 0.5*TL2*ZREF**2 - ZL1*TL3*exp(ZREF/ZL1)
      TC0=-TC1 - 0.5*TC2*ZREF**2 - ZC2*TC3*exp((ZREF-ZC1)/ZC2)
      do j=JstrR,JendR
        do i=IstrR,IendR
         cff1=YC1+YC2*tanh(YC3*xr(i,j)+YC4)
         cff2=TL0+ZL1*TL3
         cff3=TC0+ZC2*TC3*exp(-ZC1/ZC2)
         cff4=-0.5*g*Tcoef*(cff2+cff3)
         ssh(i,j)=cff4/(rho0*g)
                +Tcoef*cff2*(1+tanh((cff1-yr(i,j))/LCUR))/(2*rho0)
                +Tcoef*cff3*(1+tanh((yr(i,j)-cff1)/LCUR))/(2*rho0)
        enddo
      enddo
# else
      do j=JstrR,JendR
        do i=IstrR,IendR
          ssh(i,j)=???
        enddo
      enddo
#endif /* JET */
      return
      end
#endif
#ifdef SOLVE3D
# if defined ANA_STFLUX || defined ANA_SSFLUX
      subroutine ana_stflux_tile (Istr,Iend,Jstr,Jend, itrc)
!
! Set kinematic surface flux of tracer type variables "stflx" (tracer
! units m/s) using analytical expressions.
!
      implicit none
      integer Istr,Iend,Jstr,Jend, itrc, i,j
#  include "param.h"
#  include "grid.h"
#  include "forces.h"
#  include "scalars.h"
!
#  include "compute_auxiliary_bounds.h"
!
      if (itrc.eq.itemp) then
!
! Set kinematic surface heat flux [degC m/s] at horizontal
! RHO-points.
!
        do j=JstrR,JendR
          do i=IstrR,IendR
            stflx(i,j,itemp)=0.
          enddo
        enddo
 
#  ifdef SALINITY
      elseif (itrc.eq.isalt) then
!
! Set kinematic surface freshwater flux (m/s) at horizontal
! RHO-points, scaling by surface salinity is done in STEP3D.
!
        do j=JstrR,JendR
          do i=IstrR,IendR
            stflx(i,j,isalt)=0.
          enddo
        enddo
#  endif
      else
!
!  Set kinematic surface flux of additional tracers, if any.
!
      endif
      return
      end
# endif /* ANA_STFLUX || ANA_SSFLUX */



# if defined ANA_TCLIMA && defined TCLIMATOLOGY
      subroutine ana_tclima (tile)
      implicit none
      integer tile
# include "param.h"
# include "compute_tile_bounds.h"
      call ana_tclima_tile   (Istr,Iend,Jstr,Jend)
      return
      end

      subroutine ana_tclima_tile (Istr,Iend,Jstr,Jend)
!
! Set analytical tracer climatology fields.
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k
#  ifdef JET
      real cff1, cff2, cff3
#  endif
#  include "param.h"
#  include "climat.h"
#  include "grid.h"
#  include "ocean3d.h"
#  include "scalars.h"
!
#  include "compute_auxiliary_bounds.h"
!
#  if defined NJ_BIGHT
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            if (z_r(i,j,k).ge.-15.0) then
              tclm(i,j,k,itemp)=2.049264257728403e+01-z_r(i,j,k)*(
     &                          2.640850848793918e-01+z_r(i,j,k)*(
     &                          2.751125328535212e-01+z_r(i,j,k)*(
     &                          9.207489761648872e-02+z_r(i,j,k)*(
     &                          1.449075725742839e-02+z_r(i,j,k)*(
     &                          1.078215685912076e-03+z_r(i,j,k)*(
     &                          3.240318053903974e-05+
     &                          1.262826857690271e-07*z_r(i,j,k)
     &                                                      ))))))
              tclm(i,j,k,isalt)=3.066489149193135e+01-z_r(i,j,k)*(
     &                          1.476725262946735e-01+z_r(i,j,k)*(
     &                          1.126455760313399e-01+z_r(i,j,k)*(
     &                          3.900923281871022e-02+z_r(i,j,k)*(
     &                          6.939014937447098e-03+z_r(i,j,k)*(
     &                          6.604436696792939e-04+z_r(i,j,k)*(
     &                          3.191792361954220e-05+
     &                          6.177352634409320e-07*z_r(i,j,k)
     &                                                       ))))))
            else
               tclm(i,j,k,itemp)=14.6+6.7 *tanh(1.1*z_r(i,j,k)+15.9)
               tclm(i,j,k,isalt)=31.3-0.55*tanh(1.1*z_r(i,j,k)+15.9)
            endif
          enddo
        enddo
      enddo
#  elif defined JET
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
           cff1=TL1+ TL2*z_r(i,j,k) +TL3*exp(z_r(i,j,k)/ZL1)
           cff2=TC1+ TC2*z_r(i,j,k) +TC3*exp((z_r(i,j,k)-ZC1)/ZC2)
           cff3=YC1+YC2*tanh(YC3*xr(i,j)+YC4)
           tclm(i,j,k,itemp)=0.5*(cff2*(1+tanh((cff3-yr(i,j))/LCUR))
     &                           +cff1*(1+tanh((yr(i,j)-cff3)/LCUR)))
          enddo
        enddo
      enddo

#  elif defined ISWAKE
!
! There is no code here because for this case climatological data
! is saved from the initial conditions and remained unchanged after
! that. 
!
#  else
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            tclm(i,j,k,itemp)=???
#   ifdef SALINITY
            tclm(i,j,k,isalt)=???
#   endif
          enddo
        enddo
      enddo
#  endif
      return
      end
# endif /* ANA_TCLIMA && TCLIMATOLOGY */
#endif /* SOLVE3D */


#if defined ANA_UCLIMA && defined UCLIMATOLOGY
      subroutine ana_uclima (tile)
      implicit none
      integer tile
#include "param.h"
#include "private_scratch.h"
#include "compute_tile_bounds.h"
      call ana_uclima_tile   (Istr,Iend,Jstr,Jend)!,A2d(1,1),A2d(1,2))
      return
      end
!
      subroutine ana_uclima_tile (Istr,Iend,Jstr,Jend )!, wrk1,wrk2)
!
!---------------------------------------------------------------------
!  This routine sets analytical momentum climatology fields.
!---------------------------------------------------------------------
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k
c      real wrk1(PRIVATE_2D_SCRATCH_ARRAY),
c     &     wrk2(PRIVATE_2D_SCRATCH_ARRAY)
# ifdef JET
      real TL0, TC0, cff1, cff2, cff3, cff4, cff5
# endif
# include "param.h"
# include "climat.h"
# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
!
# include "compute_auxiliary_bounds.h"
!
! Set climatology fields for tracer type variables.
!---- ----------- ------ --- ------ ---- ----------
!
# if defined USWEST
      do j=JstrR,JendR
        do i=IstrR,IendR
          ubclm(i,j)=0.
          vbclm(i,j)=0.
        enddo
      enddo
#  ifdef SOLVE3D
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            uclm(i,j,k)=0.
            vclm(i,j,k)=0.
          enddo
        enddo
      enddo
#  endif
# elif defined JET
      TL0=-TL1-
     &    0.5*TL2*ZREF**2-
     &    ZL1*TL3*exp(ZREF/ZL1)
      TC0=-TC1-
     &    0.5*TC2*ZREF**2-
     &    ZC2*TC3*exp((ZREF-ZC1)/ZC2)
      do j=Jstr-1,Jend+1
        do i=Istr-1,Iend+1
         cff1=TL0*h(i,j)-
     &        0.5*TL1*h(i,j)**2+
     &        TL2*(h(i,j)**3)/6-
     &        (ZL1**2)*TL3*
     &         (1-exp(-h(i,j)/ZL1))
         cff2=TC0*h(i,j)-
     &        0.5*TC1*h(i,j)**2+
     &        TC2*(h(i,j)**3)/6-
     &        (ZC2**2)*TC3*
     &         exp(-ZC1/ZC2)*
     &         (1-exp(-h(i,j)/ZC2))
         cff3=YC1+YC2*
     &        tanh(YC3*xr(i,j)+YC4)
         cff4=-g*Tcoef*
     &        (1-(tanh((cff3-yr(i,j))/LCUR))**2)/
     &        (2*rho0*f(i,j)*LCUR*h(i,j))
! ubar
         wrk1(i,j)=cff4*(cff1-cff2)
! vbar
         cff5=YC2*YC3*
     &        (1-(tanh(YC3*xr(i,j)
     &            +YC4))**2)
         wrk2(i,j)=cff4*cff5*(cff1-cff2)
        enddo
      enddo
      do j=JstrR,JendR
        do i=Istr,IendR
          ubclm(i,j)=0.5*(wrk1(i,j)+wrk1(i-1,j))
        enddo
      enddo
      do j=Jstr,JendR
        do i=IstrR,IendR
          vbclm(i,j)=0.5*(wrk2(i,j)+wrk2(i,j-1))
        enddo
      enddo
#  ifdef SOLVE3D
      do k=1,N
        do j=Jstr-1,Jend+1
          do i=Istr-1,Iend+1
           cff1=TL0+
     &        TL1*z_r(i,j,k)+
     &        0.5*TL2*(z_r(i,j,k)**2)+
     &        ZL1*TL3*
     &        exp(z_r(i,j,k)/ZL1)
           cff2=TC0+
     &        TC1*z_r(i,j,k)+
     &        0.5*TC2*(z_r(i,j,k)**2)+
     &        ZC2*TC3*
     &        exp((z_r(i,j,k)-ZC1)/ZC2)
           cff3=YC1+YC2*
     &        tanh(YC3*xr(i,j)+YC4)
           cff4=g*Tcoef*
     &        (1-(tanh((cff3-yr(i,j))/LCUR))**2)/
     &        (2*rho0*f(i,j)*LCUR)
! u
           wrk1(i,j)=cff4*(cff1-cff2)
! v
           cff5=YC2*YC3*
     &          (1-(tanh(YC3*xr(i,j)
     &              +YC4))**2)
           wrk2(i,j)=cff4*cff5*(cff1-cff2)
          enddo
        enddo
        do j=JstrR,JendR
          do i=Istr,IendR
           uclm(i,j,k)=0.5*(wrk1(i,j)+wrk1(i-1,j))
          enddo
        enddo
        do j=Jstr,JendR
          do i=IstrR,IendR
           vclm(i,j,k)=0.5*(wrk2(i,j)+wrk2(i,j-1))
          enddo
        enddo
      enddo
#  endif
# elif defined ISWAKE
!
! There is no code here because for this case climatological data 
! is saved from the initial conditions and remained unchanged after 
! that.
!
# else
      do j=JstrR,JendR
        do i=IstrR,IendR
          ubclm(i,j)=???
          vbclm(i,j)=???
        enddo
      enddo
#  ifdef SOLVE3D
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            uclm(i,j,k)=???
            vclm(i,j,k)=???
          enddo
        enddo
      enddo
#  endif
# endif
      return
      end
#endif /* ANA_UCLIMA && UCLIMATOLOGY */
!
#if defined ANA_WWAVE && defined SG_BBL96
      subroutine ana_wwave_tile (Istr,Iend,Jstr,Jend)
!
! Set wind induced wave amplitude, direction and period for
! bottom boundary layer formulation.
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j
# include "param.h"
# include "bblm.h"
# include "grid.h"
# include "scalars.h"
!
# include "compute_auxiliary_bounds.h"
!
!  Set wind induced wave amplitude (m), direction (radians) and
!  period (s) at RHO-points.
!
      do j=JstrR,JendR
        do i=IstrR,IendR
          Awave(i,j)=???
          Dwave(i,j)=???
          Pwave(i,j)=???
        enddo
      enddo
      return
      end
#endif /* ANA_WWAVE && SG_BBL96 */

#if defined PSOURCE && defined ANA_PSOURCE
                              ! Set analytical tracer and mass point
      subroutine ana_psource  ! sources and sinks. River runoff can
      implicit none           ! be consider as a point source.
# include "param.h"
# include "scalars.h"
# include "sources.h"
!
      integer is, k
      real cff, fac
!
! Set-up point Sources/Sink number (Nsrc), direction (Dsrc), I- and
! J-grid locations (Isrc,Jsrc), and logical switch for type of tracer
! to apply (Lsrc). Currently, the direction can be along XI-direction
! (Dsrc = 0) or along ETA-direction (Dsrc > 0).  The mass sources are
! located at U- or V-points so the grid locations should range from
! 1 =< Isrc =< L  and  1 =< Jsrc =< M.
!
      if (iic.eq.ntstart) then
# if defined IBERIA
        Nsrc=4
        Dsrc(1)=1            ! Minho
        Isrc(1)=109
        Jsrc(1)=137
        Lsrc(1,itemp)=.true.
        Lsrc(1,isalt)=.true.

        Dsrc(2)=1            ! Douro
        Isrc(2)=116
        Jsrc(2)=114
        Lsrc(2,itemp)=.true.
        Lsrc(2,isalt)=.true.

        Dsrc(3)=1            ! Mondego
        Isrc(3)=107
        Jsrc(3)=78
        Lsrc(3,itemp)=.true.
        Lsrc(3,isalt)=.true.

        Dsrc(4)=1            ! TEJO
        Isrc(4)=102
        Jsrc(4)=36
        Lsrc(4,itemp)=.true.
        Lsrc(4,isalt)=.true.
#  ifdef SOLVE3D
        do is=1,Nsrc                   ! Set-up nondimensional shape
          do k=1,9                     ! function to dIstribute mass
            Qshape(is,k)=0.1           ! point sources/sinks
          enddo                        ! vertically.
          Qshape(is,10)=0.06           ! These most add to unity!
          Qshape(is,11)=0.03
          Qshape(is,12)=0.01
          do k=13,19
            Qshape(is,k)=0.0
          enddo
        enddo
#  endif

# elif defined RIVER
        Nsrc=3
        do is=1,Nsrc
          Dsrc(is)=0
          Isrc(is)=1
          Jsrc(is)=3*MMm/4 +is-1
          Lsrc(is,itemp)=.true.
          Lsrc(is,isalt)=.true.
        enddo
#  ifdef SOLVE3D
        cff=1./float(N)
        do k=1,N                         ! Uniform vertical
          do is=1,Nsrc                   ! dIstribution
            Qshape(is,k)=cff
          enddo
        enddo
#  endif
# else
      ERROR ###  CPP-key 'ANA_PSOURCE' is defined, but no code
      ERROR ###  is provided to set up Dsrc, Isrc, Jsrc, Lsrc.
# endif
      endif
!
! Set-up vertically integrated mass transport [m3/s] of point
! sources (these may time-dependent; positive in the positive U-
! or V-direction and vice-versa) and vertically dIstribute them
! according to mass transport profile chosen above.
!
# ifdef SOLVE3D
#  ifdef IBERIA
!     if (tdays.lt.0.5) then
!       fac=1.0+TANH((time-43200.)/43200.)
!     else
!       fac=0.5 + 0.5*sin(tdays*pi/180. +45.)
!     endif
!     do is=1,Nsrc
!       Qbar(is)=fac*400.
!       Qbar(is)=400.
!     enddo
!
      Qbar(1)=300.
      Qbar(2)=600.
      Qbar(3)=200.
      Qbar(4)=800.
#  elif defined RIVER
!     if (tdays-dstart .lt. 0.5) then
      if (tdays.lt.0.5) then
        fac=1.0+TANH((time-43200.)/43200.)
      else
        fac=1.0
      endif
      do is=1,Nsrc
        if (is.eq.2) then
          cff=13.
        else
          cff=7.
        endif
        Qbar(is)=fac*1500. * cff/27.
      enddo
#  else
      ERROR ###  CPP-key 'ANA_PSOURCE' is defined, but no code
      ERROR ###  is provided to set up Qbar(is) analytically.
#  endif
      do is=1,Nsrc
        do k=1,N
          Qsrc(is,k)=Qbar(is)*Qshape(is,k)
        enddo
      enddo
# endif /* SOLVE3D */
# if defined PSOURCE && defined SOLVE3D
!
!  Set-up tracer (tracer units) point Sources/Sinks.
!
#  ifdef IBERIA
      do k=1,N
!       do is=1,Nsrc
          Tsrc(1,k,itemp)=13.0
          Tsrc(1,k,isalt)=10.0
          Tsrc(2,k,itemp)=13.0
          Tsrc(2,k,isalt)=10.0
          Tsrc(3,k,itemp)=14.0
          Tsrc(3,k,isalt)=10.0
          Tsrc(4,k,itemp)=14.0
          Tsrc(4,k,isalt)=10.0
!       enddo
      enddo
#  elif defined RIVER
      do k=1,N
        do is=1,Nsrc
          Tsrc(is,k,itemp)=13.0
          Tsrc(is,k,isalt)=0.0
        enddo
      enddo
#  else
      ERROR ###  CPP-key 'ANA_PSOURCE' is defined, but no code
      ERROR ###  is provided to set up Tsrc(is) analytically.
#  endif
# endif
      return
      end
#endif

      subroutine ana_empty
      end
 
 
