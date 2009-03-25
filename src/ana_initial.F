#include "cppdefs.h"
#ifdef ANA_INITIAL
 
      subroutine ana_initial (tile)
      implicit none
      integer tile
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      call ana_initial_tile (Istr,Iend,Jstr,Jend, A2d(1,1))
      return
      end
 
      subroutine ana_initial_tile (Istr,Iend,Jstr,Jend, wrk1)
!
! Set initial conditions for momentum and tracer variables using
!  analytical expressions.
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k, itrc, i0,j0
      real wrk1(PRIVATE_2D_SCRATCH_ARRAY) 
      real Ampl, fac, x,y, x0,y0, cff, cff1,cff2,cff3
#ifdef ISWAKE
      integer jwake,mid_wake
      real AmpR,AmpU, cffS, Zthrm, dZthrm, xitm, alpha, csa,sna 
#endif
# include "param.h"
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "scalars.h"
# ifdef ISWAKE
#  ifdef ANA_BRY
#   include "boundary.h"
#  else
#   include "climat.h"
#  endif 
# endif
!
# include "compute_auxiliary_bounds.h"
!
      do j=JstrR,JendR       ! Set everything (except temperature
        do i=IstrR,IendR     ! and salinity) to all-zero state, then  
          zeta(i,j,1)=0.     ! modify some of the variables, if a
          ubar(i,j,1)=0.     ! non-trivial initialization required.
          vbar(i,j,1)=0.     ! Note: A code to initialize T [and S]
        enddo                ! must always be supplied for 3D  
      enddo                  ! applications.
# ifdef SOLVE3D
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            u(i,j,k,1)=0.
            u(i,j,k,2)=0.
            v(i,j,k,1)=0.
            v(i,j,k,2)=0.
          enddo
        enddo
      enddo
# endif

                             ! For the purpose of testing code
# if defined SOLITON     /*  ! symmetry the soliton is initialized */
      x0=xl/2.               ! exactly at the center of computational
      y0=el/2.               ! domain. This is because the analytical
      cff1=0.395             ! fields are terminated at the
      cff2=0.771*(cff1*cff1) ! boundarieas, where their values are 
      do j=JstrR,JendR       ! replaced via periodicity conditions. 
        do i=IstrR,IendR     ! The replacement causes differences 
          x=xr(i,j)-x0       ! between left- and right-travelling 
          y=yr(i,j)-y0       ! solitons discoverable by ETALON CHECK. 
          cff3=exp(-cff1*x)
          fac=cff2*(2.*cff3/(1.+cff3*cff3))**2
          zeta(i,j,1)=0.25*fac*(6.*y*y+3.)*exp(-0.5*y*y)
        enddo
      enddo

      do j=JstrR,JendR
        do i=Istr,IendR
          x=0.5*(xr(i-1,j)+xr(i,j))-x0
          y=0.5*(yr(i-1,j)+yr(i,j))-y0
          cff3=exp(-cff1*x)
          fac=cff2 * (2.*cff3/(1.+cff3*cff3))**2
          ubar(i,j,1)=0.25*fac*(6.*y*y-9.)*exp(-0.5*y*y)
        enddo
      enddo
      do j=Jstr,JendR
        do i=IstrR,IendR
          x=0.5*(xr(i,j-1)+xr(i,j))-x0
          y=0.5*(yr(i,j-1)+yr(i,j))-y0
          cff3=exp(-cff1*x)
          fac=cff2 * (2.*cff3/(1.+cff3*cff3))**2
          vbar(i,j,1)=2.*fac*y*(-2.*cff1*tanh(cff1*x))
     &                                  *exp(-0.5*y*y)
        enddo
      enddo
      if (fomn(Istr,Jend)-fomn(Istr,Jstr) .lt. 0.) then
C$OMP MASTER
          MPI_master_only write(stdout,'(/1x,2A/)') '>>> Eastward-',
     &          'travelling soliton: reversing initial velocities.'
C$OMP END MASTER
        do j=JstrR,JendR               ! Reverse velocities, if the 
          do i=IstrR,IendR             ! sign of gradient of Coriolis 
            ubar(i,j,1)=-ubar(i,j,1)   ! parameter "beta" is negative
            vbar(i,j,1)=-vbar(i,j,1)   ! (see ana_grid). In this case 
          enddo                        ! soliton should travel to the 
        enddo                          ! east, while maintaining all 
      endif                            ! energetics exactly the same.

# elif defined WAVE_RAD

      x0=xl/2.                     ! Set initial perturbation in 
      y0=el/2.                     ! in free surface and velocities 
      cff=64./xl                   ! for isotripically expanding 
      do j=Jstr,Jend               ! circular wave. 
        do i=Istr,Iend
          x=cff*(xr(i,j)-x0)
          y=cff*(yr(i,j)-y0)
          cff1=sqrt(x*x+y*y)
          if (cff1.gt.9. .and. cff1.lt.11.) then
            zeta(i,j,1)=1.+cos(pi*(cff1-10.))
          endif
        enddo
      enddo
      do j=Jstr,Jend
        do i=Istr,Iend
          x=cff*(0.5*(xr(i-1,j)+xr(i,j))-x0)
          y=cff*(               yr(i,j) -y0)
          cff1=sqrt(x*x+y*y)
          if (cff1.gt.9. .and. cff1.lt.11.) then
            ubar(i,j,1)=(1.+cos(pi*(cff1-10.)))
     &      *(x/cff1)*sqrt(2.*g/(h(i-1,j)+h(i,j)))
          endif
        enddo
      enddo
      do j=Jstr,Jend
        do i=Istr,Iend
          x=cff*(               xr(i,j) -x0)
          y=cff*(0.5*(yr(i,j-1)+yr(i,j))-y0)
          cff1=sqrt(x*x+y*y)
          if (cff1.gt.9. .and. cff1.lt.11.) then
            vbar(i,j,1)=(1.+cos(pi*(cff1-10.)))
     &      *(y/cff1)*sqrt(2.*g/(h(i,j-1)+h(i,j)))
          endif
        enddo
      enddo


# elif defined BASIN
#  ifdef SOLVE3D
      cff1=(44.690/39.382)**2
      cff2=cff1*(rho0*800./g)*(5.0e-5/((42.689/44.690)**2))
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            t(i,j,k,1,itemp)=cff2*exp(z_r(i,j,k)/800.)
     &                 *(0.6-0.4*tanh(z_r(i,j,k)/800.))
            t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
          enddo
        enddo
      enddo
#  endif
# elif defined CANYON_A
#  ifdef SOLVE3D
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            t(i,j,k,1,itemp)=T0
            t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
          enddo
        enddo
      enddo
#  endif
# elif defined CANYON_B
#  ifdef SOLVE3D
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            t(i,j,k,1,itemp)=3.488*exp(z_r(i,j,k)/800.)
     &        *(1.-0.666666666666*tanh(z_r(i,j,k)/800.))
            t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
          enddo
        enddo
      enddo
#  endif
# elif defined DAMEE_B
#  ifdef SOLVE3D
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
c            t(i,j,k,1,itemp)=0.2+6.*exp(z_r(i,j,k)/2500.)
c     &              +30./(1.+exp((-200.-z_r(i,j,k))/200.))
 
            t(i,j,k,1,itemp)=0.2+6.*exp(z_r(i,j,k)/2500.)
     &                    +20.*exp(z_r(i,j,k)/250.)
 
            t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
#   ifdef SALINITY
            t(i,j,k,1,isalt)=35.
            t(i,j,k,2,isalt)=35.
#   endif
          enddo
        enddo
      enddo
#  endif
# elif defined GRAV_ADJ
#  ifdef SOLVE3D
      i0=LLm/2
#  ifdef MPI
      i0=i0 -iSW_corn
#  endif
      cff=1.                         ! A provision is made to form
      do k=1,N                       ! a 2-point transitional layer
        do j=JstrR,JendR             ! between T=+1 and T=-1, hence
          do i=IstrR,IendR           ! "softening" the step function.
            if (i.lt.i0) then        ! Normally "cff" set to 0.75
              t(i,j,k,1,itemp)=+1.   ! resulting in transitional
              t(i,j,k,2,itemp)=+1.   ! values of 3/8 of the total
            elseif (i.eq.i0) then    ! step amplitude.
              t(i,j,k,1,itemp)=+cff
              t(i,j,k,2,itemp)=+cff
            elseif (i.eq.i0+1) then
              t(i,j,k,1,itemp)=-cff
              t(i,j,k,2,itemp)=-cff
            else
              t(i,j,k,1,itemp)=-1.
              t(i,j,k,2,itemp)=-1.
            endif
           enddo
        enddo
      enddo

#  endif
# elif defined OVERFLOW
#  ifdef SOLVE3D
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
c           t(i,j,k,1,itemp)=T0-0.5*T0*(1.+tanh((yr(i,j)-25000.)
c     &                                                   /1000.))
 
            t(i,j,k,1,itemp)=T0*(0.5-0.5*tanh( yr(i,j)/1000.-25.))
 
            t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
          enddo
        enddo
      enddo
#  endif
# elif defined NJ_BIGHT
#  ifdef SOLVE3D
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            if (z_r(i,j,k).ge.-15.0) then
              t(i,j,k,1,itemp)=2.049264257728403e+01-z_r(i,j,k)*(
     &                         2.640850848793918e-01+z_r(i,j,k)*(
     &                         2.751125328535212e-01+z_r(i,j,k)*(
     &                         9.207489761648872e-02+z_r(i,j,k)*(
     &                         1.449075725742839e-02+z_r(i,j,k)*(
     &                         1.078215685912076e-03+z_r(i,j,k)*(
     &                         3.240318053903974e-05+
     &                         1.262826857690271e-07*z_r(i,j,k)
     &                                                     ))))))
              t(i,j,k,1,isalt)=3.066489149193135e+01-z_r(i,j,k)*(
     &                         1.476725262946735e-01+z_r(i,j,k)*(
     &                         1.126455760313399e-01+z_r(i,j,k)*(
     &                         3.900923281871022e-02+z_r(i,j,k)*(
     &                         6.939014937447098e-03+z_r(i,j,k)*(
     &                         6.604436696792939e-04+z_r(i,j,k)*(
     &                         3.191792361954220e-05+
     &                         6.177352634409320e-07*z_r(i,j,k)
     &                                                     ))))))
            else
               t(i,j,k,1,itemp)=14.6+6.7 *tanh(1.1*z_r(i,j,k)+15.9)
               t(i,j,k,1,isalt)=31.3-0.55*tanh(1.1*z_r(i,j,k)+15.9)
            endif
            t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
            t(i,j,k,2,isalt)=t(i,j,k,1,isalt)
          enddo
        enddo
      enddo
#  endif
# elif defined SEAMOUNT
#  ifdef SOLVE3D
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
#   ifdef NONLIN_EOS
            t(i,j,k,1,itemp)=4.0  ! Just spatially uniform T,S to
            t(i,j,k,2,isalt)=33.0 ! illustrate compressibility effect.
#   else
!
! There are four possible choices here: exponential profile
! which can be initialized as either a set of values at locations
! z_r (to test prsgrd31,32,32A); or in finite volume sense (to test
! prsgrd40,41,42,44). Also there are two similar versions of linear
! profiles.
!
#define AMPL 3.
#define STRAT 500.
 
           t(i,j,k,1,itemp)=AMPL*exp(z_r(i,j,k)/STRAT)

c**      t(i,j,k,1,itemp)=AMPL*exp((z_r(i,j,k)-0.0003*yr(i,j))/STRAT)
 
c            t(i,j,k,1,itemp)=AMPL*STRAT*( exp(z_w(i,j,k)/STRAT)
c     &                                   -exp(z_w(i,j,k-1)/STRAT)
c     &                                )/(z_w(i,j,k)-z_w(i,j,k-1))
!
! Linear profile to check exact error cancellation.
!
 
c            t(i,j,k,1,itemp)=AMPL*(1.+z_r(i,j,k)/5000.)
 
c           t(i,j,k,1,itemp)=0.5*(z_w(i,j,k)+z_w(i,j,k-1))
 
c            t(i,j,k,1,itemp)=AMPL*(1.+0.5*(z_w(i,j,k)
c     &                            +z_w(i,j,k-1))/5000.)
 
#undef STRAT
#undef AMPL
 
            t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
#   endif
          enddo
        enddo
      enddo
#  endif
# elif defined ISWAKE
#  ifdef SOLVE3D
!
! Analytical initial conditions for 3D Island Wake problem:
!----------- ------- ---------- --- -- ------ ---- --------
! free-surface, velocity and temperature (actually negative density
! --- it is assumed that thermodynamic expansion coefficient is equal
! to 1 for this problem) fields are set to satisfy thermal wind
! balance  equation
!                      du      g       d rho
!                 f * ---- = ------ * -------     [here rho=-T]
!                      dz     rho0       dy
!
! subject to the conditions that surface velocity is in geostrophic
! balance with free surface, and velocity vanishes at infinitely
! large negative depth.  Both velocity and themperature field are
! set via the same hyperbolic shape function 
!
!                  1     1         z-Zthrm(y)
!                 --- + --- * tanh ---------- 
!                  2     2           dZthrm
!
! where "Zthrm" and "dZthrm" set thermocline depth and sharpness 
! respectively, and y-dependency of "Zthrm" creates termocline
! slope needed for thermal balance; free surface is determined by
! vertical integral of temperature profile and the assumption that
! total pressure (baroclinic+barotropic) gradient vanishes at
! infinite negative depth. Integration is done analytically.
!
      AmpR=3.0         ! magnitude of density perturbation [kg/m^3]
      AmpU=0.2         ! velocity magnitute above thermocline [m/s]
      Zthrm=-120.      ! median depth of thermocline [m] 
      dZthrm=60.       ! thickness of thermocline [m]

                              ! set thermocline slope parameter to
      cffS=AmpU*rho0/(AmpR*g) ! achieve geostrophic balance between 
      y0=el/2.                ! XI-component of baroclinic velocity
                              ! and density field. 

      cff=1./(1.+exp(-2.*Zthrm/dZthrm))
      do j=JstrR,JendR
        do i=IstrR,IendR
          xitm=(-Zthrm  -cffS*f(i,j)*(yr(i,j)-y0))/dZthrm
          zeta(i,j,1)=(AmpR/rho0) *0.5*dZthrm
     &                       *log( cff*(1.+exp(2.*xitm)) )
#ifdef MASKING
     &                                 *rmask(i,j)
#endif
          ubar(i,j,1)=0.
          vbar(i,j,1)=0.
        enddo
      enddo
      do k=N,1,-1
        do j=JstrR,JendR
          do i=IstrR,IendR
            xitm=(z_r(i,j,k)-Zthrm -cffS*f(i,j)*(yr(i,j)-y0))/dZthrm
            t(i,j,k,1,itemp)= 0.5*AmpR*tanh(xitm)
#ifdef MASKING
     &                                 *rmask(i,j)                
#endif
          enddo
        enddo
        do j=JstrR,JendR
          do i=Istr,IendR
            xitm=( 0.5*(z_r(i,j,k)+z_r(i-1,j,k)) -Zthrm
     &                    -cffS*0.5*(f(i,j)+f(i-1,j))
     &                  *(0.5*(yr(i,j)+yr(i-1,j))-y0) )/dZthrm

            u(i,j,k,1)=AmpU*(0.5 + 0.5*tanh(xitm))
#ifdef MASKING
     &                                  *umask(i,j)
#endif
            ubar(i,j,1)=ubar(i,j,1) +(Hz(i-1,j,k)+Hz(i,j,k))
     &                                           *u(i,j,k,1)
          enddo
        enddo
      enddo
      do j=JstrR,JendR
        do i=Istr,IendR
          ubar(i,j,1)=ubar(i,j,1)/( z_w(i,j,N)+z_w(i-1,j,N)
     &                             -z_w(i,j,0)-z_w(i-1,j,0))
        enddo
      enddo 

#define SIGNELL
#ifdef SIGNELL

      AmpU=0.15           ! Rich Signell setup for
      cff=AmpU/g          ! a nonstratified problem
      x0=xl/2.            ! posed as 3D.
      y0=el/2.

      do j=JstrR,JendR
        do i=IstrR,IendR
          zeta(i,j,1)=-cff*f(i,j)*(yr(i,j)-y0)
          ubar(i,j,1)=AmpU
          vbar(i,j,1)=0.
        enddo
      enddo
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            u(i,j,k,1)=ubar(i,j,1)
            v(i,j,k,1)=vbar(i,j,1)
            t(i,j,k,1,itemp)=0.
          enddo
        enddo
      enddo
#endif




#  else
      AmpU=0.2            ! 2D setup for flow around cylinder
      cff=AmpU/g          ! problem: set u-velocity constant and
      alpha=0.3           ! free surface from geostrophic balance. 
      csa=cos(alpha)
      sna=sin(alpha)
      x0=xl/2.
      y0=el/2.
      do j=JstrR,JendR
        do i=IstrR,IendR
          zeta(i,j,1)=-cff*f(i,j)*((yr(i,j)-y0)*csa-(xr(i,j)-y0)*sna)
          ubar(i,j,1)=AmpU*csa
          vbar(i,j,1)=AmpU*sna
        enddo
      enddo
#  endif  /* SOLVE3D */

#  ifdef ANA_BRY
      if (WESTERN_EDGE) then
#    ifdef M2_FRC_BRY
        do j=JstrR,JendR                     ! Save perimeter values
          zeta_west(j)=zeta(Istr-1,j,1)      ! of the initial fields
          ubar_west(j)=ubar(Istr  ,j,1)      ! to be used as external
          vbar_west(j)=vbar(Istr-1,j,1)      ! forcing data for side 
        enddo                                ! boundaries.
#    endif
#    ifdef SOLVE3D
        do k=1,N
          do j=JstrR,JendR
#     ifdef M3_FRC_BRY
            u_west(j,k)=u(Istr  ,j,k,1)
            v_west(j,k)=v(Istr-1,j,k,1)
#     endif
#     ifdef T_FRC_BRY
            t_west(j,k,itemp)=t(Istr-1,j,k,1,itemp)
#     endif
          enddo
        enddo
#    endif
      endif

      if (EASTERN_EDGE) then
#    ifdef M2_FRC_BRY
        do j=JstrR,JendR
          zeta_east(j)=zeta(Iend+1,j,1)
          ubar_east(j)=ubar(Iend+1,j,1)
          vbar_east(j)=vbar(Iend+1,j,1)
        enddo
#    endif
#    ifdef SOLVE3D
        do k=1,N
          do j=JstrR,JendR
#     ifdef M3_FRC_BRY
            u_east(j,k)=u(Iend+1,j,k,1)
            v_east(j,k)=v(Iend+1,j,k,1)
#     endif
#     ifdef T_FRC_BRY
            t_east(j,k,itemp)=t(Iend+1,j,k,1,itemp)
#     endif
          enddo
        enddo
#    endif
      endif

#    ifdef OBC_SOUTH
      if (SOUTHERN_EDGE) then
#     ifdef M2_FRC_BRY
        do i=IstrR,IendR
          zeta_south(i)=zeta(i,Jstr-1,1)
          ubar_south(i)=ubar(i,Jstr-1,1)
          vbar_south(i)=vbar(i,Jstr  ,1)
        enddo
#     endif
#     ifdef SOLVE3D
        do k=1,N
          do i=IstrR,IendR 
#      ifdef M3_FRC_BRY
            u_south(i,k)=u(i,Jstr-1,k,1)
            v_south(i,k)=v(i,Jstr  ,k,1)
#      endif
#      ifdef T_FRC_BRY
            t_south(i,k,itemp)=t(i,Jstr-1,k,1,itemp)
#      endif
          enddo
        enddo
#     endif
      endif
#    endif

#    ifdef OBC_NORTH
      if (NORTHERN_EDGE) then
#     ifdef M2_FRC_BRY
        do i=IstrR,IendR
          zeta_north(i)=zeta(i,Jend+1,1)
          ubar_north(i)=ubar(i,Jend+1,1)
          vbar_north(i)=vbar(i,Jend+1,1)
        enddo
#     endif
#     ifdef SOLVE3D
        do k=1,N
          do i=IstrR,IendR
#      ifdef M3_FRC_BRY
            u_north(i,k)=u(i,Jend+1,k,1)
            v_north(i,k)=v(i,Jend+1,k,1)
#      endif
#      ifdef T_FRC_BRY
            t_north(i,k,itemp)=t(i,Jend+1,k,1,itemp)
#      endif
          enddo
        enddo
#     endif
      endif
#    endif

#  else
#   ifdef UCLIMATOLOGY
      do j=JstrR,JendR                   ! Save initial data into
        do i=IstrR,IendR                 ! climatology for the
          ubclm(i,j)=ubar(i,j,1)         ! subsequent use as inflow
          vbclm(i,j)=vbar(i,j,1)         ! boundary conditions
        enddo
      enddo
#   endif
#   ifdef SOLVE3D
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
#    ifdef UCLIMATOLOGY
            uclm(i,j,k)=u(i,j,k,1)
            vclm(i,j,k)=v(i,j,k,1)
#    endif
#    ifdef TCLIMATOLOGY
            tclm(i,j,k,itemp)=t(i,j,k,1,itemp)
#    endif
          enddo
        enddo
      enddo
#   endif /* SOLVE3D */
#  endif /* ANA_BRY */


# elif defined SHELFRONT
#  ifdef SOLVE3D
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            t(i,j,k,1,itemp)=T0+2.5*tanh((yr(i,j)-50000.0)/20000.0)
            t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
            t(i,j,k,1,isalt)=S0  +  tanh((yr(i,j)-50000.0)/20000.0)
            t(i,j,k,2,isalt)=t(i,j,k,1,isalt)
          enddo
        enddo
      enddo
#  endif
# elif defined TASMAN_SEA
#  ifdef SOLVE3D
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            t(i,j,k,1,itemp)=17.4+z_r(i,j,k)*(1.727e-2+z_r(i,j,k)*
     &                           (5.707e-06+z_r(i,j,k)*(5.921e-10)))
            t(i,j,k,1,isalt)=35.08+z_r(i,j,k)*(7.56e-4+z_r(i,j,k)*
     &                           (3.185e-07+z_r(i,j,k)*(3.702e-11)))
            t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
            t(i,j,k,2,isalt)=t(i,j,k,1,isalt)
          enddo
        enddo
      enddo
#  endif
# elif defined UPWELLING
#  ifdef SOLVE3D
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
c            t(i,j,k,1,itemp)=14.+8.*exp(z_r(i,j,k)/50.)
 
#define Z0 (-35.)
#define THKNSS 6.5
#define Z1 (-75.)
#define STRAT 150.
 
c           t(i,j,k,1,itemp)=18.+4.*tanh((z_r(i,j,k)-Z0)/THKNSS)
c     &                                  +(z_r(i,j,k)-Z1)/STRAT
 
           t(i,j,k,1,itemp)=14.+4.*THKNSS*log(
     &                          cosh((z_w(i,j,k )-Z0)/THKNSS)
     &                         /cosh((z_w(i,j,k-1)-Z0)/THKNSS)
     &                             )/(z_w(i,j,k)-z_w(i,j,k-1))
     &               +((z_w(i,j,k)+z_w(i,j,k-1))/2.-Z1)/STRAT
 
#undef Z0
#undef THKNSS
#undef Z1
#undef STRAT
 
 
 
 
 
            t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
 
#   ifdef SALINITY
c*            t(i,j,k,1,isalt)=1.E-4*yr(i,j)-S0 +0.1
            t(i,j,k,1,isalt)=1.
 
            if (j.lt.Mm/2) then
              t(i,j,k,1,isalt)=0.
            elseif (j.eq.Mm/2) then
              t(i,j,k,1,isalt)=0.5
            elseif (j.gt.Mm/2) then
              t(i,j,k,1,isalt)=1.
            endif
 
            t(i,j,k,2,isalt)=t(i,j,k,1,isalt)
#   endif
          enddo
        enddo
      enddo
#  endif
# elif defined  RIVER
#  ifdef SOLVE3D
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            t(i,j,k,1,itemp)=4.+10.*exp(z_r(i,j,k)/50.)
            t(i,j,k,1,isalt)=36.
            t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
            t(i,j,k,2,isalt)=t(i,j,k,1,isalt)
          enddo
        enddo
      enddo
#  endif
# elif defined PACIFIC_2D
      Ampl=1.0 
      cff1=800./dble(LLm)

      if (LLm.eq.840 .and. MMm.eq.512) then
        i0=519
        j0=283
      elseif (LLm.eq.1680 .and. MMm.eq.1024) then
        i0=1036
        j0=565
      else
        i0=LLm/2
        j0=MMm/2
      endif
#  ifdef MPI
      i0=i0 -iSW_corn
      j0=j0 -jSW_corn
#  endif

      cff1=800./dble(LLm)
      do j=JstrR,JendR
        do i=IstrR,IendR
          cff=cff1*sqrt(dble(i-i0)**2+dble(j-j0)**2)
          if (cff.lt.pi) then
            zeta(i,j,1)=Ampl*(0.5+0.5*cos(cff))
          endif
        enddo
      enddo
# elif defined USWEST
#  ifdef SOLVE3D
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
c**         t(i,j,k,1,itemp)=10.+10.*exp(z_r(i,j,k)/200.)
c**         t(i,j,k,1,itemp)=20. + z_r(i,j,k)/4500.
c*          t(i,j,k,1,itemp)=4.+13.*exp(z_r(i,j,k)/450.)
 
 
c           t(i,j,k,1,itemp)=4.+13.*450.*( exp(z_w(i,j,k)/450.)
c     &                                   -exp(z_w(i,j,k-1)/450.))
c     &                                 /(z_w(i,j,k)-z_w(i,j,k-1))
 
 
#define Z0 (-80.)
#define THKNSS 50.
#define Z1 0.
#define STRAT 1000.
 
c*         t(i,j,k,1,itemp)=4.+4.*tanh((z_r(i,j,k)-Z0)/THKNSS)
c*     &                                  +(z_r(i,j,k)-Z1)/STRAT
 
           t(i,j,k,1,itemp)=14.+5.*THKNSS*log(
     &                            cosh((z_w(i,j,k )-Z0)/THKNSS)
     &                           /cosh((z_w(i,j,k-1)-Z0)/THKNSS)
     &                               )/(z_w(i,j,k)-z_w(i,j,k-1))
 
     &               +((z_w(i,j,k)+z_w(i,j,k-1))/2.-Z1)/STRAT
#undef Z0
#undef THKNSS
#undef Z1
#undef STRAT
 
            t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
 
# ifdef SALINITY
            t(i,j,k,1,isalt)=35.
            t(i,j,k,2,isalt)=t(i,j,k,1,isalt)
# endif
# ifdef BIOLOGY
            t(i,j,k,1,ino3_)=0.
            t(i,j,k,2,ino3_)=t(i,j,k,1,ino3_)
            t(i,j,k,1,inh4_)=0.
            t(i,j,k,2,inh4_)=t(i,j,k,1,inh4_)
            t(i,j,k,1,idet_)=0.
            t(i,j,k,2,idet_)=t(i,j,k,1,idet_)
            t(i,j,k,1,iphyt)=0.
            t(i,j,k,2,iphyt)=t(i,j,k,1,iphyt)
            t(i,j,k,1,izoo_)=0.
            t(i,j,k,2,izoo_)=t(i,j,k,1,izoo_)
# endif
          enddo
        enddo
      enddo
#  endif
# else
#  ifdef SOLVE3D
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            t(i,j,k,1,itemp)=???
            t(i,j,k,2,itemp)=???
            t(i,j,k,1,isalt)=???
            t(i,j,k,2,isalt)=???
          enddo
        enddo
      enddo
#  endif
# endif  /* end of the switch over applications */

# if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI

      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                        zeta(START_2D_ARRAY,1))
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,
     &                        ubar(START_2D_ARRAY,1))
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,
     &                        vbar(START_2D_ARRAY,1))
#  ifdef SOLVE3D
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend,
     &                         u(START_2D_ARRAY,1,1))
      call exchange_v3d_tile (Istr,Iend,Jstr,Jend,
     &                         v(START_2D_ARRAY,1,1))
      do itrc=1,NT
        call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                   t(START_2D_ARRAY,1,1,itrc))
      enddo
#  endif
#  ifdef ISWAKE
#   ifdef UCLIMATOLOGY
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend, ubclm)
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend, vbclm)
#   endif
#   ifdef SOLVE3D
#    ifdef UCLIMATOLOGY
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend,  uclm)
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend,  vclm)
#    endif
#    ifdef TCLIMATOLOGY
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend,
     &                       tclm(START_2D_ARRAY,1,itemp)
#    endif
#   endif
#  endif
# endif
      return
      end
#else
      subroutine ana_initial_empty
      return
      end
#endif /* ANA_INITIAL */
 
