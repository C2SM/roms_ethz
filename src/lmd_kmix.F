#include "cppdefs.h"
#ifdef LMD_MIXING

      subroutine lmd_kmix (tile)
      implicit none
      integer tile
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      call lmd_kmix_tile (istr,iend,jstr,jend,  A2d(1,1), A2d(1,2),
     &                                          A2d(1, 3), A2d(1,4),
     &                   A2d(1, 5), A2d(1, 6),  A2d(1, 7), A2d(1, 8),
     &                   A2d(1, 9), A2d(1,10),  A2d(1,11), A2d(1,12),
     &                   A2d(1,13), A2d(1,14),  A2d(1,15), A2d(1,16))
      return
      end

      subroutine lmd_kmix_tile (istr,iend,jstr,jend, alpha,beta,
     &                             Hz_inv, uc,vc,du,dv, dr,dRdz,
     &                             Bfsfc, FC, Cr, Kv,Kt,Ks, A) 

      implicit none
# include "param.h"
# include "scalars.h"
# include "grid.h"
# include "ocean3d.h"
# include "mixing.h"
# include "forces.h"
#define tind nstp

      integer istr,iend,jstr,jend, i,j,k, km
      real alpha(PRIVATE_2D_SCRATCH_ARRAY),      cff,
     &      beta(PRIVATE_2D_SCRATCH_ARRAY),      cff1,    cff2,

     &    Hz_inv(PRIVATE_1D_SCRATCH_ARRAY,0:N),  grho0,   rref,
     &        uc(PRIVATE_1D_SCRATCH_ARRAY,0:N),  grho0Ri, strat,
     &        vc(PRIVATE_1D_SCRATCH_ARRAY,0:N),           Vterm, 
     &        du(PRIVATE_1D_SCRATCH_ARRAY,0:N),  dpth, 
     &        dv(PRIVATE_1D_SCRATCH_ARRAY,0:N),  hbld,    patch,    
     &        dr(PRIVATE_1D_SCRATCH_ARRAY,0:N),  hbl_inv, 
     &      dRdz(PRIVATE_1D_SCRATCH_ARRAY,0:N),  sigma,
     &     Bfsfc(PRIVATE_1D_SCRATCH_ARRAY,0:N),           f1,
     &        FC(PRIVATE_1D_SCRATCH_ARRAY,0:N),  cff_up,  Av_bl,
     &        Cr(PRIVATE_1D_SCRATCH_ARRAY,0:N),  cff_dn,  dAv_bl,
     &        Kv(PRIVATE_1D_SCRATCH_ARRAY,0:N),           At_bl,
     &        Kt(PRIVATE_1D_SCRATCH_ARRAY,0:N),  a1,      dAt_bl,
     &        Ks(PRIVATE_1D_SCRATCH_ARRAY,0:N),  a2,      As_bl,
     &         A(PRIVATE_1D_SCRATCH_ARRAY, 16),  a3,      dAs_bl


      integer srf_dpth, ustar, uscl3, Bosol, kbl,zbl, kmo,h_MO,
     &        Ekm,  Bfsfc_bl, Gm1,dGm1dS, Gt1,dGt1dS, Gs1,dGs1dS
      parameter (srf_dpth=1, ustar=2, uscl3=3, Bosol=4, kbl=5,
     &           zbl=6, kmo=7, h_MO=8, Ekm=9, Bfsfc_bl=10, Gm1=11,
     &           dGm1dS=12, Gt1=13, dGt1dS=14, Gs1=15, dGs1dS=16)

      real Ritop,dVsq,Vtsq,hekman, hmonob, cekman, cmonob, dudz,dvdz
      parameter (cekman=0.7, cmonob=1.)

      real epssfc, Ricr, Cv, betaT, C_MO, C_Ek, Cstar, Cg, Vtc,
     &          nuwm,nuws, eps
# ifdef LMD_RIMIX
     &           , Ri0, nu0m, nu0s
# endif
# ifdef LMD_CONVEC
     &                      , nu0c
# endif
      parameter (epssfc=0.1,  ! fraction of surface layer within 
                              ! boundary layer.
     &   Ricr=0.3D0,     ! critical bulk Richardson number (=0.3)

     &   Cv=1.8,         ! ratio of interior Brunt-Vaisala frequency
                         !           to that at the entrainment depth
     &   betaT=-0.2,     ! ratio of entrainment flux to surface
                         !                             buoyancy flux
     &   C_MO=1.,        ! constant for computing Monin-Obukhov depth

     &   C_Ek=400.,

     &   Cstar=10.,      ! Proportionality coefficient
                         ! parameterizing nonlocal transport.

     &   nuwm=1.0e-4,    ! viscosity and diffusivity due to
     &   nuws=0.1e-4,    ! internal wave breaking, [m^2/s]         
# ifdef LMD_RIMIX 
     &   Ri0=0.7,
     &   nu0m=50.e-4,
     &   nu0s=50.e-4,
# endif
# ifdef LMD_CONVEC
     &   nu0c=0.1,       ! convective adjustment  
# endif
     &   eps=1.E-20 )    ! just a small number




!
! Parameters and variables associated with "wscale" computations. 
!
      real zetam, am,cm, zetas, as,cs, zt,pwr, wm,ws
      parameter (zetam=-0.2, am=1.257,  cm=8.360, 
     &           zetas=-1.0, as=-28.86, cs=98.96)



      integer jwt
      parameter (jwt=1)
      real r1(5), mu1(5), mu2(5), xi1,xi2, swdk

      r1(1)=0.58     !  Define fraction of the total radiance
      r1(2)=0.62     !  for wavelength band 1 as a function of
      r1(3)=0.67     !  Jerlov water type. The fraction for
      r1(4)=0.77     !  wavelength band 2 is r2=1-r1.
      r1(5)=0.78

      mu1(1)=0.35    !  Define reciprocal of the absorption
      mu1(2)=0.6     !  coefficient for each of two solar
      mu1(3)=1.0     !  wavelength bands as a function
      mu1(4)=1.5     !  of water type (Ref: Paulson and
      mu1(5)=1.4     !  Simpson, 1977).

      mu2(1)=23.0
      mu2(2)=20.0
      mu2(3)=17.0
      mu2(4)=14.0
      mu2(5)=7.9


      Cg=Cstar * vonKar * (cs*vonKar*epssfc)**(1./3.)
      Vtc=Cv * sqrt(-betaT/(cs*epssfc)) / (Ricr*vonKar**2)

       
      



      call alfabeta_tile (istr,iend,jstr,jend, alpha,beta)
!
! Preliminary step: compute vertical derivatives of potential
! density of and velocity components (which are first interpolated
! to horizontal density points). Then compute non-staggered slope-
! limited vertical derivatives of density dRdz, which are needed
! later for accurate mid-point interpolation of density.
!
      do j=jstr,jend
        do i=istr,iend
          uc(i,1)=0.5D0*(u(i,j,1,nstp)+u(i+1,j,1,nstp))
          vc(i,1)=0.5D0*(v(i,j,1,nstp)+v(i,j+1,1,nstp))
        enddo
        do k=1,N-1,+1
          do i=istr,iend
            uc(i,k+1)=0.5D0*(u(i,j,k+1,nstp)+u(i+1,j,k+1,nstp))
            vc(i,k+1)=0.5D0*(v(i,j,k+1,nstp)+v(i,j+1,k+1,nstp))

            Hz_inv(i,k)=2.D0/(Hz(i,j,k+1)+Hz(i,j,k))
            du(i,k)=Hz_inv(i,k)*(uc(i,k+1)-uc(i,k))
            dv(i,k)=Hz_inv(i,k)*(vc(i,k+1)-vc(i,k))
            dr(i,k)=Hz_inv(i,k)*( rho1(i,j,k+1)-rho1(i,j,k)
# ifdef SPLIT_EOS
     &                            +(qp1(i,j,k+1)-qp1(i,j,k))
     &                              *(z_w(i,j,N)-z_w(i,j,k))
# endif
     &                                                     )
          enddo
        enddo

        do k=2,N-1
          do i=istr,iend
            cff=(dr(i,k)*Hz_inv(i,k)+dr(i,k-1)*Hz_inv(i,k-1))
     &                              /(Hz_inv(i,k)+Hz_inv(i,k-1))
            if (dr(i,k).gt.0.D0  .and.  dr(i,k-1).gt.0.D0) then
              dRdz(i,k)=min(cff, 2.D0*min(dr(i,k),dr(i,k-1)))
            elseif (dr(i,k).lt.0.D0 .and. dr(i,k-1).lt.0.D0) then
              dRdz(i,k)=max(cff, 2.D0*max(dr(i,k),dr(i,k-1)))
            else
              dRdz(i,k)=0.D0
            endif
          enddo
        enddo
        do i=istr,iend
          dRdz(i,N)=2.D0*dr(i,N-1) - dRdz(i,N-1)
          dRdz(i,1)=2.D0*dr(i,1) - dRdz(i,2)
          du(i,N)=du(i,N-1)
          du(i,0)=du(i,1)
          dv(i,N)=dv(i,N-1)
          dv(i,0)=dv(i,1)
        enddo       !--> discard uc,vc;  keep du,dv,dr,dRdz
!
! Forcing conditions at surface: compute depth of viscous surface
! layer "srf_dpth"; friction velocity "ustar" (note that ustar is
! bounded from zero, so that subsequend divisions are all safe);
! its inverse cube "uscl3" to facilitate computation of turbulent
! velocity scales ws and wm; surface boyancy flux "Bfsfc(:,N)" due
! to latent (i.e., excluding short-wave radiation component, which
! is saved separately as "Bosol"). Also initialize various auxiliary
! variables 
!
        grho0=g/rho0
        grho0Ri=g/(rho0*Ricr)
        cff1=C_MO/vonKar 

        do i=istr,iend
          A(i,srf_dpth)=hbl(i,j)*epssfc
          A(i,ustar)=max( sqrt( 0.5D0*sqrt(
     &                              (sustr(i,j)+sustr(i+1,j))**2
     &                             +(svstr(i,j)+svstr(i,j+1))**2
     &                                                   )), eps)

          A(i,uscl3)=vonKar/(A(i,ustar)**3)

          Bfsfc(i,N)=g*( alpha(i,j)*(stflx(i,j,itemp)-srflx(i,j))
# ifdef SALINITY
     &                                -beta(i,j)*stflx(i,j,isalt)
# endif
     &                                                          )
          A(i,Bosol)=g*alpha(i,j)*srflx(i,j)
          uc(i,N)=r1(jwt)
          vc(i,N)=1.D0-r1(jwt)

          Cr(i,N)=0.D0
          FC(i,N)=0.D0 
          A(i,kmo)=-1.D0
          A(i,kbl)=-1.D0
          A(i,h_MO)=cff1*A(i,ustar)**3  !<-- temporal value
          A(i,Ekm)=C_Ek*f(i,j)*f(i,j)
        enddo
!
! Attenuate short-wave radiation from surface downward using two
! solar wavelength band model, (Paulson and impson, 1977; here "jwt"
! is Jerlov water type index; "mu1,2" reciprocal of the absorption
! coefficient of each band). Also, in order to save computational  
! cost, exponential function below is replaced with its Pade fit: 
!
        cff1=1.D0/mu1(jwt)     !                1 - (1/3)x
        cff2=1.D0/mu2(jwt)     ! exp(-x) = -----------------------
                               !            1 + (2/3)x + (1/6)x^2
        do k=N,1,-1
          do i=istr,iend
            xi1=min(3.D0, cff1*Hz(i,j,k))
            uc(i,k-1)=uc(i,k)*(3.D0-xi1)/(3.D0+xi1*(2.D0-0.5D0*xi1))

            xi2=min(3.D0, cff2*Hz(i,j,k))
            vc(i,k-1)=vc(i,k)*(3.D0-xi2)/(3.D0+xi2*(2.D0-0.5D0*xi2))

            swdk=uc(i,k-1)+vc(i,k-1)

            Bfsfc(i,k-1)=Bfsfc(i,N) + A(i,Bosol)*(1.D0-swdk)
          enddo
        enddo
c**
c**
c**
#define Rib FC

        do i=istr,iend
          Rib(N,k)=0.
          Rib(1,k)=0.
          A(i,kbl)=1.+eps
          A(i,zbl)=z_r(i,j,1)
        enddo

        cff=g/rho0
        do k=N-1,2,-1
          do i=istr,iend
            Ritop=-cff*(rho1(i,j,N)-rho1(i,j,k))
     &                   *(z_r(i,j,N)-z_r(i,j,k))

            dVsq=0.25*( ( u(i  ,j,N,tind)-u(i  ,j,k,tind)
     &                   +u(i+1,j,N,tind)-u(i+1,j,k,tind) )**2
     &                 +( v(i,j  ,N,tind)-v(i,j  ,k,tind)
     &                   +v(i,j+1,N,tind)-v(i,j+1,k,tind) )**2)


            dpth=z_w(i,j,N)-z_r(i,j,k)
            zt=0.5*(Bfsfc(i,k)+Bfsfc(i,k-1))*min(A(i,srf_dpth),dpth)
     &                                                  * A(i,uscl3)

            if (zt .ge. 0.D0) then           !--> stable regime
              ws=1.+5.*zt
              pwr=-1.D0
            elseif (zt .gt. zetas) then      !--> unstable regime
              ws=1.-16.*zt
              pwr=0.5D0
            else                             !--> convective regime
              ws=as-cs*zt
              pwr=0.333333333333D0
            endif
            ws=vonKar*A(i,ustar) * ws**pwr

            Vtsq=Vtc*ws*(z_r(i,j,N)-z_r(i,j,k))
     &         *sqrt(max(0., 0.5*(bvf(i,j,k)+bvf(i,j,k-1))))

            Rib(i,k)=Ritop/(dVsq+Vtsq+eps)

          enddo

          do i=istr,iend
            if (A(i,kbl).lt.2. .and. Rib(i,k).gt.Ricr) then
              A(i,zbl)=        ( z_r(i,j,k+1)*(Rib(i,k)-Ricr)
     &                           +z_r(i,j,k)*(Ricr-Rib(i,k+1))
     &                                 )/(Rib(i,k)-Rib(i,k+1))
              A(i,kbl)=float(k)+eps
            endif
          enddo   
        enddo


        do i=istr,iend
          k=int(A(i,kbl))
          A(i,Bfsfc_bl)=Bfsfc(i,k)
        enddo


        do i=istr,iend
          if (A(i,Bfsfc_bl).gt.0.) then
            hekman=cekman*A(i,ustar)/max(abs(f(i,j)),eps)
            hmonob=cmonob*A(i,ustar)**3/(vonKar*A(i,Bfsfc_bl))

            A(i,zbl)=z_w(i,j,N) - min( z_w(i,j,N)-A(i,zbl),
     &                                       hekman,hmonob)
          endif
          A(i,kbl)=float(N)+eps  !<-- initialize search
        enddo
                                 ! Find new boundary
        do k=N-1,1,-1            ! layer index "kbl".
          do i=istr,iend
            if (z_w(i,j,k) .gt. A(i,zbl)) A(i,kbl)=float(k)+eps
          enddo
        enddo



























#ifdef JUNK

          do i=istr,iend
/*
            FC(i,k-1)=FC(i,k) + Hz(i,j,k)*( 0.375D0*( du(i,k)**2
     &                   +du(i,k-1)**2  +dv(i,k)**2+dv(i,k-1)**2)
     &             +0.25D0*(du(i,k)*du(i,k-1)+dv(i,k)*dv(i,k-1)))

            if (Bfsfc(i,k-1) .gt. 0.D0) then
c              FC(i,k-1)=FC(i,k-1) - Hz(i,j,k)*A(i,Ekm)
            endif
*/
            dpth=z_w(i,j,N)-z_w(i,j,k-1)
            zt=Bfsfc(i,k-1) * min(A(i,srf_dpth),dpth) * A(i,uscl3)

            if (zt .ge. 0.D0) then           !--> stable regime
              ws=1.+5.*zt
              pwr=-1.D0
            elseif (zt .gt. zetas) then      !--> unstable regime
              ws=1.-16.*zt
              pwr=0.5D0
            else                             !--> convective regime
              ws=as-cs*zt
              pwr=0.333333333333D0
            endif
            ws=vonKar*A(i,ustar) * ws**pwr
/*
            if (k.gt.1) then
              rref=0.5D0*Hz_inv(i,k-1)*( Hz(i,j,k-1)*rho1(i,j,k)
     &                                   +Hz(i,j,k)*rho1(i,j,k-1))
     &                           -0.0625D0*(dRdz(i,k)-dRdz(i,k-1))
     &                                    *(Hz(i,j,k-1)+Hz(i,j,k))

              Vterm=1.5D0*dr(i,k-1)-0.25D0*(dRdz(i,k)+dRdz(i,k-1))
            else
              rref=rho1(i,j,1)  
              Vterm=0.D0
            endif
*/
c**
            km=max(k-1,1)

            FC(i,k-1)=0.25*((u(i  ,j,N,nstp)-0.5*(u(i  ,j,k ,nstp)
     &                                           +u(i  ,j,km,nstp))
     &                      +u(i+1,j,N,nstp)-0.5*(u(i+1,j,k ,nstp)
     &                                           +u(i+1,j,km,nstp))
     &               )**2 +( v(i,j  ,N,nstp)-0.5*(v(i,j  ,k ,nstp)
     &                                           +v(i,j  ,km,nstp))
     &                      +v(i,j+1,N,nstp)-0.5*(v(i,j+1,k ,nstp)
     &                                           +v(i,j+1,k ,nstp))
     &               )**2)/(z_w(i,j,N)-z_w(i,j,k-1))

            rref=0.5D0*(rho1(i,j,k)+rho1(i,j,km))

            Vterm=2.*(rho1(i,j,k)-rho1(i,j,km))/(Hz(i,j,k)+Hz(i,j,km))




c**




            strat=grho0Ri*( rref -rho1(i,j,N) )
            Vterm=Vtc*ws*sqrt( max( 0.D0, -grho0*Vterm ))
            Cr(i,k-1)=FC(i,k-1)+Vterm - strat

            if (A(i,kbl).lt.0.D0 .and. Cr(i,k-1).lt.0.D0)  A(i,kbl)=k
            if (A(i,kmo).lt.0.D0 .and. dpth*Bfsfc(i,k-1)
     &                                    .gt. A(i,h_MO))  A(i,kmo)=k
          enddo
        enddo

!
! Find extent of boundary layer
!
        do i=istr,iend
          k=int(A(i,kbl)+0.5D0)
          if (k.gt.0) then
            A(i,zbl)=(z_w(i,j,k)*Cr(i,k-1)-z_w(i,j,k-1)*Cr(i,k))
     &                                       /(Cr(i,k-1)-Cr(i,k))


            if (Cr(i,k-1)*Cr(i,k) .gt. 0.) write(*,*) k 

          else
            A(i,zbl)=z_w(i,j,0)
            A(i,kbl)=1
          endif
        enddo
!                                             C_MO * ustar^3
! Monin-Obukhov limitation depth: h_MO(z) = ------------------
!                                             vonKar*Bfsfc(z)
        do i=istr,iend
          k=int(A(i,kmo)+0.5D0)
          if (k.gt.0) then
            cff=A(i,h_MO)
            cff1=max(0.D0, Bfsfc(i,k-1))
            cff2=max(0.D0, Bfsfc(i,k  ))
            A(i,h_MO)=z_w(i,j,N)
     &          - cff*(cff2*z_w(i,j,k)-cff1*z_w(i,j,k-1))
     &              /( cff1*cff2*(z_w(i,j,k)-z_w(i,j,k-1))
     &                                 + cff*(cff1-cff2) )
            if (A(i,zbl) .lt. A(i,h_MO)) then

c          write(*,*) A(i,zbl), A(i,h_MO)

              A(i,zbl)=A(i,h_MO)
              A(i,kbl)=A(i,kmo)



            endif
          endif
        enddo
!
!------------
!
#endif






# ifdef LMD_RIMIX
# define Rig uc
        do k=1,N-1
          do i=istr,iend
            cff=0.5/(z_r(i,j,k+1)-z_r(i,j,k))
            dudz=cff*( u(i  ,j,k+1,tind)-u(i  ,j,k,tind)
     &                +u(i+1,j,k+1,tind)-u(i+1,j,k,tind))
            dvdz=cff*( v(i,j  ,k+1,tind)-v(i,j  ,k,tind)
     &                +v(i,j+1,k+1,tind)-v(i,j+1,k,tind))

            Rig(i,k)=bvf(i,j,k)/( Ri0*max(
     &                    dudz*dudz+dvdz*dvdz, 1.E-10 ))
c            uc(i,k)=-cff*dr(i,k)*Hz_inv(i,k)/max( du(i,k)**2
c     &                                      +dv(i,k)**2, eps)
#  ifdef VERT_SMOOTH_RI
          enddo
        enddo
        do i=istr,iend
          uc(i,N)=uc(i,N-1)
          uc(i,0)=uc(i,  1)
        enddo
        do k=1,N
          do i=istr,iend
            vc(i,k)=0.25*(uc(i,k)+uc(i,k-1))
          enddo
        enddo
        do k=1,N-1
          do i=istr,iend
            uc(i,k)=vc(i,k)+vc(i,k+1)
#  endif
            cff=min(1., max(0., Rig(i,k)))
            cff=( 1.D0-cff*cff )**3
# undef Rig
            Kv(i,k)=nuwm +nu0m*cff     ! vertical shear instability
            Kt(i,k)=nuws +nu0s*cff     ! and internal wave breaking
#  ifdef SALINITY
            Ks(i,k)=Kt(i,k)
#  endif
#  ifdef LMD_NONLOCAL
            ghat(i,j,k)=0.D0
#  endif
          enddo
        enddo
# else
        do k=1,N-1
          do i=istr,iend
            Kv(i,k)=nuwm   ! internal wave breaking
            Kt(i,k)=nuws   ! mixing
#  ifdef SALINITY
            Ks(i,k)=Kt(i,k)
#  endif
#  ifdef LMD_NONLOCAL
            ghat(i,j,k)=0.D0
#  endif
          enddo
        enddo
# endif
        do i=istr,iend
          Kv(i,0)=Kv(i,  1)
          Kv(i,N)=Kv(i,N-1)
          Kt(i,0)=Kt(i,  1)
          Kt(i,N)=Kt(i,N-1)
          Ks(i,0)=Ks(i,  1)
          Ks(i,N)=Ks(i,N-1)
        enddo






!
!------------  
!
        do i=istr,iend
          k=int(A(i,kbl)+0.5D0)
          cff=1.D0/(z_w(i,j,k)-z_w(i,j,k-1))
          cff_up=cff*(A(i,zbl) -z_w(i,j,k-1))
          cff_dn=cff*(z_w(i,j,k)   -A(i,zbl))

          A(i,Bfsfc_bl)=cff_up*Bfsfc(i,k)+cff_dn*Bfsfc(i,k-1)
          dpth=z_w(i,j,N)-A(i,zbl)
          A(i,srf_dpth)=dpth*epssfc

          zt=A(i,Bfsfc_bl) * A(i,srf_dpth) * A(i,uscl3)

          if (zt .ge. 0.D0) then           !--> stable regime
            wm=1.+5.*zt
            pwr=-1.D0
          elseif (zt .gt. zetam) then      !--> unstable regime
            wm=1.-16.*zt
            pwr=0.25D0
          else
            wm=am-cm*zt
            pwr=0.333333333333D0
          endif
          wm=vonKar*ustar * wm**pwr

          if (zt .ge. 0.D0) then           !--> stable regime
            ws=1.+5.*zt
            pwr=-1.D0
          elseif (zt .gt. zetas) then      !--> unstable regime
            ws=1.-16.*zt
            pwr=0.5D0
          else                             !--> convective regime
            ws=as-cs*zt
            pwr=0.333333333333D0
          endif
          ws=vonKar*A(i,ustar) * ws**pwr
        
          f1=5.D0*vonKar * A(i,Bfsfc_bl)/(A(i,ustar)**4)

          Av_bl=cff_up*Kv(i,k)+cff_dn*Kv(i,k-1)
          dAv_bl=cff * (Kv(i,k)  -   Kv(i,k-1))
          A(i,Gm1)=Av_bl/(dpth*wm+eps)
          A(i,dGm1dS)=min(0.D0, Av_bl*f1-dAv_bl/(wm+eps))

          At_bl=cff_up*Kt(i,k)+cff_dn*Kt(i,k-1)
          dAt_bl=cff * (Kt(i,k)  -   Kt(i,k-1))
          A(i,Gt1)=At_bl/(dpth*ws+eps)
          A(i,dGt1dS)=min(0.D0, At_bl*f1-dAt_bl/(ws+eps))

# ifdef SALINITY
          As_bl=cff_up*Ks(i,k)+cff_dn*Ks(i,k-1)
          dAs_bl=cff * (Ks(i,k)  -   Ks(i,k-1))
          A(i,Gs1)=As_bl/(dpth*ws+eps)
          A(i,dGs1dS)=min(0.D0, As_bl*f1-dAs_bl/(ws+eps))
# endif
        enddo
!
! Set mixing coefficients within boundary layer...
!


        do i=istr,iend
          km=int(A(i,kbl)+0.5D0) 
          hbld=z_w(i,j,N)-A(i,zbl)
          hbl_inv=1.D0/hbld
          
          do k=N-1,km,-1
            dpth=z_w(i,j,N)-z_w(i,j,k)
            zt=A(i,Bfsfc_bl) * min(A(i,srf_dpth),dpth) * A(i,uscl3)

            if (zt .ge. 0.D0) then           !--> stable regime
              wm=1.+5.*zt
              pwr=-1.D0
            elseif (zt .gt. zetam) then      !--> unstable regime
              wm=1.-16.*zt
              pwr=0.25D0
            else
              wm=am-cm*zt
              pwr=0.333333333333D0
            endif
            wm=vonKar*ustar * wm**pwr
 
            if (zt .ge. 0.D0) then           !--> stable regime
              ws=1.+5.*zt
              pwr=-1.D0
            elseif (zt .gt. zetas) then      !--> unstable regime
              ws=1.-16.*zt
              pwr=0.5D0
            else                             !--> convective regime
              ws=as-cs*zt
              pwr=0.333333333333D0
            endif
            ws=vonKar*A(i,ustar) * ws**pwr

            sigma=dpth*hbl_inv
            a1=sigma-2.
            a2=3.-2.*sigma
            a3=sigma-1.

            if (sigma.lt.0.07D0) then
              patch=0.5*(sigma-0.07D0)**2/0.07D0
            else
              patch=0.D0
            endif

            Kv(i,k)=wm*hbld*( patch + sigma*( 1.D0+sigma*( a1 + a2
     &                               *A(i,Gm1) + a3*A(i,dGm1dS) )))

            Kt(i,k)=ws*hbld*( patch + sigma*( 1.D0+sigma*( a1 + a2
     &                               *A(i,Gt1) + a3*A(i,dGt1dS) )))
# ifdef SALINITY
            Ks(i,k)=ws*hbld*( patch + sigma*( 1.D0+sigma*( a1 + a2
     &                               *A(i,Gs1) + a3*A(i,dGs1dS) )))
# endif
!
! Compute boundary layer nonlocal transport [m/s^2]
!
# ifdef LMD_NONLOCAL
            if (A(i,Bfsfc_bl) .lt. 0.D0) then
              ghat(i,j,k)=Cg/max(ws*hbld, eps)
            endif
# endif
          enddo
        enddo


# ifdef LMD_CONVEC
        do k=1,N-1
          do i=istr,iend 
            if (dr(i,k).lt.0.D0 .and. z_w(i,j,k).lt.A(i,zbl)) then
              Kv(i,k)=Kv(i,k) +nu0c
              Kt(i,k)=Kt(i,k) +nu0c
#  ifdef SALINITY
              Ks(i,k)=Ks(i,k) +nu0c
#  endif 
            endif
          enddo
        enddo
# endif

        do i=istr,iend
          hbl(i,j)=(z_w(i,j,N)-A(i,zbl))*rmask(i,j)
        enddo
        do k=0,N
          do i=istr,iend
            Akv(i,j,k)=Kv(i,k)          *rmask(i,j)
            Akt(i,j,k,itemp)=Kt(i,k)    *rmask(i,j)
#  ifdef SALINITY
            Akt(i,j,k,isalt)=Ks(i,k)    *rmask(i,j)
#  endif
          enddo
        enddo

      enddo ! <-- j
      return
      end
#else
      subroutine lmd_kmix_empty
      end
#endif
       
