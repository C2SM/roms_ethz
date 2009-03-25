#include "cppdefs.h"

      subroutine step2d (tile)
      implicit none
      integer tile
#include "param.h"
#include "private_scratch.h"
#include "compute_tile_bounds.h"
      call step2D_FB_tile (Istr,Iend,Jstr,Jend, A2d(1,1),  A2d(1,2),
     &                    A2d(1, 3), A2d(1, 4), A2d(1, 5), A2d(1, 6),
     &                    A2d(1, 7), A2d(1, 8), A2d(1, 9), A2d(1,10),
     &                               A2d(1,11), A2d(1,12), A2d(1,13))
      return
      end
 
      subroutine step2D_FB_tile (Istr,Iend,Jstr,Jend, zeta_new,Dnew,
     &                           rubar,rvbar,  urhs,vrhs,  DUon,DVom,
     &                                        Drhs, UFx,UFe,VFx,VFe)
!
! Prform one time step for barotropic mode (free-surface and baro-
! tropic 2D momentum equations) using Generalized Forward-Backward
! AB3-AM4 algorithm. Also calculate fast-time averages to interact
! with baroclinic mode.
!
      implicit none
#include "param.h"
      integer Istr,Iend,Jstr,Jend, i,j, kbak, kold
      real zeta_new(PRIVATE_2D_SCRATCH_ARRAY),
     &         Dnew(PRIVATE_2D_SCRATCH_ARRAY),  cff,
     &        rubar(PRIVATE_2D_SCRATCH_ARRAY),  cff0,
     &        rvbar(PRIVATE_2D_SCRATCH_ARRAY),  cff1,
     &         urhs(PRIVATE_2D_SCRATCH_ARRAY),  cff2,
     &         vrhs(PRIVATE_2D_SCRATCH_ARRAY),  cff3,
     &         DUon(PRIVATE_2D_SCRATCH_ARRAY),
     &         DVom(PRIVATE_2D_SCRATCH_ARRAY),
     &         Drhs(PRIVATE_2D_SCRATCH_ARRAY),
     &          UFx(PRIVATE_2D_SCRATCH_ARRAY),
     &          UFe(PRIVATE_2D_SCRATCH_ARRAY),  DUnew,
     &          VFx(PRIVATE_2D_SCRATCH_ARRAY),  DVnew,
     &          VFe(PRIVATE_2D_SCRATCH_ARRAY)
#ifdef PSOURCE
      integer is
#endif
#include "grid.h"
#include "ocean2d.h"
#ifdef SOLVE3D
# include "coupling.h"
#else
# include "forces.h"
#endif
#include "mixing.h"
#include "climat.h"
#include "scalars.h"
#include "sources.h"
!
#include "compute_auxiliary_bounds.h"
!
! Preliminary step: compute total depth (meters) of the water
! ----------------- column and vertically integrated mass fluxes
! which are needed to compute divergence in rhs_zeta and as input
! data to compute nonlinear advection terms for the barotropic
! momentum equations.
!
      if (FIRST_2D_STEP) then         ! Meaning of temporal indices
        kbak=kstp                     ! ------- -- -------- -------
        kold=kstp                     ! m-2     m-1      m      m+1
        cff1=1.                       ! kold    kbak    kstp    knew
        cff2=0.
        cff3=0.
      elseif (FIRST_2D_STEP+1) then
        kbak=kstp-1
        if (kbak.lt.1) kbak=4
        kold=kbak

cxx     cff1= 1.5
cxx     cff2=-0.5
cxx     cff3= 0.

        cff1=1.                  ! Logically AB2-AM3 forward-backward 
        cff2=0.                  ! scheme with coefficients chosen for
        cff3=0.                  ! maximum stability ... (see below)
      else
        kbak=kstp-1
        if (kbak.lt.1) kbak=4
        kold=kstp-2
        if (kold.lt.1) kold=4

czz     cff1= 1.72
czz     cff2=-0.94
czz     cff3= 0.22

        cff1= 1.781105
        cff2=-1.06221
        cff3= 0.281105
      endif
 
      do j=JstrV-2,Jend+1
        do i=IstrU-2,Iend+1
          Drhs(i,j)=h(i,j)+cff1*zeta(i,j,kstp)+cff2*zeta(i,j,kbak)
     &                                         +cff3*zeta(i,j,kold)
        enddo
      enddo
 
      do j=Jstr-1,Jend+1
        do i=IstrU-1,Iend+1
          urhs(i,j)=cff1*ubar(i,j,kstp) +cff2*ubar(i,j,kbak)
     &                                         +cff3*ubar(i,j,kold)
          DUon(i,j)=0.5*(Drhs(i,j)+Drhs(i-1,j))*dn_u(i,j)*urhs(i,j)
        enddo
      enddo
      do j=JstrV-1,Jend+1
        do i=Istr-1,Iend+1
          vrhs(i,j)=cff1*vbar(i,j,kstp) +cff2*vbar(i,j,kbak)
     &                                         +cff3*vbar(i,j,kold)
          DVom(i,j)=0.5*(Drhs(i,j)+Drhs(i,j-1))*dm_v(i,j)*vrhs(i,j)
        enddo
      enddo
 
#ifdef OBC_VOLCONS
      call set_DUV_bc_tile (Istr,Iend,Jstr,Jend, Drhs, DUon,DVom)
#endif
!
! Advance free-surface:   Compute zeta_new, which is at new time
!-------- ---- --------   step, and interpolate half-step backward
! for the subsequent computation of barotropic pressure-gradient
! terms. It should be noted that because Forward Euler step is used 
! to update zeta during the first barotropic step, the pressure term  
! must be computed via Backward step to keep it numerically stable.
! However, this would interfere with the computation of forcing terms
! "rufrc,rvfrc" because computation of pressure gradient in 3D mode
! uses exactly the initial value of "zeta", rather than value changed
! by one barotropic time step.  To resolve this conflict, the
! pressure gradient term computation during the first barotropic
! step is computed in two stages: first use just zeta(:,:,kstp) to
! insure exact consistency with 3D mode; then, after "rufrc,rvfrc"
! are finalized, add a correction term based on the difference
! zeta_new(:,:)-zeta(:,:,kstp) to "rubar,rvbar" to make them
! consistent with Backward step for pressure gradient terms.
!
      if (FIRST_2D_STEP) then
#ifdef SOLVE3D
        cff0=0.                !---> Compute pressure-gradient
        cff1=1.                !  terms using just zeta(:,:,kstp)
#else
        cff0=1.
        cff1=0.
#endif
        cff2=0.
        cff3=0.
      elseif (FIRST_2D_STEP+1) then
cxx     cff0=0.779
cxx     cff1=-0.058
cxx     cff2=0.279
cxx     cff3=0.

        cff0= 1.0833333333333  ! Logically AB2-AM3 forward-backward
        cff1=-0.1666666666666  ! scheme with coefficients chosen for
        cff2= 0.0833333333333  ! maximum stability, while maintaining
        cff3=0.                ! third-accuracy; alpha_max=1.73

      else
czz     cff0=0.6133333333333
czz     cff1=0.2821333333333
czz     cff2=0.0957333333333
czz     cff3=0.0088

        cff0=0.614
        cff1=0.285
        cff2=0.088
        cff3=0.013
      endif
 
#define zwrk UFx
#define rzeta  UFe
#define rzeta2  VFe
#define rzetaSA VFx
 
      do j=JstrV-1,Jend
        do i=IstrU-1,Iend
          zeta_new(i,j)=zeta(i,j,kstp) + dtfast*pm(i,j)*pn(i,j)
     &            *(DUon(i,j)-DUon(i+1,j)+DVom(i,j)-DVom(i,j+1))
#ifdef MASKING
          zeta_new(i,j)=zeta_new(i,j)*rmask(i,j)
#endif
          Dnew(i,j)=zeta_new(i,j)+h(i,j)
 
          zwrk(i,j)=cff0*zeta_new(i,j) +cff1*zeta(i,j,kstp)
     &             +cff2*zeta(i,j,kbak) +cff3*zeta(i,j,kold)
 
#if defined VAR_RHO_2D && defined SOLVE3D
          rzeta(i,j)=(1.+rhoS(i,j))*zwrk(i,j)
          rzeta2(i,j)=rzeta(i,j)*zwrk(i,j)
          rzetaSA(i,j)=zwrk(i,j)*(rhoS(i,j)-rhoA(i,j))
#else
          rzeta(i,j)=zwrk(i,j)
          rzeta2(i,j)=zwrk(i,j)*zwrk(i,j)
#endif
        enddo
      enddo
!
! Set boundary conditions for the free-surface, after which copy
! the newly computed free-surface zeta_new into shared array.
!
      call zetabc_tile (Istr,Iend,Jstr,Jend, zeta_new)

      do j=JstrR,JendR
        do i=IstrR,IendR
          zeta(i,j,knew)=zeta_new(i,j)
        enddo
      enddo
!
! Compute time averaged fields over all short timesteps.
!-------- ---- -------- ------ ---- --- ----- ----------
! Reset/initialise arrays for averaged fields during the first
! barotropic time step; Accumulate averages after that. Include
! physical boundary points, but not periodic ghost points or
! computation  MPI computational margins.
!
#ifdef SOLVE3D
        cff1=weight(1,iif)
        cff2=weight(2,iif)
 
        if (FIRST_2D_STEP) then
          do j=JstrR,JendR
            do i=IstrR,IendR
              Zt_avg1(i,j)=cff1*zeta(i,j,knew)
              DU_avg1(i,j)=0.
              DV_avg1(i,j)=0.
              DU_avg2(i,j)=cff2*DUon(i,j)
              DV_avg2(i,j)=cff2*DVom(i,j)
            enddo
          enddo
        else
          do j=JstrR,JendR
            do i=IstrR,IendR
              Zt_avg1(i,j)=Zt_avg1(i,j)+cff1*zeta(i,j,knew)
              DU_avg2(i,j)=DU_avg2(i,j)+cff2*DUon(i,j)
              DV_avg2(i,j)=DV_avg2(i,j)+cff2*DVom(i,j)
            enddo
          enddo
        endif
#endif
!
! Compute pressure-gradient terms  NOTE that "rubar" and "rvbar"
!-------- -------- -------- -----  are computed within the same
! fused loop despite the fact that their normal index ranges are
! different. Fusing loops causes redundant computation of one
! column of "rubar" on the western physical boundary and one row
! of "rvbar" on the southern, but, at the same time it allows to
! share references to array elements (i,j) which results in an
! increase of computational density by almost a factor of 1.5
! resulting in overall more efficient code pipelined in 26 cycles
! (61% of peak speed) on R10000 vs. 16+16 cycles of separate loop
! version for the case when both CPP switches below are defined.
!
      cff=0.5*g
      do j=Jstr,Jend
        do i=Istr,Iend
          rubar(i,j)=cff*dn_u(i,j)*( (h(i-1,j)+h(i,j))*(rzeta(i-1,j)
     &                        -rzeta(i,j)) +rzeta2(i-1,j)-rzeta2(i,j)
 
#if defined VAR_RHO_2D && defined SOLVE3D
     &              +(h(i-1,j)-h(i,j))*( rzetaSA(i-1,j)+rzetaSA(i,j)
     &                        +0.333333333333*(rhoA(i-1,j)-rhoA(i,j))
     &                                     *(zwrk(i-1,j)-zwrk(i,j)) )
#endif
     &                                                              )
!> 
          rvbar(i,j)=cff*dm_v(i,j)*( (h(i,j-1)+h(i,j))*(rzeta(i,j-1)
     &                        -rzeta(i,j)) +rzeta2(i,j-1)-rzeta2(i,j)
 
#if defined VAR_RHO_2D && defined SOLVE3D
     &              +(h(i,j-1)-h(i,j))*( rzetaSA(i,j-1)+rzetaSA(i,j)
     &                        +0.333333333333*(rhoA(i,j-1)-rhoA(i,j))
     &                                     *(zwrk(i,j-1)-zwrk(i,j)) )
#endif
     &                                                              )
        enddo
      enddo            !--> discard  zwrk, rzeta, rzeta2, rzetaSA

#undef rzetaSA
#undef rzeta2
#undef rzeta
#undef zwrk
!
! Suppress computation of advection and lateral viscous terms in
! the case of 3D model configuration. Note that this does not mean
! omission of these terms completely: they are already implicitly
! present within the baroclinic-barotropic coupling terms "rufrc"
! and "rvfrc".  This merely means that advection and viscous terms
! are frozen during barotropic mode time stepping.
!
#ifdef SOLVE3D
c---># undef UV_ADV
# undef UV_VIS2
#endif
!
! Compute horizontal advection terms for momentum equations (2D only)
!-------- ---------- --------- ----- --- -------- --------- --- -----
! NOTE: mathematically necessary (minimal) index ranges for momentum-
! flux components are 
!
!      UFx(IstrU-1:Iend,Jstr:Jend)   VFx(Istr:Iend+1,JstrV:Jend)
!      UFe(IstrU:Iend,Jstr:Jend+1)   VFe(Istr,Iend,JstrV-1,Jend)
!
! however, for the purpose computational efficiency, these ranges are
! unified by suppressing U,V-suffices in order to allow fusion of the
! consecutive loops. This leads to slight increase of the redundant
! computations near western and southern boundaries in non-periodic
! directions. 
!
#ifdef UV_ADV
      do j=Jstr,Jend
        do i=Istr-1,Iend
          UFx(i,j)=0.25*(DUon(i,j)+DUon(i+1,j))
     &                     *(urhs(i,j)+urhs(i+1,j))

          VFx(i+1,j)=0.25*(DUon(i+1,j)+DUon(i+1,j-1))
     &                       *(vrhs(i+1,j)+vrhs(i,j))
# ifdef MASKING
     &                                 *pmask(i+1,j)
# endif
        enddo
      enddo
      do j=Jstr-1,Jend
        do i=Istr,Iend
          VFe(i,j)=0.25*(DVom(i,j)+DVom(i,j+1))
     &                      *(vrhs(i,j)+vrhs(i,j+1))

          UFe(i,j+1)=0.25*(DVom(i,j+1)+DVom(i-1,j+1))
     &                       *(urhs(i,j+1)+urhs(i,j))
# ifdef MASKING
     &                                 *pmask(i,j+1)
# endif
        enddo
      enddo
      do j=Jstr,Jend
        do i=Istr,Iend
          rubar(i,j)=rubar(i,j)-UFx(i,j)+UFx(i-1,j)
     &                         -UFe(i,j+1)+UFe(i,j)

          rvbar(i,j)=rvbar(i,j)-VFx(i+1,j)+VFx(i,j)
     &                         -VFe(i,j)+VFe(i,j-1)
        enddo
      enddo !--> discard UFx,VFe,UFe,VFx, DUon,DVom
#endif
!
! Compute Coriolis (2D and 3D) term and advective curvilinear metric
! terms (2D only).
!
# if defined UV_COR || (defined CURVGRID && defined UV_ADV)
      do j=JstrV-1,Jend
        do i=IstrU-1,Iend
          cff=Drhs(i,j)*(
#  ifdef UV_COR
     &                             fomn(i,j)
#  endif
#  if (defined CURVGRID && defined UV_ADV)
     &  +0.5*( dndx(i,j)*(vrhs(i,j)+vrhs(i,j+1))
     &        -dmde(i,j)*(urhs(i,j)+urhs(i+1,j)))
#  endif
     &                                          )
          UFx(i,j)=cff*(vrhs(i,j)+vrhs(i,j+1))
          VFe(i,j)=cff*(urhs(i,j)+urhs(i+1,j))
        enddo
      enddo
      do j=Jstr,Jend
        do i=IstrU,Iend
          rubar(i,j)=rubar(i,j)+0.25*(UFx(i,j)+UFx(i-1,j))
        enddo
      enddo
      do j=JstrV,Jend
        do i=Istr,Iend
          rvbar(i,j)=rvbar(i,j)-0.25*(VFe(i,j)+VFe(i,j-1))
        enddo
      enddo
# endif
!
! Compute horizontal viscous stress terms (2D only).
!
# ifdef UV_VIS2
      do j=Jstr-1,Jend
        do i=Istr-1,Iend
          cff=2.*Drhs(i,j)*visc2_r(i,j)
          UFx(i,j)=cff*(ubar(i+1,j,kstp)-ubar(i,j,kstp))
     &                                 *pm(i,j)*dn_r(i,j)
          VFe(i,j)=cff*(vbar(i,j+1,kstp)-vbar(i,j,kstp))
     &                                 *pn(i,j)*dm_r(i,j)


          cff1=0.0625*visc2_p(i+1,j+1)*( Drhs(i,j)
     &       +Drhs(i+1,j)+Drhs(i,j+1)+Drhs(i+1,j+1) )*(

     &          (pn(i+1,j+1)+pn(i,j+1)+pn(i+1,j)+pn(i,j))
     &             *(ubar(i+1,j+1,kstp)-ubar(i+1,j,kstp))

     &         +(pm(i+1,j+1)+pm(i,j+1)+pm(i+1,j)+pm(i,j))
     &             *(vbar(i+1,j+1,kstp)-vbar(i,j+1,kstp))
     &                                                  )
#  ifdef MASKING
     &                     *pmask(i+1,j+1)
#  endif

          UFe(i+1,j+1)=cff1*dm_p(i+1,j+1)
          VFx(i+1,j+1)=cff1*dn_p(i+1,j+1)
        enddo
      enddo
      do j=Jstr,Jend
        do i=Istr,Iend
          rubar(i,j)=rubar(i,j)+UFx(i,j)-UFx(i-1,j)
     &                         +UFe(i,j+1)-UFe(i,j)

          rvbar(i,j)=rvbar(i,j)+VFx(i+1,j)-VFx(i,j)
     &                         +VFe(i,j)-VFe(i,j-1)
        enddo
      enddo
# endif /* UV_VIS2 */
!
! Bottom drag: linear and/or quadratic law. 
!
#ifdef SOLVE3D
      do j=Jstr,Jend        
        do i=Istr,Iend
          rubar(i,j)=rubar(i,j) - 0.5*(r_D(i,j)+r_D(i-1,j))
     &                  *dm_u(i,j)*dn_u(i,j)*ubar(i,j,kstp)

          rvbar(i,j)=rvbar(i,j) - 0.5*(r_D(i,j)+r_D(i,j-1))
     &                  *dm_v(i,j)*dn_v(i,j)*vbar(i,j,kstp)
        enddo
      enddo
#else
# ifndef LINEAR_DRAG_ONLY
#  define rd VFx
      if (rdrg2 .gt. 0.) then
        do j=JstrV-1,Jend
          do i=IstrU-1,Iend
            rd(i,j)=rdrg + rdrg2*sqrt( 0.333333333333*(
     &              ubar(i,j,kstp)**2 +ubar(i+1,j,kstp)**2
     &                    +ubar(i,j,kstp)*ubar(i+1,j,kstp)
     &              +vbar(i,j,kstp)**2+vbar(i,j+1,kstp)**2
     &                    +vbar(i,j,kstp)*vbar(i,j+1,kstp)
     &                                                 ))
          enddo
        enddo
        do j=Jstr,Jend
          do i=IstrU,Iend
            rubar(i,j)=rubar(i,j) - 0.5*(rd(i,j)+rd(i-1,j))
     &                  *dm_u(i,j)*dn_u(i,j)*ubar(i,j,kstp)

            rvbar(i,j)=rvbar(i,j) - 0.5*(rd(i,j)+rd(i,j-1))
     &                  *dm_v(i,j)*dn_v(i,j)*vbar(i,j,kstp)
          enddo
        enddo
      else if (rdrg .gt. 0.) then
#else
      if (rdrg .gt. 0.) then
#endif
        do j=Jstr,Jend
          do i=IstrU,Iend
            rubar(i,j)=rubar(i,j) - rdrg*dm_u(i,j)*dn_u(i,j)
     &                                       *ubar(i,j,kstp)

            rvbar(i,j)=rvbar(i,j) - rdrg*dm_v(i,j)*dn_v(i,j)
     &                                       *vbar(i,j,kstp)    
          enddo
        enddo
      endif
# undef rd
#endif
!
! Coupling between 2D and 3D parts.
!--------- ------- -- --- -- ------
! Before the predictor step of the first barotropic time step
! arrays "rufrc" and "rvfrc" contain vertically integrals of the
! 3D right-hand-side terms for the momentum equations (including
! surface and bottom stresses, if so prescribed).
!
! During the first barotropic time step connvert them into forcing
! terms by subtracting the fast-time "rubar" and "rvbar" from them;
! These forcing terms are then extrapolated forward in time using
! optimized Adams-Bashforth weights, so that the resultant rufrc
! and rvfrc are centered effectively at time n+1/2. From now on,
! these newly computed forcing terms will remain constant during
! the fast time stepping and will added to "rubar" and "rvbar"
! during all subsequent barotropic time steps.
!
#ifdef SOLVE3D
      if (FIRST_2D_STEP) then
# ifdef PRED_COUPLED_MODE
        if (FIRST_TIME_STEP) then
          cff3=0.                        ! This version is designed
          cff2=0.                        ! for coupling during 3D
          cff1=1.                        ! predictor sub-step: here
        elseif (FIRST_TIME_STEP+1) then  ! forcing term "rufrc" is
          cff3=0.                        ! computed as instantaneous
          cff2=-0.5                      ! value at 3D time step
          cff1=1.5                       ! "nstp" first, and then
        else                             ! extrapolated half-step
          cff3=0.281105                  ! forward using  AM3-like
          cff2=-0.5-2.*cff3              ! weights optimized for
          cff1=1.5+cff3                  ! maximum stability (with
        endif                            ! special care for startup)

        do j=Jstr,Jend
          do i=IstrU,Iend
            cff=rufrc(i,j)-rubar(i,j)
            rufrc(i,j)=cff1*cff + cff2*rufrc_bak(i,j,3-nstp)
     &                             + cff3*rufrc_bak(i,j,nstp)
            rufrc_bak(i,j,nstp)=cff
          enddo
        enddo
        do j=JstrV,Jend
          do i=Istr,Iend
            cff=rvfrc(i,j)-rvbar(i,j)
            rvfrc(i,j)=cff1*cff + cff2*rvfrc_bak(i,j,3-nstp)
     &                             + cff3*rvfrc_bak(i,j,nstp)
            rvfrc_bak(i,j,nstp)=cff
          enddo
        enddo
# else
        do j=Jstr,Jend                       ! This version is
          do i=Istr,Iend                     ! designed for coupling
            rufrc(i,j)=rufrc(i,j)-rubar(i,j) ! during 3D corrector
            rvfrc(i,j)=rvfrc(i,j)-rvbar(i,j) ! sub-step: no forward
          enddo                              ! extrapolation needs
        enddo                                ! to be performed.
# endif
!
! Since coupling requires that pressure gradient term is computed
! using zeta(:,:,kstp) instead of zeta_new(:,:) needed to achieve
! numerical stability, apply compensation to shift pressure gradient
! terms from "kstp" to "knew": in essense, convert the fist 2D step
! from Forward Euler to Forward-Backward].
!
# define zwrk UFx
# define rzeta  UFe
# define rzeta2  VFe
# define rzetaSA VFx

        do j=JstrV-1,Jend
          do i=IstrU-1,Iend
            zwrk(i,j)=zeta_new(i,j)-zeta(i,j,kstp)
# if defined VAR_RHO_2D && defined SOLVE3D
            rzeta(i,j)=(1.+rhoS(i,j))*zwrk(i,j)
            rzeta2(i,j)=rzeta(i,j)*(zeta_new(i,j)+zeta(i,j,kstp))
            rzetaSA(i,j)=zwrk(i,j)*(rhoS(i,j)-rhoA(i,j))
# else
            rzeta(i,j)=zwrk(i,j)
            rzeta2(i,j)=zwrk(i,j)*(zeta_new(i,j)+zeta(i,j,kstp))
# endif
          enddo
        enddo

        cff=0.5*g
        do j=Jstr,Jend
          do i=Istr,Iend
            rubar(i,j)=rubar(i,j) +cff*dn_u(i,j)*( (h(i-1,j)+h(i,j))
     &          *(rzeta(i-1,j)-rzeta(i,j)) +rzeta2(i-1,j)-rzeta2(i,j)

# if defined VAR_RHO_2D && defined SOLVE3D
     &              +(h(i-1,j)-h(i,j))*( rzetaSA(i-1,j)+rzetaSA(i,j)
     &                        +0.333333333333*(rhoA(i-1,j)-rhoA(i,j))
     &                                     *(zwrk(i-1,j)-zwrk(i,j)) )
# endif
     &                                                              )
!>
            rvbar(i,j)=rvbar(i,j) +cff*dm_v(i,j)*( (h(i,j-1)+h(i,j))
     &          *(rzeta(i,j-1)-rzeta(i,j)) +rzeta2(i,j-1)-rzeta2(i,j)

# if defined VAR_RHO_2D && defined SOLVE3D
     &              +(h(i,j-1)-h(i,j))*( rzetaSA(i,j-1)+rzetaSA(i,j)
     &                        +0.333333333333*(rhoA(i,j-1)-rhoA(i,j))
     &                                     *(zwrk(i,j-1)-zwrk(i,j)) )
# endif
     &                                                              )
          enddo
        enddo            !--> discard  zwrk, rzeta, rzeta2, rzetaSA

# undef rzetaSA
# undef rzeta2
# undef rzeta
# undef zwrk
      endif   !<-- FIRST_2D_STEP
#endif
!
! Perform time step for the 2D momentum equations. Also compute
! fast-time averaged barotropic mass fluxes. Doing so on the fly
! yields a more computationally dense code and eliminates repeated   
! multiplication by Dnew (since mass fluxes are actually available
! as volatile variables DUnew,DVnew at this moment. However, as the
! result of this arrangement, a special code is needed to compute
! fast-time averages along the physical boundaries, which is done
! immediately below. 
!
#define Dstp DUon
 
      do j=JstrV-1,Jend
        do i=IstrU-1,Iend
          Dstp(i,j)=zeta(i,j,kstp)+h(i,j)
        enddo
      enddo
 
      cff=0.5*dtfast
#ifdef SOLVE3D
      cff1=0.5*weight(1,iif)
#else
      cff2=2.*dtfast
#endif
      do j=Jstr,Jend
        do i=IstrU,Iend
          DUnew=( (Dstp(i,j)+Dstp(i-1,j))*ubar(i,j,kstp)
     &        +cff*(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
#ifdef SOLVE3D
     &                             *(rubar(i,j)+rufrc(i,j))
#else
     &                          *rubar(i,j)+cff2*sustr(i,j)
#endif
     &                                                    )
#ifdef MASKING
     &                                         *umask(i,j)
#endif
          ubar(i,j,knew)=DUnew/(Dnew(i,j)+Dnew(i-1,j))
#ifdef SOLVE3D
          DU_avg1(i,j)=DU_avg1(i,j) +cff1*DUnew*dn_u(i,j)
#endif
        enddo
      enddo
      do j=JstrV,Jend
        do i=Istr,Iend
          DVnew=( (Dstp(i,j)+Dstp(i,j-1))*vbar(i,j,kstp)
     &        +cff*(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
#ifdef SOLVE3D
     &                             *(rvbar(i,j)+rvfrc(i,j))
#else
     &                          *rvbar(i,j)+cff2*svstr(i,j)
#endif
     &                                                    )
#ifdef MASKING
     &                                         *vmask(i,j)
#endif
          vbar(i,j,knew)=DVnew/(Dnew(i,j)+Dnew(i,j-1))
#ifdef SOLVE3D
          DV_avg1(i,j)=DV_avg1(i,j) +cff1*DVnew*dm_v(i,j)
#endif
        enddo
      enddo
!
! Set boundary conditions and compute integral mass flux accross
! all open boundaries, if any.
!
      call    u2dbc_tile (Istr,Iend,Jstr,Jend, UFx)
      call    v2dbc_tile (Istr,Iend,Jstr,Jend, UFx)
#ifdef OBC_VOLCONS
      call obc_flux_tile (Istr,Iend,Jstr,Jend, UFx)
#endif
!
! Compute fast-time averaged barotropic mass fluxes along physical
! boundaries.
!
#ifdef SOLVE3D
# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
        do j=Jstr-1,JendR
          Dnew(Istr-1,j)=h(Istr-1,j)+zeta_new(Istr-1,j)
        enddo
      endif
      if (EASTERN_EDGE) then
        do j=Jstr-1,JendR
          Dnew(Iend+1,j)=h(Iend+1,j)+zeta_new(Iend+1,j)
        enddo
      endif
# endif
# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        do i=Istr-1,IendR
          Dnew(i,Jstr-1)=h(i,Jstr-1)+zeta_new(i,Jstr-1)
        enddo
      endif
      if (NORTHERN_EDGE) then
        do i=Istr-1,IendR
          Dnew(i,Jend+1)=h(i,Jend+1)+zeta_new(i,Jend+1)
        enddo
      endif
# endif
      cff1=0.5*weight(1,iif)
# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
        do j=JstrR,JendR
          DU_avg1(IstrU-1,j)=DU_avg1(IstrU-1,j)+cff1*(Dnew(IstrU-1,j)
     &         +Dnew(IstrU-2,j))*ubar(IstrU-1,j,knew)*dn_u(IstrU-1,j)
        enddo
        do j=JstrV,Jend
          DV_avg1(Istr-1,j)=DV_avg1(Istr-1,j) +cff1*(Dnew(Istr-1,j)
     &       +Dnew(Istr-1,j-1) )*vbar(Istr-1,j,knew)*dm_v(Istr-1,j)
        enddo
      endif
      if (EASTERN_EDGE) then
        do j=JstrR,JendR
          DU_avg1(Iend+1,j)=DU_avg1(Iend+1,j) +cff1*( Dnew(Iend+1,j)
     &            +Dnew(Iend,j) )*ubar(Iend+1,j,knew)*dn_u(Iend+1,j)
        enddo
        do j=JstrV,Jend
          DV_avg1(Iend+1,j)=DV_avg1(Iend+1,j) +cff1*( Dnew(Iend+1,j)
     &        +Dnew(Iend+1,j-1) )*vbar(Iend+1,j,knew)*dm_v(Iend+1,j)
        enddo
      endif
# endif
# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        do i=IstrU,Iend
          DU_avg1(i,Jstr-1)=DU_avg1(i,Jstr-1) +cff1*( Dnew(i,Jstr-1)
     &        +Dnew(i-1,Jstr-1) )*ubar(i,Jstr-1,knew)*dn_u(i,Jstr-1)
        enddo
        do i=IstrR,IendR
          DV_avg1(i,JstrV-1)=DV_avg1(i,JstrV-1)+cff1*(Dnew(i,JstrV-1)
     &         +Dnew(i,JstrV-2))*vbar(i,JstrV-1,knew)*dm_v(i,JstrV-1)
        enddo
      endif
      if (NORTHERN_EDGE) then
        do i=IstrU,Iend
          DU_avg1(i,Jend+1)=DU_avg1(i,Jend+1) +cff1*( Dnew(i,Jend+1)
     &        +Dnew(i-1,Jend+1) )*ubar(i,Jend+1,knew)*dn_u(i,Jend+1)
        enddo
        do i=IstrR,IendR
          DV_avg1(i,Jend+1)=DV_avg1(i,Jend+1) +cff1*( Dnew(i,Jend+1)
     &            +Dnew(i,Jend) )*vbar(i,Jend+1,knew)*dm_v(i,Jend+1)
        enddo
      endif
# endif 
#endif
#ifdef PSOURCE
      do is=1,Nsrc           ! Apply point mass sources associated
        i=Isrc(is)           ! with river runoff simulations.
        j=Jsrc(is)
        if (IstrR.le.i .and. i.le.IendR .and.
     &      JstrR.le.j .and. j.le.JendR) then
          if (Dsrc(is).eq.0) then
            ubar(i,j,knew)=2.*Qbar(is)/( dn_u(i,j)
     &                   *(Dnew(i-1,j)+Dnew(i,j)) )
# ifdef SOLVE3D
            DU_avg1(i,j)=Qbar(is)
# endif
          else
            vbar(i,j,knew)=2.*Qbar(is)/( dm_v(i,j)
     &                   *(Dnew(i,j-1)+Dnew(i,j)) )
# ifdef SOLVE3D
            DV_avg1(i,j)=Qbar(is)
# endif
          endif
        endif
      enddo
#endif
!
! At the end of the last 2D step replace the new free-surface field
! "zeta(:,:,knew)" with its fast-time-averaged "_avg1" version: this
! is the only version of free surface which communicates with 3D part 
! via Zt_avg1 --> zeta(:,:,knew) --> z_w(:,:,N) in set_depth and it
! is also used as initial condition for "zeta" for fast-time stepping 
! during the next 3D-step.
!
#ifdef SOLVE3D
      if (iif.eq.nfast) then
        do j=JstrR,JendR
          do i=IstrR,IendR
            zeta(i,j,knew)=Zt_avg1(i,j)
          enddo
        enddo
        call set_depth_tile (Istr,Iend,Jstr,Jend, UFx)
      endif
#else
      call diag_tile (Istr,Iend,Jstr,Jend, UFx,UFe,VFx)
#endif

#if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                   zeta(START_2D_ARRAY,knew))
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,
     &                   ubar(START_2D_ARRAY,knew))
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,
     &                   vbar(START_2D_ARRAY,knew))
#endif
      return
      end
 
