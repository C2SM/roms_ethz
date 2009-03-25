#include "cppdefs.h"

c**   module step2d_mod
c**   contains

                                ! This is an experimental rewrite of
      subroutine step2d (tile)  ! the standard loop-syntax routine
      implicit none             ! into F90 vector notation syntax for
      integer tile              ! performance evaluation purposes
#include "param.h"
#include "private_scratch.h"
#include "compute_tile_bounds.h"
      call step2D_FB_tile (Istr,Iend,Jstr,Jend, A2d(1,1),  A2d(1,2),
     &                    A2d(1, 3), A2d(1, 4), A2d(1, 5), A2d(1, 6),
     &                    A2d(1, 7), A2d(1, 8), A2d(1, 9), A2d(1,10),
     &                    A2d(1,11), A2d(1,12), A2d(1,13), A2d(1,14))
      return
      end subroutine step2d
 
      subroutine step2D_FB_tile (Istr,Iend,Jstr,Jend, zeta_new,Dnew,
     &                             rubar,rvbar, urhs,vrhs, DUon,DVom,
     &                                Drhs, Drhs_p, UFx,UFe,VFx,VFe)
!
! Prform one time step for barotropic mode (free-surface and baro-
! tropic 2D momentum equations) using Generalized Forward-Backward
! AB3-AM4 algorithm. Also calculate fast-time averages to interact
! with baroclinic mode.
!
      implicit none
#include "param.h"
      integer Istr,Iend,Jstr,Jend, i,j, kbak, kold, is
      real zeta_new(PRIVATE_2D_SCRATCH_ARRAY),  cff,
     &         Dnew(PRIVATE_2D_SCRATCH_ARRAY),  cff0,
     &        rubar(PRIVATE_2D_SCRATCH_ARRAY),  cff1,
     &        rvbar(PRIVATE_2D_SCRATCH_ARRAY),  cff2,
     &         urhs(PRIVATE_2D_SCRATCH_ARRAY),
     &         vrhs(PRIVATE_2D_SCRATCH_ARRAY),
     &         DUon(PRIVATE_2D_SCRATCH_ARRAY),  cff3,
     &         DVom(PRIVATE_2D_SCRATCH_ARRAY),  cff4,
     &         Drhs(PRIVATE_2D_SCRATCH_ARRAY),  cff5,
     &       Drhs_p(PRIVATE_2D_SCRATCH_ARRAY),  cff6,
     &          UFx(PRIVATE_2D_SCRATCH_ARRAY),
     &          UFe(PRIVATE_2D_SCRATCH_ARRAY),  DUnew,
     &          VFx(PRIVATE_2D_SCRATCH_ARRAY),  DVnew,
     &          VFe(PRIVATE_2D_SCRATCH_ARRAY)
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
        kold=kstp                     ! m-2    m-1     m       m+1
        cff1=1.                       ! kold   kbak   kstp    knew
        cff2=0.
        cff3=0.
      elseif (FIRST_2D_STEP+1) then
        kbak=kstp-1
        if (kbak.lt.1) kbak=4
        kold=kbak
        cff1= 1.5
        cff2=-0.5
        cff3= 0.
      else
        kbak=kstp-1
        if (kbak.lt.1) kbak=4
        kold=kstp-2
        if (kold.lt.1) kold=4

c       cff1= 1.72
c       cff2=-0.94
c       cff3= 0.22

        cff1= 1.781105
        cff2=-1.06221
        cff3= 0.281105
      endif
 
          Drhs(IstrU-2:Iend+1,JstrV-2:Jend+1)=
     &                   h(IstrU-2:Iend+1,JstrV-2:Jend+1)
     &          +cff1*zeta(IstrU-2:Iend+1,JstrV-2:Jend+1,kstp)
     &          +cff2*zeta(IstrU-2:Iend+1,JstrV-2:Jend+1,kbak)
     &          +cff3*zeta(IstrU-2:Iend+1,JstrV-2:Jend+1,kold)
 

          urhs(IstrU-1:Iend+1,Jstr-1:Jend+1)=
     &            cff1*ubar(IstrU-1:Iend+1,Jstr-1:Jend+1,kstp)
     &           +cff2*ubar(IstrU-1:Iend+1,Jstr-1:Jend+1,kbak)
     &           +cff3*ubar(IstrU-1:Iend+1,Jstr-1:Jend+1,kold)

          DUon(IstrU-1:Iend+1,Jstr-1:Jend+1)=
     &                0.5*( Drhs(IstrU-1:Iend+1,Jstr-1:Jend+1)
     &                     +Drhs(IstrU-2:Iend  ,Jstr-1:Jend+1)
     &                    )*dn_u(IstrU-1:Iend+1,Jstr-1:Jend+1)
     &                     *urhs(IstrU-1:Iend+1,Jstr-1:Jend+1)


          vrhs(Istr-1:Iend+1,JstrV-1:Jend+1)=
     &            cff1*vbar(Istr-1:Iend+1,JstrV-1:Jend+1,kstp)
     &           +cff2*vbar(Istr-1:Iend+1,JstrV-1:Jend+1,kbak)
     &           +cff3*vbar(Istr-1:Iend+1,JstrV-1:Jend+1,kold)

          DVom(Istr-1:Iend+1,JstrV-1:Jend+1)=
     &                0.5*( Drhs(Istr-1:Iend+1,JstrV-1:Jend+1)
     &                     +Drhs(Istr-1:Iend+1,JstrV-2:Jend  )
     &                    )*dm_v(Istr-1:Iend+1,JstrV-1:Jend+1)
     &                     *vrhs(Istr-1:Iend+1,JstrV-1:Jend+1)


 
#ifdef OBC_VOLCONS
      call set_DUV_bc_tile (Istr,Iend,Jstr,Jend, Drhs, DUon,DVom)
#endif
 
      if (FIRST_2D_STEP) then
        cff0=1.
        cff1=0.
        cff2=0.
        cff3=0.
      elseif (FIRST_2D_STEP+1) then
        cff0=0.779
        cff1=-0.058
        cff2=0.279
        cff3=0.
      else
c       cff0=0.6133333333333
c       cff1=0.2821333333333
c       cff2=0.0957333333333
c       cff3=0.0088

        cff0=0.614
        cff1=0.285
        cff2=0.088
        cff3=0.013
      endif
 
#define zwrk UFx
#define rzeta  UFe
#define rzeta2  VFe
#define rzetaSA VFx
 

          zeta_new(IstrU-1:Iend,JstrV-1:Jend)=
     &       zeta(IstrU-1:Iend,JstrV-1:Jend,kstp)
     &       +dtfast*pm(IstrU-1:Iend,JstrV-1:Jend)
     &              *pn(IstrU-1:Iend,JstrV-1:Jend)
     &            *( DUon(IstrU-1:Iend,JstrV-1:Jend)
     &              -DUon(IstrU:Iend+1,JstrV-1:Jend)
     &              +DVom(IstrU-1:Iend,JstrV-1:Jend)
     &              -DVom(IstrU-1:Iend,JstrV:Jend+1)
     &                                             )
#ifdef MASKING
          zeta_new(IstrU-1:Iend,JstrV-1:Jend)=
     &           zeta_new(IstrU-1:Iend,JstrV-1:Jend)
     &             *rmask(IstrU-1:Iend,JstrV-1:Jend)
#endif
          Dnew(IstrU-1:Iend,JstrV-1:Jend)=
     &           zeta_new(IstrU-1:Iend,JstrV-1:Jend)
     &                 +h(IstrU-1:Iend,JstrV-1:Jend)
 
          zwrk(IstrU-1:Iend,JstrV-1:Jend)=
     &       cff0*zeta_new(IstrU-1:Iend,JstrV-1:Jend)
     &          +cff1*zeta(IstrU-1:Iend,JstrV-1:Jend,kstp)
     &          +cff2*zeta(IstrU-1:Iend,JstrV-1:Jend,kbak)
     &          +cff3*zeta(IstrU-1:Iend,JstrV-1:Jend,kold)
 
#if defined VAR_RHO_2D && defined SOLVE3D
          rzeta(IstrU-1:Iend,JstrV-1:Jend)=
     &          (1.+rhoS(IstrU-1:Iend,JstrV-1:Jend))
     &             *zwrk(IstrU-1:Iend,JstrV-1:Jend)

          rzeta2(IstrU-1:Iend,JstrV-1:Jend)=
     &             rzeta(IstrU-1:Iend,JstrV-1:Jend)
     &             *zwrk(IstrU-1:Iend,JstrV-1:Jend)

          rzetaSA(IstrU-1:Iend,JstrV-1:Jend)=
     &              zwrk(IstrU-1:Iend,JstrV-1:Jend)
     &           *( rhoS(IstrU-1:Iend,JstrV-1:Jend)
     &             -rhoA(IstrU-1:Iend,JstrV-1:Jend))
#else
          rzeta(IstrU-1:Iend,JstrV-1:Jend)=
     &              zwrk(IstrU-1:Iend,JstrV-1:Jend)
          rzeta2(IstrU-1:Iend,JstrV-1:Jend)=
     &              zwrk(IstrU-1:Iend,JstrV-1:Jend)
     &             *zwrk(IstrU-1:Iend,JstrV-1:Jend)
#endif





!
! Set boundary conditions for the free-surface, after which copy
! the newly computed free-surface zeta_new into shared array.
!
      call zetabc_tile (Istr,Iend,Jstr,Jend, zeta_new)

          zeta(IstrR:IendR,JstrR:JendR,knew)=
     &       zeta_new(IstrR:IendR,JstrR:JendR)
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
! fused loop despite the fact that their normal indexe ranges are
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

      rubar(Istr:Iend,Jstr:Jend)=cff*dn_u(Istr:Iend,Jstr:Jend)*(
     &       (h(Istr-1:Iend-1,Jstr:Jend)+h(Istr:Iend,Jstr:Jend))
     &   *(rzeta(Istr-1:Iend-1,Jstr:Jend)-rzeta(Istr:Iend,Jstr:Jend))
     &   +rzeta2(Istr-1:Iend-1,Jstr:Jend)-rzeta2(Istr:Iend,Jstr:Jend)
 
#if defined VAR_RHO_2D && defined SOLVE3D
     &              +(h(Istr-1:Iend-1,Jstr:Jend)-h(Istr:Iend,Jstr:Jend))*( rzetaSA(i-1,j)+rzetaSA(i,j)
     &                        +0.333333333333*(rhoA(i-1,j)-rhoA(i,j))
     &                                      *(zwrk(i-1,j)-zwrk(i,j)))
#endif
     &                                                              )

      rvbar(Istr:Iend,Jstr:Jend)=cff*dm_v(Istr:Iend,Jstr:Jend)*(
     &            (h(Istr:Iend,Jstr-1:Jend-1)+h(Istr:Iend,Jstr:Jend))
     &   *(rzeta(Istr:Iend,Jstr-1:Jend-1)-rzeta(Istr:Iend,Jstr:Jend))
     &   +rzeta2(Istr:Iend,Jstr-1:Jend-1)-rzeta2(Istr:Iend,Jstr:Jend)
 
#if defined VAR_RHO_2D && defined SOLVE3D
     &           +(h(Istr:Iend,Jstr-1:Jend-1)-h(Istr:Iend,Jstr:Jend))
     &    *( rzetaSA(Istr:Iend,Jstr-1:Jend-1)
     &             +rzetaSA(Istr:Iend,Jstr:Jend)
     &             +0.333333333333*(rhoA(Istr:Iend,Jstr-1:Jend-1)
     &               -rhoA(Istr:Iend,Jstr:Jend))
     &    *(zwrk(Istr:Iend,Jstr-1:Jend-1)-zwrk(Istr:Iend,Jstr:Jend)))
#endif
     &                                                              )
                     !--> discard  zwrk, rzeta, rzeta2, rzetaSA

#undef rzetaSA
#undef rzeta2
#undef rzeta
#undef zwrk
!
! Suppress computation of advection and viscous terms (lateral and
! bottom drag) in the case of 3D model configuration. Note that this
! does not mean omission of these terms: they are already implicitly
! present within the baroclinic-barotropic coupling terms "rufrc"
! and "rvfrc".  This merely means that advection and viscous terms
! are frozen during barotropic mode time stepping.
!
#ifdef SOLVE3D
# undef UV_ADV
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
        UFx(Istr-1:Iend,Jstr:Jend)=0.25*( DUon(Istr-1:Iend,Jstr:Jend)
     &                                   +DUon(Istr:Iend+1,Jstr:Jend)
     &                                )*( urhs(Istr-1:Iend,Jstr:Jend)
     &                                   +urhs(Istr:Iend+1,Jstr:Jend)
     &                                                              )

        VFx(Istr:Iend+1,Jstr:Jend)=0.25*( DUon(Istr:Iend+1,Jstr:Jend)
     &                               +DUon(Istr:Iend+1,Jstr-1:Jend-1)
     &                                )*( vrhs(Istr-1:Iend,Jstr:Jend)
     &                                   +vrhs(Istr:Iend+1,Jstr:Jend)
     &                                                              )

        VFe(Istr:Iend,Jstr-1:Jend)=0.25*( DVom(Istr:Iend,Jstr-1:Jend)
     &                                   +DVom(Istr:Iend,Jstr:Jend+1)
     &                                )*( vrhs(Istr:Iend,Jstr-1:Jend)
     &                                   +vrhs(Istr:Iend,Jstr:Jend+1)
     &                                                              )

        UFe(Istr:Iend,Jstr:Jend+1)=0.25*( DVom(Istr:Iend,Jstr:Jend+1)
     &                               +DVom(Istr-1:Iend-1,Jstr:Jend+1)
     &                                )*( urhs(Istr:Iend,Jstr-1:Jend)
     &                                   +urhs(Istr:Iend,Jstr:Jend+1)
     &                                                              )

        rubar(Istr:Iend,Jstr:Jend)=rubar(Istr:Iend,Jstr:Jend)
     &         -UFx(Istr:Iend,Jstr:Jend)+UFx(Istr-1:Iend-1,Jstr:Jend)
     &         -UFe(Istr:Iend,Jstr+1:Jend+1)+UFe(Istr:Iend,Jstr:Jend)

        rvbar(Istr:Iend,Jstr:Jend)=rvbar(Istr:Iend,Jstr:Jend)
     &         -VFx(Istr+1:Iend+1,Jstr:Jend)+VFx(Istr:Iend,Jstr:Jend)
     &         -VFe(Istr:Iend,Jstr:Jend)+VFe(Istr:Iend,Jstr-1:Jend-1)

                      !--> discard UFx,VFe,UFe,VFx, DUon,DVom
#endif
!
! Compute Coriolis (2D and 3D) term and advective curvilinear metric
! terms (2D only).
!
# if defined UV_COR || (defined CURVGRID && defined UV_ADV)

#define cff UFe

        cff(IstrU-1:Iend,JstrV-1:Jend)=
     &                Drhs(IstrU-1:Iend,JstrV-1:Jend)*(
#  ifdef UV_COR
     &                   fomn(IstrU-1:Iend,JstrV-1:Jend)
#  endif
#  if (defined CURVGRID && defined UV_ADV)
     &      +0.5*( dndx(IstrU-1:Iend,JstrV-1:Jend)
     &             *( vrhs(IstrU-1:Iend,JstrV-1:Jend)
     &               +vrhs(IstrU-1:Iend,JstrV:Jend+1))
     &            -dmde(IstrU-1:Iend,JstrV-1:Jend)
     &             *( urhs(IstrU-1:Iend,JstrV-1:Jend)
     &               +urhs(IstrU:Iend+1,JstrV-1:Jend)))
#  endif
     &                                              )
        UFx(IstrU-1:Iend,JstrV-1:Jend)=
     &                 cff(IstrU-1:Iend,JstrV-1:Jend)*(
     &                   vrhs(IstrU-1:Iend,JstrV-1:Jend)
     &                  +vrhs(IstrU-1:Iend,JstrV:Jend+1)
     &                                                 )
        VFe(IstrU-1:Iend,JstrV-1:Jend)=
     &                 cff(IstrU-1:Iend,JstrV-1:Jend)*(
     &                   urhs(IstrU-1:Iend,JstrV-1:Jend)
     &                  +urhs(IstrU:Iend+1,JstrV-1:Jend)
     &                                                 )

#undef cff

        rubar(IstrU:Iend,Jstr:Jend)=rubar(IstrU:Iend,Jstr:Jend)
     &                         +0.25*( UFx(IstrU:Iend,Jstr:Jend)
     &                           +UFx(IstrU-1:Iend-1,Jstr:Jend))

        rvbar(Istr:Iend,JstrV:Jend)=rvbar(Istr:Iend,JstrV:Jend)
     &                         -0.25*( VFe(Istr:Iend,JstrV:Jend)
     &                           +VFe(Istr:Iend,JstrV-1:Jend-1))
# endif
!
! Compute horizontal viscous stress terms (2D only).
!
# ifdef UV_VIS2
      do j=JstrV-1,Jend
        do i=IstrU-1,Iend
          cff=2.*Drhs(i,j)*visc2_r(i,j)
          UFx(i,j)=cff*(urhs(i+1,j)-urhs(i,j))*pm(i,j)*dn_r(i,j)
          VFe(i,j)=cff*(vrhs(i,j+1)-vrhs(i,j))*pn(i,j)*dm_r(i,j)
        enddo
      enddo
      do j=Jstr,Jend+1
        do i=Istr,Iend+1
          cff=visc2_p(i,j)*Drhs_p(i,j)*0.25*(
     &                 (pn(i,j)+pn(i-1,j)+pn(i,j-1)+pn(i-1,j-1))
     &                        *(urhs(i,j)-urhs(i,j-1))
     &                +(pm(i,j)+pm(i-1,j)+pm(i,j-1)+pm(i-1,j-1))
     &                        *(vrhs(i,j)-vrhs(i-1,j))
     &                                                         )
#  ifdef MASKING
     &                     *pmask(i,j)
#  endif
          UFe(i,j)=cff*dm_p(i,j)
          VFx(i,j)=cff*dn_p(i,j)
        enddo
      enddo
      do j=Jstr,Jend
        do i=IstrU,Iend
          rubar(i,j)=rubar(i,j)+UFx(i,j)-UFx(i-1,j)
     &                         +UFe(i,j+1)-UFe(i,j)
        enddo
      enddo
      do j=JstrV,Jend
        do i=Istr,Iend
          rvbar(i,j)=rvbar(i,j)+VFx(i+1,j)-VFx(i,j)
     &                         +VFe(i,j)-VFe(i,j-1)
        enddo
      enddo
# endif /* UV_VIS2 */
!
! Linear and/or quadratic bottom stress.
!
#ifndef SOLVE3D
      if (rdrg2.gt.0.) then
        do j=Jstr,Jend
          do i=IstrU,Iend
            cff=0.25*( vbar(i  ,j,kstp)+vbar(i  ,j+1,kstp)
     &                +vbar(i-1,j,kstp)+vbar(i-1,j+1,kstp))
 
            rubar(i,j)=rubar(i,j)-ubar(i,j,kstp)*( rdrg+rdrg2
     &              *sqrt(ubar(i,j,kstp)*ubar(i,j,kstp)+cff*cff)
     &                                     )*dm_u(i,j)*dn_u(i,j)
          enddo
        enddo
        do j=JstrV,Jend
          do i=Istr,Iend
            cff=0.25*( ubar(i,j  ,kstp)+ubar(i+1,j  ,kstp)
     &                +ubar(i,j-1,kstp)+ubar(i+1,j-1,kstp))
 
            rvbar(i,j)=rvbar(i,j)-vbar(i,j,kstp)*( rdrg+rdrg2
     &              *sqrt(cff*cff+vbar(i,j,kstp)*vbar(i,j,kstp))
     &                                     )*dm_v(i,j)*dn_v(i,j)
          enddo
        enddo
      else if (rdrg.gt.0.0) then
        do j=Jstr,Jend
          do i=IstrU,Iend
            rubar(i,j)=rubar(i,j) - rdrg*ubar(i,j,kstp)
     &                             *dm_u(i,j)*dn_u(i,j)
          enddo
        enddo
        do j=JstrV,Jend
          do i=Istr,Iend
            rvbar(i,j)=rvbar(i,j) - rdrg*vbar(i,j,kstp)
     &                             *dm_v(i,j)*dn_v(i,j)
          enddo
        enddo
      endif
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
/*      if (FIRST_TIME_STEP) then
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
*/
        do j=Jstr,Jend                       ! This version is
          do i=Istr,Iend                     ! designed for coupling
            rufrc(i,j)=rufrc(i,j)-rubar(i,j) ! during 3D corrector
            rvfrc(i,j)=rvfrc(i,j)-rvbar(i,j) ! sub-step: no forward
          enddo                              ! extrapolation needs
        enddo                                ! to be performed.
      endif
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
#define DUnew DVom
#define DVnew DVom
 
          Dstp(IstrU-1:Iend,JstrV-1:Jend)=+h(IstrU-1:Iend,JstrV-1:Jend)
     &                            +zeta(IstrU-1:Iend,JstrV-1:Jend,kstp)
 
      cff=0.5*dtfast
#ifdef SOLVE3D
      cff1=0.5*weight(1,iif)
#else
      cff2=2.*dtfast
#endif

          DUnew(IstrU:Iend,Jstr:Jend)=( (Dstp(IstrU:Iend,Jstr:Jend)
     &                              +Dstp(IstrU-1:Iend-1,Jstr:Jend)
     &                               )*ubar(IstrU:Iend,Jstr:Jend,kstp)
     &     +cff*(pm(IstrU:Iend,Jstr:Jend)+pm(IstrU-1:Iend-1,Jstr:Jend))
     &         *(pn(IstrU:Iend,Jstr:Jend)+pn(IstrU-1:Iend-1,Jstr:Jend))
#ifdef SOLVE3D
     &        *(rubar(IstrU:Iend,Jstr:Jend)+rufrc(IstrU:Iend,Jstr:Jend))
#else
     &     *rubar(IstrU:Iend,Jstr:Jend)+cff2*sustr(IstrU:Iend,Jstr:Jend)
#endif
     &                                                    )
#ifdef MASKING
     &                                *umask(IstrU:Iend,Jstr:Jend)
#endif
           ubar(IstrU:Iend,Jstr:Jend,knew)=DUnew(IstrU:Iend,Jstr:Jend)
     &     /(Dnew(IstrU:Iend,Jstr:Jend)+Dnew(IstrU-1:Iend-1,Jstr:Jend))
#ifdef SOLVE3D
          DU_avg1(IstrU:Iend,Jstr:Jend)=DU_avg1(IstrU:Iend,Jstr:Jend)
     &    +cff1*DUnew(IstrU:Iend,Jstr:Jend)*dn_u(IstrU:Iend,Jstr:Jend)
#endif


          DVnew(Istr:Iend,JstrV:Jend)=( ( Dstp(Istr:Iend,JstrV:Jend)
     &                                   +Dstp(Istr:Iend,JstrV-1:Jend-1)
     &                                 )*vbar(Istr:Iend,JstrV:Jend,kstp)
     &      +cff*(pm(Istr:Iend,JstrV:Jend)+pm(Istr:Iend,JstrV-1:Jend-1))
     &          *(pn(Istr:Iend,JstrV:Jend)+pn(Istr:Iend,JstrV-1:Jend-1))
#ifdef SOLVE3D
     &        *(rvbar(Istr:Iend,JstrV:Jend)+rvfrc(Istr:Iend,JstrV:Jend))
#else
     &     *rvbar(Istr:Iend,JstrV:Jend)+cff2*svstr(Istr:Iend,JstrV:Jend)
#endif
     &                                                    )
#ifdef MASKING
     &                                      *vmask(Istr:Iend,JstrV:Jend)
#endif
          vbar(Istr:Iend,JstrV:Jend,knew)=DVnew(Istr:Iend,JstrV:Jend)
     &      /(Dnew(Istr:Iend,JstrV:Jend)+Dnew(Istr:Iend,JstrV-1:Jend-1))
#ifdef SOLVE3D
          DV_avg1(Istr:Iend,JstrV:Jend)=DV_avg1(Istr:Iend,JstrV:Jend)
     &      +cff1*DVnew(Istr:Iend,JstrV:Jend)*dm_v(Istr:Iend,JstrV:Jend)
#endif
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
        do j=Jstr,Jend
          DV_avg1(Istr-1,j)=DV_avg1(Istr-1,j) +cff1*(Dnew(Istr-1,j)
     &       +Dnew(Istr-1,j-1) )*vbar(Istr-1,j,knew)*dm_v(Istr-1,j)
        enddo
      endif
      if (EASTERN_EDGE) then
        do j=JstrR,JendR
          DU_avg1(Iend+1,j)=DU_avg1(Iend+1,j) +cff1*( Dnew(Iend+1,j)
     &            +Dnew(Iend,j) )*ubar(Iend+1,j,knew)*dn_u(Iend+1,j)
        enddo
        do j=Jstr,Jend
          DV_avg1(Iend+1,j)=DV_avg1(Iend+1,j) +cff1*( Dnew(Iend+1,j)
     &        +Dnew(Iend+1,j-1) )*vbar(Iend+1,j,knew)*dm_v(Iend+1,j)
        enddo
      endif
# endif
# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        do i=Istr,Iend
          DU_avg1(i,Jstr-1)=DU_avg1(i,Jstr-1) +cff1*( Dnew(i,Jstr-1)
     &        +Dnew(i-1,Jstr-1) )*ubar(i,Jstr-1,knew)*dn_u(i,Jstr-1)
        enddo
        do i=IstrR,IendR
          DV_avg1(i,JstrV-1)=DV_avg1(i,JstrV-1)+cff1*(Dnew(i,JstrV-1)
     &         +Dnew(i,JstrV-2))*vbar(i,JstrV-1,knew)*dm_v(i,JstrV-1)
        enddo
      endif
      if (NORTHERN_EDGE) then
        do i=Istr,Iend
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
#ifdef SOLVE3D
      if (iif.eq.nfast) call set_depth_tile (Istr,Iend,Jstr,Jend,UFx)
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
      end subroutine step2D_FB_tile 
 
c**   end module step2d_mod
