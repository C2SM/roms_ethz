#include "cppdefs.h"
 
      subroutine v2dbc (tile)
      implicit none
      integer tile
#include "param.h"
#include "private_scratch.h"
#include "compute_tile_bounds.h"
      call v2dbc_tile (Istr,Iend,Jstr,Jend, A2d(1,1))
      return
      end
 
      subroutine v2dbc_tile (Istr,Iend,Jstr,Jend, grad)
!
! Set lateral boundary conditions for the barotropic (i.e.
! vertically integrated) ETA-component velocity vbar(:,:,knew).
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j
      real grad(PRIVATE_2D_SCRATCH_ARRAY), eps,cff, cx,cy,
     &            dft,dfx,dfy, tau,tau_in,tau_out, uv_crss
      parameter (eps=1.D-20)
      real hx,zx 
#include "param.h"
#include "scalars.h"
#include "grid.h"
#include "ocean2d.h"
#ifdef M2_FRC_BRY
# include "boundary.h"
#else
# include "climat.h"
#endif
!
#include "compute_auxiliary_bounds.h"
!
#if defined M2_FRC_BRY || defined M2NUDGING
      tau_in=dtfast*tauM2_in
      tau_out=dtfast*tauM2_out
#endif
#ifdef OBC_FLUX_CORR
      uv_crss=ubar_xs*dtfast/tau_obc
#endif

#ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
# ifdef OBC_SOUTH
#  ifdef OBC_M2ORLANSKI
        do i=Istr,Iend+1
          grad(i,Jstr  )=(vbar(i,Jstr  ,kstp)-vbar(i-1,Jstr  ,kstp))
#   ifdef MASKING
     &                                                *pmask(i,Jstr)
#   endif
          grad(i,Jstr+1)=(vbar(i,Jstr+1,kstp)-vbar(i-1,Jstr+1,kstp))
#   ifdef MASKING
     &                                              *pmask(i,Jstr+1)
#   endif
        enddo
        do i=Istr,Iend
          dft=vbar(i,Jstr+1,kstp)-vbar(i,Jstr+1,knew)
          dfx=vbar(i,Jstr+1,knew)-vbar(i,Jstr+2,knew)
 
          if (dfx*dft .lt. 0.) then
            dft=0.                       ! <-- cancel cx, if inflow
#   if defined M2_FRC_BRY || defined M2NUDGING
            tau=tau_in
          else
            tau=tau_out
#   endif
          endif
 
          if (dft*(grad(i,Jstr+1)+grad(i+1,Jstr+1)) .gt. 0.) then
            dfy=grad(i  ,Jstr+1)
          else
            dfy=grad(i+1,Jstr+1)
          endif
 
#   ifdef OBC_RAD_NORMAL
          dfy=0.
#   endif
          cff=max(dfx*dfx+dfy*dfy, eps)
          cx=dft*dfx
#   ifdef OBC_RAD_NPO
          cy=0.
#   else
          cy=min(cff,max(dft*dfy,-cff))
#   endif
 
          vbar(i,Jstr,knew)=( cff*vbar(i,Jstr,kstp)
     &                       +cx*vbar(i,Jstr+1,knew)
     &                    -max(cy,0.)*grad(i  ,Jstr)
     &                    -min(cy,0.)*grad(i+1,Jstr)
     &                                   )/(cff+cx)
#   ifdef OBC_FLUX_CORR
     &                                    -uv_crss
#   endif
#   if defined M2_FRC_BRY || defined M2NUDGING
          vbar(i,Jstr,knew)=(1.-tau)*vbar(i,Jstr,knew)
#    ifdef M2_FRC_BRY
     &                                   +tau*vbar_south(i)

     &       -attnM2*dtfast*g*0.5*(pn(i,Jstr-1)+pn(i,Jstr))
     &                   *(zeta(i,Jstr,knew)-zeta_south(i))
#    else
     &                                   +tau*vbclm(i,Jstr)

     &       -attnM2*dtfast*g*0.5*(pn(i,Jstr-1)+pn(i,Jstr))
     &                   *(zeta(i,Jstr,knew)-ssh(i,Jstr-1))
#    endif
#   endif
#   ifdef MASKING
          vbar(i,Jstr,knew)=vbar(i,Jstr,knew)*vmask(i,Jstr)
#   endif
        enddo
#  else         /* alternative open: Flather, specified, gradient */
        do i=Istr,Iend
#    if defined OBC_M2FLATHER
c**          vbar(i,Jstr,knew)= -sqrt(2.*g/(h(i,Jstr-1)+h(i,Jstr)))
c**     &             *( 0.5*(zeta(i,Jstr-1,knew)+zeta(i,Jstr,knew))
c**#    ifdef M2_FRC_BRY
c**     &                             -zeta_south(i) )+vbar_south(i)
c**#    else
c**     &                              -ssh(i,Jstr-1))+vbclm(i,Jstr)
c**#    endif


          cff=0.5*(h(i,Jstr-1)+h(i,Jstr))
          hx=sqrt(g/cff)
          cx=dtfast*cff*hx*0.5*(pn(i,Jstr-1)+pn(i,Jstr))

          zx=(0.5+cx)*zeta(i,Jstr,kstp)+(0.5-cx)*zeta(i,Jstr-1,kstp)
          if (cx .gt. 0.292893218813452) then
            zx=zx + ( zeta(i,Jstr,knew) +cx*zeta(i,Jstr-1,kstp)
     &                               -(1.+cx)*zeta(i,Jstr,kstp)
     &                           )*(1.-0.292893218813452/cx)**2
          endif

c          if (cx .lt. 0.25) then
c           zx=(0.5+cx)*zeta(i,Jstr,kstp)+(0.5-cx)*zeta(i,Jstr-1,kstp)
c          else
c            zx=( 0.375*zeta(i,Jstr,kstp)   +0.125*zeta(i,Jstr-1,kstp)
c     &                       +(cx-0.25)*zeta(i,Jstr,knew) )/(cx+0.25)
c          endif

          vbar(i,Jstr,knew)=0.5*( (1.-cx)*vbar(i,Jstr,kstp)
     &                               +cx*vbar(i,Jstr+1,kstp)
#    ifdef M2_FRC_BRY
     &                 +vbar_south(i) -hx*(zx-zeta_south(i))
#    else
     &                 +vbclm(i,Jstr) -hx*(zx-ssh(i,Jstr-1))
#    endif
     &                                                    )









#   elif defined OBC_M2SPECIFIED
#    ifdef M2_FRC_BRY
          vbar(i,Jstr,knew)=vbar_south(i)
#    else
          vbar(i,Jstr,knew)=vbclm(i,Jstr)
#    endif
#   else
          vbar(i,Jstr,knew)=vbar(i,Jstr+1,knew)
#   endif
#   ifdef MASKING
          vbar(i,Jstr,knew)=vbar(i,Jstr,knew)*vmask(i,Jstr)
#   endif
        enddo
#  endif
# else
        do i=Istr,Iend                        ! Southern edge closed
          vbar(i,Jstr,knew)=0.                ! ======== ==== ======
        enddo                                 !  (no-flux, default)
# endif          /* OBC_SOUTH */
      endif   !<-- SOUTHERN_EDGE
 
 
 
      if (NORTHERN_EDGE) then
# ifdef OBC_NORTH
#  ifdef OBC_M2ORLANSKI
!                                          Northern edge radiation BC
!                                          ======== ==== ========= ==
        do i=Istr,Iend+1
          grad(i,Jend  )=(vbar(i,Jend  ,kstp)-vbar(i-1,Jend  ,kstp))
#   ifdef MASKING
     &                                                *pmask(i,Jend)
#   endif
          grad(i,Jend+1)=(vbar(i,Jend+1,kstp)-vbar(i-1,Jend+1,kstp))
#   ifdef MASKING
     &                                              *pmask(i,Jend+1)
#   endif
        enddo
        do i=Istr,Iend
          dft=vbar(i,Jend,kstp)-vbar(i,Jend  ,knew)
          dfx=vbar(i,Jend,knew)-vbar(i,Jend-1,knew)
 
          if (dfx*dft .lt. 0.) then
            dft=0.                         ! <-- cancel cx, if inflow
#   if defined M2_FRC_BRY || defined M2NUDGING
            tau=tau_in
          else
            tau=tau_out
#   endif
          endif
 
          if (dft*(grad(i,Jend)+grad(i+1,Jend)) .gt. 0.) then
            dfy=grad(i  ,Jend)
          else
            dfy=grad(i+1,Jend)
          endif
 
#   ifdef OBC_RAD_NORMAL
          dfy=0.
#   endif
          cff=max(dfx*dfx+dfy*dfy, eps)
          cx=dft*dfx
#   ifdef OBC_RAD_NPO
          cy=0.
#   else
          cy=min(cff,max(dft*dfy,-cff))
#   endif
 
          vbar(i,Jend+1,knew)=( cff*vbar(i,Jend+1,kstp)
     &                             +cx*vbar(i,Jend,knew)
     &                      -max(cy,0.)*grad(i  ,Jend+1)
     &                      -min(cy,0.)*grad(i+1,Jend+1)
     &                                      )/(cff+cx)
#   ifdef OBC_FLUX_CORR
     &                                      +uv_crss
#   endif
#   if defined M2_FRC_BRY || defined M2NUDGING
          vbar(i,Jend+1,knew)=(1.-tau)*vbar(i,Jend+1,knew)
#    ifdef M2_FRC_BRY
     &                                   +tau*vbar_north(i)

     &       +attnM2*dtfast*g*0.5*(pn(i,Jend)+pn(i,Jend+1))
     &                   *(zeta(i,Jend,knew)-zeta_north(i))
#    else
     &                                 +tau*vbclm(i,Jend+1)

     &       +attnM2*dtfast*g*0.5*(pn(i,Jend)+pn(i,Jend+1))
     &                   *(zeta(i,Jend,knew)-ssh(i,Jend+1))
#    endif
#   endif
#   ifdef MASKING
          vbar(i,Jend+1,knew)=vbar(i,Jend+1,knew)*vmask(i,Jend+1)
#   endif
        enddo
#  else          /* alternative open: Flather, specified, gradient */
        do i=Istr,Iend
#   if defined OBC_M2FLATHER

c**         vbar(i,Jend+1,knew)= +sqrt(2.*g/(h(i,Jend)+h(i,Jend+1)))
c**    &               *( 0.5*(zeta(i,Jend,knew)+zeta(i,Jend+1,knew))
c**#   ifdef M2_FRC_BRY
c**    &                               -zeta_north(i) )+vbar_north(i)
c**#   else
c**    &                             -ssh(i,Jend+1) )+vbclm(i,Jend+1)
c**#  endif


          cff=0.5*(h(i,Jend)+h(i,Jend+1))
          hx=sqrt(g/cff)
          cx=dtfast*cff*hx*0.5*(pn(i,Jend)+pn(i,Jend+1))

          zx=(0.5+cx)*zeta(i,Jend,kstp)+(0.5-cx)*zeta(i,Jend+1,kstp)
          if (cx .gt. 0.292893218813452) then
            zx=zx + ( zeta(i,Jend,knew) +cx*zeta(i,Jend+1,kstp)
     &                               -(1.+cx)*zeta(i,Jend,kstp)
     &                           )*(1.-0.292893218813452/cx)**2
          endif

c         if (cx .lt. 0.25) then
c          zx=(0.5+cx)*zeta(i,Jend,kstp)+(0.5-cx)*zeta(i,Jend+1,kstp)
c         else
c           zx=( 0.375*zeta(i,Jend,kstp)   +0.125*zeta(i,Jend+1,kstp)
c    &                       +(cx-0.25)*zeta(i,Jend,knew) )/(cx+0.25)
c         endif

          vbar(i,Jend+1,knew)=0.5*( (1.-cx)*vbar(i,Jend+1,kstp)
     &                                     +cx*vbar(i,Jend,kstp)
#    ifdef M2_FRC_BRY
     &                     +vbar_north(i) +hx*(zx-zeta_north(i))
#    else
     &                   +vbclm(i,Jend+1) +hx*(zx-ssh(i,Jend+1))
#    endif
     &                                                        )












#   elif defined OBC_M2SPECIFIED
#    ifdef M2_FRC_BRY
          vbar(i,Jend+1,knew)=vbar_north(i)
#    else
          vbar(i,Jend+1,knew)=vbclm(i,Jend+1)
#    endif
#   else
          vbar(i,Jend+1,knew)=vbar(i,Jend,knew)
#   endif
#   ifdef MASKING
          vbar(i,Jend+1,knew)=vbar(i,Jend+1,knew)*vmask(i,Jend+1)
#   endif
        enddo
#  endif
# else
        do i=Istr,Iend                        ! Northern edge closed
          vbar(i,Jend+1,knew)=0.              ! ======== ==== ======
        enddo                                 !  (no-flux: default)
# endif /* OBC_NORTH */
      endif          !<--  NORTHERN_EDGE
#endif                  /* !NS_PERIODIC */
 



c****
c****
#ifdef OBC_M2FLATHER
#undef OBC_M2FLATHER
#define OBC_M2ORLANSKI
#endif



 
 
#ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
# ifdef OBC_WEST
#  if defined OBC_M2ORLANSKI
!                                           Western edge radiation BC
!                                           ======= ==== ========= ==
        do j=JstrV-1,Jend
          grad(Istr-1,j)=vbar(Istr-1,j+1,kstp)-vbar(Istr-1,j,kstp)
          grad(Istr  ,j)=vbar(Istr  ,j+1,kstp)-vbar(Istr  ,j,kstp)
        enddo
        do j=JstrV,Jend
/* --->
          dft=vbar(Istr,j,kstp)-vbar(Istr  ,j,knew)
          dfx=vbar(Istr,j,knew)-vbar(Istr+1,j,knew)
 
          if (dfx*dft .lt. 0.) then
            dft=0.                        ! <-- cancel cx, if inflow
#   if defined M2_FRC_BRY || defined M2NUDGING
            tau=tau_in  
          else
            tau=tau_out
#   endif
          endif
 
          if (dft*(grad(Istr,j-1)+grad(Istr,j)) .gt. 0.) then
            dfy=grad(Istr,j-1)
          else
            dfy=grad(Istr,j  )
          endif
 
#   ifdef OBC_RAD_NORMAL
          dfy=0.
#   endif
          cff=max(dfx*dfx+dfy*dfy, eps)
          cx=dft*dfx
#   ifdef OBC_RAD_NPO
          cy=0.
#   else
          cy=min(cff,max(dft*dfy,-cff))
#   endif
 
          vbar(Istr-1,j,knew)=( cff*vbar(Istr-1,j,kstp)
     &                             +cx*vbar(Istr,j,knew)
     &                      -max(cy,0.)*grad(Istr-1,j-1)
     &                      -min(cy,0.)*grad(Istr-1,j  )
     &                                       )/(cff+cx)

---> */



          cx=-0.125*dtfast*(ubar(Istr,j,kstp)+ubar(Istr,j-1,kstp))
     &      *(pm(Istr-1,j)+pm(Istr-1,j-1)+pm(Istr,j)+pm(Istr,j-1))

          cy= 0.125*dtfast*(vbar(Istr-1,j,kstp)+vbar(Istr,j,kstp))
     &      *(pn(Istr-1,j)+pn(Istr-1,j-1)+pn(Istr,j)+pn(Istr,j-1))

          if (cx.gt.0.) then
            tau=0.
          else
            tau=-cx
            cx=0.
          endif

          vbar(Istr-1,j,knew)=(1.-cx)*( vbar(Istr-1,j,kstp)
     &                          -max(cy,0.)*grad(Istr-1,j-1)
     &                          -min(cy,0.)*grad(Istr-1,j  )
     &                                                     )
     &                       +cx*(        vbar(Istr,j,kstp)
     &                            -max(cy,0.)*grad(Istr,j-1)
     &                            -min(cy,0.)*grad(Istr,j  )
     &                                                     )







#   if defined M2_FRC_BRY || defined M2NUDGING
          vbar(Istr-1,j,knew)=(1.-tau)*vbar(Istr-1,j,knew)
#    ifdef M2_FRC_BRY
     &                                   +tau*vbar_west(j)
#    else
     &                                +tau*vbclm(Istr-1,j)
#    endif
#   endif
#   ifdef MASKING
          vbar(Istr-1,j,knew)=vbar(Istr-1,j,knew)*vmask(Istr-1,j)
#   endif
        enddo
#  else          /* alternative open: Chapman, specified, gradient */
        do j=JstrV,Jend
#   if defined OBC_M2FLATHER
          cx=dtfast*sqrt(0.5*g*(h(Istr-1,j-1)+h(Istr-1,j)))
     &                    *0.5*(pm(Istr-1,j-1)+pm(Istr-1,j))
          vbar(Istr-1,j,knew)=( vbar(Istr-1,j,kstp)
     &               +cx*vbar(Istr,j,knew) )/(1.+cx)
#   elif defined OBC_M2SPECIFIED
#    ifdef M2_FRC_BRY
          vbar(Istr-1,j,knew)=vbar_west(j)
#    else
          vbar(Istr-1,j,knew)=vbclm(Istr-1,j)
#    endif
#   else
          vbar(Istr-1,j,knew)=vbar(Istr,j,knew)
#   endif
#   ifdef MASKING
          vbar(Istr-1,j,knew)=vbar(Istr-1,j,knew)*vmask(Istr-1,j)
#   endif
        enddo
#  endif
# else
#  ifdef NS_PERIODIC
#   define J_RANGE JstrV,Jend
#  else
#   define J_RANGE Jstr,JendR
#  endif
                                   ! Closed BC: free-slip (gamma2=+1)
        do j=J_RANGE               ! ====== ===   no-slip (gamma2=-1)
          vbar(Istr-1,j,knew)=gamma2*vbar(Istr,j,knew)
#  ifdef MASKING
     &                                *vmask(Istr-1,j)
#  endif
        enddo
#  undef J_RANGE
# endif              /* OBC_WEST */
      endif       !<-- WESTERN_EDGE
 

c--#ifdef ISWAKE
c--# define OBC_M2ORLANSKI
c--#endif

 
 
      if (EASTERN_EDGE) then
# ifdef OBC_EAST
#  if defined OBC_M2ORLANSKI
!                                           Eastern edge radiation BC
!                                           ======= ==== ========= ==
        do j=JstrV-1,Jend
          grad(Iend  ,j)=vbar(Iend  ,j+1,kstp)-vbar(Iend  ,j,kstp)
          grad(Iend+1,j)=vbar(Iend+1,j+1,kstp)-vbar(Iend+1,j,kstp)
        enddo
        do j=JstrV,Jend
/* --->
          dft=vbar(Iend,j,kstp)-vbar(Iend  ,j,knew)
          dfx=vbar(Iend,j,knew)-vbar(Iend-1,j,knew)
 
          if (dfx*dft .lt. 0.) then
            dft=0.                                 ! <-- INFLOW
#   if defined M2_FRC_BRY || defined M2NUDGING
            tau=tau_in
          else
            tau=tau_out
#   endif
          endif
 
          if (dft*(grad(Iend,j-1)+grad(Iend,j)) .gt. 0.) then
            dfy=grad(Iend,j-1)
          else
            dfy=grad(Iend,j  )
          endif
 
#   ifdef OBC_RAD_NORMAL
          dfy=0.
#   endif
          cff=max(dfx*dfx+dfy*dfy, eps)
          cx=dft*dfx
#   ifdef OBC_RAD_NPO
          cy=0.
#   else
          cy=min(cff,max(dft*dfy,-cff))
#   endif

          vbar(Iend+1,j,knew)=( cff*vbar(Iend+1,j,kstp)
     &                             +cx*vbar(Iend,j,knew)
     &                      -max(cy,0.)*grad(Iend+1,j-1)
     &                      -min(cy,0.)*grad(Iend+1,j  )
     &                                       )/(cff+cx)

---> */

         
          cx=0.125*dtfast*(ubar(Iend+1,j,kstp)+ubar(Iend+1,j-1,kstp))
     &         *(pm(Iend,j)+pm(Iend,j-1)+pm(Iend+1,j)+pm(Iend+1,j-1))

          cy=0.125*dtfast*(vbar(Iend,j,kstp)+vbar(Iend+1,j,kstp))
     &         *(pn(Iend,j)+pn(Iend,j-1)+pn(Iend+1,j)+pn(Iend+1,j-1))

          if (cx.gt.0.) then 
            tau=0.
          else
            tau=-cx
            cx=0.
          endif

          vbar(Iend+1,j,knew)=(1.-cx)*( vbar(Iend+1,j,kstp)          
     &                          -max(cy,0.)*grad(Iend+1,j-1)
     &                          -min(cy,0.)*grad(Iend+1,j  )
     &                                                     )
     &                       +cx*(        vbar(Iend,j,kstp)
     &                            -max(cy,0.)*grad(Iend,j-1)
     &                            -min(cy,0.)*grad(Iend,j  )
     &                                                     )

#   if defined M2_FRC_BRY || defined M2NUDGING
          vbar(Iend+1,j,knew)=(1.-tau)*vbar(Iend+1,j,knew)
#    ifdef M2_FRC_BRY
     &                                   +tau*vbar_east(j)
#    else
     &                                +tau*vbclm(Iend+1,j)
#    endif
#   endif
#   ifdef MASKING
          vbar(Iend+1,j,knew)=vbar(Iend+1,j,knew)*vmask(Iend+1,j)
#   endif
        enddo
#  else          /* alternative open: Chapman, specified, gradient */
        do j=JstrV,Jend
#   if defined OBC_M2FLATHER
          cx=dtfast*sqrt(0.5*g*(h(Iend+1,j-1)+h(Iend+1,j)))
     &                    *0.5*(pm(Iend+1,j-1)+pm(Iend+1,j))
          vbar(Iend+1,j,knew)=( vbar(Iend+1,j,kstp)
     &                +cx*vbar(Iend,j,knew))/(1.+cx)
#   elif defined OBC_M2SPECIFIED
#    ifdef M2_FRC_BRY
          vbar(Iend+1,j,knew)=vbar_east(j)
#    else
          vbar(Iend+1,j,knew)=vbclm(Iend+1,j)
#    endif
#   else
          vbar(Iend+1,j,knew)=vbar(Iend,j,knew)
#   endif
#   ifdef MASKING
          vbar(Iend+1,j,knew)=vbar(Iend+1,j,knew)*vmask(Iend+1,j)
#   endif
        enddo
#  endif
# else
#  ifdef NS_PERIODIC
#   define J_RANGE JstrV,Jend
#  else
#   define J_RANGE Jstr,JendR
#  endif
                                        ! Wall: free-slip (gamma2=+1)
        do j=J_RANGE                    ! =====   no-slip (gamma2=-1)
          vbar(Iend+1,j,knew)=gamma2*vbar(Iend,j,knew)
#  ifdef MASKING
     &                                *vmask(Iend+1,j)
#  endif
        enddo
#  undef J_RANGE
# endif            /* OBC_EAST */
      endif      !<-- EASTERN_EDGE
#endif             /* !EW_PERIODIC */
 
                           ! Corners between adjacent open boundaries
                           ! ======= ======= ======== ==== ==========
 
#if defined OBC_SOUTH && defined OBC_WEST
      if (WESTERN_EDGE .and. SOUTHERN_EDGE) then
        vbar(Istr-1,Jstr,knew)=0.5*( vbar(Istr-1,Jstr+1,knew)
     &                              +vbar(Istr  ,Jstr  ,knew))
      endif
#endif
#if defined OBC_SOUTH && defined OBC_EAST
      if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
        vbar(Iend+1,Jstr,knew)=0.5*( vbar(Iend+1,Jstr+1,knew)
     &                              +vbar(Iend  ,Jstr  ,knew))
      endif
#endif
#if defined OBC_NORTH && defined OBC_WEST
      if (WESTERN_EDGE .and. NORTHERN_EDGE) then
        vbar(Istr-1,Jend+1,knew)=0.5*( vbar(Istr-1,Jend,knew)
     &                                +vbar(Istr,Jend+1,knew))
      endif
#endif
#if defined OBC_NORTH && defined OBC_EAST
      if (EASTERN_EDGE .and. NORTHERN_EDGE) then
        vbar(Iend+1,Jend+1,knew)=0.5*( vbar(Iend+1,Jend,knew)
     &                                +vbar(Iend,Jend+1,knew))
      endif
#endif
      return
      end
 
