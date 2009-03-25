#include "cppdefs.h"
 
      subroutine v2dbc (tile)
      implicit none
      integer tile
#include "param.h"
#include "private_scratch.h"
#include "compute_tile_bounds.h"
      call v2dbc_tile (istr,iend,jstr,jend, A2d(1,1))
      return
      end
 
      subroutine v2dbc_tile (istr,iend,jstr,jend, grad)
!
! Set lateral boundary conditions for the barotropic (i.e.
! vertically integrated) ETA-component velocity vbar(:,:,knew).
!
      implicit none
      integer istr,iend,jstr,jend, i,j
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
        do i=istr,iend+1
          grad(i,jstr  )=(vbar(i,jstr  ,kstp)-vbar(i-1,jstr  ,kstp))
#   ifdef MASKING
     &                                                *pmask(i,jstr)
#   endif
          grad(i,jstr+1)=(vbar(i,jstr+1,kstp)-vbar(i-1,jstr+1,kstp))
#   ifdef MASKING
     &                                              *pmask(i,jstr+1)
#   endif
        enddo
        do i=istr,iend
          dft=vbar(i,jstr+1,kstp)-vbar(i,jstr+1,knew)
          dfx=vbar(i,jstr+1,knew)-vbar(i,jstr+2,knew)
 
          if (dfx*dft .lt. 0.) then
            dft=0.                       ! <-- cancel cx, if inflow
#   if defined M2_FRC_BRY || defined M2NUDGING
            tau=tau_in
          else
            tau=tau_out
#   endif
          endif
 
          if (dft*(grad(i,jstr+1)+grad(i+1,jstr+1)) .gt. 0.) then
            dfy=grad(i  ,jstr+1)
          else
            dfy=grad(i+1,jstr+1)
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
 
          vbar(i,jstr,knew)=( cff*vbar(i,jstr,kstp)
     &                       +cx*vbar(i,jstr+1,knew)
     &                    -max(cy,0.)*grad(i  ,jstr)
     &                    -min(cy,0.)*grad(i+1,jstr)
     &                                   )/(cff+cx)
#   ifdef OBC_FLUX_CORR
     &                                    -uv_crss
#   endif
#   if defined M2_FRC_BRY || defined M2NUDGING
          vbar(i,jstr,knew)=(1.-tau)*vbar(i,jstr,knew)
#    ifdef M2_FRC_BRY
     &                                   +tau*vbar_south(i)

     &       -attnM2*dtfast*g*0.5*(pn(i,jstr-1)+pn(i,jstr))
     &                   *(zeta(i,jstr,knew)-zeta_south(i))
#    else
     &                                   +tau*vbclm(i,jstr)

     &       -attnM2*dtfast*g*0.5*(pn(i,jstr-1)+pn(i,jstr))
     &                   *(zeta(i,jstr,knew)-ssh(i,jstr-1))
#    endif
#   endif
#   ifdef MASKING
          vbar(i,jstr,knew)=vbar(i,jstr,knew)*vmask(i,jstr)
#   endif
        enddo
#  else         /* alternative open: Flather, specified, gradient */
        do i=istr,iend
#    if defined OBC_M2FLATHER
c**          vbar(i,jstr,knew)= -sqrt(2.*g/(h(i,jstr-1)+h(i,jstr)))
c**     &             *( 0.5*(zeta(i,jstr-1,knew)+zeta(i,jstr,knew))
c**#    ifdef M2_FRC_BRY
c**     &                             -zeta_south(i) )+vbar_south(i)
c**#    else
c**     &                              -ssh(i,jstr-1))+vbclm(i,jstr)
c**#    endif


          cff=0.5*(h(i,jstr-1)+h(i,jstr))
          hx=sqrt(g/cff)
          cx=dtfast*cff*hx*0.5*(pn(i,jstr-1)+pn(i,jstr))

          zx=(0.5+cx)*zeta(i,jstr,kstp)+(0.5-cx)*zeta(i,jstr-1,kstp)
          if (cx .gt. 0.292893218813452) then
            zx=zx + ( zeta(i,jstr,knew) +cx*zeta(i,jstr-1,kstp)
     &                               -(1.+cx)*zeta(i,jstr,kstp)
     &                           )*(1.-0.292893218813452/cx)**2
          endif

c          if (cx .lt. 0.25) then
c           zx=(0.5+cx)*zeta(i,jstr,kstp)+(0.5-cx)*zeta(i,jstr-1,kstp)
c          else
c            zx=( 0.375*zeta(i,jstr,kstp)   +0.125*zeta(i,jstr-1,kstp)
c     &                       +(cx-0.25)*zeta(i,jstr,knew) )/(cx+0.25)
c          endif

          vbar(i,jstr,knew)=0.5*( (1.-cx)*vbar(i,jstr,kstp)
     &                               +cx*vbar(i,jstr+1,kstp)
#    ifdef M2_FRC_BRY
     &                 +vbar_south(i) -hx*(zx-zeta_south(i))
#    else
     &                 +vbclm(i,jstr) -hx*(zx-ssh(i,jstr-1))
#    endif
     &                                                    )









#   elif defined OBC_M2SPECIFIED
#    ifdef M2_FRC_BRY
          vbar(i,jstr,knew)=vbar_south(i)
#    else
          vbar(i,jstr,knew)=vbclm(i,jstr)
#    endif
#   else
          vbar(i,jstr,knew)=vbar(i,jstr+1,knew)
#   endif
#   ifdef MASKING
          vbar(i,jstr,knew)=vbar(i,jstr,knew)*vmask(i,jstr)
#   endif
        enddo
#  endif
# else
        do i=istr,iend                        ! Southern edge closed
          vbar(i,jstr,knew)=0.                ! ======== ==== ======
        enddo                                 !  (no-flux, default)
# endif          /* OBC_SOUTH */
      endif   !<-- SOUTHERN_EDGE
 
 
 
      if (NORTHERN_EDGE) then
# ifdef OBC_NORTH
#  ifdef OBC_M2ORLANSKI
!                                          Northern edge radiation BC
!                                          ======== ==== ========= ==
        do i=istr,iend+1
          grad(i,jend  )=(vbar(i,jend  ,kstp)-vbar(i-1,jend  ,kstp))
#   ifdef MASKING
     &                                                *pmask(i,jend)
#   endif
          grad(i,jend+1)=(vbar(i,jend+1,kstp)-vbar(i-1,jend+1,kstp))
#   ifdef MASKING
     &                                              *pmask(i,jend+1)
#   endif
        enddo
        do i=istr,iend
          dft=vbar(i,jend,kstp)-vbar(i,jend  ,knew)
          dfx=vbar(i,jend,knew)-vbar(i,jend-1,knew)
 
          if (dfx*dft .lt. 0.) then
            dft=0.                         ! <-- cancel cx, if inflow
#   if defined M2_FRC_BRY || defined M2NUDGING
            tau=tau_in
          else
            tau=tau_out
#   endif
          endif
 
          if (dft*(grad(i,jend)+grad(i+1,jend)) .gt. 0.) then
            dfy=grad(i  ,jend)
          else
            dfy=grad(i+1,jend)
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
 
          vbar(i,jend+1,knew)=( cff*vbar(i,jend+1,kstp)
     &                             +cx*vbar(i,jend,knew)
     &                      -max(cy,0.)*grad(i  ,jend+1)
     &                      -min(cy,0.)*grad(i+1,jend+1)
     &                                      )/(cff+cx)
#   ifdef OBC_FLUX_CORR
     &                                      +uv_crss
#   endif
#   if defined M2_FRC_BRY || defined M2NUDGING
          vbar(i,jend+1,knew)=(1.-tau)*vbar(i,jend+1,knew)
#    ifdef M2_FRC_BRY
     &                                   +tau*vbar_north(i)

     &       +attnM2*dtfast*g*0.5*(pn(i,jend)+pn(i,jend+1))
     &                   *(zeta(i,jend,knew)-zeta_north(i))
#    else
     &                                 +tau*vbclm(i,jend+1)

     &       +attnM2*dtfast*g*0.5*(pn(i,jend)+pn(i,jend+1))
     &                   *(zeta(i,jend,knew)-ssh(i,jend+1))
#    endif
#   endif
#   ifdef MASKING
          vbar(i,jend+1,knew)=vbar(i,jend+1,knew)*vmask(i,jend+1)
#   endif
        enddo
#  else          /* alternative open: Flather, specified, gradient */
        do i=istr,iend
#   if defined OBC_M2FLATHER

c**         vbar(i,jend+1,knew)= +sqrt(2.*g/(h(i,jend)+h(i,jend+1)))
c**    &               *( 0.5*(zeta(i,jend,knew)+zeta(i,jend+1,knew))
c**#   ifdef M2_FRC_BRY
c**    &                               -zeta_north(i) )+vbar_north(i)
c**#   else
c**    &                             -ssh(i,jend+1) )+vbclm(i,jend+1)
c**#  endif


          cff=0.5*(h(i,jend)+h(i,jend+1))
          hx=sqrt(g/cff)
          cx=dtfast*cff*hx*0.5*(pn(i,jend)+pn(i,jend+1))

          zx=(0.5+cx)*zeta(i,jend,kstp)+(0.5-cx)*zeta(i,jend+1,kstp)
          if (cx .gt. 0.292893218813452) then
            zx=zx + ( zeta(i,jend,knew) +cx*zeta(i,jend+1,kstp)
     &                               -(1.+cx)*zeta(i,jend,kstp)
     &                           )*(1.-0.292893218813452/cx)**2
          endif

c         if (cx .lt. 0.25) then
c          zx=(0.5+cx)*zeta(i,jend,kstp)+(0.5-cx)*zeta(i,jend+1,kstp)
c         else
c           zx=( 0.375*zeta(i,jend,kstp)   +0.125*zeta(i,jend+1,kstp)
c    &                       +(cx-0.25)*zeta(i,jend,knew) )/(cx+0.25)
c         endif

          vbar(i,jend+1,knew)=0.5*( (1.-cx)*vbar(i,jend+1,kstp)
     &                                     +cx*vbar(i,jend,kstp)
#    ifdef M2_FRC_BRY
     &                     +vbar_north(i) +hx*(zx-zeta_north(i))
#    else
     &                   +vbclm(i,jend+1) +hx*(zx-ssh(i,jend+1))
#    endif
     &                                                        )












#   elif defined OBC_M2SPECIFIED
#    ifdef M2_FRC_BRY
          vbar(i,jend+1,knew)=vbar_north(i)
#    else
          vbar(i,jend+1,knew)=vbclm(i,jend+1)
#    endif
#   else
          vbar(i,jend+1,knew)=vbar(i,jend,knew)
#   endif
#   ifdef MASKING
          vbar(i,jend+1,knew)=vbar(i,jend+1,knew)*vmask(i,jend+1)
#   endif
        enddo
#  endif
# else
        do i=istr,iend                        ! Northern edge closed
          vbar(i,jend+1,knew)=0.              ! ======== ==== ======
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
        do j=jstrV-1,jend
          grad(istr-1,j)=vbar(istr-1,j+1,kstp)-vbar(istr-1,j,kstp)
          grad(istr  ,j)=vbar(istr  ,j+1,kstp)-vbar(istr  ,j,kstp)
        enddo
        do j=jstrV,jend
/* --->
          dft=vbar(istr,j,kstp)-vbar(istr  ,j,knew)
          dfx=vbar(istr,j,knew)-vbar(istr+1,j,knew)
 
          if (dfx*dft .lt. 0.) then
            dft=0.                        ! <-- cancel cx, if inflow
#   if defined M2_FRC_BRY || defined M2NUDGING
            tau=tau_in  
          else
            tau=tau_out
#   endif
          endif
 
          if (dft*(grad(istr,j-1)+grad(istr,j)) .gt. 0.) then
            dfy=grad(istr,j-1)
          else
            dfy=grad(istr,j  )
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
 
          vbar(istr-1,j,knew)=( cff*vbar(istr-1,j,kstp)
     &                             +cx*vbar(istr,j,knew)
     &                      -max(cy,0.)*grad(istr-1,j-1)
     &                      -min(cy,0.)*grad(istr-1,j  )
     &                                       )/(cff+cx)

---> */



          cx=-0.125*dtfast*(ubar(istr,j,kstp)+ubar(istr,j-1,kstp))
     &      *(pm(istr-1,j)+pm(istr-1,j-1)+pm(istr,j)+pm(istr,j-1))

          cy= 0.125*dtfast*(vbar(istr-1,j,kstp)+vbar(istr,j,kstp))
     &      *(pn(istr-1,j)+pn(istr-1,j-1)+pn(istr,j)+pn(istr,j-1))

          if (cx.gt.0.) then
            tau=0.
          else
            tau=-cx
            cx=0.
          endif

          vbar(istr-1,j,knew)=(1.-cx)*( vbar(istr-1,j,kstp)
     &                          -max(cy,0.)*grad(istr-1,j-1)
     &                          -min(cy,0.)*grad(istr-1,j  )
     &                                                     )
     &                       +cx*(        vbar(istr,j,kstp)
     &                            -max(cy,0.)*grad(istr,j-1)
     &                            -min(cy,0.)*grad(istr,j  )
     &                                                     )







#   if defined M2_FRC_BRY || defined M2NUDGING
          vbar(istr-1,j,knew)=(1.-tau)*vbar(istr-1,j,knew)
#    ifdef M2_FRC_BRY
     &                                   +tau*vbar_west(j)
#    else
     &                                +tau*vbclm(istr-1,j)
#    endif
#   endif
#   ifdef MASKING
          vbar(istr-1,j,knew)=vbar(istr-1,j,knew)*vmask(istr-1,j)
#   endif
        enddo
#  else          /* alternative open: Chapman, specified, gradient */
        do j=jstrV,jend
#   if defined OBC_M2FLATHER
          cx=dtfast*sqrt(0.5*g*(h(istr-1,j-1)+h(istr-1,j)))
     &                    *0.5*(pm(istr-1,j-1)+pm(istr-1,j))
          vbar(istr-1,j,knew)=( vbar(istr-1,j,kstp)
     &               +cx*vbar(istr,j,knew) )/(1.+cx)
#   elif defined OBC_M2SPECIFIED
#    ifdef M2_FRC_BRY
          vbar(istr-1,j,knew)=vbar_west(j)
#    else
          vbar(istr-1,j,knew)=vbclm(istr-1,j)
#    endif
#   else
          vbar(istr-1,j,knew)=vbar(istr,j,knew)
#   endif
#   ifdef MASKING
          vbar(istr-1,j,knew)=vbar(istr-1,j,knew)*vmask(istr-1,j)
#   endif
        enddo
#  endif
# else
#  ifdef NS_PERIODIC
#   define J_RANGE jstrV,jend
#  else
#   define J_RANGE jstr,jendR
#  endif
                                   ! Closed BC: free-slip (gamma2=+1)
        do j=J_RANGE               ! ====== ===   no-slip (gamma2=-1)
          vbar(istr-1,j,knew)=gamma2*vbar(istr,j,knew)
#  ifdef MASKING
     &                                *vmask(istr-1,j)
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
        do j=jstrV-1,jend
          grad(iend  ,j)=vbar(iend  ,j+1,kstp)-vbar(iend  ,j,kstp)
          grad(iend+1,j)=vbar(iend+1,j+1,kstp)-vbar(iend+1,j,kstp)
        enddo
        do j=jstrV,jend
/* --->
          dft=vbar(iend,j,kstp)-vbar(iend  ,j,knew)
          dfx=vbar(iend,j,knew)-vbar(iend-1,j,knew)
 
          if (dfx*dft .lt. 0.) then
            dft=0.                                 ! <-- INFLOW
#   if defined M2_FRC_BRY || defined M2NUDGING
            tau=tau_in
          else
            tau=tau_out
#   endif
          endif
 
          if (dft*(grad(iend,j-1)+grad(iend,j)) .gt. 0.) then
            dfy=grad(iend,j-1)
          else
            dfy=grad(iend,j  )
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

          vbar(iend+1,j,knew)=( cff*vbar(iend+1,j,kstp)
     &                             +cx*vbar(iend,j,knew)
     &                      -max(cy,0.)*grad(iend+1,j-1)
     &                      -min(cy,0.)*grad(iend+1,j  )
     &                                       )/(cff+cx)

---> */

         
          cx=0.125*dtfast*(ubar(iend+1,j,kstp)+ubar(iend+1,j-1,kstp))
     &         *(pm(iend,j)+pm(iend,j-1)+pm(iend+1,j)+pm(iend+1,j-1))

          cy=0.125*dtfast*(vbar(iend,j,kstp)+vbar(iend+1,j,kstp))
     &         *(pn(iend,j)+pn(iend,j-1)+pn(iend+1,j)+pn(iend+1,j-1))

          if (cx.gt.0.) then 
            tau=0.
          else
            tau=-cx
            cx=0.
          endif

          vbar(iend+1,j,knew)=(1.-cx)*( vbar(iend+1,j,kstp)          
     &                          -max(cy,0.)*grad(iend+1,j-1)
     &                          -min(cy,0.)*grad(iend+1,j  )
     &                                                     )
     &                       +cx*(        vbar(iend,j,kstp)
     &                            -max(cy,0.)*grad(iend,j-1)
     &                            -min(cy,0.)*grad(iend,j  )
     &                                                     )

#   if defined M2_FRC_BRY || defined M2NUDGING
          vbar(iend+1,j,knew)=(1.-tau)*vbar(iend+1,j,knew)
#    ifdef M2_FRC_BRY
     &                                   +tau*vbar_east(j)
#    else
     &                                +tau*vbclm(iend+1,j)
#    endif
#   endif
#   ifdef MASKING
          vbar(iend+1,j,knew)=vbar(iend+1,j,knew)*vmask(iend+1,j)
#   endif
        enddo
#  else          /* alternative open: Chapman, specified, gradient */
        do j=jstrV,jend
#   if defined OBC_M2FLATHER
          cx=dtfast*sqrt(0.5*g*(h(iend+1,j-1)+h(iend+1,j)))
     &                    *0.5*(pm(iend+1,j-1)+pm(iend+1,j))
          vbar(iend+1,j,knew)=( vbar(iend+1,j,kstp)
     &                +cx*vbar(iend,j,knew))/(1.+cx)
#   elif defined OBC_M2SPECIFIED
#    ifdef M2_FRC_BRY
          vbar(iend+1,j,knew)=vbar_east(j)
#    else
          vbar(iend+1,j,knew)=vbclm(iend+1,j)
#    endif
#   else
          vbar(iend+1,j,knew)=vbar(iend,j,knew)
#   endif
#   ifdef MASKING
          vbar(iend+1,j,knew)=vbar(iend+1,j,knew)*vmask(iend+1,j)
#   endif
        enddo
#  endif
# else
#  ifdef NS_PERIODIC
#   define J_RANGE jstrV,jend
#  else
#   define J_RANGE jstr,jendR
#  endif
                                        ! Wall: free-slip (gamma2=+1)
        do j=J_RANGE                    ! =====   no-slip (gamma2=-1)
          vbar(iend+1,j,knew)=gamma2*vbar(iend,j,knew)
#  ifdef MASKING
     &                                *vmask(iend+1,j)
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
        vbar(istr-1,jstr,knew)=0.5*( vbar(istr-1,jstr+1,knew)
     &                              +vbar(istr  ,jstr  ,knew))
      endif
#endif
#if defined OBC_SOUTH && defined OBC_EAST
      if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
        vbar(iend+1,jstr,knew)=0.5*( vbar(iend+1,jstr+1,knew)
     &                              +vbar(iend  ,jstr  ,knew))
      endif
#endif
#if defined OBC_NORTH && defined OBC_WEST
      if (WESTERN_EDGE .and. NORTHERN_EDGE) then
        vbar(istr-1,jend+1,knew)=0.5*( vbar(istr-1,jend,knew)
     &                                +vbar(istr,jend+1,knew))
      endif
#endif
#if defined OBC_NORTH && defined OBC_EAST
      if (EASTERN_EDGE .and. NORTHERN_EDGE) then
        vbar(iend+1,jend+1,knew)=0.5*( vbar(iend+1,jend,knew)
     &                                +vbar(iend,jend+1,knew))
      endif
#endif
      return
      end
 
