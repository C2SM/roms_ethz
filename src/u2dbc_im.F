#include "cppdefs.h"
 
      subroutine u2dbc (tile)
      implicit none
      integer tile
#include "param.h"
#include "private_scratch.h"
#include "compute_tile_bounds.h"
      call u2dbc_tile (Istr,Iend,Jstr,Jend, A2d(1,1))
      return
      end
 
      subroutine u2dbc_tile (Istr,Iend,Jstr,Jend, grad)
!
! Set lateral boundary conditions for the barotropic (i.e.
! vertically integrated) XI-component velocity ubar(:,:,knew).
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j
      real grad(PRIVATE_2D_SCRATCH_ARRAY), eps,cff, cx,cy,
     &            dft,dfx,dfy, tau,tau_in,tau_out, uv_crss
     & ,zx,hx
      parameter (eps=1.D-20)
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

#ifndef EW_PERIODIC
      if (WESTERN_EDGE) then                
# ifdef OBC_WEST                  
#  ifdef OBC_M2ORLANSKI
        do j=Jstr,Jend+1
          grad(Istr  ,j)=(ubar(Istr  ,j,kstp)-ubar(Istr  ,j-1,kstp))
#   ifdef MASKING
     &                                                *pmask(Istr,j)
#   endif
          grad(Istr+1,j)=(ubar(Istr+1,j,kstp)-ubar(Istr+1,j-1,kstp))
#   ifdef MASKING
     &                                              *pmask(Istr+1,j)
#   endif
        enddo
        do j=Jstr,Jend
          dft=ubar(Istr+1,j,kstp)-ubar(Istr+1,j,knew)
          dfx=ubar(Istr+1,j,knew)-ubar(Istr+2,j,knew)
 
          if (dfx*dft .lt. 0.) then
            dft=0.                       ! <-- cancel cx, if inflow
#   if defined M2_FRC_BRY || defined M2NUDGING
            tau=tau_in
          else
            tau=tau_out
#   endif
          endif
 
          if (dft*(grad(Istr+1,j)+grad(Istr+1,j+1)) .gt. 0.) then
            dfy=grad(Istr+1,j  )
          else
            dfy=grad(Istr+1,j+1)
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
 
          ubar(Istr,j,knew)=( cff*ubar(Istr,j,kstp)
     &                       +cx*ubar(Istr+1,j,knew)
     &                    -max(cy,0.)*grad(Istr,j  )
     &                    -min(cy,0.)*grad(Istr,j+1)
     &                                   )/(cff+cx)
#   ifdef OBC_FLUX_CORR
     &                                    -uv_crss
#   endif
!
! The "attnM2" term below introduces gentle restoring of free
! surface toward its externally-specified state --- "zeta_west"
! in this case via attenuated pressure gradient term, which tends
! to change "ubar" at the boundary in such a way that it reduces
! the mismatch between "zeta_west" and the nearest internal free
! surface value. The attenuation coefficient [typical value
! attnM2=0.01 or less] is choosen to be sufficiently small to not
! interfere with the radiation boundary algorithm, but at the
! same time provide sufficient restoring.
!
#   if defined M2_FRC_BRY || defined M2NUDGING
          ubar(Istr,j,knew)=(1.-tau)*ubar(Istr,j,knew)
#    ifdef M2_FRC_BRY
     &                                    +tau*ubar_west(j)

     &       -attnM2*dtfast*g*0.5*(pm(Istr-1,j)+pm(Istr,j))
     &                    *(zeta(Istr,j,knew)-zeta_west(j))
#    else
     &                                   +tau*ubclm(Istr,j)

     &       -attnM2*dtfast*g*0.5*(pm(Istr-1,j)+pm(Istr,j))
     &                   *(zeta(Istr,j,knew)-ssh(Istr-1,j))
#    endif
#   endif
#   ifdef MASKING
          ubar(Istr,j,knew)=ubar(Istr,j,knew)*umask(Istr,j)
#   endif
        enddo
#  else          /* alternative open: Flather, specified, gradient */
        do j=Jstr,Jend
#   if defined OBC_M2FLATHER
C**       ubar(Istr,j,knew)= -sqrt(2.*g/(h(Istr-1,j)+h(Istr,j)))
c**  &             *( 0.5*(zeta(Istr-1,j,knew)+zeta(Istr,j,knew))
#    ifdef M2_FRC_BRY
c**  &                               -zeta_west(j) )+ubar_west(j)
c**  &     -zeta_west(j) )+0.5*(ubar_west(j)+ubar(Istr+1,j,knew))
#    else
c**  &                             -ssh(Istr-1,j) )+ubclm(Istr,j)
#    endif




          cff=0.5*(h(Istr-1,j)+h(Istr,j))
          hx=sqrt(g/cff)
          cx=dtfast*cff*hx*0.5*(pm(Istr-1,j)+pm(Istr,j))

          zx=(0.5+cx)*zeta(Istr,j,kstp)+(0.5-cx)*zeta(Istr-1,j,kstp)
          if (cx .gt. 0.292893218813452) then
            zx=zx + ( zeta(Istr,j,knew) +cx*zeta(Istr-1,j,kstp)
     &                               -(1.+cx)*zeta(Istr,j,kstp)
     &                           )*(1.-0.292893218813452/cx)**2
          endif




c          if (cx .lt. 0.25) then
c           zx=(0.5+cx)*zeta(Istr,j,kstp)+(0.5-cx)*zeta(Istr-1,j,kstp)
c          else
c            zx=( 0.375*zeta(Istr,j,kstp)   +0.125*zeta(Istr-1,j,kstp)
c     &                       +(cx-0.25)*zeta(Istr,j,knew) )/(cx+0.25)
c          endif

          ubar(Istr,j,knew)=0.5*( (1.-cx)*ubar(Istr,j,kstp)
     &                               +cx*ubar(Istr+1,j,kstp)
#    ifdef M2_FRC_BRY
     &                   +ubar_west(j) -hx*(zx-zeta_west(j))
#    else
     &                 +ubclm(Istr,j) -hx*(zx-ssh(Istr-1,j))
#    endif
     &                                                    )







#   elif defined OBC_M2SPECIFIED       /* Island Wake case: convert */
#    ifdef M2_FRC_BRY                  /* specified inflow velocity */
          ubar(Istr,j,knew)=ubar_west(j) /* into constant flux.    */
#     ifdef ISWAKE
     &         *(1.+(zeta_west(j)-zeta(Istr,j,knew))/h(Istr,j))
#     endif
#    else
          ubar(Istr,j,knew)=ubclm(Istr,j)
#    endif
#   else
          ubar(Istr,j,knew)=ubar(Istr+1,j,knew)
#   endif
#   ifdef MASKING
          ubar(Istr,j,knew)=ubar(Istr,j,knew)*umask(Istr,j)
#   endif
        enddo
#  endif
# else
        do j=Jstr,Jend                          ! Western edge closed
          ubar(Istr,j,knew)=0.                  ! ======= ==== ======
        enddo                                   !  (no-flux, default)
# endif           /* OBC_WEST */
      endif     !<-- WESTERN_EDGE

c--#ifdef ISWAKE
c--# define OBC_M2ORLANSKI
c--#endif
 
      if (EASTERN_EDGE) then
# ifdef OBC_EAST
#  ifdef OBC_M2ORLANSKI
!                                           Eastern edge radiation BC
!                                           ======= ==== ========= ==
        do j=Jstr,Jend+1
          grad(Iend  ,j)=(ubar(Iend  ,j,kstp)-ubar(Iend  ,j-1,kstp))
#   ifdef MASKING 
     &                                                *pmask(Iend,j)
#   endif
          grad(Iend+1,j)=(ubar(Iend+1,j,kstp)-ubar(Iend+1,j-1,kstp))
#   ifdef MASKING
     &                                              *pmask(Iend+1,j)
#   endif
        enddo
        do j=Jstr,Jend
          dft=ubar(Iend,j,kstp)-ubar(Iend  ,j,knew)
          dfx=ubar(Iend,j,knew)-ubar(Iend-1,j,knew)
 
          if (dfx*dft .lt. 0.) then
            dft=0.                       ! <-- cancel cx, if inflow
#   if defined M2_FRC_BRY || defined M2NUDGING
            tau=tau_in
          else
            tau=tau_out
#   endif
          endif
 
          if (dft*(grad(Iend,j)+grad(Iend,j+1)) .gt. 0.) then
            dfy=grad(Iend,j)
          else
            dfy=grad(Iend,j+1)
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
 
          ubar(Iend+1,j,knew)=( cff*ubar(Iend+1,j,kstp)
     &                             +cx*ubar(Iend,j,knew)
     &                      -max(cy,0.)*grad(Iend+1,j  )
     &                      -min(cy,0.)*grad(Iend+1,j+1)
     &                                       )/(cff+cx)
#   if defined OBC_FLUX_CORR
     &                                       +uv_crss
#   endif
#   if defined M2_FRC_BRY || defined M2NUDGING
          ubar(Iend+1,j,knew)=(1.-tau)*ubar(Iend+1,j,knew)
#    ifdef M2_FRC_BRY
     &                                    +tau*ubar_east(j)

     &       +attnM2*dtfast*g*0.5*(pm(Iend,j)+pm(Iend+1,j))
     &                    *(zeta(Iend,j,knew)-zeta_east(j))
#    else
     &                                 +tau*ubclm(Iend+1,j)

     &       +attnM2*dtfast*g*0.5*(pm(Iend,j)+pm(Iend+1,j))
     &                   *(zeta(Iend,j,knew)-ssh(Iend+1,j))
#    endif
#   endif
#   ifdef MASKING
          ubar(Iend+1,j,knew)=ubar(Iend+1,j,knew)*umask(Iend+1,j)
#   endif
        enddo
#  else          /* alternative open: Flather, specified, gradient */
        do j=Jstr,Jend
#   if defined OBC_M2FLATHER
c**          ubar(Iend+1,j,knew)= +sqrt(2.*g/(h(Iend,j)+h(Iend+1,j)))
c**     &               *( 0.5*(zeta(Iend,j,knew)+zeta(Iend+1,j,knew))
c**#    ifdef M2_FRC_BRY
c**     &                                 -zeta_east(j) )+ubar_east(j)
c**#    else
c**     &                             -ssh(Iend+1,j) )+ubclm(Iend+1,j)
c**#    endif


          cff=0.5*(h(Iend,j)+h(Iend+1,j))
          hx=sqrt(g/cff)
          cx=dtfast*cff*hx*0.5*(pm(Iend,j)+pm(Iend+1,j))

          zx=(0.5+cx)*zeta(Iend,j,kstp)+(0.5-cx)*zeta(Iend+1,j,kstp)
          if (cx .gt. 0.292893218813452) then
            zx=zx + ( zeta(Iend,j,knew) +cx*zeta(Iend+1,j,kstp)
     &                               -(1.+cx)*zeta(Iend,j,kstp)
     &                           )*(1.-0.292893218813452/cx)**2
          endif




c          if (cx .gt. 0.5) then
c            zx=zeta(Iend,j,knew) -(0.5/cx)*(zeta(Iend,j,knew)
c     &                                     -zeta(Iend,j,kstp))
c          endif

c          if (cx .gt. 0.25) then
c            zx=( 0.375*zeta(Iend,j,kstp) +0.125*zeta(Iend+1,j,kstp)
c     &                     +(cx-0.25)*zeta(Iend,j,knew) )/(cx+0.25)
c          endif




          ubar(Iend+1,j,knew)=0.5*( (1.-cx)*ubar(Iend+1,j,kstp)
     &                                     +cx*ubar(Iend,j,kstp)
#    ifdef M2_FRC_BRY
     &                       +ubar_east(j) +hx*(zx-zeta_east(j))
#    else
     &                   +ubclm(Iend+1,j) +hx*(zx-ssh(Iend+1,j))
#    endif
     &                                                        )






#   elif defined OBC_M2SPECIFIED
#    ifdef M2_FRC_BRY
          ubar(Iend+1,j,knew)=ubar_east(j)
#    else
          ubar(Iend+1,j,knew)=ubclm(Iend+1,j)
#    endif
#   else
          ubar(Iend+1,j,knew)=ubar(Iend,j,knew)
#   endif
#   ifdef MASKING
          ubar(Iend+1,j,knew)=ubar(Iend+1,j,knew)*umask(Iend+1,j)
#   endif
        enddo
#  endif
# else
        do j=Jstr,Jend                       ! Eastern edge closed BC
          ubar(Iend+1,j,knew)=0.             ! ======= ==== ====== ==
        enddo
# endif /* OBC_EAST */
      endif         !<-- EASTERN_EDGE
#endif              /* !EW_PERIODIC */
 


c****
c****
#ifdef OBC_M2FLATHER
# undef OBC_M2FLATHER
# define OBC_M2ORLANSKI
#endif
 
 
#ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
# ifdef OBC_SOUTH
#  if defined OBC_M2ORLANSKI
!                                          Southern edge radiation BC
!                                          ======== ==== ========= ==
        do i=IstrU-1,Iend
          grad(i,Jstr-1)=ubar(i+1,Jstr-1,kstp)-ubar(i,Jstr-1,kstp)
          grad(i,Jstr  )=ubar(i+1,Jstr  ,kstp)-ubar(i,Jstr  ,kstp)
        enddo
        do i=IstrU,Iend

/* --->
          dft=ubar(i,Jstr,kstp)-ubar(i,Jstr  ,knew)
          dfx=ubar(i,Jstr,knew)-ubar(i,Jstr+1,knew)
 
          if (dfx*dft .lt. 0.) then
            dft=0.                        ! <-- cancel cx, if inflow
#   if defined M2_FRC_BRY || defined M2NUDGING
            tau=tau_in
          else
            tau=tau_out
#   endif
          endif
 
          if (dft*(grad(i-1,Jstr)+grad(i,Jstr)) .gt. 0.) then
            dfy=grad(i-1,Jstr)
          else
            dfy=grad(i  ,Jstr)
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
 
          ubar(i,Jstr-1,knew)=( cff*ubar(i,Jstr-1,kstp)
     &                             +cx*ubar(i,Jstr,knew)
     &                      -max(cy,0.)*grad(i-1,Jstr-1)
     &                      -min(cy,0.)*grad(i  ,Jstr-1)
     &                                       )/(cff+cx)


---> */

          cx=-0.125*dtfast*(vbar(i,Jstr,kstp)+vbar(i-1,Jstr,kstp))
     &      *(pn(i,Jstr-1)+pn(i-1,Jstr-1)+pn(i,Jstr)+pn(i-1,Jstr))

          cy= 0.125*dtfast*(ubar(i,Jstr-1,kstp)+ubar(i,Jstr,kstp))
     &      *(pm(i,Jstr-1)+pm(i-1,Jstr-1)+pm(i,Jstr)+pm(i-1,Jstr))


          if (cx.gt.0.) then
            tau=0.
          else
            tau=-cx
            cx=0.
          endif

          ubar(i,Jstr-1,knew)=(1.-cx)*( ubar(i,Jstr-1,kstp)
     &                          -max(cy,0.)*grad(i-1,Jstr-1)
     &                          -min(cy,0.)*grad(i  ,Jstr-1)
     &                                                     )
     &                       +cx*(        ubar(i,Jstr,kstp)
     &                            -max(cy,0.)*grad(i-1,Jstr)
     &                            -min(cy,0.)*grad(i  ,Jstr)
     &                                                     )







#   if defined M2_FRC_BRY || defined M2NUDGING
          ubar(i,Jstr-1,knew)=(1.-tau)*ubar(i,Jstr-1,knew)
#    ifdef M2_FRC_BRY
     &                                  +tau*ubar_south(i)
#    else
     &                                +tau*ubclm(i,Jstr-1)
#    endif
#   endif
#   ifdef MASKING
          ubar(i,Jstr-1,knew)=ubar(i,Jstr-1,knew)*umask(i,Jstr-1)
#   endif
        enddo
#  else          /* alternative open: Chapman, specified, gradient */
        do i=IstrU,Iend
#   if defined OBC_M2FLATHER
          cx=dtfast*0.5*(pn(i-1,Jstr-1)+pn(i,Jstr-1))
     &        *sqrt(0.5*g*(h(i-1,Jstr-1)+h(i,Jstr-1)))

          ubar(i,Jstr-1,knew)=( ubar(i,Jstr-1,kstp)
     &                  +cx*ubar(i,Jstr,knew) )/(1.+cx)
#   elif defined OBC_M2SPECIFIED
#    ifdef M2_FRC_BRY
          ubar(i,Jstr-1,knew)=ubar_south(i)
#    else 
          ubar(i,Jstr-1,knew)=ubclm(i,Jstr-1)
#    endif
#   else
          ubar(i,Jstr-1,knew)=ubar(i,Jstr,knew)
#   endif
#   ifdef MASKING
          ubar(i,Jstr-1,knew)=ubar(i,Jstr-1,knew)*umask(i,Jstr-1)
#   endif
        enddo
#  endif
# else
#  ifdef EW_PERIODIC
#   define I_RANGE IstrU,Iend
#  else
#   define I_RANGE Istr,IendR
#  endif
                                        ! Wall: free-slip (gamma2=+1)
        do i=I_RANGE                    ! ====    no-slip (gamma2=-1)
          ubar(i,Jstr-1,knew)=gamma2*ubar(i,Jstr,knew)
#  ifdef MASKING
     &                                *umask(i,Jstr-1)
#  endif
        enddo
#  undef I_RANGE
# endif              /* OBC_SOUTH */
      endif          !<-- SOUTHERN_EDGE
 
 
 
      if (NORTHERN_EDGE) then
# ifdef OBC_NORTH
#  if defined OBC_M2ORLANSKI
!                                          Northern edge radiation BC
!                                          ======== ==== ========= ==
        do i=IstrU-1,Iend
          grad(i,Jend  )=ubar(i+1,Jend  ,kstp)-ubar(i,Jend,kstp  )
          grad(i,Jend+1)=ubar(i+1,Jend+1,kstp)-ubar(i,Jend+1,kstp)
        enddo
        do i=IstrU,Iend
/* --->
          dft=ubar(i,Jend,kstp)-ubar(i,Jend  ,knew)
          dfx=ubar(i,Jend,knew)-ubar(i,Jend-1,knew)
 
          if (dfx*dft .lt. 0.) then
            dft=0.                        ! <-- cancel cx, if inflow
#   if defined M2_FRC_BRY || defined M2NUDGING
            tau=tau_in
          else
            tau=tau_out
#   endif
          endif
 
          if (dft*(grad(i-1,Jend)+grad(i,Jend)) .gt. 0.) then
            dfy=grad(i-1,Jend)
          else
            dfy=grad(i  ,Jend)
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
 
          ubar(i,Jend+1,knew)=( cff*ubar(i,Jend+1,kstp)
     &                             +cx*ubar(i,Jend,knew)
     &                      -max(cy,0.)*grad(i-1,Jend+1)
     &                      -min(cy,0.)*grad(i  ,Jend+1)
     &                                       )/(cff+cx)
---> */

          cx=0.125*dtfast*(vbar(i,Jend+1,kstp)+vbar(i-1,Jend+1,kstp))
     &         *(pn(i,Jend)+pn(i-1,Jend)+pn(i,Jend+1)+pn(i-1,Jend+1))

          cy=0.125*dtfast*(ubar(i,Jend,kstp)+ubar(i,Jend+1,kstp))
     &         *(pm(i,Jend)+pm(i-1,Jend)+pm(i,Jend+1)+pm(i-1,Jend+1))

          if (cx.gt.0.) then
            tau=0.
          else
            tau=-cx
            cx=0.
          endif

          ubar(i,Jend+1,knew)=(1.-cx)*( ubar(i,Jend+1,kstp)
     &                          -max(cy,0.)*grad(i-1,Jend+1)
     &                          -min(cy,0.)*grad(i  ,Jend+1)
     &                                                     )
     &                       +cx*(        ubar(i,Jend,kstp)
     &                            -max(cy,0.)*grad(i-1,Jend)
     &                            -min(cy,0.)*grad(i  ,Jend)
     &                                                     )





#   if defined M2_FRC_BRY || defined M2NUDGING
          ubar(i,Jend+1,knew)=(1.-tau)*ubar(i,Jend+1,knew)
#    ifdef M2_FRC_BRY
     &                                  +tau*ubar_north(i)
#    else
     &                                +tau*ubclm(i,Jend+1)
#    endif
#   endif
#   ifdef MASKING
          ubar(i,Jend+1,knew)=ubar(i,Jend+1,knew)*umask(i,Jend+1)
#   endif
        enddo
#  else          /* alternative open: Chapman, specified, gradient */
        do i=IstrU,Iend
#   if defined OBC_M2FLATHER
          cx=dtfast*sqrt(0.5*g*(h(i-1,Jend+1)+h(i,Jend+1)))
     &                    *0.5*(pn(i-1,Jend+1)+pn(i,Jend+1))

          ubar(i,Jend+1,knew)=( ubar(i,Jend+1,kstp)
     &                +cx*ubar(i,Jend,knew))/(1.+cx)
#   elif defined OBC_M2SPECIFIED
#    ifdef M2_FRC_BRY
          ubar(i,Jend+1,knew)=ubar_north(i)
#    else
          ubar(i,Jend+1,knew)=ubclm(i,Jend+1)
#    endif
#   else
          ubar(i,Jend+1,knew)=ubar(i,Jend,knew)
#   endif
#   ifdef MASKING
          ubar(i,Jend+1,knew)=ubar(i,Jend+1,knew)*umask(i,Jend+1)
#   endif
        enddo
#  endif
# else
#  ifdef EW_PERIODIC
#   define I_RANGE IstrU,Iend
#  else
#   define I_RANGE Istr,IendR
#  endif
                                       !  Wall: free-slip (gamma2=+1)
        do i=I_RANGE                   !  =====   no-slip (gamma2=-1)
          ubar(i,Jend+1,knew)=gamma2*ubar(i,Jend,knew)
#  ifdef MASKING
     &                                *umask(i,Jend+1)
#  endif
        enddo
#  undef I_RANGE
# endif               /* OBC_NORTH */
      endif          !<-- NORTHERN_EDGE
#endif                /* !NS_PERIODIC */
 
                           ! Corners between adjacent open boundaries
                           ! ======= ======= ======== ==== ==========
 
#if defined OBC_SOUTH && defined OBC_WEST
      if (WESTERN_EDGE .and. SOUTHERN_EDGE) then
        ubar(Istr,Jstr-1,knew)=0.5*( ubar(Istr+1,Jstr-1,knew)
     &                                  +ubar(Istr,Jstr,knew))
      endif
#endif
#if defined OBC_SOUTH && defined OBC_EAST
      if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
        ubar(Iend+1,Jstr-1,knew)=0.5*( ubar(Iend,Jstr-1,knew)
     &                                +ubar(Iend+1,Jstr,knew))
      endif
#endif
#if defined OBC_NORTH && defined OBC_WEST
      if (WESTERN_EDGE .and. NORTHERN_EDGE) then
        ubar(Istr,Jend+1,knew)=0.5*( ubar(Istr+1,Jend+1,knew)
     &                                  +ubar(Istr,Jend,knew))
      endif
#endif
#if defined OBC_NORTH && defined OBC_EAST
      if (EASTERN_EDGE .and. NORTHERN_EDGE) then
        ubar(Iend+1,Jend+1,knew)=0.5*( ubar(Iend,Jend+1,knew)
     &                                +ubar(Iend+1,Jend,knew))
      endif
#endif
      return
      end
 
