#include "cppdefs.h"

      subroutine u2dbc_tile (istr,iend,jstr,jend, grad)

! Set lateral boundary conditions for the barotropic (i.e. vertically
! integrated) XI-component velocity ubar(:,:,knew).

      implicit none
      integer istr,iend,jstr,jend, i,j
      real grad(PRIVATE_2D_SCRATCH_ARRAY), cx,cy, cff,
     &         dft,dfx,dfy, tau,tau_in,tau_out, zx,hx
      real, parameter :: eps=1.D-33
#ifdef OBC_FLUX_CORR
      real uv_crss
#endif
#include "param.h"
#include "scalars.h"
#include "grid.h"
#include "ocean2d.h"
#ifdef M2_FRC_BRY
# include "boundary.h"
#else
# include "climat.h"
#endif

#include "compute_auxiliary_bounds.h"

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
        do j=jstr,jend+1
          grad(istr  ,j)=(ubar(istr  ,j,kstp)-ubar(istr  ,j-1,kstp))
#   ifdef MASKING
     &                                                *pmask(istr,j)
#   endif
          grad(istr+1,j)=(ubar(istr+1,j,kstp)-ubar(istr+1,j-1,kstp))
#   ifdef MASKING
     &                                              *pmask(istr+1,j)
#   endif
        enddo
        do j=jstr,jend
          dft=ubar(istr+1,j,kstp)-ubar(istr+1,j,knew)
          dfx=ubar(istr+1,j,knew)-ubar(istr+2,j,knew)

          if (dfx*dft < 0.) then
            dft=0.                       ! <-- cancel cx, if inflow
#   if defined M2_FRC_BRY || defined M2NUDGING
            tau=tau_in
          else
            tau=tau_out
#   endif
          endif

          if (dft*(grad(istr+1,j)+grad(istr+1,j+1)) > 0.) then
            dfy=grad(istr+1,j  )
          else
            dfy=grad(istr+1,j+1)
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

          ubar(istr,j,knew)=( cff*ubar(istr,j,kstp)
     &                       +cx*ubar(istr+1,j,knew)
     &                    -max(cy,0.)*grad(istr,j  )
     &                    -min(cy,0.)*grad(istr,j+1)
     &                                   )/(cff+cx)
#   ifdef OBC_FLUX_CORR
     &                                    -uv_crss
#   endif

! The "attnM2" term below introduces gentle restoring of free
! surface toward its externally-specified state --- "zeta_west"
! in this case via attenuated pressure gradient term, which tends
! to change "ubar" at the boundary in such a way that it reduces
! the mismatch between "zeta_west" and the nearest internal free
! surface value. The attenuation coefficient [typical value
! attnM2=0.01 or less] is choosen to be sufficiently small to not
! interfere with the radiation boundary algorithm, but at the
! same time provide sufficient restoring.

#   if defined M2_FRC_BRY || defined M2NUDGING
          ubar(istr,j,knew)=(1.-tau)*ubar(istr,j,knew)
#    ifdef M2_FRC_BRY
     &                                    +tau*ubar_west(j)

     &       -attnM2*dtfast*g*0.5*(pm(istr-1,j)+pm(istr,j))
     &                    *(zeta(istr,j,knew)-zeta_west(j))
#    else
     &                                   +tau*ubclm(istr,j)

     &       -attnM2*dtfast*g*0.5*(pm(istr-1,j)+pm(istr,j))
     &                   *(zeta(istr,j,knew)-ssh(istr-1,j))
#    endif
#   endif
#   ifdef MASKING
          ubar(istr,j,knew)=ubar(istr,j,knew)*umask(istr,j)
#   endif
        enddo
#  else          /* alternative open: Flather, specified, gradient */
        do j=jstr,jend
#   if defined OBC_M2FLATHER
C**       ubar(istr,j,knew)= -sqrt(2.*g/(h(istr-1,j)+h(istr,j)))
c**  &             *( 0.5*(zeta(istr-1,j,knew)+zeta(istr,j,knew))
#    ifdef M2_FRC_BRY
c**  &                               -zeta_west(j) )+ubar_west(j)
c**  &     -zeta_west(j) )+0.5*(ubar_west(j)+ubar(istr+1,j,knew))
#    else
c**  &                             -ssh(istr-1,j) )+ubclm(istr,j)
#    endif




          cff=0.5*(h(istr-1,j)+h(istr,j))
          hx=sqrt(g/cff)
          cx=dtfast*cff*hx*0.5*(pm(istr-1,j)+pm(istr,j))

          zx=(0.5+cx)*zeta(istr,j,kstp)+(0.5-cx)*zeta(istr-1,j,kstp)
          if (cx > 0.292893218813452) then
            zx=zx + ( zeta(istr,j,knew) +cx*zeta(istr-1,j,kstp)
     &                               -(1.+cx)*zeta(istr,j,kstp)
     &                           )*(1.-0.292893218813452/cx)**2
          endif




c          if (cx < 0.25) then
c           zx=(0.5+cx)*zeta(istr,j,kstp)+(0.5-cx)*zeta(istr-1,j,kstp)
c          else
c            zx=( 0.375*zeta(istr,j,kstp)   +0.125*zeta(istr-1,j,kstp)
c     &                       +(cx-0.25)*zeta(istr,j,knew) )/(cx+0.25)
c          endif

          ubar(istr,j,knew)=0.5*( (1.-cx)*ubar(istr,j,kstp)
     &                               +cx*ubar(istr+1,j,kstp)
#    ifdef M2_FRC_BRY
     &                   +ubar_west(j) -hx*(zx-zeta_west(j))
#    else
     &                 +ubclm(istr,j) -hx*(zx-ssh(istr-1,j))
#    endif
     &                                                    )







#   elif defined OBC_M2SPECIFIED       /* Island Wake case: convert */
#    ifdef M2_FRC_BRY                  /* specified inflow velocity */
          ubar(istr,j,knew)=ubar_west(j) /* into constant flux.    */
#     ifdef ISWAKE
     &         *(1.+(zeta_west(j)-zeta(istr,j,knew))/h(istr,j))
#     endif
#    else
          ubar(istr,j,knew)=ubclm(istr,j)
#    endif
#   else
          ubar(istr,j,knew)=ubar(istr+1,j,knew)
#   endif
#   ifdef MASKING
          ubar(istr,j,knew)=ubar(istr,j,knew)*umask(istr,j)
#   endif
        enddo
#  endif
# else
        do j=jstr,jend                          ! Western edge closed
          ubar(istr,j,knew)=0.                  ! ======= ==== ======
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
        do j=jstr,jend+1
          grad(iend  ,j)=(ubar(iend  ,j,kstp)-ubar(iend  ,j-1,kstp))
#   ifdef MASKING
     &                                                *pmask(iend,j)
#   endif
          grad(iend+1,j)=(ubar(iend+1,j,kstp)-ubar(iend+1,j-1,kstp))
#   ifdef MASKING
     &                                              *pmask(iend+1,j)
#   endif
        enddo
        do j=jstr,jend
          dft=ubar(iend,j,kstp)-ubar(iend  ,j,knew)
          dfx=ubar(iend,j,knew)-ubar(iend-1,j,knew)

          if (dfx*dft < 0.) then
            dft=0.                       ! <-- cancel cx, if inflow
#   if defined M2_FRC_BRY || defined M2NUDGING
            tau=tau_in
          else
            tau=tau_out
#   endif
          endif

          if (dft*(grad(iend,j)+grad(iend,j+1)) > 0.) then
            dfy=grad(iend,j)
          else
            dfy=grad(iend,j+1)
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

          ubar(iend+1,j,knew)=( cff*ubar(iend+1,j,kstp)
     &                             +cx*ubar(iend,j,knew)
     &                      -max(cy,0.)*grad(iend+1,j  )
     &                      -min(cy,0.)*grad(iend+1,j+1)
     &                                       )/(cff+cx)
#   if defined OBC_FLUX_CORR
     &                                       +uv_crss
#   endif
#   if defined M2_FRC_BRY || defined M2NUDGING
          ubar(iend+1,j,knew)=(1.-tau)*ubar(iend+1,j,knew)
#    ifdef M2_FRC_BRY
     &                                    +tau*ubar_east(j)

     &       +attnM2*dtfast*g*0.5*(pm(iend,j)+pm(iend+1,j))
     &                    *(zeta(iend,j,knew)-zeta_east(j))
#    else
     &                                 +tau*ubclm(iend+1,j)

     &       +attnM2*dtfast*g*0.5*(pm(iend,j)+pm(iend+1,j))
     &                   *(zeta(iend,j,knew)-ssh(iend+1,j))
#    endif
#   endif
#   ifdef MASKING
          ubar(iend+1,j,knew)=ubar(iend+1,j,knew)*umask(iend+1,j)
#   endif
        enddo
#  else          /* alternative open: Flather, specified, gradient */
        do j=jstr,jend
#   if defined OBC_M2FLATHER
c**          ubar(iend+1,j,knew)= +sqrt(2.*g/(h(iend,j)+h(iend+1,j)))
c**     &               *( 0.5*(zeta(iend,j,knew)+zeta(iend+1,j,knew))
c**#    ifdef M2_FRC_BRY
c**     &                                 -zeta_east(j) )+ubar_east(j)
c**#    else
c**     &                             -ssh(iend+1,j) )+ubclm(iend+1,j)
c**#    endif


          cff=0.5*(h(iend,j)+h(iend+1,j))
          hx=sqrt(g/cff)
          cx=dtfast*cff*hx*0.5*(pm(iend,j)+pm(iend+1,j))

          zx=(0.5+cx)*zeta(iend,j,kstp)+(0.5-cx)*zeta(iend+1,j,kstp)
          if (cx > 0.292893218813452) then
            zx=zx + ( zeta(iend,j,knew) +cx*zeta(iend+1,j,kstp)
     &                               -(1.+cx)*zeta(iend,j,kstp)
     &                           )*(1.-0.292893218813452/cx)**2
          endif




c          if (cx > 0.5) then
c            zx=zeta(iend,j,knew) -(0.5/cx)*(zeta(iend,j,knew)
c     &                                     -zeta(iend,j,kstp))
c          endif

c          if (cx > 0.25) then
c            zx=( 0.375*zeta(iend,j,kstp) +0.125*zeta(iend+1,j,kstp)
c     &                     +(cx-0.25)*zeta(iend,j,knew) )/(cx+0.25)
c          endif




          ubar(iend+1,j,knew)=0.5*( (1.-cx)*ubar(iend+1,j,kstp)
     &                                     +cx*ubar(iend,j,kstp)
#    ifdef M2_FRC_BRY
     &                       +ubar_east(j) +hx*(zx-zeta_east(j))
#    else
     &                   +ubclm(iend+1,j) +hx*(zx-ssh(iend+1,j))
#    endif
     &                                                        )






#   elif defined OBC_M2SPECIFIED
#    ifdef M2_FRC_BRY
          ubar(iend+1,j,knew)=ubar_east(j)
#    else
          ubar(iend+1,j,knew)=ubclm(iend+1,j)
#    endif
#   else
          ubar(iend+1,j,knew)=ubar(iend,j,knew)
#   endif
#   ifdef MASKING
          ubar(iend+1,j,knew)=ubar(iend+1,j,knew)*umask(iend+1,j)
#   endif
        enddo
#  endif
# else
        do j=jstr,jend                       ! Eastern edge closed BC
          ubar(iend+1,j,knew)=0.             ! ======= ==== ====== ==
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
        do i=istrU-1,iend
          grad(i,jstr-1)=ubar(i+1,jstr-1,kstp)-ubar(i,jstr-1,kstp)
          grad(i,jstr  )=ubar(i+1,jstr  ,kstp)-ubar(i,jstr  ,kstp)
        enddo
        do i=istrU,iend

/* --->
          dft=ubar(i,jstr,kstp)-ubar(i,jstr  ,knew)
          dfx=ubar(i,jstr,knew)-ubar(i,jstr+1,knew)

          if (dfx*dft < 0.) then
            dft=0.                        ! <-- cancel cx, if inflow
#   if defined M2_FRC_BRY || defined M2NUDGING
            tau=tau_in
          else
            tau=tau_out
#   endif
          endif

          if (dft*(grad(i-1,jstr)+grad(i,jstr)) > 0.) then
            dfy=grad(i-1,jstr)
          else
            dfy=grad(i  ,jstr)
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

          ubar(i,jstr-1,knew)=( cff*ubar(i,jstr-1,kstp)
     &                             +cx*ubar(i,jstr,knew)
     &                      -max(cy,0.)*grad(i-1,jstr-1)
     &                      -min(cy,0.)*grad(i  ,jstr-1)
     &                                       )/(cff+cx)


---> */

          cx=-0.125*dtfast*(vbar(i,jstr,kstp)+vbar(i-1,jstr,kstp))
     &      *(pn(i,jstr-1)+pn(i-1,jstr-1)+pn(i,jstr)+pn(i-1,jstr))

          cy= 0.125*dtfast*(ubar(i,jstr-1,kstp)+ubar(i,jstr,kstp))
     &      *(pm(i,jstr-1)+pm(i-1,jstr-1)+pm(i,jstr)+pm(i-1,jstr))


          if (cx>0.) then
            tau=0.
          else
            tau=-cx
            cx=0.
          endif

          ubar(i,jstr-1,knew)=(1.-cx)*( ubar(i,jstr-1,kstp)
     &                          -max(cy,0.)*grad(i-1,jstr-1)
     &                          -min(cy,0.)*grad(i  ,jstr-1)
     &                                                     )
     &                       +cx*(        ubar(i,jstr,kstp)
     &                            -max(cy,0.)*grad(i-1,jstr)
     &                            -min(cy,0.)*grad(i  ,jstr)
     &                                                     )







#   if defined M2_FRC_BRY || defined M2NUDGING
          ubar(i,jstr-1,knew)=(1.-tau)*ubar(i,jstr-1,knew)
#    ifdef M2_FRC_BRY
     &                                  +tau*ubar_south(i)
#    else
     &                                +tau*ubclm(i,jstr-1)
#    endif
#   endif
#   ifdef MASKING
          ubar(i,jstr-1,knew)=ubar(i,jstr-1,knew)*umask(i,jstr-1)
#   endif
        enddo
#  else          /* alternative open: Chapman, specified, gradient */
        do i=istrU,iend
#   if defined OBC_M2FLATHER
          cx=dtfast*0.5*(pn(i-1,jstr-1)+pn(i,jstr-1))
     &        *sqrt(0.5*g*(h(i-1,jstr-1)+h(i,jstr-1)))

          ubar(i,jstr-1,knew)=( ubar(i,jstr-1,kstp)
     &                  +cx*ubar(i,jstr,knew) )/(1.+cx)
#   elif defined OBC_M2SPECIFIED
#    ifdef M2_FRC_BRY
          ubar(i,jstr-1,knew)=ubar_south(i)
#    else
          ubar(i,jstr-1,knew)=ubclm(i,jstr-1)
#    endif
#   else
          ubar(i,jstr-1,knew)=ubar(i,jstr,knew)
#   endif
#   ifdef MASKING
          ubar(i,jstr-1,knew)=ubar(i,jstr-1,knew)*umask(i,jstr-1)
#   endif
        enddo
#  endif
# else
#  ifdef EW_PERIODIC
#   define I_RANGE istrU,iend
#  else
#   define I_RANGE istr,iendR
#  endif
                                        ! Wall: free-slip (gamma2=+1)
        do i=I_RANGE                    ! ====    no-slip (gamma2=-1)
          ubar(i,jstr-1,knew)=gamma2*ubar(i,jstr,knew)
#  ifdef MASKING
     &                                *umask(i,jstr-1)
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
        do i=istrU-1,iend
          grad(i,jend  )=ubar(i+1,jend  ,kstp)-ubar(i,jend,kstp  )
          grad(i,jend+1)=ubar(i+1,jend+1,kstp)-ubar(i,jend+1,kstp)
        enddo
        do i=istrU,iend
/* --->
          dft=ubar(i,jend,kstp)-ubar(i,jend  ,knew)
          dfx=ubar(i,jend,knew)-ubar(i,jend-1,knew)

          if (dfx*dft < 0.) then
            dft=0.                        ! <-- cancel cx, if inflow
#   if defined M2_FRC_BRY || defined M2NUDGING
            tau=tau_in
          else
            tau=tau_out
#   endif
          endif

          if (dft*(grad(i-1,jend)+grad(i,jend)) > 0.) then
            dfy=grad(i-1,jend)
          else
            dfy=grad(i  ,jend)
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

          ubar(i,jend+1,knew)=( cff*ubar(i,jend+1,kstp)
     &                             +cx*ubar(i,jend,knew)
     &                      -max(cy,0.)*grad(i-1,jend+1)
     &                      -min(cy,0.)*grad(i  ,jend+1)
     &                                       )/(cff+cx)
---> */

          cx=0.125*dtfast*(vbar(i,jend+1,kstp)+vbar(i-1,jend+1,kstp))
     &         *(pn(i,jend)+pn(i-1,jend)+pn(i,jend+1)+pn(i-1,jend+1))

          cy=0.125*dtfast*(ubar(i,jend,kstp)+ubar(i,jend+1,kstp))
     &         *(pm(i,jend)+pm(i-1,jend)+pm(i,jend+1)+pm(i-1,jend+1))

          if (cx>0.) then
            tau=0.
          else
            tau=-cx
            cx=0.
          endif

          ubar(i,jend+1,knew)=(1.-cx)*( ubar(i,jend+1,kstp)
     &                          -max(cy,0.)*grad(i-1,jend+1)
     &                          -min(cy,0.)*grad(i  ,jend+1)
     &                                                     )
     &                       +cx*(        ubar(i,jend,kstp)
     &                            -max(cy,0.)*grad(i-1,jend)
     &                            -min(cy,0.)*grad(i  ,jend)
     &                                                     )





#   if defined M2_FRC_BRY || defined M2NUDGING
          ubar(i,jend+1,knew)=(1.-tau)*ubar(i,jend+1,knew)
#    ifdef M2_FRC_BRY
     &                                  +tau*ubar_north(i)
#    else
     &                                +tau*ubclm(i,jend+1)
#    endif
#   endif
#   ifdef MASKING
          ubar(i,jend+1,knew)=ubar(i,jend+1,knew)*umask(i,jend+1)
#   endif
        enddo
#  else          /* alternative open: Chapman, specified, gradient */
        do i=istrU,iend
#   if defined OBC_M2FLATHER
          cx=dtfast*sqrt(0.5*g*(h(i-1,jend+1)+h(i,jend+1)))
     &                    *0.5*(pn(i-1,jend+1)+pn(i,jend+1))

          ubar(i,jend+1,knew)=( ubar(i,jend+1,kstp)
     &                +cx*ubar(i,jend,knew))/(1.+cx)
#   elif defined OBC_M2SPECIFIED
#    ifdef M2_FRC_BRY
          ubar(i,jend+1,knew)=ubar_north(i)
#    else
          ubar(i,jend+1,knew)=ubclm(i,jend+1)
#    endif
#   else
          ubar(i,jend+1,knew)=ubar(i,jend,knew)
#   endif
#   ifdef MASKING
          ubar(i,jend+1,knew)=ubar(i,jend+1,knew)*umask(i,jend+1)
#   endif
        enddo
#  endif
# else
#  ifdef EW_PERIODIC
#   define I_RANGE istrU,iend
#  else
#   define I_RANGE istr,iendR
#  endif
                                       !  Wall: free-slip (gamma2=+1)
        do i=I_RANGE                   !  =====   no-slip (gamma2=-1)
          ubar(i,jend+1,knew)=gamma2*ubar(i,jend,knew)
#  ifdef MASKING
     &                                *umask(i,jend+1)
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
        ubar(istr,jstr-1,knew)=0.5*( ubar(istr+1,jstr-1,knew)
     &                                  +ubar(istr,jstr,knew))
      endif
#endif
#if defined OBC_SOUTH && defined OBC_EAST
      if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
        ubar(iend+1,jstr-1,knew)=0.5*( ubar(iend,jstr-1,knew)
     &                                +ubar(iend+1,jstr,knew))
      endif
#endif
#if defined OBC_NORTH && defined OBC_WEST
      if (WESTERN_EDGE .and. NORTHERN_EDGE) then
        ubar(istr,jend+1,knew)=0.5*( ubar(istr+1,jend+1,knew)
     &                                  +ubar(istr,jend,knew))
      endif
#endif
#if defined OBC_NORTH && defined OBC_EAST
      if (EASTERN_EDGE .and. NORTHERN_EDGE) then
        ubar(iend+1,jend+1,knew)=0.5*( ubar(iend,jend+1,knew)
     &                                +ubar(iend+1,jend,knew))
      endif
#endif
      end

