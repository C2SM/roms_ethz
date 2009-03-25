#include "cppdefs.h"
#ifdef SOLVE3D
 
      subroutine v3dbc_tile (Istr,Iend,Jstr,Jend, grad)
!
! Set lateral boundary conditions for ETA-component velocity
! v(:,:,:,nnew) 
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k
      real grad(PRIVATE_2D_SCRATCH_ARRAY), eps, dtfwd,
     &     cff, cx,cy, dft,dfx,dfy, tau,tau_in,tau_out
      parameter (eps=1.E-20)
# include "param.h"
# include "scalars.h"
# include "grid.h"
# include "ocean3d.h"
# ifdef M3_FRC_BRY
#  include "boundary.h"
# else
#  include "climat.h"
# endif
!
# include "compute_auxiliary_bounds.h"
!
      if (PRE_STEP) then      ! because predictor sub-step advances
        dtfwd=0.5*dt          ! u, v, t to "n+1/2", the forward step
      else                    ! employed here for upstream advection
        dtfwd=dt              ! in the vicinity of open boundary is
      endif                   ! actually a halfstep.

# define dt illegal
# if defined M3_FRC_BRY || defined M3NUDGING
      tau_in=dtfwd*tauM3_in
      tau_out=dtfwd*tauM3_out
# endif

# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
#  ifdef OBC_SOUTH
#   ifdef OBC_M3ORLANSKI
        do k=1,N                            ! Southern edge radiation
          do i=Istr,Iend+1                  ! ======== ==== =========
            grad(i,Jstr  )=(v(i,Jstr  ,k,nstp)-v(i-1,Jstr  ,k,nstp))
#    ifdef MASKING
     &                                                *pmask(i,Jstr)
#    endif
            grad(i,Jstr+1)=(v(i,Jstr+1,k,nstp)-v(i-1,Jstr+1,k,nstp))
#    ifdef MASKING
     &                                              *pmask(i,Jstr+1)
#    endif
          enddo
          do i=Istr,Iend
            dft=v(i,Jstr+1,k,nstp)-v(i,Jstr+1,k,nnew)
            dfx=v(i,Jstr+1,k,nnew)-v(i,Jstr+2,k,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                      ! <-- cancel cx, if inflow
#    if defined M3_FRC_BRY || defined M3NUDGING
              tau=tau_in
            else
              tau=tau_out
#    endif
            endif
 
            if (dft*(grad(i,Jstr+1)+grad(i+1,Jstr+1)) .gt. 0.) then
              dfy=grad(i,Jstr+1)
            else
              dfy=grad(i+1,Jstr+1)
            endif
 
#    ifdef OBC_RAD_NORMAL
            dfy=0.
#    endif
            cff=max(dfx*dfx+dfy*dfy, eps)
            cx=dft*dfx
#    ifdef OBC_RAD_NPO
            cy=0.
#    else
            cy=min(cff,max(dft*dfy,-cff))
#    endif
 
            v(i,Jstr,k,nnew)=( cff*v(i,Jstr,k,nstp)
     &                        +cx*v(i,Jstr+1,k,nnew)
     &                    -max(cy,0.)*grad(i  ,Jstr)
     &                    -min(cy,0.)*grad(i+1,Jstr)
     &                                   )/(cff+cx)
#    if defined M3_FRC_BRY  || defined M3NUDGING 
            v(i,Jstr,k,nnew)=(1.-tau)*v(i,Jstr,k,nnew)
#     ifdef M3_FRC_BRY
     &                               +tau*v_south(i,k)
#     else     
     &                             +tau*vclm(i,Jstr,k)
#     endif
#    endif
#    ifdef MASKING
            v(i,Jstr,k,nnew)=v(i,Jstr,k,nnew)*vmask(i,Jstr)
#    endif
          enddo
        enddo
#   else           /* alternative open */
        do k=1,N
          do i=Istr,Iend
#    ifdef OBC_M3SPECIFIED
#     ifdef M3_FRC_BRY
            v(i,Jstr,k,nnew)=v_south(i,k)        ! specified
#     else
            v(i,Jstr,k,nnew)=vclm(i,Jstr,k)
#     endif
#    else
            v(i,Jstr,k,nnew)=v(i,Jstr+1,k,nnew)  ! gradient (default)
#    endif
#    ifdef MASKING
     &                           *vmask(i,Jstr)
#    endif
          enddo
        enddo
#   endif
#  else
        do k=1,N                               ! Southern edge closed
          do i=Istr,Iend                       ! ======== ==== ======
            v(i,Jstr,k,nnew)=0.                !  (no-flux: default)
          enddo
        enddo
#  endif              /* OBC_SOUTH */
      endif         !<-- SOUTHERN_EDGE
 
 
 
      if (NORTHERN_EDGE) then
#  ifdef OBC_NORTH
#   ifdef OBC_M3ORLANSKI
        do k=1,N                            ! Northern edge radiation
          do i=Istr,Iend+1                  ! ======== ==== =========
            grad(i,Jend  )=(v(i,Jend  ,k,nstp)-v(i-1,Jend  ,k,nstp))
#    ifdef MASKING
     &                                                *pmask(i,Jend)
#    endif
            grad(i,Jend+1)=(v(i,Jend+1,k,nstp)-v(i-1,Jend+1,k,nstp))
#    ifdef MASKING
     &                                              *pmask(i,Jend+1)
#    endif
          enddo
          do i=Istr,Iend
            dft=v(i,Jend,k,nstp)-v(i,Jend  ,k,nnew)
            dfx=v(i,Jend,k,nnew)-v(i,Jend-1,k,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                       ! <-- cancel cx, if inflow
#    if defined M3_FRC_BRY || defined M3NUDGING
              tau=tau_in
            else
              tau=tau_out
#    endif
            endif
 
            if (dft*(grad(i,Jend)+grad(i+1,Jend)) .gt. 0.) then
              dfy=grad(i,Jend)
            else
              dfy=grad(i+1,Jend)
            endif
 
#    ifdef OBC_RAD_NORMAL
            dfy=0.
#    endif
            cff=max(dfx*dfx+dfy*dfy, eps)
            cx=dft*dfx
#    ifdef OBC_RAD_NPO
            cy=0.
#    else
            cy=min(cff,max(dft*dfy,-cff))
#    endif
 
            v(i,Jend+1,k,nnew)=( cff*v(i,Jend+1,k,nstp)
     &                              +cx*v(i,Jend,k,nnew)
     &                      -max(cy,0.)*grad(i  ,Jend+1)
     &                      -min(cy,0.)*grad(i+1,Jend+1)
     &                                      )/(cff+cx)
#    if defined M3_FRC_BRY  || defined M3NUDGING 
            v(i,Jend+1,k,nnew)=(1.-tau)*v(i,Jend+1,k,nnew)
#     ifdef M3_FRC_BRY
     &                                   +tau*v_north(i,k)
#     else     
     &                               +tau*vclm(i,Jend+1,k)
#     endif
#    endif
#    ifdef MASKING
            v(i,Jend+1,k,nnew)=v(i,Jend+1,k,nnew)*vmask(i,Jend+1)
#    endif
          enddo
        enddo
#   else                /* alternative open */
        do k=1,N
          do i=Istr,Iend
#    ifdef OBC_M3SPECIFIED
#     ifdef M3_FRC_BRY
            v(i,Jend+1,k,nnew)=v_north(i,k)      ! specified
#     else
            v(i,Jend+1,k,nnew)=vclm(i,Jend+1,k)
#     endif
#    else
            v(i,Jend+1,k,nnew)=v(i,Jend,k,nnew)  ! gradient (default)
#    endif
#    ifdef MASKING
     &                         *vmask(i,Jend+1)
#    endif
          enddo
        enddo
#   endif
#  else
        do k=1,N                               ! Northern edge closed
          do i=Istr,Iend                       ! ======== ==== ======
            v(i,Jend+1,k,nnew)=0.              !   (no-flux: default)
          enddo
        enddo
#  endif
      endif     !<--  NORTHERN_EDGE
# endif          /* !NS_PERIODIC */
 
 
 
# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
#  ifdef OBC_WEST
#   ifdef OBC_M3ORLANSKI
        do k=1,N                             ! Western edge radiation
          do j=JstrV-1,Jend                  ! ======= ==== =========
            grad(Istr-1,j)=v(Istr-1,j+1,k,nstp)-v(Istr-1,j,k,nstp)
            grad(Istr  ,j)=v(Istr  ,j+1,k,nstp)-v(Istr  ,j,k,nstp)
          enddo
          do j=JstrV,Jend
/* --->
            dft=v(Istr,j,k,nstp)-v(Istr  ,j,k,nnew)
            dfx=v(Istr,j,k,nnew)-v(Istr+1,j,k,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                       ! <-- cancel cx, if inflow
#    if defined M3_FRC_BRY || defined M3NUDGING
              tau=tau_in
            else
              tau=tau_out
#    endif
            endif
 
            if (dft*(grad(Istr,j-1)+grad(Istr,j)) .gt. 0.) then
              dfy=grad(Istr,j-1)
            else
              dfy=grad(Istr,j  )
            endif
 
#    ifdef OBC_RAD_NORMAL
            dfy=0.
#    endif
            cff=max(dfx*dfx+dfy*dfy, eps)
            cx=dft*dfx
#    ifdef OBC_RAD_NPO
            cy=0.
#    else
            cy=min(cff,max(dft*dfy,-cff))
#    endif
 
            v(Istr-1,j,k,nnew)=( cff*v(Istr-1,j,k,nstp)
     &                              +cx*v(Istr,j,k,nnew)
     &                      -max(cy,0.)*grad(Istr-1,j-1)
     &                      -min(cy,0.)*grad(Istr-1,j  )
     &                                       )/(cff+cx)
---> */


          cx=-0.125*dtfwd*(u(Istr,j,k,nrhs)+u(Istr,j-1,k,nrhs))
     &                            *( pm(Istr-1,j)+pm(Istr-1,j-1)
     &                                +pm(Istr,j)+pm(Istr,j-1) )

          cy= 0.125*dtfwd*(v(Istr-1,j,k,nrhs)+v(Istr,j,k,nrhs))
     &                            *( pn(Istr-1,j)+pn(Istr-1,j-1)
     &                                +pn(Istr,j)+pn(Istr,j-1) )

          if (cx.gt.0.) then
            tau=0.
          else
            tau=-cx
            cx=0.
          endif

          v(Istr-1,j,k,nnew)=(1.-cx)*( v(Istr-1,j,k,nstp)
     &                          -max(cy,0.)*grad(Istr-1,j-1)
     &                          -min(cy,0.)*grad(Istr-1,j  )
     &                                                     )
     &                       +cx*(        v(Istr,j,k,nstp)
     &                            -max(cy,0.)*grad(Istr,j-1)
     &                            -min(cy,0.)*grad(Istr,j  )
     &                                                     )




#    if defined M3_FRC_BRY  || defined M3NUDGING 
            v(Istr-1,j,k,nnew)=(1.-tau)*v(Istr-1,j,k,nnew)
#     ifdef M3_FRC_BRY
     &                                    +tau*v_west(j,k)
#     else     
     &                               +tau*vclm(Istr-1,j,k)
#     endif
#    endif
#    ifdef MASKING
            v(Istr-1,j,k,nnew)=v(Istr-1,j,k,nnew)*vmask(Istr-1,j)
#    endif
          enddo
        enddo
#   else
        do k=1,N
          do j=JstrV,Jend
#    ifdef OBC_M3SPECIFIED
#     ifdef M3_FRC_BRY
            v(Istr-1,j,k,nnew)=v_west(j,k)       ! specified
#     else
            v(Istr-1,j,k,nnew)=vclm(Istr-1,j,k)
#     endif
#    else
            v(Istr-1,j,k,nnew)=v(Istr,j,k,nnew)  ! gradient (default)
#    endif
#    ifdef MASKING
     &                         *vmask(Istr-1,j)
#    endif
          enddo
        enddo
#   endif
#  else
#   ifdef NS_PERIODIC
#    define J_RANGE JstrV,Jend
#   else
#    define J_RANGE Jstr,JendR
#   endif
        do k=1,N                        ! Wall: free-slip (gamma2=+1)
          do j=J_RANGE                  ! =====   no-slip (gamma2=-1)
            v(Istr-1,j,k,nnew)=gamma2*v(Istr,j,k,nnew)
#   ifdef MASKING
     &                                *vmask(Istr-1,j)
#   endif
          enddo
        enddo
#   undef J_RANGE
#  endif
      endif          !<-- WESTERN_EDGE
 
#ifdef ISWAKE
# define OBC_M3ORLANSKI
#endif
 
      if (EASTERN_EDGE) then
#  ifdef OBC_EAST
#   ifdef OBC_M3ORLANSKI
        do k=1,N                             ! Eastern edge radiation
          do j=JstrV-1,Jend                  ! ======= ==== =========
            grad(Iend  ,j)=v(Iend  ,j+1,k,nstp)-v(Iend  ,j,k,nstp)
            grad(Iend+1,j)=v(Iend+1,j+1,k,nstp)-v(Iend+1,j,k,nstp)
          enddo
          do j=JstrV,Jend
/* --->
            dft=v(Iend,j,k,nstp)-v(Iend  ,j,k,nnew)
            dfx=v(Iend,j,k,nnew)-v(Iend-1,j,k,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                       ! <-- cancel cx, if inflow
#    if defined M3_FRC_BRY || defined M3NUDGING
              tau=tau_in
            else
              tau=tau_out
#    endif
            endif
 
            if (dft*(grad(Iend,j-1)+grad(Iend,j)) .gt. 0.) then
              dfy=grad(Iend,j-1)
            else
              dfy=grad(Iend,j  )
            endif
 
#    ifdef OBC_RAD_NORMAL
            dfy=0.
#    endif
            cff=max(dfx*dfx+dfy*dfy, eps)
            cx=dft*dfx
#    ifdef OBC_RAD_NPO
            cy=0.
#    else
            cy=min(cff,max(dft*dfy,-cff))
#    endif
 
            v(Iend+1,j,k,nnew)=( cff*v(Iend+1,j,k,nstp)
     &                              +cx*v(Iend,j,k,nnew)
     &                      -max(cy,0.)*grad(Iend+1,j-1)
     &                      -min(cy,0.)*grad(Iend+1,j  )
     &                                       )/(cff+cx)

---> */

          cx=0.125*dtfwd*(u(Iend+1,j,k,nrhs)+u(Iend+1,j-1,k,nrhs))
     &                               *( pm(Iend+1,j)+pm(Iend+1,j-1)
     &                                   +pm(Iend,j)+pm(Iend,j-1) )

          cy=0.125*dtfwd*(v(Iend,j,k,nrhs)+v(Iend+1,j,k,nrhs))
     &                               *( pn(Iend+1,j)+pn(Iend+1,j-1)
     &                                   +pn(Iend,j)+pn(Iend,j-1) )

          if (cx.gt.0.) then
            tau=0.
          else
            tau=-cx
            cx=0.
          endif

          v(Iend+1,j,k,nnew)=(1.-cx)*(   v(Iend+1,j,k,nstp)
     &                          -max(cy,0.)*grad(Iend+1,j-1)
     &                          -min(cy,0.)*grad(Iend+1,j  )
     &                                                     )
     &                       +cx*(         v(Iend,j,k,nstp)
     &                            -max(cy,0.)*grad(Iend,j-1)
     &                            -min(cy,0.)*grad(Iend,j  )
     &                                                     )



#    if defined M3_FRC_BRY  || defined M3NUDGING 
            v(Iend+1,j,k,nnew)=(1.-tau)*v(Iend+1,j,k,nnew)
#     ifdef M3_FRC_BRY
     &                                    +tau*v_east(j,k)
#     else     
     &                               +tau*vclm(Iend+1,j,k)
#     endif
#    endif
#    ifdef MASKING
            v(Iend+1,j,k,nnew)=v(Iend+1,j,k,nnew)*vmask(Iend+1,j)
#    endif
          enddo
        enddo
#   else               /* alternative open */
        do k=1,N
          do j=Jstr,Jend
#    ifdef OBC_M3SPECIFIED
#     ifdef M3_FRC_BRY
            v(Iend+1,j,k,nnew)=v_east(j,k)       ! specified
#     else
            v(Iend+1,j,k,nnew)=vclm(Iend+1,j,k)
#     endif
#    else
            v(Iend+1,j,k,nnew)=v(Iend,j,k,nnew)  ! gradient (default)
#    endif
#    ifdef MASKING
     &                         *vmask(Iend+1,j)
#    endif
          enddo
        enddo
#   endif
#  else
#   ifdef NS_PERIODIC
#    define J_RANGE JstrV,Jend
#   else
#    define J_RANGE Jstr,JendR
#   endif
        do k=1,N                        ! Wall: free-slip (gamma2=+1)
          do j=J_RANGE                  ! ====    no-slip (gamma2=-1)
            v(Iend+1,j,k,nnew)=gamma2*v(Iend,j,k,nnew)
#   ifdef MASKING
     &                                *vmask(Iend+1,j)
#   endif
          enddo
        enddo
#   undef J_RANGE
#  endif
      endif     !<-- EASTERN_EDGE
# endif          /* !EW_PERIODIC */
 
                           ! Corners between adjacent open boundaries
                           ! ======= ======= ======== ==== ==========
 
# if defined OBC_SOUTH && defined OBC_WEST
      if (WESTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=1,N
          v(Istr-1,Jstr,k,nnew)=0.5*( v(Istr-1,Jstr+1,k,nnew)
     &                               +v(Istr  ,Jstr  ,k,nnew))
        enddo
      endif
# endif
# if defined OBC_SOUTH && defined OBC_EAST
      if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=1,N
          v(Iend+1,Jstr,k,nnew)=0.5*( v(Iend+1,Jstr+1,k,nnew)
     &                               +v(Iend  ,Jstr  ,k,nnew))
        enddo
      endif
# endif
# if defined OBC_NORTH && defined OBC_WEST
      if (WESTERN_EDGE .and. NORTHERN_EDGE) then
        do k=1,N
          v(Istr-1,Jend+1,k,nnew)=0.5*( v(Istr-1,Jend,k,nnew)
     &                                 +v(Istr,Jend+1,k,nnew))
        enddo
      endif
# endif
# if defined OBC_NORTH && defined OBC_EAST
      if (EASTERN_EDGE .and. NORTHERN_EDGE) then
        do k=1,N
          v(Iend+1,Jend+1,k,nnew)=0.5*( v(Iend+1,Jend,k,nnew)
     &                                 +v(Iend,Jend+1,k,nnew))
        enddo
      endif
# endif
      return
      end
#else
      subroutine v3dbc_empty
      end
#endif /* SOLVE3D */
 
