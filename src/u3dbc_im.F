#include "cppdefs.h"
#ifdef SOLVE3D
 
      subroutine u3dbc_tile (Istr,Iend,Jstr,Jend, grad)
!
! Set lateral boundary conditions for XI-component velocity
! u(:,:,:,nnew).
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

# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
#  ifdef OBC_WEST
#   ifdef OBC_M3ORLANSKI
        do k=1,N                             ! Western edge radiation
          do j=Jstr,Jend+1                   ! ======= ==== =========
            grad(Istr  ,j)=(u(Istr  ,j,k,nstp)-u(Istr  ,j-1,k,nstp))
#    ifdef MASKING
     &                                                *pmask(Istr,j)
#    endif
            grad(Istr+1,j)=(u(Istr+1,j,k,nstp)-u(Istr+1,j-1,k,nstp))
#    ifdef MASKING
     &                                              *pmask(Istr+1,j)
#    endif
          enddo
          do j=Jstr,Jend
            dft=u(Istr+1,j,k,nstp)-u(Istr+1,j,k,nnew)
            dfx=u(Istr+1,j,k,nnew)-u(Istr+2,j,k,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                       ! <-- cancel cx, if inflow
#    if defined M3_FRC_BRY || defined M3NUDGING
              tau=tau_in
            else
              tau=tau_out
#    endif
            endif
 
            if (dft*(grad(Istr+1,j)+grad(Istr+1,j+1)) .gt. 0.) then
              dfy=grad(Istr+1,j)
            else
              dfy=grad(Istr+1,j+1)
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
 
            u(Istr,j,k,nnew)=( cff*u(Istr,j,k,nstp)
     &                        +cx*u(Istr+1,j,k,nnew)
     &                    -max(cy,0.)*grad(Istr,j  )
     &                    -min(cy,0.)*grad(Istr,j+1)
     &                                   )/(cff+cx)
#    if defined M3_FRC_BRY  || defined M3NUDGING 
            u(Istr,j,k,nnew)=(1.-tau)*u(Istr,j,k,nnew)
#     ifdef M3_FRC_BRY
     &                                +tau*u_west(j,k)
#     else     
     &                             +tau*uclm(Istr,j,k)
#     endif
#    endif
#    ifdef MASKING
            u(Istr,j,k,nnew)=u(Istr,j,k,nnew)*umask(Istr,j)
#    endif
          enddo
        enddo
#   else            /* alternative open */
        do k=1,N
          do j=Jstr,Jend
#    ifdef OBC_M3SPECIFIED
#     ifdef M3_FRC_BRY
            u(Istr,j,k,nnew)=u_west(j,k)         ! specified
#     else
            u(Istr,j,k,nnew)=uclm(Istr,j,k)
#     endif
#    else
            u(Istr,j,k,nnew)=u(Istr+1,j,k,nnew)  ! Gradient: default
#    endif
#    ifdef MASKING
     &                           *umask(Istr,j)
#    endif
          enddo
        enddo
#   endif
#  else           /* alternative to open */
        do k=1,N                               ! Western edge closed
          do j=Jstr,Jend                       ! ======= ==== ======
            u(Istr,j,k,nnew)=0.                !  (no-flux: default)
          enddo
        enddo
#  endif              /* OBC_WEST */
      endif         !<-- WESTERN_EDGE

#ifdef ISWAKE
# define OBC_M3ORLANSKI
#endif
 
      if (EASTERN_EDGE) then
#  ifdef OBC_EAST
#   ifdef OBC_M3ORLANSKI
        do k=1,N                             ! Eastern edge radiation
          do j=Jstr,Jend+1                   ! ======= ==== =========
            grad(Iend  ,j)=(u(Iend  ,j,k,nstp)-u(Iend  ,j-1,k,nstp))
#    ifdef MASKING
     &                                                *pmask(Iend,j)
#    endif
            grad(Iend+1,j)=(u(Iend+1,j,k,nstp)-u(Iend+1,j-1,k,nstp))
#    ifdef MASKING
     &                                              *pmask(Iend+1,j)
#    endif
          enddo
          do j=Jstr,Jend
            dft=u(Iend,j,k,nstp)-u(Iend  ,j,k,nnew)
            dfx=u(Iend,j,k,nnew)-u(Iend-1,j,k,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                      ! <-- cancel cx, if inflow
#    if defined M3_FRC_BRY || defined M3NUDGING
              tau=tau_in
            else
              tau=tau_out
#    endif
            endif
 
            if (dft*(grad(Iend,j)+grad(Iend,j+1)) .gt. 0.) then
              dfy=grad(Iend,j)
            else
              dfy=grad(Iend,j+1)
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
 
            u(Iend+1,j,k,nnew)=( cff*u(Iend+1,j,k,nstp)
     &                              +cx*u(Iend,j,k,nnew)
     &                      -max(cy,0.)*grad(Iend+1,j  )
     &                      -min(cy,0.)*grad(Iend+1,j+1)
     &                                       )/(cff+cx)
#    if defined M3_FRC_BRY  || defined M3NUDGING 
            u(Iend+1,j,k,nnew)=(1.-tau)*u(Iend+1,j,k,nnew)
#     ifdef M3_FRC_BRY
     &                                    +tau*u_east(j,k)
#     else     
     &                               +tau*uclm(Iend+1,j,k)
#     endif
#    endif
#    ifdef MASKING
            u(Iend+1,j,k,nnew)=u(Iend+1,j,k,nnew)*umask(Iend+1,j)
#    endif
          enddo
        enddo
#   else                /* alternative open */
        do k=1,N
          do j=Jstr,Jend
#    ifdef OBC_M3SPECIFIED
#     ifdef M3_FRC_BRY
            u(Iend+1,j,k,nnew)=u_east(j,k)       ! specified
#     else
            u(Iend+1,j,k,nnew)=uclm(Iend+1,j,k)
#     endif
#    else
            u(Iend+1,j,k,nnew)=u(Iend,j,k,nnew)  ! gradient (default)
#    endif
#    ifdef MASKING
     &                         *umask(Iend+1,j)
#    endif
          enddo
        enddo
#   endif
#  else
        do k=1,N                                ! Eastern edge closed
          do j=Jstr,Jend                        ! ======= ==== ======
            u(Iend+1,j,k,nnew)=0.               !  (no-flux: default)
          enddo
        enddo
#  endif
      endif         !<-- EASTERN_EDGE
# endif             /* !EW_PERIODIC */
 
 
 
# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
#  ifdef OBC_SOUTH
#   ifdef OBC_M3ORLANSKI
        do k=1,N                            ! Southern edge radiation
          do i=IstrU-1,Iend                 ! ======== ==== =========
            grad(i,Jstr-1)=u(i+1,Jstr-1,k,nstp)-u(i,Jstr-1,k,nstp)
            grad(i,Jstr  )=u(i+1,Jstr  ,k,nstp)-u(i,Jstr  ,k,nstp)
          enddo
          do i=IstrU,Iend
/* --->
            dft=u(i,Jstr,k,nstp)-u(i,Jstr  ,k,nnew)
            dfx=u(i,Jstr,k,nnew)-u(i,Jstr+1,k,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                       ! <-- cancel cx, if inflow
#    if defined M3_FRC_BRY || defined M3NUDGING
              tau=tau_in
            else
              tau=tau_out
#    endif
            endif
 
            if (dft*(grad(i-1,Jstr)+grad(i,Jstr)) .gt. 0.) then
              dfy=grad(i-1,Jstr)
            else
              dfy=grad(i  ,Jstr)
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
            u(i,Jstr-1,k,nnew)=( cff*u(i,Jstr-1,k,nstp)
     &                              +cx*u(i,Jstr,k,nnew)
     &                      -max(cy,0.)*grad(i-1,Jstr-1)
     &                      -min(cy,0.)*grad(i  ,Jstr-1)
     &                                       )/(cff+cx)

---> */


          cx=-0.125*dtfwd*(v(i,Jstr,k,nrhs)+v(i-1,Jstr,k,nrhs))
     &                            *( pn(i,Jstr-1)+pn(i-1,Jstr-1)
     &                                +pn(i,Jstr)+pn(i-1,Jstr) )

          cy= 0.125*dtfwd*(u(i,Jstr-1,k,nrhs)+u(i,Jstr,k,nrhs))
     &                            *( pm(i,Jstr-1)+pm(i-1,Jstr-1)
     &                                +pm(i,Jstr)+pm(i-1,Jstr) )

          if (cx.gt.0.) then
            tau=0.
          else
            tau=-cx
            cx=0.
          endif

          u(i,Jstr-1,k,nnew)=(1.-cx)*(   u(i,Jstr-1,k,nstp)
     &                          -max(cy,0.)*grad(i-1,Jstr-1)
     &                          -min(cy,0.)*grad(i  ,Jstr-1)
     &                                                     )
     &                       +cx*(         u(i,Jstr,k,nstp)
     &                            -max(cy,0.)*grad(i-1,Jstr)
     &                            -min(cy,0.)*grad(i  ,Jstr)
     &                                                     )







#    if defined M3_FRC_BRY  || defined M3NUDGING 
           u(i,Jstr-1,k,nnew)=(1.-tau)*u(i,Jstr-1,k,nnew)
#     ifdef M3_FRC_BRY
     &                                  +tau*u_south(i,k)
#     else     
     &                              +tau*uclm(i,Jstr-1,k)
#     endif
#    endif
#    ifdef MASKING
            u(i,Jstr-1,k,nnew)=u(i,Jstr-1,k,nnew)*umask(i,Jstr-1)
#    endif
          enddo
        enddo
#   else
        do k=1,N
          do i=IstrU,Iend
#    ifdef OBC_M3SPECIFIED
#     ifdef M3_FRC_BRY
            u(i,Jstr-1,k,nnew)=u_south(i,k)      ! specified
#     else
            u(i,Jstr-1,k,nnew)=uclm(i,Jstr-1,k)
#     endif
#    else
            u(i,Jstr-1,k,nnew)=u(i,Jstr,k,nnew)  ! gradient (default)
#    endif
#    ifdef MASKING
     &                         *umask(i,Jstr-1)
#    endif
          enddo
        enddo
#   endif
#  else
#   ifdef EW_PERIODIC
#    define I_RANGE IstrU,Iend
#   else
#    define I_RANGE Istr,IendR
#   endif
        do k=1,N                        ! Wall: free-slip (gamma2=+1)
          do i=I_RANGE                  ! =====   no-slip (gamma2=-1)
            u(i,Jstr-1,k,nnew)=gamma2*u(i,Jstr,k,nnew)
#   ifdef MASKING
     &                                *umask(i,Jstr-1)
#   endif
          enddo
        enddo
#   undef I_RANGE
#  endif
      endif              !<-- SOUTHERN_EDGE
 
 
 
      if (NORTHERN_EDGE) then
#  ifdef OBC_NORTH
#   ifdef OBC_M3ORLANSKI
        do k=1,N                            ! Northern edge radiation
          do i=IstrU-1,Iend                 ! ======== ==== =========
            grad(i,Jend  )=u(i+1,Jend  ,k,nstp)-u(i,Jend  ,k,nstp)
            grad(i,Jend+1)=u(i+1,Jend+1,k,nstp)-u(i,Jend+1,k,nstp)
          enddo
          do i=IstrU,Iend
/* --->
            dft=u(i,Jend,k,nstp)-u(i,Jend  ,k,nnew)
            dfx=u(i,Jend,k,nnew)-u(i,Jend-1,k,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                                 ! <-- INFLOW
#    if defined M3_FRC_BRY || defined M3NUDGING
              tau=tau_in
            else
              tau=tau_out
#    endif
            endif
 
            if (dft*(grad(i-1,Jend)+grad(i,Jend)) .gt. 0.) then
              dfy=grad(i-1,Jend)
            else
              dfy=grad(i  ,Jend)
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
 
            u(i,Jend+1,k,nnew)=( cff*u(i,Jend+1,k,nstp)
     &                              +cx*u(i,Jend,k,nnew)
     &                      -max(cy,0.)*grad(i-1,Jend+1)
     &                      -min(cy,0.)*grad(i  ,Jend+1)
     &                                       )/(cff+cx)
---> */


          cx=0.125*dtfwd*(v(i,Jend+1,k,nrhs)+v(i-1,Jend+1,k,nrhs))
     &                               *( pn(i,Jend+1)+pn(i-1,Jend+1)
     &                                   +pn(i,Jend)+pn(i-1,Jend) )

          cy=0.125*dtfwd*(u(i,Jend,k,nrhs)+u(i,Jend+1,k,nrhs))
     &                               *( pm(i,Jend+1)+pm(i-1,Jend+1)
     &                                   +pm(i,Jend)+pm(i-1,Jend) )

          if (cx.gt.0.) then
            tau=0.
          else
            tau=-cx
            cx=0.
          endif

          u(i,Jend+1,k,nnew)=(1.-cx)*(  u(i,Jend+1,k,nstp)
     &                          -max(cy,0.)*grad(i-1,Jend+1)
     &                          -min(cy,0.)*grad(i  ,Jend+1)
     &                                                     )
     &                       +cx*(         u(i,Jend,k,nstp)
     &                            -max(cy,0.)*grad(i-1,Jend)
     &                            -min(cy,0.)*grad(i  ,Jend)
     &                                                     )



#    if defined M3_FRC_BRY  || defined M3NUDGING 
            u(i,Jend+1,k,nnew)=(1.-tau)*u(i,Jend+1,k,nnew)
#     ifdef M3_FRC_BRY
     &                                   +tau*u_north(i,k)
#     else     
     &                               +tau*uclm(i,Jend+1,k)
#     endif
#    endif
#    ifdef MASKING
            u(i,Jend+1,k,nnew)=u(i,Jend+1,k,nnew)*umask(i,Jend+1)
#    endif
          enddo
        enddo
#   else               /* alternative open */
        do k=1,N
          do i=IstrU,Iend
#    ifdef OBC_M3SPECIFIED
#     ifdef M3_FRC_BRY
            u(i,Jend+1,k,nnew)=u_north(i,k)      ! specified
#     else
            u(i,Jend+1,k,nnew)=uclm(i,Jend+1,k)
#     endif
#    else
            u(i,Jend+1,k,nnew)=u(i,Jend,k,nnew)  ! gradient (default)
#    endif
#    ifdef MASKING
     &                         *umask(i,Jend+1)
#    endif
          enddo
        enddo
#   endif
#  else
#   ifdef EW_PERIODIC
#    define I_RANGE IstrU,Iend
#   else
#    define I_RANGE Istr,IendR
#   endif
        do k=1,N                        ! Wall: free-slip (gamma2=+1)
          do i=I_RANGE                  ! =====   no-slip (gamma2=-1)
            u(i,Jend+1,k,nnew)=gamma2*u(i,Jend,k,nnew)
#   ifdef MASKING
     &                                *umask(i,Jend+1)
#   endif
          enddo
        enddo
#   undef I_RANGE
#  endif
      endif   !<-- NORTHERN_EDGE
# endif        /* !NS_PERIODIC */
 
                           ! Corners between adjacent open boundaries
                           ! ======= ======= ======== ==== ==========
 
# if defined OBC_SOUTH && defined OBC_WEST
      if (WESTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=1,N
          u(Istr,Jstr-1,k,nnew)=0.5*( u(Istr+1,Jstr-1,k,nnew)
     &                               +u(Istr  ,Jstr  ,k,nnew))
        enddo
      endif
# endif
# if defined OBC_SOUTH && defined OBC_EAST
      if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=1,N
          u(Iend+1,Jstr-1,k,nnew)=0.5*( u(Iend,Jstr-1,k,nnew)
     &                                 +u(Iend+1,Jstr,k,nnew))
        enddo
      endif
# endif
# if defined OBC_NORTH && defined OBC_WEST
      if (WESTERN_EDGE .and. NORTHERN_EDGE) then
        do k=1,N
          u(Istr,Jend+1,k,nnew)=0.5*( u(Istr+1,Jend+1,k,nnew)
     &                               +u(Istr  ,Jend  ,k,nnew))
        enddo
      endif
# endif
# if defined OBC_NORTH && defined OBC_EAST
      if (EASTERN_EDGE .and. NORTHERN_EDGE) then
        do k=1,N
          u(Iend+1,Jend+1,k,nnew)=0.5*( u(Iend,Jend+1,k,nnew)
     &                                 +u(Iend+1,Jend,k,nnew))
        enddo
      endif
# endif
      return
      end
#else
      subroutine u3dbc_empty
      end
#endif /* SOLVE3D */
