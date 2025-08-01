#include "cppdefs.h"
#ifdef SOLVE3D

      subroutine v3dbc_tile (istr,iend,jstr,jend, grad)

! Set lateral boundary conditions for ETA-component velocity
! v(:,:,:,nnew)

      implicit none
      integer istr,iend,jstr,jend, i,j,k
      real grad(PRIVATE_2D_SCRATCH_ARRAY), cx,cy, cext, cff,
     &                                     dtfwd, dft,dfx,dfy
      real, parameter :: eps=1.E-33
# include "param.h"
# include "scalars.h"
# include "grid.h"
# include "ocean3d.h"
# ifdef M3_FRC_BRY
#  include "boundary.h"
# else
#  include "climat.h"
# endif

# include "compute_auxiliary_bounds.h"

      if (PRED_STAGE) then    ! because predictor sub-step advances
        dtfwd=0.5*dt          ! u, v, t to "n+1/2", the forward step
      else                    ! employed here for upstream advection
        dtfwd=dt              ! in the vicinity of open boundary is
      endif                   ! actually a halfstep.

# define dt illegal

# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
#  ifdef OBC_SOUTH
#   ifdef OBC_M3ORLANSKI
        do k=1,N                            ! Southern edge radiation
          do i=istr,iend+1                  ! ======== ==== =========
            grad(i,jstr  )=(v(i,jstr  ,k,nstp)-v(i-1,jstr  ,k,nstp))
#    ifdef MASKING
     &                                                *pmask(i,jstr)
#    endif
            grad(i,jstr+1)=(v(i,jstr+1,k,nstp)-v(i-1,jstr+1,k,nstp))
#    ifdef MASKING
     &                                              *pmask(i,jstr+1)
#    endif
          enddo
          do i=istr,iend
            dft=v(i,jstr+1,k,nstp)-v(i,jstr+1,k,nnew)
            dfx=v(i,jstr+1,k,nnew)-v(i,jstr+2,k,nnew)

#    ifdef OBC_RAD_NORMAL
            cy=0. ; cff=max(dfx*dfx, eps)
#    else
            if (dft*(grad(i,jstr+1)+grad(i+1,jstr+1)) > 0.) then
              dfy=grad(i,jstr+1)
            else
              dfy=grad(i+1,jstr+1)
            endif
            cff=max(dfx*dfx+dfy*dfy, eps)
#     ifdef OBC_RAD_NPO
            cy=0.
#     else
            cy=min(cff,max(dft*dfy,-cff))
#     endif
#    endif

            cx=dft*dfx
            if (cx < 0.) then     !--> cancel, if inflow
              cx=0. ; cy=0.
#    if defined M3_FRC_BRY || defined M3NUDGING
              if (v_south(i,k) > 0.) then
                cext=v_south(i,k)
              else
                cext=ubind
              endif
              cext=cext * dtfwd * 0.5*(pn(i,jstr-1)+pn(i,jstr))
            else
              cext=0.
#    endif
            endif

            v(i,jstr,k,nnew)=( cff*v(i,jstr,k,nstp)
     &                        +cx*v(i,jstr+1,k,nnew)
     &                     -max(cy,0.)*grad(i  ,jstr)
     &                     -min(cy,0.)*grad(i+1,jstr)
     &                                    )/(cff+cx)
#    if defined M3_FRC_BRY  || defined M3NUDGING
            v(i,jstr,k,nnew)=(1.-cext)*v(i,jstr,k,nnew)
#     ifdef M3_FRC_BRY
     &                               +cext*v_south(i,k)
#     else
     &                             +cext*vclm(i,jstr,k)
#     endif
#    endif
#    ifdef MASKING
            v(i,jstr,k,nnew)=v(i,jstr,k,nnew)*vmask(i,jstr)
#    endif
          enddo
        enddo
#   else           /* alternative open */
        do k=1,N
          do i=istr,iend
#    ifdef OBC_M3SPECIFIED
#     ifdef M3_FRC_BRY
            v(i,jstr,k,nnew)=v_south(i,k)        ! specified
#     else
            v(i,jstr,k,nnew)=vclm(i,jstr,k)
#     endif
#    else
            v(i,jstr,k,nnew)=v(i,jstr+1,k,nnew)  ! gradient (default)
#    endif
#    ifdef MASKING
     &                           *vmask(i,jstr)
#    endif
          enddo
        enddo
#   endif
#  if defined OBC_SOUTH_M3SPECIFIED_TILESTR && defined OBC_SOUTH_M3SPECIFIED_TILEEND
!
!   Force OBC_M2SPECIFIED as southern boundary condition
!   in the specified tile range! MF (following northern bry MM & AH)
!
        if ((mynode.ge.OBC_SOUTH_M3SPECIFIED_TILESTR)
     &                       .and. (mynode.le.OBC_SOUTH_M3SPECIFIED_TILEEND)) then
        do k=1,N
          do i=istr,iend
#   ifdef M3_FRC_BRY
            v(i,jstr,k,nnew)=v_south(i,k)
#   else
            v(i,jstr,k,nnew)=vclm(i,jstr,k)
#   endif
#   ifdef MASKING
     &                         *vmask(i,jstr)
#   endif
          enddo
          enddo
        endif
#  endif /* OBC_SOUTH_M3SPECIFIED_TILESTR */
#  else
        do k=1,N                               ! Southern edge closed
          do i=istr,iend                       ! ======== ==== ======
            v(i,jstr,k,nnew)=0.                !  (no-flux: default)
          enddo
        enddo
#  endif              /* OBC_SOUTH */
      endif         !<-- SOUTHERN_EDGE



      if (NORTHERN_EDGE) then
#  ifdef OBC_NORTH
#   ifdef OBC_M3ORLANSKI
        do k=1,N                            ! Northern edge radiation
          do i=istr,iend+1                  ! ======== ==== =========
            grad(i,jend  )=(v(i,jend  ,k,nstp)-v(i-1,jend  ,k,nstp))
#    ifdef MASKING
     &                                                *pmask(i,jend)
#    endif
            grad(i,jend+1)=(v(i,jend+1,k,nstp)-v(i-1,jend+1,k,nstp))
#    ifdef MASKING
     &                                              *pmask(i,jend+1)
#    endif
          enddo
          do i=istr,iend
            dft=v(i,jend,k,nstp)-v(i,jend  ,k,nnew)
            dfx=v(i,jend,k,nnew)-v(i,jend-1,k,nnew)

#    ifdef OBC_RAD_NORMAL
            cy=0. ; cff=max(dfx*dfx, eps)
#    else
            if (dft*(grad(i,jend)+grad(i+1,jend)) > 0.) then
              dfy=grad(i,jend)
            else
              dfy=grad(i+1,jend)
            endif
            cff=max(dfx*dfx+dfy*dfy, eps)
#     ifdef OBC_RAD_NPO
            cy=0.
#     else
            cy=min(cff, max(dft*dfy, -cff))
#     endif
#    endif

            cx=dft*dfx
            if (cx < 0.) then     !--> cancel, if inflow
              cx=0. ; cy=0.
#    if defined M3_FRC_BRY || defined M3NUDGING
              if (v_north(i,k) < 0.) then
                cext=-v_north(i,k)
              else
                cext=ubind
              endif
              cext=cext * dtfwd * 0.5*(pn(i,jend)+pn(i,jend+1))
            else
              cext=0.
#    endif
            endif

            v(i,jend+1,k,nnew)=( cff*v(i,jend+1,k,nstp)
     &                              +cx*v(i,jend,k,nnew)
     &                      -max(cy,0.)*grad(i  ,jend+1)
     &                      -min(cy,0.)*grad(i+1,jend+1)
     &                                      )/(cff+cx)
#    if defined M3_FRC_BRY  || defined M3NUDGING
            v(i,jend+1,k,nnew)=(1.-cext)*v(i,jend+1,k,nnew)
#     ifdef M3_FRC_BRY
     &                                   +cext*v_north(i,k)
#     else
     &                               +cext*vclm(i,jend+1,k)
#     endif
#    endif
#    ifdef MASKING
            v(i,jend+1,k,nnew)=v(i,jend+1,k,nnew)*vmask(i,jend+1)
#    endif
          enddo
        enddo
#   else                /* alternative open */
        do k=1,N
          do i=istr,iend
#    ifdef OBC_M3SPECIFIED
#     ifdef M3_FRC_BRY
            v(i,jend+1,k,nnew)=v_north(i,k)      ! specified
#     else
            v(i,jend+1,k,nnew)=vclm(i,jend+1,k)
#     endif
#    else
            v(i,jend+1,k,nnew)=v(i,jend,k,nnew)  ! gradient (default)
#    endif
#    ifdef MASKING
     &                         *vmask(i,jend+1)
#    endif
          enddo
        enddo
#   endif
#  else
        do k=1,N                               ! Northern edge closed
          do i=istr,iend                       ! ======== ==== ======
            v(i,jend+1,k,nnew)=0.              !   (no-flux: default)
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
          do j=jstrV-1,jend                  ! ======= ==== =========
            grad(istr-1,j)=v(istr-1,j+1,k,nstp)-v(istr-1,j,k,nstp)
            grad(istr  ,j)=v(istr  ,j+1,k,nstp)-v(istr  ,j,k,nstp)
          enddo
          do j=jstrV,jend
#    ifdef LEGACY_ORLANSKI
            dft=v(istr,j,k,nstp)-v(istr  ,j,k,nnew)
            dfx=v(istr,j,k,nnew)-v(istr+1,j,k,nnew)

#     ifdef OBC_RAD_NORMAL
            cy=0. ; cff=max(dfx*dfx, eps)
#     else
            if (dft*(grad(istr,j-1)+grad(istr,j)) > 0.) then
              dfy=grad(istr,j-1)
            else
              dfy=grad(istr,j  )
            endif
            cff=max(dfx*dfx+dfy*dfy, eps)
#      ifdef OBC_RAD_NPO
            cy=0.
#      else
            cy=min(cff, max(dft*dfy, -cff))
#      endif
#     endif

            cx=dft*dfx
            if (cx < 0.) then     !--> cancel, if inflow
              cx=0. ; cy=0.
#     if defined M3_FRC_BRY || defined M3NUDGING
              cext=dtfwd*ubind * 0.5*(pm(istr-1,j-1)+pm(istr-1,j))
            else
              cext=0.
#     endif
            endif

            v(istr-1,j,k,nnew)=( cff*v(istr-1,j,k,nstp)
     &                              +cx*v(istr,j,k,nnew)
     &                       -max(cy,0.)*grad(istr-1,j-1)
     &                       -min(cy,0.)*grad(istr-1,j  )
     &                                        )/(cff+cx)

#    else


          cx=-0.125*dtfwd*(u(istr,j,k,nrhs)+u(istr,j-1,k,nrhs))
     &                            *( pm(istr-1,j)+pm(istr-1,j-1)
     &                                +pm(istr,j)+pm(istr,j-1) )

          cy= 0.125*dtfwd*(v(istr-1,j,k,nrhs)+v(istr,j,k,nrhs))
     &                            *( pn(istr-1,j)+pn(istr-1,j-1)
     &                                +pn(istr,j)+pn(istr,j-1) )
          if (cx > 0.) then
            cext=0.
          else
            cext=-cx ; cx=0.
          endif

          v(istr-1,j,k,nnew)=(1.-cx)*( v(istr-1,j,k,nstp)
     &                          -max(cy,0.)*grad(istr-1,j-1)
     &                          -min(cy,0.)*grad(istr-1,j  )
     &                                                     )
     &                       +cx*(        v(istr,j,k,nstp)
     &                            -max(cy,0.)*grad(istr,j-1)
     &                            -min(cy,0.)*grad(istr,j  )
     &                                                     )


#    endif /* LEGACY_ORLANSKI */

#    if defined M3_FRC_BRY  || defined M3NUDGING
            v(istr-1,j,k,nnew)=(1.-cext)*v(istr-1,j,k,nnew)
#     ifdef M3_FRC_BRY
     &                                    +cext*v_west(j,k)
#     else
     &                               +cext*vclm(istr-1,j,k)
#     endif
#    endif
#    ifdef MASKING
            v(istr-1,j,k,nnew)=v(istr-1,j,k,nnew)*vmask(istr-1,j)
#    endif
          enddo
        enddo
#   else
        do k=1,N
          do j=jstrV,jend
#    ifdef OBC_M3SPECIFIED
#     ifdef M3_FRC_BRY
            v(istr-1,j,k,nnew)=v_west(j,k)       ! specified
#     else
            v(istr-1,j,k,nnew)=vclm(istr-1,j,k)
#     endif
#    else
            v(istr-1,j,k,nnew)=v(istr,j,k,nnew)  ! gradient (default)
#    endif
#    ifdef MASKING
     &                         *vmask(istr-1,j)
#    endif
          enddo
        enddo
#   endif
#  else
#   ifdef NS_PERIODIC
#    define J_RANGE jstrV,jend
#   else
#    define J_RANGE jstr,jendR
#   endif
        do k=1,N                        ! Wall: free-slip (gamma2=+1)
          do j=J_RANGE                  ! =====   no-slip (gamma2=-1)
            v(istr-1,j,k,nnew)=gamma2*v(istr,j,k,nnew)
#   ifdef MASKING
     &                                *vmask(istr-1,j)
#   endif
          enddo
        enddo
#   undef J_RANGE
#  endif
      endif          !<-- WESTERN_EDGE

#  ifdef ISWAKE_XXX
#   define OBC_M3ORLANSKI
#  endif

      if (EASTERN_EDGE) then
#  ifdef OBC_EAST
#   ifdef OBC_M3ORLANSKI
        do k=1,N                             ! Eastern edge radiation
          do j=jstrV-1,jend                  ! ======= ==== =========
            grad(iend  ,j)=v(iend  ,j+1,k,nstp)-v(iend  ,j,k,nstp)
            grad(iend+1,j)=v(iend+1,j+1,k,nstp)-v(iend+1,j,k,nstp)
          enddo
          do j=jstrV,jend
#    ifdef LEGACY_ORLANSKI
            dft=v(iend,j,k,nstp)-v(iend  ,j,k,nnew)
            dfx=v(iend,j,k,nnew)-v(iend-1,j,k,nnew)

#     ifdef OBC_RAD_NORMAL
            cy=0. ; cff=max(dfx*dfx, eps)
#     else
            if (dft*(grad(iend,j-1)+grad(iend,j)) > 0.) then
              dfy=grad(iend,j-1)
            else
              dfy=grad(iend,j  )
            endif
            cff=max(dfx*dfx+dfy*dfy, eps)
#      ifdef OBC_RAD_NPO
            cy=0.
#      else
            cy=min(cff, max(dft*dfy, -cff))
#      endif
#     endif

            cx=dft*dfx
            if (cx < 0.) then     !--> cancel, if inflow
              cx=0. ; cy=0.
#     if defined M3_FRC_BRY || defined M3NUDGING
              cext=dtfwd*ubind * 0.5*(pm(iend+1,j-1)+pm(iend+1,j))
            else
              cext=0.
#     endif
            endif

            v(iend+1,j,k,nnew)=( cff*v(iend+1,j,k,nstp)
     &                              +cx*v(iend,j,k,nnew)
     &                       -max(cy,0.)*grad(iend+1,j-1)
     &                       -min(cy,0.)*grad(iend+1,j  )
     &                                        )/(cff+cx)

#    else

          cx=0.125*dtfwd*(u(iend+1,j,k,nrhs)+u(iend+1,j-1,k,nrhs))
     &                               *( pm(iend+1,j)+pm(iend+1,j-1)
     &                                   +pm(iend,j)+pm(iend,j-1) )

          cy=0.125*dtfwd*(v(iend,j,k,nrhs)+v(iend+1,j,k,nrhs))
     &                               *( pn(iend+1,j)+pn(iend+1,j-1)
     &                                   +pn(iend,j)+pn(iend,j-1) )
          if (cx > 0.) then
            cext=0.
          else
            cext=-cx ; cx=0.
          endif

          v(iend+1,j,k,nnew)=(1.-cx)*(   v(iend+1,j,k,nstp)
     &                          -max(cy,0.)*grad(iend+1,j-1)
     &                          -min(cy,0.)*grad(iend+1,j  )
     &                                                     )
     &                       +cx*(         v(iend,j,k,nstp)
     &                            -max(cy,0.)*grad(iend,j-1)
     &                            -min(cy,0.)*grad(iend,j  )
     &                                                     )
#    endif /* LEGACY_ORLANSKI */

#    if defined M3_FRC_BRY  || defined M3NUDGING
            v(iend+1,j,k,nnew)=(1.-cext)*v(iend+1,j,k,nnew)
#     ifdef M3_FRC_BRY
     &                                    +cext*v_east(j,k)
#     else
     &                               +cext*vclm(iend+1,j,k)
#     endif
#    endif
#    ifdef MASKING
            v(iend+1,j,k,nnew)=v(iend+1,j,k,nnew)*vmask(iend+1,j)
#    endif
          enddo
        enddo
#   else               /* alternative open */
        do k=1,N
          do j=jstr,jend
#    ifdef OBC_M3SPECIFIED
#     ifdef M3_FRC_BRY
            v(iend+1,j,k,nnew)=v_east(j,k)       ! specified
#     else
            v(iend+1,j,k,nnew)=vclm(iend+1,j,k)
#     endif
#    else
            v(iend+1,j,k,nnew)=v(iend,j,k,nnew)  ! gradient (default)
#    endif
#    ifdef MASKING
     &                         *vmask(iend+1,j)
#    endif
          enddo
        enddo
#   endif
#  if defined OBC_EAST_M3SPECIFIED_TILESTR && defined OBC_EAST_M3SPECIFIED_TILEEND
!
!   Force OBC_M2SPECIFIED as eastern boundary condition
!   in the specified tile range! DL (following northern bry MM & AH)
!
        if ((mynode.ge.OBC_EAST_M3SPECIFIED_TILESTR)
     &                       .and. (mynode.le.OBC_EAST_M3SPECIFIED_TILEEND)) then
        do k=1,N
          do j=jstr,jend
#   ifdef M3_FRC_BRY
            v(iend+1,j,k,nnew)=v_east(j,k)
#   else
            v(iend+1,j,k,nnew)=vclm(iend+1,j,k)
#   endif
#   ifdef MASKING
     &                         *vmask(iend+1,j)
#   endif
          enddo
          enddo
        endif
#  endif /* OBC_EAST_M3SPECIFIED_TILESTR */
#  else
#   ifdef NS_PERIODIC
#    define J_RANGE jstrV,jend
#   else
#    define J_RANGE jstr,jendR
#   endif
        do k=1,N                        ! Wall: free-slip (gamma2=+1)
          do j=J_RANGE                  ! ====    no-slip (gamma2=-1)
            v(iend+1,j,k,nnew)=gamma2*v(iend,j,k,nnew)
#   ifdef MASKING
     &                                *vmask(iend+1,j)
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
          v(istr-1,jstr,k,nnew)=0.5*( v(istr-1,jstr+1,k,nnew)
     &                               +v(istr  ,jstr  ,k,nnew))
        enddo
      endif
# endif
# if defined OBC_SOUTH && defined OBC_EAST
      if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=1,N
          v(iend+1,jstr,k,nnew)=0.5*( v(iend+1,jstr+1,k,nnew)
     &                               +v(iend  ,jstr  ,k,nnew))
        enddo
      endif
# endif
# if defined OBC_NORTH && defined OBC_WEST
      if (WESTERN_EDGE .and. NORTHERN_EDGE) then
        do k=1,N
          v(istr-1,jend+1,k,nnew)=0.5*( v(istr-1,jend,k,nnew)
     &                                 +v(istr,jend+1,k,nnew))
        enddo
      endif
# endif
# if defined OBC_NORTH && defined OBC_EAST
      if (EASTERN_EDGE .and. NORTHERN_EDGE) then
        do k=1,N
          v(iend+1,jend+1,k,nnew)=0.5*( v(iend+1,jend,k,nnew)
     &                                 +v(iend,jend+1,k,nnew))
        enddo
      endif
# endif
      end
#else
      subroutine v3dbc_empty
      end
#endif /* SOLVE3D */

