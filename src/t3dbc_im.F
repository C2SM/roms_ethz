#include "cppdefs.h"
#ifdef SOLVE3D
 
      subroutine t3dbc_tile (Istr,Iend,Jstr,Jend, itrc, grad)
!
! Set lateral boundary conditions for tracer field t(:,:,:,itrc)
!
      implicit none
      integer Istr,Iend,Jstr,Jend, itrc, i,j,k
      real grad(PRIVATE_2D_SCRATCH_ARRAY), eps, dtfwd,
     &     cff, cx,cy, dft,dfx,dfy, tau,tau_in,tau_out
      parameter (eps=1.E-20)
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "climat.h"
# include "scalars.h"
# include "boundary.h"
!
# include "compute_auxiliary_bounds.h"
!
      if (PRE_STEP) then      ! because predictor sub-step advances
        dtfwd=0.5*dt          ! u, v, t to "n+1/2", the forward step
      else                    ! employed here for upstream advection
        dtfwd=dt              ! in the vicinity of open boundary is
      endif                   ! actually a halfstep.

# define dt illegal
# if defined T_FRC_BRY || defined TNUDGING
      tau_in=dtfwd*tauT_in
      tau_out=dtfwd*tauT_out
# endif

# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
#  if defined OBC_WEST && defined OBC_TORLANSKI
        do k=1,N
          do j=Jstr,Jend+1
            grad(Istr-1,j)=( t(Istr-1,j  ,k,nstp,itrc)
     &                      -t(Istr-1,j-1,k,nstp,itrc))
#   ifdef MASKING
     &                                 *vmask(Istr-1,j)
#   endif
            grad(Istr  ,j)=( t(Istr  ,j  ,k,nstp,itrc)
     &                      -t(Istr  ,j-1,k,nstp,itrc))
#   ifdef MASKING
     &                                   *vmask(Istr,j)
#   endif
          enddo
          do j=Jstr,Jend
/* --->
            dft=t(Istr,j,k,nstp,itrc)-t(Istr  ,j,k,nnew,itrc)
            dfx=t(Istr,j,k,nnew,itrc)-t(Istr+1,j,k,nnew,itrc)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                  ! <-- cancel, if inflow 
#   if defined T_FRC_BRY || defined TNUDGING
              tau=tau_in
            else
              tau=tau_out
#   endif
            endif
 
            if (dft*(grad(Istr,j)+grad(Istr,j+1)) .gt. 0.) then
              dfy=grad(Istr,j)
            else
              dfy=grad(Istr,j+1)
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
            t(Istr-1,j,k,nnew,itrc)=( cff*t(Istr-1,j,k,nstp,itrc)
     &                                   +cx*t(Istr,j,k,nnew,itrc)
     &                                -max(cy,0.)*grad(Istr-1,j  )
     &                                -min(cy,0.)*grad(Istr-1,j+1)
     &                                                 )/(cff+cx)

---> */



            cx=-dtfwd*u(Istr,j,k,nrhs)*pm(Istr-1,j)
            cy=0.5*dtfwd*(v(Istr-1,j,k,nrhs)+v(Istr-1,j+1,k,nrhs))
     &                                               *pn(Istr-1,j)

            if (cx.gt.0.) then
              tau=0.
            else
              tau=-cx
              cx=0.
            endif

            t(Istr-1,j,k,nnew,itrc)=(1.-cx)*( t(Istr-1,j,k,nstp,itrc)
     &                                   -max(cy,0.)*grad(Istr-1,j  )
     &                                   -min(cy,0.)*grad(Istr-1,j+1)
     &                                                              )
     &                                  +cx*(  t(Istr,j,k,nstp,itrc)
     &                                     -max(cy,0.)*grad(Istr,j  )
     &                                     -min(cy,0.)*grad(Istr,j+1)
     &                                                              )



#   if defined T_FRC_BRY || defined TNUDGING
            t(Istr-1,j,k,nnew,itrc)=(1.-tau)*t(Istr-1,j,k,nnew,itrc)
#    ifdef T_FRC_BRY
     &                                    +tau*t_west(j,k,itrc)     
#    else
     &                                    +tau*tclm(Istr-1,j,k,itrc)
#    endif
#   endif
#   ifdef MASKING
            t(Istr-1,j,k,nnew,itrc)=t(Istr-1,j,k,nnew,itrc)
     &                                      *rmask(Istr-1,j)
#   endif
          enddo
        enddo
#  else
        do k=1,N
          do j=Jstr,Jend
#   if defined OBC_WEST && defined OBC_TSPECIFIED
#    ifdef T_FRC_BRY
            t(Istr-1,j,k,nnew,itrc)=t_west(j,k,itrc)
#    else
            t(Istr-1,j,k,nnew,itrc)=tclm(Istr-1,j,k,itrc)
#    endif
#   else
            t(Istr-1,j,k,nnew,itrc)=t(Istr,j,k,nnew,itrc)
#   endif
#   ifdef MASKING
     &                                   *rmask(Istr-1,j)
#   endif
          enddo
        enddo
#  endif
      endif     ! <-- WESTERN_EDGE

#ifdef ISWAKE
# define OBC_TORLANSKI
#endif

      if (EASTERN_EDGE) then
#  if defined OBC_EAST && defined OBC_TORLANSKI
!
!                                        !  Eastern edge radiation BC
        do k=1,N                         !  ======= ==== ========= ==
          do j=Jstr,Jend+1
           grad(Iend  ,j)=( t(Iend  ,j  ,k,nstp,itrc)
     &                     -t(Iend  ,j-1,k,nstp,itrc))
#   ifdef MASKING
     &                                  *vmask(Iend,j)
#   endif
           grad(Iend+1,j)=( t(Iend+1,j  ,k,nstp,itrc)
     &                     -t(Iend+1,j-1,k,nstp,itrc))
#   ifdef MASKING
     &                                *vmask(Iend+1,j)
#   endif
          enddo
          do j=Jstr,Jend
/* --->
            dft=t(Iend,j,k,nstp,itrc)-t(Iend  ,j,k,nnew,itrc)
            dfx=t(Iend,j,k,nnew,itrc)-t(Iend-1,j,k,nnew,itrc)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                  ! <-- cancel, if inflow 
#   if defined T_FRC_BRY || defined TNUDGING
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
 
            t(Iend+1,j,k,nnew,itrc)=( cff*t(Iend+1,j,k,nstp,itrc)
     &                                   +cx*t(Iend,j,k,nnew,itrc)
     &                                -max(cy,0.)*grad(Iend+1,j  )
     &                                -min(cy,0.)*grad(Iend+1,j+1)
     &                                                 )/(cff+cx)

---> */


            cx=dtfwd*u(Iend+1,j,k,nrhs)*pm(Iend+1,j)
            cy=0.5*dtfwd*(v(Iend+1,j,k,nrhs)+v(Iend+1,j+1,k,nrhs))
     &                                               *pn(Iend+1,j)
          
            if (cx.gt.0.) then
              tau=0.
            else
              tau=-cx
              cx=0.
            endif

            t(Iend+1,j,k,nnew,itrc)=(1.-cx)*( t(Iend+1,j,k,nstp,itrc)
     &                                   -max(cy,0.)*grad(Iend+1,j  )
     &                                   -min(cy,0.)*grad(Iend+1,j+1)
     &                                                              )
     &                                  +cx*(  t(Iend,j,k,nnew,itrc)
     &                                     -max(cy,0.)*grad(Iend,j  )
     &                                     -min(cy,0.)*grad(Iend,j+1)
     &                                                              )


#   if defined T_FRC_BRY || defined TNUDGING
            t(Iend+1,j,k,nnew,itrc)=(1.-tau)*t(Iend+1,j,k,nnew,itrc)
#    ifdef T_FRC_BRY
     &                                         +tau*t_east(j,k,itrc)
#    else
     &                                    +tau*tclm(Iend+1,j,k,itrc)
#    endif
#   endif
#   ifdef MASKING
            t(Iend+1,j,k,nnew,itrc)=t(Iend+1,j,k,nnew,itrc)
     &                                     *rmask(Iend+1,j)
#   endif
          enddo
        enddo
#  else
        do k=1,N
          do j=Jstr,Jend
#   if defined OBC_EAST && defined OBC_TSPECIFIED
#    ifdef T_FRC_BRY
            t(Iend+1,j,k,nnew,itrc)=t_east(j,k,itrc) 
#    else
            t(Iend+1,j,k,nnew,itrc)=tclm(Iend+1,j,k,itrc)
#    endif
#   else
            t(Iend+1,j,k,nnew,itrc)=t(Iend,j,k,nnew,itrc)
#   endif
#   ifdef MASKING
     &                                   *rmask(Iend+1,j)
#   endif
          enddo
        enddo
#  endif
      endif    ! <-- EASTERN_EDGE
# endif        /* !EW_PERIODIC */






 
# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
#  if defined OBC_SOUTH && defined OBC_TORLANSKI
        do k=1,N
          do i=Istr,Iend+1
            grad(i,Jstr  )=( t(i  ,Jstr  ,k,nstp,itrc)
     &                      -t(i-1,Jstr  ,k,nstp,itrc))
#   ifdef MASKING
     &                                   *umask(i,Jstr)
#   endif
            grad(i,Jstr-1)=( t(i  ,Jstr-1,k,nstp,itrc)
     &                      -t(i-1,Jstr-1,k,nstp,itrc))
#   ifdef MASKING
     &                                *umask(i,Jstr-1)
#   endif
          enddo
          do i=Istr,Iend
/* --->
            dft=t(i,Jstr,k,nstp,itrc)-t(i,Jstr  ,k,nnew,itrc)
            dfx=t(i,Jstr,k,nnew,itrc)-t(i,Jstr+1,k,nnew,itrc)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                   ! <-- cancel, if inflow 
#   if defined T_FRC_BRY || defined TNUDGING
              tau=tau_in
            else
              tau=tau_out
#   endif
            endif
 
            if (dft*(grad(i,Jstr)+grad(i+1,Jstr)) .gt. 0.) then
              dfy=grad(i,Jstr)
            else
              dfy=grad(i+1,Jstr)
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
 
            t(i,Jstr-1,k,nnew,itrc)=( cff*t(i,Jstr-1,k,nstp,itrc)
     &                                   +cx*t(i,Jstr,k,nnew,itrc)
     &                                -max(cy,0.)*grad(i  ,Jstr-1)
     &                                -min(cy,0.)*grad(i+1,Jstr-1)
     &                                                 )/(cff+cx)

---> */


            cx=-dtfwd*v(i,Jstr,k,nrhs)*pn(i,Jstr-1)
            cy=0.5*dtfwd*(u(i,Jstr-1,k,nrhs)+u(i+1,Jstr-1,k,nrhs))
     &                                            *pm(i,Jstr-1)

            if (cx.gt.0.) then
              tau=0.
            else
              tau=-cx
              cx=0.
            endif

            t(i,Jstr-1,k,nnew,itrc)=(1.-cx)*( t(i,Jstr-1,k,nstp,itrc)
     &                                   -max(cy,0.)*grad(i  ,Jstr-1)
     &                                   -min(cy,0.)*grad(i+1,Jstr-1)
     &                                                              )
     &                                  +cx*(  t(i,Jstr,k,nstp,itrc)
     &                                     -max(cy,0.)*grad(i  ,Jstr)
     &                                     -min(cy,0.)*grad(i+1,Jstr)
     &                                                              )


#   if defined T_FRC_BRY || defined TNUDGING
            t(i,Jstr-1,k,nnew,itrc)=(1.-tau)*t(i,Jstr-1,k,nnew,itrc)
#    ifdef T_FRC_BRY
     &                                        +tau*t_south(i,k,itrc)
#    else
     &                                    +tau*tclm(i,Jstr-1,k,itrc)
#    endif
#   endif
#   ifdef MASKING
            t(i,Jstr-1,k,nnew,itrc)=t(i,Jstr-1,k,nnew,itrc)
     &                                     *rmask(i,Jstr-1)
#   endif
          enddo
        enddo
#  else
        do k=1,N
          do i=Istr,Iend
#   if defined OBC_SOUTH && defined OBC_TSPECIFIED
#    ifdef T_FRC_BRY
            t(i,Jstr-1,k,nnew,itrc)=t_south(i,k,itrc)
#    else
            t(i,Jstr-1,k,nnew,itrc)=tclm(i,Jstr-1,k,itrc)
#    endif
#   else
            t(i,Jstr-1,k,nnew,itrc)=t(i,Jstr,k,nnew,itrc)
#   endif
#   ifdef MASKING
     &                                   *rmask(i,Jstr-1)
#   endif
          enddo
        enddo
#  endif
      endif    ! <-- SOUTHERN_EDGE


      if (NORTHERN_EDGE) then
#  if defined OBC_NORTH && defined OBC_TORLANSKI
        do k=1,N
          do i=Istr,Iend+1
            grad(i,Jend  )=( t(i  ,Jend  ,k,nstp,itrc)
     &                      -t(i-1,Jend  ,k,nstp,itrc))
#   ifdef MASKING
     &                                   *umask(i,Jend)
#   endif
            grad(i,Jend+1)=( t(i  ,Jend+1,k,nstp,itrc)
     &                      -t(i-1,Jend+1,k,nstp,itrc))
#   ifdef MASKING
     &                                 *umask(i,Jend+1)
#   endif
          enddo
          do i=Istr,Iend
/* --->
            dft=t(i,Jend,k,nstp,itrc)-t(i,Jend  ,k,nnew,itrc)
            dfx=t(i,Jend,k,nnew,itrc)-t(i,Jend-1,k,nnew,itrc)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                          ! <-- cancel, if inflow 
#   if defined T_FRC_BRY || defined TNUDGING
              tau=tau_in
            else
              tau=tau_out
#   endif
            endif
 
            if (dft*(grad(i,Jend)+grad(i+1,Jend)) .gt. 0.) then
              dfy=grad(i,Jend)
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
            t(i,Jend+1,k,nnew,itrc)=( cff*t(i,Jend+1,k,nstp,itrc)
     &                                 +cx*t(i,Jend  ,k,nnew,itrc)
     &                                -max(cy,0.)*grad(i  ,Jend+1)
     &                                -min(cy,0.)*grad(i+1,Jend+1)
     &                                                 )/(cff+cx)

---> */
            cx=dtfwd*v(i,Jend+1,k,nrhs)*pn(i,Jend+1)
            cy=0.5*dtfwd*(u(i,Jend+1,k,nrhs)+u(i+1,Jend+1,k,nrhs))
     &                                            *pm(i,Jend+1)

            if (cx.gt.0.) then
              tau=0.
            else
              tau=-cx
              cx=0.
            endif

            t(i,Jend+1,k,nnew,itrc)=(1.-cx)*( t(i,Jend+1,k,nstp,itrc)
     &                                   -max(cy,0.)*grad(i  ,Jend+1)
     &                                   -min(cy,0.)*grad(i+1,Jend+1)
     &                                                              )
     &                                  +cx*(  t(i,Jend,k,nnew,itrc)
     &                                     -max(cy,0.)*grad(i  ,Jend)
     &                                     -min(cy,0.)*grad(i+1,Jend)
     &                                                              )





#   if defined T_FRC_BRY || defined TNUDGING
            t(i,Jend+1,k,nnew,itrc)=(1.-tau)*t(i,Jend+1,k,nnew,itrc)
#    ifdef T_FRC_BRY
     &                                        +tau*t_north(i,k,itrc)
#    else
     &                                    +tau*tclm(i,Jend+1,k,itrc)
#    endif
#   endif
#   ifdef MASKING
            t(i,Jend+1,k,nnew,itrc)=t(i,Jend+1,k,nnew,itrc)
     &                                     *rmask(i,Jend+1)
#   endif
          enddo
        enddo
#  else
        do k=1,N
          do i=Istr,Iend
#   if defined OBC_NORTH && defined OBC_TSPECIFIED
#    ifdef T_FRC_BRY
            t(i,Jend+1,k,nnew,itrc)=t_north(i,k,itrc)
#    else
            t(i,Jend+1,k,nnew,itrc)=tclm(i,Jend+1,k,itrc)
#    endif
#   else
            t(i,Jend+1,k,nnew,itrc)=t(i,Jend,k,nnew,itrc)
#   endif
#   ifdef MASKING
     &                                   *rmask(i,Jend+1)
#   endif
          enddo
        enddo
#  endif
      endif    ! <-- NORTHERN_EDGE
# endif /* ! NS_PERIODIC */
!
! Corner points between adjacent boundaries. Note that because
! boundary conditions for tracers are of gradient nature -- either
! Neumann, or radiation, the corner points have to be set in all
! cases, even if the adjacent boundaries are closed. This differs
! from setting boundaries for velocities, where there is anisotropy
! associated with normal and tangential components. In the case when 
! one/or both points near the corner is/are masked, the use of masked
! points is avoided. 

# ifndef EW_PERIODIC
#  ifndef NS_PERIODIC
      if (SOUTHERN_EDGE .and. WESTERN_EDGE) then
#   ifdef MASKING
        cff=rmask(Istr,Jstr-1)+rmask(Istr-1,Jstr)
        if (cff.gt.0.) then
          cff=1./cff
          do k=1,N
            t(Istr-1,Jstr-1,k,nnew,itrc)=cff*(
     &              rmask(Istr,Jstr-1)*t(Istr,Jstr-1,k,nnew,itrc)
     &             +rmask(Istr-1,Jstr)*t(Istr-1,Jstr,k,nnew,itrc))
          enddo
        else
          do k=1,N
            t(Istr-1,Jstr-1,k,nnew,itrc)=0.
          enddo
        endif
#   else
        do k=1,N
          t(Istr-1,Jstr-1,k,nnew,itrc)=0.5*( t(Istr,Jstr-1,k,nnew,
     &                           itrc)+t(Istr-1,Jstr,k,nnew,itrc))
        enddo
#   endif
      endif

      if (SOUTHERN_EDGE .and. EASTERN_EDGE) then
#   ifdef MASKING
        cff=rmask(Iend,Jstr-1)+rmask(Iend+1,Jstr)
        if (cff.gt.0.) then
          cff=1./cff
          do k=1,N
            t(Iend+1,Jstr-1,k,nnew,itrc)=cff*(
     &              rmask(Iend,Jstr-1)*t(Iend,Jstr-1,k,nnew,itrc)
     &             +rmask(Iend+1,Jstr)*t(Iend+1,Jstr,k,nnew,itrc))
          enddo
        else
          do k=1,N
            t(Iend+1,Jstr-1,k,nnew,itrc)=0.
          enddo
        endif
#   else
        do k=1,N
          t(Iend+1,Jstr-1,k,nnew,itrc)=0.5*(t(Iend,Jstr-1,k,nnew,
     &                           itrc)+t(Iend+1,Jstr,k,nnew,itrc))
        enddo
#   endif
      endif

      if (NORTHERN_EDGE .and. WESTERN_EDGE) then
#   ifdef MASKING
        cff=rmask(Istr,Jend+1)+rmask(Istr-1,Jend)
        if (cff.gt.0.) then
          cff=1./cff
          do k=1,N
            t(Istr-1,Jend+1,k,nnew,itrc)=cff*(
     &              rmask(Istr,Jend+1)*t(Istr,Jend+1,k,nnew,itrc)
     &             +rmask(Istr-1,Jend)*t(Istr-1,Jend,k,nnew,itrc))
          enddo
        else
          do k=1,N
            t(Istr-1,Jend+1,k,nnew,itrc)=0.
          enddo
        endif
#   else
        do k=1,N
          t(Istr-1,Jend+1,k,nnew,itrc)=0.5*( t(Istr,Jend+1,k,nnew,
     &                           itrc)+t(Istr-1,Jend,k,nnew,itrc))
        enddo
#   endif
      endif

      if (NORTHERN_EDGE .and. EASTERN_EDGE) then
#   ifdef MASKING
        cff=rmask(Iend,Jend+1)+rmask(Iend+1,Jend)
        if (cff.gt.0.) then
          cff=1./cff
          do k=1,N
            t(Iend+1,Jend+1,k,nnew,itrc)=cff*(
     &              rmask(Iend,Jend+1)*t(Iend,Jend+1,k,nnew,itrc)
     &             +rmask(Iend+1,Jend)*t(Iend+1,Jend,k,nnew,itrc))
          enddo
        else
          do k=1,N
            t(Iend+1,Jend+1,k,nnew,itrc)=0.
          enddo
        endif
#   else
        do k=1,N
          t(Iend+1,Jend+1,k,nnew,itrc)=0.5*( t(Iend,Jend+1,k,nnew,
     &                           itrc)+t(Iend+1,Jend,k,nnew,itrc))
        enddo
#   endif
      endif
#  endif
# endif
      return
      end
#else
      subroutine t3dbc_empty
      end
#endif /* SOLVE3D */
