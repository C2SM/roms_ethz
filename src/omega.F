#include "cppdefs.h"
#ifdef SOLVE3D
 
      subroutine omega (tile)
      implicit none
      integer tile
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      call omega_tile (Istr,Iend,Jstr,Jend, A2d(1,1))
      return
      end
 
      subroutine omega_tile (Istr,Iend,Jstr,Jend, wrk)
!
! Compute S-coordinate vertical velocity, w=[Hz/(m*n)]*omega [m^3/s],
! which has meaning of FINITE_VOLUME WATER FLUX across MOVING grid-
! box interfaces of RHO-boxes. To do so, we integrate divergence of
! horizontal mass fluxes from bottom up, starting with the no-normal
! flow boundary condition at the bottom (k=0); After this operation
! W(:,:,N) contains vertical velocity flux at the free surface, which
! is the time-tendency of the free surface, d_zeta/d_t multiplied by
! grid-box area as seen from above;  To convert W(:,:,:) into
! S-coordinate vertical velocity flux, one needs to subtract the
! vertical velocities of moving S-coordinate surfaces, which are
! proportional the product of d_zeta/d_t and the fraction of the
! distance from the point to the bottom divided by the total
! depth of water column, i.e. the whole S-coordinate system is
! "breathes" by linear in Z-space expansion and contraction set by
! variation in free surface.
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k
# include "param.h"
      real wrk(PRIVATE_1D_SCRATCH_ARRAY)
# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
!
# include "compute_auxiliary_bounds.h"
!
      do j=Jstr,Jend
        do i=Istr,Iend
          W(i,j,0)=0.
        enddo
        do k=1,N,+1        !--> recursive
          do i=Istr,Iend
            W(i,j,k)=W(i,j,k-1) -FlxU(i+1,j,k)+FlxU(i,j,k)
     &                          -FlxV(i,j+1,k)+FlxV(i,j,k)
          enddo
        enddo
        do i=Istr,Iend
          wrk(i)=W(i,j,N)/(z_w(i,j,N)-z_w(i,j,0))
          W(i,j,N)=0.
        enddo
        do k=N-1,1,-1
          do i=Istr,Iend
            W(i,j,k)=W(i,j,k)-wrk(i)*(z_w(i,j,k)-z_w(i,j,0))
          enddo
        enddo
      enddo

# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then                       ! Set lateral
        do k=0,N                                   ! boundary 
          do j=Jstr,Jend                           ! conditions
            W(Istr-1,j,k)=W(Istr,j,k)
          enddo
        enddo
      endif
      if (EASTERN_EDGE) then
        do k=0,N
          do j=Jstr,Jend
            W(Iend+1,j,k)=W(Iend,j,k)
          enddo
        enddo
      endif
# endif
# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        do k=0,N
          do i=Istr,Iend
            W(i,Jstr-1,k)=W(i,Jstr,k)
          enddo
        enddo
      endif
      if (NORTHERN_EDGE) then
        do k=0,N
          do i=Istr,Iend
            W(i,Jend+1,k)=W(i,Jend,k)
          enddo
        enddo
      endif
#  ifndef EW_PERIODIC
      if (WESTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=0,N
          W(Istr-1,Jstr-1,k)=W(Istr,Jstr,k)
        enddo
      endif
      if (WESTERN_EDGE .and. NORTHERN_EDGE) then
        do k=0,N
          W(Istr-1,Jend+1,k)=W(Istr,Jend,k)
        enddo
      endif
      if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=0,N
          W(Iend+1, Jstr-1,k)=W(Iend,Jstr,k)
        enddo
      endif
      if (EASTERN_EDGE .and. NORTHERN_EDGE) then
        do k=0,N
          W(Iend+1,Jend+1,k)=W(Iend,Jend,k)
        enddo
      endif
#  endif
# endif
# if defined EW_PERIODIC || defined NS_PERIODIC  || defined MPI
      call exchange_w3d_tile (Istr,Iend,Jstr,Jend, W)
# endif
      return
      end
#else
      subroutine omega_empty
      end
#endif /* SOLVE3D */
 
