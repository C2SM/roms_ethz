#include "cppdefs.h"
#ifdef SOLVE3D

      subroutine omega (tile)
      implicit none
      integer tile
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      call omega_tile (istr,iend,jstr,jend, A2d(1,1),A2d(1,2))
      end

      subroutine omega_tile (istr,iend,jstr,jend, CX,wrk)

! Compute S-coordinate vertical velocity, w=[Hz/(m*n)]*omega [m^3/s],
! which has the meaning of FINITE_VOLUME FLUX across MOVING grid-box
! interfaces of RHO-boxes. To compute it we first integrate divergence
! of horizontal mass fluxes from bottom up, starting with the no-flow
! boundary condition at the bottom (k=0); After this operation W(:,:,N)
! contains vertical velocity flux at the free surface, which is the
! time-tendency of the free surface, d_zeta/d_t multiplied by grid-box
! area as seen from above;  To convert W(:,:,:) into S-coordinate
! vertical velocity flux, one needs to subtract the vertical velocities
! of moving S-coordinate surfaces, which are proportional the product
! of d_zeta/d_t and the fraction of the distance from the point to the
! bottom divided by the total depth of water column, i.e. the whole
! S-coordinate system is "breathes" by linear in Z-space expansion
! and contraction set by variation in free surface.

! Parameter setting: "cu_min" is threshold value for Courant Number
! below which vertical advection is fully explicit; "cu_max" is the
! maximum CN which the explicit component "We" is allowed to reach.

      implicit none
      integer istr,iend,jstr,jend, i,j,k
# include "param.h"
      real CX(PRIVATE_1D_SCRATCH_ARRAY,0:N),  dtau,cdt,
     &    wrk(PRIVATE_1D_SCRATCH_ARRAY),      cff, dh,dh2
      real, parameter :: cu_min=0.2D0, cu_max=0.5D0,
     &      cu_inv=1.D0/cu_max,  cutoff=2.D0*cu_max-cu_min,
     &                  delta=0.25D0*cu_inv/(cu_max-cu_min)

# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"

# include "compute_auxiliary_bounds.h"

      if (CORR_STAGE) then
        dtau=dt
      elseif (FIRST_TIME_STEP) then
        dtau=0.5*dt
      else
        dtau=(1.-1./6.)*dt
      endif

      do j=jstr,jend
        do i=istr,iend
          Wi(i,j,0)=0.
        enddo
        do k=1,N,+1        !--> recursive
          do i=istr,iend
            Wi(i,j,k)=Wi(i,j,k-1) -FlxU(i+1,j,k) +FlxU(i,j,k)
     &                            -FlxV(i,j+1,k) +FlxV(i,j,k)

            CX(i,k)=max(FlxU(i+1,j,k),0.)-min(FlxU(i,j,k),0.)
     &             +max(FlxV(i,j+1,k),0.)-min(FlxV(i,j,k),0.)
          enddo
        enddo
        do i=istr,iend
          wrk(i)=Wi(i,j,N)/(z_w(i,j,N)-z_w(i,j,0))
          Wi(i,j,N)=0.
          We(i,j,N)=0.
          We(i,j,0)=0.
          CX(i,0)=dtau*pm(i,j)*pn(i,j)
        enddo
        do k=N-1,1,-1
          do i=istr,iend
            Wi(i,j,k)=Wi(i,j,k)-wrk(i)*(z_w(i,j,k)-z_w(i,j,0))

            CX(i,k+1)=CX(i,k+1)-min(Wi(i,j,k),0.)
            CX(i,k  )=CX(i,k  )+max(Wi(i,j,k),0.)
          enddo
        enddo
        do k=1,N-1
          do i=istr,iend
c*          if (Wi(i,j,k)>0) then        ! Three different variants
c*            cdt=CX(i,k)                ! of computing Courant number
c*            dh=Hz(i,j,k)               ! at the interface: use value
c*          else                         ! from the grid box vertically
c*            cdt=CX(i,k+1)              ! upstream to the interface;
c*            dh=Hz(i,j,k+1)             ! average; or select maximum
c*          endif                        ! value between the two.

c>          cdt=CX(i,k) +CX(i,k+1)
c>          dh=Hz(i,j,k)+Hz(i,j,k+1)

            cdt=max(CX(i,k),  CX(i,k+1))
            dh=min(Hz(i,j,k),Hz(i,j,k+1))

            cdt=cdt*CX(i,0)     !<-- after this cdt/dh = Courant number
            dh2=dh*dh
            if (cdt<cu_min*dh) then
              cff=dh2
            elseif (cdt<cutoff*dh) then
              cff=dh2+delta*(cdt-cu_min*dh)**2
            else
              cff=cu_inv*cdt*dh
            endif

            We(i,j,k)=dh2*Wi(i,j,k)/cff    !<-- NORMAL SPLITING
            Wi(i,j,k)=Wi(i,j,k)-We(i,j,k)  !<-- CODE TO BE USED

c**          We(i,j,k)=0.          !<-- fully implicit (BE), test only

c**         We(i,j,k)=Wi(i,j,k)    !<-- for testing only: revert
c**         Wi(i,j,k)=0.           !<-- back to fully explicit code
          enddo
        enddo
      enddo

# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then                       ! Set lateral
        do k=0,N                                   ! boundary
          do j=jstr,jend                           ! conditions
            We(istr-1,j,k)=We(istr,j,k)
            Wi(istr-1,j,k)=Wi(istr,j,k)
          enddo
        enddo
      endif
      if (EASTERN_EDGE) then
        do k=0,N
          do j=jstr,jend
            We(iend+1,j,k)=We(iend,j,k)
            Wi(iend+1,j,k)=Wi(iend,j,k)
          enddo
        enddo
      endif
# endif
# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        do k=0,N
          do i=istr,iend
            We(i,jstr-1,k)=We(i,jstr,k)
            Wi(i,jstr-1,k)=Wi(i,jstr,k)
          enddo
        enddo
      endif
      if (NORTHERN_EDGE) then
        do k=0,N
          do i=istr,iend
            We(i,jend+1,k)=We(i,jend,k)
            Wi(i,jend+1,k)=Wi(i,jend,k)
          enddo
        enddo
      endif
#  ifndef EW_PERIODIC
      if (WESTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=0,N
          We(istr-1,jstr-1,k)=We(istr,jstr,k)
          Wi(istr-1,jstr-1,k)=Wi(istr,jstr,k)
        enddo
      endif
      if (WESTERN_EDGE .and. NORTHERN_EDGE) then
        do k=0,N
          We(istr-1,jend+1,k)=We(istr,jend,k)
          Wi(istr-1,jend+1,k)=Wi(istr,jend,k)
        enddo
      endif
      if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=0,N
          We(iend+1, jstr-1,k)=We(iend,jstr,k)
          Wi(iend+1, jstr-1,k)=Wi(iend,jstr,k)
        enddo
      endif
      if (EASTERN_EDGE .and. NORTHERN_EDGE) then
        do k=0,N
          We(iend+1,jend+1,k)=We(iend,jend,k)
          Wi(iend+1,jend+1,k)=Wi(iend,jend,k)
        enddo
      endif
#  endif
# endif
# ifdef EXCHANGE
      call exchange_2_tile (istr,iend,jstr,jend, We,N+1, Wi,N+1)
# endif
      end
#else
      subroutine omega_empty
      end
#endif /* SOLVE3D */

