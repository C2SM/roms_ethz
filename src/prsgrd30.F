#include "cppdefs.h"
#ifdef SOLVE3D
 
      subroutine prsgrd (tile)
      implicit none
      integer tile
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      call prsgrd30A_tile (Istr,Iend,Jstr,Jend, A3d(1,1), A3d(1,2),
     &                                          A3d(1,3), A2d(1,1))
      return
      end
 
      subroutine prsgrd30A_tile (Istr,Iend,Jstr,Jend, ru,rv, d,RX)
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k
# include "param.h"
      real ru(PRIVATE_2D_SCRATCH_ARRAY,N),
     &     rv(PRIVATE_2D_SCRATCH_ARRAY,N),
     &      d(PRIVATE_2D_SCRATCH_ARRAY,0:N), dz,
     &     RX(PRIVATE_1D_SCRATCH_ARRAY,N),  cff
# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
!
# include "compute_auxiliary_bounds.h"
!
      do j=JstrV-1,Jend
        do k=1,N-1
          do i=IstrU-1,Iend
            d(i,j,k)=(rho(i,j,k+1)-rho(i,j,k))
     &              /(z_r(i,j,k+1)-z_r(i,j,k))
          enddo
        enddo
        do i=IstrU-1,Iend
          d(i,j,N)=d(i,j,N-1)
          d(i,j,0)=d(i,j,  1)
        enddo
 
        if (j.ge.Jstr) then
          do k=1,N
            do i=IstrU,Iend
              dz=0.5*(z_r(i,j,k)-z_r(i-1,j,k))
              RX(i,k)=rho(i-1,j,k) - rho(i,j,k)
     &             +max(dz,0.)*(d(i-1,j,k)+d(i,j,k-1))
     &             +min(dz,0.)*(d(i-1,j,k-1)+d(i,j,k))
            enddo
          enddo
          do i=IstrU,Iend
            ru(i,j,N)=( rho0+0.5*( rho(i-1,j,N)+rho(i,j,N)
     &                   +d(i-1,j,N)*(z_w(i-1,j,N)-z_r(i-1,j,N))
     &                   +d(i  ,j,N)*(z_w(i  ,j,N)-z_r(i  ,j,N))
     &                            ))*(z_w(i-1,j,N)-z_w(i  ,j,N))
 
     &                 +0.5*RX(i,N)*( z_w(i  ,j,N)+z_w(i-1,j,N)
     &                               -z_r(i  ,j,N)-z_r(i-1,j,N))
          enddo
          do k=N-1,1,-1
            do i=IstrU,Iend
              ru(i,j,k)=ru(i,j,k+1) + 0.25*(RX(i,k)+RX(i,k+1))
     &                           *( z_r(i-1,j,k+1)+z_r(i,j,k+1)
     &                               -z_r(i-1,j,k) -z_r(i,j,k))
            enddo
          enddo
          cff=0.5*g/rho0
          do k=1,N
            do i=IstrU,Iend
             ru(i,j,k)=cff*ru(i,j,k)*(Hz(i,j,k)+Hz(i-1,j,k))
     &                                             *dn_u(i,j)
            enddo
          enddo
        endif
 
        if (j.ge.JstrV) then
          do k=1,N
            do i=Istr,Iend
              dz=0.5*(z_r(i,j,k)-z_r(i,j-1,k))
              RX(i,k)=rho(i,j-1,k) - rho(i,j,k)
     &             +max(dz,0.)*(d(i,j-1,k)+d(i,j,k-1))
     &             +min(dz,0.)*(d(i,j-1,k-1)+d(i,j,k))
            enddo
          enddo
          do i=Istr,Iend
            rv(i,j,N)=( rho0+0.5*( rho(i,j-1,N)+rho(i,j,N)
     &                   +d(i,j-1,N)*(z_w(i,j-1,N)-z_r(i,j-1,N))
     &                   +d(i  ,j,N)*(z_w(i,j  ,N)-z_r(i,j  ,N))
     &                            ))*(z_w(i,j-1,N)-z_w(i,j  ,N))
 
     &                 +0.5*RX(i,N)*( z_w(i  ,j,N)+z_w(i,j-1,N)
     &                               -z_r(i  ,j,N)-z_r(i,j-1,N))
          enddo
          do k=N-1,1,-1
            do i=Istr,Iend
              rv(i,j,k)=rv(i,j,k+1) + 0.25*(RX(i,k)+RX(i,k+1))
     &                           *( z_r(i,j-1,k+1)+z_r(i,j,k+1)
     &                               -z_r(i,j-1,k) -z_r(i,j,k))
            enddo
          enddo
          cff=0.5*g/rho0
          do k=1,N
            do i=Istr,Iend
             rv(i,j,k)=cff*rv(i,j,k)*(Hz(i,j,k)+Hz(i,j-1,k))
     &                                             *dm_v(i,j)
            enddo
          enddo
        endif
      enddo  !<-- j
      return
      end
#else
      subroutine prsgrd_empty
      end
#endif /* SOLVE3D */
 
