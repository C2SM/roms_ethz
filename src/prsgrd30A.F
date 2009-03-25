#include "cppdefs.h"
#ifdef SOLVE3D
 
      subroutine prsgrd (tile)
      implicit none
      integer tile
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      call prsgrd30A_tile (Istr,Iend,Jstr,Jend, A3d(1,1), A3d(1,2),
     &                                                    A2d(1,1))
      return
      end
 
      subroutine prsgrd30A_tile (Istr,Iend,Jstr,Jend, ru,rv, P)
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k
# include "param.h"
      real ru(PRIVATE_2D_SCRATCH_ARRAY,N),
     &     rv(PRIVATE_2D_SCRATCH_ARRAY,N),  GRho,
     &      P(PRIVATE_2D_SCRATCH_ARRAY),  HalfGRho
# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
!
# include "compute_auxiliary_bounds.h"
!
! This is just a alternative, primitive sigma-coordinate form of
! POM Jacobian. This code produces result identical to prsgrd31.F
! with parameter WJ_GRADP either undefined or set to zero.
!
      GRho=g/rho0
      HalfGRho=0.5*GRho
      do k=N,1,-1           !--> irreversible
        if (k.eq.N) then
          do j=JstrV-1,Jend
            do i=IstrU-1,Iend
              P(i,j)=g*z_w(i,j,N) + GRho*(z_w(i,j,N)-z_r(i,j,N))
     &                 *( rho(i,j,N)+0.5*(rho(i,j,N)-rho(i,j,N-1))
     &          *(z_w(i,j,N)-z_r(i,j,N))/(z_r(i,j,N)-z_r(i,j,N-1))
     &                                                           )
            enddo
          enddo
        else
          do j=JstrV-1,Jend
            do i=IstrU-1,Iend
              P(i,j)=P(i,j)+HalfGRho*(rho(i,j,k+1)+rho(i,j,k))
     &                              *(z_r(i,j,k+1)-z_r(i,j,k))
            enddo
          enddo
        endif
 
        do j=Jstr,Jend
          do i=IstrU,Iend
            ru(i,j,k)=0.5*(Hz(i,j,k)+Hz(i-1,j,k))*dn_u(i,j)*(
     &          P(i-1,j)-P(i,j)-HalfGRho*(rho(i,j,k)+rho(i-1,j,k))
     &                                  *(z_r(i,j,k)-z_r(i-1,j,k))
     &                                                           )
          enddo
        enddo
 
        do j=JstrV,Jend
          do i=Istr,Iend
            rv(i,j,k)=0.5*(Hz(i,j,k)+Hz(i,j-1,k))*dm_v(i,j)*(
     &          P(i,j-1)-P(i,j)-HalfGRho*(rho(i,j,k)+rho(i,j-1,k))
     &                                  *(z_r(i,j,k)-z_r(i,j-1,k))
     &                                                           )
          enddo
        enddo
      enddo
      return
      end
#else
      subroutine prsgrd_empty
      end
#endif /* SOLVE3D */
 
