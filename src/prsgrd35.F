#include "cppdefs.h"
#ifdef SOLVE3D
 
      subroutine prsgrd (tile)
      implicit none
      integer tile
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      call prsgrd_tile (Istr,Iend,Jstr,Jend,  A3d(1,1), A3d(1,2),
     &                                        A3d(1,3), A3d(1,4),
     &                    A2d(1,1), A2d(1,2), A2d(1,3), A2d(1,4),
     &                    A2d(1,5), A2d(1,6), A2d(1,7), A2d(1,8))
      return
      end
 
      subroutine prsgrd_tile (Istr,Iend,Jstr,Jend, ru,rv,
     &                                 gradZ,gradR, grdZ,avgZ,
     &                         grdR,avgR, grdR1,avgR1, dHz,CF)
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k
# include "param.h"
      real ru(PRIVATE_2D_SCRATCH_ARRAY,N),
     &     rv(PRIVATE_2D_SCRATCH_ARRAY,N),
     &     gradZ(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &     gradR(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &      grdZ(PRIVATE_1D_SCRATCH_ARRAY,0:N), cff,
     &      avgZ(PRIVATE_1D_SCRATCH_ARRAY,0:N), dCF,
     &      grdR(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &      avgR(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     grdR1(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     avgR1(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &       dHz(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &        CF(PRIVATE_1D_SCRATCH_ARRAY,0:N)
 
 
# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
!
# include "compute_auxiliary_bounds.h"
!                                               Compute XI-component
!                                               ======= == =========
      do j=Jstr,Jend
        do k=0,N
          do i=IstrU,Iend
            gradZ(i,j,k)=(z_w(i,j,k)-z_w(i-1,j,k))
# ifdef MASKING
     &                                 *umask(i,j)
# endif
          enddo
        enddo
        do k=1,N
          do i=IstrU-1,Iend+1
            gradR(i,j,k)=(rho(i,j,k)-rho(i-1,j,k))
# ifdef MASKING
     &                                 *umask(i,j)
# endif
          enddo
        enddo
 
        do k=0,N
          do i=IstrU,Iend
c??            grdZ(i,k)=1.0833333333333333*gradZ(i,j,k)
c??     &               -0.0416666666666666*( gradZ(i-1,j,k)
c??     &                                    +gradZ(i+1,j,k))
            grdZ(i,k)=gradZ(i,j,k)
            avgZ(i,k)=0.5*(z_w(i,j,k)+z_w(i-1,j,k))
c?     &             +0.0625*(gradZ(i-1,j,k)-gradZ(i+1,j,k))
          enddo
        enddo
        do k=1,N
          do i=IstrU,Iend
c??            grdR(i,k)=1.0833333333333333*gradR(i,j,k)
c??     &               -0.0416666666666666*( gradR(i-1,j,k)
c??     &                                    +gradR(i+1,j,k))
 
            grdR(i,k)=gradR(i,j,k)
            avgR(i,k)=0.5*(rho(i,j,k)+rho(i-1,j,k))
c?     &             +0.0625*(gradR(i-1,j,k)-gradR(i+1,j,k))
 
            dHz(i,k)=avgZ(i,k)-avgZ(i,k-1)
          enddo
        enddo
!-
        do i=IstrU,Iend
          CF(i,0)=-1.
          grdR1(i,0)=0.
          avgR1(i,0)=0.
        enddo
        do k=1,N-1,+1
          do i=IstrU,Iend
            cff=1./(2.*dHz(i,k+1)+dHz(i,k)*(2.-CF(i,k-1)))
 
            CF(i,k)=cff*dHz(i,k+1)
 
            grdR1(i,k)=cff*( 6.*(grdR(i,k+1)-grdR(i,k))
     &                              -dHz(i,k)*grdR1(i,k-1))
 
            avgR1(i,k)=cff*( 6.*(avgR(i,k+1)-avgR(i,k))
     &                              -dHz(i,k)*avgR1(i,k-1))
          enddo
        enddo
        do i=IstrU,Iend
          cff=1./(1.+CF(i,N-1))
          grdR1(i,N)=cff*grdR1(i,N-1)
          grdR1(i,N-1)=    grdR1(i,N)
 
          avgR1(i,N)=cff*avgR1(i,N-1)
          avgR1(i,N-1)=    avgR1(i,N)
        enddo
        do k=N-2,0,-1
          do i=IstrU,Iend
            grdR1(i,k)=grdR1(i,k)-CF(i,k)*grdR1(i,k+1)
            avgR1(i,k)=avgR1(i,k)-CF(i,k)*avgR1(i,k+1)
          enddo
        enddo
!-
        do i=IstrU,Iend
          CF(i,N)=(rho0+avgR(i,N)+0.5*avgR1(i,N)*dHz(i,N))
     &                           *(z_w(i,j,N)-z_w(i-1,j,N))
        enddo
 
        cff=0.5*g/rho0
        do k=N,1,-1
          do i=IstrU,Iend
            dCF=dHz(i,k)*( grdR(i,k)-0.25*(avgR1(i,k)+avgR1(i,k-1))
     &                                      *(grdZ(i,k)+grdZ(i,k-1))
 
     &                 -0.0833333333333333*(avgR1(i,k)-avgR1(i,k-1))
     &                                      *(grdZ(i,k)-grdZ(i,k-1))
     &                                                            )
 
            ru(i,j,k)=-cff*(Hz(i,j,k)+Hz(i-1,j,k))*dn_u(i,j)
     &            *( CF(i,k)+0.5*dCF
     &                 +0.0416666666666666*dHz(i,k)*(
     &                            dHz(i,k)*(grdR1(i,k)+grdR1(i,k-1))
 
     &                                    -(avgR1(i,k)+avgR1(i,k-1))
     &                                      *(grdZ(i,k)-grdZ(i,k-1))
 
     &                                    -(avgR1(i,k)-avgR1(i,k-1))
     &                                      *(grdZ(i,k)+grdZ(i,k-1))
     &                                                           ))
            CF(i,k-1)=CF(i,k)+dCF
          enddo
        enddo
      enddo
!                                              Compute ETA-component
!                                              ======= === =========
      do j=JstrV-1,Jend+1
        do k=0,N
          do i=Istr,Iend
            gradZ(i,j,k)=(z_w(i,j,k)-z_w(i,j-1,k))
# ifdef MASKING
     &                                 *vmask(i,j)
# endif
          enddo
        enddo
        do k=1,N
          do i=Istr,Iend
            gradR(i,j,k)=(rho(i,j,k)-rho(i,j-1,k))
# ifdef MASKING
     &                                 *vmask(i,j)
# endif
          enddo
        enddo
      enddo
 
      do j=JstrV,Jend
        do k=0,N
          do i=Istr,Iend
c??            grdZ(i,k)=1.0833333333333333*gradZ(i,j,k)
c??     &               -0.0416666666666666*( gradZ(i,j-1,k)
c??     &                                    +gradZ(i,j+1,k))
 
            grdZ(i,k)=gradZ(i,j,k)
            avgZ(i,k)=0.5*(z_w(i,j,k)+z_w(i,j-1,k))
c?     &             +0.0625*(gradZ(i,j-1,k)-gradZ(i,j+1,k))
          enddo
        enddo
        do k=1,N
          do i=Istr,Iend
c??            grdR(i,k)=1.0833333333333333*gradR(i,j,k)
c??     &               -0.0416666666666666*( gradR(i,j+1,k)
c??     &                                    +gradR(i,j-1,k))
 
            grdR(i,k)=gradR(i,j,k)
            avgR(i,k)=0.5*(rho(i,j,k)+rho(i,j-1,k))
c?     &             +0.0625*(gradR(i,j-1,k)-gradR(i,j+1,k))
 
            dHz(i,k)=avgZ(i,k)-avgZ(i,k-1)
          enddo
        enddo
 
        do i=Istr,Iend
          CF(i,0)=-1.
          grdR1(i,0)=0.
          avgR1(i,0)=0.
        enddo
        do k=1,N-1,+1
          do i=Istr,Iend
            cff=1./(2.*dHz(i,k+1)+dHz(i,k)*(2.-CF(i,k-1)))
 
            CF(i,k)=cff*dHz(i,k+1)
 
            grdR1(i,k)=cff*( 6.*(grdR(i,k+1)-grdR(i,k))
     &                              -dHz(i,k)*grdR1(i,k-1))
 
            avgR1(i,k)=cff*( 6.*(avgR(i,k+1)-avgR(i,k))
     &                              -dHz(i,k)*avgR1(i,k-1))
          enddo
        enddo
        do i=Istr,Iend
          cff=1./(1.+CF(i,N-1))
          grdR1(i,N)=cff*grdR1(i,N-1)
          grdR1(i,N-1)=    grdR1(i,N)
 
          avgR1(i,N)=cff*avgR1(i,N-1)
          avgR1(i,N-1)=    avgR1(i,N)
        enddo
        do k=N-2,0,-1
          do i=Istr,Iend
            grdR1(i,k)=grdR1(i,k)-CF(i,k)*grdR1(i,k+1)
            avgR1(i,k)=avgR1(i,k)-CF(i,k)*avgR1(i,k+1)
          enddo
        enddo
 
        do i=Istr,Iend
          CF(i,N)=(rho0+avgR(i,N)+0.5*avgR1(i,N)*dHz(i,N))
     &                           *(z_w(i,j,N)-z_w(i,j-1,N))
        enddo
        cff=0.5*g/rho0
        do k=N,1,-1
          do i=Istr,Iend
            dCF=dHz(i,k)*( grdR(i,k)-0.25*(avgR1(i,k)+avgR1(i,k-1))
     &                                      *(grdZ(i,k)+grdZ(i,k-1))
 
     &                 -0.0833333333333333*(avgR1(i,k)-avgR1(i,k-1))
     &                                      *(grdZ(i,k)-grdZ(i,k-1))
     &                                                            )
 
            rv(i,j,k)=-cff*(Hz(i,j,k)+Hz(i,j-1,k))*dm_v(i,j)
     &            *( CF(i,k)+0.5*dCF
     &                 +0.0416666666666666*dHz(i,k)*(
     &                            dHz(i,k)*(grdR1(i,k)+grdR1(i,k-1))
 
     &                                    -(avgR1(i,k)+avgR1(i,k-1))
     &                                      *(grdZ(i,k)-grdZ(i,k-1))
 
     &                                    -(avgR1(i,k)-avgR1(i,k-1))
     &                                      *(grdZ(i,k)+grdZ(i,k-1))
     &                                                           ))
            CF(i,k-1)=CF(i,k)+dCF
          enddo
        enddo
      enddo
#else
      subroutine prsgrd_empty
#endif /* SOLVE3D */
      return
      end
 
