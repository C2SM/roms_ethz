#include "cppdefs.h"
#ifdef SOLVE3D
 
      subroutine prsgrd (tile)
      implicit none
      integer tile
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      call prsgrd_tile (Istr,Iend,Jstr,Jend, A3d(1,1), A3d(1,2),
     &                                                 A2d(1,1))
      return
      end
 
      subroutine prsgrd_tile (Istr,Iend,Jstr,Jend, ru,rv, pgrd)
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k
# include "param.h"
      real ru(PRIVATE_2D_SCRATCH_ARRAY,N),  cff,  A, J1,J2,J3,
     &     rv(PRIVATE_2D_SCRATCH_ARRAY,N),        B, W1,W2,W3,
     &     pgrd(PRIVATE_1D_SCRATCH_ARRAY),  epsil
      parameter (epsil=1.E-8)
# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
!
# include "compute_auxiliary_bounds.h"
!
      do j=Jstr,Jend
        cff=0.5*g/rho0
        do i=IstrU,Iend
          pgrd(i)=cff*(rho(i,j,N)-rho(i-1,j,N))
     &                             *( z_w(i,j,N)+z_w(i-1,j,N)
     &                               -z_r(i,j,N)-z_r(i-1,j,N))
#ifdef RHO_SURF
     &                 +(g+cff*(rho(i,j,N)+rho(i-1,j,N)))
     &                              *(z_w(i,j,N)-z_w(i-1,j,N))
#endif
          ru(i,j,N)=-0.5*(Hz(i,j,N)+Hz(i-1,j,N))*dn_u(i,j)*pgrd(i)
        enddo
 
        cff=0.25*g/rho0
        k=N-1
        do i=IstrU,Iend
          A=(rho(i,j,k+1)+rho(i,j,k)-rho(i-1,j,k+1)-rho(i-1,j,k))
     &     *(z_r(i,j,k+1)-z_r(i,j,k)+z_r(i-1,j,k+1)-z_r(i-1,j,k))
 
          B=(rho(i,j,k+1)-rho(i,j,k)+rho(i-1,j,k+1)-rho(i-1,j,k))
     &     *(z_r(i,j,k+1)+z_r(i,j,k)-z_r(i-1,j,k+1)-z_r(i-1,j,k))
 
          pgrd(i)=pgrd(i)+cff*(A-B)
 
          ru(i,j,k)=-0.5*(Hz(i,j,k)+Hz(i-1,j,k))*dn_u(i,j)*pgrd(i)
        enddo
 
        do k=N-2,2,-1
         do i=IstrU,Iend
           A=(rho(i,j,k+1)+rho(i,j,k)-rho(i-1,j,k+1)-rho(i-1,j,k))
     &      *(z_r(i,j,k+1)-z_r(i,j,k)+z_r(i-1,j,k+1)-z_r(i-1,j,k))
 
           B=(rho(i,j,k+1)-rho(i,j,k)+rho(i-1,j,k+1)-rho(i-1,j,k))
     &      *(z_r(i,j,k+1)+z_r(i,j,k)-z_r(i-1,j,k+1)-z_r(i-1,j,k))
 
           J1=A-B
           W1=max(A*B,epsil)
 
 
           A=(rho(i,j,k+2)+rho(i,j,k+1)-rho(i-1,j,k)-rho(i-1,j,k-1))
     &      *(z_r(i,j,k+2)-z_r(i,j,k+1)+z_r(i-1,j,k)-z_r(i-1,j,k-1))
 
           B=(rho(i,j,k+2)-rho(i,j,k+1)+rho(i-1,j,k)-rho(i-1,j,k-1))
     &      *(z_r(i,j,k+2)+z_r(i,j,k+1)-z_r(i-1,j,k)-z_r(i-1,j,k-1))
 
           J2=A-B
           W2=max(A*B,epsil)
 
 
           A=(rho(i,j,k)+rho(i,j,k-1)-rho(i-1,j,k+2)-rho(i-1,j,k+1))
     &      *(z_r(i,j,k)-z_r(i,j,k-1)+z_r(i-1,j,k+2)-z_r(i-1,j,k+1))
 
           B=(rho(i,j,k)-rho(i,j,k-1)+rho(i-1,j,k+2)-rho(i-1,j,k+1))
     &      *(z_r(i,j,k)+z_r(i,j,k-1)-z_r(i-1,j,k+2)-z_r(i-1,j,k+1))
 
           J3=A-B
           W3=max(A*B,epsil)
 
           pgrd(i)=pgrd(i)+cff*(J1*W2*W3 + W1*J2*W3 + W1*W2*J3)
     &                                 /(W2*W3 + W1*W3 + W1*W2)
 
           ru(i,j,k)=-0.5*(Hz(i,j,k)+Hz(i-1,j,k))*dn_u(i,j)*pgrd(i)
         enddo
        enddo
 
        k=1
        do i=IstrU,Iend
          A=(rho(i,j,k+1)+rho(i,j,k)-rho(i-1,j,k+1)-rho(i-1,j,k))
     &     *(z_r(i,j,k+1)-z_r(i,j,k)+z_r(i-1,j,k+1)-z_r(i-1,j,k))
 
          B=(rho(i,j,k+1)-rho(i,j,k)+rho(i-1,j,k+1)-rho(i-1,j,k))
     &     *(z_r(i,j,k+1)+z_r(i,j,k)-z_r(i-1,j,k+1)-z_r(i-1,j,k))
 
          pgrd(i)=pgrd(i)+cff*(A-B)
 
          ru(i,j,k)=-0.5*(Hz(i,j,k)+Hz(i-1,j,k))*dn_u(i,j)*pgrd(i)
        enddo
 
 
        if (j.ge.JstrV) then
          cff=0.5*g/rho0
          do i=Istr,Iend
            pgrd(i)=cff*(rho(i,j,N)-rho(i,j-1,N))
     &                               *( z_w(i,j,N)+z_w(i,j-1,N)
     &                                 -z_r(i,j,N)-z_r(i,j-1,N))
 
#ifdef RHO_SURF
     &                   +(g+cff*(rho(i,j,N)+rho(i,j-1,N)))
     &                                *(z_w(i,j,N)-z_w(i,j-1,N))
#endif
 
            rv(i,j,N)=-0.5*(Hz(i,j,N)+Hz(i,j-1,N))
     &                                      *dm_v(i,j)*pgrd(i)
          enddo
 
 
 
          cff=0.25*g/rho0
          k=N-1
          do i=Istr,Iend
            A=(rho(i,j,k+1)+rho(i,j,k)-rho(i,j-1,k+1)-rho(i,j-1,k))
     &       *(z_r(i,j,k+1)-z_r(i,j,k)+z_r(i,j-1,k+1)-z_r(i,j-1,k))
 
            B=(rho(i,j,k+1)-rho(i,j,k)+rho(i,j-1,k+1)-rho(i,j-1,k))
     &       *(z_r(i,j,k+1)+z_r(i,j,k)-z_r(i,j-1,k+1)-z_r(i,j-1,k))
 
            pgrd(i)=pgrd(i)+cff*(A-B)
 
            rv(i,j,k)=-0.5*(Hz(i,j,k)+Hz(i,j-1,k))*dm_v(i,j)*pgrd(i)
          enddo
 
          do k=N-2,2,-1
           do i=Istr,Iend
            A=(rho(i,j,k+1)+rho(i,j,k)-rho(i,j-1,k+1)-rho(i,j-1,k))
     &       *(z_r(i,j,k+1)-z_r(i,j,k)+z_r(i,j-1,k+1)-z_r(i,j-1,k))
 
            B=(rho(i,j,k+1)-rho(i,j,k)+rho(i,j-1,k+1)-rho(i,j-1,k))
     &       *(z_r(i,j,k+1)+z_r(i,j,k)-z_r(i,j-1,k+1)-z_r(i,j-1,k))
 
            J1=A-B
            W1=max(A*B,epsil)
 
 
            A=(rho(i,j,k+2)+rho(i,j,k+1)-rho(i,j-1,k)-rho(i,j-1,k-1))
     &       *(z_r(i,j,k+2)-z_r(i,j,k+1)+z_r(i,j-1,k)-z_r(i,j-1,k-1))
 
            B=(rho(i,j,k+2)-rho(i,j,k+1)+rho(i,j-1,k)-rho(i,j-1,k-1))
     &       *(z_r(i,j,k+2)+z_r(i,j,k+1)-z_r(i,j-1,k)-z_r(i,j-1,k-1))
 
            J2=A-B
            W2=max(A*B,epsil)
 
 
            A=(rho(i,j,k)+rho(i,j,k-1)-rho(i,j-1,k+2)-rho(i,j-1,k+1))
     &       *(z_r(i,j,k)-z_r(i,j,k-1)+z_r(i,j-1,k+2)-z_r(i,j-1,k+1))
 
            B=(rho(i,j,k)-rho(i,j,k-1)+rho(i,j-1,k+2)-rho(i,j-1,k+1))
     &       *(z_r(i,j,k)+z_r(i,j,k-1)-z_r(i,j-1,k+2)-z_r(i,j-1,k+1))
 
            J3=A-B
            W3=max(A*B,epsil)
 
            pgrd(i)=pgrd(i)+cff*(J1*W2*W3 + W1*J2*W3 + W1*W2*J3)
     &                                  /(W2*W3 + W1*W3 + W1*W2)
 
            rv(i,j,k)=-0.5*(Hz(i,j,k)+Hz(i,j-1,k))*dm_v(i,j)*pgrd(i)
           enddo
          enddo
 
          k=1
          do i=Istr,Iend
            A=(rho(i,j,k+1)+rho(i,j,k)-rho(i,j-1,k+1)-rho(i,j-1,k))
     &       *(z_r(i,j,k+1)-z_r(i,j,k)+z_r(i,j-1,k+1)-z_r(i,j-1,k))
 
            B=(rho(i,j,k+1)-rho(i,j,k)+rho(i,j-1,k+1)-rho(i,j-1,k))
     &       *(z_r(i,j,k+1)+z_r(i,j,k)-z_r(i,j-1,k+1)-z_r(i,j-1,k))
 
            pgrd(i)=pgrd(i)+cff*(A-B)
 
            rv(i,j,k)=-0.5*(Hz(i,j,k)+Hz(i,j-1,k))*dm_v(i,j)*pgrd(i)
          enddo
        endif
      enddo
#else
      subroutine prsgrd_empty
#endif /* SOLVE3D */
      return
      end
 
