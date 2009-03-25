#include "cppdefs.h"
#ifdef SOLVE3D
 
      subroutine prsgrd (tile)
      implicit none
      integer tile
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      call prsgrd32A_tile (Istr,Iend,Jstr,Jend,  A3d(1,1), A3d(1,2),
     &                                A3d(1,3),  A2d(1,1), A2d(1,2),
     &                       A2d(1,1), A2d(1,2), A2d(1,3), A2d(1,4))
      return
      end
 
      subroutine prsgrd32A_tile (Istr,Iend,Jstr,Jend, ru,rv, P,
     &                                   dR,dZ, FC,aux,dRx,dZx)
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k
# include "param.h"
      real ru(PRIVATE_2D_SCRATCH_ARRAY,N),   OneFifth,
     &     rv(PRIVATE_2D_SCRATCH_ARRAY,N),   OneTwelfth,
     &      P(PRIVATE_2D_SCRATCH_ARRAY,N),   epsil,
 
     &     dR(PRIVATE_1D_SCRATCH_ARRAY,0:N), GRho,
     &     dZ(PRIVATE_1D_SCRATCH_ARRAY,0:N), HalfGRho,
 
     &      aux(PRIVATE_2D_SCRATCH_ARRAY),
     &       FC(PRIVATE_2D_SCRATCH_ARRAY),   cff,
     &      dRx(PRIVATE_2D_SCRATCH_ARRAY),   cff1,
     &      dZx(PRIVATE_2D_SCRATCH_ARRAY)
 
      parameter (OneFifth=0.2, OneTwelfth=1./12., epsil=1.E-10)
 
# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
!
# include "compute_auxiliary_bounds.h"
!
! Not-conservative Density-Jacobian scheme, based on cubic
! polynomial fits for rho and z_r as functions of nondimensianal
! coordinatess, xi, eta, s (basically their respective fortran
! indices).
! The cubic polynomials are monotonized by using harmonic mean
! instead of linear averages to interpolate slopes.
! This scheme retains exact antisymmetry J(rho,z_r)=-J(z_r,rho).
! If parameter OneFifth (see above) is set to zero, the scheme
! becomes identical to standard jacobian.
!
! NOTE: This routine is an alternative form of prsgrd32 and it
!       produces results identical to that if its prototype.
!
! Preliminary step (same for XI- and ETA-components:
!------------ ---- ----- --- --- --- ---------------
!
      GRho=g/rho0
      HalfGRho=0.5*GRho
 
      do j=JstrV-1,Jend
        do k=1,N-1
          do i=IstrU-1,Iend
            dR(i,k)=rho(i,j,k+1)-rho(i,j,k)
            dZ(i,k)=z_r(i,j,k+1)-z_r(i,j,k)
          enddo
        enddo
        do i=IstrU-1,Iend
          dR(i,N)=dR(i,N-1)
          dZ(i,N)=dZ(i,N-1)
          dR(i,0)=dR(i,1)
          dZ(i,0)=dZ(i,1)
        enddo
        do k=N,1,-1               !--> irreversible
          do i=IstrU-1,Iend
            cff=2.*dR(i,k)*dR(i,k-1)
            if (cff.gt.epsil) then
              dR(i,k)=cff/(dR(i,k)+dR(i,k-1))
            else
              dR(i,k)=0.
            endif
            dZ(i,k)=2.*dZ(i,k)*dZ(i,k-1)/(dZ(i,k)+dZ(i,k-1))
          enddo
        enddo
        do i=IstrU-1,Iend
          P(i,j,N)=g*z_w(i,j,N) + GRho*( rho(i,j,N)
     &        +0.5*(rho(i,j,N)-rho(i,j,N-1))*(z_w(i,j,N)-z_r(i,j,N))
     &           /(z_r(i,j,N)-z_r(i,j,N-1)) )*(z_w(i,j,N)-z_r(i,j,N))
        enddo
        do k=N-1,1,-1
          do i=IstrU-1,Iend
            P(i,j,k)=P(i,j,k+1)+HalfGRho*( (rho(i,j,k+1)+rho(i,j,k))
     &                                     *(z_r(i,j,k+1)-z_r(i,j,k))
 
     &     -OneFifth*( (dR(i,k+1)-dR(i,k))*( z_r(i,j,k+1)-z_r(i,j,k)
     &                              -OneTwelfth*(dZ(i,k+1)+dZ(i,k)) )
 
     &                -(dZ(i,k+1)-dZ(i,k))*( rho(i,j,k+1)-rho(i,j,k)
     &                              -OneTwelfth*(dR(i,k+1)+dR(i,k)) )
     &                                                             ))
          enddo
        enddo
      enddo
!
! Compute XI-component of pressure gradient term:
!-------- ------------ -- -------- -------- -----
!
      do k=N,1,-1
        do j=Jstr,Jend
          do i=IstrU-1,Iend+1
            aux(i,j)=(z_r(i,j,k)-z_r(i-1,j,k))
#ifdef MASKING
     &                              *umask(i,j)
#endif
            FC(i,j)=(rho(i,j,k)-rho(i-1,j,k))
#ifdef MASKING
     &                              *umask(i,j)
#endif
          enddo
        enddo
 
        do j=Jstr,Jend
          do i=IstrU-1,Iend
            cff=2.*aux(i,j)*aux(i+1,j)
            if (cff.gt.epsil) then
              dZx(i,j)=cff/(aux(i,j)+aux(i+1,j))
            else
              dZx(i,j)=0.
            endif
            cff1=2.*FC(i,j)*FC(i+1,j)
            if (cff1.gt.epsil) then
              dRx(i,j)=cff1/(FC(i,j)+FC(i+1,j))
            else
              dRx(i,j)=0.
            endif
          enddo
        enddo                 !--> discard aux, FC
 
        do j=Jstr,Jend
          do i=IstrU,Iend
            ru(i,j,k)=0.5*(Hz(i,j,k)+Hz(i-1,j,k))*dn_u(i,j)*(
     &                           P(i-1,j,k)-P(i,j,k)-HalfGRho*(
     &          (rho(i,j,k)+rho(i-1,j,k))*(z_r(i,j,k)-z_r(i-1,j,k))
 
     &   -OneFifth*( (dRx(i,j)-dRx(i-1,j))*( z_r(i,j,k)-z_r(i-1,j,k)
     &                            -OneTwelfth*(dZx(i,j)+dZx(i-1,j)) )
 
     &              -(dZx(i,j)-dZx(i-1,j))*( rho(i,j,k)-rho(i-1,j,k)
     &                            -OneTwelfth*(dRx(i,j)+dRx(i-1,j)) )
     &                                                            )))
          enddo
        enddo
!
! ETA-component of pressure gradient term:
!-------------- -- -------- -------- -----
!
        do j=JstrV-1,Jend+1
          do i=Istr,Iend
            aux(i,j)=(z_r(i,j,k)-z_r(i,j-1,k))
#ifdef MASKING
     &                              *vmask(i,j)
#endif
            FC(i,j)=(rho(i,j,k)-rho(i,j-1,k))
#ifdef MASKING
     &                              *vmask(i,j)
#endif
          enddo
        enddo                 !--> discard aux, FC
 
        do j=JstrV-1,Jend
          do i=Istr,Iend
            cff=2.*aux(i,j)*aux(i,j+1)
            if (cff.gt.epsil) then
              dZx(i,j)=cff/(aux(i,j)+aux(i,j+1))
            else
              dZx(i,j)=0.
            endif
            cff1=2.*FC(i,j)*FC(i,j+1)
            if (cff1.gt.epsil) then
              dRx(i,j)=cff1/(FC(i,j)+FC(i,j+1))
            else
              dRx(i,j)=0.
            endif
          enddo
        enddo
 
        do j=JstrV,Jend
          do i=Istr,Iend
            rv(i,j,k)=0.5*(Hz(i,j,k)+Hz(i,j-1,k))*dm_v(i,j)*(
     &      P(i,j-1,k)-P(i,j,k)-HalfGRho*( (rho(i,j,k)+rho(i,j-1,k))
     &                                     *(z_r(i,j,k)-z_r(i,j-1,k))
 
     &   -OneFifth*( (dRx(i,j)-dRx(i,j-1))*( z_r(i,j,k)-z_r(i,j-1,k)
     &                            -OneTwelfth*(dZx(i,j)+dZx(i,j-1)) )
 
     &              -(dZx(i,j)-dZx(i,j-1))*( rho(i,j,k)-rho(i,j-1,k)
     &                            -OneTwelfth*(dRx(i,j)+dRx(i,j-1)) )
     &                                                            )))
          enddo
        enddo
      enddo
      return
      end
#else
      subroutine prsgrd_empty
      end
#endif /* SOLVE3D */
 
