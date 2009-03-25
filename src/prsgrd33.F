#include "cppdefs.h"
#ifdef SOLVE3D
 
      subroutine prsgrd (tile)
      implicit none
      integer tile
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      call prsgrd33_tile (Istr,Iend,Jstr,Jend,  A3d(1,1), A3d(1,2),
     &                                          A3d(1,3), A3d(1,4),
     &                                A2d(1,1), A2d(1,2), A2d(1,1))
      return
      end
 
      subroutine prsgrd33_tile (Istr,Iend,Jstr,Jend, ru,rv,
     &                                  FX,dRdZ, dR,dZ, FC)
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k, k1,k2
# include "param.h"
      real ru(PRIVATE_2D_SCRATCH_ARRAY,N),   OneSixth,
     &     rv(PRIVATE_2D_SCRATCH_ARRAY,N),   GRho,
     &     FX(PRIVATE_2D_SCRATCH_ARRAY,N),   HalfGRho,
     &   dRdZ(PRIVATE_2D_SCRATCH_ARRAY,N),   dh, epsil,
 
     &     dR(PRIVATE_1D_SCRATCH_ARRAY,0:N), cff, cff1,
     &     dZ(PRIVATE_1D_SCRATCH_ARRAY,0:N), HalfGRho,
     &     FC(PRIVATE_2D_SCRATCH_ARRAY,2)
 
      parameter (OneSixth=1./6., epsil=1.E-10)
 
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
        do k=N,1,-1
          do i=IstrU-1,Iend
            cff=dR(i,k)*dR(i,k-1)
            if (cff.gt.epsil) then
              dRdZ(i,j,k)=cff*(dZ(i,k)+dZ(i,k-1))/( dZ(i,k)
     &                        *dZ(i,k-1)*(dR(i,k)+dR(i,k-1))
     &                                                     )
            else
              dRdZ(i,j,k)=0.
            endif
          enddo
        enddo
        do i=IstrU-1,Iend
          FX(i,j,N)=GRho*( rho(i,j,N) +0.5*(rho(i,j,N)-rho(i,j,N-1))
     &             *(z_w(i,j,N)-z_r(i,j,N))/(z_r(i,j,N)-z_r(i,j,N-1))
     &                                      )*(z_w(i,j,N)-z_r(i,j,N))
        enddo
        do k=1,N-1
          do i=IstrU-1,Iend
            dh=z_r(i,j,k+1)-z_r(i,j,k)
            FX(i,j,k)=HalfGRho*dh*( rho(i,j,k+1)+rho(i,j,k)
     &            -OneSixth*dh*(dRdZ(i,j,k+1)-dRdZ(i,j,k)) )
          enddo
        enddo
      enddo
!
! Compute XI-component of pressure gradient term:
!-------- ------------ -- -------- -------- -----
!
      do k=N,1,-1     !--> irreversible
        k1=1+mod(k,2)
        k2=3-k1
        do j=Jstr,Jend
          do i=IstrU,Iend
            dh=z_r(i,j,k)-z_r(i-1,j,k)
            FC(i,j,k1)=HalfGRho*dh*( rho(i,j,k)+rho(i-1,j,k)
     &              -OneSixth*dh*(dRdZ(i,j,k)-dRdZ(i-1,j,k)) )
          enddo
        enddo
        if (k.eq.N) then
          do j=Jstr,Jend
            do i=IstrU,Iend
              ru(i,j,N)=g*(z_w(i-1,j,N)-z_w(i,j,N))
     &             +FX(i-1,j,N)-FX(i,j,N)-FC(i,j,k1)
            enddo
          enddo
        else
          do j=Jstr,Jend
            do i=IstrU,Iend
              ru(i,j,k)=ru(i,j,k+1) +FX(i-1,j,k)-FX(i,j,k)
     &                               +FC(i,j,k2)-FC(i,j,k1)
 
              ru(i,j,k+1)=ru(i,j,k+1)*0.5*( Hz(i,j,k+1)+
     &                            Hz(i-1,j,k+1) )*dn_u(i,j)
            enddo
          enddo
        endif
      enddo
      do j=Jstr,Jend
        do i=IstrU,Iend
          ru(i,j,1)=ru(i,j,1)*0.5*(Hz(i,j,1)+Hz(i-1,j,1))*dn_u(i,j)
        enddo
      enddo
!
! ETA-component of pressure gradient term:
!-------------- -- -------- -------- -----
!
      do k=N,1,-1     !--> irreversible
        k1=1+mod(k,2)
        k2=3-k1
        do j=JstrV,Jend
          do i=Istr,Iend
            dh=z_r(i,j,k)-z_r(i,j-1,k)
            FC(i,j,k1)=HalfGRho*dh*( rho(i,j,k)+rho(i,j-1,k)
     &             -OneSixth*dh*(dRdZ(i,j,k)-dRdZ(i,j-1,k)) )
          enddo
        enddo
        if (k.eq.N) then
          do j=JstrV,Jend
            do i=Istr,Iend
              rv(i,j,N)=g*(z_w(i,j-1,N)-z_w(i,j,N))
     &             +FX(i,j-1,N)-FX(i,j,N)-FC(i,j,k1)
            enddo
          enddo
        else
          do j=JstrV,Jend
            do i=Istr,Iend
              rv(i,j,k)=rv(i,j,k+1) +FX(i,j-1,k)-FX(i,j,k)
     &                               +FC(i,j,k2)-FC(i,j,k1)
 
              rv(i,j,k+1)=rv(i,j,k+1)*0.5*( Hz(i,j,k+1)+
     &                            Hz(i,j-1,k+1) )*dm_v(i,j)
            enddo
          enddo
        endif
      enddo
      do j=JstrV,Jend
        do i=Istr,Iend
          rv(i,j,1)=rv(i,j,1)*0.5*(Hz(i,j,1)+Hz(i,j-1,1))*dm_v(i,j)
        enddo
      enddo
      return
      end
#else
      subroutine prsgrd_empty
      end
#endif /* SOLVE3D */
 
