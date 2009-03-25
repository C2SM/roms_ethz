#include "cppdefs.h"
#ifdef SOLVE3D

 
      subroutine set_depth (tile)
      implicit none
      integer tile
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      call set_depth_tile (Istr,Iend,Jstr,Jend, A2d(1,1))
      return
      end
 
      subroutine set_depth_tile (Istr,Iend,Jstr,Jend, dV)
!
! Create S-coordinate system: based on model topography h(i,j),
! fast-time-averaged free-surface field and vertical coordinate
! transformation metrics compute evolving depths of of the three-
! dimensional model grid (z_r,z_w) and vertical heights of model
! grid boxes.
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k
      real cff_r,cff1_r,cff2_r, cff_w,cff1_w,cff2_w, z_r0,z_w0, ds
# include "param.h"
# ifdef OBC_FLUX_CORR
      real*QUAD dV(PRIVATE_2D_SCRATCH_ARRAY), my_exc_vol
      logical lsumX, lsumY
      integer nsubs, inc, jnc
# else
      real dV
# endif
# include "scoord.h"
# include "scalars.h"
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "coupling.h"
!
# include "compute_auxiliary_bounds.h"
!
      if (iic.eq.0) then               ! During initialization and
        do j=JstrR,JendR               ! /or restart: copy initial
          do i=IstrR,IendR             ! free surface field into
# ifdef NEW_S_COORD
            hinv(i,j)=1./(h(i,j)+hc)   ! array for holding fast-time
# else
            hinv(i,j)=1./h(i,j)        ! averaged free surface.
#endif
          enddo
        enddo

        do j=JstrR,JendR
          do i=Istr,IendR
            DU_avg1(i,j)=0.5*( h(i-1,j)+h(i,j)+zeta(i,j,1)
     &              +zeta(i-1,j,1) )*dn_u(i,j)*ubar(i,j,1)
          enddo
        enddo

        do j=Jstr,JendR
          do i=IstrR,IendR
            DV_avg1(i,j)=0.5*( h(i,j)+h(i,j-1)+zeta(i,j,1)
     &              +zeta(i,j-1,1) )*dm_v(i,j)*vbar(i,j,1)
          enddo
        enddo
      endif
 
# ifdef OBC_FLUX_CORR
      do j=Jstr,Jend
        do i=Istr,Iend
#  ifdef MASKING
         dV(i,j)=rmask(i,j)*Zt_avg1(i,j)/(pm(i,j)*pn(i,j))
#  else
         dV(i,j)=           Zt_avg1(i,j)/(pm(i,j)*pn(i,j))
#  endif
        enddo
      enddo                        ! The following segment computes
                                   ! sums of dV within the subdomain
      lsumX=.true.                 ! tile using split-directional
      lsumY=.true.                 ! reduction-by-pairs algorithm, to
      inc=1                        ! avoid accumulation of roundoff
      jnc=1                        ! errors.
 
      do while (lsumX .or. lsumY)
        if (Istr.le.Iend-inc) then
          do j=Jstr,Jend
            do i=Istr,Iend-inc,2*inc
              dV(i,j)=dV(i,j)+dV(i+inc,j)
            enddo
          enddo
          inc=2*inc
        else
          lsumX=.false.
        endif
        if (Jstr.le.Jend-jnc) then
          do j=Jstr,Jend-jnc,2*jnc
            do i=Istr,Iend
              dV(i,j)=dV(i,j)+dV(i,j+jnc)
            enddo
          enddo
          jnc=2*jnc
        else
          lsumY=.false.
        endif
      enddo
      my_exc_vol=dV(Istr,Jstr)
                                     ! Straight summation: this code
c**   my_exc_vol=QuadZero            ! produces result which differs
c**   do j=Jstr,Jend                 ! above only by roundoff errors.
c**     do i=Istr,Iend
c**       my_exc_vol=my_exc_vol+dV(i,j)
c**     enddo
c**   enddo
 
      if (SINGLE_TILE_MODE) then
        nsubs=1
      else
        nsubs=NSUB_X*NSUB_E
      endif
 
C$OMP CRITICAL (obc_flux_corr_crgn)
      if (tile_count.eq.0) exc_vol=QuadZero
      tile_count=tile_count+1
      exc_vol=exc_vol+my_exc_vol
      if (tile_count.eq.nsubs) then
        tile_count=0
#  ifdef MPI
  ........ to be filled in
#  endif
        ubar_xs=exc_vol/(bc_crss*tau_obc)
c        write(*,*) 'ubar_xs = ', ubar_xs
      endif
C$OMP END CRITICAL (obc_flux_corr_crgn)
# endif

      ds=1.D0/dble(N)
      do j=JstrR,JendR
        do i=IstrR,IendR
          z_w(i,j,0)=-h(i,j)
        enddo
        
        do k=1,N,+1  !--> irreversible
# ifdef NEW_S_COORD
          cff_w=hc*ds* dble(k-N)
          cff_r=hc*ds*(dble(k-N)-0.5)
#else
          cff_w=hc*(ds* dble(k-N)     -Cs_w(k))
          cff_r=hc*(ds*(dble(k-N)-0.5)-Cs_r(k))

          cff2_w=ds* dble(k)
          cff2_r=ds*(dble(k)-0.5)
#endif
          cff1_w=Cs_w(k)
          cff1_r=Cs_r(k)

 
          do i=IstrR,IendR
# ifdef NEW_S_COORD
            z_w(i,j,k)=zeta(i,j,knew) +(zeta(i,j,knew)+h(i,j))
     &                        *(cff_w+cff1_w*h(i,j))*hinv(i,j) 

            z_r(i,j,k)=zeta(i,j,knew) +(zeta(i,j,knew)+h(i,j))
     &                        *(cff_r+cff1_r*h(i,j))*hinv(i,j)
# else
            z_w0=cff_w+cff1_w*h(i,j)
            z_w(i,j,k)=z_w0+zeta(i,j,knew)*(1.+z_w0*hinv(i,j))
 
            z_r0=cff_r+cff1_r*h(i,j)
            z_r(i,j,k)=z_r0+zeta(i,j,knew)*(1.+z_r0*hinv(i,j))
#endif
 
            Hz_bak(i,j,k)=Hz(i,j,k)
            Hz(i,j,k)=z_w(i,j,k)-z_w(i,j,k-1)
          enddo
        enddo
      enddo
# if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      if (iic.eq.0) then
        call exchange_r2d_tile (Istr,Iend,Jstr,Jend, hinv)
      endif
      call exchange_w3d_tile (Istr,Iend,Jstr,Jend, z_w)
      call exchange_r3d_tile (Istr,Iend,Jstr,Jend, z_r)
      call exchange_r3d_tile (Istr,Iend,Jstr,Jend, Hz)
      call exchange_r3d_tile (Istr,Iend,Jstr,Jend, Hz_bak)
# endif
      return
      end
 
 
 
      subroutine set_HUV (tile)
      implicit none
      integer tile
# include "param.h"
# include "compute_tile_bounds.h"
      call set_HUV_tile (Istr,Iend,Jstr,Jend)
      return
      end
 
      subroutine set_HUV_tile (Istr,Iend,Jstr,Jend)
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
!
# include "compute_auxiliary_bounds.h"
!
      do k=1,N
        do j=JstrR,JendR
          do i=Istr,IendR
            FlxU(i,j,k)=0.5*(Hz(i,j,k)+Hz(i-1,j,k))*dn_u(i,j)
     &                                         *u(i,j,k,nrhs)
          enddo
        enddo
        do j=Jstr,JendR
          do i=IstrR,IendR
            FlxV(i,j,k)=0.5*(Hz(i,j,k)+Hz(i,j-1,k))*dm_v(i,j)
     &                                         *v(i,j,k,nrhs)
          enddo
        enddo
      enddo
# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend, FlxU)
      call exchange_v3d_tile (Istr,Iend,Jstr,Jend, FlxV)
# endif
      return
      end
 
      subroutine set_HUV1 (tile)
      implicit none
      integer tile
#include "param.h"
#include "private_scratch.h"
#include "compute_tile_bounds.h"
      call set_HUV1_tile (Istr,Iend,Jstr,Jend, A2d(1,1), A2d(1,2))
      return
      end
 
      subroutine set_HUV1_tile (Istr,Iend,Jstr,Jend, DC,FC)
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k
# include "param.h"
      real DC(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     FC(PRIVATE_1D_SCRATCH_ARRAY,0:N)
# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
# include "coupling.h"
!
# include "compute_auxiliary_bounds.h"
!
      do j=JstrR,JendR
        do i=Istr,IendR
          DC(i,N)=0.5*(Hz(i,j,N)+Hz(i-1,j,N))*dn_u(i,j)
          DC(i,0)=DC(i,N)
          FC(i,0)=DC(i,N)*u(i,j,N,nnew)
        enddo
        do k=N-1,1,-1
          do i=Istr,IendR
            DC(i,k)=0.5*(Hz(i,j,k)+Hz(i-1,j,k))*dn_u(i,j)
            DC(i,0)=DC(i,0)+DC(i,k)
            FC(i,0)=FC(i,0)+DC(i,k)*u(i,j,k,nnew)
          enddo
        enddo
        do i=Istr,IendR
c??       FC(i,0)=(FC(i,0)-DU_avg2(i,j))/DC(i,0)
c>          FC(i,0)=(FC(i,0)-DU_avg1(i,j))/DC(i,0)
          
          FC(i,0)=(FC(i,0)-2.*DU_avg1(i,j)
     &                    +DU_avg2(i,j))/DC(i,0)

        enddo
        do k=1,N
          do i=Istr,IendR
            u(i,j,k,nnew)=(u(i,j,k,nnew)-FC(i,0))
# ifdef MASKING
     &                                *umask(i,j)
# endif
            FlxU(i,j,k)=DC(i,k)*u(i,j,k,nnew)
          enddo
        enddo
        if (j.ge.Jstr) then
          do i=IstrR,IendR
            DC(i,N)=0.5*(Hz(i,j,N)+Hz(i,j-1,N))*dm_v(i,j)
            DC(i,0)=DC(i,N)
            FC(i,0)=DC(i,N)*v(i,j,N,nnew)
          enddo
          do k=N-1,1,-1
            do i=IstrR,IendR
              DC(i,k)=0.5*(Hz(i,j,k)+Hz(i,j-1,k))*dm_v(i,j)
              DC(i,0)=DC(i,0)+DC(i,k)
              FC(i,0)=FC(i,0)+DC(i,k)*v(i,j,k,nnew)
            enddo
          enddo
          do i=IstrR,IendR
c??         FC(i,0)=(FC(i,0)-DV_avg2(i,j))/DC(i,0)
c>            FC(i,0)=(FC(i,0)-DV_avg1(i,j))/DC(i,0)

            FC(i,0)=(FC(i,0)-2.*DV_avg1(i,j)
     &                      +DV_avg2(i,j))/DC(i,0)
          enddo
          do k=1,N
            do i=IstrR,IendR
              v(i,j,k,nnew)=(v(i,j,k,nnew)-FC(i,0))
# ifdef MASKING
     &                                *vmask(i,j)
# endif
              FlxV(i,j,k)=DC(i,k)*v(i,j,k,nnew)
            enddo
          enddo
        endif
      enddo
!
! Exchange periodic boundaries, if so prescribed.
!
# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend, FlxU)
      call exchange_v3d_tile (Istr,Iend,Jstr,Jend, FlxV)
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend,
     &                          u(START_2D_ARRAY,1,nnew))
      call exchange_v3d_tile (Istr,Iend,Jstr,Jend,
     &                          v(START_2D_ARRAY,1,nnew))
# endif
      return
      end
#else
      subroutine set_depth_empty
      end
#endif /* SOLVE3D */
 
 
