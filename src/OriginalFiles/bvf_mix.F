#include "cppdefs.h"
#ifdef BVF_MIXING
 
      subroutine bvf_mix (tile)
      implicit none
      integer tile
# include "param.h"
# include "compute_tile_bounds.h"
      call bvf_mix_tile (istr,iend,jstr,jend)
      return
      end
 
      subroutine bvf_mix_tile (istr,iend,jstr,jend)
      implicit none
      integer istr,iend,jstr,jend, i,j,k
      real bvf_numin, bvf_numax, bvf_nu0c, bvf_nu0, cff
      parameter (
     &    bvf_numin=3.e-5,  ! Upper and lower bounds for
     &    bvf_numax=4.e-4,  !   vertical diffusion, [m2/s];
     &    bvf_nu0c=1.,      ! Proportionality constant, [m2/s2];
     &    bvf_nu0=1.0e-7)   ! Convective diffusion [m2/s] in
                            !                unstable regime.
 
# include "param.h"
# include "mixing.h"
# include "scalars.h"
!
! Compute diffusivities using Brunt-Vaisala frequency based vertical
! mixing scheme.  Set viscosity to its background value. If static
! unstable regime, set diffusivities to "bvf_nu0c".
!
      do k=1,N-1
        do j=jstr,jend
          do i=istr,iend
            Akv(i,j,k)=Akv_bak
            if (bvf(i,j,k).gt.0.) then
              cff=bvf_nu0/sqrt(bvf(i,j,k))
              Akt(i,j,k,itemp)=min(bvf_numax, max(bvf_numin, cff))
              Akv(i,j,k)=Akt(i,j,k,itemp)
# ifdef SALINITY
              Akt(i,j,k,isalt)=Akt(i,j,k,itemp)
# endif
            else
              Akv(i,j,k)=bvf_nu0c
              Akt(i,j,k,itemp)=bvf_nu0c
# ifdef SALINITY
              Akt(i,j,k,isalt)=bvf_nu0c
# endif
            endif
          enddo
        enddo
      enddo
 
# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_w3d_tile (istr,iend,jstr,jend, Akv)
      call exchange_w3d_tile (istr,iend,jstr,jend,
     &                     Akt(START_2D_ARRAY,0,itemp))
      call exchange_w3d_tile (istr,iend,jstr,jend,
     &                     Akt(START_2D_ARRAY,0,isalt))
# endif
      return
      end
#else
      subroutine bvf_mix_empty
      end
#endif

