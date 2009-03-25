#include "cppdefs.h"
#if defined ANA_VMIX && defined SOLVE3D
 
      subroutine ana_vmix (tile)
      implicit none
      integer tile
# include "param.h"
# include "compute_tile_bounds.h"
      call ana_vmix_tile (istr,iend,jstr,jend)
      return
      end
 
      subroutine ana_vmix_tile (istr,iend,jstr,jend)
!
! Set vertical mixing coefficients for momentum "Akv"
! and tracers "Akt" [m^2/s] using analytical expressions.
!
      implicit none
      integer istr,iend,jstr,jend, i,j,k
      real cff
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "mixing.h"
# include "scalars.h"
 
# include "compute_auxiliary_bounds.h"
 
# if defined CANYON_B
      cff=1./50.                    ! Setup both top and bottom
      do k=1,N-1                    ! boundary layers.
        do j=jstr,jend
          do i=istr,iend
            Akv(i,j,k)=0.001+0.0095*(
     &                      exp(cff*(z_w(i,j,k)-z_w(i,j,N)))
     &                     +exp(cff*(z_w(i,j,0)-z_w(i,j,k)))
     &                                                     )
            Akt(i,j,k,itemp)=Akt_bak(itemp)
          enddo
        enddo
      enddo
# elif defined UPWELLING || defined IBERIA
      cff=1./150.
      do k=1,N-1
        do j=jstr,jend
          do i=istr,iend
            Akv(i,j,k)=0.002+0.008*exp(cff*(z_w(i,j,k)-z_w(i,j,N)))
            Akt(i,j,k,itemp)=Akt_bak(itemp)
c??
c??
c             Akv(i,j,k)=0.
c             Akt(i,j,k,itemp)=0.
c??
c??
#  ifdef SALINITY
            Akt(i,j,k,isalt)=Akt_bak(isalt)
#  endif
          enddo
        enddo
      enddo
# else
      do k=1,N-1
        do j=jstr,jend
          do i=istr,iend
            Akv(i,j,k)=???
            Akt(i,j,k,itemp)=???
            Akt(i,j,k,isalt)=???
          enddo
        enddo
      enddo
# endif /* CANYON_B */
 
# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_w3d_tile (istr,iend,jstr,jend, Akv)
      call exchange_w3d_tile (istr,iend,jstr,jend,
     &                     Akt(START_2D_ARRAY,0,itemp))
#  ifdef SALINITY
      call exchange_w3d_tile (istr,iend,jstr,jend,
     &                     Akt(START_2D_ARRAY,0,isalt))
#  endif
# endif
#else
      subroutine ana_vmix_empty
#endif /* ANA_VMIX */
      return
      end
 
 
