#include "cppdefs.h"
#ifdef SOLVE3D
                                   ! This routine just sets rhs
      subroutine prsgrd (tile)     ! arrays for 3D momentum equations
      implicit none                ! to zero. It is only useful for
      integer tile                 ! debugging purposes.
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      call prsgrd00_tile (Istr,Iend,Jstr,Jend, A3d(1,1),A3d(1,2))
      return
      end
 
      subroutine prsgrd00_tile (Istr,Iend,Jstr,Jend, ru,rv)
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k
# include "param.h"
      real ru(PRIVATE_2D_SCRATCH_ARRAY,N),
     &     rv(PRIVATE_2D_SCRATCH_ARRAY,N)
      do k=1,N
        do j=Jstr,Jend
          do i=Istr,Iend
            ru(i,j,k)=0.
            rv(i,j,k)=0.
          enddo
        enddo
      enddo
      return
      end
#else
      subroutine prsgrd_empty
      end
#endif
 
