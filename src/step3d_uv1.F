#include "cppdefs.h"
#ifdef SOLVE3D
 
      subroutine step3d_uv1 (tile)
      implicit none
      integer tile
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      call step3d_uv1_tile (istr,iend,jstr,jend,  A3d(1,1), A3d(1,2),
     &                                                      A2d(1,1))
      return
      end
 
      subroutine step3d_uv1_tile (istr,iend,jstr,jend, ru,rv,DC)
      implicit none
      integer istr,iend,jstr,jend, i,j,k
# include "param.h"
      real ru(PRIVATE_2D_SCRATCH_ARRAY,N),
     &     rv(PRIVATE_2D_SCRATCH_ARRAY,N),
     &     DC(PRIVATE_1D_SCRATCH_ARRAY,0:N),  cff
# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
!
# include "compute_auxiliary_bounds.h"
!
      cff=0.25*dt
      do j=jstr,jend
        do i=istrU,iend
          DC(i,0)=cff*(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
        enddo
        do k=1,N
          do i=istrU,iend
            u(i,j,k,nnew)=u(i,j,k,nnew)+DC(i,0)*ru(i,j,k)
          enddo
        enddo
        if (j.ge.jstrV) then
          do i=istr,iend
            DC(i,0)=cff*(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
          enddo
          do k=1,N
            do i=istr,iend
              v(i,j,k,nnew)=v(i,j,k,nnew)+DC(i,0)*rv(i,j,k)
            enddo
          enddo
        endif
      enddo
      return
      end
#else
      subroutine step3d_uv1_empty
      end
#endif
 
