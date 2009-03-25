      implicit none
      integer tile
      do tile=0,1000
        call check_tiling (tile)
      enddo
      stop
      end

      subroutine check_tiling (tile)
      implicit none
      integer Lm,Mm, NSUB_X,NSUB_E, tile
      parameter (Lm=25, NSUB_X=4,     Mm=21, NSUB_E=5 )
#include "compute_tile_bounds.h"
      if (tile.eq.0) then
        write(*,*) 'tile indx margin size str end grid NSUB'  
      endif
      if (tile .lt. NSUB_X*NSUB_E) then
        if (j_E.eq.0) then
          write(*,'(9I5)') tile, i_X, margin_X, iend-istr+1,
     &                                 istr,iend, Lm,NSUB_X
          if (iend.eq.Lm) write(*,*)
c        elseif (i_X.eq.0) then
c          write(*,'(9I5)') tile, j_E, margin_E, jend-jstr+1,
c     &                               jstr,jend, Mm,NSUB_E
        endif
      endif
      return
      end


