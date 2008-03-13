      subroutine exchange_tile (istr,iend,jstr,jend, A,nmax)

      implicit none
      
#include "param.h"
#include "scalars.h"
      integer istr,iend,jstr,jend, nmax, i,j,k
      real A(GLOBAL_2D_ARRAY,nmax)
CSDISTRIBUTE_RESHAPE A(BLOCK_PATTERN,*) BLOCK_CLAUSE
!
#include "compute_auxiliary_bounds.h"
!
#ifdef EW_PERIODIC
# ifdef NS_PERIODIC
#  define J_RANGE jstr,jend
# else
#  define J_RANGE jstrR,jendR
# endif
# ifdef MPI
      if (NP_XI.eq.1) then
# endif
        if (WESTERN_EDGE) then
          do k=1,nmax
            do j=J_RANGE
              A(Lm+1,j,k)=A(1,j,k)
              A(Lm+2,j,k)=A(2,j,k)
            enddo
          enddo
        endif
        if (EASTERN_EDGE) then
          do k=1,nmax
            do j=J_RANGE
              A(-1,j,k)=A(Lm-1,j,k)
              A( 0,j,k)=A(Lm  ,j,k)
            enddo
          enddo
        endif
# ifdef MPI
      endif
# endif
# undef J_RANGE
#endif            /* EW_PERIODIC */
 
#ifdef NS_PERIODIC
# ifdef EW_PERIODIC
#  define I_RANGE istr,iend
# else
#  define I_RANGE istrR,iendR
# endif
# ifdef MPI
      if (NP_ETA.eq.1) then
# endif
        if (SOUTHERN_EDGE) then
          do k=1,nmax
            do i=I_RANGE
              A(i,Mm+1,k)=A(i,1,k)
              A(i,Mm+2,k)=A(i,2,k)
            enddo
          enddo
        endif
        if (NORTHERN_EDGE) then
          do k=1,nmax
            do i=I_RANGE
              A(i,-1,k)=A(i,Mm-1,k)
              A(i, 0,k)=A(i,Mm  ,k)
            enddo
          enddo
        endif
# ifdef MPI
      endif
# endif
# undef I_RANGE
#endif               /* NS_PERIODIC */
 
#if defined EW_PERIODIC && defined NS_PERIODIC
# ifdef MPI
      if (NP_XI.eq.1 .and. NP_ETA.eq.1) then
# endif
        if (WESTERN_EDGE .and. SOUTHERN_EDGE) then
          do k=1,nmax
            A(Lm+1,Mm+1,k)=A(1,1,k)
            A(Lm+1,Mm+2,k)=A(1,2,k)
            A(Lm+2,Mm+1,k)=A(2,1,k)
            A(Lm+2,Mm+2,k)=A(2,2,k)
          enddo
        endif
        if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
          do k=1,nmax
            A(-1,Mm+1,k)=A(Lm-1,1,k)
            A( 0,Mm+1,k)=A(Lm  ,1,k)
            A(-1,Mm+2,k)=A(Lm-1,2,k)
            A( 0,Mm+2,k)=A(Lm  ,2,k)
          enddo
        endif
        if (WESTERN_EDGE .and. NORTHERN_EDGE) then
          do k=1,nmax
            A(Lm+1,-1,k)=A(1,Mm-1,k)
            A(Lm+1, 0,k)=A(1,Mm  ,k)
            A(Lm+2,-1,k)=A(2,Mm-1,k)
            A(Lm+2, 0,k)=A(2,Mm  ,k)
          enddo
        endif
        if (EASTERN_EDGE .and. NORTHERN_EDGE) then
          do k=1,nmax
            A(-1,-1,k)=A(Lm-1,Mm-1,k)
            A( 0,-1,k)=A(Lm  ,Mm-1,k)
            A(-1, 0,k)=A(Lm-1,Mm  ,k)
            A( 0, 0,k)=A(Lm  ,Mm  ,k)
          enddo
        endif
# ifdef MPI
      endif
# endif
#endif
#ifdef MPI
      call mpi_exchange_tile (istr,iend,jstr,jend,  A,nmax)
#endif
      return
      end
 
