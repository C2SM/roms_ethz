      subroutine exchange_2d_tile (istr,iend,jstr,jend, A)
!
! Set periodic boundary conditions (if any) for a two-dimensional
! field A of ZETA-, U-, V- or PSI-type. This file is designed to
! generate four different subroutines, by redefining (via CPP) the
! name of the subroutine exchange_2d_tile above and the starting
! indices ISTART = [istr for U-,PSI-type; istrR for V-,ZETA-type]
! and JSTART = [jstr for V-,PSI-type; jstrR for U-,ZETA-type]
! below. See also mounting file exchange.F
!
      implicit none
#include "param.h"
#include "scalars.h"
      real A(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE A(BLOCK_PATTERN) BLOCK_CLAUSE
      integer istr,iend,jstr,jend, i,j
!
#include "compute_auxiliary_bounds.h"
!
#ifdef EW_PERIODIC
# ifdef NS_PERIODIC
#  define J_RANGE jstr,jend
# else
#  define J_RANGE JSTART,jendR
# endif
# ifdef MPI
      if (NP_XI.eq.1) then
# endif
        if (istr.eq.1) then
          do j=J_RANGE
            A(Lm+1,j)=A(1,j)
            A(Lm+2,j)=A(2,j)
          enddo
        endif
        if (iend.eq.Lm) then
          do j=J_RANGE
            A(-1,j)=A(Lm-1,j)
            A( 0,j)=A(Lm  ,j)
          enddo
        endif
# ifdef MPI
      endif
# endif
# undef J_RANGE
#endif
 
#ifdef NS_PERIODIC
# ifdef EW_PERIODIC
#  define I_RANGE istr,iend
# else
#  define I_RANGE ISTART,iendR
# endif
# ifdef MPI
      if (NP_ETA.eq.1) then
# endif
        if (jstr.eq.1) then
          do i=I_RANGE
            A(i,Mm+1)=A(i,1)
            A(i,Mm+2)=A(i,2)
          enddo
        endif
        if (jend.eq.Mm) then
          do i=I_RANGE
            A(i,-1)=A(i,Mm-1)
            A(i, 0)=A(i,Mm  )
          enddo
        endif
# ifdef MPI
      endif
# endif
# undef I_RANGE
#endif
 
#if defined EW_PERIODIC && defined NS_PERIODIC
# ifdef MPI
      if (NP_XI.eq.1 .and. NP_ETA.eq.1) then
# endif
        if (istr.eq.1 .and. jstr.eq.1) then
          A(Lm+1,Mm+1)=A(1,1)
          A(Lm+1,Mm+2)=A(1,2)
          A(Lm+2,Mm+1)=A(2,1)
          A(Lm+2,Mm+2)=A(2,2)
        endif
        if (iend.eq.Lm .and. jstr.eq.1) then
          A(-1,Mm+1)=A(Lm-1,1)
          A( 0,Mm+1)=A(Lm  ,1)
          A(-1,Mm+2)=A(Lm-1,2)
          A( 0,Mm+2)=A(Lm  ,2)
        endif
        if (istr.eq.1  .and. jend.eq.Mm) then
          A(Lm+1,-1)=A(1,Mm-1)
          A(Lm+1, 0)=A(1,Mm  )
          A(Lm+2,-1)=A(2,Mm-1)
          A(Lm+2, 0)=A(2,Mm  )
        endif
        if (iend.eq.Lm .and. jend.eq.Mm) then
          A(-1,-1)=A(Lm-1,Mm-1)
          A( 0,-1)=A(Lm  ,Mm-1)
          A(-1, 0)=A(Lm-1,Mm  )
          A( 0, 0)=A(Lm  ,Mm  )
        endif
# ifdef MPI
      endif
# endif
#endif
#ifdef MPI
      i=1
      call mpi_exchange_tile (istr,iend,jstr,jend,  A, i)
#endif
      return
      end
 
 
 
