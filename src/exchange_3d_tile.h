      subroutine exchange_3d_tile (istr,iend,jstr,jend, A)
!
! Set periodic boundary conditions (if any) for a three-dimensional
! field A of RHO-, U-, V- or PSI-type. This file is designed to
! generate five different subroutines, by redefining (via CPP) the
! name of the subroutine exchange_2d_tile above and the starting
! indices ISTART = [istr for U-,PSI-type; istrR for V-,RHO-type]
! and JSTART = [jstr for V-,PSI-type; jstrR for U-,RHO-type] below,
! as well as macro KSTART for the vertical RHO- and W-types. See
! also mounting file exchange.F
!
      implicit none
#include "param.h"
#include "scalars.h"
      real A(GLOBAL_2D_ARRAY,KSTART:N)
CSDISTRIBUTE_RESHAPE A(BLOCK_PATTERN,*) BLOCK_CLAUSE
      integer istr,iend,jstr,jend, i,j,k
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
          do k=KSTART,N
            do j=J_RANGE
              A(Lm+1,j,k)=A(1,j,k)
              A(Lm+2,j,k)=A(2,j,k)
            enddo
          enddo
        endif
        if (iend.eq.Lm) then
          do k=KSTART,N
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
#  define I_RANGE ISTART,iendR
# endif
# ifdef MPI
      if (NP_ETA.eq.1) then
# endif
        if (jstr.eq.1) then
          do k=KSTART,N
            do i=I_RANGE
              A(i,Mm+1,k)=A(i,1,k)
              A(i,Mm+2,k)=A(i,2,k)
            enddo
          enddo
        endif
        if (jend.eq.Mm) then
          do k=KSTART,N
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
        if (istr.eq.1 .and. jstr.eq.1) then
          do k=KSTART,N
            A(Lm+1,Mm+1,k)=A(1,1,k)
            A(Lm+1,Mm+2,k)=A(1,2,k)
            A(Lm+2,Mm+1,k)=A(2,1,k)
            A(Lm+2,Mm+2,k)=A(2,2,k)
          enddo
        endif
        if (iend.eq.Lm .and. jstr.eq.1) then
          do k=KSTART,N
            A(-1,Mm+1,k)=A(Lm-1,1,k)
            A( 0,Mm+1,k)=A(Lm  ,1,k)
            A(-1,Mm+2,k)=A(Lm-1,2,k)
            A( 0,Mm+2,k)=A(Lm  ,2,k)
          enddo
        endif
        if (istr.eq.1 .and. jend.eq.Mm) then
          do k=KSTART,N
            A(Lm+1,-1,k)=A(1,Mm-1,k)
            A(Lm+1, 0,k)=A(1,Mm  ,k)
            A(Lm+2,-1,k)=A(2,Mm-1,k)
            A(Lm+2, 0,k)=A(2,Mm  ,k)
          enddo
        endif
        if (iend.eq.Lm .and. jend.eq.Mm) then
          do k=KSTART,N
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
      k=N-KSTART+1
      call mpi_exchange8_tile (istr,iend,jstr,jend,  A,k)
#endif
      return
      end
 
