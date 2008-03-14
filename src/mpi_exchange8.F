#include "cppdefs.h"
#ifdef MPI
 
      subroutine mpi_exchange_tile (istr,iend,jstr,jend, A, nmax)
      implicit none
# include "mpif.h"
# include "param.h"
# include "mess_buffers.h"
# include "hidden_mpi_vars.h"
      integer istr,iend,jstr,jend, nmax 
      real A(GLOBAL_2D_ARRAY,nmax)
CSDISTRIBUTE_RESHAPE A(BLOCK_PATTERN) BLOCK_CLAUSE
      integer i,j,k, isize,jsize,ksize, kshft, itag,jtag, ierr,
     &        req(8), comp(8), status(MPI_STATUS_SIZE,8), mess_count
 
!
#include "compute_message_bounds.h"
!
 
!>      write(*,*) 'mpi_exchange in', mynode
 
 
      itag=(istr+iend)/(2*(iend-istr+1))   ! two-dimensional
      jtag=(jstr+jend)/(2*(jend-jstr+1))   ! indices on tile grid.

c      write(*,*) mynode, proc(2), ' itag,jtag =', itag,jtag

      ksize=4*nmax                    ! message sizes for
      isize=2*ishft*nmax              ! corner messages and sides
      jsize=2*jshft*nmax              ! in XI and ETA directions
 
c*      write(*,'(2(6x,A,I2,2x,A,I3,2x,A,I3))')
c*     &        'inode=',inode, 'imin=',imin, 'imax=',imax,
c*     &        'jnode=',jnode, 'jmin=',jmin, 'jmax=',jmax
#define write !
!
! Prepare to receive everything
!
         mess_count=0
         do i=1,8
           comp(i)=0
         enddo

          if (WEST_INTER .and. itag.eq.0) then
            call MPI_Irecv (recvW, jsize, MPI_DOUBLE_PRECISION,
     &                 p_W, 2+8*jtag, ocean_grid_comm, req(1), ierr)
            mess_count=mess_count+1
          endif

          if (EAST_INTER .and. itag.eq.NSUB_X-1) then
            call MPI_Irecv (recvE, jsize, MPI_DOUBLE_PRECISION,
     &                 p_E, 1+8*jtag, ocean_grid_comm, req(2), ierr)
            mess_count=mess_count+1
          endif

          if (SOUTH_INTER .and. jtag.eq.0) then
            call MPI_Irecv (recvS, isize, MPI_DOUBLE_PRECISION,
     &                 p_S, 4+8*itag, ocean_grid_comm, req(3), ierr)
            mess_count=mess_count+1
          endif

          if (NORTH_INTER .and. jtag.eq.NSUB_E-1) then
            call MPI_Irecv (recvN, isize, MPI_DOUBLE_PRECISION,
     &                 p_N, 3+8*itag, ocean_grid_comm, req(4), ierr)
            mess_count=mess_count+1
          endif

          if (SOUTH_INTER .and. jtag.eq.0 .and.
     &         WEST_INTER .and. itag.eq.0) then
            call MPI_Irecv (rv_SW, ksize, MPI_DOUBLE_PRECISION,
     &                      p_SW, 6, ocean_grid_comm, req(5), ierr)
            mess_count=mess_count+1
          endif

          if (NORTH_INTER .and. jtag.eq.NSUB_E-1 .and.
     &         EAST_INTER .and. itag.eq.NSUB_X-1) then
            call MPI_Irecv (rv_NE, ksize, MPI_DOUBLE_PRECISION,
     &                      p_NE, 5, ocean_grid_comm, req(6), ierr)
            mess_count=mess_count+1
          endif

          if (SOUTH_INTER .and. jtag.eq.       0 .and.
     &         EAST_INTER .and. itag.eq.NSUB_X-1) then
            call MPI_Irecv (rv_SE, ksize, MPI_DOUBLE_PRECISION,
     &                      p_SE, 8, ocean_grid_comm, req(7), ierr)
            mess_count=mess_count+1
          endif

          if (NORTH_INTER .and. jtag.eq.NSUB_E-1 .and.
     &         WEST_INTER .and. itag.eq.       0) then
            call MPI_Irecv (rv_NW, ksize, MPI_DOUBLE_PRECISION,
     &                      p_NW, 7, ocean_grid_comm, req(8), ierr)
            mess_count=mess_count+1
          endif









!
! Send everything
!

























          if (WEST_INTER .and. itag.eq.0) then
            write(*,*) 'mpi_exchange: 1.1', mynode
            do k=1,nmax
              kshft=2*(k-1)*jshft
              do j=jmin,jmax
                sendW(j-jmin       +kshft)=A(iwest  ,j,k)
                sendW(j-jmin+jshft +kshft)=A(iwest+1,j,k)
              enddo
            enddo

            call MPI_Send  (sendW, jsize, MPI_DOUBLE_PRECISION,
     &                 p_W, 1+8*jtag, ocean_grid_comm,   ierr)
          endif

          if (EAST_INTER .and. itag.eq.NSUB_X-1) then
            write(*,*) 'mpi_exchange: 1.2', mynode
            do k=1,nmax
              kshft=2*(k-1)*jshft
              do j=jmin,jmax
                sendE(j-jmin       +kshft)=A(ieast-1,j,k)
                sendE(j-jmin+jshft +kshft)=A(ieast  ,j,k)
              enddo
            enddo

            call MPI_Send  (sendE, jsize, MPI_DOUBLE_PRECISION,
     &                 p_E, 2+8*jtag, ocean_grid_comm,    ierr)
          endif

 

          if (SOUTH_INTER .and. jtag.eq.0) then
            write(*,*) 'mpi_exchange: 1.3', mynode
            do k=1,nmax
              kshft=2*(k-1)*ishft
              do i=imin,imax
                sendS(i-imin       +kshft)=A(i,1,k)
                sendS(i-imin+ishft +kshft)=A(i,2,k)
              enddo
            enddo

            call MPI_Send  (sendS, isize, MPI_DOUBLE_PRECISION,
     &                 p_S, 3+8*itag, ocean_grid_comm,    ierr)
          endif

          if (NORTH_INTER .and. jtag.eq.NSUB_E-1) then
            write(*,*) 'mpi_exchange: 1.4', mynode
            do k=1,nmax
              kshft=2*(k-1)*ishft
              do i=imin,imax
                sendN(i-imin       +kshft)=A(i,jnorth-1,k)
                sendN(i-imin+ishft +kshft)=A(i,jnorth  ,k)
              enddo
            enddo

            call MPI_Send  (sendN, isize, MPI_DOUBLE_PRECISION,
     &                 p_N, 4+8*itag, ocean_grid_comm,     ierr)
          endif

!
! ...corners:
!

          if (SOUTH_INTER .and. jtag.eq.0 .and.
     &         WEST_INTER .and. itag.eq.0) then
            write(*,*) 'mpi_exchange: 1.5',mynode
            do k=1,nmax
              sn_SW(k       )=A(iwest  ,jsouth  ,k)
              sn_SW(k+nmax  )=A(iwest+1,jsouth  ,k)
              sn_SW(k+2*nmax)=A(iwest  ,jsouth+1,k)
              sn_SW(k+3*nmax)=A(iwest+1,jsouth+1,k)
            enddo

            call MPI_Send  (sn_SW, ksize, MPI_DOUBLE_PRECISION,
     &                      p_SW, 5, ocean_grid_comm,    ierr)
          endif

          if (NORTH_INTER .and. jtag.eq.NSUB_E-1 .and.
     &         EAST_INTER .and. itag.eq.NSUB_X-1) then
            write(*,*) 'mpi_exchange: 1.6', mynode
            do k=1,nmax
              sn_NE(k       )=A(ieast-1,jnorth-1,k)
              sn_NE(k+nmax  )=A(ieast  ,jnorth-1,k)
              sn_NE(k+2*nmax)=A(ieast-1,jnorth  ,k)
              sn_NE(k+3*nmax)=A(ieast  ,jnorth  ,k)
            enddo

            call MPI_Send  (sn_NE, ksize, MPI_DOUBLE_PRECISION,
     &                      p_NE, 6, ocean_grid_comm,   ierr)
          endif

 

          if (SOUTH_INTER .and. jtag.eq.       0 .and.
     &         EAST_INTER .and. itag.eq.NSUB_X-1) then
            write(*,*) 'mpi_exchange: 1.7', mynode
            do k=1,nmax
              sn_SE(k       )=A(ieast-1,jsouth  ,k)
              sn_SE(k+nmax  )=A(ieast  ,jsouth  ,k)
              sn_SE(k+2*nmax)=A(ieast-1,jsouth+1,k)
              sn_SE(k+3*nmax)=A(ieast  ,jsouth+1,k)
            enddo

            call MPI_Send  (sn_SE, ksize, MPI_DOUBLE_PRECISION,
     &                      p_SE, 7, ocean_grid_comm,     ierr)
          endif

          if (NORTH_INTER .and. jtag.eq.NSUB_E-1 .and.
     &         WEST_INTER .and. itag.eq.       0) then
            write(*,*) 'mpi_exchange: 1.8', mynode
            do k=1,nmax
              sn_NW(k       )=A(iwest  ,jnorth-1,k)
              sn_NW(k+nmax  )=A(iwest+1,jnorth-1,k)
              sn_NW(k+2*nmax)=A(iwest  ,jnorth  ,k)
              sn_NW(k+3*nmax)=A(iwest+1,jnorth  ,k)
            enddo

            call MPI_Send  (sn_NW, ksize, MPI_DOUBLE_PRECISION,
     &                      p_NW, 8, ocean_grid_comm,   ierr)
          endif


 
 
!
! Wait for completion of receive and fill ghost points: sides...
!
 
      do while (mess_count.gt.0)
        if (WEST_INTER .and. itag.eq.0) then
          if (comp(1).eq.0) then
            call MPI_Test (req(1), comp(1), status(1,1), ierr)
            if (comp(1).ne.0) then
              mess_count=mess_count-1
              do k=1,nmax
                kshft=2*(k-1)*jshft
                do j=jmin,jmax
                  A(iwest-2,j,k)=recvW(j-jmin       +kshft)
                  A(iwest-1,j,k)=recvW(j-jmin+jshft +kshft)
                enddo
              enddo
            endif
          endif
        endif
 
        if (EAST_INTER .and. itag.eq.NSUB_X-1) then
          if (comp(2).eq.0) then
            call MPI_Test (req(2), comp(2), status(1,2), ierr)
            if (comp(2).ne.0) then
              mess_count=mess_count-1
              do k=1,nmax
                kshft=2*(k-1)*jshft
                do j=jmin,jmax
                  A(ieast+1,j,k)=recvE(j-jmin       +kshft)
                  A(ieast+2,j,k)=recvE(j-jmin+jshft +kshft)
                enddo
              enddo
            endif
          endif
        endif
 
        if (SOUTH_INTER .and. jtag.eq.0) then
          if (comp(3).eq.0) then
            call MPI_Test (req(3), comp(3), status(1,3), ierr)
            if (comp(3).ne.0) then
              mess_count=mess_count-1
              do k=1,nmax
                kshft=2*(k-1)*ishft
                do i=imin,imax
                  A(i,jsouth-2,k)=recvS(i-imin       +kshft)
                  A(i,jsouth-1,k)=recvS(i-imin+ishft +kshft)
                enddo
              enddo
            endif
          endif
        endif
 
        if (NORTH_INTER .and. jtag.eq.NSUB_E-1) then
          if (comp(4).eq.0) then
            call MPI_Test (req(4), comp(4), status(1,4), ierr)
            if (comp(4).ne.0) then
              mess_count=mess_count-1
              do k=1,nmax
                kshft=2*(k-1)*ishft
                do i=imin,imax
                  A(i,jnorth+1,k)=recvN(i-imin       +kshft)
                  A(i,jnorth+2,k)=recvN(i-imin+ishft +kshft)
                enddo
              enddo
            endif
          endif
        endif
!
! ...corners:
!
        if (SOUTH_INTER .and. jtag.eq.0 .and.
     &       WEST_INTER .and. itag.eq.0) then
          if (comp(5).eq.0) then
            call MPI_Test (req(5), comp(5), status(1,5), ierr)
            if (comp(5).ne.0) then
              mess_count=mess_count-1
              do k=1,nmax
                A(iwest-2,jsouth-2,k)=rv_SW(k       )
                A(iwest-1,jsouth-2,k)=rv_SW(k+nmax  )
                A(iwest-2,jsouth-1,k)=rv_SW(k+2*nmax)
                A(iwest-1,jsouth-1,k)=rv_SW(k+3*nmax)
              enddo
            endif
          endif
        endif
 
        if (NORTH_INTER .and. jtag.eq.NSUB_E-1 .and.
     &       EAST_INTER .and. itag.eq.NSUB_X-1) then
          if (comp(6).eq.0) then
            call MPI_Test (req(6), comp(6), status(1,6), ierr)
            if (comp(6).ne.0) then
              mess_count=mess_count-1
              do k=1,nmax
                A(ieast+1,jnorth+1,k)=rv_NE(k      )
                A(ieast+2,jnorth+1,k)=rv_NE(k+nmax  )
                A(ieast+1,jnorth+2,k)=rv_NE(k+2*nmax)
                A(ieast+2,jnorth+2,k)=rv_NE(k+3*nmax)
              enddo
            endif
          endif
        endif
 
        if (SOUTH_INTER .and. jtag.eq.       0 .and.
     &       EAST_INTER .and. itag.eq.NSUB_X-1) then
          if (comp(7).eq.0) then
            call MPI_Test (req(7), comp(7), status(1,7), ierr)
            if (comp(7).ne.0) then
              mess_count=mess_count-1
              do k=1,nmax
                A(ieast+1,jsouth-2,k)=rv_SE(k   )
                A(ieast+2,jsouth-2,k)=rv_SE(k+nmax  )
                A(ieast+1,jsouth-1,k)=rv_SE(k+2*nmax)
                A(ieast+2,jsouth-1,k)=rv_SE(k+3*nmax)
              enddo
            endif
          endif
        endif
 
        if (NORTH_INTER .and. jtag.eq.NSUB_E-1 .and.
     &       WEST_INTER .and. itag.eq.       0) then
          if (comp(8).eq.0) then
            call MPI_Test (req(8), comp(8), status(1,8), ierr)
            if (comp(8).ne.0) then
              mess_count=mess_count-1
              do k=1,nmax
                A(iwest-2,jnorth+1,k)=rv_NW(k       )
                A(iwest-1,jnorth+1,k)=rv_NW(k+nmax  )
                A(iwest-2,jnorth+2,k)=rv_NW(k+2*nmax)
                A(iwest-1,jnorth+2,k)=rv_NW(k+3*nmax)
              enddo
            endif
          endif
        endif
      enddo      !<-- while
      return
      end
#else
      subroutine mpi_exchange_empty
      end
#endif
 
