#include "cppdefs.h"
#ifdef MPI
 
      subroutine mpi_exchange_tile (istr,iend,jstr,jend, A, nmax)

      implicit none
# include "param.h"
# include "hidden_mpi_vars.h"
# include "mpif.h"
      integer istr,iend,jstr,jend, nmax 
      real A(GLOBAL_2D_ARRAY,nmax)
CSDISTRIBUTE_RESHAPE A(BLOCK_PATTERN) BLOCK_CLAUSE
      integer i,j,k, isize,jsize,ksize, kshft, itag,jtag, iter,
     &        req(8), status(MPI_STATUS_SIZE,8), ierr, md_XI,md_ETA
 
      integer sub_X,size_X, sub_E,size_E, size_Z
      parameter (size_Z=4*(N+1),
     &     sub_X=(Lm+NSUB_X-1)/NSUB_X, size_X=2*(N+1)*(4+sub_X),
     &     sub_E=(Mm+NSUB_E-1)/NSUB_E, size_E=2*(N+1)*(4+sub_E))

      real  sn_NW(size_Z),   sendN(0:size_X),   sn_NE(size_Z),
     &      rv_NW(size_Z),   recvN(0:size_X),   rv_NE(size_Z),
     &    sendW(0:size_E),                      sendE(0:size_E),
     &    recvW(0:size_E),                      recvE(0:size_E),
     &      sn_SW(size_Z),   sendS(0:size_X),   sn_SE(size_Z),
     &      rv_SW(size_Z),   recvS(0:size_X),   rv_SE(size_Z)

      common /mpi_buffers/ sn_NW,rv_NW, sendN,recvN, sn_NE,rv_NE,
     &                    sendW,recvW,                sendE,recvE,
     &                     sn_SW,rv_SW, sendS,recvS, sn_SE,rv_SE
C$OMP THREADPRIVATE(/mpi_buffers/)

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
                                ! Split message passing in two steps
                                ! to arrange Send-Recv pairing in 
                                ! such a way that if one subdomain
      do iter=0,1               ! sends message to, say, its neighbor 
       md_XI=mod(inode+iter,2)  ! on the WEST, the neighbor is
       md_ETA=mod(jnode+iter,2) ! preparing to receive this message
                                ! first (i.e. message coming from its
                                ! EAST side), rather than send his
                                ! WEST-bound message as it is done
                                ! by first subdomain.
!
! Prepare to receive and send: sides....
!
        if (md_XI.eq.0) then
          if (WEST_INTER .and. itag.eq.0) then
            write(*,*) 'mpi_exchange: 1.1', mynode
            do k=1,nmax
              kshft=2*(k-1)*jshft
              do j=jmin,jmax
                sendW(j-jmin       +kshft)=A(iwest  ,j,k)
                sendW(j-jmin+jshft +kshft)=A(iwest+1,j,k)
              enddo
            enddo

            call MPI_Irecv (recvW, jsize, MPI_DOUBLE_PRECISION,
     &                 p_W, 2+8*jtag, ocean_grid_comm, req(1), ierr)
            call MPI_Send  (sendW, jsize, MPI_DOUBLE_PRECISION,
     &                 p_W, 1+8*jtag, ocean_grid_comm,         ierr)
          endif
        else
          if (EAST_INTER .and. itag.eq.NSUB_X-1) then
            write(*,*) 'mpi_exchange: 1.2', mynode
            do k=1,nmax
              kshft=2*(k-1)*jshft
              do j=jmin,jmax
                sendE(j-jmin       +kshft)=A(ieast-1,j,k)
                sendE(j-jmin+jshft +kshft)=A(ieast  ,j,k)
              enddo
            enddo
            call MPI_Irecv (recvE, jsize, MPI_DOUBLE_PRECISION,
     &                 p_E, 1+8*jtag, ocean_grid_comm, req(2), ierr)
            call MPI_Send  (sendE, jsize, MPI_DOUBLE_PRECISION,
     &                 p_E, 2+8*jtag, ocean_grid_comm,         ierr)
          endif
        endif
 
        if (md_ETA.eq.0) then
          if (SOUTH_INTER .and. jtag.eq.0) then
            write(*,*) 'mpi_exchange: 1.3', mynode
            do k=1,nmax
              kshft=2*(k-1)*ishft
              do i=imin,imax
                sendS(i-imin       +kshft)=A(i,jsouth  ,k)
                sendS(i-imin+ishft +kshft)=A(i,jsouth+1,k)
              enddo
            enddo
            call MPI_Irecv (recvS, isize, MPI_DOUBLE_PRECISION,
     &                 p_S, 4+8*itag, ocean_grid_comm, req(3), ierr)
            call MPI_Send  (sendS, isize, MPI_DOUBLE_PRECISION,
     &                 p_S, 3+8*itag, ocean_grid_comm,         ierr)
          endif
        else
          if (NORTH_INTER .and. jtag.eq.NSUB_E-1) then
            write(*,*) 'mpi_exchange: 1.4', mynode
            do k=1,nmax
              kshft=2*(k-1)*ishft
              do i=imin,imax
                sendN(i-imin       +kshft)=A(i,jnorth-1,k)
                sendN(i-imin+ishft +kshft)=A(i,jnorth  ,k)
              enddo
            enddo
            call MPI_Irecv (recvN, isize, MPI_DOUBLE_PRECISION,
     &               p_N, 3+8*itag, ocean_grid_comm, req(4), ierr)
            call MPI_Send  (sendN, isize, MPI_DOUBLE_PRECISION,
     &               p_N, 4+8*itag, ocean_grid_comm,         ierr)
          endif
        endif
!
! ...corners:
!
        if (md_XI.eq.0) then
          if (SOUTH_INTER .and. jtag.eq.0 .and.
     &         WEST_INTER .and. itag.eq.0) then
            write(*,*) 'mpi_exchange: 1.5',mynode
            do k=1,nmax
              sn_SW(k       )=A(iwest  ,jsouth  ,k)
              sn_SW(k+nmax  )=A(iwest+1,jsouth  ,k)
              sn_SW(k+2*nmax)=A(iwest  ,jsouth+1,k)
              sn_SW(k+3*nmax)=A(iwest+1,jsouth+1,k)
            enddo
            call MPI_Irecv (rv_SW, ksize, MPI_DOUBLE_PRECISION,
     &                      p_SW, 6, ocean_grid_comm, req(5), ierr)
            call MPI_Send  (sn_SW, ksize, MPI_DOUBLE_PRECISION,
     &                      p_SW, 5, ocean_grid_comm,         ierr)
          endif
        else
          if (NORTH_INTER .and. jtag.eq.NSUB_E-1 .and.
     &         EAST_INTER .and. itag.eq.NSUB_X-1) then
            write(*,*) 'mpi_exchange: 1.6', mynode
            do k=1,nmax
              sn_NE(k       )=A(ieast-1,jnorth-1,k)
              sn_NE(k+nmax  )=A(ieast  ,jnorth-1,k)
              sn_NE(k+2*nmax)=A(ieast-1,jnorth  ,k)
              sn_NE(k+3*nmax)=A(ieast  ,jnorth  ,k)
            enddo
            call MPI_Irecv (rv_NE, ksize, MPI_DOUBLE_PRECISION,
     &                      p_NE, 5, ocean_grid_comm, req(6), ierr)
            call MPI_Send  (sn_NE, ksize, MPI_DOUBLE_PRECISION,
     &                      p_NE, 6, ocean_grid_comm,         ierr)
          endif
        endif
 
        if (md_XI.eq.1) then
          if (SOUTH_INTER .and. jtag.eq.       0 .and.
     &         EAST_INTER .and. itag.eq.NSUB_X-1) then
            write(*,*) 'mpi_exchange: 1.7', mynode
            do k=1,nmax
              sn_SE(k       )=A(ieast-1,jsouth  ,k)
              sn_SE(k+nmax  )=A(ieast  ,jsouth  ,k)
              sn_SE(k+2*nmax)=A(ieast-1,jsouth+1,k)
              sn_SE(k+3*nmax)=A(ieast  ,jsouth+1,k)
            enddo
            call MPI_Irecv (rv_SE, ksize, MPI_DOUBLE_PRECISION,
     &                      p_SE, 8, ocean_grid_comm, req(7), ierr)
            call MPI_Send  (sn_SE, ksize, MPI_DOUBLE_PRECISION,
     &                      p_SE, 7, ocean_grid_comm,         ierr)
          endif
        else
          if (NORTH_INTER .and. jtag.eq.NSUB_E-1 .and.
     &         WEST_INTER .and. itag.eq.       0) then
            write(*,*) 'mpi_exchange: 1.8', mynode
            do k=1,nmax
              sn_NW(k       )=A(iwest  ,jnorth-1,k)
              sn_NW(k+nmax  )=A(iwest+1,jnorth-1,k)
              sn_NW(k+2*nmax)=A(iwest  ,jnorth  ,k)
              sn_NW(k+3*nmax)=A(iwest+1,jnorth  ,k)
            enddo
            call MPI_Irecv (rv_NW, ksize, MPI_DOUBLE_PRECISION,
     &                      p_NW, 7, ocean_grid_comm, req(8), ierr)
            call MPI_Send  (sn_NW, ksize, MPI_DOUBLE_PRECISION,
     &                      p_NW, 8, ocean_grid_comm,         ierr)
          endif
        endif
      enddo   !<-- iter
 
 
!
! Wait for completion of receive and fill ghost points: sides...
!
 
      if (WEST_INTER .and. itag.eq.0) then
        write(*,*) 'mpi_exchange: 2.1', mynode
        call MPI_Wait (req(1),status(1,1),ierr)
        do k=1,nmax
          kshft=2*(k-1)*jshft
          do j=jmin,jmax
            A(iwest-2,j,k)=recvW(j-jmin       +kshft)
            A(iwest-1,j,k)=recvW(j-jmin+jshft +kshft)
          enddo
        enddo
      endif
 
      if (EAST_INTER .and. itag.eq.NSUB_X-1) then
        write(*,*) 'mpi_exchange: 2.2', mynode
        call MPI_Wait (req(2),status(1,2),ierr)
        do k=1,nmax
          kshft=2*(k-1)*jshft
          do j=jmin,jmax
            A(ieast+1,j,k)=recvE(j-jmin       +kshft)
            A(ieast+2,j,k)=recvE(j-jmin+jshft +kshft)
          enddo
        enddo
      endif
 
      if (SOUTH_INTER .and. jtag.eq.0) then
        write(*,*) 'mpi_exchange: 2.3', mynode
        call MPI_Wait (req(3),status(1,3),ierr)
        do k=1,nmax
          kshft=2*(k-1)*ishft
          do i=imin,imax
            A(i,jsouth-2,k)=recvS(i-imin       +kshft)
            A(i,jsouth-1,k)=recvS(i-imin+ishft +kshft)
          enddo
        enddo
      endif
 
      if (NORTH_INTER .and. jtag.eq.NSUB_E-1) then
        write(*,*) 'mpi_exchange: 2.4', mynode
        call MPI_Wait (req(4),status(1,4),ierr)
        do k=1,nmax
          kshft=2*(k-1)*ishft
          do i=imin,imax
            A(i,jnorth+1,k)=recvN(i-imin       +kshft)
            A(i,jnorth+2,k)=recvN(i-imin+ishft +kshft)
          enddo
        enddo
      endif
!
! ...corners:
!
      if (SOUTH_INTER .and. jtag.eq.0 .and.
     &     WEST_INTER .and. itag.eq.0) then
        write(*,*) 'mpi_exchange: 2.5', mynode
        call MPI_Wait (req(5),status(1,5),ierr)
        do k=1,nmax
          A(iwest-2,jsouth-2,k)=rv_SW(k       )
          A(iwest-1,jsouth-2,k)=rv_SW(k+nmax  )
          A(iwest-2,jsouth-1,k)=rv_SW(k+2*nmax)
          A(iwest-1,jsouth-1,k)=rv_SW(k+3*nmax)
        enddo
      endif
 
      if (NORTH_INTER .and. jtag.eq.NSUB_E-1 .and.
     &     EAST_INTER .and. itag.eq.NSUB_X-1) then
        write(*,*) 'mpi_exchange: 2.6', mynode
        call MPI_Wait (req(6),status(1,6),ierr)
        do k=1,nmax
          A(ieast+1,jnorth+1,k)=rv_NE(k      )
          A(ieast+2,jnorth+1,k)=rv_NE(k+nmax  )
          A(ieast+1,jnorth+2,k)=rv_NE(k+2*nmax)
          A(ieast+2,jnorth+2,k)=rv_NE(k+3*nmax)
        enddo
      endif
 
      if (SOUTH_INTER .and. jtag.eq.       0 .and.
     &     EAST_INTER .and. itag.eq.NSUB_X-1) then
        write(*,*) 'mpi_exchange: 2.7', mynode
        call MPI_Wait (req(7),status(1,7),ierr)
        do k=1,nmax
          A(ieast+1,jsouth-2,k)=rv_SE(k   )
          A(ieast+2,jsouth-2,k)=rv_SE(k+nmax  )
          A(ieast+1,jsouth-1,k)=rv_SE(k+2*nmax)
          A(ieast+2,jsouth-1,k)=rv_SE(k+3*nmax)
        enddo
      endif
 
      if (NORTH_INTER .and. jtag.eq.NSUB_E-1 .and.
     &     WEST_INTER .and. itag.eq.       0) then
        write(*,*) 'mpi_exchange: 2.8', mynode
        call MPI_Wait (req(8),status(1,8),ierr)
        do k=1,nmax
          A(iwest-2,jnorth+1,k)=rv_NW(k       )
          A(iwest-1,jnorth+1,k)=rv_NW(k+nmax  )
          A(iwest-2,jnorth+2,k)=rv_NW(k+2*nmax)
          A(iwest-1,jnorth+2,k)=rv_NW(k+3*nmax)
        enddo
      endif
      write(*,*) 'mpi_exchange:    ', mynode,' exit'
 
!>      write(*,*) 'mpi_exchange out', mynode
      return
      end
#else
      subroutine mpi_exchange_empty
      end
#endif
 
