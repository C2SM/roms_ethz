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
      integer i,j,k, isize,jsize,ksize, kshft, itag,jtag, indx,ierr,
     &        req(8), comm(8), status(MPI_STATUS_SIZE),  mess_count 
 
c**
       integer ntl_xi, ntl_eta
!
#include "compute_message_bounds.h"
!


      if (istr.eq.iwest .and. iend.eq.ieast) then
        ntl_xi=1
      else
         ntl_xi=NSUB_X
      endif
      if (jstr.eq.jsouth .and. jend.eq.jnorth) then
        ntl_eta=1
      else
        ntl_eta=NSUB_E
      endif


 
!>      write(*,*) 'mpi_exchange in', mynode
 

      itag=(istr+iend-2*iwest)/(2*(iend-istr+1))   ! two-dimensional
      jtag=(jstr+jend-2*jsouth)/(2*(jend-jstr+1))  ! indices on tile grid.


c      write(*,*) mynode, proc(2), ' itag,jtag =', itag,jtag

c       write(*,*) mynode, jnode, jstr,jend,    ' jtag =', jtag, ntl_eta 



      ksize=4*nmax                    ! message sizes for
      isize=2*ishft*nmax              ! corner messages and sides
      jsize=2*jshft*nmax              ! in XI and ETA directions
 
c       write(*,'(2(6x,A,I2,2x,A,I3,2x,A,I3))')
c    &        'inode=',inode, 'imin=',imin, 'imax=',imax,
c    &        'jnode=',jnode, 'jmin=',jmin, 'jmax=',jmax
#define write !
!                                 Permutation array comm(1:8) keeps 
! Prepare to receive everything:  track which messages are actually 
!-------- -- ------- -----------  being received (hence comm(indx)=0 
! means that no messages is expected the the direction labelled
! "indx", while for active messages "comm" keeps index of the
! corresponding requist "req".  This is needed because later in this
! code array "req" is subject to rearrangement in order to ignore
! directions from which no message is expected, as well as to ignore 
! requests from which messages are already received and unpacked.
!                     
      do i=1,8
        comm(i)=0
      enddo

      if (WEST_INTER .and. itag.eq.0) then
        call MPI_Irecv (recvW, jsize, MPI_DOUBLE_PRECISION,
     &             p_W, 2+8*jtag, ocean_grid_comm, req(1), ierr)
        comm(1)=1
      endif

      if (EAST_INTER .and. itag.eq.ntl_xi-1) then
        call MPI_Irecv (recvE, jsize, MPI_DOUBLE_PRECISION,
     &          p_E, 1+8*jtag, ocean_grid_comm, req(2), ierr)
        comm(2)=2
      endif

      if (SOUTH_INTER .and. jtag.eq.0) then
        call MPI_Irecv (recvS, isize, MPI_DOUBLE_PRECISION,
     &         p_S, 4+8*itag, ocean_grid_comm, req(3), ierr)
        comm(3)=3
      endif

      if (NORTH_INTER .and. jtag.eq.ntl_eta-1) then
        call MPI_Irecv (recvN, isize, MPI_DOUBLE_PRECISION,
     &         p_N, 3+8*itag, ocean_grid_comm, req(4), ierr)
        comm(4)=4
      endif

      if (SOUTH_INTER .and. jtag.eq.0 .and.
     &     WEST_INTER .and. itag.eq.0) then
        call MPI_Irecv (rv_SW, ksize, MPI_DOUBLE_PRECISION,
     &                  p_SW, 6, ocean_grid_comm, req(5), ierr)
        comm(5)=5
      endif

      if (NORTH_INTER .and. jtag.eq.ntl_eta-1 .and.
     &     EAST_INTER .and. itag.eq.ntl_xi-1) then
        call MPI_Irecv (rv_NE, ksize, MPI_DOUBLE_PRECISION,
     &                  p_NE, 5, ocean_grid_comm, req(6), ierr)
        comm(6)=6
      endif

      if (SOUTH_INTER .and. jtag.eq.       0 .and.
     &     EAST_INTER .and. itag.eq.ntl_xi-1) then
        call MPI_Irecv (rv_SE, ksize, MPI_DOUBLE_PRECISION,
     &                  p_SE, 8, ocean_grid_comm, req(7), ierr)
        comm(7)=7
      endif

      if (NORTH_INTER .and. jtag.eq.ntl_eta-1 .and.
     &     WEST_INTER .and. itag.eq.       0) then
        call MPI_Irecv (rv_NW, ksize, MPI_DOUBLE_PRECISION,
     &                  p_NW, 7, ocean_grid_comm, req(8), ierr)
        comm(8)=8
      endif









!
! Send everything
!----- ----------

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

          if (EAST_INTER .and. itag.eq.ntl_xi-1) then
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
                sendS(i-imin       +kshft)=A(i,jsouth  ,k)
                sendS(i-imin+ishft +kshft)=A(i,jsouth+1,k)
              enddo
            enddo

            call MPI_Send  (sendS, isize, MPI_DOUBLE_PRECISION,
     &                 p_S, 3+8*itag, ocean_grid_comm,    ierr)
          endif

          if (NORTH_INTER .and. jtag.eq.ntl_eta-1) then
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

          if (NORTH_INTER .and. jtag.eq.ntl_eta-1 .and.
     &         EAST_INTER .and. itag.eq.ntl_xi-1) then
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
     &         EAST_INTER .and. itag.eq.ntl_xi-1) then
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

          if (NORTH_INTER .and. jtag.eq.ntl_eta-1 .and.
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
! Wait for completion of receive and fill ghost points:
!

      mess_count=0                    ! Compress arrays "comm" and
      do i=1,8                        ! "req" to disregard directions
        if (comm(i).gt.0) then        ! from which no message is
          mess_count=mess_count+1     ! expected. At the end of this
          if (mess_count.lt.i) then   ! segment mess_count is equal
            comm(mess_count)=comm(i)  ! to the actual number of
            req(mess_count)=req(i)    ! messages to be received, 
          endif                       ! arrays comm,req(1:mess_count)
        endif                         ! store directional indices
      enddo                           ! and corresponding requiests.
 
      do while (mess_count.gt.0)
        call MPI_Waitany(mess_count, req, j, status, ierr)

        indx=comm(j)                  ! Save directional index for
        mess_count=mess_count-1       ! message received and ready to
        do i=j,mess_count             ! unpack, then erase its "req"
          req(i)=req(i+1)             ! and "comm" and "req" by 
          comm(i)=comm(i+1)           ! by compressing the arrays, so
        enddo                         ! that the same message will 
                                      ! be unpacked only once.
        if (indx.eq.1) then
          do k=1,nmax
            kshft=2*(k-1)*jshft
            do j=jmin,jmax
              A(iwest-2,j,k)=recvW(j-jmin       +kshft)
              A(iwest-1,j,k)=recvW(j-jmin+jshft +kshft)
            enddo
          enddo
        elseif (indx.eq.2) then
          do k=1,nmax
            kshft=2*(k-1)*jshft
            do j=jmin,jmax
              A(ieast+1,j,k)=recvE(j-jmin       +kshft)
              A(ieast+2,j,k)=recvE(j-jmin+jshft +kshft)
            enddo
          enddo
        elseif (indx.eq.3) then
          do k=1,nmax
            kshft=2*(k-1)*ishft
            do i=imin,imax
              A(i,jsouth-2,k)=recvS(i-imin       +kshft)
              A(i,jsouth-1,k)=recvS(i-imin+ishft +kshft)
            enddo
          enddo
        elseif (indx.eq.4) then 
          do k=1,nmax
            kshft=2*(k-1)*ishft
            do i=imin,imax
              A(i,jnorth+1,k)=recvN(i-imin       +kshft)
              A(i,jnorth+2,k)=recvN(i-imin+ishft +kshft)
            enddo
          enddo
        elseif (indx.eq.5) then
          do k=1,nmax
            A(iwest-2,jsouth-2,k)=rv_SW(k       )
            A(iwest-1,jsouth-2,k)=rv_SW(k+nmax  )
            A(iwest-2,jsouth-1,k)=rv_SW(k+2*nmax)
            A(iwest-1,jsouth-1,k)=rv_SW(k+3*nmax)
          enddo
        elseif (indx.eq.6) then
          do k=1,nmax
            A(ieast+1,jnorth+1,k)=rv_NE(k      )
            A(ieast+2,jnorth+1,k)=rv_NE(k+nmax  )
            A(ieast+1,jnorth+2,k)=rv_NE(k+2*nmax)
            A(ieast+2,jnorth+2,k)=rv_NE(k+3*nmax)
          enddo
        elseif (indx.eq.7) then
          do k=1,nmax
            A(ieast+1,jsouth-2,k)=rv_SE(k   )
            A(ieast+2,jsouth-2,k)=rv_SE(k+nmax  )
            A(ieast+1,jsouth-1,k)=rv_SE(k+2*nmax)
            A(ieast+2,jsouth-1,k)=rv_SE(k+3*nmax)
          enddo
        elseif (indx.eq.8) then
          do k=1,nmax
            A(iwest-2,jnorth+1,k)=rv_NW(k       )
            A(iwest-1,jnorth+1,k)=rv_NW(k+nmax  )
            A(iwest-2,jnorth+2,k)=rv_NW(k+2*nmax)
            A(iwest-1,jnorth+2,k)=rv_NW(k+3*nmax)
          enddo
        endif

      enddo      !<-- while
      return
      end
#else
      subroutine mpi_exchange_empty
      end
#endif
 
