#include "cppdefs.h"
#ifdef MPI
 
      subroutine mpi_exchange_tile (Istr,Iend,Jstr,Jend, A, nmax)
      implicit none
# include "mpif.h"
# include "param.h"
# include "mess_buffers.h"
# include "hidden_mpi_vars.h"
      integer Istr,Iend,Jstr,Jend, nmax
      real A(GLOBAL_2D_ARRAY,nmax)
CSDISTRIBUTE_RESHAPE A(BLOCK_PATTERN) BLOCK_CLAUSE

      integer imin,imax,jmin,jmax, ishft,jshft,kshft, isize,jsize,
     &        itag,jtag,  md_XI,md_ETA,  i,j,k, iter, ierr,
     &        status1(MPI_STATUS_SIZE),  status2(MPI_STATUS_SIZE),
     &        status3(MPI_STATUS_SIZE),  status4(MPI_STATUS_SIZE)
!
! Everything is the same as in "mpi_exchange4.F", except that now
! MPI_SendRecv is used everywhere instead of paired Send-Recv and 
! Recv-Send sequences. 
!
# ifdef EW_PERIODIC
      if (NP_XI.eq.1) then                ! This means that if there
        imin=Istr-2                       ! is no partition in XI-
        imax=Iend+2                       ! direction, then periodic
      else                                ! margins are included into
        imin=Istr                         ! the message; otherwise
        imax=Iend                         ! strip them out.
      endif
# else
      if (WESTERN_EDGE) then               ! Extra point on either
        imin=Istr-1                        ! side to accomodate ghost
      else                                 ! points associated with
        imin=Istr                          ! PHYSICAL boundaries.
      endif
      if (EASTERN_EDGE) then
        imax=Iend+1
      else
        imax=Iend
      endif
# endif
      ishft=imax-imin+1

      if (Jstr.eq.jsouth) then            ! Setting message bounds
# ifdef NS_PERIODIC
        jmin=Jstr-2                       ! for east- and west-bound
# else
        if (jnode.eq.0) then              ! messages differs from the
          jmin=Jstr-1                     ! south-north above because
        else                              ! now messages always
          jmin=Jstr-2                     ! inclide two ghost points
        endif                             ! on either end, which
# endif
      else                                ! contain ghost points of
        jmin=Jstr                         ! north-south computational
      endif                               ! margines.
      if (Jend.eq.jnorth) then
# ifdef NS_PERIODIC
        jmax=Jend+2
# else
        if (jnode.eq.NP_ETA-1) then
          jmax=Jend+1
        else
          jmax=Jend+2
        endif
# endif
      else
        jmax=Jend
      endif
      jshft=jmax-jmin+1

      isize=2*ishft*nmax
      jsize=2*jshft*nmax

      itag=(Istr+Iend)/(2*(Iend-Istr+1))   ! two-dimensional
      jtag=(Jstr+Jend)/(2*(Jend-Jstr+1))   ! indices on tile grid.

c*    write(*,*) mynode, proc(2), ' itag,jtag =', itag,jtag
c*    write(*,'(2(6x,A,I2,2x,A,I3,2x,A,I3))')
c*   &        'inode=',inode, 'imin=',imin, 'imax=',imax,
c*   &        'jnode=',jnode, 'jmin=',jmin, 'jmax=',jmax
!
#define write !

      do iter=0,1
        md_ETA=mod(jnode+iter,2)
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


            call MPI_SendRecv (sendS, isize, MPI_DOUBLE_PRECISION,
     &                         p_S, 1+4*itag,
     &                         recvS, isize, MPI_DOUBLE_PRECISION,
     &                         p_S, 2+4*itag,
     &                             ocean_grid_comm, status1, ierr)

            do k=1,nmax
              kshft=2*(k-1)*ishft
              do i=imin,imax
                A(i,jsouth-2,k)=recvS(i-imin       +kshft)
                A(i,jsouth-1,k)=recvS(i-imin+ishft +kshft)
              enddo
            enddo
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

            call MPI_SendRecv (sendN, isize, MPI_DOUBLE_PRECISION,
     &                         p_N, 2+4*itag,
     &                         recvN, isize, MPI_DOUBLE_PRECISION,
     &                         p_N, 1+4*itag,
     &                             ocean_grid_comm, status2, ierr)
            do k=1,nmax
              kshft=2*(k-1)*ishft
              do i=imin,imax
                A(i,jnorth+1,k)=recvN(i-imin       +kshft)
                A(i,jnorth+2,k)=recvN(i-imin+ishft +kshft)
              enddo
            enddo
          endif
        endif
      enddo  !<-- iter


      do iter=0,1
        md_XI=mod(inode+iter,2)
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

            call MPI_SendRecv (sendW, jsize, MPI_DOUBLE_PRECISION,
     &                         p_W, 3+4*jtag,
     &                         recvW, jsize, MPI_DOUBLE_PRECISION,
     &                         p_W, 4+4*jtag,
     &                             ocean_grid_comm, status3, ierr)

            do k=1,nmax
              kshft=2*(k-1)*jshft
              do j=jmin,jmax
                A(iwest-2,j,k)=recvW(j-jmin       +kshft)
                A(iwest-1,j,k)=recvW(j-jmin+jshft +kshft)
             enddo
            enddo
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

            call MPI_SendRecv (sendE, jsize, MPI_DOUBLE_PRECISION,
     &                         p_E, 4+4*jtag,
     &                         recvE, jsize, MPI_DOUBLE_PRECISION,
     &                         p_E, 3+4*jtag,
     &                             ocean_grid_comm, status4, ierr)
            do k=1,nmax
              kshft=2*(k-1)*jshft
              do j=jmin,jmax
                A(ieast+1,j,k)=recvE(j-jmin       +kshft)
                A(ieast+2,j,k)=recvE(j-jmin+jshft +kshft)
              enddo
            enddo
          endif
        endif
      enddo

!>      write(*,*) 'mpi_exchange out', mynode
      return
      end
#else
      subroutine mpi_exchange_empty
      end
#endif
 
