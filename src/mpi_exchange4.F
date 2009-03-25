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

      integer imin,imax,jmin,jmax, ishft,jshft,kshft, isize,jsize,
     &        itag,jtag,  md_XI,md_ETA,  i,j,k, iter, ierr,
     &        status1(MPI_STATUS_SIZE),  status2(MPI_STATUS_SIZE),
     &        status3(MPI_STATUS_SIZE),  status4(MPI_STATUS_SIZE)
!
! This version of mpi_exchange uses two-stage algorithm which first
! updates north and south computational margins of the subdomain
! by sending messages which contain only internal points and physical
! boundaries computed by the sending node.  After completion of this
! stage, it sends east- and west-bound messages containing not only
! internal and physical boundary points, but also the two ghost
! points on each end which belong to north and south computational
! margins filled-in during the first stage.  As the result, there is
! no need to send separate messages in the diagonal direction to fill
! corners, however, corner data travels in two stages, so that the
! second stage must start only after the first is complete.
!
! During the both north-south and east-west stages the messages are
! paired in Send-Recv and Recv-Send arrangements to avoid head-on
! collisions (a principle known as "MPI deadlock safety" --- in
! principle sends and receives can be logically synchronous and
! buffer-less in this code).
!
# ifdef EW_PERIODIC
      if (NP_XI.eq.1) then                ! this means that if there
        imin=istr-2                       ! is no partition in XI-
        imax=iend+2                       ! direction, then periodic
      else                                ! margins are included into
        imin=istr                         ! the message; otherwise
        imax=iend                         ! strip them out.
      endif
# else
      if (WESTERN_EDGE) then              ! include extra point on
        imin=istr-1                       ! either side to accomodate
      else                                ! ghost points associated
        imin=istr                         ! with PHYSICAL boundaries
      endif
      if (EASTERN_EDGE) then
        imax=iend+1
      else
        imax=iend
      endif
# endif
      ishft=imax-imin+1

      if (jstr.eq.jsouth) then            ! Setting message bounds
# ifdef NS_PERIODIC
        jmin=jstr-2                       ! for east- and west-bound
# else
        if (jnode.eq.0) then              ! messages differs from the
          jmin=jstr-1                     ! south-north above because
        else                              ! now messages always
          jmin=jstr-2                     ! inclide two ghost points
        endif                             ! on either end, which
# endif
      else                                ! contain ghost points of
        jmin=jstr                         ! north-south computational
      endif                               ! margines.
      if (jend.eq.jnorth) then
# ifdef NS_PERIODIC
        jmax=jend+2
# else
        if (jnode.eq.NP_ETA-1) then
          jmax=jend+1
        else
          jmax=jend+2
        endif
# endif
      else
        jmax=jend
      endif
      jshft=jmax-jmin+1

      isize=2*ishft*nmax
      jsize=2*jshft*nmax
 
      itag=(istr+iend)/(2*(iend-istr+1))   ! two-dimensional
      jtag=(jstr+jend)/(2*(jend-jstr+1))   ! indices on tile grid.

c*    write(*,*) mynode, proc(2), ' itag,jtag =', itag,jtag
c*    write(*,'(2(6x,A,I2,2x,A,I3,2x,A,I3))')
c*   &        'inode=',inode, 'imin=',imin, 'imax=',imax,
c*   &        'jnode=',jnode, 'jmin=',jmin, 'jmax=',jmax
!
! In the code below iter-loop is to arrange Send-Recv pairing in
! such a way that if one subdomain sends message to his neighbor
! (say on the on the south), the neighbor is receiving this message
! first (i.e. message coming from his north), rather than than send
! his south-bound message, as it is done by first subdomain.  Note
! that even-numbered jnodes use Send-Recv sequence, while the order
! is reversed for odd-numbered jnodes.    As the result, at any
! north-south contact between two MPI nodes, the south-bound message
! is transmitted and received first; the north-bound follows.
! Similar pairing takes place on east-west sides.
! 
! In addition to that, both north-south and east-west comminications
! are directionally alternated to reduce interference on dual-CPU
! nodes.  This is done via inner "md_XI.eq.0" and "md_ETA.eq.0"
! control logic and the rationale is as follows: suppose a pair of
! MPI processes working on subdomains adjacent in ETA-direction
! belong to the same dual-CPU hardware node and share the same
! communication device.  Then sages are arranged in such a way, that
! when one MPI process from the pair sends, the other one receives
! messages to/from members of a similar pair residing on neigboring
! hardware node.  The polarity reverses during the second stage
! "iter", once "md_ETA" changes its parity.  This resilts in balanced
! full-duplex communication between the hardware nodes during both
! "iter" stages.  Similar arrangement takes place for north-south
! communication if subdomains residing on the same hardware node are
! adjacent in XI-direction.  This optimization has neutral effect
! (no advantage, no penalty) for single-CPU nodes.
!
! Use synchroneous (Ssend) version of MPI Send, it makes it faster.
!
#define MPI_Send MPI_Ssend

      do iter=0,1
        md_XI=mod(inode+iter,2)
        md_ETA=mod(jnode+iter,2)
        if (md_ETA.eq.0) then
          if (SOUTH_INTER .and. jtag.eq.0) then
            do k=1,nmax
              kshft=2*(k-1)*ishft
              do i=imin,imax
                sendS(i-imin       +kshft)=A(i,jsouth  ,k)
                sendS(i-imin+ishft +kshft)=A(i,jsouth+1,k)
              enddo
            enddo

            if (md_XI.eq.0) then
              call MPI_Send (sendS, isize, MPI_DOUBLE_PRECISION, p_S,
     &                       1+4*itag, ocean_grid_comm,         ierr)

              call MPI_Recv (recvS, isize, MPI_DOUBLE_PRECISION, p_S,
     &                       2+4*itag, ocean_grid_comm,status1, ierr)
            else
              call MPI_Recv (recvS, isize, MPI_DOUBLE_PRECISION, p_S,
     &                       2+4*itag, ocean_grid_comm,status1, ierr)

              call MPI_Send (sendS, isize, MPI_DOUBLE_PRECISION, p_S,
     &                       1+4*itag, ocean_grid_comm,         ierr)
            endif

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
            do k=1,nmax
              kshft=2*(k-1)*ishft
              do i=imin,imax
                sendN(i-imin       +kshft)=A(i,jnorth-1,k)
                sendN(i-imin+ishft +kshft)=A(i,jnorth  ,k)
              enddo
            enddo

            if (md_XI.eq.0) then
              call MPI_Recv (recvN, isize, MPI_DOUBLE_PRECISION, p_N,
     &                       1+4*itag, ocean_grid_comm,status2, ierr)

              call MPI_Send (sendN, isize, MPI_DOUBLE_PRECISION, p_N,
     &                       2+4*itag, ocean_grid_comm,         ierr)
            else
              call MPI_Send (sendN, isize, MPI_DOUBLE_PRECISION, p_N,
     &                       2+4*itag, ocean_grid_comm,         ierr)

              call MPI_Recv (recvN, isize, MPI_DOUBLE_PRECISION, p_N,
     &                       1+4*itag, ocean_grid_comm,status2, ierr)
            endif

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
        md_ETA=mod(jnode+iter,2)
        if (md_XI.eq.0) then
          if (WEST_INTER .and. itag.eq.0) then
            do k=1,nmax
              kshft=2*(k-1)*jshft
              do j=jmin,jmax
                sendW(j-jmin       +kshft)=A(iwest  ,j,k)
                sendW(j-jmin+jshft +kshft)=A(iwest+1,j,k)
              enddo
            enddo

            if (md_ETA.eq.0) then
              call MPI_Send (sendW, jsize, MPI_DOUBLE_PRECISION, p_W,
     &                       3+4*jtag, ocean_grid_comm,         ierr)

              call MPI_Recv (recvW, jsize, MPI_DOUBLE_PRECISION, p_W,
     &                       4+4*jtag, ocean_grid_comm,status3, ierr)
            else
              call MPI_Recv (recvW, jsize, MPI_DOUBLE_PRECISION, p_W,
     &                       4+4*jtag, ocean_grid_comm,status3, ierr)

              call MPI_Send (sendW, jsize, MPI_DOUBLE_PRECISION, p_W,
     &                       3+4*jtag, ocean_grid_comm,         ierr)
            endif

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
            do k=1,nmax
              kshft=2*(k-1)*jshft
              do j=jmin,jmax
                sendE(j-jmin       +kshft)=A(ieast-1,j,k)
                sendE(j-jmin+jshft +kshft)=A(ieast  ,j,k)
              enddo
            enddo

            if (md_ETA.eq.0) then
              call MPI_Recv (recvE, jsize, MPI_DOUBLE_PRECISION, p_E,
     &                       3+4*jtag, ocean_grid_comm,status4, ierr)

              call MPI_Send (sendE, jsize, MPI_DOUBLE_PRECISION, p_E,
     &                       4+4*jtag, ocean_grid_comm,         ierr)
            else
              call MPI_Send (sendE, jsize, MPI_DOUBLE_PRECISION, p_E,
     &                       4+4*jtag, ocean_grid_comm,         ierr)

              call MPI_Recv (recvE, jsize, MPI_DOUBLE_PRECISION, p_E,
     &                       3+4*jtag, ocean_grid_comm,status4, ierr)
            endif

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

      return
      end
#else
      subroutine mpi_exchange_empty
      end
#endif
