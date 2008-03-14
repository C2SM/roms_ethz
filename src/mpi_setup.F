#include "cppdefs.h"
#ifdef MPI

C$    subroutine master_num_threads (nthrds)
C$    implicit none
C$    integer nthrds, trd, omp_get_num_threads, omp_get_thread_num
C$    trd=omp_get_thread_num()
C$    if (trd.eq.0) nthrds=omp_get_num_threads()
C$    return
C$    end 



 
      subroutine mpi_setup (ierr)
      implicit none
      integer ierr, nsize, i_W, i_E, j_S, j_N, off_XI, off_ETA
# include "param.h"
# include "hidden_mpi_vars.h"
# include "ncvars.h"
# include "mpif.h"
C$    integer nthrds
 
      ocean_grid_comm=MPI_COMM_WORLD
      call MPI_Comm_size (ocean_grid_comm, nsize,  ierr)
      call MPI_Comm_rank (ocean_grid_comm, mynode, ierr)

      write(*,*) 'etner mpi_setup, node ', mynode, ' out of', nsize  

                                         ! Determine number of
C$    if (mynode.eq.0) then              ! threads on the zeroth MPI
C$OMP PARALLEL SHARED(nthrds)            ! node and broadcast it, so
C$      call master_num_threads (nthrds) ! that all other nodes can
C$OMP END PARALLEL                       ! set it to be the same.
C$    endif
C$    call MPI_Bcast(nthrds, 1, MPI_INTEGER, 0, ocean_grid_comm, ierr)
C$    if (mynode.gt.0) then
C$      call omp_set_num_threads (nthrds)
C$    endif

                                ! Check whether the number of nodes
      if (nsize.eq.NNODES) then ! specified as -np argument to mpirun
        inode=mod(mynode,NP_XI) ! command is consistent with the code
        jnode=mynode/NP_XI      ! parameter settings, and if so, find
        irc=1-2*mod(jnode,2)
                                ! indices inode,jnode identifying
        if (NP_XI.eq.1) then    ! placement of the subdomain to be
          west_inter=.false.    ! belonging to MPI process with rank
          east_inter=.false.    ! mynode on the "processor grid".
        else                    ! Depending on this location find
# ifdef EW_PERIODIC
          west_inter=.true.     ! whether this subdomain has
          east_inter=.true.     ! neighbours on the four sides around
# else
          if (inode.eq.0) then  ! and set corresponding logical
            west_inter=.false.  ! flags. Here WEST_INTER.eqv..true.
          else                  ! means that I [MPI process with rank
            west_inter=.true.   ! mynode] have neighbor on west side,
          endif                       ! so I have to send message to
          if (inode.eq.NP_XI-1) then  ! him and expect incoming
            east_inter=.false.        ! messages from him. Meaning
          else                  ! of the three others, EAST_, SOUTH_,
            east_inter=.true.   ! and NORTH- _INTER is the same, except
          endif                 ! they refer to the different sides.
# endif
        endif                   ! Note: periodic boundary conditions
                                ! are treated exclussively via
/*
        if (NP_ETA.eq.1) then   ! exchange of computational margins,
          south_inter=.false.   ! so that communication takes place
          north_inter=.false.   ! even is the subdomain is located
        else                    ! on the side of the grid.
# ifdef NS_PERIODIC
          south_inter=.true.
          north_inter=.true.
# else
          if (jnode.eq.0) then
            south_inter=.false.
          else
            south_inter=.true.
          endif
          if (jnode.eq.NP_ETA-1) then
            north_inter=.false.
          else
            north_inter=.true.
          endif
# endif
*/
          
        south_inter=.false.
        north_inter=.false.
        if (NP_ETA.gt.1) then
# ifdef NS_PERIODIC
          south_inter=.true.
          north_inter=.true.
# else
          if (jnode.gt.0) then
            south_inter=.true.
          endif
          if (jnode.lt.NP_ETA-1) then
            north_inter=.true.
          endif
# endif
        endif
 
        i_W=mod(inode-1+NP_XI,NP_XI)
        i_E=mod(inode+1       ,NP_XI)
        j_S=mod(jnode-1+NP_ETA,NP_ETA)
        j_N=mod(jnode+1       ,NP_ETA)
 
        p_W=i_W +NP_XI*jnode   ! Determine MPI-ranks of my neighbors
        p_E=i_E +NP_XI*jnode   ! from the sides and corners, which
        p_S=inode+NP_XI*j_S    ! will later be used to designate
        p_N=inode+NP_XI*j_N    ! sources of incoming and targets for
                               ! outgoing messages. Here they are
        p_NW=i_W+NP_XI*j_N     ! set as for double-periodic grid
        p_SW=i_W+NP_XI*j_S     ! regardless of the actual boundary
        p_NE=i_E+NP_XI*j_N     ! conditions. There is no ambiguity,
        p_SE=i_E+NP_XI*j_S     ! since WEST_INTER...etc logic blocks
                               ! the annecessary messages.


        off_XI=NP_XI*Lm-LLm
        iSW_corn=inode*Lm-off_XI/2
        if (inode.eq.0) then
          iwest=1+off_XI/2
        else
          iwest=1
        endif
        if (inode.lt.NP_XI-1) then
          ieast=Lm
        else
          ieast=Lm -(off_XI+1)/2
        endif

        off_ETA=NP_ETA*Mm-MMm
        jSW_corn=jnode*Mm-off_ETA/2
        if (jnode.eq.0) then
          jsouth=1+off_ETA/2
        else
          jsouth=1
        endif
        if (jnode.lt.NP_ETA-1) then
          jnorth=Mm
        else
          jnorth=Mm -(off_ETA+1)/2
        endif


c      write(*,'(A,7I5,1x,A,I4)') 'XI:', LLm, off_XI, iSW_corn, Lm,
c    & ieast-iwest+1, iwest+iSW_corn,ieast+iSW_corn, 'node=', mynode
c      write(*,'(A,7I5,1x,A,I4)') 'ETA:',MMm, off_ETA, jSW_corn, Mm,
c    & jnorth-jsouth+1,jsouth+jSW_corn,jnorth+jSW_corn,'node=',mynode

        

# ifdef PARALLEL_FILES
         xi_rho=ieast-iwest+1
         if (EASTERN_MPI_EDGE) then
           xi_rho=xi_rho+1
         endif
         if (WESTERN_MPI_EDGE) then
           xi_rho=xi_rho+1
           xi_u=xi_rho-1
         else
           xi_u=xi_rho
         endif

         eta_rho=jnorth-jsouth+1
         if (NORTHERN_MPI_EDGE) then
           eta_rho=eta_rho+1
         endif
         if (SOUTHERN_MPI_EDGE) then
           eta_rho=eta_rho+1
           eta_v=eta_rho-1
         else
           eta_v=eta_rho
         endif
# endif


c#ifdef PARALLEL_FILES
c# ifndef EW_PERIODIC
c        xi_rho=Lm
c        xi_u=xi_rho
c        if (inode.eq.0) xi_rho=xi_rho+1
c        if (inode.eq.NP_XI-1) then
c          xi_rho=xi_rho+1
c          xi_u=xi_u+1
c        endif
c# endif
c# ifndef NS_PERIODIC
c        eta_rho=Mm
c        eta_v=eta_rho
c        if (jnode.eq.0) eta_rho=eta_rho+1
c        if (jnode.eq.NP_ETA-1) then
c          eta_rho=eta_rho+1
c          eta_v=eta_v+1
c        endif
c# endif
c#endif

        ierr=0
      else
        MPI_master_only write(stdout,'(/1x,A,I4,1x,A,I3,A/)')
     &   '### ERROR: mpi_setup: number of MPI-nodes should be',
     &                         NNODES, 'instead of', nsize, '.'
        ierr=99
      endif
      return
      end
 
 
 
c---#define CHECK_MPI
# ifdef CHECK_MPI
 
      subroutine MPI_Test
      implicit none
#  include "param.h"
      integer tile
      do tile=0,NSUB_X*NSUB_E-1
        call MPI_Test1 (tile)
      enddo
      return
      end
 
      subroutine MPI_Test1 (tile)
      implicit none
      integer tile
#  include "param.h"
#  include "compute_tile_bounds.h"
      call MPI_Test1_tile  (istr,iend,jstr,jend)
      return
      end
 
      subroutine MPI_Test1_tile (istr,iend,jstr,jend)
      implicit none
      integer istr,iend,jstr,jend
#  include "param.h"
#  include "scalars.h"
#  include "mpif.h"
 
      integer i,j,k,ierr
      real temp2D(GLOBAL_2D_ARRAY),
     &     temp3D(GLOBAL_2D_ARRAY,0:N)
      common /MPI_Test_Arr/ temp2D,temp3D
      character string(128)
!
#  include "compute_extended_bounds.h"
!
      do j=jstrR,jendR
        do i=istrR,iendR
c          temp2D(i,j)=1.*mynode+0.5
           temp2D(i,j)=-1.*(mynode+1)
        enddo
      enddo
      do j=jstr,jend
        do i=istr,iend
          temp2D(i,j)=1.*mynode+1.
c          temp2D(i,j)=i+inode*Lm
c          temp2D(i,j)=j+jnode*Mm
        enddo
      enddo
 
 
      call mpi_exchange_tile (istr,iend,jstr,jend, temp2D, 1)
 
      do i=mynode,NNODES
        call MPI_Barrier (ocean_grid_comm, ierr)
      enddo
      write(*,*)
      write(*,*)
      write(*,'(A5,I3,2I4)') 'node=',mynode, inode,jnode
      write(*,*)
      do j=jendR,jstrR,-1
       write(string,'(I3,3x,20F4.1)') j, (temp2D(i,j), i=istrR,iendR)
        do i=7,6+4*(iendR-istrR+1)
         if (string(i-1).eq.'.' .and. string(i).eq.'0') string(i)=' '
        enddo
       write(*,'(128A1)') (string(i), i=1,6+4*(iendR-istrR+1))
      enddo
      write(*,*)
        write(*,'(5x,20I4)') (i,i=istrR,iendR)
      write(*,*)
      write(*,*)
      do i=0,mynode
        call MPI_Barrier (ocean_grid_comm, ierr)
      enddo
 
 
c      return
 
 
      do k=0,N
        do j=jstrR,jendR
          do i=istrR,iendR
            temp3D(i,j,k)=-1.*(mynode+1)
          enddo
        enddo
        do j=jstr,jend
          do i=istr,iend
            temp3D(i,j,k)=1.*mynode+1.
             temp3D(i,j,k)=0.1*( float(j-1+(jend-jstr+1)*mynode) )
          enddo
        enddo
      enddo
 
      call mpi_exchange_tile (istr,iend,jstr,jend, temp3D,N+1)
 
      do k=0,N
        write(*,*) 'k=',k
        do i=mynode,NNODES
          call MPI_Barrier (ocean_grid_comm, ierr)
        enddo
        write(*,*)
        write(*,*)
        write(*,'(A5,I3,2I4)') 'node=',mynode, inode,jnode
        write(*,*)
        do j=jendR,jstrR,-1
        write(string,'(I3,3x,20F4.1)')j,(temp3D(i,j,k),i=istrR,iendR)
         do i=7,6+4*(iendR-istrR+1)
          if (string(i-1).eq.'.'.and.string(i).eq.'0') string(i)=' '
         enddo
        write(*,'(128A1)') (string(i), i=1,6+4*(iendR-istrR+1))
        enddo
        write(*,*)
        write(*,'(5x,20I4)') (i,i=istrR,iendR)
        write(*,*)
        write(*,*)
        do i=0,mynode
          call MPI_Barrier (ocean_grid_comm, ierr)
        enddo
      enddo
      return
      end
# endif        /* CHECK_MPI */
#else
      subroutine MPI_Setup_empty
      end
#endif    /* MPI */
 
 
