#include "cppdefs.h"

      function nf_read_bry_EW (A, ncid, varid, record, type)

! Read side boundary from an input netCDF file.
!
! Arguments: A       real array of standard horizontal dimensions
!                             which is to be read or written.
!            ncid    netCDF ID of in the file.
!            varid   variable ID of that variable in netCDF file.
!            record  record number.
!            type    type of the grid (RHO-, U, V, W, PSI etc.)

      implicit none
#include "param.h"
      integer nf_read_bry_EW, ncid, varid, record, type
      real A(0:Mm+1,N)
      integer i,j,k, kmax, shft, ierr, lstr, lenstr
      character(len=16) vname
#include "buffer.h"
#include "netcdf.inc"

#include "compute_starts_counts.h"

#ifdef VERBOSE
      write(*,*) 'entered nf_read_bry_EW...' MYID
#endif
      if (ierr==0) then      ! Note that "ierr" is initialized inside
        start(1)=start(2)    ! "compute_starts_counts.h". Suppress the
        count(1)=count(2)    ! fist horizontal dimension XI: after this
        start(2)=start(3)    ! step the updated start/count(1) becomes
        count(2)=count(3)    ! corresponding to ETA- netCDF dimension;
        start(3)=start(4)    ! index 2 to vertical dimension (if it
        count(3)=count(4)    ! exists) or time record; index 3 to
                             ! time record.
! Read array from the disk.
!===== ===== ==== === =====

        ierr=nf_get_vara_FTYPE (ncid, varid, start, count, buff)
        if (ierr == nf_noerr) then
          kmax=count(2)
          do k=1,kmax
            shft=1-jmin + (k-1)*count(1)
            do j=jmin,jmax
              A(j,k)=buff(j+shft)
            enddo
          enddo
#ifdef MPI
          call exch_bry_EW_tile (jsouth,jnorth, A, kmax)
#elif defined NS_PERIODIC
          call exch_bry_EW_tile (     1,   Mm,  A, kmax)
#endif
        else
          ierr=nf_inq_varname (ncid, varid, vname)
          if (ierr == nf_noerr) then
            lstr=lenstr(vname)
            write(*,'(1x,2A,I6,1x,3A,2x,A,2x,A,I4)')  '### ERROR: ',
     &              'nf_read_bry_EW ::Cannot read rec =',   record,
     &              'of variable ''',  vname(1:lstr),  '''.',
     &               nf_strerror(ierr) MYID
          else
            write(*,'(1x,2A,I6,1x,A,I4,2x,A,2x,A,I4)') '### ERROR: ',
     &              'nf_read_bry_EW ::Cannot read rec =',   record,
     &              'of variable with netCDF ID =',  varid,
     &               nf_strerror(ierr) MYID
          endif
        endif
      else
        write(*,'(/1x,2A,I4/)') '### ERROR: nf_read_bry_EW :: ',
     &                          'illegal grid type', type
      endif
#ifdef VERBOSE
      write(*,*) 'leaving nf_read_bry_EW'  MYID
#endif
      nf_read_bry_EW=ierr
      end


      function nf_read_bry_NS (A, ncid, varid, record, type)
      implicit none
      integer nf_read_bry_NS, ncid, varid, record, type
#include "param.h"
      real A(0:Lm+1,N)
      integer  i, k, kmax, shft, ierr, lstr, lenstr
      character(len=16) vname
#include "buffer.h"
#include "netcdf.inc"

#include "compute_starts_counts.h"

#ifdef VERBOSE
      write(*,*) 'entered nf_read_bry_NS...'   MYID
#endif
      if (ierr==0) then        ! Suppress netCDF dimension ETA. After
        start(2)=start(3)      ! this  start/count(1) corresponds
        count(2)=count(3)      ! to XI-dimensions (as before); index
        start(3)=start(4)      ! (2) to vertical dimension (if any) or
        count(3)=count(4)      ! time record; (3) (if any) to time
                               ! record.
! Read array from the disk.
!===== ===== ==== === =====

        ierr=nf_get_vara_FTYPE (ncid, varid, start, count, buff)
        if (ierr == nf_noerr) then
          kmax=count(2)
          do k=1,kmax
            shft=1-imin + (k-1)*count(1)
            do i=imin,imax
              A(i,k)=buff(i+shft)
            enddo
          enddo
#ifdef MPI
          call exch_bry_NS_tile (iwest,ieast, A, kmax)
#elif defined EW_PERIODIC
          call exch_bry_NS_tile (    1,   Lm, A, kmax)
#endif
        else
          ierr=nf_inq_varname (ncid, varid, vname)
          if (ierr == nf_noerr) then
            lstr=lenstr(vname)
            write(*,'(1x,2A,I6,1x,3A,2x,A,2x,A,I4)')  '### ERROR: ',
     &              'nf_read_bry_NS ::Cannot read rec =',   record,
     &              'of variable ''',  vname(1:lstr),  '''.',
     &               nf_strerror(ierr) MYID
          else
            write(*,'(1x,2A,I6,1x,A,I4,2x,A,2x,A,I4)') '### ERROR: ',
     &              'nf_read_bry_NS ::Cannot read rec =',   record,
     &              'of variable with netCDF ID =',  varid,
     &               nf_strerror(ierr) MYID
          endif
        endif

      else
        write(*,'(/1x,2A,I4/)') '### ERROR: nf_read_bry_NS :: ',
     &                          'illegal grid type', type
      endif
#ifdef VERBOSE
      write(*,*) 'leaving nf_read_bry_NS' MYID
#endif
      nf_read_bry_NS=ierr
      end


! The following two routines exchange ghost points of boundary arrays
! in the case of periodicity in the transversal direction or exchanging
! MPI margins. Their intended use is in the context of reading boundary
! data from a file (routines above), or creating it by other means
! (analytical). This means that the routines below can be called both
! for the whole domain (or MPI subdomain) as well as for a tile.



#if defined NS_PERIODIC || defined MPI

      subroutine exch_bry_EW_tile (jstr,jend, A, kmax)
      implicit none
# include "param.h"
      integer jstr,jend, kmax, k
      real A(0:Mm+1,kmax)
# include "buffer.h"
# if defined MPI
#  include "mpif.h"
#  include "hidden_mpi_vars.h"
      integer, parameter :: ibss=100000
      integer req(2), status(MPI_STATUS_SIZE,2), ierr
      real, dimension(N+1) :: bffr_rsv_S, bffr_snd_S,
     &                        bffr_rsv_N, bffr_snd_N
# endif

# ifdef NS_PERIODIC
#  ifdef MPI
      if (NP_ETA==1) then
#  endif
        if (jstr==1) then             ! Note that CPP-macros
          do k=1,kmax                 ! SOUTHERN_EDGE and NOTHERN_EDGE
            A(Mm+1,k)=A(1,k)          ! are undefined in the case of
          enddo                       ! periodicity. Also because this
        endif                         ! part is only applicable when
        if (jend==Mm) then            ! NP_ETA=1 there is no need to
          do k=1,kmax                 ! express the indices in terms
            A(0,k)=A(Mm,k)            ! of jsouth,jnorth in MPI case.
          enddo
        endif
#  ifdef MPI
      else
#  endif
# endif
# ifdef MPI
#  ifdef VERBOSE
        write(*,*) 'starting exch_bry_EW mpi exchange' MYID
#  endif
        if (SOUTH_MSG_EXCH) then
          call MPI_Irecv (bffr_rsv_S, kmax, MPI_DOUBLE_PRECISION,
     &               p_S, ibss+1, ocean_grid_comm, req(1), ierr)
        endif
        if (NORTH_MSG_EXCH) then
          call MPI_Irecv (bffr_rsv_N, kmax, MPI_DOUBLE_PRECISION,
     &               p_N, ibss+2, ocean_grid_comm, req(2), ierr)
        endif
        if (SOUTH_MSG_EXCH) then
          do k=1,kmax
            bffr_snd_S(k)=A(jstr,k)
          enddo
          call MPI_Send  (bffr_snd_S, kmax, MPI_DOUBLE_PRECISION,
     &                    p_S, ibss+2, ocean_grid_comm,    ierr)
        endif
        if (NORTH_MSG_EXCH) then
          do k=1,kmax
            bffr_snd_N(k)=A(jend,k)
          enddo
          call MPI_Send (bffr_snd_N, kmax, MPI_DOUBLE_PRECISION,
     &                   p_N, ibss+1, ocean_grid_comm,    ierr)
        endif
        if (SOUTH_MSG_EXCH) then
          call MPI_Wait (req(1),status(1,1),ierr)
          do k=1,kmax
            A(jstr-1,k)=bffr_rsv_S(k)
          enddo
        endif
        if (NORTH_MSG_EXCH) then
          call MPI_Wait (req(2),status(1,2),ierr)
          do k=1,kmax
            A(jend+1,k)=bffr_rsv_N(k)
          enddo
        endif
#  ifdef NS_PERIODIC
      endif ! <-- NP_ETA==1
#  endif
# endif
      end
#endif

#if defined EW_PERIODIC || defined MPI

      subroutine exch_bry_NS_tile (istr,iend, A, kmax)
      implicit none
# include "param.h"
      integer istr,iend, kmax, k
      real A(0:Lm+1,kmax)
# if defined MPI
#  include "mpif.h"
#  include "hidden_mpi_vars.h"
      integer, parameter :: ibss=100000
      integer req(2), status(MPI_STATUS_SIZE,2), ierr
      real, dimension(N+1) :: bffr_rsv_W, bffr_rsv_E,
     &                        bffr_snd_W, bffr_snd_E
# endif

# ifdef EW_PERIODIC
#  ifdef MPI
      if (NP_XI==1) then
#  endif
        if (istr==1) then             ! Note that CPP-macros
          do k=1,kmax                 ! WESTERN_EDGE and EASTERN_EDGE
            A(Lm+1,k)=A(1,k)          ! are undefined in the case of
          enddo                       ! periodicity. Also because this
        endif                         ! part is only applicable when
        if (iend==Lm) then            ! NP_XI=1 there is no need to
          do k=1,kmax                 ! express the indices in terms
            A(0,k)=A(Lm,k)            ! of iwest,ieast in MPI case.
          enddo
        endif
#  ifdef MPI
      else
#  endif
# endif
# ifdef MPI
#  ifdef VERBOSE
        write(*,*) 'starting exch_bry_NS mpi exchange' MYID
#  endif
        if (WEST_MSG_EXCH) then
          call MPI_Irecv (bffr_rsv_W, kmax, MPI_DOUBLE_PRECISION,
     &               p_W, ibss+3, ocean_grid_comm, req(1), ierr)
        endif
        if (EAST_MSG_EXCH) then
          call MPI_Irecv (bffr_rsv_E, kmax, MPI_DOUBLE_PRECISION,
     &               p_E, ibss+4, ocean_grid_comm, req(2), ierr)
        endif

        if (WEST_MSG_EXCH) then
          do k=1,kmax
            bffr_snd_W(k)=A(istr,k)
          enddo
          call MPI_Send (bffr_snd_W, kmax, MPI_DOUBLE_PRECISION,
     &                   p_W, ibss+4, ocean_grid_comm,    ierr)
        endif
        if (EAST_MSG_EXCH) then
          do k=1,kmax
            bffr_snd_E(k)=A(iend,k)
          enddo
          call MPI_Send (bffr_snd_E, kmax, MPI_DOUBLE_PRECISION,
     &                   p_E, ibss+3, ocean_grid_comm,    ierr)
        endif

        if (WEST_MSG_EXCH) then
          call MPI_Wait (req(1),status(1,1),ierr)
          do k=1,kmax
            A(istr-1,k)=bffr_rsv_W(k)
          enddo
        endif
        if (EAST_MSG_EXCH) then
          call MPI_Wait (req(2),status(1,2),ierr)
          do k=1,kmax
            A(iend+1,k)=bffr_rsv_E(k)
          enddo
        endif
#  ifdef EW_PERIODIC
      endif !<-- NP_XI==1
#  endif
# endif
      end
#endif
