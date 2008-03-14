#include "cppdefs.h"


      integer function nf_read_bry_EW (A, ncid, varid, record, type)
!
! Read side boundary from an input netCDF file.
!
! Arguments: A       real array of standard horizontal dimensions
!                             which is to be read or written.
!            ncid    NetCDF ID of in the file.
!            varid   variable ID of that variable in NetCDF file.
!            record  record number.
!            type    type of the grid (RHO-, U, V, W, PSI etc.)
!
      implicit none
#include "param.h"
      real A(0:Mm+1,N)
      integer ncid, varid, record, type,  i,j,k, kmax, shft, ierr
#include "netcdf.inc"
#include "buffer.h"
#if defined MPI
# include "hidden_mpi_vars.h"
# include "mpif.h"
      integer req(2), status(MPI_STATUS_SIZE,2)
      real bffr_rsv_S(N+1), bffr_snd_S(N+1),
     &     bffr_rsv_N(N+1), bffr_snd_N(N+1)
#endif
#include "compute_starts_counts.h"

cxx   write(*,*) 'entered nf_read_bry_EW...'

      if (ierr.eq.0) then
        start(1)=start(2)     ! Collapse one of the two horizontal
        count(1)=count(2)     ! dimensions: after this step updated

        start(2)=start(3)     ! start/count(1) corresponds to either
        count(2)=count(3)     ! XI- or ETA-dimensions; index (2) to
        start(3)=start(4)     ! vertical dimension (if any) or time
        count(3)=count(4)     ! record; (3) to time record.
!
! Read array from the disk.
!===== ===== ==== === =====
!
        ierr=nf_get_vara_FTYPE (ncid, varid, start, count, buff)
        if (ierr .eq. nf_noerr) then
          kmax=count(2)
          do k=1,kmax
            shft=1-jmin + (k-1)*count(1)
            do j=jmin,jmax
              A(j,k)=buff(j+shft)
            enddo
          enddo
#ifdef NS_PERIODIC
# ifdef MPI
          if (NP_ETA.eq.1) then
# endif
            do k=1,kmax            ! note that this periodic BC code
              A(Mm+1,k)=A( 1,k)    ! is never called from inside Open
              A(   0,k)=A(Mm,k)    ! MP parllel region associated 
            enddo                  ! with tiling.
# ifdef MPI
          endif
# endif
#endif
#ifdef MPI
cxx       write(*,*) 'starting nf_read_bry_EW mpi exchange part'
          if (south_inter) then
            call MPI_Irecv (bffr_rsv_S, kmax, MPI_DOUBLE_PRECISION,
     &                        p_S, 1, ocean_grid_comm, req(1), ierr)
          endif
          if (north_inter) then
            call MPI_Irecv (bffr_rsv_N, kmax, MPI_DOUBLE_PRECISION,
     &                        p_N, 2, ocean_grid_comm, req(2), ierr)
          endif
          if (south_inter) then
            do k=1,kmax
              bffr_snd_S(k)=A(jmin,k)
            enddo
            call MPI_Send  (bffr_snd_S, kmax, MPI_DOUBLE_PRECISION,
     &                        p_S, 2, ocean_grid_comm,        ierr)
          endif
          if (north_inter) then
            do k=1,kmax
              bffr_snd_N(k)=A(jmax,k)
            enddo
            call MPI_Send  (bffr_snd_N, kmax, MPI_DOUBLE_PRECISION,
     &                        p_N, 1, ocean_grid_comm,        ierr)
          endif
          if (south_inter) then
            call MPI_Wait (req(1),status(1,1),ierr)
            do k=1,kmax
              A(jmin-1,k)=bffr_rsv_S(k)
            enddo
          endif
          if (north_inter) then
            call MPI_Wait (req(2),status(1,2),ierr)
            do k=1,kmax
              A(jmax+1,k)=bffr_rsv_N(k)
            enddo
          endif
#endif
        else
          write(*,'(/1x,2A,I5/1x,3A,I4/)') 'ERROR in nf_read_bry: ',
     &                     'nf_get_vara netCDF error code =', ierr,
     &                     'Cause of error: ', nf_strerror(ierr)
     &                     MYID
        endif

      else
        write(*,'(/1x,2A,I4/)') 'ERROR in nf_read_bry_EW: ',
     &                          'illegal grid type', type
      endif
      nf_read_bry_EW=ierr
cxx   write(*,*) 'return from nf_read_bry_EW' 
      return
      end



      integer function nf_read_bry_NS (A, ncid, varid, record, type)
      implicit none
#include "param.h"
      real A(0:Lm+1,N)
      integer ncid, varid, record, type,  i,k,kmax, shft, ierr
#include "netcdf.inc"
#include "buffer.h"
#if defined MPI
# include "hidden_mpi_vars.h"
# include "mpif.h"
      integer req(2), status(MPI_STATUS_SIZE,2)
      real bffr_rsv_W(N+1), bffr_rsv_E(N+1),
     &     bffr_snd_W(N+1), bffr_snd_E(N+1)
#endif
#include "compute_starts_counts.h"

cxx   write(*,*) 'entered nf_read_bry_NS...'

      if (ierr.eq.0) then
        start(2)=start(3)     ! start/count(1) corresponds to either
        count(2)=count(3)     ! XI- or ETA-dimensions; index (2) to
        start(3)=start(4)     ! vertical dimension (if any) or time
        count(3)=count(4)     ! record; (3) to time record.
!
! Read array from the disk.
!===== ===== ==== === =====
!   
        ierr=nf_get_vara_FTYPE (ncid, varid, start, count, buff)
        if (ierr .eq. nf_noerr) then
          kmax=count(2)
          do k=1,kmax
            shft=1-imin + (k-1)*count(1)
            do i=imin,imax
              A(i,k)=buff(i+shft)
            enddo
          enddo
#ifdef EW_PERIODIC
# ifdef MPI
          if (NP_XI.eq.1) then
# endif
            do k=1,kmax            ! note that this periodic BC code
              A(Lm+1,k)=A( 1,k)    ! is never called from inside Open
              A(   0,k)=A(Lm,k)    ! MP parllel region associated    
            enddo                  ! with tiling.
# ifdef MPI
          endif
# endif
#endif
#ifdef MPI
cxx       write(*,*) 'starting nf_read_bry_NS mpi exchange part'

          if (west_inter) then
            call MPI_Irecv (bffr_rsv_W, kmax, MPI_DOUBLE_PRECISION,
     &                        p_W, 3, ocean_grid_comm, req(1), ierr)
          endif
          if (east_inter) then
            call MPI_Irecv (bffr_rsv_E, kmax, MPI_DOUBLE_PRECISION,
     &                        p_E, 4, ocean_grid_comm, req(2), ierr)
          endif
          if (west_inter) then
            do k=1,kmax
              bffr_snd_W(k)=A(imin,k)
            enddo
            call MPI_Send  (bffr_snd_W, kmax, MPI_DOUBLE_PRECISION,
     &                        p_W, 4, ocean_grid_comm,       ierr)
          endif
          if (east_inter) then
            do k=1,kmax
              bffr_snd_E(k)=A(imax,k)
            enddo
            call MPI_Send  (bffr_snd_E, kmax, MPI_DOUBLE_PRECISION,
     &                        p_E, 3, ocean_grid_comm,       ierr)
          endif
          if (west_inter) then
            call MPI_Wait (req(1),status(1,1),ierr)
            do k=1,kmax
              A(imin-1,k)=bffr_rsv_W(k)
            enddo
          endif
          if (east_inter) then
            call MPI_Wait (req(2),status(1,2),ierr)
            do k=1,kmax
              A(imax+1,k)=bffr_rsv_E(k)
            enddo
          endif
#endif
        else
          write(*,'(/1x,2A,I5/1x,3A,I4/)') 'ERROR in nf_read_bry: ',
     &                     'nf_get_vara netCDF error code =', ierr,
     &                     'Cause of error: ', nf_strerror(ierr)
     &                     MYID
        endif

      else
        write(*,'(/1x,2A,I4/)') 'ERROR in nf_read_bry_NS: ',
     &                          'illegal grid type', type
      endif
      nf_read_bry_NS=ierr
cxx   write(*,*) 'return from nf_read_bry_NS'
      return
      end
