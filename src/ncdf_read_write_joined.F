#ifndef WRITER
# include "cppdefs.h"

      function ncdf_read_joined(ncid, varid, record, horiz_type, A, nmax)
#else
!=================
! Wrapper function for argument order of ncdf_write. Note that this 
!    function one has one more argument, "nmax" than the old 
!    "nf_fwrite". It exists solely out of lazyness me, mm,
!    to edit by hand over 100 calls to nf_fwrite or write some script 
!    to edit the nf_fwrite calls in the wrt_bec_flux
!    routines, which are about to be rewritten anyways.
!
!     *** Hopefully is soon becomes obsolete. ***
!
      function    ncf_fwrite_joined(A, ncid, varid, record, horiz_type, nmax)
      integer ::  ncf_fwrite_joined  , ncid, varid, record, horiz_type, nmax
     &         , ncdf_write_joined
#include "param.h"
      real A(GLOBAL_2D_ARRAY,nmax)
      ncf_fwrite_joined = ncdf_write_joined(ncid, varid, record, horiz_type, A, nmax)
      return
      end
!=================
      function ncdf_write_joined(ncid, varid, record, horiz_type, A, nmax)
#endif

! Read/Write a floating point array from/into an input/output
! netCDF file.

! Arguments: A       real array of standard horizontal dimensions
!                                    which is to be read or written.
!            ncid    netCDF ID of in the file.
!            varid   variable ID of that variable in netCDF file.
!            record  record number.
!            type    type of the grid (RHO-, U, V, W, PSI etc.)

! Because significant portion of the code calculates starting and
! stopping indices for the subarray (which are exactly the same for
! both read and write operations) the code has been unified to
! guarantee that both functions are exactly adjoint.

      implicit none
#ifndef WRITER
      integer ncdf_read_joined
#else
      integer ncdf_write_joined
#endif
      integer ncid, varid, record, horiz_type, nmax
#include "param.h"
      real A(GLOBAL_2D_ARRAY,nmax)
CSDISTRIBUTE_RESHAPE  A(BLOCK_PATTERN,nmax) BLOCK_CLAUSE
      logical mask_land_data
      integer vid, i,j,k, shft, ierr
#ifdef WRITER
      integer datatype, ndims, natts, dimid(8)
      character(len=16) vname
#endif
#include "buffer.h"
#ifdef MASK_LAND_DATA
# include "grid.h"
      real*8, parameter :: spv_set=1.D+33
#endif
#include "netcdf.inc"

! compute_starts_counts.h with PARALLEL_FILES undefined:
      integer imin,imax,jmin,jmax, start(4),count(4)

      jmin=horiz_type/2             ! calculate starting indices
      imin=horiz_type-2*jmin        ! in horizontal directions.

      ierr=0            ! These are default settings. In all cases
      do i=1,4          ! start,count(1:2) correspond to XI- and ETA-
        start(i)=1      ! dimensions, while 3 is either for vertical
        count(i)=1      ! dimension (if any) or for time record
      enddo             ! (2D-fields); 4 is for time record only

#ifdef MPI
      if (WESTERN_MPI_EDGE) then
        imin=imin + iwest-1       !<-- to account for grid type
      else
        start(1)=1+iSW_corn + 1-imin
        imin=iwest
      endif
      if (EASTERN_MPI_EDGE) then
        imax=ieast+1
      else
        imax=ieast
      endif
      if (SOUTHERN_MPI_EDGE) then
        jmin=jmin + jsouth-1
      else
        start(2)=1+jSW_corn + 1-jmin
        jmin=jsouth
      endif
      if (NORTHERN_MPI_EDGE) then
        jmax=jnorth+1
      else
        jmax=jnorth
      endif
#else                        /* non-MPI --> */
      imax=Lm+1 ; jmax=Mm+1
#endif
      count(1)=imax-imin+1
      count(2)=jmax-jmin+1

#ifndef NO_RECORD_CHECK
      if (nmax > 1) then
        count(3)=nmax
        start(4)=record
      else
        start(3)=record
      endif
#endif
! end of compute_starts_counts.h

      if (varid > 0) then          ! Normally netCDF IDs are positive.
        vid=varid                  ! Negative "varid" is used here as
        mask_land_data=.true.      ! flag to signal that land masking
      else                         ! does not need to be applied for
        vid=-varid                 ! this variable (typically this is
        mask_land_data=.false.     ! reserved for grid variables and
      endif                        ! topography).

! Read/Write array from the disk.
!===== ===== ===== ==== === =====

#ifndef WRITER
      ierr=nf_get_vara_FTYPE(ncid, vid, start,count, buff)
      if (ierr /= nf_noerr) then
        write(*,'(/1x,2A,3x,A,I4/)') '### ERROR: ncdf_read_joined :: ',
     &             nf_strerror(ierr) MYID
      else
#endif

! Note that expression for "shft" is exactly the same in all five
! cases below, while application of land mask is different for the
! variables of different grid staggering; also note effectively .or.
! rather than .and. logic in setting velocity values to infinity:
! velocity components at the boundary (normal to it) are set to 0,
! while the ones fully inside (between two land points) to spv.

#ifdef MASK_LAND_DATA
        if (mask_land_data) then
          if (horiz_type == 0) then
            do k=1,nmax
              do j=jmin,jmax
                shft=1-imin+count(1)*(j-jmin+(k-1)*count(2))
                do i=imin,imax
                  if (rmask(i,j) > 0.5) then
# ifdef WRITER
                    buff(i+shft)=A(i,j,k)
# else
                    A(i,j,k)=buff(i+shft)
# endif
                  else
# ifdef WRITER
                    buff(i+shft)=spv_set
# else
                    A(i,j,k)=0.D0
# endif
                  endif
                enddo
              enddo
            enddo
          elseif (horiz_type == 1) then
            do k=1,nmax
              do j=jmin,jmax
                shft=1-imin+count(1)*(j-jmin+(k-1)*count(2))
                do i=imin,imax
                  if (rmask(i,j)+rmask(i-1,j) > 0.5) then
# ifdef WRITER
                    buff(i+shft)=A(i,j,k)
# else
                    A(i,j,k)=buff(i+shft)
# endif
                  else
# ifdef WRITER
                    buff(i+shft)=spv_set
# else
                    A(i,j,k)=0.D0
# endif
                  endif
                enddo
              enddo
            enddo
          elseif (horiz_type == 2) then
            do k=1,nmax
              do j=jmin,jmax
                shft=1-imin+count(1)*(j-jmin+(k-1)*count(2))
                do i=imin,imax
                  if (rmask(i,j)+rmask(i,j-1) > 0.5) then
# ifdef WRITER
                    buff(i+shft)=A(i,j,k)
# else
                    A(i,j,k)=buff(i+shft)
# endif
                  else
# ifdef WRITER
                    buff(i+shft)=spv_set
# else
                    A(i,j,k)=0.D0
# endif
                  endif
                enddo
              enddo
            enddo
          elseif (horiz_type == 3) then
            do k=1,nmax
              do j=jmin,jmax
                shft=1-imin+count(1)*(j-jmin+(k-1)*count(2))
                do i=imin,imax
                  if ( rmask(i,j)+rmask(i-1,j)+rmask(i,j-1)
     &                           +rmask(i-1,j-1) > 0.5 ) then
# ifdef WRITER
                    buff(i+shft)=A(i,j,k)
# else
                    A(i,j,k)=buff(i+shft)
# endif
                  else
# ifdef WRITER
                    buff(i+shft)=spv_set
# else
                    A(i,j,k)=0.D0
# endif
                  endif
                enddo
              enddo
            enddo
          endif  !<-- horiz_type == 0,1,2,3
        else  !<-- mask_land_data
#endif
          do k=1,nmax
            do j=jmin,jmax
              shft=1-imin+count(1)*(j-jmin+(k-1)*count(2))
              do i=imin,imax
# ifdef WRITER
                buff(i+shft)=A(i,j,k)
# else
                A(i,j,k)=buff(i+shft)
# endif
              enddo
            enddo
          enddo
#ifdef MASK_LAND_DATA
        endif  !<-- mask_land_data
#endif


#ifdef WRITER
        ierr=nf_put_vara_FTYPE(ncid, vid, start, count, buff)
        if (ierr /= nf_noerr) then
          write(*,'(/1x,2A,3x,A,I4/)')  '### ERROR: ncdf_write_joined :: ',
     &             nf_strerror(ierr) MYID
          write(*,'(12x,A,I7,3x,A,I7/12x,A,I3,7I6)') 'ncid =', ncid,
     &   'varid =', vid, 'start,count =', (start(i),count(i), i=1,4)

          i=nf_inq_var(ncid, vid,vname, datatype, ndims,dimid,natts)
          if (i == nf_noerr) then
            write(*,'(1x,2A,1x,A,I2,2x,A,I3,2x,A,8I3)') 'vname = ',
     &             vname, 'datatype =', datatype, 'ndims =', ndims,
     &                             'dimid =', (dimid(i), i=1,ndims)
            do i=1,ndims
              j=nf_inq_dim(ncid, dimid(i), vname, k)
              if (j == nf_noerr) write(*,'(29x,2A,I5)') vname,' =',k
            enddo
          endif
#endif
        endif

! Exchange periodic and computational margins (reader only).

#ifndef WRITER
# ifdef EXCHANGE
#  ifdef MPI
#   define EXCH_ARR_RANGE iwest,ieast,jsouth,jnorth
#  else
#   define EXCH_ARR_RANGE 1,Lm,1,Mm
#  endif
#  ifdef SOLVE3D
      call exchange_tile(EXCH_ARR_RANGE, A,nmax)
#  else
      call exchange2d_tile(EXCH_ARR_RANGE, A)
#  endif
# endif
#endif

#ifdef WRITER
      ncdf_write_joined=ierr
#else
      ncdf_read_joined=ierr
#endif
      end

#ifndef WRITER
# define WRITER
# include "ncdf_read_write_joined.F"
#endif
