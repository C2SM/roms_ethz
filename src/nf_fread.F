#ifndef WRITER
# include "cppdefs.h"

      function nf_fread  (A, ncid, varid, record, type)
#else
      function nf_fwrite (A, ncid, varid, record, type)
#endif

! Read/Write a floating point array from/into an input/output
! netCDF file.
!
! Arguments: A       real array of standard horizontal dimensions
!                                    which is to be read or written.
!            ncid    netCDF ID of in the file.
!            varid   variable ID of that variable in netCDF file.
!            record  record number.
!            type    type of the grid (RHO-, U, V, W, PSI etc.)
!
! Because significant portion of the code calculates starting and
! stopping indices for the subarray (which are exactly the same for
! both read and write operations) the code has been unified to
! guarantee that both functions are exactly adjoint.

      implicit none
#ifndef WRITER
      integer nf_fread
#else
      integer nf_fwrite
#endif
      integer ncid, varid, record, type
#include "param.h"
      real A(GLOBAL_2D_ARRAY,N+1)
CSDISTRIBUTE_RESHAPE  A(BLOCK_PATTERN,*) BLOCK_CLAUSE
      logical mask_land_data
      integer vid, i,j,k, shft, ierr
#ifdef WRITER
      integer datatype, ndims, natts, dimid(8)
      character*12 vname
#endif
#include "buffer.h"
#ifdef MASK_LAND_DATA
# include "grid.h"
      real*8, parameter :: spv_set=1.D+33
#endif
#include "netcdf.inc"
#include "compute_starts_counts.h"


      if (varid>0) then         ! Normally netCDF IDs are positive.
        vid=varid                  ! Negative "varid" is used here as
        mask_land_data=.true.      ! flag to signal that land masking
      else                         ! does not need to be applied for
        vid=-varid                 ! this variable (typically this is
        mask_land_data=.false.     ! reserved for grid variables and
      endif                        ! topography).


! Read/Write array from the disk.
!===== ===== ===== ==== === =====

      if (ierr==0) then
#ifndef WRITER
        ierr=nf_get_vara_FTYPE (ncid, vid, start, count, buff)
        if (ierr /= nf_noerr) then
          write(*,'(/1x,2A,3x,A,I4/)') '### ERROR: nf_fread :: ',
     &             nf_strerror(ierr) MYID
        else
#endif

#ifdef MASK_LAND_DATA
          if (mask_land_data) then
            if (horiz_type==0) then
              do k=1,count(3)
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
            elseif (horiz_type==1) then
              do k=1,count(3)
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
            elseif (horiz_type==2) then
              do k=1,count(3)
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
            endif
          else  !<-- mask_land_data
#endif
            do k=1,count(3)
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
        ierr=nf_put_vara_FTYPE (ncid, vid, start, count, buff)
        if (ierr/=nf_noerr) then
          write(*,'(/1x,2A,3x,A,I4/)')  '### ERROR: nf_fwrite :: ',
     &             nf_strerror(ierr) MYID
          write(*,'(12x,A,I7,3x,A,I7/12x,A,I3,7I6)') 'ncid =', ncid,
     &   'varid =', vid, 'start,count =', (start(i),count(i), i=1,4)

          i=nf_inq_var (ncid,vid, vname, datatype, ndims,dimid,natts)
          if (i==nf_noerr) then
            write(*,'(1x,2A,1x,A,I2,2x,A,I3,2x,A,8I3)') 'vname = ',
     &             vname, 'datatype =', datatype, 'ndims =', ndims,
     &                             'dimid =', (dimid(i), i=1,ndims)
            do i=1,ndims
              j=nf_inq_dim (ncid, dimid(i), vname, k)
              if (j==nf_noerr) write(*,'(29x,2A,I5)') vname,' =',k
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

        if (vert_type==0) then
          call exchange2d_tile (EXCH_ARR_RANGE, A)
#  ifdef SOLVE3D
        elseif (vert_type==1) then
          call exchange_tile (EXCH_ARR_RANGE, A,  N)
        elseif (vert_type==2) then
          call exchange_tile (EXCH_ARR_RANGE, A,N+1)
#  endif
        endif

# endif
#endif
      else
#ifdef WRITER
        write(*,'(/1x,2A,I4/)') '### ERROR: nf_fwrite :: ',
#else
        write(*,'(/1x,2A,I4/)')  '### ERROR: nf_fread :: ',
#endif
     &                        'illegal grid type', type
      endif
#ifdef WRITER
      nf_fwrite=ierr
#else
      nf_fread=ierr
#endif
      end

#ifndef WRITER
# define WRITER
# include "nf_fread.F"
#endif
