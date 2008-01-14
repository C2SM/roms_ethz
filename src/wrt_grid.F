#include "cppdefs.h"
 
      subroutine wrt_grid (ncid, ncname, lstr)
!
! Write grid variables in output NetCDF file, which may be restart,
! history, averages, etc. All variables are assumed to be previously
! defined by def_grid.
!
! Arguments: ncid    netCDF unit-ID of NetCDF file, which must be
!                            already opened and in definition mode;
!            ncname  netCDF file name (used only in error messages)
!            lstr    length of ncname
!
      implicit none
      character*(*) ncname
      integer ncid, lstr, varid, ierr, nf_fwrite
#include "param.h"
#include "scalars.h"
#include "ncvars.h"
#include "netcdf.inc"
#include "grid.h"
 
#if defined MPI & !defined PARALLEL_FILES
      if (mynode.eq.0) then 
#endif
!
! Grid type switch: Spherical or Cartesian.
!
        ierr=nf_inq_varid (ncid, 'spherical', varid)
        if (ierr .eq. nf_noerr) then
          ierr=nf_put_var1_text (ncid, varid, 1,
#ifdef SPHERICAL
     &                                          'T')
#else
     &                                          'F')
#endif
          if (ierr .ne. nf_noerr) then
            write(stdout,2) 'spherical', ncname(1:lstr)
          endif
        else
          write(stdout,1) 'spherical', ncname(1:lstr) 
        endif
        if (ierr .ne. nf_noerr)  goto 99                   !--> ERROR

#ifndef SPHERICAL
!
! Physical Dimensions of Model Domain, xl,el (Cartesian grid only).
!
        ierr=nf_inq_varid (ncid, 'xl', varid)
        if (ierr .eq. nf_noerr) then
          ierr=nf_put_var1_FTYPE (ncid, varid, 1, xl)
          if (ierr.ne.nf_noerr) then
            write(stdout,2) 'xl', ncname(1:lstr)
          endif
        else
          write(stdout,1) 'xl', ncname(1:lstr)
        endif
        if (ierr .ne. nf_noerr)  goto 99                   !--> ERROR

        ierr=nf_inq_varid (ncid, 'el', varid)
        if (ierr .eq. nf_noerr) then
          ierr=nf_put_var1_FTYPE (ncid, varid, 1, el)
          if (ierr.ne.nf_noerr) then
            write(stdout,2) 'el', ncname(1:lstr)
          endif
        else
          write(stdout,1) 'el', ncname(1:lstr)
        endif
        if (ierr .ne. nf_noerr)  goto 99                   !--> ERROR
#endif


#if defined MPI & !defined PARALLEL_FILES
      endif
#endif
!
! NOTE: In the case when masking of output data is activated by
! CPP-switch MASK_LAND_DATA, nf_fwrite uses sign of its argument
! "varid" as switch to perform on not perform masking.  Because
! none of the grid variables should actually be masked, in the
! code below varid is passed into nf_fwrite with negative sign. 
!
! Bathymetry.
!
      ierr=nf_inq_varid (ncid, 'h', varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fwrite (h(START_2D_ARRAY), ncid, -varid, 0, r2dvar)
        if (ierr.ne.nf_noerr) then
          write(stdout,2) 'h', ncname(1:lstr)
        endif
      else
        write(stdout,1) 'h', ncname(1:lstr)
      endif
      if (ierr .ne. nf_noerr)  goto 99                     !--> ERROR
!
! Coriolis parameter.
!
      ierr=nf_inq_varid (ncid, 'f', varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fwrite (f(START_2D_ARRAY), ncid, -varid, 0, r2dvar)
        if (ierr .ne. nf_noerr) then
          write(stdout,2) 'f', ncname(1:lstr)
        endif
      else
        write(stdout,1) 'f', ncname(1:lstr)
      endif
      if (ierr .ne. nf_noerr)  goto 99                     !--> ERROR
!
! Curvilinear transformation metrics.
!
      ierr=nf_inq_varid (ncid, 'pm', varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fwrite (pm(START_2D_ARRAY), ncid, -varid, 0, r2dvar)
        if (ierr .ne. nf_noerr) then
          write(stdout,2) 'pm', ncname(1:lstr)
        endif
      else
        write(stdout,1) 'pm', ncname(1:lstr)
      endif
      if (ierr .ne. nf_noerr)  goto 99                     !--> ERROR

      ierr=nf_inq_varid (ncid, 'pn', varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fwrite (pn(START_2D_ARRAY), ncid, -varid, 0, r2dvar)
        if (ierr .ne. nf_noerr) then
          write(stdout,2) 'pn', ncname(1:lstr)
        endif
      else
        write(stdout,1) 'pn', ncname(1:lstr)
      endif
      if (ierr .ne. nf_noerr)  goto 99                     !--> ERROR
!
! Longitude/latitude or cartezian coordinates of RHO-points
!
#ifdef SPHERICAL
      ierr=nf_inq_varid (ncid, 'lon_rho', varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fwrite (lonr(START_2D_ARRAY), ncid, -varid, 0,r2dvar)
        if (ierr .ne. nf_noerr) then
          write(stdout,2) 'lon_rho', ncname(1:lstr)
        endif
      else
        write(stdout,1) 'lon_rho', ncname(1:lstr)
      endif
      if (ierr .ne. nf_noerr)  goto 99                     !--> ERROR

      ierr=nf_inq_varid (ncid, 'lat_rho', varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fwrite (latr(START_2D_ARRAY), ncid, -varid, 0,r2dvar)
        if (ierr .ne. nf_noerr) then
          write(stdout,2) 'lat_rho', ncname(1:lstr)
        endif
      else
        write(stdout,1) 'lat_rho', ncname(1:lstr)
      endif
      if (ierr .ne. nf_noerr)  goto 99                     !--> ERROR
#else
      ierr=nf_inq_varid (ncid, 'x_rho', varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fwrite (xr(START_2D_ARRAY), ncid, -varid, 0, r2dvar)
        if (ierr .ne. nf_noerr) then
          write(stdout,2) 'x_rho', ncname(1:lstr)
        endif
      else
        write(stdout,1) 'x_rho', ncname(1:lstr)
      endif
      if (ierr .ne. nf_noerr)  goto 99                     !--> ERROR

      ierr=nf_inq_varid (ncid, 'y_rho', varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fwrite (yr(START_2D_ARRAY), ncid, -varid, 0, r2dvar)
        if (ierr .ne. nf_noerr) then
          write(stdout,2) 'y_rho', ncname(1:lstr)
        endif
      else
        write(stdout,1) 'y_rho', ncname(1:lstr)
      endif
      if (ierr .ne. nf_noerr)  goto 99                     !--> ERROR
#endif
#ifdef CURVGRID
!
! Angle between XI-axis and EAST at RHO-points
!
      ierr=nf_inq_varid (ncid, 'angle', varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fwrite (angler(START_2D_ARRAY),ncid, -varid,0,r2dvar)
        if (ierr .ne. nf_noerr) then
          write(stdout,2) 'angle', ncname(1:lstr)
        endif
      else
        write(stdout,1) 'angle', ncname(1:lstr)
      endif
      if (ierr .ne. nf_noerr)  goto 99                     !--> ERROR

#endif
#ifdef MASKING
!
! Masking fields at RHO-points.
!
      ierr=nf_inq_varid (ncid, 'mask_rho', varid)
      if (ierr .eq. nf_noerr) then
        ierr=nf_fwrite (rmask(START_2D_ARRAY),ncid, -varid, 0,r2dvar)
        if (ierr .ne. nf_noerr) then
          write(stdout,2) 'mask_rho', ncname(1:lstr)
        endif
      else
        write(stdout,1) 'mask_rho', ncname(2:lstr)
      endif
      if (ierr .ne. nf_noerr)  goto 99                     !--> ERROR
#endif
      write(stdout,'(6x,4A,2x,A,I4)') 'WRT_GRID -- wrote grid ',
     &                 'data into file ''', ncname(1:lstr), '''.'
     &                  MYID
      return
 99   may_day_flag=3
      return

  1   format(/1x,'### ERROR: wrt_grid:  Cannot get netCDF ID for ''',
     &             A, ''' from', /11x, 'netCDF file ''', A, '''.'/)
  2   format(/1x,'### ERROR: wrt_grid:  Cannot write variable ''',
     &             A, ''' into', /11x, 'netCDF file ''', A, '''.'/)
      end
 
