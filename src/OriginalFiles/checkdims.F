#include "cppdefs.h"
 
      function checkdims (ncid, ncname, lstr, max_rec)
!
! Verify that all spatial dimensions in file with netCDF ID=ncid and
! name=ncname are consistent with the corresponding model dimensions
! and find the current size of unlimited dimension.
!
! input:  ncid       netCDF ID, name and length of name of
!         ncname     input netCDF file (all three arguments
!         lstr       arguments are assumed defined).
!
! output: checkdims  (returned value) error status [=nf_noerr=0,
!                                          if no error occurs.]
!         max_rec    current size of unlimited dimension
!                    [=0, if there is no unlimited dimension].
!
      implicit none
      integer checkdims, ncid, max_rec, ierr, icount,
     &        ndims, recdim, dsize, id, ldim, lstr, lenstr
      character ncname*(*),     dname*16
#include "param.h"
#include "ncvars.h"
#include "netcdf.inc"
 
      icount=0      !--> initialize: error counter (must remain 0);
      recdim=0      ! netCDF ID of unlimited dimension (if exists); 
      max_rec=0     ! current size unlimited dimension (if exists).
 
      ierr=nf_inq_ndims (ncid, ndims)
      if (ierr .eq. nf_noerr) then
        ierr=nf_inq_unlimdim (ncid, recdim)
        if (ierr .ne. nf_noerr) then
          write(stdout,'(/1x,4A/)')        'WARNING: No unlimited ',
     &               'dimension found in ''', ncname(1:lstr), '''.' 
        endif
        do id=1,ndims
          ierr=nf_inq_dim (ncid, id, dname, dsize)
          if (ierr .eq. nf_noerr) then
            ldim=lenstr(dname)
            if ((ldim.eq.6 .and. dname(1:ldim).eq.'xi_rho') .or.
     &          (ldim.eq.4 .and. dname(1:ldim).eq.'xi_v'  )) then
              if (dsize.ne.xi_rho) then
                write(stdout,1) dname(1:ldim), dsize,
     &                           ncname(1:lstr), xi_rho
                icount=icount+1
              endif
            elseif ((ldim.eq.4 .and.dname(1:ldim).eq.'xi_u'  ) .or.
     &              (ldim.eq.6 .and. dname(1:ldim).eq.'xi_psi')) then
              if (dsize.ne.xi_u) then
                write(stdout,1) dname(1:ldim), dsize,
     &                           ncname(1:lstr), xi_u
                icount=icount+1
              endif
            elseif ((ldim.eq.7 .and. dname(1:ldim).eq.'eta_rho') .or.
     &              (ldim.eq.5 .and. dname(1:ldim).eq.'eta_u' )) then
              if (dsize.ne.eta_rho) then
                write(stdout,1) dname(1:ldim), dsize,
     &                           ncname(1:lstr), eta_rho
                icount=icount+1
              endif
            elseif ((ldim.eq.5 .and.dname(1:ldim).eq.'eta_v') .or.
     &              (ldim.eq.7 .and.dname(1:ldim).eq.'eta_psi')) then
              if (dsize.ne.eta_v) then
                write(stdout,1) dname(1:ldim), dsize,
     &                           ncname(1:lstr), eta_v
                icount=icount+1
              endif
#ifdef SOLVE3D
            elseif (ldim.eq.5 .and. dname(1:ldim).eq.'s_rho') then
              if (dsize.ne.N) then
                write(stdout,1) dname(1:ldim), dsize,
     &                           ncname(1:lstr), N
                icount=icount+1
              endif
            elseif (ldim.eq.3 .and. dname(1:ldim).eq.'s_w') then
              if (dsize.ne.N+1) then
                write(stdout,1) dname(1:ldim), dsize,
     &                           ncname(1:lstr), N+1
                icount=icount+1
              endif
#endif
            elseif (id.eq.recdim) then
              max_rec=dsize
            endif
          else
            write(stdout,'(/1x,2A,I3/12x,3A/12x,A/)') '### ERROR: ',
     &            'checkdims :: Cannot get size of dimension #', id,
     &            'from netCDF file ''',    ncname(1:lstr),   '''.',
     &                                           nf_strerror(ierr)
            icount=icount+1
          endif
        enddo
      else
        write(stdout,'(/1x,4A/12x,A/)')  '### ERROR: checkdims :: ',
     &          'Cannot get number of dimensions in netCDF file ''',
     &                     ncname(1:lstr), '''.', nf_strerror(ierr)
        icount=icount+1
      endif
      checkdims=icount
      return

  1   format(/' ### ERROR: checkdims :: wrong size of dimension ''',
     &                  A, ''' =', i5 / 12x, 'in netCDF file ''', A,
     &                            ''': must be', i5,1x, 'instead.'/)
      end
 
