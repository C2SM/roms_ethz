#include "cppdefs.h"
      subroutine oas_roms_computebounds(grd)
!     - ML - WARNING: This is very dirty, using compute_starts_counts.h ONLY works
!                     when PARALLEL_FILES is not activated.
!                     We should switch to a more robust solution in the future
!                     based on istr, istrU, etc... It would probably work except for
!                     global starting indices of local tile.
#ifdef PARALLEL_FILES
      print *, 'ERROR'
      print *, '-----'
      print *, 'OASIS coupling not supported with PARALLEL_FILES'
      call abort
#endif
      
      USE oas_roms_def, ONLY: cpl_grd, k_rho, k_u, k_v
      
      TYPE(OAS_GRID) :: grd
      integer type

#include "ncvars.h"

      if (TRIM(grd)%pt == 'rho') then
         type = r2dvar
         grd%pt = 'rho'
         grd%grd_name = 'Rrho'
         grd%dims_g = (/xi_rho, eta_rho/)
      elseif (TRIM(grd)%pt == 'u') then
         type = u2dvar
         grd%pt = 'u'
         grd%grd_name = 'R__u'
         grd%dims_g = (/xi_u, eta_rho/)
      elseif (TRIM(grd)%pt == 'v') then
         grd%pt = 'v'
         grd%grd_name = 'R__v'
         grd%dims_g = (/xi_rho, eta_v/)
         type = v2dvar
      endif
      
#include "compute_starts_counts.h"

      grd%imin = imin
      grd%imax = imax
      grd%jmin = jmin
      grd%jmax = jmax
      grd%dims_l = (/count(1), count(2)/)
      grd%start_g = (/start(1), start(2)/)
      ALLOCATE(grd%exfld(imin:imax,jmin:jmax))
      
      if ( IOASISDEBUGLVL .eq. 1 ) then
         print *, '***********************************************'
         print *, 'Grid Variables ROMS', TRIM(grd%pt), type
         print *, 'start', grd%start_g
         print *, 'local_rho, local_eta', count(1), count(2)
         print *, 'imin,imax, jmin,jmax', imin, imax, jmin, jmax
         print *, '**********************************************'
      endif

      end
