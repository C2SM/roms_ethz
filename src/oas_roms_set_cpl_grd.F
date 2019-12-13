#include "cppdefs.h"
      subroutine oas_roms_set_grd(grd, k_pt)
!     Description
!     -----------
!     Set coupling grid strucutre using compute_starts_counts.h
!     WARNING: This is very dirty, using compute_starts_counts.h ONLY works
!              when PARALLEL_FILES is not activated.
!              We should switch to a more robust solution in the future
!              based on istr, istrU, etc... It would probably work except for
!              global starting indices of local tile.
#ifdef PARALLEL_FILES
      print *, 'ERROR'
      print *, '-----'
      print *, 'OASIS coupling not supported with PARALLEL_FILES'
      call abort
#endif
      
      USE oas_roms_def, ONLY: OAS_GRID, cpl_grd, k_rho, k_u, k_v

!     Arguments
      TYPE(OAS_GRID) :: grd
      integer k_pt
      
#include "param.h"
#include "ncvars.h"
      
      if (k_prt == k_rho) then
         type = r2dvar
         grd%pt = 'rho'
         grd%grd_name = 'Rrho'
         grd%dims_g = (/xi_rho, eta_rho/)
      elseif (k_prt == k_u) then
         type = u2dvar
         grd%pt = 'u'
         grd%grd_name = 'R__u'
         grd%dims_g = (/xi_u, eta_rho/)
      elseif (k_prt == k_v) then
         grd%pt = 'v'
         grd%grd_name = 'R__v'
         grd%dims_g = (/xi_rho, eta_v/)
         type = v2dvar
      endif

      call oas_roms_set_grd_local(grd, type)

      end

!     ! -------------------------------------------------------------------------------- !


      subroutine oas_roms_set_grd_local(grd, type)
!     Description
!     -----------
!     Set coupling grid properties depending on the compute_starts_counts.h include

      USE oas_roms_def, ONLY: OAS_GRID, IOASISDEBUGLVL

!     Arguments
      TYPE(OAS_GRID) :: grd
      integer type
      
#include "param.h"
#include "compute_starts_counts.h"

      grd%imin = imin
      grd%imax = imax
      grd%jmin = jmin
      grd%jmax = jmax
      grd%dims_l = (/count(1), count(2)/)
      grd%start_g = (/start(1), start(2)/)
      ALLOCATE(grd%exfld(imin:imax,jmin:jmax))
      
      if ( IOASISDEBUGLVL == 1 ) then
         print *, '***********************************************'
         print *, 'Grid Variables ROMS', TRIM(grd%pt), type
         print *, 'start', grd%start_g
         print *, 'local_rho, local_eta', count(1), count(2)
         print *, 'imin,imax, jmin,jmax', imin, imax, jmin, jmax
         print *, '**********************************************'
      endif
      
      end
