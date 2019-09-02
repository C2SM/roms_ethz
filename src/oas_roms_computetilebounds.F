#include "cppdefs.h"
       subroutine oas_roms_computetilebounds(tile)
       USE oas_roms_vardef
       integer tile
#include "param.h"
#include "grid.h"
#include "ncvars.h"
#include "compute_tile_bounds.h"

        call oas_roms_settilebounds(istr,iend,jstr,jend)
        end

       subroutine oas_roms_settilebounds(istr,iend,jstr,jend)
       USE oas_roms_vardef
       integer tile
#include "param.h"
#include "grid.h"
#include "ncvars.h"
#include "compute_auxiliary_bounds.h"

          oas_imin = istrR
          oas_imax = iendR
          oas_jmin = jstrR
          oas_jmax = jendR

          oas_imin_u = istrR
          oas_imax_u = iendR
          oas_jmin_u = jstrR
          oas_jmax_u = jendR

          oas_imin_v = istrR
          oas_imax_v = iendR
          oas_jmin_v = jstrR
          oas_jmax_v = jendR
        end
