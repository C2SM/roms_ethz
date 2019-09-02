#include "cppdefs.h"
       subroutine oas_roms_computebounds(type)
       USE oas_roms_vardef
       integer type
#include "param.h"
#include "grid.h"
#include "ncvars.h"
#include "compute_starts_counts.h"


       if (type.eq.r2dvar) then
         ! Keep local array sizes
          oas_xi = count(1)
          oas_eta = count(2)
          oas_imin = imin
          oas_imax = imax
          oas_jmin = jmin
          oas_jmax = jmax
          oas_start(1) = start(1)
          oas_start(2) = start(2)
         if ( IOASISDEBUGLVL .eq. 1 ) then
            print *, '***********************************************'
            print *, 'Grid Variables ROMS RHO', type
            print *, 'start', oas_start
            print *, 'local_rho, local_eta', oas_xi, oas_eta
            print *, 'imin,imax, jmin,jmax',oas_imin, oas_imax, oas_jmin, oas_jmax
            print *, 'count', count
            print *, '**********************************************'
         endif
       endif           

       if (type.eq.u2dvar) then
         ! Keep local array sizes
          oas_xi_u = count(1)
          oas_eta_u = count(2)
          oas_imin_u = imin
          oas_imax_u = imax
          oas_jmin_u = jmin
          oas_jmax_u = jmax
          oas_start_u(1) = start(1)
          oas_start_u(2) = start(2)
         if (IOASISDEBUGLVL .eq. 1 ) then
            print *, '***********************************************'
            print *, 'Grid Variables ROMS U', type
            print *, 'start', oas_start_u
            print *, 'local_rho, local_eta', oas_xi_u, oas_eta_u
            print *, 'imin,imax, jmin,jmax',oas_imin_u, oas_imax_u, oas_jmin_u, oas_jmax_u  
            print *, 'count', count
            print *, '**********************************************'
         endif
       endif

       if (type.eq.v2dvar) then
         ! Keep local array sizes                                              
          oas_xi_v = count(1)                                                  
          oas_eta_v = count(2)
          oas_imin_v = imin
          oas_imax_v = imax
          oas_jmin_v = jmin
          oas_jmax_v = jmax
          oas_start_v(1) = start(1)
          oas_start_v(2) = start(2)
         if ( IOASISDEBUGLVL .eq. 1 ) then
            print *, '***********************************************'
            print *, 'Grid Variables ROMS V', type
            print *, 'start', oas_start_v
            print *, 'local_rho, local_eta', oas_xi_v, oas_eta_v
            print *, 'imin,imax, jmin,jmax',oas_imin_v, oas_imax_v, oas_jmin_v, oas_jmax_v 
            print *, 'count', count
            print *, '**********************************************'
         endif
       endif

       end
