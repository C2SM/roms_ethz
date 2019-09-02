#include "cppdefs.h"

      subroutine oas_set_smflux_tile (istr,iend,jstr,jend, ierr)
      USE oas_roms_vardef
      implicit none
      integer istr,iend,jstr,jend, ierr, i,j
      real cff
# define SMFLUX_DATA
# include "param.h"
# ifdef CURVGRID
#  include "grid.h"
# endif
# include "forces.h"
# include "scalars.h"
!
# include "compute_extended_bounds.h"

!Wind stress received from Cosmo in [N/m^2] convert to kinematic momentum flux [m^2/s^2]
      if (IOASISDEBUGLVL .eq. 4) then
        print *, '**************************'
        print *, 'Oasis updating Wind Stress'   
        print *, '**************************'
      endif
   
      cff=1./rho0
      do j=oas_jmin, oas_jmax
        do i=oas_imin, oas_imax
          sustr(i,j)=cff*frcv(i,j, oas_ust)
          svstr(i,j)=cff*frcv(i,j, oas_vst)
        enddo                                
      enddo


! Update boundary information from neighbouring processors
      call exchange_u2d_tile(istr,iend,jstr,jend,sustr)
      call exchange_v2d_tile(istr,iend,jstr,jend,svstr)
      

      end
