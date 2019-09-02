#include "cppdefs.h"
      subroutine oas_set_stflux_tile (istr,iend,jstr,jend, itrc, ierr)
!
! Set-up surface tracer flux for current tile.
!       
      use oas_roms_vardef
      use netcdf
      implicit none
      integer istr,iend,jstr,jend,itrc,ierr,i,j
      real cff

# define STFLUX_DATA
# define SST_DATA

# include "param.h"
# include "scalars.h"
# include "ocean3d.h"
# include "forces.h"
# if defined PACIFIC || defined STFLX_LIM
#  include "mixing.h"
# endif
!
# include "compute_extended_bounds.h"
      if (IOASISDEBUGLVL .eq. 4) then
        if (itrc.eq.itemp) then 
          print *, '****************************'
          print *, 'Oasis updating Net Heat Flux'
          print *, '****************************'
        elseif (itrc.eq.isalt) then
          print *, '*******************************'
          print *, 'Oasis updating Fresh Water Flux'
          print *, '*******************************'
        endif
      endif

      if (itrc.eq.itemp) then            ! surface tracer flux
        cff=1./(rho0*Cp)
        do j=oas_jmin, oas_jmax
          do i=oas_imin, oas_imax
            stflx(i,j,itemp)=cff*frcv(i,j, oas_nhf)
          enddo
        enddo 
# ifdef SALINITY
      elseif (itrc.eq.isalt) then
          cff=1/rho0      !(Received field in (Kg/m2*s) convert to (m/s)
          do j=oas_jmin, oas_jmax
            do i=oas_imin, oas_imax
              stflx(i,j,isalt)=cff*frcv(i,j, oas_TEP)
     &                                       *t(i,j,N,nrhs,isalt)
            enddo
          enddo
# endif
      endif   

! Update boundary information from neighbouring processors
      call exchange_r2d_tile(istr,iend,jstr,jend,stflx(:,:,itemp))

!
! Restrict stflx to prevent surface temperature to go below -2
! degrees C.
!
#  if defined PACIFIC || defined STFLX_LIM
              stflx(i,j,itemp)=max( stflx(i,j,itemp),
#   ifdef LMD_KPP
     &                          max(hbls(i,j,nstp), Hz(i,j,N))
#   else
     &                                               Hz(i,j,N)
#   endif
     &                         *(-2.D0-t(i,j,N,nrhs,itemp))/dt
     &                                                       )
#  endif


      end
