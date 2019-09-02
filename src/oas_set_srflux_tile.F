#include "cppdefs.h"

      subroutine oas_set_srflux_tile (istr,iend,jstr,jend, ierr)
!
! Set-up shortwave radiation flux for current tile.
!
      USE oas_roms_vardef
      implicit none
      integer istr,iend,jstr,jend, ierr, i,j
      real cff, cff1, cff2
# ifdef DIURNAL_SRFLUX
      real phase
!cc      real cos_h, dec, cos_d, sin_d, tan_d, phi, h0
#endif
# define SRFLUX_DATA
# include "param.h"
# include "scalars.h"
# include "grid.h"
# include "forces.h"
!
# include "compute_extended_bounds.h"

      if (IOASISDEBUGLVL .eq. 4) then
        print *, '**************************'
        print *, 'Oasis updating Short Wave Radiation'
        print *, '**************************'
      endif

# ifdef DIURNAL_SRFLUX
      cff=2.*dt*sec2day
      phase=4.*(tdays-int(tdays))-2.
      cff1=max(-1., min(1., phase-cff))
      cff2=max(-1., min(1., phase+cff))
      phase=(cff2-cff1)/cff + (sin(pi*cff2)-sin(pi*cff1))/(pi*cff)

c     if (ZEROTH_TILE) write(*,*) 'swrad modulation', tdays, phase
# endif

          cff=1/(rho0*Cp)                   ! flux
      do j=oas_jmin, oas_jmax
        do i=oas_imin, oas_imax
            srflx(i,j)=cff*frcv(i,j,oas_swr)
        enddo
      enddo

! Update boundary information from neighbouring processors
      call exchange_r2d_tile(istr,iend,jstr,jend,srflx)

        do j=jstrR,jendR
          do i=istrR,iendR
# ifdef DIURNAL_SRFLUX
            cff=stflx(i,j,itemp)-srflx(i,j)  ! subtract shortwave
                                             ! radiation from the net, 
# if defined DAILYPAR_PHOTOINHIBITION || defined DAILYPAR_BEC
! save the daily average value for the evaluation of photoinhibited 
! nitrification
            srflx_dailyavg(i,j)=srflx(i,j)
# endif 
            srflx(i,j)=srflx(i,j)*phase      ! modulate 
            stflx(i,j,itemp)=cff+srflx(i,j)  ! and add back
# endif
          enddo
        enddo
 
# ifdef XXXXX 
!
! DIURNAL CYCLE - USED IN BOTH PHYSICAL AND ECOSYSTEM MODELS
! Patrick Marchesiello - 1999: Modulate average dayly insolation
! to get diurnal cycle by:
!
!              cos(h)*cos(d)*cos(phi) + sin(d)*sin(phi)
!       pi * ---------------------------------------------
!             sin(h0)*cos(d)*cos(phi) + h0*sin(d)sin(phi)
!
! where: h, d, phi -- are hour, declination, latitude angles;
!        h0 is hour angle at sunset and sunrise
!
      cos_h=cos(2.*pi*(tdays-int(tdays)))
      dec=-0.406*cos(deg2rad*(tdays-int(tdays/360.)*360.))
      cos_d=cos(dec)
      sin_d=sin(dec)
      tan_d=tan(dec)
      do j=jstrR,jendR
        do i=istrR,iendR
          phi=latr(i,j)*deg2rad
          h0=acos(-tan(phi)*tan_d)
          cff1=cos_d*cos(phi)
          cff2=sin_d*sin(phi)
#if defined DAILYPAR_PHOTOINHIBITION || defined DAILYPAR_BEC
! save the daily average value for the biogeochemical model
          srflx_dailyavg(i,j)=srflx(i,j)
#endif /* DAILYPAR_PHOTOINHIBITION || DAILYPAR_BEC */
          srflx(i,j)=srflx(i,j)*pi*(cos_h*cff1+cff2)
     &                        /(sin(h0)*cff1+h0*cff2)
          srflx(i,j)=max(0.,srflx(i,j))
        enddo
      enddo
# endif
      return
      end
