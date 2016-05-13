#include "cppdefs.h"
#if defined SOLVE3D && defined ICEOBS
 
      subroutine set_seaice_tile (istr,iend,jstr,jend, ierr)
!
! Set-up sea-ice concentration for current tile.
!
      implicit none
      integer istr,iend,jstr,jend, ierr, i,j, it1,it2
      real cff, cff1, cff2, cff3, cff4
# define ICEOBS_DATA
# include "param.h"
# include "scalars.h"
# include "grid.h"
# include "forces.h"
# include "ocean3d.h"
!
# include "compute_extended_bounds.h"
!

! Set coefficients for interpolation. Check that for the next time
! step [when time=time+dt] both weights will still be positive, and
! if not, set synchro_flag to signal that new data should be read
! from an appropriate netCDF input file (master thread only).
! After that either load time-invariant data or interpolate in time.
! Complain about error and signal to quit, if interpolation is
! needed, but not possible.
!
      it1=3-itice
      it2=itice
      cff1=ice_time(it2)-time
      cff2=time-ice_time(it1)
      if (cff1.lt.dt) synchro_flag=.true.
 
      if (ntice.eq.1) then                  ! Load time-invariant
        if (iic.eq.ntstart) then            ! sea-ice concentration
          cff=0.01/86400.                   ! fresh water flux: convert
          do j=jstrR,jendR                  ! from [cm/day] to [m/s]
            do i=istrR,iendR
              sic(i,j)=sicg(i,j,itice)
              freezing(i,j)=cff*freezingg(i,j,itice)*t(i,j,N,nrhs,isalt)
              melting(i,j)=cff*meltingg(i,j,itice)*t(i,j,N,nrhs,isalt)
            enddo
          enddo
        endif
      elseif (cff1.ge.0. .and. cff2.ge.0.) then     ! Interpolate in time.
        cff=0.01/(86400.*(cff1+cff2))               ! Fresh water flux: convert
        cff3=cff1*cff                               ! from [cm/day] to [m/s]
        cff4=cff2*cff
        cff=1./(cff1+cff2)
        cff1=cff1*cff
        cff2=cff2*cff
        do j=jstrR,jendR
          do i=istrR,iendR
            sic(i,j)=cff1*sicg(i,j,it1)+cff2*sicg(i,j,it2)
            freezing(i,j)=(cff3*freezingg(i,j,it1)+cff4*freezingg(i,j,it2))
     &                                       *t(i,j,N,nrhs,isalt)
            melting(i,j)=(cff3*meltingg(i,j,it1)+cff4*meltingg(i,j,it2))
     &                                       *t(i,j,N,nrhs,isalt)
          enddo
        enddo
      elseif (ZEROTH_TILE) then
        write(*,'(/1x,2A/3(1x,A,F16.10)/)')
     &     'SET_ICELUX_TILE - current model time is outside bounds ',
     &     'of ''ice_time''.',  'ICE_TSTART=', ice_time(it1)*sec2day,
     &                          'TDAYS=',      tdays,
     &                          'ICE_TEND=',   ice_time(it2)*sec2day
        ierr=ierr+1
      endif
      return
      end
#else
      subroutine set_seaice_empty
      end
#endif /* SOLVE3D && ICEOBS */
 
