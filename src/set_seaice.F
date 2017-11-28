#include "cppdefs.h"
#if defined SOLVE3D && defined ICEOBS
 
      subroutine set_seaice_tile (istr,iend,jstr,jend, ierr)
!
! Set-up sea-ice concentration for current tile.
!
      implicit none
      integer istr,iend,jstr,jend, ierr, i,j, it1,it2
      real tmid, cff, cff1, cff2, cff3, cff4
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
      tmid=time+0.5*dt
      it1=3-itice
      it2=itice
      cff1=ice_time(it2)-tmid
      cff2=tmid-ice_time(it1)
      if (cff1 < dt .and. ntice > 1) synchro_flag=.true.
      cff=0.01/(day2sec*(cff1+cff2))
      cff3=cff1*cff ; cff4=cff2*cff
      cff=1./(cff1+cff2)
      cff1=cff1*cff ; cff2=cff2*cff

      if (ntice == 1) then                ! Load time-invariant
        cff=0.01/day2sec                  ! sea-ice concentration
        do j=jstrR,jendR                  ! fresh water flux: convert
          do i=istrR,iendR                ! from [cm/day] to [m/s]
            sic(i,j)=sicg(i,j,itice)
            freezing(i,j)=cff*freezingg(i,j,itice)*t(i,j,N,nrhs,isalt)
            melting(i,j)=cff*meltingg(i,j,itice)*t(i,j,N,nrhs,isalt)
          enddo
        enddo
      else                                ! Interpolate in time.
        do j=jstrR,jendR                  ! Fresh water flux: convert
          do i=istrR,iendR                ! from [cm/day] to [m/s]
            sic(i,j)=cff1*sicg(i,j,it1)+cff2*sicg(i,j,it2)
            freezing(i,j)=t(i,j,N,nrhs,isalt)*(
     &                     cff3*freezingg(i,j,it1)
     &                    +cff4*freezingg(i,j,it2))
            melting(i,j)=t(i,j,N,nrhs,isalt)*(
     &                     cff3*meltingg(i,j,it1)
     &                    +cff4*meltingg(i,j,it2))
          enddo
        enddo

        if ( cff1 < 0. .or. cff2 < 0.
     &       .or. cff3 < 0. .or. cff4 < 0.) then
          if (ZEROTH_TILE) then
            write(*,'(/1x,3A/3(1x,A,F16.10)/)')       '### WARNING: ',
     &  'set_seaice_tile :: Current model time is outside bounds of ',
     &  '''ice_time''.',  'ice_tstart =', ice_time(it1)*sec2day,
     &                    'tdays =',      tdays,
     &                    'ice_tend =',   ice_time(it2)*sec2day
          endif
          ierr=ierr+1
        endif
      endif
      return
      end
#else
      subroutine set_seaice_empty
      end
#endif /* SOLVE3D && ICEOBS */

