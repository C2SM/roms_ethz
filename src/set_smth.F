      subroutine SET_SMTH _tile(istr,iend,jstr,jend, ierr)
!
! Set surface forcing at current time by interpolating 
! outside forcing in time
!
      implicit none
      integer istr,iend,jstr,jend, ierr, i,j, it1,it2
      real cff, cff1, cff2, cff3, cff4
# include "param.h"
# include "scalars.h"
# include "grid.h"
# include "forces.h"
# include "ocean3d.h"
 
# include "compute_extended_bounds.h"
 

! Set coefficients for interpolation. Check that for the next time
! step [when time=time+dt] both weights will still be positive, and
! if not, set synchro_flag to signal that new data should be read
! from an appropriate netCDF input file (master thread only).
! After that either load time-invariant data or interpolate in time.
! Complain about error and signal to quit, if interpolation is
! needed, but not possible.
 
      it1=3- itsmth
      it2= itsmth
      cff= VAR_CFF                           ! conversion factor 
      cff1= timevar (it2)-time
      cff2=time- timevar (it1)
      if (cff1.lt.dt) synchro_flag=.true.
 
      if (ntsmth.eq.1) then                  ! Load time-invariant
        if (iic.eq. ntstart ) then             ! forcing field
          do j=jstrR,jendR
            do i=istrR,iendR
              VAR_NOW (i,j)= var (i,j, itsmth )
            enddo
          enddo
        endif
      elseif (cff1.ge.0. .and. cff2.ge.0.) then     ! Interpolate in time.
        cff3=cff1*cff
        cff4=cff2*cff
        cff=1./(cff1+cff2)
        cff1=cff1*cff
        cff2=cff2*cff
        do j=jstrR,jendR
          do i=istrR,iendR
            VAR_NOW (i,j)=cff1* var (i,j,it1)+cff2* var (i,j,it2)
          enddo
        enddo
      elseif (ZEROTH_TILE) then
        write(*,*) '###ERROR: ', varname
     &     , '_tile - current model time '
     &     , 'is outside of time bounds: day ', timevar(it1)*sec2day
     &     , ' to ', timevar(it2)*sec2day,'.'
        ierr=ierr+1
      endif
      return
      end
#undef varname
#undef var
#undef timevar
#undef ntsmth 
#undef itsmth 
#undef SET_SMTH
#undef VAR_CFF
#undef VAR_NOW
