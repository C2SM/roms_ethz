#include "cppdefs.h"
#if defined SOLVE3D && defined LMD_KPP

      subroutine alfabeta_tile (istr,iend,jstr,jend,
     &               imin,imax,jmin,jmax, alpha,beta)

! Compute thermal expansion and saline contraction coefficients
! as functions of potential temperature, salinity from a polynomial
! expression (Jackett & McDougall, 1992). The coefficients are
! evaluated at the surface.
!
!  alpha(Ts,Tt,0)=-d(rho1(Ts,Tt,0))/d(Tt) / rho0
!  beta(Ts,Tt,0) = d(rho1(Ts,Tt,0))/d(Ts) / rho0
!
!  Adapted from original "rati" and "beta" routines.

      implicit none
# include "param.h"
      integer istr,iend,jstr,jend, imin,imax,jmin,jmax, i,j
      real, dimension (PRIVATE_2D_SCRATCH_ARRAY) :: alpha, beta
# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
# ifdef NONLIN_EOS
      real, parameter ::         Q01=6.793952E-2,   Q02=-9.095290E-3,
     &        Q03=+1.001685E-4,  Q04=-1.120083E-6,  Q05=+6.536332E-9,
     &        U00=+0.824493   ,  U01=-4.08990E-3 ,  U02=+7.64380E-5 ,
     &        U03=-8.24670E-7 ,  U04=+5.38750E-9 ,  V00=-5.72466E-3 ,
     &        V01=+1.02270E-4 ,  V02=-1.65460E-6 ,  W00=+4.8314E-4
      real    Tt, Ts, sqrtTs, cff
# else
#  include "eos_vars.h"
# endif

# ifdef NONLIN_EOS

      cff=1./rho0
      do j=jmin,jmax
        do i=imin,imax
          Tt=t(i,j,N,nstp,itemp)
#  ifdef SALINITY
          Ts=t(i,j,N,nstp,isalt)
          sqrtTs=sqrt(Ts)
#  else
          Ts=0.
          sqrtTs=0.
#  endif
          alpha(i,j)=-cff*( Q01+Tt*( 2.*Q02+Tt*( 3.*Q03+Tt*(
     &                                  4.*Q04 +Tt*5.*Q05 )))
     &                         +Ts*( U01+Tt*( 2.*U02+Tt*(
     &                                     3.*U03 +Tt*4.*U04 ))
     &                                +sqrtTs*( V01+Tt*2.*V02))
     &                                                       )
 
          beta(i,j)= cff*( U00+Tt*(U01+Tt*(U02+Tt*(U03+Tt*U04)))
     &            +1.5*(V00+Tt*(V01+Tt*V02))*sqrtTs+2.*W00*Ts )
        enddo
      enddo
# else
      do j=jmin,jmax                 ! Thermal expansion and saline
        do i=imin,imax               ! contraction coefficients for
          alpha(i,j)=abs(Tcoef)      ! Linear Equation of State
#  ifdef SALINITY
          beta(i,j)=abs(Scoef)
#  else
          beta(i,j)=0.
#  endif
        enddo
      enddo
# endif /* NONLIN_EOS */
      end
#else
      subroutine alfabeta_empty
      end
#endif /* LMD_KPP */
