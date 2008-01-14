#include "cppdefs.h"
#ifdef SOLVE3D
# define POWER_FUNCTION
c--# define TEST_WEIGHTS

 
      subroutine set_weights
      implicit none
# include "param.h"
# include "scalars.h"
# include "coupling.h"
      integer i,j, iter
# ifdef POWER_FUNCTION
      real p,q,r, scale
# else
      real arg
# endif
# ifdef TEST_WEIGHTS
      real zeta(4), DUon, Zt_avg1, DU_avg2, cff1,cff2,cff3
# endif
      real*QUAD sum, shft, cff
      nfast=0
      do i=1,2*ndtfast                   ! Reset both sets of
        weight(1,i)=0.                   ! weights to all-zeros.
        weight(2,i)=0.
      enddo
 
# ifdef POWER_FUNCTION
!                                        ! Possible settings of
! Power-function shaped filters          ! parameters to yield the
!                                        ! second-order accuracy:
!      f(xi)=xi^p*(1-xi^q)-r*xi          !
!                                        ! p   q   r,I3=3I2-2  I2=1
! where xi=scale*i/ndtfast; scale, p,    ! -------------------------
! q, r, and normalization are chosen to  !  2.  1.    0.1181   0.169
! yield correct zeroth- (normalization), !  2.  2.    0.1576   0.234
! first- (consistency), and second-order !  2.  3.    0.1772   0.266
! moments, resulting in second-order     !  2.  4.    0.1892   0.284
! temporal accuracy for time-averaged    !  2.  5.    0.1976   0.296
! barotropic motions resolved by         !  2.  6.    0.2039   0.304
! baroclinic time step.                  !  2.  8.    0.2129   0.314
!
! NOTE: Theoretical values of "r" presented in the table above are
! derived assuming "exact" barotropic mode stepping. Consequently, it
! does not account for effects caused by Forward-Euler (FE) startup
! of the barotropic mode at every 3D time step. As the result, the
! code may become unstable if the theoretical value of "r" is used
! when mode splitting ratio "ndtfast" is small, thus yielding non-
! negligible start up effects.  To compensate this, the accepted 
! value of "r" is reduced relatively to theoretical one, depending
! on splitting ratio "ndtfast".  This measure is empirical. It is
! shown to work with setting of "ndtfast" as low as 8...10, which is
! even more robust that the Hamming-window (cos^2) weights below.
! It should also be noted that 
! 
      p=2.0
      q=4.0
      r=0.281*(1.-4.7/(dble(ndtfast)-6.75))

c      r=0.2846158868  ! non-dissipative, p=2, q=4, ndtfast=60

c      r=0.25

      scale=(p+1.)*(p+q+1.)/((p+2.)*(p+q+2.)*dble(ndtfast))
      do iter=1,16
        nfast=0
        do i=1,2*ndtfast
          cff=scale*dble(i)
          weight(1,i)=cff**p - cff**(p+q) - r*cff
          if (weight(1,i).gt.0.) nfast=i
          if (nfast.gt.0 .and. weight(1,i).lt.0.) weight(1,i)=0.
        enddo
                                         ! Find center of gravity
        sum=0.                           ! of the primary weighting
        shft=0.                          ! shape function and adjust
        do i=1,nfast                     ! "scale" iteratively to
          sum=sum+weight(1,i)            ! place the centroid
          shft=shft+weight(1,i)*dble(i)  ! exactly at "ndtfast".
        enddo
        scale=scale * shft/(sum*dble(ndtfast))
#  ifdef TEST_WEIGHTS
        write(*,*) shft/sum, ndtfast
#  endif
      enddo
# else
      cff=pi/dble(ndtfast)                   ! cos**2 shaped filter
      do i=1,2*ndtfast
        arg=cff*dble(i-ndtfast)
        if (2.*abs(arg) .lt. pi) then
          weight(1,i)=(cos(arg))**2 +0.0882  ! <--- Hamming Window
          
!!!        weight(1,i)=1.                    !<-- FLAT
          nfast=i
        endif
      enddo
# endif
!
! Post-processing of primary weights: Although it is assumed that
! the initial settings of the primary weights has its center of
! gravity "reasonably close" to ndtfast, it may be not so according
! to the discrete rules of integration. The following procedure is
! designed to put the senter of gravity exactly to ndtfast by
! computing mismatch "ndtfast-shft" and applying basically an
! upstream advection of weights to eliminate the mismatch
! iteratively. Once this procedure is complete primary weights are
! normalized.
!
      do iter=1,ndtfast                    ! Find center of gravity
        sum=0.                             ! of the primary weights
        shft=0.                            ! and subsequently
        do i=1,nfast                       ! calculate the mismatch
          sum=sum+weight(1,i)              ! to be compensated
          shft=shft+dble(i)*weight(1,i)
        enddo
        shft=shft/sum
        cff=dble(ndtfast)-shft
# ifdef TEST_WEIGHTS
        MPI_master_only write(*,'(A,2I4,2F22.18)')
     &  'centering weights:', iter, ndtfast, shft, cff
# endif
        if (cff .gt. 1.) then              ! Apply advection step
          nfast=nfast+1                    ! using either whole, or
          do i=nfast,2,-1                  ! fractional shifts.
            weight(1,i)=weight(1,i-1)
          enddo                            ! Note that none of
          weight(1,1)=0.                   ! the four loops here
        elseif (cff .gt. 0.) then          ! is reversible.
          sum=1.-cff
          do i=nfast,2,-1
            weight(1,i)=sum*weight(1,i)+cff*weight(1,i-1)
          enddo
          weight(1,1)=sum*weight(1,1)
        elseif (cff .lt. -1.) then
          nfast=nfast-1
          do i=1,nfast,+1
            weight(1,i)=weight(1,i+1)
          enddo
          weight(1,nfast+1)=0.
        elseif (cff .lt. 0.) then
          sum=1.+cff
          do i=1,nfast-1,+1
            weight(1,i)=sum*weight(1,i)-cff*weight(1,i+1)
          enddo
          weight(1,nfast)=sum*weight(1,nfast)
        endif
      enddo
                                           ! Set SECONDARY weights
      do j=1,nfast                         ! assuming that backward
        cff=weight(1,j)                    ! Euler time step is used
        do i=1,j                           ! for free surface. NOTE
          weight(2,i)=weight(2,i)+cff      ! that array weight(2,i)
        enddo                              ! is assumed to have all-
      enddo                                ! zero status at entry in
      sum=0.                               ! this segment of code.
      cff=0.
      do i=1,nfast                         ! Normalize both sets of
        sum=sum+weight(1,i)                ! weights.
        cff=cff+weight(2,i)
      enddo
      sum=1./sum
      cff=1./cff
      do i=1,nfast
        weight(1,i)=sum*weight(1,i)
        weight(2,i)=cff*weight(2,i)
      enddo

      MPI_master_only write(*,'(/1x,A,I3,4x,A,I4,8x,A,2F5.1,F9.4/)')
     &        'Mode splitting: ndtfast =', ndtfast, 'nfast =', nfast
# ifdef POWER_FUNCTION
     &                                             ,'p,q,r =', p,q,r
# endif


 
# ifdef TEST_WEIGHTS
      MPI_master_only write(*,'(/1x,A,I3,4x,A,I3/4x,A,2(12x,A))')
     &               'Mode Splitting weights: ndtfast =', ndtfast,
     &               'nfast =',  nfast,   'primary',  'secondary',
     &               'accumulated-to-current-step'
      cff=0.
      cff1=0.
      cff2=0.
      cff3=0.
      sum=0.
      shft=0.
      do i=1,nfast
        cff=cff   + weight(1,i)
        cff1=cff1 + weight(1,i)*dble(i)
        cff2=cff2 + weight(1,i)*dble(i*i)
        cff3=cff3 + weight(1,i)*dble(i*i*i)
        sum=sum+weight(2,i)
        shft=shft + weight(2,i)*(dble(i)-0.5)
        MPI_master_only write(*,'(I3,4F19.15)')
     &    i, weight(1,i), weight(2,i), cff, sum
      enddo
      cff1=cff1/dble(ndtfast)
      cff2=cff2/dble(ndtfast*ndtfast)
      cff3=cff3/dble(ndtfast*ndtfast*ndtfast)
      shft=shft/dble(ndtfast)

      MPI_master_only write(*,'(3(/A,2F14.10),F14.10/A,2I4,F8.4)')
     &            '  Checking integrals (must be 1, 1)', cff,  sum,
     &            '        centroid  (must be 1, ~0.5)', cff1, shft,
     &            ' second and third moments (>~1, ~1)', cff2, cff3,
     &                        cff3-(3*cff2-2.),
     &            '     ndtfast, nfast, nfast/ndtfast ',
     &                  ndtfast, nfast, dble(nfast)/dble(ndtfast)

      if (cff2 .lt. 1.000000000001) then
        MPI_master_only write(*,'(/1x,A/)')
     &  'WARNING: unstable weights, reduce parameter "r".'
      endif
 
      MPI_master_only write(*,'(/A/A,1x,A,15x,A,7x,A/)')
     &  'Checking consistency of primary and secondary weights...',
     &                     'step',  'zeta_avr1', 'dt*divVBAR_avr2',
     &                     'zeta_avr1-dt*divVBAR_avr2'
 
      do j=1,nfast              !--> Loop over realizations
        next_kstp=1
        kstp=1                  ! set initial conditions 
        knew=1                  ! for each realization.
        zeta(1)=0.
        zeta(2)=0.
        do iif=1,nfast
          if (iif.eq.j) then
            DUon=1.
          else                  ! Set up delta function over
            DUon=0.             ! over ensamble of individual
          endif                 ! realizations j.

#  define FORW_BAK
#  ifdef FORW_BAK

          kstp=knew
          knew=kstp+1
          if (knew.gt.4) knew=1
          call step2d_FB_imitator (zeta, DUon, Zt_avg1, DU_avg2)

#  else
          kstp=next_kstp
          knew=3
          call step2d_LFAM3_imitator (zeta, DUon, Zt_avg1, DU_avg2)
 
          knew=3-kstp
          next_kstp=knew
          call step2d_LFAM3_imitator (zeta, DUon, Zt_avg1, DU_avg2)
#  endif
        enddo
        write(*,'(I4,2F24.18,F24.20)') j, Zt_avg1, dt*DU_avg2,
     &                                     Zt_avg1-dt*DU_avg2
      enddo
      stop
                                       ! Return values
      iif=1                            ! of time indices
      kstp=1                           ! back to their
      knew=1                           ! default initial
      return                           ! values, as set
      end                              ! by "init_scalars".
 
 
#  ifdef FORW_BAK
      subroutine step2d_FB_imitator (zeta, DUon, Zt_avg1, DU_avg2)
      implicit none
      real zeta(4), DUon, Zt_avg1, DU_avg2, zeta_new, cff1,cff2
#  include "param.h"
#  include "scalars.h"

      zeta(knew)=zeta(kstp) + dtfast*DUon

      cff1=weight(1,iif)
      cff2=weight(2,iif)
      if (FIRST_2D_STEP) then
        Zt_avg1=cff1*zeta(knew)
        DU_avg2=cff2*DUon
      else
        Zt_avg1=Zt_avg1+cff1*zeta(knew)
        DU_avg2=DU_avg2+cff2*DUon
      endif

      return
      end

#  else
 
      subroutine step2d_LFAM3_imitator (zeta, DUon, Zt_avg1, DU_avg2)
      implicit none
      logical PREDICTOR_2D_STEP
      integer krhs
      real zeta(3), DUon, Zt_avg1, DU_avg2, zeta_new
#  include "param.h"
#  include "scalars.h"
#  include "coupling.h"
 
      if (knew.eq.3) then
        krhs=kstp
        PREDICTOR_2D_STEP=.true.
      else
        krhs=3
        PREDICTOR_2D_STEP=.false.
      endif
!
! Imitate fast time averaging procedure. This module MUST BE
! consistent (to every minor detail, including startup process)
! with the fast-time-averaging procedure in step2d_tile (see
! file "step2D_LF_AM3.F").
!
 
      if (PREDICTOR_2D_STEP) then
        if (FIRST_2D_STEP) then
          Zt_avg1=0.
          DU_avg2=0.
        else
          Zt_avg1=Zt_avg1 + weight(1,iif-1)*zeta(krhs)
        endif
      else
        DU_avg2=DU_avg2 + weight(2,iif)*DUon
      endif
!
! Imitate the actual fast-time stepping for free-surface elevation.
! This module must be consistent with the time stepping in step2d,
! see file "step2d_LF_AM3.F".
!
      if (PREDICTOR_2D_STEP) then
 
      else
        zeta(knew)=zeta(kstp) + dtfast*DUon
      endif
!
! Finalize computation of barotropic mode averages.
!
      if (iif.eq.nfast .and. .not.PREDICTOR_2D_STEP) then
        Zt_avg1=Zt_avg1 + weight(1,iif)*zeta(knew)
      endif
      return
      end

#  endif   /* FORW_BAK */
# else
      return
      end
# endif   /* TEST_WEIGHTS */
#else
      subroutine set_weights_empty
      end
#endif     /* SOLVE3D */
 
