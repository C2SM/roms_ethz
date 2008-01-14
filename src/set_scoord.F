#include "cppdefs.h"
#ifdef SOLVE3D
                                    ! Setup S-coordinate system:
      subroutine set_scoord         ! input:  theta_s, theta_b, hc
      implicit none                 ! output:   Cs_w(0:N), Cs_r(1:N) 
# include "param.h"
# include "scoord.h"
# include "scalars.h"
# include "comm_vars.h"
      integer k                       ! Compute vertical stretching
      real ds,sc, cff,cff1,cff2       ! curves at W- and RHO-points,
                                       
# ifndef NEW_S_COORD
      if (hc.le.hmin) then            !    -1 < Cs_r,Cs_w < 0, 
# endif
        ds=1.D0/dble(N)                  ! then print z-coordinates 
        if (theta_s .gt. 0.D0) then      ! of vertical S-levels over 
          cff1=1.D0/sinh(theta_s)        !  three representative 
          cff2=0.5D0/tanh(0.5D0*theta_s) ! depths: minumum, maximum 
          Cs_w(N)=0.D0                   ! and half-way over slope.
          do k=N-1,1,-1
            sc=ds*dble(k-N)
            Cs_w(k)=(1.D0-theta_b)*cff1*sinh(theta_s*sc)
     &        +theta_b*(cff2*tanh(theta_s*(sc+0.5D0))-0.5D0)
          enddo
          Cs_w(0)=-1.D0

          do k=1,N
            sc=ds*(dble(k-N)-0.5D0)
            Cs_r(k)=(1.D0-theta_b)*cff1*sinh(theta_s*sc)
     &         +theta_b*(cff2*tanh(theta_s*(sc+0.5D0))-0.5D0)
          enddo
        else
                                    ! When theta_s=0, set up
          Cs_w(N)=0.D0                   ! a uniformly-spaced sigma
          do k=N-1,1,-1                  ! coordinate in vertical
            Cs_w(k)=ds*dble(k-N)        ! direction.
          enddo
          Cs_w(0)=-1.D0
          do k=1,N
            Cs_r(k)=ds*(dble(k-N)-0.5D0)
          enddo
        endif

        MPI_master_only write(stdout,'(/1x,A/,/1x,A,10x,A/)')
     &                         'Vertical S-coordinate System:',
     &                         'level   S-coord     Cs-curve',
     &                         'at_hc  over_slope     at_hmax'
        do k=N,0,-1
# ifdef NEW_S_COORD
          sc=ds*dble(k-N)
          cff=0.5*(hmax+hmin)
          cff1=cff *(hc*sc + cff *Cs_w(k))/(hc +cff)
          cff2=hmax*(hc*sc + hmax*Cs_w(k))/(hc+hmax)
          cff= hc  *(hc*sc + hc  *Cs_w(k))/(hc + hc)
# else
          cff=hc*ds*(k-N)
          cff1=cff +0.5D0*(hmax+hc)*Cs_w(k)
          cff2=cff +      (hmax-hc)*Cs_w(k)
# endif
          MPI_master_only write(stdout,'(I6,2F12.7,4x,3F12.3)')
     &                    k, ds*(k-N), Cs_w(k), cff, cff1,cff2
        enddo
# ifndef NEW_S_COORD 
      else
        write(stdout,'(/1x,2A,F7.2/8x,A,F7.2/)') 'ERROR: ',
     &    'Specified S-coordinate critical depth   hc   =',  hc,
     &    'exceeds minimum unmasked topography.    hmin =',  hmin
        may_day_flag=8
      endif
# endif
      return
      end
#else
      subroutine set_scoord_empty
      end
#endif /* SOLVE3D */
 
