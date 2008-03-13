#include "cppdefs.h"
#ifdef SOLVE3D
 
c--#define CONST_TRACERS
 
      subroutine pre_step3d (tile)
      implicit none
      integer tile
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      call pre_step3d_tile (istr,iend,jstr,jend,  A3d(1,1), A3d(1,2),
     &                                                      A3d(1,3),
     &                        A2d(1,1), A2d(1,2), A2d(1,3),
     &                        A2d(1,1), A2d(1,2), A2d(1,3), A2d(1,4))
      return
      end
 
      subroutine pre_step3d_tile (istr,iend,jstr,jend, ru,rv,
     &                     Hz_half, FC,CF,DC, FX,FE,WORK,dTdz)
      implicit none
# include "param.h"
      integer istr,iend,jstr,jend,  itrc, indx, 
     &        imin,imax,jmin,jmax,  i,j,k,   is
      real ru(PRIVATE_2D_SCRATCH_ARRAY,N),    cff,
     &     rv(PRIVATE_2D_SCRATCH_ARRAY,N),    cff1,
     &     FC(PRIVATE_1D_SCRATCH_ARRAY,0:N),  cff2,
     &     CF(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     DC(PRIVATE_1D_SCRATCH_ARRAY,0:N),  gamma,
     &         FX(PRIVATE_2D_SCRATCH_ARRAY),  epsil,
     &         FE(PRIVATE_2D_SCRATCH_ARRAY),
     &       WORK(PRIVATE_2D_SCRATCH_ARRAY),
 
     &        dTdz(PRIVATE_2D_SCRATCH_ARRAY,2),
 
     &      Hz_half(PRIVATE_2D_SCRATCH_ARRAY,N)
 
      parameter (gamma=1./6., epsil=1.E-16)
# include "grid.h"
# include "ocean3d.h"
# include "coupling.h"
# include "forces.h"
# include "mixing.h"
# include "scalars.h"
# include "sources.h"
!
# include "compute_auxiliary_bounds.h"
!
! Preliminary step: initialize computations of the new time step
! 3D primitive variables.
!
! Start computation of the auxiliary tracer field.
!------ ----------- -- --- --------- ------ ------
! After this stage the resultant t(:,:,:,nnew,:) is time-centered 
! halfway between steps n and n+1. A high spatial order, centered, 
! non-conservative, but constancy preserving scheme is applied to 
! accomplish it.  The constancy preservation property is achieved 
! by introducing an artificial continuity equation [a''la Easter, 
! 1993], so that the actual advection scheme is still in the flux 
! form, however the new-time-step grid box height "Hz_half" (see 
! code segment just below) has no relation with the true grid-box
! heights determined from the updated free surface (not available 
! at this stage yet), but is rather chosen to absorb the
! 3D-divergence of mass fluxes FlxU, FlxV, and W consistently with
! time-stepping algorithm of this preliminary step (recall that
! computation of "Hz_half" mimics time step for tracers themselves).
! Later in this code the "Hz_half"-field plays the role of new-step
! "Hz" in the updates for tracer and 3D momenta to n+1/2, and it
! does not participate in any further computation.  Hence, division
! by "Hz_half" during computation of t(:,:,:,nnew,:) below is merely
! a mechanism to ensure constancy preservation, at the expense of
! loosing conservation property. 
!
! This is acceptable because t(:,:,:,n+1/2,:) fields will be used
! exclussively to compute the tracer fluxes during subsequent
! step3d_t operation, and the final values of t(i,j,k,n+1,itrc)
! alfer step3d_t will be computed in a flux-conservative manner.
! The overall time step will be both conservative and constancy
! preserving.
!
      indx=3-nstp
 
      if (FIRST_TIME_STEP) then           ! Advance Hz by
        cff=0.5*dt                        ! artificial continuity
        cff1=1.                           ! equation [Easter, 1993]
        cff2=0.
      else
        cff=(1.-gamma)*dt
        cff1=0.5+gamma
        cff2=0.5-gamma
      endif

c      write(*,'(A,2I3,3F16.8)') 'iic,indx,cff[1,2] =', iic,indx,cff,cff1,cff2

      do k=1,N
        do j=jstrV-1,jend
          do i=istrU-1,iend
            Hz_half(i,j,k)=cff1*Hz(i,j,k)+cff2*Hz_bak(i,j,k)
     &       -cff*pm(i,j)*pn(i,j)*( FlxU(i+1,j,k)-FlxU(i,j,k)
     &                             +FlxV(i,j+1,k)-FlxV(i,j,k)
     &                                   +W(i,j,k)-W(i,j,k-1)
     &                                                      )
          enddo
        enddo
      enddo
                                          ! Advance tracer fields
      do itrc=1,NT                        ! starting with applying 
        do k=1,N                          ! horizontal fluxes... 

#include "compute_horiz_tracer_fluxes.h"
 
          if (FIRST_TIME_STEP) then
            cff=0.5*dt
            do j=jstr,jend
              do i=istr,iend
                t(i,j,k,nnew,itrc)=Hz(i,j,k)*t(i,j,k,nstp,itrc)
     &                  -cff*pm(i,j)*pn(i,j)*( FX(i+1,j)-FX(i,j)
     &                                        +FE(i,j+1)-FE(i,j))
              enddo
            enddo

          else

            cff=(1.-gamma)*dt
            cff1=0.5+gamma
            cff2=0.5-gamma
            do j=jstr,jend
              do i=istr,iend
                t(i,j,k,nnew,itrc)=cff1*Hz(i,j,k)*t(i,j,k,nstp,itrc)
     &                         +cff2*Hz_bak(i,j,k)*t(i,j,k,indx,itrc)
     &                      -cff*pm(i,j)*pn(i,j)*( FX(i+1,j)-FX(i,j)
     &                                            +FE(i,j+1)-FE(i,j))
              enddo
            enddo
          endif
        enddo
      enddo
! 
! ...continue computation of the auxiliary tracer field: compute
! its change due to vertical advection.  Vertical advective fluxes
! require interpolation of tracer values to the verical grid-box
! interfaces (W-points). This can be is done by either using
! parabolic spline interpolation or, more simple local cubic
! polynomial [with or without monotonicity constraint; linear
! interpolation is considered obsolete, but the code is retained for
! reference].
!
      do j=jstr,jend                      ! this is just to share
        do k=1,N                          ! division by Hz_half
          do i=istr,iend                  ! among all the tracers
            DC(i,k)=1./Hz_half(i,j,k)     ! indexed as itrc
          enddo
        enddo
        do itrc=1,NT
 
# include "compute_vert_tracer_fluxes.h"
 
          if (FIRST_TIME_STEP) then
            cff=0.5*dt
          else
            cff=(1.-gamma)*dt
          endif
          do k=1,N
            do i=istr,iend
              t(i,j,k,nnew,itrc)=DC(i,k)*( t(i,j,k,nnew,itrc)
     &               -cff*pm(i,j)*pn(i,j)*(FC(i,k)-FC(i,k-1)))
#ifdef CONST_TRACERS
              t(i,j,k,nnew,itrc)=t(i,j,k,nstp,itrc)
#endif
            enddo
          enddo            !--> discard FC
        enddo   !<-- itrc  !--> discard DC
! 
!                       ! At entry scratch arrays ru,rv contain
! Momentum equations:   ! fully computed finite-volume r.h.s. for
!--------- ----------   ! 3D momentum equations, it is just added 
!                       ! in to the appropriate shared array. 
!
        do i=istrU,iend
          DC(i,0)=0.25*(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
        enddo
        if (FIRST_TIME_STEP) then
          do k=1,N
            do i=istrU,iend
              u(i,j,k,nnew)=( u(i,j,k,nstp)*(Hz(i,j,k)+Hz(i-1,j,k))
     &                                         +dt*DC(i,0)*ru(i,j,k)
     &                           )/(Hz_half(i,j,k)+Hz_half(i-1,j,k))
 
              u(i,j,k,indx)=0.5*(Hz(i,j,k)+Hz(i-1,j,k))
     &                                    *u(i,j,k,nstp)
            enddo
          enddo
        else
          cff=2.*(1.-gamma)*dt
          cff1=0.5+gamma
          cff2=0.5-gamma
          do k=1,N
            do i=istrU,iend
              u(i,j,k,nnew)=( cff1*u(i,j,k,nstp)*( Hz(i,j,k)
     &                                            +Hz(i-1,j,k))
     &                       +cff2*u(i,j,k,indx)*( Hz_bak(i,j,k)
     &                                         +Hz_bak(i-1,j,k))
     &                       +cff*DC(i,0)*ru(i,j,k)
     &                      )/(Hz_half(i,j,k)+Hz_half(i-1,j,k))
 
              u(i,j,k,indx)=0.5*(Hz(i,j,k)+Hz(i-1,j,k))
     &                                   *u(i,j,k,nstp)
            enddo
          enddo
        endif                    !--> discard DC
 
        if (j.ge.jstrV) then
          do i=istr,iend
            DC(i,0)=0.25*(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
          enddo
          if (FIRST_TIME_STEP) then
            do k=1,N
              do i=istr,iend
                v(i,j,k,nnew)=(v(i,j,k,nstp)*(Hz(i,j,k)+Hz(i,j-1,k))
     &                                          +dt*DC(i,0)*rv(i,j,k)
     &                            )/(Hz_half(i,j,k)+Hz_half(i,j-1,k))
 
                v(i,j,k,indx)=0.5*(Hz(i,j,k)+Hz(i,j-1,k))
     &                                     *v(i,j,k,nstp)
              enddo
            enddo
          else
            cff=2.*(1.-gamma)*dt
            cff1=0.5+gamma
            cff2=0.5-gamma
            do k=1,N
              do i=istr,iend
                v(i,j,k,nnew)=( cff1*v(i,j,k,nstp)*( Hz(i,j,k)
     &                                              +Hz(i,j-1,k))
     &                         +cff2*v(i,j,k,indx)*( Hz_bak(i,j,k)
     &                                           +Hz_bak(i,j-1,k))
     &                         +cff*DC(i,0)*rv(i,j,k)
     &                        )/(Hz_half(i,j,k)+Hz_half(i,j-1,k))
 
                v(i,j,k,indx)=0.5*(Hz(i,j,k)+Hz(i,j-1,k))
     &                                     *v(i,j,k,nstp)
              enddo
            enddo
          endif                 !--> discard DC
        endif
      enddo     !<-- j
 
      call u3dbc_tile (istr,iend,jstr,jend, DC)
      call v3dbc_tile (istr,iend,jstr,jend, DC)

# ifdef PSOURCE
      do is=1,Nsrc           ! Apply point mass sources associated
        i=Isrc(is)           ! with river runoff simulations.
        j=Jsrc(is)
        if (istrR.le.i .and. i.le.iendR .and. 
     &      jstrR.le.j .and. j.le.jendR) then
          if (Dsrc(is).eq.0) then
            do k=1,N
              u(i,j,k,nnew)=2.*Qsrc(is,k)/( dn_u(i,j)*(
     &                       z_w(i-1,j,k)-z_w(i-1,j,k-1)
     &                      +z_w(i  ,j,k)-z_w(i  ,j,k-1)
     &                                                ))
            enddo
          else
            do k=1,N
              v(i,j,k,nnew)=2.*Qsrc(is,k)/( dm_v(i,j)*(
     &                       z_w(i,j-1,k)-z_w(i,j-1,k-1)
     &                      +z_w(i,j  ,k)-z_w(i,j  ,k-1)
     &                                                ))
            enddo
          endif
        endif
      enddo 
# endif
!
! WARNING: Preliminary time step for 3D momentum equitions is not
! complete after this moment: the computed fields u,v(i,j,k,nnew)
! have wrong vertical integrals, which will be corrected later
! after computation of barotropic mode.
!
! Set PHYSICAL lateral boundary conditions for tracer field.
!
      do itrc=1,NT
        call t3dbc_tile (istr,iend,jstr,jend, itrc, WORK)
# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
        call exchange_r3d_tile (istr,iend,jstr,jend,
     &                          t(START_2D_ARRAY,1,nnew,itrc))
# endif
      enddo
      return
      end
#else
      subroutine pre_step3d_empty
      end
#endif  /* SOLVE3D */
