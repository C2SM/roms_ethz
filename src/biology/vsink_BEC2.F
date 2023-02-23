
#include "cppdefs.h"

#if defined BIOLOGY_BEC2 && defined USE_EXPLICIT_VSINK

      subroutine vert_sinking_bec2(Istr,Iend,j)

         implicit none

#include "param.h"
#include "param_bec2.h"
#include "scalars.h"
#include "forces.h"
#include "ecosys_bec2.h"
#include "ocean3d.h"
! For rmask:
#include "grid.h"

         integer, intent(in) :: Istr, Iend, j

         ! Local variables:
         integer i, k, iB, iflux
         real cff, cff1, cff2, cff6, cu, aL, aR, dtdays
         real FC(0:N), SB(N,nsink), dSB(0:N,nsink), wSB(nsink)
         real ColumnMassOld(nsink), ColumnMassNew(nsink)
         real ThisVSinkFlux(N,nsink), LastVSinkFlux


         dtdays=dt/(24.*3600.)  ! time step as fraction of day.
!
! Sinking velocities:
!
         wSB(1)  = wDustHard
         wSB(2)  = wPOCHard
         wSB(3)  = wPCaCO3Hard
         wSB(4)  = wPSiO2Hard
         wSB(5)  = wPIronHard
         wSB(6)  = wDustSoft
         wSB(7)  = wPOCSoft
         wSB(8)  = wPCaCO3Soft
         wSB(9)  = wPSiO2Soft
         wSB(10) = wPIronSoft
!
! Vertical sinking: Vertical advection algorithm based on monotonic,
! continuous conservative parabolic splines.
!
         do i=istr,iend
         k_loop4: do k=1,N
           do iB=1,nsink
             SB(k,iB)=tracer(i,j,k,dusthard_ind_t+iB-1)
           end do
         end do k_loop4

         iB_loop: do iB=1,nsink

! Part (i): Construct parabolic splines: compute vertical derivatives
! of the fields SB. The derivatives are located at W-points;
! Neumann boundary conditions are assumed on top and bottom.
!
           dSB(0,iB)=0.
           FC(0)=0.
           cff6=6.
           k_loop5: do k=1,N-1
             cff=1./(2.*Hz(i,j,k+1)+Hz(i,j,k)*(2.-FC(k-1)))
             FC(k)=cff*Hz(i,j,k+1)
             dSB(k,iB)=cff*(cff6*(SB(k+1,iB)-SB(k,iB))
     &                             -Hz(i,j,k)*dSB(k-1,iB))
           end do k_loop5
           dSB(N,iB)=0.
           k_loop6: do k=N-1,1,-1     !<-- irreversible
             dSB(k,iB)=dSB(k,iB)-FC(k)*dSB(k+1,iB)
           end do k_loop6
!
! Part (ii): Convert dSB [which are now vertical derivatives
! of fields SB at the grid box interfaces] into field values
! at these interfaces, assuming parabolic profiles within each grid
! box. Restrict these values to lie between bounds determined from
! box-averaged values of grid boxes adjscent from above and below.
! (This restriction is part of PPM-like monotonization procedure.)
!
           cff=1./3.
           dSB(0,iB)=SB(1,iB) !-cff*Hz(1)*(dSB(0,iB)+0.5*dSB(1,iB))
           dSB(N,iB)=SB(N,iB) !+cff*Hz(N)*(dSB(N,iB)+0.5*dSB(N-1,iB))
           k_loop7: do k=2,N          !<-- irreversible
             dSB(k-1,iB)=SB(k,iB)
     &                  -cff*Hz(i,j,k)*(0.5*dSB(k,iB)+dSB(k-1,iB))
             dSB(k-1,iB)=max(dSB(k-1,iB),min(SB(k-1,iB),SB(k,iB)))
             dSB(k-1,iB)=min(dSB(k-1,iB),max(SB(k-1,iB),SB(k,iB)))
           end do k_loop7
!  
! Part (iii): Convert dSB into flux-integrated values,
! complete PPM flux limiting. This procedure starts from assigning
! Left and Right (aR,aL) values of the interpolating parabolae, then
! monotonicity conditions are checked and aL,aR are modified to fit.
! Overall, from this moment and further on it follows Colella--
! --Woodward, 1984 bombmaking code almost exactly.
!
           k_loop8: do k=1,N           !<-- irreversible
             FC(k)=dtdays/Hz(i,j,k)
             aR=dSB(k,iB)
             aL=dSB(k-1,iB)
             cff1=(aR-aL)*6.*(SB(k,iB)-.5*(aR+aL))
             cff2=(aR-aL)**2
             if ((aR-SB(k,iB))*(SB(k,iB)-aL).lt.0.) then
               aL=SB(k,iB)
               aR=SB(k,iB)
             elseif (cff1.gt.cff2) then
               aL=3.*SB(k,iB)-2.*aR
             elseif (cff1.lt.-cff2) then
               aR=3.*SB(k,iB)-2.*aL
             endif
             cu=wSB(iB)*FC(k)
             dSB(k-1,iB)=SB(k,iB)-(1.-cu)*(.5*(aR-aL)-(.5*(aR+aL)
     &                                   -SB(k,iB) )*(1.-2.*cu))
           end do k_loop8
           dSB(N,iB)=0.   ! Set no-flux boundary conditions at top.
!
! Apply fluxes:
!
           k_loop9: do k=1,N
             SB(k,iB)=SB(k,iB)+wSB(iB)*FC(k)*(dSB(k,iB)-dSB(k-1,iB))
           end do k_loop9
         end do iB_loop
!
! Compute vertical sink fluxes: also update tracer array with new
! concentrations:
!
         VSinkFlux(i,j,:,:) = 0.0
         do iB=1,nsink
            ColumnMassOld(iB) = 0.0
            ColumnMassNew(iB) = 0.0
            do k=1,N
               ColumnMassOld(iB) = ColumnMassOld(iB) +
     &           tracer(i,j,k,dusthard_ind_t+iB-1)*Hz(i,j,k)
               ThisVSinkFlux(k,iB) = tracer(i,j,k,dusthard_ind_t+iB-1)-SB(k,iB)
               tracer(i,j,k,dusthard_ind_t+iB-1) = SB(k,iB)
               ColumnMassNew(iB) = ColumnMassNew(iB) +
     &           tracer(i,j,k,dusthard_ind_t+iB-1)*Hz(i,j,k)
            end do
            VSinkFlux(i,j,0,iB) = ( ColumnMassNew(iB) -
     &          ColumnMassOld(iB) ) / dt
# ifdef MASKING
     &          * rmask(i,j)
# endif
            LastVSinkFlux = VSinkFlux(i,j,0,iB)
            do k = 1,N
               LastVSinkFlux = LastVSinkFlux + 
     &             ( ThisVSinkFlux(k,iB) * Hz(i,j,k) ) / dt
               VSinkFlux(i,j,k,iB) = LastVSinkFlux
# ifdef MASKING
     &             * rmask(i,j)
# endif
            end do ! k
         end do ! iB
         end do ! i

         return
         end subroutine vert_sinking_bec2

#else /* BIOLOGY_BEC2 && USE_EXPLICIT_VSINK */
      subroutine vert_sinking_bec2()
      return
      end
#endif /* BIOLOGY_BEC2 && USE_EXPLICIT_VSINK */
