#ifndef LINEAR_DRAG_ONLY
        if (Zob > 0.) then
          do j=jstrV-1,jend
            do i=istrU-1,iend
              cff=sqrt( 0.333333333333*(
     &              u(i,j,1,nrhs)**2 +u(i+1,j,1,nrhs)**2
     &                     +u(i,j,1,nrhs)*u(i+1,j,1,nrhs)
     &               +v(i,j,1,nrhs)**2+v(i,j+1,1,nrhs)**2
     &                     +v(i,j,1,nrhs)*v(i,j+1,1,nrhs)
     &                                                 ))


! Canonical log-layer drag law formula valid for Zob << Hz only.   It
! should NEVER be used as it may cause instability in extremely shallow
! areas due to the singularity as Zob approaches Hz  (e.g., Yusuke PV4
! configuration in the extreme low-tide phase).

c**           rd(i,j)=rdrg + cff*(vonKar/log(Hz(i,j,1)/Zob))**2

! The following formula comes from the finite-volume interpretation of
! log-layer in the bottom-most grid box derived WITHOUT using Zob << Hz
! assumption. It covers the entire range of from Zob << Hz to Zob >> Hz
! and correctly asymptotes to the viscous limit in the latter extreme.
! The next formula after it is a simplified version which nevertheless
! yields correct asymptotic limits in both extremes, safely passing
! through Zob=Hz transition, and closely matching the upper formula
! there: setting Zob=Hz makes [vonKar/(2*ln 2-1)]^2 = [vonKar/0.386]^2
! upper vs. [vonKar/ln(3/2)]^2 = [vonKar/0.405]^2 lower.

c**           rd(i,j)=rdrg + cff*( vonKar/( (1.+Zob/Hz(i,j,1))
c**                                   *log(1.+Hz(i,j,1)/Zob) -1. ))**2

              rd(i,j)=rdrg + cff*(vonKar/log(1.+0.5*Hz(i,j,1)/Zob))**2


! A couple drag laws of academic interest.

c**           rd(i,j)=2.*Akv(i,j,0)/Hz(i,j,1)  !<-- laminar drag law

c**           rd(i,j)=rdrg + rdrg2*cff

c**           rd(i,j)=min(rd(i,j), Hz(i,j,1)/dt) !!! FOR_TEST_ONLY

# if !defined IMPLICIT_BOTTOM_DRAG
#  if !defined IMPLCT_NO_SLIP_BTTM_BC

        rd(i,j)=min(rd(i,j), 0.8*Hz(i,j,1)/dt) !<-- needed for stability

#  endif
# endif


c??           if (i==Lm/2 .and. j==Mm/16) then
c??             write(*,'(1x,A,3F16.10)') 'rd check',
c??     &             dt*rd(i,j)/Hz(i,j,1),
c??     &       dt*Akv(i,j,1)/(Hz(i,j,1)*Hz(i,j,1))
c??           endif

            enddo
          enddo
        else  !<-- Zob>0.
#endif
          do j=jstrV-1,jend
            do i=istrU-1,iend
              rd(i,j)=rdrg

              rd(i,j)=min(rd(i,j), 0.8*Hz(i,j,1)/dt) !!! FOR_TEST_ONLY

            enddo
          enddo
#ifndef LINEAR_DRAG_ONLY
        endif  !<-- Zob>0
#endif

! Save "rd" into shared array "r_D" for subsequent use in barotropic
! mode. Note that "rd" is computed with an extra row of points on the
! western and southern sides because this is needed for interpolation
! to U- and V-points.  Array "r_D" saved here inherits these extra
! rows into its periodic and/or computational margins, so there is no
! need for exchange call.

        call ext_copy_prv2shr_2d_tile (istr,iend,jstr,jend, rd,r_D)

