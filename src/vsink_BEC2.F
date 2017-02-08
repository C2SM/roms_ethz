
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

!
! Apply forcing fluxes to dust:
!
         ! MF
         !mpi_master_only write(*,*) 'Start of vert_sinking_bec2...'

         do i=istr,iend
            t(i,j,N,nnew,iDUSTHARD) = t(i,j,N,nnew,iDUSTHARD) 
     &                                + dt*dust_gamma*dust(i,j)/Hz(i,j,N)
            t(i,j,N,nnew,iDUSTSOFT) = t(i,j,N,nnew,iDUSTSOFT) 
     &                                + dt*(1-dust_gamma)*dust(i,j)/Hz(i,j,N)
         end do
!
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
             SB(k,iB)=t(i,j,k,nnew,iDUSTHARD+iB-1)
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
     &           t(i,j,k,nnew,iDUSTHARD+iB-1)*Hz(i,j,k)
               ThisVSinkFlux(k,iB) = t(i,j,k,nnew,iDUSTHARD+iB-1)-SB(k,iB)
               t(i,j,k,nnew,iDUSTHARD+iB-1) = SB(k,iB)
               ColumnMassNew(iB) = ColumnMassNew(iB) +
     &           t(i,j,k,nnew,iDUSTHARD+iB-1)*Hz(i,j,k)
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

         !---------------------------------------------------------------------
         ! Compute benthic processes
         !---------------------------------------------------------------------

         ! MF
         !mpi_master_only write(*,*) 'Calling vert_sinking_sediments...'

         ! MF: Test without calling benthic processes
         call vert_sinking_sediments(istr,iend,j)

         ! MF
         !mpi_master_only write(*,*) 'Out of vert_sinking_sediments...'



         !-----------------------------------------------------------------------
         !  JUST TO TEST WITHOUT CALLING BENTHIC PROCESSES !!!!
         !-----------------------------------------------------------------------

# ifdef 0         
         P_iron_sed_loss(istr:iend) = c0
         POC_sed_loss(istr:iend) = c0
         P_CaCO3_sed_loss(istr:iend) = c0
         P_SiO2_sed_loss(istr:iend) = c0
         dust_sed_loss(istr:iend) = c0
         SED_DENITRIF(istr:iend) = c0
         OTHER_REMIN(istr:iend) = c0
# endif

         return
         end subroutine vert_sinking_bec2

!-------------------------------------------------------------------------
!
!     ************   SUBROUTINE VERT_SINKING_SEDIMENTS   *****************
!    
!-------------------------------------------------------------------------
        
        ! MF (Feb2017): 
        ! Merge benthic processes from compute_particulate_terms 
        ! (implicit sinking) to the new explicit sinking routine.

        subroutine vert_sinking_sediments(Istr,Iend,j)

        implicit none
#include "param.h"
#include "ecosys_bec2.h"
#include "param_bec2.h"
#include "ocean3d.h"
#include "scalars.h"

        integer, intent(in) :: Istr, Iend, j
        
        ! Local variables
        integer i, k, iB
        real, dimension(istr:iend) ::
     &   flux_oxidated, ! carbon oxidation rate in sediment
     &   FESEDFLUX,     ! sedimentary Fe inputs
     &   dust_remin    ! remineralization of dust (base units/m^3/sec)
        real
     &   flux, flux_alt,    ! temp variables used to update sinking flux
     &   dz_loc, dzr_loc,   ! Hz and its inverse at a particular i,j,k location
     &   O2_loc, NO3_loc

        ! MF
        !mpi_master_only write(*,*) 'Start of vert_sinking_sediments...'

        do k = 1,N
           ! Compute remineralization of particulate pools:
           ! The parameters param_dusthard_remin etc. are in 1/day, so convert to 1/s.
           dusthard_remin(istr:iend,k) = param_dusthard_remin * dps * tracer(istr:iend,j,k,dusthard_ind_t)
           POChard_remin(istr:iend,k) = param_pochard_remin * dps * tracer(istr:iend,j,k,pochard_ind_t)
           P_CaCO3hard_remin(istr:iend,k) = param_pcaco3hard_remin * dps * tracer(istr:iend,j,k,pcaco3hard_ind_t)
           P_SiO2hard_remin(istr:iend,k) = param_psio2hard_remin *dps * tracer(istr:iend,j,k,psio2hard_ind_t)
           P_ironhard_remin(istr:iend,k) = param_pironhard_remin * dps * tracer(istr:iend,j,k,pironhard_ind_t)
           dustsoft_remin(istr:iend,k) = param_dustsoft_remin * dps * tracer(istr:iend,j,k,dustsoft_ind_t)
           POCsoft_remin(istr:iend,k) = param_pocsoft_remin * dps * tracer(istr:iend,j,k,pocsoft_ind_t)
           P_CaCO3soft_remin(istr:iend,k) = param_pcaco3soft_remin * dps * tracer(istr:iend,j,k,pcaco3soft_ind_t)
           P_SiO2soft_remin(istr:iend,k) = param_psio2soft_remin * dps * tracer(istr:iend,j,k,psio2soft_ind_t)
           P_ironsoft_remin(istr:iend,k) = param_pironsoft_remin * dps * tracer(istr:iend,j,k,pironsoft_ind_t)
           ! Add hard and soft components
           POC_remin(istr:iend,k) = POChard_remin(istr:iend,k) + POCsoft_remin(istr:iend,k)
           P_iron_remin(istr:iend,k) = P_ironhard_remin(istr:iend,k)+ P_ironsoft_remin(istr:iend,k)
           P_SiO2_remin(istr:iend,k) = P_SiO2hard_remin(istr:iend,k) + P_SiO2soft_remin(istr:iend,k)
           P_CaCO3_remin(istr:iend,k) = P_CaCO3hard_remin(istr:iend,k) + P_CaCO3soft_remin(istr:iend,k)
           dust_remin(istr:iend) = dusthard_remin(istr:iend,k) + dustsoft_remin(istr:iend,k)
           
        !-----------------------------------------------------------------------
        !  Compute iron release from dust remin/dissolution
        !
        !  dust remin gDust = 0.035 / 55.847 * 1.0e9 = 626712.0 nmolFe
        !                      gFe     molFe     nmolFe
        !  Also add in Fe source from sediments if applicable to this cell.
        !-----------------------------------------------------------------------
           
           P_Iron_remin(istr:iend,k) = P_Iron_remin(istr:iend,k) + dust_remin(istr:iend) * dust_to_Fe


           ! MF
           !mpi_master_only write(*,*) 'After remin, k=', k

#  ifdef BEC2_DIAG
           bec2_diag_3d(istr:iend,j,k,pironhardremin_idx_t) = P_Ironhard_remin(istr:iend,k)
           bec2_diag_3d(istr:iend,j,k,caco3hardremin_idx_t) = P_CaCO3hard_remin(istr:iend,k)
           bec2_diag_3d(istr:iend,j,k,sio2hardremin_idx_t) = P_SiO2hard_remin(istr:iend,k)
           bec2_diag_3d(istr:iend,j,k,pochardremin_idx_t) = POChard_remin(istr:iend,k)
           bec2_diag_3d(istr:iend,j,k,dusthardremin_idx_t) = dusthard_remin(istr:iend,k)
           bec2_diag_3d(istr:iend,j,k,pironsoftremin_idx_t) = P_Ironsoft_remin(istr:iend,k)
           bec2_diag_3d(istr:iend,j,k,caco3softremin_idx_t) = P_CaCO3soft_remin(istr:iend,k)
           bec2_diag_3d(istr:iend,j,k,sio2softremin_idx_t) = P_SiO2soft_remin(istr:iend,k)
           bec2_diag_3d(istr:iend,j,k,pocsoftremin_idx_t) = POCsoft_remin(istr:iend,k)
           bec2_diag_3d(istr:iend,j,k,dustsoftremin_idx_t) = dustsoft_remin(istr:iend,k)
#  endif

#  if 0 /* STILL DONE IN ecosys_set_interior */
        !-----------------------------------------------------------------------
        !  Collect tendencies for tracers (in first implementation, this
        !  was done at the very end of ecosystem_set_interior, but since
        !  the corresponding fields do not change in between, I moved it
        !  here. Even though particulate production happens after this
        !  routine in ecosystem_set_interior, this way all the
        !  particulate treatment is consistent and basically lags the
        !  production by one time step!) MF
        !-----------------------------------------------------------------------

           DTRACER_MODULE(istr:iend,j,k,dusthard_ind_t) = -dusthard_remin(istr:iend,k)
           DTRACER_MODULE(istr:iend,j,k,pochard_ind_t) = POC_gamma*POC_prod - POChard_remin(istr:iend,k)
           DTRACER_MODULE(istr:iend,j,k,pcaco3hard_ind_t) = P_CaCO3_gamma*P_CaCO3_prod - P_CaCO3hard_remin(istr:iend,k)
           DTRACER_MODULE(istr:iend,j,k,psio2hard_ind_t) = P_SiO2_gamma*P_SiO2_prod - P_SiO2hard_remin(istr:iend,k)
           DTRACER_MODULE(istr:iend,j,k,pironhard_ind_t) = P_iron_gamma*P_iron_prod - P_ironhard_remin(istr:iend,k)
           DTRACER_MODULE(istr:iend,j,k,dustsoft_ind_t) = -dustsoft_remin(istr:iend,k)
           DTRACER_MODULE(istr:iend,j,k,pocsoft_ind_t) = (c1-POC_gamma)*POC_prod - POCsoft_remin(istr:iend,k)
           DTRACER_MODULE(istr:iend,j,k,pcaco3soft_ind_t) = (c1-P_CaCO3_gamma)*P_CaCO3_prod - 
     &                                        P_CaCO3soft_remin(istr:iend,k)
           DTRACER_MODULE(istr:iend,j,k,psio2soft_ind_t) = (c1-P_SiO2_gamma)*P_SiO2_prod - P_SiO2soft_remin(istr:iend,k)
           DTRACER_MODULE(istr:iend,j,k,pironsoft_ind_t) = (c1-P_iron_gamma)*P_iron_prod - P_ironsoft_remin(istr:iend,k)
#  endif
           
        if (k==1) then

           !-----------------------------------------------------------------------
           !  initialize loss to sediments = 0 and local copy of percent sed
           !-----------------------------------------------------------------------
              
           ! MF
           !mpi_master_only write(*,*) 'Init arrays...'
           
           P_iron_sed_loss(istr:iend) = c0
           POC_sed_loss(istr:iend) = c0
           P_CaCO3_sed_loss(istr:iend) = c0
           P_SiO2_sed_loss(istr:iend) = c0
           dust_sed_loss(istr:iend) = c0
           SED_DENITRIF(istr:iend) = c0
           OTHER_REMIN(istr:iend) = c0

        do i=istr,iend
           ! MF
           !mpi_master_only write(*,*) 'Entering i loop... i=',i
           !mpi_master_only write(*,*) 'LANDMASK(i,j)=',LANDMASK(i,j)

           if (LANDMASK(i,j)) then

           
           !-----------------------------------------------------------------------
           !  Bottom Sediments Cell: 
           !  Compute sedimentary burial and denitrification N losses.
           !  Using empirical relations from Bohlen et al., 2012 
           !   (doi:10.1029/2011GB004198) for Sed Denitrification
           !  OTHER_REMIN estimates organic matter remineralized in the sediments
           !   by the processes other than oxic remin and denitrification (SO4 and CO2, etc..)
           !   based on Soetaert et al., 1996, varies between 10% and 50%
           !   0.4_r8 is a coefficient with units mmolC/cm2/yr sinking flux,
           !  OTHER_REMIN is 50% above this high flux value,
           !   In special case where bottom O2 has been depleted to < 1.0uM,
           !   all sedimentary remin is due to DENITRIFICATION + OTHER_REMIN
           !  POC burial from Dunne et al. 2007 (doi:10.1029/2006GB002907),
           !   maximum of 80% burial efficiency imposed
           !  Bsi preservation in sediments based on
           !   Ragueneau et al. 2000 (doi:10.1016/S0921-8181(00)00052-7)
           !  Calcite is preserved in sediments above the lysocline, dissolves
           !   below.
           !  Here a constant depth is used for lysocline.
           !-----------------------------------------------------------------------
              
              ! Get thickness of bottom layer and inverse
              dz_loc = Hz(i,j,k)
              dzr_loc = c1 / dz_loc
              ! Get current tracer values:
              O2_loc = max(c0,tracer(i,j,k,o2_ind_t))
              NO3_loc = max(c0,tracer(i,j,k,no3_ind_t))
              
              ! MF
              !mpi_master_only write(*,*) 'Before POC flux...'

              ! Flux out of bottom layer (negative in explicit sinking case)
              flux = -(VSinkFlux(i,j,k-1,iPOCSOFT_VSink)+VSinkFlux(i,j,k-1,iPOCHARD_VSink)) ! mmol C/m^2/s
              
              ! MF
              !mpi_master_only write(*,*) 'After POC flux... flux(i,j):', flux

              if (flux > c0) then 
                 flux_alt = flux*spd ! convert to mmol C/m^2/day

                 ! POC Sediment loss
                 POC_sed_loss(i) = flux * min(0.8, parm_POMbury
     &               * (0.013 + 0.53 * flux_alt*flux_alt / (7.0 + flux_alt)**2))

                 ! CN: set denitr to zero if NO3<=0 in bottom water (to not
                 ! further deplete already not existing NO3)
                 ! if this works: actually, this might still take out too
                 ! much NO3 if NO3_BW is just slightly above zero. Include
                 ! something like "if SED_DENITRIF > NO3_loc,
                 ! reduce SED_DENITRIF to available NO3"
                 ! check: is some NO3 threshold implemented for water col
                 ! denitrif? could also cause neg NO3...
                 ! FOR NOW: NO3min=5 (see Simon's draft!! he uses 2, but said
                 ! it's kind of arbitrary...), below this
                 ! sed_denitr is reduced! same for water column denitrf!
                 
                 ! CN (Oct 2015): constrain SED_DENITRIF by what is left to
                 ! be remineralized after POC_sed_loss
                 
                 if (NO3_loc < parm_sed_denitrif_NO3_limit) then
                    SED_DENITRIF(i) = dzr_loc * min(flux *(0.06 + 0.19 *
     &                 0.99**(O2_loc-NO3_loc)),
     &                 flux-POC_sed_loss(i)*denitrif_C_N)
                    SED_DENITRIF(i) = 1/parm_sed_denitrif_NO3_limit*NO3_loc*SED_DENITRIF(i)                        
                 else
                    SED_DENITRIF(i)= dzr_loc * min(flux * (0.06 + 0.19 *
     &                 0.99**(O2_loc-NO3_loc)),
     &                 flux-POC_sed_loss(i)*denitrif_C_N)
                 endif


#ifdef BEC2_DIAG
# ifdef BEC2_DIAG_USER
                 if (bec2_diag_2d_l(seddenitrif_idx_t))
     &               bec2_diag_2d(i,j,bec2_diag_2d_idx(seddenitrif_idx_t)) = SED_DENITRIF(i)
# else
                 bec2_diag_2d(i,j,seddenitrif_idx_t) = SED_DENITRIF(i)
                 bec2_diag_2d(i,j,fluxtosed_idx_t) = flux
                 bec2_diag_2d(i,j,pocsedloss_idx_t) = POC_sed_loss(i)
# endif
#endif
                 flux_alt = flux*1e-4*spd*365.0 ! convert to mmol C/cm^2/year
                 OTHER_REMIN(i) = dzr_loc
     &                * min(min(0.1 + flux_alt,0.5) * (flux - POC_sed_loss(i)),
     &                (flux - POC_sed_loss(i) - (SED_DENITRIF(i)*dz_loc*denitrif_C_N)))

           !--------------------------------------------------------------------
           !        if bottom water O2 is depleted, assume all remin is
           !        denitrif + other               
           !--------------------------------------------------------------------

                 if (O2_loc < c1) then
                     OTHER_REMIN(i) = dzr_loc *
     &                 (flux - POC_sed_loss(i) - (SED_DENITRIF(i)*dz_loc*denitrif_C_N))
                 endif
  
#ifdef BEC2_DIAG
                 bec2_diag_2d(i,j,otherremin_idx_t) = OTHER_REMIN(i)
#endif

# ifdef CONSTANT_FE_SEDFLUX

           ! Add diffusive iron flux if depth < 1100.0m, based on
           ! Johnson et al.1999, value of 5.0 umolFe/m2/day.
           !
           ! 2.0 umolFe/m2/day,  2.3148e-6  nmolFe/cm2/sec
           ! 2.0 umolFe/m2/day,  2.3148e-3  mmolFe/m2/sec
           !-----------------------------------------------------------------
           ! CN: take value from Moore 2004 (in mmolFe/m2/sec,see old BEC code)
                 if (z_r(i,j,k) < 1100.) then
                    FESEDFLUX(i) = 2.0*0.001/spd
                 endif
#  else
                 ! CN (Sep 2015): FESEDFLUX parametrization from Dale 2015
                 ! carbon oxidation rate in mmol m-2 s-1 
                 flux_oxidated(i) = flux - POC_sed_loss(i)
                 
                 ! convert from mmol m-2 s-1 to mmol m-2 d-1
                 flux_oxidated(i) = flux_oxidated(i) * spd
                 
                 ! constant 170 in umol m-2 d-1, flux_oxidated in mmol m-2
                 ! day-1, o2 in muM
                 if (flux_oxidated(i) > 0) then
                    FESEDFLUX(i) = 170 * tanh(flux_oxidated(i)/O2_loc)
                 endif
                 ! convert from umol m-2 d-1 to mmol m-2 s-1
                 FESEDFLUX(i) = FESEDFLUX(i)*0.001/spd     
# endif
# ifdef BEC2_DIAG
                 bec2_diag_2d(i,j,fesedflux_idx_t) = FESEDFLUX(i)
# endif
              endif  ! flux > c0

              ! SiO2 Sediment loss (flux negative in eclixit case)
              flux = -(VSinkFlux(i,j,k-1,iPSIO2SOFT_VSink)+VSinkFlux(i,j,k-1,iPSIO2HARD_VSink))
              flux_alt = flux*spd ! convert to mmol/m^2/day
              ! first compute burial efficiency, then compute loss to sediments
              if (flux_alt > c1) then
                 P_SiO2_sed_loss(i) = 0.3 * flux_alt - 0.06
              else
                 P_SiO2_sed_loss(i) = 0.04
              endif
              P_SiO2_sed_loss(i) = flux * parm_BSIbury * P_SiO2_sed_loss(i)

# ifdef BEC2_DIAG
              bec2_diag_2d(i,j,sio2fluxtosed_idx_t) = flux
              bec2_diag_2d(i,j,sio2sedloss_idx_t) = P_SiO2_sed_loss(i)
# endif

              ! CaCO3 Sediment loss
              flux = c0  ! set flux back to zero
              if (-z_w(i,j,k) < 3300.0) then
                 flux = -(VSinkFlux(i,j,k-1,iPCACO3SOFT_VSink)+VSinkFlux(i,j,k-1,iPCACO3HARD_VSink))
                 P_CaCO3_sed_loss(i) = flux
              endif

# ifdef BEC2_DIAG
              bec2_diag_2d(i,j,caco3fluxtosed_idx_t) = flux
              bec2_diag_2d(i,j,caco3sedloss_idx_t) = P_CaCO3_sed_loss(i)
# endif
           !----------------------------------------------------------------------------------
           !  Update remin fluxes, accounting for sediment losses.
           !  flux used to hold sinking fluxes before update.
           !----------------------------------------------------------------------------------
            
              ! MF: in the explicit sinking, negative values indicate sinking fluxes
       
              flux = -(VSinkFlux(i,j,k-1,iPCACO3SOFT_VSink)+VSinkFlux(i,j,k-1,iPCACO3HARD_VSink))
              if (flux > c0) then
                 P_CaCO3_remin(i,k) = P_CaCO3_remin(i,k)
     &             + ((flux - P_CaCO3_sed_loss(i)) * dzr_loc)
              endif

              flux = -(VSinkFlux(i,j,k-1,iPSIO2SOFT_VSink)+VSinkFlux(i,j,k-1,iPSIO2HARD_VSink))
              if (flux > c0) then
                 P_SiO2_remin(i,k) = P_SiO2_remin(i,k)
     &             + ((flux - P_SiO2_sed_loss(i)) * dzr_loc)
              endif

              flux = -(VSinkFlux(i,j,k-1,iPOCSOFT_VSink)+VSinkFlux(i,j,k-1,iPOCHARD_VSink))
              if (flux > c0) then
                 POC_remin(i,k) = POC_remin(i,k)
     &             + ((flux - POC_sed_loss(i)) * dzr_loc)
              endif

           !-----------------------------------------------------------------------
           !   Remove all Piron and dust that hits bottom, sedimentary Fe source 
           !        accounted for by FESEDFLUX elsewhere.
           !-----------------------------------------------------------------------

              ! Fe Sediment loss
              flux = c0  ! set flux back to zero
              flux = -(VSinkFlux(i,j,k-1,iPIRONSOFT_VSink)+VSinkFlux(i,j,k-1,iPIRONHARD_VSink))
              ! MF: in the explicit sinking, negative values indicate sinking fluxes
              if (flux > c0) then
                 P_iron_sed_loss(i) = flux
              endif
 
              ! Dust Sediment loss
              dust_sed_loss(i) = -(VSinkFlux(i,j,k-1,iDUSTSOFT_VSink)+VSinkFlux(i,j,k-1,iDUSTHARD_VSink))

# ifdef BEC2_DIAG
              bec2_diag_2d(i,j,pironfluxtosed_idx_t) = flux
              bec2_diag_2d(i,j,dustfluxtosed_idx_t) = dust_sed_loss(i)
# endif

           !-----------------------------------------------------------------------
           !   Bottom layer: set all outgoing fluxes to 0.0
           !-----------------------------------------------------------------------

              do iB=1,nsink
                 VSinkFlux(i,j,k-1,iB) = c0
              end do 

              ! Update Fe Remin
              P_Iron_remin(i,k) = P_Iron_remin(i,k)
     &           + (FESEDFLUX(i) * dzr_loc)

           end if ! LANDMASK
        end do ! i
        end if ! k==1

# ifdef BEC2_DIAG
#  ifdef BEC2_DIAG_USER
         if (bec2_diag_3d_l(pironremin_idx_t))
     &    bec2_diag_3d(istr:iend,j,k,bec2_diag_3d_idx(pironremin_idx_t)) = P_Iron_remin(istr:iend,k)
       if (bec2_diag_3d_l(caco3remin_idx_t))
     &    bec2_diag_3d(istr:iend,j,k,bec2_diag_3d_idx(caco3remin_idx_t)) = P_CaCO3_remin(istr:iend,k)
       if (bec2_diag_3d_l(sio2remin_idx_t))
     &    bec2_diag_3d(istr:iend,j,k,bec2_diag_3d_idx(sio2remin_idx_t)) = P_SiO2_remin(istr:iend,k)
       if (bec2_diag_3d_l(pocremin_idx_t))
     &    bec2_diag_3d(istr:iend,j,k,bec2_diag_3d_idx(pocremin_idx_t)) = POC_remin(istr:iend,k)
       if (bec2_diag_3d_l(dustremin_idx_t))
     &    bec2_diag_3d(istr:iend,j,k,bec2_diag_3d_idx(dustremin_idx_t)) = dust_remin(istr:iend)
#  else
       ! MF
       !mpi_master_only write(*,*) 'Update DIAG remin fields...'
       bec2_diag_3d(istr:iend,j,k,pironremin_idx_t) = P_Iron_remin(istr:iend,k)
       bec2_diag_3d(istr:iend,j,k,caco3remin_idx_t) = P_CaCO3_remin(istr:iend,k)
       bec2_diag_3d(istr:iend,j,k,sio2remin_idx_t) = P_SiO2_remin(istr:iend,k)
       bec2_diag_3d(istr:iend,j,k,pocremin_idx_t) = POC_remin(istr:iend,k)
       bec2_diag_3d(istr:iend,j,k,dustremin_idx_t) = dust_remin(istr:iend)
#  endif
# endif

        end do ! k loop

        return
        end subroutine vert_sinking_sediments

#else /* BIOLOGY_BEC2 && USE_EXPLICIT_VSINK */
      subroutine vert_sinking_bec2()
      return
      end
#endif /* BIOLOGY_BEC2 && USE_EXPLICIT_VSINK */
