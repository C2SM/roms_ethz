#include "cppdefs.h"
#ifdef BIOLOGY_NPZDOC

      subroutine biology_tile (istr,iend,jstr,jend)
!
! Compute biological forcing functions as defined by the
! Moisan et al. [in prep.]
!
! In this particular implementation there are several model options
! that allow the user to toggle between one of 3 possible models formulations:
! (1) BIOLOGY with NITROGEN cycling only
! (2) BIOLOGY with NITROGEN and OXYGEN cycling
! (3) BIOLOGY with NITROGEN, CARBON and OXYGEN cycling
!
! The NITROGEN cycle variables include: 
!   Nitrate                 ==> NO3   [mmol N m-3]
!   Ammonia                 ==> NH4   [mmol N m-3]
!   Chlorophyl a            ==> chlor [mg Chla m-3]
!   Phytoplankton           ==> Phyt  [mmol N m-3]
!   Zooplankton             ==> Zoo   [mmol N m-3]
!   Small Detrital Nitrogen ==> SDetN [mmol N m-3]
!   Large Detrital Nitrogen ==> LDetN [mmol N m-3]
!
! The Oxygen cycle variables include:
!   Oxygen                  ==> O2    [mmol O2 m-3]
!
! The CARBON cycle variables include:
!   Dissolved Inorganic Carbon  ==> DIC   [mmol C m-3]
!   Total Alkalinity        ==> TALK  [meq m-3]
!   Small Detrital Carbon   ==> SDetC [mmol C m-3]
!   Large Detrital Carbon   ==> LDetC [mmol C m-3]
!

      implicit none
      integer istr,iend,jstr,jend
#include "param.h"
#include "grid.h"
#include "ocean2d.h"
#include "ocean3d.h"
#include "scalars.h"
#include "forces.h"
#include "biopar.h"
#include "bgcflux.h"
#if defined MPI && defined VARIABLE_ANN_ATM_PCO2
# include "mpif.h"
#endif
#include "gasexc.h"
#if defined BIO_SWR_FRAC && defined TIME_VAR_SWR_FRAC
# include "mixing.h"
#endif

#ifdef DAILYPAR_PHOTOINHIBITION
      real thisPARdaily
      real PARdaily(N)
#endif /* DAILYPAR_PHOTOINHIBITION */

! flux analysis
      real ThisFlux(N, NumFluxTerms)         ! [mmol m-3 s-1]
#ifdef OXYGEN
      real ThisGasExcFlux(NumGasExcTerms)    ! [mmol m-2 s-1], flux into water is positive
#endif
#ifdef BGC_FLUX_ANALYSIS
      real ThisVSinkFlux(N, NumVSinkTerms)   ! [mmol m-2 s-1], upward flux is positive
#endif /* BGC_FLUX_ANALYSIS */
#ifdef SEDIMENT_BIOLOGY
      real ThisSedFlux(NumSedFluxTerms)      ! [mmol m-2 s-1]
#endif /* SEDIMENT_BIOLOGY */
!
#ifdef CARBON
      real pCO2air_loc
#endif
!
      integer i,j,k, ITER, iB, iflux
!
      real tem(N), sal(N)
#if (defined OXYGEN && !defined OCMIP_OXYGENSAT) || defined CARBON
      real den_N
#endif
#if defined OXYGEN || defined BGC_FLUX_ANALYSIS
      real dtsec        ! length of time step in seconds (for gas exchange)
#endif /* OXYGEN || BGC_FLUX_ANALYSIS */
      real NO3(N), NH4(N), Phyt(N), Zoo(N),
     &     SDetN(N), LDetN(N), theta(N), chla(N)
#ifdef SEDIMENT_BIOLOGY
      real SedOrgN
# ifdef CARBON
      real SedOrgC, SedCaCO3
# endif /* CARBON */
# ifdef OXYGEN
# ifdef OXYLIM_SED
      real O2_req, NO3_req, Flc, den
# endif /* OXYLIM_SED */
# endif /* OXYGEN */
#endif /* SEDIMENT_BIOLOGY */
!
      integer nsink
      parameter (nsink = NumVSinkTerms + 1) ! add'lly: Chlorophyll 

      real dtdays, thisPAR, inhNH4, attn, Vp, Epp, LTOT, L_NO3, L_NH4,
     &     cu, aL, aR, t_PPmax, FC(0:N),
     &     cff,cff1,cff2,cff3,cff4,cff6,
     &     SB(N,nsink),dSB(0:N,nsink),wSB(nsink)
#if defined BIO_SWR_FRAC && defined TIME_VAR_SWR_FRAC
     &     ,cffi
#endif

#if defined SEDIMENT_BIOLOGY || defined BGC_FLUX_ANALYSIS
! variables for computation of vertical sinking fluxes      
      real ColumnMassOld(0:NumVSinkTerms),ColumnMassNew(0:NumVSinkTerms)
#endif /* SEDIMENT_BIOLOGY || BGC_FLUX_ANALYSIS */
#ifdef BGC_FLUX_ANALYSIS
      real LastVSinkFlux
#endif /* BGC_FLUX_ANALYSIS */
!
      real eos80
!
#ifdef OXYGEN
      real O2(N)
      real u10_loc,Sc_O2,O2satu_loc,Kv_O2_loc
      integer su_i2,sv_j2
#  ifdef OCMIP_OXYGENSAT
      real o2sato   ! OCMIP function, calculates O2 saturation
#  else /* OCMIP_OXYGENSAT */
      real satpc         ! oxygen saturation in % (calculated, but unused)
      real AOU           ! Apparent oxygen utilization (calc., but unused)
#  endif /* OCMIP_OXYGENSAT */
#endif /* OXYGEN */
!
#ifdef CARBON
      real DIC(N),TALK(N),SDetC(N),LDetC(N),CaCO3(N)
      real TP,TSi,TA,TC,CO2sol_loc,pCO2sw_loc,Kv_CO2_loc,Sc_CO2
#  ifdef OCMIP_CARBON
      real pHlo, pHhi   ! bounds for the iterative solver
! output variables from OCMIP subroutines
      real pHsw_loc     ! pH values
      real CO2star
      real dCO2star
      real dpCO2
      real atmpres
#  else /* OCMIP_CARBON */
      real Pdbar
#  endif /* OCMIP_CARBON */
#endif /* CARBON */
!

! local flux terms
#ifdef CARBON
      real CFlux_NewProd, CFlux_RegProd, CFlux_Precip
#endif /* CARBON */
!
!DL: needed for interannual variations of pCO2:
# ifdef VARIABLE_ANN_ATM_PCO2
        real tdays_pco2, curr_year
        integer ierr
        external get_atmco2
        real, external:: get_frac_year
# endif

#include "compute_auxiliary_bounds.h"

      dtdays=dt/(24.*3600.*float(ITERMAX))  ! time step as fraction of day.
#if defined OXYGEN || defined BGC_FLUX_ANALYSIS
      dtsec = dt / float(ITERMAX)           ! time step in seconds
#endif /* OXYGEN || BGC_FLUX_ANALYSIS */

#ifdef CARBON
# ifndef PCO2AIR_FORCING
! Set a default value for pCO2 in the air. Will be overwritten if 
! VARIABLE_ANN_ATM_PCO2 and/or VARIABLE_ATM_PCO2 is defined:
! 

! For 2050
!          pCO2air_loc = 550.0 
! For present day
           pCO2air_loc = 370.0
! For preindustrial
!          pCO2air_loc = 278.0
# ifdef VARIABLE_ANN_ATM_PCO2
           ! Compute number of simulated days since the start of the simulation:
           tdays_pco2 = time*sec2day
           ! Compute where in which year we are:
           curr_year = start_year + get_frac_year(tdays_pco2)
           ! Determine atm pco2:
#  ifdef HINDCAST
           call get_atmco2(curr_year,pCO2air_loc)
#  elif defined FUTURE_SCENARIO
           call get_atmco2(curr_year,futr_scen,pCO2air_loc)
#  endif /* HINDCAST */
# endif /* VARIABLE_ANN_ATM_PCO2 */
# ifdef VARIABLE_ATM_PCO2
      ! the factor deg2rad converts 360d into one full cycle of 2 pi
      ! the subtraction of 30d ensures that the maximum is reached
      ! at the end of April and the minimum at the end of October
      pCO2air_loc = pCO2air_loc + 2.9 * sin(deg2rad*(tdays-30.))
# endif /* VARIABLE_ATM_PCO2 */
#endif /* PCO2AIR_FORCING */
#endif /* CARBON */

!
!
! Since the following solver is iterative to achieve implicit
! discretization of the biological interaction, two time slices are
! required, BIO where BIO is understood as vector of
! biological state variables: BIO=[NO3,NH4,Phyt,Zoo,SDet]. Assume
! that the iterations converge, the newly obtained state variables
! satisfy equations
!
!           BIO = BIO + dtdays * rhs(BIO)
! 
! where rhs(BIO) is the vector of biological r.h.s. computed at
! the new time step. During the iterative procedure a series of
! fractional time steps is performed in a chained mode (splitting
! by different biological conversion processes) in sequence NO3 -- 
! NH4 -- Phyt -- Zoo -- SDet, that is the main food chain. In all 
! stages the concentration of the component being consumed is
! treated in fully implicit manner, so that the algorithm guarantees
! non-negative values, no matter how strong is the concentration of
! active consuming component (Phyto or Zoo).
!
! The overall algorithm, as well as any stage of it is formulated
! in conservative form (except explicit sinking) in sense that the
! sum of concentration of all five components is conserved.
!

#  ifdef EW_PERIODIC
#   define I_RANGE istr,iend
#   define I_MAX iend
#  else
#   define I_RANGE istrR,iendR
#   define I_MAX iendR
#  endif
#  ifdef NS_PERIODIC
#   define J_RANGE jstr,jend
#   define J_MAX jend
#  else
#   define J_RANGE jstrR,jendR
#   define J_MAX jendR
#  endif

      j_loop: do j=J_RANGE
#ifdef OXYGEN
        if (j .lt. J_MAX) then
           sv_j2 = j+1
        else
           sv_j2 = j-1
        end if
#endif /* OXYGEN */
        i_loop: do i=I_RANGE
#ifdef OXYGEN
        if (i .lt. I_MAX) then
           su_i2 = i+1
        else
           su_i2 = i-1
        end if
        GasExcFlux(i,j,:) = 0.0
#endif /* OXYGEN */
#ifdef BGC_FLUX_ANALYSIS
! Reset the biogeochemical fluxes (use array notation).
! This is necessary because the biological routine uses multiple 
! time steps for each physical time step.
        Flux(i,j,:,:) = 0.0
        VSinkFlux(i,j,:,:) = 0.0
# ifdef SEDIMENT_BIOLOGY
        SedFlux(i,j,:) = 0.0
# endif /* SEDIMENT_BIOLOGY */
#endif /* BGC_FLUX_ANALYSIS */
# ifdef MASKING
        rmask_if: if (rmask(i,j) .eq. 1) then
# endif /* MASKING */
!
! Extract biological variables from tracer arrays; place them into
! scratch variables; restrict their values to be positive definite.
!
      k_loop1: do k=1,N
        tem(k)   = max(t(i,j,k,nnew,itemp),0.)   ! temperature;       [deg. C]
        sal(k)   = max(t(i,j,k,nnew,isalt),0.)   ! salinity;          [PSU]
#ifdef OXYGEN
        O2(k)    = max(t(i,j,k,nnew,iO2),0.)     ! Oxygen;  [mmol O2 m-3]
# ifdef CARBON
        DIC(k)   = max(t(i,j,k,nnew,iDIC),0.)    ! Total Inorganic Carbon;  [mmol-C m-3]
        TALK(k)  = max(t(i,j,k,nnew,iTALK),0.)   ! Total Alkalinity;        [meq m-3]
# endif
#endif
        NO3(k)   = max(t(i,j,k,nnew,iNO3_),0.)   ! Nitrate;                 [mmol-N m-3]
        NH4(k)   = max(t(i,j,k,nnew,iNH4_),0.)   ! Ammonium;                [mmol-N m-3]
        SDetN(k) = max(t(i,j,k,nnew,iSDet),0.)   ! Small Detritus Nitrogen  [mmol-N m-3]
#ifdef CARBON
        SDetC(k) = max(t(i,j,k,nnew,iSDetC),0.)  ! Small Detritus Carbon;   [mmol-C m-3]
#endif
        LDetN(k) = max(t(i,j,k,nnew,iLDet),0.)   ! Large Detritus Nitrogen; [mmol-N m-3]
#ifdef CARBON
        LDetC(k) = max(t(i,j,k,nnew,iLDetC),0.)  ! Large Detritus Carbon;   [mmol-C m-3]
        CaCO3(k) = max(t(i,j,k,nnew,iCaCO3),0.)  ! CaCO3 [mmol-C m-3]
#endif
        Phyt(k)  = max(t(i,j,k,nnew,iPhyt),0.)   ! Phytoplankton;           [mmol-N m-3]
        chla(k)  = max(t(i,j,k,nnew,iChla),0.)   ! Chlor a;                 [mg Chl a m-3]
        Zoo(k)   = max(t(i,j,k,nnew,iZoo_),0.)   ! Zooplankton;             [mmol-N m-3]
!
        if (Phyt(k) .gt. 0.001 .and. chla(k) .gt. 0.001) then
           theta(k) = chla(k)/(Phyt(k)*CN_P*12.) ! Chla/Phyt ratio; [mg Chla (mg C)-1]
        else
           theta(k) = theta_m
        endif
      end do k_loop1

#if (defined OXYGEN && !defined OCMIP_OXYGENSAT) || defined CARBON
        den_N   = 1000.+eos80(0.,tem(N),sal(N)) ! potential density; [kg m-3]
#endif

# ifdef SEDIMENT_BIOLOGY
      SedOrgN = t_sed(i,j,iSedOrgN)
#  ifdef CARBON
      SedOrgC = t_sed(i,j,iSedOrgC)
      SedCaCO3 = t_sed(i,j,iSedCaCO3)
#  ifdef PCO2AIR_FORCING
!
!     pCO2air is an input forcing field 
!     Overwriting any previous value (default/VARIABLE_*_PCO2)
! 
      pCO2air_loc = pCO2air(i,j)
#  endif
#  endif /* CARBON */
# endif /* SEDIMENT_BIOLOGY */

! PAR is the same for all iterations
# ifdef PAR_DIRECT_INPUT
      PARinc(i,j) = parflx(i,j)
# else /* PAR_DIRECT_INPUT */
      PARinc(i,j) = srflx(i,j)*rho0*Cp*0.43
# endif /* PAR_DIRECT_INPUT */

! all of the right-hand side values in the following equations
! do not change from one iteration to the next, so they will be computed
! outside of the iteration loop
#ifdef OXYGEN
#  ifdef OCMIP_OXYGEN_SC
!*********************************************************************
!  alternative formulation (Sc_O2 will be slightly smaller up to about 35 C)
!  Computes the Schmidt number of oxygen in seawater using the
!  formulation proposed by Keeling et al. (1998, Global Biogeochem.
!  Cycles, 12, 141-163).  Input is temperature in deg C.
!
        Sc_O2 = 1638.0 - 81.83*Tem(N) + 1.483*(Tem(N)**2) - 
     &       0.008004*(Tem(N)**3)
!*********************************************************************
#  else /* OCMIP_OXYGEN_SC */
!       calculate the Schmidt number for O2 in sea water [Wanninkhof, 1992]
        Sc_O2=1953.4 - 128.0*Tem(N) + 3.9918*(Tem(N)**2) -
     &     0.050091*(Tem(N)**3)
#  endif /* OCMIP_OXYGEN_SC */
!
!       calculate the wind speed from the surface stress values
        u10_loc = sqrt(sqrt( (0.5*(sustr(i,j)+sustr(su_i2,j)))**2
     &          +(0.5*(svstr(i,j)+svstr(i,sv_j2)))**2)
     &          * rho0 * 550.)  ! 550 = 1 / (1.3 * 0.0014) (=rho_air * CD)
!       calculate the gas transfer coef for O2
        Kv_O2_loc=0.31*u10_loc*u10_loc*sqrt(660./Sc_O2)/(100.*3600.) ! denominator: convert Kv from [cm/h] to [m/s]
!       calculate the saturation oxygen level
#  ifdef OCMIP_OXYGENSAT
        O2satu_loc = o2sato(Tem(N), Sal(N))
#  else /* OCMIP_OXYGENSAT */
        call O2sat(O2(N),Tem(N),Sal(N),den_N,O2satu_loc,satpc,AOU)
#  endif /* OCMIP_OXYGENSAT */

#endif /* OXYGEN */
#ifdef CARBON
!       calculate the Schmidt number for CO2 in sea water [Wanninkhof, 1992]
        Sc_CO2=2073.1 - 125.62*Tem(N) + 3.6276*(Tem(N)**2) -
     &     0.043219*(Tem(N)**3)
!       calculate the gas transfer coef from eq. 3 in [Wanninkhof, 1992]
        Kv_CO2_loc=0.31*u10_loc*u10_loc*sqrt(660./Sc_CO2)/(100.*3600.) !  denominator: convert Kv from [cm/h] to [m/s]
#endif /* CARBON */
      iter_loop: do ITER=1,ITERMAX   !--> Start internal iterations to achieve
                                     !    nonlinear backward-implicit solution.
      thisPAR = PARinc(i,j) 
#if defined BIO_SWR_FRAC && defined TIME_VAR_SWR_FRAC
      cffi=1./max(thisPAR,1D-30)     ! avoid division in k-loop below
      swr_frac(i,j,N) = 1.
#endif

# ifdef DAILYPAR_PHOTOINHIBITION
#  ifdef PAR_DIRECT_INPUT
! not yet implemented
#  else /* PAR_DIRECT_INPUT */
           thisPARdaily = srflx_dailyavg(i,j)*rho0*Cp*0.43
#  endif /* PAR_DIRECT_INPUT */
# endif /* DAILYPAR_PHOTOINHIBITION */
!
!
        PAR_if: if (thisPAR.gt.0.) then

!
!         *** SUN IS UP ***
!
!         Calulate t_PPmax: Set Photosynthetically Available Radiation (PAR) at
!         surface from solar radiation x 0.43. Then, within each grid box
!         compute attenuation coefficient based on the concentration of
!         chlorphyll a inside the grid box, and attenuate PAR from surface
!         down (thus, PAR at certain depth depends on the whole distribution
!         of chlorophyll a above). To compute t_PPmax, one needs PAR somewhat in
!         the middle of the gridbox, so that attenuation "attn" corresponds
!         to half of the grid box height, while PAR is multiplied by it
!         twice: once to get it in the middle of grid-box and once the
!         compute on the lower grid-box interface.
!
          k_loop2: do k=N,1,-1     !<-- irreversible
!
!           attenuate the light to the center of the grid cell
            attn=exp(-0.5*(kwater+kChla*chla(k))*
     &              (z_w(i,j,k)-z_w(i,j,k-1)))
            thisPAR=thisPAR*attn
            PAR(i,j,k) = thisPAR
!
!           temperature- and light-limited growth rate
            Vp=Vp0*0.59*(1.066**t(i,j,k,nnew,itemp)) ! From Eppley: [0.59=ln(2.)*.851]
!           (Eppley, R.W., 1972, Fishery Bulletin, 70: 1063-1085)
            cff=thisPAR*palpha*theta(k)              ! Vp=2.9124317 at
            Epp=Vp/sqrt(Vp*Vp+cff*cff)               ! T=19.25 degrees
            t_PPmax=Epp*cff
!
!           nutrient-limitation terms
            inhNH4=1./(1.+(NH4(k)*K_NH4)) ! Parker 1993 Ecol Mod., 66, 113-120.
!
            L_NO3=NO3(k)*K_NO3*inhNH4/(1. + NO3(k)*K_NO3)
            L_NH4=NH4(k)*K_NH4/(1. + NH4(k)*K_NH4)
            LTOT=L_NO3+L_NH4   ! (L </= 1)
!
            cff=dtdays*t_PPmax*LTOT
!
!           theta adaptation
            theta(k)=(theta(k)+theta_m*Epp*LTOT*cff)/(1.+cff)
!
!           uptake of NO3 and NH4 by Phyt
            cff1 = dtdays*t_PPmax*Phyt(k)*K_NO3*
     &           inhNH4 / (1. + NO3(k)*K_NO3)
            NO3(k)=NO3(k)/(1. + cff1)
            ThisFlux(k, NFlux_NewProd) = cff1 * NO3(k)

            cff2 = dtdays*t_PPmax*Phyt(k)*
     &           K_NH4/(1. + NH4(k)*K_NH4)
            NH4(k)=NH4(k)/(1. + cff2)
            ThisFlux(k, NFlux_RegProd) = cff2 * NH4(k)
            Phyt(k) = Phyt(k) + ( ThisFlux(k, NFlux_NewProd) + 
     &           ThisFlux(k, NFlux_RegProd) )
!
#ifdef OXYGEN
!           evolution of O2 by phyto growth
            O2(k) = O2(k) + ThisFlux(k, NFlux_NewProd) * ON_NO3 +
     &           ThisFlux(k, NFlux_RegProd) * ON_NH4
#endif /* OXYGEN */
!
#ifdef CARBON
!           uptake of DIC (CO2) by phyto growth
            CFlux_NewProd = 
     &           ThisFlux(k, NFlux_NewProd) * CN_P
            CFlux_RegProd = 
     &           ThisFlux(k, NFlux_RegProd) * CN_P
            DIC(k) = DIC(k) - (CFlux_NewProd + CFlux_RegProd)

! precipitation of CaCO3, proportional to NPP
            CFlux_Precip = (CFlux_NewProd+CFlux_RegProd) * r_CaCO3_orgC
            DIC(k) = DIC(k) - CFlux_Precip
            TALK(k) = TALK(k) - 2. * CFlux_Precip
            CaCO3(k) = CaCO3(k) + CFlux_Precip

!           account for the uptake of NO3 on TALK
            TALK(k) = TALK(k) + ThisFlux(k, NFlux_NewProd)
#endif /* CARBON */
!
!           attenuate the light to the bottom of the grid cell
            thisPAR=thisPAR*attn
#if defined BIO_SWR_FRAC && defined TIME_VAR_SWR_FRAC
!           Solar radiation fraction reaching the next level:
            swr_frac(i,j,k-1) = cffi*thisPAR ! cffi = 1./PARinc(i,j)
#endif
!
# ifdef DAILYPAR_PHOTOINHIBITION
            thisPARdaily=thisPARdaily*attn
            PARdaily(k) = thisPARdaily
            thisPARdaily=thisPARdaily*attn
# endif /* DAILYPAR_PHOTOINHIBITION */
          end do k_loop2
!
       else PAR_if
          k_loop2a: do k = N, 1, -1
            ThisFlux(k, NFlux_NewProd) = 0.0
            ThisFlux(k, NFlux_RegProd) = 0.0
            PAR(i,j,k) = 0.0

# ifdef DAILYPAR_PHOTOINHIBITION
            attn=exp(-0.5*(kwater+kChla*chla(k))*
     &            (z_w(i,j,k)-z_w(i,j,k-1)))

            thisPARdaily=thisPARdaily*attn
            PARdaily(k) = thisPARdaily
            thisPARdaily=thisPARdaily*attn
# endif /* DAILYPAR_PHOTOINHIBITION */

          end do k_loop2a
       end if PAR_if
!
        k_loop3: do k=1,N
!
!         Nitrification of NH4 ==> NO3  ! thought to occur only in dark and only in
!                                     aerobic water !!!
!         [See Olson, R. J., 1981, JMR: (39), 227-238.]
!         NH4+ + 3/2 O2  ==> NO2-  + H2O; via Nitrosomonas bacteria
!         NO2-  + 1/2 O2 ==> NO3-       ; via Nitrobacter bacteria
!         Note that the entire process has a total loss of 2 O2 per 1 NH4 mol. to mol.
!         If we were to resolve NO2 profiles, this is where we would change the code
!         to split out the differential effects of the two different bacteria types.
!         Also, at this time, this process is not constrained by [O2].
!
#ifdef DAILYPAR_PHOTOINHIBITION
! do not use the current PAR, but the daily average value - inhibition will
! be less by day but still prevail at night, see Olson 1981
          cff1=(PARdaily(k) - I_thNH4)/(D_p5NH4 + 
     &          PARdaily(k) - I_thNH4)
#else /* DAILYPAR_PHOTOINHIBITION */
          cff1=(PAR(i,j,k) - I_thNH4)/(D_p5NH4 + 
     &          PAR(i,j,k) - I_thNH4)
#endif /* DAILYPAR_PHOTOINHIBITION */
          cff2=1.-max(0.,cff1)
          cff=dtdays*t_nitri*cff2  ! linear with a light inhibition term

#ifdef OXYLIM
! Test to see if water is close to anoxic, if so, then no nitrification
          if (O2(k) < 4.0) then
             ThisFlux(k, NFlux_Nitrif) = 0.0
          else
#endif /* OXYLIM */
          NH4(k)=NH4(k)/(1.+cff)
          ThisFlux(k, NFlux_Nitrif) = cff*NH4(k)
          NO3(k) = NO3(k) + ThisFlux(k, NFlux_Nitrif)
!
#ifdef OXYLIM
          endif
#endif /* OXYLIM */
#ifdef OXYGEN
          ! nitrification term for O2 variable [NH4==>NO3]
          O2(k) = O2(k) - 2. * ThisFlux(k, NFlux_Nitrif)
#endif /* OXYGEN */
!
#ifdef CARBON
!         addition of Nitrate to TA sum
          TALK(k) = TALK(k) - ThisFlux(k, NFlux_Nitrif)
#endif /* CARBON */
!
!         Phyt grazing by Zoo, assimilated to Zoo, and egested to SDetN
!         Phyt mortality to SDetN (t_Pmort)
          cff1=dtdays*t_Zgraz*Zoo(k)/(K_P+Phyt(k))
          cff2=dtdays*t_Pmort
          Phyt(k)=Phyt(k)/(1.+cff1+cff2)
          ! Note: This is the N flux into zooplankton due to grazing
          ! The N loss for phytoplankton does NOT have the factor AE_N
          ThisFlux(k, NFlux_Grazing) = cff1 * Phyt(k) * AE_N
          ThisFlux(k, NFlux_Pmort) = Phyt(k) * cff2
          Zoo(k) = Zoo(k) + ThisFlux(k, NFlux_Grazing)

          cff3 = Phyt(k) * cff1 * (1.-AE_N)
          ThisFlux(k, NFlux_SlopFeed) = cff3 * (1. - Zgamma)
          ThisFlux(k, NFlux_Zfecp) = cff3 * Zgamma
          SDetN(k) = SDetN(k) + ( ThisFlux(k, NFlux_SlopFeed) +
     &         ThisFlux(k, NFlux_Pmort) )
          LDetN(k) = LDetN(k) + ThisFlux(k, NFlux_Zfecp)
!
#ifdef CARBON
!         Phyto grazing by Zoo and egested as SDetC
!         Phyto mortality to SDetC (t_Pmort)
          ! See the remarks above for NFlux_Grazing
          SDetC(k) = SDetC(k) + (ThisFlux(k, NFlux_SlopFeed) * CN_P + 
     &         ThisFlux(k, NFlux_Pmort) * CN_P )
          LDetC(k) = LDetC(k) + ThisFlux(k, NFlux_Zfecp) * CN_P
#endif /* CARBON */
!
!         Zoo basal metabolism to NH4  (rate t_Zbmet)
!         Zoo mortality to SDetN (rate t_Zmort)
!         Zoo ingestion-related excretion rate ==> E_N
          cff1=dtdays*t_Zbmet             ! basal metabolism
          cff2=dtdays*t_Zmort*Zoo(k)      ! grazing by higher trophic levels
          cff3=dtdays*t_Zgraz*(Phyt(k)/(K_P+Phyt(k)))*
     &         CN_P*(AE_N-GGE_C)         ! R_C: grazing-dependent respiration
          cff4=dtdays*t_Zgraz*(Phyt(k)/(K_P+Phyt(k)))*CN_P*AE_N*
     &         ((1./CN_P)-(GGE_C/(AE_N*CN_Z)))  ! E_N: grazing-dependent excretion
          Zoo(k)=Zoo(k)/(1.+cff1+cff2+cff4)
          ThisFlux(k, NFlux_Zmetab) = cff1 * Zoo(k)
          ThisFlux(k, NFlux_ZmortS) = cff2 * Zoo(k) * (1. - Zomega)
          ThisFlux(k, NFlux_ZmortL) = cff2 * Zoo(k) * Zomega
          ThisFlux(k, NFlux_Zexcr) = cff4 * Zoo(k)

          NH4(k) = NH4(k) + ( ThisFlux(k, NFlux_Zmetab) + 
     &         ThisFlux(k, NFlux_Zexcr) )
          SDetN(k) = SDetN(k) + ThisFlux(k, NFlux_ZmortS)
          LDetN(k) = LDetN(k) + ThisFlux(k, NFlux_ZmortL)
!
#ifdef CARBON
!         Zoo mortality to SDetC (rate t_Zmort)
          ThisFlux(k, CFlux_Zresp) = cff3 * Zoo(k)
          SDetC(k) = SDetC(k) + ThisFlux(k, NFlux_ZmortS) * CN_Z
          LDetC(k) = LDetC(k) + ThisFlux(k, NFlux_ZmortL) * CN_Z
#endif /* CARBON */
!
#ifdef OXYGEN
!         Zoo uptake of O2 (rate t_Zbmet + R_C)
!         there is no control yet for assuring non-negative Oxygen values!
          O2(k)  = O2(k) - ( ThisFlux(k, NFlux_Zmetab) * ON_NH4 + 
     &         cff3 * Zoo(k) )
#endif /* OXYGEN */
!
#ifdef CARBON
!         Zoo respiration of CO2 (rate t_Zbmet + R_C)
          DIC(k) = DIC(k) + ThisFlux(k, NFlux_Zmetab) * CN_Z + 
     &         ThisFlux(k, CFlux_Zresp)
#endif /* CARBON */
!
!         Coagulation of Phyt+SDetN to LDetN
          cff1=dtdays*t_coag*(SDetN(k)+Phyt(k))
          Phyt(k)=Phyt(k)/(1.+cff1)
          ThisFlux(k, NFlux_DetCoagP) = Phyt(k) * cff1
          SDetN(k)=SDetN(k)/(1.+cff1)
          ThisFlux(k, NFlux_DetCoagD) = SDetN(k) * cff1
          LDetN(k) = LDetN(k) + ( ThisFlux(k, NFlux_DetCoagP) +
     &         ThisFlux(k, NFlux_DetCoagD) )
!
#ifdef CARBON
!         Coagulation of Phyt*CN_P+SDetC to LDetC
          SDetC(k)=SDetC(k)/(1.+cff1)
          ThisFlux(k, CFlux_DetCoagD) = SDetC(k) * cff1
          LDetC(k) = LDetC(k) + ( ThisFlux(k, NFlux_DetCoagP) * CN_P + 
     &         ThisFlux(k, CFlux_DetCoagD) )
#endif /* CARBON */
!
!         SDetN recycling to NH4 (remineralization)
#ifdef OXYLIM
          if (O2(k) < 4.0) then
             cff1=dtdays*t_SDremin*0.5
             ThisFlux(k, NFlux_DenitrS) = 0
          else
             cff1=dtdays*t_SDremin
             ThisFlux(k, NFlux_DenitrS) = 0
          endif
#else
          cff1=dtdays*t_SDremin
#endif /* OXYLIM */
          SDetN(k)=SDetN(k)/(1.+cff1)
          ThisFlux(k, NFlux_ReminS) = SDetN(k) * cff1
          NH4(k) = NH4(k) + ThisFlux(k, NFlux_ReminS)
!
#ifdef CARBON
!         breakdown of SDetC/SDetN to CO2 from remineralization
! We might want to modify the max rate of remin if denitrification
! occurs instead of oxygenic remin - add OXYLIM condition to 
! cff1 calculated just above -- Sept 2011 LRK - This has been enabled
! now if O2 is limiting reminerialization rate of C and N is 50% the
! oxygenic value 

          SDetC(k)=SDetC(k)/(1.+cff1)
          ThisFlux(k, CFlux_ReminS) = SDetC(k) * cff1
          DIC(k) = DIC(k) + ThisFlux(k, CFlux_ReminS)
#endif /* CARBON */
!
#ifdef OXYGEN
# ifdef OXYLIM
          if (O2(k) < 4.0) then
             ThisFlux(k, NFlux_DenitrS)=N_CDET*ThisFlux(k, CFlux_ReminS)
             NO3(k) = NO3(k) - N_CDET * ThisFlux(k, CFlux_ReminS)
! Account for NO3 consumption via denitrification on alkalinity
             TALK(k) = TALK(k) + ThisFlux(k, NFlux_DenitrS)
          else
!             Loss of O2 in SDetN/SDetC remineralization
             O2(k) = O2(k) - ON_NH4 * ThisFlux(k, NFlux_ReminS)
          endif
# else /* OXYLIM */
!            Loss of O2 in SDetN/SDetC remineralization
             O2(k) = O2(k) - ON_NH4 * ThisFlux(k, NFlux_ReminS)
# endif /* OXYLIM */
#endif /* OXYGEN */

!
!         LDetN recycling to NH4 (remineralization)

#ifdef OXYLIM
          if (O2(k) < 4.0) then
             cff1=dtdays*(t_LDremin*0.5)
             ThisFlux(k, NFlux_DenitrL) = 0
          else
             cff1=dtdays*t_LDremin
             ThisFlux(k, NFlux_DenitrL) = 0
          endif
#else
          cff1=dtdays*t_LDremin
#endif /* OXYLIM */
          LDetN(k)=LDetN(k)/(1.+cff1)
          ThisFlux(k, NFlux_ReminL) = LDetN(k) * cff1
          NH4(k) = NH4(k) + ThisFlux(k, NFlux_ReminL)
!
#ifdef CARBON
!         LDetC breakdown of LDetC to CO2 from remineralization
          LDetC(k)=LDetC(k)/(1.+cff1)
          ThisFlux(k, CFlux_ReminL) = LDetC(k) * cff1
          DIC(k) = DIC(k) + ThisFlux(k, CFlux_ReminL)

!         Dissolution of CaCO3
          cff1 = dtdays * t_dissCaCO3
          CaCO3(k) = CaCO3(k) / (1. + cff1)
          ThisFlux(k, CFlux_Dissolv) = CaCO3(k) * cff1
          DIC(k) = DIC(k) + ThisFlux(k, CFlux_Dissolv)
          TALK(k) = TALK(k) + 2. * ThisFlux(k, CFlux_Dissolv)

#ifdef OXYGEN
# ifdef OXYLIM
          if (O2(k) < 4.0) then
             ThisFlux(k, NFlux_DenitrL)=N_CDET*ThisFlux(k, CFlux_ReminL)
             NO3(k) = NO3(k) - N_CDET * ThisFlux(k, CFlux_ReminL)
!            Account for NO3 consumption via denitr on alkalinity 
             TALK(k) = TALK(k) + ThisFlux(k, NFlux_DenitrL)
          else
!            Loss of O2 in SDetN/SDetC remineralization        
             O2(k) = O2(k) - ON_NH4 * ThisFlux(k, NFlux_ReminL)
          endif
# else /* OXYLIM */
!            Loss of O2 in LDetC/LDetN remineralization
             O2(k) = O2(k) - ON_NH4 * ThisFlux(k, NFlux_ReminL)
# endif /* OXYLIM */
#endif /* OXYGEN */
#endif /* CARBON */
!
        end do k_loop3 ! go to the next depth/level
!
#ifdef OXYGEN
!       air-sea flux of O2
!       Hz(i,j,N) ==> thickness of upper layer
        ThisGasExcFlux(OFlux_GasExc) = Kv_O2_loc * (O2satu_loc - O2(N))
!       ThisGasExcFlux is positive if ocean takes up O2 from the atmosphere
        O2(N) = O2(N) + ThisGasExcFlux(OFlux_GasExc) * dtsec /
     &       Hz(i,j,N)
#endif /* OXYGEN */
!
#ifdef CARBON
!       calculate the pCO2 at the surface of the ocean using the CO2 SYSTEM program
!       calculate the TP using a poly fit to the CalCOFI data set
        TP=0.334+0.0669*NO3(N)        ! PO4 conc. [mmol-P/m^3]
!       calculate the TSi using a poly fit to the CalCOFI data set
        TSi=2.1205 + 1.0146*NO3(N) - 0.0263*(NO3(N)**2) +
     &      0.0013*(NO3(N)**3)        ! SiO4 conc. [mmol-Si/m^3]
        TA=TALK(N)                    ! total alkalinity [meq/m^3]
        TC=DIC(N)                     ! total inorganic carbon [mmol C/m^3]
#  ifdef OCMIP_CARBON
        atmpres=1.                    ! atmospheric pressure [atm]
! co2calc is from the OCMIP code
! it uses [mol/m^3] instead of [mmol/m^3]
        TA = TA * 1.d-3
        TC = TC * 1.d-3
        TP = TP * 1.d-3
        TSi = TSi * 1.d-3
! carbon system calculation is not necessary at every iteration,
! values do not change quickly
!HF: WARN
#define CALC_CARBON_ONCE
#ifdef CALC_CARBON_ONCE
        if (ITER .eq. 1) then
#endif
! this test is necessary as early runs have shown possible runaways,
! especially for boundary cells
!HF: WARN
#define USE_PH_SAVED
#ifdef USE_PH_SAVED
          if (abs(ph_hist(i,j) - 8.1d0) .lt. 2.d-1) then
             pHLo = ph_hist(i,j) - pHeps
             pHHi = ph_hist(i,j) + pHeps
          else
#endif
             pHLo = 6.5d0
             phHi = 9.d0
#ifdef USE_PH_SAVED
          end if
#endif
          call co2calc_SWS(Tem(N), Sal(N), TC, TA, TP, TSi, pHLo, pHHi,
     &       pHsw_loc,pCO2air_loc, den_N,atmpres, CO2star, dCO2star,
     &       pCO2sw_loc, dpCO2, CO2sol_loc)
#  else /* OCMIP_CARBON */
          Pdbar=0.                        ! water pressure [db]
          call CO2SYS(TP,TSi,Sal(N),Tem(N),den_N,Pdbar,TA,TC,
     &              pCO2sw_loc,CO2sol_loc)
#  endif /* OCMIP_CARBON */
#ifdef CALC_CARBON_ONCE
        end if
#endif
!
!       air-sea flux of CO2
!       Hz(i,j,N) ==> thickness of upper layer 
        ThisGasExcFlux(CFlux_GasExc) = CO2sol_loc * Kv_CO2_loc *
     &       (pCO2air_loc - pCO2sw_loc)
        DIC(N) = DIC(N) + ThisGasExcFlux(CFlux_GasExc) * dtsec /
     &       Hz(i,j,N)
#endif /* CARBON */
!
! Vertical sinking: Vertical advection algorithm based on monotonic,
! continuous conservative parabolic splines.
!
        k_loop4: do k=1,N
          SB(k,1)=theta(k)*Phyt(k)*CN_P*12.
          SB(k,2)=Phyt(k)
          SB(k,3)=SDetN(k)
          SB(k,4)=LDetN(k)
#ifdef CARBON
          SB(k,5)=SDetC(k)
          SB(k,6)=LDetC(k)
          SB(k,7)=CaCO3(k)
#endif /* CARBON */
        end do k_loop4
!
        wSB(1)=wPhyt
        wSB(2)=wPhyt
        wSB(3)=wSDet
        wSB(4)=wLDet
#ifdef CARBON
        wSB(5)=wSDet
        wSB(6)=wLDet
        wSB(7)=wCaCO3
#endif /* CARBON */

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
     &                 -cff*Hz(i,j,k)*(0.5*dSB(k,iB)+dSB(k-1,iB))
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
#if defined SEDIMENT_BIOLOGY || defined BGC_FLUX_ANALYSIS
          ColumnMassOld(iB-1) = 0.0
          ColumnMassNew(iB-1) = 0.0
#endif /* SEDIMENT_BIOLOGY || BGC_FLUX_ANALYSIS */
        end do iB_loop

        k_loop10: do k=1,N
          ! chlorophyll
          theta(k) = SB(k,1)/(SB(k,2)*CN_P*12.+1.E-20)

          ! phytoplankton
#if defined SEDIMENT_BIOLOGY || defined BGC_FLUX_ANALYSIS
! ColumnMassOld and ColumnMassNew are needed to compute the sinking flux
! into the sediment
          ColumnMassOld(1) = ColumnMassOld(1) +
     &       Phyt(k) * Hz(i,j,k)
#endif /* SEDIMENT_BIOLOGY || BGC_FLUX_ANALYSIS */
#ifdef BGC_FLUX_ANALYSIS
          ThisVSinkFlux(k, PFlux_VSink) = Phyt(k) - SB(k,2)
#endif /* BGC_FLUX_ANALYSIS */
          Phyt(k) = SB(k,2)
#if defined SEDIMENT_BIOLOGY || defined BGC_FLUX_ANALYSIS
          ColumnMassNew(1) = ColumnMassNew(1) +
     &       Phyt(k) * Hz(i,j,k)
#endif /* SEDIMENT_BIOLOGY || BGC_FLUX_ANALYSIS */
 
          ! small detritus nitrogen
#if defined SEDIMENT_BIOLOGY || defined BGC_FLUX_ANALYSIS
          ColumnMassOld(2) = ColumnMassOld(2) +
     &       SDetN(k) * Hz(i,j,k)
#endif /* SEDIMENT_BIOLOGY || BGC_FLUX_ANALYSIS */
#ifdef BGC_FLUX_ANALYSIS
          ThisVSinkFlux(k, SDNFlux_VSink) = SDetN(k) - SB(k,3)
#endif /* BGC_FLUX_ANALYSIS */
          SDetN(k) = SB(k,3)
#if defined SEDIMENT_BIOLOGY || defined BGC_FLUX_ANALYSIS
          ColumnMassNew(2) = ColumnMassNew(2) +
     &       SDetN(k) * Hz(i,j,k)
#endif /* SEDIMENT_BIOLOGY || BGC_FLUX_ANALYSIS */

          ! large detritus nitrogen
#if defined SEDIMENT_BIOLOGY || defined BGC_FLUX_ANALYSIS
          ColumnMassOld(3) = ColumnMassOld(3) +
     &       LDetN(k) * Hz(i,j,k)
#endif /* SEDIMENT_BIOLOGY || BGC_FLUX_ANALYSIS */
#ifdef BGC_FLUX_ANALYSIS
          ThisVSinkFlux(k, LDNFlux_VSink) = LDetN(k) - SB(k,4)
#endif /* BGC_FLUX_ANALYSIS */
          LDetN(k) = SB(k,4)
#if defined SEDIMENT_BIOLOGY || defined BGC_FLUX_ANALYSIS
          ColumnMassNew(3) = ColumnMassNew(3) +
     &       LDetN(k) * Hz(i,j,k)
#endif /* SEDIMENT_BIOLOGY || BGC_FLUX_ANALYSIS */

#ifdef CARBON
          ! small detritus carbon
#if defined SEDIMENT_BIOLOGY || defined BGC_FLUX_ANALYSIS
          ColumnMassOld(4) = ColumnMassOld(4) +
     &       SDetC(k) * Hz(i,j,k)
#endif /* SEDIMENT_BIOLOGY || BGC_FLUX_ANALYSIS */
#ifdef BGC_FLUX_ANALYSIS
          ThisVSinkFlux(k, SDCFlux_VSink) = SDetC(k) - SB(k,5)
#endif /* BGC_FLUX_ANALYSIS */
          SDetC(k) = SB(k,5)
#if defined SEDIMENT_BIOLOGY || defined BGC_FLUX_ANALYSIS
          ColumnMassNew(4) = ColumnMassNew(4) +
     &       SDetC(k) * Hz(i,j,k)
#endif /* SEDIMENT_BIOLOGY || BGC_FLUX_ANALYSIS */

          ! large detritus carbon
#if defined SEDIMENT_BIOLOGY || defined BGC_FLUX_ANALYSIS
          ColumnMassOld(5) = ColumnMassOld(5) +
     &       LDetC(k) * Hz(i,j,k)
#endif /* SEDIMENT_BIOLOGY || BGC_FLUX_ANALYSIS */
#ifdef BGC_FLUX_ANALYSIS
          ThisVSinkFlux(k, LDCFlux_VSink) = LDetC(k) - SB(k,6)
#endif /* BGC_FLUX_ANALYSIS */
          LDetC(k) = SB(k,6)
#if defined SEDIMENT_BIOLOGY || defined BGC_FLUX_ANALYSIS
          ColumnMassNew(5) = ColumnMassNew(5) +
     &       LDetC(k) * Hz(i,j,k)
#endif /* SEDIMENT_BIOLOGY || BGC_FLUX_ANALYSIS */

          ! CaCO3
#if defined SEDIMENT_BIOLOGY || defined BGC_FLUX_ANALYSIS
          ColumnMassOld(6) = ColumnMassOld(6) +
     &       CaCO3(k) * Hz(i,j,k)
#endif /* SEDIMENT_BIOLOGY || BGC_FLUX_ANALYSIS */
#ifdef BGC_FLUX_ANALYSIS
          ThisVSinkFlux(k, CaCO3Flux_VSink) = CaCO3(k) - SB(k,7)
#endif /* BGC_FLUX_ANALYSIS */
          CaCO3(k) = SB(k,7)
#if defined SEDIMENT_BIOLOGY || defined BGC_FLUX_ANALYSIS
          ColumnMassNew(6) = ColumnMassNew(6) +
     &       CaCO3(k) * Hz(i,j,k)
#endif /* SEDIMENT_BIOLOGY || BGC_FLUX_ANALYSIS */

#endif /* CARBON */

        end do k_loop10
#ifdef BGC_FLUX_ANALYSIS 
! Transfer fluxes to global arrays at the end of each biological time step
! for computational efficiency, divide now by dtsec to get the correct units
!DL: also multiply by box height Hz(i,j,k)
        do iflux = 1, NumFluxTerms
           do k = 1, N
              Flux(i,j,k,iflux) = ( Flux(i,j,k,iflux) +
     &             Hz(i,j,k) * ThisFlux(k, iflux) / dt )
# ifdef MASKING
     &             * rmask(i,j)
# endif /* MASKING */
           end do
        end do
#endif /* BGC_FLUX_ANALYSIS */

#if defined OXYGEN
! ThisGasExcFlux is already in the correct units [mmol m^-2 s^-1]
        do iflux = 1, NumGasExcTerms
           GasExcFlux(i,j,iflux) = ( GasExcFlux(i,j,iflux) +
     &          ThisGasExcFlux(iflux) / float(ITERMAX) )
# ifdef MASKING
     &          * rmask(i,j)
# endif
        end do
#endif /* OXYGEN */

#if defined SEDIMENT_BIOLOGY || defined BGC_FLUX_ANALYSIS 
        iflux_loop: do iflux = 1, NumVSinkTerms
! Compute the vertical sinking flux into the sediment by comparing 
! previous and current mass in this (i,j) column
! The flux is positive if upward, so usually it will be negative, i.e.
! into the sediment.
#ifdef BGC_FLUX_ANALYSIS
           LastVSinkFlux = ( ColumnMassNew(iflux) -
     &          ColumnMassOld(iflux) ) / dtsec
#endif /* BGC_FLUX_ANALYSIS */
# ifdef SEDIMENT_BIOLOGY
           if (iflux .lt. 4) then
! Phytoplankton, SDetN, LDetN
              SedOrgN = SedOrgN + ColumnMassOld(iflux) - 
     &             ColumnMassNew(iflux)
              
#  ifdef CARBON
           else if (iflux .lt. 6) then
! SDetC, LDetC
              SedOrgC = SedOrgC + ColumnMassOld(iflux) - 
     &             ColumnMassNew(iflux)      
           else if (iflux .eq. 6) then
! CaCO3
              SedCaCO3 = SedCaCO3 + ColumnMassOld(iflux) - 
     &             ColumnMassNew(iflux)
#  endif /* CARBON */
           end if
#  ifdef CARBON
           if (iflux .eq. 1) then
! C content of phytoplankton
              SedOrgC = SedOrgC + (ColumnMassOld(iflux) - 
     &             ColumnMassNew(iflux) ) * CN_P
           end if
#  endif /* CARBON */
# endif /* SEDIMENT_BIOLOGY */
# ifdef BGC_FLUX_ANALYSIS 
           VSinkFlux(i,j,0,iflux) = ( VSinkFlux(i,j,0,iflux) +
     &          LastVSinkFlux / float(ITERMAX) )
#  ifdef MASKING
     &          * rmask(i,j)
#  endif /* MASKING */
           do k = 1, N
              LastVSinkFlux = LastVSinkFlux + 
     &             ( ThisVSinkFlux(k,iflux) * Hz(i,j,k) ) / dtsec

              VSinkFlux(i,j,k,iflux) = 
     &             ( VSinkFlux(i,j,k,iflux) + 
     &             LastVSinkFlux / float(ITERMAX) )
#  ifdef MASKING
     &             * rmask(i,j)
#  endif /* MASKING */
           end do
#endif /* BGC_FLUX_ANALYSIS */
        end do iflux_loop
#endif /* SEDIMENT_BIOLOGY || BGC_FLUX_ANALYSIS */

#ifdef SEDIMENT_BIOLOGY
!         remineralization of sedimented organic material      
          cff1=dtdays*t_SedRemin
          SedOrgN=SedOrgN/(1.+cff1)
!         the sediment concentration is in mmol m-2
          ThisSedFlux(NFlux_ReminSed) = SedOrgN * cff1
          NH4(1) = NH4(1) + ThisSedFlux(NFlux_ReminSed) / Hz(i,j,1)

# ifdef OXYGEN
# ifdef OXYLIM_SED
!         Include loss of NO3 from denitrification in the sediments
!         when O2 is low. Middleburg 1996 parameterization
          
          O2_req = ON_NH4*ThisSedFlux(NFlux_ReminSed) / Hz(i,j,1)

          if (O2(1) .lt. O2_req) then

!          Put organic sediment flux into units of umol C cm-2 s-1
           Flc = ThisSedFlux(NFlux_ReminSed) * (CN_P * 8640)

!         More complicated Middleburg parameterization
!          den=-2.2567-0.185 * log(Flc)- 0.221 * log(Flc) * 
!     &         log(Flc) - 0.3995 * log(NO3(1)) * log(O2(1)) + 
!     &         1.25 * log(NO3(1)) + 0.4721 * log(O2(1)) - 
!     &         0.0996 * log(h) + o.4256 * log(Flc) * log(O2(1))

          den = -0.9543 + 0.7662 * log(Flc) - 0.235 * log(Flc)*log(Flc)

!         Convert back to mmol N m-2 d-1
          NO3_req = (exp(den) / CN_P * 10) / Hz(i,j,1)

            if (NO3(1) .ge. NO3_req) then
               ThisSedFlux(NFlux_DenitrSed) = exp(den) / (CN_P * 8640)
               NO3(1) = NO3(1) - ThisSedFlux(NFlux_DenitrSed) / Hz(i,j,1)
            else
               ThisSedFlux(NFlux_DenitrSed) = 0
            end if
          else
             ThisSedFlux(NFlux_DenitrSed) = 0
# endif /* OXYLIM_SED */
!         Loss of O2 in LDetC/LDetN remineralization
          O2(1) = O2(1) - ON_NH4*ThisSedFlux(NFlux_ReminSed) / Hz(i,j,1)

# ifdef OXYLIM_SED
          end if
# endif /* OXYLIM_SED */

# endif /* OXYGEN */
# ifdef CARBON
          SedOrgC=SedOrgC/(1.+cff1)
!         the sediment concentration is in mmol m-2
          ThisSedFlux(CFlux_ReminSed) = SedOrgC * cff1
          DIC(1) = DIC(1) + ThisSedFlux(CFlux_ReminSed) / Hz(i,j,1) 

!         Dissolution of CaCO3
          cff1 = dtdays * t_SedDissCaCO3
          SedCaCO3 = SedCaCO3/(1.+cff1)
!         the sediment concentration is in mmol m-2
          ThisSedFlux(CFlux_DissolvSed) = SedCaCO3 * cff1
          DIC(1) = DIC(1) + ThisSedFlux(CFlux_DissolvSed) / Hz(i,j,1)
          TAlk(1) = TAlk(1) + 2. * ThisSedFlux(CFlux_DissolvSed) / 
     &         Hz(i,j,1)
# endif /* CARBON */

# ifdef BGC_FLUX_ANALYSIS
           do iflux = 1, NumSedFluxTerms
              SedFlux(i,j,iflux) = SedFlux(i,j,iflux) +
     &             ThisSedFlux(iflux) / dt
           end do

# endif /* BGC_FLUX_ANALYSIS */
#endif /* SEDIMENT_BIOLOGY */
      end do iter_loop

!
! Write back
!
          k_loop11: do k=1,N
           t(i,j,k,nnew,iNO3_)=min(t(i,j,k,nnew,iNO3_),0.)   + NO3(k)
           t(i,j,k,nnew,iNH4_)=min(t(i,j,k,nnew,iNH4_),0.)   + NH4(k)
           t(i,j,k,nnew,iPhyt)=min(t(i,j,k,nnew,iPhyt),0.) + Phyt(k)
           t(i,j,k,nnew,iZoo_)=min(t(i,j,k,nnew,iZoo_),0.)   + Zoo(k)
           t(i,j,k,nnew,iSDet)=min(t(i,j,k,nnew,iSDet),0.) + SDetN(k)
           t(i,j,k,nnew,iLDet)=min(t(i,j,k,nnew,iLDet),0.) + LDetN(k)
           t(i,j,k,nnew,iChla)=min(t(i,j,k,nnew,iChla),0.) +
     &                                 CN_P*12.*Phyt(k)*theta(k)
#ifdef OXYGEN
           t(i,j,k,nnew,iO2)=min(t(i,j,k,nnew,iO2),0.) + O2(k)
#endif /* OXYGEN */

#ifdef CARBON
           t(i,j,k,nnew,iDIC)=min(t(i,j,k,nnew,iDIC),0.)    +DIC(k)
           t(i,j,k,nnew,iTALK)=min(t(i,j,k,nnew,iTALK),0.)  +TALK(k)
           t(i,j,k,nnew,iSDetC)=min(t(i,j,k,nnew,iSDetC),0.)+SDetC(k)
           t(i,j,k,nnew,iLDetC)=min(t(i,j,k,nnew,iLDetC),0.)+LDetC(k)
           t(i,j,k,nnew,iCaCO3)=min(t(i,j,k,nnew,iCaCO3),0.)+CaCO3(k)
#endif /* CARBON */
          end do k_loop11
#ifdef OXYGEN
          O2satu(i,j) = O2satu_loc
          Kv_O2(i,j) = Kv_O2_loc
          u10(i,j) = u10_loc
#endif /* OXYGEN */

#ifdef CARBON
          pCO2sw(i,j) = pCO2sw_loc
          CO2sol(i,j) = CO2sol_loc
#  ifdef OCMIP_CARBON
          ph_hist(i,j) = pHsw_loc
#   ifndef PCO2AIR_FORCING
          pCO2air(i,j) = pCO2air_loc
#   endif
          Kv_CO2(i,j) = Kv_CO2_loc
#  endif /* OCMIP_CARBON */
#endif /* CARBON */

# ifdef SEDIMENT_BIOLOGY
          t_sed(i,j,iSedOrgN) = SedOrgN
#  ifdef CARBON
          t_sed(i,j,iSedOrgC) = SedOrgC
          t_sed(i,j,iSedCaCO3) = SedCaCO3
#  endif /* CARBON */
# endif /* SEDIMENT_BIOLOGY */
# ifdef MASKING
        end if rmask_if
# endif /* MASKING */
        end do i_loop
      end do j_loop

#else /* BIOLOGY_NPZDOC */
      subroutine biology_npzdoc_empty ()
#endif /* BIOLOGY_NPZDOC */
      return
      end
