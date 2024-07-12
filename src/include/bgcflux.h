! This header file contains all variables for the
! computation of biogeochemical fluxes.
! It must always be included in the biology and some other routines.
! The variables for the netcdf output are in ncroms_bgcflux.h, which
! needs to be included only if BGC_FLUX_ANALYSIS is defined.

#if defined SOLVE3D && defined BIOLOGY_NPZDOC

! Fluxes: all bgc fluxes are defined in a way that they will be positive under
! normal circumstances
      ! fluxes of Nitrogen [mmol N m^-2 s^-1]
      integer NFlux_NewProd, NFlux_RegProd, NFlux_Grazing
      integer NFlux_SlopFeed, NFlux_Zfecp, NFlux_Pmort, NFlux_Zmetab
      integer NFlux_Zexcr, NFlux_ZmortS, NFlux_ZmortL, NFlux_DetCoagP
      integer NFlux_DetCoagD, NFlux_ReminS, NFlux_ReminL, NFlux_Nitrif
      integer NumFluxTermsN, NumFluxTerms
      integer NFlux_Theta, NFlux_PhytoLimTemp, NFlux_PhytoLimNO3
      integer NFlux_PhytoLimNH4, NFlux_PhytoLimTempLight
      ! DL: number of volume fluxes: does not include diagnostics related
      ! related to phyto growth
      integer NumFluxTermsVol

      parameter(NFlux_NewProd = 1)   ! new production (NO3 -> Phyto.)
      parameter(NFlux_RegProd = 2)   ! regenerated prod. (NH4 -> Phyto.)
      parameter(NFlux_Grazing = 3)   ! grazing (Phytoplankton -> Zoo.)
      parameter(NFlux_SlopFeed = 4)  ! sloppy feeding (Phyto. -> SDetN)
      parameter(NFlux_Zfecp = 5)     ! fecal pellets (Phyto. -> LDetN)
      parameter(NFlux_Pmort = 6)     ! Phyto. mortality (Phy. -> SDetN)
      parameter(NFlux_Zmetab = 7)    ! Zoopl. metabolism (Zoo. -> NH4)
      parameter(NFlux_Zexcr = 8)     ! Zoopl. excretion (Zoo. -> NH4)
      parameter(NFlux_ZmortS = 9)     ! Zoopl. mortality (Zoo. -> SDetN)
      parameter(NFlux_ZmortL = 10)    ! Zoopl. mortality (Zoo. -> LDetN)
      parameter(NFlux_DetCoagP = 11)  ! Coagul. of detr. (Phy. -> LDetN)
      parameter(NFlux_DetCoagD = 12) ! Coagul. of detr. (SDetN -> LDetN)
      parameter(NFlux_ReminS = 13)   ! Remineralization (SDetN -> NH4)
      parameter(NFlux_ReminL = 14)   ! Remineralization (LDetN -> NH4)
      parameter(NFlux_Nitrif = 15)   ! Nitrification (NH4 -> NO3)
# ifdef OXYLIM
      integer NFlux_DenitrS, NFlux_DenitrL
      parameter(NFlux_DenitrS = 16)  ! Denitrification (SdetC -> DIC, consuming DIC)
      parameter(NFlux_DenitrL = 17)
      parameter(NumFluxTermsN = NFlux_DenitrL)
# else
      parameter(NumFluxTermsN = NFlux_Nitrif)
# endif /* OXYLIM */
#   ifdef CARBON
      integer CFlux_Zresp
      integer CFlux_DetCoagD, CFlux_ReminS, CFlux_ReminL, CFlux_Dissolv
      integer NumFluxTermsC
      ! fluxes of and changes in carbon [mmol C m^-2 s^-1]
!DL: CFlux_Zresp is always NumFluxTermsN+1:
      parameter(CFlux_Zresp = NumFluxTermsN + 1)      ! Zoopl. respiration (Zoo. -> DIC)
!#ifdef OXYLIM
!      parameter(CFlux_Zresp = NFlux_DenitrL + 1)      ! Zoopl. respiration (Zoo. -> DIC)
!#else
!      parameter(CFlux_Zresp = NFlux_Nitrif + 1)      ! Zoopl. respiration (Zoo. -> DIC)
!#endif /* OXYLIM */
      parameter(CFlux_DetCoagD = CFlux_Zresp + 1)! Coagul. of det. (SDetC -> LDetC)
      parameter(CFlux_ReminS = CFlux_DetCoagD + 1)  ! Remineralization (SDetC -> NH4)
      parameter(CFlux_ReminL = CFlux_ReminS + 1)    ! Remineralization (LDetC -> NH4)
      parameter(CFlux_Dissolv = CFlux_ReminL + 1)   ! Dissolution of CaCO3 (CaCO3 -> DIC)
      parameter(NumFluxTermsC = CFlux_Dissolv - NumFluxTermsN)

      parameter(NumFluxTermsVol = NumFluxTermsN + NumFluxTermsC)
#   else /* CARBON */
      parameter(NumFluxTermsVol = NumFluxTermsN)
#   endif /* CARBON */

!DL: some diagnustic variables related to phyto growth:
      parameter(NFlux_Theta = NumFluxTermsVol + 1)    ! Ratio chl/C [mg Chla (mg C)-1]
      parameter(NFlux_PhytoLimTemp = NFlux_Theta + 1) ! Temperature limitation factor of P
      parameter(NFlux_PhytoLimNO3 = NFlux_PhytoLimTemp + 1)  ! NO3 nutrient limitation factor of P
      parameter(NFlux_PhytoLimNH4 = NFlux_PhytoLimNO3 + 1)  ! NH4 nutrient limitation factor of P
      parameter(NFlux_PhytoLimTempLight = NFlux_PhytoLimNH4 + 1) ! Temperature-dependent, light-limited growth rate of P

      parameter(NumFluxTerms = NumFluxTermsVol + 5)

!DL: moved since the gas exchange fluxes are always output
!      integer NumGasExcTerms
!#   ifdef OXYGEN
!      ! gas exchange fluxes
!      integer OFlux_GasExc
!      parameter(OFlux_GasExc = 1)
!#    ifdef CARBON
!      integer CFlux_GasExc
!      parameter(CFlux_GasExc = 2)
!      parameter(NumGasExcTerms = 2)
!#    else /* CARBON */
!      parameter(NumGasExcTerms = 1)
!#    endif /* CARBON */
!#   else /* OXYGEN */
!      parameter(NumGasExcTerms = 0)
!#   endif /* OXYGEN */

      ! vertical sinking fluxes
      integer PFlux_VSink, SDNFlux_VSink, LDNFlux_VSink, NumVSinkTerms
      parameter(PFlux_VSink = 1)    ! phytoplankton
      parameter(SDNFlux_VSink = 2)  ! small detritus N
      parameter(LDNFlux_VSink = 3)  ! large detritus N
#   ifdef CARBON
      integer SDCFlux_VSink, LDCFlux_VSink, CaCO3Flux_VSink
      parameter(SDCFlux_VSink = 4)  ! small detritus C
      parameter(LDCFlux_VSink = 5)  ! large detritus C
      parameter(CaCO3Flux_VSink = 6)! CaCO3
      parameter(NumVSinkTerms = 6)
#   else /* CARBON */
      parameter(NumVSinkTerms = 3)
#   endif /* CARBON */
#   ifdef SEDIMENT_BIOLOGY
      integer NFlux_ReminSed, NumSedFluxTerms
      parameter(NFlux_ReminSed = 1) ! Remineralization (Sed. OrgN -> NH4)
#   ifdef CARBON
#   ifdef OXYLIM_SED
      integer NFlux_DenitrSed
      parameter(NFlux_DenitrSed = NFlux_ReminSed + 1) ! NO3 consumption during benthic denitr
#   endif /* OXYLIM_SED */
      integer CFlux_ReminSed, CFlux_DissolvSed
#   ifdef OXYLIM_SED
      parameter(CFlux_ReminSed = NFlux_DenitrSed + 1) ! Remineralization (Sed. OrgC -> DIC)
#   else /* OXYLIM_SED */
      parameter(CFlux_ReminSed = NFlux_ReminSed + 1) ! Remineralization (Sed. OrgC -> DIC)
#   endif /* OXYLIM_SED */
      parameter(CFlux_DissolvSed = CFlux_ReminSed + 1) ! Dissolution of SedCaCO3
      parameter(NumSedFluxTerms = CFlux_DissolvSed)
#   else /* CARBON */
      parameter(NumSedFluxTerms = NFlux_ReminSed)
#    endif /* CARBON */
#   endif /* SEDIMENT_BIOLOGY */

! The PARinc value represents the incoming radiation. PAR values are
! at rho-points, which is the value used for determining new and
! regenerated production. All values are averaged over the number of
! biological time steps.
      real PAR(GLOBAL_2D_ARRAY,N)
      real PARinc(GLOBAL_2D_ARRAY)
      common /ocean_par/PAR,PARinc

#ifdef OXYGEN
      real u10(GLOBAL_2D_ARRAY)
      real Kv_O2(GLOBAL_2D_ARRAY)
      real O2satu(GLOBAL_2D_ARRAY)
      common /gasexc_o2/ u10, Kv_O2, O2satu
#  ifdef CARBON
      real Kv_CO2(GLOBAL_2D_ARRAY)
      real CO2sol(GLOBAL_2D_ARRAY)
      !DL: use pH_hist instead of pHsw:
      !real pHsw(GLOBAL_2D_ARRAY)
      real pH_hist(GLOBAL_2D_ARRAY)
      real pCO2sw(GLOBAL_2D_ARRAY)
      !common /gasexc_co2/Kv_CO2, CO2sol, pHsw, pCO2sw, pCO2air
      common /gasexc_co2/Kv_CO2, CO2sol, pH_hist, pCO2sw !, pCO2air
#   ifndef PCO2AIR_FORCING
      real pCO2air(GLOBAL_2D_ARRAY)
      common /gasexc_co2/ pCO2air
#   endif
#   ifndef N2OAIR_FORCING
      real N2Oair(GLOBAL_2D_ARRAY)
      common /gasexc_n2o/ N2Oair
#   endif
#  endif /* CARBON */
#endif /* OXYGEN */

# ifdef SEDIMENT_BIOLOGY
! These components represent tracer concentrations
! in the sediment. The unit is mmol m-2.
      real t_sed(GLOBAL_2D_ARRAY,NT_sed)
      common /ocean_t_sed/t_sed
      real t_sed_avg(GLOBAL_2D_ARRAY,NT_sed)
      common /ocean_t_sed_avg/t_sed_avg
#  ifdef SLICE_AVG
      real t_sed_slavg(GLOBAL_2D_ARRAY,NT_sed)
      common /ocean_t_sed_slavg/t_sed_slavg
#  endif
# endif /* SEDIMENT_BIOLOGY */

      real*QUAD global_sum(0:2*NT+1)
#  if defined SALINITY && defined VFLX_CORR
     &     , global_srf_sum(0:NT)
#  endif /* SALINITY && VFLX_CORR */
      common /communicators_rq_npzdoc/ global_sum
# if defined SALINITY && defined VFLX_CORR
     &     , global_srf_sum
# endif /* SALINITY && VFLX_CORR */

# ifdef BGC_FLUX_ANALYSIS
#  ifdef SEDIMENT_BIOLOGY
      real SedFlux(GLOBAL_2D_ARRAY,NumSedFluxTerms)
      common /ocean_bgc_sedflux/SedFlux
#  endif /* SEDIMENT_BIOLOGY */

      real Flux(GLOBAL_2D_ARRAY,N,NumFluxTerms)
      common /ocean_bgc_flux/Flux

!DL: is always output to avg and his tracer files:
!#  ifdef OXYGEN
!      real GasExcFlux(GLOBAL_2D_ARRAY,NumGasExcTerms)
!      common /ocean_bgc_gasexcflux/GasExcFlux
!#  endif /* OXYGEN */

      real VSinkFlux(GLOBAL_2D_ARRAY,0:N,NumVSinkTerms)
      common /ocean_bgc_vsinkflux/VSinkFlux

# endif /* BGC_FLUX_ANALYSIS */

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# ifdef AVERAGES

      real PAR_avg(GLOBAL_2D_ARRAY,N)
      real PARinc_avg(GLOBAL_2D_ARRAY)
      common /avg_par/ PAR_avg,PARinc_avg
#  ifdef SLICE_AVG
      real PAR_slavg(GLOBAL_2D_ARRAY)
      real PARinc_slavg(GLOBAL_2D_ARRAY)
      common /slavg_par/ PAR_slavg,PARinc_slavg
#  endif

#  ifdef BGC_FLUX_ANALYSIS
      real PAR_flux_avg(GLOBAL_2D_ARRAY,N)
      real PARinc_flux_avg(GLOBAL_2D_ARRAY)
      common /avg_par_flux/ PAR_flux_avg,PARinc_flux_avg

      real zeta_bgc_flux_avg(GLOBAL_2D_ARRAY)
      common /ocean_zeta_bgc_flux_avg/zeta_bgc_flux_avg
#  endif /* BGC_FLUX_ANALYSIS */

#  ifdef OXYGEN
      real u10_avg(GLOBAL_2D_ARRAY)
      real Kv_O2_avg(GLOBAL_2D_ARRAY)
      real O2satu_avg(GLOBAL_2D_ARRAY)
      common /gasexc_o2_avg/ u10_avg, Kv_O2_avg, O2satu_avg
#   ifdef CARBON
      real Kv_CO2_avg(GLOBAL_2D_ARRAY)
      real CO2sol_avg(GLOBAL_2D_ARRAY)
      real pco2_avg(GLOBAL_2D_ARRAY)
      real pCO2air_avg(GLOBAL_2D_ARRAY)
      real pH_avg(GLOBAL_2D_ARRAY)
      common /gasexc_co2_avg/ Kv_CO2_avg, CO2sol_avg, pco2_avg,
     &     pCO2air_avg, pH_avg
#    ifdef SLICE_AVG
      real Kv_CO2_slavg(GLOBAL_2D_ARRAY)
      real CO2sol_slavg(GLOBAL_2D_ARRAY)
      real pco2_slavg(GLOBAL_2D_ARRAY)
      real pCO2air_slavg(GLOBAL_2D_ARRAY)
      real pH_slavg(GLOBAL_2D_ARRAY)
      common /gasexc_co2_slavg/ Kv_CO2_slavg, CO2sol_slavg,
     &        pco2_slavg, pCO2air_slavg, pH_slavg
#    endif /* SLICE_AVG */
#   endif /* CARBON */
#  endif /* OXYGEN */

#  ifdef BGC_FLUX_ANALYSIS
#   ifdef SEDIMENT_BIOLOGY
      real SedFlux_avg(GLOBAL_2D_ARRAY,NumSedFluxTerms)
      common /ocean_bgc_sedflux_avg/SedFlux_avg
#   endif /* SEDIMENT_BIOLOGY */

      real Flux_avg(GLOBAL_2D_ARRAY,N,NumFluxTerms)
      common /bgc_flux_avg/Flux_avg

!DL:
!#   ifdef OXYGEN
!      real GasExcFlux_avg(GLOBAL_2D_ARRAY,NumGasExcTerms)
!      common /bgc_gasexcflux_avg/GasExcFlux_avg
!#   endif /* OXYGEN */

      real VSinkFlux_avg(GLOBAL_2D_ARRAY,0:N,NumVSinkTerms)
      common /bgc_vsinkflux_avg/VSinkFlux_avg
#  endif /* BGC_FLUX_ANALYSIS */
# endif /* AVERAGES */
#endif /* SOLVE3D && BIOLOGY_NPZDOC */
