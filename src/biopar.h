! 
! Values as in Table 1; Moisan et al. [in prep.]
! An "*" indicates that the parameters and/or their ranges are poorly understood
#ifdef BIOLOGY_NPZDOC
      real kwater   ! light attenuation due to sea water 
                    ! range:(0.04<==>0.04];        [m-1]
      real kChla    ! light attenuation by Chlor
                    ! range:(0.025<==>0.025);      [(m^2 mg Chla)-1]
      real CN_P     ! C:N ratio for Phyt
                    ! range:(6.625<==>6.625);      [mol-C (mol-N)-1]
      real CN_Z     ! C:N ratio for zoo
                    ! range:(4.<==>6.);            [mol-C (mol-N)-1]
#ifdef CARBON
      real r_CaCO3_orgC ! ratio of CaCO3 precipitation to NPP  [-]
      real t_dissCaCO3  ! Dissolution rate of CaCO3 [d-1]
#endif /* CARBON */
      real theta_m  ! maximum Cellular Chlor to C Ratio 
                    ! range:(0.015<==>0.072);      [mg Chla (mg C)-1]
      real palpha   !*initial slope of the P-I curve
      !*range:(1.00<==>1.00);        [mg C (mg Chla W m-2 d)-1]
      real K_NO3    !*inverse half-saturation for Phyt NO3 uptake 
                    ! range:(1./.0 <==> 1./.9);    [1/(mmol-N m-3)]
      real K_NH4    !*inverse half-saturation for Phyt NH4 uptake
                    ! range:(1./.0 <==> 1./.62 );  [1/(mmol-N m-3)]
      real t_Pmort  !*Phyt mortality to SDetN rate
                    ! range:(0. <==> ?);           [d-1]
      real t_Zgraz  !*Zoo-specific maximum grazing rate 
                    ! range:(0. <==> ?);           [d-1]
      real AE_N     !*Zoo assimilation efficiency for N
                    ! range:(0. <==> 1.);          [n.d.]
#ifdef CARBON
      real AE_C     !*Zoo assimilation efficiency for C
                    ! range:(0. <==> 1.);          [n.d.]
#endif /* CARBON */
      real GGE_C    !*Zoo gross growth efficiency for C (GGE_C < AE_C)
                    ! range:(0. <==> 1.);          [n.d.]
      real K_P      !*Zoo half-saturation constant for ingestion
                    ! range:(0. <==> ?);           [mmol-N m-3]
      real t_Zbmet  !*Zoo specific excretion rate 
                    ! range:(0. <==> ?);           [d-1]
      real t_Zmort  !*Zoo quadratic mortality to Detritus
                    ! range:(0. <==> ?);           [d-1 (mmol-N m-3)-1]
      real Zgamma   ! fraction of sloppy feeding that goes into
                    ! fecal pellet production to LDET    [-]
      real Zomega   ! fraction of Zooplankton mortality that goes into LDET [-]
      real t_SDremin !*Small Detrital breakdown to NH4 rate 
                     ! range:(0. <==> ?);           [d-1]
      real t_LDremin !*Specific rate of LDetN recycling to NH4 
                     ! range:(0. <==> ?);           [d-1]
#ifdef SEDIMENT_BIOLOGY
      real t_SedRemin !*Specific rate of sedimented OrgN recycling to NH4
                      ! range:(0. <==> ?);           [d-1]
# ifdef CARBON
      real t_SedDissCaCO3 !*Specific rate of sedimented CaCO3 dissolution
                      ! range:(0. <==> ?);           [d-1]
# endif /* CARBON */
#endif /* SEDIMENT_BIOLOGY */
      real t_coag    !*Specific (per unit Phyt+SDetN) aggregation rate
                     !       SDetN+Phyt ==> LDetN 
                     ! range:(0. <==> ?);           [(mmol-N m-3)-1 d-1]
      real t_nitri   !*Oxidation of NH4 to NO3 (Nitrification) 
                     ! range:(0. <==> ?);           [d-1]
      real I_thNH4   ! threshold PAR for nitrification inhibition 
                     ! range:(0. <==> ?);           [Watts m-2]
      real D_p5NH4   ! 0.5 dose for nitrification inhibition
                     ! range:(0. <==> ?);           [Watts m-2]
#if defined OXYGEN 
      real ON_NO3    ! O2:N ratio for uptake of nitrate
                     ! [mol-O2 (mol-N)-1]
      real ON_NH4    ! O2:N ratio for uptake of ammonium
                     ! [mol-O2 (mol-N)-1]
#ifdef OXYLIM
      real N_CDET    ! NO3:DIC ratio for denitrification 
#endif /*OXYLIM*/

#endif /* OXYGEN */
      real wSDet     !*Sinking velocity for SDetN 
                     ! range:(0. <==> ?);           [m d-1]
      real wLDet     !*Sinking velocity for LDetN
                     ! range:(0. <==> ?);           [m d-1]
      real wPhyt     !*Sinking velocity for Phyt
                     ! range:(0. <==> ?);           [m d-1]
#ifdef CARBON
      real wCaCO3    !*Sinking velocity for CaCO3   [m d-1]
      real pHeps     ! variation of pH value for iterative solver [-]
#endif /* CARBON */
      real Vp0       ! "Fudge factor" for phytoplankton growth rate [-]

      common /biology_param/ kwater, kChla, CN_P, CN_Z
#ifdef CARBON
     &     , r_CaCO3_orgC, t_dissCaCO3, AE_C
#endif /* CARBON */
     &     , GGE_C
     &     , theta_m, palpha, K_NO3, K_NH4, t_Pmort, t_Zgraz, AE_N, K_P
     &     , t_Zbmet, t_Zmort, Zgamma, Zomega, t_SDremin, t_LDremin
#ifdef SEDIMENT_BIOLOGY
     &     , t_SedRemin
# ifdef CARBON
     &     , t_SedDissCaCO3
# endif /* CARBON */
#endif /* SEDIMENT_BIOLOGY */
     &     , t_coag, t_nitri, I_thNH4, D_p5NH4
#if defined OXYGEN
     &     , ON_NO3, ON_NH4
#endif /* OXYGEN */
#ifdef OXYLIM
     &     , N_CDET
#endif /* OXYLIM */
     &     , wSDet, wLDet, wPhyt
#ifdef CARBON
     &     , wCaCO3, pHeps
#endif /* CARBON */
     &     , Vp0

      integer ITERMAX
      parameter (ITERMAX = 1)      ! number of small implicit time steps
      
      namelist /biology_param_nml/ kwater, kChla, CN_P, CN_Z
#ifdef CARBON
     &     , r_CaCO3_orgC, t_dissCaCO3, AE_C, GGE_C
#endif /* CARBON */
     &     , theta_m, palpha, K_NO3, K_NH4, t_Pmort, t_Zgraz, AE_N, K_P 
     &     , t_Zbmet, t_Zmort, Zgamma, Zomega, t_SDremin, t_LDremin
#ifdef SEDIMENT_BIOLOGY
     &     , t_SedRemin
# ifdef CARBON
     &     , t_SedDissCaCO3
# endif /* CARBON */
#endif /* SEDIMENT_BIOLOGY */
     &     , t_coag, t_nitri, I_thNH4, D_p5NH4
#if defined OXYGEN
     &     , ON_NO3, ON_NH4
#ifdef OXLIM
     &     , N_CDET
#endif /* OXYLIM */
#endif /* OXYGEN */
     &     , wSDet, wLDet, wPhyt
#ifdef CARBON
     &     , wCaCO3, pHeps
#endif /* CARBON */
     &     , Vp0

 
#endif /* BIOLOGY_NPZDOC */
