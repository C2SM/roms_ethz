!zhc:  This file includes variables originally defined in ecosys_mod.F90 from WHOI

!MODHF      integer kmt(GLOBAL_2D_ARRAY)
! k index of deepest grid cell on T grid

!HF       real dz(GLOBAL_2D_ARRAY,N),
!HF     & dzr(GLOBAL_2D_ARRAY,N),
!HF     & zt(GLOBAL_2D_ARRAY,N)
!dz: thickness of layer k
!dzr: reciprocals of dz,
!zt: vert dist from sfc to midpoint of layer

       real tracer(GLOBAL_2D_ARRAY,N,ntrc_bio,2)
! Initial tracers values for the bottom layer of the one dimension configuration (JDS)
       real INITIAL_TRACER(GLOBAL_2D_ARRAY,ntrc_bio)
       real initial_temp(GLOBAL_2D_ARRAY)
       real initial_salt(GLOBAL_2D_ARRAY)

        common /tracers/ tracer,initial_tracer,initial_temp,initial_salt
!MODHF        common /vert/ kmt
!HF        common /dzc/dz
!HF        common /dzrc/dzr
!HF        common /ztc/zt

        real ifrac(GLOBAL_2D_ARRAY),
     &    press(GLOBAL_2D_ARRAY)
!    IFRAC  sea ice fraction (non-dimensional)
!    PRESS  sea level atmospheric pressure (Pascals)
        common /fic_ap/ifrac,press

!--------------------------------------------------------------------------
!   variables used for time-averaging
!--------------------------------------------------------------------------
# ifdef CH_CARBON_DEPTH
!ethsigma3_old       real PH_HIST(GLOBAL_2D_ARRAY,N),
!ethsigma3_old     &   HCO3_HIST(GLOBAL_2D_ARRAY,N),
!ethsigma3_old     &   CO3_HIST(GLOBAL_2D_ARRAY,N),
!ethsigma3_old     &   CO2STAR_HIST(GLOBAL_2D_ARRAY,N),
!ethsigma3_old     &   pCO2sw(GLOBAL_2D_ARRAY,N),
!ethsigma3_old     &   DCO2STAR_HIST(GLOBAL_2D_ARRAY,N)
!ethsigma3_old
!ethsigma3_old       common /time_averaging1/HCO3_HIST, CO3_HIST
!ethsigma3_old# else
!ethsigma3_old       real PH_HIST(GLOBAL_2D_ARRAY),
!ethsigma3_old     &   CO2STAR_HIST(GLOBAL_2D_ARRAY),
!ethsigma3_old     &   pCO2sw(GLOBAL_2D_ARRAY),
!ethsigma3_old     &   DCO2STAR_HIST(GLOBAL_2D_ARRAY)
       real PHd_HIST(GLOBAL_2D_ARRAY,N),
     &   HCO3d_HIST(GLOBAL_2D_ARRAY,N),
     &   CO3d_HIST(GLOBAL_2D_ARRAY,N),
     &   CO2STARd_HIST(GLOBAL_2D_ARRAY,N)

       common /time_averaging1/PHd_HIST, HCO3d_HIST,
     &   CO3d_HIST, CO2STARd_HIST

# endif /* CH_CARBON_DEPTH */

       real WS_HIST(GLOBAL_2D_ARRAY),
     &   XKW_HIST(GLOBAL_2D_ARRAY),
     &   AP_HIST(GLOBAL_2D_ARRAY),
     &   SCHMIDT_O2_HIST(GLOBAL_2D_ARRAY),
     &   O2SAT_HIST(GLOBAL_2D_ARRAY),
     &   FG_O2_HIST(GLOBAL_2D_ARRAY),
     &    SCHMIDT_CO2_HIST(GLOBAL_2D_ARRAY),
     &   PH_HIST(GLOBAL_2D_ARRAY),
     &   CO2STAR_HIST(GLOBAL_2D_ARRAY),
     &   DCO2STAR_HIST(GLOBAL_2D_ARRAY),
     &   pCO2sw(GLOBAL_2D_ARRAY),
!hf     &   DpCO2(GLOBAL_2D_ARRAY),
     &   pCO2air(GLOBAL_2D_ARRAY),
     &    FG_CO2_HIST(GLOBAL_2D_ARRAY),
     &   IRON_FLUX_HIST(GLOBAL_2D_ARRAY),
     &       PARinc(GLOBAL_2D_ARRAY)
        real
     &    PO4_RESTORE_HIST(GLOBAL_2D_ARRAY,N),
     &    NO3_RESTORE_HIST(GLOBAL_2D_ARRAY,N),
     &    SiO3_RESTORE_HIST(GLOBAL_2D_ARRAY,N),
     &   PAR(GLOBAL_2D_ARRAY,N),
     &   PO4STAR_HIST(GLOBAL_2D_ARRAY,N),
     &    POC_FLUX_IN_HIST(GLOBAL_2D_ARRAY,N),
     &    POC_PROD_HIST(GLOBAL_2D_ARRAY,N),
     &   POC_REMIN_HIST(GLOBAL_2D_ARRAY,N),
     &    CaCO3_FLUX_IN_HIST(GLOBAL_2D_ARRAY,N),
     &    CaCO3_PROD_HIST(GLOBAL_2D_ARRAY,N),
     &    CaCO3_REMIN_HIST(GLOBAL_2D_ARRAY,N),
     &    SiO2_FLUX_IN_HIST(GLOBAL_2D_ARRAY,N),
     &    SiO2_PROD_HIST(GLOBAL_2D_ARRAY,N),
     &    SiO2_REMIN_HIST(GLOBAL_2D_ARRAY,N),
     &    dust_FLUX_IN_HIST(GLOBAL_2D_ARRAY,N)

          real  dust_REMIN_HIST(GLOBAL_2D_ARRAY,N),
     &    P_iron_FLUX_IN_HIST(GLOBAL_2D_ARRAY,N),
     &    P_iron_PROD_HIST(GLOBAL_2D_ARRAY,N),
     &    P_iron_REMIN_HIST(GLOBAL_2D_ARRAY,N),
     &    graze_sp_HIST(GLOBAL_2D_ARRAY,N),
     &    graze_diat_HIST(GLOBAL_2D_ARRAY,N),
     &    graze_tot_HIST(GLOBAL_2D_ARRAY,N),
     &    sp_loss_HIST(GLOBAL_2D_ARRAY,N),
     &    diat_loss_HIST(GLOBAL_2D_ARRAY,N),
     &    zoo_loss_HIST(GLOBAL_2D_ARRAY,N),
     &    sp_agg_HIST(GLOBAL_2D_ARRAY,N),
     &    diat_agg_HIST(GLOBAL_2D_ARRAY,N)

          real  photoC_sp_HIST(GLOBAL_2D_ARRAY,N),
     &   photoC_diat_HIST(GLOBAL_2D_ARRAY,N),
     &   tot_prod_HIST(GLOBAL_2D_ARRAY,N),
     &    DOC_prod_HIST(GLOBAL_2D_ARRAY,N),
     &    DOC_remin_HIST(GLOBAL_2D_ARRAY,N),
     &    Fe_scavenge_HIST(GLOBAL_2D_ARRAY,N),
     &    sp_N_lim_HIST(GLOBAL_2D_ARRAY,N),
     &    sp_Fe_lim_HIST(GLOBAL_2D_ARRAY,N),
     &     sp_PO4_lim_HIST(GLOBAL_2D_ARRAY,N),
     &    sp_light_lim_HIST(GLOBAL_2D_ARRAY,N),
     &    diat_N_lim_HIST(GLOBAL_2D_ARRAY,N),
     &     diat_Fe_lim_HIST(GLOBAL_2D_ARRAY,N),
     &    diat_PO4_lim_HIST(GLOBAL_2D_ARRAY,N),
     &    diat_SiO3_lim_HIST(GLOBAL_2D_ARRAY,N)

         real diat_light_lim_HIST(GLOBAL_2D_ARRAY,N),
     &   CaCO3_form_HIST(GLOBAL_2D_ARRAY,N),
     &    diaz_Nfix_HIST(GLOBAL_2D_ARRAY,N),
     &    graze_diaz_HIST(GLOBAL_2D_ARRAY,N),
     &    diaz_loss_HIST(GLOBAL_2D_ARRAY,N)
        real photoC_diaz_HIST(GLOBAL_2D_ARRAY,N),
     &    diaz_P_lim_HIST(GLOBAL_2D_ARRAY,N),
     &    diaz_Fe_lim_HIST(GLOBAL_2D_ARRAY,N),
     &     diaz_light_lim_HIST(GLOBAL_2D_ARRAY,N),
     &     Fe_scavenge_rate_HIST(GLOBAL_2D_ARRAY,N),
     &    DON_prod_HIST(GLOBAL_2D_ARRAY,N),
     &    DON_remin_HIST(GLOBAL_2D_ARRAY,N),
     &    DOFe_prod_HIST(GLOBAL_2D_ARRAY,N)

         real  DOFe_remin_HIST(GLOBAL_2D_ARRAY,N),
     &   DOP_prod_HIST(GLOBAL_2D_ARRAY,N),
     &    DOP_remin_HIST(GLOBAL_2D_ARRAY,N),
     &    bSI_form_HIST(GLOBAL_2D_ARRAY,N),
     &    photoFe_diaz_HIST(GLOBAL_2D_ARRAY,N),
     &    photoFe_diat_HIST(GLOBAL_2D_ARRAY,N),
     &    photoFe_sp_HIST(GLOBAL_2D_ARRAY,N),
     &    nitrif_HIST(GLOBAL_2D_ARRAY,N)

#ifdef OXYLIM_BEC
          real denitr_DOC_HIST(GLOBAL_2D_ARRAY,N),
     &    denitr_POC_HIST(GLOBAL_2D_ARRAY,N)
#endif

       common /time_averaging1/WS_HIST, XKW_HIST,
     &   AP_HIST, SCHMIDT_O2_HIST, O2SAT_HIST, FG_O2_HIST,
     &    SCHMIDT_CO2_HIST, PH_HIST, CO2STAR_HIST,
     &    DCO2STAR_HIST, pCO2sw,pCO2air,
!hf DpCO2,
     &    FG_CO2_HIST, IRON_FLUX_HIST, PARinc,
     &    PO4_RESTORE_HIST, NO3_RESTORE_HIST,
     &    SiO3_RESTORE_HIST, PAR, PO4STAR_HIST,
     &    POC_FLUX_IN_HIST, POC_PROD_HIST, POC_REMIN_HIST,
     &    CaCO3_FLUX_IN_HIST, CaCO3_PROD_HIST,
     &    CaCO3_REMIN_HIST,  SiO2_FLUX_IN_HIST,
     &    SiO2_PROD_HIST, SiO2_REMIN_HIST, dust_FLUX_IN_HIST,
     &    dust_REMIN_HIST, P_iron_FLUX_IN_HIST,
     &    P_iron_PROD_HIST, P_iron_REMIN_HIST,
     &    graze_sp_HIST, graze_diat_HIST, graze_tot_HIST,
     &    sp_loss_HIST, diat_loss_HIST, zoo_loss_HIST,
     &    sp_agg_HIST, diat_agg_HIST,
     &    photoC_sp_HIST, photoC_diat_HIST, tot_prod_HIST,
     &    DOC_prod_HIST, DOC_remin_HIST, Fe_scavenge_HIST

       common /time_averaging2/
     &    sp_N_lim_HIST, sp_Fe_lim_HIST, sp_PO4_lim_HIST,
     &    sp_light_lim_HIST, diat_N_lim_HIST, diat_Fe_lim_HIST,
     &    diat_PO4_lim_HIST, diat_SiO3_lim_HIST,
     &    diat_light_lim_HIST, CaCO3_form_HIST,
     &    diaz_Nfix_HIST, graze_diaz_HIST, diaz_loss_HIST,
     &     photoC_diaz_HIST, diaz_P_lim_HIST,
     &    diaz_Fe_lim_HIST, diaz_light_lim_HIST,
     &     Fe_scavenge_rate_HIST, DON_prod_HIST,
     &    DON_remin_HIST, DOFe_prod_HIST,
     &    DOFe_remin_HIST, DOP_prod_HIST,
     &    DOP_remin_HIST, bSI_form_HIST,
     &    photoFe_diaz_HIST, photoFe_diat_HIST, photoFe_sp_HIST,
#ifdef OXYLIM_BEC
     &    nitrif_HIST, denitr_DOC_HIST, denitr_POC_HIST
#else
     &    nitrif_HIST
#endif


!--------------------------------------------------------------------------
!   relative tracer indices
!--------------------------------------------------------------------------

!This index is not same as the index in param.h(roms).
       integer  po4_ind , no3_ind,sio3_ind, nh4_ind,fe_ind,
     & o2_ind, dic_ind,alk_ind,doc_ind,spC_ind,spChl_ind,
     & spCaCO3_ind,diatC_ind,diatChl_ind,zooC_ind,spFe_ind,
     &  diatSi_ind,diatFe_ind,diazC_ind,diazChl_ind, diazFe_ind,
     &  don_ind,dofe_ind,dop_ind

       parameter (po4_ind=1 , no3_ind=2,sio3_ind=3, nh4_ind=4,
     &  fe_ind=5,o2_ind=6, dic_ind=7,alk_ind=8,doc_ind=9,
     &  spC_ind=10,spChl_ind=11, spCaCO3_ind=12,diatC_ind=13,
     &  diatChl_ind=14,zooC_ind=15,spFe_ind=16,
     &  diatSi_ind=17,diatFe_ind=18,diazC_ind=19,
     &  diazChl_ind=20, diazFe_ind=21,
     &  don_ind=22,dofe_ind=23,dop_ind=24)

!  po4_ind          =  1,    dissolved inorganic phosphate
!  no3_ind          =  2,    dissolved inorganic nitrate
!  sio3_ind         =  3,    dissolved inorganic silicate
!  nh4_ind          =  4,    dissolved ammonia
!  fe_ind           =  5,    dissolved inorganic iron
!  o2_ind           =  6,    dissolved oxygen
!  dic_ind          =  7,    dissolved inorganic carbon
!  alk_ind          =  8,    alkalinity
!  doc_ind          =  9,    dissolved organic carbon
!  spC_ind          = 10,    small phytoplankton carbon
!  spChl_ind        = 11,    small phytoplankton chlorophyll
!  spCaCO3_ind      = 12,    small phytoplankton caco3
!  diatC_ind        = 13,    diatom carbon
!  diatChl_ind      = 14,    diatom chlorophyll
!  zooC_ind         = 15,    zooplankton carbon
!  spFe_ind         = 16     small phytoplankton iron
!  diatSi_ind       = 17,    diatom silicon
!  diatFe_ind       = 18,    diatom iron
!  diazC_ind        = 19,    diazotroph carbon
!  diazChl_ind      = 20,    diazotroph Chlorophyll
!  diazFe_ind       = 21,    diazotroph iron
!  don_ind          = 22,    dissolved organic nitrogen
!  dofe_ind         = 23,    dissolved organic iron
!  dop_ind          = 24     dissolved organic phosphorus


!!HF        real ph_prev(GLOBAL_2D_ARRAY)
!computed ph from previous time step
!!HF        common /ph/ph_prev


       logical lsource_sink,lflux_gas_o2, lflux_gas_co2,
     &  liron_flux,ldust_flux
        common /ecoflag/lsource_sink,lflux_gas_o2,lflux_gas_co2,
     &   liron_flux,ldust_flux

!------------------------------------------------------------------------
!   restoring climatologies for nutrients
!------------------------------------------------------------------------


       logical lrest_po4,lrest_no3,lrest_sio3
!     lrest_po4,    restoring on po4
!     lrest_no3,    restoring on no3
!     lrest_sio3     restoring on sio3

       real po4_clim(GLOBAL_2D_ARRAY,N),
     &   no3_clim(GLOBAL_2D_ARRAY,N),
     &   sio3_clim(GLOBAL_2D_ARRAY,N)
           real nutr_rest_time_inv(N)
! inverse restoring time scale for nutrients (1/secs)
        common /restore_flag/lrest_po4,lrest_no3,lrest_sio3
        common /restore_clim/po4_clim,
     &      no3_clim,sio3_clim,nutr_rest_time_inv


        real sinking_particle_POC(6,GLOBAL_2D_ARRAY),
     & sinking_particle_P_CaCO3(6,GLOBAL_2D_ARRAY),
     & sinking_particle_P_sio2(6,GLOBAL_2D_ARRAY),
     & sinking_particle_dust(6,GLOBAL_2D_ARRAY),
     & sinking_particle_P_iron(6,GLOBAL_2D_ARRAY)

!        sinking_particle_POC,        base units = mmol C
!        sinking_particle_P_CaCO3,    base units = mmol CaCO3
!        sinking_particle_P_SiO2,     base units = mmol SiO2
!        sinking_particle_dust,       base units = kg
!        sinking_particle_P_iron      base units = mmol Fe

!1 for sflux_in,    incoming flux of soft subclass (base units/m^2/sec)
!2 for hflux_in,    incoming flux of hard subclass (base units/m^2/sec)
!3 for prod,        production term (base units/m^3/sec)
!4 for sflux_out,   outgoing flux of soft subclass (base units/m^2/sec)
!5 for hflux_out,   outgoing flux of hard subclass (base units/m^2/sec)
!6 for remin        remineralization term (base units/m^3/sec)

!           diss,        dissolution length for soft subclass
!           gamma,       fraction of production -> hard subclass
!           mass,         mass of 1e6 base units in kg
!           rhoo         QA mass ratio of POC to this particle class
! 1 for poc, 2 for p_caco3, 3 for p_sio2, 4 for dust, 5 for p_iron

       real diss(5),gamma(5),mass(5),rhoo(5)
      common /sinking_part/sinking_particle_POC,
     &  sinking_particle_P_CaCO3,sinking_particle_P_SiO2,
     &  sinking_particle_dust,sinking_particle_P_iron,
     &  diss,gamma,mass,rhoo



        logical landmask(GLOBAL_2D_ARRAY)
        common /calcation/landmask
