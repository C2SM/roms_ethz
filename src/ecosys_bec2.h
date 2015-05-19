!
!  tracer: values passed to biological model by ROMS
!  DTRACER_MODULE: stores the tendencies of the biological tracers
!
      real tracer(GLOBAL_2D_ARRAY,N,ntrc_bio)
      real DTRACER_MODULE(GLOBAL_2D_ARRAY,N,ntrc_bio)
      common /tracers/ tracer, DTRACER_MODULE

#ifdef BEC2_DIAG
!
! Diagnostic variables appearing in average and history files:
!
      integer nr_bec2_diag_2d, nr_bec2_diag_3d, nr_bec2_diag
<<<<<<< HEAD
      parameter( nr_bec2_diag_3d=64, nr_bec2_diag_2d=18 )
=======
      parameter( nr_bec2_diag_3d=79, nr_bec2_diag_2d=18 )
>>>>>>> BEC2
      parameter( nr_bec2_diag=nr_bec2_diag_2d+nr_bec2_diag_3d )
      real bec2_diag_3d(GLOBAL_2D_ARRAY,N,nr_bec2_diag_3d)
      real bec2_diag_2d(GLOBAL_2D_ARRAY,nr_bec2_diag_2d)

      common /bec2_diag1/ bec2_diag_2d, bec2_diag_3d

      ! Indices to be used in bec2_diag_3d:
      integer par_idx_t,pocfluxin_idx_t,pocprod_idx_t,pocremin_idx_t,caco3fluxin_idx_t,
     &        pcaco3prod_idx_t,caco3remin_idx_t,sio2fluxin_idx_t,sio2prod_idx_t,
     &        sio2remin_idx_t,dustfluxin_idx_t,dustremin_idx_t,pironfluxin_idx_t,
     &        pironprod_idx_t,pironremin_idx_t,grazesp_idx_t,grazediat_idx_t,grazediaz_idx_t,
     &        sploss_idx_t,diatloss_idx_t,zooloss_idx_t,spagg_idx_t,diatagg_idx_t,
     &        photocsp_idx_t,photocdiat_idx_t,totprod_idx_t,docprod_idx_t,docremin_idx_t,
     &        fescavenge_idx_t,spnlim_idx_t,spfeuptake_idx_t,sppo4uptake_idx_t,
     &        splightlim_idx_t,diatnlim_idx_t,diatfeuptake_idx_t,diatpo4uptake_idx_t,
     &        diatsio3uptake_idx_t,diatlightlim_idx_t,caco3prod_idx_t,diaznfix_idx_t,
     &        diazloss_idx_t,photocdiaz_idx_t,diazpo4uptake_idx_t,diazfeuptake_idx_t,diazlightlim_idx_t,
     &        fescavengerate_idx_t,donprod_idx_t,donremin_idx_t,dofeprod_idx_t,doferemin_idx_t,
     &        dopprod_idx_t,dopremin_idx_t,diatsiuptake_idx_t,ironuptakesp_idx_t,ironuptakediat_idx_t,
     &        ironuptakediaz_idx_t,nitrif_idx_t,denitrif_idx_t,spno3uptake_idx_t,diatno3uptake_idx_t,
<<<<<<< HEAD
     &        diazno3uptake_idx_t,spnh4uptake_idx_t,diatnh4uptake_idx_t,diaznh4uptake_idx_t
=======
     &        diazno3uptake_idx_t,spnh4uptake_idx_t,diatnh4uptake_idx_t,diaznh4uptake_idx_t,grazedicsp_idx_t,
     &        grazedicdiat_idx_t,grazedicdiaz_idx_t,lossdicsp_idx_t,lossdicdiat_idx_t,lossdicdiaz_idx_t,
     &        zoolossdic_idx_t,diazagg_idx_t,grazespzoo_idx_t,grazediatzoo_idx_t,grazediazzoo_idx_t,
     &        spqcaco3_idx_t,spphotoacc_idx_t,diatphotoacc_idx_t,diazphotoacc_idx_t
>>>>>>> BEC2
      parameter( par_idx_t=1,pocfluxin_idx_t=par_idx_t+1,
     &   pocprod_idx_t=par_idx_t+2,pocremin_idx_t=par_idx_t+3,caco3fluxin_idx_t=par_idx_t+4,
     &   pcaco3prod_idx_t=par_idx_t+5,caco3remin_idx_t=par_idx_t+6,sio2fluxin_idx_t=par_idx_t+7,
     &   sio2prod_idx_t=par_idx_t+8,sio2remin_idx_t=par_idx_t+9,dustfluxin_idx_t=par_idx_t+10,
     &   dustremin_idx_t=par_idx_t+11,pironfluxin_idx_t=par_idx_t+12,pironprod_idx_t=par_idx_t+13,
     &   pironremin_idx_t=par_idx_t+14,grazesp_idx_t=par_idx_t+15,grazediat_idx_t=par_idx_t+16,
     &   grazediaz_idx_t=par_idx_t+17,sploss_idx_t=par_idx_t+18,diatloss_idx_t=par_idx_t+19,
     &   zooloss_idx_t=par_idx_t+20,spagg_idx_t=par_idx_t+21,diatagg_idx_t=par_idx_t+22,
     &   photocsp_idx_t=par_idx_t+23,photocdiat_idx_t=par_idx_t+24,totprod_idx_t=par_idx_t+25,
     &   docprod_idx_t=par_idx_t+26,docremin_idx_t=par_idx_t+27,fescavenge_idx_t=par_idx_t+28,
     &   spnlim_idx_t=par_idx_t+29,spfeuptake_idx_t=par_idx_t+30,sppo4uptake_idx_t=par_idx_t+31,
     &   splightlim_idx_t=par_idx_t+32,diatnlim_idx_t=par_idx_t+33,diatfeuptake_idx_t=par_idx_t+34,
     &   diatpo4uptake_idx_t=par_idx_t+35,diatsio3uptake_idx_t=par_idx_t+36,diatlightlim_idx_t=par_idx_t+37,
     &   caco3prod_idx_t=par_idx_t+38,diaznfix_idx_t=par_idx_t+39,diazloss_idx_t=par_idx_t+40,
     &   photocdiaz_idx_t=par_idx_t+41,diazpo4uptake_idx_t=par_idx_t+42,diazfeuptake_idx_t=par_idx_t+43,
     &   diazlightlim_idx_t=par_idx_t+44,fescavengerate_idx_t=par_idx_t+45,donprod_idx_t=par_idx_t+46,
     &   donremin_idx_t=par_idx_t+47,dofeprod_idx_t=par_idx_t+48,doferemin_idx_t=par_idx_t+49,
     &   dopprod_idx_t=par_idx_t+50,dopremin_idx_t=par_idx_t+51,diatsiuptake_idx_t=par_idx_t+52,
     &   ironuptakesp_idx_t=par_idx_t+53,ironuptakediat_idx_t=par_idx_t+54,ironuptakediaz_idx_t=par_idx_t+55,
     &   nitrif_idx_t=par_idx_t+56,denitrif_idx_t=par_idx_t+57,spno3uptake_idx_t=par_idx_t+58,
     &   diatno3uptake_idx_t=par_idx_t+59,diazno3uptake_idx_t=par_idx_t+60,spnh4uptake_idx_t=par_idx_t+61,
<<<<<<< HEAD
     &   diatnh4uptake_idx_t=par_idx_t+62,diaznh4uptake_idx_t=par_idx_t+63 )
=======
     &   diatnh4uptake_idx_t=par_idx_t+62,diaznh4uptake_idx_t=par_idx_t+63,grazedicsp_idx_t=par_idx_t+64,
     &   grazedicdiat_idx_t=par_idx_t+65,grazedicdiaz_idx_t=par_idx_t+66,lossdicsp_idx_t=par_idx_t+67,
     &   lossdicdiat_idx_t=par_idx_t+68,lossdicdiaz_idx_t=par_idx_t+69,zoolossdic_idx_t=par_idx_t+70,
     &   diazagg_idx_t=par_idx_t+71,grazespzoo_idx_t=par_idx_t+72,grazediatzoo_idx_t=par_idx_t+73,
     &   grazediazzoo_idx_t=par_idx_t+74,spqcaco3_idx_t=par_idx_t+75,spphotoacc_idx_t=par_idx_t+76,
     &   diatphotoacc_idx_t=par_idx_t+77,diazphotoacc_idx_t=par_idx_t+78 )
>>>>>>> BEC2
      ! Indices to be used in bec2_diag_2d:
      integer ph_idx_t, pco2ws_idx_t, pco2air_idx_t, parinc_idx_t,
     &        fgo2_idx_t, fgco2_idx_t,ws10m_idx_t,xkw_idx_t,atmpress_idx_t,
     &        schmidto2_idx_t,o2sat_idx_t,schmidtco2_idx_t,pvo2_idx_t,
     &        pvco2_idx_t,co2star_idx_t,dco2star_idx_t,ironflux_idx_t,seddenitrif_idx_t
      parameter( ph_idx_t=1, pco2ws_idx_t=ph_idx_t+1, pco2air_idx_t=ph_idx_t+2,
     &   parinc_idx_t=ph_idx_t+3, fgo2_idx_t=ph_idx_t+4, fgco2_idx_t=ph_idx_t+5,
     &   ws10m_idx_t=ph_idx_t+6, xkw_idx_t=ph_idx_t+7, atmpress_idx_t=ph_idx_t+8,
     &   schmidto2_idx_t=ph_idx_t+9, o2sat_idx_t=ph_idx_t+10, schmidtco2_idx_t=ph_idx_t+11,
     &   pvo2_idx_t=ph_idx_t+12,pvco2_idx_t=ph_idx_t+13,co2star_idx_t=ph_idx_t+14,
     &   dco2star_idx_t=ph_idx_t+15, ironflux_idx_t=ph_idx_t+16,seddenitrif_idx_t=ph_idx_t+17 )

      ! Array for storing the Netcdf variable IDs of the diagnostics:
      ! The IDs of the 2d vars are first, the those of the 3d.
      integer hisT_bec2_diag(nr_bec2_diag), avgT_bec2_diag(nr_bec2_diag), slavgT_bec2_diag(nr_bec2_diag),
     &        rstT_bec2_diag(nr_bec2_diag)

      ! Arrays storing information (name, unit, fill value) about each diagnostic variable:
      character*72  vname_bec2_diag_2d(4,nr_bec2_diag_2d)
      character*72  vname_bec2_diag_3d(4,nr_bec2_diag_3d)

      common /bec2_diag2/ hisT_bec2_diag, avgT_bec2_diag, slavgT_bec2_diag, rstT_bec2_diag,
     &   vname_bec2_diag_2d, vname_bec2_diag_3d

      real bec2_diag_3d_avg(GLOBAL_2D_ARRAY,N,nr_bec2_diag_3d)
      real bec2_diag_2d_avg(GLOBAL_2D_ARRAY,nr_bec2_diag_2d)
      real bec2_diag_3d_slavg(GLOBAL_2D_ARRAY,nr_bec2_diag_3d)
      real bec2_diag_2d_slavg(GLOBAL_2D_ARRAY,nr_bec2_diag_2d)
      common /bec2_diag3/ bec2_diag_3d_avg, bec2_diag_2d_avg, bec2_diag_3d_slavg, bec2_diag_2d_slavg
#endif /* BEC2_DIAG */

!     IFRAC  sea ice fraction (non-dimensional)
!     PRESS  sea level atmospheric pressure (Pascals)
      real ifrac(GLOBAL_2D_ARRAY),
     &   press(GLOBAL_2D_ARRAY)
      common /fic_ap/ ifrac,press

      logical landmask(GLOBAL_2D_ARRAY)
      common /calcation/landmask

      logical lsource_sink,lflux_gas_o2, lflux_gas_co2,
     &  liron_flux,ldust_flux
      common /ecoflag/lsource_sink,lflux_gas_o2,lflux_gas_co2,
     &   liron_flux,ldust_flux

!
! Relative tracer indices for prognostic variables:
!
      integer po4_ind_t, no3_ind_t, sio3_ind_t, nh4_ind_t, fe_ind_t, dic_ind_t, alk_ind_t,
     &        o2_ind_t, doc_ind_t, don_ind_t, dofe_ind_t, dop_ind_t, dopr_ind_t, donr_ind_t,
     &        zooc_ind_t, spchl_ind_t, spc_ind_t, spfe_ind_t, spcaco3_ind_t, diatchl_ind_t,
     &        diatc_ind_t, diatfe_ind_t, diatsi_ind_t, diazchl_ind_t, diazc_ind_t, diazfe_ind_t
      parameter( po4_ind_t=1, no3_ind_t=2, sio3_ind_t=3, nh4_ind_t=4, fe_ind_t=5,
     &    o2_ind_t=6, dic_ind_t=7, alk_ind_t=8, doc_ind_t=9, don_ind_t=10, dofe_ind_t=11,
     &    dop_ind_t=12, dopr_ind_t=13, donr_ind_t=14, zooc_ind_t=15, spchl_ind_t=16,
     &    spc_ind_t=17, spfe_ind_t=18, spcaco3_ind_t=19, diatchl_ind_t=20, diatc_ind_t=21,
     &    diatfe_ind_t=22, diatsi_ind_t=23, diazchl_ind_t=24, diazc_ind_t=25, diazfe_ind_t=26 )

!
! Parameters related to sinking particles:
!

      real 
     &   POC_diss,       ! diss. length (m), modified by TEMP
     &   POC_mass,       ! molecular weight of POC
     &   P_CaCO3_diss,   ! diss. length (m)
     &   P_CaCO3_gamma,  ! prod frac -> hard subclass
     &   P_CaCO3_mass,   ! molecular weight of CaCO
     &   P_CaCO3_rho,    ! QA mass ratio for CaCO3
     &   P_SiO2_diss,    ! diss. length (m), modified by TEMP
     &   P_SiO2_gamma,   ! prod frac -> hard subclass
     &   P_SiO2_mass,    ! molecular weight of SiO2
     &   P_SiO2_rho,     ! QA mass ratio for SiO2
     &   dust_diss,      ! diss. length (m)
     &   dust_gamma,     ! prod frac -> hard subclass
     &   dust_mass,      ! base units are already grams
     &   dust_rho,       ! QA mass ratio for dust
     &   P_iron_gamma    ! prod frac -> hard subclass


      common /sinking_particles1/ POC_diss,POC_mass,P_CaCO3_diss,P_CaCO3_gamma,
     &   P_CaCO3_mass,P_CaCO3_rho,P_SiO2_diss,P_SiO2_gamma,P_SiO2_mass,P_SiO2_rho,
     &   dust_diss,dust_gamma,dust_mass,dust_rho,P_iron_gamma
!
! Arrays related to sinking particles:
!
!  *_hflux_in: incoming flux of hard subclass (base units/m^2/sec)
!  *_hflux_out: outgoing flux of hard subclass (base units/m^2/sec)
!  *_sflux_in: incoming flux of soft subclass (base units/m^2/sec)
!  *_sflux_out: outgoing flux of soft subclass (base units/m^2/sec)
!  *_sed_loss: loss to sediments (base units/m^2/sec)
!  *_remin: remineralization term (base units/m^3/sec)

      real, dimension(GLOBAL_2D_ARRAY) ::
     &   P_CaCO3_sflux_out, P_CaCO3_hflux_out, P_CaCO3_sed_loss,
     &   P_SiO2_sflux_out, P_SiO2_hflux_out, P_SiO2_sed_loss,
     &   dust_sflux_out, dust_hflux_out, dust_sed_loss,
     &   P_iron_sflux_out, P_iron_hflux_out, P_iron_sed_loss,
     &   POC_sflux_out, POC_hflux_out, POC_sed_loss,
     &   P_CaCO3_sflux_in, P_CaCO3_hflux_in,
     &   P_SiO2_sflux_in, P_SiO2_hflux_in, 
     &   dust_sflux_in, dust_hflux_in,
     &   P_iron_sflux_in, P_iron_hflux_in,
     &   POC_sflux_in, POC_hflux_in,
     &   POC_remin, P_iron_remin, P_SiO2_remin,
     &   DOP_remin, DOPr_remin, P_CaCO3_remin

      common /sinking_particles2/
     &   P_CaCO3_sflux_out, P_CaCO3_hflux_out, P_CaCO3_sed_loss,
     &   P_SiO2_sflux_out, P_SiO2_hflux_out, P_SiO2_sed_loss,
     &   dust_sflux_out, dust_hflux_out, dust_sed_loss,
     &   P_iron_sflux_out, P_iron_hflux_out, P_iron_sed_loss,
     &   POC_sflux_out, POC_hflux_out, POC_sed_loss,
     &   P_CaCO3_sflux_in, P_CaCO3_hflux_in,
     &   P_SiO2_sflux_in, P_SiO2_hflux_in, 
     &   dust_sflux_in, dust_hflux_in,
     &   P_iron_sflux_in, P_iron_hflux_in,
     &   POC_sflux_in, POC_hflux_in,
     &   POC_remin, P_iron_remin, P_SiO2_remin,
     &   DOP_remin, DOPr_remin, P_CaCO3_remin
