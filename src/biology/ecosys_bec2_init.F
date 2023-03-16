#include "cppdefs.h"
#ifdef BIOLOGY_BEC2
      subroutine ecosys2_init()
      implicit none
 
#include "param.h" 
#include "param_bec2.h"
#include "ecosys_bec2.h"
#include "grid.h"

      ! Variables used for namelist parameter input:
      integer status,  lvar,itrc,lenstr
! #ifdef BIOPAR_NAMELIST
      character*100 filename_biopar_bec_nml, filename_biopar_out_bec_nml

      ! Define namelists  for ecosystem parameters:

      namelist /phyto_growth/
     &    PCref, ! [1/d]
     &    alphaPI, ! [mmol m^2/W d]
     &    Q_10_phyto,
     &    Q_10_zoo,
     &    kFe, ! [nmol/m^3]
     &    kPO4, kNO3, kNH4, kSiO3, kDOP, ! [mmol/m^3]
     &    temp_thres ! [C]

      namelist /phyto_loss/
     &    z_umax_0,
     &    z_grz,
     &    mort, mort2,
     &    parm_z_mort_0, parm_z_mort2_0,
     &    agg_rate_min, agg_rate_max,
     &    loss_thres, loss_thres2,
     &    loss_thres_zoo

      namelist /loss_routing/
     &    graze_zoo, graze_poc, graze_doc,
     &    loss_poc,
     &    f_zoo_detr,
     &    f_graze_CaCO3_remin, f_graze_si_remin

      namelist /carbon_routing/
     &    DOC_reminR,
     &    DON_reminR,
     &    DOP_reminR,
     &    DOFe_reminR,
     &    DONr_reminR, DOPr_reminR

      namelist /element_ratios/    ! Never changed by ETH tuning
     &    Qp,                              ! P/C per PFT
     &    gQfe_0, gQfe_min, Qfe_zoo, ! Fe/C per PFT
     &    cks, 
     &    thetaN_max, 
     &    gQsi_0, gQsi_max, gQsi_min,
     &    cksi, Q, Qp_zoo_pom

      namelist /nutrient_change/
     &    CaCO3_temp_thres1, CaCO3_temp_thres2,
     &    CaCO3_sp_thres, parm_f_prod_sp_CaCO3,
     &    QCaCO3_max,
     &    parm_POC_diss, parm_SiO2_diss, parm_CaCO3_diss,
     &    parm_scalelen_z, parm_scalelen_vals, 
     &    parm_labile_ratio,
     &    parm_POMbury, parm_BSIbury,
     &    parm_kappa_nitrif, parm_nitrif_par_lim,
     &    parm_Fe_bioavail, parm_Fe_scavenge_rate0,
     &    parm_o2_min, parm_o2_min_delta,
     &    r_Nexcrete

      filename_biopar_bec_nml = 'bec_parameter_in.nml'
      filename_biopar_out_bec_nml = 'bec_parameter_used.nml'
!#endif

      !---------------------------------------------------------------------------
      !   default namelist settings
      !---------------------------------------------------------------------------

      parm_Fe_bioavail       = 0.02
      parm_o2_min            = 1.0  ! mmol/m^3 = nmol/cm^3 (MF 11.11.2016: Taking down DENITRIF following discussion with SY (Babbin et al. 2014))
      parm_o2_min_delta      = 2.0  ! mmol/m^3 = nmol/cm^3
      parm_kappa_nitrif      = 0.06 * dps ! (= 1/( days))
      parm_nitrif_par_lim    = 1.0
      parm_z_mort_0          = 0.1 * dps
      parm_z_mort2_0         = 0.4 * dps
      !parm_labile_ratio      = 0.85
      parm_labile_ratio      = 0.94
      parm_POMbury           = 1.4 ! x1 default
      parm_BSIbury           = 0.45 ! x1 default
      !parm_Fe_scavenge_rate0 = 1.3 ! x1 default     2.8
      parm_Fe_scavenge_rate0 = 0.5 ! x1 default     2.8
      !parm_Fe_scavenge_rate0 = 3.9 ! 300%
      parm_f_prod_sp_CaCO3   = 0.055 ! x1 default   0.04
      parm_POC_diss          = 82.0   ! 88 in cBEC, 82
      parm_SiO2_diss         = 360.0  ! 250 in cBEC, 360
      parm_CaCO3_diss        = 400.0  ! 150 in cBEC, 400
# ifdef USE_EXPLICIT_VSINK
      parm_scalelen_z    = (/ 100.0, 250.0, 500.0, 750.0 /) ! DL: converted to m
      parm_scalelen_vals = (/     1.0,     2.9,     5.6,      5.7 /) ! x1 default
# else
      parm_scalelen_z    = (/ 100.0, 250.0, 500.0, 750.0 /) ! DL: converted to m
      parm_scalelen_vals = (/     1.0,     2.9,     5.6,      5.7 /) ! x1 default
# endif

! ETH default parameter for sp, diatoms, diazotrophs since 2023
! parameter PFT indices: (1, 2, 3) = (sp, diat, diaz)  
      sname(1:3)         = (/ 'sp', 'diat', 'diaz' /)
      lname(1:3)         = (/ 'Small Phyto', 'Diatom', 'Diazotroph' /)
      Nfixer(1:3)     = (/ .false., .false., .true. /)
      exp_calcifier(1:3) = (/ .false., .false., .false. /)
      exp_calcifier(1:3) = (/ .false., .false., .false. /)
      grazee_ind(1:3)    = (/ sp_ind, diat_ind, diaz_ind /)
      C_ind(1:3)      = (/ spC_ind_t, diatC_ind_t, diazC_ind_t /)
      Chl_ind(1:3)    = (/spChl_ind_t, diatChl_ind_t, diazChl_ind_t/)
      Fe_ind(1:3)     = (/spFe_ind_t, diatFe_ind_t, diazFe_ind_t/)
      Si_ind(1:3)        = (/ 0, diatSi_ind_t, 0 /)
      imp_calcifier(1:3) = (/ .true. , .false., .false. /)! sp .true. is over-written in case of BEC_COCCO
      CaCO3_ind(1:3)     = (/spCaCO3_ind_t, 0, 0 /)
      r_Nexcrete(1:3)  = (/ 0.0, 0.0, 0.3 /) ! Diaz: 30% overfixation 

      ! Default S12_1_k scenario parameter -> https://upwiki.ethz.ch/upwiki/doku.php?id=romsdoc:bec_parameters_setups
      PCref(1:3)   = (/ 0.5, 1.1, 0.2 /)  ! d-1
      Q_10_phyto(1:3) = (/ 1.6, 1.9, 1.8 /)
      kFe (1:3) = (/ 1.0d-4, 1.6d-4, 6.0d-4 /)  ! mmol m-3
      kPO4(1:3) = (/ 0.08, 0.13, 0.02 /)  ! mmol m-3
      kNO3(1:3) = (/ 1.2, 2.1, 0.3 /)  ! mmol m-3
      kNH4(1:3) = (/ 0.12, 0.21, 0.03 /)  ! mmol m-3
      kSiO3(1:3) = (/ 0.0, 2.0, 0.0 /)  ! mmol m-3
      kDOP(1:3) = (/ 0.9, 0.9, 0.02 /)  ! mmol m-3
      z_umax_0(1:3) = (/ 0.6, 0.9, 0.4 /)  ! d-1
      z_grz(1:3) = (/ 1.05, 1.0, 1.2 /)  ! mmol m-3
      mort(1:3) = (/ 0.06, 0.06, 0.15 /)  ! d-1

#ifdef BIOPAR_R
      MPI_master_only print*, 'BEC parameters initialized with ',
     & 'S12_1_r scenario '
      PCref(1:3)   = (/ 1.0, 2.4, 0.6  /)  ! d-1
      Q_10_phyto(1:3) = (/ 1.6, 1.9, 1.8 /)
      kFe(1:3)  = (/ 1.0d-4, 1.6d-4, 6.0d-4 /)  ! mmol m-3
      kPO4(1:3) = (/ 0.05, 0.08, 0.006 /)  ! mmol m-3
      kNO3(1:3) = (/ 0.8, 1.3, 0.1 /)  ! mmol m-3
      kNH4(1:3) = (/ 0.08, 0.13, 0.01 /)  ! mmol m-3
      kSiO3(1:3) = (/ 0.0, 1.2, 0.0 /)  ! mmol m-3
      kDOP(1:3) = (/ 0.9, 0.9, 0.006 /)  ! mmol m-3
      z_umax_0(1:3) = (/ 1.875, 2.875, 1.625 /)  ! d-1
      z_grz(1:3) = (/ 1.05, 1.0, 1.2 /)  ! mmol m-3
      mort(1:3) = (/ 0.12, 0.12, 0.15 /)  ! d-1
#endif /* BIOPAR_R */


      alphaPI(1:3)   = (/ 0.44, 0.44, 0.38 /)  ! mmol C m2 (mgChl)-1 W-1 d-1
      Q_10_zoo = 1.5
      temp_thres(1:3) = (/ -10, -10, 14 /)  ! C
      mort2(1:3) = (/ 0.001, 0.001, 0.0 /)  ! d-1
      agg_rate_max(1:3) = (/ 0.9, 0.9, 0.0 /)  ! d-1
      agg_rate_min(1:3) = (/ 0.01, 0.02, 0.0 /)  ! d-1
      loss_thres(1:3) = (/ 0.04, 0.04, 0.022 /) ! mmol m-3
      loss_thres2(1:3) = (/ 0.0, 0.0, 0.001 /) ! mmol m-3
      loss_thres_zoo = 0.005    ! mmol m-3
      thres_z1 = 100            ! m
      thres_z2 = 150            ! m
      parm_z_mort_0 = 0.1  ! d-1
      parm_z_mort2_0 = 0.4  ! d-1
      graze_zoo(1:3) = (/ 0.3, 0.3, 0.3 /)
      graze_poc(1:3) = (/ 0.05, 0.42, 0.05 /)
      graze_doc(1:3) = (/ 0.15, 0.15, 0.15 /)
      loss_poc(1:3) = (/ 0.0, 0.0, 0.0 /)
      f_zoo_detr(1:3) = (/ 0.15, 0.2, 0.15 /)
      f_graze_CaCO3_remin = 0.75
      f_graze_si _remin = 0.35
      DONrefract = 0.08
      DOPrefract = 0.03
      DOC_reminR = (c1/250.0) ! d-1
      DON_reminR = (c1/160.0) ! d-1
      DOP_reminR = (c1/160.0) ! d-1
      DOFe_reminR  = (c1/160.0) ! d-1
      DONr_reminR = (c1/(365*2.5)) ! d-1
      DOPr_reminR = (c1/(365*2.5)) ! d-1
      Qp(1:3) = (/ 0.00855, 0.00855, 0.002735 /)
      gQfe_0(1:3) = (/ 2d-5, 2d-5, 6d-5 /)
      gQfe_min(1:3) = (/ 3d-6, 3d-6, 1.2d-5 /)
      Qfe_zoo = 3d-6
      cks = 9.0
      thetaN_max(1:3) = (/ 2.5, 4.0, 2.5 /)
      gQsi_0 = 0.137
      gQsi_max = 0.685
      gQsi_min = 0.0457
      cksi = 5.0
      Q = 0.137
      Qp_zoo_pom = 0.00855
      CaCO3_temp_thres1 = 6.0 ! DegC
      CaCO3_temp_thres2 = -2.0 ! DegC
      CaCO3_sp_thres = 4.0  ! mmol m-3
      parm_f_prod_sp_CaCO3 = 0.055
      QCaCO3_max = 0.4
      parm_POC_diss = 82.0   ! m
      parm_SiO2_diss = 360.0    ! m
      parm_CaCO3_diss = 400.0   ! m
      parm_scalelen_z = (/ 130, 250, 500, 750 /) ! m
      parm_scalelen_vals = (/ 1.0, 2.9, 5.6, 5.7 /)
      parm_labile_ratio = 0.85
      parm_POMbury = 1.4
      parm_BSIbury = 0.45
      parm_kappa_nitrif = 0.06 ! d-1
      parm_nitrif_par_lim = 4.0
      parm_Fe_bioavail = 0.02
      parm_o2_min = 4.0         ! mmol/m^3
      parm_o2_min_delta = 2.0   ! mmol/m^3
      parm_fe_scavenge_rate0 = 1.3


! indices >3 for  cocco, dda, phaeo, ucyn turn on via CPP switches
#ifdef BEC_COCCO
      ! over-write sp information: no implicit calcifiers in this case    
      imp_calcifier(sp_ind) = .false.
      CaCO3_ind(sp_ind)     = 0
! Parameters for coccos
      sname(cocco_ind)         = 'cocco'
      lname(cocco_ind)         = 'Coccolithophores'
      Nfixer(cocco_ind)        = .false.
      imp_calcifier(cocco_ind) = .false.
      exp_calcifier(cocco_ind) = .true.
      grazee_ind(cocco_ind)    = cocco_ind
      C_ind(cocco_ind)         = coccoC_ind_t
      Chl_ind(cocco_ind)       = coccoChl_ind_t
      Fe_ind(cocco_ind)        = coccoFe_ind_t
      Si_ind(cocco_ind)        = 0
      CaCO3_ind(cocco_ind)     = coccoCal_ind_t
      kFe(cocco_ind)           = 0.10e-3
      kPO4(cocco_ind)          = 0.03
      kDOP(cocco_ind)          = 0.3
      kNO3(cocco_ind)          = 0.3
      kNH4(cocco_ind)          = 0.03
      kSiO3(cocco_ind)         = 0.0
      Qp(cocco_ind)            = 0.00855
      gQfe_0(cocco_ind)        = 20e-6
      gQfe_min(cocco_ind)      = 3.0e-6
      alphaPI(cocco_ind)       = 0.44
      PCref(cocco_ind)         = 0.909
      Q_10_phyto(cocco_ind)    = 1.9  ! old ratio to diat 1.78
      thetaN_max(cocco_ind)    = 2.5
      loss_thres(cocco_ind)   = 0.04
      loss_thres2(cocco_ind)  = 0.0
      temp_thres(cocco_ind)   = -10.0
      mort(cocco_ind)          = 0.06
      mort2(cocco_ind)         = 0.001
      agg_rate_max(cocco_ind)  = 0.9
      agg_rate_min(cocco_ind)  = 0.01
      z_umax_0(cocco_ind)      = 1.042
      z_grz(cocco_ind)         = 1.05
      graze_zoo(cocco_ind)     = 0.3
      graze_poc(cocco_ind)     = 0.20
      graze_doc(cocco_ind)     = 0.15
      loss_poc(cocco_ind)      = 0.2
      f_zoo_detr(cocco_ind)    = 0.2

#ifdef BIOPAR_R
      MPI_master_only print*, 'BEC parameters initialized with ',
     & 'S12_1_r scenario '
      PCref(cocco_ind)   = 1.983
      Q_10_phyto(cocco_ind) = 1.9
      z_umax_0(cocco_ind) = 3.329
      z_grz(cocco_ind) = 1.05
      mort(cocco_ind) = 0.12
#endif /* BIOPAR_R */

#endif


#ifdef BEC_DDA
      sname(dda_ind)         = 'dda'
      lname(dda_ind)         = 'Diatom Diazotroph Assemblages'
      Nfixer(dda_ind)        = .true.
      imp_calcifier(dda_ind) = .false.
      exp_calcifier(dda_ind) = .false.
      grazee_ind(dda_ind)    = dda_ind
      C_ind(dda_ind)         = ddaC_ind_t
      Chl_ind(dda_ind)       = ddaChl_ind_t
      Fe_ind(dda_ind)        = ddaFe_ind_t
      Si_ind(dda_ind)        = ddaSi_ind_t
      CaCO3_ind(dda_ind)     = 0
      r_Nexcrete(dda_ind)  = 0.
      kFe(dda_ind)           = 0.18e-3
      kPO4(dda_ind)          = 0.1
      kDOP(dda_ind)          = 0.9
      kNO3(dda_ind)          = 0.8
      kNH4(dda_ind)          = 0.05
      kSiO3(dda_ind)         = 1.0
      Qp(dda_ind)            = 0.00855
      gQfe_0(dda_ind)        = 20e-6
      gQfe_min(dda_ind)      = 3.0e-6
      alphaPI(dda_ind)       = 0.44
      PCref(dda_ind)         = 2.5
      Q_10_phyto(dda_ind)    = 1.9   ! same as diat 
      thetaN_max(dda_ind)    = 4.0
      loss_thres(dda_ind)   = 0.04
      loss_thres2(dda_ind)  = 0.0
      temp_thres(dda_ind)   = -10.0
      mort(dda_ind)          = 0.12
      mort2(dda_ind)         = 0.001
      agg_rate_max(dda_ind)  = 0.9
      agg_rate_min(dda_ind)  = 0.02
      z_umax_0(dda_ind)      = 3.8
      z_grz(dda_ind)         = 1.0
      graze_zoo(dda_ind)     = 0.3
      graze_poc(dda_ind)     = 0.42
      graze_doc(dda_ind)     = 0.15
      loss_poc(dda_ind)      = 0.0
      f_zoo_detr(dda_ind)    = 0.2
#endif

# ifdef BEC_UCYN
      sname(ucyn_ind) = 'ucyn'
      lname(ucyn_ind) = 'UCYN'
      Nfixer(ucyn_ind) = .true.
      imp_calcifier(ucyn_ind) = .false.
      exp_calcifier(ucyn_ind) = .false.
      grazee_ind(ucyn_ind)    = ucyn_ind
      C_ind(ucyn_ind)         = ucynC_ind_t
      Chl_ind(ucyn_ind)       = ucynChl_ind_t
      Fe_ind(ucyn_ind)        = ucynFe_ind_t
      CaCO3_ind(ucyn_ind)     = 0
      r_Nexcrete(dda_ind)  = 0.
      kFe(ucyn_ind)           = 6.0e-4
      kPO4(ucyn_ind)          = 0.08
      kDOP(ucyn_ind)          = 0.088
      kNO3(ucyn_ind)          = 1.2
      kNH4(ucyn_ind)          = 0.12
      kSiO3(ucyn_ind)         = 0.0
      Qp(ucyn_ind)            = 0.00855
      gQfe_0(ucyn_ind)        = 60.0e-6
      gQfe_min(ucyn_ind)      = 12.0e-6
      alphaPI(ucyn_ind)       = 0.44*dps
      PCref(ucyn_ind)         = 0.35*dps
      Q_10_phyto(ucyn_ind)   = 1.85  ! same as phaeo 
      thetaN_max(ucyn_ind)    = 2.5
      loss_thres(ucyn_ind)   = 0.04
      loss_thres2(ucyn_ind)  = 0.0
      temp_thres(ucyn_ind)   = 14.0
      mort(ucyn_ind)          = 0.06*dps
      mort2(ucyn_ind)         = 0.001*dps
      agg_rate_max(ucyn_ind)  = 0.9
      agg_rate_min(ucyn_ind)  = 0.01
      z_umax_0(ucyn_ind)      = 0.6*dps
      z_grz(ucyn_ind)         = 1.1
      graze_zoo(ucyn_ind)     = 0.3
      graze_poc(ucyn_ind)     = 0.05
      graze_doc(ucyn_ind)     = 0.15
      loss_poc(ucyn_ind)      = 0.0
      f_zoo_detr(ucyn_ind)    = 0.15
#endif

#ifdef BEC_PHAEO
! Parameters for Phaeocystis AA
      sname(phaeo_ind)         = 'phaeo'
      lname(phaeo_ind)         = 'Phaeocystis Antarctica'
      Nfixer(phaeo_ind)        = .false.
      imp_calcifier(phaeo_ind) = .false.
      exp_calcifier(phaeo_ind) = .false.
      grazee_ind(phaeo_ind)    = phaeo_ind
      C_ind(phaeo_ind)         = phaeoC_ind_t
      Chl_ind(phaeo_ind)       = phaeoChl_ind_t
      Fe_ind(phaeo_ind)        = phaeoFe_ind_t
      Si_ind(phaeo_ind)        = 0
      CaCO3_ind(phaeo_ind)     = 0
      r_Nexcrete(dda_ind)      = 0.
      kFe(phaeo_ind)           = 0.18e-3
      kPO4(phaeo_ind)          = 0.1
      kDOP(phaeo_ind)          = 0.9
      kNO3(phaeo_ind)          = 0.8
      kNH4(phaeo_ind)          = 0.05
      kSiO3(phaeo_ind)         = 0.0
      Qp(phaeo_ind)            = 0.00855
      gQfe_0(phaeo_ind)        = 20e-6
      gQfe_min(phaeo_ind)      = 3.0e-6
      alphaPI(phaeo_ind)       = 0.5
      PCref(phaeo_ind)         = 4.6
      Q_10_phyto(phaeo_ind)    = 1.85  ! old ratio to diat: 1.84
      thetaN_max(phaeo_ind)    = 2.5
      loss_thres(phaeo_ind)   = 0.04
      loss_thres2(phaeo_ind)  = 0.0
      temp_thres(phaeo_ind)   = 8.0
      mort(phaeo_ind)          = 0.12
      mort2(phaeo_ind)         = 0.001
      agg_rate_max(phaeo_ind)  = 0.9
      agg_rate_min(phaeo_ind)  = 0.02
      z_umax_0(phaeo_ind)      = 3.6
      z_grz(phaeo_ind)         = 1.0
      graze_zoo(phaeo_ind)     = 0.3
      graze_poc(phaeo_ind)     = 0.42
      graze_doc(phaeo_ind)     = 0.15
      loss_poc(phaeo_ind)      = 0.0
      f_zoo_detr(phaeo_ind)    = 0.2
#endif
!#ifdef BIOPAR_NAMELIST
      !---------------------------------------------------------------------------
      !   read in parameter namelists 
      !---------------------------------------------------------------------------

      open(73,file=filename_biopar_bec_nml, status='old',iostat=status
     &      , access='sequential')
      ! Since this file is optional, it is okay to proceed if it does not exist or
      ! cannot be read.
      if (status .eq. 0) then
         read(73,NML=phyto_growth,iostat=status)
         if (status /= 0) 
     &       print*,'Info: "phyto_growth" namelist not found'
     &                    , '      --> using defaults.'
         rewind(73)

         read(73,NML=phyto_loss,iostat=status)
         if (status /= 0) print*,'Error reading phyto_loss namelist'
     &                    , ' --> using defaults'
         rewind(73)

         read(73,NML=loss_routing,iostat=status)
         if (status /= 0) 
     &       print*,'Info: "loss_routing" namelist not found'
     &                    , '      --> using defaults.'
         rewind(73)

         read(73,NML=carbon_routing,iostat=status)
         if (status /= 0) 
     &       print*,'Info: "carbon_routing" namelist not found'
     &                    , '      --> using defaults.'
         rewind(73)

         read(73,NML=element_ratios,iostat=status)
         if (status /= 0) 
     &       print*,'Info: "element_ratios" namelist not found'
     &                    , '      --> using defaults.'
         rewind(73)
         
         read(73,NML=nutrient_change,iostat=status)
         if (status /= 0) 
     &       print*,'Info: "nutrient_change" namelist not found'
     &                    , '      --> using defaults.'
#ifdef MPI
         if (mynode.eq.0) then
#endif
            if (status .ne. 0) then
               print *, 'Could not read the biology parameter namelist!'
            else
               print *, 'Biology parameters were read from ',
     &              filename_biopar_bec_nml
            end if
#ifdef MPI
         end if  ! mynode.eq.0
#endif
         close(73)
      else  ! status .eq. 0
# ifdef MPI
         if (mynode.eq.0) then
# endif
            print *, 'Biology parameter namelist file was not found'
            print *, 'Default values will be used.'
# ifdef MPI
         end if
# endif
      end if  ! status .eq. 0
#ifndef BIOPAR_NOWRITE
# ifdef MPI
      ! Master process writes file with parameters used:
      if (mynode.eq.0) then
# endif
         open(74,file=filename_biopar_out_bec_nml, status='unknown',
     &           iostat=status, delim='quote')
         if (status .eq. 0) then
            write(74,NML=phyto_growth)
            write(74,NML=phyto_loss)
            write(74,NML=loss_routing)
            write(74,NML=carbon_routing)
            write(74,NML=element_ratios)
            write(74,NML=nutrient_change)
         close(74)
         else
            print *, 'Could not create output file ',
     &           filename_biopar_out_bec_nml
         end if
# ifdef MPI
      end if  ! (mynode.eq.0)
# endif
#endif /* BIOPAR_NOWRITE */
!#endif /* BIOPAR_NAMELIST */

# ifdef MPI
      ! Master process writes file with parameters used:
      if (mynode.eq.0) then
# endif
         write(*,*)'! phyto_growth:'
         write(*,NML=phyto_growth)
         write(*,*)'! phyto_loss:'
         write(*,NML=phyto_loss)
         write(*,*)'! loss_routing:'
         write(*,NML=loss_routing)
         write(*,*)'! carbon_routing:'
         write(*,NML=carbon_routing)
         write(*,*)'! carbon_routing:'
         write(*,NML=element_ratios)
         write(*,*)'! nutrient_chang:'
         write(*,NML=nutrient_change)
# ifdef MPI
      end if  ! (mynode.eq.0)
# endif
! -----------------
! Rescale parameters from human readable to ROMS units

      parm_kappa_nitrif = parm_kappa_nitrif*dps
      parm_z_mort_0 = parm_z_mort_0*dps
      parm_z_mort2_0 = parm_z_mort2_0*dps
      PCref = PCref*dps
      alphaPI = alphaPI*dps
      z_umax_0 = z_umax_0*dps
      mort = mort*dps
      mort2 = mort2*dps
      DOC_reminR = DOC_reminR*dps
      DON_reminR = DON_reminR*dps
      DOP_reminR = DOP_reminR*dps
      DOFe_reminR = DOFe_reminR*dps
      DONr_reminR = DONr_reminR*dps
      DOPr_reminR = DOPr_reminR*dps



#ifdef BEC2_DIAG
      !---------------------------------------------------------------------------
      !   Initialize diagnostic variables:
      !---------------------------------------------------------------------------
# ifdef BEC2_DIAG_USER
      ! Allocate arrays for diagnostics:
      allocate(bec2_diag_2d(GLOBAL_2D_ARRAY,nr_bec2_diag_2d_user))
      allocate(bec2_diag_3d(GLOBAL_2D_ARRAY,N,nr_bec2_diag_3d_user))
#  ifdef AVERAGES
      allocate(bec2_diag_2d_avg(GLOBAL_2D_ARRAY,nr_bec2_diag_2d_user))
      allocate(bec2_diag_3d_avg(GLOBAL_2D_ARRAY,N,nr_bec2_diag_3d_user))
#   ifdef SLICE_AVG
      allocate(bec2_diag_2d_slavg(GLOBAL_2D_ARRAY,nr_bec2_diag_2d_user))
      allocate(bec2_diag_3d_slavg(GLOBAL_2D_ARRAY,nr_bec2_diag_3d_user))
#   endif
#  endif /* AVERAGES */
      MPI_master_only write (*,'(a,i3,a)') 'bec2_diag_2d allocated for ',
     &    nr_bec2_diag_2d_user, ' variables'
      MPI_master_only write (*,'(a,i3,a)') 'bec2_diag_3d allocated for ',
     &    nr_bec2_diag_3d_user, ' variables'
# endif /* BEC2_DIAG_USER */
      bec2_diag_2d = c0
      bec2_diag_3d = c0
#else /* BEC2_DIAG */
      ph_hist = c0
#endif /* BEC2_DIAG */

      !---------------------------------------------------------------------------
      !   Initialize ice fraction and atm. pressure field:
      !---------------------------------------------------------------------------
      ifrac = 0.0
      press = 1.

      lflux_gas_o2  = .TRUE.
      lflux_gas_co2 = .TRUE.
      liron_flux = .TRUE.
#ifdef RIVER_LOAD_BIO
      lriver_load_n = .TRUE.
      lriver_load_p = .TRUE.
      !lriver_load_alk = .TRUE.
      !lriver_load_dic = .TRUE.
      !lriver_load_si = .TRUE.
#endif
      lsource_sink  = .TRUE.  

      where(rmask==1)
         landmask=.TRUE.
      elsewhere 
         landmask=.FALSE.
      endwhere

#ifdef CCHEM_MOCSY
       optcon  = 'mol/m3'   ! input concentrations of DIC, alk, P, Si are in MOL/M3
       optt    = 'Tpot'     ! 'temp' is POTENTIAL temp [C]
       optp    = 'm'        ! input variable z_r is in METERS
       optb    = 'u74'      ! Uppstrom (1974) formulation for total boron
       optk1k2 = 'm95'      ! Millero (1995) formulations for k1 and k2
       optkf   = 'pf'       ! Perez & Fraga (1987) formulation for Kf
       optgas  = 'Pinsitu'  ! compute in situ values for fCO2 and pCO2 (i.e. accounting for pressure)
#endif

      end subroutine ecosys2_init


#else /* BIOLOGY_BEC2 */
      subroutine ecosys2_init()
      return 
      end
#endif /* BIOLOGY_BEC2 */
