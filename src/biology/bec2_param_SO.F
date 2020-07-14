      !---------------------------------------------------------------------------
      !   default namelist settings SO (Southern Ocean)
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
      parm_Fe_scavenge_rate0 = 0.5 ! x1 default     2.8
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

! Parameters for small phytoplankton
      auto_ind = sp_ind
      sname(auto_ind)         = 'sp'
      lname(auto_ind)         = 'Small Phyto'
      Nfixer(auto_ind)        = .false.
      exp_calcifier(auto_ind) = .false.
      grazee_ind(auto_ind)    = auto_ind
      C_ind(auto_ind)         = spC_ind_t
      Chl_ind(auto_ind)       = spChl_ind_t
      Fe_ind(auto_ind)        = spFe_ind_t
      Si_ind(auto_ind)        = 0
      imp_calcifier(auto_ind) = .true. ! this is over-written in case of BEC_COCCO
      CaCO3_ind(auto_ind)     = spCaCO3_ind_t
      kFe(auto_ind)           = 0.08e-3   ! Master branch default: 0.08
      kPO4(auto_ind)          = 0.01
      kDOP(auto_ind)          = 0.26
      kNO3(auto_ind)          = 0.1
      kNH4(auto_ind)          = 0.01
      kSiO3(auto_ind)         = 0.0
      Qp(auto_ind)            = 0.00855
      gQfe_0(auto_ind)        = 20e-6
      gQfe_min(auto_ind)      = 3.0e-6
      alphaPI(auto_ind)       = 0.44*dps   ! Master branch default: 0.44
      !PCref(auto_ind)         = 3.6*dps    ! Master branch default: 3.6
      PCref(auto_ind)         = 2.7*dps    ! 75%
      !thetaN_max(auto_ind)    = 2.5         ! Master branch default: 2.5
      thetaN_max(auto_ind)    = 3.5          ! MF: sensitivity test
      loss_thres(auto_ind)   = 0.04
      loss_thres2(auto_ind)  = 0.0
      temp_thres(auto_ind)   = -10.0
      mort(auto_ind)          = 0.12*dps
      mort2(auto_ind)         = 0.001*dps
      agg_rate_max(auto_ind)  = 0.9
      agg_rate_min(auto_ind)  = 0.01
      !z_umax_0(auto_ind)      = 4.4*dps   ! Master branch default: 4.4
      z_umax_0(auto_ind)      = 3.1*dps   ! 70%
      z_grz(auto_ind)         = 1.05
      graze_zoo(auto_ind)     = 0.3
      graze_poc(auto_ind)     = 0.05
      !graze_doc(auto_ind)     = 0.15
      graze_doc(auto_ind)     = 0.06
      loss_poc(auto_ind)      = 0.0
      f_zoo_detr(auto_ind)    = 0.15

! Parameters for diatoms
      auto_ind = diat_ind
      sname(auto_ind)         = 'diat'
      lname(auto_ind)         = 'Diatom'
      Nfixer(auto_ind)        = .false.
      imp_calcifier(auto_ind) = .false.
      exp_calcifier(auto_ind) = .false.
      grazee_ind(auto_ind)    = auto_ind
      C_ind(auto_ind)         = diatC_ind_t
      Chl_ind(auto_ind)       = diatChl_ind_t
      Fe_ind(auto_ind)        = diatFe_ind_t
      Si_ind(auto_ind)        = diatSi_ind_t
      CaCO3_ind(auto_ind)     = 0
      kFe(auto_ind)           = 0.12e-3   ! Master branch default: 0.12e-3
      kPO4(auto_ind)          = 0.05
      kDOP(auto_ind)          = 0.9
      kNO3(auto_ind)          = 0.5
      kNH4(auto_ind)          = 0.05
      kSiO3(auto_ind)         = 1.0       ! Master branch default: 1.0
      Qp(auto_ind)            = 0.00855
      gQfe_0(auto_ind)        = 20e-6
      gQfe_min(auto_ind)      = 3.0e-6
      alphaPI(auto_ind)       = 0.465*dps ! Master branch default: 0.44
      !PCref(auto_ind)         = 4.6*dps  ! Master branch default: 4.6
      PCref(auto_ind)         = 3.4*dps  ! 75%
      thetaN_max(auto_ind)    = 4.0      ! Master branch default: 4.0
      loss_thres(auto_ind)   = 0.04
      loss_thres2(auto_ind)  = 0.0
      temp_thres(auto_ind)   = -10.0
      mort(auto_ind)          = 0.12*dps
      mort2(auto_ind)         = 0.001*dps
      agg_rate_max(auto_ind)  = 0.9
      agg_rate_min(auto_ind)  = 0.02
      !z_umax_0(auto_ind)      = 3.8*dps  ! Master branch default: 3.8
      z_umax_0(auto_ind)      = 3.3*dps  ! 90%
      z_grz(auto_ind)         = 1.0
      graze_zoo(auto_ind)     = 0.3
      graze_poc(auto_ind)     = 0.42
      !graze_doc(auto_ind)     = 0.15
      graze_doc(auto_ind)     = 0.06
      loss_poc(auto_ind)      = 0.0
      f_zoo_detr(auto_ind)    = 0.2

! Parameters for diazotrophs
      auto_ind = diaz_ind
      sname(auto_ind)         = 'diaz'
      lname(auto_ind)         = 'Diazotroph'
      Nfixer(auto_ind)        = .true.
      imp_calcifier(auto_ind) = .false.
      exp_calcifier(auto_ind) = .false.
      grazee_ind(auto_ind)    = auto_ind
      C_ind(auto_ind)         = diazC_ind_t
      Chl_ind(auto_ind)       = diazChl_ind_t
      Fe_ind(auto_ind)        = diazFe_ind_t
      Si_ind(auto_ind)        = 0
      CaCO3_ind(auto_ind)     = 0
      kFe(auto_ind)           = 0.08e-3   ! Master branch default:0.08e-3
      kPO4(auto_ind)          = 0.02
      kDOP(auto_ind)          = 0.09
      kNO3(auto_ind)          = 1.0
      kNH4(auto_ind)          = 0.15
      kSiO3(auto_ind)         = 0.0
      Qp(auto_ind)            = 0.002735
      gQfe_0(auto_ind)        = 60e-6
      gQfe_min(auto_ind)      = 12.0e-6
      alphaPI(auto_ind)       = 0.38*dps  ! Master branch default: 0.38
      !PCref(auto_ind)         = 0.9*dps  ! Master branch default: 0.9
      PCref(auto_ind)         = 0.7*dps  ! 75%
      thetaN_max(auto_ind)    = 2.5      ! Master branch default: 2.5
      loss_thres(auto_ind)   = 0.022
      loss_thres2(auto_ind)  = 0.001
      temp_thres(auto_ind)   = 14.0
      mort(auto_ind)          = 0.15*dps
      mort2(auto_ind)         = 0.0*dps
      agg_rate_max(auto_ind)  = 0.0
      agg_rate_min(auto_ind)  = 0.0
      !z_umax_0(auto_ind)      = 2.0*dps  ! Master branch default: 2.0
      z_umax_0(auto_ind)      = 1.4*dps  ! 70%
      z_grz(auto_ind)         = 1.2
      graze_zoo(auto_ind)     = 0.3
      graze_poc(auto_ind)     = 0.05
      !graze_doc(auto_ind)     = 0.15
      graze_doc(auto_ind)     = 0.06
      loss_poc(auto_ind)      = 0.0
      f_zoo_detr(auto_ind)    = 0.15

#ifdef BEC_COCCO
      ! over-write sp information: no implicit calcifiers in this case    
      imp_calcifier(sp_ind) = .false.
      CaCO3_ind(sp_ind)     = 0
! Parameters for coccos
      auto_ind = cocco_ind
      sname(auto_ind)         = 'cocco'
      lname(auto_ind)         = 'Coccolithophores'
      Nfixer(auto_ind)        = .false.
      imp_calcifier(auto_ind) = .false.
      exp_calcifier(auto_ind) = .true.
      grazee_ind(auto_ind)    = auto_ind
      C_ind(auto_ind)         = coccoC_ind_t
      Chl_ind(auto_ind)       = coccoChl_ind_t
      Fe_ind(auto_ind)        = coccoFe_ind_t
      Si_ind(auto_ind)        = 0
      CaCO3_ind(auto_ind)     = coccoCal_ind_t
      kFe(auto_ind)           = 0.10e-3
      kPO4(auto_ind)          = 0.03
      kDOP(auto_ind)          = 0.3
      kNO3(auto_ind)          = 0.3
      kNH4(auto_ind)          = 0.03
      kSiO3(auto_ind)         = 0.0
      Qp(auto_ind)            = 0.00855
      gQfe_0(auto_ind)        = 20e-6
      gQfe_min(auto_ind)      = 3.0e-6
      alphaPI(auto_ind)       = 0.4*dps
      PCref(auto_ind)         = 3.8*dps
      thetaN_max(auto_ind)    = 2.5
      loss_thres(auto_ind)   = 0.04
      loss_thres2(auto_ind)  = 0.0
      temp_thres(auto_ind)   = -10.0
      mort(auto_ind)          = 0.12*dps
      mort2(auto_ind)         = 0.001*dps
      agg_rate_max(auto_ind)  = 0.9
      agg_rate_min(auto_ind)  = 0.01
      z_umax_0(auto_ind)      = 4.4*dps
      z_grz(auto_ind)         = 1.05
      graze_zoo(auto_ind)     = 0.3
      graze_poc(auto_ind)     = 0.20 ! 0.2 before, set to 0.05 for NoCalc run (as for SP)
      graze_doc(auto_ind)     = 0.15
      loss_poc(auto_ind)      = 0.2  ! 0.2 before, set to zero for noCalc run
      f_zoo_detr(auto_ind)    = 0.2
#endif
#ifdef BEC_DDA
      auto_ind = dda_ind
      sname(auto_ind)         = 'dda'
      lname(auto_ind)         = 'Diatom Diazotroph Assemblages'
      Nfixer(auto_ind)        = .true.
      imp_calcifier(auto_ind) = .false.
      exp_calcifier(auto_ind) = .false.
      grazee_ind(auto_ind)    = auto_ind
      C_ind(auto_ind)         = ddaC_ind_t
      Chl_ind(auto_ind)       = ddaChl_ind_t
      Fe_ind(auto_ind)        = ddaFe_ind_t
      Si_ind(auto_ind)        = ddaSi_ind_t
      CaCO3_ind(auto_ind)     = 0
      kFe(auto_ind)           = 0.18e-3
      kPO4(auto_ind)          = 0.1
      kDOP(auto_ind)          = 0.9
      kNO3(auto_ind)          = 0.8
      kNH4(auto_ind)          = 0.05
      kSiO3(auto_ind)         = 1.0
      Qp(auto_ind)            = 0.00855
      gQfe_0(auto_ind)        = 20e-6
      gQfe_min(auto_ind)      = 3.0e-6
      alphaPI(auto_ind)       = 0.44*dps
      PCref(auto_ind)         = 2.5*dps
      thetaN_max(auto_ind)    = 4.0
      loss_thres(auto_ind)   = 0.04
      loss_thres2(auto_ind)  = 0.0
      temp_thres(auto_ind)   = -10.0
      mort(auto_ind)          = 0.12*dps
      mort2(auto_ind)         = 0.001*dps
      agg_rate_max(auto_ind)  = 0.9
      agg_rate_min(auto_ind)  = 0.02
      z_umax_0(auto_ind)      = 3.8*dps
      z_grz(auto_ind)         = 1.0
      graze_zoo(auto_ind)     = 0.3
      graze_poc(auto_ind)     = 0.42
      graze_doc(auto_ind)     = 0.15
      loss_poc(auto_ind)      = 0.0
      f_zoo_detr(auto_ind)    = 0.2
#endif
#ifdef BEC_PHAEO
! Parameters for Phaeocystis AA
      auto_ind = phaeo_ind
      sname(auto_ind)         = 'phaeo'
      lname(auto_ind)         = 'Phaeocystis Antarctica'
      Nfixer(auto_ind)        = .false.
      imp_calcifier(auto_ind) = .false.
      exp_calcifier(auto_ind) = .false.
      grazee_ind(auto_ind)    = auto_ind
      C_ind(auto_ind)         = phaeoC_ind_t
      Chl_ind(auto_ind)       = phaeoChl_ind_t
      Fe_ind(auto_ind)        = phaeoFe_ind_t
      Si_ind(auto_ind)        = 0
      CaCO3_ind(auto_ind)     = 0
      kFe(auto_ind)           = 0.18e-3
      kPO4(auto_ind)          = 0.1
      kDOP(auto_ind)          = 0.9
      kNO3(auto_ind)          = 0.8
      kNH4(auto_ind)          = 0.05
      kSiO3(auto_ind)         = 0.0
      Qp(auto_ind)            = 0.00855
      gQfe_0(auto_ind)        = 20e-6
      gQfe_min(auto_ind)      = 3.0e-6
      alphaPI(auto_ind)       = 0.5*dps
      PCref(auto_ind)         = 4.6*dps
      thetaN_max(auto_ind)    = 2.5
      loss_thres(auto_ind)   = 0.04
      loss_thres2(auto_ind)  = 0.0
      temp_thres(auto_ind)   = 8.0
      mort(auto_ind)          = 0.12*dps
      mort2(auto_ind)         = 0.001*dps
      agg_rate_max(auto_ind)  = 0.9
      agg_rate_min(auto_ind)  = 0.02
      z_umax_0(auto_ind)      = 3.6*dps
      z_grz(auto_ind)         = 1.0
      graze_zoo(auto_ind)     = 0.3
      graze_poc(auto_ind)     = 0.42
      graze_doc(auto_ind)     = 0.15
      loss_poc(auto_ind)      = 0.0
      f_zoo_detr(auto_ind)    = 0.2
#endif
#ifdef BIOPAR_NAMELIST
      !---------------------------------------------------------------------------
      !   read in namelist
      !---------------------------------------------------------------------------

      open(73,file=filename_biopar_bec_nml,status='old',iostat=status)
      ! Since this file is optional, it is okay to proceed if it does not exist or
      ! cannot be read.
      if (status .eq. 0) then
         read(73,NML=ecosys_parms_nml,iostat=status)
# ifdef MPI
         if (mynode.eq.0) then
# endif
            if (status .ne. 0) then
               print *, 'Could not read the biology parameter namelist!'
            else
               print *, 'Biology parameters were read from ',
     &              filename_biopar_bec_nml
            end if
# ifdef MPI
         end if  ! mynode.eq.0
# endif
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
# ifndef BIOPAR_NOWRITE
#  ifdef MPI
      ! Master process writes file with parameters used:
      if (mynode.eq.0) then
#  endif
         open(74,file=filename_biopar_out_bec_nml,status='unknown',
     &           iostat=status)
         if (status .eq. 0) then
            write(74,NML=ecosys_parms_nml,iostat=status)
            if (status .ne. 0) then
               print *,'Could not write the biology parameter namelist!'
            end if
            close(74)
         else
            print *, 'Could not create output file ',
     &           filename_biopar_out_bec_nml
         end if
#  ifdef MPI
      end if  ! (mynode.eq.0)
#  endif
# endif /* BIOPAR_NOWRITE */
#endif /* BIOPAR_NAMELIST */


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
