#include "cppdefs.h"
#ifdef BIOLOGY_BEC2
      subroutine ecosys2_init()
      implicit none
 
#include "param.h" 
#include "param_bec2.h"
#include "ecosys_bec2.h"
#include "grid.h"

      integer auto_ind
      ! Variables used for namelist parameter input:
      integer status,  lvar,itrc,lenstr
#ifdef BIOPAR_NAMELIST
      character*100 filename_biopar_bec_nml, filename_biopar_out_bec_nml

      ! Define namelist for ecosystem parameters:
      namelist /ecosys_parms_nml/ 
     &    parm_Fe_bioavail,
     &    parm_o2_min,
     &    parm_o2_min_delta,
     &    parm_kappa_nitrif,
     &    parm_nitrif_par_lim,
     &    parm_z_mort_0,
     &    parm_z_mort2_0,
     &    parm_labile_ratio,
     &    parm_POMbury,
     &    parm_BSIbury,
     &    parm_Fe_scavenge_rate0,
     &    parm_f_prod_sp_CaCO3,
     &    parm_POC_diss,
     &    parm_SiO2_diss,
     &    parm_CaCO3_diss,
     &    parm_scalelen_z,
     &    parm_scalelen_vals,
     &    kFe, kPO4, kDOP, kNO3, kNH4, kSiO3, Qp, gQfe_0, gQfe_min, 
     &    alphaPI, PCref,
     &    thetaN_max, loss_thres, loss_thres2, temp_thres, mort, mort2, 
     &    agg_rate_max,
     &    agg_rate_min, z_umax_0, z_grz, graze_zoo, graze_poc, 
     &    graze_doc, loss_poc, f_zoo_detr


      filename_biopar_bec_nml = 'biopar_bec.nml'
      filename_biopar_out_bec_nml = 'biopar_out_bec.nml'
#endif

      !---------------------------------------------------------------------------
      !   default namelist settings
      !---------------------------------------------------------------------------

      parm_Fe_bioavail       = 0.02
      !parm_o2_min            = 4.0  ! mmol/m^3 = nmol/cm^3
      parm_o2_min            = 1.0  ! mmol/m^3 = nmol/cm^3 (MF 11.11.2016: Taking down DENITRIF following discussion with SY (Babbin et al. 2014))
      parm_o2_min_delta      = 2.0  ! mmol/m^3 = nmol/cm^3
      parm_kappa_nitrif      = 0.06 * dps ! (= 1/( days))
      parm_nitrif_par_lim    = 1.0
      parm_z_mort_0          = 0.1 * dps
      parm_z_mort2_0         = 0.4 * dps
      parm_labile_ratio      = 0.85
      parm_POMbury           = 1.4 ! x1 default
# ifdef BEC_COCCO
      parm_BSIbury           = 0.45 ! x1 default
      parm_Fe_scavenge_rate0 = 1.3 ! x1 default     2.8
      parm_f_prod_sp_CaCO3   = 0.055 ! x1 default   0.04
      parm_POC_diss          = 82.0   ! 88 in cBEC, 82
      parm_SiO2_diss         = 360.0  ! 250 in cBEC, 360
      parm_CaCO3_diss        = 400.0  ! 150 in cBEC, 400
      parm_scalelen_z    = (/ 100.0, 250.0, 500.0, 750.0 /) ! DL: converted to m    ! 130, 290, 670, 1700 in cBEC; 100 250  500 750  
      parm_scalelen_vals = (/     1.0,     2.9,    5.6,      5.7 /) ! x1 default     ! 1, 2.9, 4.8, 8 in cBEC; 1  2.9  5.6 5.7      
      ! The following arrays give parameters in this order: sp, diat, diaz, coccos
      kFe = (/ 0.04e-3, 0.06e-3, 0.04e-3, 0.045e-3 /) ! mmol Fe/m3 
      kPO4 = (/ 0.01, 0.05, 0.02, 0.02 /)         ! mmol P/m3
      kDOP = (/ 0.26, 0.9, 0.09, 0.2 /)          ! 
      kNO3 = (/ 0.1, 0.5, 1.0, 0.2 /)            ! mmol N/m3   ! 0.11 0.45  8
      kNH4 = (/ 0.01, 0.05, 0.15, 0.02 /)         ! mmol N/m3  ! 0.01 0.04  0.8
      kSiO3 = (/ 0.0, 1.0, 0.0, 0.0 /)           ! mmol SiO3/m3
      Qp = (/ 0.00855, 0.00855, 0.002735, 0.00855 /)
      gQfe_0 = (/ 20.0e-6, 20.0e-6, 60.0e-6, 20.0e-6 /)
      gQfe_min = (/ 3.0e-6, 3.0e-6, 12.0e-6, 3.0e-6 /)
      alphaPI = (/ 0.46*dps, 0.46*dps, 0.4*dps, 0.42*dps /)  ! 0.6, 0.465, 0.4, 0.6 cBEC,  0.54, 0.42, 0.5 in CESM  Chl. specific initial slope of P_I curve (GD98) (mmol C m^2/(mg Chl W sec))
      PCref = (/ 3.8*dps, 4.8*dps, 0.9*dps, 4*dps /) ! 3, 5, 0.7, 4.6 in cBEC;  5.3, 5.3, 0.9 in CESM
      thetaN_max = (/ 2.5, 4.0, 2.5, 2.5 /)
      loss_thres = (/ 0.04, 0.04, 0.022, 0.04 /)
      loss_thres2 = (/ 0.0, 0.0, 0.001, 0.0 /)
      temp_thres = (/ -10.0, -10.0, 14.0, -10.0 /)
      mort = (/ 0.12*dps, 0.12*dps, 0.15*dps, 0.12*dps /)  ! non-grazing mortality (1/sec)
      mort2 = (/ 0.001*dps, 0.001*dps, 0.0, 0.001*dps /)
      agg_rate_max = (/ 0.9, 0.9, 0.0, 0.9 /)
      agg_rate_min = (/ 0.01, 0.02, 0.0, 0.01 /)
      z_umax_0 = (/ 3.15*dps, 2.95*dps, 1.6*dps, 3.15*dps /) ! 3.07,2.86, 1.6 CESM  ; 3.3 3.08 0.6 3.28 cBEC  incr SP,dIAT,COCCO by 0.2
      z_grz = (/ 1.05, 1.0, 1.2, 1.05 /)      ! grazing coefficient (mmol C/m^3)
      graze_zoo = (/ 0.3, 0.3, 0.3, 0.3 /)
      graze_poc = (/ 0.05, 0.42, 0.05, 0.42 /) 
      graze_doc = (/ 0.15, 0.15, 0.15, 0.15 /)
      loss_poc = (/ 0.0, 0.0, 0.0, 1.0 /)
      f_zoo_detr = (/ 0.15, 0.2, 0.15, 0.2 /)

# else /* case not BEC_COCCO */
      parm_BSIbury           = 0.65 ! x1 default
      parm_Fe_scavenge_rate0 = 3.0  ! previous default
      !parm_Fe_scavenge_rate0 = 4.0  ! MF: sensitivity test
      parm_f_prod_sp_CaCO3   = 0.055 ! x1 default
      parm_POC_diss          = 88.0
      parm_SiO2_diss         = 250.0
      parm_CaCO3_diss        = 150.0

# ifdef USE_EXPLICIT_VSINK
      parm_scalelen_z    = (/ 130.0, 290.0, 670.0, 1700.0 /) 
      parm_scalelen_vals = (/     1.0,     1.8,     3.8,      8.0 /) ! MF: try new parametrization for explicit sinking
# else
      parm_scalelen_z    = (/ 130.0, 290.0, 670.0, 1700.0 /) ! DL: converted to m
      parm_scalelen_vals = (/     1.0,     3.0,     5.0,      9.0 /) ! x1 default
# endif

      ! The following arrays give parameters in this order: sp, diat, diaz
      kFe = (/ 0.10e-3, 0.15e-3, 0.10e-3 /) ! mmol Fe/m3 
      kPO4 = (/ 0.025, 0.125, 0.05 /)         ! mmol P/m3
      kDOP = (/ 0.65, 2.25, 0.225 /)          ! 
      kNO3 = (/ 0.25, 1.25, 2.5 /)            ! mmol N/m3   ! 0.11 0.45  8
      kNH4 = (/ 0.025, 0.125, 0.38 /)         ! mmol N/m3  ! 0.01 0.04  0.8
      kSiO3 = (/ 0.0, 2.5, 0.0 /)           ! mmol SiO3/m3
      Qp = (/ 0.00855, 0.00855, 0.002735 /)
      gQfe_0 = (/ 20.0e-6, 20.0e-6, 60.0e-6 /)
      gQfe_min = (/ 3.0e-6, 3.0e-6, 12.0e-6 /)
      alphaPI = (/ 0.6*dps, 0.465*dps, 0.4*dps /)  ! previous defaults, Chl. specific initial slope of P_I curve (GD98) (mmol C m^2/(mg Chl W sec))
      !alphaPI = (/ 0.45*dps, 0.348*dps, 0.3*dps /)  ! MF: sensitivity tests
      !PCref = (/ 5.5*dps, 5.5*dps, 0.7*dps /) ! MF: previous defaults
      !PCref = (/ 4.4*dps, 4.4*dps, 0.56*dps /) ! MF: sensitivity tests 80%
      !PCref = (/ 4.1*dps, 4.1*dps, 0.52*dps /) ! MF: sensitivity tests 75%
      PCref = (/ 3.85*dps, 3.85*dps, 0.5*dps /) ! MF: sensitivity tests 70%
      thetaN_max = (/ 2.5, 4.0, 2.5 /)
      loss_thres = (/ 0.04, 0.04, 0.022 /)
      loss_thres2 = (/ 0.0, 0.0, 0.001 /)
      temp_thres = (/ -10.0, -10.0, 14.0 /)
      mort = (/ 0.12*dps, 0.12*dps, 0.15*dps /)  ! non-grazing mortality (1/sec)
      mort2 = (/ 0.001*dps, 0.001*dps, 0.0 /)
      agg_rate_max = (/ 0.9, 0.9, 0.0 /)
      agg_rate_min = (/ 0.01, 0.02, 0.0 /)
      !z_umax_0 = (/ 3.3*dps, 3.08*dps, 0.6*dps /) ! previous defaults
      z_umax_0 = (/ 2.64*dps, 2.46*dps, 0.48*dps /) ! MF: sensitivity test 80%
      !z_umax_0 = (/ 2.47*dps, 2.31*dps, 0.45*dps /) ! MF: sensitivity test 75%
      !z_umax_0 = (/ 2.8*dps, 2.6*dps, 0.51*dps /) ! MF: sensitivity test 85%
      z_grz = (/ 1.05, 1.0, 1.2 /)      ! grazing coefficient (mmol C/m^3)
      graze_zoo = (/ 0.3, 0.3, 0.3 /)
      graze_poc = (/ 0.0, 0.42, 0.05 /)
      graze_doc = (/ 0.15, 0.15, 0.15 /)
      loss_poc = (/ 0.0, 0.0, 0.0 /)
      f_zoo_detr = (/ 0.15, 0.2, 0.15 /)

# endif

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

      ! More default parameters for diatoms:
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

      ! More default parameters for diazotrophs:
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

#ifdef BEC_COCCO
      ! over-write sp information: no implicit calcifiers in this case    
      imp_calcifier(sp_ind) = .false.
      CaCO3_ind(sp_ind)     = 0

      ! Some more default parameters for coccolithophores:
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
# ifdef MPI
      end if  ! (mynode.eq.0)
# endif
#endif /* BIOPAR_NOWRITE */
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
      lriver_load_alk = .TRUE.
      lriver_load_dic = .TRUE.
      lriver_load_si = .TRUE.
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
