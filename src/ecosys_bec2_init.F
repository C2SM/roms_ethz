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
     &    parm_fe_scavenge_rate0,
     &    parm_f_prod_sp_CaCO3,
     &    parm_POC_diss,
     &    parm_SiO2_diss,
     &    parm_CaCO3_diss,
     &    parm_scalelen_z,
     &    parm_scalelen_vals,
     &    kFe, kPO4, kDOP, kNO3, kNH4, kSiO3, Qp, gQfe_0, gQfe_min, alphaPI, PCref,
     &    thetaN_max, loss_thres, loss_thres2, temp_thres, mort, mort2, agg_rate_max,
     &    agg_rate_min, z_umax_0, z_grz, graze_zoo, graze_poc, graze_doc, 
     &    loss_poc, f_zoo_detr


      filename_biopar_bec_nml = 'biopar_bec.nml'
      filename_biopar_out_bec_nml = 'biopar_out_bec.nml'

      !---------------------------------------------------------------------------
      !   default namelist settings
      !---------------------------------------------------------------------------

      parm_Fe_bioavail       = 1.0
      parm_o2_min            = 4.0  ! mmol/m^3 = nmol/cm^3
      parm_o2_min_delta      = 2.0  ! mmol/m^3 = nmol/cm^3
      parm_kappa_nitrif      = 0.06 * dps ! (= 1/( days))
      !!!!parm_nitrif_par_lim    = 1.0
      parm_nitrif_par_lim    = 4.0
      parm_z_mort_0          = 0.1 * dps
      parm_z_mort2_0         = 0.4 * dps
      parm_labile_ratio      = 0.85
      parm_POMbury           = 1.4 ! x1 default
      parm_BSIbury           = 0.45 ! x1 default
      parm_fe_scavenge_rate0 = 1.3 ! x1 default
      parm_f_prod_sp_CaCO3   = 0.055 ! x1 default
      parm_POC_diss          = 88.0
      parm_SiO2_diss         = 250.0
      parm_CaCO3_diss        = 150.0

      parm_scalelen_z    = (/ 130.0, 290.0, 670.0, 1700.0 /) ! DL: converted to m
      parm_scalelen_vals = (/     1.0,     2.9,     4.8,      8.0 /) ! x1 default
      
      ! The following arrays give parameters in this order: sp, diat, diaz
      kFe = (/ 0.04e-3, 0.06e-3, 0.04e-3 /) ! mmol Fe/m3
      kPO4 = (/ 0.01, 0.05, 0.02 /)         ! mmol P/m3
      kDOP = (/ 0.26, 0.9, 0.09 /)          ! 
      kNO3 = (/ 0.1, 0.5, 1.0 /)            ! mmol N/m3
      kNH4 = (/ 0.01, 0.05, 0.15 /)         ! mmol N/m3
      kSiO3 = (/ 0.0, 0.8, 0.0 /)           ! mmol SiO3/m3
      Qp = (/ 0.00855, 0.00855, 0.002735 /)
      gQfe_0 = (/ 20.0e-6, 20.0e-6, 60.0e-6 /)
      gQfe_min = (/ 3.0e-6, 3.0e-6, 12.0e-6 /)
      alphaPI = (/ 0.6*dps, 0.465*dps, 0.4*dps /)  ! Chl. specific initial slope of P_I curve (GD98) (mmol C m^2/(mg Chl W sec))
      PCref = (/ 5.5*dps, 5.5*dps, 0.7*dps /)
      thetaN_max = (/ 2.5, 4.0, 2.5 /)
      loss_thres = (/ 0.04, 0.04, 0.022 /)
      loss_thres2 = (/ 0.0, 0.0, 0.001 /)
      temp_thres = (/ -10.0, -10.0, 14.0 /)
      mort = (/ 0.12*dps, 0.12*dps, 0.15*dps /)  ! non-grazing mortality (1/sec)
      mort2 = (/ 0.001*dps, 0.001*dps, 0.0 /)
      agg_rate_max = (/ 0.9, 0.9, 0.0 /)
      agg_rate_min = (/ 0.01, 0.02, 0.0 /)
      z_umax_0 = (/ 3.3*dps, 3.08*dps, 0.6*dps /)
      z_grz = (/ 1.05, 1.0, 1.2 /)      ! grazing coefficient (mmol C/m^3)
      graze_zoo = (/ 0.3, 0.3, 0.3 /)
      graze_poc = (/ 0.0, 0.42, 0.05 /)
      graze_doc = (/ 0.15, 0.15, 0.15 /)
      loss_poc = (/ 0.0, 0.0, 0.0 /)
      f_zoo_detr = (/ 0.15, 0.2, 0.15 /)

      ! Some more default parameters for small phytoplankton:
      auto_ind = sp_ind
      sname(auto_ind)         = 'sp'
      lname(auto_ind)         = 'Small Phyto'
      Nfixer(auto_ind)        = .false.
      imp_calcifier(auto_ind) = .true.
      exp_calcifier(auto_ind) = .false.
      grazee_ind(auto_ind)    = auto_ind
      C_ind(auto_ind)         = spC_ind_t
      Chl_ind(auto_ind)       = spChl_ind_t
      Fe_ind(auto_ind)        = spFe_ind_t
      Si_ind(auto_ind)        = 0
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
#ifdef MPI
         if (mynode.eq.0) then
#endif
            print *, 'Biology parameter namelist file was not found'
            print *, 'Default values will be used.'
#ifdef MPI
         end if
#endif
      end if  ! status .eq. 0
#ifdef MPI
      ! Master process writes file with parameters used:
      if (mynode.eq.0) then
#endif
         open(74,file=filename_biopar_out_bec_nml,status='unknown',
     &           iostat=status)
         if (status .eq. 0) then
            write(74,NML=ecosys_parms_nml,iostat=status)
            if (status .ne. 0) then
               print *, 'Could not write the biology parameter namelist!'
            end if
            close(74)
         else
            print *, 'Could not create output file ',
     &           filename_biopar_out_bec_nml
         end if
#ifdef MPI
      end if  ! (mynode.eq.0)
#endif

#ifdef BEC2_DIAG
      !---------------------------------------------------------------------------
      !   Initialize diagnostic variables:
      !---------------------------------------------------------------------------
      bec2_diag_2d = c0
      bec2_diag_3d = c0
#endif

      !---------------------------------------------------------------------------
      !   Initialize ice fraction and atm. pressure field:
      !---------------------------------------------------------------------------
      ifrac = 0.0
      press = 1.

      lflux_gas_o2  = .TRUE.
      lflux_gas_co2 = .TRUE.
      liron_flux = .TRUE.
      lsource_sink  = .TRUE.  

      where(rmask ==1)
         landmask=.TRUE.
      elsewhere 
         landmask=.FALSE.
      endwhere


      end subroutine ecosys2_init

#else /* BIOLOGY_BEC2 */
      subroutine ecosys2_init()
      return 
      end
#endif /* BIOLOGY_BEC2 */
