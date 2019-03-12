!zhc: initialize ecosystem tracer , grid information, parameters ,variables

#include "cppdefs.h"
#ifdef BIOLOGY_BEC
         subroutine ecosys_init()
         implicit none

#include "param.h"
 !need for im_eco,jm_eco,tracer_cnt_eco
#include "ocean3d.h"
!need for tracer (t in roms),zt,dz
#include "grid.h"
 !need for landmask form rmask(roms)
#include "param_bec.h"
#include "ecosys_bec.h"

!since tracer structure is different to t(roms),first, assign to tracer
      integer::i,j,k,m,nn

!JDS namelist files for parameter input/output for optimization purposes
      integer status
      character*100 filename_biopar_bec_nml, filename_biopar_out_bec_nml

      filename_biopar_bec_nml = 'biopar_bec.nml'
      filename_biopar_out_bec_nml = 'biopar_out_bec.nml'

       sinking_particle_POC=0.0
       sinking_particle_P_CaCO3=0.0
       sinking_particle_P_SiO2=0.0
       sinking_particle_dust=0.0
       sinking_particle_P_iron =0.0

       parm_Fe_bioavail    = 0.02
       parm_o2_min         = 4.0
       parm_kappa_nitrif   = 0.06 * dps       ! (= 1/( days)=6.944e-7)
       parm_nitrif_par_lim = 4.0
! HF: changed from 6.6e-5 on Oct 3, 2005, following Keith Moore's changes
       parm_POC_flux_ref   = 2.e-3           ! (mmol C/m^2/sec) - WAS 6.6e-3 nmol C/cm^2/sec

       parm_z_umax_0       = 2.75 * dps !=2.8356481481481483e-05
       parm_diat_umax_0    = 2.07 * dps !=2.3958333333333331e-05
       parm_z_mort_0       = 0.1 * dps !=1.1574074074074074e-06
! HF: changed from 0.46 on Oct 3, 2005, following Keith Moore's changes
       parm_z_mort2_0      = 0.45 * dps !5.2083333333333332e-06
       parm_sd_remin_0     = 0.01 * dps !=1.1574074074074074e-07
       parm_diaz_umax_0    = 1.2 * dps  !=1.3888888888888888e-05

       parm_sp_kNO3        = 0.5
       parm_sp_kNH4        = 0.006
       parm_sp_kPO4        = 0.0005
       parm_sp_kFe         = 0.06e-3          ! mmol Fe/m3

       parm_alphaChl       = 0.3 * dps !3.472222222222222e-06
       parm_alphaDiaz      = 0.036 * dps !4.1666666666666661e-07

       parm_diat_kNO3      = 2.5
! HF: changed from 0.06 on Oct 3, 2005, following Keith Moore's changes
       parm_diat_kNH4      = 0.08
! HF: changed from 0.16e-3 on Oct 3, 2005, following Keith Moore's changes
       parm_diat_kFe       = 0.15e-3          ! mmol Fe/m3
       parm_diat_kSiO3     = 1.0
       parm_diat_kPO4      = 0.005

       parm_z_grz          = 1.05
! HF: changed from "0.25 * dps" on Oct 3, 2005, following Keith Moore's changes
       parm_labile_ratio   = 0.70
       diat_mort  = 0.1 * dps !=1.1574074074074074e-06
       diat_mort2 = 0.009 * dps !1.0416666666666665e-07
       sp_mort    = 0.1 * dps
       sp_mort2   = 0.009 * dps
       z_ingest   = 0.3
       thres_z1   = 100.0   ! 100 m
       thres_z2   = 200.0   ! 200 m

! HF: changed from "0.025*dps" on Oct 3, 2005, following Keith Moore's changes
       parm_alphaDiaz      = 0.036 * dps
       parm_diaz_umax_0    = 1.2 * dps
       gQsi_0        = 0.137
       gQsi_coef     = 2.5
       gQsi_max      = 0.685

        lrest_po4      = .FALSE.
        lrest_no3      = .FALSE.
        lrest_sio3     = .FALSE.

        lsource_sink    = .TRUE.
        lflux_gas_o2    = .TRUE.  !read gas flux
        lflux_gas_co2   = .TRUE.
        liron_flux      =.TRUE.
        ldust_flux      =.TRUE.


! JDS: parameters that were initialized within ecosys_bec.F are now included here:
         PCref      = 3.0 * dps
         sp_mort    = 0.1 * dps
         sp_mort2   = 0.009 * dps
         diat_mort  = 0.1 * dps
         diat_mort2 = 0.009 * dps
         z_ingest   = 0.3
         thres_z1   = 100.0   ! 100 m
         thres_z2   = 200.0   ! 200 m

! HF: changed from "0.18" on Oct 3, 2005, following Keith Moore's changes
         diaz_mort  = 0.16 * dps !1.8518518518518519e-06
! HF: changed from "0.008" on Oct 3, 2005, following Keith Moore's changes
         diaz_kPO4  = 0.005 ! 1.6 * diatom value
         diaz_kFe   = 0.1e-3

    !---------------------------------------------------------------------------
    !   SET FIXED RATIOS for N/C, P/C, SiO3/C, Fe/C
    !   assumes C/N/P of 117/16/1 based on Anderson and Sarmiento, 1994
    !   for diazotrophs a N/P of 45 is assumed based on Letelier & Karl, 1998
    !---------------------------------------------------------------------------

         Q         = 0.137
         Qp        = 0.00855
! HF: changed from "0.00304" on Oct 3, 2005, following Keith Moore's changes
         Qp_diaz   = 0.002735
         Qfe_zoo   = 2.5e-6

!Parameters concerning particulate terms computation
         Q10_growth =  2.0
         Q10_POC   =  1.0
         parm_diss          =  1.0
         parm_gamma         =  1.0
         parm_decay_hard    =  2.5e-5
         parm_nitrif_o2     =  2.0

         where(rmask ==1)
              landmask=.TRUE.
         elsewhere
              landmask=.FALSE.
         endwhere

    !------------------------------------------------------------------------
    ! initialize tracers and apply land mask to tracers (suppose done by roms
    !------------------------------------------------------------------------


    !------------------------------------------------------------------------
    !   read gas flux forcing (if required)(fice_file,xkw_file and ap_file
    !   don't need right now
    !------------------------------------------------------------------------
         ifrac=0.0
         press=1.


    !-------------------------------------------------------------------------
    !   initialize restoring timescale (if required)
    !------------------------------------------------------------------------
      IF (lrest_po4 .OR. lrest_no3 .OR. lrest_sio3) THEN
!zhc: all flags are false, no action right now
      END IF
    !-----------------------------------------------------------------------
    !   load restoring fields (if required)
    !-----------------------------------------------------------------------

    !------------------------------------------------------------------------
    !   load dust flux fields (if required)dust_flux,iron_flux
    !------------------------------------------------------------------------
    !------------------------------------------------------------------------
    !  If co2 gas flux is to be computed, initialize PH_HIST. The way in which
    !   this is done is based upon aux_restart_option.
    !------------------------------------------------------------------------
!!HF      IF (lflux_gas_co2) THEN
!!HF         ph_hist=0.0            !zhc:initial ph with zero
!!HF      endif

# if defined CH_CARBON_DEPTH
       HCO3d_HIST            = c0
       CO3d_HIST             = c0
       CO2STARd_HIST         = c0
       PHd_HIST              = c0
# endif /* CH_CARBON_DEPTH */

       WS_HIST              = c0
       XKW_HIST             = c0
       AP_HIST              = c0
       SCHMIDT_O2_HIST      = c0
       O2SAT_HIST           = c0
       FG_O2_HIST           = c0
       SCHMIDT_CO2_HIST     = c0
       pH_hist              = c0
       CO2STAR_HIST         = c0
       DCO2STAR_HIST        = c0
       pCO2sw               = c0
!HF       DpCO2_HIST           = c0
       pCO2air              = c0
       FG_CO2_HIST          = c0
       IRON_FLUX_HIST       = c0
       PO4_RESTORE_HIST     = c0
       NO3_RESTORE_HIST     = c0
       SiO3_RESTORE_HIST    = c0
       PAR                  = c0
       PO4STAR_HIST         = c0
       POC_FLUX_IN_HIST     = c0
       POC_PROD_HIST        = c0
       POC_REMIN_HIST       = c0
       CaCO3_FLUX_IN_HIST   = c0
       CaCO3_PROD_HIST      = c0
       CaCO3_REMIN_HIST     = c0
       SiO2_FLUX_IN_HIST    = c0
       SiO2_PROD_HIST       = c0
       SiO2_REMIN_HIST      = c0
       dust_FLUX_IN_HIST    = c0
       dust_REMIN_HIST      = c0
       P_iron_FLUX_IN_HIST  = c0
       P_iron_PROD_HIST     = c0
       P_iron_REMIN_HIST    = c0
       graze_sp_HIST        = c0
       graze_diat_HIST      = c0
       graze_tot_HIST       = c0
       sp_loss_HIST         = c0
       diat_loss_HIST       = c0
       zoo_loss_HIST        = c0
       sp_agg_HIST          = c0
       diat_agg_HIST        = c0
       photoC_sp_HIST       = c0
       photoC_diat_HIST     = c0
       tot_prod_HIST        = c0
       DOC_prod_HIST        = c0
       DOC_remin_HIST       = c0
       Fe_scavenge_HIST     = c0
       sp_N_lim_HIST        = c0
       sp_Fe_lim_HIST       = c0
       sp_PO4_lim_HIST      = c0
       sp_light_lim_HIST    = c0
       diat_N_lim_HIST      = c0
       diat_Fe_lim_HIST     = c0
       diat_PO4_lim_HIST    = c0
       diat_SiO3_lim_HIST   = c0
       diat_light_lim_HIST  = c0
       CaCO3_form_HIST      = c0
       diaz_Nfix_HIST       = c0
       graze_diaz_HIST      = c0
       diaz_loss_HIST       = c0
       photoC_diaz_HIST     = c0
       diaz_P_lim_HIST      = c0
       diaz_Fe_lim_HIST     = c0
       diaz_light_lim_HIST  = c0
       Fe_scavenge_rate_HIST = c0
       DON_prod_HIST        = c0
       DON_remin_HIST       = c0
       DOFe_prod_HIST       = c0
       DOFe_remin_HIST      = c0
       DOP_prod_HIST        = c0
       DOP_remin_HIST       = c0
       bSi_form_HIST        = c0
       photoFe_diat_HIST    = c0
       photoFe_diaz_HIST    = c0
       photoFe_sp_HIST      = c0
       nitrif_HIST          = c0
#ifdef OXYLIM_BEC
       denitr_DOC_HIST      = c0
       denitr_POC_HIST      = c0
#endif

!Jds: writing and reading namelist files to prescribe parameters

      open(73,file=filename_biopar_bec_nml,status='old',iostat=status)
! Since this file is optional, it's okay to proceed if it doesn't exist or
! can't be read.
      if (status .eq. 0) then
         read(73,NML=biology_param_bec_nml,iostat=status)
         if (status .ne. 0) then
            print *, 'Could not read the biology parameter namelist!'
         else
            print *, 'Biology parameters were read from ',
     &           filename_biopar_bec_nml
         end if
         close(73)
      else
         print *, 'Biology parameter namelist file was not found'
         print *, 'Default values will be used.'
      end if

      open(74,file=filename_biopar_out_bec_nml,status='unknown',
     &     iostat=status)
      if (status .eq. 0) then
         write(74,NML=biology_param_bec_nml,iostat=status)
         if (status .ne. 0) then
            print *, 'Could not write the biology parameter namelist!'
         end if
         close(74)
      else
         print *, 'Could not create output file ',
     &        filename_biopar_out_bec_nml
      end if

# ifdef BGC_FLUX_ANALYSIS
       call init_scalars_becflux()
# endif
#else
       subroutine ecosys_init_empty()
#endif  /*BIOLOGY_BEC*/

       return
       end
