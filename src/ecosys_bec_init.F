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

         do nn=1,N
           dz(:,:,nn)=hz(:,:,nn)
!HF2           dz(:,:,nn)=hz(:,:,km_eco-nn+1)
!HF           dz(:,:,nn)=hz(:,:,km_eco-nn+1)*100.

           zt(:,:,nn)=-1.0*z_r(:,:,nn)
!HF2           zt(:,:,nn)=-1.0*z_r(:,:,km_eco-nn+1)
!HF           zt(:,:,nn)=-1.0*z_r(:,:,km_eco-nn+1)*100.
           dzr(:,:,nn)=1.0/dz(:,:,nn)
         enddo

!         print *, "dz",dz(5,5,1:km_eco)
!         print *, "hz",hz(5,5,1:km_eco)

        sinking_particle_POC=0.0
        sinking_particle_P_CaCO3=0.0
        sinking_particle_P_SiO2=0.0
        sinking_particle_dust=0.0
        sinking_particle_P_iron =0.0

       parm_Fe_bioavail    = 0.02
       parm_prod_dissolve  = 0.67
       parm_o2_min         = 4.0
       parm_Rain_CaCO3     = 0.07
       parm_Rain_SiO2      = 0.03
       parm_kappa_nitrif   = 0.06 * dps       ! (= 1/( days))
       parm_nitrif_par_lim = 4.0
! HF: changed from 6.6e-5 on Oct 3, 2005, following Keith Moore's changes
       parm_POC_flux_ref   = 2.e-3           ! (mmol C/m^2/sec) - WAS 6.6e-3 nmol C/cm^2/sec
       parm_rest_prod_tau  = 30.0 * dps       ! (= 30 days)
       parm_rest_prod_z_c  = 75.              ! (m)
       parm_z_umax_0       = 2.75 * dps
       parm_diat_umax_0    = 2.07 * dps     
       parm_z_mort_0       = 0.1 * dps
! HF: changed from 0.46 on Oct 3, 2005, following Keith Moore's changes
       parm_z_mort2_0      = 0.45 * dps
       parm_sd_remin_0     = 0.01 * dps       ! (= 1/(100 days))
       parm_sp_kNO3        = 0.5
       parm_diat_kNO3      = 2.5
       parm_sp_kNH4        = 0.006
! HF: changed from 0.06 on Oct 3, 2005, following Keith Moore's changes
       parm_diat_kNH4      = 0.08
       parm_sp_kFe         = 0.06e-3          ! mmol Fe/m3
! HF: changed from 0.16e-3 on Oct 3, 2005, following Keith Moore's changes
       parm_diat_kFe       = 0.15e-3          ! mmol Fe/m3
       parm_diat_kSiO3     = 1.0
       parm_sp_kPO4        = 0.0005
       parm_diat_kPO4      = 0.005
       parm_z_grz          = 1.05
! HF: changed from "0.25 * dps" on Oct 3, 2005, following Keith Moore's changes
       parm_alphaChl       = 0.3 * dps
       parm_labile_ratio   = 0.70
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

       WS_HIST              = c0   
       XKW_HIST             = c0    
       AP_HIST              = c0    
       SCHMIDT_O2_HIST      = c0    
       O2SAT_HIST           = c0    
       FG_O2_HIST           = c0    
       SCHMIDT_CO2_HIST     = c0    
       PH_HIST              = c0    
       CO2STAR_HIST         = c0    
       DCO2STAR_HIST        = c0    
       pCO2SURF_HIST        = c0    
       DpCO2_HIST           = c0    
       FG_CO2_HIST          = c0    
       IRON_FLUX_HIST       = c0    
       PO4_RESTORE_HIST     = c0    
       NO3_RESTORE_HIST     = c0    
       SiO3_RESTORE_HIST    = c0    
       PAR_avg_HIST         = c0    
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
       DON_prod_HIST	 = c0	 
       DON_remin_HIST	 = c0    
       DOFe_prod_HIST	 = c0	 
       DOFe_remin_HIST	 = c0    
       DOP_prod_HIST	 = c0	 
       DOP_remin_HIST	 = c0    
       bSi_form_HIST	 = c0    
       photoFe_diat_HIST    = c0    
       photoFe_diaz_HIST    = c0    
       photoFe_sp_HIST      = c0    

# ifdef BGC_FLUX_ANALYSIS
       call init_scalars_becflux()
# endif
#else
       subroutine ecosys_init_empty()
#endif  /*BIOLOGY_BEC*/

       return 
       end
