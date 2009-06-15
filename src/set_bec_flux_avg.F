#include "cppdefs.h"
#if defined BIOLOGY_BEC && defined BGC_FLUX_ANALYSIS && defined AVERAGES
      subroutine set_bec_flux_avg(tile)
      implicit none
      integer tile
# include "param.h"

# include "compute_tile_bounds.h"
      call set_bec_flux_avg_tile(Istr,Iend,Jstr,Jend)
      return
      end

         subroutine set_bec_flux_avg_tile (Istr,Iend,Jstr,Jend)
         implicit none
# include "param.h"
# include "ecosys_bec.h"
# include "bec_avg.h"
# include "ncroms_bgcflux.h"
# include "scalars.h"
# include "ocean2d.h"
         integer istr,jstr,iend,jend
         real cff

# include "compute_auxiliary_bounds.h"
! calculation of averaged fluxes will only be performed if n_bgc_flux_avg
! is a positive number
      if (n_bgc_flux_avg < 1) return

         if (iic.gt.nts_bgc_flux_avg) then
            if (n_bgc_flux_avg .eq. 1 .or.
     &        mod(iic-1,n_bgc_flux_avg).eq.1) then
! 2D variables
            if (ZEROTH_TILE) then
               time_bgc_flux_avg=time
          endif

            zeta_bgc_flux_avg(istrR:iendR,jstrR:jendR) = 
     &                  zeta(istrR:iendR,jstrR:jendR,knew)
            WS_AVG(istrR:iendR,jstrR:jendR) =
     &                  WS_hist(istrR:iendR,jstrR:jendR)
            XKW_AVG(istrR:iendR,jstrR:jendR) =
     &                  XKW_hist(istrR:iendR,jstrR:jendR)
            ap_AVG(istrR:iendR,jstrR:jendR) =
     &                  ap_hist(istrR:iendR,jstrR:jendR)
            SCHMIDT_O2_AVG(istrR:iendR,jstrR:jendR) = 
     &                  SCHMIDT_O2_hist(istrR:iendR,jstrR:jendR)
            O2SAT_AVG(istrR:iendR,jstrR:jendR)= 
     &                  O2SAT_hist(istrR:iendR,jstrR:jendR)
            FG_O2_AVG(istrR:iendR,jstrR:jendR) = 
     &                  FG_O2_hist(istrR:iendR,jstrR:jendR)
            SCHMIDT_CO2_AVG(istrR:iendR,jstrR:jendR) = 
     &                  SCHMIDT_CO2_hist(istrR:iendR,jstrR:jendR)
            CO2STAR_AVG(istrR:iendR,jstrR:jendR) = 
     &                  CO2STAR_hist(istrR:iendR,jstrR:jendR)
            DCO2STAR_AVG(istrR:iendR,jstrR:jendR) = 
     &                  DCO2STAR_hist(istrR:iendR,jstrR:jendR)
            FG_CO2_AVG(istrR:iendR,jstrR:jendR) = 
     &                  FG_CO2_hist(istrR:iendR,jstrR:jendR)
            IRON_FLUX_AVG(istrR:iendR,jstrR:jendR) = 
     &                  IRON_FLUX_hist(istrR:iendR,jstrR:jendR)
            PARinc_flux_AVG(istrR:iendR,jstrR:jendR) = 
     &                  PARinc(istrR:iendR,jstrR:jendR)
! 3D variables
            PO4_RESTORE_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  PO4_RESTORE_hist(istrR:iendR,jstrR:jendR,:)
            NO3_RESTORE_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  NO3_RESTORE_hist(istrR:iendR,jstrR:jendR,:)
            SiO3_RESTORE_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  SiO3_RESTORE_hist(istrR:iendR,jstrR:jendR,:)
            PAR_flux_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  PAR(istrR:iendR,jstrR:jendR,:)
            PO4STAR_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  PO4STAR_hist(istrR:iendR,jstrR:jendR,:)
            POC_FLUX_IN_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  POC_FLUX_IN_hist(istrR:iendR,jstrR:jendR,:)
            POC_PROD_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  POC_PROD_hist(istrR:iendR,jstrR:jendR,:)
            POC_REMIN_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  POC_REMIN_hist(istrR:iendR,jstrR:jendR,:)
            CaCO3_FLUX_IN_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  CaCO3_FLUX_IN_hist(istrR:iendR,jstrR:jendR,:)
            CaCO3_PROD_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  CaCO3_PROD_hist(istrR:iendR,jstrR:jendR,:)
            CaCO3_REMIN_AVG(istrR:iendR,jstrR:jendR,:) =
     &                  CaCO3_REMIN_hist(istrR:iendR,jstrR:jendR,:)
            SiO2_FLUX_IN_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  SiO2_FLUX_IN_hist(istrR:iendR,jstrR:jendR,:)
            SiO2_PROD_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  SiO2_PROD_hist(istrR:iendR,jstrR:jendR,:)
            SiO2_REMIN_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  SiO2_REMIN_hist(istrR:iendR,jstrR:jendR,:)
            dust_FLUX_IN_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  dust_FLUX_IN_hist(istrR:iendR,jstrR:jendR,:)
            dust_REMIN_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  dust_REMIN_hist(istrR:iendR,jstrR:jendR,:)
            P_iron_FLUX_IN_AVG(istrR:iendR,jstrR:jendR,:) =
     &                  P_iron_FLUX_IN_hist(istrR:iendR,jstrR:jendR,:)
            P_iron_PROD_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  P_iron_PROD_hist(istrR:iendR,jstrR:jendR,:)
            P_iron_REMIN_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  P_iron_REMIN_hist(istrR:iendR,jstrR:jendR,:)
            graze_sp_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  graze_sp_hist(istrR:iendR,jstrR:jendR,:)
            graze_diat_AVG(istrR:iendR,jstrR:jendR,:) =  
     &                  graze_diat_hist(istrR:iendR,jstrR:jendR,:)
            graze_tot_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  graze_tot_hist(istrR:iendR,jstrR:jendR,:)
            sp_loss_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  sp_loss_hist(istrR:iendR,jstrR:jendR,:)
            diat_loss_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  diat_loss_hist(istrR:iendR,jstrR:jendR,:)
            zoo_loss_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  zoo_loss_hist(istrR:iendR,jstrR:jendR,:)
            sp_agg_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  sp_agg_hist(istrR:iendR,jstrR:jendR,:)
            diat_agg_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  diat_agg_hist(istrR:iendR,jstrR:jendR,:)
            photoC_sp_AVG(istrR:iendR,jstrR:jendR,:) =  
     &                  photoC_sp_hist(istrR:iendR,jstrR:jendR,:)
            photoC_diat_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  photoC_diat_hist(istrR:iendR,jstrR:jendR,:)
            tot_prod_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  tot_prod_hist(istrR:iendR,jstrR:jendR,:)
            DOC_prod_AVG(istrR:iendR,jstrR:jendR,:) =  
     &                  DOC_prod_hist(istrR:iendR,jstrR:jendR,:)
            DOC_remin_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  DOC_remin_hist(istrR:iendR,jstrR:jendR,:)
            Fe_scavenge_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  Fe_scavenge_hist(istrR:iendR,jstrR:jendR,:)
            sp_N_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  sp_N_lim_hist(istrR:iendR,jstrR:jendR,:)
            sp_Fe_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  sp_Fe_lim_hist(istrR:iendR,jstrR:jendR,:)
            sp_PO4_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  sp_PO4_lim_hist(istrR:iendR,jstrR:jendR,:)
            sp_light_lim_AVG(istrR:iendR,jstrR:jendR,:) =
     &                  sp_light_lim_hist(istrR:iendR,jstrR:jendR,:)
            diat_N_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  diat_N_lim_hist(istrR:iendR,jstrR:jendR,:)
            diat_Fe_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  diat_Fe_lim_hist(istrR:iendR,jstrR:jendR,:)
            diat_PO4_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  diat_PO4_lim_hist(istrR:iendR,jstrR:jendR,:)
            diat_SiO3_lim_AVG(istrR:iendR,jstrR:jendR,:) =  
     &                  diat_SiO3_lim_hist(istrR:iendR,jstrR:jendR,:)
            diat_light_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  diat_light_lim_hist(istrR:iendR,jstrR:jendR,:)
            CaCO3_form_AVG(istrR:iendR,jstrR:jendR,:) =
     &                  CaCO3_form_hist(istrR:iendR,jstrR:jendR,:)
            diaz_Nfix_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  diaz_Nfix_hist(istrR:iendR,jstrR:jendR,:)
            graze_diaz_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  graze_diaz_hist(istrR:iendR,jstrR:jendR,:)
            diaz_loss_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  diaz_loss_hist(istrR:iendR,jstrR:jendR,:)
            photoC_diaz_AVG(istrR:iendR,jstrR:jendR,:) =
     &                  photoC_diaz_hist(istrR:iendR,jstrR:jendR,:)
            diaz_P_lim_AVG(istrR:iendR,jstrR:jendR,:) =
     &                  diaz_P_lim_hist(istrR:iendR,jstrR:jendR,:)
            diaz_Fe_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  diaz_Fe_lim _hist(istrR:iendR,jstrR:jendR,:)
            diaz_light_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  diaz_light_lim_hist(istrR:iendR,jstrR:jendR,:)
            Fe_scavenge_rate_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  Fe_scavenge_rate_hist(istrR:iendR,jstrR:jendR,:)
            DON_prod_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  DON_prod_hist(istrR:iendR,jstrR:jendR,:)
            DON_remin_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  DON_remin_hist(istrR:iendR,jstrR:jendR,:)
            DOFe_prod_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  DOFe_prod_hist(istrR:iendR,jstrR:jendR,:)
            DOFe_remin_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  DOFe_remin_hist(istrR:iendR,jstrR:jendR,:)
            DOP_prod_AVG(istrR:iendR,jstrR:jendR,:) =
     &                  DOP_prod_hist(istrR:iendR,jstrR:jendR,:)
            DOP_remin_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  DOP_remin_hist(istrR:iendR,jstrR:jendR,:)
            bSI_form_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  bSI_form_hist(istrR:iendR,jstrR:jendR,:)
            photoFe_diaz_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  photoFe_diaz_hist(istrR:iendR,jstrR:jendR,:)
            photoFe_diat_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  photoFe_diat_hist(istrR:iendR,jstrR:jendR,:)
            photoFe_sp_AVG(istrR:iendR,jstrR:jendR,:) =  
     &                  photoFe_sp_hist(istrR:iendR,jstrR:jendR,:)
            nitrif_AVG(istrR:iendR,jstrR:jendR,:) =  
     &                  nitrif_hist(istrR:iendR,jstrR:jendR,:)

         elseif (mod(iic-1,n_bgc_flux_avg).gt.1) then
            if (ZEROTH_TILE) time_bgc_flux_avg=time_bgc_flux_avg+time
! 2D variables
            zeta_bgc_flux_avg(istrR:iendR,jstrR:jendR) = 
     &        zeta_bgc_flux_avg(istrR:iendR,jstrR:jendR) +
     &        zeta(istrR:iendR,jstrR:jendR,knew)
            WS_AVG(istrR:iendR,jstrR:jendR) = WS_AVG(istrR:iendR,jstrR:jendR) 
     &                       + WS_hist(istrR:iendR,jstrR:jendR)
            XKW_AVG(istrR:iendR,jstrR:jendR) = XKW_AVG(istrR:iendR,jstrR:jendR) 
     &                         + XKW_hist(istrR:iendR,jstrR:jendR)
            ap_AVG(istrR:iendR,jstrR:jendR) = ap_AVG(istrR:iendR,jstrR:jendR) 
     &                          + ap_hist(istrR:iendR,jstrR:jendR)
            SCHMIDT_O2_AVG(istrR:iendR,jstrR:jendR) = 
     &                         SCHMIDT_O2_AVG(istrR:iendR,jstrR:jendR) 
     &                       + SCHMIDT_O2_hist(istrR:iendR,jstrR:jendR)
            O2SAT_AVG(istrR:iendR,jstrR:jendR)= 
     &                      O2SAT_AVG(istrR:iendR,jstrR:jendR) 
     &                      +O2SAT_hist(istrR:iendR,jstrR:jendR)
            FG_O2_AVG(istrR:iendR,jstrR:jendR) = 
     &                       FG_O2_AVG(istrR:iendR,jstrR:jendR) 
     &                      +FG_O2_hist(istrR:iendR,jstrR:jendR)
            SCHMIDT_CO2_AVG(istrR:iendR,jstrR:jendR) = 
     &                 SCHMIDT_CO2_AVG(istrR:iendR,jstrR:jendR) 
     &                + SCHMIDT_CO2_hist(istrR:iendR,jstrR:jendR)
            CO2STAR_AVG(istrR:iendR,jstrR:jendR) = 
     &                     CO2STAR_AVG(istrR:iendR,jstrR:jendR) 
     &                    + CO2STAR_hist(istrR:iendR,jstrR:jendR)
            DCO2STAR_AVG(istrR:iendR,jstrR:jendR) = 
     &                  DCO2STAR_AVG(istrR:iendR,jstrR:jendR) 
     &                  + DCO2STAR_hist(istrR:iendR,jstrR:jendR)
            FG_CO2_AVG(istrR:iendR,jstrR:jendR) = 
     &                   FG_CO2_AVG(istrR:iendR,jstrR:jendR) 
     &                 + FG_CO2_hist(istrR:iendR,jstrR:jendR)
            IRON_FLUX_AVG(istrR:iendR,jstrR:jendR) = 
     &                 IRON_FLUX_AVG(istrR:iendR,jstrR:jendR) 
     &               + IRON_FLUX_hist(istrR:iendR,jstrR:jendR)
            PARinc_flux_AVG(istrR:iendR,jstrR:jendR) = 
     &                PARinc_flux_AVG(istrR:iendR,jstrR:jendR) 
     &                + PARinc(istrR:iendR,jstrR:jendR)
! 3D variables
            PO4_RESTORE_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                 PO4_RESTORE_AVG(istrR:iendR,jstrR:jendR,:) 
     &                 + PO4_RESTORE_hist(istrR:iendR,jstrR:jendR,:)
            NO3_RESTORE_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                NO3_RESTORE_AVG(istrR:iendR,jstrR:jendR,:)  
     &               + NO3_RESTORE_hist(istrR:iendR,jstrR:jendR,:)
            SiO3_RESTORE_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                SiO3_RESTORE_AVG(istrR:iendR,jstrR:jendR,:) 
     &                + SiO3_RESTORE_hist(istrR:iendR,jstrR:jendR,:)
            PAR_flux_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                PAR_flux_AVG(istrR:iendR,jstrR:jendR,:) 
     &                + PAR(istrR:iendR,jstrR:jendR,:)
            PO4STAR_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                PO4STAR_AVG(istrR:iendR,jstrR:jendR,:) 
     &               + PO4STAR_hist(istrR:iendR,jstrR:jendR,:)
            POC_FLUX_IN_AVG(istrR:iendR,jstrR:jendR,:) = 
     &             POC_FLUX_IN_AVG(istrR:iendR,jstrR:jendR,:)  
     &           + POC_FLUX_IN_hist(istrR:iendR,jstrR:jendR,:)
            POC_PROD_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                POC_PROD_AVG(istrR:iendR,jstrR:jendR,:) 
     &             + POC_PROD_hist(istrR:iendR,jstrR:jendR,:)
            POC_REMIN_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                   POC_REMIN_AVG(istrR:iendR,jstrR:jendR,:) 
     &                  + POC_REMIN_hist(istrR:iendR,jstrR:jendR,:)
            CaCO3_FLUX_IN_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  CaCO3_FLUX_IN_AVG(istrR:iendR,jstrR:jendR,:) 
     &                 + CaCO3_FLUX_IN_hist(istrR:iendR,jstrR:jendR,:)
            CaCO3_PROD_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                    CaCO3_PROD_AVG(istrR:iendR,jstrR:jendR,:) 
     &                 + CaCO3_PROD_hist(istrR:iendR,jstrR:jendR,:)
            CaCO3_REMIN_AVG(istrR:iendR,jstrR:jendR,:) =
     &                CaCO3_REMIN _AVG(istrR:iendR,jstrR:jendR,:) +
     &                   CaCO3_REMIN_hist(istrR:iendR,jstrR:jendR,:)
            SiO2_FLUX_IN_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                SiO2_FLUX_IN_AVG(istrR:iendR,jstrR:jendR,:) +
     &                SiO2_FLUX_IN_hist(istrR:iendR,jstrR:jendR,:)
            SiO2_PROD_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  SiO2_PROD_AVG(istrR:iendR,jstrR:jendR,:) 
     &                + SiO2_PROD_hist(istrR:iendR,jstrR:jendR,:)
            SiO2_REMIN_AVG(istrR:iendR,jstrR:jendR,:) = 
     &               SiO2_REMIN_AVG(istrR:iendR,jstrR:jendR,:) 
     &              + SiO2_REMIN_hist(istrR:iendR,jstrR:jendR,:)
            dust_FLUX_IN_AVG(istrR:iendR,jstrR:jendR,:) = 
     &               dust_FLUX_IN_AVG(istrR:iendR,jstrR:jendR,:) 
     &               + dust_FLUX_IN_hist(istrR:iendR,jstrR:jendR,:)
            dust_REMIN_AVG(istrR:iendR,jstrR:jendR,:) = 
     &              dust_REMIN_AVG(istrR:iendR,jstrR:jendR,:) 
     &              +dust_REMIN_hist(istrR:iendR,jstrR:jendR,:)
            P_iron_FLUX_IN_AVG(istrR:iendR,jstrR:jendR,:) =
     &           P_iron_FLUX_IN_AVG(istrR:iendR,jstrR:jendR,:) 
     &          +P_iron_FLUX_IN_hist(istrR:iendR,jstrR:jendR,:)
            P_iron_PROD_AVG(istrR:iendR,jstrR:jendR,:) = 
     &            P_iron_PROD_AVG(istrR:iendR,jstrR:jendR,:) 
     &             + P_iron_PROD_hist(istrR:iendR,jstrR:jendR,:)
            P_iron_REMIN_AVG(istrR:iendR,jstrR:jendR,:) = 
     &            P_iron_REMIN_AVG(istrR:iendR,jstrR:jendR,:) 
     &          + P_iron_REMIN_hist(istrR:iendR,jstrR:jendR,:)
            graze_sp_AVG(istrR:iendR,jstrR:jendR,:) = 
     &            graze_sp_AVG(istrR:iendR,jstrR:jendR,:) 
     &           + graze_sp_hist(istrR:iendR,jstrR:jendR,:)
            graze_diat_AVG(istrR:iendR,jstrR:jendR,:) =  
     &             graze_diat_AVG(istrR:iendR,jstrR:jendR,:) 
     &           + graze_diat_hist(istrR:iendR,jstrR:jendR,:)
            graze_tot_AVG(istrR:iendR,jstrR:jendR,:) = 
     &            graze_tot_AVG(istrR:iendR,jstrR:jendR,:) 
     &              + graze_tot_hist(istrR:iendR,jstrR:jendR,:)
            sp_loss_AVG(istrR:iendR,jstrR:jendR,:) = 
     &            sp_loss_AVG(istrR:iendR,jstrR:jendR,:) 
     &            +sp_loss_hist(istrR:iendR,jstrR:jendR,:)
            diat_loss_AVG(istrR:iendR,jstrR:jendR,:) = 
     &             diat_loss_AVG(istrR:iendR,jstrR:jendR,:) 
     &            +diat_loss_hist(istrR:iendR,jstrR:jendR,:)
            zoo_loss_AVG(istrR:iendR,jstrR:jendR,:) = 
     &              zoo_loss_AVG(istrR:iendR,jstrR:jendR,:) 
     &             + zoo_loss_hist(istrR:iendR,jstrR:jendR,:)
            sp_agg_AVG(istrR:iendR,jstrR:jendR,:) = 
     &             sp_agg_AVG(istrR:iendR,jstrR:jendR,:) 
     &              +sp_agg_hist(istrR:iendR,jstrR:jendR,:)
            diat_agg_AVG(istrR:iendR,jstrR:jendR,:) = 
     &              diat_agg_AVG(istrR:iendR,jstrR:jendR,:) 
     &              +diat_agg_hist(istrR:iendR,jstrR:jendR,:)
            photoC_sp_AVG(istrR:iendR,jstrR:jendR,:) =  
     &             photoC_sp_AVG(istrR:iendR,jstrR:jendR,:) 
     &            + photoC_sp_hist(istrR:iendR,jstrR:jendR,:)
            photoC_diat_AVG(istrR:iendR,jstrR:jendR,:) = 
     &             photoC_diat_AVG(istrR:iendR,jstrR:jendR,:) 
     &            + photoC_diat_hist(istrR:iendR,jstrR:jendR,:)
            tot_prod_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                tot_prod_AVG(istrR:iendR,jstrR:jendR,:) 
     &                +tot_prod_hist(istrR:iendR,jstrR:jendR,:)
            DOC_prod_AVG(istrR:iendR,jstrR:jendR,:) =  
     &                 DOC_prod_AVG(istrR:iendR,jstrR:jendR,:) 
     &              + DOC_prod_hist(istrR:iendR,jstrR:jendR,:)
            DOC_remin_AVG(istrR:iendR,jstrR:jendR,:) = 
     &               DOC_remin_AVG(istrR:iendR,jstrR:jendR,:) 
     &                 +DOC_remin_hist(istrR:iendR,jstrR:jendR,:)
            Fe_scavenge_AVG(istrR:iendR,jstrR:jendR,:) = 
     &               Fe_scavenge_AVG(istrR:iendR,jstrR:jendR,:) 
     &            + Fe_scavenge_hist(istrR:iendR,jstrR:jendR,:)
            sp_N_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &              sp_N_lim_AVG(istrR:iendR,jstrR:jendR,:) 
     &             + sp_N_lim_hist(istrR:iendR,jstrR:jendR,:)
            sp_Fe_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &             sp_Fe_lim_AVG(istrR:iendR,jstrR:jendR,:) 
     &            + sp_Fe_lim_hist(istrR:iendR,jstrR:jendR,:)
            sp_PO4_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &               sp_PO4_lim_AVG(istrR:iendR,jstrR:jendR,:) 
     &             + sp_PO4_lim_hist(istrR:iendR,jstrR:jendR,:)
            sp_light_lim_AVG(istrR:iendR,jstrR:jendR,:) =
     &              sp_light_lim_AVG(istrR:iendR,jstrR:jendR,:) 
     &            + sp_light_lim_hist(istrR:iendR,jstrR:jendR,:)
            diat_N_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                 diat_N_lim_AVG(istrR:iendR,jstrR:jendR,:) 
     &                +diat_N_lim_hist(istrR:iendR,jstrR:jendR,:)
            diat_Fe_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &              diat_Fe_lim_AVG(istrR:iendR,jstrR:jendR,:) 
     &             +diat_Fe_lim_hist(istrR:iendR,jstrR:jendR,:)
            diat_PO4_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &             diat_PO4_lim_AVG(istrR:iendR,jstrR:jendR,:) 
     &              +diat_PO4_lim_hist(istrR:iendR,jstrR:jendR,:)
            diat_SiO3_lim_AVG(istrR:iendR,jstrR:jendR,:) =  
     &            diat_SiO3_lim_AVG(istrR:iendR,jstrR:jendR,:) 
     &            + diat_SiO3_lim_hist(istrR:iendR,jstrR:jendR,:)
            diat_light_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &            diat_light_lim_AVG(istrR:iendR,jstrR:jendR,:) 
     &          + diat_light_lim_hist(istrR:iendR,jstrR:jendR,:)
            CaCO3_form_AVG(istrR:iendR,jstrR:jendR,:) =
     &            CaCO3_form_AVG(istrR:iendR,jstrR:jendR,:) 
     &           +CaCO3_form_hist(istrR:iendR,jstrR:jendR,:)
            diaz_Nfix_AVG(istrR:iendR,jstrR:jendR,:) = 
     &           diaz_Nfix_AVG(istrR:iendR,jstrR:jendR,:) 
     &           +diaz_Nfix_hist(istrR:iendR,jstrR:jendR,:)
            graze_diaz_AVG(istrR:iendR,jstrR:jendR,:) = 
     &           graze_diaz_AVG(istrR:iendR,jstrR:jendR,:) 
     &         + graze_diaz_hist(istrR:iendR,jstrR:jendR,:)
            diaz_loss_AVG(istrR:iendR,jstrR:jendR,:) = 
     &          diaz_loss_AVG(istrR:iendR,jstrR:jendR,:) 
     &        + diaz_loss_hist(istrR:iendR,jstrR:jendR,:)
            photoC_diaz_AVG(istrR:iendR,jstrR:jendR,:) =
     &            photoC_diaz_AVG(istrR:iendR,jstrR:jendR,:) 
     &            + photoC_diaz_hist(istrR:iendR,jstrR:jendR,:)
            diaz_P_lim_AVG(istrR:iendR,jstrR:jendR,:) =
     &           diaz_P_lim _AVG(istrR:iendR,jstrR:jendR,:) 
     &          + diaz_P_lim_hist(istrR:iendR,jstrR:jendR,:)
            diaz_Fe_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &           diaz_Fe_lim_AVG(istrR:iendR,jstrR:jendR,:) 
     &          +diaz_Fe_lim _hist(istrR:iendR,jstrR:jendR,:)
            diaz_light_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &            diaz_light_lim_AVG(istrR:iendR,jstrR:jendR,:) 
     &            + diaz_light_lim_hist(istrR:iendR,jstrR:jendR,:)
            Fe_scavenge_rate_AVG(istrR:iendR,jstrR:jendR,:) = 
     &              Fe_scavenge_rate_AVG(istrR:iendR,jstrR:jendR,:) 
     &            + Fe_scavenge_rate_hist(istrR:iendR,jstrR:jendR,:)
            DON_prod_AVG(istrR:iendR,jstrR:jendR,:) = 
     &               DON_prod_AVG(istrR:iendR,jstrR:jendR,:) + 
     &               DON_prod_hist(istrR:iendR,jstrR:jendR,:)
            DON_remin_AVG(istrR:iendR,jstrR:jendR,:) = 
     &               DON_remin_AVG(istrR:iendR,jstrR:jendR,:) 
     &               + DON_remin_hist(istrR:iendR,jstrR:jendR,:)
            DOFe_prod_AVG(istrR:iendR,jstrR:jendR,:) = 
     &             DOFe_prod_AVG(istrR:iendR,jstrR:jendR,:)
     &              + DOFe_prod_hist(istrR:iendR,jstrR:jendR,:)
            DOFe_remin_AVG(istrR:iendR,jstrR:jendR,:) = 
     &            DOFe_remin_AVG(istrR:iendR,jstrR:jendR,:) 
     &            + DOFe_remin_hist(istrR:iendR,jstrR:jendR,:)
            DOP_prod_AVG(istrR:iendR,jstrR:jendR,:) =
     &          DOP_prod_AVG(istrR:iendR,jstrR:jendR,:) 
     &          + DOP_prod_hist(istrR:iendR,jstrR:jendR,:)
            DOP_remin_AVG(istrR:iendR,jstrR:jendR,:) = 
     &          DOP_remin_AVG(istrR:iendR,jstrR:jendR,:) 
     &         + DOP_remin_hist(istrR:iendR,jstrR:jendR,:)
            bSI_form_AVG(istrR:iendR,jstrR:jendR,:) = 
     &           bSI_form_AVG(istrR:iendR,jstrR:jendR,:) 
     &           + bSI_form_hist(istrR:iendR,jstrR:jendR,:)
            photoFe_diaz_AVG(istrR:iendR,jstrR:jendR,:) = 
     &           photoFe_diaz_AVG(istrR:iendR,jstrR:jendR,:)
     &            + photoFe_diaz_hist(istrR:iendR,jstrR:jendR,:)
            photoFe_diat_AVG(istrR:iendR,jstrR:jendR,:) = 
     &          photoFe_diat_AVG(istrR:iendR,jstrR:jendR,:) +  
     &          photoFe_diat_hist(istrR:iendR,jstrR:jendR,:)
            photoFe_sp_AVG(istrR:iendR,jstrR:jendR,:) =  
     &          photoFe_sp_AVG(istrR:iendR,jstrR:jendR,:) 
     &          + photoFe_sp_hist(istrR:iendR,jstrR:jendR,:)
            nitrif_AVG(istrR:iendR,jstrR:jendR,:) =  
     &          nitrif_AVG(istrR:iendR,jstrR:jendR,:) 
     &          + nitrif_hist(istrR:iendR,jstrR:jendR,:)

         elseif (mod(iic-1,n_bgc_flux_avg).eq.0) then
            cff=1./float(n_bgc_flux_avg)
            if (ZEROTH_TILE) then
               time_bgc_flux_avg=cff*
     &           (time_bgc_flux_avg + time)
            endif
               
! 2D variables
            zeta_bgc_flux_avg(istrR:iendR,jstrR:jendR) = 
     &        cff*( zeta_bgc_flux_avg(istrR:iendR,jstrR:jendR) +
     &        zeta(istrR:iendR,jstrR:jendR,knew) )
            WS_AVG(istrR:iendR,jstrR:jendR) = cff*( WS_AVG(istrR:iendR,jstrR:jendR) 
     &                       + WS_hist(istrR:iendR,jstrR:jendR) )
            XKW_AVG(istrR:iendR,jstrR:jendR) = cff*( XKW_AVG(istrR:iendR,jstrR:jendR) 
     &                         + XKW_hist(istrR:iendR,jstrR:jendR) )
            ap_AVG(istrR:iendR,jstrR:jendR) = cff*( ap_AVG(istrR:iendR,jstrR:jendR) 
     &                          + ap_hist(istrR:iendR,jstrR:jendR) )
            SCHMIDT_O2_AVG(istrR:iendR,jstrR:jendR) = 
     &                         cff*( SCHMIDT_O2_AVG(istrR:iendR,jstrR:jendR) 
     &                       + SCHMIDT_O2_hist(istrR:iendR,jstrR:jendR) )
            O2SAT_AVG(istrR:iendR,jstrR:jendR)= 
     &                      cff*( O2SAT_AVG(istrR:iendR,jstrR:jendR) 
     &                      +O2SAT_hist(istrR:iendR,jstrR:jendR) )
            FG_O2_AVG(istrR:iendR,jstrR:jendR) = 
     &                       cff*( FG_O2_AVG(istrR:iendR,jstrR:jendR) 
     &                      +FG_O2_hist(istrR:iendR,jstrR:jendR) )
            SCHMIDT_CO2_AVG(istrR:iendR,jstrR:jendR) = 
     &                 cff*( SCHMIDT_CO2_AVG(istrR:iendR,jstrR:jendR) 
     &                + SCHMIDT_CO2_hist(istrR:iendR,jstrR:jendR) )
            CO2STAR_AVG(istrR:iendR,jstrR:jendR) = 
     &                     cff*( CO2STAR_AVG(istrR:iendR,jstrR:jendR) 
     &                    + CO2STAR_hist(istrR:iendR,jstrR:jendR) )
            DCO2STAR_AVG(istrR:iendR,jstrR:jendR) = 
     &                  cff*( DCO2STAR_AVG(istrR:iendR,jstrR:jendR) 
     &                  + DCO2STAR_hist(istrR:iendR,jstrR:jendR) )
            FG_CO2_AVG(istrR:iendR,jstrR:jendR) = 
     &                   cff*( FG_CO2_AVG(istrR:iendR,jstrR:jendR) 
     &                 + FG_CO2_hist(istrR:iendR,jstrR:jendR) )
            IRON_FLUX_AVG(istrR:iendR,jstrR:jendR) = 
     &                 cff*( IRON_FLUX_AVG(istrR:iendR,jstrR:jendR) 
     &               + IRON_FLUX_hist(istrR:iendR,jstrR:jendR) )
            PARinc_flux_AVG(istrR:iendR,jstrR:jendR) = 
     &                cff*( PARinc_flux_AVG(istrR:iendR,jstrR:jendR) 
     &                + PARinc(istrR:iendR,jstrR:jendR) )
! 3D variables
            PO4_RESTORE_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                 cff*( PO4_RESTORE_AVG(istrR:iendR,jstrR:jendR,:) 
     &                 + PO4_RESTORE_hist(istrR:iendR,jstrR:jendR,:) )
            NO3_RESTORE_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                cff*( NO3_RESTORE_AVG(istrR:iendR,jstrR:jendR,:)  
     &               + NO3_RESTORE_hist(istrR:iendR,jstrR:jendR,:) )
            SiO3_RESTORE_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                cff*( SiO3_RESTORE_AVG(istrR:iendR,jstrR:jendR,:) 
     &                + SiO3_RESTORE_hist(istrR:iendR,jstrR:jendR,:) )
            PAR_flux_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                cff*( PAR_flux_AVG(istrR:iendR,jstrR:jendR,:) 
     &                + PAR(istrR:iendR,jstrR:jendR,:) )
            PO4STAR_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                cff*( PO4STAR_AVG(istrR:iendR,jstrR:jendR,:) 
     &               + PO4STAR_hist(istrR:iendR,jstrR:jendR,:) )
            POC_FLUX_IN_AVG(istrR:iendR,jstrR:jendR,:) = 
     &             cff*( POC_FLUX_IN_AVG(istrR:iendR,jstrR:jendR,:)  
     &           + POC_FLUX_IN_hist(istrR:iendR,jstrR:jendR,:) )
            POC_PROD_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                cff*( POC_PROD_AVG(istrR:iendR,jstrR:jendR,:) 
     &             + POC_PROD_hist(istrR:iendR,jstrR:jendR,:) )
            POC_REMIN_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                   cff*( POC_REMIN_AVG(istrR:iendR,jstrR:jendR,:) 
     &                  + POC_REMIN_hist(istrR:iendR,jstrR:jendR,:) )
            CaCO3_FLUX_IN_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  cff*( CaCO3_FLUX_IN_AVG(istrR:iendR,jstrR:jendR,:) 
     &                 + CaCO3_FLUX_IN_hist(istrR:iendR,jstrR:jendR,:) )
            CaCO3_PROD_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                    cff*( CaCO3_PROD_AVG(istrR:iendR,jstrR:jendR,:) 
     &                 + CaCO3_PROD_hist(istrR:iendR,jstrR:jendR,:) )
            CaCO3_REMIN_AVG(istrR:iendR,jstrR:jendR,:) =
     &                cff*( CaCO3_REMIN _AVG(istrR:iendR,jstrR:jendR,:) +
     &                   CaCO3_REMIN_hist(istrR:iendR,jstrR:jendR,:) )
            SiO2_FLUX_IN_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                cff*( SiO2_FLUX_IN_AVG(istrR:iendR,jstrR:jendR,:) +
     &                SiO2_FLUX_IN_hist(istrR:iendR,jstrR:jendR,:) )
            SiO2_PROD_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                  cff*( SiO2_PROD_AVG(istrR:iendR,jstrR:jendR,:) 
     &                + SiO2_PROD_hist(istrR:iendR,jstrR:jendR,:) )
            SiO2_REMIN_AVG(istrR:iendR,jstrR:jendR,:) = 
     &               cff*( SiO2_REMIN_AVG(istrR:iendR,jstrR:jendR,:) 
     &              + SiO2_REMIN_hist(istrR:iendR,jstrR:jendR,:) )
            dust_FLUX_IN_AVG(istrR:iendR,jstrR:jendR,:) = 
     &               cff*( dust_FLUX_IN_AVG(istrR:iendR,jstrR:jendR,:) 
     &               + dust_FLUX_IN_hist(istrR:iendR,jstrR:jendR,:) )
            dust_REMIN_AVG(istrR:iendR,jstrR:jendR,:) = 
     &              cff*( dust_REMIN_AVG(istrR:iendR,jstrR:jendR,:) 
     &              +dust_REMIN_hist(istrR:iendR,jstrR:jendR,:) )
            P_iron_FLUX_IN_AVG(istrR:iendR,jstrR:jendR,:) =
     &           cff*( P_iron_FLUX_IN_AVG(istrR:iendR,jstrR:jendR,:) 
     &          +P_iron_FLUX_IN_hist(istrR:iendR,jstrR:jendR,:) )
            P_iron_PROD_AVG(istrR:iendR,jstrR:jendR,:) = 
     &            cff*( P_iron_PROD_AVG(istrR:iendR,jstrR:jendR,:) 
     &             + P_iron_PROD_hist(istrR:iendR,jstrR:jendR,:) )
            P_iron_REMIN_AVG(istrR:iendR,jstrR:jendR,:) = 
     &            cff*( P_iron_REMIN_AVG(istrR:iendR,jstrR:jendR,:) 
     &          + P_iron_REMIN_hist(istrR:iendR,jstrR:jendR,:) )
            graze_sp_AVG(istrR:iendR,jstrR:jendR,:) = 
     &            cff*( graze_sp_AVG(istrR:iendR,jstrR:jendR,:) 
     &           + graze_sp_hist(istrR:iendR,jstrR:jendR,:) )
            graze_diat_AVG(istrR:iendR,jstrR:jendR,:) =  
     &             cff*( graze_diat_AVG(istrR:iendR,jstrR:jendR,:) 
     &           + graze_diat_hist(istrR:iendR,jstrR:jendR,:) )
            graze_tot_AVG(istrR:iendR,jstrR:jendR,:) = 
     &            cff*( graze_tot_AVG(istrR:iendR,jstrR:jendR,:) 
     &              + graze_tot_hist(istrR:iendR,jstrR:jendR,:) )
            sp_loss_AVG(istrR:iendR,jstrR:jendR,:) = 
     &            cff*( sp_loss_AVG(istrR:iendR,jstrR:jendR,:) 
     &            +sp_loss_hist(istrR:iendR,jstrR:jendR,:) )
            diat_loss_AVG(istrR:iendR,jstrR:jendR,:) = 
     &             cff*( diat_loss_AVG(istrR:iendR,jstrR:jendR,:) 
     &            +diat_loss_hist(istrR:iendR,jstrR:jendR,:) )
            zoo_loss_AVG(istrR:iendR,jstrR:jendR,:) = 
     &              cff*( zoo_loss_AVG(istrR:iendR,jstrR:jendR,:) 
     &             + zoo_loss_hist(istrR:iendR,jstrR:jendR,:) )
            sp_agg_AVG(istrR:iendR,jstrR:jendR,:) = 
     &             cff*( sp_agg_AVG(istrR:iendR,jstrR:jendR,:) 
     &              +sp_agg_hist(istrR:iendR,jstrR:jendR,:) )
            diat_agg_AVG(istrR:iendR,jstrR:jendR,:) = 
     &              cff*( diat_agg_AVG(istrR:iendR,jstrR:jendR,:) 
     &              +diat_agg_hist(istrR:iendR,jstrR:jendR,:) )
            photoC_sp_AVG(istrR:iendR,jstrR:jendR,:) =  
     &             cff*( photoC_sp_AVG(istrR:iendR,jstrR:jendR,:) 
     &            + photoC_sp_hist(istrR:iendR,jstrR:jendR,:) )
            photoC_diat_AVG(istrR:iendR,jstrR:jendR,:) = 
     &             cff*( photoC_diat_AVG(istrR:iendR,jstrR:jendR,:) 
     &            + photoC_diat_hist(istrR:iendR,jstrR:jendR,:) )
            tot_prod_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                cff*( tot_prod_AVG(istrR:iendR,jstrR:jendR,:) 
     &                +tot_prod_hist(istrR:iendR,jstrR:jendR,:) )
            DOC_prod_AVG(istrR:iendR,jstrR:jendR,:) =  
     &                 cff*( DOC_prod_AVG(istrR:iendR,jstrR:jendR,:) 
     &              + DOC_prod_hist(istrR:iendR,jstrR:jendR,:) )
            DOC_remin_AVG(istrR:iendR,jstrR:jendR,:) = 
     &               cff*( DOC_remin_AVG(istrR:iendR,jstrR:jendR,:) 
     &                 +DOC_remin_hist(istrR:iendR,jstrR:jendR,:) )
            Fe_scavenge_AVG(istrR:iendR,jstrR:jendR,:) = 
     &               cff*( Fe_scavenge_AVG(istrR:iendR,jstrR:jendR,:) 
     &            + Fe_scavenge_hist(istrR:iendR,jstrR:jendR,:) )
            sp_N_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &              cff*( sp_N_lim_AVG(istrR:iendR,jstrR:jendR,:) 
     &             + sp_N_lim_hist(istrR:iendR,jstrR:jendR,:) )
            sp_Fe_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &             cff*( sp_Fe_lim_AVG(istrR:iendR,jstrR:jendR,:) 
     &            + sp_Fe_lim_hist(istrR:iendR,jstrR:jendR,:) )
            sp_PO4_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &               cff*( sp_PO4_lim_AVG(istrR:iendR,jstrR:jendR,:) 
     &             + sp_PO4_lim_hist(istrR:iendR,jstrR:jendR,:) )
            sp_light_lim_AVG(istrR:iendR,jstrR:jendR,:) =
     &              cff*( sp_light_lim_AVG(istrR:iendR,jstrR:jendR,:) 
     &            + sp_light_lim_hist(istrR:iendR,jstrR:jendR,:) )
            diat_N_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &                 cff*( diat_N_lim_AVG(istrR:iendR,jstrR:jendR,:) 
     &                +diat_N_lim_hist(istrR:iendR,jstrR:jendR,:) )
            diat_Fe_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &              cff*( diat_Fe_lim_AVG(istrR:iendR,jstrR:jendR,:) 
     &             +diat_Fe_lim_hist(istrR:iendR,jstrR:jendR,:) )
            diat_PO4_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &             cff*( diat_PO4_lim_AVG(istrR:iendR,jstrR:jendR,:) 
     &              +diat_PO4_lim_hist(istrR:iendR,jstrR:jendR,:) )
            diat_SiO3_lim_AVG(istrR:iendR,jstrR:jendR,:) =  
     &            cff*( diat_SiO3_lim_AVG(istrR:iendR,jstrR:jendR,:) 
     &            + diat_SiO3_lim_hist(istrR:iendR,jstrR:jendR,:) )
            diat_light_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &            cff*( diat_light_lim_AVG(istrR:iendR,jstrR:jendR,:) 
     &          + diat_light_lim_hist(istrR:iendR,jstrR:jendR,:) )
            CaCO3_form_AVG(istrR:iendR,jstrR:jendR,:) =
     &            cff*( CaCO3_form_AVG(istrR:iendR,jstrR:jendR,:) 
     &           +CaCO3_form_hist(istrR:iendR,jstrR:jendR,:) )
            diaz_Nfix_AVG(istrR:iendR,jstrR:jendR,:) = 
     &           cff*( diaz_Nfix_AVG(istrR:iendR,jstrR:jendR,:) 
     &           +diaz_Nfix_hist(istrR:iendR,jstrR:jendR,:) )
            graze_diaz_AVG(istrR:iendR,jstrR:jendR,:) = 
     &           cff*( graze_diaz_AVG(istrR:iendR,jstrR:jendR,:) 
     &         + graze_diaz_hist(istrR:iendR,jstrR:jendR,:) )
            diaz_loss_AVG(istrR:iendR,jstrR:jendR,:) = 
     &          cff*( diaz_loss_AVG(istrR:iendR,jstrR:jendR,:) 
     &        + diaz_loss_hist(istrR:iendR,jstrR:jendR,:) )
            photoC_diaz_AVG(istrR:iendR,jstrR:jendR,:) =
     &            cff*( photoC_diaz_AVG(istrR:iendR,jstrR:jendR,:) 
     &            + photoC_diaz_hist(istrR:iendR,jstrR:jendR,:) )
            diaz_P_lim_AVG(istrR:iendR,jstrR:jendR,:) =
     &           cff*( diaz_P_lim _AVG(istrR:iendR,jstrR:jendR,:) 
     &          + diaz_P_lim_hist(istrR:iendR,jstrR:jendR,:) )
            diaz_Fe_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &           cff*( diaz_Fe_lim_AVG(istrR:iendR,jstrR:jendR,:) 
     &          +diaz_Fe_lim _hist(istrR:iendR,jstrR:jendR,:) )
            diaz_light_lim_AVG(istrR:iendR,jstrR:jendR,:) = 
     &            cff*( diaz_light_lim_AVG(istrR:iendR,jstrR:jendR,:) 
     &            + diaz_light_lim_hist(istrR:iendR,jstrR:jendR,:) )
            Fe_scavenge_rate_AVG(istrR:iendR,jstrR:jendR,:) = 
     &              cff*( Fe_scavenge_rate_AVG(istrR:iendR,jstrR:jendR,:) 
     &            + Fe_scavenge_rate_hist(istrR:iendR,jstrR:jendR,:) )
            DON_prod_AVG(istrR:iendR,jstrR:jendR,:) = 
     &               cff*( DON_prod_AVG(istrR:iendR,jstrR:jendR,:) + 
     &               DON_prod_hist(istrR:iendR,jstrR:jendR,:) )
            DON_remin_AVG(istrR:iendR,jstrR:jendR,:) = 
     &               cff*( DON_remin_AVG(istrR:iendR,jstrR:jendR,:) 
     &               + DON_remin_hist(istrR:iendR,jstrR:jendR,:) )
            DOFe_prod_AVG(istrR:iendR,jstrR:jendR,:) = 
     &             cff*( DOFe_prod_AVG(istrR:iendR,jstrR:jendR,:)
     &              + DOFe_prod_hist(istrR:iendR,jstrR:jendR,:) )
            DOFe_remin_AVG(istrR:iendR,jstrR:jendR,:) = 
     &            cff*( DOFe_remin_AVG(istrR:iendR,jstrR:jendR,:) 
     &            + DOFe_remin_hist(istrR:iendR,jstrR:jendR,:) )
            DOP_prod_AVG(istrR:iendR,jstrR:jendR,:) =
     &          cff*( DOP_prod_AVG(istrR:iendR,jstrR:jendR,:) 
     &          + DOP_prod_hist(istrR:iendR,jstrR:jendR,:) )
            DOP_remin_AVG(istrR:iendR,jstrR:jendR,:) = 
     &          cff*( DOP_remin_AVG(istrR:iendR,jstrR:jendR,:) 
     &         + DOP_remin_hist(istrR:iendR,jstrR:jendR,:) )
            bSI_form_AVG(istrR:iendR,jstrR:jendR,:) = 
     &           cff*( bSI_form_AVG(istrR:iendR,jstrR:jendR,:) 
     &           + bSI_form_hist(istrR:iendR,jstrR:jendR,:) )
            photoFe_diaz_AVG(istrR:iendR,jstrR:jendR,:) = 
     &           cff*( photoFe_diaz_AVG(istrR:iendR,jstrR:jendR,:)
     &            + photoFe_diaz_hist(istrR:iendR,jstrR:jendR,:) )
            photoFe_diat_AVG(istrR:iendR,jstrR:jendR,:) = 
     &          cff*( photoFe_diat_AVG(istrR:iendR,jstrR:jendR,:) +  
     &          photoFe_diat_hist(istrR:iendR,jstrR:jendR,:) )
            photoFe_sp_AVG(istrR:iendR,jstrR:jendR,:) =  
     &          cff*( photoFe_sp_AVG(istrR:iendR,jstrR:jendR,:) 
     &          + photoFe_sp_hist(istrR:iendR,jstrR:jendR,:) )
            nitrif_AVG(istrR:iendR,jstrR:jendR,:) =  
     &          cff*( nitrif_AVG(istrR:iendR,jstrR:jendR,:) 
     &          + nitrif_hist(istrR:iendR,jstrR:jendR,:) )
         end if
         end if ! iic.gt.nts_bgc_flux_avg

         return
         end

#else
      subroutine set_bec_flux_avg_empty
      end
#endif /* BIOLOGY_BEC && BGC_FLUX_ANALYSIS */
