#include "cppdefs.h"
#if defined BIOLOGY_BEC && defined BGC_FLUX_ANALYSIS
           subroutine set_bec_flux_avg_tile (Istr,Iend,Jstr,Jend)
           implicit none
# include "param.h"
# include "ecosys_bec.h"
# include "bec_avg.h"
# include "ncroms_bgcflux.h"
# include "scalars.h"
# include "ocean2d.h"
           integer::istr,jstr,iend,jend

         time_bgc_flux_avg = time_bgc_flux_avg + time
! 2D variables
         zeta_bgc_flux_avg(ISTR:IEND,JSTR:JEND) = 
     &        zeta_bgc_flux_avg(ISTR:IEND,JSTR:JEND) +
     &        zeta(istr:iend,jstr:jend,nstp)
         WS_AVG(ISTR:IEND,JSTR:JEND) = WS_AVG(ISTR:IEND,JSTR:JEND) 
     &                       + WS_hist(istr:iend,jstr:jend)
         XKW_AVG(ISTR:IEND,JSTR:JEND) = XKW_AVG(ISTR:IEND,JSTR:JEND) 
     &                         + XKW_hist(istr:iend,jstr:jend)
         ap_AVG(ISTR:IEND,JSTR:JEND) = ap_AVG(ISTR:IEND,JSTR:JEND) 
     &                          + ap_hist(istr:iend,jstr:jend)
         SCHMIDT_O2_AVG(ISTR:IEND,JSTR:JEND) = 
     &                         SCHMIDT_O2_AVG(ISTR:IEND,JSTR:JEND) 
     &                       + SCHMIDT_O2_hist(istr:iend,jstr:jend)
         O2SAT_AVG(ISTR:IEND,JSTR:JEND)= 
     &                      O2SAT_AVG(ISTR:IEND,JSTR:JEND) 
     &                      +O2SAT_hist(istr:iend,jstr:jend)
         FG_O2_AVG(ISTR:IEND,JSTR:JEND) = 
     &                       FG_O2_AVG(ISTR:IEND,JSTR:JEND) 
     &                      +FG_O2_hist(istr:iend,jstr:jend)
# ifdef NOT_USED___NEEDS_RECODING_IF_USED
         PH_AVG(ISTR:IEND,JSTR:JEND) =  PH_AVG(ISTR:IEND,JSTR:JEND) 
     &                       + PH_hist(istr:iend,jstr:jend)
         pCO2SURF_AVG(ISTR:IEND,JSTR:JEND) = 
     &                 pCO2SURF_AVG(ISTR:IEND,JSTR:JEND) 
     &                 + pCO2SURF_hist(istr:iend,jstr:jend)
         DpCO2_AVG(ISTR:IEND,JSTR:JEND) = 
     &                    DpCO2_AVG(ISTR:IEND,JSTR:JEND) 
     &                  + DpCO2_hist(istr:iend,jstr:jend)
# endif /* NOT_USED___NEEDS_RECODING_IF_USED */
         SCHMIDT_CO2_AVG(ISTR:IEND,JSTR:JEND) = 
     &                 SCHMIDT_CO2_AVG(ISTR:IEND,JSTR:JEND) 
     &                + SCHMIDT_CO2_hist(istr:iend,jstr:jend)
         CO2STAR_AVG(ISTR:IEND,JSTR:JEND) = 
     &                     CO2STAR_AVG(ISTR:IEND,JSTR:JEND) 
     &                    + CO2STAR_hist(istr:iend,jstr:jend)
         DCO2STAR_AVG(ISTR:IEND,JSTR:JEND) = 
     &                  DCO2STAR_AVG(ISTR:IEND,JSTR:JEND) 
     &                  + DCO2STAR_hist(istr:iend,jstr:jend)
         FG_CO2_AVG(ISTR:IEND,JSTR:JEND) = 
     &                   FG_CO2_AVG(ISTR:IEND,JSTR:JEND) 
     &                 + FG_CO2_hist(istr:iend,jstr:jend)
         IRON_FLUX_AVG(ISTR:IEND,JSTR:JEND) = 
     &                 IRON_FLUX_AVG(ISTR:IEND,JSTR:JEND) 
     &               + IRON_FLUX_hist(istr:iend,jstr:jend)
         PARinc_flux_AVG(ISTR:IEND,JSTR:JEND) = 
     &                PARinc_flux_AVG(ISTR:IEND,JSTR:JEND) 
     &                + PARinc(istr:iend,jstr:jend)
! 3D variables
         PO4_RESTORE_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &                 PO4_RESTORE_AVG(ISTR:IEND,JSTR:JEND,:) 
     &                 + PO4_RESTORE_hist(istr:iend,jstr:jend,:)
         NO3_RESTORE_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &                NO3_RESTORE_AVG(ISTR:IEND,JSTR:JEND,:)  
     &               + NO3_RESTORE_hist(istr:iend,jstr:jend,:)
         SiO3_RESTORE_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &                SiO3_RESTORE_AVG(ISTR:IEND,JSTR:JEND,:) 
     &                + SiO3_RESTORE_hist(istr:iend,jstr:jend,:)
         PAR_flux_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &                PAR_flux_AVG(ISTR:IEND,JSTR:JEND,:) 
     &                + PAR(istr:iend,jstr:jend,:)
         PO4STAR_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &                PO4STAR_AVG(ISTR:IEND,JSTR:JEND,:) 
     &               + PO4STAR_hist(istr:iend,jstr:jend,:)
         POC_FLUX_IN_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &             POC_FLUX_IN_AVG(ISTR:IEND,JSTR:JEND,:)  
     &           + POC_FLUX_IN_hist(istr:iend,jstr:jend,:)
         POC_PROD_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &                POC_PROD_AVG(ISTR:IEND,JSTR:JEND,:) 
     &             + POC_PROD_hist(istr:iend,jstr:jend,:)
         POC_REMIN_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &                   POC_REMIN_AVG(ISTR:IEND,JSTR:JEND,:) 
     &                  + POC_REMIN_hist(istr:iend,jstr:jend,:)
         CaCO3_FLUX_IN_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &                  CaCO3_FLUX_IN_AVG(ISTR:IEND,JSTR:JEND,:) 
     &                 + CaCO3_FLUX_IN_hist(istr:iend,jstr:jend,:)
         CaCO3_PROD_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &                    CaCO3_PROD_AVG(ISTR:IEND,JSTR:JEND,:) 
     &                 + CaCO3_PROD_hist(istr:iend,jstr:jend,:)
         CaCO3_REMIN_AVG(ISTR:IEND,JSTR:JEND,:) =
     &                CaCO3_REMIN _AVG(ISTR:IEND,JSTR:JEND,:) +
     &                   CaCO3_REMIN_hist(istr:iend,jstr:jend,:)
         SiO2_FLUX_IN_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &                SiO2_FLUX_IN_AVG(ISTR:IEND,JSTR:JEND,:) +
     &                SiO2_FLUX_IN_hist(istr:iend,jstr:jend,:)
         SiO2_PROD_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &                  SiO2_PROD_AVG(ISTR:IEND,JSTR:JEND,:) 
     &                + SiO2_PROD_hist(istr:iend,jstr:jend,:)
         SiO2_REMIN_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &               SiO2_REMIN_AVG(ISTR:IEND,JSTR:JEND,:) 
     &              + SiO2_REMIN_hist(istr:iend,jstr:jend,:)
         dust_FLUX_IN_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &               dust_FLUX_IN_AVG(ISTR:IEND,JSTR:JEND,:) 
     &               + dust_FLUX_IN_hist(istr:iend,jstr:jend,:)
         dust_REMIN_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &              dust_REMIN_AVG(ISTR:IEND,JSTR:JEND,:) 
     &              +dust_REMIN_hist(istr:iend,jstr:jend,:)
         P_iron_FLUX_IN_AVG(ISTR:IEND,JSTR:JEND,:) =
     &           P_iron_FLUX_IN_AVG(ISTR:IEND,JSTR:JEND,:) 
     &          +P_iron_FLUX_IN_hist(istr:iend,jstr:jend,:)
         P_iron_PROD_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &            P_iron_PROD_AVG(ISTR:IEND,JSTR:JEND,:) 
     &             + P_iron_PROD_hist(istr:iend,jstr:jend,:)
         P_iron_REMIN_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &            P_iron_REMIN_AVG(ISTR:IEND,JSTR:JEND,:) 
     &          + P_iron_REMIN_hist(istr:iend,jstr:jend,:)
         graze_sp_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &            graze_sp_AVG(ISTR:IEND,JSTR:JEND,:) 
     &           + graze_sp_hist(istr:iend,jstr:jend,:)
         graze_diat_AVG(ISTR:IEND,JSTR:JEND,:) =  
     &             graze_diat_AVG(ISTR:IEND,JSTR:JEND,:) 
     &           + graze_diat_hist(istr:iend,jstr:jend,:)
         graze_tot_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &            graze_tot_AVG(ISTR:IEND,JSTR:JEND,:) 
     &              + graze_tot_hist(istr:iend,jstr:jend,:)
         sp_loss_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &            sp_loss_AVG(ISTR:IEND,JSTR:JEND,:) 
     &            +sp_loss_hist(istr:iend,jstr:jend,:)
         diat_loss_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &             diat_loss_AVG(ISTR:IEND,JSTR:JEND,:) 
     &            +diat_loss_hist(istr:iend,jstr:jend,:)
         zoo_loss_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &              zoo_loss_AVG(ISTR:IEND,JSTR:JEND,:) 
     &             + zoo_loss_hist(istr:iend,jstr:jend,:)
         sp_agg_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &             sp_agg_AVG(ISTR:IEND,JSTR:JEND,:) 
     &              +sp_agg_hist(istr:iend,jstr:jend,:)
         diat_agg_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &              diat_agg_AVG(ISTR:IEND,JSTR:JEND,:) 
     &              +diat_agg_hist(istr:iend,jstr:jend,:)
         photoC_sp_AVG(ISTR:IEND,JSTR:JEND,:) =  
     &             photoC_sp_AVG(ISTR:IEND,JSTR:JEND,:) 
     &            + photoC_sp_hist(istr:iend,jstr:jend,:)
         photoC_diat_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &             photoC_diat_AVG(ISTR:IEND,JSTR:JEND,:) 
     &            + photoC_diat_hist(istr:iend,jstr:jend,:)
         tot_prod_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &                tot_prod_AVG(ISTR:IEND,JSTR:JEND,:) 
     &                +tot_prod_hist(istr:iend,jstr:jend,:)
          DOC_prod_AVG(ISTR:IEND,JSTR:JEND,:) =  
     &                 DOC_prod_AVG(ISTR:IEND,JSTR:JEND,:) 
     &              + DOC_prod_hist(istr:iend,jstr:jend,:)
         DOC_remin_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &               DOC_remin_AVG(ISTR:IEND,JSTR:JEND,:) 
     &                 +DOC_remin_hist(istr:iend,jstr:jend,:)
         Fe_scavenge_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &               Fe_scavenge_AVG(ISTR:IEND,JSTR:JEND,:) 
     &            + Fe_scavenge_hist(istr:iend,jstr:jend,:)
         sp_N_lim_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &              sp_N_lim_AVG(ISTR:IEND,JSTR:JEND,:) 
     &             + sp_N_lim_hist(istr:iend,jstr:jend,:)
         sp_Fe_lim_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &             sp_Fe_lim_AVG(ISTR:IEND,JSTR:JEND,:) 
     &            + sp_Fe_lim_hist(istr:iend,jstr:jend,:)
         sp_PO4_lim_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &               sp_PO4_lim_AVG(ISTR:IEND,JSTR:JEND,:) 
     &             + sp_PO4_lim_hist(istr:iend,jstr:jend,:)
         sp_light_lim_AVG(ISTR:IEND,JSTR:JEND,:) =
     &              sp_light_lim_AVG(ISTR:IEND,JSTR:JEND,:) 
     &            + sp_light_lim_hist(istr:iend,jstr:jend,:)
         diat_N_lim_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &                 diat_N_lim_AVG(ISTR:IEND,JSTR:JEND,:) 
     &                +diat_N_lim_hist(istr:iend,jstr:jend,:)
         diat_Fe_lim_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &              diat_Fe_lim_AVG(ISTR:IEND,JSTR:JEND,:) 
     &             +diat_Fe_lim_hist(istr:iend,jstr:jend,:)
         diat_PO4_lim_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &             diat_PO4_lim_AVG(ISTR:IEND,JSTR:JEND,:) 
     &              +diat_PO4_lim_hist(istr:iend,jstr:jend,:)
         diat_SiO3_lim_AVG(ISTR:IEND,JSTR:JEND,:) =  
     &            diat_SiO3_lim_AVG(ISTR:IEND,JSTR:JEND,:) 
     &            + diat_SiO3_lim_hist(istr:iend,jstr:jend,:)
         diat_light_lim_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &            diat_light_lim_AVG(ISTR:IEND,JSTR:JEND,:) 
     &          + diat_light_lim_hist(istr:iend,jstr:jend,:)
         CaCO3_form_AVG(ISTR:IEND,JSTR:JEND,:) =
     &            CaCO3_form_AVG(ISTR:IEND,JSTR:JEND,:) 
     &           +CaCO3_form_hist(istr:iend,jstr:jend,:)
         diaz_Nfix_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &           diaz_Nfix_AVG(ISTR:IEND,JSTR:JEND,:) 
     &           +diaz_Nfix_hist(istr:iend,jstr:jend,:)
         graze_diaz_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &           graze_diaz_AVG(ISTR:IEND,JSTR:JEND,:) 
     &         + graze_diaz_hist(istr:iend,jstr:jend,:)
         diaz_loss_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &          diaz_loss_AVG(ISTR:IEND,JSTR:JEND,:) 
     &        + diaz_loss_hist(istr:iend,jstr:jend,:)
         photoC_diaz_AVG(ISTR:IEND,JSTR:JEND,:) =
     &            photoC_diaz_AVG(ISTR:IEND,JSTR:JEND,:) 
     &            + photoC_diaz_hist(istr:iend,jstr:jend,:)
         diaz_P_lim_AVG(ISTR:IEND,JSTR:JEND,:) =
     &           diaz_P_lim _AVG(ISTR:IEND,JSTR:JEND,:) 
     &          + diaz_P_lim_hist(istr:iend,jstr:jend,:)
         diaz_Fe_lim_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &           diaz_Fe_lim_AVG(ISTR:IEND,JSTR:JEND,:) 
     &          +diaz_Fe_lim _hist(istr:iend,jstr:jend,:)
         diaz_light_lim_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &            diaz_light_lim_AVG(ISTR:IEND,JSTR:JEND,:) 
     &            + diaz_light_lim_hist(istr:iend,jstr:jend,:)
         Fe_scavenge_rate_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &              Fe_scavenge_rate_AVG(ISTR:IEND,JSTR:JEND,:) 
     &            + Fe_scavenge_rate_hist(istr:iend,jstr:jend,:)
         DON_prod_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &               DON_prod_AVG(ISTR:IEND,JSTR:JEND,:) + 
     &               DON_prod_hist(istr:iend,jstr:jend,:)
         DON_remin_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &               DON_remin_AVG(ISTR:IEND,JSTR:JEND,:) 
     &               + DON_remin_hist(istr:iend,jstr:jend,:)
         DOFe_prod_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &             DOFe_prod_AVG(ISTR:IEND,JSTR:JEND,:)
     &              + DOFe_prod_hist(istr:iend,jstr:jend,:)
         DOFe_remin_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &            DOFe_remin_AVG(ISTR:IEND,JSTR:JEND,:) 
     &            + DOFe_remin_hist(istr:iend,jstr:jend,:)
         DOP_prod_AVG(ISTR:IEND,JSTR:JEND,:) =
     &          DOP_prod_AVG(ISTR:IEND,JSTR:JEND,:) 
     &          + DOP_prod_hist(istr:iend,jstr:jend,:)
         DOP_remin_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &          DOP_remin_AVG(ISTR:IEND,JSTR:JEND,:) 
     &         + DOP_remin_hist(istr:iend,jstr:jend,:)
         bSI_form_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &           bSI_form_AVG(ISTR:IEND,JSTR:JEND,:) 
     &           + bSI_form_hist(istr:iend,jstr:jend,:)
         photoFe_diaz_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &           photoFe_diaz_AVG(ISTR:IEND,JSTR:JEND,:)
     &            + photoFe_diaz_hist(istr:iend,jstr:jend,:)
         photoFe_diat_AVG(ISTR:IEND,JSTR:JEND,:) = 
     &          photoFe_diat_AVG(ISTR:IEND,JSTR:JEND,:) +  
     &          photoFe_diat_hist(istr:iend,jstr:jend,:)
         photoFe_sp_AVG(ISTR:IEND,JSTR:JEND,:) =  
     &        photoFe_sp_AVG(ISTR:IEND,JSTR:JEND,:) 
     &        + photoFe_sp_hist(istr:iend,jstr:jend,:)
         nitrif_AVG(ISTR:IEND,JSTR:JEND,:) =  
     &        nitrif_AVG(ISTR:IEND,JSTR:JEND,:) 
     &        + nitrif_hist(istr:iend,jstr:jend,:)
         return
         end

#else
      subroutine set_bec_flux_avg_empty
      end
#endif /* BIOLOGY_BEC && BGC_FLUX_ANALYSIS */

