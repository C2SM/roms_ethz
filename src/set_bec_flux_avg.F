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
         integer Istr,Jstr,Iend,Jend
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

            zeta_bgc_flux_avg(ISTRR:IENDR,JSTRR:JENDR) = 
     &                  zeta(Istrr:Iendr,Jstrr:Jendr,knew)
            WS_AVG(ISTRR:IENDR,JSTRR:JENDR) =
     &                  WS_hist(Istrr:Iendr,Jstrr:Jendr)
            XKW_AVG(ISTRR:IENDR,JSTRR:JENDR) =
     &                  XKW_hist(Istrr:Iendr,Jstrr:Jendr)
            ap_AVG(ISTRR:IENDR,JSTRR:JENDR) =
     &                  ap_hist(Istrr:Iendr,Jstrr:Jendr)
            SCHMIDT_O2_AVG(ISTRR:IENDR,JSTRR:JENDR) = 
     &                  SCHMIDT_O2_hist(Istrr:Iendr,Jstrr:Jendr)
            O2SAT_AVG(ISTRR:IENDR,JSTRR:JENDR)= 
     &                  O2SAT_hist(Istrr:Iendr,Jstrr:Jendr)
            FG_O2_AVG(ISTRR:IENDR,JSTRR:JENDR) = 
     &                  FG_O2_hist(Istrr:Iendr,Jstrr:Jendr)
            SCHMIDT_CO2_AVG(ISTRR:IENDR,JSTRR:JENDR) = 
     &                  SCHMIDT_CO2_hist(Istrr:Iendr,Jstrr:Jendr)
            CO2STAR_AVG(ISTRR:IENDR,JSTRR:JENDR) = 
     &                  CO2STAR_hist(Istrr:Iendr,Jstrr:Jendr)
            DCO2STAR_AVG(ISTRR:IENDR,JSTRR:JENDR) = 
     &                  DCO2STAR_hist(Istrr:Iendr,Jstrr:Jendr)
            FG_CO2_AVG(ISTRR:IENDR,JSTRR:JENDR) = 
     &                  FG_CO2_hist(Istrr:Iendr,Jstrr:Jendr)
            IRON_FLUX_AVG(ISTRR:IENDR,JSTRR:JENDR) = 
     &                  IRON_FLUX_hist(Istrr:Iendr,Jstrr:Jendr)
            PARinc_flux_AVG(ISTRR:IENDR,JSTRR:JENDR) = 
     &                  PARinc(Istrr:Iendr,Jstrr:Jendr)
! 3D variables
            PO4_RESTORE_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  PO4_RESTORE_hist(Istrr:Iendr,Jstrr:Jendr,:)
            NO3_RESTORE_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  NO3_RESTORE_hist(Istrr:Iendr,Jstrr:Jendr,:)
            SiO3_RESTORE_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  SiO3_RESTORE_hist(Istrr:Iendr,Jstrr:Jendr,:)
            PAR_flux_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  PAR(Istrr:Iendr,Jstrr:Jendr,:)
            PO4STAR_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  PO4STAR_hist(Istrr:Iendr,Jstrr:Jendr,:)
            POC_FLUX_IN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  POC_FLUX_IN_hist(Istrr:Iendr,Jstrr:Jendr,:)
            POC_PROD_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  POC_PROD_hist(Istrr:Iendr,Jstrr:Jendr,:)
            POC_REMIN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  POC_REMIN_hist(Istrr:Iendr,Jstrr:Jendr,:)
            CaCO3_FLUX_IN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  CaCO3_FLUX_IN_hist(Istrr:Iendr,Jstrr:Jendr,:)
            CaCO3_PROD_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  CaCO3_PROD_hist(Istrr:Iendr,Jstrr:Jendr,:)
            CaCO3_REMIN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =
     &                  CaCO3_REMIN_hist(Istrr:Iendr,Jstrr:Jendr,:)
            SiO2_FLUX_IN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  SiO2_FLUX_IN_hist(Istrr:Iendr,Jstrr:Jendr,:)
            SiO2_PROD_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  SiO2_PROD_hist(Istrr:Iendr,Jstrr:Jendr,:)
            SiO2_REMIN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  SiO2_REMIN_hist(Istrr:Iendr,Jstrr:Jendr,:)
            dust_FLUX_IN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  dust_FLUX_IN_hist(Istrr:Iendr,Jstrr:Jendr,:)
            dust_REMIN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  dust_REMIN_hist(Istrr:Iendr,Jstrr:Jendr,:)
            P_iron_FLUX_IN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =
     &                  P_iron_FLUX_IN_hist(Istrr:Iendr,Jstrr:Jendr,:)
            P_iron_PROD_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  P_iron_PROD_hist(Istrr:Iendr,Jstrr:Jendr,:)
            P_iron_REMIN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  P_iron_REMIN_hist(Istrr:Iendr,Jstrr:Jendr,:)
            graze_sp_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  graze_sp_hist(Istrr:Iendr,Jstrr:Jendr,:)
            graze_diat_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =  
     &                  graze_diat_hist(Istrr:Iendr,Jstrr:Jendr,:)
            graze_tot_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  graze_tot_hist(Istrr:Iendr,Jstrr:Jendr,:)
            sp_loss_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  sp_loss_hist(Istrr:Iendr,Jstrr:Jendr,:)
            diat_loss_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  diat_loss_hist(Istrr:Iendr,Jstrr:Jendr,:)
            zoo_loss_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  zoo_loss_hist(Istrr:Iendr,Jstrr:Jendr,:)
            sp_agg_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  sp_agg_hist(Istrr:Iendr,Jstrr:Jendr,:)
            diat_agg_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  diat_agg_hist(Istrr:Iendr,Jstrr:Jendr,:)
            photoC_sp_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =  
     &                  photoC_sp_hist(Istrr:Iendr,Jstrr:Jendr,:)
            photoC_diat_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  photoC_diat_hist(Istrr:Iendr,Jstrr:Jendr,:)
            tot_prod_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  tot_prod_hist(Istrr:Iendr,Jstrr:Jendr,:)
            DOC_prod_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =  
     &                  DOC_prod_hist(Istrr:Iendr,Jstrr:Jendr,:)
            DOC_remin_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  DOC_remin_hist(Istrr:Iendr,Jstrr:Jendr,:)
            Fe_scavenge_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  Fe_scavenge_hist(Istrr:Iendr,Jstrr:Jendr,:)
            sp_N_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  sp_N_lim_hist(Istrr:Iendr,Jstrr:Jendr,:)
            sp_Fe_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  sp_Fe_lim_hist(Istrr:Iendr,Jstrr:Jendr,:)
            sp_PO4_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  sp_PO4_lim_hist(Istrr:Iendr,Jstrr:Jendr,:)
            sp_light_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =
     &                  sp_light_lim_hist(Istrr:Iendr,Jstrr:Jendr,:)
            diat_N_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  diat_N_lim_hist(Istrr:Iendr,Jstrr:Jendr,:)
            diat_Fe_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  diat_Fe_lim_hist(Istrr:Iendr,Jstrr:Jendr,:)
            diat_PO4_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  diat_PO4_lim_hist(Istrr:Iendr,Jstrr:Jendr,:)
            diat_SiO3_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =  
     &                  diat_SiO3_lim_hist(Istrr:Iendr,Jstrr:Jendr,:)
            diat_light_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  diat_light_lim_hist(Istrr:Iendr,Jstrr:Jendr,:)
            CaCO3_form_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =
     &                  CaCO3_form_hist(Istrr:Iendr,Jstrr:Jendr,:)
            diaz_Nfix_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  diaz_Nfix_hist(Istrr:Iendr,Jstrr:Jendr,:)
            graze_diaz_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  graze_diaz_hist(Istrr:Iendr,Jstrr:Jendr,:)
            diaz_loss_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  diaz_loss_hist(Istrr:Iendr,Jstrr:Jendr,:)
            photoC_diaz_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =
     &                  photoC_diaz_hist(Istrr:Iendr,Jstrr:Jendr,:)
            diaz_P_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =
     &                  diaz_P_lim_hist(Istrr:Iendr,Jstrr:Jendr,:)
            diaz_Fe_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  diaz_Fe_lim _hist(Istrr:Iendr,Jstrr:Jendr,:)
            diaz_light_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  diaz_light_lim_hist(Istrr:Iendr,Jstrr:Jendr,:)
            Fe_scavenge_rate_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  Fe_scavenge_rate_hist(Istrr:Iendr,Jstrr:Jendr,:)
            DON_prod_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  DON_prod_hist(Istrr:Iendr,Jstrr:Jendr,:)
            DON_remin_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  DON_remin_hist(Istrr:Iendr,Jstrr:Jendr,:)
            DOFe_prod_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  DOFe_prod_hist(Istrr:Iendr,Jstrr:Jendr,:)
            DOFe_remin_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  DOFe_remin_hist(Istrr:Iendr,Jstrr:Jendr,:)
            DOP_prod_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =
     &                  DOP_prod_hist(Istrr:Iendr,Jstrr:Jendr,:)
            DOP_remin_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  DOP_remin_hist(Istrr:Iendr,Jstrr:Jendr,:)
            bSI_form_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  bSI_form_hist(Istrr:Iendr,Jstrr:Jendr,:)
            photoFe_diaz_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  photoFe_diaz_hist(Istrr:Iendr,Jstrr:Jendr,:)
            photoFe_diat_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  photoFe_diat_hist(Istrr:Iendr,Jstrr:Jendr,:)
            photoFe_sp_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =  
     &                  photoFe_sp_hist(Istrr:Iendr,Jstrr:Jendr,:)
            nitrif_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =  
     &                  nitrif_hist(Istrr:Iendr,Jstrr:Jendr,:)

         elseif (mod(iic-1,n_bgc_flux_avg).gt.1) then
            if (ZEROTH_TILE) time_bgc_flux_avg=time_bgc_flux_avg+time
! 2D variables
            zeta_bgc_flux_avg(ISTRR:IENDR,JSTRR:JENDR) = 
     &        zeta_bgc_flux_avg(ISTRR:IENDR,JSTRR:JENDR) +
     &        zeta(Istrr:Iendr,Jstrr:Jendr,knew)
            WS_AVG(ISTRR:IENDR,JSTRR:JENDR) = WS_AVG(ISTRR:IENDR,JSTRR:JENDR) 
     &                       + WS_hist(Istrr:Iendr,Jstrr:Jendr)
            XKW_AVG(ISTRR:IENDR,JSTRR:JENDR) = XKW_AVG(ISTRR:IENDR,JSTRR:JENDR) 
     &                         + XKW_hist(Istrr:Iendr,Jstrr:Jendr)
            ap_AVG(ISTRR:IENDR,JSTRR:JENDR) = ap_AVG(ISTRR:IENDR,JSTRR:JENDR) 
     &                          + ap_hist(Istrr:Iendr,Jstrr:Jendr)
            SCHMIDT_O2_AVG(ISTRR:IENDR,JSTRR:JENDR) = 
     &                         SCHMIDT_O2_AVG(ISTRR:IENDR,JSTRR:JENDR) 
     &                       + SCHMIDT_O2_hist(Istrr:Iendr,Jstrr:Jendr)
            O2SAT_AVG(ISTRR:IENDR,JSTRR:JENDR)= 
     &                      O2SAT_AVG(ISTRR:IENDR,JSTRR:JENDR) 
     &                      +O2SAT_hist(Istrr:Iendr,Jstrr:Jendr)
            FG_O2_AVG(ISTRR:IENDR,JSTRR:JENDR) = 
     &                       FG_O2_AVG(ISTRR:IENDR,JSTRR:JENDR) 
     &                      +FG_O2_hist(Istrr:Iendr,Jstrr:Jendr)
            SCHMIDT_CO2_AVG(ISTRR:IENDR,JSTRR:JENDR) = 
     &                 SCHMIDT_CO2_AVG(ISTRR:IENDR,JSTRR:JENDR) 
     &                + SCHMIDT_CO2_hist(Istrr:Iendr,Jstrr:Jendr)
            CO2STAR_AVG(ISTRR:IENDR,JSTRR:JENDR) = 
     &                     CO2STAR_AVG(ISTRR:IENDR,JSTRR:JENDR) 
     &                    + CO2STAR_hist(Istrr:Iendr,Jstrr:Jendr)
            DCO2STAR_AVG(ISTRR:IENDR,JSTRR:JENDR) = 
     &                  DCO2STAR_AVG(ISTRR:IENDR,JSTRR:JENDR) 
     &                  + DCO2STAR_hist(Istrr:Iendr,Jstrr:Jendr)
            FG_CO2_AVG(ISTRR:IENDR,JSTRR:JENDR) = 
     &                   FG_CO2_AVG(ISTRR:IENDR,JSTRR:JENDR) 
     &                 + FG_CO2_hist(Istrr:Iendr,Jstrr:Jendr)
            IRON_FLUX_AVG(ISTRR:IENDR,JSTRR:JENDR) = 
     &                 IRON_FLUX_AVG(ISTRR:IENDR,JSTRR:JENDR) 
     &               + IRON_FLUX_hist(Istrr:Iendr,Jstrr:Jendr)
            PARinc_flux_AVG(ISTRR:IENDR,JSTRR:JENDR) = 
     &                PARinc_flux_AVG(ISTRR:IENDR,JSTRR:JENDR) 
     &                + PARinc(Istrr:Iendr,Jstrr:Jendr)
! 3D variables
            PO4_RESTORE_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                 PO4_RESTORE_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &                 + PO4_RESTORE_hist(Istrr:Iendr,Jstrr:Jendr,:)
            NO3_RESTORE_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                NO3_RESTORE_AVG(ISTRR:IENDR,JSTRR:JENDR,:)  
     &               + NO3_RESTORE_hist(Istrr:Iendr,Jstrr:Jendr,:)
            SiO3_RESTORE_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                SiO3_RESTORE_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &                + SiO3_RESTORE_hist(Istrr:Iendr,Jstrr:Jendr,:)
            PAR_flux_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                PAR_flux_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &                + PAR(Istrr:Iendr,Jstrr:Jendr,:)
            PO4STAR_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                PO4STAR_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &               + PO4STAR_hist(Istrr:Iendr,Jstrr:Jendr,:)
            POC_FLUX_IN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &             POC_FLUX_IN_AVG(ISTRR:IENDR,JSTRR:JENDR,:)  
     &           + POC_FLUX_IN_hist(Istrr:Iendr,Jstrr:Jendr,:)
            POC_PROD_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                POC_PROD_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &             + POC_PROD_hist(Istrr:Iendr,Jstrr:Jendr,:)
            POC_REMIN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                   POC_REMIN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &                  + POC_REMIN_hist(Istrr:Iendr,Jstrr:Jendr,:)
            CaCO3_FLUX_IN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  CaCO3_FLUX_IN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &                 + CaCO3_FLUX_IN_hist(Istrr:Iendr,Jstrr:Jendr,:)
            CaCO3_PROD_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                    CaCO3_PROD_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &                 + CaCO3_PROD_hist(Istrr:Iendr,Jstrr:Jendr,:)
            CaCO3_REMIN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =
     &                CaCO3_REMIN _AVG(ISTRR:IENDR,JSTRR:JENDR,:) +
     &                   CaCO3_REMIN_hist(Istrr:Iendr,Jstrr:Jendr,:)
            SiO2_FLUX_IN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                SiO2_FLUX_IN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) +
     &                SiO2_FLUX_IN_hist(Istrr:Iendr,Jstrr:Jendr,:)
            SiO2_PROD_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  SiO2_PROD_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &                + SiO2_PROD_hist(Istrr:Iendr,Jstrr:Jendr,:)
            SiO2_REMIN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &               SiO2_REMIN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &              + SiO2_REMIN_hist(Istrr:Iendr,Jstrr:Jendr,:)
            dust_FLUX_IN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &               dust_FLUX_IN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &               + dust_FLUX_IN_hist(Istrr:Iendr,Jstrr:Jendr,:)
            dust_REMIN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &              dust_REMIN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &              +dust_REMIN_hist(Istrr:Iendr,Jstrr:Jendr,:)
            P_iron_FLUX_IN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =
     &           P_iron_FLUX_IN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &          +P_iron_FLUX_IN_hist(Istrr:Iendr,Jstrr:Jendr,:)
            P_iron_PROD_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &            P_iron_PROD_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &             + P_iron_PROD_hist(Istrr:Iendr,Jstrr:Jendr,:)
            P_iron_REMIN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &            P_iron_REMIN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &          + P_iron_REMIN_hist(Istrr:Iendr,Jstrr:Jendr,:)
            graze_sp_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &            graze_sp_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &           + graze_sp_hist(Istrr:Iendr,Jstrr:Jendr,:)
            graze_diat_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =  
     &             graze_diat_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &           + graze_diat_hist(Istrr:Iendr,Jstrr:Jendr,:)
            graze_tot_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &            graze_tot_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &              + graze_tot_hist(Istrr:Iendr,Jstrr:Jendr,:)
            sp_loss_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &            sp_loss_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &            +sp_loss_hist(Istrr:Iendr,Jstrr:Jendr,:)
            diat_loss_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &             diat_loss_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &            +diat_loss_hist(Istrr:Iendr,Jstrr:Jendr,:)
            zoo_loss_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &              zoo_loss_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &             + zoo_loss_hist(Istrr:Iendr,Jstrr:Jendr,:)
            sp_agg_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &             sp_agg_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &              +sp_agg_hist(Istrr:Iendr,Jstrr:Jendr,:)
            diat_agg_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &              diat_agg_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &              +diat_agg_hist(Istrr:Iendr,Jstrr:Jendr,:)
            photoC_sp_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =  
     &             photoC_sp_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &            + photoC_sp_hist(Istrr:Iendr,Jstrr:Jendr,:)
            photoC_diat_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &             photoC_diat_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &            + photoC_diat_hist(Istrr:Iendr,Jstrr:Jendr,:)
            tot_prod_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                tot_prod_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &                +tot_prod_hist(Istrr:Iendr,Jstrr:Jendr,:)
            DOC_prod_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =  
     &                 DOC_prod_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &              + DOC_prod_hist(Istrr:Iendr,Jstrr:Jendr,:)
            DOC_remin_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &               DOC_remin_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &                 +DOC_remin_hist(Istrr:Iendr,Jstrr:Jendr,:)
            Fe_scavenge_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &               Fe_scavenge_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &            + Fe_scavenge_hist(Istrr:Iendr,Jstrr:Jendr,:)
            sp_N_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &              sp_N_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &             + sp_N_lim_hist(Istrr:Iendr,Jstrr:Jendr,:)
            sp_Fe_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &             sp_Fe_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &            + sp_Fe_lim_hist(Istrr:Iendr,Jstrr:Jendr,:)
            sp_PO4_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &               sp_PO4_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &             + sp_PO4_lim_hist(Istrr:Iendr,Jstrr:Jendr,:)
            sp_light_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =
     &              sp_light_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &            + sp_light_lim_hist(Istrr:Iendr,Jstrr:Jendr,:)
            diat_N_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                 diat_N_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &                +diat_N_lim_hist(Istrr:Iendr,Jstrr:Jendr,:)
            diat_Fe_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &              diat_Fe_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &             +diat_Fe_lim_hist(Istrr:Iendr,Jstrr:Jendr,:)
            diat_PO4_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &             diat_PO4_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &              +diat_PO4_lim_hist(Istrr:Iendr,Jstrr:Jendr,:)
            diat_SiO3_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =  
     &            diat_SiO3_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &            + diat_SiO3_lim_hist(Istrr:Iendr,Jstrr:Jendr,:)
            diat_light_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &            diat_light_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &          + diat_light_lim_hist(Istrr:Iendr,Jstrr:Jendr,:)
            CaCO3_form_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =
     &            CaCO3_form_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &           +CaCO3_form_hist(Istrr:Iendr,Jstrr:Jendr,:)
            diaz_Nfix_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &           diaz_Nfix_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &           +diaz_Nfix_hist(Istrr:Iendr,Jstrr:Jendr,:)
            graze_diaz_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &           graze_diaz_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &         + graze_diaz_hist(Istrr:Iendr,Jstrr:Jendr,:)
            diaz_loss_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &          diaz_loss_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &        + diaz_loss_hist(Istrr:Iendr,Jstrr:Jendr,:)
            photoC_diaz_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =
     &            photoC_diaz_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &            + photoC_diaz_hist(Istrr:Iendr,Jstrr:Jendr,:)
            diaz_P_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =
     &           diaz_P_lim _AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &          + diaz_P_lim_hist(Istrr:Iendr,Jstrr:Jendr,:)
            diaz_Fe_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &           diaz_Fe_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &          +diaz_Fe_lim _hist(Istrr:Iendr,Jstrr:Jendr,:)
            diaz_light_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &            diaz_light_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &            + diaz_light_lim_hist(Istrr:Iendr,Jstrr:Jendr,:)
            Fe_scavenge_rate_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &              Fe_scavenge_rate_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &            + Fe_scavenge_rate_hist(Istrr:Iendr,Jstrr:Jendr,:)
            DON_prod_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &               DON_prod_AVG(ISTRR:IENDR,JSTRR:JENDR,:) + 
     &               DON_prod_hist(Istrr:Iendr,Jstrr:Jendr,:)
            DON_remin_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &               DON_remin_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &               + DON_remin_hist(Istrr:Iendr,Jstrr:Jendr,:)
            DOFe_prod_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &             DOFe_prod_AVG(ISTRR:IENDR,JSTRR:JENDR,:)
     &              + DOFe_prod_hist(Istrr:Iendr,Jstrr:Jendr,:)
            DOFe_remin_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &            DOFe_remin_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &            + DOFe_remin_hist(Istrr:Iendr,Jstrr:Jendr,:)
            DOP_prod_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =
     &          DOP_prod_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &          + DOP_prod_hist(Istrr:Iendr,Jstrr:Jendr,:)
            DOP_remin_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &          DOP_remin_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &         + DOP_remin_hist(Istrr:Iendr,Jstrr:Jendr,:)
            bSI_form_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &           bSI_form_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &           + bSI_form_hist(Istrr:Iendr,Jstrr:Jendr,:)
            photoFe_diaz_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &           photoFe_diaz_AVG(ISTRR:IENDR,JSTRR:JENDR,:)
     &            + photoFe_diaz_hist(Istrr:Iendr,Jstrr:Jendr,:)
            photoFe_diat_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &          photoFe_diat_AVG(ISTRR:IENDR,JSTRR:JENDR,:) +  
     &          photoFe_diat_hist(Istrr:Iendr,Jstrr:Jendr,:)
            photoFe_sp_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =  
     &          photoFe_sp_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &          + photoFe_sp_hist(Istrr:Iendr,Jstrr:Jendr,:)
            nitrif_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =  
     &          nitrif_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &          + nitrif_hist(Istrr:Iendr,Jstrr:Jendr,:)

         elseif (mod(iic-1,n_bgc_flux_avg).eq.0) then
            cff=1./float(n_bgc_flux_avg)
            if (ZEROTH_TILE) then
               time_bgc_flux_avg=cff*
     &           (time_bgc_flux_avg + time)
            endif
               
! 2D variables
            zeta_bgc_flux_avg(ISTRR:IENDR,JSTRR:JENDR) = 
     &        cff*( zeta_bgc_flux_avg(ISTRR:IENDR,JSTRR:JENDR) +
     &        zeta(Istrr:Iendr,Jstrr:Jendr,knew) )
            WS_AVG(ISTRR:IENDR,JSTRR:JENDR) = cff*( WS_AVG(ISTRR:IENDR,JSTRR:JENDR) 
     &                       + WS_hist(Istrr:Iendr,Jstrr:Jendr) )
            XKW_AVG(ISTRR:IENDR,JSTRR:JENDR) = cff*( XKW_AVG(ISTRR:IENDR,JSTRR:JENDR) 
     &                         + XKW_hist(Istrr:Iendr,Jstrr:Jendr) )
            ap_AVG(ISTRR:IENDR,JSTRR:JENDR) = cff*( ap_AVG(ISTRR:IENDR,JSTRR:JENDR) 
     &                          + ap_hist(Istrr:Iendr,Jstrr:Jendr) )
            SCHMIDT_O2_AVG(ISTRR:IENDR,JSTRR:JENDR) = 
     &                         cff*( SCHMIDT_O2_AVG(ISTRR:IENDR,JSTRR:JENDR) 
     &                       + SCHMIDT_O2_hist(Istrr:Iendr,Jstrr:Jendr) )
            O2SAT_AVG(ISTRR:IENDR,JSTRR:JENDR)= 
     &                      cff*( O2SAT_AVG(ISTRR:IENDR,JSTRR:JENDR) 
     &                      +O2SAT_hist(Istrr:Iendr,Jstrr:Jendr) )
            FG_O2_AVG(ISTRR:IENDR,JSTRR:JENDR) = 
     &                       cff*( FG_O2_AVG(ISTRR:IENDR,JSTRR:JENDR) 
     &                      +FG_O2_hist(Istrr:Iendr,Jstrr:Jendr) )
            SCHMIDT_CO2_AVG(ISTRR:IENDR,JSTRR:JENDR) = 
     &                 cff*( SCHMIDT_CO2_AVG(ISTRR:IENDR,JSTRR:JENDR) 
     &                + SCHMIDT_CO2_hist(Istrr:Iendr,Jstrr:Jendr) )
            CO2STAR_AVG(ISTRR:IENDR,JSTRR:JENDR) = 
     &                     cff*( CO2STAR_AVG(ISTRR:IENDR,JSTRR:JENDR) 
     &                    + CO2STAR_hist(Istrr:Iendr,Jstrr:Jendr) )
            DCO2STAR_AVG(ISTRR:IENDR,JSTRR:JENDR) = 
     &                  cff*( DCO2STAR_AVG(ISTRR:IENDR,JSTRR:JENDR) 
     &                  + DCO2STAR_hist(Istrr:Iendr,Jstrr:Jendr) )
            FG_CO2_AVG(ISTRR:IENDR,JSTRR:JENDR) = 
     &                   cff*( FG_CO2_AVG(ISTRR:IENDR,JSTRR:JENDR) 
     &                 + FG_CO2_hist(Istrr:Iendr,Jstrr:Jendr) )
            IRON_FLUX_AVG(ISTRR:IENDR,JSTRR:JENDR) = 
     &                 cff*( IRON_FLUX_AVG(ISTRR:IENDR,JSTRR:JENDR) 
     &               + IRON_FLUX_hist(Istrr:Iendr,Jstrr:Jendr) )
            PARinc_flux_AVG(ISTRR:IENDR,JSTRR:JENDR) = 
     &                cff*( PARinc_flux_AVG(ISTRR:IENDR,JSTRR:JENDR) 
     &                + PARinc(Istrr:Iendr,Jstrr:Jendr) )
! 3D variables
            PO4_RESTORE_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                 cff*( PO4_RESTORE_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &                 + PO4_RESTORE_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            NO3_RESTORE_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                cff*( NO3_RESTORE_AVG(ISTRR:IENDR,JSTRR:JENDR,:)  
     &               + NO3_RESTORE_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            SiO3_RESTORE_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                cff*( SiO3_RESTORE_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &                + SiO3_RESTORE_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            PAR_flux_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                cff*( PAR_flux_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &                + PAR(Istrr:Iendr,Jstrr:Jendr,:) )
            PO4STAR_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                cff*( PO4STAR_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &               + PO4STAR_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            POC_FLUX_IN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &             cff*( POC_FLUX_IN_AVG(ISTRR:IENDR,JSTRR:JENDR,:)  
     &           + POC_FLUX_IN_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            POC_PROD_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                cff*( POC_PROD_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &             + POC_PROD_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            POC_REMIN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                   cff*( POC_REMIN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &                  + POC_REMIN_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            CaCO3_FLUX_IN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  cff*( CaCO3_FLUX_IN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &                 + CaCO3_FLUX_IN_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            CaCO3_PROD_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                    cff*( CaCO3_PROD_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &                 + CaCO3_PROD_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            CaCO3_REMIN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =
     &                cff*( CaCO3_REMIN _AVG(ISTRR:IENDR,JSTRR:JENDR,:) +
     &                   CaCO3_REMIN_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            SiO2_FLUX_IN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                cff*( SiO2_FLUX_IN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) +
     &                SiO2_FLUX_IN_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            SiO2_PROD_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                  cff*( SiO2_PROD_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &                + SiO2_PROD_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            SiO2_REMIN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &               cff*( SiO2_REMIN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &              + SiO2_REMIN_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            dust_FLUX_IN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &               cff*( dust_FLUX_IN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &               + dust_FLUX_IN_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            dust_REMIN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &              cff*( dust_REMIN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &              +dust_REMIN_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            P_iron_FLUX_IN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =
     &           cff*( P_iron_FLUX_IN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &          +P_iron_FLUX_IN_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            P_iron_PROD_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &            cff*( P_iron_PROD_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &             + P_iron_PROD_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            P_iron_REMIN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &            cff*( P_iron_REMIN_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &          + P_iron_REMIN_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            graze_sp_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &            cff*( graze_sp_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &           + graze_sp_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            graze_diat_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =  
     &             cff*( graze_diat_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &           + graze_diat_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            graze_tot_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &            cff*( graze_tot_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &              + graze_tot_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            sp_loss_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &            cff*( sp_loss_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &            +sp_loss_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            diat_loss_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &             cff*( diat_loss_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &            +diat_loss_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            zoo_loss_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &              cff*( zoo_loss_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &             + zoo_loss_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            sp_agg_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &             cff*( sp_agg_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &              +sp_agg_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            diat_agg_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &              cff*( diat_agg_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &              +diat_agg_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            photoC_sp_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =  
     &             cff*( photoC_sp_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &            + photoC_sp_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            photoC_diat_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &             cff*( photoC_diat_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &            + photoC_diat_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            tot_prod_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                cff*( tot_prod_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &                +tot_prod_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            DOC_prod_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =  
     &                 cff*( DOC_prod_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &              + DOC_prod_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            DOC_remin_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &               cff*( DOC_remin_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &                 +DOC_remin_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            Fe_scavenge_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &               cff*( Fe_scavenge_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &            + Fe_scavenge_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            sp_N_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &              cff*( sp_N_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &             + sp_N_lim_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            sp_Fe_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &             cff*( sp_Fe_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &            + sp_Fe_lim_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            sp_PO4_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &               cff*( sp_PO4_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &             + sp_PO4_lim_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            sp_light_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =
     &              cff*( sp_light_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &            + sp_light_lim_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            diat_N_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &                 cff*( diat_N_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &                +diat_N_lim_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            diat_Fe_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &              cff*( diat_Fe_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &             +diat_Fe_lim_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            diat_PO4_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &             cff*( diat_PO4_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &              +diat_PO4_lim_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            diat_SiO3_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =  
     &            cff*( diat_SiO3_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &            + diat_SiO3_lim_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            diat_light_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &            cff*( diat_light_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &          + diat_light_lim_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            CaCO3_form_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =
     &            cff*( CaCO3_form_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &           +CaCO3_form_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            diaz_Nfix_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &           cff*( diaz_Nfix_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &           +diaz_Nfix_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            graze_diaz_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &           cff*( graze_diaz_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &         + graze_diaz_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            diaz_loss_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &          cff*( diaz_loss_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &        + diaz_loss_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            photoC_diaz_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =
     &            cff*( photoC_diaz_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &            + photoC_diaz_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            diaz_P_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =
     &           cff*( diaz_P_lim _AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &          + diaz_P_lim_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            diaz_Fe_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &           cff*( diaz_Fe_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &          +diaz_Fe_lim _hist(Istrr:Iendr,Jstrr:Jendr,:) )
            diaz_light_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &            cff*( diaz_light_lim_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &            + diaz_light_lim_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            Fe_scavenge_rate_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &              cff*( Fe_scavenge_rate_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &            + Fe_scavenge_rate_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            DON_prod_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &               cff*( DON_prod_AVG(ISTRR:IENDR,JSTRR:JENDR,:) + 
     &               DON_prod_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            DON_remin_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &               cff*( DON_remin_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &               + DON_remin_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            DOFe_prod_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &             cff*( DOFe_prod_AVG(ISTRR:IENDR,JSTRR:JENDR,:)
     &              + DOFe_prod_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            DOFe_remin_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &            cff*( DOFe_remin_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &            + DOFe_remin_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            DOP_prod_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =
     &          cff*( DOP_prod_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &          + DOP_prod_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            DOP_remin_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &          cff*( DOP_remin_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &         + DOP_remin_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            bSI_form_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &           cff*( bSI_form_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &           + bSI_form_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            photoFe_diaz_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &           cff*( photoFe_diaz_AVG(ISTRR:IENDR,JSTRR:JENDR,:)
     &            + photoFe_diaz_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            photoFe_diat_AVG(ISTRR:IENDR,JSTRR:JENDR,:) = 
     &          cff*( photoFe_diat_AVG(ISTRR:IENDR,JSTRR:JENDR,:) +  
     &          photoFe_diat_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            photoFe_sp_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =  
     &          cff*( photoFe_sp_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &          + photoFe_sp_hist(Istrr:Iendr,Jstrr:Jendr,:) )
            nitrif_AVG(ISTRR:IENDR,JSTRR:JENDR,:) =  
     &          cff*( nitrif_AVG(ISTRR:IENDR,JSTRR:JENDR,:) 
     &          + nitrif_hist(Istrr:Iendr,Jstrr:Jendr,:) )
         end if
         end if ! iic.gt.nts_bgc_flux_avg

         return
         end

#else
      subroutine set_bec_flux_avg_empty
      end
#endif /* BIOLOGY_BEC && BGC_FLUX_ANALYSIS */
