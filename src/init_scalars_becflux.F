#include "cppdefs.h"
#if defined BIOLOGY_BEC && defined BGC_FLUX_ANALYSIS
       subroutine init_scalars_becflux()
       implicit none

#include "param.h"
#include "ecosys_bec.h"
#include "ncroms_bgcflux.h"

       integer::ind

!
! Initialize netCDF files IDs to closed status.
!
       ncid_bgc_flux_his=-1
!DL       nrec_bgc_flux_his=0
#ifdef AVERAGES
       ncid_bgc_flux_avg=-1
!DL       nrec_bgc_flux_avg=0
#endif
       ind=1
       vname_bgcflux(1,ind)='WS'
       vname_bgcflux(2,ind)='Wind speed'
       vname_bgcflux(3,ind)='m/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='XKW'
       vname_bgcflux(2,ind)='XKW_AVG'
       vname_bgcflux(3,ind)='m/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='ATM_PRESS'
       vname_bgcflux(2,ind)='Atmospheric pressure'
       vname_bgcflux(3,ind)='atm '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='SCHMIDT_O2'
       vname_bgcflux(2,ind)='Schmidt number for O2'
       vname_bgcflux(3,ind)=' '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='O2SAT'
       vname_bgcflux(2,ind)='O2SAT_AVG'
       vname_bgcflux(3,ind)='mmol/m3 '
       vname_bgcflux(4,ind)=' '


       ind=ind+1
       vname_bgcflux(1,ind)='FG_O2'
       vname_bgcflux(2,ind)='Air-sea flux of O2'
       vname_bgcflux(3,ind)='mmol/m2/s'
       vname_bgcflux(4,ind)=' '


# ifdef NOT_USED___NEEDS_RECODING_IF_USED
       ind=ind+1
       vname_bgcflux(1,ind)='pH'
       vname_bgcflux(2,ind)='pH value'
       vname_bgcflux(3,ind)=' '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='pCO2'
       vname_bgcflux(2,ind)='Surface water pCO2'
       vname_bgcflux(3,ind)='ppmv '
       vname_bgcflux(4,ind)=' '
 
        ind=ind+1
       vname_bgcflux(1,ind)='DpCO2'
       vname_bgcflux(2,ind)='DPCO2'
       vname_bgcflux(3,ind)='ppmv '
       vname_bgcflux(4,ind)=' '
# endif /* NOT_USED___NEEDS_RECODING_IF_USED */

       ind=ind+1
       vname_bgcflux(1,ind)='CO2STAR'
       vname_bgcflux(2,ind)='CO2STAR_AVG'
       vname_bgcflux(3,ind)='mmol/m3 '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='DCO2STAR'
       vname_bgcflux(2,ind)='DCO2STAR_AVG'
       vname_bgcflux(3,ind)='mmol/m3'
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='SCHMIDT_CO2'
       vname_bgcflux(2,ind)='Schmidt number for CO2'
       vname_bgcflux(3,ind)=' '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='FG_CO2'
       vname_bgcflux(2,ind)='Air-sea flux of CO2'
       vname_bgcflux(3,ind)='mmol/m2/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='IRON_FLUX'
       vname_bgcflux(2,ind)='Iron surface flux'
       vname_bgcflux(3,ind)='mmol/m2/s'
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='PARinc'
       vname_bgcflux(2,ind)=
     &      'Inc. Photosynthetically available radiation'
       vname_bgcflux(3,ind)='W/m2'
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='PO4_RESTORE'
       vname_bgcflux(2,ind)='PO4 restoring flux'
       vname_bgcflux(3,ind)=' '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='NO3_RESTORE'
       vname_bgcflux(2,ind)='NO3 restoring flux'
       vname_bgcflux(3,ind)=' '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='SIO3_RESTORE'
       vname_bgcflux(2,ind)='SiO3 restoring flux'
       vname_bgcflux(3,ind)=' '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='PAR'
       vname_bgcflux(2,ind)='Photosynthetically available radiation'
       vname_bgcflux(3,ind)='W/m2'
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='PO4STAR'
       vname_bgcflux(2,ind)='PO4STAR_AVG'
       vname_bgcflux(3,ind)=' '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='POC_FLUX_IN'
       vname_bgcflux(2,ind)='POC_FLUX_IN_AVG'
       vname_bgcflux(3,ind)='mmol/m2/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='POC_PROD'
       vname_bgcflux(2,ind)='POC_PROD_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='POC_REMIN'
       vname_bgcflux(2,ind)='POC_REMIN_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s'
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='CACO3_FLUX_IN'
       vname_bgcflux(2,ind)='CACO3_FLUX_IN_AVG'
       vname_bgcflux(3,ind)='mmol/m2/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='CACO3_PROD'
       vname_bgcflux(2,ind)='CACO3_PRO_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='CACO3_REMIN'
       vname_bgcflux(2,ind)='CACO3_REMIN_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='SIO2_FLUX_IN'
       vname_bgcflux(2,ind)='SIO2_FLUX_IN_AVG'
       vname_bgcflux(3,ind)='mmol/m2/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='SIO2_PROD'
       vname_bgcflux(2,ind)='SIO2_PROD_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='SIO2_REMIN'
       vname_bgcflux(2,ind)='SIO2_REMIN_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s'
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='DUST_FLUX_IN'
       vname_bgcflux(2,ind)='DUST_FLUX_IN_AVG'
       vname_bgcflux(3,ind)='mmol/m2/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='DUST_REMIN'
       vname_bgcflux(2,ind)='DUST_REMIN_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='P_IRON_FLUX_IN'
       vname_bgcflux(2,ind)='P_IRON_FLUX_IN_AVG'
       vname_bgcflux(3,ind)='mmol/m2/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='P_IRON_PROD'
       vname_bgcflux(2,ind)='P_IRON_PROD_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='P_IRON_REMIN'
       vname_bgcflux(2,ind)='P_IRON_REMIN_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='GRAZE_SP'
       vname_bgcflux(2,ind)='GRAZE_SP_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='GRAZE_DIAT'
       vname_bgcflux(2,ind)='GRAZE_DIAT_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s'
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='GRAZE_TOT'
       vname_bgcflux(2,ind)='GRAZE_TOT_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s'
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='SP_LOSS'
       vname_bgcflux(2,ind)='SP_LOSS_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='DIAT_LOSS'
       vname_bgcflux(2,ind)='DIAT_LOSS_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='ZOO_LOSS'
       vname_bgcflux(2,ind)='ZOO_LOSS_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='SP_AGG'
       vname_bgcflux(2,ind)='SP_AGG_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='DIAT_AGG'
       vname_bgcflux(2,ind)='DIAT_AGG_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='PHOTOC_SP'
       vname_bgcflux(2,ind)='PHOTOC_SP_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='PHOTOC_DIAT'
       vname_bgcflux(2,ind)='PHOTOC_DIAT_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='TOT_PROD'
       vname_bgcflux(2,ind)='TOT_PROD_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='DOC_PROD'
       vname_bgcflux(2,ind)='DOC_PROD_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='DOC_REMIN'
       vname_bgcflux(2,ind)='DOC_REMIN_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s  '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='FE_SCAVENGE'
       vname_bgcflux(2,ind)='FE_SCAVENGE_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='SP_N_LIM'
       vname_bgcflux(2,ind)='SP_N_LIM_AVG'
       vname_bgcflux(3,ind)=' '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='SP_FE_LIM'
       vname_bgcflux(2,ind)='SP_FE_LIM_AVG'
       vname_bgcflux(3,ind)=' '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='SP_PO4_LIM'
       vname_bgcflux(2,ind)='SP_PO4_LIM_AVG'
       vname_bgcflux(3,ind)=' '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='SP_LIGHT_LIM'
       vname_bgcflux(2,ind)='SP_LIGHT_LIM_AVG'
       vname_bgcflux(3,ind)=' '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='DIAT_N_LIM'
       vname_bgcflux(2,ind)='DIAT_N_LIM_AVG'
       vname_bgcflux(3,ind)=' '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='DIAT_FE_LIM'
       vname_bgcflux(2,ind)='DIAT_FE_LIM_AVG'
       vname_bgcflux(3,ind)=' '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='DIAT_PO4_LIM'
       vname_bgcflux(2,ind)='DIAT_PO4_LIM_AVG'
       vname_bgcflux(3,ind)=' '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='DIAT_SIO3_LIM'
       vname_bgcflux(2,ind)='DIAT_SIO3_LIM_AVG'
       vname_bgcflux(3,ind)=' '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='DIAT_LIGHT_LIM'
       vname_bgcflux(2,ind)='DIAT_LIGHT_LIM_AVG'
       vname_bgcflux(3,ind)=' '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='CACO3_FORM'
       vname_bgcflux(2,ind)='CACO3_FORM_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='DIAZ_NFIX'
       vname_bgcflux(2,ind)='DIAZ_NFIX_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s   '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='GRAZE_DIAZ'
       vname_bgcflux(2,ind)='GRAZE_DIAZ_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s  '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='DIAZ_LOSS'
       vname_bgcflux(2,ind)='DIAZ_LOSS_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s  '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='PHOTOC_DIAZ'
       vname_bgcflux(2,ind)='PHOTOC_DIAZ_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s  '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='DIAZ_P_LIM'
       vname_bgcflux(2,ind)='Diazotroph P limitation'
       vname_bgcflux(3,ind)=' '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='DIAZ_FE_LIM'
       vname_bgcflux(2,ind)='Diazotroph Fe limitation'
       vname_bgcflux(3,ind)=' '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='DIAZ_LIGHT_LIM'
       vname_bgcflux(2,ind)='Diazotroph light limitation'
       vname_bgcflux(3,ind)=' '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='FE_SCAVENGE_RATE'
       vname_bgcflux(2,ind)='Iron scavenging rate'
       vname_bgcflux(3,ind)=' '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='DON_PROD'
       vname_bgcflux(2,ind)='DON_PROD_AVG'
       vname_bgcflux(3,ind)=' '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='DON_REMIN'
       vname_bgcflux(2,ind)='DON_REMIN_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s  '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='DOFE_PROD'
       vname_bgcflux(2,ind)='DOFE_PROD_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s  '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='DOFE_REMIN'
       vname_bgcflux(2,ind)='DOFE_REMIN_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s  '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='DOP_PROD'
       vname_bgcflux(2,ind)='DOP_PROD_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s  '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='DOP_REMIN'
       vname_bgcflux(2,ind)='DOP_REMIN_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='BSI_FORM'
       vname_bgcflux(2,ind)='BSI_FORM_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='PHOTOFE_DIAZ'
       vname_bgcflux(2,ind)='PHOTOFE_DIAZ_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s  '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='PHOTOFE_DIAT'
       vname_bgcflux(2,ind)='PHOTOFE_DIAT_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s  '
       vname_bgcflux(4,ind)=' '

       ind=ind+1
       vname_bgcflux(1,ind)='PHOTOFE_SP'
       vname_bgcflux(2,ind)='PHOTOFE_SP_AVG'
       vname_bgcflux(3,ind)='mmol/m3/s  '
       vname_bgcflux(4,ind)=' '
  
       ind=ind+1
       vname_bgcflux(1,ind)='NITRIF'
       vname_bgcflux(2,ind)='Nitrification'
       vname_bgcflux(3,ind)='mmol/m3/s  '
       vname_bgcflux(4,ind)=' '

       end subroutine init_scalars_becflux

#else /* BIOLOGY_BEC && BGC_FLUX_ANALYSIS */
      subroutine init_scalars_becflux_empty()
      return
      end
#endif  /* BIOLOGY_BEC && BGC_FLUX_ANALYSIS */
 
