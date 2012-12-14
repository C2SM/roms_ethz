/* This is include file "averages.h": time-averaged fields
 for output purposes:
*/
#ifdef AVERAGES
      real zeta_avg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE zeta_avg(BLOCK_PATTERN) BLOCK_CLAUSE
      real ubar_avg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE  ubar_avg(BLOCK_PATTERN) BLOCK_CLAUSE
      real vbar_avg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE  vbar_avg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /avg_zeta/zeta_avg /avg_ubar/ubar_avg
     &                          /avg_vbar/vbar_avg
# ifdef SOLVE3D
      real u_avg(GLOBAL_2D_ARRAY,N)
CSDISTRIBUTE_RESHAPE u_avg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      real v_avg(GLOBAL_2D_ARRAY,N)
CSDISTRIBUTE_RESHAPE v_avg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      real t_avg(GLOBAL_2D_ARRAY,N,NT)
CSDISTRIBUTE_RESHAPE t_avg(BLOCK_PATTERN,*,*) BLOCK_CLAUSE
      real rho_avg(GLOBAL_2D_ARRAY,N)
CSDISTRIBUTE_RESHAPE rho_avg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      real w_avg(GLOBAL_2D_ARRAY,0:N)
      real wt_avg(GLOBAL_2D_ARRAY,N)
CSDISTRIBUTE_RESHAPE w_avg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /avg_u/u_avg /avg_v/v_avg /avg_t/t_avg
     &                /avg_rho/rho_avg /avg_w/w_avg
     &                /avg_wt/wt_avg
      real akv_avg(GLOBAL_2D_ARRAY,0:N)
      real akt_avg(GLOBAL_2D_ARRAY,0:N)
CSDISTRIBUTE_RESHAPE akt_avg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /avg_akv/akv_avg /avg_akt/akt_avg
#  ifdef KPP_DIAGNOSE
      real rich_avg(GLOBAL_2D_ARRAY,0:N), richN_avg(GLOBAL_2D_ARRAY,0:N)
CSDISTRIBUTE_RESHAPE rich_avg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /rich_akt/rich_avg, richN_avg
#  endif
#  ifdef SALINITY
      real aks_avg(GLOBAL_2D_ARRAY,0:N)
CSDISTRIBUTE_RESHAPE aks_avg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /avg_aks/aks_avg
#  endif
#  ifdef LMD_KPP
      real hbl_avg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE hbl_avg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /avg_hbl/hbl_avg
#  endif
#  ifdef LMD_BKPP
      real hbbl_avg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE hbbl_avg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /avg_hbbl/hbbl_avg
#  endif
!DL:
#  ifdef WRITE_DEPTHS
      real z_r_avg(GLOBAL_2D_ARRAY,N)
      real z_w_avg(GLOBAL_2D_ARRAY,0:N), Hz_avg(GLOBAL_2D_ARRAY,N)
      common /avg_depth/z_r_avg, z_w_avg, Hz_avg
#  endif /* WRITE_DEPTHS */
# endif /* SOLVE3D */
!--> #endif /* AVERAGES */
 
# ifdef SLICE_AVG
      real zeta_slavg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE zeta_slavg(BLOCK_PATTERN) BLOCK_CLAUSE
      real ubar_slavg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE  ubar_slavg(BLOCK_PATTERN) BLOCK_CLAUSE
      real vbar_slavg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE  vbar_slavg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /slavg_zeta/zeta_slavg /slavg_ubar/ubar_slavg
     &                          /slavg_vbar/vbar_slavg
# ifdef SOLVE3D
      real u_slavg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE u_slavg(BLOCK_PATTERN) BLOCK_CLAUSE
      real v_slavg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE v_slavg(BLOCK_PATTERN) BLOCK_CLAUSE
      real t_slavg(GLOBAL_2D_ARRAY,NT)
CSDISTRIBUTE_RESHAPE t_slavg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      real rho_slavg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE rho_slavg(BLOCK_PATTERN) BLOCK_CLAUSE
      real w_slavg(GLOBAL_2D_ARRAY)
      real wt_slavg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE w_slavg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /slavg_u/u_slavg /slavg_v/v_slavg /slavg_t/t_slavg
     &                /slavg_rho/rho_slavg /slavg_w/w_slavg
     &                /slavg_wt/wt_slavg
      real akv_slavg(GLOBAL_2D_ARRAY)
      real akt_slavg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE akt_slavg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /slavg_akv/akv_slavg /slavg_akt/akt_slavg
#ifdef KPP_DIAGNOSE
      real rich_slavg(GLOBAL_2D_ARRAY,0:N), richN_slavg(GLOBAL_2D_ARRAY,0:N)
CSDISTRIBUTE_RESHAPE rich_slavg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /rich_akt/rich_slavg, richN_slavg
#endif
#  ifdef SALINITY
      real aks_slavg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE aks_slavg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /slavg_aks/aks_slavg
#  endif
#  ifdef LMD_KPP
      real hbl_slavg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE hbl_slavg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /slavg_hbl/hbl_slavg
#  endif
#  ifdef LMD_BKPP
      real hbbl_slavg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE hbbl_slavg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /slavg_hbbl/hbbl_slavg
#  endif
# endif /* SOLVE3D */
# endif /* SLICE_AVG */
#endif /* AVERAGES */
 
