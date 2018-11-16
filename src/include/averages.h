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
CSDISTRIBUTE_RESHAPE w_avg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /avg_u/u_avg /avg_v/v_avg /avg_t/t_avg
     &                /avg_rho/rho_avg /avg_w/w_avg
      real wvl_avg(GLOBAL_2D_ARRAY,0:N)
CSDISTRIBUTE_RESHAPE wvl_avg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /avg_wvl/wvl_avg
      real akv_avg(GLOBAL_2D_ARRAY,0:N)
CSDISTRIBUTE_RESHAPE akv_avg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      real akt_avg(GLOBAL_2D_ARRAY,0:N)
CSDISTRIBUTE_RESHAPE akt_avg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /avg_akv/akv_avg /avg_akt/akt_avg
#  ifdef SALINITY
      real aks_avg(GLOBAL_2D_ARRAY,0:N)
CSDISTRIBUTE_RESHAPE aks_avg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /avg_aks/aks_avg
#  endif
#  ifdef WRITE_CO2FLX
      real co2flx_avg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE co2flx_avg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /avg_co2flx/co2flx_avg
#  endif
#  ifdef WRITE_HEATFLX
      real heatflx_avg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE heatflx_avg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /avg_heatflx/heatflx_avg
#  endif
#  if defined QCORRECTION && defined WRITE_TEMP_REST
      real restflx_temp_avg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE restflx_temp_avg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /avg_restflx_temp/restflx_temp_avg
#  endif /* QCORRECTION && WRITE_TEMP_REST */
#  if defined SFLX_CORR && defined WRITE_SALT_REST
      real restflx_salt_avg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE restflx_salt_avg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /avg_restflx_salt/restflx_salt_avg
#  endif /* SFLX_CORR && WRITE_SALT_REST */
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
#  ifdef KPP_DIAGNOSE
! kppwavm kppwavt kppcon kppddt kppdds
! kppbblm kppbblt kppbbls kppsblm kppsblt kppsbls
!      real kppwavm_avg(GLOBAL_2D_ARRAY,0:N)
!CSDISTRIBUTE_RESHAPE kppwavm_avg(BLOCK_PATTERN) BLOCK_CLAUSE
!      common /avg_kppwavm/kppwavm_avg
!      real kppwavt_avg(GLOBAL_2D_ARRAY,0:N)
!CSDISTRIBUTE_RESHAPE kppwavt_avg(BLOCK_PATTERN) BLOCK_CLAUSE
!      common /avg_kppwavt/kppwavt_avg
!      real kppcon_avg(GLOBAL_2D_ARRAY,0:N)
!CSDISTRIBUTE_RESHAPE kppcon_avg(BLOCK_PATTERN) BLOCK_CLAUSE
!      common /avg_kppcon/kppcon_avg
!      real kppddt_avg(GLOBAL_2D_ARRAY,0:N)
!CSDISTRIBUTE_RESHAPE kppddt_avg(BLOCK_PATTERN) BLOCK_CLAUSE
!      common /avg_kppddt/kppddt_avg
!      real kppdds_avg(GLOBAL_2D_ARRAY,0:N)
!CSDISTRIBUTE_RESHAPE kppdds_avg(BLOCK_PATTERN) BLOCK_CLAUSE
!      common /avg_kppdds/kppdds_avg
!      real kppbblm_avg(GLOBAL_2D_ARRAY,0:N)
!CSDISTRIBUTE_RESHAPE kppbblm_avg(BLOCK_PATTERN) BLOCK_CLAUSE
!      common /avg_kppbblm/kppbblm_avg
!      real kppbblt_avg(GLOBAL_2D_ARRAY,0:N)
!CSDISTRIBUTE_RESHAPE kppbblt_avg(BLOCK_PATTERN) BLOCK_CLAUSE
!      common /avg_kppbblt/kppbblt_avg
!      real kppbbls_avg(GLOBAL_2D_ARRAY,0:N)
!CSDISTRIBUTE_RESHAPE kppbbls_avg(BLOCK_PATTERN) BLOCK_CLAUSE
!      common /avg_kppbbls/kppbbls_avg
!      real kppsblm_avg(GLOBAL_2D_ARRAY,0:N)
!CSDISTRIBUTE_RESHAPE kppsblm_avg(BLOCK_PATTERN) BLOCK_CLAUSE
!      common /avg_kppsblm/kppsblm_avg
!      real kppsblt_avg(GLOBAL_2D_ARRAY,0:N)
!CSDISTRIBUTE_RESHAPE kppsblt_avg(BLOCK_PATTERN) BLOCK_CLAUSE
!      common /avg_kppsblt/kppsblt_avg
!      real kppsbls_avg(GLOBAL_2D_ARRAY,0:N)
!CSDISTRIBUTE_RESHAPE kppsbls_avg(BLOCK_PATTERN) BLOCK_CLAUSE
!      common /avg_kppsbls/kppsbls_avg
!
! kpprichm kppricht
! kppsblsh kppsblst kppsblrot kppsblent
!
      real kpprichm_avg(GLOBAL_2D_ARRAY,0:N)
CSDISTRIBUTE_RESHAPE kpprichm_avg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /avg_kpprichm/kpprichm_avg
      real kppricht_avg(GLOBAL_2D_ARRAY,0:N)
CSDISTRIBUTE_RESHAPE kppricht_avg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /avg_kppricht/kppricht_avg
      real kppsblsh_avg(GLOBAL_2D_ARRAY,0:N)
CSDISTRIBUTE_RESHAPE kppsblsh_avg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /avg_kppsblsh/kppsblsh_avg
      real kppsblst_avg(GLOBAL_2D_ARRAY,0:N)
CSDISTRIBUTE_RESHAPE kppsblst_avg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /avg_kppsblst/kppsblst_avg
      real kppsblrot_avg(GLOBAL_2D_ARRAY,0:N)
CSDISTRIBUTE_RESHAPE kppsblrot_avg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /avg_kppsblrot/kppsblrot_avg
      real kppsblent_avg(GLOBAL_2D_ARRAY,0:N)
CSDISTRIBUTE_RESHAPE kppsblent_avg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /avg_kppsblent/kppsblent_avg
#  endif
!DL:
#  ifdef WRITE_DEPTHS
      real z_r_avg(GLOBAL_2D_ARRAY,N)
      real z_w_avg(GLOBAL_2D_ARRAY,0:N), Hz_avg(GLOBAL_2D_ARRAY,N)
      common /avg_depth/ z_r_avg, z_w_avg, Hz_avg
#  endif /* WRITE_DEPTHS */

#  if defined BIOLOGY_NPZDOC && defined OXYGEN
      real GasExcFlux_avg(GLOBAL_2D_ARRAY,NumGasExcTerms)
      common /bgc_gasexcflux_avg/GasExcFlux_avg
#  endif /* BIOLOGY_NPZDOC && OXYGEN */

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
      real wvl_slavg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE w_slavg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /slavg_u/u_slavg /slavg_v/v_slavg /slavg_t/t_slavg
     &                /slavg_rho/rho_slavg /slavg_w/w_slavg
     &                /slavg_wvl/wvl_slavg
      real akv_slavg(GLOBAL_2D_ARRAY)
      real akt_slavg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE akt_slavg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /slavg_akv/akv_slavg /slavg_akt/akt_slavg
#  ifdef SALINITY
      real aks_slavg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE aks_slavg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /slavg_aks/aks_slavg
#  endif
#  ifdef WRITE_CO2FLX
      real co2flx_slavg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE co2flx_slavg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /slavg_co2flx/co2flx_slavg
#  endif
#  ifdef WRITE_HEATFLX
      real heatflx_slavg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE heatflx_slavg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /slavg_heatflx/heatflx_slavg
#  endif
#  if defined QCORRECTION && defined WRITE_TEMP_REST
      real restflx_temp_slavg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE restflx_temp_slavg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /slavg_restflx_temp/restflx_temp_slavg
#  endif /* QCORRECTION && WRITE_TEMP_REST */
#  if defined SFLX_CORR && defined WRITE_SALT_REST
      real restflx_salt_slavg(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE restflx_salt_slavg(BLOCK_PATTERN) BLOCK_CLAUSE
      common /slavg_restflx_salt/restflx_salt_slavg
#  endif /* SFLX_CORR && WRITE_SALT_REST */
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

#  ifdef WRITE_DEPTHS
      real z_r_slavg(GLOBAL_2D_ARRAY)
      real z_w_slavg(GLOBAL_2D_ARRAY), Hz_slavg(GLOBAL_2D_ARRAY)
      common /slavg_depth/ z_r_slavg, z_w_slavg, Hz_slavg
#  endif /* WRITE_DEPTHS */


#  if defined BIOLOGY_NPZDOC && defined OXYGEN
      real GasExcFlux_slavg(GLOBAL_2D_ARRAY,NumGasExcTerms)
      common /bgc_gasexcflux_avg/GasExcFlux_slavg
#  endif /* BIOLOGY_NPZDOC && OXYGEN */

# endif /* SOLVE3D */
# endif /* SLICE_AVG */
#endif /* AVERAGES */
