! This header file contains all variables and parameters for the
! netcdf output of physical fluxes.

#if defined SOLVE3D && defined PHYS_FLUX_ANALYSIS
      logical new_phys_flux_his
      integer n_phys_flux_his
      common /nc_phys_flux_his/n_phys_flux_his, new_phys_flux_his

      integer hisHorXAdvFlux(NT_PFA), hisHorYAdvFlux(NT_PFA)
     &     , hisVertAdvFlux(NT_PFA)
#ifdef VERT_DIFF_ANALYSIS
     &     , hisVertDiffFlux(NT_PFA)
#endif
     &     , phys_flux_hisTime, phys_flux_hisTstep, phys_flux_hisZ
     &     , nrpf_phys_flux_his
     &     , ncid_phys_flux_his, nrec_phys_flux_his

      common /inc_phys_flux/ hisHorXAdvFlux, hisHorYAdvFlux
     &     , hisVertAdvFlux
#ifdef VERT_DIFF_ANALYSIS
     &     , hisVertDiffFlux
#endif
     &     , phys_flux_hisTime, phys_flux_hisTstep, phys_flux_hisZ
     &     , nrpf_phys_flux_his
     &     , ncid_phys_flux_his, nrec_phys_flux_his


      integer indxHorXAdvFlux, indxHorYAdvFlux
      integer indxVertAdvFlux
#ifdef VERT_DIFF_ANALYSIS
      integer indxVertDiffFlux
#endif

      parameter(indxHorXAdvFlux = 1)
! first horizontal advective flux in y-direction
      parameter(indxHorYAdvFlux = indxHorXAdvFlux + NT_PFA)
! first vertical advective flux
      parameter(indxVertAdvFlux = indxHorYAdvFlux + NT_PFA)
#ifdef VERT_DIFF_ANALYSIS
! first vertical diffusive flux
      parameter(indxVertDiffFlux = indxVertAdvFlux + NT_PFA)
#endif

      character*80 phys_flux_his_name
      character*70 vname_phys(4,(3
#ifdef VERT_DIFF_ANALYSIS
     &     + 1
#  endif
# if defined FULL_PHYS_FLUX_ANALYSIS && !defined SELECTED_FLUX_ANALYSIS /* AH */
     &     + 6
# endif
# if defined FULL_PHYS_FLUX_ANALYSIS && defined SELECTED_FLUX_ANALYSIS /* AH */
     &     + 1
# endif
     &     ) * NT_PFA)
      common /cnc_phys_flux/ phys_flux_his_name, vname_phys

# ifdef FULL_PHYS_FLUX_ANALYSIS
# if !defined SELECTED_FLUX_ANALYSIS /* AH */
      integer indxTopFlux, indxBottomFlux, indxSRAbsFlux
      integer indxHorXMixFlux, indxHorYMixFlux, indxVertMixFlux
      integer indxNudgingFlux
# endif /* SELECTED_FLUX_ANALYSIS */
# if defined SELECTED_FLUX_ANALYSIS /* AH */
      integer indxTopFlux, indxSRAbsFlux
# endif /* SELECTED_FLUX_ANALYSIS */
#ifdef VERT_DIFF_ANALYSIS
! 1st top flux
      parameter(indxTopFlux = indxVertDiffFlux + NT_PFA)
#else
      parameter(indxTopFlux = indxVertAdvFlux + NT_PFA)
#endif
# if !defined SELECTED_FLUX_ANALYSIS /* AH */
! 1st bottom fl.
      parameter(indxBottomFlux = indxTopFlux + NT_PFA)
! mixing fluxes in xi, eta, z directions
      parameter(indxHorXMixFlux = indxBottomFlux + NT_PFA)
      parameter(indxHorYMixFlux = indxHorXMixFlux + NT_PFA)
      parameter(indxVertMixFlux = indxHorYMixFlux + NT_PFA)
      parameter(indxNudgingFlux = indxVertMixFlux + NT_PFA)
      parameter(indxSRAbsFlux = indxNudgingFlux + NT_PFA)
#else
      parameter(indxSRAbsFlux = indxTopFlux + NT_PFA)
#endif /* SELECTED_FLUX_ANALYSIS */


# if !defined SELECTED_FLUX_ANALYSIS /* AH */
      integer hisTopFlux(NT_PFA), hisBottomFlux(NT_PFA)
     &     , hisHorXMixFlux(NT_PFA), hisHorYMixFlux(NT_PFA)
     &     , hisVertMixFlux(NT_PFA)
     &     , hisNudgingFlux(NT_PFA)
     &     , hisSRAbsFlux
      common /inc_full_phys_flux/ hisTopFlux, hisBottomFlux
     &     , hisHorXMixFlux, hisHorYMixFlux
     &     , hisVertMixFlux
     &     , hisNudgingFlux
     &     , hisSRAbsFlux
#endif /* SELECTED_FLUX_ANALYSIS */
# if defined SELECTED_FLUX_ANALYSIS /* AH */
      integer hisTopFlux(NT_PFA), hisSRAbsFlux
      common /inc_full_phys_flux/ hisTopFlux, hisSRAbsFlux
#endif /* SELECTED_FLUX_ANALYSIS */
# endif /* FULL_PHYS_FLUX_ANALYSIS */

# ifdef AVERAGES
      real time_phys_flux_avg
      common /t_phys_flux_avg/time_phys_flux_avg

#if !defined SELECTED_FLUX_ANALYSIS /* AH */
      real zeta_phys_flux_avg(GLOBAL_2D_ARRAY)
      common /zeta_phys_flux_avg/zeta_phys_flux_avg
#endif /* SELECTED_FLUX_ANALYSIS */

      logical new_phys_flux_avg
      integer nts_phys_flux_avg, n_phys_flux_avg
      common /nc_phys_flux_avg/nts_phys_flux_avg, n_phys_flux_avg
     &        , new_phys_flux_avg

      integer avgHorXAdvFlux(NT_PFA), avgHorYAdvFlux(NT_PFA)
     &     , avgVertAdvFlux(NT_PFA)
#ifdef VERT_DIFF_ANALYSIS
     &     , avgVertDiffFlux(NT_PFA)
#endif
     &     , phys_flux_avgTime, phys_flux_avgTstep, phys_flux_avgZ
     &     , nrpf_phys_flux_avg
     &     , ncid_phys_flux_avg, nrec_phys_flux_avg
      common /inc_phys_flux_avg/ avgHorXAdvFlux, avgHorYAdvFlux
     &     , avgVertAdvFlux
#ifdef VERT_DIFF_ANALYSIS
     &     , avgVertDiffFlux
#endif
     &     , phys_flux_avgTime, phys_flux_avgTstep, phys_flux_avgZ
     &     , nrpf_phys_flux_avg
     &     , ncid_phys_flux_avg, nrec_phys_flux_avg

      character*80 phys_flux_avg_name
      common /cnc_phys_flux_avg/ phys_flux_avg_name


#  ifdef FULL_PHYS_FLUX_ANALYSIS
#if !defined SELECTED_FLUX_ANALYSIS /* AH */
      integer avgTopFlux(NT_PFA), avgBottomFlux(NT_PFA)
     &     , avgHorXMixFlux(NT_PFA), avgHorYMixFlux(NT_PFA)
     &     , avgVertMixFlux(NT_PFA)
     &     , avgNudgingFlux(NT_PFA)
     &     , avgSRAbsFlux

       common /inc_full_phys_flux_avg/ avgTopFlux, avgBottomFlux
     &     , avgHorXMixFlux, avgHorYMixFlux, avgVertMixFlux
     &     , avgNudgingFlux
     &     , avgSRAbsFlux
#endif /* SELECTED_FLUX_ANALYSIS */
#if defined SELECTED_FLUX_ANALYSIS /* AH */
      integer avgTopFlux(NT_PFA), avgSRAbsFlux

       common /inc_full_phys_flux_avg/ avgTopFlux, avgSRAbsFlux
#endif /* SELECTED_FLUX_ANALYSIS */
#  endif /* FULL_PHYS_FLUX_ANALYSIS */
# endif /* AVERAGES */
#endif /* SOLVE3D && PHYS_FLUX_ANALYSIS */
