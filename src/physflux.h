! This header file contains all variables for the 
! computation of physical fluxes.
! The variables for the netcdf output are in ncroms_physflux.h

#if defined SOLVE3D && defined PHYS_FLUX_ANALYSIS

!HF: this had to be introduced because of memory constraints on tungsten
      integer NT_PFA
#ifdef CONST_TRACERS
! only T and S have fluxes
      parameter (NT_PFA = 2)
#else
# if defined VERT_DIFF_ANALYSIS && defined BIOLOGY_BEC
! number of components has to be limited so that program fits into memory
      parameter (NT_PFA = 22)
# else
      parameter (NT_PFA = NT)
# endif
#endif

      real HorXAdvFlux(GLOBAL_2D_ARRAY,N,NT_PFA)
      common /ocean_horxadv_flux/HorXAdvFlux

      real HorYAdvFlux(GLOBAL_2D_ARRAY,N,NT_PFA)
      common /ocean_horyadv_flux/HorYAdvFlux

      real VertAdvFlux(GLOBAL_2D_ARRAY,0:N,NT_PFA)
      common /ocean_vertadv_flux/VertAdvFlux

#ifdef VERT_DIFF_ANALYSIS
      real VertDiffFlux(GLOBAL_2D_ARRAY,0:N,NT_PFA)
      common /ocean_vertdiff_flux/VertDiffFlux
#endif

# ifdef FULL_PHYS_FLUX_ANALYSIS

      real TopFlux(GLOBAL_2D_ARRAY,NT_PFA)
      common /ocean_top_flux/TopFlux

      real BottomFlux(GLOBAL_2D_ARRAY,NT_PFA)
      common /ocean_bottom_flux/BottomFlux

      real HorXMixFlux(GLOBAL_2D_ARRAY,N,NT_PFA)
      common /ocean_horxmix_flux/HorXMixFlux

      real HorYMixFlux(GLOBAL_2D_ARRAY,N,NT_PFA)
      common /ocean_horymix_flux/HorYMixFlux

      real VertMixFlux(GLOBAL_2D_ARRAY,0:N,NT_PFA)
      common /ocean_vertmix_flux/VertMixFlux

      real NudgingFlux(GLOBAL_2D_ARRAY,N,NT_PFA)
      common /ocean_nudging_flux/NudgingFlux
# endif /* FULL_PHYS_FLUX_ANALYSIS */

# ifdef AVERAGES
      real HorXAdvFlux_avg(GLOBAL_2D_ARRAY,N,NT_PFA)
      common /ocean_horxadv_flux_avg/HorXAdvFlux_avg

      real HorYAdvFlux_avg(GLOBAL_2D_ARRAY,N,NT_PFA)
      common /ocean_horyadv_flux_avg/HorYAdvFlux_avg

      real VertAdvFlux_avg(GLOBAL_2D_ARRAY,0:N,NT_PFA)
      common /ocean_vertadv_flux_avg/VertAdvFlux_avg

#ifdef VERT_DIFF_ANALYSIS
      real VertDiffFlux_avg(GLOBAL_2D_ARRAY,0:N,NT_PFA)
      common /ocean_vertdiff_flux_avg/VertDiffFlux_avg
#endif

#  ifdef FULL_PHYS_FLUX_ANALYSIS
      real TopFlux_avg(GLOBAL_2D_ARRAY,NT_PFA)
      common /ocean_top_flux_avg/TopFlux_avg

      real BottomFlux_avg(GLOBAL_2D_ARRAY,NT_PFA)
      common /ocean_bottom_flux_avg/BottomFlux_avg

      real HorXMixFlux_avg(GLOBAL_2D_ARRAY,N,NT_PFA)
      common /ocean_horxmix_flux_avg/HorXMixFlux_avg

      real HorYMixFlux_avg(GLOBAL_2D_ARRAY,N,NT_PFA)
      common /ocean_horymix_flux_avg/HorYMixFlux_avg

      real VertMixFlux_avg(GLOBAL_2D_ARRAY,0:N,NT_PFA)
      common /ocean_vertmix_flux_avg/VertMixFlux_avg

      real NudgingFlux_avg(GLOBAL_2D_ARRAY,N,NT_PFA)
      common /ocean_nudging_flux_avg/NudgingFlux_avg
#  endif /* FULL_PHYS_FLUX_ANALYSIS */
# endif /* AVERAGES */
#endif /* SOLVE3D && PHYS_FLUX_ANALYSIS */
