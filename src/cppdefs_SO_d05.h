/* This is "cppdefs.h": MODEL CONFIGURATION FILE
   ==== == ============ ===== ============= ====
*/

# define SO

#ifdef SO
                     /*  Choose setup */
# define SO_d05

                     /*  Dynamics */
# define SOLVE3D
# define UV_ADV
# define UV_COR

                     /*  Equation of State */
# define NONLIN_EOS
# define SPLIT_EOS
# define SALINITY

                     /*  Forcing and Climatology */
# define QCORRECTION
# define DIURNAL_SRFLUX
# define SALINITY_MASK
# define T_FRC_BRY
# define Z_FRC_BRY
# define M3_FRC_BRY
# define M2_FRC_BRY
# define EXACT_RESTART
# define TSOURCE
# define ICEOBS

                      /* Lateral Mixing */
# define UV_VIS2
# define MIX_GP_TS
# define TS_DIF2
# define MIX_GP_UV

                      /* Vertical Mixing Parameterization */
# define LMD_MIXING
# define LMD_RIMIX
# define LMD_CONVEC
# define LMD_KPP
# define LMD_NONLOCAL
# define LIMIT_MIN_HBLS 35.0
/* # define LIMIT_MIN_KS 0.01 */
/* # define LIMIT_MIN_KT 0.01 */

                      /* Grid Configuration */
# define CURVGRID
# define SPHERICAL
# define MASKING
# define MASK_LAND_DATA

                      /* Open Boundary Conditions */
# define EW_PERIODIC
# define OBC_NORTH
# define OBC_M2FLATHER /* Barotop. BC: OBC_M2FLATHER, OBC_M2ORLANSKI, OBC_M2SPECIFIED */
# define OBC_NORTH_M2SPECIFIED_TILESTR 280 /* OBC_M2SPECIFIED for a certain range of tiles */
# define OBC_NORTH_M2SPECIFIED_TILEEND 285 /* OBC_M2SPECIFIED for a certain range of tiles */
# define OBC_M3ORLANSKI /* Baroclin. BC: OBC_M3ORLANSKI, OBC_M3SPECIFIED */
# define OBC_TORLANSKI /* Tracer BC: OBC_TORLANSKI, OBC_TSPECIFIED */
# define SPONGE

                      /* New sigma coords */
# define NEW_S_COORD

                      /* Flux Analysis */
/* # define PHYS_FLUX_ANALYSIS */
/* # define FULL_PHYS_FLUX_ANALYSIS */
/* # define VERT_DIFF_ANALYSIS */
/* # define SELECTED_FLUX_ANALYSIS */
# define PASSIVE_TRACER
# define AGE_DYE_TRACER
/* # define SWFLUX_TRACER */

                      /* Other Output */
# define AVERAGES
# define SLICE_AVG
# define COMPUTE_SPEED_DIAGNOSE
/* # define KPP_DIAGNOSE */

                      /* Biology */
/* # define BIOLOGY_BEC2 */
/* # define BEC2_DIAG */
/* # define VFLX_CORR */
/* # define PCO2AIR_FORCING */
/* # define DAILYPAR_BEC */
/* # undef RIVER_LOAD_N */
/* # undef RIVER_LOAD_P */
/* # define VARIABLE_ATM_PCO2 */

                      /* Sea ice */

#endif /* SO */

/* ------------------------------------ */
#include "set_global_definitions.h"
