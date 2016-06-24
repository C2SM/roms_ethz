/* This is "cppdefs.h": MODEL CONFIGURATION FILE
   ==== == ============ ===== ============= ====
*/
                     /* 1/2 degree setup (SO_d05) */
#define GRID_SIZE LLm=720, MMm=216, N=64
#define DOMAIN_TILING NP_XI=8, NP_ETA=36, NSUB_X=1, NSUB_E=1

                     /*  Dynamics */
# define SOLVE3D
# define UV_ADV
# define UV_COR
/* # define ADV_ISONEUTRAL */

                     /*  Equation of State */
# define NONLIN_EOS
# define SPLIT_EOS
# define SALINITY

                     /*  Forcing and Climatology */
# define QCORRECTION
# define SFLX_CORR
# define DIURNAL_SRFLUX
# define SALINITY_MASK
# define SALINITY_MASKLATSTR -43.0
# define SALINITY_MASKLATEND -53.01
# define T_FRC_BRY
# define Z_FRC_BRY
# define M3_FRC_BRY
# define M2_FRC_BRY
# define DEFAULT_BRY_VALUES
# define EXACT_RESTART
# define TSOURCE
# define ICEOBS
# define VFLX_CORR

                      /* Lateral Mixing */
# define UV_VIS2
# define TS_DIF2

                      /* Vertical Mixing Parameterization */
# define LMD_MIXING
# define LMD_RIMIX
# define LMD_DDMIX
# define LMD_CONVEC
# define LMD_KPP
# define LMD_NONLOCAL
# define LMD_BKPP
/* # define LMD_LIMIT_STABLE */
/* # define MLCONVEC */
/* # define LIMIT_MIN_HBLS 35.0 */
/* # define LIMIT_MIN_KS 0.01 */
/* # define LIMIT_MIN_KT 0.01 */
/* # define KPP_DIAGNOSE */

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
# define SPONGE_WIDTH

                      /* Flux Analysis */
/* # define PHYS_FLUX_ANALYSIS */
/* # define FULL_PHYS_FLUX_ANALYSIS */
/* # define VERT_DIFF_ANALYSIS */
/* # define SELECTED_FLUX_ANALYSIS */

                      /* Other Output */
# define AVERAGES
/* # define SLICE_AVG*/                                         /* switched off, does not work anymore in v5 */
# define WRITE_SALT_REST
# define WRITE_TEMP_REST
# define COMPUTE_SPEED_DIAGNOSE
/* # define KPP_DIAGNOSE */
/* # define PASSIVE_TRACER */                                         /* switched off, does not work anymore: merge with v3 */
/* # define AGE_DYE_TRACER */                                         /* switched off, does not work anymore: merge with v3 */
/* # define SWFLUX_TRACER */

                      /* Biology */
/* # define BIOLOGY_BEC2 */
/* # define BEC2_DIAG */
/* # define PCO2AIR_FORCING */
/* # define DAILYPAR_BEC */
/* # undef RIVER_LOAD_N */
/* # undef RIVER_LOAD_P */
/* # define MULT_CLIM_FILES */
/* # define VARIABLE_ATM_PCO2 */

                      /* Sea ice model */

/* ------------------------------------ */
#include "set_global_definitions.h"