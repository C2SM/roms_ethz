/* 
  UP ETH Pacific Telescopic Setup - USWC Focus
  == === ======= ========== ===== = ==== =====
*/
         /* Include standard CPP switches for UP ETH Zurich */  
#include "cppdefs_UP.h"

#define GRID_SIZE LLm=416, MMm=346, N=42      ! 5km-66km telescopic
!--> Euler: #define DOMAIN_TILING NP_XI=10, NP_ETA=24, NSUB_X=1, NSUB_E=1 ! Euler
!--> #define DOMAIN_TILING NP_XI=3, NP_ETA=2, NSUB_X=1, NSUB_E=1  !ch4
!--> #define DOMAIN_TILING NP_XI=1, NP_ETA=1, NSUB_X=1, NSUB_E=1  !ch4
#define DOMAIN_TILING NP_XI=4, NP_ETA=6, NSUB_X=1, NSUB_E=1  !ch4

         /* Open Boundaries */
#define OBC_SOUTH


                      /* Open Boundary Conditions */
!-- #define CLMFORING

#ifdef CLMFORCING    /*  Use CLM Files */
# define TCLIMATOLOGY
# define UCLIMATOLOGY
# define TNUDGING
# define M3NUDGING
# define M2NUDGING
# define SPONGE
# ifdef TCLIMATOLOGY
#      define CLIMAT_TS_MIXH
# endif
  /* undefine default BRY settings */
# undef T_FRC_BRY
# undef Z_FRC_BRY
# undef M3_FRC_BRY
# undef M2_FRC_BRY
#else                /* Default: Use BRY Files*/
# undef SPONGE /* note: UCLA uses SPONGE with BRY */
#endif

#undef OBC_M2FLATHER  /* undefine default in cppdefs_UP.h */
#define OBC_M2SPECIFIED

                      /* Output */
#define AVERAGES
#define SLICE_AVG
#define CALENDAR '365_day'     /* netCDF CF-convention CALENDAR attribute */
#define STARTDATE '1979-01-01' /* netCDF CF-convention time units attribute */


                      /* Biology */
!--> #define BIOLOGY_NPZDOC
# define BIOLOGY_BEC2

#ifdef BIOLOGY_BEC2
# define DEFAULT_BGC_BRY_VALUES
!--> # define MULT_CLIM_FILES
!--> # define VFLX_CORR
!--> # define BEC2_DIAG
!# define RIVER_LOAD_N
!# define RIVER_LOAD_P
!--> # define PCO2AIR_FORCING
#endif /* BIOLOGY_BEC2 */


                      /* Flux Analysis */
!!# define BGC_FLUX_ANALYSIS
!!#define BGC_FLUX_EXT_HIS_OUTPUT
!# define PHYS_FLUX_ANALYSIS
!!# define FULL_PHYS_FLUX_ANALYSIS
!# define VERT_DIFF_ANALYSIS
!# define SELECTED_FLUX_ANALYSIS
!# define WRITE_DEPTHS /* For Budget Analysis Closure */

                     
!#define USE_REAL_YEAR /* Only used by age tracers*/
!--> #define VERBOSE
#define HIS_DOUBLE
!--> #define DEBUG

#include "set_global_definitions.h"

#undef PARALLEL_FILES

