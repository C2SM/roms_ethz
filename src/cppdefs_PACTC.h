/* 
  UP ETH Pacific Telescopic Setup - USWC Focus
  == === ======= ========== ===== = ==== =====
*/
         /* Include standard CPP switches for UP ETH Zurich */  
#include "cppdefs_UP.h"

!--> #define GRID_SIZE LLm=416, MMm=346, N=42      ! pacsg 5km-66km telescopic
#define GRID_SIZE LLm=602, MMm=516, N=42      ! pactcs30 4.1-65km telescopic up to Antarctica
#define DOMAIN_TILING NP_XI=10, NP_ETA=24, NSUB_X=1, NSUB_E=1 ! Euler
!--> #define DOMAIN_TILING NP_XI=4, NP_ETA=6, NSUB_X=1, NSUB_E=1  !ch4

         /* Open Boundaries */
!--> Testing!! #define OBC_SOUTH
#define SPONGE


                      /* Open Boundary Conditions */
!-- #define CLMFORING


#ifdef CLMFORCING    /*  Use CLM Files */
# define TCLIMATOLOGY
# define UCLIMATOLOGY
# define TNUDGING
# define M3NUDGING
# define M2NUDGING
# ifdef TCLIMATOLOGY
#      define CLIMAT_TS_MIXH
# endif
  /* undefine default BRY settings */
# undef T_FRC_BRY
# undef Z_FRC_BRY
# undef M3_FRC_BRY
# undef M2_FRC_BRY
--> #else                /* Default: Use BRY Files*/
--> # undef SPONGE /* note: UP ETH before 8/2015 did not use SPONGE with BRY */
#endif


!--> #undef OBC_M2FLATHER  /* undefine default in cppdefs_UP.h */
!--> #define OBC_M2SPECIFIED

                      /* Output */
#define AVERAGES
#define SLICE_AVG
#define CALENDAR '360_day'     /* netCDF CF-convention CALENDAR attribute default: '360_day' */
!--> #define STARTDATE '0001-01-01' /* part of netCDF CF-convention time units attribute default: '0001-01-01'*/

!--> #define ADV_ISONEUTRAL

                      /* Biology */
!--> #define BIOLOGY_NPZDOC
!--> # define BIOLOGY_BEC2

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
!#define HIS_DOUBLE
!--> #define DEBUG

#include "set_global_definitions.h"

!--> #undef PARALLEL_FILES

