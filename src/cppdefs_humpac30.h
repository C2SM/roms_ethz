/* 
  UP ETH HUMBOLDT Telescopic Setup - USWC Focus
  == === ======== ========== ===== = ==== =====
*/

     /* Include standard CPP switches for UP ETH Zurich */  
#include "cppdefs_UP.h"

!-->#define GRID_SIZE LLm=699, MMm=1007, N=42 
#define GRID_SIZE LLm=349, MMm=504, N=42
#define DOMAIN_TILING NP_XI=8, NP_ETA=30, NSUB_X=1, NSUB_E=1 ! Euler
!-->#define DOMAIN_TILING NP_XI=6, NP_ETA=8, NSUB_X=1, NSUB_E=1 ! Euler
/* Open Boundaries */
#define OBC_WEST /* Open boundary in the west (in order: SO out, SO in, Ind. throughflow) */
#define OBC_W_M2SPEC_STR (0.5*NP_XI*NP_ETA+1) /* OBC_M2SPECIFIED (specify barotropica component... */
#define OBC_W_M2SPEC_END ((NP_XI-1.)*NP_ETA+1) /*... For the upper half of domain i.e. SO in and IndT  */
#define OBC_NORTH  /* Open boundary North (Arctic) */
#define OBC_N_M2SPEC_STR  1/* OBC_M2SPECIFIED for all tiles*/
#define OBC_N_M2SPEC_END  (NP_XI*NP_ETA) /* OBC_M2SPECIFIED for all tiles*/
!-- #define SPONGE


/* Switches required for Flux correction */
#define SFLX_CORR 
#define QCORRECTION
#define VFLX_CORR

#define SALINITY_MASK
!--#define SALINITY_MASKLATSTR -48.0
!--#define SALINITY_MASKLATEND -53.01

#define TDEP_REMIN
!--> # define ICEOBS

     /* Output */
#define AVERAGES
#define SLICE_AVG
#define CALENDAR '365_day'     /* netCDF CF-convention CALENDAR attribute default: '360_day' */
#define STARTDATE '1969-01-01' /* Ana's Hindcast */

#define ADV_ISONEUTRAL

     /* Biology */
!--> #define BIOLOGY_NPZDOC
# define BIOLOGY_BEC2

#ifdef BIOLOGY_BEC2
# define BIOLOGY
# define DEFAULT_BRY_VALUES
!--> # define MULT_CLIM_FILES
# define VFLX_CORR
# define BEC2_DIAG
!--# define Ncycle_SY
!--# define N2O_TRACER_DECOMP
# define N2O_NEV
!-- # define RIVER_LOAD_N
!-- # define RIVER_LOAD_P
!-- # define NHY_FORCING
!-- # define NOX_FORCING
# define PCO2AIR_FORCING
#endif /* BIOLOGY_BEC2 */


    /* Flux Analysis */
!!# define BGC_FLUX_ANALYSIS
!!#define BGC_FLUX_EXT_HIS_OUTPUT
!# define PHYS_FLUX_ANALYSIS
!!# define FULL_PHYS_FLUX_ANALYSIS
!# define VERT_DIFF_ANALYSIS
!# define SELECTED_FLUX_ANALYSIS
!# define WRITE_DEPTHS /* For Budget Analysis Closure */

                     
#define USE_REAL_YEAR /* Only used by age tracers*/
!--> #define VERBOSE
!#define HIS_DOUBLE
!--> #define DEBUG

#include "set_global_definitions.h"

!-- #undef PARALLEL_FILES
!--#undef EXACT_RESTART
