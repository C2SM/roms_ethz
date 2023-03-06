/* 
  OBEG - Peru-Chile Set-up 0.1 degree resolution. 
  == === ======== ========== ===== = ==== =====
*/

/* Include standard CPP switches for UP ETH Zurich */  
#include "cppdefs_UP.h"

/* Define grid size and domain tiling */
#define GRID_SIZE LLm=400, MMm=500, N=42
#define DOMAIN_TILING NP_XI=32, NP_ETA=16, NSUB_X=1, NSUB_E=1 ! Euler

/* Open Boundaries */
#define OBC_WEST    /* Open boundary in the west   */
#define OBC_NORTH   /* Open boundary in the north  */
!--#define OBC_EAST /* Closed boundary in the east */
#define OBC_SOUTH   /* Open boundary to the south  */

/* Not sure what these do, but they're off */
!--#define OBC_S_M2SPEC_STR (NP_XI*(NP_ETA-1.)) 		   /* OBC_M2SPECIFIED for a certain range of tiles */
!--#define OBC_N_M2SPEC_STR  1                  		   /* OBC_M2SPECIFIED for all tiles*/
!--#define OBC_N_M2SPEC_END  (NP_XI*NP_ETA)     		   /* OBC_M2SPECIFIED for all tiles*/
!--#define OBC_S_M2SPEC_END ((NP_XI*(NP_ETA-1.))+(NP_XI*0.75)-1.)  /* OBC_M2SPECIFIED for a certain range of tiles */

/* Not sure what these do, but they're on */
#define OBC_M3ORLANSKI /* Baroclin. BC: OBC_M3ORLANSKI, OBC_M3SPECIFIED */
#define OBC_TORLANSKI  /* Tracer BC: OBC_TORLANSKI, OBC_TSPECIFIED */
#undef SPONGE

/* Switches required for Flux correction */
#define SFLX_CORR 
#define QCORRECTION
#define VFLX_CORR

/* Output */
#define AVERAGES
#define SLICE_AVG
#define CALENDAR '365_day'     /* netCDF CF-convention CALENDAR attribute default: '360_day' */
#define STARTDATE '1929-01-01' /* Ana's Hindcast */

/* Rotate diffusive operator to apply it along isopyncals */
/* Off after consulting PDamien */
!--#define ADV_ISONEUTRAL

/* Biology */
# define BIOLOGY_BEC2
#ifdef BIOLOGY_BEC2
# define BIOLOGY
# define DEFAULT_BRY_VALUES
# define VFLX_CORR
# define BEC2_DIAG
# define Ncycle_SY
# define N2O_TRACER_DECOMP
!# define N2O_NEV
# undef IODINE
# define PCO2AIR_FORCING
#endif /* BIOLOGY_BEC2 */
#define DAILYPAR_BEC

    /* Flux Analysis */
!# define BGC_FLUX_ANALYSIS
# define BGC_FLUX_EXT_HIS_OUTPUT
# define PHYS_FLUX_ANALYSIS
!# define FULL_PHYS_FLUX_ANALYSIS
# define VERT_DIFF_ANALYSIS
!# define SELECTED_FLUX_ANALYSIS
# define WRITE_DEPTHS /* For Budget Analysis Closure */

                     
!#define USE_REAL_YEAR /* Only used by age tracers*/
!--> #define VERBOSE
!#define HIS_DOUBLE
!--> #define DEBUG

#include "set_global_definitions.h"

!-- #undef PARALLEL_FILES
!--#undef EXACT_RESTART
