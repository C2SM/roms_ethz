/*
  UP ETH HUMBOLDT Telescopic Setup - USWC Focus
  == === ======== ========== ===== = ==== =====
*/

     /* Include standard CPP switches for UP ETH Zurich */
#include "cppdefs_UP.h"

#define GRID_SIZE LLm=699, MMm=1007, N=64
#define DOMAIN_TILING NP_XI=8, NP_ETA=48, NSUB_X=1, NSUB_E=1 ! Euler

     /* Open Boundaries */
#define OBC_WEST

     /* Open Boundary Conditions */
#define OBC_S_M2SPEC_STR 000 /* OBC_M2SPECIFIED for a certain range of tiles */
#define OBC_S_M2SPEC_END 004 /* OBC_M2SPECIFIED for a certain range of tiles */
#define OBC_M3ORLANSKI /* Baroclin. BC: OBC_M3ORLANSKI, OBC_M3SPECIFIED */
#define OBC_TORLANSKI /* Tracer BC: OBC_TORLANSKI, OBC_TSPECIFIED */

#define SPONGE
#define SPONGE_WIDTH

     /* Seaice SSS masking and read seaice conc */
#define SALINITY_MASK
#define SALINITY_MASKLATSTR -48.0
#define SALINITY_MASKLATEND -53.01

!--> # define ICEOBS

     /* Output */
!--> # define AVERAGES
#define CALENDAR '365_day'     /* netCDF CF-convention CALENDAR attribute default: '360_day' */
#define STARTDATE '1969-01-01' /* Ana's Hindcast */

     /* Advection*/
#define ADV_ISONEUTRAL

     /* Biology */
!--> #define BIOLOGY_NPZDOC
# define BIOLOGY_BEC2

#ifdef BIOLOGY_BEC2
# define Ncycle_SY
# define N2O_TRACER_DECOMP
!--># define N2O_NEV
# define NO3_UCLA
!--> # define IODINE
# define BIOLOGY
# define DEFAULT_BRY_VALUES
!--> # define MULT_CLIM_FILES
# define VFLX_CORR
# define BEC2_DIAG
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

#undef PARALLEL_FILES
