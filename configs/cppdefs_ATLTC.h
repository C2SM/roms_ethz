/*
  UP ETH Atlantic Telescopic Setup
  == === ======== ========== =====
*/
         /* Include standard CPP switches for UP ETH Zurich */
#include "cppdefs_UP.h"

         /* Open Boundaries */
#define OBC_WEST
#define OBC_SOUTH

                      /* Open Boundary Conditions */
#undef SPONGE /* note: UCLA uses SPONGE with BRY */
#undef OBC_M2FLATHER  /* undefine default in cppdefs_UP.h */
#define OBC_M2SPECIFIED

                      /* Output */
#define AVERAGES
#define SLICE_AVG
#define CALENDAR '365_day'     /* netCDF CF-convention CALENDAR attribute */
#define STARTDATE '1979-01-01' /* netCDF CF-convention time units attribute */


                      /* Biology */
#define BIOLOGY_NPZDOC

#ifdef BIOLOGY_NPZDOC
# define MULT_CLIM_FILES
# define VFLX_CORR
# define USE_REAL_YEAR
# define VFLX_CORR
# define DAILYPAR_PHOTOINHIBITION
# define OXYGEN
# define OXYLIM
# define OXYLIM_SED
# define OCMIP_OXYGENSAT
# define OCMIP_OXYGEN_SC
# define CARBON
# define OCMIP_CARBON
# define CALC_CARBON_ONCE
# define USE_PH_SAVED
# define SEDIMENT_BIOLOGY
# define PCO2AIR_FORCING
#endif /* BIOLOGY_NPZDOC
                      /* Flux Analysis */
!!# define BGC_FLUX_ANALYSIS
!!#define BGC_FLUX_EXT_HIS_OUTPUT
!# define PHYS_FLUX_ANALYSIS
!!# define FULL_PHYS_FLUX_ANALYSIS
!# define VERT_DIFF_ANALYSIS
!# define SELECTED_FLUX_ANALYSIS
!# define WRITE_DEPTHS /* For Budget Analysis Closure */


!#define USE_REAL_YEAR /* Only used by age tracers*/

#include "set_global_definitions.h"
