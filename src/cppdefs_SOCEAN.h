/* 
  UP ETH Southern Ocean Setup 
  == === ======== ===== =====
*/
         /* Include standard CPP switches for UP ETH Zurich */                   
#include "cppdefs_UP.h"
#define GRID_SIZE LLm=720, MMm=216, N=42
#define DOMAIN_TILING  NP_XI=8, NP_ETA=36, NSUB_X=1, NSUB_E=1

DOMAIN_TILING

         /* Open Boundary Conditions */
#define OBC_M2FLATHER
#define OBC_M3ORLANSKI
#define OBC_TORLANSKI

         /* Forcing */
#define SALINITY_MASK

         /* Open Boundaries */
#define EW_PERIODIC
#define OBC_NORTH

         /* Open Boundary Conditions */
#undef SPONGE /* note: UCLA uses SPONGE with BRY */

#undef OBC_M2FLATHER
/* Barotop. BC: OBC_M2FLATHER, OBC_M2ORLANSKI, OBC_M2SPECIFIED */
#define OBC_M2SPECIFIED 

         /* Output */
#define AVERAGES
#define SLICE_AVG
#define COMPUTE_SPEED_DIAGNOSE

        /* Flux Analysis */
#undef PHYS_FLUX_ANALYSIS
#undef FULL_PHYS_FLUX_ANALYSIS
#undef VERT_DIFF_ANALYSIS
#undef SELECTED_FLUX_ANALYSIS
/* #define PASSIVE_TRACER */
/* #define SWFLUX_TRACER */

        /* Biology */
#define BIOLOGY_BEC2
#define BEC2_DIAG
#define RIVER_LOAD_N
#define RIVER_LOAD_P
#define PCO2AIR_FORCING
/* # define MULT_CLIM_FILES */
# define VFLX_CORR

/* #define SO */ /* used in param.h */

#include "set_global_definitions.h"

