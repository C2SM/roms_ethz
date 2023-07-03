#define DOMAIN_TILING NP_XI=8, NP_ETA=8, NSUB_X=1, NSUB_E=1 ! Euler
#define UPWELLING
#define SOLVE3D
#define UV_ADV
#define UV_COR
#define MIX_GP_UV
#define SALINITY

#undef LMD_VMIX
#define MIX_GP_TS
#define EW_PERIODIC
#undef NONLIN_EOS
 
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_SSFLUX
# define ANA_SRFLUX
# define ANA_VMIX
# define NO_RESTART
# define NO_HISTORY

  /* *** ACTIVATE BIOLOGY HERE *** */
  /* (Otherwise standard UP bio switches are not set) */
!--> # define BIOLOGY_BEC2

  /* cppdef_UP.h contains the standard switches at UP ETH Zurich */  
#include "cppdefs_UP.h"

 /* Undefine some standard switches that require netCDF input */
#undef SPHERICAL
#undef EXACT_RESTART
#undef T_FRC_BRY
#undef Z_FRC_BRY
#undef M3_FRC_BRY
#undef M2_FRC_BRY
#undef LMD_MIXING
#undef LMD_BKPP


     /* Output */
!--> #define AVERAGES
!--> #define ADV_ISONEUTRAL

     /* Biology */
!--> #define BIOLOGY_NPZDOC

#ifdef BIOLOGY_BEC2
# define DAILYPAR_BEC
# define USE_EXPLICIT_VSINK
# define DEFAULT_BRY_VALUES
# define VFLX_CORR /* MF: make sure this is always on if running with BIOLOGY */
# define BEC2_DIAG
# define RIVER_LOAD_ALK_DIC_SI
# define RIVER_LOAD_N
# define RIVER_LOAD_P
# define PCO2AIR_FORCING
# define NHY_FORCING
# define NOX_FORCING
#endif /* BIOLOGY_BEC2 */


#include "set_global_definitions.h"

