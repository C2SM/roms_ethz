/* This is "cppdefs.h": MODEL CONFIGURATION FILE
   ==== == ============ ===== ============= ====
*/

#define SAEP

#ifdef SAEP

# define MULT_CLIM_FILES /* to avoid compile error */

# define SOLVE3D
# define UV_COR
# define UV_ADV
!                       Equation of State
# define NONLIN_EOS
# define SPLIT_EOS
# define SALINITY

!# define EXACT_RESTART
# define AVERAGES
!# define DIURNAL_SRFLUX
#define STFLX_LIM

               /*  Forcing and Climatology */

#   define QCORRECTION
#   define TCLIMATOLOGY
#   define UCLIMATOLOGY
#   define TNUDGING
#   define M3NUDGING
#   define M2NUDGING
#   define SPONGE

                      /* Lateral Mixing */

# define UV_VIS2
# define MIX_GP_TS
 
!# ifdef SOLVE3D
#   define TS_DIF2
#   define MIX_GP_UV
!# endif
                      /* Vertical Mixing */
# define LMD_MIXING
# define LMD_RIMIX
# define LMD_CONVEC
# define LMD_KPP
# define LMD_NONLOCAL

                      /* Grid Configuration */
# define CURVGRID
# define SPHERICAL
# define MASKING
# define MASK_LAND_DATA

                      /* Open Boundary Conditions */
# define OBC_EAST
# define OBC_WEST
# define OBC_NORTH
# define OBC_SOUTH
!c--> # define OBC_VOLCONS
!c--> # define OBC_FLUX_CORR
 
# define OBC_M2FLATHER
# define OBC_M3ORLANSKI
# define OBC_TORLANSKI

!
!# define AVERAGES
! new sigma coords:
# define NEW_S_COORD

/* Flux Analysis */
!# define PHYS_FLUX_ANALYSIS
!# define FULL_PHYS_FLUX_ANALYSIS

!                       Biology
#endif /* SAEP */

/* ------------------------------------ */
#include "set_global_definitions.h"

