/* This is "cppdefs.h": MODEL CONFIGURATION FILE
   ==== == ============ ===== ============= ====
*/

# define humpac15

# ifdef humpac15

                       /*  Dynamics */
# define SOLVE3D
# define UV_COR
# define UV_ADV

                       /*  Equation of State */
# define NONLIN_EOS
# define SPLIT_EOS
# define SALINITY
!# define USE_GLOBAL_SRF_SUM

                       /*  Forcing and Climatology */
# define QCORRECTION
# define SFLX_CORR
                       /*  BRY Settings */
# define BRY_FRC
# ifdef BRY_FRC
#   define T_FRC_BRY
#   define Z_FRC_BRY
#   define M3_FRC_BRY
#   define M2_FRC_BRY
# endif
                       /*  CLM Settings */
!# define CLM_FRC
# ifdef CLM_FRC
#   define TCLIMATOLOGY
#   define UCLIMATOLOGY
#   define TNUDGING
#   define M3NUDGING
#   define M2NUDGING
#   define SPONGE
#   ifdef TCLIMATOLOGY
#      define CLIMAT_TS_MIXH
#   endif
# endif

# define STFLX_LIM  /* this switch CHECKS min Temp!!!! */
# define DIURNAL_SRFLUX

                      /* Seaice SSS masking and read seaice conc */
# define SALINITY_MASK
!# define ICEOBS
                      /* Lateral Mixing */
# define UV_VIS2
# define TS_DIF2
# define MIX_GP_TS
# define MIX_GP_UV

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
!# define MASK_LAND_DATA

                      /* Open Boundary Conditions */
# define OBC_WEST
!# define OBC_SOUTH
# define OBC_TORLANSKI
# define OBC_M2FLATHER
!# define OBC_M2SPECIFIED
# define OBC_M3ORLANSKI

# define AVERAGES
# define SLICE_AVG

                      /* Biology */
# define BIOL
# ifdef BIOL
#   define BIOLOGY_BEC2
#   define UP_PARAMS /* UP ROMS BEC2 parameter set*/
!#   define CESM_PARAMS /* CESM BEC parameter set */
#   define BEC2_DIAG
!#   define BUDGETVARS /* BEC2_DIAG Budget Analysis variables needed */
!#   define RIVER_LOAD_BIO
! Switch to calculate carbonsystem
!#  define CH_CARBON_DEPTH
!#   define MULT_CLIM_FILES
#   define VFLX_CORR
#   define PCO2AIR_FORCING
# endif
                      /* New sigma coords */
# define NEW_S_COORD

                      /* Flux Analysis */
!# define BGC_FLUX_ANALYSIS
!# define PHYS_FLUX_ANALYSIS
!# define FULL_PHYS_FLUX_ANALYSIS
!# define VERT_DIFF_ANALYSIS
!# define SELECTED_FLUX_ANALYSIS
!# define WRITE_DEPTHS /* For Budget Analysis Closure */

                     /*Time and Calendar*/
# define EXACT_RESTART
# define USE_REAL_YEAR
!# define CALENDAR '365_day'
!# define CALENDAR '360_day'
# define STARTDATE '1979-01-01'
# endif /* USWC_CENTRAL */

!--> # define WORKSTATION
# include "set_global_definitions.h"
