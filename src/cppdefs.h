/* This is "cppdefs.h": MODEL CONFIGURATION FILE
   ==== == ============ ===== ============= ====
*/
# define USWC_CENTRAL

# ifdef USWC_CENTRAL   /* US West Coast Configuration */

!--> #define HINDCAST
!---> #define HCAST_START_CLUTCH

# define SOLVE3D
# define UV_COR
# define UV_ADV

!                       Equation of State
# define NONLIN_EOS
# define SPLIT_EOS
# define SALINITY

!                       Forcing and Climatology
# ifdef SOLVE3D
#   define QCORRECTION
!#   define TCLIMATOLOGY
!#   define UCLIMATOLOGY
!#   define TNUDGING
#   define T_FRC_BRY
#   define Z_FRC_BRY
#   define M2_FRC_BRY
#   define M3_FRC_BRY
#   define MOOREA

!--> #define BIO_SWR_FRAC
!--> #define TIME_VAR_SWR_FRAC
!--> #define KPP_DIAGNOSE

!mm #   define M3NUDGING
!mm #   define M2NUDGING
!mm #   define SPONGE
#   define SFLX_CORR
#   define STFLX_LIM

#   undef ROBUST_DIAG
#   undef ANA_SSFLUX
#   undef ANA_SST
#   undef ANA_SRFLUX
#   undef ANA_STFLUX
#   define DIURNAL_SRFLUX

#   undef DIF_GRID
#   define TS_DIF2
#   define MIX_GP_UV
#   undef TS_DIF4
#   ifdef TCLIMATOLOGY
#      define CLIMAT_TS_MIXH
#   endif
# endif /* SOLVE3D */

                      /* Lateral Mixing */
# undef VIS_GRID
# define UV_VIS2
# define MIX_GP_TS
 
                      /* Vertical Mixing */
# define LMD_MIXING
# define LMD_RIMIX
# define LMD_CONVEC
# undef LMD_DDMIX
# define LMD_KPP
# define LMD_NONLOCAL

                      /* Grid Configuration */
# define CURVGRID
# define SPHERICAL
# define MASKING
# define MASK_LAND_DATA

                      /* Open Boundary Conditions */
# undef OBC_EAST
# undef OBC_WEST
# undef OBC_NORTH
# define OBC_SOUTH
# define OBC_TORLANSKI
# undef OBC_M2FLATHER
# define OBC_M3ORLANSKI
# undef OBC_TSPECIFIED
# define OBC_M2SPECIFIED
# undef OBC_M3SPECIFIED
!
# define AVERAGES
# define SLICE_AVG
!
!                       Biology
# undef BIOLOGY
# define BIOLOGY_NPZDOC
! Switch to calculate carbonsystem
!# define CH_CARBON_DEPTH

# define MULT_CLIM_FILES
# define NEW_S_COORD
# define DIURNAL_SRFLUX
# define VFLX_CORR
# define USE_REAL_YEAR

# include "set_global_definitions.h"

# ifdef BIOLOGY_NPZDOC
#   define VFLX_CORR
#   define DAILYPAR_PHOTOINHIBITION
#   define OXYGEN
#   define OXYLIM
#   define OXYLIM_SED
#   define OCMIP_OXYGENSAT
#   define OCMIP_OXYGEN_SC
#   define CARBON
#   define OCMIP_CARBON
!mm #   define VARIABLE_ATM_PCO2
!--> #   define VARIABLE_ANN_ATM_PCO2
!-->#   define PCO2AIR_FORCING
!--> #   define ONLY_FUTURE_TIME_AT_START_OK

#   define CALC_CARBON_ONCE
#   define USE_PH_SAVED
#   define SEDIMENT_BIOLOGY
! --V-- not on in pacsg npzd bio spinup --V--
!--> #   define BGC_FLUX_ANALYSIS
!--> #   define PHYS_FLUX_ANALYSIS
!--> #   define VERT_DIFF_ANALYSIS
! --^-- not on in pacsg npzd bio spinup --^--
!#   define FULL_PHYS_FLUX_ANALYSIS
!#   define BGC_FLUX_EXT_HIS_OUTPUT
# endif

#  define EXACT_RESTART
!--> # define CALENDAR '365_day'
# define CALENDAR 'gregorian'
!--> # define STARTDATE '1979-01-01'
# endif /* USWC_CENTRAL */

