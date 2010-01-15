/* This is "cppdefs.h": MODEL CONFIGURATION FILE
   ==== == ============ ===== ============= ====
*/
#define PACIFIC         /* North-Equatorial Pacific Application */


/*
   Main switch starts here: model configuration choice.
*/
#if defined PACIFIC || defined ATLANTIC /* North-Equatorial Pacific Configuration */
# define SOLVE3D
# define UV_COR
# define UV_ADV
# define CURVGRID
# define SPHERICAL
# define MASKING
# define MASK_LAND_DATA

# define EXACT_RESTART
# define AVERAGES
!                       Equation of State
# define SALINITY
# define NONLIN_EOS
# define SPLIT_EOS

# define QCORRECTION
# define SFLX_CORR
# define SSS_dQdSST   ! dQdSSSt must be read together with
                      ! SSS or not (in this case with SST)

!                       Lateral Mixing
# define UV_VIS2
# define MIX_GP_TS
# undef VIS_GRID

# define TS_DIF2
# define MIX_GP_UV
# undef DIF_GRID
!                       Vertical Mixing
# define LMD_MIXING
# define LMD_KPP
# define LMD_RIMIX
# define LMD_CONVEC
# define LMD_NONLOCAL
# define DIURNAL_SRFLUX
# undef LMD_DDMIX
!                       Open Boundary Conditions
# undef OBC_EAST
# undef OBC_WEST
# undef OBC_NORTH
# undef OBC_SOUTH
# undef OBC_M2FLATHER
# undef OBC_M3ORLANSKI
# undef OBC_TORLANSKI
! restoring layer, needs BOC conditions enabled 
# undef M2NUDGING
# undef M3NUDGING

c--DL:
# ifdef PACIFIC
# define EW_PERIODIC
# define NS_PERIODIC
# undef TNUDGING
# undef TCLIMATOLOGY
# define BIO_1ST_USTREAM_TEST
# define DAILYPAR_BEC
# define DIURNAL_SRFLUX
# ifdef TCLIMATOLOGY
#   define CLIMAT_TS_MIXH
# endif
# undef OBC_M2FLATHER
# undef OBC_M3ORLANSKI
# undef OBC_TORLANSKI
# undef M2NUDGING
# undef M3NUDGING
# undef LMD_NONLOCAL
# define EXACT_RESTART
# define NEW_S_COORD
! Switches related to biology:
!#define BIOLOGY_NPZDOC
#define BIOLOGY_BEC
#define BOTTOM_RESTORE_LAYER 
!#define FOUR_CLIM_FILES
!#define BGC_FLUX_ANALYSIS
!#define PHYS_FLUX_ANALYSIS
!#define VERT_DIFF_ANALYSIS
!#define FULL_PHYS_FLUX_ANALYSIS
!#define PHYS_FLUX_ALL_COMPS 

! Other switches:
#define VFLX_CORR
#endif

# undef SPONGE

#endif

#include "set_global_definitions.h"

#ifndef BIOLOGY_BEC
# undef BIO_1ST_USTREAM_TEST
# undef DAILYPAR_BEC
#endif
