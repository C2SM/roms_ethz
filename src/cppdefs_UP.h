/*
   Standard UP ETH Zurich Settings for Regional and Basin Configurations
*/
                     /* Basics */
#define SOLVE3D
#define UV_ADV
#define UV_COR
                     /*  Equation of State */
#define NONLIN_EOS
#define SPLIT_EOS
#define SALINITY
                     /*  Forcing */
                     /*         - surface */
#define DIURNAL_SRFLUX
#define QCORRECTION
                     /*         - lateral */
#define T_FRC_BRY
#define Z_FRC_BRY
#define M3_FRC_BRY
#define M2_FRC_BRY
#define SPONGE
#define SPONGE_WIDTH /* # of sponge points is input parameter */
                     /* Mixing */
                     /*        - lateral */
#define UV_VIS2
#define TS_DIF2
                     /*        - vertical */
#define LMD_MIXING
#define LMD_KPP
#define LMD_NONLOCAL
#define LMD_RIMIX
#define LMD_CONVEC

                      /* Grid Configuration */
#define CURVGRID
#define SPHERICAL
#define MASKING

                      /* Output Options */
#define MASK_LAND_DATA

                      /* Restart */
!--> #define EXACT_RESTART

         /* Open Boundary Conditions */
#define OBC_M2FLATHER
#define OBC_M3ORLANSKI
#define OBC_TORLANSKI

/* End of UP ETH Standard Settings */
