/*
   Standard UP ETH Zurich Settings for Regional and Basin Configurations
   #### PLEASE do not change but use undef in cppdefs.h to undefine 
   #### what as needed e.g., put #undef SPONGE in your personal cppdefs.h
*/
                     /* Basics */
#define SOLVE3D
#define UV_ADV
#define UV_COR
                     /*  Equation of State */
#define NONLIN_EOS
#define SPLIT_EOS
#define SALINITY
#define SFLX_CORR
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
#define OBC_M2FLATHER  /* Barotop. BC: OBC_M2FLATHER, OBC_M2ORLANSKI, OBC_M2SPECIFIED */
#define OBC_M3ORLANSKI /* Baroclin. BC: OBC_M3ORLANSKI, OBC_M3SPECIFIED */
#define OBC_TORLANSKI  /* Tracer BC: OBC_TORLANSKI, OBC_TSPECIFIED */

                      /* Biology Settings */
#ifdef BIOLOGY_BEC2
# define BIOLOGY
# define DAILYPAR_BEC
#endif


/* End of UP ETH Standard Settings */
