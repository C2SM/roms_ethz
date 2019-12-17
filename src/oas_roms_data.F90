MODULE oas_roms_data
   ! Description
   ! -----------
   ! Module holding variables to be used in different parts of the OASIS interface

   IMPLICIT NONE

   PRIVATE

   ! Communication variables
   ! -----------------------
   INTEGER(KIND=4), SAVE, PUBLIC :: kl_comm   ! Local communicator
   INTEGER(KIND=4), SAVE, PUBLIC :: ncomp_id   ! id returned by oasis_init_comp
   INTEGER, PARAMETER, PUBLIC :: OASIS_Success=0   ! return code if no error in oasis
   
   ! OASIS debug level
   ! -----------------
   INTEGER(KIND=4), PUBLIC, SAVE :: IOASISDEBUGLVL   ! OASIS debug level
   !                                                 ! 0 : Minimum debugging
   !                                                 ! 1 : Debugging
   !                                                 ! 2 : Perfs measurement
   !                                                 ! 3 : OASIS restart production

   ! Coupling grids
   ! --------------
   TYPE, PUBLIC :: OAS_GRID
      CHARACTER(len=3) :: pt   ! point string
      CHARACTER(len=4) :: grd_name   ! Grid name
      INTEGER :: part_id=-999   ! Id of the OASIS partition
      INTEGER :: imin, imax, jmin, jmax   ! local indices
      INTEGER, DIMENSION(2) :: dims_l   ! local dimensions (could get from local indices)
      INTEGER, DIMENSION(2) :: dims_g   ! global dimensions
      INTEGER, DIMENSION(2) :: start_g   ! global starting indices of local grid
      REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: exfld   ! tmp buffer for receiving data
   END TYPE OAS_GRID
   INTEGER, PARAMETER, PUBLIC :: k_rho=1, k_u=2, k_v=3
   TYPE(OAS_GRID), SAVE, DIMENSION(3), PUBLIC :: cpl_grd

   ! Exchanged fields
   ! ----------------
   TYPE, PUBLIC :: FLD_CPL
      LOGICAL :: laction=.FALSE.   ! To be coupled or not, by default not
      CHARACTER(len=8) :: clname   ! Name of the coupling field
      INTEGER(KIND=4) :: nid   ! Id of the field given by OASIS
      INTEGER(KIND=4) :: k_pt   ! Index of the grid on which the field is defined
      REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: pdata   ! actual field data
   END TYPE FLD_CPL
   
   INTEGER(KIND=4), PARAMETER :: nmaxfld=40   ! Maximum number of coupling fields
   INTEGER(KIND=4), SAVE, PUBLIC :: ksnd=0, krcv=0   ! Number of send/received coupling fields
   TYPE(FLD_CPL), DIMENSION(nmaxfld), PUBLIC :: srcv, ssnd   ! Coupling fields

   ! Sent fields ids
   INTEGER, SAVE, PUBLIC :: oas_itemp   ! sea surface temperature [K]
   ! received fields ids
   INTEGER, SAVE, PUBLIC :: oas_UST_U   ! | 
   INTEGER, SAVE, PUBLIC :: oas_VST_U   ! |  oas_XST_Y corresponds to COSMO X-wind STress
   INTEGER, SAVE, PUBLIC :: oas_UST_V   ! |  at ROMS Y-points (momentum flux) [N/m2]
   INTEGER, SAVE, PUBLIC :: oas_VST_V   ! |
   INTEGER, SAVE, PUBLIC :: oas_NHF     ! Net Heat Flux [W/m2]
   INTEGER, SAVE, PUBLIC :: oas_SWR     ! direct ShortWave downward Radiation [W/m2]
   INTEGER, SAVE, PUBLIC :: oas_TEP     ! Total Evaporation - Precipitation [kg/m2*s]

   ! ROMSOC auxiliary variables
   ! --------------------------
   REAL(KIND=8), SAVE, DIMENSION(:,:), ALLOCATABLE, PUBLIC ::   &
      & alpha_rho, alpha_u, alpha_v,   &   ! coupling coefficients
      & u_cos_proj_u, v_cos_proj_u,    &   ! Cosmo velocitiy projection at u-points
      & u_cos_proj_v, v_cos_proj_v         ! Cosmo velocitiy projection at v-points
   
END MODULE oas_roms_data
