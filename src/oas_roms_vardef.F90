   MODULE oas_roms_vardef

   !**** *oas_roms_vardef * - Controls, definitions and variables
   !                   for OASIS communications
   !
   !     AUTHOR.
   !     -------
   !     2007-1-30 Kristian Mogensen

   !     MODIFICATIONS.
   !     --------------
   !     2010-3  Eric Maisonnave: OASIS4
   !     2010-11 Eric Maisonnave: COSMO-CLM coupling
   !       A. Dobler: adapted to OASIS3
   !	 2012-03 D. Byrne: Roms Coupling

   !USE mpi


     USE mod_oasis
     !USE mod_prism_proto


   IMPLICIT NONE

   SAVE

   ! Debug level of OASIS
   !     0 : Minimum debugging
   !     1 : Debugging
   !     2 : Perfs measurement
   !     3 : OASIS restart production

   INTEGER(KIND=4) :: IOASISDEBUGLVL = 0

  INTEGER, PARAMETER       ::                                         &
       ireals    = SELECTED_REAL_KIND (12,200),                       &
                     ! number of desired significant digits for
                     ! real variables
                     ! corresponds to 8 byte real variables

       iintegers = KIND  (1)
                     ! kind-type parameter of the integer values
                     ! corresponds to the default integers

   ! Variable ids

   INTEGER(KIND=4)            :: ncomp_id           ! id returned by prism_init_comp
   INTEGER(KIND=4)            :: kl_comm            ! Local communicator 
   INTEGER(KIND=4)            :: nerror             ! return error code
   INTEGER(KIND=4), PUBLIC    :: OASIS_Rcv  = 1     ! return code if received field
   INTEGER(KIND=4), PUBLIC    :: OASIS_idle = 0     ! return code if nothing done by oasis
   INTEGER, PUBLIC    :: PRISM_Success = 0  ! return code if no error in oasis

   !Local sizes of grids
   INTEGER(KIND=4)   :: oas_xi, oas_eta, oas_imin, oas_imax, oas_jmin, oas_jmax, oas_start(2)
   INTEGER(KIND=4)   :: oas_xi_u, oas_eta_u, oas_imin_u, oas_imax_u, oas_jmin_u, oas_jmax_u, oas_start_u(2)
   INTEGER(KIND=4)   :: oas_xi_v, oas_eta_v, oas_imin_v, oas_imax_v, oas_jmin_v, oas_jmax_v, oas_start_v(2)

   INTEGER(KIND=4), PARAMETER :: nmaxfld=40    ! Maximum number of coupling fields
   INTEGER(KIND=4)            :: ksnd, krcv, rcv_st    ! Number of send/received coupling fields
   INTEGER(kind=4), DIMENSION(:), ALLOCATABLE :: cplcount !number of steps between coupling for each field.

   TYPE, PUBLIC ::   FLD_CPL                 ! Type for coupling field information
      LOGICAL            ::        laction   ! To be coupled or not
      CHARACTER(len = 8) ::        clname    ! Name of the coupling field
      CHARACTER(len = 1) ::        clgrid    ! Grid type
      INTEGER(KIND=4) ::   nid       ! Id of the field
   END TYPE FLD_CPL

   TYPE(FLD_CPL), DIMENSION(nmaxfld), PUBLIC :: srcv, ssnd   ! Number of Coupling fields

   REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: exfld,exfldu,exfldv  ! Temporary buffer for receiving

   REAL(KIND=8), DIMENSION(:,:,:),ALLOCATABLE  ::   frcv  ! all Rho grid fields recieved 
   REAL(KIND=8), DIMENSION(:,:),ALLOCATABLE  ::   frcvu,frcvv  ! U,V fields recieved

   REAL(KIND=8), PARAMETER ::       dlon=0.05
   REAL(KIND=8), PARAMETER ::       dlat=0.05
   REAL(KIND=8), PARAMETER ::	    startlon_tot=-9 + 3.0_ireals*dlon !Move starting point to start of forcast grid in cosmo
   REAL(KIND=8), PARAMETER ::       startlat_tot=-6.5 + 3.0_ireals*dlat !Move starting point to start of forcast grid in cosmo
   REAL(KIND=8), PARAMETER ::       pollon=58
   REAL(KIND=8), PARAMETER ::       pollat=54
   REAL(KIND=8), PARAMETER ::       polgam=0

   

   REAL(KIND=8), DIMENSION(:,:,:),ALLOCATABLE  ::   fsndar  ! all fields sent to ocean model (averaged over coupling time step)

   !Sent Fields
   INTEGER, PARAMETER ::   oas_itemp = 1
  
   !Receieved Fields
   INTEGER, PARAMETER ::   oas_UST  =  1           !u-momentum flux (surface)          ( N/m2)
   INTEGER, PARAMETER ::   oas_VST  =  2           ! v-momentum flux (surface)          ( N/m2)
   INTEGER, PARAMETER ::   oas_NHF  =  3           ! Net heat flux (surface)            ( W/m2) 
   INTEGER, PARAMETER ::   oas_SWR  =  4           ! direct shortwave downward radiation (W/m2)
   INTEGER, PARAMETER ::   oas_TEP  =  5           ! Total Evaporation -Precipitation   (kg/m2*s)



END MODULE oas_roms_vardef
