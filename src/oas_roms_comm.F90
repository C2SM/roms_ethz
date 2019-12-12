MODULE oas_roms_comm

   ! Description
   ! -----------
   ! This module contains subroutines for initializing and finalizing MPI communications

   ! Subroutines
   ! -----------
   ! SUBROUTINE oas_roms_init()
   !
   ! SUBROUTINE oas_roms_finalize()

   ! Used modules
   ! ------------
   USE mod_oasis_method, ONLY:   &
      oasis_init_comp,           &
      oasis_terminate

   USE mod_oasis_auxiliary_routines, ONLY:   &
      oasis_get_localcomm

   USE mod_oasis_sys, ONLY:   &
      oasis_abort

   
   IMPLICIT NONE

   PRIVATE

   PUBLIC :: oas_roms_init, oas_roms_finalize

   ! Module variables
   ! ----------------
   INTEGER(KIND=4), SAVE, PUBLIC :: kl_comm   ! Local communicator
   INTEGER(KIND=4), SAVE, PUBLIC :: ncomp_id   ! id returned by oasis_init_comp
   INTEGER, PUBLIC :: OASIS_Success=0   ! return code if no error in oasis

   
   CONTAINS

   ! *********************************************************************************** !
   !                                 PUBLIC SUBROUTINES                                  !
   ! *********************************************************************************** !

   SUBROUTINE oas_roms_init()

      !**** *INIOASIS*  - Initialize coupled mode communication
      !
      !     Purpose.
      !     --------
      !     Initialize coupler to get the MPI communicator
      !
      !**   Interface.
      !     ----------
      !       *CALL*  *oas_roms_init*
      !
      !     Input:
      !     -----
      !
      !     Output:
      !     ------
      !      
      !
      !     Method:
      !     ------
      !       OASIS usage is controlled by environment variables
      !
      !     Externals:
      !     ---------
      !       GETENV - Get enviroment variables
      !       prism_init, prism_init_comp, prism_get_localcomm, prism_abort : prism library
      !
      !     Reference:
      !     ---------
      !       S. Valcke, R. Redler, 2007: OASIS4 User Guide ,
      !       PRISM Support Initiative Report No 4,
      !       CERFACS, Toulouse, France, 60 pp.
      !
      !     Author:
      !     -------
      !       D. Byrne ; ETHZ
      !
      !     Modifications.
      !     --------------
      !

      CHARACTER(LEN=*), PARAMETER :: MODNAME = 'ROMSOC'   ! Name of the model
      INTEGER(KIND=4) :: ierror   ! return error code


      !------------------------------------------------------------------
      ! 1st Initialize the PRISM system for the component
      !------------------------------------------------------------------
      CALL oasis_init_comp( ncomp_id, MODNAME, ierror )
      IF( ierror /= OASIS_Success )  THEN
         CALL oasis_abort( ncomp_id, 'oas_roms_init', 'Failure in prism_init_comp' )
      END IF

      !------------------------------------------------------------------
      ! 2nd Get an MPI communicator for local communication
      !------------------------------------------------------------------
      CALL oasis_get_localcomm( kl_comm, ierror )
      IF( ierror /= OASIS_Success ) THEN
         CALL oasis_abort( ncomp_id, 'oas_roms_init', 'Failure in prism_get_localcomm' )
      END IF


   END SUBROUTINE oas_roms_init

   ! ------------------------------------------------------------------------------------!

   SUBROUTINE oas_roms_finalize()

      !!---------------------------------------------------------------------
      !!              ***  ROUTINE oas_roms_finalize  ***
      !!
      !! ** Purpose : - Finalizes the coupling. If MPI_init has not been
      !!      called explicitly before oas_roms_init it will also close
      !!      MPI communication.
      !!----------------------------------------------------------------------
      USE oas_roms_vardef

      INTEGER(KIND=4) :: ierror   ! return error code

      !write(6,*) 'call prism_terminate '
      call flush(6)
      !DEALLOCATE(exfld)
      CALL oasis_terminate( ierror )         

   END SUBROUTINE oas_roms_finalize


END MODULE oas_roms_comm
