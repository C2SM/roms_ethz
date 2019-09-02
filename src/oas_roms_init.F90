   SUBROUTINE oas_roms_init

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
       USE oas_roms_vardef

       IMPLICIT NONE
   
       CHARACTER(LEN=6)   :: MODNAME = 'ROMSCO'    ! Name of the model

      
      !------------------------------------------------------------------
      ! 1st Initialize the PRISM system for the component
      !------------------------------------------------------------------
      CALL prism_init_comp_proto( ncomp_id, MODNAME, nerror )
      IF( nerror /= 0 )   CALL prism_abort_proto( ncomp_id, 'oas_roms_init', 'Failure in prism_init_comp' )

      !------------------------------------------------------------------
      ! 2nd Get an MPI communicator for local communication
      !------------------------------------------------------------------
      CALL prism_get_localcomm_proto( kl_comm, nerror )
      IF( nerror /= 0 )   CALL prism_abort_proto( ncomp_id, 'oas_roms_init', 'Failure in prism_get_localcomm' )


   END SUBROUTINE oas_roms_init
