   SUBROUTINE oas_roms_allocate


!     Purpose.
!     --------
!     Allocate Memory for arrays to store received fields between coupling steps
!
!**   Interface.
!     ----------
!       *CALL*  *oas_roms_allocate*
!
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


      !Line up array indexing to be consistent with roms internal indexing, 

      ALLOCATE( frcv(oas_imin:oas_imax, oas_jmin:oas_jmax, krcv), stat = nerror )
      IF ( nerror > 0 ) THEN
         CALL oasis_abort( ncomp_id, 'oas_roms_define', 'Failure in allocating frcv' )
         RETURN
      ENDIF

      ALLOCATE( exfld(oas_imin:oas_imax, oas_jmin:oas_jmax), stat = nerror )
      IF ( nerror > 0 ) THEN
         CALL oasis_abort( ncomp_id, 'oas_roms_define', 'Failure in allocating exfld' )
         RETURN
      ENDIF

      ALLOCATE( exfldu(oas_imin_u:oas_imax_u, oas_jmin_u:oas_jmax_u), stat = nerror )
      IF ( nerror > 0 ) THEN
         CALL oasis_abort( ncomp_id, 'oas_roms_define', 'Failure in allocating exfld' )
         RETURN
      ENDIF

      ALLOCATE( exfldv(oas_imin_v:oas_imax_v, oas_jmin_v:oas_jmax_v), stat = nerror )
      IF ( nerror > 0 ) THEN
         CALL oasis_abort( ncomp_id, 'oas_roms_define', 'Failure in allocating exfld' )
         RETURN
      ENDIF


      ! Allocate array to store snd fields between two coupling steps for averaging
      ALLOCATE( fsndar(oas_imin:oas_imax, oas_jmin:oas_jmax, ksnd), stat = nerror )
      IF ( nerror > 0 ) THEN
         CALL oasis_abort( ncomp_id, 'oas_roms_define', 'Failure in allocating fsndar' )
         RETURN
      ENDIF

      ALLOCATE( cplcount(ksnd), stat = nerror )
      IF ( nerror > 0 ) THEN
         CALL oasis_abort( ncomp_id, 'oas_roms_define', 'Failure in allocating cplcount' )
         RETURN
      ENDIF

     frcv(:,:,:)=0.
     exfld(:,:)=0.
     exfldu(:,:)=0.
     exfldv(:,:)=0.
     cplcount(:) = 0
     fsndar(:,:,:) = 0.


   END SUBROUTINE oas_roms_allocate
