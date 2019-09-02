      subroutine oas_roms_rcv(kid, kstep, pdata, kinfo)
      !**** *oas_roms_rcv*  - 
!
!     Purpose.
!     --------
!     Receive Fields from Atmospheric model
!
!**   Interface.
!     ----------
!       *CALL*  *oas_roms_rcv*
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
!       
!
!     Externals:
!     ---------
!      oas_roms_vardef
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
      !use mod_prism_get_proto
      use mod_oasis


      implicit none

      INTEGER, INTENT( IN    )   :: kid    ! variable intex in the array
      INTEGER, INTENT( IN    )   :: kstep  ! ocean time-step in seconds
      REAL(KIND=8), INTENT( OUT )   :: pdata(oas_imin:oas_imax, oas_jmin:oas_jmax)
      INTEGER, INTENT(   OUT )   :: kinfo  ! OASIS4 info argument
      !!
      LOGICAL                   :: llaction
      
      !---------------------------------------------------------------------------
      exfld=0.
      exfldu=0.
      exfldv=0.
      
      if(kid == oas_UST) call oasis_get( srcv(kid)%nid, kstep, exfldu, kinfo )
      if(kid == oas_VST) call oasis_get( srcv(kid)%nid, kstep, exfldv, kinfo )
      if(kid >= 3) call oasis_get( srcv(kid)%nid, kstep, exfld, kinfo )

      llaction =.false.
      if( kinfo == PRISM_Recvd   .OR. kinfo == PRISM_FromRest .OR.   &
          kinfo == PRISM_RecvOut .OR. kinfo == PRISM_FromRestOut )   llaction = .TRUE.

      ! If coupling time step
      if ( llaction ) then

         ! Declare to calling routine that OASIS provided coupling field
         kinfo = OASIS_Rcv

         ! Update array which contains coupling field (only on valid shape)
         if(kid == oas_UST) pdata(oas_imin_u:oas_imax_u, oas_jmin_u:oas_jmax_u) = exfldu(:,:)
         if(kid == oas_VST) pdata(oas_imin_v:oas_imax_v, oas_jmin_v:oas_jmax_v) = exfldv(:,:)
         if(kid >= 3) pdata(:,:) = exfld(:,:)
      else
         ! Declare to calling routine that OASIS did not provide coupling field
         kinfo = OASIS_idle     
      endif
 
      end
