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


      implicit none

      INTEGER, INTENT( IN    )   :: kid    ! variable intex in the array
      INTEGER, INTENT( IN    )   :: kstep  ! ocean time-step in seconds
      REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE,  INTENT( OUT )   :: pdata
      INTEGER, INTENT(   OUT )   :: kinfo  ! OASIS4 info argument
      !!
      LOGICAL                   :: llaction
      
      !---------------------------------------------------------------------------
      exfld=0.
      exfldu=0.
      exfldv=0.
      
      if(kid==oas_UST) ALLOCATE(pdata(oas_xi_u,oas_eta_u), stat =nerror)
      if(kid==oas_VST) ALLOCATE(pdata(oas_xi_v,oas_eta_v), stat =nerror)
      if(kid>=3) ALLOCATE(pdata(oas_xi,oas_eta), stat =nerror)

      if(kid ==oas_UST) call prism_get_proto( srcv(kid)%nid, kstep, exfldu, kinfo )
      if(kid == oas_VST) call prism_get_proto( srcv(kid)%nid, kstep, exfldv, kinfo )
      if(kid >= 3) call prism_get_proto( srcv(kid)%nid, kstep, exfld, kinfo )

      llaction =.false.
      if( kinfo == PRISM_Recvd   .OR. kinfo == PRISM_FromRest .OR.   &
          kinfo == PRISM_RecvOut .OR. kinfo == PRISM_FromRestOut )   llaction = .TRUE.

      ! If coupling time step
      if ( llaction ) then

         ! Declare to calling routine that OASIS provided coupling field
         kinfo = OASIS_Rcv

         ! Update array which contains coupling field (only on valid shape)
         if(kid == oas_UST) pdata(:,:) = exfldu(:,:)
         if(kid == oas_VST) pdata(:,:) = exfldv(:,:)
         if(kid >= 3) pdata(:,:) = exfld(:,:)
      else
         ! Declare to calling routine that OASIS did not provide coupling field
         kinfo = OASIS_idle     
      endif
      
      print *,'kid pdata',kid,pdata(10,10)
 
      end
