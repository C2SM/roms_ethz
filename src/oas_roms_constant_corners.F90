SUBROUTINE oas_roms_constant_corners (lonc, latc, startlonc, startlatc,xi_rho,eta_rho)

USE oas_roms_vardef, ONLY : &
  ireals,                 &
  iintegers,		  &
  pollat,		  &
  pollon,		  &
  polgam,                 &
  dlat,			  &
  dlon                    
  
USE oas_roms_utilities, ONLY :  &
  phirot2phi,      &
  rlarot2rla

IMPLICIT NONE

INTEGER (KIND=iintegers)  ::  &
  i, j, xi_rho, eta_rho

REAL (KIND=ireals), INTENT (IN)    :: &
  startlonc,       &
  startlatc

REAL (KIND=ireals), INTENT (OUT)   :: &
  lonc(xi_rho,eta_rho),    &
  latc(xi_rho,eta_rho)
 
REAL(KIND=ireals), PARAMETER :: pi       = 4.0_ireals * ATAN (1.0_ireals)

! Local variables:
REAL (KIND=ireals)        ::  &
  zlats, zlons

!------------------------------------------------------------------------------
!- Begin SUBROUTINE constant_corners
!------------------------------------------------------------------------------

DO j=1, eta_rho
  DO i=1, xi_rho
   zlats  = startlatc + j * dlat
   zlons  = startlonc + i * dlon

   IF (zlons  > 180.0) THEN
     zlons  = zlons  - 360.0_ireals
   ENDIF

   latc(i,j) = phirot2phi ( zlats , zlons , pollat, pollon, polgam)
   lonc(i,j) = rlarot2rla ( zlats , zlons , pollat, pollon, polgam) 

   ENDDO
ENDDO


END SUBROUTINE oas_roms_constant_corners


