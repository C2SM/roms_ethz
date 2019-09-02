!+ Source module for utility routines
!==============================================================================

MODULE oas_roms_utilities

!==============================================================================

CONTAINS

!==============================================================================


!==============================================================================
!==============================================================================
!+ Function for rotation of geographical coordinates
!------------------------------------------------------------------------------

FUNCTION  phirot2phi ( phirot, rlarot, polphi, pollam, polgam )

!------------------------------------------------------------------------------
!
! Description:
!   This function converts phi from one rotated system to phi in another
!   system. If the optional argument polgam is present, the other system
!   can also be a rotated one, where polgam is the angle between the two
!   north poles.
!   If polgam is not present, the other system is the real geographical
!   system.
!
! Method:
!   Transformation formulas for converting between these two systems.
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!
! Declarations:
!
!------------------------------------------------------------------------------

USE oas_roms_vardef, ONLY : &
  ireals,                 &
  iintegers

! Parameter list:
REAL (KIND=ireals), INTENT (IN)      ::        &
  polphi,   & ! latitude of the rotated north pole
  pollam,   & ! longitude of the rotated north pole
  phirot,   & ! latitude in the rotated system
  rlarot      ! longitude in the rotated system

REAL (KIND=ireals), INTENT (IN)      ::        &
  polgam      ! angle between the north poles of the systems

REAL (KIND=ireals)                   ::        &
  phirot2phi  ! latitude in the geographical system

! Local variables
REAL (KIND=ireals)                   ::        &
  zsinpol, zcospol, zphis, zrlas, zarg, zgam

REAL (KIND=ireals), PARAMETER        ::        &
  zrpi18 = 57.2957795_ireals,                  &
  zpir18 = 0.0174532925_ireals

!------------------------------------------------------------------------------

! Begin function phirot2phi

  zsinpol     = SIN (zpir18 * polphi)
  zcospol     = COS (zpir18 * polphi)
 
  zphis       = zpir18 * phirot
  IF (rlarot > 180.0_ireals) THEN
    zrlas = rlarot - 360.0_ireals
  ELSE
    zrlas = rlarot
  ENDIF
  zrlas       = zpir18 * zrlas

  IF (polgam /= 0.0_ireals) THEN
    zgam  = zpir18 * polgam
    zarg  = zsinpol*SIN (zphis) +                                           &
        zcospol*COS(zphis) * ( COS(zrlas)*COS(zgam) - SIN(zgam)*SIN(zrlas) )
  ELSE
    zarg  = zcospol * COS (zphis) * COS (zrlas) + zsinpol * SIN (zphis)
  ENDIF
 
  phirot2phi  = zrpi18 * ASIN (zarg)

END FUNCTION phirot2phi

!==============================================================================
!==============================================================================

FUNCTION  rlarot2rla (phirot, rlarot, polphi, pollam, polgam)

!------------------------------------------------------------------------------
!
! Description:
!   This function converts lambda from one rotated system to lambda in another
!   system. If the optional argument polgam is present, the other system
!   can also be a rotated one, where polgam is the angle between the two
!   north poles.
!   If polgam is not present, the other system is the real geographical
!   system.
!
! Method:
!   Transformation formulas for converting between these two systems.
!
! Modules used:    NONE
!
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!
! Declarations:
!
!------------------------------------------------------------------------------

USE oas_roms_vardef, ONLY : &
  ireals,                 &
  iintegers


! Parameter list:
REAL (KIND=ireals), INTENT (IN)      ::        &
  polphi,   & ! latitude of the rotated north pole
  pollam,   & ! longitude of the rotated north pole
  phirot,   & ! latitude in the rotated system
  rlarot      ! longitude in the rotated system

REAL (KIND=ireals), INTENT (IN)      ::        &
  polgam      ! angle between the north poles of the systems

REAL (KIND=ireals)                   ::        &
  rlarot2rla  ! longitude in the geographical system

! Local variables
REAL (KIND=ireals)                   ::        &
  zsinpol, zcospol, zlampol, zphis, zrlas, zarg1, zarg2, zgam

REAL (KIND=ireals), PARAMETER        ::        &
  zrpi18 = 57.2957795_ireals,                  & !
  zpir18 = 0.0174532925_ireals

!------------------------------------------------------------------------------

! Begin function rlarot2rla

  zsinpol = SIN (zpir18 * polphi)
  zcospol = COS (zpir18 * polphi)

  zlampol = zpir18 * pollam
  zphis   = zpir18 * phirot
  IF (rlarot > 180.0_ireals) THEN
    zrlas = rlarot - 360.0_ireals
  ELSE
    zrlas = rlarot
  ENDIF
  zrlas   = zpir18 * zrlas

  IF (polgam /= 0.0_ireals) THEN
    zgam    = zpir18 * polgam
    zarg1   = SIN (zlampol) *                                                &
      (- zsinpol*COS(zphis) * (COS(zrlas)*COS(zgam) - SIN(zrlas)*SIN(zgam))  &
       + zcospol * SIN(zphis))                                               &
    - COS (zlampol)*COS(zphis) * (SIN(zrlas)*COS(zgam) + COS(zrlas)*SIN(zgam))

    zarg2   = COS (zlampol) *                                                &
      (- zsinpol*COS(zphis) * (COS(zrlas)*COS(zgam) - SIN(zrlas)*SIN(zgam))  &
       + zcospol * SIN(zphis))                                               &
    + SIN (zlampol)*COS(zphis) * (SIN(zrlas)*COS(zgam) + COS(zrlas)*SIN(zgam))
  ELSE
    zarg1   = SIN (zlampol) * (-zsinpol * COS(zrlas) * COS(zphis)  +    &
                                zcospol *              SIN(zphis)) -    &
              COS (zlampol) *             SIN(zrlas) * COS(zphis)
    zarg2   = COS (zlampol) * (-zsinpol * COS(zrlas) * COS(zphis)  +    &
                                zcospol *              SIN(zphis)) +   &
              SIN (zlampol) *             SIN(zrlas) * COS(zphis)
  ENDIF
 
  IF (zarg2 == 0.0) zarg2 = 1.0E-20_ireals
 
  rlarot2rla = zrpi18 * ATAN2(zarg1,zarg2)
 
END FUNCTION rlarot2rla

!==============================================================================
!==============================================================================

END MODULE oas_roms_utilities
