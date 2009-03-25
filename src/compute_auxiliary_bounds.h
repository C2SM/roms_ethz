! Auxiliary module "compute_auxiliary_bounds.h":
!---------- ------ -----------------------------
! Compute derived bounds for the loop indices over a subdomain
! "tile". The extended bounds [labelled by suffix R] are designed to
! cover also the outer ghost points, if the subdomain "tile" is
! adjacent to a PHYSICAL boundary. (NOTE: IstrR,IendR,JstrR,JendR
! computed by this module DO NOT COVER ghost points associated with
! periodic boundaries (if any) or with 2-point computational marhins
! of MPI subdomains.
! 
! This module also computes loop-bounds for U- and V-type variables
! which belong to the interior of the computational domain. These are
! labelled by suffixes U,V and they step one grid point inward from
! the side of the subdomain adjacent to the physical boundary.
! Conversely, for an internal subdomain [which does not have segments
! of physical boundary] all variables with suffixes R,U,V are set to
! the same values are the corresponding non-suffixed variables.
! 
! Because this module also contains type declarations for these
! bounds, it must be included just after the last type declaration
! inside a subroutine, but before the first executable statement.
! 
#ifdef EW_PERIODIC
# undef IstrU
# define IstrU Istr
# undef IstrR
# define IstrR Istr
# undef IendR
# define IendR Iend
#else
      integer IstrU, IstrR, IendR
#endif
 
#ifdef NS_PERIODIC
# undef JstrV
# define JstrV Jstr
# undef JstrR
# define JstrR Jstr
# undef JendR
# define JendR Jend
#else
      integer JstrV, JstrR, JendR
#endif
 
#ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
        IstrR=Istr-1
        IstrU=Istr+1
      else
        IstrR=Istr
        IstrU=Istr
      endif
      if (EASTERN_EDGE) then
        IendR=Iend+1
      else
        IendR=Iend
      endif
#endif
 
#ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        JstrR=Jstr-1
        JstrV=Jstr+1
      else
        JstrR=Jstr
        JstrV=Jstr
      endif
      if (NORTHERN_EDGE) then
        JendR=Jend+1
      else
        JendR=Jend
      endif
#endif
 
