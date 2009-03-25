! Auxiliary module "compute_extended_bounds.h":
!---------- ------ ----------------------------
! Bounds which cover interior points of an array together with
! ghost points of PHYSICAL side boundaries and halo points associated
! with computational margins of MPI-subdomains.
!
#ifdef IstrR
# undef IstrR
#endif
#ifdef IendR
# undef IendR
#endif
#ifdef JstrR
# undef JstrR
#endif
#ifdef JendR
# undef JendR
#endif
#ifndef MPI
# define iwest 1
# define ieast Lm
# define jsouth 1
# define jnorth Mm
#endif




      integer IstrR,IendR,JstrR,JendR
      if (Istr.eq.iwest) then
# ifdef EW_PERIODIC
        IstrR=Istr-2
# else
#  ifdef MPI
        if (WEST_INTER) then
          IstrR=Istr-2
        else
          IstrR=Istr-1
        endif
#  else
        IstrR=Istr-1
#  endif
# endif
      else
        IstrR=Istr
      endif
 
      if (Iend.eq.ieast) then
# ifdef EW_PERIODIC
        IendR=Iend+2
# else
#  ifdef MPI
        if (EAST_INTER) then
          IendR=Iend+2
        else
          IendR=Iend+1
        endif
#  else
        IendR=Iend+1
#  endif
# endif
      else
        IendR=Iend
      endif
 
      if (Jstr.eq.jsouth) then
# ifdef NS_PERIODIC
        JstrR=Jstr-2
# else
#  ifdef MPI
        if (SOUTH_INTER) then
          JstrR=Jstr-2
        else
          JstrR=Jstr-1
        endif
#  else
        JstrR=Jstr-1
#  endif
# endif
      else
        JstrR=Jstr
      endif
 
      if (Jend.eq.jnorth) then
# ifdef NS_PERIODIC
        JendR=Jend+2
# else
#  ifdef MPI
        if (NORTH_INTER) then
          JendR=Jend+2
        else
          JendR=Jend+1
        endif
#  else
        JendR=Jend+1
#  endif
# endif
      else
        JendR=Jend
      endif

#ifndef MPI
# undef iwest
# undef ieast
# undef jsouth
# undef jnorth
#endif
 
 
