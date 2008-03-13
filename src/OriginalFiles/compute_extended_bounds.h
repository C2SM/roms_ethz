! Auxiliary module "compute_extended_bounds.h":
!---------- ------ ----------------------------
! Bounds which cover interior points of an array together with
! ghost points of PHYSICAL side boundaries and halo points associated
! with computational margins of MPI-subdomains.
!
#ifdef istrR
# undef istrR
#endif
#ifdef iendR
# undef iendR
#endif
#ifdef jstrR
# undef jstrR
#endif
#ifdef jendR
# undef jendR
#endif
#ifndef MPI
# define iwest 1
# define ieast Lm
# define jsouth 1
# define jnorth Mm
#endif




      integer istrR,iendR,jstrR,jendR
      if (istr.eq.iwest) then
# ifdef EW_PERIODIC
        istrR=istr-2
# else
#  ifdef MPI
        if (WEST_INTER) then
          istrR=istr-2
        else
          istrR=istr-1
        endif
#  else
        istrR=istr-1
#  endif
# endif
      else
        istrR=istr
      endif
 
      if (iend.eq.ieast) then
# ifdef EW_PERIODIC
        iendR=iend+2
# else
#  ifdef MPI
        if (EAST_INTER) then
          iendR=iend+2
        else
          iendR=iend+1
        endif
#  else
        iendR=iend+1
#  endif
# endif
      else
        iendR=iend
      endif
 
      if (jstr.eq.jsouth) then
# ifdef NS_PERIODIC
        jstrR=jstr-2
# else
#  ifdef MPI
        if (SOUTH_INTER) then
          jstrR=jstr-2
        else
          jstrR=jstr-1
        endif
#  else
        jstrR=jstr-1
#  endif
# endif
      else
        jstrR=jstr
      endif
 
      if (jend.eq.jnorth) then
# ifdef NS_PERIODIC
        jendR=jend+2
# else
#  ifdef MPI
        if (NORTH_INTER) then
          jendR=jend+2
        else
          jendR=jend+1
        endif
#  else
        jendR=jend+1
#  endif
# endif
      else
        jendR=jend
      endif

#ifndef MPI
# undef iwest
# undef ieast
# undef jsouth
# undef jnorth
#endif
 
 
