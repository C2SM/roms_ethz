      integer imin,imax,ishft, jmin,jmax,jshft
# ifdef EW_PERIODIC
      if (NP_XI.eq.1) then                ! this means that if there
        imin=istr-2                       ! is no partition in XI-
        imax=iend+2                       ! direction, then periodic
      else                                ! margins are included into
        imin=istr                         ! the message; otherwise
        imax=iend                         ! strip them out.
      endif
# else
      if (WESTERN_EDGE) then              ! extra point on either
        imin=istr-1                       ! side to accomodate ghost
      else                                ! points associated with
        imin=istr                         ! PHYSICAL boundaries.
      endif
      if (EASTERN_EDGE) then
        imax=iend+1
      else
        imax=iend
      endif
# endif
      ishft=imax-imin+1
 
# ifdef NS_PERIODIC
      if (NP_ETA.eq.1) then               ! if no partition in ETA- 
        jmin=jstr-2                       ! include periodic into the
        jmax=jend+2                       ! message; otherwise strip 
      else                                ! them out 
        jmin=jstr
        jmax=jend
      endif
# else
      if (SOUTHERN_EDGE) then             ! extra point on either
        jmin=jstr-1                       ! side to accomodate ghost
      else                                ! points associated with
        jmin=jstr                         ! PHYSICAL boundaries.
      endif
      if (NORTHERN_EDGE) then
        jmax=jend+1
      else
        jmax=jend
      endif
# endif
      jshft=jmax-jmin+1
 
