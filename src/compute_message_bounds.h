      integer imin,imax,ishft, jmin,jmax,jshft
# ifdef EW_PERIODIC
      if (NP_XI.eq.1) then                ! this means that if there
        imin=Istr-2                       ! is no partition in XI-
        imax=Iend+2                       ! direction, then periodic
      else                                ! margins are included into
        imin=Istr                         ! the message; otherwise
        imax=Iend                         ! strip them out.
      endif
# else
      if (WESTERN_EDGE) then              ! extra point on either
        imin=Istr-1                       ! side to accomodate ghost
      else                                ! points associated with
        imin=Istr                         ! PHYSICAL boundaries.
      endif
      if (EASTERN_EDGE) then
        imax=Iend+1
      else
        imax=Iend
      endif
# endif
      ishft=imax-imin+1
 
# ifdef NS_PERIODIC
      if (NP_ETA.eq.1) then               ! if no partition in ETA- 
        jmin=Jstr-2                       ! include periodic into the
        jmax=Jend+2                       ! message; otherwise strip 
      else                                ! them out 
        jmin=Jstr
        jmax=Jend
      endif
# else
      if (SOUTHERN_EDGE) then             ! extra point on either
        jmin=Jstr-1                       ! side to accomodate ghost
      else                                ! points associated with
        jmin=Jstr                         ! PHYSICAL boundaries.
      endif
      if (NORTHERN_EDGE) then
        jmax=Jend+1
      else
        jmax=Jend
      endif
# endif
      jshft=jmax-jmin+1
 
