      integer imin,imax,ishft, jmin,jmax,jshft
# ifdef EW_PERIODIC
      if (NP_XI.eq.1) then                ! This means that if there
        imin=istr-2                       ! is no partition in XI-
        imax=iend+2                       ! direction, then periodic
      else                                ! margins are included into
        imin=istr                         ! the message;
        imax=iend                         ! otherwise strip them out.
      endif
# else
      if (inode.eq.0 .and. istr.eq.1) then ! Extra point on either
        imin=istr-1                        ! side to accomodate ghost
      else                                 ! points associated with
        imin=istr                          ! PHYSICAL boundaries.
      endif
      if (inode.eq.NP_XI-1 .and. iend.eq.Lm) then
        imax=iend+1
      else
        imax=iend
      endif
# endif
      ishft=imax-imin+1
 
# ifdef NS_PERIODIC
      if (NP_ETA.eq.1) then               ! This means that if there
        jmin=jstr-2                       ! is no partition in ETA-
        jmax=jend+2                       ! direction, then periodic
      else                                ! margins are included into
        jmin=jstr                         ! the message;
        jmax=jend                         ! otherwise strip them out.
      endif
# else
      if (jnode.eq.0 .and. jstr.eq.1) then ! Extra point on either
        jmin=jstr-1                        ! side to accomodate ghost
      else                                 ! points associated with
        jmin=jstr                          ! PHYSICAL boundaries.
      endif
      if (jnode.eq.NP_ETA-1 .and. jend.eq.Mm) then
        jmax=jend+1
      else
        jmax=jend
      endif
# endif
      jshft=jmax-jmin+1
 
