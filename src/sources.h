#ifdef PSOURCE

! Nsrc       Number of point Sources/Sinks.
! Dsrc       Direction of point Sources/Sinks:  0 = along XI-;
!                                          1= along ETA-direction.
! Isrc,Jsrc  i,j-grid location of point Sources/Sinks,
!              0 <= Isrc <= Lm+1;   0 =<- Jsrc <= Mm+1.
! Lsrc       Logical switch indicating which tracer field to apply
!                                         the point Sources/Sinks.
! Qsrc       Mass transport profile (m3/s) of point Sources/Sinks.
! Qbar       Vertically integrated Qsrc (m3/s) of point
! QbarG      Latest two-time snapshots of vertically integrated
!              mass transport (m3/s) of point Sources/Sinks.
! Tsrc       Tracer (tracer units) point Sources/Sinks.
! TsrcG      Latest two-time snapshots of tracer (tracer units)
!              point Sources/Sinks.
! Qshape     Nondimensional shape function to distribute mass
!             ass point Sources/Sinks vertically.

      real Qbar(Msrc), Qsrc(Msrc,N),  Qshape(Msrc,N),
     &                                Tsrc(Msrc,N,NT)
# ifndef ANA_PSOURCE
     &             , QbarG(Msrc,2), TsrcG(Msrc,N,2,NT)
# endif
      integer Nsrc, Dsrc(Msrc), Isrc(Msrc), Jsrc(Msrc)
      logical Lsrc(Msrc,NT)
      common /sources_all/ Qbar, Qsrc, Qshape, Tsrc,
# ifndef ANA_PSOURCE
     &                                 QbarG,  TsrcG,
# endif
     &                 Nsrc, Dsrc, Isrc, Jsrc, Lsrc
#endif

! DL:
#if defined PASSIVE_TRACER && defined AGE_DYE_TRACER
!
! N_agedye: number of age/dye tracers (must be <= ntrc_pas)
! Max_src_ad: maximum number of sources/sinks per age/dye tracer (parameter)
! Isrc_ad, Jsrc_ad, Ksrc_ad: location of each source (i and j denote cells in the
!    horizontal physical grid)
! Ssrc_ad: source strength (M/s, where M is the unit used for tracer concentration)
! Dsrc_ad: if a flux bc is used for corresponding tracer, then this array gives the
!     direction of the flux (0: along xi   1: along eta). Directions other than along
!     the horizontal coordinate axes are not possible.
! Age: true if tracer is an age tracer, otherwise false
! Restore_ad: if set to true, a restoring bc will be used for the corresponding tracer
!     otherwise a flux bc is used
! Tnudgcof_ad: time scale for restoring [1/s] (used only for tracers with restoring bc)
! bc_val: contains values to be used for the bc (i.e. fluxes if a flux bc is used,
!     otherwise tracer concentrations)
! rest_where: logical array, true at points where the bcs are to be applied, false
!     otherwise. This is optional. If not present, bcs will be applied at all cells
!     which contain sources.
! 
      integer N_agedye, Max_src_ad, Nsrc_ad(ntrc_pas)
      parameter (Max_src_ad=5000)
      integer Isrc_ad(ntrc_pas,Max_src_ad), Jsrc_ad(ntrc_pas,Max_src_ad)
      integer Ksrc_ad(ntrc_pas,Max_src_ad)
      integer inode, jnode, k_ad, l, it_ad(ntrc_pas)
      logical Age(ntrc_pas), Restore_ad(ntrc_pas)
      real Ssrc_ad(ntrc_pas,Max_src_ad), Switch_off_src_ad(ntrc_pas,Max_src_ad)
      real Switch_on_src_ad(ntrc_pas,Max_src_ad)
      real ad_time(ntrc_pas,2)
      real ad_cycle(ntrc_pas)
      real bc_valg(GLOBAL_2D_ARRAY,N,2,ntrc_pas)
      integer, dimension(ntrc_pas):: nt_ad, ad_ncycle, ad_rec
      logical switch_is_days_ad(ntrc_pas)
      common /sources_ad/ ad_time, Ssrc_ad, ad_cycle, bc_valg, Switch_on_src_ad, Switch_off_src_ad
      common /sources_ad/ N_agedye, Nsrc_ad, Age, Restore_ad
      common /sources_ad/ Isrc_ad, Jsrc_ad, Ksrc_ad
      common /sources_ad/ nt_ad, it_ad, ad_ncycle, ad_rec, switch_is_days_ad
#endif /* PASSIVE_TRACER && AGE_DYE_TRACER */
