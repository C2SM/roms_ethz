#ifdef PSOURCE
!
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
!
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
