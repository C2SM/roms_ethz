! Dimensions of Physical Grid and array dimensions:
!----------- -- -------- ---- --- ----- -----------
! LLm   Number of the internal points of the PHYSICAL grid in XI-
! MMm   and ETA-directions, excluding physical side boundary points,
!       peroodic ghost points, and MPI-margins (if any). 
!
! Lm    Number of the internal points [see above] of array covering
! Mm    a single MPI-subdomain.  These are identical to LLm,MMm there 
!       is no MPI-partitioning. 
!
      integer  LLm, MMm, N
# if defined GRID_LEVEL && GRID_LEVEL < MAX_GRID_LEVEL
     &    , imin_child,imax_child, jmin_child,jmax_child
# endif
      parameter (
#if defined BASIN
     &               LLm=180, MMm=140, N=12
#elif defined CANYON_A
     &               LLm=65,  MMm=48,  N=10
#elif defined CANYON_B
     &               LLm=66,  MMm=48,  N=12
#elif defined DAMEE_B
     &               LLm=128, MMm=128, N=20
c**  &               LLm=256, MMm=256, N=20
#elif defined GRAV_ADJ
     &               LLm=128, MMm=2,   N=32
c     &               LLm=256, MMm=2,   N=64
#elif defined NJ_BIGHT
c*   &               LLm=98,  MMm=206, N=12
     &               LLm=24,  MMm=34,  N=10
#elif defined PACIFIC
c    &               LLm=384, MMm=224, N=30
c    &               LLm=392, MMm=288, N=30

c    &               LLm=432, MMm=320, N=32
     &               LLm=488, MMm=360, N=32

c    &               LLm=768, MMm=512, N=40
#elif defined ATL360X408
     &               LLm=360,  MMm=408,  N=32
#elif defined ATL50S70N
     &               LLm=360,  MMm=468,  N=32

#elif defined PACIFIC_2D
     &               LLm=768, MMm=512, N=1
c    &               LLm=1520, MMm=1088, N=1

#elif defined OVERFLOW
     &               LLm=4,   MMm=128, N=20
#elif defined SEAMOUNT
     &               LLm=40,  MMm=40,  N=20
c     &               LLm=80,  MMm=80,  N=30
c    &               LLm=192, MMm=96,  N=22
#elif defined ISWAKE
c     &               LLm=192, MMm=84, N=20
c     &               LLm=384, MMm=168, N=20
     &               LLm=360, MMm=160, N=10
c     &               LLm=768, MMm=336, N=20
c     &              LLm=1536, MMm=672, N=20 
#elif defined SHELFRONT
     &               LLm=4,   MMm=40,  N=12
#elif defined SOLITON
c     &               LLm=96,  MMm=32,  N=1
c     &                 LLm=97, MMm=33, N=1
c     &               LLm=192, MMm=64,  N=1
     &               LLm=384, MMm=128, N=1
c     &                LLm=576, MMm=192, N=1
c     &               LLm=768, MMm=256, N=1
#elif defined BRAZIL 
     &               LLm=168, MMm=272, N=40

#elif defined TASMAN_SEA
     &               LLm=128, MMm=128, N=4
#elif defined RIVER
     &               LLm=40,  MMm=160,  N=16
c     &                LLm=60,  MMm=240,  N=24
#elif defined UPWELLING
     &               LLm=20,  MMm=80,  N=16
#elif defined HUMBOLDT
     &               LLm=384, MMm=800, N=30       ! SAWC 8 km
#elif defined USWEST
# ifdef GRID_LEVEL
#  if GRID_LEVEL == 1
     &               LLm=83,  MMm=168, N=20,         ! Monteray Bay,
     &               imin_child=40,  imax_child=71,  ! Level 1, 15 km
     &               jmin_child=54,  jmax_child=117
#  elif GRID_LEVEL == 2
     &               LLm=93,  MMm=189, N=20          ! Level 2, 5 km 
#  endif 
# else  

                     
c>>  &               LLm=72,   MMm=240, N=32    ! PEC2 of Xavier
c**  &               LLm=62,   MMm=126, N=40    ! SCB L0 grid 
c**  &               LLm=83,   MMm=168, N=20    ! MB_L1
c**  &               LLm=126,  MMm=254, N=20    ! USWEST grid 16 
     &		     LLm=62,   MMm=126, N=20    ! USWEST grid 15 (20 km)
!HF     &               LLm=312,  MMm=512, N=32    ! USW8 - lev0, 8km

# endif /* GRID_LEVEL */
#elif defined USWC_CENTRAL
     &               LLm=83, MMm=168, N=20
#elif defined WAVE_RAD
     &              LLm=384,  MMm=384, N=1
#else
     &                LLm=??, MMm=??, N=??
#endif
     &                                      )
!
! Domain subdivision parameters:
!------- ----------- -----------
! NNODES          total number of MPI processes (nodes);
! NP_XI,  NP_ETA  number of MPI subdomains in XI-, ETA-directions;
! NSUB_X, NSUB_E  number of shared memory subdomains (tiles) in
!                                      XI- and ETA-directions;
      integer NSUB_X, NSUB_E
#ifdef MPI
      integer NP_XI, NP_ETA, NNODES
      parameter (NP_XI=4, NP_ETA=8, NSUB_X=1, NSUB_E=1)
      parameter (NNODES=NP_XI*NP_ETA)
#else
c        parameter (NSUB_X=4, NSUB_E=60)
c      parameter (NSUB_X=1, NSUB_E=1)
       parameter (NSUB_X=2, NSUB_E=22)
#endif
!
! Array dimensions and bounds of the used portions of sub-arrays
!
      integer Lm,Mm, irc
#ifdef MPI
      parameter (Lm=(LLm+NP_XI-1)/NP_XI, Mm=(MMm+NP_ETA-1)/NP_ETA)
      logical west_inter, east_inter, south_inter, north_inter
      integer ocean_grid_comm, mynode, iwest, ieast, jsouth, jnorth,
     &                                 iSW_corn,     jSW_corn
      common /mpi_comm_vars/  ocean_grid_comm, mynode, iwest, ieast,
     &        jsouth, jnorth, iSW_corn, jSW_corn, irc,
     &        west_inter, east_inter, south_inter, north_inter
#else
      parameter (Lm=LLm, Mm=MMm, irc=1)
#endif


!
! Number of tracers and tracer identification indices:
!------- -- ------- --- ------ -------------- --------
!
#ifdef SOLVE3D
      integer NT, itemp, ntrc_salt, ntrc_pas, ntrc_bio
# ifdef SALINITY
     &          , isalt
# endif
# if defined BIOLOGY || defined BIOLOGY_NPZDOC
     &          , itrc_bio
#  ifdef SEDIMENT_BIOLOGY
     &     , NT_sed
#  endif
# endif /* BIOLOGY || BIOLOGY_NPZDOC */
# ifdef BIOLOGY
     &          , iNO3_, iNH4_, iDet_, iPhyt, iZoo_
# endif
# ifdef BIOLOGY_NPZDOC
     &          , iNO3_, iNH4_, iChla, iPhyt, iZoo_
     &          , iSDet, iLDet
#  ifdef OXYGEN
     &          , iO2
#   ifdef CARBON
     &          , iDIC, iTALK, iSDetC, iLDetC, iCaCO3
#   endif /* CARBON */
#  endif /* OXYGEN */
#  ifdef SEDIMENT_BIOLOGY
     &          , iSedOrgN
#   ifdef CARBON
     &          , iSedOrgC, iSedCaCO3
#   endif /* CARBON */
#  endif /* SEDIMENT_BIOLOGY */
# endif /* BIOLOGY_NPZDOC */
      parameter (itemp=1
# ifdef SALINITY
     &           , isalt=2, ntrc_salt=1
# else
     &           , ntrc_salt=0
# endif
     &     )
# ifdef PASSIVE_TRACER
      parameter (ntrc_pas=20)
      integer itpas(ntrc_pas)
      common /pass_tracer/ itpas
# else
      parameter (ntrc_pas=0)
# endif

# ifdef BIOLOGY
      parameter (ntrc_bio=5, itrc_bio=itemp+ntrc_salt+ntrc_pas+1
     &           , iNO3_=itrc_bio, iNH4_=iNO3_+1, iDet_=iNO3_+2
     &           , iPhyt=iNO3_+3, iZoo_=iNO3_+4)
# elif !defined BIOLOGY_NPZDOC
      parameter (ntrc_bio=0)
# endif

# ifdef BIOLOGY_NPZDOC
      parameter (itrc_bio=itemp+ntrc_salt+ntrc_pas+1) 
      parameter (iNO3_=itrc_bio, 
     &            iNH4_=iNO3_+1, iChla=iNO3_+2, iPhyt=iNO3_+3,
     &            iZoo_=iNO3_+4, iSDet=iNO3_+5, iLDet=iNO3_+6)
#   ifdef OXYGEN
      parameter(iO2 = iLDet + 1) ! Oxygen
#    ifdef CARBON
      parameter(iDIC = iO2 + 1)      ! Total inorganic carbon
      parameter(iTALK = iDIC + 1)    ! Alkalinity 
      parameter(iSDetC = iTALK + 1)  ! Small detritus carbon
      parameter(iLDetC = iSDetC + 1) ! Large detritus carbon
      parameter(iCaCO3 = iLDetC + 1) ! CaCO3
      parameter (ntrc_bio = 13)
#    else /* CARBON */
      parameter (ntrc_bio = 8)
#    endif /* CARBON */
#   else /* OXYGEN */
      parameter (ntrc_bio = 7)
#   endif /* OXYGEN */
#  ifdef SEDIMENT_BIOLOGY
      parameter(iSedOrgN = 1)
#   ifdef CARBON
      parameter(iSedOrgC = iSedOrgN + 1)
      parameter(iSedCaCO3 = iSedOrgC + 1)
      parameter (NT_sed=3)
#   else
      parameter (NT_sed=1)
#   endif
#  endif /* SEDIMENT_BIOLOGY */
# endif /* BIOLOGY_NPZDOC */

      parameter (NT=itemp+ntrc_salt+ntrc_pas+ntrc_bio)

#endif /*SOLVE3D */


#ifdef PSOURCE
      integer Msrc         ! Number of point sources, if any
      parameter (Msrc=10)
#endif
#ifdef STATIONS
      integer NS           ! Number of output stations (if any).
      parameter (NS=5)
#endif
#ifdef FLOATS
       integer Mfloats          ! Maximum number of floats
       parameter (Mfloats=32000)! ====== == ====== ========
#endif
!
! Derived dimension parameters.
!
      integer stdout, padd_X,padd_E
      parameter (stdout=6, padd_X=(Lm+2)/2-(Lm+1)/2,
     &                     padd_E=(Mm+2)/2-(Mm+1)/2)
 
