! Dimensions of Physical Grid and array dimensions:
!----------- -- -------- ---- --- ----- -----------
! LLm   Number of the internal points of the PHYSICAL grid in XI-
! MMm   and ETA-directions, excluding physical side boundary points,
!       peroodic ghost points, and MPI-margins (if any).
!
! Lm    Number of the internal points [see above] of array covering
! Mm    a single MPI-subdomain.  These are identical to LLm, MMm if
!       there is no MPI-partitioning.

      integer, parameter ::
#if defined BASIN
     &               LLm=180, MMm=140, N=12
#elif defined GRID_SIZE
     &               GRID_SIZE ! LLm, MMm, N are set in cppdefs
#if defined DOUBLE_GYRE
c     &               LLm=192, MMm=192, N=12
     &                LLm=384, MMm=384, N=16
#elif defined CANYON_A
     &               LLm=65,  MMm=48,  N=10
#elif defined CANYON_B
     &               LLm=66,  MMm=48,  N=12
#elif defined DAMEE_B
     &               LLm=128, MMm=128, N=20
c**  &               LLm=256, MMm=256, N=20
#elif defined EKMAN_SPIRAL
     &               LLm=2, MMm=2, N=30

#elif defined GRAV_ADJ
     &               LLm=160, MMm=2,   N=40

#elif defined COLD_FILAMENT
     &               LLm=400, MMm=2,   N=40
c     &                LLm=800, MMm=2,   N=80
#elif defined NJ_BIGHT
c*   &               LLm=98,  MMm=206, N=12
     &               LLm=24,  MMm=34,  N=10
c     &                LLm=800, MMm=2,  N=80
#elif defined ATLANTIC
     &            LLm=1218,  MMm=896, N=50 !<-- 1.5km, full Gulf
c     &             LLm=1536,  MMm=1024, N=50
c     &           LLm=1024,  MMm=1536, N=60


#elif defined PACIFIC

c    &               LLm=384, MMm=224, N=30
c    &               LLm=392, MMm=288, N=30

c    &               LLm=432, MMm=320, N=32
c     &               LLm=488, MMm=360, N=40  ! PAC44
     &                 LLm=976, MMm=720, N=40 ! PAC22

c     &               LLm=460, MMm=240, N=50  ! PACSMA grid
#elif defined ONE_DIM
     &               LLm=2, MMm=2, N=32

#elif defined PACIFIC_2D
     &               LLm=768, MMm=512, N=1
c    &               LLm=1520, MMm=1088, N=1

#elif defined OVERFLOW
     &               LLm=4,   MMm=128, N=20
#elif defined SEAMOUNT
     &               LLm=41,  MMm=41,  N=30
c     &               LLm=80,  MMm=80,  N=30
c    &               LLm=192, MMm=96,  N=22
#elif defined ISWAKE
c     &               LLm=192, MMm=84, N=20
c     &               LLm=384, MMm=168, N=20
c**     &              LLm=768, MMm=288, N=2
     &              LLm=1920, MMm=576, N=2
c     &              LLm=3840, MMm=1152, N=2

#elif defined SHELFRONT
     &               LLm=4,   MMm=40,  N=12
#elif defined SOLITON
c     &               LLm=96,  MMm=32,  N=1
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
c     &                 LLm=20,  MMm=80,  N=24
     &                 LLm=20,  MMm=160,  N=48


c     &               LLm=20,  MMm=80,  N=32
#elif defined CANBAS2
c     &                LLm=224, MMm=288, N=32      ! CanBas
c     &                LLm=225, MMm=328, N=32      ! NEA_EXT
c     &                LLm=384, MMm=480, N=32      ! GranCan
     &                LLm=1200, MMm=1800, N=42      ! UNPR
#elif defined SPIRAL
     &                LLm=694, MMm=972, N=84      ! SPIRAL
#elif defined PACBIG
     &                LLm=1840, MMm=960, N=32      ! PAC_big
#elif defined ATLSMA
     &                LLm= 420, MMm= 560, N=42      ! ATLSMA

#elif defined BALTIC
     &                LLm=440, MMm=384, N=32

#elif defined HUMBOLDT
     &               LLm=384, MMm=800, N=30       ! SAWC 8 km
#elif defined HBCS60
     &               LLm=119, MMm=215, N=20       ! 25km test setup
#elif defined HBCS5
     &               LLm=399, MMm=616, N=32       ! 5km setup
#elif defined SAWC
     &               LLm=384, MMm=800, N=30       ! HBCS 7.5km setup
c================   ALEX
#elif defined SO_d05
     &               LLm=720,  MMm=216, N=42     ! SO_d05
#elif defined SO_d025
     &               LLm=1440, MMm=432, N=42     ! SO_d025
#elif defined SO_d0125
     &               LLm=2880, MMm=864, N=42     ! SO_d0125
c================
#elif defined USWC
c     &    LLm=400,  MMm=512, N=40  ! US West Coast 2010 (L1, 5km)
c     &    LLm=512,  MMm=400, N=40  ! US West Coast 2010 (L2, 1km)
c     &    LLm=1120, MMm=560, N=40  ! US West Coast 2010 (L3, 0.25km)
c     &    LLm=640,  MMm=400, N=32  ! US West Coast 2010 (L4 SMB, 75m)
     &     LLm=1600, MMm=560, N=32  ! US West Coast 2010 (L4 PV, 75 m)


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
     &               LLm=62,   MMm=126, N=20    ! USWEST grid 15 (20 km)
!HF     &               LLm=312,  MMm=512, N=32    ! USW8 - lev0, 8km
     &               LLm=312,  MMm=512, N=32    ! USW51 - lev0

# endif /* GRID_LEVEL */
#elif defined USWC_CENTRAL
     &               LLm=20, MMm=40, N=32        ! 60km config
c**     &               LLm=80, MMm=168, N=32       ! 15km config
c**     &               LLm=248, MMm=504, N=32      ! 5km config
c**     &               LLm=248, MMm=504, N=42      ! 5km config
#elif defined PACTC
     &               LLm=416, MMm=346, N=42      ! 5km-66km telescopic
#elif defined WAVE_RAD
     &              LLm=384,  MMm=384, N=1
#else
     &                LLm=??, MMm=??, N=??
#endif


! Domain subdivision parameters:
!------- ----------- -----------
! NNODES             total number of MPI processes (nodes);
! NP_XI,  NP_ETA     number of MPI subdomains in XI-, ETA-directions;
! NSUB_X, NSUB_E     number of shared memory subdomains (tiles) in
!                                             XI- and ETA-directions;
#ifdef MPI
      integer, parameter ::
# ifdef DOMAIN_TILING /* tiling parameter are set in cppdefs */
     &  DOMAIN_TILING 
# elif defined SO_d05
     &   NP_XI=8, NP_ETA=36, NSUB_X=1, NSUB_E=1
# elif defined SO_d025
     &   NP_XI=16, NP_ETA=24, NSUB_X=1, NSUB_E=1
# elif defined SO_d0125
     &   NP_XI=32, NP_ETA=12, NSUB_X=1, NSUB_E=1
# elif defined ATLTC25 || ATLEL25
     &   NP_XI=12, NP_ETA=16, NSUB_X=1, NSUB_E=1
# elif defined ATLSMA
     &   NP_XI=4, NP_ETA=16, NSUB_X=1, NSUB_E=1
# elif defined HUMBOLDT
     &    NP_XI=2, NP_ETA=16, NSUB_X=1, NSUB_E=1
# elif defined HBCS60
     &    NP_XI=8, NP_ETA=8, NSUB_X=1, NSUB_E=1
# elif defined HBCS5
     &    NP_XI=8, NP_ETA=8, NSUB_X=1, NSUB_E=1
# elif defined SAWC
     &    NP_XI=4, NP_ETA=16, NSUB_X=1, NSUB_E=1
# elif defined USWC_CENTRAL
!    &    NP_XI=1, NP_ETA=4, NSUB_X=1, NSUB_E=1
     &    NP_XI=10, NP_ETA=24, NSUB_X=1, NSUB_E=1
# elif defined PACSG
     &    NP_XI=10, NP_ETA=24, NSUB_X=1, NSUB_E=1
# elif defined USWC_CENTRAL
     &    NP_XI=2, NP_ETA=4, NSUB_X=1, NSUB_E=1  ! 60km 
c     &    NP_XI=4, NP_ETA=24, NSUB_X=1, NSUB_E=1  ! 15km & 5km 
# elif defined USTC90
     &    NP_XI=8, NP_ETA=12, NSUB_X=1, NSUB_E=1
#elif defined SO_d05
     &               LLm=720,  MMm=216, N=42     ! SO_d05
#elif defined SO_d025
     &               LLm=1440, MMm=432, N=42     ! SO_d025
#elif defined SO_d0125
     &               LLm=2880, MMm=864, N=42     ! SO_d0125
     &      NP_XI=8, NP_ETA=32, NSUB_X=1, NSUB_E=3
#else
c     &      NSUB_X=4, NSUB_E=40  ! PAC44
c     &      NSUB_X=8, NSUB_E=80   ! PAC22
     &      NSUB_X=3, NSUB_E=32
c     &      NSUB_X=1, NSUB_E=4
c     &      NSUB_X=4, NSUB_E=26  ! <-- 384x128 soliton
c     &      NSUB_X=4, NSUB_E=52
c     &      NSUB_X=2, NSUB_E=8  ! <-- iswake 768x192
# endif /* DOMAIN_TILING */
#endif

! Array dimensions and bounds of the used portions of sub-arrays

#ifdef MPI
      integer, parameter :: NNODES=NP_XI*NP_ETA,
     &    Lm=(LLm+NP_XI-1)/NP_XI, Mm=(MMm+NP_ETA-1)/NP_ETA

      integer ocean_grid_comm, mynode,  iSW_corn, jSW_corn,
     &                         iwest, ieast, jsouth, jnorth
# ifndef EW_PERIODIC
      logical west_exchng,  east_exchng
# endif
# ifndef NS_PERIODIC
      logical south_exchng, north_exchng
# endif
      common /mpi_comm_vars/  ocean_grid_comm, mynode,
     &     iSW_corn, jSW_corn, iwest, ieast, jsouth, jnorth
# ifndef EW_PERIODIC
     &                , west_exchng,  east_exchng
# endif
# ifndef NS_PERIODIC
     &                , south_exchng, north_exchng
# endif
#else
      integer, parameter :: Lm=LLm, Mm=MMm
#endif

! Derived dimension parameters, number of tracers and tracer
! identification indices:

      integer, parameter :: padd_X=(Lm+2)/2-(Lm+1)/2,
     &                      padd_E=(Mm+2)/2-(Mm+1)/2
#ifdef SOLVE3D
     &       , itemp=1
# ifdef PASSIVE_TRACER
     &, ntrc_pas=4
! in pre 2015 code:     &, itpas(ntrc_pas)
! in pre 2015 code:     common /pass_tracer/ itpas
# else 
     &, ntrc_pas=0
# endif /* PASSIVE_TRACER */
# ifdef SALINITY
     &       , isalt=2, ntrc_salt=1
     &       , itrc_bio=itemp+ntrc_salt+ntrc_pas+1 
#  ifdef BIOLOGY
     &       , ntrc_bio=5
     &       , iNO3_=3, iNH4_=4, iDet_=5, iPhyt=6, iZoo_=7
#  elif defined BIOLOGY_NPZDOC
     &       , iNO3_=itrc_bio
     &       , iNH4_=iNO3_+1, iChla=iNO3_+2, iPhyt=iNO3_+3
     &       , iZoo_=iNO3_+4, iSDet=iNO3_+5, iLDet=iNO3_+6
#   ifdef OXYGEN
     &       , iO2 = iLDet + 1     ! Oxygen
#    ifdef CARBON
     &       , iDIC = iO2 + 1      ! Total inorganic carbon
     &       , iTALK = iDIC + 1    ! Alkalinity 
     &       , iSDetC = iTALK + 1  ! Small detritus carbon
     &       , iLDetC = iSDetC + 1 ! Large detritus carbon
     &       , iCaCO3 = iLDetC + 1 ! CaCO3
     &       , ntrc_bio = 13
#    else /* CARBON */
     &       , ntrc_bio = 8
#    endif /* CARBON */
#   else /* OXYGEN */
     &       , ntrc_bio = 7
#   endif /* OXYGEN */
#   ifdef SEDIMENT_BIOLOGY
     &       , iSedOrgN = 1
#    ifdef CARBON
     &       , iSedOrgC = iSedOrgN + 1
     &       , iSedCaCO3 = iSedOrgC + 1
     &       , NT_sed=3
#    else
      &      , NT_sed=1
#    endif
#   endif /* SEDIMENT_BIOLOGY */
# elif defined BIOLOGY_BEC
     &       , iPO4=itrc_bio, 
     &       , iNO3=iPO4+1, iSIO3=iPO4+2, iNH4=iPO4+3
     &       , iFE=iPO4+4, iO2=iPO4+5, iDIC=iPO4+6
     &       , iALK=iPO4+7, iDOC=iPO4+8, iSPC=iPO4+9
     &       , iSPCHL=iPO4+10, iSPCACO3=iPO4+11, iDIATC=iPO4+12
     &       , iDIATCHL=iPO4+13, iZOOC=iPO4+14, iSPFE=iPO4+15
     &       , iDIATSI=iPO4+16, iDIATFE=iPO4+17, iDIAZC=iPO4+18
     &       , iDIAZCHL=iPO4+19, iDIAZFE=iPO4+20, iDON=iPO4+21
     &       , iDOFE=iPO4+22, iDOP=iPO4+23
     &       , ntrc_bio=24
# elif defined BIOLOGY_BEC2
     &       , iPO4=itrc_bio
     &       , iNO3=iPO4+1, iSIO3=iPO4+2, iNH4=iPO4+3
     &       , iFE=iPO4+4, iO2=iPO4+5, iDIC=iPO4+6
     &       , iALK=iPO4+7, iDOC=iPO4+8, iDON=iPO4+9
     &       , iDOFE=iPO4+10, iDOP=iPO4+11, iDOPR=iPO4+12
     &       , iDONR=iPO4+13, iZOOC=iPO4+14
     &       , iSPCHL=iPO4+15, iSPC=iPO4+16, iSPFE=iPO4+17
     &       , iSPCACO3=iPO4+18, iDIATCHL=iPO4+19, iDIATC=iPO4+20
     &       , iDIATFE=iPO4+21, iDIATSI=iPO4+22, iDIAZCHL=iPO4+23
     &       , iDIAZC=iPO4+24, iDIAZFE=iPO4+25
     &       , ntrc_bio=26
#  else  /* ifdef BIOLOGY */
     &       , ntrc_bio=0
#  endif /* BIOLOGY */
# else /* ifdef SALINITY */
     &       , ntrc_salt=0
#  ifdef BIOLOGY
     &       , iNO3_=itrc_bio, iNH4_=3, iDet_=4, iPhyt=5, iZoo_=6
#  else
     &       , ntrc_bio=0
#  endif
# endif
#endif
     &       , NT=itemp+ntrc_salt+ntrc_pas+ntrc_bio

#ifdef PSOURCE
     &       , Msrc=10   ! Number of point sources
#endif
#ifdef STATIONS
     &       , NS=5      ! Number of output stations
#endif
#ifdef FLOATS
     &       , Mfloats=32000 ! Maximum number of floats
#endif
