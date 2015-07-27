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
#elif defined PACIFIC

c    &               LLm=384, MMm=224, N=30
c    &               LLm=392, MMm=288, N=30

c    &               LLm=432, MMm=320, N=32
c     &               LLm=488, MMm=360, N=40  ! PAC44
     &                 LLm=976, MMm=720, N=40 ! PAC22

c     &               LLm=460, MMm=240, N=50  ! PACSMA grid

#elif defined PACIFIC_2D
     &               LLm=768, MMm=512, N=1
c    &               LLm=1520, MMm=1088, N=1

#elif defined SO
     &               LLm=720, MMm=216, N=42
c     &               LLm=1440, MMm=432, N=42
c     &               LLm=2880, MMm=864, N=42


#elif defined OVERFLOW
     &               LLm=4,   MMm=128, N=20
#elif defined SEAMOUNT
     &               LLm=41,  MMm=41,  N=30
c     &               LLm=80,  MMm=80,  N=30
c    &               LLm=192, MMm=96,  N=22
#elif defined ISWAKE
c     &               LLm=192, MMm=84, N=20
c     &               LLm=384, MMm=168, N=20
c     &               LLm=720, MMm=160, N=2
c     &              LLm=768, MMm=192, N=2
     &              LLm=1920, MMm=384, N=2
c     &               LLm=1440, MMm=320, N=2
c     &               LLm=768, MMm=336, N=20
c     &              LLm=1536, MMm=672, N=20
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

#elif defined RIVER
     &               LLm=40,  MMm=160,  N=16
c     &                LLm=60,  MMm=240,  N=24
#elif defined UPWELLING
c     &                 LLm=20,  MMm=80,  N=16
     &                 LLm=20,  MMm=128,  N=40


c     &               LLm=20,  MMm=80,  N=32
#elif defined CANBAS2
c     &                LLm=224, MMm=288, N=32      ! CanBas
     &                LLm=225, MMm=328, N=32      ! NEA_EXT
c     &                LLm=384, MMm=480, N=32      ! GranCan

#elif defined BALTIC
     &                LLm=440, MMm=384, N=32

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
     &               LLm=312,  MMm=512, N=32    ! USW51 - lev0


# endif
#elif defined USWC_CENTRAL
     &               LLm=416, MMm=346, N=42
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
      integer, parameter ::
#ifdef MPI
     &      NP_XI=16, NP_ETA=18, NSUB_X=1, NSUB_E=1
!--> Sasha orig     &      NP_XI=2, NP_ETA=2, NSUB_X=2, NSUB_E=13
#else
c     &      NSUB_X=4, NSUB_E=40  ! PAC44
c     &      NSUB_X=8, NSUB_E=80   ! PAC22
c     &      NSUB_X=4, NSUB_E=1
c     &      NSUB_X=2, NSUB_E=2
     &      NSUB_X=6, NSUB_E=24
c     &      NSUB_X=4, NSUB_E=52
c     &      NSUB_X=2, NSUB_E=8  ! <-- iswake 768x192
c     &      NSUB_X=8, NSUB_E=48
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
# ifdef SALINITY
     &       , isalt=2, ntrc_salt=1
#  ifdef BIOLOGY
     &       , NT=7, iNO3_=3, iNH4_=4, iDet_=5, iPhyt=6, iZoo_=7
#  else
     &       , NT=2
#  endif
# else
     &       ,  ntrc_salt=0
#  ifdef BIOLOGY
     &       , NT=6, iNO3_=2, iNH4_=3, iDet_=4, iPhyt=5, iZoo_=6
#  else
     &       , NT=1
#  endif
# endif
#endif

#ifdef PSOURCE
     &       , Msrc=10   ! Number of point sources
#endif
#ifdef STATIONS
     &       , NS=5      ! Number of output stations
#endif
#ifdef FLOATS
     &       , Mfloats=32000 ! Maximum number of floats
#endif
