#include "cppdefs.h"
#ifdef COUP_OAS
      USE oas_roms_comm, ONLY: oas_roms_init, oas_roms_finalize
      USE oas_roms_def, ONLY: oas_roms_define
# endif /* COUP_OAS */
#if !defined GRID_LEVEL || GRID_LEVEL == 1

!!    program main               ! Open MP version of ROMS driver
!mm we should soon moved to using the MPI module
!# ifdef MPI
!      use mpi
!# endif
      implicit none              ! with single parallel region using
      integer ierr               ! explicit barrier synchronization.
# include "param.h"
# include "ncvars.h"
      logical, save :: coupled = .false.  ! coupled yes/no
# ifdef COUP_OAS
      logical :: use_oasis
# endif /* COUP_OAS */
# ifdef MPI
      real*8 tstart, tend
C$    integer level,req_lev
#  include "mpif.h"
# ifdef COUP_OAS
      coupled=use_oasis()
      print*,'ROMS(main): coupled = ',coupled
      if ( coupled ) then
              mpi_master_only print*,'(main): calling oas_roms_init.'
        call oas_roms_init !Let Oasis define MPI communicator (Local instead of MPI_World_Comm)
              mpi_master_only print*,' (main): Calling oas_roms_init.'
      else
        mpi_master_only print*,'(main): ROMS uncoupled -> MPI_init'
        ierr=1
        call MPI_Init(ierr) !If Oasis Coupling, MPI init handled by Oasis
      endif
# else
        mpi_master_only print*,'(main): ROMS-only -> MPI_init'
      call MPI_Init(ierr) !If Oasis Coupling, MPI init handled by Oasis
# endif
      call mpi_setup (ierr)
      tstart=MPI_Wtime()
      if (ierr == 0) then
# endif
        call init_scalars(ierr)          ! Initialize global scalars,
#ifdef COUP_OAS
      if ( coupled ) call oas_roms_define(grdname) !Defines Grids and exchange fields for Oasis Coupler
#endif /* COUP_OAS */

# ifdef OBSOLETE_SUB_INIT_SCALARS_IN_MAIN
#  ifdef BIOLOGY_NPZDOC 
      if (ierr == 0) then
      call init_scalars_npzdoc(ierr)
      endif
#   ifdef BGC_FLUX_ANALYSIS
      if (ierr == 0) then
      call init_scalars_bgcflux(ierr)
      endif 
#   endif
#  endif
# endif
        if (ierr == 0) then              ! model tunable parameters,
C$        call omp_set_dynamic(.false.)
C$OMP PARALLEL                           ! fast-time averaging weights
          call roms_thread(coupled)      ! for barotropic mode, and
C$OMP END PARALLEL                       ! launch the model in OpenMP
        endif                            ! parallel regime.
# ifdef MPI
      endif
      call MPI_Barrier(ocean_grid_comm, ierr)
      tend=MPI_Wtime()
      mpi_master_only write(*,*) 'MPI_run_time =', tend-tstart
#  ifdef COUP_OAS
      if ( coupled ) then
        call oas_roms_finalize   !If Oasis Coupling, MPI finalize handled by Oasis
      else
        call MPI_Finalize (ierr)
      endif
#  else
      call MPI_Finalize (ierr)
#  endif /* COUP_OAS */
# endif
      stop
      end


      subroutine roms_thread(coupled)
      implicit none
# include "param.h"
# include "scalars.h"
      logical :: coupled
                                         ! Note: Because there is
      call start_timers()                ! a possibility of I/O error
      call roms_init                     ! occurring on some MPI nodes,
      if (may_day_flag /= 0) goto 99     ! but not simultaneously on
      do iic=ntstart,ntstart+ntimes      ! all, exiting is deferred
        diag_sync=.false.                ! until "may_day_flag" is
        call roms_step(coupled)
        if (diag_sync .and. may_day_flag /= 0) goto 99
      enddo
  99  call stop_timers()                 ! summarized among all nodes
C$OMP BARRIER                            ! and broadcasted by "diag"
#if !defined NO_RESTART && !defined NO_HISTORY
C$OMP MASTER                             ! [which sets diag_sync=T to
      call closecdf                      ! signal that "may_day_flag"
C$OMP END MASTER                         ! is in uniform state]. This
#endif
      return                             ! is THE ONLY route to exit
      end                                ! if something goes wrong.
#endif

      subroutine roms_init
      implicit none
      integer trd, tile, my_first, my_last, range
C$    integer omp_get_thread_num, omp_get_num_threads
#include "param.h"
#include "scalars.h"

# include "sources.h"
# include "ncvars.h"
#ifdef FLOATS
! grid.h is needed so that lonr and latr are readily available
# include "grid.h"
# include "floats.h"
# include "ncvars_floats.h"
#endif
      numthreads=1 ; trd=0
C$    numthreads=omp_get_num_threads()
C$    trd=omp_get_thread_num()
      proc(2)=trd

      if (mod(NSUB_X*NSUB_E,numthreads) /= 0) then
C$OMP MASTER
        mpi_master_only write(*,'(/3(1x,A,I3),A/)')
     &    '### ERROR: Wrong choice of numthreads =', numthreads,
     &         'while NSUB_X =', NSUB_X, 'NSUB_E =', NSUB_E,'.'
        may_day_flag=8
C$OMP END MASTER
C$OMP BARRIER
        goto 99 !-->  EXIT
      endif
                                       ! NOTE: This code is written
      iic=0  ; kstp=1 ; knew=1         ! under an assumption that all
#ifdef SOLVE3D
      iif=1  ; nstp=1                  ! the scalar variables assigned
      nnew=1 ; nrhs=1                  ! on the left here are placed
#endif
      synchro_flag=.true.              ! into a THREADPRIVATE common
      diag_sync=.false.                ! block so each thread must
      priv_count=0                     ! assign them to same values.

      range=(NSUB_X*NSUB_E+numthreads-1)/numthreads
      my_first=trd*range
      my_last=min(my_first + range-1, NSUB_X*NSUB_E-1)
#define my_tile_range my_first,my_last

      do tile=my_tile_range            ! Initialize or "first touch"
        call init_arrays(tile)         ! global arrays (most of them
      enddo                            ! are just set to to zero).
C$OMP BARRIER

c--#define CR
CR      write(*,*) '-11' MYID
#ifdef ANA_GRID
      do tile=my_tile_range            ! Set horizontal curvilinear
        call ana_grid(tile)            ! grid and model bathymetry
      enddo                            ! (either analytically or read
C$OMP BARRIER                          ! from grid netCDF file).
#else
C$OMP MASTER
      call get_grid
C$OMP END MASTER
C$OMP BARRIER
      if (may_day_flag /= 0) goto 99 !-->  EXIT
#endif
      do tile=my_tile_range            ! Compute various metric terms
        call setup_grid1(tile)         ! and their combinations.
      enddo
C$OMP BARRIER
CR      write(*,*) '-10' MYID
      do tile=my_tile_range
        call setup_grid2(tile)
      enddo
C$OMP BARRIER
CR      write(*,*) '-9' MYID

#ifdef SOLVE3D
C$OMP MASTER                           ! Setup vertical stretching
      call set_scoord                  ! functions for S-coordinate
C$OMP END MASTER                       ! system
C$OMP BARRIER
      if (may_day_flag /= 0) goto 99
#endif
CR      write(*,*) ' -8' MYID

#if (defined UV_VIS2 && defined VIS_GRID) ||\
    (defined TS_DIF2 && defined DIF_GRID)
      do tile=my_tile_range            ! Rescale horizontal mixing
        call visc_rescale(tile)        ! coefficients according to
      enddo                            ! local grid size.
C$OMP BARRIER
CR      write(*,*) ' -7' MYID
#endif

#ifdef SOLVE3D
      do tile=my_tile_range            ! Create three-dimensional
        call set_depth(tile)           ! S-coordinate system, which
# ifdef LMD_KPP
        call swr_frac(tile)            ! may be needed by ana_init.
# endif
      enddo
C$OMP BARRIER                          ! Here it is assumed that free
      do tile=my_tile_range            ! surface zeta is at rest state,
        call grid_stiffness(tile)      ! zeta=0). Also find and report
      enddo                            ! extremal values of topographic
C$OMP BARRIER                          ! slope parameters "rx0", "rx1".
CR      write(*,*) ' -6' MYID
#endif

# ifdef BIOLOGY_BEC
      ! pH is initialized to 0 in ecosys_init
      call ecosys_init
# endif
# ifdef BIOLOGY_BEC2
      ! pH is initialized to 0 in ecosys_init
      call ecosys2_init
# endif
      ! Initialisation of lmd parameters (JDS)
!      call lmd_kpp_init
#ifdef ANA_INITIAL
      do tile=my_tile_range            ! Set initial conditions for
        call set_forces(tile)          ! model prognostic variables,
      enddo                            ! either analytically or read
C$OMP BARRIER                          ! from netCDF file.  Note that
      do tile=my_tile_range            ! analytical initialization
        call ana_init(tile)            ! may require surface forcing
      enddo                            ! fields (e.g, wind stress),
C$OMP BARRIER                          ! so call forces first.
#endif
#ifndef NO_RESTART
#ifdef ANA_INITIAL
      if (nrrec > 0) then              ! Also note that ana_init may
#endif
#ifdef EXACT_RESTART
C$OMP MASTER                           ! setup environmental variables
        call get_init(nrrec-1,2)       ! (e.g. analytical boundary
C$OMP END MASTER                       ! forcing), call it first, even
C$OMP BARRIER                          ! in the case of restart run.
# ifdef SOLVE3D
        do tile=my_tile_range
          call set_depth(tile)
        enddo
C$OMP BARRIER
# endif
#endif
C$OMP MASTER
        call get_init(nrrec, 1)
C$OMP END MASTER
#ifdef ANA_INITIAL
      endif    !<-- nrrec>0
#endif
#endif /* NO_RESTART */
C$OMP BARRIER
#if defined PASSIVE_TRACER && defined AGE_DYE_TRACER
      do tile=my_tile_range
         call init_agedye(tile)
      enddo
C$OMP BARRIER
      if (may_day_flag /= 0) goto 99      !--> ERROR
#endif
CR      write(*,*) ' -5' MYID
                                  ! Set initial model clock: at this
      time=start_time             ! moment "start_time" (global scalar)
      tdays=time*sec2day          ! is set by get_init or analytically
                                  ! copy it into threadprivate "time"
#ifdef SOLVE3D
      do tile=my_tile_range       ! recompute three-dimensional S-
        call set_depth(tile)      ! coordinate system: at this moment
      enddo                       ! free surface has non-zero status
C$OMP BARRIER

CR      write(*,*)  ' -4' MYID
      do tile=my_tile_range
        call set_HUV(tile)
      enddo
C$OMP BARRIER
CR      write(*,*)  ' -3' MYID

      do tile=my_tile_range
        call omega(tile)
        call rho_eos(tile)
      enddo
C$OMP BARRIER
CR      write(*,*)  ' -2' MYID
# ifdef BIOLOGY_NPZDOC
      call init_biopar               ! set up biogeochemical parameters
# endif /* BIOLOGY_NPZDOC */
#endif

! Set up climatological environment: Set nudging coefficient for
!==== == ============== ============ sea-surface hight and tracer
! climatology; create analytical tracer and sea-surface hight
! climatology fields (if applicable); set bottom sediment grain
! size [m] and density [kg/m^3] used for bottom boundary layer
! formulation;

#if defined SPONGE || defined TCLIMATOLOGY \
  || (defined SG_BBL96 && defined ANA_BSEDIM)\
  || (defined TCLIMATOLOGY && defined ANA_TCLIMA)\
  || defined ANA_SSH

      do tile=my_tile_range
# if defined SPONGE || defined TCLIMATOLOGY
        call set_nudgcof(tile)
# endif
# if defined TCLIMATOLOGY && defined ANA_TCLIMA && defined SOLVE3D
        call ana_tclima(tile)
# endif
# ifdef ANA_SSH
        call ana_ssh(tile)
# endif
# if defined SG_BBL96 && defined ANA_BSEDIM
        call ana_bsedim(tile)
# endif
      enddo
C$OMP BARRIER
#endif
CR      write(*,*) ' -1' MYID

! Read initial input data for forcing fields; tracer and sea surface
! climatology; bottom sediment grain size and density (if applicable)
! from input netCDF files.  Note that CPP-logic here is mutually
! exclusive with respect to calls ana_tclima, ana_ssh, and ana_bsedim
! just above.
!

#if defined SALINITY && defined VFLX_CORR && defined USE_GLOBAL_SRF_SUM
        call reset_global_srf_sum()
! If the virtual flux correction is used, globally averaged concentrations
! for the surface cells must be available for subroutine set_stflux_tile
! (which is called by get_forces)
C$OMP PARALLEL DO PRIVATE(tile)
        do tile=0,NSUB_X*NSUB_E-1
           call calc_global_srf_sum(tile)
        enddo

! Keep this routine out of the parallel region
        call scale_global_srf_sum()
#endif /* SALINITY && VFLX_CORR ..*/

#ifdef BIOLOGY_NPZDOC
C$OMP PARALLEL DO PRIVATE(tile)
         do tile=0,NSUB_X*NSUB_E-1
            call calcpar(tile)
         enddo
#endif /* BIOLOGY_NPZDOC */

C$OMP MASTER
# if !defined NO_HISTORY
#  if defined ANA_GRID
        call wrt_ana_grid
#  endif
        if (ldefhis .and. wrthis(indxTime)) call wrt_his
# endif /* NO_HISTORY */
#ifdef FLOATS
! Initialization for Lagrangian floats
!-------------------------------------------------------
      nrecflt=0      ! initialization done here and not in
      ncidflt=-1     ! init_scalars since it must be done only
                     ! once (whether child levels exist or not)
      spval=1.E15    ! spval is the nodata flag for float variables

      deltac2p=2.3   ! distance from the boundary at which a float
                     ! is transferred from child to parent
      deltap2c=2.5   ! same for transfer from parent to child

      call init_arrays_floats
      call init_floats
# ifdef SPHERICAL
      call interp_r2d_type_ini (lonr(START_2D_ARRAY), iflon)

      call interp_r2d_type_ini (latr(START_2D_ARRAY), iflat)
# else
      call interp_r2d_type_ini (  xr(START_2D_ARRAY), iflon)
      call interp_r2d_type_ini (  yr(START_2D_ARRAY), iflat)
# endif
# ifdef SOLVE3D
      call fill_ini   ! fills in trackaux for ixgrd,iygrd,izgrd
                      ! and ifld (either izgrd or ifld is meaningful)
# endif
      if (ldefflt) call wrt_floats
#endif /* FLOATS */
C$OMP END MASTER
C$OMP BARRIER
CR      write(*,*) '  0' MYID
      if (may_day_flag /=0 ) goto 99     !-->  EXIT

#ifdef COUP_DEBUG
      if (mynode == 60) then
        ! For a central tile (60) presumably in the coupling region
        ! Open CSV file to let oas_merge_forces write the  forcing
        ! fields of one grid point before and after the merge.
        open(unit=107, file="atm_forces_era.txt",
     &     form = "formatted")
        write(107,*)'time/s, ', 'sustr, ', 'svstr, '
     &     , 'srflx, ', 'stflx(itemp), ', 'stflx(isalt)'
        open(unit=108, file="atm_forces_merged.txt",
     &     form = "formatted")
        write(108,*)'time/s, ', 'sustr, ', 'svstr, '
     &     , 'srflx, ', 'stflx(itemp), ', 'stflx(isalt)'
      endif
#endif

C$OMP MASTER
        mpi_master_only write(*,'(/1x,A/)')
     &     'main :: initialization complete; starting time-stepping.'
C$OMP END MASTER

! Initialize child-grid model, if any. Note that this call introduces
! recursive sequence, since child-grid model may also have child on its
! own.

#ifdef GRID_LEVEL
# if GRID_LEVEL < MAX_GRID_LEVEL
#  if GRID_LEVEL == 1
        call roms_init_2
#  elif GRID_LEVEL == 2
        call roms_init_3
#  elif GRID_LEVEL == 3
        call roms_init_4
#  endif
# endif
#endif
  99  return
      end


!      *****    *********    ******   *******    *********
!    ***   ***  *  ***  *   **  ***   ***   ***  *  ***  *
!    ***           ***     **   ***   ***   ***     ***
!      *****       ***    ***   ***   ***   **      ***
!          ***     ***    *********   ******        ***
!    ***   ***     ***    ***   ***   ***  **       ***
!      *****       ***    ***   ***   ***   ***     ***


      subroutine roms_step(coupled)
#ifdef COUP_OAS
      USE oas_roms_send_receive, ONLY: oas_roms_send, oas_roms_receive
#endif
      implicit none
      !logical :: use_oasis
      !logical, save :: coupled = .false. 
      logical :: coupled
      integer trd, tile, my_first, my_last, range
#include "param.h"
#include "scalars.h"
#include "ncvars.h"
#ifdef FLOATS
# include "ncvars_floats.h"
# include "floats.h"
      integer chunk_size_flt, Lstr,Lend, flt_str
      common /floats_step/ flt_str
#endif
#ifdef GRID_LEVEL
      integer iter
#endif
#ifdef BGC_FLUX_ANALYSIS
# include "bgcflux.h"
# include "ncroms_bgcflux.h"
#endif
#ifdef PHYS_FLUX_ANALYSIS
# include "physflux.h"
# include "ncroms_physflux.h"
#endif
#ifdef BIOLOGY_BEC
# include "bec_avg.h"
#endif
 
      !coupled=use_oasis()
      trd=proc(2)
      range=(NSUB_X*NSUB_E+numthreads-1)/numthreads
      my_first=trd*range
      my_last=min(my_first + range-1, NSUB_X*NSUB_E-1)

! Increment time-step index and set model clock. Note that "time" set
! below corresponds to step "n" (denoted here as "nstp"), while counter
! "iic" corresponds to "n+1", so normally, assuming that time is
! counted from zero, the following relation holds: time=dt*(iic-1).
!  Also note that the output history/restart/averages routines write
! time and all the fields at step "n" (not n+1), while the first
! element of structure "time_index" written into the files is actually
! iic-1, hence normally time=time_index*dt there.  Same rule applies
! to the diagnostic routine "diag" which prints time and time step
! (actually iic-1) on the screen.

      time=start_time+dt*float(iic-ntstart) !<-- corresp. to "nstp"
      tdays=time*sec2day
#ifdef SOLVE3D
      nstp=1+mod(iic-ntstart,2)
      nrhs=nstp ; nnew=3
#endif

#ifdef FLOATS
      nfp1=mod(nfp1+1,NFT+1)           ! rotate time
      nf  =mod(nf  +1,NFT+1)           ! indices for
      nfm1=mod(nfm1+1,NFT+1)           ! floats
      nfm2=mod(nfm2+1,NFT+1)
      nfm3=mod(nfm3+1,NFT+1)
C$OMP MASTER
      flt_str=0
C$OMP END MASTER
#endif


                                       ! Read forcing and climatology
      if (synchro_flag) then           ! data. This operation may raise
        synchro_flag=.false.           ! "may_day_flag" in the case of
#if !(defined ANA_GRID && defined ANA_INITIAL && defined ANA_SMFLUX && defined ANA_STFLUX && defined  ANA_SSFLUX && defined ANA_SRFLUX && defined ANA_VMIX)
C$OMP MASTER                           ! I/O errors which may occur on
        call get_forces                ! some nodes, but not on all at 
C$OMP END MASTER                       ! the same time.  Therefore to
C$OMP BARRIER                          ! avoid MPI deadlock the exit is
#endif
      endif                            ! deferred until after broadcast
                                       ! of "may_day_flag" by diag.


#ifdef SOLVE3D
      do tile=my_tile_range                 ! interpolate forcing
        call set_forces(tile)               ! data to model time and
# if defined SSH_TIDES || defined UV_TIDES
        call set_tides(tile)                ! compute surface fluxes.
# endif
        call    rho_eos(tile)
        call    set_HUV(tile)
        call diag(tile)

#if defined BIOLOGY || defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC
!        call   bio_diag (tile)
#endif
      enddo
C$OMP BARRIER

#ifdef COUP_OAS 
      !Receive Coupling fields from Cosmo/Oasis
      if ( coupled ) call oas_roms_receive(iic-ntstart)
#endif

      do tile=my_tile_range
        call omega(tile)
# if defined ANA_VMIX
        call ana_vmix(tile)
# elif defined LMD_MIXING
        call lmd_vmix(tile)
# elif defined BVF_MIXING
        call bvf_mix(tile)
# endif
      enddo
C$OMP BARRIER

      do tile=my_tile_range
        call     prsgrd(tile)
        call pre_step3d(tile)
# ifdef PRED_COUPLED_MODE
#  ifdef UV_VIS2
        call     visc3d(tile)
#  endif
# endif
# ifdef AVERAGES
        call    set_avg(tile)
#  ifdef SLICE_AVG
        call    set_slavg(tile)
#  endif
#  ifdef BGC_FLUX_ANALYSIS
#   ifdef BIOLOGY_NPZDOC
        call set_bgc_flux_avg(tile)
#   elif defined BIOLOGY_BEC
        call set_bec_flux_avg(tile)
#   endif
#  endif
#  ifdef PHYS_FLUX_ANALYSIS
        call set_phys_flux_avg(tile)
#  endif
# endif /* AVERAGES */
      enddo
C$OMP BARRIER

# ifdef CORR_COUPLED_MODE
      do tile=my_tile_range              ! This code segment is for
        call set_HUV1(tile)              ! corrector-coupled version
      enddo                              ! of time stepping only.
C$OMP BARRIER

      nrhs=3 ; nnew=3-nstp   !!! WARNING

      do tile=my_tile_range
        call omega(tile)
        call rho_eos(tile)
      enddo
C$OMP BARRIER

      do tile=my_tile_range
        call     prsgrd(tile)
        call step3d_uv1(tile)
#  ifdef UV_VIS2
        call     visc3d(tile)
#  endif
      enddo
C$OMP BARRIER
# endif
#endif  /* SOLVE3D */
# ifdef COUP_OAS
      if ( coupled ) then
        call oas_roms_send(iic-ntstart)
      endif
# endif

! Output block: write restart/history files.
!======= ====== ===== =============== ======

!DL: place following block before writing of rst file:
#ifdef BGC_FLUX_ANALYSIS
      if (iic > ntstart .and. n_bgc_flux_his > 0 .and. 
     &     mod(iic-ntstart, n_bgc_flux_his) == 0) then
         nrec_bgc_flux_his = nrec_bgc_flux_his + 1
      end if
# ifdef AVERAGES
      if (iic > ntstart .and. n_bgc_flux_avg > 0 .and. 
     &     mod(iic-ntstart, n_bgc_flux_avg) == 0) then
         nrec_bgc_flux_avg = nrec_bgc_flux_avg + 1
      end if
# endif
#endif /* BGC_FLUX_ANALYSIS */
#ifdef PHYS_FLUX_ANALYSIS
      if (iic > ntstart .and. n_phys_flux_his > 0 .and. 
     &     mod(iic-ntstart, n_phys_flux_his) == 0) then
         nrec_phys_flux_his = nrec_phys_flux_his + 1
      end if
# ifdef AVERAGES
      if (iic > ntstart .and. n_phys_flux_avg > 0 .and. 
     &     mod(iic-ntstart, n_phys_flux_avg) == 0) then
         nrec_phys_flux_avg = nrec_phys_flux_avg + 1
      end if
# endif
#endif /* PHYS_FLUX_ANALYSIS */

      if ( iic > ntstart .and. ( mod(iic-ntstart,nrst) == 0
#ifdef EXACT_RESTART
     &                         .or. mod(iic-ntstart+1,nrst) == 0
#endif
     &   .or. (mod(iic-ntstart,nwrt) == 0 .and. wrthis(indxTime))
#ifdef AVERAGES
     &   .or. (mod(iic-ntsavg,navg) == 0  .and. wrtavg(indxTime))
# ifdef SLICE_AVG
     &   .or. ( mod(iic-ntsslavg,nslavg) == 0  
     &           .and. wrtslavg(indxTime) )
# endif
# ifdef BGC_FLUX_ANALYSIS
     &   .or. ( mod(iic-nts_bgc_flux_avg,n_bgc_flux_avg)==0 )
# endif
# ifdef PHYS_FLUX_ANALYSIS
     &   .or. ( mod(iic-nts_phys_flux_avg,n_phys_flux_avg)==0 )
# endif
#endif /* AVERAGES */
#ifdef BGC_FLUX_ANALYSIS
     &   .or. ( mod(iic-ntstart,n_bgc_flux_his)==0 )
#endif
#ifdef PHYS_FLUX_ANALYSIS
     &   .or. ( mod(iic-ntstart,n_phys_flux_his)==0 )
#endif
#ifdef STATIONS
     &   .or. (mod(iic-ntstart,nsta) == 0 .and. nstation > 0)
#endif
#ifdef FLOATS
     &   .or. (mod(iic-ntstart,nflt) == 0 .and. nfloats > 0)
#endif
     &                                                  )) then
C$OMP MASTER
        if (mod(iic-ntstart,nrst) == 0
#ifdef EXACT_RESTART
     &         .or. mod(iic-ntstart+1,nrst) == 0
#endif
     &                                ) nrecrst=nrecrst+1
        if (mod(iic-ntstart,nwrt) == 0) nrechis=nrechis+1
#ifdef AVERAGES
        if (mod(iic-ntsavg,navg) == 0) nrecavg=nrecavg+1
# ifdef SLICE_AVG
        if (mod(iic-ntsslavg,nslavg) == 0) nrecslavg=nrecslavg+1
# endif
#endif
#ifdef STATIONS
        if (mod(iic-ntstart,nsta) == 0) nrecstn=nrecstn+1
#endif
#ifdef FLOATS
        if (mod(iic-ntstart,nflt) == 0) nrecflt=nrecflt+1
#endif
#ifndef NO_RESTART
        if (mod(iic-ntstart,nrst) == 0
#ifdef EXACT_RESTART
     &         .or. mod(iic-ntstart+1,nrst) == 0
#endif
     &                                 ) call wrt_rst
        if (mod(iic-ntstart,nwrt) == 0 .and. wrthis(indxTime)) then
          call wrt_his
c          if (iic>60) nwrt=1 !<-- useful for debugging
        endif
#endif /* NO_RESTART */

#ifdef AVERAGES
# ifdef SLICE_AVG
        if (mod(iic-ntsslavg,nslavg)==0 .and. wrtslavg(indxTime))
     &      call wrt_slavg
# endif
        if (mod(iic-ntsavg,navg)==0 .and. wrtavg(indxTime))
     &      call wrt_avg
#endif
#ifdef STATIONS
        if (mod(iic-ntstart,nsta)==0 .and. nstation>0)
     &      call wrt_statn
#endif
#ifdef FLOATS
        if (mod(iic-ntstart,nflt)==0 .and. nfloats>0)
     &      call wrt_floats
        diagfloats=.false.
#endif

#ifdef BGC_FLUX_ANALYSIS
      if (iic > ntstart .and. n_bgc_flux_his > 0 .and. 
     &     mod(iic-ntstart, n_bgc_flux_his) == 0) then
# ifdef BIOLOGY_NPZDOC
         call wrt_bgc_flux_his
# elif defined BIOLOGY_BEC
         call wrt_bec_flux_his
# endif
      end if
# ifdef AVERAGES
      if (iic > ntstart .and. n_bgc_flux_avg > 0 .and. 
     &     mod(iic-nts_bgc_flux_avg, n_bgc_flux_avg) == 0) then
# ifdef BIOLOGY_NPZDOC
         call wrt_bgc_flux_avg
# elif defined BIOLOGY_BEC
         call wrt_bec_flux_avg
# endif
      end if
# endif
#endif /* BGC_FLUX_ANALYSIS */
#ifdef PHYS_FLUX_ANALYSIS
      if (iic > ntstart .and. n_phys_flux_his > 0 .and. 
     &     mod(iic-ntstart, n_phys_flux_his) == 0) then
         call wrt_phys_flux_his
      end if
# ifdef AVERAGES
      if (iic > ntstart .and. n_phys_flux_avg > 0 .and. 
     &     mod(iic-nts_phys_flux_avg, n_phys_flux_avg) == 0) then
         call wrt_phys_flux_avg
      end if
# endif
#endif /* PHYS_FLUX_ANALYSIS */

C$OMP END MASTER
C$OMP BARRIER
        if (iic-ntstart > ntimes) goto 99   !-->  DONE
      endif

#ifdef FLOATS
! flag for diagnostic computation (for writing at next time step)
      if (mod(iic-ntstart,nflt)==0) then
        diagfloats=.true.
      endif
#endif

! Solve the 2D equations for the barotropic mode.
!------ --- -- --------- --- --- ---------- -----
#ifdef SOLVE3D
      do iif=1,nfast
#endif
#define FORW_BAK
#ifdef FORW_BAK
        kstp=knew                      ! This might look a bit silly,
        knew=kstp+1                    ! because both branches of this
        if (knew > 4) knew=1           ! "if" statement are identical.
        if (mod(knew,2) == 0) then     ! Nevertheless, it makes sense,
          do tile=my_tile_range        ! since mpc will reverse one of
# ifndef SOLVE3D
            call set_forces(tile)      ! these loops to make zig-zag
# endif
            call     step2d(tile)      ! tile-processing sequence.
          enddo
C$OMP BARRIER
        else
          do tile=my_tile_range
# ifndef SOLVE3D
            call set_forces(tile)
# endif
            call     step2d(tile)
          enddo
C$OMP BARRIER
        endif
#else
        kstp=knew
        knew=3
        do tile=my_tile_range
# ifndef SOLVE3D
          call set_forces(tile)
# endif
          call     step2d(tile)
        enddo
C$OMP BARRIER
        knew=3-kstp
        do tile=my_tile_range
           call step2d(tile)
        enddo
C$OMP BARRIER
#endif
#ifdef SOLVE3D
      enddo    ! <-- iif

# ifdef PRED_COUPLED_MODE
      do tile=my_tile_range              ! This code segment is for
        call set_HUV1(tile)              ! predictor-coupled version
      enddo                              ! of time stepping only. 
C$OMP BARRIER

      nrhs=3 ; nnew=3-nstp

      do tile=my_tile_range
        call omega(tile)
        call rho_eos(tile)
      enddo
C$OMP BARRIER
      do tile=my_tile_range
        call     prsgrd(tile)
        call step3d_uv1(tile)
      enddo
C$OMP BARRIER
# endif

      do tile=my_tile_range              ! Continue solution of
        call step3d_uv2(tile)            ! the three-dimensional
      enddo                              ! equations: finalize
C$OMP BARRIER                            ! time step for momenta
      do tile=my_tile_range              ! and tracers.
        call omega(tile)
        call step3d_t (tile)
# if defined TS_DIF2 || defined TS_DIF4
        call t3dmix(tile)
# endif
      enddo
C$OMP BARRIER
#endif /* SOLVE3D */

#ifdef FLOATS
      chunk_size_flt=32
      do while (flt_str < nfloats)
C$OMP CRITICAL
        Lstr=flt_str+1
        flt_str=Lstr+chunk_size_flt-1
C$OMP END CRITICAL
        Lend=min(Lstr+chunk_size_flt-1,nfloats)
        call step_floats(Lstr,Lend)
      enddo
c**    call step_floats(1,nfloats)    ! serial version for debugging
#endif

#ifdef GRID_LEVEL
# if GRID_LEVEL < MAX_GRID_LEVEL
      do iter=1,3
#  if GRID_LEVEL == 1
        call roms_step_2
#  elif GRID_LEVEL == 2
        call roms_step_3
#  elif GRID_LEVEL == 3
        call roms_step_4
#  endif
      enddo
# endif
#endif
  99  return
      end

#ifdef COUP_OAS
      function use_oasis()
      ! Read send field flags from the "romsoc.nml" namelist
      ! To determine if the OASIS is needed at all (use_oasis=.true.)

      USE oas_roms_data, ONLY: l_snd_sst, l_snd_sm
      logical :: use_oasis
      logical, save :: l_exchange = .true.
     &               , firstcall = .true.

      INTEGER, PARAMETER :: funit=67
      CHARACTER(len=*), PARAMETER :: nml_file = "romsoc.nml"
      INTEGER :: ierr

      if (firstcall) then
        ! only read the send switches once and return the save
        ! value from then onwards. 
        firstcall = .false.

        mpi_master_only print*, "(use_oasis) Reading romsoc.nml"

        namelist /exchange_flags/ l_exchange, l_snd_sst, l_snd_sm

        OPEN(funit, FILE=nml_file, IOSTAT=ierr)
        IF (ierr /= 0) return

        READ(funit, exchange_flags, IOSTAT=ierr)
        CLOSE(funit)
        IF (ierr /= 0)
     &     print*, 'WARNING: (use_oasis) Error reading ROMS->OASIS '
     &        ,'field sfrom ', nml_file
           mpi_master_only print*, '=> Using coupler by default' 
           use_oasis = .true.
           return
        else
           if (l_exchange) l_exchange = (l_snd_sst .or. l_snd_sm)
           if (l_exchange) then
              mpi_master_only print*, 'Coupler selected (romsoc.nml)'
           else
              mpi_master_only print*, 'Coupler deselected (romsoc.nml)'
           endif
      endif  ! firstcall

      use_oasis = l_exchange
      return
      end
#endif
