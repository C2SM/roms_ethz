#include "cppdefs.h"
#if !defined GRID_LEVEL || GRID_LEVEL == 1

!!    program main                   ! Open MP version of ROMS
      implicit none                  ! driver with single parallel
      real*8 tstart, tend            ! region using explicit barrier
      integer ierr,  level,req_lev   ! synchronization.
# include "param.h"
# ifdef MPI
#  include "mpif.h"

      ierr=1 
C$    req_lev=MPI_THREAD_MULTIPLE
C$    call MPI_Init_thread (req_lev, level, ierr)
C$    write(*,*) 'MPI thread support levels=', req_lev,level
C$    ierr=0
      if (ierr.eq.1) call MPI_Init (ierr)

      call mpi_setup (ierr)
      tstart=MPI_Wtime()
c**   call MPI_Test
      if (ierr.ne.0) goto 100
# endif
! Initialize global scalars, fast-time averaging weights for barotropic mode,
! and launch the model in OpenMP parallel regime.
      call init_scalars (ierr)
      if (ierr.ne.0) goto 100
# ifdef BIOLOGY_NPZDOC
      call init_scalars_npzdoc(ierr)
      if (ierr.ne.0) goto 100 
#  ifdef BGC_FLUX_ANALYSIS
      call init_scalars_bgcflux(ierr)
      if (ierr.ne.0) goto 100 
#  endif
# endif /* BIOLOGY_NPZDOC */
# ifdef PHYS_FLUX_ANALYSIS
      call init_scalars_physflux(ierr)
      if (ierr.ne.0) goto 100 
# endif /* PHYS_FLUX_ANALYSIS */
C$       call omp_set_dynamic(.false.)
C$OMP PARALLEL
      call roms_thread
C$OMP END PARALLEL
 100  continue
# ifdef MPI
      call MPI_Barrier(ocean_grid_comm, ierr)
      tend=MPI_Wtime()
      MPI_master_only write(*,*) 'run_time =', tend-tstart
      call MPI_Finalize (ierr)
# endif
      stop
      end


      subroutine roms_thread
      implicit none
      integer my_iic
# include "param.h"
# include "scalars.h"

        call start_timers ()
        call roms_init
        if (may_day_flag.ne.0) goto 99
        do my_iic=1,ntimes+1
          call roms_step
          if (may_day_flag.ne.0) goto 99
        enddo
  99    call stop_timers()

C$OMP BARRIER
C$OMP MASTER
        call closecdf
C$OMP END MASTER
      return
      end


#endif
 
 
 
      subroutine roms_init
      implicit none
      integer trd, tile, my_first, my_last, range
C$    integer omp_get_thread_num, omp_get_num_threads
# include "param.h"
# include "scalars.h"

# include "ncvars.h"
#ifdef FLOATS
! grid.h is needed so that lonr and latr are readily available
# include "grid.h"
# include "floats.h"
# include "ncvars_floats.h"
#endif
 
      numthreads=1
C$    numthreads=omp_get_num_threads()
      trd=0
C$    trd=omp_get_thread_num()
      proc(2)=trd

      if (mod(NSUB_X*NSUB_E,numthreads).ne.0) then
C$OMP MASTER
        MPI_master_only write(stdout,'(/3(1x,A,I3),A/)')
     &    '### ERROR: wrong choice of numthreads =', numthreads,
     &         'while NSUB_X =', NSUB_X, 'NSUB_E =', NSUB_E,'.'
        may_day_flag=8
C$OMP END MASTER
C$OMP BARRIER
        goto 99 !-->  EXIT
      endif
 
      iic=0                     ! WARNING: This code is written
      kstp=1                    ! under assumption that the scalars
      knew=1                    ! on the left -- numthreads, iic,
#ifdef SOLVE3D
      iif=1                     ! kstp, knew, iif, nstp, nnew --
      nstp=1                    ! belong to a  THREADPRIVATE common
      nrhs=1                    ! block, so there no false sharing
      nnew=1                    ! here.
      nnew=1
#endif
      synchro_flag=.true.
      priv_count=0

      range=(NSUB_X*NSUB_E+numthreads-1)/numthreads
      my_first=trd*range
      my_last=min(my_first + range-1, NSUB_X*NSUB_E-1)
#define my_tile_range my_first,my_last
 
      do tile=my_tile_range      ! Initialize (FIRST-TOUCH) model
        call init_arrays (tile)  ! global arrays (most of them are
      enddo                      ! just set to to zero).
C$OMP BARRIER
 
#define CR
CR      write(*,*) '-11' MYID
 
 
#ifdef ANA_GRID
      do tile=my_tile_range       ! Set horizontal curvilinear grid
        call ana_grid (tile)      ! and model bathymetry (analyticaly
      enddo                       ! or read from GRID netCDF file).
C$OMP BARRIER
#else
C$OMP MASTER
      call get_grid
C$OMP END MASTER
C$OMP BARRIER
      if (may_day_flag.ne.0) goto 99 !-->  EXIT
#endif
      do tile=my_tile_range          ! Compute various metric terms
        call setup_grid1 (tile)      ! and their combinations.
      enddo
C$OMP BARRIER
CR      write(*,*) '-10' MYID
      do tile=my_tile_range
        call setup_grid2 (tile)
      enddo
C$OMP BARRIER
CR      write(*,*) '-9' MYID
 
#ifdef SOLVE3D
C$OMP MASTER                         ! Setup vertical grid-stretching
      call set_scoord                ! functions for S-coordinate
C$OMP END MASTER                     ! system
C$OMP BARRIER
      if (may_day_flag.ne.0) goto 99
#endif
CR      write(*,*) ' -8' MYID
 
#if defined VIS_GRID || defined DIF_GRID
      do tile=my_tile_range          ! Rescale horizontal mixing
        call visc_rescale (tile)     ! coefficients according to
      enddo                          ! local grid size.
C$OMP BARRIER
CR      write(*,*) ' -7' MYID
#endif
#ifdef SOLVE3D
      do tile=my_tile_range          ! Create three-dimensional
        call set_depth (tile)        ! S-coordinate system, which
#ifdef LMD_KPP
        call swr_frac (tile)
#endif
      enddo                          ! may be needed by ana_initial.
C$OMP BARRIER                        ! Here it is assumed that free
      do tile=my_tile_range          ! surface zeta is at rest state,
        call grid_stiffness (tile)   ! 'zeta=0). Also find and report
      enddo                          ! extremal values of topographic
C$OMP BARRIER                        ! slope parameters "rx0", "rx1".
CR      write(*,*) ' -6' MYID
#endif
 
                                     ! Set initial conditions for
#ifdef ANA_INITIAL
      do tile=my_tile_range          ! model prognostic variables,
        call ana_initial (tile)      ! either analytically or read
      enddo                          ! from netCDF file. Note that
C$OMP BARRIER
      if (nrrec.gt.0) then           ! because ana_initial may also
#endif
#ifdef EXACT_RESTART
C$OMP MASTER                         ! setup environmental variables
        call get_initial (nrrec-1,2) ! (e.g. analytical boundary
C$OMP END MASTER                     ! forcing), call it first, even
C$OMP BARRIER                        ! in the case of restart run.
# ifdef SOLVE3D
        do tile=my_tile_range
          call set_depth (tile)     !<-- needed to initialize Hz_bak
        enddo
C$OMP BARRIER
# endif
#endif
C$OMP MASTER
        call get_initial (nrrec, 1)
C$OMP END MASTER
#ifdef ANA_INITIAL
      endif    !<-- nrrec.gt.0
#endif
C$OMP BARRIER


      if (may_day_flag.ne.0) goto 99      !--> ERROR
CR      write(*,*) ' -5' MYID
                              ! Set initial model clock: at this
      time=start_time         ! moment "start_time" (global scalar)
      tdays=time*sec2day      ! is set by get_initial or analytically
                              ! --> copy it into threadprivate "time"
#ifdef SOLVE3D
      do tile=my_tile_range       ! Re-compute three-dimensional S-
        call set_depth (tile)     ! coordinate system: at this moment
      enddo                       ! free surface has non-zero status
C$OMP BARRIER

CR      write(*,*)  ' -4' MYID
      do tile=my_tile_range
        call set_HUV (tile)
      enddo
C$OMP BARRIER
CR      write(*,*)  ' -3' MYID
 
      do tile=my_tile_range
        call omega (tile)
        call rho_eos (tile)
      enddo
C$OMP BARRIER
CR      write(*,*)  ' -2' MYID
# ifdef BIOLOGY_NPZDOC
      call init_biopar               ! set up biogeochemical parameters
# endif /* BIOLOGY_NPZDOC */
#endif
!
! Set up climatological environment: Set nudging coefficient for
!==== == ============== ============ sea-surface hight and tracer
! climatology; create analytical tracer and sea-surface hight
! climatology fields (if applicable); set bottom sediment grain
! size [m] and density [kg/m^3] used for bottom boundary layer
! formulation;
!
#if defined SPONGE || defined TCLIMATOLOGY \
  || (defined SG_BBL96 && defined ANA_BSEDIM)\
  || (defined TCLIMATOLOGY && defined ANA_TCLIMA)\
  || defined ANA_SSH
 
      do tile=my_tile_range
# if defined SPONGE || defined TCLIMATOLOGY 
        call set_nudgcof (tile)
# endif
# if defined TCLIMATOLOGY && defined ANA_TCLIMA && defined SOLVE3D
        call ana_tclima (tile)
# endif
# ifdef ANA_SSH
        call ana_ssh (tile)
# endif
# if defined SG_BBL96 && defined ANA_BSEDIM
        call ana_bsedim (tile)
# endif
      enddo
C$OMP BARRIER
#endif
CR      write(*,*) ' -1' MYID
!
! Read initial input data for forcing fields; tracer and sea surface
! climatology; bottom sediment grain size and density (if applicable)
! from input netCDF files. Recall that CPP-logic here is mutually
! exclussive with respect to cals ana_tclima, ana_ssh, and ana_bsedim
! just above.
!

#if defined SALINITY && defined BIOLOGY_NPZDOC && defined VFLX_CORR
        call reset_global_srf_sum()
! If the virtual flux correction is used, globally averaged concentrations
! for the surface cells must be available for subroutine set_vbc_tile
C$OMP PARALLEL DO PRIVATE(tile)
        do tile=0,NSUB_X*NSUB_E-1
           call calc_global_srf_sum(tile)
        enddo

! Keep this routine out of the parallel region        
        call scale_global_srf_sum()
#endif /* SALINITY && BIOLOGY_NPZDOC && VFLX_CORR */

#ifdef BIOLOGY_NPZDOC
C$OMP PARALLEL DO PRIVATE(tile)
         do tile=0,NSUB_X*NSUB_E-1
            call calcpar(tile)
         enddo
# endif /* BIOLOGY_NPZDOC */

C$OMP MASTER
        if (ldefhis .and. wrthis(indxTime)) call wrt_his
#ifdef STATIONS
        if (nstation.gt.0) def_station
#endif
#ifdef FLOATS
! Initialization for Lagrangian floats
!-------------------------------------------------------
      nrecflt=0    ! initialization done here and not in
      ncidflt=-1   ! init_scalars since it must be done only
                   ! once (whether child levels exist or not)
      spval=1.E15  ! spval is the nodata flag for float variables

      deltac2p=2.3 ! distance from the boundary at which a float
                   ! is transferred from child to parent
      deltap2c=2.5 ! same for transfer from parent to child

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
      call fill_ini ! fills in trackaux for ixgrd,iygrd,izgrd
                    ! and ifld (either izgrd or ifld is meaningful)
# endif
      if (ldefflt) call wrt_floats
#endif /* FLOATS */
C$OMP END MASTER
C$OMP BARRIER
CR      write(*,*) '  0' MYID
      if (may_day_flag.ne.0) goto 99     !-->  EXIT

C$OMP MASTER
        MPI_master_only write(stdout,'(/1x,A/)')
     &               'MAIN: started time-steping.'
C$OMP END MASTER

      iic=ntstart-1
!
! Initialize child-grid model, if any. Note that this call
! introduces recursive sequence, since child-grid model may
! also have child on its own.
!
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

!
!      *****    *********    ******   *******    *********
!    ***   ***  *  ***  *   *   ***   ***   ***  *  ***  *
!    ***           ***     **   ***   ***   ***     ***
!      *****       ***    ***   ***   ***   **      ***
!          ***     ***    *********   ******        ***
!    ***   ***     ***    ***   ***   ***  **       ***
!      *****       ***    ***   ***   ***   ***     ***
!
 
      subroutine roms_step
      implicit none
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
 
      trd=proc(2)
      range=(NSUB_X*NSUB_E+numthreads-1)/numthreads
      my_first=trd*range
      my_last=min(my_first + range-1, NSUB_X*NSUB_E-1)

                                 ! increment time index anc clock
      iic=iic+1 
      time=start_time+dt*float(iic-ntstart)
      tdays=time*sec2day
#ifdef SOLVE3D
      nstp=1+mod(iic-ntstart,2)
      nrhs=nstp
      nnew=3
#endif

#ifdef FLOATS
      nfp1=MOD(nfp1+1,NFT+1)  ! Shift time indices
      nf  =MOD(nf  +1,NFT+1)  ! for floats
      nfm1=MOD(nfm1+1,NFT+1)
      nfm2=MOD(nfm2+1,NFT+1)
      nfm3=MOD(nfm3+1,NFT+1)
C$OMP MASTER
      flt_str=0
C$OMP END MASTER
#endif

      if (synchro_flag) then
        synchro_flag=.false.
C$OMP MASTER
        call get_forces               ! Read forcing/climatology data
C$OMP END MASTER
C$OMP BARRIER
        if (may_day_flag.ne.0) goto 99  !-->  EXIT
      endif
 
#ifdef SOLVE3D
      do tile=my_tile_range                ! Interpolate forcing
        call set_forces (tile)             ! date to model time and
# if defined SSH_TIDES || defined UV_TIDES
        call set_tides  (tile)             ! compute surface fluxes.  
# endif 
        call    rho_eos (tile)
        call    set_HUV (tile)
        call       diag (tile)
#if defined BIOLOGY || defined BIOLOGY_NPZDOC
        call   bio_diag (tile)
#endif
      enddo
C$OMP BARRIER
      if (may_day_flag.ne.0) go to 99  !-->  EXIT
 
 
      do tile=my_tile_range
        call omega (tile)
# if defined ANA_VMIX
        call ana_vmix (tile)
# elif defined LMD_MIXING
        call lmd_vmix (tile)
c        call lmd_kmix (tile)
# elif defined BVF_MIXING
        call bvf_mix (tile)
# endif
      enddo
C$OMP BARRIER
 
      do tile=my_tile_range
        call     prsgrd (tile)
        call      rhs3d (tile)
        call pre_step3d (tile)
# ifdef PRED_COUPLED_MODE
#  ifdef UV_VIS2
        call     visc3d (tile)
#  endif
# endif
# ifdef AVERAGES
        call    set_avg (tile)
#  ifdef BGC_FLUX_ANALYSIS
        call set_bgc_flux_avg(tile)
#  endif
#  ifdef PHYS_FLUX_ANALYSIS
        call set_phys_flux_avg(tile)
#  endif
# endif /* AVERAGES */
      enddo
C$OMP BARRIER

# ifdef CORR_COUPLED_MODE
      do tile=my_tile_range
        call set_HUV1 (tile)
      enddo
C$OMP BARRIER

      nrhs=3
      nnew=3-nstp   !!! WARNING

      do tile=my_tile_range
        call omega (tile)
        call rho_eos (tile)
      enddo
C$OMP BARRIER

      do tile=my_tile_range  ! sensitive
        call     prsgrd (tile)
        call      rhs3d (tile)
        call step3d_uv1 (tile)
#  ifdef UV_VIS2
        call     visc3d (tile)
#  endif
      enddo
C$OMP BARRIER
# endif
#endif  /* SOLVE3D */
!
! Output block: write restart/history files.
!======= ====== ===== =============== ======
!
      if ( iic.gt.ntstart .and. ( mod(iic-ntstart,nrst).eq.0
#ifdef EXACT_RESTART
     &                        .or. mod(iic-ntstart+1,nrst).eq.0
#endif
     &   .or. (mod(iic-ntstart,nwrt).eq.0 .and. wrthis(indxTime))
#ifdef AVERAGES
     &   .or. (mod(iic-ntsavg,navg).eq.0  .and. wrtavg(indxTime))
#endif
#ifdef STATIONS
     &   .or. (mod(iic-ntstart,nsta).eq.0 .and. nstation.gt.0)
#endif
#ifdef FLOATS
     &   .or. (mod(iic-ntstart,nflt).eq.0 .and. nfloats.gt.0)
#endif
     &                                                  )) then
C$OMP MASTER
        if (mod(iic-ntstart,nrst).eq.0
#ifdef EXACT_RESTART
     &                      .or. mod(iic-ntstart+1,nrst).eq.0
#endif
     &                                ) nrecrst=nrecrst+1
        if (mod(iic-ntstart,nwrt).eq.0) nrechis=nrechis+1
#ifdef AVERAGES
        if (mod(iic-ntsavg,navg) .eq.0) nrecavg=nrecavg+1
#endif
#ifdef STATIONS
        if (mod(iic-ntstart,nsta).eq.0) nrecsta=nrecsta+1
#endif
#ifdef FLOATS
        if (mod(iic-ntstart,nflt).eq.0) nrecflt=nrecflt+1
#endif
        if (mod(iic-ntstart,nrst).eq.0
#ifdef EXACT_RESTART
     &                      .or. mod(iic-ntstart+1,nrst).eq.0
#endif
     &    ) call wrt_rst
        if (mod(iic-ntstart,nwrt).eq.0 .and. wrthis(indxTime))
     &      call wrt_his
#ifdef AVERAGES
        if (mod(iic-ntsavg,navg) .eq.0 .and. wrtavg(indxTime))
     &      call wrt_avg
#endif
#ifdef STATIONS
        if (mod(iic-ntstart,nsta).eq.0 .and. nstation.gt.0)
     &      call wrt_station
#endif
#ifdef FLOATS
        if (mod(iic-ntstart,nflt).eq.0 .and. nfloats.gt.0)
     &      call wrt_floats
        diagfloats=.false.
#endif
C$OMP END MASTER
C$OMP BARRIER
        if (may_day_flag .ne. 0)     goto 99   !-->  EXIT
        if (iic-ntstart .gt. ntimes) goto 99   !-->  DONE
      endif

#ifdef FLOATS
! flag for diagnostic computation (for writing at next time step)
      if (mod(iic-ntstart,nflt).eq.0) then
        diagfloats=.true.
      endif
#endif

#ifdef BGC_FLUX_ANALYSIS
      if (iic > ntstart .and. n_bgc_flux_his > 0 .and. 
     &     mod(iic-1, n_bgc_flux_his) == 0) then
         nrec_bgc_flux_his = nrec_bgc_flux_his + 1
         call wrt_bgc_flux_his
      end if
# ifdef AVERAGES
      if (iic > ntstart .and. n_bgc_flux_avg > 0 .and. 
     &     mod(iic-1, n_bgc_flux_avg) == 0) then
         nrec_bgc_flux_avg = nrec_bgc_flux_avg + 1
         call wrt_bgc_flux_avg
      end if
# endif
#endif /* BGC_FLUX_ANALYSIS */
#ifdef PHYS_FLUX_ANALYSIS
      if (iic > ntstart .and. n_phys_flux_his > 0 .and. 
     &     mod(iic-1, n_phys_flux_his) == 0) then
         nrec_phys_flux_his = nrec_phys_flux_his + 1
         call wrt_phys_flux_his
      end if
# ifdef AVERAGES
      if (iic > ntstart .and. n_phys_flux_avg > 0 .and. 
     &     mod(iic-1, n_phys_flux_avg) == 0) then
         nrec_phys_flux_avg = nrec_phys_flux_avg + 1
         call wrt_phys_flux_avg
      end if
# endif
#endif

#ifdef SOLVE3D
!
! Solve the 2D primitive equations for the barotropic mode.
!------ --- -- --------- --------- --- --- ---------- -----
!
      do iif=1,nfast
#endif
#define FORW_BAK
#ifdef FORW_BAK
        kstp=knew                   ! This might look a bit silly,
        knew=kstp+1                 ! since both branches of this
        if (knew.gt.4) knew=1       ! if statement are identical.
        if (mod(knew,2).eq.0) then  ! Nevertheless, it makes sense,
          do tile=my_tile_range     ! since mpc will reverse one of
# ifndef SOLVE3D
            call set_forces (tile)  ! these loops to make zig-zag
# endif
            call     step2d (tile)  ! tile-processing sequence.
          enddo
C$OMP BARRIER
        else
          do tile=my_tile_range
# ifndef SOLVE3D
            call set_forces (tile)
# endif
            call     step2d (tile)
          enddo
C$OMP BARRIER
        endif
#else
        kstp=knew
        knew=3
        do tile=my_tile_range
# ifndef SOLVE3D
          call set_forces (tile)
# endif
          call     step2d (tile)
        enddo
C$OMP BARRIER
        knew=3-kstp
        do tile=my_tile_range
           call step2d (tile)
        enddo
C$OMP BARRIER
#endif
#ifndef SOLVE3D
        if (may_day_flag.ne.0) goto 99 !-->  EXIT
#else
      enddo    ! <-- iif
 
 
# ifdef PRED_COUPLED_MODE
      do tile=my_tile_range      ! This code segment is for
        call set_HUV1 (tile)     ! predictor-coupled version.
      enddo
C$OMP BARRIER
 
      nrhs=3
      nnew=3-nstp

      do tile=my_tile_range
        call omega (tile)
        call rho_eos (tile)
      enddo
C$OMP BARRIER
      do tile=my_tile_range
        call     prsgrd (tile)
        call      rhs3d (tile)
        call step3d_uv1 (tile)
      enddo
C$OMP BARRIER
# endif
                                   ! Continue solution of
      do tile=my_tile_range        ! the three-dimensional
        call step3d_uv2 (tile)     ! equations: finalize
      enddo                        ! time step for momenta
C$OMP BARRIER                      ! and tracers
      do tile=my_tile_range
        call omega (tile)
        call step3d_t (tile)
# if defined TS_DIF2 || defined TS_DIF4
        call t3dmix (tile)
# endif
      enddo
C$OMP BARRIER
#endif /* SOLVE3D */

#ifdef FLOATS
      chunk_size_flt=32
      do while (flt_str.lt.nfloats)
C$OMP CRITICAL
        Lstr=flt_str+1
        flt_str=Lstr+chunk_size_flt-1
C$OMP END CRITICAL
        Lend=min(Lstr+chunk_size_flt-1,nfloats)
        call step_floats (Lstr,Lend)
      enddo
c**    call step_floats (1,nfloats) ! serial version for debugging
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
 
 
