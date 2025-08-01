#include "cppdefs.h"

      subroutine init_arrays(tile)
      implicit none
      integer tile, i,j
#include "param.h"
#include "private_scratch.h"
#include "compute_tile_bounds.h"

#ifdef SOLVE3D
      do j=1,6           ! Initialize (first touch) private
        do i=1,N3d       ! scratch arrays in parallel by each
          A3d(i,j)=0.    ! thread.
        enddo
      enddo
      do i=1,N2d
        iA2d(i,1)=0
        iA2d(i,2)=0
      enddo
#endif
      do j=1,32
        do i=1,N2d
          A2d(i,j)=0.
        enddo
      enddo

      call init_arrays_tile(istr,iend,jstr,jend)
      end

      subroutine init_arrays_tile (istr,iend,jstr,jend)

! This routine initialize "first-touches" model shared arrays. Most
! of them are assigned to zeros, vertical mixing coefficients are set
! to their background values and will remain unchanged if no vertical
! mixing scheme is applied. The main point here is that because of the
! "first touch" default data placement policy on Linux operating
! system, this operation actually performs distribution of the shared
! arrays accross the nun-uniform-access memory (NUMA) computer (i.e.,
! within a cluster node) unless another distribution policy is
! specified to override the default.

      implicit none
      integer istr,iend,jstr,jend, i,j,k,itrc
      real, parameter :: init=0.    !!!!  0xFFFA5A5A ==> NaN
#define ALL_DATA
#include "param.h"
#include "scalars.h"
#include "grid.h"
#include "ocean2d.h"
#include "ocean3d.h"
#include "eos_vars.h"
#include "coupling.h"
#include "averages.h"
#include "mixing.h"
#include "forces.h"
#include "climat.h"
#undef ALL_DATA
#ifdef PRINT_TILE_RANGES
# ifdef MPI
#  include "mpif.h"
      integer status(MPI_STATUS_SIZE), blank, ierr
# endif
#endif

#ifdef SOLVE3D
# ifdef BIOLOGY_NPZDOC
#  include "bgcflux.h"
# endif

# if defined BIOLOGY_BEC && defined BGC_FLUX_ANALYSIS && defined AVERAGES
#  include "bec_avg.h"
# endif
# ifdef TSOURCE
#  include "sources.h"
# endif
#endif /* SOLVE3D */

#include "compute_extended_bounds.h"

#ifdef PRINT_TILE_RANGES
# ifdef MPI
      if (mynode>0) then
        call MPI_Recv (blank, 1, MPI_INTEGER, mynode-1,
     &                 1, ocean_grid_comm, status, ierr)
      endif
      i=mynode
# else
      i=proc(2)
# endif
      write(*,'(I4/2(6x,A6,I3,3x,A6,I3))') i, 'istr =',istr,
     &        'iend =',iend,   'jstr =',jstr, 'jend =',jend
      write(*,'(4x,2(6x,A6,I3,3x,A6,I3)/)')   'istrR=',istrR,
     &        'iendR=',iendR, 'jstrR=',jstrR, 'jendR=',jendR
# ifdef MPI
      if (mynode < NNODES) then
        call MPI_Send (blank, 1, MPI_INTEGER, mynode+1,
     &                        1, ocean_grid_comm,  ierr)
      endif
# endif
#endif
      do j=jstrR,jendR               ! Initialize
        do i=istrR,iendR             ! 2-D primitive
          zeta(i,j,1)=0. ! init      ! variables.
          zeta(i,j,2)=init
          zeta(i,j,3)=init

          ubar(i,j,1)=init
          ubar(i,j,2)=init
          ubar(i,j,3)=init

          vbar(i,j,1)=init
          vbar(i,j,2)=init
          vbar(i,j,3)=init
#ifdef AVERAGES
          zeta_avg(i,j)=init
          ubar_avg(i,j)=init
          vbar_avg(i,j)=init
#endif
#ifdef SOLVE3D
          rufrc(i,j)=init
          rufrc(i,j)=init
# ifdef VAR_RHO_2D
          rhoA(i,j)=0.
          rhoS(i,j)=0.
# endif
          Zt_avg1(i,j)=0. !init
          DU_avg1(i,j)=0. !init
          DV_avg1(i,j)=0. !init
          DU_avg2(i,j)=0. !init
          DV_avg2(i,j)=0. !init
#endif

! WARNING: "rmask" MUST BE initialized to all-one state in order to
! read grid variables (coordinates, metric, topography), which should
! not be masked.

#ifdef MASKING
          rmask(i,j)=1.
#endif
        enddo
      enddo
#ifdef SOLVE3D
      do k=1,N                       ! Initialize
        do j=jstrR,jendR             ! 3-D primitive
          do i=istrR,iendR           ! variables.
            u(i,j,k,1)=init
            u(i,j,k,2)=init

            v(i,j,k,1)=init
            v(i,j,k,2)=init

# ifdef SPLIT_EOS
            rho1(i,j,k)=init
            qp1(i,j,k)=init
# else
            rho(i,j,k)=init
# endif
# ifdef AVERAGES
            rho_avg(i,j,k)=init
            u_avg(i,j,k)=init
            v_avg(i,j,k)=init
# endif
          enddo
        enddo
      enddo
      do k=0,N
        do j=jstrR,jendR
          do i=istrR,iendR
            We(i,j,k)=init
            Wi(i,j,k)=init
# ifdef AVERAGES
            w_avg(i,j,k)=init
            wvl_avg(i,j,k)=init
# endif
          enddo
        enddo
      enddo
      do itrc=1,NT
        do k=1,N
          do j=jstrR,jendR
            do i=istrR,iendR
              t(i,j,k,1,itrc)=init
              t(i,j,k,2,itrc)=init
# ifdef AVERAGES
              t_avg(i,j,k,itrc)=init
# endif
            enddo
          enddo
        enddo
      enddo
# ifdef TSOURCE
      do itrc=0,NT
        do k=1,N
          do j=jstrR,jendR
            do i=istrR,iendR
              T_src(i,j,k,itrc)=init
            enddo
          enddo
        enddo
        tsrc_id(itrc)=0
      enddo
      do itrc=1,NT
        do k=1,N
          do j=jstrR,jendR
            do i=istrR,iendR
              Tflx(i,j,k,itrc)=init
            enddo
          enddo
        enddo
      enddo
# endif
#endif /* SOLVE3D */

! Initialize forcing arrays (see "forces.h").

      do j=jstrR,jendR
        do i=istrR,iendR
          sustr(i,j)=init
          svstr(i,j)=init
#ifndef ANA_SMFLUX
          uwind(i,j,1)=init
          vwind(i,j,1)=init
          uwind(i,j,2)=init
          vwind(i,j,2)=init
#endif
        enddo
      enddo
#ifdef SOLVE3D
      do itrc=1,NT
        do j=jstrR,jendR
          do i=istrR,iendR
            stflx(i,j,itrc)=init
# if !defined ANA_STFLUX || !defined ANA_SSFLUX
            stflxg(i,j,1,itrc)=init
            stflxg(i,j,2,itrc)=init
# endif
          enddo
        enddo
      enddo
      do j=jstrR,jendR
        do i=istrR,iendR
# ifdef QCORRECTION
          dqdtg(i,j,1)=init
          sstg (i,j,1)=init
          dqdtg(i,j,2)=init
          sstg (i,j,2)=init
# endif
          srflx(i,j)=init
# ifndef ANA_SRFLUX
          swradg(i,j,1)=init
          swradg(i,j,2)=init
# endif
# ifdef ICEOBS
          sic(i,j)=init
          sicg(i,j,1)=init
          sicg(i,j,2)=init
          freezing(i,j)=init
          freezingg(i,j,1)=init
          freezingg(i,j,2)=init
          melting(i,j)=init
          meltingg(i,j,1)=init
          meltingg(i,j,2)=init
# endif
# if defined SG_BBL96 && !defined ANA_WWAVE
          wwag(i,j,1)=init
          wwdg(i,j,1)=init
          wwpg(i,j,1)=init
          wwag(i,j,2)=init
          wwdg(i,j,2)=init
          wwpg(i,j,2)=init
# endif
        enddo
      enddo
#endif /* SOLVE3D */

! Initialize climatology arrays (see "climat.h").

#if defined M2NUDGING && !defined M2_FRC_BRY
      do j=jstrR,jendR
        do i=istrR,iendR
          ssh(i,j)=init
# ifndef ANA_SSH
          sshg(i,j,1)=init
          sshg(i,j,2)=init
# endif
        enddo
      enddo
#endif
#ifdef SOLVE3D
# ifdef TCLIMATOLOGY
      do itrc=1,NT
        do j=jstrR,jendR
          do i=istrR,iendR
            Tnudgcof(i,j,itrc)=init
          enddo
        enddo
      enddo
# endif
# if (defined TCLIMATOLOGY || defined TNUDGING) && !defined ANA_TCLIMA
      do itrc=1,mxnudg    ! was NT
        do k=1,N
          do j=jstrR,jendR
            do i=istrR,iendR
              tclm(i,j,k,itrc)=init
              tclima(i,j,k,1,itrc)=init
              tclima(i,j,k,2,itrc)=init
            enddo
          enddo
        enddo
      enddo
# endif
#endif

#ifdef UCLIMATOLOGY
# ifndef ANA_UCLIMA
      do j=jstrR,jendR
        do i=istrR,iendR
          ubclm(i,j)=init
          vbclm(i,j)=init
          ubclima(i,j,1)=init
          ubclima(i,j,2)=init
          vbclima(i,j,1)=init
          vbclima(i,j,2)=init
        enddo
      enddo
#  ifdef SOLVE3D
      do k=1,N
        do j=jstrR,jendR
          do i=istrR,iendR
            uclm(i,j,k)=init
            vclm(i,j,k)=init
            uclima(i,j,k,1)=init
            uclima(i,j,k,2)=init
            vclima(i,j,k,1)=init
            vclima(i,j,k,2)=init
          enddo
        enddo
      enddo
#  endif
# endif
#endif

! Set variable horizontal viscosities and tracer diffusion
! coefficients (see "mixing.h") to their background values.

#ifdef UV_VIS2
        do j=jstrR,jendR
          do i=istrR,iendR
            visc2_r(i,j)=visc2
            visc2_p(i,j)=visc2
          enddo
        enddo
#endif
#ifdef SOLVE3D
# ifdef TS_DIF2
        do itrc=1,NT
          do j=jstrR,jendR
            do i=istrR,iendR
              diff2(i,j,itrc)=tnu2(itrc)
            enddo
          enddo
        enddo
# endif

! Initialize vertical mixing coefficients (see "mixing.h") to their
! background values. If no vertical closure scheme is selected, the
! vertical mixing coefficients are those specified by the background
! values.

      do k=0,N
        do j=jstrR,jendR
          do i=istrR,iendR
# if !defined LMD_MIXING && !defined BVF_MIXING && !defined PP_MIXING\
                         && !defined MY2_MIXING && !defined MY25_MIXING

            Akv(i,j,k)=Akv_bak
# else
            Akv(i,j,k)=0.
# endif
# ifdef AVERAGES
            akv_avg(i,j,k)=init
# endif
# if defined BVF_MIXING || defined LMD_MIXING  || defined LMD_KPP \
  || defined MY2_MIXING || defined MY25_MIXING || defined PP_MIXING \
  || defined LMD_BKPP
            bvf(i,j,k)=init
# endif
          enddo
        enddo
        do itrc=1,min(NT,2)
          do j=jstrR,jendR
            do i=istrR,iendR
# if !defined LMD_MIXING && !defined BVF_MIXING && !defined PP_MIXING\
                         && !defined MY2_MIXING && !defined MY25_MIXING
              Akt(i,j,k,itrc)=Akt_bak(itrc)
# else
              Akt(i,j,k,itrc)=0.
# endif
# ifdef AVERAGES
              akt_avg(i,j,k)=init
#  ifdef SALINITY
              aks_avg(i,j,k)=init
#  endif
# endif
            enddo
          enddo
        enddo
      enddo
# if defined LMD_KPP && defined LMD_NONLOCAL
      do k=1,N
        do j=jstrR,jendR
          do i=istrR,iendR
            ghat(i,j,k)=init
          enddo
        enddo
      enddo
# endif
# ifdef LMD_KPP
! Depth of surface planetary boundary layer.
      do j=jstrR,jendR
        do i=istrR,iendR
          hbls(i,j,1)=0.  ! init
          hbls(i,j,2)=0.
#  ifdef ICEOBS
          kbl(i,j)=N
#  endif
#  ifdef AVERAGES
          hbl_avg(i,j)=init
#  endif
        enddo
      enddo
# endif
# ifdef LMD_BKPP
! Thickness of bottom boundary layer.
      do j=jstrR,jendR
        do i=istrR,iendR
          hbbl(i,j,1)=0.  ! init
          hbbl(i,j,2)=0.
#  ifdef AVERAGES
          hbbl_avg(i,j)=init
#  endif
        enddo
      enddo
# endif

# ifdef KPP_DIAGNOSE
      do k=0,N
        do j=jstrR,jendR
          do i=istrR,iendR
            KppRichm(i,j,k)=init
            KppRicht(i,j,k)=init
            KppSblSh(i,j,k)=init
            KppSblSt(i,j,k)=init
            KppSblRot(i,j,k)=init
            KppSblEnt(i,j,k)=init
# ifdef AVERAGES
            kpprichm_avg(i,j,k)=init
            kppricht_avg(i,j,k)=init
            kppsblsh_avg(i,j,k)=init
            kppsblst_avg(i,j,k)=init
            kppsblrot_avg(i,j,k)=init
            kppsblent_avg(i,j,k)=init
# endif
          enddo
        enddo
      enddo
# endif

!AH16:
# if defined WRITE_CO2FLX
! Surface CO2 flux
      do j=jstrR,jendR
        do i=istrR,iendR
          CO2flx(i,j)=0.  ! init
#  ifdef AVERAGES
          co2flx_avg(i,j)=init
#  endif
        enddo
      enddo
# endif
# if defined WRITE_N2OFLX
! Surface N2O flux
      do j=jstrR,jendR
        do i=istrR,iendR
          N2Oflx(i,j)=0.  ! init
#  ifdef AVERAGES
          n2oflx_avg(i,j)=init
#  endif
        enddo
      enddo
# endif
# if defined WRITE_HEATFLX
! Surface heat flux
      do j=jstrR,jendR
        do i=istrR,iendR
          Heatflx(i,j)=0.  ! init
#  ifdef AVERAGES
          heatflx_avg(i,j)=init
#  endif
        enddo
      enddo
# endif
!AH16:
# if defined WRITE_TEMP_REST
! Restoring flux for temperature
      do j=jstrR,jendR
        do i=istrR,iendR
          RestflxTemp(i,j)=0.  ! init
#  ifdef AVERAGES
          restflx_temp_avg(i,j)=init
#  endif
        enddo
      enddo
# endif
!AH16:
# if defined WRITE_SALT_REST
! Restoring flux for salinity
      do j=jstrR,jendR
        do i=istrR,iendR
          RestflxSalt(i,j)=0.  ! init
#  ifdef AVERAGES
          restflx_salt_avg(i,j)=init
#  endif
        enddo
      enddo
# endif

# ifdef SEDIMENT_BIOLOGY
! Initialize sediment array:

      do itrc=1,NT_sed
         do k=1,N
            do j=jstrR,jendR
               do i=istrR,iendR
                  t_sed(i,j,itrc) = init
# ifdef AVERAGES
                  t_sed_avg(i,j,itrc) = init
# endif
               enddo
            enddo
         enddo
      enddo
# endif /* SEDIMENT_BIOLOGY */

! Initialize for NPZDOC model:

# ifdef BIOLOGY_NPZDOC
      do j=jstrR,jendR
         do i=istrR,iendR
#  if defined OXYGEN && defined CARBON
           pH_hist(i,j)=0.
           pCO2sw(i,j)=0.
           pCO2air(i,j)=0.
#  endif
           PARinc(i,j)=0.
#  ifdef AVERAGES
#   if defined OXYGEN && defined CARBON
           pH_avg(i,j)=init
           pCO2_avg(i,j)=init
           pCO2air_avg(i,j)=init
#   endif
           PARinc_avg(i,j)=init
#  endif
         end do
      end do
      do k=1,N
         do j=jstrR,jendR
           do i=istrR,iendR
              PAR(i,j,k)=0.
#  ifdef AVERAGES
              PAR_avg(i,j,k)=init
#  endif
           end do
         end do
      end do
# endif /* BIOLOGY_NPZDOC */
# if defined BGC_FLUX_ANALYSIS && defined AVERAGES
      do j=jstrR,jendR
         do i=istrR,iendR
           PARinc_flux_avg(i,j)=init
         end do
      end do
      do k=1,N
         do j=jstrR,jendR
           do i=istrR,iendR
              PAR_flux_avg(i,j,k)=init
           end do
         end do
      end do
# endif /* BGC_FLUX_ANALYSIS && AVERAGES */
# if defined WRITE_DEPTHS && defined AVERAGES
      do k=1,N
         do j=jstrR,jendR
           do i=istrR,iendR
              z_r_avg(i,j,k)=init
              Hz_avg(i,j,k)=init
           end do
         end do
      end do
      do k=0,N
         do j=jstrR,jendR
           do i=istrR,iendR
              z_w_avg(i,j,k)=init
           end do
         end do
      end do
# endif /* WRITE_DEPTHS && AVERAGES */
#endif /* SOLVE3D */
      end
