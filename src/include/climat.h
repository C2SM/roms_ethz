! This is include file "climat.h"
!----- -- ------- ---- ----------
! Free surface climatology:
! ==== ======= ============
!   ssh        sea surface height climatology at current time-step.
!   Znudgcof   inverse relaxation time [1/sec] for nudging toward
!                               free surface climatological fields.
!   sshg       two-time-level array to hold climatological data for
!                                                     free surface.
!   tssh       time of read in sea surface height climatology.

#if defined M2NUDGING && !defined M2_FRC_BRY
      real ssh(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE ssh(BLOCK_PATTERN) BLOCK_CLAUSE
      common /climat_ssh/ssh
# ifndef ANA_SSH
#  if defined SSH_DATA || defined ALL_DATA
      real sshg(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE sshg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /climat_sshg/sshg

      real(kind=8) ssh_cycle, ssh_time(2)
      integer itssh, ntssh, ssh_ncycle, ssh_rec, ssh_id, ssh_tid
     &        ssh_file_id
      common /climat_zdat/  ssh_cycle,  ssh_time,
     &        itssh, ntssh, ssh_ncycle, ssh_rec, ssh_id, ssh_tid
     &        ssh_file_id

#   ifndef SET_SMTH
#    undef SSH_DATA
#   endif
#  endif /* SSH_DATA */
# endif /* !ANA_SSH */
#endif
!
! Temperature, salinity climatology:
!------------- -------- ------------
!   tclm         climatology for tracer variables at current time-step.
!   Tnudgcof     inverse relaxation time [1/sec] for nudging toward
!                                       tracer climatological fields.
!   tclima       two-time-level array to hold climatological data for
!                                               tracer variables.
!   ttclm        time of read in climatology for tracer type variables.
!   nudgweights  2D mask with weights defining where to restore temp
!                                       and salinity (MF)
!   nudgweightsg two-time-level array holding climatological data for
!                                       partial restoring of TS (MF)
!
! CPP-switch TNUDGING introduces forcing at the boundary
! only, while TCLIMATOLOGY activates spatially nonuniform nudging
! inside the domain using Tnudgcof specified in set_nudgcof.
!
#ifdef SOLVE3D
# if defined TCLIMATOLOGY || defined TNUDGING
      real tclm(GLOBAL_2D_ARRAY,N,NT)
CSDISTRIBUTE_RESHAPE tclm(BLOCK_PATTERN,*,*) BLOCK_CLAUSE
      common /climat_tclm/tclm
#  ifdef TCLIMATOLOGY
      real Tnudgcof(GLOBAL_2D_ARRAY,NT)
CSDISTRIBUTE_RESHAPE Tnudgcof(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /climat_Tnudgcof/Tnudgcof

! MF: define nudging weights if TNUDGE_WEIGHTS
#   ifdef TNUDGE_WEIGHTS
      real nudgweights(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE nudgweights(BLOCK_PATTERN) BLOCK_CLAUSE
      common /climat_nudgweights/nudgweights
#    if defined TCLIMA_DATA || defined ALL_DATA || defined NUDG_WEIGHTS_DATA
      real nudgweightsg(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE nudgweightsg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /climat_nudgweightsg/nudgweightsg

      real nudg_time(2), nudg_cycle
      integer nudg_ncycle, nudg_rec, itnudg, ntnudg,
     &        nudg_file_id, nudg_tid, nudg_id
      common /climat_nudgdat/         nudg_time,     nudg_cycle,
     &        nudg_ncycle, nudg_rec, itnudg, ntnudg,
     &        nudg_file_id, nudg_tid, nudg_id

#    endif
#    ifndef SET_SMTH
#     undef NUDG_WEIGHTS_DATA
#    endif
#   endif /* NUDG_WEIGHTS_DATA */
! MF: end change (define nudging weights)

#  endif /* TCLIMATOLOGY */
#  ifndef ANA_TCLIMA
#   if defined TCLIMA_DATA || defined ALL_DATA
      real tclima(GLOBAL_2D_ARRAY,N,2,NT)
CSDISTRIBUTE_RESHAPE tclima(BLOCK_PATTERN,*,*,*) BLOCK_CLAUSE
      common /climat_tclima/tclima

      real(kind=8) tclm_cycle(NT), tclm_time(2,NT)
      integer tclm_ncycle(NT), tclm_rec(NT), ittclm(NT),
     &        nttclm(NT),      tclm_tid(NT), tclm_id(NT),
     &        tclm_clmidx(NT)
      common /climat_tdat/     tclm_cycle,   tclm_time,
     &        tclm_ncycle,     tclm_rec,     ittclm,
     &        nttclm,          tclm_tid,     tclm_id,
     &        tclm_clmidx
#    undef TCLIMA_DATA
#   endif
#  endif
# endif
#endif
!
! barotropic and baroclinic velocity climatology:
! ========== === ========== ======== ===========
!   ubclm     climatology for bar. u-velocity at current time-step.
!   vbclm     climatology for bar. v-velocity at current time-step.
!   uclm      climatology for u-velocity at current time-step.
!   vclm      climatology for v-velocity at current time-step.
!
!   ubclima   two-time-level array to hold climatological data
!   vbclima
!   uclima
!   vclima
!
#ifdef UCLIMATOLOGY
      real ubclm(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE ubclm(BLOCK_PATTERN) BLOCK_CLAUSE
      real vbclm(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE vbclm(BLOCK_PATTERN) BLOCK_CLAUSE
      common /climat_ubclm/ubclm /climat_vbclm/vbclm
      real ubclima(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE ubclima(BLOCK_PATTERN,*) BLOCK_CLAUSE
      real vbclima(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE vbclima(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /climat_ubclima/ubclima /climat_vbclima/vbclima

# ifdef SOLVE3D
      real uclm(GLOBAL_2D_ARRAY,N)
CSDISTRIBUTE_RESHAPE uclm(BLOCK_PATTERN,*) BLOCK_CLAUSE
      real vclm(GLOBAL_2D_ARRAY,N)
CSDISTRIBUTE_RESHAPE vclm(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /climat_uclm/uclm /climat_vclm/vclm
      real uclima(GLOBAL_2D_ARRAY,N,2)
CSDISTRIBUTE_RESHAPE uclima(BLOCK_PATTERN,*,*) BLOCK_CLAUSE
      real vclima(GLOBAL_2D_ARRAY,N,2)
CSDISTRIBUTE_RESHAPE vclima(BLOCK_PATTERN,*,*) BLOCK_CLAUSE
      common /climat_uclima/uclima /climat_vclima/vclima
# endif

      real(kind=8) uclm_cycle,  uclm_time(2)
      integer uclm_ncycle,   uclm_rec,  ituclm,  ntuclm,
     &   uclm_tid, ubclm_id, vbclm_id,  uclm_id, vclm_id
      common /climat_udat/   uclm_time, uclm_cycle,
     &        uclm_ncycle,   uclm_rec,  ituclm,  ntuclm,
     &   uclm_tid, ubclm_id, vbclm_id,  uclm_id, vclm_id

#endif /* UCLIMATOLOGY */
