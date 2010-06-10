#include "cppdefs.h"

      subroutine setup_kwds (ierr)

!!!!!! WARNING: THIS IS A MACHINE GENERATED CODE, DO NOT EDIT! !!!!!!
!!!!!! This file needs to be updated only if new keywords were !!!!!!
!!!!!! introduced into "read_inp.F". To create or refresh this !!!!!!
!!!!!! file use compile and execute "checkkwds.F" as an        !!!!!!
!!!!!! independent program, or use commands "make checkkwds"   !!!!!!
!!!!!! or "make depend".                                       !!!!!!

      implicit none
      integer ierr, is,ie
#include "param.h"
#include "strings.h"
#ifdef MPI
# include "scalars.h"
#endif
      do is=1,max_opt_size
        kwds(is:is)=' '
      enddo
      is=1
#ifndef NONLIN_EOS
#endif
#ifdef FLOATS
#endif
#ifdef MPI
#endif
#ifdef SOLVE3D
#endif
#ifdef SOLVE3D
# ifdef LMD_KPP
# endif
#endif
#if defined SOLITON
#elif defined SEAMOUNT
#elif defined WAVE_RAD
#elif defined ISWAKE
#elif defined UPWELLING
#elif defined GRAV_ADJ
#elif defined IBERIA
#elif defined RIVER
#elif defined PACIFIC
#elif defined PACIFIC_2D
#elif defined CANBAS2
#elif defined SPIRAL
#elif defined PACBIG
#elif defined ATLSMA
#elif defined USWEST
#elif defined DAMEE_B
#else
#endif
#ifdef MPI
#else
#endif
#ifdef AVERAGES
#endif
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='title'
      kwds(ie+1:ie+1)=' '
      is=ie+2
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='time_stepping'
      kwds(ie+1:ie+1)=' '
      is=ie+2
#ifdef SOLVE3D
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='S-coord'
      kwds(ie+1:ie+1)=' '
      is=ie+2
#endif
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='initial'
      kwds(ie+1:ie+1)=' '
      is=ie+2
#ifdef ANA_INITIAL
#endif
#if defined MPI && defined PARALLEL_FILES
#endif
#ifdef ANA_INITIAL
#endif
#ifndef ANA_GRID
      ie=is + 4
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='grid'
      kwds(ie+1:ie+1)=' '
      is=ie+2
# if defined MPI && defined PARALLEL_FILES
# endif
#endif
#if !defined ANA_SMFLUX  ||  defined SOLVE3D  &&\
   ( !defined ANA_STFLUX  ||\
     (defined SG_BBL96    && !defined ANA_BSEDIM) ||\
     (defined SG_BBL96    && !defined ANA_WWAVE)  ||\
     (defined QCORRECTION && !defined ANA_SST)    ||\
     (defined SALINITY    && !defined ANA_SSFLUX) ||\
     (defined LMD_KPP     && !defined ANA_SRFLUX))
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='forcing'
      kwds(ie+1:ie+1)=' '
      is=ie+2
#if defined MPI && defined PARALLEL_FILES
#endif
#endif
#if defined SOLVE3D &&( defined TCLIMATOLOGY ||( defined TNUDGING &&\
              defined T_FRC_BRY )) && !defined ANA_TCLIMA
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='climatology'
      kwds(ie+1:ie+1)=' '
      is=ie+2
# if defined MPI && defined PARALLEL_FILES
# endif
#endif
#if defined T_FRC_BRY || defined M2_FRC_BRY || \
    defined M3_FRC_BRY || defined Z_FRC_BRY
# ifndef ANA_BRY
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='boundary'
      kwds(ie+1:ie+1)=' '
      is=ie+2
#  if defined MPI && defined PARALLEL_FILES
#  endif
# endif
#endif
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='restart'
      kwds(ie+1:ie+1)=' '
      is=ie+2
# if defined MPI && defined PARALLEL_FILES
# endif
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='history'
      kwds(ie+1:ie+1)=' '
      is=ie+2
# if defined MPI && defined PARALLEL_FILES
# endif
#ifdef AVERAGES
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='averages'
      kwds(ie+1:ie+1)=' '
      is=ie+2
# if defined MPI && defined PARALLEL_FILES
# endif
#endif
#ifdef MOORE_BEC
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='mgm_history'
      kwds(ie+1:ie+1)=' '
      is=ie+2
# if defined MPI && defined PARALLEL_FILES
# endif
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='mgm_avg'
      kwds(ie+1:ie+1)=' '
      is=ie+2
# if defined MPI && defined PARALLEL_FILES
# endif
# ifdef MULT_CLIM_FILE
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='clim_file2'
      kwds(ie+1:ie+1)=' '
      is=ie+2
#  if defined MPI && defined PARALLEL_FILES
#  endif
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='clim_file3'
      kwds(ie+1:ie+1)=' '
      is=ie+2
#  if defined MPI && defined PARALLEL_FILES
#  endif
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='clim_file4'
      kwds(ie+1:ie+1)=' '
      is=ie+2
#  if defined MPI && defined PARALLEL_FILES
#  endif
# endif /* MULT_CLIM_FILE */
#endif   /* MOORE_BEC */
#ifdef STATIONS
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='stations'
      kwds(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef FLOATS
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='floats'
      kwds(ie+1:ie+1)=' '
      is=ie+2
# if defined MPI && defined PARALLEL_FILES
# endif
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='float_fields'
      kwds(ie+1:ie+1)=' '
      is=ie+2
# ifdef AGRIF
# endif
# ifdef AGRIF
# endif
#endif /* FLOATS */
#ifdef ASSIMILATION
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='assimilation'
      kwds(ie+1:ie+1)=' '
      is=ie+2
#endif
      ie=is +22
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='primary_history_fields'
      kwds(ie+1:ie+1)=' '
      is=ie+2
#ifdef SOLVE3D
#endif
#ifdef SOLVE3D
#endif
#ifdef SOLVE3D
      ie=is +24
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='auxiliary_history_fields'
      kwds(ie+1:ie+1)=' '
      is=ie+2
# ifdef SALINITY
#  ifdef LMD_KPP
#  endif
# endif
# ifdef SALINITY
#  ifdef LMD_KPP
#  endif
# endif
# ifdef SALINITY
#  ifdef LMD_KPP
#  endif
# endif
#endif /* SOLVE3D */
#ifdef AVERAGES
      ie=is +16
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='primary_averages'
      kwds(ie+1:ie+1)=' '
      is=ie+2
# ifdef SOLVE3D
# endif
# ifdef SOLVE3D
# endif
# ifdef SOLVE3D
      ie=is +18
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='auxiliary_averages'
      kwds(ie+1:ie+1)=' '
      is=ie+2
#  ifdef SALINITY
#   ifdef LMD_KPP
#   endif
#  endif
#  ifdef SALINITY
#   ifdef LMD_KPP
#   endif
#  endif
#  ifdef SALINITY
#   ifdef LMD_KPP
#   endif
#  endif
# endif /* SOLVE3D */
#endif /* AVERAGES */
#ifdef UV_VIS2
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='lateral_visc'
      kwds(ie+1:ie+1)=' '
      is=ie+2
#endif
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='bottom_drag'
      kwds(ie+1:ie+1)=' '
      is=ie+2
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='gamma2'
      kwds(ie+1:ie+1)=' '
      is=ie+2
#ifdef SOLVE3D
      ie=is + 4
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='rho0'
      kwds(ie+1:ie+1)=' '
      is=ie+2
# ifndef NONLIN_EOS
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='lin_EOS_cff'
      kwds(ie+1:ie+1)=' '
      is=ie+2
#  ifdef SALINITY
#  endif
#  ifdef SALINITY
#  endif
# endif
# ifdef TS_DIF2
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='tracer_diff2'
      kwds(ie+1:ie+1)=' '
      is=ie+2
# endif
# if !defined LMD_MIXING && !defined BVF_MIXING\
  && !defined MY2_MIXING && !defined MY25_MIXING\
                         && !defined PP_MIXING
      ie=is +15
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='vertical_mixing'
      kwds(ie+1:ie+1)=' '
      is=ie+2
# endif
# ifdef MY25_MIXING
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='MY_bak_mixing'
      kwds(ie+1:ie+1)=' '
      is=ie+2
#  ifdef Q_DIF2
#  endif
#  ifdef Q_DIF4
#  endif
# endif
#endif /* SOLVE3D */
#ifdef SPONGE
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='v_sponge'
      kwds(ie+1:ie+1)=' '
      is=ie+2
#endif
#if  defined T_FRC_BRY || defined M2_FRC_BRY || defined TNUDGING \
  || defined Z_FRC_BRY || defined M3_FRC_BRY || defined M2NUDGING \
                                             || defined M3NUDGING
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='nudg_cof'
      kwds(ie+1:ie+1)=' '
      is=ie+2
# ifdef SOLVE3D
# endif
# ifdef SOLVE3D
# endif
# ifdef SOLVE3D
# endif
#endif
#ifdef OBC_FLUX_CORR
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      kwds(is:ie)='tau_obc'
      kwds(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef STATIONS
#endif
#ifdef MPI
#endif
      return
  99  MPI_master_only write(stdout,'(/1x,2A/14x,A)')
     &  'SETUP_KWDS ERROR: Insufficient size of string kwds',
     &  'in file "strings.h".', 'Increase the size it and recompile.'
      ierr=ierr+1
      return
      end
