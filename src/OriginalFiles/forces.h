! Surface momentum flux (wind stress):
!======== ======== ==== ===== ========
! sustr   XI- and ETA-components of kinematic surface momentum
! svstr   flux (wind stresses) at defined horizontal U- and
!         V-points,  dimensioned as [m^2/s^2].
! tsms    time of surface momentum stresses.
! sustrg  two-time level gridded data for XI- anf ETA-componets
! svstrg  of wind stess (normally assumed to be in [Newton/m^2].
!
      real sustr(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE sustr(BLOCK_PATTERN) BLOCK_CLAUSE
      real svstr(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE svstr(BLOCK_PATTERN) BLOCK_CLAUSE
      common /forces_sustr/sustr /forces_svstr/svstr
#ifndef ANA_SMFLUX
# if defined SMFLUX_DATA || defined ALL_DATA
#  undef SMFLUX_DATA
      real sustrg(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE sustrg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      real svstrg(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE svstrg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /smsdat_sustrg/sustrg /smsdat_svstrg/svstrg
 
      real sms_cycle, sms_time(2)
      integer sms_ncycle,  sms_rec,  itsms, ntsms,
     &        sms_file_id, sms_tid,  susid, svsid
      common /smsdat/ sms_cycle, sms_time,
     &        sms_ncycle,  sms_rec,  itsms, ntsms,
     &        sms_file_id, sms_tid,  susid, svsid
 
# endif /* SMFLUX_DATA */
#endif /* !ANA_SMFLUX */


#ifdef SOLVE3D
!
! Surface tracer fluxes:
!======== ====== =======
!  stflx   kinematic surface fluxes of tracer type variables at
!          horizontal RHO-points. Physical dimensions [degC m/s]
!          - temperature; [PSU m/s] - salinity.
!  stflxg  two-time level surface tracer flux grided data.
!  tstflx  time of surface tracer flux.
!
      real stflx(GLOBAL_2D_ARRAY,NT)
CSDISTRIBUTE_RESHAPE stflx(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /forces_stflx/stflx
# if !defined ANA_STFLUX || !defined ANA_SSFLUX
#  if defined STFLUX_DATA || defined ALL_DATA
#   undef STFLUX_DATA

      real stflxg(GLOBAL_2D_ARRAY,2,NT)
CSDISTRIBUTE_RESHAPE stflxg(BLOCK_PATTERN,*,*) BLOCK_CLAUSE
      common /stfdat_stflxg/stflxg
 
      real stf_cycle(NT),      stf_time(2,NT)
      integer stf_ncycle(NT),  stf_rec(NT), itstf(NT),  ntstf(NT),
     &        stf_file_id(NT), stf_id(NT),  stf_tid(NT)

      common /stfdat/          stf_cycle,   stf_time,
     &        stf_ncycle,      stf_rec,     itstf,      ntstf,
     &        stf_file_id,     stf_id,      stf_tid
 
#  endif /*  STFLUX_DATA */
# endif /* !ANA_STFLUX || !ANA_SSFLUX */
 
# if defined QCORRECTION && !defined ANA_SST
#  if defined SST_DATA || defined ALL_DATA
#   undef SST_DATA
!
! Heat flux correction:
!===== ==== ===========
! sstg    two-time-level  grided data for sea surface temperature
! dqdtg   SST[deg C] and net surface heat flux sensitivity to SST
!         dQdSST [Watts/m^2/Celsius]
! tsst    time of sea surface temperature data.
!
      real sstg(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE  sstg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      real dqdtg(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE dqdtg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      real sssg(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE  sssg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /sst_dat/sstg /dqdtg_dat/dqdtg /sss_dat/sssg
 
      real sst_cycle, sst_time(2), sss_cycle,  sss_time(2),
     &                             dqdt_cycle, dqdt_time(2)

      integer sst_ncycle,   sst_rec,  itsst,    ntsst,
     &        sst_file_id,  sst_id,   sst_tid,
     &        dqdt_ncycle,  dqdt_rec, itdqdt,   ntdqdt,
     &        dqdt_file_id, dqdt_id,  dqdt_tid,
     &        sss_ncycle,   sss_rec,  itsss,    ntsss,
     &        sss_file_id,  sss_id,   sss_tid

      common /qcorr_data/ sst_cycle,  sst_time, sss_cycle,
     &        sss_time,   dqdt_cycle, dqdt_time,
     &        sst_ncycle,   sst_rec,  itsst,    ntsst,
     &        sst_file_id,  sst_id,   sst_tid,
     &        dqdt_ncycle,  dqdt_rec, itdqdt,   ntdqdt,
     &        dqdt_file_id, dqdt_id,  dqdt_tid,
     &        sss_ncycle,   sss_rec,  itsss,    ntsss, 
     &        sss_file_id,  sss_id,   sss_tid

#  endif /* SST_DATA */
# endif /* QCORRECTION && !ANA_SST */
!
! Solar short-wave radiation flux:
!====== ===== ==== ========= =====
! srflx   kinematic surface shortwave solar radiation flux
!              in [degC m/s] at horizontal RHO-points
! srflxg  two-time-level grided data for surface [Watts/m^2]
! tsrflx  time of solar shortwave radiation flux.
!
      real srflx(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE srflx(BLOCK_PATTERN) BLOCK_CLAUSE
      common /forces_srflx/srflx
# ifndef ANA_SRFLUX
#  if defined SRFLUX_DATA || defined ALL_DATA
#   undef SRFLUX_DATA

      real srflxg(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE srflxg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /srfdat_srflxg/srflxg
 
      real srf_cycle, srf_time(2)
      integer srf_ncycle,  srf_rec, itsrf, ntsrf,
     &        srf_file_id, srf_tid, srf_id

      common /srfdat/ srf_cycle, srf_time,
     &        srf_ncycle,  srf_rec, itsrf, ntsrf,
     &        srf_file_id, srf_tid, srf_id
 
#  endif /* SRFLUX_DATA */
# endif /* !ANA_SRFLUX */
# if defined SG_BBL96 && !defined ANA_WWAVE
#  if defined WWAVE_DATA || defined ALL_DATA
!
!  WIND INDUCED WAVES:
!--------------------------------------------------------------------
!  wwag  |  Two-time-level       | wave amplitude [m]
!  wwdg  |  gridded data         | wave direction [radians]
!  wwpg  |  for wind induced     ! wave period [s]
!
!  wwap  |  Two-time-level       | wave amplitude [m]
!  wwdp  |  point data           | wave direction [radians]
!  wwpp  |  for wind induced     ! wave period [s]
!
!  tww      Time of wind induced waves.
!
      real wwag(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE wwag(BLOCK_PATTERN,*) BLOCK_CLAUSE
      real wwdg(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE wwdg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      real wwpg(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE wwpg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /wwf_wwag/wwag /wwf_wwdg/wwdg /wwf_wwpg/wwpg
 
      real ww_tintrp(2), wwap(2), wwdp(2),  wwpp(2), tww(2), tsww,
     &        ww_tstart, ww_tend, sclwwa,   sclwwd,  sclwwp, wwclen
      integer itww,      twwindx, wwaid,    wwdid,   wwpid,  wwtid
      logical lwwgrd,    wwcycle, ww_onerec
      common /wwfdat/
     &        ww_tintrp, wwap,    wwdp,     wwpp,    tww,    tsww,
     &        ww_tstart, ww_tend, sclwwa,   sclwwd,  sclwwp, wwclen,
     &        itww,      twwindx, wwaid,    wwdid,   wwpid,  wwtid,
     &        lwwgrd,    wwcycle, ww_onerec
 
#   undef WWAVE_DATA
#  endif /* WWAVE_DATA */
# endif /* SG_BBL96 && !ANA_WWAVE */
#endif /* SOLVE3D */
 
 
