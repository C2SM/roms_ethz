! Surface momentum flux (wind stress):
!======== ======== ==== ===== ========
! sustr   XI- and ETA-components of kinematic surface momentum
! svstr   flux (wind stresses) at defined horizontal U- and
!         V-points,  dimensioned as [m^2/s^2].

! uwind  two-time level gridded data for XI- anf ETA-componets
! vwind  of wind stess (normally assumed to be in [Newton/m^2].
      real sustr(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE sustr(BLOCK_PATTERN) BLOCK_CLAUSE
      real svstr(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE svstr(BLOCK_PATTERN) BLOCK_CLAUSE
      common /frc_sustr/sustr /frc_svstr/svstr
#ifdef WIND_MAGN
      real wndmag(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE wndmag(BLOCK_PATTERN) BLOCK_CLAUSE
      common /frc_wmag/wndmag
#endif

! Two-time-slice 2D arrays and associated timing variables to store
! wind data read from netCDF file. These fields, "uwind" and "vwind",
! may be either wind stress components or wind velocities (air speed
! measured at the standard height of 10 meters, and to be converted
! into wind stress using bulk formula), so the variable names here
! are kind of "neutral" to fit both possibilities.

#ifndef ANA_SMFLUX
# if defined WIND_DATA || defined ALL_DATA
#  undef WIND_DATA
      real uwind(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE uwind(BLOCK_PATTERN,*) BLOCK_CLAUSE
      real vwind(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE vwind(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /wndat_uwnd/uwind /wndat_vwnd/vwind
#  ifdef WIND_MAGN
      real windmag(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE windmag(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /frc_wmag/windmag
#  endif

      real wnd_time(2), wnd_cycle
      integer wnd_ncycle,  wnd_rec, itwnd,  ntwnd,
     &        wnd_file_id, wnd_tid, uwndid, vwndid
      common /wndat/ wnd_time,      wnd_cycle,
     &        wnd_ncycle,  wnd_rec, itwnd,  ntwnd,
     &        wnd_file_id, wnd_tid, uwndid, vwndid

#  ifdef WIND_MAGN
      real wmag_time(2), wmag_cycle,
      integer wmag_ncycle,  wmag_rec, itwmag, ntwmag,
     &        wmag_file_id, wmag_tid, wmag_id
      common /wmagdat/  wmag_time,    wmag_cycle,
     &        wmag_ncycle,  wmag_rec, itwmag, ntwmag,
     &        wmag_file_id, wmag_tid, wmag_id
#  endif
# endif /* WIND_DATA */
#endif /* !ANA_SMFLUX */


#ifdef SOLVE3D
! Solar short-wave radiation flux:
!------ ---------- --------- ------
! srflx   kinematic surface shortwave solar radiation flux in
!                         [degC m/s] at horizontal RHO-points
! swradg  two-time-level grided data for surface [Watts/m^2]

      real srflx(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE srflx(BLOCK_PATTERN) BLOCK_CLAUSE
      common /frc_srflx/srflx
# ifndef ANA_SRFLUX
#  if defined SWRAD_DATA || defined ALL_DATA
#   undef SWRAD_DATA
      real swradg(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE swradg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /srfdat_srflxg/swradg

      real srf_time(2), srf_cycle
      integer srf_ncycle,  srf_rec, itsrf, ntsrf,
     &        srf_file_id, srf_tid, srf_id
      common /srfdat/ srf_time, srf_cycle,
     &        srf_ncycle,  srf_rec, itsrf, ntsrf,
     &        srf_file_id, srf_tid, srf_id

#  endif /* SWRAD_DATA */
# endif /* !ANA_SRFLUX */



# ifdef BULK_FLUX
! Long-wave radiation flux [Watts/m^2]
!---------- ------ -- ---- -----------
#  if defined LWRAD_DATA || defined ALL_DATA
#   undef LWRAD_DATA
      real lwradg(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE lwflxg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /lwfdat_lwflxg/lwradg

      real lrf_time(2), lrf_cycle
      integer lrf_ncycle,  lrf_rec, itlrf, ntlrf,
     &        lrf_file_id, lrf_tid, lrf_id
      common /lrfdat/ lrf_time, lrf_cycle,
     &        lrf_ncycle,  lrf_rec, itlrf, ntlrf,
     &        lrf_file_id, lrf_tid, lrf_id

#  endif /* SWRAD_DATA */





!  HEAT FLUX BULK FORMULATION
!--------------------------------------------------------------------
!  tair     surface air temperature at 2m [degree Celsius].
!  wsp      wind speed at 10m [degree Celsius].
!  rhum     surface air relative humidity 2m [fraction]
!  prate    surface precipitation rate [cm day-1]
!  radlw    net terrestrial longwave radiation [Watts meter-2]
!  radsw    net solar shortwave radiation [Watts meter-2]


! Air temperature [degree Celsius] at 2m above ocean surface
!---- ----------- ------- -------- -- -- ----- ----- --------
#  if defined TAIR_DATA || defined ALL_DATA
#   undef TAIR_DATA
      real tairg(GLOBAL_2D_ARRAY,2)
      common /bulk_tair/ tairg

      real tair_time(2), tair_cycle
      integer tair_ncycle,  ittair, nttair, tair_rec,
     &        tair_file_id, tair_tid, tair_id
      common /tairdat/ tair_time,   tair_cycle,
     &        tair_ncycle,  ittair, nttair, tair_rec,
     &        tair_file_id, tair_tid, tair_id
#  endif

! Relative humidity of air [fraction] at 2m above ocean surface
!--------- -------- -- --- ---------- -- -- ----- ------ -------
#  if defined RHUM_DATA || defined ALL_DATA
#   undef RHUM_DATA
      real rhumg(GLOBAL_2D_ARRAY,2)
      common /bulk_rhum/ rhumg

      real rhum_time(2), rhum_cycle
      integer rhum_ncycle,  itrhum, ntrhum, rhum_rec,
     &        rhum_file_id, rhum_tid, rhum_id
      common /rhumdat/ rhum_time,  rhum_cycle,
     &        rhum_ncycle,  itrhum, ntrhum, rhum_rec,
     &        rhum_file_id, rhum_tid, rhum_id
#  endif

! Precifitation rate (a.k.a. rain fall), [cm day-1]
!-------------- ---- ------- ---- ------ -----------
#  if defined PRATE_DATA || defined ALL_DATA
#   undef PRATE_DATA
      real prateg(GLOBAL_2D_ARRAY,2)
      common/bulk_prate/ prateg

      real prate_time(2), prate_cycle
      integer prate_ncycle,  itprate, ntprate, prate_rec,
     &        prate_file_id, prate_tid, prate_id
      common /pratedat/ prate_time,   prate_cycle,
     &        prate_ncycle,  itprate, ntprate, prate_rec,
     &        prate_file_id, prate_tid, prate_id
#  endif










      real radlwg(GLOBAL_2D_ARRAY,2)
      real radswg(GLOBAL_2D_ARRAY,2)
#  ifdef DIURNAL_INPUT_SRFLX
      real radswbiog(GLOBAL_2D_ARRAY,2)
#  endif

      common /bulkdat_radlwg/radlwg
     &       /bulkdat_radswg/radswg
#  ifdef DIURNAL_INPUT_SRFLX
     &       /bulkdat_radswbiog/radswbiog
#  endif


# endif /* BULK_FLUX */

!XXXXXXXXXXXXXXXXXXX
!XXXXXXXXXXXXXXXXXXX
!XXXXXXXXXXXXXXXXXXX








! Surface tracer fluxes:
!======== ====== =======
!  stflx   kinematic surface fluxes for tracer-type variables at
!          horizontal RHO-points. Physical dimensions [degC m/s]
!          - temperature; [PSU m/s] - salinity.
!  stflxg  two-time level surface tracer flux grided data.
!  tstflx  time of surface tracer flux.

      real stflx(GLOBAL_2D_ARRAY,NT)
CSDISTRIBUTE_RESHAPE stflx(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /frc_stflx/stflx
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



! Data for net heat flux sensitivity to sea-surface temperature,
! dQdSST = d NetHeatFlux/d SST [Watts/m^2/Celsius, always negative]
! See Eq.(6) from B. Barnier, L. Siefridt, and P. Marchesiello, 1995.

# if defined QCORRECTION && !defined ANA_SST
#  if defined DQDT_DATA || defined ALL_DATA
#   undef DQDT_DATA

      real dqdtg(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE dqdtg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /dqdtg_dat/dqdtg
      real dqdt_cycle, dqdt_time(2)
      integer dqdt_ncycle,  dqdt_rec, itdqdt, ntdqdt,
     &        dqdt_file_id, dqdt_id,  dqdt_tid
      common /dqdtdat/ dqdt_cycle,    dqdt_time,
     &        dqdt_ncycle,  dqdt_rec, itdqdt, ntdqdt,
     &        dqdt_file_id, dqdt_id,  dqdt_tid

#  endif /* SST_DATA */
# endif /* QCORRECTION && !ANA_SST */


! Sea-surface temperature (SST) data

# if defined QCORRECTION && !defined ANA_SST
#  if defined SST_DATA || defined ALL_DATA
#   undef SST_DATA

      real sstg(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE  sstg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /sst_dat/sstg
      real sst_cycle, sst_time(2)
      integer sst_ncycle,  sst_rec, itsst, ntsst,
     &        sst_file_id, sst_id,  sst_tid
      common /sstdat/ sst_cycle,    sst_time,
     &        sst_ncycle,  sst_rec, itsst, ntsst,
     &        sst_file_id, sst_id,  sst_tid

#  endif /* SST_DATA */
# endif /* QCORRECTION && !ANA_SST */

# if defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC || defined BIOLOGY_BEC2
#include "bgc_forces.h"
# endif /* BIOLOG_BEC || BIOLOG_BEC2 */ 

! Sea-surface salinity (SSS) data

# if defined SFLX_CORR && defined SALINITY
#  if defined SSS_DATA || defined ALL_DATA
      real sssg(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE  sssg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /sss_dat/sssg
      real sss_cycle,  sss_time(2)
      integer sss_ncycle,  itsss,  ntsss, sss_rec,
     &        sss_file_id, sss_id, sss_tid
      common /sssdat/  sss_cycle,  sss_time,
     &        sss_ncycle,  itsss,  ntsss, sss_rec,
     &        sss_file_id, sss_id, sss_tid
#  endif /* SSS_DATA */
# endif 






# if defined SG_BBL96 && !defined ANA_WWAVE
#  if defined WWAVE_DATA || defined ALL_DATA

!  WIND INDUCED WAVES:
!----------------------------------------------------------
!  wwag  |  Two-time-level       | wave amplitude [m]
!  wwdg  |  gridded data         | wave direction [radians]
!  wwpg  |  for wind induced     ! wave period [s]
!
!  wwap  |  Two-time-level       | wave amplitude [m]
!  wwdp  |  point data           | wave direction [radians]
!  wwpp  |  for wind induced     ! wave period [s]
!
!  tww      Time of wind induced waves.

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

# ifdef ICEOBS
! Sea-ice observation data:
! ------ ---------- --------- ------
!      sic: sea ice fraction [-]
! freezing: sea ice freezing [cm/day]
!  melting: sea ice melting [cm/day]

      real sic(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE sic(BLOCK_PATTERN) BLOCK_CLAUSE
      real freezing(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE freezing(BLOCK_PATTERN) BLOCK_CLAUSE
      real melting(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE melting(BLOCK_PATTERN) BLOCK_CLAUSE
      common /frc_sic/sic /frc_freezing/freezing /frc_melting/melting
#  if defined ICEOBS_DATA || defined ALL_DATA
#   undef ICEOBS_DATA
      real, dimension(GLOBAL_2D_ARRAY,2) :: sicg, freezingg, meltingg
CSDISTRIBUTE_RESHAPE sicg(BLOCK_PATTERN,*) BLOCK_CLAUSE
CSDISTRIBUTE_RESHAPE freezingg(BLOCK_PATTERN,*) BLOCK_CLAUSE
CSDISTRIBUTE_RESHAPE meltingg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /forces_iceobs/sicg, freezingg, meltingg

      real ice_time(2), ice_cycle
      integer ice_ncycle,  ice_rec, itice, ntice,
     &        ice_file_id, ice_tid, sic_id, freez_id, melt_id
      common /icedat/ ice_time, ice_cycle,
     &        ice_ncycle,  ice_rec, itice, ntice,
     &        ice_file_id, ice_tid, sic_id, freez_id, melt_id
#  endif /* defined ICEOBS_DATA || defined ALL_DATA */
# endif /* ICEOBS */
#endif /* SOLVE3D */
