#include "cppdefs.h"
 
      subroutine get_forces

! Read various forcing and climatological fields from their netCDF
! files and save them as globally accessible arrays in common blocks
! "forces.h" and "climat.h". NOTE that NO EFFORT has been made to
! convert units of these fluxes at this stage (i.e., convert them
! into kinematic fluxes): typically data is assumed to have dynamical
! physical units, thus wind stress is in [Newton/m^2]; surface heat
! and short wave radiation fluxes are in [Watts/m^2]; SST sensitivity
! to heat flux dQdSST is in [Watts/(deg C * m^2)]; surface fresh
! water flux is in [mm/day].

      implicit none
      integer ierr
#include "param.h"
#include "scalars.h"
c--#define VERBOSE
#ifdef VERBOSE
      write(*,*) 'entering get_forces' MYID
#endif
      ierr=0
#if defined M2NUDGING && !defined M2_FRC_BRY && !defined ANA_SSH
      call get_ssh(ierr)
#endif
#if defined UCLIMATOLOGY && !defined ANA_UCLIMA
      call get_uclima(ierr)
#endif
#ifdef SOLVE3D
# if ( defined TCLIMATOLOGY ||( defined TNUDGING && \
       defined T_FRC_BRY )) && !defined ANA_TCLIMA
      call get_tclima(ierr)
#  ifdef TNUDGE_WEIGHTS
      call get_nudg_weights(ierr)
#  endif
# endif
#endif /* SOLVE3D */
#if  defined T_FRC_BRY  || defined M2_FRC_BRY || \
      defined M3_FRC_BRY || defined Z_FRC_BRY
# ifndef ANA_BRY
      call get_bry_all(ierr)
# endif
#endif
#if defined SSH_TIDES || defined UV_TIDES
      call get_tides(ierr)
#endif
#ifdef ICEOBS
      call get_seaice(ierr)
#endif
#ifndef ANA_SMFLUX
      call get_wind(ierr)
# ifdef WIND_MAGN
      call get_windmag(ierr)
# endif
#endif
#ifdef SOLVE3D
# if defined LMD_KPP && !defined ANA_SRFLUX
      call get_swrad(ierr)
# endif

# ifdef BULK_FLUX
      call get_lwrad(ierr)
      call get_tair(ierr)
      call get_rhum(ierr)
      call get_prate(ierr)
# else
#  ifndef ANA_STFLUX
      call get_stflux(itemp, ierr)
#  endif
#  if defined SALINITY && !defined ANA_SSFLUX
      call get_stflux(isalt, ierr)
#  endif
# endif /* BULK_FLUX */

# if defined RIVER_VOLUME
      call get_swf_river
# endif 

# if defined QCORRECTION && !defined ANA_SST
      call get_sst(ierr)
      call get_dqdt(ierr)
# endif
# ifdef SFLX_CORR
      call get_sss(ierr)
# endif
# if defined TSOURCE
      call get_tsource(ierr)
# endif
# if defined BIOLOGY_BEC || defined BIOLOGY_BEC2
      call get_dust(ierr)
      call get_iron(ierr)
# endif
# ifdef PCO2AIR_FORCING
      call get_pco2air(ierr)
# endif
# ifdef N2OAIR_FORCING
      call get_n2oair(ierr)
# endif
# ifdef BIOLOGY_BEC2
#  ifdef NOX_FORCING
      call get_nox(ierr)
#  endif
#  ifdef NHY_FORCING
      call get_nhy(ierr)
#  endif
#  ifdef POX_FORCING
      call get_pox(ierr)
#  endif
#  ifdef RIVER_LOAD_N
      call get_din_river(ierr)
#  endif
#  ifdef RIVER_LOAD_P
      call get_dip_river(ierr)
#  endif
#endif
# if defined PASSIVE_TRACER && defined AGE_DYE_TRACER
      call get_agedye(ierr)
# endif
#endif /* SOLVE3D */
#ifdef SG_BBL96
# ifndef ANA_WWAVE
      call get_wwave(ierr)
# endif
# ifndef ANA_BSEDIM
      call get_bsedim(ierr)
# endif
#endif
      if (ierr /= 0) may_day_flag=2
#ifdef VERBOSE
      write(*,*) 'leaving get_forces' MYID
#endif
      end



#ifndef ANA_SMFLUX
! Wind velocities or wind stress components
!----- ---------- -- ---- ------ -----------
# define get_smth get_wind
# ifdef BULK_SM_UPDATE
#  define timename 'wnd_time'
#  define varname 'uwnd'
#  define varname2 'vwnd'
# else
#  define timename 'sms_time'
#  define varname 'sustr'
#  define varname2 'svstr'
# endif
# ifndef WIND_AT_RHO_POINTS
#  define VAR_GRD_TYPE u_var
#  define VAR_GRD_TYPE2 v_var
# endif
# define WIND_DATA
# define var uwind
# define var2 vwind
# define timevar wnd_time
# define file_id wnd_file_id
# define NC_ID_LIST ncfrc
# define NC_FILE_LIST frcfile
# define varid uwndid
# define varid2 vwndid
# define timevarid wnd_tid
# define record wnd_rec
# define ntsmth ntwnd
# define itsmth itwnd
# define icycle wnd_ncycle
# define cycle_length wnd_cycle
# include "get_smth.F"

# ifdef WIND_MAGN
! Wind or wind stress magnitude (scalar field)
!----- -- ---- ------ --------- ------- -------
#  define get_smth get_windmag
#  define varname 'wmag'
#  define timename 'wnd_time'
#  define WIND_DATA
#  define var windmag
#  define timevar wmag_time
#  define file_id wmag_file_id
#  define varid wmag_id
#  define timevarid wmag_tid
#  define record wmag_rec
#  define ntsmth ntwmag
#  define itsmth itwmag
#  define icycle wmag_ncycle
#  define cycle_length wmag_cycle
#  include "get_smth.F"
# endif
#endif  /* !ANA_SMFLUX */

#if defined M2NUDGING && !defined M2_FRC_BRY && !defined ANA_SSH
! Sea surface height (SSH) climatology
!---- ------- ------ ----- ------------
# define get_smth get_ssh
# define varname 'SSH'
# define timename 'ssh_time'
# define SSH_DATA
# define var sshg 
# define timevar ssh_time
# define file_id ssh_file_id
# define NC_ID_LIST ncidclm
# define NC_FILE_LIST clm_file
# define varid ssh_id
# define timevarid ssh_tid
# define record ssh_rec
# define ntsmth ntssh
# define itsmth itssh
# define icycle ssh_ncycle
# define cycle_length ssh_cycle
# define SET_SMTH set_ssh
# define VAR_CFF 1.0
# define VAR_NOW ssh
# include "get_smth.F"
#endif

#ifdef TNUDGE_WEIGHTS
! Tracer Restoring weights 
!---- ------- ------ ----- ------------
# define get_smth get_nudg_weights
# define varname 'nudg_weights'
# define timename 'nudg_time'
# define NUDG_WEIGHTS_DATA
# define var nudgweightsg 
# define timevar nudg_time
# define file_id nudg_file_id
# define NC_ID_LIST ncidclm
# define NC_FILE_LIST clm_file
# define varid nudg_id
# define timevarid nudg_tid
# define record nudg_rec
# define ntsmth ntnudg
# define itsmth itnudg
# define icycle nudg_ncycle
# define cycle_length nudg_cycle
# define SET_SMTH set_nudg_weights
# define VAR_CFF 1.0
# define VAR_NOW nudgweights
# include "get_smth.F"
#endif

#ifndef ANA_SRFLUX
! Solar short-wave radiation flux [Watts/m^2]
!------ ---------- --------- ---- ------------
# define get_smth get_swrad
# define varname 'swrad'
# define timename 'srf_time'
# define SWRAD_DATA
# define var swradg
# define timevar  srf_time
# define file_id srf_file_id
# define NC_ID_LIST ncfrc
# define NC_FILE_LIST frcfile
# define varid srf_id
# define timevarid srf_tid
# define record srf_rec
# define ntsmth ntsrf
# define itsmth itsrf
# define icycle srf_ncycle
# define cycle_length srf_cycle
# include "get_smth.F"
#endif


#ifdef BULK_FLUX
! Long-wave radiation flux [Watts/m^2]
!---------- ------ -- ---- -----------
# define get_smth get_lwrad
# define varname 'lwrad_down'
# define timename 'lrf_time'
# define LWRAD_DATA
# define var lwradg
# define timevar  lrf_time
# define file_id lrf_file_id
# define varid lrf_id
# define timevarid lrf_tid
# define record lrf_rec
# define ntsmth ntlrf
# define itsmth itlrf
# define icycle lrf_ncycle
# define cycle_length lrf_cycle
# include "get_smth.F"

! Air temperature [degree Celsius] at 2m above ocean surface
!---- ----------- ------- -------- -- -- ----- ----- --------
# define get_smth get_tair
# define varname 'Tair'
# define timename 'tair_time'
# define TAIR_DATA
# define var tairg
# define timevar tair_time
# define file_id tair_file_id
# define varid tair_id
# define timevarid tair_tid
# define record tair_rec
# define ntsmth nttair
# define itsmth ittair
# define icycle tair_ncycle
# define cycle_length tair_cycle
# include "get_smth.F"

! Relative humidity of air [fraction] at 2m above ocean surface
!--------- -------- -- --- ---------- -- -- ----- ------ -------
# define get_smth get_rhum
# define varname 'Qair'
# define timename 'qair_time'
# define RHUM_DATA
# define var rhumg
# define timevar rhum_time
# define file_id rhum_file_id
# define varid rhum_id
# define timevarid rhum_tid
# define record rhum_rec
# define ntsmth ntrhum
# define itsmth itrhum
# define icycle rhum_ncycle
# define cycle_length rhum_cycle
# include "get_smth.F"

! Precifitation rate (a.k.a. rain fall), [cm day-1]
!-------------- ---- ------- ---- ------ -----------
# define get_smth get_prate
# define varname 'rain'
# define timename 'rain_time'
# define PRATE_DATA
# define var prateg
# define timevar prate_time
# define file_id prate_file_id
# define varid prate_id
# define timevarid prate_tid
# define record prate_rec
# define ntsmth ntprate
# define itsmth itprate
# define icycle prate_ncycle
# define cycle_length prate_cycle
# include "get_smth.F"

# ifdef SST_SKIN
          'sst_skin'
# endif
#endif  /* BULK_FLUX */

#if defined QCORRECTION && !defined ANA_SST
# define get_smth get_sst
# define varname 'SST'
# define timename 'sst_time'
# define SST_DATA
# define var sstg
# define timevar sst_time
# define file_id sst_file_id
# define NC_ID_LIST ncfrc
# define NC_FILE_LIST frcfile
# define varid sst_id
# define timevarid sst_tid
# define record sst_rec
# define ntsmth ntsst
# define itsmth itsst
# define icycle sst_ncycle
# define cycle_length sst_cycle
# include "get_smth.F"

# define get_smth get_dqdt
# define varname 'dQdSST'
c--#  define timename 'dqdt_time'
# define DQDT_DATA
# define var dqdtg
# define timevar dqdt_time
# define file_id dqdt_file_id
# define NC_ID_LIST ncfrc
# define NC_FILE_LIST frcfile
# define varid dqdt_id
# define timevarid dqdt_tid
# define record dqdt_rec
# define ntsmth ntdqdt
# define itsmth itdqdt
# define icycle dqdt_ncycle
# define cycle_length dqdt_cycle
# include "get_smth.F"
#endif /* QCORRECTION */

#ifdef SFLX_CORR
# define get_smth get_sss
# define varname 'SSS'
# define timename 'sss_time'
# define SSS_DATA
# define var sssg
# define timevar sss_time
# define file_id sss_file_id
# define NC_ID_LIST ncfrc
# define NC_FILE_LIST frcfile
# define varid sss_id
# define timevarid sss_tid
# define record sss_rec
# define ntsmth ntsss
# define itsmth itsss
# define icycle sss_ncycle
# define cycle_length sss_cycle
# include "get_smth.F"
#endif

#ifdef ICEOBS
! Sea ice concentration, melting, and freezing flux
! --- --- -------------- -------- --- -------- ----
# define get_smth get_seaice
# define timename 'ice_time'
# define varname 'seaice'
# define varname2 'freezing'
# define varname3 'melting'
# define ICEOBS_DATA
# define var sicg
# define var2 freezingg
# define var3 meltingg
# define timevar ice_time
# define file_id ice_file_id
# define NC_ID_LIST ncfrc
# define NC_FILE_LIST frcfile
# define varid sic_id
# define varid2 freez_id
# define varid3 melt_id
# define timevarid ice_tid
# define record ice_rec
# define ntsmth ntice
# define itsmth itice
# define icycle ice_ncycle
# define cycle_length ice_cycle
# include "get_smth.F"
#endif

#if defined BIOLOGY_BEC ||  defined BIOLOGY_BEC2
! dust flux
! ---- ---- 
# define get_smth get_dust
# define DUST_DATA
# define varname 'dust'
# define timename 'dust_time'
# define var dustg
# define timevar dust_time
# define file_id dust_file_id
# define NC_ID_LIST ncfrc
# define NC_FILE_LIST frcfile
# define varid dust_id
# define timevarid dust_tid
# define record dust_rec
# define ntsmth ntdust
# define itsmth itdust
# define icycle dust_ncycle
# define cycle_length dust_cycle
# define SET_SMTH set_dust
# define VAR_CFF 1.0
# define VAR_NOW dust
# include "get_smth.F"
! iron flux
! ---- ---- 
# define get_smth get_iron
# define varname 'iron'
# define timename 'iron_time'
# define IRON_DATA
# define var irong
# define timevar iron_time
# define file_id iron_file_id
# define NC_ID_LIST ncfrc
# define NC_FILE_LIST frcfile
# define varid iron_id
# define timevarid iron_tid
# define record iron_rec
# define ntsmth ntiron
# define itsmth itiron
# define icycle iron_ncycle
# define cycle_length iron_cycle
# define SET_SMTH set_iron
# define VAR_CFF 1.0
# define VAR_NOW iron
# include "get_smth.F"
#endif

# ifdef RIVER_VOLUME
!  River Mass flux (added via precipitation)
!  --- ----- ---- 
#  define get_smth get_swf_river
#  define varname 'swflux_river'
#  define timename 'swflux_time'
#  define RIVER_VOLUME_DATA
#  define var swf_riverg
#  define timevar swf_river_time
#  define file_id swf_river_file_id
#  define NC_ID_LIST ncfrc
#  define NC_FILE_LIST frcfile
#  define varid swf_river_id
#  define timevarid swf_river_tid
#  define record swf_river_rec
#  define ntsmth nt_swf_river
#  define itsmth it_swf_river
#  define icycle swf_river_ncycle
#  define cycle_length swf_river_cycle
#  define SET_SMTH set_swf_river
#  define VAR_CFF 1.0
#  define VAR_NOW swf_river
#  include "get_smth.F"
# endif

# ifdef RIVER_LOAD_N
!  din river flux
!  --- ----- ---- 
#  define get_smth get_din_river
#  define varname 'din_river_flux'
#  define timename 'din_river_time'
#  define RIVER_DATA
#  define var din_riverg
#  define timevar din_river_time
#  define file_id din_river_file_id
#  define NC_ID_LIST ncfrc
#  define NC_FILE_LIST frcfile
#  define varid din_river_id
#  define timevarid din_river_tid
#  define record din_river_rec
#  define ntsmth ntdin_river
#  define itsmth itdin_river
#  define icycle din_river_ncycle
#  define cycle_length din_river_cycle
#  define SET_SMTH set_din_river
#  define VAR_CFF 1.0
#  define VAR_NOW din_river
#  include "get_smth.F"
# endif


# ifdef RIVER_LOAD_P
!  dip river flux
!  --------- ---- 
#  define get_smth get_dip_river
#  define varname 'dip_river_flux'
#  define timename 'dip_river_time'
#  define RIVER_DATA
#  define var dip_riverg
#  define timevar dip_river_time
#  define file_id dip_river_file_id
#  define NC_ID_LIST ncfrc
#  define NC_FILE_LIST frcfile
#  define varid dip_river_id
#  define timevarid dip_river_tid
#  define record dip_river_rec
#  define ntsmth ntdip_river
#  define itsmth itdip_river
#  define icycle dip_river_ncycle
#  define cycle_length dip_river_cycle
#  define SET_SMTH set_dip_river
#  define VAR_CFF 1.0
#  define VAR_NOW dip_river
#  include "get_smth.F"
# endif

# ifdef NHY_FORCING
!  NHY flux
!  --- ---- 
#  define get_smth get_nhy
#  define varname 'nhy_flux'
#  define timename 'nhy_time'
#  define NHY_DATA
#  define var nhyg
#  define timevar nhy_time
#  define file_id nhy_file_id
#  define NC_ID_LIST ncfrc
#  define NC_FILE_LIST frcfile
#  define varid nhy_id
#  define timevarid nhy_tid
#  define record nhy_rec
#  define ntsmth ntnhy
#  define itsmth itnhy
#  define icycle nhy_ncycle
#  define cycle_length nhy_cycle
#  define SET_SMTH set_nhy
#  define VAR_CFF 1.0
#  define VAR_NOW nhy
#  include "get_smth.F"
# endif

# ifdef NOX_FORCING
!  NOX flux
!  --- ---- 
#  define get_smth get_nox
#  define varname 'nox_flux'
#  define timename 'nox_time'
#  define NOX_DATA
#  define var noxg
#  define timevar nox_time
#  define file_id nox_file_id
#  define NC_ID_LIST ncfrc
#  define NC_FILE_LIST frcfile
#  define varid nox_id
#  define timevarid nox_tid
#  define record nox_rec
#  define ntsmth ntnox
#  define itsmth itnox
#  define icycle nox_ncycle
#  define cycle_length nox_cycle
#  define SET_SMTH set_nox
#  define VAR_CFF 1.0
#  define VAR_NOW nox
#  include "get_smth.F"
# endif

# ifdef POX_FORCING
!  POX flux
!  --- ----
#  define get_smth get_pox
#  define varname 'pox_flux'
#  define timename 'pox_time'
#  define POX_DATA
#  define var poxg
#  define timevar pox_time
#  define file_id pox_file_id
#  define NC_ID_LIST ncfrc
#  define NC_FILE_LIST frcfile
#  define varid pox_id
#  define timevarid pox_tid
#  define record pox_rec
#  define ntsmth ntpox
#  define itsmth itpox
#  define icycle pox_ncycle
#  define cycle_length pox_cycle
#  define SET_SMTH set_pox
#  define VAR_CFF 1.0
#  define VAR_NOW pox
#  include "get_smth.F"
# endif

# ifdef PCO2AIR_FORCING
!  pco2air flux
!  ------- ---- 
#  define get_smth get_pco2air
#  define varname 'pco2_air'
#  define timename 'pco2_time'
#  define PCO2AIR_DATA
#  define var pco2airg
#  define timevar pco2air_time
#  define file_id pco2air_file_id
#  define NC_ID_LIST ncfrc
#  define NC_FILE_LIST frcfile
#  define varid pco2air_id
#  define timevarid pco2air_tid
#  define record pco2air_rec
#  define ntsmth ntpco2air
#  define itsmth itpco2air
#  define icycle pco2air_ncycle
#  define cycle_length pco2air_cycle
#  define SET_SMTH set_pco2air
#  define VAR_CFF 1.0
#  define VAR_NOW pco2air
#  include "get_smth.F"
# endif

# ifdef N2OAIR_FORCING
!  n2oair flux
!  ------- ---- 
#  define get_smth get_n2oair
#  define varname 'n2o_air'
#  define timename 'pn2o_time'
#  define N2OAIR_DATA
#  define var n2oairg
#  define timevar n2oair_time
#  define file_id n2oair_file_id
#  define NC_ID_LIST ncfrc
#  define NC_FILE_LIST frcfile
#  define varid n2oair_id
#  define timevarid n2oair_tid
#  define record n2oair_rec
#  define ntsmth ntn2oair
#  define itsmth itn2oair
#  define icycle n2oair_ncycle
#  define cycle_length n2oair_cycle
#  define SET_SMTH set_n2oair
#  define VAR_CFF 1.0
#  define VAR_NOW n2oair
#  include "get_smth.F"
# endif

