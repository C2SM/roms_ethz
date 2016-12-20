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
      write(*,*) 'enter get_forces' MYID
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
#endif
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


# if defined QCORRECTION && !defined ANA_SST
      call get_sst(ierr)
      call get_dqdt(ierr)
# endif
# ifdef SFLX_CORR
      call get_sss(ierr)
# endif
#if defined TSOURCE
      call get_tsource(ierr)
#endif
# if defined BIOLOGY_BEC || defined BIOLOGY_BEC2
      call get_dust(ierr)
      call get_iron(ierr)
# endif
# ifdef PCO2AIR_FORCING
        if (ierr.eq.0)  call get_pCO2air(ierr)
# endif
!# ifdef BIOLOGY_BEC2
!      call get_nox(ierr)
!      call get_nhy(ierr)
!      call get_din_river(ierr)
!# endif
# if defined PASSIVE_TRACER && defined AGE_DYE_TRACER
      call get_agedye(ierr)
# endif
#endif
#if defined SG_BBL96
# ifndef ANA_WWAVE
      call get_wwave(ierr)
# endif
# ifndef ANA_BSEDIM
      call get_bsedim(ierr)
# endif
#endif
      if (ierr /= 0) then
        may_day_flag=2
        write(*,'(1x,2A,2x,A,I4)') '### ERROR: get_forces :: ',
     &          'set may_day_flag = 2' MYID
      endif

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
# define irecord wnd_rec
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
#  define irecord wmag_rec
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
# define irecord ssh_rec
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
# define irecord nudg_rec
# define ntsmth ntnudg
# define itsmth itnudg
# define icycle nudg_ncycle
# define cycle_length nudg_cycle
# define SET_SMTH set_nudg_weights
# define VAR_CFF 1.0
# define VAR_NOW nudgweights
# include "get_smth.F"
#endif

#ifdef SOLVE3D
# ifndef ANA_SRFLUX
! Solar short-wave radiation flux [Watts/m^2]
!------ ---------- --------- ---- ------------
#  define get_smth get_swrad
#  define varname 'swrad'
#  define timename 'srf_time'
#  define SWRAD_DATA
#  define var swradg
#  define timevar  srf_time
#  define file_id srf_file_id
# define NC_ID_LIST ncfrc
# define NC_FILE_LIST frcfile
#  define varid srf_id
#  define timevarid srf_tid
#  define irecord srf_rec
#  define ntsmth ntsrf
#  define itsmth itsrf
#  define icycle srf_ncycle
#  define cycle_length srf_cycle
#  include "get_smth.F"
# endif


# ifdef BULK_FLUX
! Long-wave radiation flux [Watts/m^2]
!---------- ------ -- ---- -----------
#  define get_smth get_lwrad
#  define varname 'lwrad_down'
#  define timename 'lrf_time'
#  define LWRAD_DATA
#  define var lwradg
#  define timevar  lrf_time
#  define file_id lrf_file_id
#  define varid lrf_id
#  define timevarid lrf_tid
#  define irecord lrf_rec
#  define ntsmth ntlrf
#  define itsmth itlrf
#  define icycle lrf_ncycle
#  define cycle_length lrf_cycle
#  include "get_smth.F"

! Air temperature [degree Celsius] at 2m above ocean surface
!---- ----------- ------- -------- -- -- ----- ----- --------
#  define get_smth get_tair
#  define varname 'Tair'
#  define timename 'tair_time'
#  define TAIR_DATA
#  define var tairg
#  define timevar tair_time
#  define file_id tair_file_id
#  define varid tair_id
#  define timevarid tair_tid
#  define irecord tair_rec
#  define ntsmth nttair
#  define itsmth ittair
#  define icycle tair_ncycle
#  define cycle_length tair_cycle
#  include "get_smth.F"

! Relative humidity of air [fraction] at 2m above ocean surface
!--------- -------- -- --- ---------- -- -- ----- ------ -------
#  define get_smth get_rhum
#  define varname 'Qair'
#  define timename 'qair_time'
#  define RHUM_DATA
#  define var rhumg
#  define timevar rhum_time
#  define file_id rhum_file_id
#  define varid rhum_id
#  define timevarid rhum_tid
#  define irecord rhum_rec
#  define ntsmth ntrhum
#  define itsmth itrhum
#  define icycle rhum_ncycle
#  define cycle_length rhum_cycle
#  include "get_smth.F"

! Precifitation rate (a.k.a. rain fall), [cm day-1]
!-------------- ---- ------- ---- ------ -----------
#  define get_smth get_prate
#  define varname 'rain'
#  define timename 'rain_time'
#  define PRATE_DATA
#  define var prateg
#  define timevar prate_time
#  define file_id prate_file_id
#  define varid prate_id
#  define timevarid prate_tid
#  define irecord prate_rec
#  define ntsmth ntprate
#  define itsmth itprate
#  define icycle prate_ncycle
#  define cycle_length prate_cycle
#  include "get_smth.F"

#  ifdef SST_SKIN
          'sst_skin'
#  endif
# endif  /* BULK_FLUX */

# if defined QCORRECTION && !defined ANA_SST
#  define get_smth get_sst
#  define varname 'SST'
#  define timename 'sst_time'
#  define SST_DATA
#  define var sstg
#  define timevar sst_time
#  define file_id sst_file_id
#  define NC_ID_LIST ncfrc
#  define NC_FILE_LIST frcfile
#  define varid sst_id
#  define timevarid sst_tid
#  define irecord sst_rec
#  define ntsmth ntsst
#  define itsmth itsst
#  define icycle sst_ncycle
#  define cycle_length sst_cycle
#  include "get_smth.F"

#  define get_smth get_dqdt
#  define varname 'dQdSST'
c--#  define timename 'dqdt_time'
#  define DQDT_DATA
#  define var dqdtg
#  define timevar dqdt_time
#  define file_id dqdt_file_id
#  define NC_ID_LIST ncfrc
#  define NC_FILE_LIST frcfile
#  define varid dqdt_id
#  define timevarid dqdt_tid
#  define irecord dqdt_rec
#  define ntsmth ntdqdt
#  define itsmth itdqdt
#  define icycle dqdt_ncycle
#  define cycle_length dqdt_cycle
#  include "get_smth.F"
# endif /* QCORRECTION */

# ifdef SFLX_CORR
#  define get_smth get_sss
#  define varname 'SSS'
#  define timename 'sss_time'
#  define SSS_DATA
#  define var sssg
#  define timevar sss_time
#  define file_id sss_file_id
#  define NC_ID_LIST ncfrc
#  define NC_FILE_LIST frcfile
#  define varid sss_id
#  define timevarid sss_tid
#  define irecord sss_rec
#  define ntsmth ntsss
#  define itsmth itsss
#  define icycle sss_ncycle
#  define cycle_length sss_cycle
#  include "get_smth.F"
# endif

# ifdef ICEOBS
! Sea ice concentration, melting, and freezing flux
! --- --- -------------- -------- --- -------- ----
#  define get_smth get_seaice
#  define timename 'ice_time'
#  define varname 'seaice'
#  define varname2 'freezing'
#  define varname3 'melting'
#  define ICEOBS_DATA
#  define var sicg
#  define var2 freezingg
#  define var3 meltingg
#  define timevar ice_time
#  define file_id ice_file_id
#  define NC_ID_LIST ncfrc
#  define NC_FILE_LIST frcfile
#  define varid sic_id
#  define varid2 freez_id
#  define varid3 melt_id
#  define timevarid ice_tid
#  define irecord ice_rec
#  define ntsmth ntice
#  define itsmth itice
#  define icycle ice_ncycle
#  define cycle_length ice_cycle
#  include "get_smth.F"
# endif

# if defined BIOLOGY_BEC ||  defined BIOLOGY_BEC2
! dust flux
! ---- ---- 
#  define get_smth get_dust
#  define DUST_DATA
#  define varname 'dust'
#  define timename 'dust_time'
#  define var dustg
#  define timevar dust_time
#  define file_id dust_file_id
#  define NC_ID_LIST ncfrc
#  define NC_FILE_LIST frcfile
#  define varid dust_id
#  define timevarid dust_tid
#  define irecord dust_rec
#  define ntsmth ntdust
#  define itsmth itdust
#  define icycle dust_ncycle
#  define cycle_length dust_cycle
#  define SET_SMTH set_dust
#  define VAR_CFF 1.0
#  define VAR_NOW dust
#  include "get_smth.F"
! iron flux
! ---- ---- 
#  define get_smth get_iron
#  define varname 'iron'
#  define timename 'iron_time'
#  define IRON_DATA
#  define var irong
#  define timevar iron_time
#  define file_id iron_file_id
#  define NC_ID_LIST ncfrc
#  define NC_FILE_LIST frcfile
#  define varid iron_id
#  define timevarid iron_tid
#  define irecord iron_rec
#  define ntsmth ntiron
#  define itsmth itiron
#  define icycle iron_ncycle
#  define cycle_length iron_cycle
#  define SET_SMTH set_iron
#  define VAR_CFF 1.0
#  define VAR_NOW iron
#  include "get_smth.F"
# endif
#endif /* SOLVE3D */
