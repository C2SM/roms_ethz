#ifdef PCO2AIR_FORCING
! pCO2air concentration
! ------- -------------
!     pCO2air: pCO2air concentraion [ppm]

      real pco2air(GLOBAL_2D_ARRAY)
      common /frc_pco2air/ pco2air
CSDISTRIBUTE_RESHAPE  pCO2air(BLOCK_PATTERN,*) BLOCK_CLAUSE
# if defined PCO2AIR_DATA || defined ALL_DATA
# ifndef SET_SMTH
#  undef PCO2AIR_DATA
# endif
      real pco2airg(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE  pco2airg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /pco2airg_dat/ pco2airg
      real pco2air_cycle, pco2air_time(2)
      integer pco2air_ncycle,  pco2air_rec, itpco2air, ntpco2air,
     &        pco2air_file_id, pco2air_id,  pco2air_tid
      common /pco2airdat/ pco2air_cycle,    pco2air_time,
     &        pco2air_ncycle,  pco2air_rec, itpco2air, ntpco2air,
     &        pco2air_file_id, pco2air_id,  pco2air_tid
# endif
#endif /* PCO2AIR_FORCING */

#ifdef N2OAIR_FORCING
! N2Oair concentration
! ------- -------------
!     N2Oair: N2Oair concentraion [ppb]

      real n2oair(GLOBAL_2D_ARRAY)
      common /frc_n2oair/ n2oair
CSDISTRIBUTE_RESHAPE  N2Oair(BLOCK_PATTERN,*) BLOCK_CLAUSE
# if defined N2OAIR_DATA || defined ALL_DATA
# ifndef SET_SMTH
#  undef N2OAIR_DATA
# endif
      real n2oairg(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE  n2oairg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /n2oairg_dat/ n2oairg
      real n2oair_cycle, n2oair_time(2)
      integer n2oair_ncycle,  n2oair_rec, itn2oair, ntn2oair,
     &        n2oair_file_id, n2oair_id,  n2oair_tid
      common /n2oairdat/ n2oair_cycle,    n2oair_time,
     &        n2oair_ncycle,  n2oair_rec, itn2oair, ntn2oair,
     &        n2oair_file_id, n2oair_id,  n2oair_tid
# endif
#endif /* N2OAIR_FORCING */

!--------- NHY_FORCING_START

#ifdef NHY_FORCING
! NHY flux
! --- ----

      real nhy(GLOBAL_2D_ARRAY)
      common /frc_nhy/ nhy
CSDISTRIBUTE_RESHAPE  nhy(BLOCK_PATTERN,*) BLOCK_CLAUSE
# if defined NHY_DATA || defined ALL_DATA
# ifndef SET_SMTH
#  undef NHY_DATA
# endif
      real nhyg(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE  nhyg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /nhyg_dat/ nhyg
      real nhy_cycle, nhy_time(2)
      integer nhy_ncycle,  nhy_rec, itnhy, ntnhy,
     &        nhy_file_id, nhy_id, nhy_tid
      common /nhydat/ nhy_cycle, nhy_time,
     &        nhy_ncycle, nhy_rec, itnhy, ntnhy,
     &        nhy_file_id, nhy_id, nhy_tid
# endif
#endif /* NHY_FORCING */
!--------- NHY_FORCING_END

#ifdef NOX_FORCING
! NOX flux
! --- ----

      real nox(GLOBAL_2D_ARRAY)
      common /frc_nox/ nox
CSDISTRIBUTE_RESHAPE  nox(BLOCK_PATTERN,*) BLOCK_CLAUSE
# if defined NOX_DATA || defined ALL_DATA
# ifndef SET_SMTH
#  undef NOX_DATA
# endif
      real noxg(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE  noxg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /noxg_dat/ noxg
      real nox_cycle, nox_time(2)
      integer nox_ncycle,  nox_rec, itnox, ntnox,
     &        nox_file_id, nox_id, nox_tid
      common /noxdat/ nox_cycle, nox_time,
     &        nox_ncycle, nox_rec, itnox, ntnox,
     &        nox_file_id, nox_id, nox_tid
# endif
#endif /* NOX_FORCING */

#ifdef POX_FORCING
! POX flux
! --- ----

      real pox(GLOBAL_2D_ARRAY)
      common /frc_pox/ pox
CSDISTRIBUTE_RESHAPE  pox(BLOCK_PATTERN,*) BLOCK_CLAUSE
# if defined POX_DATA || defined ALL_DATA
# ifndef SET_SMTH
#  undef POX_DATA
# endif
      real poxg(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE  poxg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /poxg_dat/ poxg
      real pox_cycle, pox_time(2)
      integer pox_ncycle,  pox_rec, itpox, ntpox,
     &        pox_file_id, pox_id, pox_tid
      common /poxdat/ pox_cycle, pox_time,
     &        pox_ncycle, pox_rec, itpox, ntpox,  
     &        pox_file_id, pox_id, pox_tid
# endif
#endif /* POX_FORCING */


#if defined BIOLOGY_BEC || defined BIOLOGY_BEC2
! dust flux
! ---- ----
!      dust: dust flux [kg m-2 s-1]

      real dust(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE  dust(BLOCK_PATTERN) BLOCK_CLAUSE
           common /frc_dust/dust
# if defined DUST_DATA || defined ALL_DATA
#  ifndef SET_SMTH
#   undef DUST_DATA
#  endif
      real dustg(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE  dustg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /dustg_dat/dustg
      real dust_cycle, dust_time(2)
      integer dust_ncycle,  dust_rec, itdust, ntdust,
     &        dust_file_id, dust_id,  dust_tid
      common /dustdat/ dust_cycle,    dust_time,
     &        dust_ncycle,  dust_rec, itdust, ntdust,
     &        dust_file_id, dust_id,  dust_tid
# endif /* defined DUST_DATA || defined ALL_DATA */

! iron flux
! ---- ----
!     iron: iron flux [nmol cm-2 s-1]

      real iron(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE  iron(BLOCK_PATTERN) BLOCK_CLAUSE
           common /frc_iron/iron
# if defined IRON_DATA || defined ALL_DATA
#  ifndef SET_SMTH
#   undef IRON_DATA
#  endif
      real irong(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE  irong(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /irong_dat/irong
      real iron_cycle, iron_time(2)
      integer iron_ncycle,  iron_rec, itiron, ntiron,
     &        iron_file_id, iron_id,  iron_tid
      common /irondat/ iron_cycle,    iron_time,
     &        iron_ncycle,  iron_rec, itiron, ntiron,
     &        iron_file_id, iron_id,  iron_tid
# endif /* defined IRON_DATA || defined ALL_DATA */

# if defined  RIVER_LOAD_N || defined RIVER_LOAD_P
! river input: DIN, DIP from rivers as surface flux
!        din_river, dip_river
      real, dimension(GLOBAL_2D_ARRAY) :: din_river, dip_river
      common /frc_river/ din_river, dip_river
CSDISTRIBUTE_RESHAPE  din_river(BLOCK_PATTERN) BLOCK_CLAUSE
CSDISTRIBUTE_RESHAPE  dip_river(BLOCK_PATTERN) BLOCK_CLAUSE
#  if defined RIVER_DATA || defined ALL_DATA
#   ifndef SET_SMTH
#    undef RIVER_DATA
#   endif
      real, dimension(GLOBAL_2D_ARRAY,2) :: din_riverg, dip_riverg
CSDISTRIBUTE_RESHAPE  din_riverg(BLOCK_PATTERN,*) BLOCK_CLAUSE
CSDISTRIBUTE_RESHAPE  dip_riverg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /riverg_dat/din_riverg,dip_riverg
      real din_river_cycle, din_river_time(2),
     &     dip_river_cycle, dip_river_time(2)
      integer din_river_ncycle, din_river_rec, itdin_river, ntdin_river,
     &        din_river_file_id, din_river_id, din_river_tid,
     &        dip_river_ncycle, dip_river_rec, itdip_river, ntdip_river,
     &        dip_river_file_id, dip_river_id,  dip_river_tid
      common/riverdat/
     &        din_river_cycle, din_river_time,
     &        dip_river_cycle, dip_river_time,
     &        din_river_ncycle, din_river_rec, itdin_river, ntdin_river,
     &        din_river_file_id, din_river_id, din_river_tid,
     &        dip_river_ncycle, dip_river_rec, itdip_river, ntdip_river,
     &        dip_river_file_id, dip_river_id, dip_river_tid
#  endif /* defined RIVER_DATA || defined ALL_DATA */
# endif /* RIVER_LOAD_N || RIVER_LOAD_P */

# ifdef RIVER_LOAD_ALK_DIC_SI
      real, dimension(GLOBAL_2D_ARRAY) :: alk_river, dic_river, si_river
      common /frc_river_alk/ alk_river, dic_river, si_river
CSDISTRIBUTE_RESHAPE  alk_river(BLOCK_PATTERN) BLOCK_CLAUSE
CSDISTRIBUTE_RESHAPE  dic_river(BLOCK_PATTERN) BLOCK_CLAUSE
CSDISTRIBUTE_RESHAPE  si_river(BLOCK_PATTERN) BLOCK_CLAUSE
#  if defined RIVER_DATA_ALK_DIC_SI || defined ALL_DATA
#   ifndef SET_SMTH
#    undef RIVER_DATA_ALK_DIC_SI
#   endif
      real, dimension(GLOBAL_2D_ARRAY,2) :: alk_riverg, dic_riverg, si_riverg
CSDISTRIBUTE_RESHAPE  alk_riverg(BLOCK_PATTERN,*) BLOCK_CLAUSE
CSDISTRIBUTE_RESHAPE  dic_riverg(BLOCK_PATTERN,*) BLOCK_CLAUSE
CSDISTRIBUTE_RESHAPE  si_riverg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /riverg_dat_alk/alk_riverg,dic_riverg,si_riverg
      real alk_river_cycle, alk_river_time(2),
     &     dic_river_cycle, dic_river_time(2),
     &     si_river_cycle,  si_river_time(2)
      integer alk_river_ncycle, alk_river_rec, italk_river, ntalk_river,
     &        alk_river_file_id, alk_river_id, alk_river_tid,
     &        dic_river_ncycle, dic_river_rec, itdic_river, ntdic_river,
     &        dic_river_file_id, dic_river_id,  dic_river_tid,
     &        si_river_ncycle, si_river_rec, itsi_river, ntsi_river,
     &        si_river_file_id, si_river_id,  si_river_tid
      common/riverdat_alk/
     &        alk_river_cycle, alk_river_time,
     &        dic_river_cycle, dic_river_time,
     &        si_river_cycle,  si_river_time,
     &        alk_river_ncycle, alk_river_rec, italk_river, ntalk_river,
     &        alk_river_file_id, alk_river_id, alk_river_tid,
     &        dic_river_ncycle, dic_river_rec, itdic_river, ntdic_river,
     &        dic_river_file_id, dic_river_id, dic_river_tid,
     &        si_river_ncycle, si_river_rec, itsi_river, ntsi_river,
     &     si_river_file_id, si_river_id, si_river_tid
#  endif /* defined RIVER_DATA_ALK_DIC_SI || defined ALL_DATA */
# endif  /* RIVER_LOAD_ALK_DIC_SI */
#endif /* BIOLOG_BEC || BIOLOG_BEC2 */
