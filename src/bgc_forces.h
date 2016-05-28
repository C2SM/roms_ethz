#ifdef PCO2AIR_FORCING
! pCO2air concentration
! ------- -------------
!     pCO2air: pCO2air concentraion [ppm]

      real pCO2air(GLOBAL_2D_ARRAY) 
      common /frc_pCO2air/ pCO2air
CSDISTRIBUTE_RESHAPE  pCO2air(BLOCK_PATTERN,*) BLOCK_CLAUSE
# if defined PCO2AIR_DATA || defined ALL_DATA
#  undef PCO2AIR_DATA
      real pCO2airg(GLOBAL_2D_ARRAY,2)
CSDISTRIBUTE_RESHAPE  pCO2airg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /pCO2airg_dat/ pCO2airg

      real pCO2air_time(2),pCO2air_cycle
      integer itpCO2air, ntpCO2air, pCO2air_id,
     &       pCO2air_ncycle,pCO2air_rec,pCO2air_tid
      common/pCO2airdat/ itpCO2air, ntpCO2air, pCO2air_id,
     &       pCO2air_ncycle,pCO2air_rec,pCO2air_tid
# endif
#endif /* PCO2AIR_FORCING */

#if defined BIOLOGY_BEC || defined BIOLOGY_BEC2
! dust flux
! ---- ----
!      dust: dust flux [kg m-2 s-1]

      real dust(GLOBAL_2D_ARRAY) 
CSDISTRIBUTE_RESHAPE  dust(BLOCK_PATTERN) BLOCK_CLAUSE
           common /frc_dust/dust
# if defined DUST_DATA || defined ALL_DATA
#  undef DUST_DATA
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
#  undef IRON_DATA
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
# ifdef RIVER_SURFACE_FLUX
! river input: DIN, DIP from rivers as surface flux
!        din_river, dip_river
      real, dimension(GLOBAL_2D_ARRAY) :: din_river, dip_river
      common /frc_river/ din_river, dip_river
CSDISTRIBUTE_RESHAPE  din_river(BLOCK_PATTERN) BLOCK_CLAUSE
CSDISTRIBUTE_RESHAPE  dip_river(BLOCK_PATTERN) BLOCK_CLAUSE
#  if defined RIVER_DATA || defined ALL_DATA
      real, dimension(GLOBAL_2D_ARRAY,2) :: din_riverg, dip_riverg
CSDISTRIBUTE_RESHAPE  din_riverg(BLOCK_PATTERN,*) BLOCK_CLAUSE
CSDISTRIBUTE_RESHAPE  dip_riverg(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /riverg_dat/din_riverg,dip_riverg
      real din_river_cycle, din_river_time(2),
     &     dip_river_cycle, dip_river_time(2)
      integer din_river_ncycle,  din_river_rec, itdin_river, ntdin_river,
     &        din_river_file_id, din_river_id,  din_river_tid,
     &        dip_river_ncycle,  dip_river_rec, itdip_river, ntdip_river,
     &        dip_river_file_id, dip_river_id,  dip_river_tid
      common/riverdat/ 
     &        din_river_ncycle,  din_river_rec, itdin_river, ntdin_river,
     &        din_river_file_id, din_river_id,  din_river_tid,
     &        dip_river_ncycle,  dip_river_rec, itdip_river, ntdip_river,
     &        dip_river_file_id, dip_river_id,  dip_river_tid
#  endif /* defined RIVER_DATA || defined ALL_DATA */
# endif /* RIVER_SURFACE_FLUX */
#endif /* BIOLOG_BEC || BIOLOG_BEC2 */ 
