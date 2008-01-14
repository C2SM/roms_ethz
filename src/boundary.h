#ifndef ANA_BRY
      real bry_time(2), bry_cycle
      integer bry_id, bry_time_id, bry_ncycle, bry_rec, itbry, ntbry
      common /bry_indices/ bry_time, bry_cycle,
     &        bry_id, bry_time_id, bry_ncycle, bry_rec, itbry, ntbry

# ifdef OBC_WEST
#  ifdef Z_FRC_BRY
      integer zeta_west_id
      common /bry_indices/ zeta_west_id
#  endif
#  ifdef M2_FRC_BRY
      integer ubar_west_id, vbar_west_id
      common /bry_indices/ ubar_west_id, vbar_west_id
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
      integer u_west_id, v_west_id
      common /bry_indices/ u_west_id, v_west_id
#   endif
#   ifdef T_FRC_BRY
      integer t_west_id(NT)
      common /bry_indices/ t_west_id
#   endif
#  endif
# endif

# ifdef OBC_EAST
#  ifdef Z_FRC_BRY
      integer zeta_east_id
      common /bry_indices/ zeta_east_id
#  endif
#  ifdef M2_FRC_BRY
      integer ubar_east_id, vbar_east_id
      common /bry_indices/ ubar_east_id, vbar_east_id
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
      integer u_east_id, v_east_id
      common /bry_indices/ u_east_id, v_east_id
#   endif
#   ifdef T_FRC_BRY
      integer t_east_id(NT)
      common /bry_indices/ t_east_id
#   endif
#  endif
# endif

# ifdef OBC_SOUTH
#  ifdef Z_FRC_BRY
      integer zeta_south_id
      common /bry_indices/ zeta_south_id
#  endif
#  ifdef M2_FRC_BRY
      integer ubar_south_id, vbar_south_id
      common /bry_indices/ ubar_south_id, vbar_south_id
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
      integer u_south_id, v_south_id
      common /bry_indices/ u_south_id, v_south_id
#   endif
#   ifdef T_FRC_BRY
      integer t_south_id(NT)
      common /bry_indices/ t_south_id
#   endif
#  endif
# endif

# ifdef OBC_NORTH
#  ifdef Z_FRC_BRY
      integer zeta_north_id
      common /bry_indices/ zeta_north_id
#  endif
#  ifdef M2_FRC_BRY
      integer ubar_north_id, vbar_north_id
      common /bry_indices/ ubar_north_id, vbar_north_id
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
      integer u_north_id, v_north_id
      common /bry_indices/ u_north_id, v_north_id
#   endif
#   ifdef T_FRC_BRY
      integer t_north_id(NT)
      common /bry_indices/ t_north_id
#   endif
#  endif
# endif
#endif  /* ANA_BRY */






# ifdef OBC_WEST
#  ifdef Z_FRC_BRY
      real zeta_west(0:Mm+1), zeta_west_dt(0:Mm+1,2)
      common /bry_west/ zeta_west, zeta_west_dt
#  endif
#  ifdef M2_FRC_BRY
      real ubar_west(0:Mm+1), ubar_west_dt(0:Mm+1,2),
     &     vbar_west(0:Mm+1), vbar_west_dt(0:Mm+1,2)
      common /bry_west/ ubar_west, ubar_west_dt,
     &                  vbar_west, vbar_west_dt
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
      real u_west(0:Mm+1,N), u_west_dt(0:Mm+1,N,2),
     &     v_west(0:Mm+1,N), v_west_dt(0:Mm+1,N,2)
      common /bry_west/ u_west, u_west_dt,
     &                  v_west, v_west_dt
#   endif
#   ifdef T_FRC_BRY
      real t_west(0:Mm+1,N,NT), t_west_dt(0:Mm+1,N,2,NT)
      common /bry_west/ t_west, t_west_dt
#   endif
#  endif
# endif

# ifdef OBC_EAST
#  ifdef Z_FRC_BRY
      real zeta_east(0:Mm+1), zeta_east_dt(0:Mm+1,2)
      common /bry_east/ zeta_east, zeta_east_dt
#  endif
#  ifdef M2_FRC_BRY
      real ubar_east(0:Mm+1), ubar_east_dt(0:Mm+1,2),
     &     vbar_east(0:Mm+1), vbar_east_dt(0:Mm+1,2)
      common /bry_east/ ubar_east, ubar_east_dt,
     &                  vbar_east, vbar_east_dt
#  endif
#  ifdef SOLVE3D 
#   ifdef M3_FRC_BRY
      real u_east(0:Mm+1,N), u_east_dt(0:Mm+1,N,2),
     &     v_east(0:Mm+1,N), v_east_dt(0:Mm+1,N,2)
      common /bry_east/ u_east, u_east_dt,
     &                  v_east, v_east_dt
#   endif
#   ifdef T_FRC_BRY
      real t_east(0:Mm+1,N,NT), t_east_dt(0:Mm+1,N,2,NT)
      common /bry_east/ t_east, t_east_dt
#   endif
#  endif
# endif

# ifdef OBC_SOUTH
#  ifdef Z_FRC_BRY 
      real zeta_south(0:Lm+1), zeta_south_dt(0:Lm+1,2)
      common /bry_south/ zeta_south, zeta_south_dt
#  endif
#  ifdef M2_FRC_BRY
      real ubar_south(0:Lm+1), ubar_south_dt(0:Lm+1,2),
     &     vbar_south(0:Lm+1), vbar_south_dt(0:Lm+1,2)
      common /bry_south/ ubar_south, ubar_south_dt,
     &                   vbar_south, vbar_south_dt
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
      real u_south(0:Lm+1,N), u_south_dt(0:Lm+1,N,2),
     &     v_south(0:Lm+1,N), v_south_dt(0:Lm+1,N,2)
      common /bry_south/ u_south, u_south_dt,
     &                   v_south, v_south_dt
#   endif
#   ifdef T_FRC_BRY
      real t_south(0:Lm+1,N,NT), t_south_dt(0:Lm+1,N,2,NT)
      common /bry_south/ t_south, t_south_dt
#   endif
#  endif
# endif

# ifdef OBC_NORTH
#  ifdef Z_FRC_BRY
      real zeta_north(0:Lm+1), zeta_north_dt(0:Lm+1,2)
      common /bry_north/ zeta_north, zeta_north_dt
#  endif
#  ifdef M2_FRC_BRY
      real ubar_north(0:Lm+1), ubar_north_dt(0:Lm+1,2),
     &     vbar_north(0:Lm+1), vbar_north_dt(0:Lm+1,2)
      common /bry_north/ ubar_north, ubar_north_dt,
     &                   vbar_north, vbar_north_dt
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
      real u_north(0:Lm+1,N), u_north_dt(0:Lm+1,N,2),
     &     v_north(0:Lm+1,N), v_north_dt(0:Lm+1,N,2)
      common /bry_north/ u_north, u_north_dt,
     &                   v_north, v_north_dt
#   endif
#   ifdef T_FRC_BRY
      real t_north(0:Lm+1,N,NT), t_north_dt(0:Lm+1,N,2,NT)
      common /bry_north/ t_north, t_north_dt
#   endif
#  endif
# endif

