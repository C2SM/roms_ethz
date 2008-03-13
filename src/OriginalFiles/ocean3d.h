! This is include file "ocean3d.h".
!----- -- ------- ---- ------------
!
#ifdef SOLVE3D
      real u(GLOBAL_2D_ARRAY,N,3)
CSDISTRIBUTE_RESHAPE u(BLOCK_PATTERN,*,*) BLOCK_CLAUSE
      real v(GLOBAL_2D_ARRAY,N,3)
CSDISTRIBUTE_RESHAPE v(BLOCK_PATTERN,*,*) BLOCK_CLAUSE
      real t(GLOBAL_2D_ARRAY,N,3,NT)
CSDISTRIBUTE_RESHAPE t(BLOCK_PATTERN,*,*,*) BLOCK_CLAUSE
      common /ocean_u/u /ocean_v/v /ocean_t/t
 
      real Hz(GLOBAL_2D_ARRAY,N)
CSDISTRIBUTE_RESHAPE Hz(BLOCK_PATTERN,*) BLOCK_CLAUSE
      real Hz_bak(GLOBAL_2D_ARRAY,N)
CSDISTRIBUTE_RESHAPE Hz_bak(BLOCK_PATTERN,*) BLOCK_CLAUSE
      real z_r(GLOBAL_2D_ARRAY,N)
CSDISTRIBUTE_RESHAPE z_r(BLOCK_PATTERN,*) BLOCK_CLAUSE
      real z_w(GLOBAL_2D_ARRAY,0:N)
CSDISTRIBUTE_RESHAPE z_w(BLOCK_PATTERN,*) BLOCK_CLAUSE
      real FlxU(GLOBAL_2D_ARRAY,N)
CSDISTRIBUTE_RESHAPE  FlxU(BLOCK_PATTERN,*) BLOCK_CLAUSE
      real FlxV(GLOBAL_2D_ARRAY,N)
CSDISTRIBUTE_RESHAPE  FlxV(BLOCK_PATTERN,*) BLOCK_CLAUSE
      real W(GLOBAL_2D_ARRAY,0:N)
CSDISTRIBUTE_RESHAPE W(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /grid_Hz/Hz    /grid_zr/z_r  /grid_W/W
     &  /grid_Hz_bak/Hz_bak /grid_zw/z_w  /grid_FlxU/FlxU
     &                                    /grid_FlxV/FlxV
#endif  /* SOLVE3D */
 
