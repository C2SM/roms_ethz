! This is include file "grid.h": Environmental two-dimensional arrays
! associated with curvilinear horizontal coordinate system.
!
! h         Model bottom topography (depth [m] at RHO-points.)
! f, fomn   Coriolis parameter [1/s] and compound term f/[pm*pn]
!                                                   at RHO points.
! angler      Angle [radians] between XI-axis and the direction
!                                       to the EAST at RHO-points.
! latr, lonr  Latitude (degrees north) and Longitude (degrees east)
!                                                  at RHO-points.
! xr, xp      XI-coordinates [m] at RHO- and PSI-points.
! yr, yp      ETA-coordinates [m] at RHO- and PSI-points.
!
! pm, pm  Coordinate transformation metric "m" and "n" associated
!         with the differential distances in XI- and ETA-directions.
!
! dm_u, dm_r  Grid spacing [meters] in the XI-direction
! dm_v, dm_p       at U-, RHO-,  V- and vorticity points.
! dn_u, dn_r  Grid spacing [meters] in the ETA-direction
! dn_v, dn_p      at U-, RHO-,  V- and vorticity points.
!
! dmde     ETA-derivative of inverse metric factor "m" d(1/M)/d(ETA)
! dndx     XI-derivative  of inverse metric factor "n" d(1/N)/d(XI)
!
! pmon_u   Compound term, pm/pn at U-points.
! pnom_v   Compound term, pn/pm at V-points.
!
! umask, rmask  Land-sea masking arrays at RHO-,U-,V- and PSI-points
! pmask, vmask      (rmask,umask,vmask) = (0=Land, 1=Sea);
!                    pmask = (0=Land, 1=Sea, 1-gamma2 =boundary).
!
      real h(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE h(BLOCK_PATTERN) BLOCK_CLAUSE
      real hinv(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE h(BLOCK_PATTERN) BLOCK_CLAUSE
      real f(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE f(BLOCK_PATTERN) BLOCK_CLAUSE
      real fomn(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE fomn(BLOCK_PATTERN) BLOCK_CLAUSE
      common /grid_h/h /grid_hinv/hinv /grid_f/f /grid_fomn/fomn
 
# ifdef CURVGRID
      real angler(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE angler(BLOCK_PATTERN) BLOCK_CLAUSE
      common /grid_angler/angler
# endif
 
#ifdef SPHERICAL
      real latr(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE   latr(BLOCK_PATTERN) BLOCK_CLAUSE
      real lonr(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE   lonr(BLOCK_PATTERN) BLOCK_CLAUSE
      common /grid_latr/latr /grid_lonr/lonr
#else
      real xp(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE xp(BLOCK_PATTERN) BLOCK_CLAUSE
      real xr(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE xr(BLOCK_PATTERN) BLOCK_CLAUSE
      real yp(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE yp(BLOCK_PATTERN) BLOCK_CLAUSE
      real yr(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE yr(BLOCK_PATTERN) BLOCK_CLAUSE
      common /grid_xr/xr /grid_xp/xp /grid_yp/yp /grid_yr/yr
#endif
 
      real pm(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE pm(BLOCK_PATTERN) BLOCK_CLAUSE
      real pn(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE pn(BLOCK_PATTERN) BLOCK_CLAUSE
      real dm_r(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE dm_r(BLOCK_PATTERN) BLOCK_CLAUSE
      real dn_r(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE dn_r(BLOCK_PATTERN) BLOCK_CLAUSE
      real dm_u(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE dm_u(BLOCK_PATTERN) BLOCK_CLAUSE
      real dn_u(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE dn_u(BLOCK_PATTERN) BLOCK_CLAUSE
      real dm_v(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE dm_v(BLOCK_PATTERN) BLOCK_CLAUSE
      real dn_v(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE dn_v(BLOCK_PATTERN) BLOCK_CLAUSE
      real dm_p(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE dm_p(BLOCK_PATTERN) BLOCK_CLAUSE
      real dn_p(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE on_p(BLOCK_PATTERN) BLOCK_CLAUSE
      common /metrics_pm/pm     /metrics_pn/pn
     &       /metrics_dm_r/dm_r /metrics_dn_r/dn_r
     &       /metrics_dm_u/dm_u /metrics_dn_u/dn_u
     &       /metrics_dm_v/dm_v /metrics_dn_v/dn_v
     &       /metrics_dm_p/dm_p /metrics_dn_p/dn_p
 
#if (defined CURVGRID && defined UV_ADV)
      real dmde(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE dmde(BLOCK_PATTERN) BLOCK_CLAUSE
      real dndx(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE dndx(BLOCK_PATTERN) BLOCK_CLAUSE
      common /metrics_dmde/dmde   /metrics_dndx/dndx
#endif
      real pmon_u(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE pmon_u(BLOCK_PATTERN) BLOCK_CLAUSE
      real pnom_v(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE pnom_v(BLOCK_PATTERN) BLOCK_CLAUSE
      real grdscl(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE grdscl(BLOCK_PATTERN) BLOCK_CLAUSE
      common /metrics_pmon_u/pmon_u /metrics_pnom_v/pnom_v
     &                              /metrics_grdscl/grdscl
 
#ifdef MASKING
      real rmask(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE rmask(BLOCK_PATTERN) BLOCK_CLAUSE
      real pmask(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE pmask(BLOCK_PATTERN) BLOCK_CLAUSE
      real umask(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE umask(BLOCK_PATTERN) BLOCK_CLAUSE
      real vmask(GLOBAL_2D_ARRAY)
CSDISTRIBUTE_RESHAPE vmask(BLOCK_PATTERN) BLOCK_CLAUSE
      common /mask_r/rmask /mask_p/pmask
     &       /mask_u/umask /mask_v/vmask
#endif
