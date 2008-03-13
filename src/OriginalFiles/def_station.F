#include "cppdefs.h"
#ifdef STATIONS
                                 ! Create station data NetCDF file:
      subroutine def_station     ! Define its variables, dimensions,
                                 ! and attributes
      implicit none
# include "param.h"
# include "grid.h"
# include "mixing.h"
# include "ncvars.h"
# include "netcdf.inc"
# include "scalars.h"
# include "strings.h"
!
      logical gotd, gotub, gotvb, gotz
      integer i, k, lstr, lsvar, lvar, stadim, status, timedim, varid
      integer pgrd(2)
      integer lenstr
# ifdef SOLVE3D
      logical gotaks, gotakt, gotakv, gotr, gotu, gotv, gotw
#  ifdef LMD_KPP
      logical gothbl
#  endif /* LMD_KPP */
      logical gott(NT)
      integer itrc, srdim, swdim, trcdim
      integer rgrd(3), wgrd(3)
# endif /* SOLVE3D */
      real wrk(NS)
      character*1 char1
!-
      if (may_day_flag.ne.0) return      !-->  EXIT
!
!  Create a new station data file.
!---------------------------------
!
      if (ldefhis) then
        lstr=lenstr(staname)
        status=nf_create(staname(1:lstr),NF_CLOBBER,ncstaid)
        if (status.ne.NF_NOERR) then
          write(stdout,10) staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
!
!  Define the dimensions of staggered fields.
!--------------------------------------------
!
        status=nf_def_dim(ncstaid,'station',nstation,stadim)
# ifdef SOLVE3D
        status=nf_def_dim(ncstaid,'s_rho',N,srdim)
        status=nf_def_dim(ncstaid,'s_w',N,swdim)
        status=nf_def_dim(ncstaid,'tracer',NT,trcdim)
# endif /* SOLVE3D */
        status=nf_def_dim(ncstaid,'time',NF_UNLIMITED,timedim)
!
!  Define dimension vectors for point variables.
!
        pgrd(1)=stadim
        pgrd(2)=timedim
#  ifdef SOLVE3D
!
!  Define dimension vectors for cast variables at vertical RHO-points.
!
        rgrd(1)=srdim
        rgrd(2)=stadim
        rgrd(3)=timedim
!
!  Define dimension vectors for cast variables at vertical W-points.
!
        wgrd(1)=swdim
        wgrd(2)=stadim
        wgrd(3)=timedim
#  endif /* SOLVE3D */
!
!  Define global attributes.
!------------------------------
!
        call get_date (date_str)
        lvar=lenstr(date_str)
        if (lvar.gt.0) then
          history='SCRUM, Version '/ /version/ /', '/ /date_str(1:lvar)
        else
          history='SCRUM, Version '/ /version
        endif
        status=nf_put_att_text(ncstaid,NF_GLOBAL,'type',18,
     &                         'SCRUM station file')
!
!  Define global attributes to NetCDF file.
!
        lvar=lenstr(title)
        status=nf_put_att_text(ncstaid,NF_GLOBAL,'title',lvar,
     &                         title(1:lvar))
        lvar=lenstr(rstname)
        status=nf_put_att_text(ncstaid,NF_GLOBAL,'rst_file',lvar,
     &                         rstname(1:lvar))
        if (lwrthis) then
          lvar=lenstr(hisname)
          status=nf_put_att_text(ncstaid,NF_GLOBAL,'his_file',lvar,
     &                           hisname(1:lvar))
        endif
# ifdef AVERAGES
        lvar=lenstr(avgname)
        status=nf_put_att_text(ncstaid,NF_GLOBAL,'avg_file',lvar,
     &                         avgname(1:lvar))
# endif /* AVERAGES */
        lvar=lenstr(staname)
        status=nf_put_att_text(ncstaid,NF_GLOBAL,'sta_file',lvar,
     &                         staname(1:lvar))
        lvar=lenstr(sposnam)
        status=nf_put_att_text(ncstaid,NF_GLOBAL,'spos_file',lvar,
     &                         sposnam(1:lvar))
# ifdef FLOATS
        lvar=lenstr(fltname)
        status=nf_put_att_text(ncstaid,NF_GLOBAL,'flt_file',lvar,
     &                         fltname(1:lvar))
# endif /* FLOATS */
# ifndef ANA_GRID
        lvar=lenstr(grdname)
        status=nf_put_att_text(ncstaid,NF_GLOBAL,'grd_file',lvar,
     &                         grdname(1:lvar))
# endif /* !ANA_GRID */
# ifndef ANA_INITIAL
        lvar=lenstr(ininame)
        status=nf_put_att_text(ncstaid,NF_GLOBAL,'ini_file',lvar,
     &                         ininame(1:lvar))
# endif /* !ANA_INITIAL */
# if !defined ANA_SMFLUX || !defined ANA_STFLUX || \
    (defined SG_BBL96 && !defined ANA_BSEDIM) || \
    (defined SG_BBL96 && !defined ANA_WWAVE) || \
    (defined SALINITY && !defined ANA_SSFLUX) || \
    (defined LMD_KPP && !defined ANA_SRFLUX)
        lvar=lenstr(frcname)
        status=nf_put_att_text(ncstaid,NF_GLOBAL,'frc_file',lvar,
     &                         frcname(1:lvar))
# endif /* !ANA_XXFLUX */
# ifdef ASSIMILATION
        lvar=lenstr(assname)
        status=nf_put_att_text(ncstaid,NF_GLOBAL, 'ass_file', lvar,
     &                         assname(1:lvar))
        lvar=lenstr(aparnam)
        status=nf_put_att_text(ncstaid,NF_GLOBAL, 'apar_file', lvar,
     &                                              aparnam(1:lvar))
# endif
# ifdef FLOATS
        lvar=lenstr(fparnam)
        status=nf_put_att_text(ncstaid,NF_GLOBAL,'fpar_file', lvar,
     &                                              fparnam(1:lvar))
# endif
        lvar=lenstr(history)
        status=nf_put_att_text(ncstaid,NF_GLOBAL, 'history', lvar,
     &                                             history(1:lvar))
        lvar=lenstr(cpps)
        status=nf_put_att_text(ncstaid,NF_GLOBAL, 'CPP-options',
     &                                        lvar, cpps(1:lvar))
!
!  Define running parameters.
!----------------------------
!
!  Time stepping parameters.
!
        status=nf_def_var(ncstaid,'ntimes',NF_INT,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',25,
     &                               'number of long time-steps')
        status=nf_def_var(ncstaid,'ndtfast',NF_INT,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',26,
     &                                 'number of short time-steps')
        status=nf_def_var(ncstaid,'dt',NF_FTYPE,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',23,
     &                                    'size of long time-steps')
        status=nf_put_att_text(ncstaid,varid,'units',6,'second')
        status=nf_def_var(ncstaid,'dtfast',NF_FTYPE,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',24,
     &                                   'size of short time-steps')
        status=nf_put_att_text(ncstaid,varid,'units',6,'second')
 
        status=nf_def_var(ncstaid,'nwrt',NF_INT,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',44,
     &               'number of time-steps between history records')
        status=nf_def_var(ncstaid,'nrst',NF_INT,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',44,
     &               'number of time-steps between restart records')
        if (ncycle.gt.0) then
          status=nf_put_att_text(ncstaid,varid,'cycle',38,
     &                     'only latest two records are maintained')
        endif
# ifdef AVERAGES
        status=nf_def_var(ncstaid,'ntsavg',NF_INT,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',59,
     & 'starting time-step for accumulation of time-averaged fields')
        status=nf_def_var(ncstaid,'navg',NF_INT,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',50,
     &          'number of time-steps between time-averaged records')
# endif
        status=nf_def_var(ncstaid,'nsta',NF_INT,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',45,
     &               'number of time-steps between stations records')
!
!  Horizontal mixing coefficients.
!
# if defined SOLVE3D && defined TS_DIF2
        status=nf_def_var(ncstaid,'tnu2',NF_FTYPE,1,trcdim,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',40,
     &                    'Laplacian mixing coefficient for tracers')
        status=nf_put_att_text(ncstaid,varid,'units',15,
     &                         'meter2 second-1')
# endif
# ifdef UV_VIS2
        status=nf_def_var(ncstaid,'visc2',NF_FTYPE,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',41,
     &                   'Laplacian mixing coefficient for momentum')
        status=nf_put_att_text(ncstaid,varid,'units',15,
     &                         'meter2 second-1')
# endif
# if defined SOLVE3D && defined MY25_MIXING
#  ifdef Q2_DIF2
        status=nf_def_var(ncstaid,'q2nu2',NF_FTYPE,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',49,
     &           'Laplacian mixing coefficient for turbulent energy')
        status=nf_put_att_text(ncstaid,varid,'units',15,
     &                                             'meter2 second-1')
#  endif
#  ifdef Q2_DIF4
        status=nf_def_var(ncstaid,'q2nu4',NF_FTYPE,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',50,
     &          'biharmonic mixing coefficient for turbulent energy')
        status=nf_put_att_text(ncstaid,varid,'units',15,
     &                                             'meter4 second-1')
#  endif
# endif /* SOLVE3D && MY25_MIXING */
# ifdef SOLVE3D
!
!  Background vertical mixing coefficients.
!
        status=nf_def_var(ncstaid,'Akt_bak',NF_FTYPE,1,trcdim,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',50,
     &          'background vertical mixing coefficient for tracers')
        status=nf_put_att_text(ncstaid,varid,'units',15,
     &                         'meter2 second-1')
        status=nf_def_var(ncstaid,'Akv_bak',NF_FTYPE,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',51,
     &         'background vertical mixing coefficient for momentum')
        status=nf_put_att_text(ncstaid,varid,'units',15,
     &                         'meter2 second-1')
#  ifdef MY25_MIXING
        status=nf_def_var(ncstaid,'Akq_bak',NF_FTYPE,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',59,
     & 'background vertical mixing coefficient for turbulent energy')
        status=nf_put_att_text(ncstaid,varid,'units',15,
     &                                             'meter2 second-1')
#  endif
# endif /* SOLVE3D */
!
!  Bottom drag coefficients.
!
        status=nf_def_var(ncstaid,'rdrg',NF_FTYPE,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',23,
     &                         'linear drag coefficient')
        status=nf_put_att_text(ncstaid,varid,'units',14,
     &                         'meter second-1')
        status=nf_def_var(ncstaid,'rdrg2',NF_FTYPE,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',26,
     &                         'quadratic drag coefficient')
        status=nf_put_att_text(ncstaid,varid,'units',14,
     &                         'nondimensional')
!
!  Equation of State parameters.
!
        status=nf_def_var(ncstaid,'rho0',NF_FTYPE,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',45,
     &             'mean density used in Boussinesq approximation')
        status=nf_put_att_text(ncstaid,varid,'units',16,
     &                         'kilogram meter-3')
# if defined SOLVE3D && !defined NONLIN_EOS
        status=nf_def_var(ncstaid,'R0',NF_FTYPE,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',51,
     &       'background density used in linear equation of state')
        status=nf_put_att_text(ncstaid,varid,'units',16,
     &                         'kilogram meter-3')
        status=nf_def_var(ncstaid,'Tcoef',NF_FTYPE,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',29,
     &                             'thermal expansion coefficient')
        status=nf_put_att_text(ncstaid,varid,'units',9,'Celsius-1')
        status=nf_def_var(ncstaid,'Scoef',NF_FTYPE,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',30,
     &                            'Saline contraction coefficient')
        status=nf_put_att_text(ncstaid,varid,'units',9,'PSU-1')
# endif /* SOLVE3D && !NONLIN_EOS */
!
!  Slipperiness parameters.
!
        status=nf_def_var(ncstaid,'gamma2',NF_FTYPE,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',22,
     &                               'Slipperiness parameter')
        status=nf_put_att_text(ncstaid,varid,'units',14,
     &                                       'nondimensional')
!
!  Define grid variables.
!------------------------------------------------
!
!  Grid type switch: Spherical or Cartesian.
!
        status=nf_def_var(ncstaid,'spherical',NF_CHAR,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',24,
     &                               'grid type logical switch')
        status=nf_put_att_text(ncstaid,varid,'option_T',9,'spherical')
        status=nf_put_att_text(ncstaid,varid,'option_F',9,'Cartesian')
!
!  Domain Length.
!
        status=nf_def_var(ncstaid,'xl',NF_FTYPE,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',33,
     &                         'domain length in the XI-direction')
        status=nf_put_att_text(ncstaid,varid,'units',5,'meter')
        status=nf_def_var(ncstaid,'el',NF_FTYPE,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',34,
     &                         'domain length in the ETA-direction')
        status=nf_put_att_text(ncstaid,varid,'units',5,'meter')
# ifdef SOLVE3D
!
!  S-coordinate parameters.
!
        status=nf_def_var(ncstaid,'theta_s',NF_FTYPE,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',38,
     &                     'S-coordinate surface control parameter')
        status=nf_put_att_text(ncstaid,varid,'units',14,
     &                         'nondimensional')
        status=nf_def_var(ncstaid,'theta_b',NF_FTYPE,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',37,
     &                     'S-coordinate bottom control parameter')
        status=nf_put_att_text(ncstaid,varid,'units',14,
     &                         'nondimensional')
        status=nf_def_var(ncstaid,'hc',NF_FTYPE,0,0,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',38,
     &                     'S-coordinate parameter, critical depth')
        status=nf_put_att_text(ncstaid,varid,'units',5,'meter')
!
!  S-coordinate non-dimensional independent variable at RHO-points.
!
        status=nf_def_var(ncstaid,'sc_r',NF_FTYPE,1,srdim,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',26,
     &                              'S-coordinate at RHO-points')
        status=nf_put_att_text(ncstaid,varid,'units',14,
     &                                           'nondimensional')
        status=nf_put_att_FTYPE(ncstaid,varid,'valid_min',NF_FTYPE,
     &                                                       1,-1.)
        status=nf_put_att_FTYPE(ncstaid,varid,'valid_max',NF_FTYPE,
     &                                                        1,0.)
        status=nf_put_att_text(ncstaid,varid,'field',12,
     &                                              'sc_r, scalar')
!
!  S-coordinate non-dimensional independent variable at W-points.
!
        status=nf_def_var(ncstaid,'sc_w',NF_FTYPE,1,swdim,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',24,
     &                                   'S-coordinate at W-points')
        status=nf_put_att_text(ncstaid,varid,'units',14,
     &                                             'nondimensional')
        status=nf_put_att_FTYPE(ncstaid,varid,'valid_min',NF_FTYPE,
     &                                                        1,-1.)
        status=nf_put_att_FTYPE(ncstaid,varid,'valid_max',NF_FTYPE,
     &                                                         1,0.)
        status=nf_put_att_text(ncstaid,varid,'field',12,
     &                                         'sc_w, scalar')
!
!  S-coordinate non-dimensional stretching curves at RHO-points.
!
        status=nf_def_var(ncstaid,'Cs_r',NF_FTYPE,1,srdim,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',44,
     &                'S-coordinate stretching curves at RHO-points')
        status=nf_put_att_text(ncstaid,varid,'units',14,
     &                                              'nondimensional')
        status=nf_put_att_FTYPE(ncstaid,varid,'valid_min',NF_FTYPE,
     &                                                         1,-1.)
        status=nf_put_att_FTYPE(ncstaid,varid,'valid_max',NF_FTYPE,
     &                                                          1,0.)
        status=nf_put_att_text(ncstaid,varid,'field',12,
     &                                                'Cs_r, scalar')
!
!  S-coordinate non-dimensional stretching curves at W-points.
!
        status=nf_def_var(ncstaid,'Cs_w',NF_FTYPE,1,swdim,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',42,
     &                  'S-coordinate stretching curves at W-points')
        status=nf_put_att_text(ncstaid,varid,'units',14,
     &                                              'nondimensional')
        status=nf_put_att_FTYPE(ncstaid,varid,'valid_min',NF_FTYPE,
     &                                                         1,-1.)
        status=nf_put_att_FTYPE(ncstaid,varid,'valid_max',NF_FTYPE,
     &                                                          1,0.)
        status=nf_put_att_text(ncstaid,varid,'field',12,
     &                                                'Cs_w, scalar')
# endif /* SOLVE3D */
!
!  IJ-index station positions
!
        status=nf_def_var(ncstaid,'Ipos',NCLONG,1,stadim,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',25,
     &                         'I-index station positions')
        status=nf_put_att_text(ncstaid,varid,'units',14,
     &                         'nondimensional')
        status=nf_put_att_int(ncstaid,varid,'valid_min',NF_INT,1,2)
        status=nf_put_att_int(ncstaid,varid,'valid_max',NF_INT,1,LLm)
        status=nf_put_att_text(ncstaid,varid,'field',12,
     &                                               'Ipos, scalar')
        status=nf_def_var(ncstaid,'Jpos',NCLONG,1,stadim,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',25,
     &                                  'J-index station positions')
        status=nf_put_att_text(ncstaid,varid,'units',14,
     &                                             'nondimensional')
        status=nf_put_att_int(ncstaid,varid,'valid_min',NF_INT,1,2)
        status=nf_put_att_int(ncstaid,varid,'valid_max',NF_INT,1,MMm)
        status=nf_put_att_text(ncstaid,varid,'field',12,
     &                                               'Jpos, scalar')
!
!  Bathymetry.
!
        status=nf_def_var(ncstaid,'h',NF_FTYPE,1,stadim,varid)
        status=nf_put_att_text(ncstaid,varid,'long_name',24,
     &                                   'bathymetry at RHO-points')
        status=nf_put_att_text(ncstaid,varid,'units',5,'meter')
        status=nf_put_att_text(ncstaid,varid,'field',12,
     &                                               'bath, scalar')
!
!  Longitude and latitude coordinates of RHO-points, if appropriate.
!
        if (spherical) then
          status=nf_def_var(ncstaid,'lon_rho',NF_FTYPE,1,stadim,varid)
          status=nf_put_att_text(ncstaid,varid,'long_name',23,
     &                                     'longitude of RHO-points')
          status=nf_put_att_text(ncstaid,varid,'units',11,
     &                                                 'degree_east')
          status=nf_put_att_text(ncstaid,varid,'field',15,
     &                                             'lon_rho, scalar')
          status=nf_def_var(ncstaid,'lat_rho',NF_FTYPE,1,stadim,varid)
          status=nf_put_att_text(ncstaid,varid,'long_name',22,
     &                                      'latitude of RHO-points')
          status=nf_put_att_text(ncstaid,varid,'units',12,
     &                                                'degree_north')
          status=nf_put_att_text(ncstaid,varid,'field',15,
     &                                             'lat_rho, scalar')
        else
          status=nf_def_var(ncstaid,'x_rho',NF_FTYPE,1,stadim,varid)
          status=nf_put_att_text(ncstaid,varid,'long_name',25,
     &                           'x-locations of RHO-points')
          status=nf_put_att_text(ncstaid,varid,'units',5,'meter')
          status=nf_put_att_text(ncstaid,varid,'field',13,
     &                           'x_rho, scalar')
          status=nf_def_var(ncstaid,'y_rho',NF_FTYPE,1,stadim,varid)
          status=nf_put_att_text(ncstaid,varid,'long_name',25,
     &                           'y-locations of RHO-points')
          status=nf_put_att_text(ncstaid,varid,'units',5,'meter')
          status=nf_put_att_text(ncstaid,varid,'field',13,
     &                           'x_rho, scalar')
        endif
# ifdef CURVGRID
!
!  Angle between XI-axis and EAST at RHO-points, if appropriate.
!
        if (spherical) then
          status=nf_def_var(ncstaid,'angle',NF_FTYPE,1,stadim,varid)
          status=nf_put_att_text(ncstaid,varid,'long_name',30,
     &                           'angle between XI-axis and EAST')
          status=nf_put_att_text(ncstaid,varid,'units',7,'radians')
          status=nf_put_att_text(ncstaid,varid,'field',13,
     &                           'angle, scalar')
        endif
# endif /* CURVGRID */
!
!  Define variables and their attributes.
!-----------------------------------------------------------
!
!  Define model time.
!
        lvar=lenstr(vname(1,indexd))
        status=nf_def_var(ncstaid,vname(1,indexd)(1:lvar),NF_FTYPE,
     &                    1,timedim,stadid)
        lvar=lenstr(vname(2,indexd))
        status=nf_put_att_text(ncstaid,stadid,'long_name',lvar,
     &                         vname(2,indexd)(1:lvar))
        lvar=lenstr(vname(3,indexd))
        status=nf_put_att_text(ncstaid,stadid,'units',lvar,
     &                         vname(3,indexd)(1:lvar))
# ifdef SOLVE3D
!
!  Define 3D momentum component in the XI-direction.
!
        if (wrtU) then
          lvar=lenstr(vname(1,indexu))
          status=nf_def_var(ncstaid,vname(1,indexu)(1:lvar),NF_FOUT,
     &                      3,rgrd,stauid)
          lvar=lenstr(vname(2,indexu))
          status=nf_put_att_text(ncstaid,stauid,'long_name',lvar,
     &                           vname(2,indexu)(1:lvar))
          lvar=lenstr(vname(3,indexu))
          status=nf_put_att_text(ncstaid,stauid,'units',lvar,
     &                           vname(3,indexu)(1:lvar))
        endif
!
!  Define 3D momentum component in the ETA-direction.
!
        if (wrtV) then
          lvar=lenstr(vname(1,indexv))
          status=nf_def_var(ncstaid,vname(1,indexv)(1:lvar),NF_FOUT,
     &                      3,rgrd,stavid)
          lvar=lenstr(vname(2,indexv))
          status=nf_put_att_text(ncstaid,stavid,'long_name',lvar,
     &                           vname(2,indexv)(1:lvar))
          lvar=lenstr(vname(3,indexv))
          status=nf_put_att_text(ncstaid,stavid,'units',lvar,
     &                           vname(3,indexv)(1:lvar))
        endif
!
!  Define 3D momentum component in the S-direction.
!
        if (wrtW) then
          lvar=lenstr(vname(1,indexw))
          status=nf_def_var(ncstaid,vname(1,indexw)(1:lvar),NF_FOUT,
     &                      3,rgrd,stawid)
          lvar=lenstr(vname(2,indexw))
          status=nf_put_att_text(ncstaid,stawid,'long_name',lvar,
     &                           vname(2,indexw)(1:lvar))
          lvar=lenstr(vname(3,indexw))
          status=nf_put_att_text(ncstaid,stawid,'units',lvar,
     &                           vname(3,indexw)(1:lvar))
        endif
# endif /* SOLVE3D */
!
!  Define 2D momentum in the XI-direction.
!
        if (wrtUBAR) then
          lvar=lenstr(vname(1,indexub))
          status=nf_def_var(ncstaid,vname(1,indexub)(1:lvar),
     &                      NF_FOUT,2,pgrd,staubid)
          lvar=lenstr(vname(2,indexub))
          status=nf_put_att_text(ncstaid,staubid,'long_name',lvar,
     &                           vname(2,indexub)(1:lvar))
          lvar=lenstr(vname(3,indexub))
          status=nf_put_att_text(ncstaid,staubid,'units',lvar,
     &                           vname(3,indexub)(1:lvar))
        endif
!
!  Define 2D momentum in the ETA-direction.
!
        if (wrtVBAR) then
          lvar=lenstr(vname(1,indexvb))
          status=nf_def_var(ncstaid,vname(1,indexvb)(1:lvar),
     &                      NF_FOUT,2,pgrd,stavbid)
          lvar=lenstr(vname(2,indexvb))
          status=nf_put_att_text(ncstaid,stavbid,'long_name',lvar,
     &                           vname(2,indexvb)(1:lvar))
          lvar=lenstr(vname(3,indexvb))
          status=nf_put_att_text(ncstaid,stavbid,'units',lvar,
     &                           vname(3,indexvb)(1:lvar))
        endif
!
!  Define free-surface.
!
        if (wrtZ) then
          lvar=lenstr(vname(1,indexz))
          status=nf_def_var(ncstaid,vname(1,indexz)(1:lvar),NF_FOUT,
     &                      2,pgrd,stazid)
          lvar=lenstr(vname(2,indexz))
          status=nf_put_att_text(ncstaid,stazid,'long_name',lvar,
     &                           vname(2,indexz)(1:lvar))
          lvar=lenstr(vname(3,indexz))
          status=nf_put_att_text(ncstaid,stazid,'units',lvar,
     &                           vname(3,indexz)(1:lvar))
        endif
# ifdef SOLVE3D
!
!  Define tracer type variables.
!
        do itrc=1,NT
          if (wrtT(itrc)) then
            lvar=lenstr(vname(1,indext(itrc)))
            status=nf_def_var(ncstaid,vname(1,indext(itrc))(1:lvar),
     &                        NF_FOUT,3,rgrd,statid(itrc))
            lvar=lenstr(vname(2,indext(itrc)))
            status=nf_put_att_text(ncstaid,statid(itrc),'long_name',
     &                            lvar,vname(2,indext(itrc))(1:lvar))
            lvar=lenstr(vname(3,indext(itrc)))
            status=nf_put_att_text(ncstaid,statid(itrc),'units',lvar,
     &                             vname(3,indext(itrc))(1:lvar))
          endif
        enddo
!
!  Define density anomaly.
!
        if (wrtRHO) then
          lvar=lenstr(vname(1,indexr))
          status=nf_def_var(ncstaid,vname(1,indexr)(1:lvar),NF_FOUT,
     &                      3,rgrd,starid)
          lvar=lenstr(vname(2,indexr))
          status=nf_put_att_text(ncstaid,starid,'long_name',lvar,
     &                           vname(2,indexr)(1:lvar))
          lvar=lenstr(vname(3,indexr))
          status=nf_put_att_text(ncstaid,starid,'units',lvar,
     &                           vname(3,indexr)(1:lvar))
        endif
#  ifdef LMD_KPP
!
!  Define depth of planetary boundary layer.
!
        if (wrtHBL) then
          lvar=lenstr(vname(1,indexhbl))
          status=nf_def_var(ncstaid,vname(1,indexhbl)(1:lvar),
     &                      NF_FOUT,2,pgrd,stahblid)
          lvar=lenstr(vname(2,indexhbl))
          status=nf_put_att_text(ncstaid,stahblid,'long_name',lvar,
     &                           vname(2,indexhbl)(1:lvar))
          lvar=lenstr(vname(3,indexhbl))
          status=nf_put_att_text(ncstaid,stahblid,'units',lvar,
     &                           vname(3,indexhbl)(1:lvar))
        endif
#  endif /* LMD_KPP */
!
!  Define vertical viscosity coefficient.
!
        if (wrtAKV) then
          lvar=lenstr(vname(1,indexakv))
          status=nf_def_var(ncstaid,vname(1,indexakv)(1:lvar),
     &                      NF_FOUT,3,wgrd,staakvid)
          lvar=lenstr(vname(2,indexakv))
          status=nf_put_att_text(ncstaid,staakvid,'long_name',lvar,
     &                           vname(2,indexakv)(1:lvar))
          lvar=lenstr(vname(3,indexakv))
          status=nf_put_att_text(ncstaid,staakvid,'units',lvar,
     &                           vname(3,indexakv)(1:lvar))
        endif
!
!  Define vertical diffusion coefficient for potential temperature.
!
        if (wrtAKT) then
          lvar=lenstr(vname(1,indexakt))
          status=nf_def_var(ncstaid,vname(1,indexakt)(1:lvar),
     &                      NF_FOUT,3,wgrd,staaktid)
          lvar=lenstr(vname(2,indexakt))
          status=nf_put_att_text(ncstaid,staaktid,'long_name',lvar,
     &                           vname(2,indexakt)(1:lvar))
          lvar=lenstr(vname(3,indexakt))
          status=nf_put_att_text(ncstaid,staaktid,'units',lvar,
     &                           vname(3,indexakt)(1:lvar))
        endif
#  ifdef SALINITY
!
!  Define vertical diffusion coefficient for salinity.
!
        if (wrtAKS) then
          lvar=lenstr(vname(1,indexaks))
          status=nf_def_var(ncstaid,vname(1,indexaks)(1:lvar),
     &                      NF_FOUT,3,wgrd,staaksid)
          lvar=lenstr(vname(2,indexaks))
          status=nf_put_att_text(ncstaid,staaksid,'long_name',lvar,
     &                           vname(2,indexaks)(1:lvar))
          lvar=lenstr(vname(3,indexaks))
          status=nf_put_att_text(ncstaid,staaksid,'units',lvar,
     &                           vname(3,indexaks)(1:lvar))
        endif
#  endif
# endif /* SOLVE3D */
!
!  Initialize unlimited time record dimension.
!
        tstaindx=0
!
!  Leave definition mode.
!---------------------------
!
        status=nf_enddef(ncstaid)
!
!  Write out running parameters.
!-------------------------------
!
!  Time stepping parameters.
!
        status=nf_inq_varid(ncstaid,'ntimes',varid)
        status=nf_put_var1_int(ncstaid,varid,1,ntimes)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'ntimes', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
        status=nf_inq_varid(ncstaid,'ndtfast',varid)
        status=nf_put_var1_int(ncstaid,varid,1,ndtfast)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'ndtfast', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
        status=nf_inq_varid(ncstaid,'dt',varid)
        status=nf_put_var1_FTYPE(ncstaid,varid,1,dt)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'dt', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
        status=nf_inq_varid(ncstaid,'dtfast',varid)
        status=nf_put_var1_FTYPE(ncstaid,varid,1,dtfast)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'dtfast', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
        status=nf_inq_varid(ncstaid,'nwrt',varid)
        status=nf_put_var1_int(ncstaid,varid,1,nwrt)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'nwrt', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
        status=nf_inq_varid(ncstaid,'nrst',varid)
        status=nf_put_var1_int(ncstaid,varid,1,nrst)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'nrst', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
# ifdef AVERAGES
        status=nf_inq_varid(ncstaid,'ntsavg',varid)
        status=nf_put_var1_int(ncstaid,varid,1,ntsavg)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'ntsavg', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
        status=nf_inq_varid(ncstaid,'navg',varid)
        status=nf_put_var1_int(ncstaid,varid,1,navg)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'navg', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
# endif /* AVERAGES */
        status=nf_inq_varid(ncstaid,'nsta',varid)
        status=nf_put_var1_int(ncstaid,varid,1,nsta)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'nsta', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
!
!  Horizontal mixing coefficients.
!
# if defined SOLVE3D && defined TS_DIF2
        status=nf_inq_varid(ncstaid,'tnu2',varid)
        status=nf_put_vara_FTYPE(ncstaid,varid,1,NT,tnu2)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'tnu2', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
# endif /* SOLVE3D && TS_DIF2 */
# ifdef UV_VIS2
        status=nf_inq_varid(ncstaid,'visc2',varid)
        status=nf_put_var1_FTYPE(ncstaid,varid,1,visc2)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'visc2', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
# endif
# if defined SOLVE3D && defined MY25_MIXING
#  ifdef Q2_DIF2
        status=nf_inq_varid(ncstaid,'q2nu2',varid)
        status=nf_put_var1_FTYPE(ncstaid,varid,1,q2nu2)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'q2nu2', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
#  endif
#  ifdef Q2_DIF4
        status=nf_inq_varid(ncstaid,'q2nu4',varid)
        status=nf_put_var1_FTYPE(ncstaid,varid,1,q2nu4)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'q2nu4', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
#  endif
# endif /* SOLVE3D && MY25_MIXING */
# ifdef SOLVE3D
!
!  Background vertical mixing coefficients.
!
        status=nf_inq_varid(ncstaid,'Akt_bak',varid)
        status=nf_put_vara_FTYPE(ncstaid,varid,1,NT,Akt_bak)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'Akt_bak', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
        status=nf_inq_varid(ncstaid,'Akv_bak',varid)
        status=nf_put_var1_FTYPE(ncstaid,varid,1,Akv_bak)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'Akv_bak', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
#  ifdef MY25_MIXING
        status=nf_inq_varid(ncstaid,'Akq_bak',varid)
        status=nf_put_var1_FTYPE(ncstaid,varid,1,Akq_bak)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'Akq_bak', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
#  endif /* MY25_MIXING */
# endif /* SOLVE3D */
!
!  Bottom drag coefficients.
!
        status=nf_inq_varid(ncstaid,'rdrg',varid)
        status=nf_put_var1_FTYPE(ncstaid,varid,1,rdrg)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'rdrg', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
        status=nf_inq_varid(ncstaid,'rdrg2',varid)
        status=nf_put_var1_FTYPE(ncstaid,varid,1,rdrg2)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'rdgr2', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
!
!  Equation of State parameters.
!
        status=nf_inq_varid(ncstaid,'rho0',varid)
        status=nf_put_var1_FTYPE(ncstaid,varid,1,rho0)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'rho0', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
# if defined SOLVE3D && !defined NONLIN_EOS
        status=nf_inq_varid(ncstaid,'R0',varid)
        status=nf_put_var1_FTYPE(ncstaid,varid,1,R0)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'R0', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
        status=nf_inq_varid(ncstaid,'Tcoef',varid)
        status=nf_put_var1_FTYPE(ncstaid,varid,1,Tcoef)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'Tcoef', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
        status=nf_inq_varid(ncstaid,'Scoef',varid)
        status=nf_put_var1_FTYPE(ncstaid,varid,1,Scoef)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'Scoef', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
# endif /* SOLVE3D && !NONLIN_EOS */
!
!  Slipperiness parameters.
!
        status=nf_inq_varid(ncstaid,'gamma2',varid)
        status=nf_put_var1_FTYPE(ncstaid,varid,1,gamma2)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'gamma2', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
!
!  Write out grid variables.
!---------------------------------------------------
!
!  Grid type switch.
!
        status=nf_inq_varid(ncstaid,'spherical',varid)
        write(char1,'(l1)') spherical
        status=nf_put_var1_text(ncstaid,varid,1,char1)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'spherical', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
!
!  Domain Length.
!
        status=nf_inq_varid(ncstaid,'xl',varid)
        status=nf_put_var1_FTYPE(ncstaid,varid,1,xl)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'xl', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
        status=nf_inq_varid(ncstaid,'el',varid)
        status=nf_put_var1_FTYPE(ncstaid,varid,1,el)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'el', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
# ifdef SOLVE3D
!
!  S-coordinate parameters.
!
        status=nf_inq_varid(ncstaid,'theta_s',varid)
        status=nf_put_var1_FTYPE(ncstaid,varid,1,theta_s)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'theta_s', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
        status=nf_inq_varid(ncstaid,'theta_b',varid)
        status=nf_put_var1_FTYPE(ncstaid,varid,1,theta_b)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'theta_b', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
        status=nf_inq_varid(ncstaid,'hc',varid)
        status=nf_put_var1_FTYPE(ncstaid,varid,1,hc)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'hc', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
!
!  S-coordinate non-dimensional independent variables.
!
        status=nf_inq_varid(ncstaid,'sc_r',varid)
        status=nf_put_vara_FTYPE(ncstaid,varid,1,N,sc_r)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'sc_r', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
        status=nf_inq_varid(ncstaid,'sc_w',varid)
        status=nf_put_vara_FTYPE(ncstaid,varid,1,N,sc_w(1))
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'sc_w', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
!
!  S-coordinate non-dimensional stretching curves.
!
        status=nf_inq_varid(ncstaid,'Cs_r',varid)
        status=nf_put_vara_FTYPE(ncstaid,varid,1,N,Cs_r)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'Cs_r', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
        status=nf_inq_varid(ncstaid,'Cs_w',varid)
        status=nf_put_vara_FTYPE(ncstaid,varid,1,N,Cs_w(1))
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'Cs_w', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
# endif /* SOLVE3D */
!
!  IJ-index station positions.
!
        status=nf_inq_varid(ncstaid,'Ipos',varid)
        status=nf_put_vara_int(ncstaid,varid,1,nstation,ispos)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'Ipos', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
        status=nf_inq_varid(ncstaid,'Jpos',varid)
        status=nf_put_vara_int(ncstaid,varid,1,nstation,jspos)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'Jpos', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
!
!  Bathymetry.
!
        do k=1,nstation
          wrk(k)=h(ispos(k),jspos(k))
        enddo
        status=nf_inq_varid(ncstaid,'h',varid)
        status=nf_put_vara_FTYPE(ncstaid,varid,1,nstation,wrk)
        if (status.ne.NF_NOERR) then
          write(stdout,20) 'h', staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
!
!  Longitude and latitude of RHO-points, if appropriate.
!
        if (spherical) then
          do k=1,nstation
            wrk(k)=lonr(ispos(k),jspos(k))
          enddo
          status=nf_inq_varid(ncstaid,'lon_rho',varid)
          status=nf_put_vara_FTYPE(ncstaid,varid,1,nstation,wrk)
          if (status.ne.NF_NOERR) then
            write(stdout,20) 'lon_rho', staname(1:lstr)
            may_day_flag=3
            return                       !-->  EXIT
          endif
          do k=1,nstation
            wrk(k)=latr(ispos(k),jspos(k))
          enddo
          status=nf_inq_varid(ncstaid,'lat_rho',varid)
          status=nf_put_vara_FTYPE(ncstaid,varid,1,nstation,wrk)
          if (status.ne.NF_NOERR) then
            write(stdout,20) 'lat_rho', staname(1:lstr)
            may_day_flag=3
            return                       !-->  EXIT
          endif
        else
          do k=1,nstation
            wrk(k)=xr(ispos(k),jspos(k))
          enddo
          status=nf_inq_varid(ncstaid,'x_rho',varid)
          status=nf_put_vara_FTYPE(ncstaid,varid,1,nstation,wrk)
          if (status.ne.NF_NOERR) then
            write(stdout,20) 'x_rho', staname(1:lstr)
            may_day_flag=3
            return                       !-->  EXIT
          endif
          do k=1,nstation
            wrk(k)=yr(ispos(k),jspos(k))
          enddo
          status=nf_inq_varid(ncstaid,'y_rho',varid)
          status=nf_put_vara_FTYPE(ncstaid,varid,1,nstation,wrk)
          if (status.ne.NF_NOERR) then
            write(stdout,20) 'y_rho', staname(1:lstr)
            may_day_flag=3
            return                       !-->  EXIT
          endif
        endif
# ifdef CURVGRID
!
!  Angle between XI-axis and EAST at RHO-points, if appropriate.
!
        if (spherical) then
          do k=1,nstation
            wrk(k)=angler(ispos(k),jspos(k))
          enddo
          status=nf_inq_varid(ncstaid,'angle',varid)
          status=nf_put_vara_FTYPE(ncstaid,varid,1,nstation,wrk)
          if (status.ne.NF_NOERR) then
            write(stdout,20) 'angle', staname(1:lstr)
            may_day_flag=3
            return                       !-->  EXIT
          endif
        endif
# endif /* CURVGRID */
!
!  Open an existing stations file, check its contents,
!  and prepare for appending data.
!===================================================
!
      else
!
!  Inquire about the contents of stations NetCDF file:
!  Inquire about the dimensions and variables. Check for consistency.
!--------------------------------------------------------------------
!
        call opencdf (staname,N)
        if (may_day_flag.ne.0) return    !-->  EXIT
!
!  Open stations file for read/write.
!
        lstr=lenstr(staname)
        status=nf_open(staname(1:lstr),NF_WRITE,ncstaid)
        if (status.ne.NF_NOERR) then
          write(stdout,30) staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
!
!  Scan variable list from input NetCDF and activate switches for
!  stations variables. Get variable IDs.
!
        do i=1,nvars
          lvar=lenstr(varnam(i))
          lsvar=lenstr(vname(1,indexd))
          if (varnam(i)(1:lvar).eq.vname(1,indexd)(1:lsvar)) then
            gotd=.true.
            status=nf_inq_varid(ncstaid,vname(1,indexd)(1:lsvar),
     &                                                      stadid)
          endif
          lsvar=lenstr(vname(1,indexub))
          if (varnam(i)(1:lvar).eq.vname(1,indexub)(1:lsvar)) then
            gotub=.true.
            status=nf_inq_varid(ncstaid,vname(1,indexub)(1:lsvar),
     &                          staubid)
          endif
          lsvar=lenstr(vname(1,indexvb))
          if (varnam(i)(1:lvar).eq.vname(1,indexvb)(1:lsvar)) then
            gotvb=.true.
            status=nf_inq_varid(ncstaid,vname(1,indexvb)(1:lsvar),
     &                          stavbid)
          endif
          lsvar=lenstr(vname(1,indexz))
          if (varnam(i)(1:lvar).eq.vname(1,indexz)(1:lsvar)) then
            gotz=.true.
            status=nf_inq_varid(ncstaid,vname(1,indexz)(1:lsvar),
     &                                                      stazid)
          endif
# ifdef SOLVE3D
          lsvar=lenstr(vname(1,indexu))
          if (varnam(i)(1:lvar).eq.vname(1,indexu)(1:lsvar)) then
            gotu=.true.
            status=nf_inq_varid(ncstaid,vname(1,indexu)(1:lsvar),
     &                                                      stauid)
          endif
          lsvar=lenstr(vname(1,indexv))
          if (varnam(i)(1:lvar).eq.vname(1,indexv)(1:lsvar)) then
            gotv=.true.
            status=nf_inq_varid(ncstaid,vname(1,indexv)(1:lsvar),
     &                                                     stavid)
          endif
          lsvar=lenstr(vname(1,indexw))
          if (varnam(i)(1:lvar).eq.vname(1,indexw)(1:lsvar)) then
            gotw=.true.
            status=nf_inq_varid(ncstaid,vname(1,indexw)(1:lsvar),
     &                                                     stawid)
          endif
          do itrc=1,NT
            lsvar=lenstr(vname(1,indext(itrc)))
            if(varnam(i)(1:lvar).eq.vname(1,indext(itrc))(1:lsvar))
     &                                                        then
             gott(itrc)=.true.
             status=nf_inq_varid(ncstaid,vname(1,indext(itrc))
     &                                      (1:lsvar),statid(itrc))
            endif
          enddo
          lsvar=lenstr(vname(1,indexr))
          if (varnam(i)(1:lvar).eq.vname(1,indexr)(1:lsvar)) then
            gotr=.true.
            status=nf_inq_varid(ncstaid,vname(1,indexr)(1:lsvar),
     &                                                     starid)
          endif
#  ifdef LMD_KPP
          lsvar=lenstr(vname(1,indexhbl))
          if (varnam(i)(1:lvar).eq.vname(1,indexhbl)(1:lsvar)) then
            gothbl=.true.
            status=nf_inq_varid(ncstaid,vname(1,indexhbl)(1:lsvar),
     &                          stahblid)
          endif
#  endif /* LMD_KPP */
          lsvar=lenstr(vname(1,indexakv))
          if (varnam(i)(1:lvar).eq.vname(1,indexakv)(1:lsvar)) then
            gotakv=.true.
            status=nf_inq_varid(ncstaid,vname(1,indexakv)(1:lsvar),
     &                          staakvid)
          endif
          lsvar=lenstr(vname(1,indexakt))
          if (varnam(i)(1:lvar).eq.vname(1,indexakt)(1:lsvar)) then
            gotakt=.true.
            status=nf_inq_varid(ncstaid,vname(1,indexakt)(1:lsvar),
     &                          staaktid)
          endif
          lsvar=lenstr(vname(1,indexaks))
          if (varnam(i)(1:lvar).eq.vname(1,indexaks)(1:lsvar)) then
            gotaks=.true.
            status=nf_inq_varid(ncstaid,vname(1,indexaks)(1:lsvar),
     &                          staaksid)
          endif
# endif /* SOLVE3D */
        enddo
!
!  Check if station variables are available in input NetCDF file.
!
        if (.not.gotd) then
          lsvar=lenstr(vname(1,indexd))
          write(stdout,40) vname(1,indexd)(1:lsvar), staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
# ifdef SOLVE3D
        if (.not.gotu.and.wrtU) then
          lsvar=lenstr(vname(1,indexu))
          write(stdout,40) vname(1,indexu)(1:lsvar), staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
        if (.not.gotv.and.wrtV) then
          lsvar=lenstr(vname(1,indexv))
          write(stdout,40) vname(1,indexv)(1:lsvar), staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
        if (.not.gotw.and.wrtW) then
          lsvar=lenstr(vname(1,indexw))
          write(stdout,40) vname(1,indexw)(1:lsvar), staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
        do itrc=1,NT
          if (.not.gott(itrc).and.wrtT(itrc)) then
            lsvar=lenstr(vname(1,indext(itrc)))
            write(stdout,40) vname(1,indext(itrc))(1:lsvar),
     &                       staname(1:lstr)
            may_day_flag=3
            return                       !-->  EXIT
          endif
        enddo
        if (.not.gotr.and.wrtRHO) then
          lsvar=lenstr(vname(1,indexr))
          write(stdout,40) vname(1,indexr)(1:lsvar), staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
#  ifdef LMD_KPP
        if (.not.gothbl.and.wrtHBL) then
          lsvar=lenstr(vname(1,indexhbl))
          write(stdout,40) vname(1,indexr)(1:lsvar), staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
#  endif /* LMD_KPP */
        if (.not.gotakv.and.wrtAKV) then
          lsvar=lenstr(vname(1,indexakv))
          write(stdout,40) vname(1,indexakv)(1:lsvar),staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
        if (.not.gotakt.and.wrtAKT) then
          lsvar=lenstr(vname(1,indexakt))
          write(stdout,40) vname(1,indexakt)(1:lsvar),staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
#  ifdef SALINITY
        if (.not.gotaks.and.wrtAKS) then
          lsvar=lenstr(vname(1,indexaks))
          write(stdout,40) vname(1,indexaks)(1:lsvar),staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
#  endif /* SALINITY */
# endif /* SOLVE3D */
        if (.not.gotub.and.wrtUBAR) then
          lsvar=lenstr(vname(1,indexub))
          write(stdout,40) vname(1,indexub)(1:lsvar), staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
        if (.not.gotvb.and.wrtVBAR) then
          lsvar=lenstr(vname(1,indexvb))
          write(stdout,40) vname(1,indexvb)(1:lsvar), staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
        if (.not.gotz.and.wrtZ) then
          lsvar=lenstr(vname(1,indexz))
          write(stdout,40) vname(1,indexz)(1:lsvar), staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif
!
!  Set unlimited time record dimension to current value.
!
        tstaindx=tsize
      endif
!
  10  format(/' DEF_STATION - unable to create stations file: ',a)
  20  format(/' DEF_STATION - error while writing variable: ',a,
     &        /,15x,'into stations file: ',a)
  30  format(/' DEF_STATION - unable to open stations file: ',a)
  40  format(/' DEF_STATION - unable to find variable: ',a,2x,
     &        ' in stations file: ',a)
      return
      end
#else
      subroutine def_station_empty
      return
      end
#endif /* STATIONS */
 
