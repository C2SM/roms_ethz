#ifndef AVRH
# include "cppdefs.h"
# define NETCDF4
# define NCCOMPRESS
      subroutine def_his (ncid, total_rec, ierr)
#else
# ifndef AVRH2
      subroutine def_avg (ncid, total_rec, ierr)
# else
#  ifdef HFREQ_2D_AVERAGES
      subroutine def_avg2 (ncid, total_rec, ierr)
#  endif
# endif
#endif
!
! Create/open averages/history netCDF file. In the case when a new
! netCDF file is created, define all variables, their dimensions and
! attributes. In the case when a previously existing netCDF file is
! to be opened for addition of new data, verify that all dimensions
! of the file are consistent with the present model configuration
! and all necessary variables exist. Save netCDF IDs for all needed
! variables. Also determine size of the unlimited dimension.
!
! The difference between def_his and def_avg is as follows: they
! have different netCDF file name (hisname/avgname); netCDF file ID
! (passed as argument); time record index (hisindx/avgindx); array
! of switches which variables to write (wrthis/wrtavg); and different
! sets of netCDF variable IDs (hisTime...hisHbl/avgTime...avgHbl);
! and the first attribute of each variable, long_name, has prefix
! 'averaged'. Because most of the code is identical for both
! routines, the second one is generated from the first entirely
! by CPP.
!
      implicit none
      logical create_new_file
      integer ncid, total_rec, ierr, rec, lstr,lvar,lenstr, timedim
     &      , r2dgrd(3),  u2dgrd(3), v2dgrd(3),  auxil(2),  checkdims
#ifdef SOLVE3D
     &      , r3dgrd(4),  u3dgrd(4), v3dgrd(4),  w3dgrd(4), itrc
#endif
      character*60 text
      integer my_nf_def_dim, ifour
#ifdef NCCOMPRESS
      integer ncshuff, ncdefl, nclev, ncctg
     &        , ncrchnk2(3), ncuchnk2(3), ncvchnk2(3)
     &        , ncrchnk3(4), ncuchnk3(4), ncvchnk3(4)
      data ncshuff, ncdefl, nclev, ncctg / 0, 1, 1, 0 /
#endif

      parameter (ifour=4)
#include "param.h"
#include "scalars.h"
#include "ncvars.h"
#include "netcdf.inc"


#ifdef MASK_LAND_DATA
# ifdef HIS_DOUBLE
      real*8 spv_set
      parameter (spv_set=1.D+33)
# else
      real*4 spv_set
      parameter (spv_set=1.E+33)
# endif
#endif

#ifdef NCCOMPRESS
c  Chunk Size (atomic access size)
c  for netcdf4 (HDF5) 2 and 3d fields in output
      ncrchnk2(1)=xi_rho
      ncrchnk2(2)=eta_rho
      ncrchnk2(3)=1
      ncuchnk2(1)=xi_u
      ncuchnk2(2)=eta_rho
      ncuchnk2(3)=1
      ncvchnk2(1)=xi_rho
      ncvchnk2(2)=eta_v
      ncvchnk2(3)=1
      ncrchnk3(1)=xi_rho
      ncrchnk3(2)=eta_rho
      ncrchnk3(3)=1
      ncrchnk3(4)=1
      ncuchnk3(1)=xi_u
      ncuchnk3(2)=eta_rho
      ncuchnk3(3)=1
      ncuchnk3(4)=1
      ncvchnk3(1)=xi_rho
      ncvchnk3(2)=eta_v
      ncvchnk3(3)=1
      ncvchnk3(4)=1
#endif

#ifndef AVRH
# define ncname hisname
# define rec_per_file nrpfhis
# define wrt wrthis
# define vidTime hisTime
# define vidTstep hisTstep
# define vidZ hisZ
# define vidUb hisUb
# define vidVb hisVb
# define vidU hisU
# define vidV hisV
# define vidT hisT
# define vidR hisR
# define vidO hisO
# define vidW hisW
# define vidAkv hisAkv
# define vidAkt hisAkt
# ifdef KPP_DIAGNOSE
#  define vidRich hisRich
#  define vidRichN hisRichN
# endif
# define vidAks hisAks
# define vidHbl hisHbl
# define vidHbbl hisHbbl
# if defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC
#  if defined CH_CARBON_DEPTH
#   define vidPHd hisPHd
#   define vidCO2STARd hisCO2STARd
#   define vidHCO3d hisHCO3d
#   define vidCO3d hisCO3d
#  endif /* CH_CARBON_DEPTH */
#  define vidPH hisPH
#  define vidPCO2 hisPCO2
#  define vidPCO2air hisPCO2air
#  define vidPARinc hisPARinc
#  define vidPAR hisPAR
#  ifdef SEDIMENT_BIOLOGY
#   define vidTsed hisTsed
#  endif
# endif /* BIOLOGY_NPZDOC || BIOLOGY_BEC*/
#else
# ifndef AVRH2
# define ncname avgname
# define rec_per_file nrpfavg
# define wrt wrtavg
# define vidTime avgTime
# define vidTstep avgTstep
# define vidZ avgZ
# define vidUb avgUb
# define vidVb avgVb
# define vidU avgU
# define vidV avgV
# define vidT avgT
# define vidR avgR
# define vidO avgO
# define vidW avgW
# define vidAkv avgAkv
# define vidAkt avgAkt
# ifdef KPP_DIAGNOSE
#  define vidRich avgRich
#  define vidRichN avgRichN
# endif
# define vidAks avgAks
# define vidHbl avgHbl
# define vidHbbl avgHbbl
# if defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC
#  if defined CH_CARBON_DEPTH
#   define vidPHd avgPHd
#   define vidCO2STARd avgCO2STARd
#   define vidHCO3d avgHCO3d
#   define vidCO3d avgCO3d
#  endif /* CH_CARBON_DEPTH */
#  define vidPH avgPH
#  define vidPCO2 avgPCO2
#  define vidPCO2air avgPCO2air
#  define vidPARinc avgPARinc
#  define vidPAR avgPAR
#  ifdef SEDIMENT_BIOLOGY
#    define vidTsed avgTsed
#  endif
# endif /* BIOLOGY_NPZDOC || BIOLOGY_BEC */
# else /* AVRH2 is defined */
#  define ncname avg2name
#  define rec_per_file nrpfavg2
#  define wrt wrtavg2
#  define vidTime avg2Time
#  define vidTstep avg2Tstep
#  define vidZ avg2Z
#  define vidUb avg2Ub
#  define vidVb avg2Vb
#  define vidU avg2Us
#  define vidV avg2Vs
#  define vidT avg2Ts
#  define vidR avg2Rs
#  define vidO avg2Os
#  define vidW avg2Ws
#  define vidAkv avg2Akvs
#  define vidAkt avg2Akts
#  ifdef KPP_DIAGNOSE
#   define vidRich avg2Richs
#   define vidRichN avg2RichNs
#  endif
#  define vidAks avg2Akss
#  define vidHbl avg2Hbl
#  define vidHbbl avg2Hbbl
#  if defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC
#   if defined CH_CARBON_DEPTH
#    define vidPHd avg2PHds
#    define vidCO2STARd avg2CO2STARds
#    define vidHCO3d avg2HCO3ds
#    define vidCO3d avg2CO3ds
#   endif /* CH_CARBON_DEPTH */
#   define vidPH avg2PHs
#   define vidPCO2 avg2PCO2
#   define vidPCO2air avg2PCO2air
#   define vidPARinc avg2PARinc
#   define vidPAR avg2PAR
#   ifdef SEDIMENT_BIOLOGY
#     define vidTsed avg2Tsed
#   endif
#  endif /* BIOLOGY_NPZDOC || BIOLOGY_BEC */
# endif /* AVRH2 */
#endif /* AVRH */
!
! Put time record index into file name. In  the case when model
! output is to be arranged into sequence of named files, the naming
! convention is as follows: 'his_root.INDEX.[MPI_node.]nc', where
! INDEX is an integer number such that (i) it is divisible by the
! specified number of records per file; and (ii)
!
!      INDEX + record_within_the_file = total_record
!
! where, 1 =< record_within_the_file =< records_per_file, so that
! total_record changes continuously throughout the sequence of files.
!
      ierr=0
      lstr=lenstr(ncname)
      if (rec_per_file.gt.0) then
        lvar=total_rec-(1+mod(total_rec-1, rec_per_file))
        call insert_time_index (ncname, lstr, lvar, ierr)
        if (ierr .ne. 0) goto 99
      endif
!
! Decide whether to create a new file, or open existing one.
! Overall the whole code below is organized into 3-way switch,
!
! 10  if (create_new_file) then
!        .... create new file, save netCDF ids for all variables;
!     elseif (ncid.eq.-1) then
!        .... try to open existing file and check its dimensions
!       if (cannot be opened or rejected) then
!         create_new_file=.true.
!         goto 10
!       endif   and prepare
!        .... prepare the file for adding new data,
!        .... find and save netCDF ids for all variables
!     else
!        .... just open, no checking, all ids are assumed to be
!        .... already known (MPI single file output only).
!     endif
!
! which is designed to implement flexible opening policy:
! if ldefhis=.true., it forces creation of a new file [if the
! file already exists, it will be overwritten]; on the other hand,
! ldefhis=.false., it is assumed that the file already exists and
! an attempt to open it is made; if the attempt is successful, the
! file is prepared for appending hew data; if it fails, a new file
! is created.
!
      create_new_file=ldefhis
      if (ncid.ne.-1) create_new_file=.false.
#if defined MPI && !defined PARALLEL_FILES
      if (mynode.gt.0) create_new_file=.false.
#endif
!
! Create new history/averages file:    Put global attributes
! ====== === ======= ======== =====    and define all variables.
!
  10  if (create_new_file) then
!  10  continue
#ifdef NETCDF4
        ierr=nf_create(ncname(1:lstr), 
     &                 or(nf_clobber,nf_netcdf4), ncid)
#else
        ierr=nf_create(ncname(1:lstr), 
     &                 or(nf_clobber,nf_64bit_offset), ncid)
#endif
        if (ierr .ne. nf_noerr) then
          write(stdout,'(/1x,4A/)') 'ERROR in def_his/avg: Can not ',
     &                 'create netCDF file ''', ncname(1:lstr), '''.'
          goto 99
        endif
        if (rec_per_file.eq.0) total_rec=0
!
! Put global attributes.
! --- ------ -----------
!
        call put_global_atts (ncid, ierr)
!
! Define dimensions of staggered fields.
! ------ ---------- -- --------- -------
!
        ierr=my_nf_def_dim (ncid, 'xi_rho',  xi_rho,  r2dgrd(1))
        ierr=my_nf_def_dim (ncid, 'xi_u',    xi_u,    u2dgrd(1))
        ierr=my_nf_def_dim (ncid, 'eta_rho', eta_rho, r2dgrd(2))
        ierr=my_nf_def_dim (ncid, 'eta_v',   eta_v,   v2dgrd(2))
#ifdef SOLVE3D
# if defined HFREQ_2D_AVERAGES && defined AVRH2
!       Only 2D slices of 3d vars
        ierr=my_nf_def_dim (ncid, 's_rho',   1,       r3dgrd(3))
        ierr=my_nf_def_dim (ncid, 's_w',     1,     w3dgrd(3))
# else
        ierr=my_nf_def_dim (ncid, 's_rho',   N,       r3dgrd(3))
        ierr=my_nf_def_dim (ncid, 's_w',     N+1,     w3dgrd(3))
# endif
#endif
        ierr=my_nf_def_dim (ncid, 'time', nf_unlimited, timedim)
        ierr=my_nf_def_dim (ncid, 'auxil',   ifour,    auxil(1))

        auxil(2)=timedim
 
        r2dgrd(3)=timedim           ! Free surface
 
        u2dgrd(2)=r2dgrd(2)         ! 2D UBAR-type
        u2dgrd(3)=timedim
 
        v2dgrd(1)=r2dgrd(1)         ! 2D VBAR-type
        v2dgrd(3)=timedim
 
#ifdef SOLVE3D
        r3dgrd(1)=r2dgrd(1)         !
        r3dgrd(2)=r2dgrd(2)         ! 3D RHO-type
        r3dgrd(4)=timedim           !
 
        u3dgrd(1)=u2dgrd(1)         !
        u3dgrd(2)=r2dgrd(2)         ! 3D U-type
        u3dgrd(3)=r3dgrd(3)         !
        u3dgrd(4)=timedim
 
        v3dgrd(1)=r2dgrd(1)         !
        v3dgrd(2)=v2dgrd(2)         ! 3D V-type
        v3dgrd(3)=r3dgrd(3)         !
        v3dgrd(4)=timedim
 
        w3dgrd(1)=r2dgrd(1)         !
        w3dgrd(2)=r2dgrd(2)         ! 3D W-type
        w3dgrd(4)=timedim           !
#endif
#if (defined PUT_GRID_INTO_HISTORY && !defined AVRH)\
 || (defined PUT_GRID_INTO_AVERAGES && defined AVRH)
!
! Define grid variables.
! ------ ---- ----------
!
        if (total_rec.le.1) call def_grid (ncid, r2dgrd)
#endif
 
!
! Define evolving model variables.
! ------ -------- ----- ----------
!
! Time step number and time record indices:
!
        ierr=nf_def_var (ncid, 'time_step', nf_int, 2, auxil,
     &                                                 vidTstep)
        ierr=nf_put_att_text (ncid, vidTstep, 'long_name', 48,
     &       'time step and record numbers from initialization')
!
! Time.
!
        lvar=lenstr(vname(1,indxTime))
        ierr=nf_def_var (ncid, vname(1,indxTime)(1:lvar), NF_FOUT,
     &                                        1, timedim, vidTime)
#ifdef AVRH
        text='averaged '/ /vname(2,indxTime)
#else
        text=vname(2,indxTime)
#endif
        lvar=lenstr(text)
        ierr=nf_put_att_text (ncid, vidTime, 'long_name', lvar,
     &                                             text(1:lvar))
        lvar=lenstr(vname(3,indxTime))
        ierr=nf_put_att_text (ncid, vidTime, 'units',  lvar,
     &                                vname(3,indxTime)(1:lvar))
!
! Free-surface.
!
        if (wrt(indxZ)) then
          lvar=lenstr(vname(1,indxZ))
          ierr=nf_def_var (ncid, vname(1,indxZ)(1:lvar), NF_FOUT,
     &                                           3, r2dgrd, vidZ)
#ifdef AVRH
          text='averaged '/ /vname(2,indxZ)
#else
          text=vname(2,indxZ)
#endif
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidZ, 'long_name', lvar,
     &                                            text(1:lvar))
          lvar=lenstr(vname(3,indxZ))
          ierr=nf_put_att_text (ncid, vidZ, 'units',     lvar,
     &                                  vname(3,indxZ)(1:lvar))
#ifdef MASK_LAND_DATA
          ierr=nf_put_att_FOUT (ncid, vidZ, '_FillValue',
     &                                   NF_FOUT, 1, spv_set)
#endif
#ifdef NCCOMPRESS
          ierr=nf_def_var_deflate(ncid, vidZ, ncshuff, ncdefl, nclev)
          ierr=nf_def_var_chunking(ncid, vidZ, ncctg, ncrchnk2)
#endif

        endif
!
! 2D momenta in XI- and ETA-directions.
!
        if (wrt(indxUb)) then
          lvar=lenstr(vname(1,indxUb))
          ierr=nf_def_var (ncid, vname(1,indxUb)(1:lvar), NF_FOUT,
     &                                           3, u2dgrd, vidUb)
#ifdef AVRH
          text='averaged '/ /vname(2,indxUb)
#else
          text=vname(2,indxUb)
#endif
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidUb, 'long_name', lvar,
     &                                             text(1:lvar))
          lvar=lenstr(vname(3,indxUb))
          ierr=nf_put_att_text (ncid, vidUb, 'units',     lvar,
     &                                  vname(3,indxUb)(1:lvar))
#ifdef MASK_LAND_DATA
          ierr=nf_put_att_FOUT (ncid, vidUb, '_FillValue',
     &                                   NF_FOUT, 1, spv_set)
#endif
#ifdef NCCOMPRESS
          ierr=nf_def_var_deflate(ncid, vidUb, ncshuff, ncdefl, nclev)
          ierr=nf_def_var_chunking(ncid, vidUb, ncctg, ncuchnk2)
#endif
        endif
 
        if (wrt(indxVb)) then
          lvar=lenstr(vname(1,indxVb))
          ierr=nf_def_var (ncid, vname(1,indxVb)(1:lvar), NF_FOUT,
     &                                           3, v2dgrd, vidVb)
#ifdef AVRH
          text='averaged '/ /vname(2,indxVb)
#else
          text=vname(2,indxVb)
#endif
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidVb, 'long_name', lvar,
     &                                             text(1:lvar))
          lvar=lenstr(vname(3,indxVb))
          ierr=nf_put_att_text (ncid, vidVb, 'units',     lvar,
     &                                  vname(3,indxVb)(1:lvar))
#ifdef MASK_LAND_DATA
          ierr=nf_put_att_FOUT (ncid, vidVb, '_FillValue',
     &                                      NF_FOUT, 1, spv_set)
#endif
        endif
#ifdef NCCOMPRESS
          ierr=nf_def_var_deflate(ncid, vidVb, ncshuff, ncdefl, nclev)
          ierr=nf_def_var_chunking(ncid, vidVb, ncctg, ncvchnk2)
#endif
#ifdef SOLVE3D
!
! 3D momenta in XI- and ETA-directions.
!
        if (wrt(indxU)) then
          lvar=lenstr(vname(1,indxU))
          ierr=nf_def_var (ncid, vname(1,indxU)(1:lvar), NF_FOUT,
     &                                           4, u3dgrd, vidU)
# ifdef AVRH
          text='averaged '/ /vname(2,indxU)
# else
          text=vname(2,indxU)
# endif
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidU, 'long_name', lvar,
     &                                            text(1:lvar))
          lvar=lenstr(vname(3,indxU))
          ierr=nf_put_att_text (ncid, vidU, 'units',     lvar,
     &                                  vname(3,indxU)(1:lvar))
#ifdef MASK_LAND_DATA
          ierr=nf_put_att_FOUT (ncid, vidU, '_FillValue',
     &                                   NF_FOUT, 1, spv_set)
#endif
#ifdef NCCOMPRESS
          ierr=nf_def_var_deflate(ncid, vidU, ncshuff, ncdefl, nclev )
          ierr=nf_def_var_chunking(ncid, vidU, ncctg, ncuchnk3)
#endif

        endif
 
        if (wrt(indxV)) then
          lvar=lenstr(vname(1,indxV))
          ierr=nf_def_var (ncid, vname(1,indxV)(1:lvar), NF_FOUT,
     &                                           4, v3dgrd, vidV)
# ifdef AVRH
          text='2d-slice of averaged '/ /vname(2,indxV)
# else
          text='2d-slice of '/ /vname(2,indxV)
# endif
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidV, 'long_name', lvar,
     &                                            text(1:lvar))
          lvar=lenstr(vname(3,indxV))
          ierr=nf_put_att_text (ncid, vidV, 'units',     lvar,
     &                                  vname(3,indxV)(1:lvar))
# ifdef MASK_LAND_DATA
          ierr=nf_put_att_FOUT (ncid, vidV, '_FillValue',
     &                                   NF_FOUT, 1, spv_set)
# endif
# ifdef NCCOMPRESS
          ierr=nf_def_var_deflate(ncid, vidV, ncshuff, ncdefl, nclev )
          ierr=nf_def_var_chunking(ncid, vidV, ncctg, ncvchnk3)
# endif
# ifdef HFREQ_2D_AVERAGES
            ierr=nf_put_att_text (ncid, vidT(itrc), 'sigma-level',
     &                                          lvar, 'top')
# endif
        endif
!
! Tracer variables.
!
        do itrc=1,NT
          if (wrt(indxT+itrc-1)) then
            lvar=lenstr(vname(1,indxT+itrc-1))
            ierr=nf_def_var (ncid, vname(1,indxT+itrc-1)(1:lvar),
     &                             NF_FOUT, 4, r3dgrd, vidT(itrc))
# ifdef AVRH
            text='averaged '/ /vname(2,indxT+itrc-1)
# else
            text=vname(2,indxT+itrc-1)
# endif
            lvar=lenstr(text)
            ierr=nf_put_att_text (ncid, vidT(itrc), 'long_name',
     &                                          lvar, text(1:lvar))
            lvar=lenstr(vname(3,indxT+itrc-1))
            ierr=nf_put_att_text (ncid, vidT(itrc), 'units', lvar,
     &                               vname(3,indxT+itrc-1)(1:lvar))
# ifdef MASK_LAND_DATA
            ierr=nf_put_att_FOUT (ncid, vidT(itrc), '_FillValue',
     &                                       NF_FOUT, 1, spv_set)
# endif
# ifdef NCCOMPRESS
          ierr=nf_def_var_deflate(ncid, vidT, ncshuff, ncdefl, nclev )
          ierr=nf_def_var_chunking(ncid, vidT, ncctg, ncrchnk3)
# endif
# ifdef HFREQ_2D_AVERAGES
            ierr=nf_put_att_text (ncid, vidT(itrc), 'sigma-level',
     &                                          lvar, 'top')
# endif
          endif
        enddo
# ifdef SEDIMENT_BIOLOGY
!
! Sediment tracer variables.
!
        do itrc=1,NT_sed
          if (wrt(indxSedOrgN+itrc-1)) then
            lvar=lenstr(vname(1,indxSedOrgN+itrc-1))
            ierr=nf_def_var (ncid, 
     &           vname(1,indxSedOrgN+itrc-1)(1:lvar),
     &           NF_FOUT, 3, r2dgrd, vidTsed(itrc))
# ifndef AVRH
            lvar=lenstr(vname(2,indxSedOrgN+itrc-1))
            ierr=nf_put_att_text (ncid, vidTsed(itrc), 'long_name',
     &           lvar, vname(2,indxSedOrgN+itrc-1)(1:lvar))
# else
            text='averaged '/ /vname(2,indxSedOrgN+itrc-1)
            lvar=lenstr(text)
            ierr=nf_put_att_text (ncid, vidTsed(itrc), 'long_name',
     &           lvar, text(1:lvar))
# endif
            lvar=lenstr(vname(3,indxSedOrgN+itrc-1))
            ierr=nf_put_att_text (ncid, vidTsed(itrc), 'units', lvar,
     &           vname(3,indxSedOrgN+itrc-1)(1:lvar))
            lvar=lenstr(vname(4,indxSedOrgN+itrc-1))
            ierr=nf_put_att_text (ncid, vidTsed(itrc), 'field', lvar,
     &           vname(4,indxSedOrgN+itrc-1)(1:lvar))
          endif
        enddo
# endif /* SEDIMENT_BIOLOGY */
!
! Density anomaly.
!
        if (wrt(indxR)) then
          lvar=lenstr(vname(1,indxR))
          ierr=nf_def_var (ncid, vname(1,indxR)(1:lvar), NF_FOUT,
     &                                           4, r3dgrd, vidR)
# ifdef AVRH
          text='averaged '/ /vname(2,indxR)
# else
          text=vname(2,indxR)
# endif
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidR, 'long_name', lvar,
     &                                            text(1:lvar))
          lvar=lenstr(vname(3,indxR))
          ierr=nf_put_att_text (ncid, vidR, 'units',     lvar,
     &                                  vname(3,indxR)(1:lvar))
#ifdef MASK_LAND_DATA
          ierr=nf_put_att_FOUT (ncid, vidR, '_FillValue',
     &                                   NF_FOUT, 1, spv_set)
#endif
#ifdef NCCOMPRESS
          ierr=nf_def_var_deflate(ncid, vidR, ncshuff, ncdefl, nclev )
          ierr=nf_def_var_chunking(ncid, vidR, ncctg, ncrchnk3)
#endif
        endif
!
! S-coordinate "omega" vertical velocity.
!
        if (wrt(indxO)) then
          lvar=lenstr(vname(1,indxO))
          ierr=nf_def_var (ncid, vname(1,indxO)(1:lvar), NF_FOUT,
     &                                           4, w3dgrd, vidO)
# ifdef AVRH
          text='averaged '/ /vname(2,indxO)
# else
          text=vname(2,indxO)
# endif
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidO, 'long_name', lvar,
     &                                            text(1:lvar))
          lvar=lenstr(vname(3,indxO))
          ierr=nf_put_att_text (ncid, vidO, 'units',     lvar,
     &                                  vname(3,indxO)(1:lvar))
#ifdef MASK_LAND_DATA 
          ierr=nf_put_att_FOUT (ncid, vidO, '_FillValue',
     &                                   NF_FOUT, 1, spv_set)
#endif
#ifdef NCCOMPRESS
          ierr=nf_def_var_deflate(ncid, vidO, ncshuff, ncdefl, nclev )
          ierr=nf_def_var_chunking(ncid, vidO, ncctg, ncrchnk3)
#endif
        endif
!
! True vertical velocity.
!
        if (wrt(indxW)) then
          lvar=lenstr(vname(1,indxW))
          ierr=nf_def_var (ncid, vname(1,indxW)(1:lvar), NF_FOUT,
     &                                           4, r3dgrd, vidW)
# ifdef AVRH
          text='averaged '/ /vname(2,indxW)
# else
          text=vname(2,indxW)
# endif
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidW, 'long_name', lvar,
     &                                            text(1:lvar))
          lvar=lenstr(vname(3,indxW))
          ierr=nf_put_att_text (ncid, vidW, 'units',     lvar,
     &                                  vname(3,indxW)(1:lvar))
#ifdef MASK_LAND_DATA 
          ierr=nf_put_att_FOUT (ncid, vidW, '_FillValue',
     &                                   NF_FOUT, 1, spv_set)
#endif
#ifdef NCCOMPRESS
          ierr=nf_def_var_deflate(ncid, vidW, ncshuff, ncdefl, nclev )
          ierr=nf_def_var_chunking(ncid, vidW, ncctg, ncrchnk3)
#endif
        endif
!
! Vertical viscosity coefficient.
!
        if (wrt(indxAkv)) then
          lvar=lenstr(vname(1,indxAkv))
          ierr=nf_def_var (ncid, vname(1,indxAkv)(1:lvar), NF_FOUT,
     &                                           4, w3dgrd, vidAkv)
# ifdef AVRH
          text='averaged '/ /vname(2,indxAkv)
# else
          text=vname(2,indxAkv)
# endif
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidAkv, 'long_name', lvar,
     &                                              text(1:lvar))
          lvar=lenstr(vname(3,indxAkv))
          ierr=nf_put_att_text (ncid, vidAkv, 'units',     lvar,
     &                                  vname(3,indxAkv)(1:lvar))
#ifdef MASK_LAND_DATA 
          ierr=nf_put_att_FOUT (ncid, vidAkv, '_FillValue',
     &                                   NF_FOUT, 1, spv_set)
#endif
#ifdef NCCOMPRESS
          ierr=nf_def_var_deflate(ncid, vidAkv, ncshuff, ncdefl, nclev)
          ierr=nf_def_var_chunking(ncid, vidAkv, ncctg, ncrchnk3)
#endif
        endif
!
! Vertical diffusion coefficient for potential temperature.
!
        if (wrt(indxAkt)) then
          lvar=lenstr(vname(1,indxAkt))
          ierr=nf_def_var (ncid, vname(1,indxAkt)(1:lvar), NF_FOUT,
     &                                           4, w3dgrd, vidAkt)
# ifdef AVRH
          text='averaged '/ /vname(2,indxAkt)
# else
          text=vname(2,indxAkt)
# endif
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidAkt, 'long_name', lvar,
     &                                              text(1:lvar))
          lvar=lenstr(vname(3,indxAkt))
          ierr=nf_put_att_text (ncid, vidAkt, 'units',     lvar,
     &                                  vname(3,indxAkt)(1:lvar))
#ifdef MASK_LAND_DATA 
          ierr=nf_put_att_FOUT (ncid, vidAkt, '_FillValue',
     &                                   NF_FOUT, 1, spv_set)
#endif
#ifdef NCCOMPRESS
          ierr=nf_def_var_deflate(ncid, vidAkt, ncshuff, ncdefl, nclev)
          ierr=nf_def_var_chunking(ncid, vidAkt, ncctg, ncrchnk3)
#endif
        endif
# if 0
# ifdef KPP_DIAGNOSE
!
! Diffusivity due to vertical velocity shear:
!
        if (wrt(indxRich)) then
          lvar=lenstr(vname(1,indxRich))
          ierr=nf_def_var (ncid, vname(1,indxRich)(1:lvar), NF_FOUT,
     &                                           4, w3dgrd, vidRich)
# ifdef AVRH
          text='averaged '/ /vname(2,indxRich)
# else
          text=vname(2,indxRich)
# endif
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidRich, 'long_name', lvar,
     &                                              text(1:lvar))
          lvar=lenstr(vname(3,indxRich))
          ierr=nf_put_att_text (ncid, vidRich, 'units',     lvar,
     &                                  vname(3,indxRich)(1:lvar))
#ifdef MASK_LAND_DATA 
          ierr=nf_put_att_FOUT (ncid, vidRich, '_FillValue',
     &                                   NF_FOUT, 1, spv_set)
#endif
        endif
!
! Local gradient Richardson number:
!
        if (wrt(indxRichN)) then
          lvar=lenstr(vname(1,indxRichN))
          ierr=nf_def_var (ncid, vname(1,indxRichN)(1:lvar), NF_FOUT,
     &                                           4, w3dgrd, vidRichN)
# ifdef AVRH
          text='averaged '/ /vname(2,indxRichN)
# else
          text=vname(2,indxRichN)
# endif
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidRichN, 'long_name', lvar,
     &                                              text(1:lvar))
          lvar=lenstr(vname(3,indxRichN))
          ierr=nf_put_att_text (ncid, vidRichN, 'units',     lvar,
     &                                  vname(3,indxRichN)(1:lvar))
#ifdef MASK_LAND_DATA 
          ierr=nf_put_att_FOUT (ncid, vidRichN, '_FillValue',
     &                                   NF_FOUT, 1, spv_set)
#endif
        endif
# endif /* KPP_DIAGNOSE */
#endif /* 0 */
# ifdef SALINITY
!
! Vertical diffusion coefficient for salinity.
!
        if (wrt(indxAks)) then
          lvar=lenstr(vname(1,indxAks))
          ierr=nf_def_var (ncid, vname(1,indxAks)(1:lvar), NF_FOUT,
     &                                           4, w3dgrd, vidAks)
#  ifdef AVRH
          text='averaged '/ /vname(2,indxAks)
#  else
          text=vname(2,indxAks)
#  endif
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidAks, 'long_name', lvar,
     &                                              text(1:lvar))
          lvar=lenstr(vname(3,indxAks))
          ierr=nf_put_att_text (ncid, vidAks, 'units',     lvar,
     &                                  vname(3,indxAks)(1:lvar))
#ifdef MASK_LAND_DATA 
          ierr=nf_put_att_FOUT (ncid, vidAks, '_FillValue',
     &                                   NF_FOUT, 1, spv_set)
#endif
#ifdef NCCOMPRESS
          ierr=nf_def_var_deflate(ncid, vidAks, ncshuff, ncdefl, nclev)
          ierr=nf_def_var_chunking(ncid, vidAks, ncctg, ncrchnk3)
#endif
        endif
# endif /* SALINITY */
# ifdef LMD_KPP
!
! Depth of planetary boundary layer.
!
        if (wrt(indxHbl)) then
          lvar=lenstr(vname(1,indxHbl))
          ierr=nf_def_var (ncid, vname(1,indxHbl)(1:lvar), NF_FOUT,
     &                                           3, r2dgrd, vidHbl)
#  ifdef AVRH
          text='averaged '/ /vname(2,indxHbl)
#  else
          text=vname(2,indxHbl)
#  endif
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidHbl, 'long_name', lvar,
     &                                              text(1:lvar))
          lvar=lenstr(vname(3,indxHbl))
          ierr=nf_put_att_text (ncid, vidHbl, 'units',     lvar,
     &                                  vname(3,indxHbl)(1:lvar))
#ifdef MASK_LAND_DATA 
          ierr=nf_put_att_FOUT (ncid, vidHbl, '_FillValue',
     &                                   NF_FOUT, 1, spv_set)
#endif
#ifdef NCCOMPRESS
          ierr=nf_def_var_deflate(ncid, vidHbl, ncshuff, ncdefl, nclev)
          ierr=nf_def_var_chunking(ncid, vidHbl, ncctg, ncrchnk2)
#endif
        endif
# endif /* LMD_KPP */
!
# ifdef LMD_BKPP
!
! Depth of bottom boundary layer.
!
        if (wrt(indxHbbl)) then
          lvar=lenstr(vname(1,indxHbbl))
          ierr=nf_def_var (ncid, vname(1,indxHbbl)(1:lvar), NF_FOUT,
     &                                           3, r2dgrd, vidHbbl)
#  ifdef AVRH
          text='averaged '/ /vname(2,indxHbbl)
#  else
          text=vname(2,indxHbbl)
#  endif
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidHbbl, 'long_name', lvar,
     &                                              text(1:lvar))
          lvar=lenstr(vname(3,indxHbbl))
          ierr=nf_put_att_text (ncid, vidHbbl, 'units',     lvar,
     &                                  vname(3,indxHbbl)(1:lvar))
        endif
# endif /* LMD_BKPP */

# if defined BIOLOGY_BEC || (defined BIOLOGY_NPZDOC && \
    defined OXYGEN && defined CARBON)
#  if defined CH_CARBON_DEPTH

!
! HCO3_depth
!
        lvar=lenstr(vname(1,indxHCO3d_rst))
        ierr=nf_def_var (ncid, vname(1,indxHCO3d_rst)(1:lvar), NF_FOUT,
     &                                           4, r3dgrd, vidHCO3d)
#  ifdef AVRH
        text='averaged '/ /vname(2,indxHCO3d_rst)
#  else
        text=vname(2,indxHCO3d_rst)
#  endif
        lvar=lenstr(text)
        ierr=nf_put_att_text (ncid, vidHCO3d, 'long_name', lvar,
     &                                              text(1:lvar))
        lvar=lenstr(vname(3,indxHCO3d_rst))
        ierr=nf_put_att_text (ncid, vidHCO3d, 'units',     lvar,
     &                                  vname(3,indxHCO3d_rst)(1:lvar))
#ifdef MASK_LAND_DATA
        ierr=nf_put_att_FOUT (ncid, vidHCO3d, '_FillValue',
     &                                  NF_FOUT, 1, spv_set)
#endif

!
! CO3_depth
!
        lvar=lenstr(vname(1,indxCO3d_rst))
        ierr=nf_def_var (ncid, vname(1,indxCO3d_rst)(1:lvar), NF_FOUT,
     &                                           4, r3dgrd, vidCO3d)
#  ifdef AVRH
        text='averaged '/ /vname(2,indxCO3d_rst)
#  else
        text=vname(2,indxCO3d_rst)
#  endif
        lvar=lenstr(text)
        ierr=nf_put_att_text (ncid, vidCO3d, 'long_name', lvar,
     &                                              text(1:lvar))
        lvar=lenstr(vname(3,indxCO3d_rst))
        ierr=nf_put_att_text (ncid, vidCO3d, 'units',     lvar,
     &                                  vname(3,indxCO3d_rst)(1:lvar))
#ifdef MASK_LAND_DATA
        ierr=nf_put_att_FOUT (ncid, vidCO3d, '_FillValue',
     &                                  NF_FOUT, 1, spv_set)
#endif


!
! CO2STAR_DEPTH
!
        lvar=lenstr(vname(1,indxCO2STARd_rst))
        ierr=nf_def_var (ncid, vname(1,indxCO2STARd_rst)(1:lvar), NF_FOUT,
     &                                           4, r3dgrd, vidCO2STARd)
#  ifdef AVRH
        text='averaged '/ /vname(2,indxCO2STARd_rst)
#  else
        text=vname(2,indxCO2STARd_rst)
#  endif
        lvar=lenstr(text)
        ierr=nf_put_att_text (ncid, vidCO2STARd, 'long_name', lvar,
     &                                              text(1:lvar))
        lvar=lenstr(vname(3,indxCO2STARd_rst))
        ierr=nf_put_att_text (ncid, vidCO2STARd, 'units',     lvar,
     &                                  vname(3,indxCO2STARd_rst)(1:lvar))
#ifdef MASK_LAND_DATA
        ierr=nf_put_att_FOUT (ncid, vidCO2STARd, '_FillValue',
     &                                  NF_FOUT, 1, spv_set)
#endif

!
! PH_DEPTH
!         
         lvar=lenstr(vname(1,indxPHd_rst))
        ierr=nf_def_var (ncid, vname(1,indxPHd_rst)(1:lvar), NF_FOUT,
     &                                           4, r3dgrd, vidPHd)
#  ifdef AVRH
        text='averaged '/ /vname(2,indxPHd_rst)
#  else
        text=vname(2,indxPHd_rst)
#  endif
        lvar=lenstr(text)
        ierr=nf_put_att_text (ncid, vidPHd, 'long_name', lvar,
     &                                              text(1:lvar))
        lvar=lenstr(vname(3,indxPHd_rst))
        ierr=nf_put_att_text (ncid, vidPHd, 'units',     lvar,
     &                                  vname(3,indxPHd_rst)(1:lvar))
#ifdef MASK_LAND_DATA
        ierr=nf_put_att_FOUT (ncid, vidPHd, '_FillValue',
     &                                  NF_FOUT, 1, spv_set)
#endif

#endif /*CH_CARBON_DEPTH*/

!
! pH
!
         lvar=lenstr(vname(1,indxPH_rst))
        ierr=nf_def_var (ncid, vname(1,indxPH_rst)(1:lvar), NF_FOUT,
     &                                           3, r2dgrd, vidPH)
#  ifdef AVRH
        text='averaged '/ /vname(2,indxPH_rst)
#  else
        text=vname(2,indxPH_rst)
#  endif
        lvar=lenstr(text)
        ierr=nf_put_att_text (ncid, vidPH, 'long_name', lvar,
     &                                              text(1:lvar))
        lvar=lenstr(vname(3,indxPH_rst))
        ierr=nf_put_att_text (ncid, vidPH, 'units',     lvar,
     &                                  vname(3,indxPH_rst)(1:lvar))
#ifdef MASK_LAND_DATA 
        ierr=nf_put_att_FOUT (ncid, vidPH, '_FillValue',
     &                                  NF_FOUT, 1, spv_set)
#endif
!
! pCO2:
!
        lvar=lenstr(vname(1,indxPCO2_rst))
        ierr=nf_def_var (ncid, vname(1,indxPCO2_rst)(1:lvar), NF_FOUT,
     &                                           3, r2dgrd, vidPCO2)
#  ifdef AVRH
        text='averaged '/ /vname(2,indxPCO2_rst)
#  else
        text=vname(2,indxPCO2_rst)
#  endif
        lvar=lenstr(text)
        ierr=nf_put_att_text (ncid, vidPCO2, 'long_name', lvar,
     &                                              text(1:lvar))
        lvar=lenstr(vname(3,indxPCO2_rst))
        ierr=nf_put_att_text (ncid, vidPCO2, 'units',     lvar,
     &                                  vname(3,indxPCO2_rst)(1:lvar))
#ifdef MASK_LAND_DATA 
        ierr=nf_put_att_FOUT (ncid, vidPCO2, '_FillValue',
     &                                  NF_FOUT, 1, spv_set)
#endif

!
! pCO2air:
!
        lvar=lenstr(vname(1,indxPCO2air_rst))
        ierr=nf_def_var (ncid, vname(1,indxPCO2air_rst)(1:lvar), NF_FOUT,
     &                                           3, r2dgrd, vidPCO2air)
#  ifdef AVRH
        text='averaged '/ /vname(2,indxPCO2air_rst)
#  else
        text=vname(2,indxPCO2air_rst)
#  endif
        lvar=lenstr(text)
        ierr=nf_put_att_text (ncid, vidPCO2air, 'long_name', lvar,
     &                                              text(1:lvar))
        lvar=lenstr(vname(3,indxPCO2air_rst))
        ierr=nf_put_att_text (ncid, vidPCO2air, 'units',     lvar,
     &       vname(3,indxPCO2air_rst)(1:lvar))
#ifdef MASK_LAND_DATA
        ierr=nf_put_att_FOUT (ncid, vidPCO2air, '_FillValue',
     &                                  NF_FOUT, 1, spv_set)
#endif


# endif /* BIOLOGY_BEC || (BIOLOGY_NPZDOC && OXYGEN && CARBON) */

# if defined BIOLOGY_BEC || defined BIOLOGY_NPZDOC
!
! PARinc:
!
        lvar=lenstr(vname(1,indxPARinc_rst))
        ierr=nf_def_var (ncid, vname(1,indxPARinc_rst)(1:lvar), NF_FOUT,
     &       3, r2dgrd, vidPARinc)
#  ifdef AVRH
        text='averaged '/ /vname(2,indxPARinc_rst)
#  else
        text=vname(2,indxPARinc_rst)
#  endif
        lvar=lenstr(text)
        ierr=nf_put_att_text (ncid, vidPARinc, 'long_name', lvar,
     &                                              text(1:lvar))
        lvar=lenstr(vname(3,indxPARinc_rst))
        ierr=nf_put_att_text (ncid, vidPARinc, 'units',     lvar,
     &                                  vname(3,indxPARinc_rst)(1:lvar))
#ifdef MASK_LAND_DATA 
        ierr=nf_put_att_FOUT (ncid, vidPARinc, '_FillValue',
     &                                  NF_FOUT, 1, spv_set)
#endif
!
! PAR:
!
        lvar=lenstr(vname(1,indxPAR_rst))
        ierr=nf_def_var (ncid, vname(1,indxPAR_rst)(1:lvar), NF_FOUT,
     &                                           4, r3dgrd, vidPAR)
#  ifdef AVRH
        text='averaged '/ /vname(2,indxPAR_rst)
#  else
        text=vname(2,indxPAR_rst)
#  endif
        lvar=lenstr(text)
        ierr=nf_put_att_text (ncid, vidPAR, 'long_name', lvar,
     &                                              text(1:lvar))
        lvar=lenstr(vname(3,indxPAR_rst))
        ierr=nf_put_att_text (ncid, vidPAR, 'units',     lvar,
     &                                  vname(3,indxPAR_rst)(1:lvar))
#ifdef MASK_LAND_DATA 
        ierr=nf_put_att_FOUT (ncid, vidPAR, '_FillValue',
     &                                  NF_FOUT, 1, spv_set)
#endif
#endif /* BIOLOGY_BEC || BIOLOGY_NPZDOC */
#endif /* SOLVE3D */
!
! Leave definition mode.
! ----- ---------- -----
!
        ! ierr=nf_enddef(ncid)
        !  Pad header section (20kB) to allow faster adding of new variables
        ierr=nf_enddef(ncid,20000,8,20000,8)
        write(stdout,'(6x,5A,i4)')     'def_his/avg: Created new ',
     &  'netCDF file ''', ncname(1:lstr), '''.' MYID
!
! Open existing file and prepare for appending data:   Inquire about
! ==== ======== ==== === ======= === ========= ====    dimensions and
! variables. Check for consistency with model dimensions. In the case
! when file is rejected (whether it cannot be opened, or something is
! wrong with its dimensions) create a new file.  After that verify
! that all necessary variables are already defined, find and save
! their netCDF IDs.
!
      elseif (ncid.eq.-1) then
        ierr=nf_open (ncname(1:lstr), nf_write, ncid)
        if (ierr. eq. nf_noerr) then
          write(stdout,'(1x,4A,1x,A,I3,A,I4)') 'Opened existing ',
     &                 'netCDF file ''',   ncname(1:lstr),   '''',
     &                 'ncid =', ncid  MYID
          ierr=checkdims (ncid, ncname(1:lstr), lstr, rec)
          if (ierr .eq. nf_noerr) then
            if (rec_per_file.eq.0) then
              ierr=rec+1 - total_rec
            else
              ierr=rec+1 - (1+mod(total_rec-1, rec_per_file))
            endif
            if (ierr.gt.0) then
              MPI_master_only write( stdout,
     &                 '(/1x,A,I5,1x,A/8x,3A,I5,/8x,A,I5,1x,A/)'
     &           ) 'WARNING: def_his/avg: Actual number of records',
     &               rec,  'in netCDF file',  '''',  ncname(1:lstr),
     &             ''' exceeds the record number from restart data',
     &             rec+1-ierr,'/', total_rec,', restart is assumed.'
              rec=rec-ierr
            elseif (rec_per_file.eq.0) then
              total_rec=rec+1           ! <-- set to the next record
#if defined MPI && !defined PARALLEL_FILES
              if (mynode.gt.0) total_rec=total_rec-1
#endif
            endif
            ierr=nf_noerr
          endif
        endif
 
        if (ierr. ne. nf_noerr) then
#if defined MPI && !defined PARALLEL_FILES
          if (mynode.eq.0) then
            create_new_file=.true.
            goto 10
          else
            write(stdout,'(/1x,5A,I4/)') '### ERROR: def_his/avg: ',
     &        'Cannot open file ''', ncname(1:lstr), '''.' MYID
            goto 99                                       !--> ERROR
          endif
#else
          create_new_file=.true.
          goto 10
#endif
        endif
!
! Find netCDF IDs of evolving model variables:
! ---- ------ --- -- -------- ----- ----------
!
! Time step indices:
!
        ierr=nf_inq_varid (ncid, 'time_step', vidTstep)
        if (ierr .ne. nf_noerr) then
          write(stdout,1) 'time_step', ncname(1:lstr)
          goto 99                                         !--> ERROR
        endif
!
! Time.
!
        lvar=lenstr(vname(1,indxTime))
        ierr=nf_inq_varid (ncid,vname(1,indxTime)(1:lvar),vidTime)
        if (ierr .ne. nf_noerr) then
          write(stdout,1) vname(1,indxTime)(1:lvar), ncname(1:lstr)
          goto 99                                         !--> ERROR
        endif
!
! Free-surface.
!
        if (wrt(indxZ)) then
          lvar=lenstr(vname(1,indxZ))
          ierr=nf_inq_varid (ncid, vname(1,indxZ)(1:lvar), vidZ)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxZ)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
!
! 2D momenta in XI- and ETA-directions.
!
        if (wrt(indxUb)) then
          lvar=lenstr(vname(1,indxUb))
          ierr=nf_inq_varid (ncid, vname(1,indxUb)(1:lvar), vidUb)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxUb)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
 
        if (wrt(indxVb)) then
          lvar=lenstr(vname(1,indxVb))
          ierr=nf_inq_varid (ncid, vname(1,indxVb)(1:lvar), vidVb)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxVb)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
 
 
#ifdef SOLVE3D
!
! 3D momenta in XI- and ETA-directions.
!
        if (wrt(indxU)) then
          lvar=lenstr(vname(1,indxU))
          ierr=nf_inq_varid (ncid, vname(1,indxU)(1:lvar), vidU)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxU)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
 
        if (wrt(indxV)) then
          lvar=lenstr(vname(1,indxV))
          ierr=nf_inq_varid (ncid, vname(1,indxV)(1:lvar), vidV)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxV)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
!
! Tracer variables.
!
        do itrc=1,NT
          if (wrt(indxT+itrc-1)) then
            lvar=lenstr(vname(1,indxT+itrc-1))
            ierr=nf_inq_varid (ncid, vname(1,indxT+itrc-1)(1:lvar),
     &                                                 vidT(itrc))
            if (ierr .ne. nf_noerr) then
              write(stdout,1) vname(1,indxT+itrc-1)(1:lvar),
     &                                       ncname(1:lstr)
              goto 99                                     !--> ERROR
            endif
          endif
        enddo
# ifdef SEDIMENT_BIOLOGY
!
! Sediment tracer variables.
!
        do itrc=1,NT_sed
          if (wrt(indxSedOrgN+itrc-1)) then
            lvar=lenstr(vname(1,indxSedOrgN+itrc-1))
            ierr=nf_inq_varid (ncid, 
     &           vname(1,indxSedOrgN+itrc-1)(1:lvar),vidTsed(itrc))
            if (ierr .ne. nf_noerr) then
              write(stdout,1) vname(1,indxSedOrgN+itrc-1)(1:lvar),
     &                                       ncname(1:lstr) 
              goto 99                                     !--> ERROR
            endif
          endif
        enddo
# endif /* SEDIMENT_BIOLOGY */
!
! Density anomaly.
!
        if (wrt(indxR)) then
          lvar=lenstr(vname(1,indxR))
          ierr=nf_inq_varid (ncid, vname(1,indxR)(1:lvar), vidR)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxR)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
!
! S-coordinate "omega" vertical velocity.
!
        if (wrt(indxO)) then
          lvar=lenstr(vname(1,indxO))
          ierr=nf_inq_varid (ncid, vname(1,indxO)(1:lvar), vidO)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxO)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
!
! True vertical velocity.
!
        if (wrt(indxW)) then
          lvar=lenstr(vname(1,indxW))
          ierr=nf_inq_varid (ncid, vname(1,indxW)(1:lvar), vidW)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxW)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
!
! Vertical viscosity coefficient.
!
        if (wrt(indxAkv)) then
          lvar=lenstr(vname(1,indxAkv))
          ierr=nf_inq_varid (ncid, vname(1,indxAkv)(1:lvar), vidAkv)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxAkv)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
!
! Vertical diffusion coefficient for potential temperature.
!
        if (wrt(indxAkt)) then
          lvar=lenstr(vname(1,indxAkt))
          ierr=nf_inq_varid (ncid,vname(1,indxAkt)(1:lvar), vidAkt)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxAkt)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
# ifdef KPP_DIAGNOSE
!
! Diffusivity due to vertical velocity shear:
!
        if (wrt(indxRich)) then
          lvar=lenstr(vname(1,indxRich))
          ierr=nf_inq_varid (ncid,vname(1,indxRich)(1:lvar), vidRich)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxRich)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
!
! Local gradient Richardson number:
!
        if (wrt(indxRichN)) then
          lvar=lenstr(vname(1,indxRichN))
          ierr=nf_inq_varid (ncid,vname(1,indxRichN)(1:lvar), vidRichN)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxRichN)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
# endif /* KPP_DIAGNOSE */
# ifdef SALINITY
!
! Vertical diffusion coefficient for salinity.
!
        if (wrt(indxAks)) then
          lvar=lenstr(vname(1,indxAks))
          ierr=nf_inq_varid (ncid,vname(1,indxAks)(1:lvar), vidAks)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxAks)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
# endif
# ifdef LMD_KPP
!
! Depth of planetary boundary layer.
!
        if (wrt(indxHbl)) then
          lvar=lenstr(vname(1,indxHbl))
          ierr=nf_inq_varid (ncid,vname(1,indxHbl)(1:lvar), vidHbl)
          if (ierr .ne. nf_noerr) then
          write(stdout,1) vname(1,indxHbl)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
# endif
# ifdef LMD_BKPP
!
! Depth of bottom planetary boundary layer.
!
        if (wrt(indxHbbl)) then
          lvar=lenstr(vname(1,indxHbbl))
          ierr=nf_inq_varid (ncid,vname(1,indxHbbl)(1:lvar), vidHbbl)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxHbbl)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
# endif

# if defined BIOLOGY_BEC || (defined BIOLOGY_NPZDOC && \
    defined OXYGEN && defined CARBON)
#  if defined CH_CARBON_DEPTH

!
! HCO3_DEPTH
!
        lvar=lenstr(vname(1,indxHCO3d_rst))
        ierr=nf_inq_varid (ncid,vname(1,indxHCO3d_rst)(1:lvar), vidHCO3d)
        if (ierr .ne. nf_noerr) then
           write(stdout,1) vname(1,indxHCO3d_rst)(1:lvar), ncname(1:lstr)
           goto 99                                       !--> ERROR
        endif

!
! CO3_DEPTH
!
        lvar=lenstr(vname(1,indxCO3d_rst))
        ierr=nf_inq_varid (ncid,vname(1,indxCO3d_rst)(1:lvar), vidCO3d)
        if (ierr .ne. nf_noerr) then
           write(stdout,1) vname(1,indxCO3d_rst)(1:lvar), ncname(1:lstr)
           goto 99                                       !--> ERROR
        endif

!
! Co2STAR_DEPTH
!
        lvar=lenstr(vname(1,indxCO2STARd_rst))
        ierr=nf_inq_varid (ncid,vname(1,indxCO2STARd_rst)(1:lvar), vidCO2STARd)
        if (ierr .ne. nf_noerr) then
           write(stdout,1) vname(1,indxCO2STARd_rst)(1:lvar), ncname(1:lstr)
           goto 99                                       !--> ERROR
        endif

!
! PH_DEPTH
!
        lvar=lenstr(vname(1,indxPHd_rst))
        ierr=nf_inq_varid (ncid,vname(1,indxPHd_rst)(1:lvar), vidPHd)
        if (ierr .ne. nf_noerr) then
           write(stdout,1) vname(1,indxPHd_rst)(1:lvar), ncname(1:lstr)
           goto 99                                       !--> ERROR
        endif


#  endif /* CH_CARBON_DEPTH */
!
! pH
!
        lvar=lenstr(vname(1,indxPH_rst))
        ierr=nf_inq_varid (ncid,vname(1,indxPH_rst)(1:lvar), vidPH)
        if (ierr .ne. nf_noerr) then
           write(stdout,1) vname(1,indxPH_rst)(1:lvar), ncname(1:lstr)
           goto 99                                       !--> ERROR
        endif
!
! pCO2:
!
        lvar=lenstr(vname(1,indxPCO2_rst))
        ierr=nf_inq_varid (ncid,vname(1,indxPCO2_rst)(1:lvar), vidPCO2)
        if (ierr .ne. nf_noerr) then
           write(stdout,1) vname(1,indxPCO2_rst)(1:lvar), ncname(1:lstr)
           goto 99                                       !--> ERROR
        endif
!
! pCO2air:
!
        lvar=lenstr(vname(1,indxPCO2air_rst))
        ierr=nf_inq_varid (ncid,vname(1,indxPCO2air_rst)(1:lvar), 
     &       vidPCO2air)
        if (ierr .ne. nf_noerr) then
           write(stdout,1) vname(1,indxPCO2air_rst)(1:lvar), 
     &          ncname(1:lstr)
           goto 99                                       !--> ERROR
        endif
# endif /* BIOLOGY_BEC || (BIOLOGY_NPZDOC && OXYGEN && CARBON) */

# if defined BIOLOGY_BEC || defined BIOLOGY_NPZDOC
!
! PARinc:
!
        lvar=lenstr(vname(1,indxPARinc_rst))
        ierr=nf_inq_varid (ncid,vname(1,indxPARinc_rst)(1:lvar), 
     &       vidPARinc)
        if (ierr .ne. nf_noerr) then
           write(stdout,1) vname(1,indxPARinc_rst)(1:lvar), 
     &          ncname(1:lstr)
           goto 99                                       !--> ERROR
        endif
!
! PAR:
!
        lvar=lenstr(vname(1,indxPAR_rst))
        ierr=nf_inq_varid (ncid,vname(1,indxPAR_rst)(1:lvar), vidPAR)
        if (ierr .ne. nf_noerr) then
           write(stdout,1) vname(1,indxPAR_rst)(1:lvar), ncname(1:lstr)
           goto 99                                       !--> ERROR
        endif
# endif /* BIOLOGY_BEC || BIOLOGY_NPZDOC */
#endif /* SOLVE3D */
        write(*,'(6x,2A,i4,1x,A,i4)')    'def_his/avg: Opened ',
     &              'existing file from record =', rec MYID
 
#if defined MPI && !defined PARALLEL_FILES
      else
        ierr=nf_open (ncname(1:lstr), nf_write, ncid)
        if (ierr .eq. nf_noerr) then
          write(stdout,'(1x,4A,I3)')        'def_his/avg: Opened ',
     &      'existing file ''', ncname(1:lstr), ''' ncid =', ncid
        else
          write(stdout,'(/1x,5A,I4/)')   '### ERROR: def_his/avg: ',
     &      'Cannot open file ''', ncname(1:lstr), '''.' MYID
          goto 99                                         !--> ERROR
        endif
#endif
      endif             !<--  create_new_file
 
      ierr=nf_set_fill (ncid, nf_nofill, lvar)
      if (ierr .ne. nf_noerr) then
        write(*,'(6x,4A,i4)')    '### ERROR: def_his/avg: Cannot ',
     &          'switch to ''nf_nofill'' mode.', nf_strerror(ierr)
     &           MYID
      endif
   1  format(/1x,'### ERROR: def_his/avg: Cannot find variable ''',
     &                       A, ''' in netCDF file ''', A, '''.'/)
#if (defined PUT_GRID_INTO_HISTORY && !defined AVRH)\
 || (defined PUT_GRID_INTO_AVERAGES && defined AVRH)
!
! Write grid variables.
!------ ---- ----------
!
        if (total_rec.le.1) call wrt_grid (ncid, ncname, lstr)
#endif
  99  return
      end
 
#undef ncname
#undef rec_per_file
#undef wrt
#undef vidTime
#undef vidTstep
#undef vidZ
#undef vidUb
#undef vidVb
#undef vidU
#undef vidV
#undef vidT
#if defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC
# if defined CH_CARBON_DEPTH
#  undef vidPHDEPTH
#  undef vidCO2STAR
#  undef vidHCO3
#  undef vidCO3
# endif /* CH_CARBON_DEPTH */
# undef vidPH
# undef vidPCO2
# undef vidPCO2air
# undef vidPARinc
# undef vidPAR
#  ifdef SEDIMENT_BIOLOGY
#   undef vidTsed
#  endif
#endif /* BIOLOGY_NPZDOC || BIOLOGY_BEC */

#undef vidR
#undef vidO
#undef vidW
#undef vidAkv
#undef vidAkt
#undef vidRich
#undef vidRichN
#undef vidAks
#undef vidHbl
#undef vidHbbl
 
#ifndef AVRH
# define AVRH
# ifdef AVERAGES
#  include "def_his.F"
# endif
#else
# ifndef AVRH2
#  define AVRH2
#  ifdef HFREQ_2D_AVERAGES
#   include "def_his.F"
#  endif
# endif
#endif
