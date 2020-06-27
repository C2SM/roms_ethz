!   $Log: def_phys_flux.F,v $
!   Revision 1.2  2002/04/19 00:47:46  frenzel
!   Added code for physical flux history files and for the analysis of
!   mixing and nudging fluxes.
!
!   Revision 1.1  2002/01/16 19:58:53  frenzel
!   New routines related to flux analysis.
!
!
#ifndef AVRH
# include "cppdefs.h"
#endif
#ifdef PHYS_FLUX_ANALYSIS
#ifndef AVRH
      subroutine def_phys_flux_his(ncid, total_rec, ierr)
#else
      subroutine def_phys_flux_avg(ncid, total_rec, ierr)
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
!      USE scalars
!      USE ncscrum

      implicit none
      logical create_new_file
      integer ncid, total_rec, ierr, rec, lstr,lvar,lenstr, timedim
     &      , r2dgrd(3),  u2dgrd(3), v2dgrd(3),  auxil(2),  checkdims
#ifdef SOLVE3D
     &      , r3dgrd(4),  u3dgrd(4), v3dgrd(4),  w3dgrd(4), itrc
      integer iflux
#endif /* SOLVE3D */
#include "netcdf.inc"
#include "param.h"
#include "scalars.h"
#include "ncvars.h"
#include "physflux.h"
#include "ncroms_physflux.h"

#ifdef MASK_LAND_DATA
# ifdef HIS_DOUBLE
      real*8 spv_set
      parameter (spv_set=1.D+33)
# else
      real*4 spv_set
      parameter (spv_set=1.E+33)
# endif
#endif

#ifndef AVRH
# define ncname phys_flux_his_name
# define rec_per_file nrpf_phys_flux_his
# define vidTime phys_flux_hisTime
# define vidTstep phys_flux_hisTstep
# define vidZ phys_flux_hisZ
# define vidHorXAdvFlux hisHorXAdvFlux
# define vidHorYAdvFlux hisHorYAdvFlux
# define vidVertAdvFlux hisVertAdvFlux
# define vidVertDiffFlux hisVertDiffFlux
# ifdef FULL_PHYS_FLUX_ANALYSIS
# define vidTopFlux hisTopFlux
# define vidSRAbsFlux hisSRAbsFlux
#if !defined SELECTED_FLUX_ANALYSIS /* AH */
# define vidBottomFlux hisBottomFlux
# define vidHorXMixFlux hisHorXMixFlux
# define vidHorYMixFlux hisHorYMixFlux
# define vidVertMixFlux hisVertMixFlux
# define vidNudgingFlux hisNudgingFlux
#endif /* SELECTED_FLUX_ANALYSIS */
# endif /* FULL_PHYS_FLUX_ANALYSIS */
#else /* AVRH */
# define ncname phys_flux_avg_name
# define rec_per_file nrpf_phys_flux_avg
# define vidTime phys_flux_avgTime
# define vidTstep phys_flux_avgTstep
# define vidZ phys_flux_avgZ
# define vidHorXAdvFlux avgHorXAdvFlux
# define vidHorYAdvFlux avgHorYAdvFlux
# define vidVertAdvFlux avgVertAdvFlux
# define vidVertDiffFlux avgVertDiffFlux
# ifdef FULL_PHYS_FLUX_ANALYSIS
# define vidTopFlux avgTopFlux
# define vidSRAbsFlux avgSRAbsFlux
#if !defined SELECTED_FLUX_ANALYSIS /* AH */
# define vidBottomFlux avgBottomFlux
# define vidHorXMixFlux avgHorXMixFlux
# define vidHorYMixFlux avgHorYMixFlux
# define vidVertMixFlux avgVertMixFlux
# define vidNudgingFlux avgNudgingFlux
#endif /* SELECTED_FLUX_ANALYSIS */
# endif /* FULL_PHYS_FLUX_ANALYSIS */
      character*60 text
#endif /* AVRH */

! This check is not performed in def_his.F, but very useful:
! If the output frequency is not a positive number, do nothing at all
#ifndef AVRH
      if (n_phys_flux_his < 1) return
#else
      if (n_phys_flux_avg < 1) return
#endif
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
      lstr=lenstr(ncname)
      if (rec_per_file.gt.0) then
        ierr=0
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
! file is prepared for appending new data; if it fails, a new file
! is created.
!
#ifndef AVRH
      create_new_file = new_phys_flux_his
#else
      create_new_file = new_phys_flux_avg
#endif
      if (ncid.ne.-1) create_new_file=.false.
#if defined MPI && !defined PARALLEL_FILES
      if (mynode.gt.0) create_new_file=.false.
#endif
!
! Create new history/averages file:    Put global attributes
! ====== === ======= ======== =====    and define all variables.
!
  10  create_new_file_if: if (create_new_file) then
! #ifdef LARGE_PHYS_FILES
        ierr=nf_create(ncname(1:lstr), or(nf_clobber,NCFILEFORMAT),
     &        ncid)
! #else
        ! ierr=nf_create(ncname(1:lstr), nf_clobber, ncid)
! #endif
        if (ierr .ne. nf_noerr) then
          write(*,'(/3(1x,A)/)') 'ERROR in def_phys_flux_his/avg:',
     &           'Cannot create netCDF file:', 
     &          ncname(1:lstr)
          goto 99                                         !--> ERROR
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
        ierr=nf_def_dim (ncid, 'xi_rho',   xi_rho,  r2dgrd(1))
        ierr=nf_def_dim (ncid, 'xi_u',     xi_u,     u2dgrd(1))
        ierr=nf_def_dim (ncid, 'eta_rho',  eta_rho,  r2dgrd(2))
        ierr=nf_def_dim (ncid, 'eta_v',    eta_v,    v2dgrd(2))
#ifdef SOLVE3D
        ierr=nf_def_dim (ncid, 's_rho',    N,        r3dgrd(3))
        ierr=nf_def_dim (ncid, 's_w',      N+1,      w3dgrd(3))
#endif
        ierr=nf_def_dim (ncid, 'time', nf_unlimited, timedim)
        ierr=nf_def_dim (ncid, 'auxil',    4,        auxil(1))
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
#endif /* SOLVE3D */
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
     &       vidTstep)
        ierr=nf_put_att_text (ncid, vidTstep, 'long_name', 48,
     &       'time step and record numbers from initialization')
!
! Time.
!
        lvar=lenstr(vname(1,indxTime))
        ierr=nf_def_var (ncid, vname(1,indxTime)(1:lvar),
     &       NF_DOUBLE, 1, timedim, vidTime)
#ifndef AVRH
        lvar=lenstr(vname(2,indxTime))
        ierr=nf_put_att_text (ncid, vidTime, 'long_name', 
     &       lvar, vname(2,indxTime)(1:lvar))
#else
        text='averaged '/ /vname(2,indxTime)
        lvar=lenstr(text)
        ierr=nf_put_att_text (ncid, vidTime, 'long_name', 
     &       lvar, text(1:lvar))
#endif
        lvar=lenstr(vname(3,indxTime))
        ierr=nf_put_att_text (ncid, vidTime, 'units',  lvar,
     &       vname(3,indxTime)(1:lvar))
        lvar=lenstr(vname(4,indxTime))
!        ierr=nf_put_att_text (ncid, vidTime, 'field',  lvar,
!     &       vname(4,indxTime)(1:lvar))

! Free surface
        lvar=lenstr(vname(1,indxZ))
        ierr=nf_def_var (ncid, vname(1,indxZ)(1:lvar),
     &       NF_FOUT, 3, r2dgrd, vidZ)
#ifndef AVRH
          lvar=lenstr(vname(2,indxZ))
          ierr=nf_put_att_text (ncid, vidZ, 'long_name', lvar,
     &                                  vname(2,indxZ)(1:lvar))
#else
          text='averaged '/ /vname(2,indxZ)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidZ, 'long_name', lvar,
     &         text(1:lvar))
#endif
        lvar=lenstr(vname(3,indxZ))
        ierr=nf_put_att_text (ncid, vidZ, 'units',     lvar,
     &       vname(3,indxZ)(1:lvar))
        lvar=lenstr(vname(4,indxZ))
!        ierr=nf_put_att_text (ncid, vidZ, 'field',     lvar,
!     &       vname(4,indxZ)(1:lvar))
#ifdef MASK_LAND_DATA
        ierr=nf_put_att_FOUT (ncid, vidZ, '_FillValue',
     &       NF_FOUT, 1, spv_set)
#endif
!
! Fluxes of tracer variables
!
! horizontal advective fluxes (xi-direction)
        do iflux = 1, NT_PFA
         if (wrtavg(indxT+iflux-1)) then
           lvar=lenstr(vname_phys(1,indxHorXAdvFlux+iflux-1))
           ierr=nf_def_var (ncid, 
     &          vname_phys(1,indxHorXAdvFlux+iflux-1)(1:lvar),
     &          NF_FOUT, 4, r3dgrd, vidHorXAdvFlux(iflux))
#  ifndef AVRH
           lvar=lenstr(vname_phys(2,indxHorXAdvFlux+iflux-1))
           ierr=nf_put_att_text (ncid, vidHorXAdvFlux(iflux), 
     &          'long_name', 
     &          lvar, vname_phys(2,indxHorXAdvFlux+iflux-1)(1:lvar))
#  else
           text='averaged '/ /vname_phys(2,indxHorXAdvFlux+iflux-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidHorXAdvFlux(iflux), 
     &          'long_name', 
     &          lvar, text(1:lvar))
#  endif
           lvar=lenstr(vname_phys(3,indxHorXAdvFlux+iflux-1))
           ierr=nf_put_att_text(ncid, vidHorXAdvFlux(iflux),     
     &          'units', lvar, 
     &          vname_phys(3,indxHorXAdvFlux+iflux-1)(1:lvar))
           lvar=lenstr(vname_phys(4,indxHorXAdvFlux+iflux-1))
!           ierr=nf_put_att_text(ncid, vidHorXAdvFlux(iflux),   
!     &             'field', lvar, 
!     &          vname_phys(4,indxHorXAdvFlux+iflux-1)(1:lvar))
#ifdef MASK_LAND_DATA
          ierr=nf_put_att_FOUT (ncid, vidHorXAdvFlux(iflux),
     &          '_FillValue', NF_FOUT, 1, spv_set)
#endif
         end if
        end do
! horizontal advective fluxes (eta-direction)
        do iflux = 1, NT_PFA
         if (wrtavg(indxT+iflux-1)) then
           lvar=lenstr(vname_phys(1,indxHorYAdvFlux+iflux-1))
           ierr=nf_def_var (ncid, 
     &          vname_phys(1,indxHorYAdvFlux+iflux-1)(1:lvar),
     &          NF_FOUT, 4, r3dgrd, vidHorYAdvFlux(iflux))
#  ifndef AVRH
           lvar=lenstr(vname_phys(2,indxHorYAdvFlux+iflux-1))
           ierr=nf_put_att_text (ncid, vidHorYAdvFlux(iflux), 
     &          'long_name', 
     &          lvar, vname_phys(2,indxHorYAdvFlux+iflux-1)(1:lvar))
#  else
           text='averaged '/ /vname_phys(2,indxHorYAdvFlux+iflux-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidHorYAdvFlux(iflux), 
     &          'long_name', 
     &          lvar, text(1:lvar))
#  endif
           lvar=lenstr(vname_phys(3,indxHorYAdvFlux+iflux-1))
           ierr=nf_put_att_text(ncid, vidHorYAdvFlux(iflux),     
     &          'units', lvar, 
     &          vname_phys(3,indxHorYAdvFlux+iflux-1)(1:lvar))
           lvar=lenstr(vname_phys(4,indxHorYAdvFlux+iflux-1))
!           ierr=nf_put_att_text (ncid, vidHorYAdvFlux(iflux),     
!     &          'field', lvar, 
!     &          vname_phys(4,indxHorYAdvFlux+iflux-1)(1:lvar))
#ifdef MASK_LAND_DATA
          ierr=nf_put_att_FOUT (ncid, vidHorYAdvFlux(iflux),
     &          '_FillValue', NF_FOUT, 1, spv_set)
#endif
         end if
        end do
! vertical advective fluxes
        do iflux = 1, NT_PFA
         if (wrtavg(indxT+iflux-1)) then
           lvar=lenstr(vname_phys(1,indxVertAdvFlux+iflux-1))
           ierr=nf_def_var (ncid, 
     &          vname_phys(1,indxVertAdvFlux+iflux-1)(1:lvar),
     &          NF_FOUT, 4, w3dgrd, vidVertAdvFlux(iflux))
#  ifndef AVRH
           lvar=lenstr(vname_phys(2,indxVertAdvFlux+iflux-1))
           ierr=nf_put_att_text (ncid, vidVertAdvFlux(iflux), 
     &          'long_name', 
     &          lvar, vname_phys(2,indxVertAdvFlux+iflux-1)(1:lvar))
#  else
           text='averaged '/ /vname_phys(2,indxVertAdvFlux+iflux-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidVertAdvFlux(iflux), 
     &          'long_name', 
     &          lvar, text(1:lvar))
#  endif
           lvar=lenstr(vname_phys(3,indxVertAdvFlux+iflux-1))
           ierr=nf_put_att_text (ncid, vidVertAdvFlux(iflux),     
     &          'units', lvar, 
     &          vname_phys(3,indxVertAdvFlux+iflux-1)(1:lvar))
           lvar=lenstr(vname_phys(4,indxVertAdvFlux+iflux-1))
!           ierr=nf_put_att_text (ncid, vidVertAdvFlux(iflux),     
!     &          'field', lvar, 
!     &          vname_phys(4,indxVertAdvFlux+iflux-1)(1:lvar))
#ifdef MASK_LAND_DATA
          ierr=nf_put_att_FOUT (ncid, vidVertAdvFlux(iflux),
     &          '_FillValue', NF_FOUT, 1, spv_set)
#endif
         end if
        end do
#ifdef VERT_DIFF_ANALYSIS
! vertical diffusive fluxes
        do iflux = 1, NT_PFA
         if (wrtavg(indxT+iflux-1)) then
           lvar=lenstr(vname_phys(1,indxVertDiffFlux+iflux-1))
           ierr=nf_def_var (ncid, 
     &          vname_phys(1,indxVertDiffFlux+iflux-1)(1:lvar),
     &          NF_FOUT, 4, w3dgrd, vidVertDiffFlux(iflux))
#  ifndef AVRH
           lvar=lenstr(vname_phys(2,indxVertDiffFlux+iflux-1))
           ierr=nf_put_att_text (ncid, vidVertDiffFlux(iflux), 
     &          'long_name', 
     &          lvar, vname_phys(2,indxVertDiffFlux+iflux-1)(1:lvar))
#  else
           text='averaged '/ /vname_phys(2,indxVertDiffFlux+iflux-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidVertDiffFlux(iflux), 
     &          'long_name', 
     &          lvar, text(1:lvar))
#  endif
           lvar=lenstr(vname_phys(3,indxVertDiffFlux+iflux-1))
           ierr=nf_put_att_text(ncid, vidVertDiffFlux(iflux),'units',
     &          lvar, vname_phys(3,indxVertDiffFlux+iflux-1)(1:lvar))
           lvar=lenstr(vname_phys(4,indxVertDiffFlux+iflux-1))
!           ierr=nf_put_att_text(ncid, vidVertDiffFlux(iflux),'field',
!     &          lvar, vname_phys(4,indxVertDiffFlux+iflux-1)(1:lvar))
#ifdef MASK_LAND_DATA
          ierr=nf_put_att_FOUT (ncid, vidVertDiffFlux(iflux),
     &          '_FillValue', NF_FOUT, 1, spv_set)
#endif
         end if
        end do
#endif /* VERT_DIFF_ANALYSIS */
# ifdef FULL_PHYS_FLUX_ANALYSIS
! top fluxes
        do iflux = 1, NT_PFA
         if (wrtavg(indxT+iflux-1)) then
           lvar=lenstr(vname_phys(1,indxTopFlux+iflux-1))
           ierr=nf_def_var (ncid, 
     &          vname_phys(1,indxTopFlux+iflux-1)(1:lvar),
     &          NF_FOUT, 3, r2dgrd, vidTopFlux(iflux))
#  ifndef AVRH
           lvar=lenstr(vname_phys(2,indxTopFlux+iflux-1))
           ierr=nf_put_att_text (ncid, vidTopFlux(iflux), 
     &          'long_name', 
     &          lvar, vname_phys(2,indxTopFlux+iflux-1)(1:lvar))
#  else
           text='averaged '/ /vname_phys(2,indxTopFlux+iflux-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidTopFlux(iflux), 
     &          'long_name', 
     &          lvar, text(1:lvar))
#  endif
           lvar=lenstr(vname_phys(3,indxTopFlux+iflux-1))
           ierr=nf_put_att_text (ncid, vidTopFlux(iflux),'units',     
     &          lvar, vname_phys(3,indxTopFlux+iflux-1)(1:lvar))
           lvar=lenstr(vname_phys(4,indxTopFlux+iflux-1))
!           ierr=nf_put_att_text (ncid, vidTopFlux(iflux),'field',     
!     &          lvar, vname_phys(4,indxTopFlux+iflux-1)(1:lvar))
#ifdef MASK_LAND_DATA
          ierr=nf_put_att_FOUT (ncid, vidTopFlux(iflux),
     &          '_FillValue', NF_FOUT, 1, spv_set)
#endif
         end if
        end do
! shortwave radiation absorption fluxes !AH15
        if (wrtavg(indxT+itemp-1)) then
           lvar=lenstr(vname_phys(1,indxSRAbsFlux))
           ierr=nf_def_var (ncid, 
     &          vname_phys(1,indxSRAbsFlux)(1:lvar),
     &          NF_FOUT, 4, r3dgrd, vidSRAbsFlux)
#  ifndef AVRH
           lvar=lenstr(vname_phys(2,indxSRAbsFlux))
           ierr=nf_put_att_text (ncid, vidSRAbsFlux, 
     &          'long_name', 
     &          lvar, vname_phys(2,indxSRAbsFlux)(1:lvar))
#  else
           text='averaged '/ /vname_phys(2,indxSRAbsFlux)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidSRAbsFlux, 
     &          'long_name', 
     &          lvar, text(1:lvar))
#  endif
           lvar=lenstr(vname_phys(3,indxSRAbsFlux))
           ierr=nf_put_att_text (ncid, vidSRAbsFlux,     
     &          'units', lvar, 
     &          vname_phys(3,indxSRAbsFlux)(1:lvar))
           lvar=lenstr(vname_phys(4,indxSRAbsFlux))
!           ierr=nf_put_att_text (ncid, vidSRAbsFlux,     
!     &          'field', lvar, 
!     &          vname_phys(4,indxSRAbsFlux)(1:lvar))
#ifdef MASK_LAND_DATA
          ierr=nf_put_att_FOUT (ncid, vidSRAbsFlux,
     &          '_FillValue', NF_FOUT, 1, spv_set)
#endif
        end if
#if !defined SELECTED_FLUX_ANALYSIS /* AH */
! bottom fluxes
        do iflux = 1, NT_PFA
         if (wrtavg(indxT+iflux-1)) then
           lvar=lenstr(vname_phys(1,indxBottomFlux+iflux-1))
           ierr=nf_def_var (ncid, 
     &          vname_phys(1,indxBottomFlux+iflux-1)(1:lvar),
     &          NF_FOUT, 3, r2dgrd, vidBottomFlux(iflux))
#  ifndef AVRH
           lvar=lenstr(vname_phys(2,indxBottomFlux+iflux-1))
           ierr=nf_put_att_text (ncid, vidBottomFlux(iflux), 
     &          'long_name', 
     &          lvar, vname_phys(2,indxBottomFlux+iflux-1)(1:lvar))
#  else
           text='averaged '/ /vname_phys(2,indxBottomFlux+iflux-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidBottomFlux(iflux), 
     &          'long_name', 
     &          lvar, text(1:lvar))
#  endif
           lvar=lenstr(vname_phys(3,indxBottomFlux+iflux-1))
           ierr=nf_put_att_text (ncid, vidBottomFlux(iflux),'units',
     &          lvar, vname_phys(3,indxBottomFlux+iflux-1)(1:lvar))
           lvar=lenstr(vname_phys(4,indxBottomFlux+iflux-1))
!           ierr=nf_put_att_text (ncid, vidBottomFlux(iflux),'field',
!     &          lvar, vname_phys(4,indxBottomFlux+iflux-1)(1:lvar))
#ifdef MASK_LAND_DATA
          ierr=nf_put_att_FOUT (ncid, vidBottomFlux(iflux),
     &          '_FillValue', NF_FOUT, 1, spv_set)
#endif
         end if
        end do
! horizontal mixing fluxes (xi-direction)
        do iflux = 1, NT_PFA
         if (wrtavg(indxT+iflux-1)) then
           lvar=lenstr(vname_phys(1,indxHorXMixFlux+iflux-1))
           ierr=nf_def_var (ncid, 
     &          vname_phys(1,indxHorXMixFlux+iflux-1)(1:lvar),
     &          NF_FOUT, 4, r3dgrd, vidHorXMixFlux(iflux))
#  ifndef AVRH
           lvar=lenstr(vname_phys(2,indxHorXMixFlux+iflux-1))
           ierr=nf_put_att_text (ncid, vidHorXMixFlux(iflux), 
     &          'long_name', 
     &          lvar, vname_phys(2,indxHorXMixFlux+iflux-1)(1:lvar))
#  else
           text='averaged '/ /vname_phys(2,indxHorXMixFlux+iflux-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidHorXMixFlux(iflux), 
     &          'long_name', 
     &          lvar, text(1:lvar))
#  endif
           lvar=lenstr(vname_phys(3,indxHorXMixFlux+iflux-1))
           ierr=nf_put_att_text(ncid, vidHorXMixFlux(iflux),     
     &          'units', lvar, 
     &          vname_phys(3,indxHorXMixFlux+iflux-1)(1:lvar))
           lvar=lenstr(vname_phys(4,indxHorXMixFlux+iflux-1))
!           ierr=nf_put_att_text(ncid, vidHorXMixFlux(iflux),   
!     &             'field', lvar, 
!     &          vname_phys(4,indxHorXMixFlux+iflux-1)(1:lvar))
#ifdef MASK_LAND_DATA
          ierr=nf_put_att_FOUT (ncid, vidHorXMixFlux(iflux),
     &          '_FillValue', NF_FOUT, 1, spv_set)
#endif
         end if
        end do
! horizontal mixing fluxes (eta-direction)
        do iflux = 1, NT_PFA
         if (wrtavg(indxT+iflux-1)) then
           lvar=lenstr(vname_phys(1,indxHorYMixFlux+iflux-1))
           ierr=nf_def_var (ncid, 
     &          vname_phys(1,indxHorYMixFlux+iflux-1)(1:lvar),
     &          NF_FOUT, 4, r3dgrd, vidHorYMixFlux(iflux))
#  ifndef AVRH
           lvar=lenstr(vname_phys(2,indxHorYMixFlux+iflux-1))
           ierr=nf_put_att_text (ncid, vidHorYMixFlux(iflux), 
     &          'long_name', 
     &          lvar, vname_phys(2,indxHorYMixFlux+iflux-1)(1:lvar))
#  else
           text='averaged '/ /vname_phys(2,indxHorYMixFlux+iflux-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidHorYMixFlux(iflux), 
     &          'long_name', 
     &          lvar, text(1:lvar))
#  endif
           lvar=lenstr(vname_phys(3,indxHorYMixFlux+iflux-1))
           ierr=nf_put_att_text(ncid, vidHorYMixFlux(iflux),     
     &          'units', lvar, 
     &          vname_phys(3,indxHorYMixFlux+iflux-1)(1:lvar))
           lvar=lenstr(vname_phys(4,indxHorYMixFlux+iflux-1))
!           ierr=nf_put_att_text (ncid, vidHorYMixFlux(iflux),     
!     &          'field', lvar, 
!     &          vname_phys(4,indxHorYMixFlux+iflux-1)(1:lvar))
#ifdef MASK_LAND_DATA
          ierr=nf_put_att_FOUT (ncid, vidHorYMixFlux(iflux),
     &          '_FillValue', NF_FOUT, 1, spv_set)
#endif
         end if
        end do
! vertical mixing fluxes
        do iflux = 1, NT_PFA
         if (wrtavg(indxT+iflux-1)) then
           lvar=lenstr(vname_phys(1,indxVertMixFlux+iflux-1))
           ierr=nf_def_var (ncid, 
     &          vname_phys(1,indxVertMixFlux+iflux-1)(1:lvar),
     &          NF_FOUT, 4, w3dgrd, vidVertMixFlux(iflux))
#  ifndef AVRH
           lvar=lenstr(vname_phys(2,indxVertMixFlux+iflux-1))
           ierr=nf_put_att_text (ncid, vidVertMixFlux(iflux), 
     &          'long_name', 
     &          lvar, vname_phys(2,indxVertMixFlux+iflux-1)(1:lvar))
#  else
           text='averaged '/ /vname_phys(2,indxVertMixFlux+iflux-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidVertMixFlux(iflux), 
     &          'long_name', 
     &          lvar, text(1:lvar))
#  endif
           lvar=lenstr(vname_phys(3,indxVertMixFlux+iflux-1))
           ierr=nf_put_att_text (ncid, vidVertMixFlux(iflux),     
     &          'units', lvar, 
     &          vname_phys(3,indxVertMixFlux+iflux-1)(1:lvar))
           lvar=lenstr(vname_phys(4,indxVertMixFlux+iflux-1))
!           ierr=nf_put_att_text (ncid, vidVertMixFlux(iflux),     
!     &          'field', lvar, 
!     &          vname_phys(4,indxVertMixFlux+iflux-1)(1:lvar))
#ifdef MASK_LAND_DATA
          ierr=nf_put_att_FOUT (ncid, vidVertMixFlux(iflux),
     &          '_FillValue', NF_FOUT, 1, spv_set)
#endif
         end if
        end do
! nudging fluxes
        do iflux = 1, NT_PFA
         if (wrtavg(indxT+iflux-1)) then
           lvar=lenstr(vname_phys(1,indxNudgingFlux+iflux-1))
           ierr=nf_def_var (ncid, 
     &          vname_phys(1,indxNudgingFlux+iflux-1)(1:lvar),
     &          NF_FOUT, 4, r3dgrd, vidNudgingFlux(iflux))
#  ifndef AVRH
           lvar=lenstr(vname_phys(2,indxNudgingFlux+iflux-1))
           ierr=nf_put_att_text (ncid, vidNudgingFlux(iflux), 
     &          'long_name', 
     &          lvar, vname_phys(2,indxNudgingFlux+iflux-1)(1:lvar))
#  else
           text='averaged '/ /vname_phys(2,indxNudgingFlux+iflux-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidNudgingFlux(iflux), 
     &          'long_name', 
     &          lvar, text(1:lvar))
#  endif
           lvar=lenstr(vname_phys(3,indxNudgingFlux+iflux-1))
           ierr=nf_put_att_text (ncid, vidNudgingFlux(iflux),     
     &          'units', lvar, 
     &          vname_phys(3,indxNudgingFlux+iflux-1)(1:lvar))
           lvar=lenstr(vname_phys(4,indxNudgingFlux+iflux-1))
!           ierr=nf_put_att_text (ncid, vidNudgingFlux(iflux),     
!     &          'field', lvar, 
!     &          vname_phys(4,indxNudgingFlux+iflux-1)(1:lvar))
#ifdef MASK_LAND_DATA
          ierr=nf_put_att_FOUT (ncid, vidNudgingFlux(iflux),
     &          '_FillValue', NF_FOUT, 1, spv_set)
#endif
         end if
        end do
#endif /* SELECTED_FLUX_ANALYSIS */
# endif /* FULL_PHYS_FLUX_ANALYSIS */
!
! Leave definition mode.
! ----- ---------- -----
!
        ierr=nf_enddef(ncid)
        MPI_master_only write(*,'(6x,4A,1x,A,i4)')
#ifdef AVRH
     &       'DEF_PHYS_FLUX_AVG - Created ',
#else
     &       'DEF_PHYS_FLUX_HIS - Created ',
#endif /* AVRH */
     &                'new netCDF file ''', 
     &       ncname(1:lstr), '''.'
     &                 MYID
!
! Open an existing file and prepare for appending data.
! ==== == ======== ==== === ======= === ========= =====
! Inquire about the dimensions and variables. Check for
! consistency with model dimensions. In the case when file 
! is rejected (whether it cannot be opened, or something
! is wrong with its dimensions) create a new file.
!
! After that verify that all necessary variables are already
! defined there and find their netCDF IDs.
!
      elseif (ncid.eq.-1) then create_new_file_if
        ierr=nf_open (ncname(1:lstr), nf_write, ncid)
        if (ierr. eq. nf_noerr) then
          MPI_master_only write(*,'(1x,2A,1x,A,I3)') 
     &          'Opened existing phys_flux_his file ',
     &          ncname(1:lstr), 'ncid =', ncid
          ierr=checkdims (ncid, ncname, rec)
          if (ierr .eq. nf_noerr) then
            if (rec_per_file.eq.0) then
              ierr=rec+1 - total_rec
            else
              ierr=rec+1 - (1+mod(total_rec-1, rec_per_file))
            endif
            if (ierr.gt.0) then
              MPI_master_only write( *,
     &                 '(/1x,A,I5,1x,A/8x,3A,I5,/8x,A,I5,1x,A/)')
#ifdef AVRH
     &        'DEF_PHYS_FLUX_AVG WARNING: Actual number of records',
#else
     &        'DEF_PHYS_FLUX_HIS WARNING: Actual number of records',
#endif /* AVRH */
     &               rec,  'in netCDF file',  '''',  
     &              ncname(1:lstr),
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
            write(*,'(/1x,4A,2x,A,I4/)')
#ifdef AVRH
     &            'DEF_PHYS_FLUX_AVG ERROR: ',
#else
     &            'DEF_PHYS_FLUX_HIS ERROR: ',
#endif /* AVRH */
     &                  'Cannot open file ''', 
     &            ncname(1:lstr), '''.'
     &                   MYID
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
          write(*,1) 'time_step', ncname(1:lstr)
          goto 99                                         !--> ERROR
        endif
!
! Time.
!
        lvar=lenstr(vname(1,indxTime))
        ierr=nf_inq_varid (ncid,vname(1,indxTime)(1:lvar),
     &       vidTime)
        if (ierr .ne. nf_noerr) then
          write(*,1) vname(1,indxTime)(1:lvar), 
     &          ncname(1:lstr)
          goto 99                                         !--> ERROR 
        endif
! Free surface
        lvar=lenstr(vname(1,indxZ))
        ierr=nf_inq_varid (ncid,vname(1,indxZ)(1:lvar),
     &       vidZ)
        if (ierr .ne. nf_noerr) then
          write(*,1) vname(1,indxZ)(1:lvar), 
     &          ncname(1:lstr)
          goto 99                                         !--> ERROR 
        endif
!
! Fluxes of tracer variables
!
! horizontal advective fluxes (xi-direction)
        do iflux = 1, NT_PFA
         if (wrtavg(indxT+iflux-1)) then
           lvar=lenstr(vname_phys(1,indxHorXAdvFlux+iflux-1))
           ierr=nf_inq_varid(ncid, 
     &          vname_phys(1,indxHorXAdvFlux+iflux-1)(1:lvar),
     &          vidHorXAdvFlux(iflux))
           if (ierr .ne. nf_noerr) then
              write(*,1) 
     &             vname_phys(1,indxHorXAdvFlux+iflux-1)(1:lvar), 
     &             ncname(1:lstr)
              goto 99           !--> ERROR 
           endif
         end if
        end do

! horizontal advective fluxes (eta-direction)
        do iflux = 1, NT_PFA
         if (wrtavg(indxT+iflux-1)) then
           lvar=lenstr(vname_phys(1,indxHorYAdvFlux+iflux-1))
           ierr=nf_inq_varid(ncid, 
     &          vname_phys(1,indxHorYAdvFlux+iflux-1)(1:lvar),
     &          vidHorYAdvFlux(iflux))
           if (ierr .ne. nf_noerr) then
              write(*,1) 
     &             vname_phys(1,indxHorYAdvFlux+iflux-1)(1:lvar), 
     &             ncname(1:lstr)
              goto 99           !--> ERROR 
           endif
         end if
        end do

! vertical advective fluxes
        do iflux = 1, NT_PFA
         if (wrtavg(indxT+iflux-1)) then
           lvar=lenstr(vname_phys(1,indxVertAdvFlux+iflux-1))
           ierr=nf_inq_varid(ncid, 
     &          vname_phys(1,indxVertAdvFlux+iflux-1)(1:lvar),
     &          vidVertAdvFlux(iflux))
           if (ierr .ne. nf_noerr) then
              write(*,1) 
     &             vname_phys(1,indxVertAdvFlux+iflux-1)(1:lvar), 
     &             ncname(1:lstr)
              goto 99           !--> ERROR 
           endif
         end if
        end do

#ifdef VERT_DIFF_ANALYSIS
! vertical diffusive fluxes
        do iflux = 1, NT_PFA
         if (wrtavg(indxT+iflux-1)) then
           lvar=lenstr(vname_phys(1,indxVertDiffFlux+iflux-1))
           ierr=nf_inq_varid(ncid, 
     &          vname_phys(1,indxVertDiffFlux+iflux-1)(1:lvar),
     &          vidVertDiffFlux(iflux))
           if (ierr .ne. nf_noerr) then
              write(*,1) 
     &             vname_phys(1,indxVertDiffFlux+iflux-1)(1:lvar), 
     &             ncname(1:lstr)
              goto 99           !--> ERROR 
           endif
         end if
        end do
#endif /* VERT_DIFF_ANALYSIS */

#ifdef FULL_PHYS_FLUX_ANALYSIS
! top fluxes
        do iflux = 1, NT_PFA
         if (wrtavg(indxT+iflux-1)) then
           lvar=lenstr(vname_phys(1,indxTopFlux+iflux-1))
           ierr=nf_inq_varid(ncid, 
     &          vname_phys(1,indxTopFlux+iflux-1)(1:lvar),
     &          vidTopFlux(iflux))
           if (ierr .ne. nf_noerr) then
              write(*,1) 
     &             vname_phys(1,indxTopFlux+iflux-1)(1:lvar), 
     &             ncname(1:lstr)
              goto 99           !--> ERROR 
           endif
         end if
        end do
! shortwave radiation absorption fluxes !AH15
        if (wrtavg(indxT+itemp-1)) then
           lvar=lenstr(vname_phys(1,indxSRAbsFlux))
           ierr=nf_inq_varid(ncid, 
     &          vname_phys(1,indxSRAbsFlux)(1:lvar),
     &          vidSRAbsFlux)
           if (ierr .ne. nf_noerr) then
              write(*,1) 
     &             vname_phys(1,indxSRAbsFlux)(1:lvar), 
     &             ncname(1:lstr)
              goto 99           !--> ERROR 
           endif
        end if
#if !defined SELECTED_FLUX_ANALYSIS /* AH */
! bottom fluxes
        do iflux = 1, NT_PFA
         if (wrtavg(indxT+iflux-1)) then
           lvar=lenstr(vname_phys(1,indxBottomFlux+iflux-1))
           ierr=nf_inq_varid(ncid, 
     &          vname_phys(1,indxBottomFlux+iflux-1)(1:lvar),
     &          vidBottomFlux(iflux))
           if (ierr .ne. nf_noerr) then
              write(*,1) 
     &             vname_phys(1,indxBottomFlux+iflux-1)(1:lvar), 
     &             ncname(1:lstr)
              goto 99           !--> ERROR 
           endif
         end if
        end do
! horizontal mixing fluxes (xi-direction)
        do iflux = 1, NT_PFA
         if (wrtavg(indxT+iflux-1)) then
           lvar=lenstr(vname_phys(1,indxHorXMixFlux+iflux-1))
           ierr=nf_inq_varid(ncid, 
     &          vname_phys(1,indxHorXMixFlux+iflux-1)(1:lvar),
     &          vidHorXMixFlux(iflux))
           if (ierr .ne. nf_noerr) then
              write(*,1) 
     &             vname_phys(1,indxHorXMixFlux+iflux-1)(1:lvar), 
     &             ncname(1:lstr)
              goto 99           !--> ERROR 
           endif
         end if
        end do
! horizontal mixing fluxes (eta-direction)
        do iflux = 1, NT_PFA
         if (wrtavg(indxT+iflux-1)) then
           lvar=lenstr(vname_phys(1,indxHorYMixFlux+iflux-1))
           ierr=nf_inq_varid(ncid, 
     &          vname_phys(1,indxHorYMixFlux+iflux-1)(1:lvar),
     &          vidHorYMixFlux(iflux))
           if (ierr .ne. nf_noerr) then
              write(*,1) 
     &             vname_phys(1,indxHorYMixFlux+iflux-1)(1:lvar), 
     &             ncname(1:lstr)
              goto 99           !--> ERROR 
           endif
         end if
        end do
! vertical mixing fluxes
        do iflux = 1, NT_PFA
         if (wrtavg(indxT+iflux-1)) then
           lvar=lenstr(vname_phys(1,indxVertMixFlux+iflux-1))
           ierr=nf_inq_varid(ncid, 
     &          vname_phys(1,indxVertMixFlux+iflux-1)(1:lvar),
     &          vidVertMixFlux(iflux))
           if (ierr .ne. nf_noerr) then
              write(*,1) 
     &             vname_phys(1,indxVertMixFlux+iflux-1)(1:lvar), 
     &             ncname(1:lstr)
              goto 99           !--> ERROR 
           endif
         end if
        end do
! nudging fluxes
        do iflux = 1, NT_PFA
         if (wrtavg(indxT+iflux-1)) then
           lvar=lenstr(vname_phys(1,indxNudgingFlux+iflux-1))
           ierr=nf_inq_varid(ncid, 
     &          vname_phys(1,indxNudgingFlux+iflux-1)(1:lvar),
     &          vidNudgingFlux(iflux))
           if (ierr .ne. nf_noerr) then
              write(*,1) 
     &             vname_phys(1,indxNudgingFlux+iflux-1)(1:lvar), 
     &             ncname(1:lstr)
              goto 99           !--> ERROR 
           endif
         end if
        end do
#endif /* SELECTED_FLUX_ANALYSIS */
#endif /* FULL_PHYS_FLUX_ANALYSIS */

        MPI_master_only write(*,'(6x,2A,i4,1x,A,i4)')
#ifdef AVRH
     &       'DEF_PHYS_FLUX_AVG -- Opened ',
#else
     &       'DEF_PHYS_FLUX_HIS -- Opened ',
#endif /* AVRH */
     &                     'existing file  from record =', rec
     &                      MYID

#if defined MPI && !defined PARALLEL_FILES
      else
        ierr=nf_open (ncname(1:lstr), nf_write, ncid)
        if (ierr .ne. nf_noerr) then
#ifdef AVRH
          write(*,'(/1x,4A,2x,A,I4/)') 'DEF_PHYS_FLUX_AVG ERROR: ',
#else
          write(*,'(/1x,4A,2x,A,I4/)') 'DEF_PHYS_FLUX_HIS ERROR: ',
#endif /* AVRH */
     &                'Cannot open file ''', ncname(1:lstr), '''.'
     &                 MYID
          goto 99                                         !--> ERROR
        endif
#endif
      end if create_new_file_if
      
      ierr=nf_set_fill (ncid, nf_nofill, lvar)
      if (ierr .ne. nf_noerr) then
        write(*,'(6x,2A,i4,1x,A,i4)') 
#ifdef AVRH
     &        'DEF_PHYS_FLUX_AVG ERROR: Cannot ', 
#else
     &        'DEF_PHYS_FLUX_HIS ERROR: Cannot ', 
#endif /* AVRH */
     &    'switch to ''nf_nofill'' more; netCDF error code =', ierr 
      endif
#ifdef AVRH
   1  format(/1x,'DEF_PHYS_FLUX_AVG ERROR: Cannot find variable ''',
#else
   1  format(/1x,'DEF_PHYS_FLUX_HIS ERROR: Cannot find variable ''',
#endif /* AVRH */
     &                   A, ''' in netCDF file ''', A, '''.'/)
#if (defined PUT_GRID_INTO_HISTORY && !defined AVRH)\
 || (defined PUT_GRID_INTO_AVERAGES && defined AVRH)
!
! Write grid variables.
! ----- ---- ----------
!
        if (total_rec.le.1) call wrt_grid (ncid, 
     &      ncname, lstr)
#endif
  99  return
      end

#undef ncname
#undef rec_per_file
#undef vidTime
#undef vidTstep
#undef vidZ
#undef vidHorXAdvFlux
#undef vidHorYAdvFlux
#undef vidVertAdvFlux
#undef vidVertDiffFlux
# ifdef FULL_PHYS_FLUX_ANALYSIS
#undef vidTopFlux
#undef vidSRAbsFlux
#if !defined SELECTED_FLUX_ANALYSIS /* AH */
#undef vidBottomFlux
#undef vidHorXMixFlux
#undef vidHorYMixFlux
#undef vidVertMixFlux
#undef vidNudgingFlux
#endif /* SELECTED_FLUX_ANALYSIS */
# endif /* FULL_PHYS_FLUX_ANALYSIS */

# ifndef AVRH
#  ifdef AVERAGES
#   define AVRH
#   include "def_phys_flux.F"
#  endif /* AVERAGES */
#  endif
#else /* PHYS_FLUX_ANALYSIS */
      subroutine def_phys_flux_empty()
      return
      end
#endif /* PHYS_FLUX_ANALYSIS */
