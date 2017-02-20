!   $Log: def_bgc_flux.F $
!   Revision 1.2  2002/04/19 00:45:49  frenzel
!   Added code for biogeochemical flux history files.
!
!   Revision 1.1  2002/01/16 19:58:53  frenzel
!   New routines related to flux analysis.
!
!
#ifndef AVRH
# include "cppdefs.h"
#endif
#if defined BIOLOGY_NPZDOC && defined BGC_FLUX_ANALYSIS
#ifdef AVRH
      subroutine def_bgc_flux_avg (ncid, total_rec, ierr)
#else
      subroutine def_bgc_flux_his (ncid, total_rec, ierr)
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
      integer iflux
      character*80 text
#endif /* SOLVE3D */
#include "netcdf.inc"
#include "param.h"
#include "scalars.h"
#include "ncvars.h"
#include "biopar.h"
#include "bgcflux.h"
#include "ncroms_bgcflux.h"

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
# define ncname bgc_flux_his_name 
# define rec_per_file nrpf_bgc_flux_his
# define vidTime bgc_flux_hisTime
# define vidTstep bgc_flux_hisTstep
# define vidZ bgc_flux_hisZ
# define vidU10 hisU10
# define vidKvO2 hisKvO2
# define vidO2sat hisO2sat
# define vidKvCO2 hisKvCO2
# define vidCO2sol hisCO2sol
!# define vidPCO2 hisPCO2
!# define vidPCO2air hisPCO2air
!# define vidPH hisPH
# define vidPAR hisPAR_flux
# define vidPARinc hisPARinc_flux
# define vidFlux hisFlux
!# define vidGasExcFlux hisGasExcFlux
# define vidVSinkFlux hisVSinkFlux
# define vidSedFlux hisSedFlux
# define vidONNO3 hisONNO3
# define vidONNH4 hisONNH4
# define vidCNP hisCNP
# define vidCNZ hisCNZ
# define vidrCaCO3orgC hisrCaCO3orgC
# define vidNCDET hisNCDET
#else /* AVRH */
# define ncname bgc_flux_avg_name
# define rec_per_file nrpf_bgc_flux_avg
# define vidTime bgc_flux_avgTime
# define vidTstep bgc_flux_avgTstep
# define vidZ bgc_flux_avgZ
# define vidU10 avgU10
# define vidKvO2 avgKvO2
# define vidO2sat avgO2sat
# define vidKvCO2 avgKvCO2
# define vidCO2sol avgCO2sol
!# define vidPCO2 avgPCO2
!# define vidPCO2air avgPCO2air
!# define vidPH avgPH
# define vidPAR avgPAR_flux
# define vidPARinc avgPARinc_flux
# define vidFlux avgFlux
!# define vidGasExcFlux avgGasExcFlux
# define vidVSinkFlux avgVSinkFlux
# define vidSedFlux avgSedFlux
# define vidONNO3 avgONNO3
# define vidONNH4 avgONNH4
# define vidCNP avgCNP
# define vidCNZ avgCNZ
# define vidrCaCO3orgC avgrCaCO3orgC
# define vidNCDET avgNCDET
#endif /* AVRH */

! This check is not performed in def_his.F, but very useful:
! If the output frequency is not a positive number, do nothing at all
#ifdef AVRH
      if (n_bgc_flux_avg < 1) return
#else
      if (n_bgc_flux_his < 1) return
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
! if new_bgc_flux_avg=.true., it forces creation of a new file [if the
! file already exists, it will be overwritten]; on the other hand,
! new_bgc_flux_avg=.false., it is assumed that the file already exists and
! an attempt to open it is made; if the attempt is successful, the
! file is prepared for appending new data; if it fails, a new file
! is created.
!
#ifdef AVRH
      create_new_file = new_bgc_flux_avg
#else
      create_new_file = new_bgc_flux_his
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
#ifdef LARGE_BGC_FILES
        ierr=nf_create(ncname(1:lstr), or(nf_clobber,NCFILEFORMAT),
     &        ncid)
#else
        ierr=nf_create(ncname(1:lstr), nf_clobber, ncid)
#endif
        if (ierr .ne. nf_noerr) then
          write(stdout,'(/3(1x,A)/)') 'ERROR in def_bgc_flux_his/avg:',
     &          'Cannot create netCDF file:', 
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
! Put global attributes related to biogeochemical fluxes.
! --- ------ ---------- ------- -- -------------- -------
!
#ifdef OXYGEN
        text =
     &  'OGain_NewProd = NFlux_NewProd * ON_NO3'
        lvar = lenstr(text)
        ierr=nf_put_att_text(ncid,nf_global,'bgc_note1',lvar,text)
!
        text =
     &  'OGain_RegProd = NFlux_RegProd * ON_NH4'
        lvar = lenstr(text)
        ierr=nf_put_att_text(ncid,nf_global,'bgc_note2',lvar,text)
!
        text =
     &  'OLoss_Nitrif = 2. * NFlux_Nitrif'
        lvar = lenstr(text)
        ierr=nf_put_att_text(ncid,nf_global,'bgc_note3',lvar,text)
!
        text =
     &  'OLoss_Zmetab = NFlux_Zmetab * ON_NH4'
        lvar = lenstr(text)
        ierr=nf_put_att_text(ncid,nf_global,'bgc_note4',lvar,text)
!
        text =
     &  'OLoss_Zmetab = NFlux_Zmetab * ON_NH4'
        lvar = lenstr(text)
        ierr=nf_put_att_text(ncid,nf_global,'bgc_note5',lvar,text)
!
        text =
     &  'OLoss_Zresp = CFlux_Zresp'
        lvar = lenstr(text)
        ierr=nf_put_att_text(ncid,nf_global,'bgc_note6',lvar,text)
!
        text =
     &  'OLoss_ReminS = ON_NH4 * NFlux_ReminS'
        lvar = lenstr(text)
        ierr=nf_put_att_text(ncid,nf_global,'bgc_note7',lvar,text)
!
        text =
     &  'OLoss_ReminL = ON_NH4 * NFlux_ReminL'
        lvar = lenstr(text)
        ierr=nf_put_att_text(ncid,nf_global,'bgc_note8',lvar,text)
!
        text =
     &  'OLoss_ReminSed = ON_NH4 * NFlux_ReminSed'
        lvar = lenstr(text)
        ierr=nf_put_att_text(ncid,nf_global,'bgc_note9',lvar,text)
#endif /* OXYGEN */
!
#ifdef CARBON
        text =
     &  'CFlux_NewProd = NFlux_NewProd * CN_P'
        lvar = lenstr(text)
        ierr=nf_put_att_text(ncid,nf_global,'bgc_note10',lvar,text)
!
        text =
     &  'CFlux_RegProd =  NFlux_RegProd * CN_P'
        lvar = lenstr(text)
        ierr=nf_put_att_text(ncid,nf_global,'bgc_note11',lvar,text)
!
        text =
     &  'CFlux_Precip = (CFlux_NewProd + CFlux_RegProd) * r_CaCO3_orgC'
        lvar = lenstr(text)
        ierr=nf_put_att_text(ncid,nf_global,'bgc_note12',lvar,text)
!
        text =
     &  'CFlux_Grazing = NFlux_Grazing * CN_P'
        lvar = lenstr(text)
        ierr=nf_put_att_text(ncid,nf_global,'bgc_note13',lvar,text)
!
        text =
     &  'CFlux_SlopFeed = NFlux_SlopFeed * CN_P'
        lvar = lenstr(text)
        ierr=nf_put_att_text(ncid,nf_global,'bgc_note14',lvar,text)
!
        text =
     &  'CFlux_Zfecp = NFlux_Zfecp * CN_P'
        lvar = lenstr(text)
        ierr=nf_put_att_text(ncid,nf_global,'bgc_note15',lvar,text)
!
        text =
     &  'CFlux_Pmort = NFlux_Pmort * CN_P'
        lvar = lenstr(text)
        ierr=nf_put_att_text(ncid,nf_global,'bgc_note16',lvar,text)
!
        text =
     &  'CFlux_Zmetab = NFlux_Zmetab * CN_Z'
        lvar = lenstr(text)
        ierr=nf_put_att_text(ncid,nf_global,'bgc_note17',lvar,text)
!
        text =
     &  'CFlux_Zexcr = NFlux_Zexcr * CN_Z'
        lvar = lenstr(text)
        ierr=nf_put_att_text(ncid,nf_global,'bgc_note18',lvar,text)
!
        text =
     &  'CFlux_ZmortS = NFlux_ZmortS * CN_Z'
        lvar = lenstr(text)
        ierr=nf_put_att_text(ncid,nf_global,'bgc_note19',lvar,text)
!
        text =
     &  'CFlux_ZmortL = NFlux_ZmortL * CN_Z'
        lvar = lenstr(text)
        ierr=nf_put_att_text(ncid,nf_global,'bgc_note20',lvar,text)
!
        text =
     &  'CFlux_DetCoagP = NFlux_DetCoagP * CN_P'
        lvar = lenstr(text)
        ierr=nf_put_att_text(ncid,nf_global,'bgc_note21',lvar,text)
#endif /* CARBON */
#ifdef OXYLIM
        text = 
     &  'NFlux_DenitrS = N_CDET * CFlux_ReminS'
        lvar = lenstr(text)
        ierr=nf_put_att_text(ncid,nf_global,'bgc_note22',lvar,text)
!
        text = 
     &  'NFlux_DenitrL = N_CDET * CFlux_ReminL'
        lvar = lenstr(text)
        ierr=nf_put_att_text(ncid,nf_global,'bgc_note23',lvar,text)
#endif /* OXYLIM */
!

!
! Define dimensions of staggered fields.
! ------ ---------- -- --------- -------
!
! hf: define all dimensions even if they are not used - 
! program "partit" may still require them

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

!!        u2dgrd(2)=r2dgrd(2)         ! 2D UBAR-type
!!        u2dgrd(3)=timedim

!!        v2dgrd(1)=r2dgrd(1)         ! 2D VBAR-type
!!        v2dgrd(3)=timedim

#ifdef SOLVE3D
        r3dgrd(1)=r2dgrd(1)         !
        r3dgrd(2)=r2dgrd(2)         ! 3D RHO-type
        r3dgrd(4)=timedim           !

!!        u3dgrd(1)=u2dgrd(1)         !
!!        u3dgrd(2)=r2dgrd(2)         ! 3D U-type
!!        u3dgrd(3)=r3dgrd(3)         !
!!        u3dgrd(4)=timedim

!!        v3dgrd(1)=r2dgrd(1)         !
!!        v3dgrd(2)=v2dgrd(2)         ! 3D V-type
!!        v3dgrd(3)=r3dgrd(3)         !
!!        v3dgrd(4)=timedim

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
! Define variables related to biogeochemical fluxes.
! ------ --------- ------- -- -------------- -------
!
#ifdef OXYGEN
        ierr = nf_def_var(ncid, 'ON_NO3', NF_FOUT, 0, 0, vidONNO3)
        text =
     &  'Ratio of O flux to N flux for new production'
        lvar = lenstr(text)
        ierr = nf_put_att_text(ncid, vidONNO3, 'long_name', 
     &          lvar, text(1:lvar))
        if (ierr .ne. nf_noerr) then
          write(stdout,'(a,1x,i3)') 'Error while defining ON_NO3', ierr
          goto 99                                           !--> ERROR
        endif
!
        ierr = nf_def_var(ncid, 'ON_NH4', NF_FOUT, 0, 0, vidONNH4)
        text =
     &  'Ratio of O fluxes to N fluxes for reg. prod. and zoo metab.'
        lvar = lenstr(text)
        ierr = nf_put_att_text(ncid, vidONNH4, 'long_name', 
     &          lvar, text(1:lvar))
#endif /* OXYGEN */
!
#ifdef CARBON
        ierr = nf_def_var(ncid, 'CN_P', NF_FOUT, 0, 0, vidCNP)
        text =
     &  'Ratio of C fluxes to N fluxes regarding phytoplankton'
        lvar = lenstr(text)
        ierr = nf_put_att_text(ncid, vidCNP, 'long_name', 
     &          lvar, text(1:lvar))
!
        ierr = nf_def_var(ncid, 'CN_Z', NF_FOUT, 0, 0, vidCNZ)
        text =
     &  'Ratio of C fluxes to N fluxes regarding zooplankton'
        lvar = lenstr(text)
        ierr = nf_put_att_text(ncid, vidCNZ, 'long_name', 
     &          lvar, text(1:lvar))
!
        ierr = nf_def_var(ncid, 'r_CaCO3_orgC', NF_FOUT, 0, 0, 
     &       vidrCaCO3orgC)
        text =
     &  'Ratio of CaCO3 precipitation to total production'
        lvar = lenstr(text)
        ierr = nf_put_att_text(ncid, vidrCaCO3orgC, 'long_name', 
     &          lvar, text(1:lvar))
#endif /* CARBON */
#ifdef OXYLIM
        ierr = nf_def_var(ncid, 'N_CDET', NF_FOUT, 0, 0, vidNCDET)
        text =
     &  'Ratio of NO3 flux to DIC flux for denitrification'
        lvar = lenstr(text)
        ierr = nf_put_att_text(ncid, vidNCDET, 'long_name',
     &         lvar, text(1:lvar))
#endif /* OXYLIM */
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
! hf: always use double precision for the time variable
        ierr=nf_def_var (ncid, vname(1,indxTime)(1:lvar),
     &       NF_DOUBLE, 1, timedim, vidTime)
#ifndef AVRH
        lvar=lenstr(vname(2,indxTime))
        ierr=nf_put_att_text (ncid, vidTime, 'long_name', lvar,
     &       vname(2,indxTime)(1:lvar))
#else
        text='averaged '/ /vname(2,indxTime)
        lvar=lenstr(text)
        ierr=nf_put_att_text (ncid, vidTime, 'long_name', lvar,
     &       text(1:lvar))
#endif
        lvar=lenstr(vname(3,indxTime))
        ierr=nf_put_att_text (ncid, vidTime, 'units',  lvar,
     &       vname(3,indxTime)(1:lvar))
!        lvar=lenstr(vname(4,indxTime))
!        ierr=nf_put_att_text (ncid, vidTime, 'field',  lvar,
!     &       vname(4,indxTime)(1:lvar))

! Free surface
        lvar=lenstr(vname(1,indxZ))
        ierr=nf_def_var (ncid, vname(1,indxZ)(1:lvar),
     &       NF_FOUT, 3, r2dgrd, vidZ)
#ifndef AVRH
          lvar=lenstr(vname(2,indxZ))
          ierr=nf_put_att_text (ncid, vidZ, 'long_name', lvar,
     &         vname(2,indxZ)(1:lvar))
#else
          text='averaged '/ /vname(2,indxZ)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidZ, 'long_name', lvar,
     &         text(1:lvar))
#endif
        lvar=lenstr(vname(3,indxZ))
        ierr=nf_put_att_text (ncid, vidZ, 'units',     lvar,
     &       vname(3,indxZ)(1:lvar))
!        lvar=lenstr(vname(4,indxZ))
!        ierr=nf_put_att_text (ncid, vidZ, 'field',     lvar,
!     &       vname(4,indxZ)(1:lvar))
#ifdef MASK_LAND_DATA
        ierr=nf_put_att_FOUT (ncid, vidZ, '_FillValue',
     &       NF_FOUT, 1, spv_set)
#endif

#  ifdef OXYGEN
!
! Wind speed at 10 m
!
!        if (wrt(indxU10)) then
          lvar=lenstr(vname_bgcflux(1,indxU10))
          ierr=nf_def_var (ncid, vname_bgcflux(1,indxU10)(1:lvar),
     &                             NF_FOUT, 3, r2dgrd, vidU10)
#  ifndef AVRH
          lvar=lenstr(vname_bgcflux(2,indxU10))
          ierr=nf_put_att_text (ncid, vidU10, 'long_name', lvar,
     &         vname_bgcflux(2,indxU10)(1:lvar))
#  else
          text='averaged '/ /vname_bgcflux(2,indxU10)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidU10, 'long_name', lvar,
     &                                              text(1:lvar))
#  endif
          lvar=lenstr(vname_bgcflux(3,indxU10))
          ierr=nf_put_att_text (ncid, vidU10, 'units',     lvar,
     &         vname_bgcflux(3,indxU10)(1:lvar))
!          lvar=lenstr(vname_bgcflux(4,indxU10))
!          ierr=nf_put_att_text (ncid, vidU10, 'field',     lvar,
!     &         vname_bgcflux(4,indxU10)(1:lvar))
#ifdef MASK_LAND_DATA
          ierr=nf_put_att_FOUT (ncid, vidU10, '_FillValue',
     &       NF_FOUT, 1, spv_set)
#endif
!     endif
!
! Gas exchange coefficient of O2
!
!        if (wrt(indxKvO2)) then
          lvar=lenstr(vname_bgcflux(1,indxKvO2))
          ierr=nf_def_var (ncid, vname_bgcflux(1,indxKvO2)(1:lvar),
     &                             NF_FOUT, 3, r2dgrd, vidKvO2)
#  ifndef AVRH
          lvar=lenstr(vname_bgcflux(2,indxKvO2))
          ierr=nf_put_att_text (ncid, vidKvO2, 'long_name', lvar,
     &         vname_bgcflux(2,indxKvO2)(1:lvar))
#  else
          text='averaged '/ /vname_bgcflux(2,indxKvO2)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidKvO2, 'long_name', lvar,
     &                                              text(1:lvar))
#  endif
          lvar=lenstr(vname_bgcflux(3,indxKvO2))
          ierr=nf_put_att_text (ncid, vidKvO2, 'units',     lvar,
     &         vname_bgcflux(3,indxKvO2)(1:lvar))
!          lvar=lenstr(vname_bgcflux(4,indxKvO2))
!          ierr=nf_put_att_text (ncid, vidKvO2, 'field',     lvar,
!     &         vname_bgcflux(4,indxKvO2)(1:lvar))
#ifdef MASK_LAND_DATA
          ierr=nf_put_att_FOUT (ncid, vidKvO2, '_FillValue',
     &         NF_FOUT, 1, spv_set)
#endif
!        endif
!
! Saturation concentration of O2
!
!        if (wrt(indxO2sat)) then
          lvar=lenstr(vname_bgcflux(1,indxO2sat))
          ierr=nf_def_var (ncid, vname_bgcflux(1,indxO2sat)(1:lvar),
     &                             NF_FOUT, 3, r2dgrd, vidO2sat)
#  ifndef AVRH
          lvar=lenstr(vname_bgcflux(2,indxO2sat))
          ierr=nf_put_att_text (ncid, vidO2sat, 'long_name', lvar,
     &         vname_bgcflux(2,indxO2sat)(1:lvar))
#  else
          text='averaged '/ /vname_bgcflux(2,indxO2sat)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidO2sat, 'long_name', lvar,
     &                                              text(1:lvar))
#  endif
          lvar=lenstr(vname_bgcflux(3,indxO2sat))
          ierr=nf_put_att_text (ncid, vidO2sat, 'units',     lvar,
     &         vname_bgcflux(3,indxO2sat)(1:lvar))
!          lvar=lenstr(vname_bgcflux(4,indxO2sat))
!          ierr=nf_put_att_text (ncid, vidO2sat, 'field',     lvar,
!     &         vname_bgcflux(4,indxO2sat)(1:lvar))
#ifdef MASK_LAND_DATA
          ierr=nf_put_att_FOUT (ncid, vidO2sat, '_FillValue',
     &         NF_FOUT, 1, spv_set)
#endif
!        endif
#   ifdef CARBON
!
! Gas exchange coefficient of CO2
!
!        if (wrt(indxKvCO2)) then
          lvar=lenstr(vname_bgcflux(1,indxKvCO2))
          ierr=nf_def_var (ncid, vname_bgcflux(1,indxKvCO2)(1:lvar),
     &                             NF_FOUT, 3, r2dgrd, vidKvCO2)
#  ifndef AVRH
          lvar=lenstr(vname_bgcflux(2,indxKvCO2))
          ierr=nf_put_att_text (ncid, vidKvCO2, 'long_name', lvar,
     &         vname_bgcflux(2,indxKvCO2)(1:lvar))
#  else
          text='averaged '/ /vname_bgcflux(2,indxKvCO2)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidKvCO2, 'long_name', lvar,
     &                                              text(1:lvar))
#  endif
          lvar=lenstr(vname_bgcflux(3,indxKvCO2))
          ierr=nf_put_att_text (ncid, vidKvCO2, 'units',     lvar,
     &         vname_bgcflux(3,indxKvCO2)(1:lvar))
!          lvar=lenstr(vname_bgcflux(4,indxKvCO2))
!          ierr=nf_put_att_text (ncid, vidKvCO2, 'field',     lvar,
!     &         vname_bgcflux(4,indxKvCO2)(1:lvar))
#ifdef MASK_LAND_DATA
          ierr=nf_put_att_FOUT (ncid, vidKvCO2, '_FillValue',
     &         NF_FOUT, 1, spv_set)
#endif
!        endif
!
! Solubility of CO2
!
!        if (wrt(indxCO2sol)) then
          lvar=lenstr(vname_bgcflux(1,indxCO2sol))
          ierr=nf_def_var (ncid, vname_bgcflux(1,indxCO2sol)(1:lvar),
     &                             NF_FOUT, 3, r2dgrd, vidCO2sol)
#  ifndef AVRH
          lvar=lenstr(vname_bgcflux(2,indxCO2sol))
          ierr=nf_put_att_text (ncid, vidCO2sol, 'long_name', lvar,
     &         vname_bgcflux(2,indxCO2sol)(1:lvar))
#  else
          text='averaged '/ /vname_bgcflux(2,indxCO2sol)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidCO2sol, 'long_name', lvar,
     &                                              text(1:lvar))
#  endif
          lvar=lenstr(vname_bgcflux(3,indxCO2sol))
          ierr=nf_put_att_text (ncid, vidCO2sol, 'units',     lvar,
     &         vname_bgcflux(3,indxCO2sol)(1:lvar))
!          lvar=lenstr(vname_bgcflux(4,indxCO2sol))
!          ierr=nf_put_att_text (ncid, vidCO2sol, 'field',     lvar,
!     &         vname_bgcflux(4,indxCO2sol)(1:lvar))
#ifdef MASK_LAND_DATA
          ierr=nf_put_att_FOUT (ncid, vidCO2sol, '_FillValue',
     &         NF_FOUT, 1, spv_set)
#endif
!        endif
!
! Surface pCO2
!
!        if (wrt(indxPCO2)) then
!DL          lvar=lenstr(vname_bgcflux(1,indxPCO2))
!DL          ierr=nf_def_var (ncid, vname_bgcflux(1,indxPCO2)(1:lvar),
!DL     &                             NF_FOUT, 3, r2dgrd, vidPCO2)
!DL#  ifndef AVRH
!DL          lvar=lenstr(vname_bgcflux(2,indxPCO2))
!DL          ierr=nf_put_att_text (ncid, vidPCO2, 'long_name', lvar,
!DL     &         vname_bgcflux(2,indxPCO2)(1:lvar))
!DL#  else
!DL          text='averaged '/ /vname_bgcflux(2,indxPCO2)
!DL          lvar=lenstr(text)
!DL          ierr=nf_put_att_text (ncid, vidPCO2, 'long_name', lvar,
!DL     &                                              text(1:lvar))
!DL#  endif
!DL          lvar=lenstr(vname_bgcflux(3,indxPCO2))
!DL          ierr=nf_put_att_text (ncid, vidPCO2, 'units',     lvar,
!DL     &         vname_bgcflux(3,indxPCO2)(1:lvar))
!DL!          lvar=lenstr(vname_bgcflux(4,indxPCO2))
!DL!          ierr=nf_put_att_text (ncid, vidPCO2, 'field',     lvar,
!DL!     &         vname_bgcflux(4,indxPCO2)(1:lvar))
!DL#ifdef MASK_LAND_DATA
!DL          ierr=nf_put_att_FOUT (ncid, vidPCO2, '_FillValue',
!DL     &         NF_FOUT, 1, spv_set)
!DL#endif
!        endif
!
! Air pCO2
!
!        if (wrt(indxPCO2air)) then
!DL          lvar=lenstr(vname_bgcflux(1,indxPCO2air))
!DL          ierr=nf_def_var (ncid, vname_bgcflux(1,indxPCO2air)(1:lvar),
!DL     &                             NF_FOUT, 3, r2dgrd, vidPCO2air)
!DL#  ifndef AVRH
!DL          lvar=lenstr(vname_bgcflux(2,indxPCO2air))
!DL          ierr=nf_put_att_text (ncid, vidPCO2air, 'long_name', lvar,
!DL     &         vname_bgcflux(2,indxPCO2air)(1:lvar))
!DL#  else
!DL          text='averaged '/ /vname_bgcflux(2,indxPCO2air)
!DL          lvar=lenstr(text)
!DL          ierr=nf_put_att_text (ncid, vidPCO2air, 'long_name', lvar,
!DL     &                                              text(1:lvar))
!DL#  endif
!DL          lvar=lenstr(vname_bgcflux(3,indxPCO2air))
!DL          ierr=nf_put_att_text (ncid, vidPCO2air, 'units',     lvar,
!DL     &         vname_bgcflux(3,indxPCO2air)(1:lvar))
!DL!          lvar=lenstr(vname_bgcflux(4,indxPCO2air))
!DL!          ierr=nf_put_att_text (ncid, vidPCO2air, 'field',     lvar,
!DL!     &         vname_bgcflux(4,indxPCO2air)(1:lvar))
!DL#ifdef MASK_LAND_DATA
!DL          ierr=nf_put_att_FOUT (ncid, vidPCO2air, '_FillValue',
!DL     &         NF_FOUT, 1, spv_set)
!DL#endif
!        endif
!
!     Surface pH
!
!        if (wrt(indxPH)) then
!DL          lvar=lenstr(vname_bgcflux(1,indxPH))
!DL          ierr=nf_def_var (ncid, vname_bgcflux(1,indxPH)(1:lvar),
!DL     &                             NF_FOUT, 3, r2dgrd, vidPH)
!DL#  ifndef AVRH
!DL          lvar=lenstr(vname_bgcflux(2,indxPH))
!DL          ierr=nf_put_att_text (ncid, vidPH, 'long_name', lvar,
!DL     &                                  vname_bgcflux(2,indxPH)(1:lvar))
!DL#  else
!DL          text='averaged '/ /vname_bgcflux(2,indxPH)
!DL          lvar=lenstr(text)
!DL          ierr=nf_put_att_text (ncid, vidPH, 'long_name', lvar,
!DL     &                                              text(1:lvar))
!DL#  endif
!DL          lvar=lenstr(vname_bgcflux(3,indxPH))
!DL          ierr=nf_put_att_text (ncid, vidPH, 'units',     lvar,
!DL     &                                  vname_bgcflux(3,indxPH)(1:lvar))
!DL!          lvar=lenstr(vname_bgcflux(4,indxPH))
!DL!          ierr=nf_put_att_text (ncid, vidPH, 'field',     lvar,
!DL!     &                                  vname_bgcflux(4,indxPH)(1:lvar))
!DL#ifdef MASK_LAND_DATA
!DL          ierr=nf_put_att_FOUT (ncid, vidPH, '_FillValue',
!DL     &         NF_FOUT, 1, spv_set)
!DL#endif
!        endif
#   endif /* CARBON */
#  endif /* OXYGEN */
!
! Photosynthetically active radiation
!
!        if (wrt(indxPAR)) then
          lvar=lenstr(vname_bgcflux(1,indxPAR))
          ierr=nf_def_var (ncid, vname_bgcflux(1,indxPAR)(1:lvar),
     &                             NF_FOUT, 4, r3dgrd, vidPAR)
#  ifndef AVRH
          lvar=lenstr(vname_bgcflux(2,indxPAR))
          ierr=nf_put_att_text (ncid, vidPAR, 'long_name', lvar,
     &         vname_bgcflux(2,indxPAR)(1:lvar))
#  else
          text='averaged '/ /vname_bgcflux(2,indxPAR)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidPAR, 'long_name', lvar,
     &                                              text(1:lvar))
#  endif
          lvar=lenstr(vname_bgcflux(3,indxPAR))
          ierr=nf_put_att_text (ncid, vidPAR, 'units',     lvar,
     &         vname_bgcflux(3,indxPAR)(1:lvar))
!          lvar=lenstr(vname_bgcflux(4,indxPAR))
!          ierr=nf_put_att_text (ncid, vidPAR, 'field',     lvar,
!     &         vname_bgcflux(4,indxPAR)(1:lvar))
#ifdef MASK_LAND_DATA
          ierr=nf_put_att_FOUT (ncid, vidPAR, '_FillValue',
     &         NF_FOUT, 1, spv_set)
#endif

          lvar=lenstr(vname_bgcflux(1,indxPARinc))
          ierr=nf_def_var (ncid, vname_bgcflux(1,indxPARinc)(1:lvar),
     &                             NF_FOUT, 3, r2dgrd, vidPARinc)
#  ifndef AVRH
          lvar=lenstr(vname_bgcflux(2,indxPARinc))
          ierr=nf_put_att_text (ncid, vidPARinc, 'long_name', lvar,
     &         vname_bgcflux(2,indxPARinc)(1:lvar))
#  else
          text='averaged '/ /vname_bgcflux(2,indxPARinc)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidPARinc, 'long_name', lvar,
     &                                              text(1:lvar))
#  endif
          lvar=lenstr(vname_bgcflux(3,indxPARinc))
          ierr=nf_put_att_text (ncid, vidPARinc, 'units',     lvar,
     &         vname_bgcflux(3,indxPARinc)(1:lvar))
!          lvar=lenstr(vname_bgcflux(4,indxPARinc))
!          ierr=nf_put_att_text (ncid, vidPARinc, 'field',     lvar,
!     &         vname_bgcflux(4,indxPARinc)(1:lvar))
#ifdef MASK_LAND_DATA
          ierr=nf_put_att_FOUT (ncid, vidPARinc, '_FillValue',
     &         NF_FOUT, 1, spv_set)
#endif
!        endif
!
! Fluxes of tracer variables
!
! biogeochemical fluxes
#  if defined AVRH || defined BGC_FLUX_EXT_HIS_OUTPUT 
        do iflux = 1, NumFluxTerms
#  else /* AVRH || BGC_FLUX_EXT_HIS_OUTPUT */
        do iflux = 1, 2 
#  endif /* AVRH || BGC_FLUX_EXT_HIS_OUTPUT */
           lvar=lenstr(vname_bgcflux(1,indxFlux+iflux-1))
           ierr=nf_def_var (ncid, 
     &          vname_bgcflux(1,indxFlux+iflux-1)(1:lvar),
     &          NF_FOUT, 4, r3dgrd, vidFlux(iflux))
#  ifndef AVRH
           lvar=lenstr(vname_bgcflux(2,indxFlux+iflux-1))
           ierr=nf_put_att_text (ncid, vidFlux(iflux), 'long_name', 
     &          lvar, vname_bgcflux(2,indxFlux+iflux-1)(1:lvar))
#  else
           text='averaged '/ /vname_bgcflux(2,indxFlux+iflux-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidFlux(iflux), 'long_name', 
     &          lvar, text(1:lvar))
#  endif
           lvar=lenstr(vname_bgcflux(3,indxFlux+iflux-1))
           ierr=nf_put_att_text (ncid, vidFlux(iflux), 'units',     
     &          lvar, vname_bgcflux(3,indxFlux+iflux-1)(1:lvar))
!           lvar=lenstr(vname_bgcflux(4,indxFlux+iflux-1))
!           ierr=nf_put_att_text (ncid, vidFlux(iflux), 'field',     
!     &          lvar, vname_bgcflux(4,indxFlux+iflux-1)(1:lvar))
#  ifdef MASK_LAND_DATA
           ierr=nf_put_att_FOUT (ncid, vidFlux(iflux), '_FillValue',
     &           NF_FOUT, 1, spv_set)
#  endif
        end do
!DL: moved to def_his.F:
!#  ifdef OXYGEN
!! gas exchange fluxes
!        do iflux = 1, NumGasExcTerms
!           lvar=lenstr(vname_bgcflux(1,indxGasExcFlux+iflux-1))
!           ierr=nf_def_var (ncid, 
!     &          vname_bgcflux(1,indxGasExcFlux+iflux-1)(1:lvar),
!     &          NF_FOUT, 3, r2dgrd, vidGasExcFlux(iflux))
!#  ifndef AVRH
!           lvar=lenstr(vname_bgcflux(2,indxGasExcFlux+iflux-1))
!           ierr=nf_put_att_text (ncid, vidGasExcFlux(iflux), 
!     &          'long_name', 
!     &          lvar, vname_bgcflux(2,indxGasExcFlux+iflux-1)(1:lvar))
!#  else
!           text='averaged '/ /vname_bgcflux(2,indxGasExcFlux+iflux-1)
!           lvar=lenstr(text)
!           ierr=nf_put_att_text (ncid, vidGasExcFlux(iflux), 
!     &          'long_name', 
!     &          lvar, text(1:lvar))
!#  endif
!           lvar=lenstr(vname_bgcflux(3,indxGasExcFlux+iflux-1))
!           ierr=nf_put_att_text (ncid, vidGasExcFlux(iflux), 'units',
!     &          lvar, vname_bgcflux(3,indxGasExcFlux+iflux-1)(1:lvar))
!!           lvar=lenstr(vname_bgcflux(4,indxGasExcFlux+iflux-1))
!!           ierr=nf_put_att_text (ncid, vidGasExcFlux(iflux), 'field',
!!     &          lvar, vname_bgcflux(4,indxGasExcFlux+iflux-1)(1:lvar))
!#  ifdef MASK_LAND_DATA
!           ierr=nf_put_att_FOUT(ncid, vidGasExcFlux(iflux),'_FillValue',
!     &           NF_FOUT, 1, spv_set)
!#  endif
!        end do
!#  endif /* OXYGEN */
#  if defined AVRH || defined BGC_FLUX_EXT_HIS_OUTPUT 
! vertical sinking fluxes
        do iflux = 1, NumVSinkTerms
           lvar=lenstr(vname_bgcflux(1,indxVSinkFlux+iflux-1))
           ierr=nf_def_var (ncid, 
     &          vname_bgcflux(1,indxVSinkFlux+iflux-1)(1:lvar),
     &          NF_FOUT, 4, w3dgrd, vidVSinkFlux(iflux))
#  ifndef AVRH
           lvar=lenstr(vname_bgcflux(2,indxVSinkFlux+iflux-1))
           ierr=nf_put_att_text (ncid, vidVSinkFlux(iflux), 
     &          'long_name', 
     &          lvar, vname_bgcflux(2,indxVSinkFlux+iflux-1)(1:lvar))
#  else
           text='averaged '/ /vname_bgcflux(2,indxVSinkFlux+iflux-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidVSinkFlux(iflux), 
     &          'long_name', 
     &          lvar, text(1:lvar))
#  endif
           lvar=lenstr(vname_bgcflux(3,indxVSinkFlux+iflux-1))
           ierr=nf_put_att_text (ncid, vidVSinkFlux(iflux), 'units',
     &          lvar, vname_bgcflux(3,indxVSinkFlux+iflux-1)(1:lvar))
!           lvar=lenstr(vname_bgcflux(4,indxVSinkFlux+iflux-1))
!           ierr=nf_put_att_text (ncid, vidVSinkFlux(iflux), 'field',
!     &          lvar, vname_bgcflux(4,indxVSinkFlux+iflux-1)(1:lvar))
#  ifdef MASK_LAND_DATA
           ierr=nf_put_att_FOUT(ncid, vidVSinkFlux(iflux), '_FillValue',
     &           NF_FOUT, 1, spv_set)
#  endif
        end do
# ifdef SEDIMENT_BIOLOGY
! Sediment-related fluxes
        do iflux = 1, NumSedFluxTerms
           lvar=lenstr(vname_bgcflux(1,indxSedFlux+iflux-1))
           ierr=nf_def_var (ncid, 
     &          vname_bgcflux(1,indxSedFlux+iflux-1)(1:lvar),
     &          NF_FOUT, 3, r2dgrd, vidSedFlux(iflux))
#  ifndef AVRH
           lvar=lenstr(vname_bgcflux(2,indxSedFlux+iflux-1))
           ierr=nf_put_att_text (ncid, vidSedFlux(iflux), 
     &          'long_name', 
     &          lvar, vname_bgcflux(2,indxSedFlux+iflux-1)(1:lvar))
#  else
           text='averaged '/ /vname_bgcflux(2,indxSedFlux+iflux-1)
           lvar=lenstr(text)
           ierr=nf_put_att_text (ncid, vidSedFlux(iflux), 
     &          'long_name', 
     &          lvar, text(1:lvar))
#  endif
           lvar=lenstr(vname_bgcflux(3,indxSedFlux+iflux-1))
           ierr=nf_put_att_text (ncid, vidSedFlux(iflux), 'units',
     &          lvar, vname_bgcflux(3,indxSedFlux+iflux-1)(1:lvar))
!           lvar=lenstr(vname_bgcflux(4,indxSedFlux+iflux-1))
!           ierr=nf_put_att_text (ncid, vidSedFlux(iflux), 'field',
!     &          lvar, vname_bgcflux(4,indxSedFlux+iflux-1)(1:lvar))
#  ifdef MASK_LAND_DATA
           ierr=nf_put_att_FOUT (ncid, vidSedFlux(iflux), '_FillValue',
     &         NF_FOUT, 1, spv_set)
#  endif
        end do

# endif /* SEDIMENT_BIOLOGY */
# endif /* AVRH || BGC_FLUX_EXT_HIS_OUTPUT */
!
! Leave definition mode.
! ----- ---------- -----
!
        ierr=nf_enddef(ncid)
! put the scalar variables regarding biogeochemical fluxes
#ifdef OXYGEN
      ierr = nf_put_var1_FTYPE(ncid, vidONNO3, 1, ON_NO3)
      ierr = nf_put_var1_FTYPE(ncid, vidONNH4, 1, ON_NH4)
#endif /* OXYGEN */
#ifdef CARBON
      ierr = nf_put_var1_FTYPE(ncid, vidCNP, 1, CN_P)
      ierr = nf_put_var1_FTYPE(ncid, vidCNZ, 1, CN_Z)
      ierr = nf_put_var1_FTYPE(ncid, vidrCaCO3orgC, 1, r_CaCO3_orgC)
#endif /* CARBON */
#ifdef OXYLIM
      ierr = nf_put_var1_FTYPE(ncid, vidNCDET, 1, N_CDET)
#endif /* OXYLIM */
        write(stdout,'(6x,4A,1x,A,i4)') 
#ifdef AVRH
     &       'DEF_BGC_FLUX_AVG - Created ',
#else
     &       'DEF_BGC_FLUX_HIS - Created ',
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
          ierr=checkdims (ncid, ncname, rec) 
          if (ierr .eq. nf_noerr) then
            if (rec_per_file.eq.0) then
              ierr=rec+1 - total_rec
            else
              ierr=rec+1 - (1+mod(total_rec-1, rec_per_file))
            endif
            if (ierr.gt.0) then
              MPI_master_only write(stdout,
     &              '(/1x,A,I5,1x,A/8x,3A,I5,/8x,A,I5,1x,A/)')
#ifdef AVRH
     &         'DEF_BGC_FLUX_AVG WARNING: Actual number of records',
#else
     &         'DEF_BGC_FLUX_HIS WARNING: Actual number of records',
#endif /* AVRH */
     &              rec,  'in netCDF file',  '''',  
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
            write(stdout,'(/1x,4A,2x,A,I4/)') 
#ifdef AVRH
     &            'DEF_BGC_FLUX_AVG ERROR: ',
#else
     &            'DEF_BGC_FLUX_HIS ERROR: ',
#endif /* AVRH */
     &                  'Cannot open file ''', ncname(1:lstr), '''.'
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
          write(stdout,1) 'time_step', ncname(1:lstr)
          goto 99                                         !--> ERROR
        endif
!
! Time.
!
        lvar=lenstr(vname(1,indxTime))
        ierr=nf_inq_varid (ncid,vname(1,indxTime)(1:lvar),
     &       vidTime)
        if (ierr .ne. nf_noerr) then
          write(stdout,1) vname(1,indxTime)(1:lvar), 
     &          ncname(1:lstr)
          goto 99                                         !--> ERROR 
        endif

!
! Free-surface.
!
        lvar=lenstr(vname(1,indxZ))
        ierr=nf_inq_varid (ncid, vname(1,indxZ)(1:lvar),
     &       vidZ)
        if (ierr .ne. nf_noerr) then
          write(stdout,1) vname(1,indxZ)(1:lvar), 
     &          ncname(1:lstr)
          goto 99                                         !--> ERROR 
        endif

#  ifdef OXYGEN
!
! Wind speed at 10 m
!
!        if (wrt(indxU10)) then
          lvar=lenstr(vname_bgcflux(1,indxU10))
          ierr=nf_inq_varid (ncid, vname_bgcflux(1,indxU10)(1:lvar),
     &                             vidU10)
          if (ierr .ne. nf_noerr) then
             write(stdout,1) vname_bgcflux(1,indxU10)(1:lvar), 
     &            ncname(1:lstr)
             goto 99            !--> ERROR
          endif
!     endif
!
! Gas exchange coefficient of O2
!
!        if (wrt(indxKvO2)) then
          lvar=lenstr(vname_bgcflux(1,indxKvO2))
          ierr=nf_inq_varid (ncid, vname_bgcflux(1,indxKvO2)(1:lvar),
     &                             vidKvO2)
          if (ierr .ne. nf_noerr) then
             write(stdout,1) vname_bgcflux(1,indxKvO2)(1:lvar), 
     &            ncname(1:lstr)
             goto 99            !--> ERROR
          endif
!        endif
!
! Saturation concentration of O2
!
!        if (wrt(indxO2sat)) then
          lvar=lenstr(vname_bgcflux(1,indxO2sat))
          ierr=nf_inq_varid (ncid, vname_bgcflux(1,indxO2sat)(1:lvar),
     &                             vidO2sat)
          if (ierr .ne. nf_noerr) then
             write(stdout,1) vname_bgcflux(1,indxO2sat)(1:lvar), 
     &            ncname(1:lstr)
             goto 99            !--> ERROR
          endif
!        endif
#   ifdef CARBON
!
! Gas exchange coefficient of CO2
!
!        if (wrt(indxKvCO2)) then
          lvar=lenstr(vname_bgcflux(1,indxKvCO2))
          ierr=nf_inq_varid (ncid, vname_bgcflux(1,indxKvCO2)(1:lvar),
     &                             vidKvCO2)
          if (ierr .ne. nf_noerr) then
             write(stdout,1) vname_bgcflux(1,indxKvCO2)(1:lvar), 
     &            ncname(1:lstr)
             goto 99            !--> ERROR
          endif
!        endif
!
! Solubility of CO2
!
!        if (wrt(indxCO2sol)) then
          lvar=lenstr(vname_bgcflux(1,indxCO2sol))
          ierr=nf_inq_varid (ncid, vname_bgcflux(1,indxCO2sol)(1:lvar),
     &                             vidCO2sol)
          if (ierr .ne. nf_noerr) then
             write(stdout,1) vname_bgcflux(1,indxCO2sol)(1:lvar), 
     &            ncname(1:lstr)
             goto 99            !--> ERROR
          endif
!        endif
!
! Surface pCO2
!
!        if (wrt(indxPCO2)) then
!DL          lvar=lenstr(vname_bgcflux(1,indxPCO2))
!DL          ierr=nf_inq_varid (ncid, vname_bgcflux(1,indxPCO2)(1:lvar),
!DL     &                             vidPCO2)
!DL          if (ierr .ne. nf_noerr) then
!DL             write(stdout,1) vname_bgcflux(1,indxPCO2)(1:lvar), 
!DL     &            ncname(1:lstr)
!DL             goto 99            !--> ERROR
!DL          endif
!        endif
!
! Air pCO2
!
!        if (wrt(indxPCO2air)) then
!DL          lvar=lenstr(vname_bgcflux(1,indxPCO2air))
!DL          ierr=nf_inq_varid (ncid, vname_bgcflux(1,indxPCO2air)(1:lvar),
!DL     &                             vidPCO2air)
!DL          if (ierr .ne. nf_noerr) then
!DL             write(stdout,1) vname_bgcflux(1,indxPCO2air)(1:lvar), 
!DL     &            ncname(1:lstr)
!DL             goto 99            !--> ERROR
!DL          endif
!        endif
!
!     Surface pH
!
!        if (wrt(indxPH)) then
!DL          lvar=lenstr(vname_bgcflux(1,indxPH))
!DL          ierr=nf_inq_varid (ncid, vname_bgcflux(1,indxPH)(1:lvar),
!DL     &                             vidPH)
!DL          if (ierr .ne. nf_noerr) then
!DL             write(stdout,1) vname_bgcflux(1,indxPH)(1:lvar), 
!DL     &            ncname(1:lstr)
!DL             goto 99            !--> ERROR
!DL          endif
!        endif
#   endif /* CARBON */
#  endif /* OXYGEN */
!
! Photosynthetically active radiation
!
!        if (wrt(indxPAR)) then
          lvar=lenstr(vname_bgcflux(1,indxPAR))
          ierr=nf_inq_varid (ncid, vname_bgcflux(1,indxPAR)(1:lvar),
     &                             vidPAR)
          if (ierr .ne. nf_noerr) then
             write(stdout,1) vname_bgcflux(1,indxPAR)(1:lvar), 
     &            ncname(1:lstr)
             goto 99            !--> ERROR
          endif

          lvar=lenstr(vname_bgcflux(1,indxPARinc))
          ierr=nf_inq_varid (ncid, vname_bgcflux(1,indxPARinc)(1:lvar),
     &                             vidPARinc)
          if (ierr .ne. nf_noerr) then
             write(stdout,1) vname_bgcflux(1,indxPARinc)(1:lvar), 
     &            ncname(1:lstr)
             goto 99            !--> ERROR
          endif
!        endif
!
! Fluxes of tracer variables
!
! biogeochemical fluxes
#  if defined AVRH || defined BGC_FLUX_EXT_HIS_OUTPUT 
        do iflux = 1, NumFluxTerms
#  else /* AVRH || BGC_FLUX_EXT_HIS_OUTPUT */
        do iflux = 1, 2 
#  endif /* AVRH || BGC_FLUX_EXT_HIS_OUTPUT */
           lvar=lenstr(vname_bgcflux(1,indxFlux+iflux-1))
           ierr=nf_inq_varid (ncid, 
     &          vname_bgcflux(1,indxFlux+iflux-1)(1:lvar),
     &          vidFlux(iflux))
           if (ierr .ne. nf_noerr) then
              write(stdout,1) vname_bgcflux(1,indxFlux+iflux-1)(1:lvar),
     &             ncname(1:lstr)
              goto 99           !--> ERROR
        endif
        end do
!#  ifdef OXYGEN
! gas exchange fluxes
!        do iflux = 1, NumGasExcTerms
!           lvar=lenstr(vname_bgcflux(1,indxGasExcFlux+iflux-1))
!           ierr=nf_inq_varid (ncid, 
!     &          vname_bgcflux(1,indxGasExcFlux+iflux-1)(1:lvar),
!     &          vidGasExcFlux(iflux))
!           if (ierr .ne. nf_noerr) then
!              write(stdout,1) 
!     &             vname_bgcflux(1,indxGasExcFlux+iflux-1)(1:lvar), 
!     &             ncname(1:lstr)
!              goto 99           !--> ERROR
!        endif
!        end do
!#  endif /* OXYGEN */
#  if defined AVRH || defined BGC_FLUX_EXT_HIS_OUTPUT 
! vertical sinking fluxes
        do iflux = 1, NumVSinkTerms
           lvar=lenstr(vname_bgcflux(1,indxVSinkFlux+iflux-1))
           ierr=nf_inq_varid (ncid, 
     &          vname_bgcflux(1,indxVSinkFlux+iflux-1)(1:lvar),
     &          vidVSinkFlux(iflux))
           if (ierr .ne. nf_noerr) then
              write(stdout,1) 
     &             vname_bgcflux(1,indxVSinkFlux+iflux-1)(1:lvar), 
     &             ncname(1:lstr)
              goto 99           !--> ERROR
        endif
        end do
# ifdef SEDIMENT_BIOLOGY
! Sediment-related fluxes
        do iflux = 1, NumSedFluxTerms
           lvar=lenstr(vname_bgcflux(1,indxSedFlux+iflux-1))
           ierr=nf_inq_varid (ncid, 
     &          vname_bgcflux(1,indxSedFlux+iflux-1)(1:lvar),
     &          vidSedFlux(iflux))
           if (ierr .ne. nf_noerr) then
              write(stdout,1) vname_bgcflux(1,
     &             indxSedFlux+iflux-1)(1:lvar), ncname(1:lstr)
              goto 99           !--> ERROR
           endif
        end do

# endif /* SEDIMENT_BIOLOGY */
# endif /* AVRH || BGC_FLUX_EXT_HIS_OUTPUT */
        
        write(*,'(6x,2A,i4,1x,A,i4)') 
#ifdef AVRH
     &       'DEF_BGC_FLUX_AVG -- Opened ',
#else
     &       'DEF_BGC_FLUX_HIS -- Opened ',
#endif /* AVRH */
     &                     'existing file  from record =', rec
     &                      MYID

#if defined MPI && !defined PARALLEL_FILES
      else create_new_file_if
        ierr=nf_open (ncname(1:lstr), nf_write, ncid)
        if (ierr .ne. nf_noerr) then
          write(stdout,'(/1x,4A,2x,A,I4/)') 
#ifdef AVRH
     &          'DEF_BGC_FLUX_AVG ERROR: ',
#else
     &          'DEF_BGC_FLUX_HIS ERROR: ',
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
     &        'DEF_BGC_FLUX_AVG ERROR: Cannot ', 
#else
     &        'DEF_BGC_FLUX_HIS ERROR: Cannot ', 
#endif /* AVRH */
     &    'switch to ''nf_nofill'' more; netCDF error code =', ierr 
      endif
#ifdef AVRH
   1  format(/1x,'DEF_BGC_FLUX_AVG ERROR: Cannot find variable ''',
#else
   1  format(/1x,'DEF_BGC_FLUX_HIS ERROR: Cannot find variable ''',
#endif /* AVRH */
     &                   A, ''' in netCDF file ''', A, '''.'/)
#if (defined PUT_GRID_INTO_HISTORY && !defined AVRH)\
 || (defined PUT_GRID_INTO_AVERAGES && defined AVRH)
!
! Write grid variables.
! ----- ---- ----------
!
      if (total_rec.le.1) call wrt_grid(ncid, ncname, lstr)
#endif
  99  return
      end

#undef ncname
#undef rec_per_file
#undef vidTime
#undef vidTstep
#undef vidZ
#undef vidU10
#undef vidKvO2
#undef vidO2sat
#undef vidKvCO2
#undef vidCO2sol
#undef vidPCO2
#undef vidPCO2air
#undef vidPH
#undef vidPAR
#undef vidPARinc
#undef vidFlux
!#undef vidGasExcFlux
#undef vidVSinkFlux
#undef vidSedFlux
#undef vidONNO3
#undef vidONNH4
#undef vidCNP
#undef vidCNZ
#undef vidrCaCO3orgC
#undef vidNCDET
# ifndef AVRH
#  ifdef AVERAGES      
#   define AVRH
#   include "def_bgc_flux.F"
#  endif /* AVERAGES */
# endif
#else /* BIOLOGY_NPZDOC && BGC_FLUX_ANALYSIS */
      subroutine def_bgc_flux_empty()
      return
      end
#endif /* BIOLOGY_NPZDOC && BGC_FLUX_ANALYSIS */
