#include "cppdefs.h"
                                                  ! Create/open
      subroutine def_rst (ncid, total_rec, ierr)  ! restart netCDF
      implicit none                               ! file.
      integer ncid, total_rec, my_nf_def_dim, ifour, ierr
      parameter (ifour=4)
#include "param.h"
#include "scalars.h"
#include "ncvars.h"
#include "netcdf.inc"
#ifdef BIOLOGY_BEC
# include "ecosys_bec.h"
#endif
#ifdef FLOATS
# include "floats.h"
# include "ncvars_floats.h"
#endif
      logical create_new_file
      integer rec, lstr, lvar, timedim, lenstr, checkdims,
     &        r2dgrd(3), u2dgrd(3), v2dgrd(3),  auxil(2)
#ifdef SOLVE3D
     &      , r3dgrd(4), u3dgrd(4), v3dgrd(4),  w3dgrd(4), itrc
#endif
#ifdef FLOATS
      integer fltdim, NFVdim, Tinfovardim, NFTdim,
     &        dim_Tinfo(3), dim_track(4), dim_fltgrd(2)
      character*65 vinfo(4)
#endif
#ifdef MASK_LAND_DATA
# ifdef DBLEPREC
      real*8 spv_set
      parameter (spv_set=1.D+33)
# else
      real*4 spv_set
      parameter (spv_set=1.E+33)
# endif
#endif
!
! Put time record index into file name. In  the case when model
! output is to be arranged into sequence of named files, the naming
! convention is as follows: 'rst_root.INDEX.[MPI_node.]nc', where
! INDEX is an integer number such that (i) it is divisible by the
! specified number of records per file; and (ii)
!
!      INDEX + record_within_the_file = total_record
!
! where, 1 =< record_within_the_file =< records_per_file, so that
! total_record changes continuously throughout the sequence of files.
!
      ierr=0
      lstr=lenstr(rstname)
      if (nrpfrst.gt.0) then
        lvar=total_rec - (1+mod(total_rec-1, nrpfrst))
        call insert_time_index (rstname, lstr, lvar, ierr)
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
! Create new restart file:    Put global attributes
!======= === ======= =====    and define all variables.
!
  10  if (create_new_file) then
        ierr=nf_create(rstname(1:lstr), or(nf_clobber,nf_64bit_offset),
     &        ncid)
        if (ierr.ne.nf_noerr) then
          write(stdout,'(/1x,4A/12x,A/)')  '### ERROR: def_rst :: ',
     &      'Cannot create restart NetCDF file ''', rstname(1:lstr),
     &                                      '''',  nf_strerror(ierr)
          goto 99              !--> ERROR
        endif
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
        ierr=my_nf_def_dim (ncid, 's_rho',   N,       r3dgrd(3))
        ierr=my_nf_def_dim (ncid, 's_w',     N+1,     w3dgrd(3))
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

#ifdef FLOATS
        ierr=my_nf_def_dim (ncid, 'drifter', nfloats, fltdim)
        ierr=my_nf_def_dim (ncid, 'Tinfovar', 5, Tinfovardim)
        ierr=my_nf_def_dim (ncid, 'NFV',      6,      NFVdim)
        ierr=my_nf_def_dim (ncid, 'NFT',     NFT+1,   NFTdim)

        dim_Tinfo(1)=Tinfovardim
        dim_Tinfo(2)=fltdim
        dim_Tinfo(3)=timedim

        dim_track(1)=NFVdim
        dim_track(2)=NFTdim
        dim_track(3)=fltdim
        dim_track(4)=timedim

        dim_fltgrd(1)=fltdim
        dim_fltgrd(2)=timedim
#endif

#ifdef PUT_GRID_INTO_RESTART
!
! Define grid variables.
! ------ ---- ----------
!
        if (total_rec.eq.1) call def_grid (ncid, r2dgrd)
#endif
!
! Define evolving model variables:
! ------ -------- ----- ----------
!
!
! Time step number and time record numbers:
!
        ierr=nf_def_var (ncid, 'time_step', nf_int, 2, auxil,
     &                                                 rstTstep)
        ierr=nf_put_att_text (ncid, rstTstep, 'long_name', 48,
     &       'time step and record numbers from initialization')
#if defined BGC_FLUX_ANALYSIS || defined PHYS_FLUX_ANALYSIS
!
! Time record numbers for flux analysis:
!
        ierr=nf_def_var (ncid, 'fa_time_step', nf_int, 2, auxil,
     &                                                 rstTstepFA)
        ierr=nf_put_att_text (ncid, rstTstepFA, 'long_name', 57,
     &  'time record numbers from initialization for flux analysis')
#endif /* BGC_FLUX_ANALYSIS || PHYS_FLUX_ANALYSIS */
!
! Time.
!
        lvar=lenstr(vname(1,indxTime))
        ierr=nf_def_var (ncid, vname(1,indxTime)(1:lvar), NF_FTYPE,
     &                                         1, timedim, rstTime)
        lvar=lenstr(vname(2,indxTime))
        ierr=nf_put_att_text (ncid, rstTime, 'long_name', lvar,
     &                                   vname(2,indxTime)(1:lvar))
        lvar=lenstr(vname(3,indxTime))
        ierr=nf_put_att_text (ncid, rstTime, 'units',     lvar,
     &                                   vname(3,indxTime)(1:lvar))
!
! Free-surface.
!
        lvar=lenstr(vname(1,indxZ))
        ierr=nf_def_var (ncid, vname(1,indxZ)(1:lvar), NF_FTYPE,
     &                                              3, r2dgrd,rstZ)
        lvar=lenstr(vname(2,indxZ))
        ierr=nf_put_att_text (ncid, rstZ, 'long_name', lvar,
     &                                      vname(2,indxZ)(1:lvar))
        lvar=lenstr(vname(3,indxZ))
        ierr=nf_put_att_text (ncid, rstZ, 'units', lvar,
     &                                      vname(3,indxZ)(1:lvar))
#ifdef MASK_LAND_DATA
        ierr=nf_put_att_FTYPE (ncid, rstZ, '_FillValue', NF_FTYPE,
     &                                                 1, spv_set)
#endif

!
! 2D momenta in XI- and ETA-directions.
!
        lvar=lenstr(vname(1,indxUb))
        ierr=nf_def_var (ncid, vname(1,indxUb)(1:lvar), NF_FTYPE,
     &                                           3, u2dgrd, rstUb)
        lvar=lenstr(vname(2,indxUb))
        ierr=nf_put_att_text (ncid, rstUb, 'long_name', lvar,
     &                                     vname(2,indxUb)(1:lvar))
        lvar=lenstr(vname(3,indxUb))
        ierr=nf_put_att_text (ncid, rstUb, 'units', lvar,
     &                                     vname(3,indxUb)(1:lvar))
#ifdef MASK_LAND_DATA
        ierr=nf_put_att_FTYPE (ncid, rstUb, '_FillValue', NF_FTYPE,
     &                                                  1, spv_set)
#endif
 
        lvar=lenstr(vname(1,indxVb))
        ierr=nf_def_var (ncid, vname(1,indxVb)(1:lvar), NF_FTYPE,
     &                                           3, v2dgrd, rstVb)
        lvar=lenstr(vname(2,indxVb))
        ierr=nf_put_att_text (ncid, rstVb, 'long_name', lvar,
     &                                     vname(2,indxVb)(1:lvar))
        lvar=lenstr(vname(3,indxVb))
        ierr=nf_put_att_text (ncid, rstVb, 'units',     lvar,
     &                                     vname(3,indxVb)(1:lvar))
#ifdef MASK_LAND_DATA
        ierr=nf_put_att_FTYPE (ncid, rstVb, '_FillValue', NF_FTYPE,
     &                                                  1, spv_set)
#endif

#ifdef SOLVE3D
!
! 3D momenta in XI- and ETA-directions.
!
        lvar=lenstr(vname(1,indxU))
        ierr=nf_def_var (ncid, vname(1,indxU)(1:lvar), NF_FTYPE,
     &                                           4, u3dgrd, rstU)
        lvar=lenstr(vname(2,indxU))
        ierr=nf_put_att_text (ncid, rstU, 'long_name', lvar,
     &                                     vname(2,indxU)(1:lvar))
        lvar=lenstr(vname(3,indxU))
        ierr=nf_put_att_text (ncid, rstU, 'units',     lvar,
     &                                     vname(3,indxU)(1:lvar))
# ifdef MASK_LAND_DATA
        ierr=nf_put_att_FTYPE (ncid, rstU, '_FillValue', NF_FTYPE,
     &                                                 1, spv_set)
# endif
 
        lvar=lenstr(vname(1,indxV))
        ierr=nf_def_var (ncid, vname(1,indxV)(1:lvar), NF_FTYPE,
     &                                           4, v3dgrd, rstV)
        lvar=lenstr(vname(2,indxV))
        ierr=nf_put_att_text (ncid, rstV, 'long_name', lvar,
     &                                     vname(2,indxV)(1:lvar))
        lvar=lenstr(vname(3,indxV))
        ierr=nf_put_att_text (ncid, rstV, 'units',     lvar,
     &                                     vname(3,indxV)(1:lvar))
# ifdef MASK_LAND_DATA
        ierr=nf_put_att_FTYPE (ncid, rstV, '_FillValue', NF_FTYPE,
     &                                                 1, spv_set)
# endif

!
! Tracer variables.
!
        do itrc=1,NT
          lvar=lenstr(vname(1,indxT+itrc-1))
          ierr=nf_def_var (ncid, vname(1,indxT+itrc-1)(1:lvar),
     &                          NF_FTYPE, 4, r3dgrd, rstT(itrc))
          lvar=lenstr(vname(2,indxT+itrc-1))
          ierr=nf_put_att_text (ncid, rstT(itrc), 'long_name',
     &                     lvar,   vname(2,indxT+itrc-1)(1:lvar))
          lvar=lenstr(vname(3,indxT+itrc-1))
          ierr=nf_put_att_text (ncid, rstT(itrc), 'units', lvar,
     &                             vname(3,indxT+itrc-1)(1:lvar))
# ifdef MASK_LAND_DATA
          ierr=nf_put_att_FTYPE (ncid, rstT(itrc), '_FillValue',
     &                                     NF_FTYPE, 1, spv_set)
# endif
        enddo
# ifdef SEDIMENT_BIOLOGY
!
! Sediment tracer variables.
!
        do itrc=1,NT_sed
          lvar=lenstr(vname(1,indxSedOrgN+itrc-1))
          ierr=nf_def_var(ncid,vname(1,indxSedOrgN+itrc-1)(1:lvar),
     &                          NF_FTYPE, 3, r2dgrd, rstTsed(itrc))
          lvar=lenstr(vname(2,indxSedOrgN+itrc-1))
          ierr=nf_put_att_text (ncid, rstTsed(itrc), 'long_name',
     &         lvar,   vname(2,indxSedOrgN+itrc-1)(1:lvar))
          lvar=lenstr(vname(3,indxSedOrgN+itrc-1))
          ierr=nf_put_att_text (ncid, rstTsed(itrc), 'units', lvar,
     &         vname(3,indxSedOrgN+itrc-1)(1:lvar))
!          lvar=lenstr(vname(4,indxSedOrgN+itrc-1))
!          ierr=nf_put_att_text (ncid, rstTsed(itrc), 'field', lvar,
!     &         vname(4,indxSedOrgN+itrc-1)(1:lvar))
#ifdef MASK_LAND_DATA
        ierr=nf_put_att_FTYPE (ncid, rstTsed(itrc), '_FillValue',
     &         NF_FTYPE, 1, spv_set)
#endif
        enddo
# endif /* SEDIMENT_BIOLOGY */

# ifdef LMD_KPP
        lvar=lenstr(vname(1,indxHbl))
        ierr=nf_def_var (ncid, vname(1,indxHbl)(1:lvar), NF_FTYPE,
     &                                           3, r2dgrd, rstHbl)
        lvar=lenstr(vname(2,indxHbl))
        ierr=nf_put_att_text (ncid, rstHbl, 'long_name', lvar,
     &                                    vname(2,indxHbl)(1:lvar))
        lvar=lenstr(vname(3,indxHbl))
        ierr=nf_put_att_text (ncid, rstHbl, 'units', lvar,
     &                                    vname(3,indxHbl)(1:lvar))
#  ifdef MASK_LAND_DATA
        ierr=nf_put_att_FTYPE (ncid, rstHbl, '_FillValue', NF_FTYPE,
     &                                                 1, spv_set)
#  endif
# endif

# ifdef LMD_BKPP
        lvar=lenstr(vname(1,indxHbbl))
        ierr=nf_def_var (ncid, vname(1,indxHbbl)(1:lvar), NF_FTYPE,
     &                                           3, r2dgrd, rstHbbl)
        lvar=lenstr(vname(2,indxHbbl))
        ierr=nf_put_att_text (ncid, rstHbbl, 'long_name', lvar,
     &                                    vname(2,indxHbbl)(1:lvar))
        lvar=lenstr(vname(3,indxHbbl))
        ierr=nf_put_att_text (ncid, rstHbbl, 'units', lvar,
     &                                    vname(3,indxHbbl)(1:lvar))
#  ifdef MASK_LAND_DATA
        ierr=nf_put_att_FTYPE (ncid, rstHbbl, '_FillValue', NF_FTYPE,
     &                                                 1, spv_set)
#  endif
# endif
#endif /* SOLVE3D */
#ifdef FLOATS
        vinfo(1)='nfloats'
        vinfo(2)='total number of floats'

        lvar=lenstr(vinfo(1))
        ierr=nf_def_var(ncid,vinfo(1)(1:lvar),nf_int,
     &                    1,timedim,rstnfloats)
        lvar=lenstr(vinfo(2))
        ierr=nf_put_att_text(ncid,rstnfloats,'long_name',lvar,
     &                         vinfo(2)(1:lvar))
!
! define Tinfo
!
        vinfo(1)='Tinfo'
        vinfo(2)='initial float release information array'

        lvar=lenstr(vinfo(1))
        ierr=nf_def_var (ncid, vinfo(1)(1:lvar), NF_FTYPE,
     &                                3, dim_Tinfo, rstTinfo)
        lvar=lenstr(vinfo(2))
        ierr=nf_put_att_text (ncid,rstTinfo, 'long_name', lvar,
     &                                        vinfo(2)(1:lvar))
!
! define grid level
!
        vinfo(1)='grid_level'
        vinfo(2)='float position in nested grids hierarchy'

        lvar=lenstr(vinfo(1))
        ierr=nf_def_var(ncid,vinfo(1)(1:lvar),nf_int,
     &                    2,dim_fltgrd,rstfltgrd)
        lvar=lenstr(vinfo(2))
        ierr=nf_put_att_text(ncid,rstfltgrd,'long_name',lvar,
     &                         vinfo(2)(1:lvar))
!
! define track
!
        vinfo(1)='track'
        vinfo(2)='Position and velocity array for released floats'
        vinfo(3)='x,y,z,u,v,w in this order'
        lvar=lenstr(vinfo(1))
        ierr=nf_def_var(ncid,vinfo(1)(1:lvar),NF_FTYPE,
     &                   4,dim_track,rsttrack)
        lvar=lenstr(vinfo(2))
        ierr=nf_put_att_text(ncid,rsttrack,'long_name',lvar,
     &                         vinfo(2)(1:lvar))
        lvar=lenstr(vinfo(3))
        ierr=nf_put_att_text(ncid,rsttrack,'Content',lvar,
     &                         vinfo(3)(1:lvar))
#endif /* FLOATS */
# if defined BIOLOGY_BEC 
#  if defined CH_CARBON_DEPTH

!
! pH_DEPTH:
!
        lvar = lenstr(vname(1,indxPHd_rst))
        ierr=nf_def_var (ncid, vname(1,indxPHd_rst)(1:lvar),
     &       NF_FTYPE, 3, r3dgrd, rstPHd)
        lvar=lenstr(vname(2,indxPHd_rst))
        ierr=nf_put_att_text (ncid, rstPHd, 'long_name',
     &       lvar, vname(2,indxPHd_rst)(1:lvar))
        lvar=lenstr(vname(3,indxPHd_rst))
        ierr=nf_put_att_text (ncid, rstPHd, 'units', lvar,
     &       vname(3,indxPHd_rst)(1:lvar))
#ifdef MASK_LAND_DATA
        ierr=nf_put_att_FTYPE (ncid, rstPHd, '_FillValue', NF_FTYPE,
     &                                                 1, spv_set)
#endif
#endif /*CH_CARBON_DEPTH*/


!
! pH:
!
        lvar = lenstr(vname(1,indxPH_rst))
        ierr=nf_def_var (ncid, vname(1,indxPH_rst)(1:lvar),
     &       NF_FTYPE, 3, r2dgrd, rstPH)
        lvar=lenstr(vname(2,indxPH_rst))
        ierr=nf_put_att_text (ncid, rstPH, 'long_name',
     &       lvar, vname(2,indxPH_rst)(1:lvar))
        lvar=lenstr(vname(3,indxPH_rst))
        ierr=nf_put_att_text (ncid, rstPH, 'units', lvar,
     &       vname(3,indxPH_rst)(1:lvar))
#ifdef MASK_LAND_DATA
        ierr=nf_put_att_FTYPE (ncid, rstPH, '_FillValue', NF_FTYPE,
     &                                                 1, spv_set)
#endif

!
! pCO2:
!
        lvar = lenstr(vname(1,indxPCO2_rst))
        ierr=nf_def_var (ncid, vname(1,indxPCO2_rst)(1:lvar),
     &       NF_FTYPE, 3, r2dgrd, rstPCO2)
        lvar=lenstr(vname(2,indxPCO2_rst))
        ierr=nf_put_att_text (ncid, rstPCO2, 'long_name',
     &       lvar, vname(2,indxPCO2_rst)(1:lvar))
        lvar=lenstr(vname(3,indxPCO2_rst))
        ierr=nf_put_att_text (ncid, rstPCO2, 'units', lvar,
     &       vname(3,indxPCO2_rst)(1:lvar))
#ifdef MASK_LAND_DATA
        ierr=nf_put_att_FTYPE (ncid, rstPCO2, '_FillValue', NF_FTYPE,
     &                                                 1, spv_set)
#endif

#  ifdef OUTPUT_PCO2AIR_RST
!
! pCO2air:
!
        lvar = lenstr(vname(1,indxPCO2air_rst))
        ierr=nf_def_var (ncid, vname(1,indxPCO2air_rst)(1:lvar),
     &       NF_FTYPE, 3, r2dgrd, rstPCO2air)
        lvar=lenstr(vname(2,indxPCO2air_rst))
        ierr=nf_put_att_text (ncid, rstPCO2air, 'long_name',
     &       lvar, vname(2,indxPCO2air_rst)(1:lvar))
        lvar=lenstr(vname(3,indxPCO2air_rst))
        ierr=nf_put_att_text (ncid, rstPCO2air, 'units', lvar,
     &       vname(3,indxPCO2air_rst)(1:lvar))
#ifdef MASK_LAND_DATA
        ierr=nf_put_att_FTYPE (ncid, rstPCO2air, '_FillValue', NF_FTYPE,
     &                                                 1, spv_set)
#endif
#  endif /* OUTPUT_PCO2AIR_RST */
!
! PAR:
!
        lvar = lenstr(vname(1,indxPAR_rst))
        ierr=nf_def_var (ncid, vname(1,indxPAR_rst)(1:lvar),
     &       NF_FTYPE, 4, r3dgrd, rstPAR)
        lvar=lenstr(vname(2,indxPAR_rst))
        ierr=nf_put_att_text (ncid, rstPAR, 'long_name',
     &       lvar, vname(2,indxPAR_rst)(1:lvar))
        lvar=lenstr(vname(3,indxPAR_rst))
        ierr=nf_put_att_text (ncid, rstPAR, 'units', lvar,
     &       vname(3,indxPAR_rst)(1:lvar))
#ifdef MASK_LAND_DATA
        ierr=nf_put_att_FTYPE (ncid, rstPAR, '_FillValue', NF_FTYPE,
     &                                                 1, spv_set)
#endif

# endif /* BIOLOGY_BEC || BIOLOGY_NPZDOC */

!
! Leave definition mode.                  Also initialize record
! ----- ---------- -----                  dimension size to zero.
!
        ierr=nf_enddef(ncid)
        write(*,'(6x,4A,1x,A,i4)') 'DEF_RST - Created new ',
     &              'netCDF file ''', rstname(1:lstr), '''.'
     &               MYID
!
! Open an existing file and prepare for appending data.
! ==== == ======== ==== === ======= === ========= =====
! Check consistency of the dimensions of fields from the
! file with model dimensions. Determine the current size
! of unlimited dimension and set initial record [in the
! case of MPI serialized output, at this moment the last
! time record is assumed to be **partially** written by
! MPI processes with lower rank. Thus the next write is
! expected to be into the same record rather than next
! one (except MPI-master, who initializes the record).
!
! In the case when file is rejected (whether it cannot
! be opened, or something is wrong with its dimensions,
! create new file.
!
      elseif (ncid.eq.-1) then
        ierr=nf_open (rstname(1:lstr), nf_write, ncid)
        if (ierr. eq. nf_noerr) then
          ierr=checkdims (ncid, rstname(1:lstr), lstr, rec)
          if (ierr .eq. nf_noerr) then
            if (nrpfrst.eq.0) then
              ierr=rec+1 - nrecrst
            else
              ierr=rec+1 - (1+mod(nrecrst-1, abs(nrpfrst)))
            endif
            if (ierr.gt.0) then
              MPI_master_only write( stdout,
     &                 '(/1x,A,I5,1x,A/8x,3A,I5/8x,A,I5,1x,A/)'
     &           ) 'DEF_RST WARNING: Actual number of records', rec,
     &             'in netCDF file',  '''',  rstname(1:lstr),
     &             ''' exceeds the record number from restart data',
     &             rec+1-ierr,'/', total_rec,', restart is assumed.'
              rec=rec-ierr
            elseif (nrpfrst.eq.0) then
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
            write(stdout,'(/1x,4A, 1x,A,I4/)')     'DEF_RST ERROR: ',
     &     'Cannot open restart netCDF file ''',rstname(1:lstr),'''.'
     &               MYID
            goto 99                                     !--> ERROR
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
        ierr=nf_inq_varid (ncid, 'time_step', rstTstep)
        if (ierr .ne. nf_noerr) then
          write(stdout,1) 'time_step', rstname(1:lstr)
          goto 99                                         !--> ERROR
        endif
#if defined BGC_FLUX_ANALYSIS || defined PHYS_FLUX_ANALYSIS
!
! Time record numbers for flux analysis:
!
        ierr=nf_inq_varid (ncid, 'fa_time_step', rstTstepFA)
        if (ierr .ne. nf_noerr) then
          write(stdout,1) 'fa_time_step', rstname(1:lstr)
          goto 99                                         !--> ERROR
        endif
#endif /* BGC_FLUX_ANALYSIS || PHYS_FLUX_ANALYSIS */
!
! Time.
!
        lvar=lenstr(vname(1,indxTime))
        ierr=nf_inq_varid (ncid, vname(1,indxTime)(1:lvar), rstTime)
        if (ierr. ne. nf_noerr) then
          write(stdout,1) vname(1,indxTime)(1:lvar), rstname(1:lstr)
          goto 99                                         !--> ERROR
        endif
!
! Free-surface.
!
        lvar=lenstr(vname(1,indxZ))
        ierr=nf_inq_varid (ncid, vname(1,indxZ)(1:lvar), rstZ)
        if (ierr. ne. nf_noerr) then
          write(stdout,1) vname(1,indxZ)(1:lvar),  rstname(1:lstr)
          goto 99                                         !--> ERROR
        endif
!
! 2D momenta in XI- and ETA-directions.
!
        lvar=lenstr(vname(1,indxUb))
        ierr=nf_inq_varid (ncid, vname(1,indxUb)(1:lvar), rstUb)
        if (ierr. ne. nf_noerr) then
          write(stdout,1) vname(1,indxUb)(1:lvar), rstname(1:lstr)
          goto 99                                         !--> ERROR
        endif
 
        lvar=lenstr(vname(1,indxVb))
        ierr=nf_inq_varid (ncid, vname(1,indxVb)(1:lvar), rstVb)
        if (ierr. ne. nf_noerr) then
          write(stdout,1) vname(1,indxVb)(1:lvar), rstname(1:lstr)
          goto 99                                         !--> ERROR
        endif
 
#ifdef SOLVE3D
!
! 3D momenta n XI- and ETA-directions.
!
        lvar=lenstr(vname(1,indxU))
        ierr=nf_inq_varid (ncid, vname(1,indxU)(1:lvar), rstU)
        if (ierr. ne. nf_noerr) then
          write(stdout,1) vname(1,indxU)(1:lvar), rstname(1:lstr)
          goto 99                                         !--> ERROR
        endif
 
        lvar=lenstr(vname(1,indxV))
        ierr=nf_inq_varid (ncid, vname(1,indxV)(1:lvar), rstV)
        if (ierr. ne. nf_noerr) then
          write(stdout,1) vname(1,indxV)(1:lvar), rstname(1:lstr)
          goto 99                                         !--> ERROR
        endif
!
! Tracer variables.
!
        do itrc=1,NT
          lvar=lenstr(vname(1,indxT+itrc-1))
          ierr=nf_inq_varid (ncid, vname(1,indxT+itrc-1)(1:lvar),
     &                                                rstT(itrc))
          if (ierr. ne. nf_noerr) then
            write(stdout,1) vname(1,indxT+itrc-1)(1:lvar),
     &                                            rstname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        enddo
# ifdef SEDIMENT_BIOLOGY
!
! Sediment tracer variables.
!
        do itrc=1,NT_sed
           lvar=lenstr(vname(1,indxSedOrgN+itrc-1))
           ierr=nf_inq_varid (ncid, 
     &          vname(1,indxSedOrgN+itrc-1)(1:lvar),rstTsed(itrc))
           if (ierr .ne. nf_noerr) then
              write(stdout,1) vname(1,indxSedOrgN+itrc-1)(1:lvar),
     &             rstname(1:lstr) 
              goto 99           !--> ERROR
           endif
        enddo
# endif /* SEDIMENT_BIOLOGY */

# if defined BIOLOGY_BEC 
!#ifdef CH_CARBON_DEPTH
!
! HCO3:
!
!        lvar=lenstr(vname(1,indxHCO3_rst))
!        ierr=nf_inq_varid (ncid, vname(1,indxHCO3_rst)(1:lvar), rstHCO3)
!        if (ierr. ne. nf_noerr) then
!          write(stdout,1) vname(1,indxHCO3_rst)(1:lvar), rstname(1:lstr)
!          goto 99                                         !--> ERROR
!        endif
!
! CO3:
!
!        lvar=lenstr(vname(1,indxCO3_rst))
!        ierr=nf_inq_varid (ncid, vname(1,indxCO3_rst)(1:lvar), rstCO3)
!        if (ierr. ne. nf_noerr) then
!          write(stdout,1) vname(1,indxCO3_rst)(1:lvar), rstname(1:lstr)
!          goto 99                                         !--> ERROR
!        endif

# if defined CH_CARBON_DEPTH
!
! PH_DEPTH
!
        lvar=lenstr(vname(1,indxPHd_rst))
        ierr=nf_inq_varid (ncid, vname(1,indxPHd_rst)(1:lvar), rstPHd)
        if (ierr. ne. nf_noerr) then
          write(stdout,1) vname(1,indxPHd_rst)(1:lvar), rstname(1:lstr)
          goto 99                                         !--> ERROR
        endif
!
! PH
!
!        lvar=lenstr(vname(1,indxPH_rst))
!        ierr=nf_inq_varid (ncid, vname(1,indxPH_rst)(1:lvar), rstPH)
!        if (ierr. ne. nf_noerr) then
!          write(stdout,1) vname(1,indxPH_rst)(1:lvar), rstname(1:lstr)
!          goto 99                                         !--> ERROR
!        endif
!#else

#endif CH_CARBON_DEPTH

!
! PH
!
        lvar=lenstr(vname(1,indxPH_rst))
        ierr=nf_inq_varid (ncid, vname(1,indxPH_rst)(1:lvar), rstPH)
        if (ierr. ne. nf_noerr) then
          write(stdout,1) vname(1,indxPH_rst)(1:lvar), rstname(1:lstr)
          goto 99                                         !--> ERROR
        endif
!
! pCO2:
!
        lvar=lenstr(vname(1,indxPCO2_rst))
        ierr=nf_inq_varid (ncid, vname(1,indxPCO2_rst)(1:lvar), rstPCO2)
        if (ierr. ne. nf_noerr) then
          write(stdout,1) vname(1,indxPCO2_rst)(1:lvar), rstname(1:lstr)
          goto 99                                         !--> ERROR
        endif

# ifdef OUTPUT_PCO2AIR_RST
!
! pCO2air:
!
        lvar=lenstr(vname(1,indxPCO2air_rst))
        ierr=nf_inq_varid (ncid, vname(1,indxPCO2air_rst)(1:lvar), rstPCO2air)
        if (ierr. ne. nf_noerr) then
          write(stdout,1) vname(1,indxPCO2air_rst)(1:lvar), rstname(1:lstr)
          goto 99                                         !--> ERROR
        endif
# endif /* OUTPUT_PCO2AIR_RST */
# endif /* BIOLOGY_BEC || (BIOLOGY_NPZDOC && OXYGEN && CARBON) */

# if defined BIOLOGY_BEC || defined BIOLOGY_NPZDOC
!
! PAR:
!
        lvar=lenstr(vname(1,indxPAR_rst))
        ierr=nf_inq_varid (ncid, vname(1,indxPAR_rst)(1:lvar), rstPAR)
        if (ierr. ne. nf_noerr) then
          write(stdout,1) vname(1,indxPAR_rst)(1:lvar), rstname(1:lstr)
          goto 99                                         !--> ERROR
        endif
# endif /* BIOLOGY_BEC || BIOLOGY_NPZDOC */

# ifdef LMD_KPP
        lvar=lenstr(vname(1,indxHbl))
        ierr=nf_inq_varid (ncid, vname(1,indxHbl), rstHbl)
        if (ierr. ne. nf_noerr) then
          write(stdout,1) vname(1,indxHbl)(1:lvar), rstname(1:lstr)
          goto 99                                         !--> ERROR
        endif
# endif

# ifdef LMD_BKPP
        lvar=lenstr(vname(1,indxHbbl))
        ierr=nf_inq_varid (ncid, vname(1,indxHbbl), rstHbbl)
        if (ierr. ne. nf_noerr) then
          write(stdout,1) vname(1,indxHbbl)(1:lvar), rstname(1:lstr)
          goto 99                                         !--> ERROR
        endif
# endif
#endif


        write(*,'(6x,2A,i4,1x,A,i4)') 'DEF_RST -- Opened ',
     &             'existing restart file,  record =', rec
     &              MYID
 
#if defined MPI && !defined PARALLEL_FILES
      else
        ierr=nf_open (rstname(1:lstr), nf_write, ncid)
        if (ierr .ne. nf_noerr) then
          write(stdout,'(/1x,4A,1x,A,I4/)') 'DEF_RST ERROR: Cannot',
     &         'open restart netCDF file ''', rstname(1:lstr), '''.'
     &          MYID
          goto 99                                         !--> ERROR
        endif
#endif
      endif              !<-- create_new_file
   1  format(/1x,'DEF_RST ERROR: Cannot find variable ''',
     &               A, ''' in netCDF file ''', A, '''.'/)
#ifdef PUT_GRID_INTO_RESTART
!
! Write grid variables.
! ----- ---- ----------
!
      if (total_rec.eq.1) call wrt_grid (ncid, rstname, lstr)
#endif
  99  return                                              !--> ERROR
      end

!
! The following is just a instrumented wrapped-around of nf_def_dim
! designed to print out an error message, if something goes wrong.
! This is needed because there is no error handling code in the
! calling routine. 
!
      function my_nf_def_dim (ncid, name, len, dimid)
      implicit none
      integer my_nf_def_dim, ncid, len, dimid, ierr
      character name*(*)
# include "param.h"
# ifdef MPI
#  include "scalars.h"
# endif
# include "netcdf.inc"
      ierr=nf_def_dim (ncid, name, len, dimid)
      if (ierr.ne.nf_noerr) then
        write(stdout,'(/1x,A,I3,3A,I4,A,I10/12x,A,I3,3A,I3/)')
     &       '#### ERROR: nf_def_dim: ncid =', ncid, ' name = ''',
     &       name,  ''' len =', len, ' dimid =', dimid, '  ierr=',
     &       ierr, ' Cause of error: ', nf_strerror(ierr) MYID
      endif 
      my_nf_def_dim=ierr
      return
      end

 
