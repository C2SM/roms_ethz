#ifndef AVRH
#include "cppdefs.h"
#endif
#if defined BIOLOGY_BEC && defined BGC_FLUX_ANALYSIS
#ifdef AVRH
       subroutine def_bec_flux_avg(ncid,total_rec,ierr)
#else
       subroutine def_bec_flux_his(ncid,total_rec,ierr)
#endif
       implicit none
#include "param.h"
#include "ecosys_bec.h"
#include "netcdf.inc"
#include "ncvars.h"
#include "scalars.h"
#include "ncroms_bgcflux.h"

      logical create_new_file
      integer ncid, total_rec, ierr, rec, lstr,lvar,lenstr, timedim
     &      , r2dgrd(3), checkdims,auxil(2)
     &       , r3dgrd(4), itrc
      character*80 text

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
# define vid_bec_flux vid_bec_flux_his
# define vidTime bgc_flux_hisTime
# define vidTstep bgc_flux_hisTstep
# define vidZ bgc_flux_hisZ
#else /* AVRH */
# define ncname bgc_flux_avg_name
# define rec_per_file nrpf_bgc_flux_avg
# define vid_bec_flux vid_bec_flux_avg
# define vidTime bgc_flux_avgTime
# define vidTstep bgc_flux_avgTstep
# define vidZ bgc_flux_avgZ
#endif

      ierr=0
      lstr=lenstr(ncname)
      if (rec_per_file.gt.0) then
        lvar=total_rec-(1+mod(total_rec-1, rec_per_file))
        call insert_time_index (ncname, lstr, lvar, ierr)
        if (ierr .ne. 0) goto 99
      endif

#ifdef AVRH
      create_new_file = new_bgc_flux_avg
#else
      create_new_file = new_bgc_flux_his
#endif

        if (ncid.ne.-1) create_new_file=.false.
#if defined MPI & !defined PARALLEL_FILES
      if (mynode.gt.0) create_new_file=.false.
#endif

  10  if (create_new_file) then
#ifdef LARGE_BGC_FILES
        ierr=nf_create(ncname(1:lstr), or(nf_clobber,NCFILEFORMAT),
     &        ncid)
#else
        ierr=nf_create(ncname(1:lstr), nf_clobber, ncid)
#endif
        if (ierr .ne. nf_noerr) then
          write(stdout,'(/3(1x,A)/)') 'ERROR in wrt_bec_flux_his:',
     &           'Cannot create netCDF file:', ncname(1:lstr)
          goto 99                                         !--> ERROR
        endif
        if (rec_per_file.eq.0) total_rec=0  

!
! Put global attributes.
! --- ------ -----------
!
        call put_global_atts (ncid, ierr)

         ierr=nf_def_dim (ncid, 'xi_rho',   xi_rho,  r2dgrd(1))
        ierr=nf_def_dim (ncid, 'eta_rho',  eta_rho,  r2dgrd(2))
        ierr=nf_def_dim (ncid, 's_rho',    N,        r3dgrd(3))
        ierr=nf_def_dim (ncid, 'time', nf_unlimited, timedim)
        ierr=nf_def_dim (ncid, 'auxil',    4,        auxil(1))
        auxil(2)=timedim

        r2dgrd(3)=timedim           
        r3dgrd(1)=r2dgrd(1)         !
        r3dgrd(2)=r2dgrd(2)         ! 3D RHO-type
        r3dgrd(4)=timedim           !

#if (defined PUT_GRID_INTO_HISTORY && !defined AVRH)\
 || (defined PUT_GRID_INTO_AVERAGES && defined AVRH)
!
! Define grid variables.
! ------ ---- ----------
!
        if (total_rec.le.1) call def_grid (ncid, r2dgrd)
#endif

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
 
        do itrc=1,num_bgcflux_2d
          lvar=lenstr(vname_bgcflux(1,itrc))
          ierr=nf_def_var (ncid, vname_bgcflux(1,itrc)(1:lvar),
     &                             NF_FOUT, 3, r2dgrd, 
     &         vid_bec_flux(itrc))
          lvar=lenstr(vname_bgcflux(2,itrc))
          ierr=nf_put_att_text (ncid, vid_bec_flux(itrc), 
     &         'long_name', lvar,
     &         vname_bgcflux(2,itrc)(1:lvar))
          lvar=lenstr(vname_bgcflux(3,itrc))
          ierr=nf_put_att_text (ncid, vid_bec_flux(itrc), 'units',
     &         lvar, vname_bgcflux(3,itrc)(1:lvar))
          lvar=lenstr(vname_bgcflux(4,itrc))
!          ierr=nf_put_att_text (ncid, vid_bec_flux(itrc), 'field',  
!     &         lvar, vname_bgcflux(4,itrc)(1:lvar))
#ifdef MASK_LAND_DATA
          ierr=nf_put_att_FOUT (ncid, vid_bec_flux(itrc), '_FillValue',
     &         NF_FOUT, 1, spv_set)
#endif
         
        enddo
       
        do itrc=num_bgcflux_2d + 1,num_bgcflux
          lvar=lenstr(vname_bgcflux(1,itrc))
          ierr=nf_def_var (ncid, vname_bgcflux(1,itrc)(1:lvar),
     &         NF_FOUT, 4, r3dgrd, vid_bec_flux(itrc))

          lvar=lenstr(vname_bgcflux(2,itrc))
          ierr=nf_put_att_text (ncid, vid_bec_flux(itrc),
     &         'long_name',lvar, vname_bgcflux(2,itrc)(1:lvar))
          lvar=lenstr(vname_bgcflux(3,itrc))
          ierr=nf_put_att_text (ncid, vid_bec_flux(itrc), 'units',
     &         lvar,vname_bgcflux(3,itrc)(1:lvar))
          lvar=lenstr(vname_bgcflux(4,itrc))
!          ierr=nf_put_att_text (ncid, vid_bec_flux(itrc), 'field',  
!     &         lvar, vname_bgcflux(4,itrc)(1:lvar))
#ifdef MASK_LAND_DATA
          ierr=nf_put_att_FOUT (ncid, vid_bec_flux(itrc), '_FillValue',
     &         NF_FOUT, 1, spv_set)
#endif
         
        enddo
       
        ierr=nf_enddef(ncid)

#ifdef AVRH
        write(stdout,'(6x,4A,1x,A,i4)') 'DEF_BEC_FLUX_AVG created ',
#else
        write(stdout,'(6x,4A,1x,A,i4)') 'DEF_BEC_FLUX_HIS created ',
#endif
     &                'new netCDF file ''', 
     &                 ncname(1:lstr), '''.' MYID

      elseif (ncid.eq.-1) then
        ierr=nf_open (ncname(1:lstr), nf_write, ncid)
        if (ierr. eq. nf_noerr) then
          ierr=checkdims (ncid, ncname(1:lstr), lstr, rec) 
          if (ierr .eq. nf_noerr) then
            if (nrpf_bgc_flux_his.eq.0) then
              ierr=rec+1 - total_rec
            else
              ierr=rec+1 - (1+mod(total_rec-1, nrpf_bgc_flux_his))
            endif
            if (ierr.gt.0) then
              MPI_master_only write( stdout,
     &                 '(/1x,A,I5,1x,A/8x,3A,I5,/8x,A,I5,1x,A/)'
     &          ) 'DEF_BEC_FLUX  WARNING: Actual number of records',
     &               rec,  'in netCDF file',  '''',  
     &              ncname(1:lstr),
     &             ''' exceeds the record number from restart data',
     &             rec+1-ierr,'/', total_rec,', restart is assumed.'
              rec=rec-ierr
            elseif (nrpf_bgc_flux_his.eq.0) then
              total_rec=rec+1           ! <-- set to the next record
#if defined MPI & !defined PARALLEL_FILES
              if (mynode.gt.0) total_rec=total_rec-1
#endif
            endif
            ierr=nf_noerr
          endif
        endif

        if (ierr. ne. nf_noerr) then
#if defined MPI & !defined PARALLEL_FILES
          if (mynode.eq.0) then
            create_new_file=.true.
            goto 10
          else
            write(stdout,'(/1x,4A,2x,A,I4/)') 
     &            'DEF_BEC_FLUX ERROR: ',
     &                  'Cannot open file ''', 
     &                   ncname(1:lstr), '''.' MYID
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

         do itrc=1,num_bgcflux
          lvar=lenstr(vname_bgcflux(1,itrc))
          ierr=nf_inq_varid (ncid, vname_bgcflux(1,itrc)(1:lvar), 
     &           vid_bec_flux(itrc))
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), 
     &            ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
         enddo
        write(*,'(6x,2A,i4,1x,A,i4)') 'DEF_BEC_FLUX -- Opened ',
     &                     'existing file  from record =', rec
     &                      MYID

#if defined MPI & !defined PARALLEL_FILES
      else
        ierr=nf_open (ncname(1:lstr), nf_write, ncid)
        if (ierr .ne. nf_noerr) then
          write(stdout,'(/1x,4A,2x,A,I4/)') 'DEF_BEC_FLUX ERROR: ',
     &                'Cannot open file ''', 
     &                 ncname(1:lstr), '''.' MYID
          goto 99                                         !--> ERROR
        endif
#endif
      endif             !<--  create_new_file  
      
      ierr=nf_set_fill (ncid, nf_nofill, lvar)
      if (ierr .ne. nf_noerr) then
        write(*,'(6x,2A,i4,1x,A,i4)') 'DEF_BEC_FLUX ERROR: Cannot ',
     &    'switch to ''nf_nofill'' more; netCDF error code =', ierr 
      endif
   1  format(/1x,'DEF_BEC_FLUX ERROR: Cannot find variable ''',
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
#undef vid_bec_flux
#undef vidTime
#undef vidTstep
#undef vidZ

# ifndef AVRH
#  ifdef AVERAGES      
#   define AVRH
#   include "def_bec_flux.F"
#  endif /* AVERAGES */
# endif

#else  /* BIOLOGY_BEC && BGC_FLUX_ANALYSIS */
       subroutine def_bec_flux_empty ()
       
         end
#endif  /* BIOLOGY_BEC && BGC_FLUX_ANALYSIS */
 
