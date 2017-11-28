#include "cppdefs.h"
#if defined BIOLOGY_BEC && defined BGC_FLUX_ANALYSIS && defined AVERAGES

       subroutine wrt_bec_flux_avg
       implicit none
#include "param.h"
#include "ncroms_bgcflux.h"
#include "bec_avg.h"
#include "netcdf.inc"
#include "scalars.h"
!for iic
#include "ncvars.h"

      integer ierr,  record, lstr, lvar, lenstr,itrc
     &   , start(2), count(2), ibuff(4), ncf_fwrite
     &   ,varid
      REAL,DIMENSION(GLOBAL_2D_ARRAY,1)::WORK
   
#if defined MPI && !defined PARALLEL_FILES
# include "mpif.h"
      integer status(MPI_STATUS_SIZE), blank
      if (mynode.gt.0) call MPI_Recv (blank, 1, MPI_INTEGER,
     &            mynode-1, 4, MPI_COMM_WORLD, status, ierr)
#ifdef ASCII_OUTPUT
      write(stdout,*)"bec_flux_avg recv,mynode,blank",mynode,blank
#endif
#endif

      call def_bec_flux_avg (ncid_bgc_flux_avg, nrec_bgc_flux_avg, ierr)
      if (ierr .ne. nf_noerr) goto 99
      lstr=lenstr(bgc_flux_avg_name)
!                                                !!! WARNING: Here it is
! Set record within the file.                    !!! assumed that global
!                                                !!! restart record index
      nrec_bgc_flux_avg=max(nrec_bgc_flux_avg,1) !!! that global avg
      if (nrpf_bgc_flux_avg.eq.0) then           !!! record index is
        record=nrec_bgc_flux_avg                 !!! advanced by main.  
      else
        record=1+mod(nrec_bgc_flux_avg-1, nrpf_bgc_flux_avg)
      endif
!
! Time step and record indices.
!
      ibuff(1)=iic
      ibuff(2)=nrecrst
      ibuff(3)=nrechis
      ibuff(4)=nrec_bgc_flux_avg

      start(1)=1
      start(2)=record
      count(1)=4
      count(2)=1
      ierr=nf_put_vara_int(ncid_bgc_flux_avg, bgc_flux_avgTstep,
     &     start, count, ibuff)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'time_step', record,ierr MYID
        goto 99                                           !--> ERROR
      endif
!
! Averaged time
!
      ierr=nf_put_var1_double (ncid_bgc_flux_avg, bgc_flux_avgTime,
     &     record, time_bgc_flux_avg)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxTime))
        write(stdout,1) vname(1,indxTime)(1:lvar), record, ierr
     &                  MYID
        goto 99                                           !--> ERROR
      endif

! Barotropic mode variable: free-surface
! Always include this in the output so that depths of sigma levels
! can be computed
      ierr=ncf_fwrite (zeta_bgc_flux_avg(START_2D_ARRAY), 
     &     ncid_bgc_flux_avg, bgc_flux_avgZ,record, r_var, 1)
      if (ierr .ne. nf_noerr) then
         lvar=lenstr(vname(1,indxZ))
         write(stdout,1) vname(1,indxZ)(1:lvar), record, ierr
     &        MYID
         goto 99                !--> ERROR
      endif

       itrc=1
          ierr=ncf_fwrite(ws_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
     &                 vid_bec_flux_avg(itrc), record, r_var, 1)
            if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
         

       itrc=itrc+1
          ierr=ncf_fwrite(xkw_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r_var, 1)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
          ierr=ncf_fwrite(ap_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r_var, 1)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=ncf_fwrite(schmidt_o2_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r_var, 1)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       

        itrc=itrc+1
          ierr=ncf_fwrite(o2sat_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r_var, 1)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
          ierr=ncf_fwrite(fg_o2_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r_var, 1)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif


# ifdef NOT_USED___NEEDS_RECODING_IF_USED
       itrc=itrc+1
          ierr=ncf_fwrite(ph_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r_var, 1)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=ncf_fwrite(pco2surf_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r_var, 1)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
          ierr=ncf_fwrite(dpco2_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r_var, 1)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
# endif /* NOT_USED___NEEDS_RECODING_IF_USED */

       itrc=itrc+1
       ierr=ncf_fwrite(schmidt_co2_avg(START_2D_ARRAY),ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r_var, 1)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
          ierr=ncf_fwrite(co2star_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r_var, 1)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=ncf_fwrite(dco2star_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r_var, 1)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
          ierr=ncf_fwrite(fg_co2_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r_var, 1)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=ncf_fwrite(iron_flux_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r_var, 1)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=ncf_fwrite(PARinc_flux_avg(START_2D_ARRAY),ncid_bgc_flux_avg,
     &      vid_bec_flux_avg(itrc), record, r_var, 1)
       if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname_bgcflux(1,itrc))
          write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &         ierr MYID
          goto 99               !--> ERROR
       endif

!write 3d fluxes

       itrc=itrc+1
       ierr=ncf_fwrite(po4_restore_avg(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_avg, vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=ncf_fwrite(no3_restore_avg(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=ncf_fwrite(sio3_restore_avg(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

        itrc=itrc+1
        ierr=ncf_fwrite(par_flux_avg(START_2D_ARRAY,1),ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=ncf_fwrite(po4star_avg(START_2D_ARRAY,1), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=ncf_fwrite(poc_flux_in_avg(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

        itrc=itrc+1
          ierr=ncf_fwrite(poc_prod_avg(START_2D_ARRAY,1), 
     &       ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
       itrc=itrc+1
          ierr=ncf_fwrite(poc_remin_avg(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
          ierr=ncf_fwrite(caco3_flux_in_avg(START_2D_ARRAY,1),
     &      ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=ncf_fwrite(caco3_prod_avg(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
          ierr=ncf_fwrite(caco3_remin_avg(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
          ierr=ncf_fwrite(sio2_flux_in_avg(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
        itrc=itrc+1
          ierr=ncf_fwrite(sio2_prod_avg(START_2D_ARRAY,1), 
     &       ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
       itrc=itrc+1
       ierr=ncf_fwrite(sio2_remin_avg(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=ncf_fwrite(dust_flux_in_avg(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
         itrc=itrc+1
         ierr=ncf_fwrite(dust_remin_avg(START_2D_ARRAY,1), 
     &        ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(p_iron_flux_in_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
        itrc=itrc+1
        ierr=ncf_fwrite(p_iron_prod_avg(START_2D_ARRAY,1), 
     &       ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(p_iron_remin_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(graze_sp_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(graze_diat_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(graze_tot_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(sp_loss_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(diat_loss_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(zoo_loss_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
       itrc=itrc+1
       ierr=ncf_fwrite(sp_agg_avg(START_2D_ARRAY,1), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(diat_agg_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(photoc_sp_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(photoc_diat_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(tot_prod_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(doc_prod_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(doc_remin_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(fe_scavenge_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(sp_n_lim_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(sp_fe_lim_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(sp_po4_lim_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(sp_light_lim_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(diat_n_lim_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(diat_fe_lim_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(diat_po4_lim_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(diat_sio3_lim_avg(START_2D_ARRAY,1),
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(diat_light_lim_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(caco3_form_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(diaz_nfix_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(graze_diaz_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(diaz_loss_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(photoc_diaz_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(diaz_p_lim_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(diaz_fe_lim_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(diaz_light_lim_avg(START_2D_ARRAY,1),
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(fe_scavenge_rate_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(don_prod_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
           ierr=ncf_fwrite(don_remin_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(dofe_prod_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(dofe_remin_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(dop_prod_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
           ierr=ncf_fwrite(dop_remin_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(bsi_form_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(photofe_diaz_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(photofe_diat_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(photofe_sp_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(nitrif_avg(START_2D_ARRAY,1),ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
#ifdef OXYLIM_BEC
          itrc=itrc+1
          ierr=ncf_fwrite(denitr_DOC_avg(START_2D_ARRAY,1),ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=ncf_fwrite(denitr_POC_avg(START_2D_ARRAY,1),ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r_var, N)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
#endif

  1   format(/1x,'WRT_bec_flux_avg ERROR while writing variable ''', A,
     &          ''' into avg file.', /11x, 'Time record:',
     &                   I6,3x,'netCDF error code',i4,3x,a,i4)
       goto 100 
  99  may_day_flag=3
 100  continue
!
! Synchronize netCDF file to disk to allow other processes
! to access data immediately after it is written.
!
#if defined MPI & !defined PARALLEL_FILES
      ierr=nf_close (ncid_bgc_flux_avg)
      if (nrpf_bgc_flux_avg.gt.0 .and. record.ge.nrpf_bgc_flux_avg) 
     &     ncid_bgc_flux_avg=-1
#else
      if (nrpf_bgc_flux_avg.gt.0 .and. record.ge.nrpf_bgc_flux_avg) then
        ierr=nf_close (ncid_bgc_flux_avg)
        ncid_bgc_flux_avg=-1
      else
        ierr=nf_sync(ncid_bgc_flux_avg)
      endif
#endif
      if (ierr .eq. nf_noerr) then
        MPI_master_only write(stdout,'(6x,A,2(A,I4,1x),A,I3)') 
     &             'WRT_BEC_FLUX_AVG -- wrote ',
     &            'avg fields into time record =', record, '/',
     &             nrec_bgc_flux_avg  MYID
      else
        write(stdout,'(/1x,2A/)') 'WRT_BEC_FLUX_AVG ERROR: Cannot ',
     &             'synchronize/close avg netCDF file.'
        may_day_flag=3
      endif

#if defined MPI & !defined PARALLEL_FILES
      if (mynode .lt. NNODES-1) then
         blank=40+mynode
        call MPI_Send (blank, 1, MPI_INTEGER, mynode+1,
     &                        4, MPI_COMM_WORLD,  ierr)
#ifdef ASCII_OUTPUT
        write(stdout,*)"wrt_bec_flux_avg send,mynode,blank",mynode,blank
#endif
      endif
#endif

      return
       end subroutine wrt_bec_flux_avg
#else
       subroutine ecosys_wrt_bec_flux_avg_empty ()
       
         end
#endif  /* BIOLOGY_BEC && BGC_FLUX_ANALYSIS && AVERAGES */
