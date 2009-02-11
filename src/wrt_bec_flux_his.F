#include "cppdefs.h"
#if defined BIOLOGY_BEC && defined BGC_FLUX_ANALYSIS
       subroutine wrt_bec_flux_his
       implicit none
#include "param.h"
#include "ecosys_bec.h"
#include "netcdf.inc"
#include "scalars.h"
#include "ocean2d.h"
!for iic
#include "ncvars.h"
#include "ncroms_bgcflux.h"

      integer ierr,  record, lstr, lvar, lenstr,itrc
     &   , start(2), count(2), ibuff(4), nf_fwrite
     &   ,varid
      REAL,DIMENSION(GLOBAL_2D_ARRAY,1)::WORK
#if defined MPI && !defined PARALLEL_FILES
# include "mpif.h"
      integer status(MPI_STATUS_SIZE), blank
      if (mynode.gt.0) call MPI_Recv (blank, 1, MPI_INTEGER,
     &            mynode-1, 3, MPI_COMM_WORLD, status, ierr)
!HF        write(stdout,*)"mynode,blank",mynode,blank
#endif
   
      call def_bec_flux_his (ncid_bgc_flux_his, nrec_bgc_flux_his, ierr)
      if (ierr .ne. nf_noerr) goto 99
      lstr=lenstr(bgc_flux_his_name)
!                                            !!! WARNING: Here it is
! Set record within the file.                !!! assumed that global
!                                            !!! restart record index
      nrec_bgc_flux_his=max(nrec_bgc_flux_his,1) !!! nrec_bgc_flux_his 
      if (nrpf_bgc_flux_his.eq.0) then               !!! record index is
        record=nrec_bgc_flux_his                    !!! advanced by main.  
      else
        record=1+mod(nrec_bgc_flux_his-1, nrpf_bgc_flux_his)
      endif

! Time step number and record numbers.

      ibuff(1)=iic
      ibuff(2)=nrecrst
      ibuff(3)=nrechis
      ibuff(4)=nrec_bgc_flux_his

      start(1)=1
      start(2)=record
      count(1)=4
      count(2)=1

      ierr=nf_put_vara_int(ncid_bgc_flux_his, bgc_flux_hisTstep, 
     &     start, count, ibuff)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'time_step', record,ierr MYID
        goto 99                                           !--> ERROR
      endif
!
! Time
!
      ierr=nf_put_var1_double (ncid_bgc_flux_his, bgc_flux_hisTime,  
     &     record, time)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxTime))
        write(stdout,1) vname(1,indxTime)(1:lvar), record, ierr
     &                  MYID
        goto 99                                           !--> ERROR
      endif

! Barotropic mode variable: free-surface
! Always include this in the output so that depths of sigma levels
! can be computed
      ierr=nf_fwrite (zeta(START_2D_ARRAY,fast_indx_out), 
     &     ncid_bgc_flux_his, bgc_flux_hisZ,record, r2dvar)
      if (ierr .ne. nf_noerr) then
         lvar=lenstr(vname(1,indxZ))
         write(stdout,1) vname(1,indxZ)(1:lvar), record, ierr
     &        MYID
         goto 99                !--> ERROR
      endif

       itrc=1
          ierr=nf_fwrite(ws_hist(START_2D_ARRAY), ncid_bgc_flux_his,
     &                 vid_bec_flux_his(itrc), record, r2dvar)
            if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
         

       itrc=itrc+1
          ierr=nf_fwrite(xkw_hist(START_2D_ARRAY), ncid_bgc_flux_his,
     &                          vid_bec_flux_his(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
          ierr=nf_fwrite(ap_hist(START_2D_ARRAY), ncid_bgc_flux_his,
     &                          vid_bec_flux_his(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=nf_fwrite(schmidt_o2_hist(START_2D_ARRAY),ncid_bgc_flux_his,
     &                          vid_bec_flux_his(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       

        itrc=itrc+1
          ierr=nf_fwrite(o2sat_hist(START_2D_ARRAY), ncid_bgc_flux_his,
     &                          vid_bec_flux_his(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
          ierr=nf_fwrite(fg_o2_hist(START_2D_ARRAY), ncid_bgc_flux_his,
     &                          vid_bec_flux_his(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif


# ifdef NOT_USED___NEEDS_RECODING_IF_USED
       itrc=itrc+1
          ierr=nf_fwrite(ph_hist(START_2D_ARRAY), ncid_bgc_flux_his,
     &                          vid_bec_flux_his(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=nf_fwrite(pco2surf_hist(START_2D_ARRAY), ncid_bgc_flux_his,
     &                          vid_bec_flux_his(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
       itrc=itrc+1
          ierr=nf_fwrite(dpco2_hist(START_2D_ARRAY), ncid_bgc_flux_his,
     &                          vid_bec_flux_his(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

# endif /* NOT_USED___NEEDS_RECODING_IF_USED */

       itrc=itrc+1
       ierr=nf_fwrite(schmidt_co2_hist(START_2D_ARRAY), 
     &      ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=nf_fwrite(co2star_hist(START_2D_ARRAY), ncid_bgc_flux_his,
     &                          vid_bec_flux_his(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=nf_fwrite(dco2star_hist(START_2D_ARRAY), ncid_bgc_flux_his,
     &                          vid_bec_flux_his(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
          ierr=nf_fwrite(fg_co2_hist(START_2D_ARRAY), ncid_bgc_flux_his,
     &                          vid_bec_flux_his(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=nf_fwrite(iron_flux_hist(START_2D_ARRAY), ncid_bgc_flux_his,
     &                          vid_bec_flux_his(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=nf_fwrite(PARinc(START_2D_ARRAY), ncid_bgc_flux_his,
     &      vid_bec_flux_his(itrc), record, r2dvar)
       if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname_bgcflux(1,itrc))
          write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &         ierr MYID
          goto 99               !--> ERROR
       endif
       
!write 3d flux snapshots

       itrc=itrc+1
       ierr=nf_fwrite(po4_restore_hist(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=nf_fwrite(no3_restore_hist(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=nf_fwrite(sio3_restore_hist(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

        itrc=itrc+1
        ierr=nf_fwrite(par(START_2D_ARRAY,1), 
     &       ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=nf_fwrite(po4star_hist(START_2D_ARRAY,1), ncid_bgc_flux_his,
     &                          vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=nf_fwrite(poc_flux_in_hist(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

        itrc=itrc+1
        ierr=nf_fwrite(poc_prod_hist(START_2D_ARRAY,1), 
     &       ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
       itrc=itrc+1
       ierr=nf_fwrite(poc_remin_hist(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=nf_fwrite(caco3_flux_in_hist(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=nf_fwrite(caco3_prod_hist(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=nf_fwrite(caco3_remin_hist(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=nf_fwrite(sio2_flux_in_hist(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
        itrc=itrc+1
        ierr=nf_fwrite(sio2_prod_hist(START_2D_ARRAY,1), 
     &       ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
       itrc=itrc+1
       ierr=nf_fwrite(sio2_remin_hist(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=nf_fwrite(dust_flux_in_hist(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
         itrc=itrc+1
         ierr=nf_fwrite(dust_remin_hist(START_2D_ARRAY,1), 
     &        ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(p_iron_flux_in_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
        itrc=itrc+1
        ierr=nf_fwrite(p_iron_prod_hist(START_2D_ARRAY,1), 
     &       ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(p_iron_remin_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(graze_sp_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(graze_diat_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(graze_tot_hist(START_2D_ARRAY,1),
     &         ncid_bgc_flux_his, vid_bec_flux_his(itrc), record,r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(sp_loss_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(diat_loss_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(zoo_loss_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
       itrc=itrc+1
          ierr=nf_fwrite(sp_agg_hist(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(diat_agg_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(photoc_sp_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(photoc_diat_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(tot_prod_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(doc_prod_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(doc_remin_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(fe_scavenge_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(sp_n_lim_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(sp_fe_lim_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(sp_po4_lim_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(sp_light_lim_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(diat_n_lim_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(diat_fe_lim_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(diat_po4_lim_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(diat_sio3_lim_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(diat_light_lim_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(caco3_form_hist(START_2D_ARRAY,1),
     &         ncid_bgc_flux_his, vid_bec_flux_his(itrc), record,r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(diaz_nfix_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(graze_diaz_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(diaz_loss_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(photoc_diaz_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(diaz_p_lim_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(diaz_fe_lim_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(diaz_light_lim_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(fe_scavenge_rate_hist(START_2D_ARRAY,1),
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(don_prod_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
           ierr=nf_fwrite(don_remin_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(dofe_prod_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(dofe_remin_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(dop_prod_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
           ierr=nf_fwrite(dop_remin_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(bsi_form_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(photofe_diaz_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(photofe_diat_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(photofe_sp_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(nitrif_hist(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_his,vid_bec_flux_his(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif


  1   format(/1x,'WRT_BEC_FLUX_HIS ERROR while writing variable ''', A,
     &          ''' into history file.', /11x, 'Time record:',
     &                   I6,3x,'netCDF error code',i4,3x,a,i4)
       goto 100 
  99  may_day_flag=3
 100  continue
!
! Synchronize netCDF file to disk to allow other processes
! to access data immediately after it is written.
!
#if defined MPI & !defined PARALLEL_FILES
      ierr=nf_close (ncid_bgc_flux_his)
      if (nrpf_bgc_flux_his.gt.0 .and. record.ge.nrpf_bgc_flux_his) 
     &     ncid_bgc_flux_his=-1
#else
      if (nrpf_bgc_flux_his.gt.0 .and. record.ge.nrpf_bgc_flux_his) then
        ierr=nf_close (ncid_bgc_flux_his)
        ncid_bgc_flux_his=-1
      else
        ierr=nf_sync(ncid_bgc_flux_his)
      endif
#endif
      if (ierr .eq. nf_noerr) then
        write(stdout,'(6x,A,2(A,I4,1x),A,I3)') 
     &        'WRT_BEC_FLUX_HIS -- wrote ',
     &            'history fields into time record =', record, '/',
     &             nrec_bgc_flux_his  MYID
      else
        write(stdout,'(/1x,2A/)') 'WRT_BEC_FLUX_HIS ERROR: Cannot ',
     &             'synchronize/close history netCDF file.'
        may_day_flag=3
      endif

#if defined MPI & !defined PARALLEL_FILES
      if (mynode .lt. NNODES-1) then
         blank=30+mynode
        call MPI_Send (blank, 1, MPI_INTEGER, mynode+1,
     &                        3, MPI_COMM_WORLD,  ierr)
      endif
#endif
      return
       end subroutine wrt_bec_flux_his
#else
       subroutine ecosys_wrt_bec_flux_his_empty ()
       
         end
#endif  /*BIOLOGY_BEC*/
 
