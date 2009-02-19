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
     &   , start(2), count(2), ibuff(4), nf_fwrite
     &   ,varid
      REAL,DIMENSION(GLOBAL_2D_ARRAY,1)::WORK
      real cff
   
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

      start(1)=1
      start(2)=record
      count(1)=4
      count(2)=1
      cff=1./float(n_bgc_flux_avg)


         time_bgc_flux_avg = time_bgc_flux_avg * cff
         WS_AVG = WS_AVG * cff
         XKW_AVG = XKW_AVG * cff
         ap_AVG = ap_AVG * cff
         SCHMIDT_O2_AVG = SCHMIDT_O2_AVG* cff
         O2SAT_AVG = O2SAT_AVG* cff 
         FG_O2_AVG = FG_O2_AVG * cff
# ifdef NOT_USED___NEEDS_RECODING_IF_USED
         PH_AVG =  PH_AVG * cff 
         pCO2SURF_AVG = pCO2SURF_AVG * cff
         DpCO2_AVG = DpCO2_AVG * cff
# endif /* NOT_USED___NEEDS_RECODING_IF_USED */
         SCHMIDT_CO2_AVG = SCHMIDT_CO2_AVG * cff
         CO2STAR_AVG = CO2STAR_AVG * cff
         DCO2STAR_AVG = DCO2STAR_AVG * cff
         FG_CO2_AVG = FG_CO2_AVG * cff
         IRON_FLUX_AVG = IRON_FLUX_AVG * cff
         PARinc_flux_avg = PARinc_flux_avg * cff
         PO4_RESTORE_AVG = PO4_RESTORE_AVG * cff
         NO3_RESTORE_AVG = NO3_RESTORE_AVG * cff
         SiO3_RESTORE_AVG = SiO3_RESTORE_AVG * cff
         PAR_flux_AVG = PAR_flux_AVG * cff
         PO4STAR_AVG = PO4STAR_AVG * cff
         POC_FLUX_IN_AVG = POC_FLUX_IN_AVG * cff
         POC_PROD_AVG = POC_PROD_AVG * cff
         POC_REMIN_AVG = POC_REMIN_AVG * cff
         CaCO3_FLUX_IN_AVG = CaCO3_FLUX_IN_AVG * cff
         CaCO3_PROD_AVG = CaCO3_PROD_AVG * cff
         CaCO3_REMIN_AVG =CaCO3_REMIN_AVG * cff
         SiO2_FLUX_IN_AVG = SiO2_FLUX_IN_AVG * cff
         SiO2_PROD_AVG = SiO2_PROD_AVG * cff
         SiO2_REMIN_AVG = SiO2_REMIN_AVG * cff
         dust_FLUX_IN_AVG = dust_FLUX_IN_AVG * cff
         dust_REMIN_AVG = dust_REMIN_AVG * cff
         P_iron_FLUX_IN_AVG = P_iron_FLUX_IN_AVG * cff
         P_iron_PROD_AVG = P_iron_PROD_AVG * cff
         P_iron_REMIN_AVG = P_iron_REMIN_AVG * cff
         graze_sp_AVG = graze_sp_AVG * cff
         graze_diat_AVG =  graze_diat_AVG * cff
         graze_tot_AVG = graze_tot_AVG * cff
         sp_loss_AVG = sp_loss_AVG * cff
         diat_loss_AVG = diat_loss_AVG * cff
         zoo_loss_AVG = zoo_loss_AVG * cff
         sp_agg_AVG = sp_agg_AVG * cff
         diat_agg_AVG = diat_agg_AVG * cff
         photoC_sp_AVG =  photoC_sp_AVG * cff
         photoC_diat_AVG = photoC_diat_AVG * cff
         tot_prod_AVG = tot_prod_AVG * cff
          DOC_prod_AVG =  DOC_prod_AVG * cff
         DOC_remin_AVG = DOC_remin_AVG * cff
         Fe_scavenge_AVG = Fe_scavenge_AVG * cff
         sp_N_lim_AVG = sp_N_lim_AVG * cff
         sp_Fe_lim_AVG = sp_Fe_lim_AVG * cff
         sp_PO4_lim_AVG = sp_PO4_lim_AVG * cff
         sp_light_lim_AVG = sp_light_lim_AVG * cff
         diat_N_lim_AVG = diat_N_lim_AVG * cff
         diat_Fe_lim_AVG = diat_Fe_lim_AVG * cff
         diat_PO4_lim_AVG = diat_PO4_lim_AVG * cff
         diat_SiO3_lim_AVG =  diat_SiO3_lim_AVG * cff 
         diat_light_lim_AVG = diat_light_lim_AVG * cff
         CaCO3_form_AVG = CaCO3_form_AVG * cff
         diaz_Nfix_AVG = diaz_Nfix_AVG * cff
         graze_diaz_AVG = graze_diaz_AVG * cff
         diaz_loss_AVG = diaz_loss_AVG * cff 
         photoC_diaz_AVG = photoC_diaz_AVG * cff 
         diaz_P_lim_AVG =diaz_P_lim_AVG * cff
         diaz_Fe_lim_AVG = diaz_Fe_lim_AVG * cff
         diaz_light_lim_AVG = diaz_light_lim_AVG * cff
         Fe_scavenge_rate_AVG = Fe_scavenge_rate_AVG * cff
         DON_prod_AVG = DON_prod_AVG * cff 
         DON_remin_AVG = DON_remin_AVG * cff 
         DOFe_prod_AVG = DOFe_prod_AVG * cff 
         DOFe_remin_AVG = DOFe_remin_AVG * cff
         DOP_prod_AVG = DOP_prod_AVG * cff 
         DOP_remin_AVG = DOP_remin_AVG * cff 
         bSI_form_AVG = bSI_form_AVG * cff
         photoFe_diaz_AVG = photoFe_diaz_AVG * cff
         photoFe_diat_AVG = photoFe_diat_AVG * cff
         photoFe_sp_AVG = photoFe_sp_AVG * cff
         nitrif_AVG = nitrif_AVG * cff


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
      ierr=nf_fwrite (zeta_bgc_flux_avg(START_2D_ARRAY), 
     &     ncid_bgc_flux_avg, bgc_flux_avgZ,record, r2dvar)
      if (ierr .ne. nf_noerr) then
         lvar=lenstr(vname(1,indxZ))
         write(stdout,1) vname(1,indxZ)(1:lvar), record, ierr
     &        MYID
         goto 99                !--> ERROR
      endif

       itrc=1
          ierr=nf_fwrite(ws_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
     &                 vid_bec_flux_avg(itrc), record, r2dvar)
            if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
         

       itrc=itrc+1
          ierr=nf_fwrite(xkw_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
          ierr=nf_fwrite(ap_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=nf_fwrite(schmidt_o2_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       

        itrc=itrc+1
          ierr=nf_fwrite(o2sat_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
          ierr=nf_fwrite(fg_o2_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif


# ifdef NOT_USED___NEEDS_RECODING_IF_USED
       itrc=itrc+1
          ierr=nf_fwrite(ph_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=nf_fwrite(pco2surf_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
          ierr=nf_fwrite(dpco2_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
# endif /* NOT_USED___NEEDS_RECODING_IF_USED */

       itrc=itrc+1
       ierr=nf_fwrite(schmidt_co2_avg(START_2D_ARRAY),ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
          ierr=nf_fwrite(co2star_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=nf_fwrite(dco2star_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
          ierr=nf_fwrite(fg_co2_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=nf_fwrite(iron_flux_avg(START_2D_ARRAY), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=nf_fwrite(PARinc_flux_avg(START_2D_ARRAY),ncid_bgc_flux_avg,
     &      vid_bec_flux_avg(itrc), record, r2dvar)
       if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname_bgcflux(1,itrc))
          write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &         ierr MYID
          goto 99               !--> ERROR
       endif

!write 3d fluxes

       itrc=itrc+1
       ierr=nf_fwrite(po4_restore_avg(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_avg, vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=nf_fwrite(no3_restore_avg(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=nf_fwrite(sio3_restore_avg(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

        itrc=itrc+1
        ierr=nf_fwrite(par_flux_avg(START_2D_ARRAY,1),ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=nf_fwrite(po4star_avg(START_2D_ARRAY,1), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=nf_fwrite(poc_flux_in_avg(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

        itrc=itrc+1
          ierr=nf_fwrite(poc_prod_avg(START_2D_ARRAY,1), 
     &       ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
       itrc=itrc+1
          ierr=nf_fwrite(poc_remin_avg(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
          ierr=nf_fwrite(caco3_flux_in_avg(START_2D_ARRAY,1),
     &      ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=nf_fwrite(caco3_prod_avg(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
          ierr=nf_fwrite(caco3_remin_avg(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
          ierr=nf_fwrite(sio2_flux_in_avg(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
        itrc=itrc+1
          ierr=nf_fwrite(sio2_prod_avg(START_2D_ARRAY,1), 
     &       ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
       itrc=itrc+1
       ierr=nf_fwrite(sio2_remin_avg(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif

       itrc=itrc+1
       ierr=nf_fwrite(dust_flux_in_avg(START_2D_ARRAY,1), 
     &      ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
         itrc=itrc+1
         ierr=nf_fwrite(dust_remin_avg(START_2D_ARRAY,1), 
     &        ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(p_iron_flux_in_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
        itrc=itrc+1
        ierr=nf_fwrite(p_iron_prod_avg(START_2D_ARRAY,1), 
     &       ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(p_iron_remin_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(graze_sp_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(graze_diat_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(graze_tot_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(sp_loss_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(diat_loss_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(zoo_loss_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
       itrc=itrc+1
       ierr=nf_fwrite(sp_agg_avg(START_2D_ARRAY,1), ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(diat_agg_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(photoc_sp_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(photoc_diat_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(tot_prod_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(doc_prod_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(doc_remin_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(fe_scavenge_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(sp_n_lim_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(sp_fe_lim_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(sp_po4_lim_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(sp_light_lim_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(diat_n_lim_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(diat_fe_lim_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(diat_po4_lim_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(diat_sio3_lim_avg(START_2D_ARRAY,1),
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(diat_light_lim_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(caco3_form_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(diaz_nfix_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(graze_diaz_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(diaz_loss_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(photoc_diaz_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(diaz_p_lim_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(diaz_fe_lim_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(diaz_light_lim_avg(START_2D_ARRAY,1),
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(fe_scavenge_rate_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(don_prod_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
           ierr=nf_fwrite(don_remin_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(dofe_prod_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(dofe_remin_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(dop_prod_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
           ierr=nf_fwrite(dop_remin_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(bsi_form_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(photofe_diaz_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(photofe_diat_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(photofe_sp_avg(START_2D_ARRAY,1), 
     &         ncid_bgc_flux_avg,vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
          itrc=itrc+1
          ierr=nf_fwrite(nitrif_avg(START_2D_ARRAY,1),ncid_bgc_flux_avg,
     &                          vid_bec_flux_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname_bgcflux(1,itrc))
            write(stdout,1) vname_bgcflux(1,itrc)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif


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


         time_bgc_flux_avg = 0.0
         WS_AVG = 0.0 
         XKW_AVG = 0.0 
         ap_AVG = 0.0 
         SCHMIDT_O2_AVG = 0.0 
         O2SAT_AVG = 0.0 
         FG_O2_AVG = 0.0 
# ifdef NOT_USED___NEEDS_RECODING_IF_USED
         PH_AVG = 0.0  
         pCO2SURF_AVG = 0.0
         DpCO2_AVG = 0.0
# endif /* NOT_USED___NEEDS_RECODING_IF_USED */
         SCHMIDT_CO2_AVG = 0.0 
         CO2STAR_AVG = 0.0 
         DCO2STAR_AVG = 0.0 
         FG_CO2_AVG = 0.0 
         IRON_FLUX_AVG = 0.0 
         PARinc_flux_avg = 0.0 
         PO4_RESTORE_AVG = 0.0 
         NO3_RESTORE_AVG = 0.0 
         SiO3_RESTORE_AVG = 0.0 
         PAR_flux_AVG = 0.0 
         PO4STAR_AVG = 0.0 
         POC_FLUX_IN_AVG = 0.0 
         POC_PROD_AVG = 0.0
         POC_REMIN_AVG = 0.0 
         CaCO3_FLUX_IN_AVG = 0.0
         CaCO3_PROD_AVG = 0.0 
         CaCO3_REMIN_AVG = 0.0
         SiO2_FLUX_IN_AVG = 0.0 
         SiO2_PROD_AVG = 0.0
         SiO2_REMIN_AVG = 0.0
         dust_FLUX_IN_AVG = 0.0 
         dust_REMIN_AVG = 0.0 
         P_iron_FLUX_IN_AVG = 0.0
         P_iron_PROD_AVG = 0.0 
         P_iron_REMIN_AVG = 0.0
         graze_sp_AVG = 0.0
         graze_diat_AVG = 0.0  
         graze_tot_AVG = 0.0 
         sp_loss_AVG = 0.0
         diat_loss_AVG = 0.0 
         zoo_loss_AVG = 0.0
         sp_agg_AVG = 0.0
         diat_agg_AVG = 0.0 
         photoC_sp_AVG = 0.0  
         photoC_diat_AVG = 0.0 
         tot_prod_AVG = 0.0
          DOC_prod_AVG = 0.0  
         DOC_remin_AVG = 0.0 
         Fe_scavenge_AVG = 0.0 
         sp_N_lim_AVG = 0.0 
         sp_Fe_lim_AVG = 0.0 
         sp_PO4_lim_AVG = 0.0 
         sp_light_lim_AVG = 0.0 
         diat_N_lim_AVG = 0.0
         diat_Fe_lim_AVG = 0.0 
         diat_PO4_lim_AVG = 0.0 
         diat_SiO3_lim_AVG = 0.0  
         diat_light_lim_AVG = 0.0
         CaCO3_form_AVG = 0.0
         diaz_Nfix_AVG = 0.0
         graze_diaz_AVG = 0.0
         diaz_loss_AVG = 0.0 
         photoC_diaz_AVG = 0.0
         diaz_P_lim_AVG = 0.0
         diaz_Fe_lim_AVG = 0.0 
         diaz_light_lim_AVG = 0.0
         Fe_scavenge_rate_AVG = 0.0
         DON_prod_AVG = 0.0 
         DON_remin_AVG = 0.0  
         DOFe_prod_AVG = 0.0 
         DOFe_remin_AVG = 0.0
         DOP_prod_AVG = 0.0 
         DOP_remin_AVG = 0.0 
         bSI_form_AVG = 0.0
         photoFe_diaz_AVG = 0.0
         photoFe_diat_AVG = 0.0
         photoFe_sp_AVG = 0.0 
         nitrif_AVG = 0.0

#ifdef ASCII_OUTPUT
         write(stdout,*) 'leaving wrt_bec_flux_avg, node ', mynode
#endif
      return
       end subroutine wrt_bec_flux_avg
#else
       subroutine ecosys_wrt_bec_flux_avg_empty ()
       
         end
#endif  /* BIOLOGY_BEC && BGC_FLUX_ANALYSIS && AVERAGES */
