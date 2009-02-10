#include "cppdefs.h"
#if defined SOLVE3D &&( defined TCLIMATOLOGY ||( defined TNUDGING &&\
              defined T_FRC_BRY )) && !defined ANA_TCLIMA

 
                                       ! Read climatology fields for
      subroutine get_tclima (ierr)     ! tracers from climatological
      implicit none                    ! file at appropriate time.
# define TCLIMA_DATA
# include "param.h"
# include "scalars.h"
# include "climat.h"
# include "ncvars.h"
# include "netcdf.inc"
      real cff
      integer i,itrc, lstr,lvar,lenstr, ierr, nf_fread
# if defined BIOLOGY_NPZDOC || defined BIOLOGY_BEC
      integer lstr2, lstr3, lstr4
# endif
!# ifdef BIOLOGY_NPZDOC
!      integer lstr2, lstr3
!# endif
!
! Initialization: Check, whether climatological data file is already
!================ opened, and if not, open it.  Find and save netCDF
! IDs for relevant variables.  Determine whether there is cycling to
! reuse the input data and find cycling period "tclm_cycle", set
! initial cycling index "tclm_ncycle" and record index "tclm_rec",
! time index "itclm" and both time record bounds to large negative
! artificial values, so that it will trigger the logic in reading
! part below.
!
      ierr=nf_noerr
      lstr=lenstr(clm_file)
      if (iic.eq.ntstart) then
        if (ncidclm.eq.-1) then
          ierr=nf_open (clm_file(1:lstr), nf_nowrite, ncidclm)
          if (ierr .ne. nf_noerr) then
             write(stdout,'(/1x,4A/)') 'ERROR ',
     &            'in get_tclima: can not open netCDF file ''',
     &            clm_file(1:lstr),   '''.'
             goto 99
          end if
        endif
#if defined TWO_CLIM_FILES || defined THREE_CLIM_FILES || defined FOUR_CLIM_FILES
        if (ncidclm2.eq.-1) then
           lstr2=lenstr(clm_file2)
           ierr=nf_open (clm_file2(1:lstr2), nf_nowrite, ncidclm2)
           if (ierr .ne. nf_noerr) goto 22 !--> ERROR
        endif
#else
        ncidclm2 = ncidclm
        clm_file2 = clm_file
        lstr2 = lstr
#endif
#if defined THREE_CLIM_FILES || defined FOUR_CLIM_FILES
        if (ncidclm3.eq.-1) then
          lstr3=lenstr(clm_file3)
          ierr=nf_open (clm_file3(1:lstr3), nf_nowrite, ncidclm3)
          if (ierr .ne. nf_noerr) goto 33                !--> ERROR
        endif
#else
        ncidclm3 = ncidclm2
        clm_file3 = clm_file2
        lstr3 = lstr2
#endif
# ifdef FOUR_CLIM_FILES
        if (ncidclm4.eq.-1) then
          lstr4=lenstr(clm_file4)
          ierr=nf_open (clm_file4(1:lstr4), nf_nowrite, ncidclm4)
          if (ierr .ne. nf_noerr) goto 44
        endif
# else
#  ifdef BIOLOGY_BEC
        ncidclm4 = ncidclm3
        clm_file4 = clm_file3
        lstr4 = lstr3
#  endif
# endif /* FOUR_CLIM_FILES */

        do itrc=1,NT
          if (ierr.eq.nf_noerr) then
            if (itrc.eq.itemp) then
              ierr=nf_inq_varid (ncidclm,'tclm_time',tclm_tid(itrc))
              if (ierr .ne. nf_noerr) write(stdout,1) 'tclm_time',
     &                                         itrc, clm_file(1:lstr)
# ifdef SALINITY
            elseif (itrc.eq.isalt) then
              ierr=nf_inq_varid (ncidclm, 'sclm_time',tclm_tid(itrc))
              if (ierr .ne. nf_noerr) write(stdout,1) 'sclm_time',
     &                                         itrc, clm_file(1:lstr)
# endif
# ifdef BIOLOGY_NPZDOC
            elseif (itrc.eq.iNO3_) then
              ierr=nf_inq_varid (ncidclm2, 'no3_time', tclm_tid(itrc))
              if (ierr .ne. nf_noerr) then
                MPI_master_only write(stdout,3) 'no3_time', 
     &                itrc, clm_file2(1:lstr2)
                tclm_tid(itrc) = 0
                ierr = nf_noerr
              endif
            elseif (itrc.eq.iNH4_) then
              ierr=nf_inq_varid (ncidclm2, 'nh4_time', tclm_tid(itrc))
              if (ierr .ne. nf_noerr) then
                MPI_master_only write(stdout,3) 'nh4_time', 
     &                itrc, clm_file2(1:lstr2)
                tclm_tid(itrc) = 0
                ierr = nf_noerr
              endif
            elseif (itrc.eq.iChla) then
              ierr=nf_inq_varid (ncidclm2, 'chla_time', tclm_tid(itrc))
              if (ierr .ne. nf_noerr) then
                MPI_master_only write(stdout,3) 'chla_time', 
     &                itrc, clm_file2(1:lstr2)
                tclm_tid(itrc) = 0
                ierr = nf_noerr
            endif
            elseif (itrc.eq.iPhyt) then
              ierr=nf_inq_varid (ncidclm2, 'phyto_time', tclm_tid(itrc))
              if (ierr .ne. nf_noerr) then
                MPI_master_only write(stdout,3) 'phyto_time', 
     &                itrc, clm_file2(1:lstr2)
                tclm_tid(itrc) = 0
                ierr = nf_noerr
              endif
            elseif (itrc.eq.iZoo_) then
              ierr=nf_inq_varid (ncidclm2, 'zoo_time', tclm_tid(itrc))
              if (ierr .ne. nf_noerr) then
                MPI_master_only write(stdout,3) 'zoo_time', 
     &                itrc, clm_file2(1:lstr2)
                tclm_tid(itrc) = 0
                ierr = nf_noerr
              endif
            elseif (itrc.eq.iSDet) then
              ierr=nf_inq_varid (ncidclm2, 'sdet_time', tclm_tid(itrc))
              if (ierr .ne. nf_noerr) then
                MPI_master_only write(stdout,3) 'sdet_time', 
     &                itrc, clm_file2(1:lstr2)
                tclm_tid(itrc) = 0
                ierr = nf_noerr
              endif
            elseif (itrc.eq.iLDet) then
              ierr=nf_inq_varid (ncidclm2, 'ldet_time', tclm_tid(itrc))
              if (ierr .ne. nf_noerr) then
                MPI_master_only write(stdout,3) 'ldet_time', 
     &                itrc, clm_file2(1:lstr2)
                tclm_tid(itrc) = 0
                ierr = nf_noerr
              endif
#  ifdef OXYGEN
            elseif (itrc.eq.iO2) then
              ierr=nf_inq_varid (ncidclm3, 'o2_time', tclm_tid(itrc))
              if (ierr .ne. nf_noerr) then
                MPI_master_only write(stdout,3) 'o2_time', 
     &                itrc, clm_file3(1:lstr3)
                tclm_tid(itrc) = 0
                ierr = nf_noerr
              endif
#   ifdef CARBON
            elseif (itrc.eq.iDIC) then
              ierr=nf_inq_varid (ncidclm3, 'dic_time', tclm_tid(itrc))
              if (ierr .ne. nf_noerr) then
                MPI_master_only write(stdout,3) 'dic_time', 
     &                itrc, clm_file3(1:lstr3)
                tclm_tid(itrc) = 0
                ierr = nf_noerr
            endif
            elseif (itrc.eq.iTALK) then
              ierr=nf_inq_varid (ncidclm3, 'alk_time', tclm_tid(itrc))
              if (ierr .ne. nf_noerr) then
                MPI_master_only write(stdout,3) 'alk_time', 
     &                itrc, clm_file3(1:lstr3)
                tclm_tid(itrc) = 0
                ierr = nf_noerr
            endif
            elseif (itrc.eq.iSDetC) then
              ierr=nf_inq_varid (ncidclm3, 'sdetc_time', tclm_tid(itrc))
              if (ierr .ne. nf_noerr) then
                MPI_master_only write(stdout,3) 'sdetc_time', 
     &                itrc, clm_file3(1:lstr3)
                tclm_tid(itrc) = 0
                ierr = nf_noerr
              endif
            elseif (itrc.eq.iLDetC) then
              ierr=nf_inq_varid (ncidclm3, 'ldetc_time', tclm_tid(itrc))
              if (ierr .ne. nf_noerr) then
                MPI_master_only write(stdout,3) 'ldetc_time', 
     &                itrc, clm_file3(1:lstr3)
                tclm_tid(itrc) = 0
                ierr = nf_noerr
              endif
            elseif (itrc.eq.iCaCO3) then
              ierr=nf_inq_varid (ncidclm3, 'caco3_time', tclm_tid(itrc))
              if (ierr .ne. nf_noerr) then
                MPI_master_only write(stdout,3) 'caco3_time', 
     &                itrc, clm_file3(1:lstr3)
                tclm_tid(itrc) = 0
                ierr = nf_noerr
              endif
#   endif /* CARBON */
#  endif /* OXYGEN */
# endif /* BIOLOGY_NPZDOC */
# ifdef BIOLOGY_BEC
          elseif (itrc.eq.iPO4) then
            ierr=nf_inq_varid (ncidclm, 'po4_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              write(stdout,1) 'po4_time', itrc, clm_file(1:lstr)
            endif
          elseif (itrc.eq.iNO3) then
            ierr=nf_inq_varid (ncidclm, 'no3_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              write(stdout,1) 'no3_time', itrc, clm_file(1:lstr)
            endif
          elseif (itrc.eq.iSIO3) then
            ierr=nf_inq_varid (ncidclm, 'sio3_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
             write(stdout,1) 'sio3_time', itrc, clm_file(1:lstr)
           endif
          elseif (itrc.eq.iO2) then
            ierr=nf_inq_varid (ncidclm, 'o2_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
            write(stdout,1) 'o2_time', itrc, clm_file(1:lstr)
         endif
         elseif (itrc.eq.iFE) then
            ierr=nf_inq_varid (ncidclm2, 'fe_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              write(stdout,1) 'fe_time', itrc, clm_file2(1:lstr2)
            endif
         elseif (itrc.eq.iDIC) then
             ierr=nf_inq_varid (ncidclm2, 'dic_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
             write(stdout,1) 'dic_time', itrc, clm_file2(1:lstr2)
            endif
         elseif (itrc.eq.iALK) then
             ierr=nf_inq_varid (ncidclm2, 'alk_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
               write(stdout,1) 'alk_time', itrc, clm_file2(1:lstr2)
            endif
         elseif (itrc.eq.iNH4) then
            ierr=nf_inq_varid (ncidclm2, 'nh4_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
                MPI_master_only write(stdout,3) 'nh4_time', 
     &                itrc, clm_file2(1:lstr2)
                tclm_tid(itrc) = 0
                ierr = nf_noerr
!             write(stdout,1) 'nh4_time', itrc, clm_file2(1:lstr2)
            endif
         elseif (itrc.eq.iDOC) then
            ierr=nf_inq_varid (ncidclm2, 'doc_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
                MPI_master_only write(stdout,3) 'doc_time', 
     &                itrc, clm_file2(1:lstr2)
                tclm_tid(itrc) = 0
                ierr = nf_noerr
!               write(stdout,1) 'doc_time', itrc, clm_file2(1:lstr2)
            endif
         elseif (itrc.eq.iDOP) then
            ierr=nf_inq_varid (ncidclm2, 'dop_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
                MPI_master_only write(stdout,3) 'dop_time', 
     &                itrc, clm_file2(1:lstr2)
                tclm_tid(itrc) = 0
                ierr = nf_noerr
!               write(stdout,1) 'dop_time', itrc, clm_file2(1:lstr2)
c              goto 99                                     !--> ERROR
            endif
         elseif (itrc.eq.iDON) then
            ierr=nf_inq_varid (ncidclm2, 'don_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
                MPI_master_only write(stdout,3) 'don_time', 
     &                itrc, clm_file2(1:lstr2)
                tclm_tid(itrc) = 0
                ierr = nf_noerr
!                write(stdout,1) 'don_time', itrc, clm_file2(1:lstr2)
c              goto 99                                     !--> ERROR
            endif
         elseif (itrc.eq.iDOFE) then
            ierr=nf_inq_varid (ncidclm2, 'dofe_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
                MPI_master_only write(stdout,3) 'dofe_time', 
     &                itrc, clm_file2(1:lstr2)
                tclm_tid(itrc) = 0
                ierr = nf_noerr
!               write(stdout,1) 'dofe_time', itrc, clm_file2(1:lstr2)
c               goto 99                     !--> ERROR
            endif
         elseif (itrc.eq.iSPC) then
            ierr=nf_inq_varid (ncidclm3, 'spc_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              write(stdout,1) 'spc_time', itrc, clm_file3(1:lstr3)
            endif
         elseif (itrc.eq.iSPCHL) then
            ierr=nf_inq_varid (ncidclm3, 'spchl_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
               write(stdout,1) 'spchl_time', itrc, clm_file3(1:lstr3)
c              goto 99                                     !--> ERROR
            endif
         elseif (itrc.eq.iSPCACO3) then
            ierr=nf_inq_varid (ncidclm3, 'spcaco3_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
             write(stdout,1) 'spcaco3_time', itrc, clm_file3(1:lstr3)
c              goto 99                                     !--> ERROR
            endif
         elseif (itrc.eq.iSPFE) then
            ierr=nf_inq_varid (ncidclm3, 'spfe_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              write(stdout,1) 'spfe_time', itrc, clm_file3(1:lstr3)
c              goto 99                                     !--> ERROR
            endif
         elseif (itrc.eq.iDIATC) then
            ierr=nf_inq_varid (ncidclm3, 'diatc_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
               write(stdout,1) 'diatc_time', itrc, clm_file3(1:lstr3)
c              goto 99                                     !--> ERROR
            endif
         elseif (itrc.eq.iDIATCHL) then
            ierr=nf_inq_varid (ncidclm3, 'diatchl_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
                write(stdout,1) 'diatchl_time', itrc, clm_file3(1:lstr3)
c              goto 99                                     !--> ERROR
            endif
         elseif (itrc.eq.iDIATSI) then
            ierr=nf_inq_varid (ncidclm3, 'diatsi_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              write(stdout,1) 'diatsi_time', itrc, clm_file3(1:lstr3)
c              goto 99                                     !--> ERROR
            endif
         elseif (itrc.eq.iDIATFE) then
            ierr=nf_inq_varid (ncidclm3, 'diatfe_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              write(stdout,1) 'diatfe_time', itrc, clm_file3(1:lstr3)
c              goto 99                                     !--> ERROR
            endif
         elseif (itrc.eq.iZOOC) then
            ierr=nf_inq_varid (ncidclm4, 'zooc_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              write(stdout,1) 'zooc_time', itrc, clm_file4(1:lstr4)
c              goto 99                                     !--> ERROR
            endif
         elseif (itrc.eq.iDIAZC) then
            ierr=nf_inq_varid (ncidclm4, 'diazc_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              write(stdout,1) 'diazc_time', itrc, clm_file4(1:lstr4)
c              goto 99                                     !--> ERROR
            endif
         elseif (itrc.eq.iDIAZCHL) then
            ierr=nf_inq_varid (ncidclm4, 'diazchl_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              write(stdout,1) 'diazchl_time', itrc, clm_file4(1:lstr4)
c              goto 99                                     !--> ERROR
            endif
         elseif (itrc.eq.iDIAZFE) then
            ierr=nf_inq_varid (ncidclm4, 'diazfe_time', tclm_tid(itrc))
            if (ierr .ne. nf_noerr) then
              write(stdout,1) 'diazfe_time', itrc, clm_file4(1:lstr4)
c              goto 99                                     !--> ERROR
            endif
# endif /* BIOLOGY_BEC */
# ifdef BIOLOGY
            elseif (itrc.eq.iNO3_) then
              ierr=nf_inq_varid (ncidclm, 'no3_time', tclm_tid(itrc))
              if (ierr .ne. nf_noerr) write(stdout,2) 'no3_time',
     &                                         itrc, clm_file(1:lstr)
            elseif (itrc.eq.iNH4_) then
              ierr=nf_inq_varid (ncidclm, 'nh4_time', tclm_tid(itrc))
              if (ierr .ne. nf_noerr) write(stdout,2) 'nh4_time',
     &                                         itrc, clm_file(1:lstr)
            elseif (itrc.eq.iChla) then
              ierr=nf_inq_varid (ncidclm, 'chla_time',tclm_tid(itrc))
              if (ierr .ne. nf_noerr) write(stdout,2)  'chla_time',
     &                                         itrc, clm_file(1:lstr)
            elseif (itrc.eq.iPhyt) then
              ierr=nf_inq_varid (ncidclm,'phyto_time',tclm_tid(itrc))
              if (ierr .ne. nf_noerr) write(stdout,2) 'phyto_time',
     &                                         itrc, clm_file(1:lstr)
            elseif (itrc.eq.iZoo_) then
              ierr=nf_inq_varid (ncidclm, 'zoo_time', tclm_tid(itrc))
              if (ierr .ne. nf_noerr) write(stdout,2) 'zoo_time',
     &                                         itrc, clm_file(1:lstr)
            elseif (itrc.eq.iSDet) then
              ierr=nf_inq_varid (ncidclm, 'sdet_time',tclm_tid(itrc))
              if (ierr .ne. nf_noerr) write(stdout,2) 'sdet_time',
     &                                         itrc, clm_file(1:lstr)
            elseif (itrc.eq.iLDet) then
              ierr=nf_inq_varid (ncidclm, 'ldet_time',tclm_tid(itrc))
              if (ierr .ne. nf_noerr) write(stdout,2) 'ldet_time',
     &                                         itrc, clm_file(1:lstr)
            endif ! if (itrc.eq.itemp) when BIOLOGY is defined
            if (ierr.ne.nf_noerr .and. itrc.ge.iNO3_) then
              ierr=nf_noerr
              tclm_tid(itrc)=-1
# endif /* BIOLOGY */
            endif ! if (itrc.eq.itemp) when BIOLOGY is undefined

            if (ierr.eq.nf_noerr .and. tclm_tid(itrc).gt.0) then
              lvar=lenstr(vname(1,indxT+itrc-1))
#if defined TWO_CLIM_FILES
              if (itrc .ge. iNO3_) then
                 ierr=nf_inq_varid(ncidclm2,
     &                vname(1,indxT+itrc-1)(1:lvar),
     &              tclm_id(itrc))
                if (ierr .ne. nf_noerr) then
                  write(stdout,3) vname(1,indxT+itrc-1)(1:lvar),
     &                 itrc, clm_file2(1:lstr2)
                  goto 99       !--> ERROR
                endif
              else   
#elif defined THREE_CLIM_FILES
              if (itrc .ge. iNO3_ .and. itrc .le. iLDet) then
                 ierr=nf_inq_varid(ncidclm2,
     &                vname(1,indxT+itrc-1)(1:lvar),
     &              tclm_id(itrc))
                if (ierr .ne. nf_noerr) then
                  write(stdout,3) vname(1,indxT+itrc-1)(1:lvar),
     &                 itrc, clm_file2(1:lstr2)
                  goto 99       !--> ERROR
                endif
              else if (itrc .gt. iLDet) then
                 ierr=nf_inq_varid(ncidclm3,
     &                vname(1,indxT+itrc-1)(1:lvar),
     &              tclm_id(itrc))
                if (ierr .ne. nf_noerr) then
                  write(stdout,3) vname(1,indxT+itrc-1)(1:lvar),
     &                 itrc, clm_file3(1:lstr3)
                  goto 99       !--> ERROR
                endif
              else
#endif
#ifdef BIOLOGY_BEC
              if (itrc.eq.itemp .or. itrc.eq.isalt
     &             .or. itrc.eq.ipo4 
     &            .or. itrc.eq.ino3 .or. itrc.eq.isio3 .or. 
     &             itrc.eq.io2
     &             ) then 
#endif /* BIOLOGY_BEC */
              ierr=nf_inq_varid (ncidclm, vname(1,indxT+itrc-1)
     &                                  (1:lvar), tclm_id(itrc))
#if defined TWO_CLIM_FILES || defined THREE_CLIM_FILES
              endif
#endif
              if (ierr .eq. nf_noerr) then
#if defined TWO_CLIM_FILES
                if (itrc .ge. iNO3_) then
                   call set_cycle (ncidclm2,tclm_tid(itrc),nttclm(itrc),
     &              tclm_cycle(itrc), tclm_ncycle(itrc), tclm_rec(itrc), 
     &                  ierr)
                else
#elif defined THREE_CLIM_FILES
                if (itrc .ge. iNO3_ .and. itrc .le. iLDet) then
                  call set_cycle (ncidclm2,tclm_tid(itrc),nttclm(itrc),
     &              tclm_cycle(itrc), tclm_ncycle(itrc), tclm_rec(itrc), 
     &                  ierr)
                else if (itrc .gt. iLDet) then
                  call set_cycle (ncidclm3, tclm_tid(itrc),nttclm(itrc),
     &              tclm_cycle(itrc), tclm_ncycle(itrc), tclm_rec(itrc), 
     &                  ierr)
                else
#endif
                call set_cycle (ncidclm,tclm_tid(itrc), nttclm(itrc),
     &                          tclm_cycle(itrc),  tclm_ncycle(itrc),
     &                                         tclm_rec(itrc), ierr)
#if defined TWO_CLIM_FILES || defined THREE_CLIM_FILES
                endif
#endif

                ittclm(itrc)=1
                tclm_time(1,itrc)=-1.E+20
                tclm_time(2,itrc)=-1.E+20
              else
                write(stdout,1) vname(1,indxT+itrc-1)(1:lvar),
     &                                   itrc, clm_file(1:lstr)
              endif
#if defined BIOLOGY_BEC
             elseif (itrc.eq.inh4 .or. itrc.eq. ife .or. itrc.eq.idic
     &            .or. itrc.eq. ialk .or. itrc.eq. idoc .or. 
     &             itrc.eq.idon .or. itrc.eq. idop .or. 
     &             itrc.eq.idofe) then
              ierr=nf_inq_varid (ncidclm2, vname(1,indxT+itrc-1)
     &                                  (1:lvar), tclm_id(itrc))
              if (ierr .eq. nf_noerr) then
                call set_cycle (ncidclm2,tclm_tid(itrc), nttclm(itrc),
     &                          tclm_cycle(itrc),  tclm_ncycle(itrc),
     &                                         tclm_rec(itrc), ierr)
                ittclm(itrc)=1
                tclm_time(1,itrc)=-1.E+20
                tclm_time(2,itrc)=-1.E+20
              else
                write(stdout,1) vname(1,indxT+itrc-1)(1:lvar),
     &                                   itrc, clm_file2(1:lstr2)
              endif
!              write(stdout,*)"itrc,tclm_rec",mynode,itrc,ncidclm2,
!     &           tclm_rec(itrc)

             elseif (itrc.eq.ispc .or. itrc.eq. ispchl .or. 
     &             itrc.eq. ispcaco3 .or. itrc.eq.ispfe .or. 
     &             itrc.eq. idiatc .or. itrc.eq. idiatchl .or. 
     &             itrc.eq. idiatsi .or. itrc.eq.idiatfe) then
              ierr=nf_inq_varid (ncidclm3, vname(1,indxT+itrc-1)
     &                                  (1:lvar), tclm_id(itrc))
!              write(stdout,*)"itrc,tclm_rec",mynode,itrc,ncidclm3,
!     &           tclm_id(itrc),ierr

              if (ierr .eq. nf_noerr) then
                call set_cycle (ncidclm3,tclm_tid(itrc), nttclm(itrc),
     &                          tclm_cycle(itrc),  tclm_ncycle(itrc),
     &                                         tclm_rec(itrc), ierr)
                ittclm(itrc)=1
                tclm_time(1,itrc)=-1.E+20
                tclm_time(2,itrc)=-1.E+20
              else
                write(stdout,1) vname(1,indxT+itrc-1)(1:lvar),
     &                                   itrc, clm_file3(1:lstr3)
              endif
           elseif ( itrc .eq.izooc .or. itrc.eq. idiazc .or. 
     &        itrc.eq. idiazchl .or. itrc.eq. idiazfe
     &             ) then
              ierr=nf_inq_varid (ncidclm4, vname(1,indxT+itrc-1)
     &                                  (1:lvar), tclm_id(itrc))
              if (ierr .eq. nf_noerr) then
                call set_cycle (ncidclm4,tclm_tid(itrc), nttclm(itrc),
     &                          tclm_cycle(itrc),  tclm_ncycle(itrc),
     &                                         tclm_rec(itrc), ierr)
                ittclm(itrc)=1
                tclm_time(1,itrc)=-1.E+20
                tclm_time(2,itrc)=-1.E+20
              else
                write(stdout,1) vname(1,indxT+itrc-1)(1:lvar),
     &                                   itrc, clm_file4(1:lstr4)
              endif
           endif ! if (itrc.eq.itemp .or. itrc.eq.isalt ... itrc.eq.io2)
#endif /* BIOLOGY_BEC */

            endif ! if (ierr.eq.nf_noerr .and. tclm_tid(itrc).gt.0)
          endif !if (ierr.eq.nf_noerr)
        enddo ! do itrc=1,NT
      endif ! if (iic.eq.ntstart)
  1   format(/1x,'ERROR in get_tclima: cannot find variable ''',
     &    A, ''', itrc =',I3/7x, 'in netCDF file ''', A, '''.' /)
# ifdef BIOLOGY
  2   format(/1x,'WARNING: get_tclima cannot find variable ''',
     &    A, ''', itrc =',I3/10x,'in netCDF file ''', A,'''.'/10x,
     &       'Setting analytical climatology for this variable.')
# endif
  3   format(1x,'get_tclima: cannot find variable ''',
     &    A, ''', itrc =',I3/7x, ' in netCDF file ''', A, 
     &    '''. Default values will be used for corresponding tracer.' )
!
! Read data from the file:  Check if model time is bounded by past
!===== ==== ==== === =====  and future data times: if not, increment
! record and cycling indices, flip time index and read a new portion
! of data. Repeat until model time falls between the two data times.
!
      do itrc=1,NT
        if (tclm_tid(itrc).gt.0) then
          do while (tclm_time(ittclm(itrc),itrc).lt.time+0.5*dt .and.
     &                                              ierr.eq.nf_noerr)
            call advance_cycle (tclm_cycle(itrc),  nttclm(itrc),
     &                 tclm_ncycle(itrc), tclm_rec(itrc), ierr)
            if (ierr.eq.nf_noerr) then
#if defined TWO_CLIM_FILES
              if (itrc .ge. iNO3_) then
                ierr=nf_get_var1_FTYPE(ncidclm2, tclm_tid(itrc),
     &            tclm_rec(itrc), cff)
              else
#elif defined THREE_CLIM_FILES
              if (itrc .ge. iNO3_ .and. itrc .le. iLDet) then
                ierr=nf_get_var1_FTYPE(ncidclm2, tclm_tid(itrc),
     &            tclm_rec(itrc), cff)
              else if (itrc .gt. iLDet) then
                ierr=nf_get_var1_FTYPE(ncidclm3, tclm_tid(itrc),
     &            tclm_rec(itrc), cff)
              else
#endif
#ifdef BIOLOGY_BEC
              if (itrc.eq.itemp .or. itrc.eq. isalt
     &           .or. itrc.eq. ipo4 .or. itrc.eq. ino3 
     &           .or. itrc.eq. isio3 .or. itrc.eq. io2
     &         ) then
#endif
                ierr=nf_get_var1_FTYPE(ncidclm, tclm_tid(itrc),
     &            tclm_rec(itrc), cff)
#if defined TWO_CLIM_FILES || defined THREE_CLIM_FILES
              end if
#endif
#if defined BIOLOGY_BEC
              elseif (itrc.eq. inh4 .or. itrc.eq. ife .or. 
     &          itrc.eq. idic .or. itrc.eq.ialk .or. 
     &           itrc.eq. idoc .or. itrc.eq. idon .or. 
     &           itrc.eq. idop .or. itrc.eq.idofe) then
                ierr=nf_get_var1_FTYPE(ncidclm2, tclm_tid(itrc),
     &                                  tclm_rec(itrc), cff)
              elseif (itrc.eq.ispc .or. itrc.eq. ispchl .or. 
     &             itrc.eq. ispcaco3 .or. itrc.eq.ispfe .or. 
     &             itrc.eq. idiatc .or. itrc.eq. idiatchl .or. 
     &             itrc.eq. idiatsi .or. itrc.eq.idiatfe) then
                ierr=nf_get_var1_FTYPE(ncidclm3, tclm_tid(itrc),
     &                                  tclm_rec(itrc), cff)
              elseif( itrc .eq.izooc .or. itrc.eq. idiazc .or. 
     &           itrc.eq. idiazchl .or. itrc.eq.idiazfe) then
                ierr=nf_get_var1_FTYPE(ncidclm4, tclm_tid(itrc),
     &                                  tclm_rec(itrc), cff)
              endif
#endif /*  BIOLOGY_BEC */
              if (ierr.eq.nf_noerr) then
                ittclm(itrc)=min(3-ittclm(itrc),nttclm(itrc))
                tclm_time(ittclm(itrc),itrc)=cff*day2sec
     &               + tclm_cycle(itrc)*tclm_ncycle(itrc)

#if defined TWO_CLIM_FILES
                if (itrc .ge. iNO3_) then
              ierr=nf_fread (tclima(START_2D_ARRAY,1,ittclm(itrc),itrc),
     &                    ncidclm2, tclm_id(itrc),
     &                    tclm_rec(itrc), r3dvar)
                else
#elif defined THREE_CLIM_FILES
                if (itrc .ge. iNO3_ .and. itrc .le. iLDet) then
              ierr=nf_fread (tclima(START_2D_ARRAY,1,ittclm(itrc),itrc),
     &                    ncidclm2, tclm_id(itrc),
     &                    tclm_rec(itrc), r3dvar)
                else if (itrc .gt. iLDet) then
              ierr=nf_fread (tclima(START_2D_ARRAY,1,ittclm(itrc),itrc),
     &                    ncidclm3, tclm_id(itrc),
     &                    tclm_rec(itrc), r3dvar)
                else
#endif
#ifdef BIOLOGY_BEC
              if (itrc.eq.itemp .or. itrc.eq. isalt
     &            .or. itrc.eq. ipo4 .or. itrc.eq. ino3 
     &            .or. itrc.eq. isio3 .or. itrc.eq. io2
     &               ) then
#endif
                ierr=nf_fread (tclima(START_2D_ARRAY,1,ittclm(itrc),
     &                                itrc), ncidclm, tclm_id(itrc),
     &                                      tclm_rec(itrc), r3dvar)
#if defined TWO_CLIM_FILES || defined THREE_CLIM_FILES
                end if
#endif
#if defined BIOLOGY_BEC
             elseif (itrc.eq.inh4 .or. itrc.eq. ife .or. 
     &              itrc.eq. idic .or. itrc.eq.ialk .or. 
     &              itrc.eq. idoc .or. itrc.eq. idon .or. 
     &              itrc.eq. idop .or. itrc.eq.idofe) then
                  ierr=nf_fread (tclima(START_2D_ARRAY,1,ittclm(itrc),
     &                                itrc), ncidclm2, tclm_id(itrc),
     &                                      tclm_rec(itrc), r3dvar)
              elseif (itrc.eq.ispc .or. itrc.eq. ispchl .or. 
     &              itrc.eq. ispcaco3 .or. itrc.eq.ispfe .or. 
     &              itrc.eq. idiatc .or. itrc.eq. idiatchl .or. 
     &              itrc.eq. idiatsi .or. itrc.eq.idiatfe) then
                  ierr=nf_fread (tclima(START_2D_ARRAY,1,ittclm(itrc),
     &                                itrc), ncidclm3, tclm_id(itrc),
     &                                      tclm_rec(itrc), r3dvar)
              elseif( itrc .eq.izooc .or. itrc.eq. idiazc .or. 
     &              itrc.eq. idiazchl .or. itrc.eq.idiazfe) then
                  ierr=nf_fread (tclima(START_2D_ARRAY,1,ittclm(itrc),
     &                                itrc), ncidclm4, tclm_id(itrc),
     &                                      tclm_rec(itrc), r3dvar)
              endif ! if (itrc.eq.itemp .or. itrc.eq. isalt ...)
#endif
                if (ierr.eq.nf_noerr) then
                  MPI_master_only write(stdout,
     &            '(6x,A,I3,1x,A,F12.4,2(1x,A,I4))')
     &            'get_tclima -- read climatology for tracer', itrc,
     &            'tclm_time =', cff,  'rec =', 
     &            tclm_rec(itrc) MYID
                  if (nttclm(itrc).eq.1) goto 30
                else
                  lvar=lenstr(vname(1,indxT+itrc-1))
                  write(stdout,4) vname(1,indxT+itrc-1)(1:lvar),
     &                                     itrc, tclm_rec(itrc)
                endif
              else
                write(stdout,4) 'Xclm_time', itrc, tclm_rec(itrc)
              endif
            else
              write(stdout,'(/1x,A,I3,1x,A,I3/7x,3A/7x,2(A,G12.4)/)')
     &       'ERROR in get_tclima: requested time record ',
     &        tclm_rec(itrc), 'exeeds the last record', nttclm(itrc),
     &       'available in netCDF file ''',  clm_file(1:lstr),  '''',
     &       'tdays = ', tdays,' but the last available tclm_time =',
     &                          tclm_time(ittclm(itrc),itrc)*sec2day
            endif
          enddo
        endif
 30   continue
      enddo
      return

  4   format(/1x,'ERROR in get_tclima: cannot read variable ''',
     &                  A, ''', itrc =', I3, ' for record', I3/)

  5   format(/1x,'ERROR in get_tclima: cannot open file ''',
     &                  A, '''.'/)

#if defined TWO_CLIM_FILES || defined THREE_CLIM_FILES || defined FOUR_CLIM_FILES
 22   write(stdout,5) clm_file2(1:lstr2)
      goto 99
#endif
#if defined THREE_CLIM_FILES || defined FOUR_CLIM_FILES
 33   write(stdout,5) clm_file3(1:lstr3)
      goto 99
#endif
#ifdef FOUR_CLIM_FILES
 44   write(stdout,5) clm_file4(1:lstr4)
      goto 99
#endif

 99   ierr = nf_noerr + 1

      return
      end
 
 
      subroutine set_tclima_tile (istr,iend,jstr,jend, ierr)
!
! Set-up tracer climatology for current tile.
!
      implicit none
      integer istr,iend,jstr,jend, ierr, itrc,i,j,k, it1,it2
      real cff, cff1, cff2
# define TCLIMA_DATA
# include "param.h"
# include "climat.h"
# include "scalars.h"
# include "ocean3d.h"
# ifdef BIOLOGY_NPZDOC
      real CN_avg
# include "biopar.h"
# endif /* BIOLOGY_NPZDOC */
!
# include "compute_extended_bounds.h"
!
! Set coefficients for interpolation. Check that for the next time
! step [when time=time+dt] both weights will still be positive, and
! if not, set synchro_flag to signal that new data should be read
! from an appropriate netCDF input file (master thread only).
! After that either load time-invariant data, or interpolate in time
! or complain about error and signal to quit, if interpolation is
! needed, but not possible.
!
      do itrc=1,NT
        if (tclm_tid(itrc).gt.0) then
          it1=3-ittclm(itrc)
          it2=ittclm(itrc)
          cff1=tclm_time(it2,itrc)-time
          cff2=time-tclm_time(it1,itrc)

          if (cff1.lt.dt) synchro_flag=.true.
 
          if (nttclm(itrc).eq.1) then         ! Load time-invariant
            if (iic.eq.0) then                ! tracer climatology.
              do k=1,N
                do j=jstrR,jendR
                  do i=istrR,iendR
                    tclm(i,j,k,itrc)=tclima(i,j,k,ittclm(itrc),itrc)
                  enddo
                enddo
              enddo
            endif
          elseif (cff1.ge.0. .and. cff2.ge.0.) then
            cff=1./(cff1+cff2)                        ! Interpolate
            cff1=cff1*cff                             ! tracer
            cff2=cff2*cff                             ! climatology
            do k=1,N                                  ! climatology
              do j=jstrR,jendR                        ! in time.
                do i=istrR,iendR
                  tclm(i,j,k,itrc)=cff1*tclima(i,j,k,it1,itrc)
     &                            +cff2*tclima(i,j,k,it2,itrc)
                enddo
              enddo
            enddo
          elseif (ZEROTH_TILE) then
            write(stdout,'(/2A/3(1x,A,F16.10)/)') ' SET_TCLIMA_TILE',
     &         ' --  model time is outside bounds of ''tclm_time''.',
     &         'TCLM_TSTART=', tclm_time(it1,itrc)*sec2day, 'TDAYS=',
     &          tdays, 'TCLM_TEND=',     tclm_time(it2,itrc)*sec2day
            ierr=ierr+1
          endif
# ifdef BIOLOGY
#  define temp cff
#  define SiO4 cff1
        elseif (itrc.eq.iNO3_) then     ! <-- analytical setup for
          do k=1,N                      ! climatology for biological
            do j=jstrR,jendR            ! constituents.
              do i=istrR,iendR
                temp=tclm(i,j,k,itemp)
                if (temp.lt.8.) then
                  SiO4=30.
                elseif (temp.ge.8. .and. temp.le.11.) then
                  SiO4=30.-((temp-8.)*(20./3.))
                elseif (temp.gt.11. .and. temp.le.13.) then
                  SiO4=10.-((temp-11.)*(8./2.))
                elseif (temp.gt.13. .and. temp.le.16.) then
                  SiO4=2.-((temp-13.)*(2./3.))
                elseif (temp.gt.16.) then
                  SiO4=0.
                endif
 
                tclm(i,j,k,iNO3_)=1.67 + SiO4*( 0.5873 + SiO4*(
     &                                0.0144 + SiO4*0.0003099 ))
!
! Cut off surface NO3
!
                tclm(i,j,k,iNO3_)=tclm(i,j,k,iNO3_)
     &               *(1-0.5*(tanh((z_r(i,j,k)+100.)/8.)+1.))
              enddo
            enddo
          enddo
        elseif (iic.eq.ntstart .and. itrc.eq.iNH4_) then
          do k=1,N
            do j=jstrR,jendR
              do i=istrR,iendR
                if (tclm_tid(iNH4_).lt.0) tclm(i,j,k,iNH4_)=0.10
                if (tclm_tid(iChla).lt.0) tclm(i,j,k,iChla)=0.08
                if (tclm_tid(iPhyt).lt.0) tclm(i,j,k,iPhyt)=0.06
                if (tclm_tid(iZoo_).lt.0) tclm(i,j,k,iZoo_)=0.04
                if (tclm_tid(iSDet).lt.0) tclm(i,j,k,iSDet)=0.02
                if (tclm_tid(iLDet).lt.0) tclm(i,j,k,iLDet)=0.02
              enddo
            enddo
          enddo
#  undef SiO4
#  undef temp
# endif

# ifdef BIOLOGY_NPZDOC
      else if (FIRST_TIME_STEP) then  ! -> got_tclm(itrc) == .false.
! hf: Note that the following settings are only provided for cases
! where no boundary conditions for the bgc state variables have been
! provided, e.g. test runs. In all "serious" model runs, boundary 
! conditions should be provided at least for NO3, O2, DIC, and Alk,
! and ideally for Chla, Phyto, and Zoo as well.
         if (itrc .eq. iNO3_) then
#  define temp cff
#  define SiO4 cff1
            do k=1,N
               do j=JstrR,JendR
                  do i=IstrR,IendR
                     temp=tclm(i,j,k,itemp)
                     if (temp.lt.8.) then
                        SiO4=30.
                     elseif (temp.ge.8. .and. temp.le.11.) then
                        SiO4=30.-((temp-8.)*(20./3.))
                     elseif (temp.gt.11. .and. temp.le.13.) then
                        SiO4=10.-((temp-11.)*(8./2.))
                     elseif (temp.gt.13. .and. temp.le.16.) then
                        SiO4=2.-((temp-13.)*(2./3.))
                     elseif (temp.gt.16.) then
                        SiO4=0.
                     endif
                     tclm(i,j,k,iNO3_)=1.67+0.5873*SiO4
     &                    +0.0144*SiO4**2
     &                    +0.0003099*SiO4**3

!hf: by default, do not perform the cutoff!
#ifdef NO3_SFC_CUTOFF 
!
!  Cut off surface NO3
!
                     tclm(i,j,k,iNO3_)=tclm(i,j,k,iNO3_)*
     &                    (1-.5*(tanh((z_r(i,j,k)+100.)/8.)+1))
#endif /* NO3_SFC_CUTOFF */
                  end do
               end do
            end do
         else if (itrc .eq. iNH4_ .or. itrc .eq. iSDet .or.
     &           itrc .eq. iLDet) then
            tclm(:,:,:,itrc)=0.0
         else if (itrc .eq. iChla) then
! small positive concentrations for plankton to avoid "dead water"
            tclm(:,:,:,itrc)=0.2
         else if (itrc .eq. iPhyt) then
! small positive concentrations for plankton to avoid "dead water"
            tclm(:,:,:,itrc)=0.1
         else if (itrc .eq. iZoo_) then
            tclm(:,:,:,itrc)=0.05
#  ifdef OXYGEN
! the following three values are way too high at depth, these default
! values are mostly provided as convenience for test purposes
         else if (itrc .eq. iO2) then  
            tclm(:,:,:,iO2)=250.0 
#   ifdef CARBON
         else if (itrc .eq. iDIC) then  
            tclm(:,:,:,iDIC)=1988.0 
         else if (itrc .eq. iTALK) then  
            tclm(:,:,:,iTALK)=2214.0 
         else if (itrc .eq. iSDetC) then  
            CN_avg = 0.5 * (CN_P + CN_Z)
            tclm(:,:,:,iSDetC)=tclm(:,:,:,iSDet) * CN_avg
         else if (itrc .eq. iLDetC) then  
            CN_avg = 0.5 * (CN_P + CN_Z)
            tclm(:,:,:,iLDetC)=tclm(:,:,:,iLDet) * CN_avg
         else if (itrc .eq. iCaCO3) then  
            tclm(:,:,:,iCaCO3)=0.0
#   endif /* CARBON */
#  endif /* OXYGEN */
         endif
#  undef SiO4
#  undef temp
# endif /* BIOLOGY_NPZDOC */
# ifdef BIOLOGY_BEC
        elseif (FIRST_TIME_STEP) then
          do k=1,N
          do j=JstrR,JendR
           do i=IstrR,IendR
              if (tclm_tid(iDIC).lt.0) tclm(i,j,k,iDIC)=2200.
              if (tclm_tid(iALK).lt.0) tclm(i,j,k,iALK)=2300.
              if (tclm_tid(iNH4).lt.0) tclm(i,j,k,iNH4)=0.0
              if (tclm_tid(iDOC).lt.0) tclm(i,j,k,iDOC)=0.0 ! 0.001
              if (tclm_tid(iSPC).lt.0) tclm(i,j,k,iSPC)=0.1
              if (tclm_tid(iSPCHL).lt.0) tclm(i,j,k,iSPCHL)=0.03
              if (tclm_tid(iSPCACO3).lt.0) tclm(i,j,k,iSPCACO3)=0.1
              if (tclm_tid(iDIATC).lt.0) tclm(i,j,k,iDIATC)=0.1
              if (tclm_tid(iDIATCHL).lt.0) tclm(i,j,k,iDIATCHL)=0.03
              if (tclm_tid(iZOOC).lt.0) tclm(i,j,k,iZOOC)=0.01
              if (tclm_tid(iSPFE).lt.0) tclm(i,j,k,iSPFE)=5.47e-7
              if (tclm_tid(iDIATSI).lt.0) tclm(i,j,k,iDIATSI)=0.02735
              if (tclm_tid(iDIATFE).lt.0) tclm(i,j,k,iDIATFE)=5.47e-7
              if (tclm_tid(iDIAZC).lt.0) tclm(i,j,k,iDIAZC)=0.1
              if (tclm_tid(iDIAZCHL).lt.0) tclm(i,j,k,iDIAZCHL)=0.03
              if (tclm_tid(iDIAZFE).lt.0) tclm(i,j,k,iDIAZFE)=5.47e-7
              if (tclm_tid(iDON).lt.0) tclm(i,j,k,iDON)=0.0 ! 1.368e-4
              if (tclm_tid(iDOFE).lt.0) tclm(i,j,k,iDOFE)=0.0 ! 5.47e-9
              if (tclm_tid(iDOP).lt.0) tclm(i,j,k,iDOP)=0.0 ! 8.55e-4
           enddo
        enddo
      enddo
# endif /* BIOLOGY_BEC */
        endif   ! <-- tclm_tid(itrc).gt.0
      enddo   ! <-- itrc
      return
      end
#else
      subroutine get_tclima_empty
      end
#endif
