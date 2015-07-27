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
# ifdef MULT_CLIM_FILES
      integer ncidclm_loc(4)
# else
      integer ncidclm_loc
# endif
      integer ii
      logical var_found
      ! Array giving file in which data for each tracer are:
      character(len=80) clm_file_tr(NT)

! Initialization: Check, whether climatological data file is already
!---------------- opened, and if not, open it.  Find and save netCDF
! IDs for relevant variables.  Determine whether there is cycling to
! reuse the input data and find cycling period "tclm_cycle", set
! initial cycling index "tclm_ncycle" and record index "tclm_rec",
! time index "itclm" and both time record bounds to large negative
! artificial values, so that it will trigger the logic in reading
! part below.

      ierr=nf_noerr
      lstr=lenstr(clm_file)
      if (iic==ntstart) then
        if (ncidclm==-1) then
          ierr=nf_open (clm_file(1:lstr), nf_nowrite, ncidclm)
          if (ierr /= nf_noerr) write(*,'(/1x,4A/)') '### ERROR: ',
     &                          'get_tclima :: can not open file ''',
     &                                     clm_file(1:lstr),   '''.'
        endif
        do itrc=1,NT
          if (ierr==nf_noerr) then
            if (itrc==itemp) then
              ierr=nf_inq_varid (ncidclm,'tclm_time',tclm_tid(itrc))
              if (ierr /= nf_noerr) write(*,1) 'tclm_time',
     &                                         itrc, clm_file(1:lstr)
# ifdef SALINITY
            elseif (itrc==isalt) then
              ierr=nf_inq_varid (ncidclm, 'sclm_time',tclm_tid(itrc))
              if (ierr /= nf_noerr) write(*,1) 'sclm_time',
     &                                         itrc, clm_file(1:lstr)
# endif
# ifdef BIOLOGY
            elseif (itrc==iNO3_) then
              ierr=nf_inq_varid (ncidclm, 'no3_time', tclm_tid(itrc))
              if (ierr /= nf_noerr) write(*,2) 'no3_time',
     &                                         itrc, clm_file(1:lstr)
            elseif (itrc==iNH4_) then
              ierr=nf_inq_varid (ncidclm, 'nh4_time', tclm_tid(itrc))
              if (ierr /= nf_noerr) write(*,2) 'nh4_time',
     &                                         itrc, clm_file(1:lstr)
            elseif (itrc==iChla) then
              ierr=nf_inq_varid (ncidclm, 'chla_time',tclm_tid(itrc))
              if (ierr /= nf_noerr) write(*,2)  'chla_time',
     &                                         itrc, clm_file(1:lstr)
            elseif (itrc==iPhyt) then
              ierr=nf_inq_varid (ncidclm,'phyto_time',tclm_tid(itrc))
              if (ierr /= nf_noerr) write(*,2) 'phyto_time',
     &                                         itrc, clm_file(1:lstr)
            elseif (itrc==iZoo_) then
              ierr=nf_inq_varid (ncidclm, 'zoo_time', tclm_tid(itrc))
              if (ierr /= nf_noerr) write(*,2) 'zoo_time',
     &                                         itrc, clm_file(1:lstr)
            elseif (itrc==iSDet) then
              ierr=nf_inq_varid (ncidclm, 'sdet_time',tclm_tid(itrc))
              if (ierr /= nf_noerr) write(*,2) 'sdet_time',
     &                                         itrc, clm_file(1:lstr)
            elseif (itrc==iLDet) then
              ierr=nf_inq_varid (ncidclm, 'ldet_time',tclm_tid(itrc))
              if (ierr /= nf_noerr) write(*,2) 'ldet_time',
     &                                         itrc, clm_file(1:lstr)
            endif
            if (ierr/=nf_noerr .and. itrc>=iNO3_) then
              ierr=nf_noerr
              tclm_tid(itrc)=-1
# endif
            endif
            if (ierr==nf_noerr .and. tclm_tid(itrc)>0) then
              lvar=lenstr(vname(1,indxT+itrc-1))
              ierr=nf_inq_varid (ncidclm, vname(1,indxT+itrc-1)
     &                                  (1:lvar), tclm_id(itrc))
              if (ierr == nf_noerr) then
                call set_cycle (ncidclm,tclm_tid(itrc), nttclm(itrc),
     &                          tclm_cycle(itrc),  tclm_ncycle(itrc),
     &                                         tclm_rec(itrc), ierr)
                ittclm(itrc)=1
                tclm_time(1,itrc)=-1.E+20
                tclm_time(2,itrc)=-1.E+20
              else
                write(*,1) vname(1,indxT+itrc-1)(1:lvar),
     &                                   itrc, clm_file(1:lstr)
              endif
            endif
          endif
        enddo
      endif
  1   format(1x,'### ERROR: get_tclima :: Cannot find variable ''',
     &             A, ''', itrc =', I3,1x, 'in file ''', A, '''.')
# ifdef BIOLOGY
  2   format(1x,  'WARNING: get_tclima :: Cannot find variable ''',
     &          A, ''', itrc =', I3, 1x, 'in file ''', A,'''.'/12x,
     &        'Setting analytical climatology for this variable.')
# endif

! Read data from the file:   Check if model time is bounded by past
!----- ---- ---- --- -----   and future data times: if not, increment
! record and cycling indices, flip time index and read a new portion
! of data. Repeat until model time falls between the two data times.

      do itrc=1,NT
        if (tclm_tid(itrc)>0) then
          do while (tclm_time(ittclm(itrc),itrc)<time+0.5*dt
     &                                   .and. ierr==nf_noerr)
            call advance_cycle (tclm_cycle(itrc),  nttclm(itrc),
     &                 tclm_ncycle(itrc), tclm_rec(itrc), ierr)
            if (ierr==nf_noerr) then
              ierr=nf_get_var1_FTYPE(ncidclm, tclm_tid(itrc),
     &                                  tclm_rec(itrc), cff)
              if (ierr==nf_noerr) then
                ittclm(itrc)=min(3-ittclm(itrc),nttclm(itrc))
                tclm_time(ittclm(itrc),itrc)=cff*day2sec
     &               + tclm_cycle(itrc)*tclm_ncycle(itrc)

                ierr=nf_fread (tclima(START_2D_ARRAY,1,ittclm(itrc),
     &                                itrc), ncidclm, tclm_id(itrc),
     &                                      tclm_rec(itrc), r3dvar)
                if (ierr==nf_noerr) then
#ifdef MPI_SILENT_MODE
                  if (mynode==0) then
#endif
                    write(*,'(3x,A,I2,1x,A,F12.4,2(1x,A,I4))')
     &             'get_tclima :: read climatology for tracer', itrc,
     &             'tclm_time =', cff,  'rec =', tclm_rec(itrc) MYID
# ifdef MPI_SILENT_MODE
                  endif
# endif
                  IF (nttclm(itrc)==1) goto 3
                else
                  lvar=lenstr(vname(1,indxT+itrc-1))
                  write(*,4) vname(1,indxT+itrc-1)(1:lvar),
     &                                     itrc, tclm_rec(itrc)
                endif
              else
                write(*,4) 'Xclm_time', itrc, tclm_rec(itrc)
              endif
            else
              write(*,'(/1x,A,I3,1x,A,I3/7x,3A/7x,2(A,G12.4)/)')
     &       '### ERROR: get_tclima :: requested time record ',
     &        tclm_rec(itrc), 'exeeds the last record', nttclm(itrc),
     &       'available in netCDF file ''',  clm_file(1:lstr),  '''',
     &       'tdays = ', tdays,' but the last available tclm_time =',
     &                          tclm_time(ittclm(itrc),itrc)*sec2day
            endif
          enddo  !<-- while()
        endif
  3    continue
      enddo  !<-- itrc
  4   format(1x,'### ERROR: get_tclima :: Cannot read variable ''',
     &                     A, ''', itrc =', I3, ' for record', I3)
      return
      end


      subroutine set_tclima_tile (istr,iend,jstr,jend, ierr)

! Set-up tracer climatology for current tile.

      implicit none
      integer istr,iend,jstr,jend, ierr, itrc,i,j,k, it1,it2
      real cff, cff1, cff2
# if defined PASSIVE_TRACER && defined AGE_DYE_TRACER
      integer is
# endif
# define TCLIMA_DATA
# include "param.h"
# include "climat.h"
# include "scalars.h"
# include "ocean3d.h"
# ifdef BIOLOGY_NPZDOC
      real CN_avg
# include "biopar.h"
# endif /* BIOLOGY_NPZDOC */
# if defined PASSIVE_TRACER && defined AGE_DYE_TRACER
#  include "ncvars.h"
#  include "sources.h"
# endif

# include "compute_extended_bounds.h"

! Set coefficients for interpolation. Check that for the next time
! step [when time=time+dt] both weights will still be positive, and
! if not, set synchro_flag to signal that new data should be read
! from an appropriate netCDF input file (master thread only).
! After that either load time-invariant data, or interpolate in time
! or complain about error and signal to quit, if interpolation is
! needed, but not possible.

      do itrc=1,NT
        if (tclm_tid(itrc)>0) then
          it1=3-ittclm(itrc)
          it2=ittclm(itrc)
          cff=time+0.5*dt
          cff1=tclm_time(it2,itrc)-cff
          cff2=cff-tclm_time(it1,itrc)

          if (cff1<dt .and. nttclm(itrc)>1) synchro_flag=.true.

          if (nttclm(itrc)==1) then         ! Load time-invariant
            if (iic==0) then                ! tracer climatology.
              do k=1,N
                do j=jstrR,jendR
                  do i=istrR,iendR
                    tclm(i,j,k,itrc)=tclima(i,j,k,ittclm(itrc),itrc)
                  enddo
                enddo
              enddo
            endif
          else
            cff=1./(cff1+cff2)               ! Interpolate tracer
            cff1=cff1*cff                    ! climatology in time.
            cff2=cff2*cff
            do k=1,N
              do j=jstrR,jendR
                do i=istrR,iendR
                  tclm(i,j,k,itrc)=cff1*tclima(i,j,k,it1,itrc)
     &                            +cff2*tclima(i,j,k,it2,itrc)
                enddo
              enddo
            enddo
            if (cff1<0. .or. cff2<0.) then
              if (ZEROTH_TILE) then
                write(*,'(/1x,3A/3(1x,A,F16.10)/)')   '### WARNING: ',
     &            'set_tclima_tile :: Current model time is outside ',
     &            'bounds of ''tclm_time''.', 'tclm_tstart =',
     &             tclm_time(it1,itrc)*sec2day, 'tdays =',      tdays,
     &            'tclm_tend =', tclm_time(it2,itrc)*sec2day
              endif
              ierr=ierr+1
            endif
          endif
# if defined BIOLOGY || defined BIOLOGY_NPZDOC
#  define temp cff
#  define SiO4 cff1
        elseif (itrc==iNO3_) then     ! <-- analytical setup for
          do k=1,N                      ! climatology for biological
            do j=jstrR,jendR            ! constituents.
              do i=istrR,iendR
                temp=tclm(i,j,k,itemp)
                if (temp<8.) then
                  SiO4=30.
                elseif (temp>=8. .and. temp<=11.) then
                  SiO4=30.-((temp-8.)*(20./3.))
                elseif (temp>11. .and. temp<=13.) then
                  SiO4=10.-((temp-11.)*(8./2.))
                elseif (temp>13. .and. temp<=16.) then
                  SiO4=2.-((temp-13.)*(2./3.))
                elseif (temp>16.) then
                  SiO4=0.
                endif
                     tclm(i,j,k,iNO3_)=1.67+0.5873*SiO4
     &                    +0.0144*SiO4**2
     &                    +0.0003099*SiO4**3

!hf: by default, do not perform the cutoff!
#ifdef NO3_SFC_CUTOFF 

! Cut off surface NO3

                tclm(i,j,k,iNO3_)=tclm(i,j,k,iNO3_)
     &               *(1-0.5*(tanh((z_r(i,j,k)+100.)/8.)+1.))
#endif /* NO3_SFC_CUTOFF */
              enddo
            enddo
          end do
         else if (itrc == iNH4_ .or. itrc == iSDet .or.
     &           itrc == iLDet) then
            tclm(:,:,:,itrc)=0.0
         else if (itrc == iChla) then
! small positive concentrations for plankton to avoid "dead water"
            tclm(:,:,:,itrc)=0.2
         else if (itrc == iPhyt) then
! small positive concentrations for plankton to avoid "dead water"
            tclm(:,:,:,itrc)=0.1
         else if (itrc == iZoo_) then
            tclm(:,:,:,itrc)=0.05
#  ifdef OXYGEN
! the following three values are way too high at depth, these default
! values are mostly provided as convenience for test purposes
         else if (itrc == iO2) then  
            tclm(:,:,:,iO2)=250.0 
#   ifdef CARBON
         else if (itrc == iDIC) then  
            tclm(:,:,:,iDIC)=1988.0 
         else if (itrc == iTALK) then  
            tclm(:,:,:,iTALK)=2214.0 
         else if (itrc == iSDetC) then  
            CN_avg = 0.5 * (CN_P + CN_Z)
            tclm(:,:,:,iSDetC)=tclm(:,:,:,iSDet) * CN_avg
         else if (itrc == iLDetC) then  
            CN_avg = 0.5 * (CN_P + CN_Z)
            tclm(:,:,:,iLDetC)=tclm(:,:,:,iLDet) * CN_avg
         else if (itrc == iCaCO3) then  
            tclm(:,:,:,iCaCO3)=0.0
#   endif /* CARBON */
#  endif /* OXYGEN */
#  undef SiO4
#  undef temp
# endif /* BIOLOGY_NPZDOC */

# if defined BIOLOGY_BEC || defined BIOLOGY_BEC2
          else if (itrc == iNO3) then
#  define temp cff
#  define SiO4 cff1
             do k=1,N
               do j=jstrR,jendR
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
                    tclm(i,j,k,iNO3)=1.67+0.5873*SiO4
     &                   +0.0144*SiO4**2
     &                   +0.0003099*SiO4**3
!hf: by default, do not perform the cutoff!
#ifdef NO3_SFC_CUTOFF 
!
!  Cut off surface NO3
!
                    tclm(i,j,k,iNO3)=tclm(i,j,k,iNO3)*
     &                   (1-.5*(tanh((z_r(i,j,k)+100.)/8.)+1))
#endif /* NO3_SFC_CUTOFF */
                 end do
               end do
             end do

           else if (itrc == iPO4) then
              ! Set PO4 to NO3/16:
#  define temp cff
#  define SiO4 cff1
              do k=1,N
                do j=jstrR,jendR
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
                     tclm(i,j,k,itrc)=(1.67+0.5873*SiO4
     &                    +0.0144*SiO4**2
     &                    +0.0003099*SiO4**3)/16
!hf: by default, do not perform the cutoff!
#ifdef NO3_SFC_CUTOFF 
!
!  Cut off surface NO3
!
                     tclm(i,j,k,itrc)=tclm(i,j,k,itrc)*
     &                    (1-.5*(tanh((z_r(i,j,k)+100.)/8.)+1))
#endif /* NO3_SFC_CUTOFF */
                  end do
                end do
              end do
           else if (itrc == iSiO3) then
              tclm(:,:,:,iSiO3)=tclm(:,:,:,iNO3)
           else if (itrc == iNH4) then
              tclm(:,:,:,iNH4)=0.0
           else if (itrc == iFE) then
              tclm(:,:,:,iFE)=5.395e-4  ! mmol/m3
           else if (itrc == iO2) then
              tclm(:,:,:,iO2)=177.9  ! mmol/m3
           else if (itrc == iDIC) then
              tclm(:,:,:,iDIC)=2200.
           else if (itrc == iALK) then
              tclm(:,:,:,iALK)=2300.
           else if (itrc == iDOC) then
              tclm(:,:,:,iDOC)=0.0 ! 0.001
           else if (itrc == iSPC) then
              tclm(:,:,:,iSPC)=0.1
           else if (itrc == iSPCHL) then
              tclm(:,:,:,iSPCHL)=0.03
           else if (itrc == iSPCACO3) then
              tclm(:,:,:,iSPCACO3)=0.1
           else if (itrc == iDIATC) then
              tclm(:,:,:,iDIATC)=0.1
           else if (itrc == iDIATCHL) then
              tclm(:,:,:,iDIATCHL)=0.03
           else if (itrc == iZOOC) then
              tclm(:,:,:,iZOOC)=0.01
           else if (itrc == iSPFE) then
              tclm(:,:,:,iSPFE)=5.47e-7
           else if (itrc == iDIATSI) then
              tclm(:,:,:,iDIATSI)=0.02735
           else if (itrc == iDIATFE) then
              tclm(:,:,:,iDIATFE)=5.47e-7
           else if (itrc == iDIAZC) then
              tclm(:,:,:,iDIAZC)=0.1
           else if (itrc == iDIAZCHL) then
              tclm(:,:,:,iDIAZCHL)=0.03
           else if (itrc == iDIAZFE) then
              tclm(:,:,:,iDIAZFE)=5.47e-7
           else if (itrc == iDON) then
              tclm(:,:,:,iDON)=0.0 ! 1.368e-4
           else if (itrc == iDOFE) then
              tclm(:,:,:,iDOFE)=0.0 ! 5.47e-9
           else if (itrc == iDOP) then
              tclm(:,:,:,iDOP)=0.0 ! 8.55e-4
# endif /* BIOLOGY_BEC || BIOLOGY_BEC2 */
! Provide default values for additional tracers in the later version of BEC:
# ifdef BIOLOGY_BEC2
           else if (itrc == iDOPR) then
              tclm(:,:,:,iDOPR)=0.0
           else if (itrc == iDONR) then
              tclm(:,:,:,iDONR)=0.0
# endif
!DL:
# if defined PASSIVE_TRACER && defined AGE_DYE_TRACER
           else if (itrc.gt.itemp+ntrc_salt .and. itrc.le.itemp+ntrc_salt+N_agedye) then
              is = itrc - (itemp+ntrc_salt)
              if (.not. Age(is)) then
                 ! Dye tracer:
                 tclm(:,:,:,itrc) = 0.0
              else
                 ! Age tracers: set age to time elapsed since start of simulation,
                 ! except at the surface:
                 do k = 1,N-1
                    tclm(:,:,k,itrc) = time*sec2day
#  ifdef USE_REAL_YEAR
     &                  / 365
#  else
     &                  / 360
#  endif
                 enddo
                 tclm(:,:,N,itrc) = 0.0
              endif ! .not. Age(is)
# endif /* PASSIVE_TRACER && AGE_DYE_TRACER */
        endif   ! <-- tclm_tid(itrc)>0
      enddo   ! <-- itrc
      return
      end
#else
      subroutine get_tclima_empty
      end
#endif
