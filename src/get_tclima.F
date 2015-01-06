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
      if (iic.eq.ntstart) then
# ifdef MULT_CLIM_FILES
         if (any(ncidclm.eq.-1)) then
            do i = 1,nclimfiles
               lstr=lenstr(clm_file(i))
               ierr=nf_open (clm_file(i)(1:lstr), nf_nowrite, ncidclm_loc(i))
               if (ierr .ne. nf_noerr) then
                  write(stdout,'(/1x,4A/)') 'ERROR ',
     &            'in get_tclima: can not open netCDF file ''',
     &            clm_file(i)(1:lstr),   '''.'
                  goto 99
               end if
            end do
            ! For each tracer, check all the climatology files.
            ! If no file contains values for tracer itrc, then set ncidclm(itrc)
            ! to 0 (and a default value will be used).
            do itrc=1,NT
               var_found = .false.
               lvar=lenstr(vname(1,indxT+itrc-1))
               do i = 1,nclimfiles
                  ierr=nf_inq_varid (ncidclm_loc(i),vname(1,indxT+itrc-1),ii)
                  if (ierr .eq. nf_noerr) then
                     ! Just to be sure: check also that the data times are there:
                     ierr=nf_inq_varid (ncidclm_loc(i),tclm_name(itrc),ii)
                     if (ierr .ne. nf_noerr) then
                        lstr=lenstr(clm_file(i))
                        write(stdout,'(/1x,4A/)') 'ERROR ',
     &                  'in get_tclima: cannot find variable ',tclm_name(itrc),
     &                  ' in file ',clm_file(i)(1:lstr)
                        goto 99
                     end if
                     var_found = .true.
                     lstr=lenstr(clm_file(i))
                     ncidclm(itrc) = ncidclm_loc(i)
                     clm_file_tr(itrc) = clm_file(i)(1:lstr)
                     tclm_tid(itrc) = ii
                     MPI_master_only write(stdout,'(6x,4a)') 
     &                  'get_tclima -- found data for ',
     &                  vname(1,indxT+itrc-1)(1:lvar),
     &                  ' in file ',clm_file(i)(1:lstr)
                     exit
                  end if
               end do ! i = 1,nclimfiles
               ! If variable has not been found in any clim file, then we set
               ! ncidclm(itrc) to zero and tclm_tid(itrc) to -1:
               if (.not.var_found) then
                  ncidclm(itrc) = 0
                  clm_file_tr(itrc) = ''
                  tclm_tid(itrc) = -1
                  MPI_master_only write(stdout,'(6x,4a)') 
     &                  'get_tclima -- ','NO data found for ',
     &                  vname(1,indxT+itrc-1)(1:lvar),
     &                  ' in climatology file(s). Using default value.'
                  ierr = nf_noerr
               end if
            end do ! itrc=1,NT
         end if  ! any(ncidclm.eq.-1)
# else /* MULT_CLIM_FILES */
        if (any(ncidclm.eq.-1)) then
          lstr=lenstr(clm_file)
          ierr = nf_open(clm_file(1:lstr), nf_nowrite, ncidclm_loc)
          if (ierr .ne. nf_noerr) then
             lstr=lenstr(clm_file)
             write(stdout,'(/1x,4A/)') 'ERROR ',
     &            'in get_tclima: can not open netCDF file ''',
     &            clm_file(1:lstr),   '''.'
             goto 99
          end if
          ! For each tracer, we check whether the climatology file contains
          ! matching data:
          do itrc=1,NT
             var_found = .false.
             lvar=lenstr(vname(1,indxT+itrc-1))
             ierr=nf_inq_varid (ncidclm_loc,vname(1,indxT+itrc-1),ii)
             if (ierr .eq. nf_noerr) then
                ! Just to be sure: check also that the data times are there:
                ierr=nf_inq_varid (ncidclm_loc,tclm_name(itrc),ii)
                if (ierr .ne. nf_noerr) then
                   lstr=lenstr(clm_file)
                   write(stdout,'(/1x,4A/)') 'ERROR ',
     &                  'in get_tclima: cannot find variable ',tclm_name(itrc),
     &                  ' in file ',clm_file(1:lstr)
                   goto 99
                end if
                var_found = .true.
                lstr=lenstr(clm_file)
                ncidclm(itrc) = ncidclm_loc
                clm_file_tr(itrc) = clm_file(1:lstr)
                tclm_tid(itrc) = ii
                MPI_master_only write(stdout,'(6x,4a)') 
     &                  'get_tclima -- found data for ',
     &                  vname(1,indxT+itrc-1)(1:lvar),
     &                  ' in file ',clm_file(1:lstr)
             end if
             ! If variable has not been found in any clim file, then we set
             ! ncidclm(itrc) to zero and tclm_tid(itrc) to -1:
             if (.not.var_found) then
                ncidclm(itrc) = 0
                clm_file_tr(itrc) = ''
                tclm_tid(itrc) = -1
                MPI_master_only write(stdout,'(6x,4a)') 
     &             'get_tclima -- ','NO data found for ',
     &              vname(1,indxT+itrc-1)(1:lvar),
     &             ' in climatology file. Using default value.'
                ierr = nf_noerr
             end if
          end do ! itrc=1,NT
        end if  ! any(ncidclm.eq.-1)
# endif /* MULT_CLIM_FILES */


        do itrc=1,NT
           if (ierr.eq.nf_noerr .and. tclm_tid(itrc).gt.0) then
              lvar=lenstr(vname(1,indxT+itrc-1))
              ierr=nf_inq_varid (ncidclm(itrc), vname(1,indxT+itrc-1)
     &                                  (1:lvar), tclm_id(itrc))
              if (ierr .eq. nf_noerr) then
                call set_cycle (ncidclm(itrc),tclm_tid(itrc), nttclm(itrc),
     &                          tclm_cycle(itrc),  tclm_ncycle(itrc),
     &                                         tclm_rec(itrc), ierr)

                ittclm(itrc)=1
                tclm_time(1,itrc)=-1.E+20
                tclm_time(2,itrc)=-1.E+20
              else
                lstr=lenstr(clm_file_tr(itrc))
                write(stdout,1) vname(1,indxT+itrc-1)(1:lvar),
     &                          itrc, clm_file_tr(itrc)(1:lstr)
              endif
           endif ! if (ierr.eq.nf_noerr .and. tclm_tid(itrc).gt.0)
        enddo ! do itrc=1,NT
      endif ! if (iic.eq.ntstart)

  1   format(/1x,'ERROR in get_tclima: cannot find variable ''',
     &    A, ''', itrc =',I3/7x, 'in netCDF file ''', A, '''.' /)
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
               ierr=nf_get_var1_FTYPE(ncidclm(itrc), tclm_tid(itrc),
     &            tclm_rec(itrc), cff)
               if (ierr.eq.nf_noerr) then
                  ittclm(itrc)=min(3-ittclm(itrc),nttclm(itrc))
                  tclm_time(ittclm(itrc),itrc)=cff*day2sec
     &               + tclm_cycle(itrc)*tclm_ncycle(itrc)

                  ierr=nf_fread (tclima(START_2D_ARRAY,1,ittclm(itrc),
     &                                itrc), ncidclm(itrc), tclm_id(itrc),
     &                                      tclm_rec(itrc), r3dvar)
                  if (ierr.eq.nf_noerr) then
                    MPI_master_only write(stdout,
     &              '(6x,A,I3,1x,A,F12.4,2(1x,A,I4))')
     &              'get_tclima -- read climatology for tracer', itrc,
     &              'tclm_time =', cff,  'rec =', 
     &              tclm_rec(itrc) MYID
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

      else if (FIRST_TIME_STEP) then  ! no data found for tracer itrc
         if (itrc .eq. itemp) then
            ! Default value for temperature: mean of WOA2005
            do k=1,N
               do j=jstrR,jendR
                  do i=istrR,iendR
                     tclm(i,j,k,itemp) = 3.568  ! C
                  end do
               end do
            end do
         else if (itrc .eq. isalt) then
            ! Default value for salinity: mean of WOA2005
            do k=1,N
               do j=jstrR,jendR
                  do i=istrR,iendR
                     tclm(i,j,k,isalt) = 34.72  ! PSU
                  end do
               end do
            end do
# ifdef BIOLOGY_NPZDOC
! hf: Note that the following settings are only provided for cases
! where no boundary conditions for the bgc state variables have been
! provided, e.g. test runs. In all "serious" model runs, boundary 
! conditions should be provided at least for NO3, O2, DIC, and Alk,
! and ideally for Chla, Phyto, and Zoo as well.
         else if (itrc .eq. iNO3_) then
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
#  undef SiO4
#  undef temp
# endif /* BIOLOGY_NPZDOC */

# if defined BIOLOGY_BEC || defined BIOLOGY_BEC2
          else if (itrc .eq. iNO3) then
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

           else if (itrc .eq. iPO4) then
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
           else if (itrc .eq. iSiO3) then
              tclm(:,:,:,iSiO3)=tclm(:,:,:,iNO3)
           else if (itrc .eq. iNH4) then
              tclm(:,:,:,iNH4)=0.0
           else if (itrc .eq. iFE) then
              tclm(:,:,:,iFE)=5.395e-4  ! mmol/m3
           else if (itrc .eq. iO2) then
              tclm(:,:,:,iO2)=177.9  ! mmol/m3
           else if (itrc .eq. iDIC) then
              tclm(:,:,:,iDIC)=2200.
           else if (itrc .eq. iALK) then
              tclm(:,:,:,iALK)=2300.
           else if (itrc .eq. iDOC) then
              tclm(:,:,:,iDOC)=0.0 ! 0.001
           else if (itrc .eq. iSPC) then
              tclm(:,:,:,iSPC)=0.1
           else if (itrc .eq. iSPCHL) then
              tclm(:,:,:,iSPCHL)=0.03
           else if (itrc .eq. iSPCACO3) then
              tclm(:,:,:,iSPCACO3)=0.1
           else if (itrc .eq. iDIATC) then
              tclm(:,:,:,iDIATC)=0.1
           else if (itrc .eq. iDIATCHL) then
              tclm(:,:,:,iDIATCHL)=0.03
           else if (itrc .eq. iZOOC) then
              tclm(:,:,:,iZOOC)=0.01
           else if (itrc .eq. iSPFE) then
              tclm(:,:,:,iSPFE)=5.47e-7
           else if (itrc .eq. iDIATSI) then
              tclm(:,:,:,iDIATSI)=0.02735
           else if (itrc .eq. iDIATFE) then
              tclm(:,:,:,iDIATFE)=5.47e-7
           else if (itrc .eq. iDIAZC) then
              tclm(:,:,:,iDIAZC)=0.1
           else if (itrc .eq. iDIAZCHL) then
              tclm(:,:,:,iDIAZCHL)=0.03
           else if (itrc .eq. iDIAZFE) then
              tclm(:,:,:,iDIAZFE)=5.47e-7
           else if (itrc .eq. iDON) then
              tclm(:,:,:,iDON)=0.0 ! 1.368e-4
           else if (itrc .eq. iDOFE) then
              tclm(:,:,:,iDOFE)=0.0 ! 5.47e-9
           else if (itrc .eq. iDOP) then
              tclm(:,:,:,iDOP)=0.0 ! 8.55e-4
# endif /* BIOLOGY_BEC || BIOLOGY_BEC2 */
! Provide default values for additional tracers in the later version of BEC:
# ifdef BIOLOGY_BEC2
           else if (itrc .eq. iDOPR) then
              tclm(:,:,:,iDOPR)=0.0
           else if (itrc .eq. iDONR) then
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
      endif   ! <-- itrc .eq. itemp
      endif   ! <-- tclm_tid(itrc).gt.0
      enddo   ! <-- itrc
      return
      end
#else
      subroutine get_tclima_empty
      end
#endif
