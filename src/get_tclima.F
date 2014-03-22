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
# define TCLIMA_DATA
# include "param.h"
# include "climat.h"
# include "scalars.h"
# include "ocean3d.h"

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
# ifdef BIOLOGY
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

                tclm(i,j,k,iNO3_)=1.67 + SiO4*( 0.5873 + SiO4*(
     &                                0.0144 + SiO4*0.0003099 ))

! Cut off surface NO3

                tclm(i,j,k,iNO3_)=tclm(i,j,k,iNO3_)
     &               *(1-0.5*(tanh((z_r(i,j,k)+100.)/8.)+1.))
              enddo
            enddo
          enddo
        elseif (iic==ntstart .and. itrc==iNH4_) then
          do k=1,N
            do j=jstrR,jendR
              do i=istrR,iendR
                if (tclm_tid(iNH4_)<0) tclm(i,j,k,iNH4_)=0.10
                if (tclm_tid(iChla)<0) tclm(i,j,k,iChla)=0.08
                if (tclm_tid(iPhyt)<0) tclm(i,j,k,iPhyt)=0.06
                if (tclm_tid(iZoo_)<0) tclm(i,j,k,iZoo_)=0.04
                if (tclm_tid(iSDet)<0) tclm(i,j,k,iSDet)=0.02
                if (tclm_tid(iLDet)<0) tclm(i,j,k,iLDet)=0.02
              enddo
            enddo
          enddo
#  undef SiO4
#  undef temp
# endif
        endif   ! <-- tclm_tid(itrc)>0
      enddo   ! <-- itrc
      return
      end
#else
      subroutine get_tclima_empty
      end
#endif
