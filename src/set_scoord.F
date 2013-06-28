#include "cppdefs.h"
#ifdef SOLVE3D
                                     ! Setup S-coordinate system:
      subroutine set_scoord          ! input:  theta_s, theta_b, hc
      implicit none                  ! output: Cs_w(0:N), Cs_r(1:N) 
# include "param.h"
# include "scoord.h"
# include "scalars.h"
# include "comm_vars.h"
# include "grid.h"
# include "ncvars.h"
# include "netcdf.inc"

! LRK Dec 2010
! Check for consistency in sigma levels between clim files 
! and those calculated in roms

! Open first a climatology netCDF file for reading in variable.
! If  MULT_CLIM_FILES  is defined, then ncidclm and clm_file 
! are arrays, and we need to pick one element. The s-coordinate 
! info is assumed to be in the first climatology file 
! (like the temperature).
 
      integer ncidclm_loc, Csr_id, scr_id, ths_id, thb_id
      character(len=180) clm_file_loc
      integer k, ierr, ncid, varid, lstr, lenstr, checkdims, nf_fread
            
      integer start
      integer count
      double precision Csvals(N), scrvals(N), ths_val, thb_val
      double precision Cs_diff(n), sc_diff(N)

      real ds,sc,sc2,cff,cff1,cff2, CSF 

      data start/1/
      data count/N/  

      ncidclm_loc = ncidclm(itemp)

# ifdef MULT_CLIM_FILES
      clm_file_loc = clm_file(1)
# else
      clm_file_loc = clm_file
# endif

      ierr=nf_noerr
      lstr=lenstr(clm_file_loc)

         if (ncidclm_loc .eq. -1) then
            ierr=nf_open(clm_file_loc(1:lstr), nf_nowrite, ncidclm_loc)
            ncidclm(itemp) = ncidclm_loc

            if (ierr .ne. nf_noerr) then 
               write(stdout,'(/1x,4A/)') 'ERROR ',
     &                   'in set_scoord.F: can not open netCDF file ''',
     &                                       clm_file_loc(1:lstr), '''.'
            endif
 
         else
           print *,'Error! Cannot identify the netcdf file to open'
         endif 
  
!
! Inquire about the variable ID then read in data. Model theta_s, 
! theta_b, hc, Cs_w, and Cs_r are global variables read in read_inp.F.  
! Store values from the clim file in vector Csvals(N) for later comparison
!

      ierr=nf_inq_varid (ncidclm_loc, 'theta_s', ths_id)
      if (ierr .eq. nf_noerr) then
        ierr=nf_get_vara_double(ncidclm_loc, ths_id, 1, 1, ths_val)
        if (ierr .eq. nf_noerr) then
           ierr=ths_val - theta_s
           if (ierr .ne. nf_noerr) then
            MPI_master_only print *,'Warning!! Theta_s values differ between' 
            MPI_master_only print *,'clim:',ths_val 
            MPI_master_only print *,'and .in files !',theta_s 
           endif 
       else
          print *, 'Error! Cannot retrieve theta_s value'
        endif
      else
         print *,'Error! Variable inquiry failed'
      endif

      ierr=nf_inq_varid (ncidclm_loc, 'theta_b', thb_id)
      if (ierr .eq. nf_noerr) then
        ierr=nf_get_vara_double(ncidclm_loc, thb_id, 1, 1, thb_val)
        if (ierr .eq. nf_noerr) then
          ierr=thb_val - theta_b
          if (ierr .ne. nf_noerr) then
            MPI_master_only print *,'Warning!! Theta_b values'
            MPI_master_only print *,' differ between clim and .in files!' 
          endif
        else
          print *, 'Error! Cannot retrieve theta_b value'
        endif
      else
         print *,'Error! Variable inquiry failed'
      endif

      ierr=nf_inq_varid (ncidclm_loc, 'Cs_r', Csr_id)
      if (ierr .eq. nf_noerr) then
        ierr=nf_get_vara_double(ncidclm_loc, Csr_id, start, count, Csvals)
        if (ierr .eq. nf_noerr) then
          MPI_master_only print *, 'Read in Cs_r:'
        else
          print *, 'Error! Cannot retrieve Cs_r values'
        endif
      else
         print *,'Error! Variable inquiry failed'
      endif

      ierr=nf_inq_varid (ncidclm_loc, 'sc_r', scr_id)
      if (ierr .eq. nf_noerr) then
        ierr=nf_get_vara_double(ncidclm_loc, scr_id, start, count, scrvals)
        if (ierr .eq. nf_noerr) then
          MPI_master_only print *, 'Read in Sc_r:'
        else
          print *, 'Error! Cannot retrieve sc_r values'
        endif
      else
         print *,'Error! Variable inquiry failed'
      endif
                                     ! Compute vertical stretching
                                     ! curves at W- and RHO-points,
# ifdef LEGACY_S_COORD
      if (hc.le.hmin) then           !    -1 < Cs_r,Cs_w < 0, 
# endif
        ds=1.D0/dble(N)              ! then print z-coordinates 
        Cs_w(N)=0.D0                 ! of vertical S-levels over
        do k=N-1,1,-1                ! depths: minumum, maximum
          sc=ds*dble(k-N)            ! and half-way over slope.
          Cs_w(k)=CSF(sc, theta_s, theta_b)
        enddo
        Cs_w(0)=-1.D0
        do k=1,N
c          sc2=(dble(k-N)-0.5D0)/dble(N)
          sc2=ds*(dble(k-N)-0.5D0)
          Cs_r(k)=CSF(sc2, theta_s, theta_b)

          Cs_diff(k)=Csvals(k)-Cs_r(k)
          sc_diff(k)=scrvals(k)-sc2
          
          if (Cs_diff(k) .ne. 0) then
          MPI_master_only print *,'k= ',k, 'Cs_r diff (clm-model): ',Cs_diff(k)
          endif

          if (sc_diff(k) .ne. 0) then
          MPI_master_only print *,'k= ',k, 'sc_diff (clm-model): ',sc_diff(k)
          endif
        enddo

        MPI_master_only print *,'If significant clm-model differences'
        MPI_master_only print *,'are listed above, check to make sure'
        MPI_master_only print *,'that the S-coord type is consistent '
        MPI_master_only print *,'between Romstools input and Roms.'
        MPI_master_only print *,'Cs_r and sc_r could also be switched'
        MPI_master_only print *,'in the clm file, in which case there'
        MPI_master_only print *,'is likely no problem with this run.'

        MPI_master_only write(stdout,'(/1x,A/,/1x,A,10x,A/)')
     &                         'Vertical S-coordinate System:',
     &                         'level   S-coord     Cs-curve',
     &                         'at_hc  over_slope     at_hmax'
        do k=N,0,-1
# ifdef NEW_S_COORD
          sc=ds*dble(k-N)
          cff=0.5*(hmax+hmin)
          cff1=cff *(hc*sc + cff *Cs_w(k))/(hc +cff)
          cff2=hmax*(hc*sc + hmax*Cs_w(k))/(hc+hmax)
          cff= hc  *(hc*sc + hc  *Cs_w(k))/(hc + hc)
# else
          cff=hc*ds*(k-N)
          cff1=cff +0.5D0*(hmax+hc)*Cs_w(k)
          cff2=cff +      (hmax-hc)*Cs_w(k)
# endif
          MPI_master_only write(stdout,'(I6,2F12.7,4x,3F12.3)')
     &                    k, ds*(k-N), Cs_w(k), cff, cff1,cff2
        enddo
# ifndef NEW_S_COORD 
      else
        write(stdout,'(/1x,2A,F7.2/12x,A,F7.2/)') '### ERROR: ',
     &    'Specified S-coordinate critical depth  hc   =', hc,
     &    'exceeds minimum unmasked topography.   hmin =', hmin
        may_day_flag=8
      endif
# endif
      return
      end


# if defined LEGACY_S_COORD && defined NEW_S_COORD

      function CSF (sc, theta_s,theta_b)          ! Legacy
      real*8 CSF, sc, theta_s,theta_b             ! S-coordinate
                                                  ! transformation
      if (theta_s.gt.0.D0) then
        CSF=(1.D0-theta_b)*sinh(theta_s*sc)/sinh(theta_s)
     &           +theta_b*( 0.5D0*tanh(theta_s*(sc+0.5D0))
     &                       /tanh(0.5*theta_s)   -0.5D0 )
      else
        CSF=sc
      endif
      return
      end

# else /* NEW_S_COORD  and not LEGACY_S_COORD used in ETH Roms Version 1.0 */
                                            ! NOTE: Mathematical 
      function CSF (sc, theta_s,theta_b)    ! limits of CSF,csrf for
      implicit none                         ! theta_s, theta_b --> 0
      real*8 CSF, sc, theta_s,theta_b,csrf  ! match that under "else"
                                            ! logical branches.

      if (theta_s .gt. 0.D0) then
        csrf=(1.D0-cosh(theta_s*sc))/(cosh(theta_s)-1.D0)
      else
        csrf=-sc**2
      endif
      if (theta_b .gt. 0.D0) then
        CSF=(exp(theta_b*csrf)-1.D0)/(1.D0-exp(-theta_b))
c        CSF=(exp(theta_b*(csrf+1.D0))-1.D0)/(exp(theta_b)-1.D0) -1.D0
      else
        CSF=csrf
      endif
      return
      end

# endif    /* NEW_S_COORD */



      subroutine check_scoord_switches (ierr)
      implicit none
      integer ierr, is,ie, lenstr
# include "param.h"
# include "strings.h"
      ie=lenstr(cpps)
      is=ie+2
      ie=is+10
      if (ie.gt.max_opt_size) goto 99
      cpps(is:ie)='<scoord.h>'
      MPI_master_only write(stdout,'(1x,A)') cpps(is:ie)
      is=ie+2
# ifdef NEW_S_COORD 
      ie=is+14
      if (ie.gt.max_opt_size) goto 99
      cpps(is:ie)='NEW_S_COORD'
      MPI_master_only write(stdout,'(10x,A)') cpps(is:ie)
      is=ie+2
# endif
      return
  99  MPI_master_only write(stdout,'(/1x,2A/12x,A/)') '### ERROR: ',
     &  'Unsufficient lenght of string "cpps" in file "strings.h".',
     &        'Increase parameter "max_opt_size" it and recompile.'
      ierr=ierr+1
      return
      end

#else
      subroutine set_scoord_empty
      end
#endif /* SOLVE3D */
 
