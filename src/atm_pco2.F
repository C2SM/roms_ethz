c =====================================================================
c
c     SUBROUTINE get_atmco2
c
c     PURPOSE: determines atmospheric CO2 concentration at the
c                current time step of the model. Based on code
c                supplied by OCMIP2 (J. Orr)
c
c
c     VARIABLES:
c 
c     IN:  through modules
c           pert_time : perturbation time (real years)
c
c     OUT: atmco2_t
c
c     AUTHOR:  Nicolas Gruber (gruber@splash.princeton.edu)  
c
c     VERSION: MOM3
c
c     REVISIONS:
c
c     date     author   remarks
c     
c     07.12.98  ng      started coding, adapted from update_bc written
c                         by Rick Slater
c     14.12.98  ng      consider using subroutine timeinterp.F
c     17.12.98  ng      corrected error in bc_fract_next_mo calculation
c     March/April 2009 dl small modifications to get it working in ROMS.
c                       Last argument (previously called pert_atmco2) is now
c                       the atmospheric CO2 conc. at time pert_time (previously
c                       it was the difference to the preindustrial conc)
c      
c =====================================================================
c
      subroutine get_atmco2(pert_time,atmco2_out) 
c
c-----------------------------------------------------------------------
c     modules (have to come first)
c-----------------------------------------------------------------------
c
!DL      use stdunits
c
!DL      use pconst
c
c-----------------------------------------------------------------------
c     common blocks
c-----------------------------------------------------------------------
c
c      implicit none
c
c-----------------------------------------------------------------------
c     local variables
c-----------------------------------------------------------------------
c	
      integer maxrec
      parameter (maxrec=1200)
      
      real, intent(in)	:: pert_time
c      character*4, intent(in):: futr_scen
      real, intent(out)	:: atmco2_out
      
      integer, save		:: nrec_co2
      real, save	:: year_co2(maxrec), atmco2(maxrec)
  
      integer :: n

!DL: the following change is necessary due to mpc, the version with
!DL two colons is not handled correctly.
!DL      real	:: yr
!DL      real	:: x
      real x, yr

      integer, save	:: ientry = 0

      real yrco2min, yrco2max
c      real, parameter	:: yrco2min = 1765.0
c      real, parameter	:: yrco2max = 2300.5
c
c-----------------------------------------------------------------------
c     Counter "ientry" for number of entries in this SUBROUTINE
c     (Read atmospheric records on 1st entry ONLY)
c     use CIS9 scenario
c-----------------------------------------------------------------------
c	
      ientry = ientry + 1

      if (ientry .eq. 1) then
!DL         call read_co2atm('CIS9',nrec_co2, year_co2, atmco2)
         call read_co2atm(nrec_co2, year_co2, atmco2)
      endif
c
c-----------------------------------------------------------------------
c     assign perturbation time (real years e.g. 1935.67)
c-----------------------------------------------------------------------
c	
      yrco2min = year_co2(1)
      yrco2max = year_co2(nrec_co2)
      if (pert_time .lt. yrco2min) then 
         yr = yrco2min
      else if((pert_time .ge. yrco2min)  .and.  
     *        (pert_time .le. yrco2max)) then 
         yr = pert_time
      else if (pert_time .gt. yrco2max) then 
         yr = yrco2max
      end if 
c
c-----------------------------------------------------------------------
c     Find relative POSITION n for pert_time in CO2 record
c-----------------------------------------------------------------------
c	
      call locate(year_co2,nrec_co2, yr, n)
c
c-----------------------------------------------------------------------
c     Determine linear interpolation factor "x"
c-----------------------------------------------------------------------
c	
      x = (yr - year_co2(n)) / (year_co2(n+1) - year_co2(n))
c
c-----------------------------------------------------------------------
c     Perform temporal interpolation for atmospheric CO2, subtract
c      pre-industrial concentration to get perturbation CO2
c-----------------------------------------------------------------------
c    
!DL      pert_atmco2 = (atmco2(n) * (1. - x) + atmco2(n+1) * x) -
!     *     atmco2(1)
      atmco2_out = atmco2(n) * (1. - x) + atmco2(n+1) * x
c     
c-----------------------------------------------------------------------
c     end of subroutine
c-----------------------------------------------------------------------
c
      return
      end subroutine  get_atmco2




c =====================================================================
c
c     SUBROUTINE read_co2atm
c
c     PURPOSE: Reads temporal history of atmospheric CO2 (uatm)
c               based on OCMIP2 code
c
c     Note: Variable TYPE is given in square brackets (below)
c     (r-REAL, i-INTEGER, l-LOGICAL, c-CHARACTER; s-scaler, a-array).
c     ===============
c     INPUT:
c     -------
c     [cs]  futr_scen  = IPCC future scenario: either S350, S450, S550,
c                        S650, S750, DS450, or DS550 from Enting et al.
c                        (1994), or CIS9 signifying c-IS92A for
c                        IPCC (2000) run.  From 1765-1990.5, it 
c                        doesn't matter which scenario you use, i.e., 
c                        atmospheric CO2 will be the same (from a
c                        spline fit to Siple Ice core and Mauna Loa
c                        data.  Subsequently, atmospheric CO2 is 
c                        different, according to the choice given above.
c
c     OUTPUT: 
c     -------
c     [is]  nco2rec    =  Number of records (years) for atmospheric CO2
c                         from historical (splco2.dat) plus
c                         future (stab.dat) records
c     [ra]  yrco2rec   =  sequential list of times (in decimal years)
c                         for WHEN atmospheric CO2 data is available
c     [ra]  atmco2rec  =  corresponding sequential list of atmospheric 
c                         co2 (ppm).
c                         This record is from Enting et al. (1994).
c     ==================================================================
c     
c     Reference
c     ---------
c     Enting, I.G., T. M. L. Wigley, M. Heimann, 1994. Future emissions 
c     and concentrations of carbon dioxide: key ocean / atmosphere / 
c     land analyses, CSIRO Aust. Div. Atmos. Res., Tech. Pap. No. 31, 
c     118 pp.
c     ------------------------------------------------------------------
c
c     AUTHOR:  Nicolas Gruber (gruber@splash.princeton.edu)  
c                 based on OCMIP2 code supplied by Jim Orr
c
c     VERSION: MOM3
c
c     REVISIONS:
c
c     date     author   remarks
c     
c     08.01.00  ng      started coding
c      
c =====================================================================
c
 
      subroutine read_co2atm(nco2rec, yrco2rec, atmco2rec)
c
      implicit none
c
c-----------------------------------------------------------------------
c     local variables
c-----------------------------------------------------------------------
c
      integer maxrec
      parameter (maxrec=1200)
      
      integer , intent(out)		:: nco2rec
      real, intent(out)	:: yrco2rec(maxrec), atmco2rec(maxrec)
      integer irec
      
c      integer	:: lun(3), irec
c      integer is, ifuture, ireadf
c      integer nsipl, nstab
      
c      real futco2(nmxr,8)
c      real dummy
      
c
c =====================================================================
c     begin of executable code
c =====================================================================
c
c
c-----------------------------------------------------------------------
c     open file
c-----------------------------------------------------------------------
c
      open(10,file='atm_pco2.dat',status='old',form='formatted',access=
     &    'sequential',position='rewind',err=219)
c
c-----------------------------------------------------------------------
c     give info
c-----------------------------------------------------------------------
c      
!      write(*,*) '  '
!      write(*,'(a)') 'File atm_pco2.dat opened.'      
c
c-----------------------------------------------------------------------
c     read data from file (pairs year, pco2)
c-----------------------------------------------------------------------
c
      nco2rec = 0 ! number of pco2 data values read
      do irec=1,maxrec
            read(10,*,err=220,end=222) yrco2rec(irec), atmco2rec(irec)
            nco2rec = nco2rec + 1
      end do
      go to 222
      
 219  write (*,'(a,1x,a)') 'File(s) with atmospheric pCO2 data could',
     &   'not be opened!'
      stop
 220  write(*,'(2a)') 'Error occurred while reading from file',
     &   'atm_pco2.dat.'
      stop
 222  continue
c
      close(10)
c
c-----------------------------------------------------------------------
c     end of subroutine
c-----------------------------------------------------------------------
c
      return
      end subroutine  read_co2atm




c =====================================================================
c
c     SUBROUTINE locate
c
c     PURPOSE: From Numerical recipes
c              Given an array XX of length N, and a given value of X, returns a
c              value of J such that X is between XX(J) and XX(J+1).  XX must be
c              monotonic, either increasing or decreasing. J=0 or J=N is
c              returned to indicate that X is out of range.
c
c
c     VARIABLES:
c 
c     IN:  xx  : array
c          n   : length of array
c          x   : value for which index is sought
c
c     OUT: j   : index so that xx(j) < x < xx(j+1)
c
c     AUTHOR:  Numerical recipes
c
c     VERSION: MOM3
c
c     REVISIONS:
c
c     date     author   remarks
c     
c     08.01.00  ng      adapted from Rick Slater
c      
c =====================================================================
c

	subroutine locate(xx,n,x,j) !{
c
c-----------------------------------------------------------------------
c     local variables
c-----------------------------------------------------------------------
c	
	integer, intent(in)	:: n
	real, intent(in)	:: x
	real, dimension(n), intent(in)	:: xx
	integer, intent(out)	:: j
c
c =====================================================================
c     begin of executable code
c =====================================================================
c	
	jl = 0
	ju = n+1
 10	continue
	if (ju - jl .gt. 1) then
	   jm = (ju + jl) / 2
	   if ((xx(n) .gt. xx(1)) .eqv. (x .gt. xx(jm))) then
	      jl = jm
	   else
	      ju = jm
	   endif
	   go to 10
	endif
	j = jl
c     
c-----------------------------------------------------------------------
c     end of subroutine
c-----------------------------------------------------------------------
c
	return
	end subroutine  locate  !}
