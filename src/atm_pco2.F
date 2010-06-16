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
      subroutine get_atmco2(pert_time,futr_scen,atmco2_out) 
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
      
      real, intent(in) :: pert_time
      character*4, intent(in):: futr_scen
      real, intent(out) :: atmco2_out
      
      integer, save :: nrec_co2
      real, save :: year_co2(maxrec), atmco2(maxrec)
  
      integer :: n

!DL: the following change is necessary due to mpc, the version with
!DL two colons is not handled correctly.
!DL      real :: yr
!DL      real :: x
      real x, yr

      integer, save :: ientry = 0

      real, parameter :: yrco2min = 1765.0
      real, parameter :: yrco2max = 2300.5
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
         call read_co2atm(futr_scen,nrec_co2, year_co2, atmco2)
      endif
c
c-----------------------------------------------------------------------
c     assign perturbation time (real years e.g. 1935.67)
c-----------------------------------------------------------------------
c 
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
 
      subroutine read_co2atm(futr_scen, nco2rec, yrco2rec, atmco2rec)
c
      implicit none
c
c-----------------------------------------------------------------------
c     local variables
c-----------------------------------------------------------------------
c
      integer maxrec, nmxr
      parameter (maxrec=1200, nmxr=700)
      
      character(len=4), intent(in) :: futr_scen
      integer , intent(out)  :: nco2rec
      real, intent(out) :: yrco2rec(maxrec), atmco2rec(maxrec)
      
      integer :: lun(3), irec
      integer is, ifuture, ireadf
      integer nsipl, nstab
      
      real futco2(nmxr,8)
      real dummy
      
      character*4 ipcc_scen(8)
      
c     note that the 1st 7 future scenarios are in file "stab.dat";
c     the last scenario "cis9" is short for "cis92a" (see "cis92a.dat").
      
      data ipcc_scen/'S350', 'S450', 'S550', 'S650', 'S750',  
     *               'DS45', 'DS55', 'CIS9'/
      
c
c =====================================================================
c     begin of executable code
c =====================================================================
c
c
c-----------------------------------------------------------------------
c     determine index for future scenario
c-----------------------------------------------------------------------
c
      ifuture = 0
      do is=1,8
         if (futr_scen(1:4) .eq. ipcc_scen(is))then
            ifuture = is
         endif
      end do
c
c-----------------------------------------------------------------------
c     check to be sure that chosen scenario is from allowed ipcc list
c-----------------------------------------------------------------------
c
      if (ifuture .eq. 0) then
         write(*,*) 'improper 1st argument for read_co2atm.F: ' 
     *        , futr_scen(1:4)
         write(*,'(a,7(1x,a))') 'you must chose from '  
     *        , (ipcc_scen(is), is=1,8)
         write(*,*)
     *        ' for ocmip-2, s650 and cis9 are the only accetable choices'
         stop
      else
         write(*,'(2a)') 'read_co2atm: you have chosen ipcc scenario '
     *        , ipcc_scen(ifuture)
      end if 
c
c-----------------------------------------------------------------------
c     open file
c-----------------------------------------------------------------------
c
!      call getunit (lun(1), 'splco2.dat', 'formatted sequential rewind')
!      call getunit (lun(2), 'stab.dat',   'formatted sequential rewind')
!      call getunit (lun(3), 'cis92a.dat', 'formatted sequential rewind')
      lun(1) = 10
      lun(2) = 11
      lun(3) = 12
      open(lun(1),file='splco2.dat',status='old',form='formatted',access='sequential',position='rewind',err=219)
      open(lun(2),file='stab.dat',status='old',form='formatted',access='sequential',position='rewind',err=219)
      open(lun(3),file='cis92a.dat',status='old',form='formatted',access='sequential',position='rewind',err=219)
c
c-----------------------------------------------------------------------
c     give info
c-----------------------------------------------------------------------
c      
      write(*,*) '  '
      write(*,*) '--------------------------------------------------'
      write(*,*) 'atm. co2 from spline fit to siple-mauna loa record'
      write(*,*) ' and ipcc scenario ', futr_scen(1:4)
      write(*,*) '--------------------------------------------------'
      
c
c-----------------------------------------------------------------------
c     skip first lines in historical data
c-----------------------------------------------------------------------
c      
      read(lun(1),'(////)')
      ireadf = 0
      nsipl = 0
      nstab = 0
      do irec=1,maxrec
 210     continue
c
c--------------------------------------------------------------
c     read either historical or future co2 concentrations, depending
c     upon the value of ireadf, which is enabled (set to 1) during
c     the read operation, at the end of the historical file (if
c     ifuture > 0)
c --------------------------------------------------------------
c
c
c-----------------------------------------------------------------------
c     read from splco2.dat (historical emissions)
c-----------------------------------------------------------------------
c
         if (ireadf .eq. 0) then
            read(lun(1),*,err=220,end=220) yrco2rec(irec), atmco2rec(irec)
            nsipl = nsipl + 1
c
c-----------------------------------------------------------------------
c     read from stab.dat (future atm co2 scenario)
c-----------------------------------------------------------------------
c
         else if (ireadf .eq. 1) then
            
            read(lun(2),*,err=222,end=222) yrco2rec(irec)
     *           , (futco2(irec-nsipl,is), is=1,7)
            atmco2rec(irec) = futco2(irec-nsipl,ifuture)
            nstab = nstab + 1
c
c-----------------------------------------------------------------------
c     read from cis92a
c-----------------------------------------------------------------------
c
         else if (ireadf .eq. 2) then 
            read(lun(3),*,err=222,end=222) yrco2rec(irec)
     *           , futco2(irec-nsipl,8), dummy
            atmco2rec(irec) = futco2(irec-nsipl,8)
            nstab = nstab + 1
         endif
         
         go to 221
c
c-----------------------------------------------------------------------
c     when end of historical co2 reached, turn on read ability
c     (ireadf>0) for future file, go back to read one or the other, 
c     then continue reading chosen future file until it endsc     
c-----------------------------------------------------------------------
c         
 220     continue
         if (ifuture .gt. 0 .and. ifuture .le. 7) then
            ireadf = 1
c
c-----------------------------------------------------------------------
c     read over 1st 5 lines of description + 1st line of data
c     in future file that is repeated from siple recorddetermine index 
c     for future scenario
c-----------------------------------------------------------------------
c           
            read (lun(2),'(/////)')
            go to 210
         elseif (ifuture .eq. 8) then
            ireadf = 2
c
c-----------------------------------------------------------------------
c       read over 1 header line in future file cis92a.dat
c       note that 
c         - 1st line is 1990 (not identical to historical run)
c         - records are yearly, not every 6 months as for 
c           1 <= ifuture <=7      
c       read first line of data (same year as last line in splco2.dat)
c     then for cis92a only, replace that with 
c-----------------------------------------------------------------------
c                                
            read(lun(3),'(1x)')
c
c-----------------------------------------------------------------------
c     then for cis92a only, replace that with 
c-----------------------------------------------------------------------
c                                
            write (*,*) 'replace: nsipl = ', nsipl
            read(lun(3),*)yrco2rec(nsipl), atmco2rec(nsipl), dummy
            write(*,*)yrco2rec(nsipl), atmco2rec(nsipl), dummy
            go to 210
         else
            go to 222
         endif
c 221    nco2rec = nsipl + nstab
 221     continue
         nco2rec = irec
      end do
      go to 222
      
 219  write (*,'(a,1x,a)') 'File(s) with atmospheric pCO2 data could',
     &   'not be opened!'
      stop
 222  continue
c
c-----------------------------------------------------------------------
c      write(0,*) 'number records in splco2.dat:', nsipl
c      write(0,*) 'number records in future (stab.dat or cis92a.dat):', nstab
c      write(0,*) 'sum                          ', nstab + nsiplc     
c-----------------------------------------------------------------------
c                 
!DL      write(*,*) 'atm. co2:  no. of entries for 1-box atmosphere =',	
!     *     nco2rec
c
c-----------------------------------------------------------------------
c     release units
c-----------------------------------------------------------------------
c
!DL      call relunit(lun(1))
!      call relunit(lun(2))
!      call relunit(lun(3))
      close(lun(1))
      close(lun(2))
      close(lun(3))
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
      integer, intent(in) :: n
      real, intent(in) :: x
      real, dimension(n), intent(in) :: xx
      integer, intent(out) :: j
c
c =====================================================================
c     begin of executable code
c =====================================================================
c      
      jl = 0
      ju = n+1
 10   continue
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
