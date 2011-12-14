#include "cppdefs.h"
#ifdef BIOLOGY_BEC

!-------------------------------------------------------------------------
!
!     ************   SUBROUTINE ECOSYS_TILE   ****************************
!
!-------------------------------------------------------------------------

         subroutine ecosys_tile(Istr,Iend,Jstr,Jend)
         implicit none

!-------------------------------------------------------------------------
!    Carrie Zhang   Mar 18,2004
!
!  Computing ecosystem source terms based on ROMS.
!  Origin code is form :
!   Multispecies ecosystem based on Doney et al. 1996, Moore et al., 2002
!   ecosys_mod.F90, Version 2.03., July 2003
!
!  Modifications by:
!  Hartmut Frenzel  December 2004 - January 2005
!  Latest modifications: Dec 18, 2006
!-----------------------------------------------------------------------

#include "param.h"  
!need for im_eco,jm_eco
#include "param_bec.h"
#include "ecosys_bec.h"
#include "ocean3d.h" 
!need for tracer (t in roms)
#include "forces.h" 
!need for surface velocity
#include "scalars.h" 
!need for dt,nnew

         integer::i,j,k,m,nn,ctime
         integer Istr,Iend,Jstr,Jend

         real,dimension(istr:iend,jstr:jend)::
     &    SSTT,   ! sea surface temperature (C)
     &    SSSS,    ! sea surface salinity (psu)
     &    SHF_QSW,             ! penetrative solar heat flux (W/m^2)
     &    QA_dust_def,       ! incoming deficit in the QA(dust) POC flux
     &    PAR_out           ! photosynthetically available radiation (W/m^2)

         real,dimension(istr:iend,jstr:jend,ntrc_bio) :: STF

         real,dimension(istr:iend,jstr:jend,N)::temp 
!         common /sflux/QA_dust_def, PAR_out

#ifdef CH_CARBON_DEPTH
         real,dimension(istr:iend,jstr:jend,N)::sal_ch !to calculate sal and press
                                                       !dependent diss. const.
         real,dimension(istr:iend,jstr:jend,N)::press_ch !in [Pa]
#endif /*CH_CARBON_DEPTH*/

         real,dimension(istr:iend,jstr:jend)::
     &      FICE_USED,   ! used ice fraction (non-dimensional)
     &      WS_USED,     ! used wind speed (m/s) ! HF: changed from (cm/s)
     &      XKW,         ! a * WS ** 2 (m/s) ! HF: changed from (cm/s)
     &      AP_USED,     ! used atmospheric pressure (atm)
     &      XKW_ICE,   ! common portion of piston vel., a*(1-fice)*u**2 (m/s) ! HF: changed from (cm/s)
     &      SCHMIDT_USED, ! used Schmidt number
     &      PV,           ! piston velocity (m/s) ! HF: changed from (cm/s)
     &      O2SAT_1atm,   ! O2 saturation @ 1 atm (mmol/m^3)
     &      O2SAT_USED,   ! used O2 saturation (mmol/m^3)
     &      FLUX          ! tracer flux (mmol/m^2/s)
!HF WARN: tracer flux was in nmol/cm^2/s, is it corrected everywhere???

        real,dimension(istr:iend)::
     &    XCO2,         ! atmospheric co2 conc. (dry-air, 1 atm)
     &    PHLO,         ! lower bound for ph in solver
     &    PHHI,         ! upper bound for ph in solver
     &    PH_NEW,       ! computed PH from solver
     &    CO2STAR_ROW,  ! CO2STAR from solver
     &    DCO2STAR_ROW, ! DCO2STAR from solver
     &    pCO2SURF_ROW, ! pCO2SURF from solver
     &    DpCO2_ROW     ! DpCO2 from solver

        real, PARAMETER :: 
     &    a = 8.6e-7,       ! a = 0.31 cm/hr s^2/m^2 in (s/m)
     &    phlo_init = 5.0,  ! low bound for ph for no prev soln
     &    phhi_init = 9.0,  ! high bound for ph for no prev soln
     &    del_ph = 0.25     ! delta-ph for prev soln


#ifdef CH_CARBON_DEPTH
        real, PARAMETER ::
     &    p0 = 101325.0       ! avg. sea level pressure [Pa]

        real,dimension(istr:iend)::
     &    HCO3_ROW,     ! HCO3 from solver
     &    CO3_ROW       ! CO3 from solver
#endif /*CH_CARBON_DEPTH*/


!DL: needed for interannual variations of pCO2:
# ifdef VARIABLE_ANN_ATM_PCO2
        integer ierr
        real tdays_pco2, curr_year, atm_co2
        external get_atmco2
        real, external:: get_frac_year
# endif


!------------------------------------------------------------------------
!Interface for exchanging variables between ROMS and MGM
!
!--------------------------------------------------------------------------

         ctime = 1
          do m=1,ntrc_bio
             do k=1,N
               tracer(istr:iend,jstr:jend,k,m,ctime)=
     &             t(istr:iend,jstr:jend,k,nnew,1+ntrc_salt+ntrc_pas+m)
            enddo 
         enddo

         sstt(istr:iend,jstr:jend)=t(istr:iend,jstr:jend,N,nnew,1)
         ssss(istr:iend,jstr:jend)=t(istr:iend,jstr:jend,N,nnew,2)
         stf=0.0

#ifdef DAILYPAR_BEC
          shf_qsw(istr:iend,jstr:jend) =
     &               srflx_dailyavg(istr:iend,jstr:jend)*rho0*Cp
#else
         shf_qsw(istr:iend,jstr:jend)=srflx(istr:iend,jstr:jend)*rho0*Cp
#endif
         do k=1,N
            temp(istr:iend,jstr:jend,k)=
     &         t(istr:iend,jstr:jend,k,nnew,1)
         enddo

#ifdef CH_CARBON_DEPTH
         do k=1,N
            sal_ch(istr:iend,jstr:jend,k)=
     &         t(istr:iend,jstr:jend,k,nnew,2) 
         enddo
#endif /*CH_CARBON_DEPTH*/

         do j=jstr,jend
            do i=istr,iend
             if (.NOT. landmask(i,j))  then
                dust(i,j)=0.0
                iron(i,j)=0.0
             endif
           enddo
         enddo

    !------------------------------------------------------------------------
    !   Interpolate gas flux forcing data if necessary
    !------------------------------------------------------------------------

          FICE_USED = IFRAC
       call WS(sustr(istr:iend,jstr:jend),
     &    svstr(istr:iend,jstr:jend),landmask(istr:iend,jstr:jend),
     &    ws_used, istr,iend,jstr,jend)

          XKW = a * WS_USED*WS_USED
          WS_HIST(ISTR:IEND,JSTR:JEND) = WS_USED
!        write(stdout,*)"wshist",ws_hist(90:100,90:100)
       !---------------------------------------------------------------------
       !   convert PRESS from Pascals to atm,no, it is atm already
       !----------------------------------------------------------------------
          AP_USED = PRESS 

       !----------------------------------------------------------------------
       !   Set bad AP values to 1. This is necessary for runs restarting off
       !   a run in which the flux coupler didn't restart on AP correctly.
       !----------------------------------------------------------------------
          WHERE (AP_USED > 1.5 .OR. AP_USED < 0.5)
             AP_USED = c1
          END WHERE

       XKW_HIST(ISTR:IEND,JSTR:JEND) = XKW
       AP_HIST(ISTR:IEND,JSTR:JEND) = AP_USED

    !------------------------------------------------------------------------
    !   Compute XKW_ICE. XKW is zero over land, so XKW_ICE is too.
    !-------------------------------------------------------------------------

       IF (lflux_gas_o2 .OR. lflux_gas_co2) THEN
         XKW_ICE = XKW
         WHERE (FICE_USED > 0.2 .AND. FICE_USED < 0.9999)
            XKW_ICE = (c1 - FICE_USED) * XKW_ICE
         END WHERE
         WHERE (FICE_USED >= 0.9999)
           XKW_ICE = c0
         END WHERE
       END IF

    !------------------------------------------------------------------------
    !   compute O2 flux
    !------------------------------------------------------------------------

       IF (lflux_gas_o2) THEN
         call CSCHMIDT_O2(SSTT(istr:iend,jstr:jend),
     &     landmask(istr:iend,jstr:jend),SCHMIDT_USED,
     &     istr,iend,jstr,jend)
         SCHMIDT_O2_HIST(istr:iend,jstr:jend) = SCHMIDT_USED
         call O2SATU(SSTT(istr:iend,jstr:jend),
     &     SSSS(istr:iend,jstr:jend),landmask(istr:iend,jstr:jend),
     &     O2SAT_1atm,istr, iend,jstr,jend)
         WHERE (LANDMASK(istr:iend,jstr:jend))
            PV = XKW_ICE * SQRT(660.0 / SCHMIDT_USED)
            O2SAT_USED = AP_USED * O2SAT_1atm
            O2SAT_HIST(ISTR:IEND,JSTR:JEND) = O2SAT_USED
            FLUX = PV * (O2SAT_USED - 
     &            tracer(istr:iend,jstr:jend,N,o2_ind,ctime))
            STF(istr:iend,jstr:jend,o2_ind) = FLUX
         tracer(istr:iend,jstr:jend,N,o2_ind,ctime)=
     &         tracer(istr:iend,jstr:jend,N,o2_ind,ctime)+
     &         stf(istr:iend,jstr:jend,o2_ind)
     &         *dt/Hz(istr:iend,jstr:jend,N)
            FG_O2_HIST(ISTR:IEND,JSTR:JEND) = FLUX
        ELSEWHERE
            O2SAT_HIST(ISTR:IEND,JSTR:JEND) = c0
           FG_O2_HIST(ISTR:IEND,JSTR:JEND) = c0
         END WHERE

       END IF

    !------------------------------------------------------------------------
    !   compute CO2 flux, computing disequilibrium one row at a time
    !------------------------------------------------------------------------
        IF (lflux_gas_co2) THEN
           call CSCHMIDT_CO2(SSTT(istr:iend,jstr:jend),
     &       landmask(istr:iend,jstr:jend),SCHMIDT_USED,
     &       istr,iend,jstr,jend)
           SCHMIDT_CO2_HIST(ISTR:IEND,JSTR:JEND) = SCHMIDT_USED

           WHERE (LANDMASK(istr:iend,jstr:jend))
              PV = XKW_ICE * SQRT(660.0 / SCHMIDT_USED)
           ELSEWHERE
             PV = c0
           END WHERE

! Set a default value for pCO2 in the air. Will be overwritten if
! VARIABLE_ANN_ATM_PCO2 and/or VARIABLE_ATM_PCO2 is defined:
           XCO2 = 370.0           
! OCMIP-2 mandated value (278 ppm, dry-air, 1 atm)
!HF           XCO2 = 278.0
# ifdef VARIABLE_ANN_ATM_PCO2
           ! Compute number of simulated days since the start of the simulation:
           tdays_pco2 = time*sec2day
           ! Compute where in which year we are:
           curr_year = start_year + get_frac_year(tdays_pco2)
           ! Determine atm pco2:
           call get_atmco2(curr_year,futr_scen,atm_co2)
           XCO2 = atm_co2           
# endif
# ifdef VARIABLE_ATM_PCO2
      ! the factor deg2rad converts 360d into one full cycle of 2 pi
      ! the subtraction of 30d ensures that the maximum is reached
      ! at the end of April and the minimum at the end of October
           XCO2 = XCO2 + 2.9 * sin(deg2rad*(tdays-30.))
# endif /* VARIABLE_ATM_PCO2 */


#  ifdef CH_CARBON_DEPTH
!           g    - grav. const. [m/s²]
!           p0   - avg sea level pressure [Pa]
!           z_r  - depth [m]
!           rho0 - in situ density [kg/m³]
           press_ch = (g*rho0*(-z_r))+p0  !hydrostatic pressure in [Pa]  
        DO k = 1,N 
         DO j = jstr,jend
          WHERE (PH_HIST(istr:iend,j,k) .ne. c0) 
             PHLO = PH_HIST(istr:iend,j,k) - del_ph 
             PHHI = PH_HIST(istr:iend,j,k) + del_ph  
          ELSEWHERE
             PHLO = phlo_init
             PHHI = phhi_init
          END WHERE

          CALL co2calc_row(LANDMASK(istr,j),
     &        temp(istr,j,k), sal_ch(istr,j,k),
     &        press_ch(istr,j,k),
     &         tracer(istr,j,k,dic_ind,ctime),
     &         tracer(istr,j,k,alk_ind,ctime),
     &         tracer(istr,j,k,po4_ind,ctime),
     &         tracer(istr,j,k,sio3_ind,ctime),
     &          PHLO, PHHI, PH_NEW, XCO2,
     &          AP_USED(istr,j), CO2STAR_ROW,
     &          DCO2STAR_ROW, pCO2SURF_ROW,
     &           DpCO2_ROW, HCO3_ROW, CO3_ROW, istr,iend)

           pH_hist(istr:iend,j,k) = PH_NEW 
           CO2STAR_HIST(istr:iend,j,k) = CO2STAR_ROW 
           DCO2STAR_HIST(istr:iend,j,N) = DCO2STAR_ROW !is used to calculate air-sea flux
           HCO3_HIST(istr:iend,j,k) = HCO3_ROW
           CO3_HIST(istr:iend,j,k) = CO3_ROW 
           pCO2sw(istr:iend,j,N) = pCO2SURF_ROW !it is only at surface!!!
           pCO2air(istr:iend,j)  = pCO2SURF_ROW - DpCO2_ROW

           FLUX(istr:iend,j) = PV(istr:iend,j) * DCO2STAR_ROW
         END DO
        END DO
 
#  else

         DO j = jstr,jend
          WHERE (PH_HIST(istr:iend,j) .ne. c0)
             PHLO = PH_HIST(istr:iend,j) - del_ph
             PHHI = PH_HIST(istr:iend,j) + del_ph
          ELSEWHERE
             PHLO = phlo_init
             PHHI = phhi_init
          END WHERE

          CALL co2calc_row(LANDMASK(istr,j), 
     &        SSTT(istr,j), SSSS(istr,j), 
     &         tracer(istr,j,N,dic_ind,ctime),
     &         tracer(istr,j,N,alk_ind,ctime),
     &         tracer(istr,j,N,po4_ind,ctime),
     &         tracer(istr,j,N,sio3_ind,ctime),
     &          PHLO, PHHI, PH_NEW, XCO2, 
     &          AP_USED(istr,j), CO2STAR_ROW, 
     &          DCO2STAR_ROW, pCO2SURF_ROW, 
     &           DpCO2_ROW,istr,iend)

           pH_hist(istr:iend,j) = PH_NEW

           CO2STAR_HIST(istr:iend,j)  = CO2STAR_ROW
           DCO2STAR_HIST(istr:iend,j) = DCO2STAR_ROW
           pCO2sw(istr:iend,j) = pCO2SURF_ROW
           pCO2air(istr:iend,j)  = pCO2SURF_ROW - DpCO2_ROW

           FLUX(istr:iend,j) = PV(istr:iend,j) * DCO2STAR_ROW
         END DO

#  endif /* CH_CARBON_DEPTH */

         STF(istr:iend,jstr:jend,dic_ind) = 
     &              STF(istr:iend,jstr:jend,dic_ind) + FLUX

         tracer(istr:iend,jstr:jend,N,dic_ind,ctime)=
     &       tracer(istr:iend,jstr:jend,N,dic_ind,ctime)+
     &       stf(istr:iend,jstr:jend,dic_ind)
     &       *dt/Hz(istr:iend,jstr:jend,N)
         FG_CO2_HIST(istr:iend,jstr:jend) = 
     &                   FLUX(istr:iend,jstr:jend)
         

         endif

       !---------------------------------------------------------------------
       !   set surface iron flux from instant release from dust
       !----------------------------------------------------------------------
             if (liron_flux) then
!           write(stdout,*) "iron",iron(90:100,90:100)
               FLUX(istr:iend,jstr:jend)=iron(istr:iend,jstr:jend)
             else
                FLUX = c0
             endif 
           FLUX = FLUX * parm_Fe_bioavail
          STF(istr:iend,jstr:jend,fe_ind) = FLUX
         tracer(istr:iend,jstr:jend,N,fe_ind,ctime)=
     &         tracer(istr:iend,jstr:jend,N,fe_ind,ctime)+
     &         stf(istr:iend,jstr:jend,fe_ind)
     &         *dt/Hz(istr:iend,jstr:jend,N)

          IRON_FLUX_HIST(ISTR:IEND,JSTR:JEND) = FLUX
!end of set_sflux part

!start of set_interior part - loops need to be in this order 
! because PAR etc. is initialized for k==n
        do j = jstr, jend
           set_interior: do k=N,1,-1
           call ecosys_set_interior(k,temp(istr:iend,j,k),
     &          SHF_QSW(istr:iend,j),  
     &          PAR_out(istr:iend,j),
     &          qa_dust_def(istr:iend,j),istr,iend,j
     &          ,dt,ctime,dust(istr:iend,j))
           end do set_interior
        end do
        
!tracer value write back to roms
        do k=1,n
           do m=1,ntrc_bio
              t(istr:iend,jstr:jend,k,nnew,1+ntrc_salt+ntrc_pas+m)=
     &             tracer(istr:iend,jstr:jend,k,m,ctime)

c           IF (j == 50) THEN
c              IF (k == 30) THEN
c        print *,'j: ',j,' k: ',k,' m: ',m,' nnew: ',nnew,' iic: ',iic
c        print *,'iic: ',iic,' PO4: ',t(200,50,30,1,ctime)
c        print *,'iic: ',iic,' NO3: ',tracer(200,50,30,2,ctime)  
c              ENDIF
c           ENDIF

           enddo 
        enddo

      return 
       end

!-------------------------------------------------------------------------
!
!     ************   SUBROUTINE ECOSYS_SET_INTERIOR   ********************
!
!-------------------------------------------------------------------------

        subroutine ecosys_set_interior(k,temp,SHF_QSW,  
     &  PAR_out,qa_dust_def,istr,iend,j,dt2,ctime,dust_flux)
!MODHF     &  PAR_out,qa_dust_def,istr,jstr,iend,jend,dt,ctime,dust_flux)
        implicit none
#include "param.h"  
!need for im_eco,jm_eco
#include "param_bec.h"
#include "ecosys_bec.h"
#include "ocean3d.h"
#include "scalars.h"

        integer ctime,istr,iend,j
        real dt2

!MODHF        real, DIMENSION(istr:iend,jstr:jend) :: temp,SHF_QSW,
        real, DIMENSION(istr:iend) :: temp,SHF_QSW,
     &    QA_dust_def ,PAR_out,dust_flux

!zhc: local variable for set_interior
       REAL, PARAMETER :: 
     &    epsC      = 1.00e-8,  ! small C concentration (mmol C/m^3)
     &    epsTinv   = 3.17e-8,  ! small inverse time scale (1/year) (1/sec)
     &    epsnondim = 1.00e-6    ! small non-dimensional number (non-dim)
!MODHF       REAL, DIMENSION(istr:iend,jstr:jend) :: 
       REAL, DIMENSION(istr:iend) :: 
     &    PO4_loc,         ! local copy of model PO4
     &    NO3_loc,         ! local copy of model NO3
     &    SiO3_loc,        ! local copy of model SiO3
     &    NH4_loc,         ! local copy of model NH4
     &    Fe_loc,          ! local copy of model Fe
     &    O2_loc,          ! local copy of model O2
     &    DOC_loc,         ! local copy of model DOC
     &    spC_loc,         ! local copy of model spC
     &    spChl_loc,       ! local copy of model spChl
     &    spCaCO3_loc,     ! local copy of model spCaCO3
     &    diatC_loc,       ! local copy of model diatC
     &    diatChl_loc,     ! local copy of model diatChl
     &    zooC_loc,        ! local copy of model zooC
     &    spFe_loc,        ! local copy of model spFe
     &    diatSi_loc,      ! local copy of model diatSi
     &    diatFe_loc,      ! local copy of model diatFe
     &    diazC_loc,       ! local copy of model diazC
     &    diazChl_loc,     ! local copy of model diazChl
     &    diazFe_loc,      ! local copy of model diazFe
     &    DON_loc,         ! local copy of model DON
     &    DOFe_loc,        ! local copy of model DOFe
     &    DOP_loc          ! local copy of model DOP

       REAL :: 
     &    z_grz_sqr       ! square of parm_z_grz (mmol C/m^3)^2
       
!MODHF        REAL, DIMENSION(istr:iend,jstr:jend) :: 
        REAL, DIMENSION(istr:iend) :: 
     &    PAR_in,          ! photosynthetically available radiation (W/m^2)
     &    KPARdz,          ! PAR adsorption coefficient (non-dim)
     &    PAR_lay,         ! average PAR over layer depth (W/m^2)
     &    DOC_prod,        ! production of DOC (mmol C/m^3/sec)
     &    DOC_remin,       ! remineralization of DOC (mmol C/m^3/sec)
     &    NITRIF,          ! nitrification (NH4 -> NO3) (mmol N/m^3/sec)
     &    RESTORE          ! restoring terms for nutrients (mmol ./m^3/sec)
#ifdef OXYLIM_BEC
        REAL, DIMENSION(istr:iend) :: 
     &    DENITR_DOC,      ! denitrification of DOC (remin C, consume NO3)
     &    DENITR_POC       ! denitrification of POC (remin C, consume NO3)
#endif
        REAL, DIMENSION(istr:iend) :: 
     &    z_umax,          ! max. zoo growth rate on sp at local T (1/sec)
     &    diat_umax,       ! max. zoo growth rate on diatoms at local T (1/sec)
     &    z_mort,          ! zoo respiration loss, (1/sec/((mmol C/m3))
     &    C_loss_diaz,     ! bio-C threshold at which losses go to zero (mmol C/m^3)
     &    z_mort2,         ! zoo quad mort rate, tohigherlevels (1/sec/((mmol C/m3))
     &    diaz_umax        ! max. zoo growth rate on diazotrophs at local T (1/sec)

       REAL, DIMENSION(istr:iend) :: 
     &    thetaC_sp,       ! local Chl/C ratio in small phyto (mg Chl/mmol C)
     &    thetaC_diat,     ! local Chl/C ratio in diatoms (mg Chl/mmol C)
     &    QCaCO3,          ! small phyto CaCO3/C ratio (mmol CaCO3/mmol C)
     &    Tfunc,           ! temp response function GD98 (non-dim)
     &    VNO3_sp,         ! small phyto NO3 uptake rate (non-dim)
     &    VNH4_sp,         ! small phyto NH4 uptake rate (non-dim)
     &    VNtot_sp,        ! small phyto total N uptake rate (non-dim)
     &    VFeC_sp,         ! ??? small phyto C-specific iron uptake (non-dim)
     &    VPO4_sp,         ! ??? (non-dim)
     &    f_nut,           ! nut limitation factor, modifies C fixation (non-dim)
     &    PCmax,           ! max value of PCphoto at temperature TEMP (1/sec)
     &    PCphoto_sp,      ! small C-specific rate of photosynth. (1/sec)
     &    photoC_sp,       ! small phyto C-fixation (mmol C/m^3/sec)
     &    NO3_V_sp,        ! nitrate uptake by small phyto (mmol NO3/m^3/sec)
     &    NH4_V_sp,        ! ammonium uptake by small phyto (mmol NH4/m^3/sec)
     &    VNC_sp,          ! small phyto C-specific N uptake rate (mmol N/mmol C/sec)
     &    pChl,            ! Chl synth. regulation term (mg Chl/mmol N)
     &    photoacc_sp,     ! Chl synth. term in photoadapt. (GD98) (mg Chl/m^3/sec)
     &    CaCO3_prod,      ! prod. of CaCO3 by small phyto (mmol CaCO3/m^3/sec)
     &    VNO3_diat,       ! diatom nitrate uptake rate (non-dim)
     &    VNH4_diat,       ! diatom ammonium uptake rate (non-dim)
     &    VNtot_diat,      ! diatom total N uptake rate (non-dim)
     &    VFeC_diat,       ! diatom C-specific iron uptake (non-dim)
     &    VPO4_diat,       ! diatom C-specific PO4 uptake (non-dim)
     &    VSiO3_diat,      ! C-specific SiO3 uptake for diatoms (non-dim)
     &    PCphoto_diat,    ! diatom C-specific rate of photosynth. (1/sec)
     &    photoC_diat,     ! diatom C-fixation (mmol C/m^3/sec)
     &    NO3_V_diat,      ! nitrate uptake by diatoms (mmol NO3/m^3/sec)
     &    NH4_V_diat,      ! ammonium uptake by diatoms (mmol NH4/m^3/sec)
     &    VNC_diat,        ! diatom C-specific N uptake rate (mmol N/mmol C/sec)
     &    photoacc_diat,   ! Chl synth. term in photoadapt. (GD98) (mg Chl/m^3/sec)
     &    reduceV,         ! factor in nutrient uptake (mmol C/m^3)^2
     &    graze_sp,        ! grazing rate on small phyto (mmol C/m^3/sec)
     &    graze_sp_zoo,    ! graze_sp routed to zoo (mmol C/m^3/sec)
     &    graze_sp_poc,    ! graze_sp routed to poc (mmol C/m^3/sec)
     &    graze_sp_doc,    ! graze_sp routed to doc (mmol C/m^3/sec)
     &    graze_sp_dic      ! graze_sp routed to dic (mmol C/m^3/sec)

        REAL, DIMENSION(istr:iend) ::  ! max of 39 continuation lines
     &    graze_diat,      ! grazing rate on diatoms (mmol C/m^3/sec)
     &    graze_diat_zoo,  ! graze_diat routed to zoo (mmol C/m^3/sec)
     &    graze_diat_poc,  ! graze_diat routed to poc (mmol C/m^3/sec)
     &    graze_diat_doc,  ! graze_diat routed to doc (mmol C/m^3/sec)
     &    graze_diat_dic,  ! graze_diat routed to dic (mmol C/m^3/sec)
     &    Pprime,          ! used to limit phyto mort at low biomass (mmol C/m^3)
     &    sp_loss,         ! small phyto non-grazing mort (mmol C/m^3/sec)
     &    sp_loss_poc,     ! sp_loss routed to poc (mmol C/m^3/sec)
     &    sp_loss_doc,     ! sp_loss routed to doc (mmol C/m^3/sec)
     &    sp_loss_dic,     ! sp_loss routed to dic (mmol C/m^3/sec)
     &    sp_agg,          ! small phyto agg loss (mmol C/m^3/sec)
     &    diat_loss,       ! diatom non-grazing mort (mmol C/m^3/sec)
     &    diat_loss_poc,   ! diat_loss routed to poc (mmol C/m^3/sec)
     &    diat_loss_doc,   ! diat_loss routed to doc (mmol C/m^3/sec)
     &    diat_loss_dic,   ! diat_loss routed to dic (mmol C/m^3/sec)
     &    diat_agg,        ! diatom agg (mmol C/m^3/sec)
     &    f_zoo_detr,      ! frac of zoo losses into large detrital pool (non-dim)
     &    Fe_scavenge,     ! loss of dissolved iron, scavenging (mmol Fe/m^3/sec)
     &    Zprime,          ! used to limit zoo mort at low biomass (mmol C/m^3)
     &    zoo_loss,        ! mortality  higher trophic grazing on zooplankton (mmol C/m^3/sec)
     &    zoo_loss_doc,    ! zoo_loss routed to doc (mmol C/m^3/sec)
     &    zoo_loss_dic,    ! zoo_loss routed to dic (mmol C/m^3/sec)
     &    WORK,            ! intermediate value in photsyntheis computation (1/sec)
     &    light_lim,       ! light limitation factor
     &    Qsi,             ! Diatom initial Si/C ratio (mmol Si/mmol C)
     &    gQsi,            ! diatom Si/C ratio for growth (new biomass)
     &    Qfe_sp,          ! small phyto init fe/C ratio (mmolFe/mmolC)
     &    gQfe_sp,         ! small phyto fe/C for growth 
     &    Qfe_diat,        ! diatom init fe/C ratio
     &    gQfe_diat,       ! diatom fe/C ratio for growth
     &    Qfe_diaz,        ! diazotrophs init fe/C ratio
     &    gQfe_diaz        ! diazotroph fe/C ratio for new growth
         
!MODHF       REAL, DIMENSION(istr:iend,jstr:jend) ::  ! max of 39 continuation lines
       REAL, DIMENSION(istr:iend) ::  ! max of 39 continuation lines
     &    PCphoto_diaz,    ! diazotroph C-specific rate of photosynth. (1/sec)
     &    photoC_diaz,     ! diazotroph C-fixation (mmol C/m^3/sec)
     &    Vfec_diaz,       ! diazotroph C-specific iron uptake (non-dim)
     &    Vpo4_diaz,       ! diazotroph C-specific po4 uptake (non-dim)
     &    photoacc_diaz,   ! Chl synth. term in photoadapt. (GD98) (mg Chl/m^3/sec)
     &    Vnc_diaz,        ! diazotroph C-specific N uptake rate (mmol N/mmol C/sec)
     &    diaz_Nexcrete,   ! diazotroph fixed N excretion
     &    diaz_Nfix,       ! diazotroph total Nitrogen fixation (mmol N/m^3/sec)
     &    thetaC_diaz,     ! local Chl/C ratio in diazotrophs (mg Chl/mmol C)
     &    photoFe_diaz,    ! iron uptake by diazotrophs (mmolFe/m^3/sec)
     &    photoFe_diat,    ! iron uptake by diatoms
     &    photoFe_sp,      ! iron uptake by small phyto
     &    photoN_diaz,     ! nitrogen for Nfixation added to diaz biomass
     &    photoSi_diat,    ! silicon uptake by diatoms (mmol Si/m^3/sec)
     &    remaining_diazP, ! used in routing P from diazotroph losses
     &    diaz_loss,       ! diazotroph non-grazing mort (mmol C/m^3/sec)
     &    diaz_loss_doc,   ! mortality routed to DOM pool
     &    diaz_loss_dic,   ! mortality routed to remin 
     &    diaz_loss_dop,   ! P from mort routed to DOP pool
     &    diaz_loss_dip,   ! P from mort routed to remin 
                           ! P from diaz losses must be routed differently than
                           ! other elements to ensure that sinking detritus and
                           ! zooplankton pools get their fixed P/C ratios, the
                           ! remaining P is split evenly between DOP and PO4
     &   graze_diaz,       ! grazing rate on diazotrophs (mmol C/m^3/sec)
     &   graze_diaz_poc,   ! grazing routed to sinking detr (mmol C/m^3/sec)
     &   graze_diaz_doc,   ! grazing routed to DOC (mmol C/m^3/sec)
     &   graze_diaz_dic,   ! grazing routed to remin (mmol C/m^3/sec)
     &   graze_diaz_zoo,   ! grazing routed to new zoo biomass
     &   DON_remin,        ! portion of DON remineralized
     &   DOFe_remin,       ! portion of DOFe remineralized
     &   DOP_remin,        ! portion of DOP remineralized
     &   DOM_remin,        ! fraction of DOM remineralized at current TEMP
     &   Fe_scavenge_rate,  ! annual scavenging rate of iron as % of ambient
     &   fe_max_scale1,
     &   fe_scavenge_thres2,
     &   CaCO3_temp_thres1,
     &   CaCO3_temp_thres2, ! hf: added on 11/13/06
     &   f_prod_sp_CaCO3,
     &   spc_poc_fac

!MODHF         REAL, DIMENSION(istr:iend,jstr:jend) ::  ! max of 39 continuation lines
         REAL, DIMENSION(istr:iend) ::  ! max of 39 continuation lines
     &   DON_prod,	   ! production of dissolved organic N
     &   DOFe_prod,        ! produciton of dissolved organic Fe
     &   DOP_prod,         ! production of dissolved organic P
!zhc zt is 3d
     &    C_loss_thres    ! bio-C threshold at which losses go to zero (mmol C/m^3)

         integer setinterior,curtime,k

          setinterior=1
         
    !---------------------------------------------------------------------------
    !   exit immediately if computations are not to be performed
    !---------------------------------------------------------------------------

         IF (.NOT. lsource_sink) RETURN

    !---------------------------------------------------------------------------
    !   create local copies of model tracers
    !   treat negative values as zero
    !   apply mask to local copies
    !---------------------------------------------------------------------------
         curtime=2
       PO4_loc  = MAX(c0, TRACER(istr:iend,j,k,po4_ind,ctime))
       NO3_loc  = MAX(c0, TRACER(istr:iend,j,k,no3_ind,ctime))
       SiO3_loc = MAX(c0, TRACER(istr:iend,j,k,sio3_ind,ctime))
       NH4_loc  = MAX(c0, TRACER(istr:iend,j,k,nh4_ind,ctime))
       Fe_loc   = MAX(c0, TRACER(istr:iend,j,k,fe_ind,ctime))
       O2_loc   = MAX(c0, TRACER(istr:iend,j,k,o2_ind,ctime))
       DOC_loc  = MAX(c0, TRACER(istr:iend,j,k,doc_ind,ctime))
       spC_loc  = MAX(c0, TRACER(istr:iend,j,k,spC_ind,ctime))
       spChl_loc= MAX(c0, TRACER(istr:iend,j,k,spChl_ind,ctime))
       spCaCO3_loc  = 
     &           MAX(c0,TRACER(istr:iend,j,k,spCaCO3_ind,ctime))
       diatC_loc= MAX(c0, TRACER(istr:iend,j,k,diatC_ind,ctime))
       diatChl_loc  = 
     &          MAX(c0, TRACER(istr:iend,j,k,diatChl_ind,ctime))
       zooC_loc  = MAX(c0, TRACER(istr:iend,j,k,zooC_ind,ctime))
       spFe_loc  = MAX(c0, TRACER(istr:iend,j,k,spFe_ind,ctime))
       diatSi_loc = 
     &          MAX(c0, TRACER(istr:iend,j,k,diatSi_ind,ctime))
       diatFe_loc  = 
     &         MAX(c0, TRACER(istr:iend,j,k,diatFe_ind,ctime))
       diazC_loc    = 
     &         MAX(c0, TRACER(istr:iend,j,k,diazC_ind,ctime))
       diazChl_loc  = 
     &         MAX(c0, TRACER(istr:iend,j,k,diazChl_ind,ctime))
       diazFe_loc   = 
     &       MAX(c0, TRACER(istr:iend,j,k,diazFe_ind,ctime))
       DON_loc  = MAX(c0, TRACER(istr:iend,j,k,don_ind,ctime))
       DOFe_loc = MAX(c0, TRACER(istr:iend,j,k,dofe_ind,ctime))
       DOP_loc  = MAX(c0, TRACER(istr:iend,j,k,dop_ind,ctime))

       WHERE (.NOT. LANDMASK(istr:iend,j) ) !MODHF.OR. 
!MODHF     &           k > KMT(istr:iend,j))
          PO4_loc      = c0
          NO3_loc      = c0
          SiO3_loc     = c0
          NH4_loc      = c0
          Fe_loc       = c0
          O2_loc       = c0
          DOC_loc      = c0
          spC_loc      = c0
          spChl_loc    = c0
          spCaCO3_loc  = c0
          diatC_loc    = c0
          diatChl_loc  = c0
          zooC_loc     = c0
          spFe_loc     = c0
          diatSi_loc   = c0
          diatFe_loc   = c0
          diazC_loc    = c0
          diazChl_loc  = c0
          diazFe_loc   = c0
          DON_loc      = c0   
          DOFe_loc     = c0   
          DOP_loc      = c0   
        END WHERE

    !---------------------------------------------------------------------------
    !   If any phyto box are zero, set others to zeros.
    !---------------------------------------------------------------------------

       WHERE (spC_loc == c0 .OR. spChl_loc == c0 .OR. spFe_loc == c0)
         spC_loc = c0
         spChl_loc = c0
         spCaCO3_loc = c0
         spFe_loc = c0
       END WHERE

       WHERE (diatC_loc == c0 .OR. diatChl_loc == c0 .OR. 
     &     diatFe_loc == c0 
     &    .OR. diatSi_loc == c0)
          diatC_loc = c0
          diatChl_loc = c0
          diatFe_loc = c0
          diatSi_loc = c0
        END WHERE
    
       WHERE (diazC_loc == c0 .OR. diazChl_loc == c0 .OR. 
     &   diazFe_loc == c0)
         diazC_loc = c0
         diazChl_loc = c0
         diazFe_loc = c0
        END WHERE

    !---------------------------------------------------------------------------
    !   set local variables, with incoming ratios
    !---------------------------------------------------------------------------

         thetaC_sp   = spChl_loc / (spC_loc + epsC)
         thetaC_diat = diatChl_loc / (diatC_loc + epsC)
         thetaC_diaz = diazChl_loc / (diazC_loc + epsC)
         Qsi         = diatSi_loc / (diatC_loc + epsC)
         Qfe_diat    = diatFe_loc / (diatC_loc + epsC)
         Qfe_sp      = spFe_loc / (spC_loc + epsC)
        Qfe_diaz     = diazFe_loc / (diazC_loc + epsC)
        WHERE (Qsi > 0.685) Qsi = 0.685

    !---------------------------------------------------------------------------
    !   DETERMINE NEW ELEMENTAL RATIOS FOR GROWTH (NEW BIOMASS)
    !---------------------------------------------------------------------------
    
        gQsi = gQsi_0
        gQfe_diat = 6.0e-6
        gQfe_sp	= 6.0e-6
! HF: changed from "48.0e-6" on Oct 3, 2005, following Keith Moore's changes
        gQfe_diaz = 42.0e-6

    !---------------------------------------------------------------------------
    ! Modify these initial ratios under low ambient iron conditions
    !---------------------------------------------------------------------------
    
      WHERE (Fe_loc .LT. c2 * parm_diat_kfe)
        gQfe_diat = MAX((gQfe_diat * Fe_loc /(c2 * parm_diat_kfe)), 
     &    2.5e-6)
       END WHERE

       WHERE ((Fe_loc .LT. c2 * parm_diat_kfe).AND. fe_loc .gt. c0
     &   .and. (SiO3_loc.GT. (c2 * parm_diat_kSiO3) ) )
! HF: modified on Nov 2, 2005, following Keith Moore's changes,
! subtracted number was 0.274
         gQsi = MIN(((gQsi*gQsi_coef*c2*parm_diat_kfe/Fe_loc)
     &	   - (gQsi_coef-c1)*gQsi_0), 0.685)
       END WHERE

       where (Fe_loc == c0)
          gQsi = gQsi_max
       endwhere

       WHERE (Fe_loc .LT. c2 * parm_sp_kfe)
         gQfe_sp = MAX((gQfe_sp*Fe_loc/(c2 * parm_sp_kfe)), 
     &	   2.5e-6)
        END WHERE
    
       WHERE (Fe_loc .LT. c2 * diaz_kFe)
	   gQfe_diaz = MAX((gQfe_diaz*Fe_loc/(c2 * diaz_kFe)), 
     &	   		14.0e-6)
         END WHERE
    

    !---------------------------------------------------------------------------
    ! Modify the initial si/C ratio under low ambient Si conditions
    !---------------------------------------------------------------------------

        WHERE (SiO3_loc .LT. (c2 * parm_diat_kSiO3))
         gQsi = MAX((gQsi*SiO3_loc/(c2 * parm_diat_kSiO3)), 
     & 			0.0685)
        END WHERE


    !---------------------------------------------------------------------------
    !   various k==N initializations
    !
    !   0.45   fraction of incoming SW -> PAR (non-dim)
    !---------------------------------------------------------------------------

        IF (k == N) THEN
           WHERE (LANDMASK(istr:iend,j))
              PAR_out = MAX(c0, 0.45 * SHF_QSW)
           ELSE WHERE
              PAR_out = c0
           END WHERE
           PARinc(istr:iend,j) = PAR_out(istr:iend)
           CALL init_particulate_terms(QA_dust_def,istr,iend,j,
     &          dust_flux)
        END IF
    !---------------------------------------------------------------------------
    !   QCaCO3 is the percentage of sp organic matter which is associated
    !   with coccolithophores
    !---------------------------------------------------------------------------

         QCaCO3 = spCaCO3_loc / (spC_loc + epsC)
        WHERE (QCaCO3 > 0.4) QCaCO3 = 0.4

    !---------------------------------------------------------------------------
    !   compute PAR related quantities
    !
    !   0.03   atten. coeff. per unit chlorophyll (1/m/(mg Chl/m^3))
    !   0.04   atten. coeff. for water (1/m)
!HF    !   0.03e-2   atten. coeff. per unit chlorophyll (1/cm/(mg Chl/m^3))
!HF    !   0.04e-2   atten. coeff. for water (1/cm)
    !---------------------------------------------------------------------------

         PAR_in = PAR_out

         WHERE (.NOT. LANDMASK(ISTR:IEND,J)) !MODHF .OR. 
     &        PAR_in = c0

        KPARdz = (0.03 * (spChl_loc + diatChl_loc + 
     &     	diazChl_loc) + 0.04) * Hz(istr:iend,j,k)
         PAR_out = PAR_in * EXP(-KPARdz)
         PAR_lay = PAR_in * (c1 - EXP(-KPARdz)) / KPARdz

    !---------------------------------------------------------------------------
    !   Tref = 30.0 reference temperature (deg. C)
    !
    !   Using q10 formulation with Q10 value of 2.0 (Doney et al., 1996).
    !---------------------------------------------------------------------------

!         Tfunc = 2.0**(((temp + T0_Kelvin)-(30.0 + T0_Kelvin)) 
!     &    		/ 10.0)
         Tfunc = Q10_growth**(0.1 * temp - 3.0)
    !---------------------------------------------------------------------------
    !   modify growth mort rates by Tfunc
    !---------------------------------------------------------------------------

        z_umax = parm_z_umax_0 * Tfunc
        diat_umax = parm_diat_umax_0 * Tfunc
        z_mort2 = parm_z_mort2_0 * Tfunc
        z_mort = parm_z_mort_0 * Tfunc
        diaz_umax = parm_diaz_umax_0 * Tfunc
    
         DOM_remin= parm_sd_remin_0

    !---------------------------------------------------------------------------
    !   Get relative nutrient uptake rates for phytoplankton,
    !   min. relative uptake rate modifies C fixation in the manner
    !   that the min. cell quota does in GD98.
    !---------------------------------------------------------------------------
        

        VNO3_sp = (NO3_loc / parm_sp_kNO3) / 
     &    (c1 + (NO3_loc / parm_sp_kNO3) + (NH4_loc / parm_sp_kNH4))

        VNH4_sp = (NH4_loc / parm_sp_kNH4) / 
     &    (c1 + (NO3_loc / parm_sp_kNO3) + (NH4_loc / parm_sp_kNH4))

        VNtot_sp = VNO3_sp + VNH4_sp
        sp_N_lim_HIST(ISTR:IEND,J,K) = VNtot_sp


c        WHERE (VNtot_sp > 0)
c        print *,'iic: ',iic,' j: ',j,' k: ',k
c        print *,'VNtot_sp(200,j,k): ',VNtot_sp(200,j,k)
c        END WHERE

    !---------------------------------------------------------------------------
    !   get relative Fe uptake by phytoplankton
    !   get relative P uptake rates for phytoplankton
    !---------------------------------------------------------------------------

           VFeC_sp = Fe_loc / (Fe_loc + parm_sp_kFe)
           sp_Fe_lim_HIST(ISTR:IEND,J,K) = VFeC_sp
           VPO4_sp = PO4_loc / (PO4_loc + parm_sp_kPO4)
           sp_PO4_lim_HIST(ISTR:IEND,J,K) = VPO4_sp

    !---------------------------------------------------------------------------
    !   Small Phytoplankton C-fixation - given light and relative uptake rates
    !   determine small phyto nutrient limitation factor for carbon fixation
    !---------------------------------------------------------------------------

           f_nut = MIN(VNtot_sp, VFeC_sp)
           f_nut = MIN(f_nut, VPO4_sp)

    !---------------------------------------------------------------------------
    !   get small phyto photosynth. rate, phyto C biomass change, photoadapt
    !---------------------------------------------------------------------------

           PCmax = PCref * f_nut * Tfunc

        light_lim = (c1 - EXP((-c1 * parm_alphaChl * thetaC_sp * 
     &   PAR_lay) / (PCmax + epsTinv)))
        PCphoto_sp = PCmax * light_lim
       
        sp_light_lim_HIST(ISTR:IEND,J,K) = light_lim

        photoC_sp = PCphoto_sp * spC_loc

    !---------------------------------------------------------------------------
    !   Get nutrient uptakes by small phyto based on calculated C fixation
    !   total N uptake (VNC_sp) is used in photoadaption
    !---------------------------------------------------------------------------

       WHERE (VNtot_sp > c0)
          NO3_V_sp = (VNO3_sp / VNtot_sp) * photoC_sp * Q
          NH4_V_sp = (VNH4_sp / VNtot_sp) * photoC_sp * Q
          VNC_sp = PCphoto_sp * Q
       ELSEWHERE
          NO3_V_sp = c0
          NH4_V_sp = c0
          VNC_sp = c0
       END WHERE
    
        photoFe_sp = photoC_sp * gQfe_sp
        photoFe_sp_HIST(ISTR:IEND,J,K) = photoFe_sp
    

    !---------------------------------------------------------------------------
    !   calculate pChl, (used in photoadapt., GD98)
    !   2.3   max value of thetaN (Chl/N ratio) (mg Chl/mmol N)
    !   GD 98 Chl. synth. term
    !---------------------------------------------------------------------------

         WORK = parm_alphaChl * thetaC_sp * PAR_lay
       WHERE (WORK > c0)
        pChl = 2.3 * PCphoto_sp / WORK
        photoacc_sp = (pChl * VNC_sp / thetaC_sp) * spChl_loc
       ELSEWHERE
        photoacc_sp = c0
       END WHERE

    !---------------------------------------------------------------------------
    !   CaCO3 Production, parameterized as function of small phyto production
    !   decrease CaCO3 as function of nutrient limitation
    !   decrease CaCO3 prod at low temperatures
    !   increase CaCO3 prod under bloom conditions
    !   maximum calcification rate is 40% of primary production
    !---------------------------------------------------------------------------

! HF: this value used to be hard-coded (0.03), introduced the variable
! with a different value on Oct 3, 2005, following Keith Moore's changes
        f_prod_sp_CaCO3 = 0.026
        CaCO3_prod = f_prod_sp_CaCO3 * photoC_sp
        CaCO3_prod = CaCO3_prod * f_nut

! HF: this value used to be hard-coded (5.0), introduced the variable
! with a different value on Oct 3, 2005, following Keith Moore's changes
        CaCO3_temp_thres1 = 1.0
! HF: this value used to be hard-coded (-2.0), but the value in the 
! denominator was not adapted after introducing CaCO3_temp_thres1
! with a modified value
! added on 11/13/2006, based on Jeff Dusenberry's email
        CaCO3_temp_thres2 = -2.0
       WHERE (temp < CaCO3_temp_thres1) 
     &      CaCO3_prod = CaCO3_prod * 
     &       MAX((temp - CaCO3_temp_thres2), c0) / 
     &       (CaCO3_temp_thres1 - CaCO3_temp_thres2)
!! HF: BUG     &      CaCO3_prod = CaCO3_prod * MAX((temp+2.0), c0) / 7.0

        WHERE (spC_loc > 3.0) 
     &      CaCO3_prod = MIN((CaCO3_prod*spC_loc/3.0),(0.4*photoC_sp)) 
    
        CaCO3_form_HIST(ISTR:IEND,J,K) = CaCO3_prod

    !---------------------------------------------------------------------------
    !   Relative uptake rates for diatoms nitrate is VNO3, ammonium is VNH4
    !---------------------------------------------------------------------------

        VNO3_diat = (NO3_loc / parm_diat_kNO3) / 
     &    (c1 + (NO3_loc / parm_diat_kNO3) + (NH4_loc / parm_diat_kNH4))

        VNH4_diat = (NH4_loc / parm_diat_kNH4) / 
     &    (c1 + (NO3_loc / parm_diat_kNO3) + (NH4_loc / parm_diat_kNH4))

        VNtot_diat = VNO3_diat + VNH4_diat
        diat_N_lim_HIST(ISTR:IEND,J,K) = VNtot_diat


    !---------------------------------------------------------------------------
    !   get relative Fe uptake by diatoms
    !   get relative P uptake rates for diatoms
    !   get relative SiO3 uptake rate for diatoms
    !---------------------------------------------------------------------------

          VFeC_diat = Fe_loc / (Fe_loc + parm_diat_kFe)
          diat_Fe_lim_HIST(ISTR:IEND,J,K) = VFeC_diat
          VPO4_diat = PO4_loc / (PO4_loc + parm_diat_kPO4)
          diat_PO4_lim_HIST(ISTR:IEND,J,K) = VPO4_diat
          VSiO3_diat = SiO3_loc / (SiO3_loc + parm_diat_kSiO3)
          diat_SiO3_lim_HIST(ISTR:IEND,J,K) = VSiO3_diat

    !---------------------------------------------------------------------------
    !   Diatom carbon fixation and photoadapt.
    !   determine diatom nutrient limitation factor for carbon fixation
    !---------------------------------------------------------------------------
          f_nut = MIN(VNtot_diat, VFeC_diat)
          f_nut = MIN(f_nut, VSiO3_diat)
          f_nut = MIN(f_nut, VPO4_diat)
    !---------------------------------------------------------------------------
    !   get diatom photosynth. rate, phyto C biomass change, photoadapt
    !---------------------------------------------------------------------------

         PCmax = PCref * f_nut * Tfunc

        light_lim = (c1 - EXP((-c1 * parm_alphaChl * thetaC_diat 
     &    * PAR_lay) / 
     &              (PCmax + epsTinv)))

         PCphoto_diat = PCmax * light_lim
         diat_light_lim_HIST(ISTR:IEND,J,K) = light_lim
         photoC_diat = PCphoto_diat * diatC_loc

    !---------------------------------------------------------------------------
    !   Get nutrient uptake by diatoms based on C fixation
    !---------------------------------------------------------------------------

        WHERE (VNtot_diat > c0)
            NO3_V_diat = (VNO3_diat / VNtot_diat) * photoC_diat * Q
            NH4_V_diat = (VNH4_diat / VNtot_diat) * photoC_diat * Q
            VNC_diat = PCphoto_diat * Q
        ELSEWHERE
           NO3_V_diat = c0
           NH4_V_diat = c0
           VNC_diat = c0
         END WHERE
    
         photoFe_diat = photoC_diat * gQfe_diat
         photoSi_diat = photoC_diat * gQsi
    
          photoFe_diat_HIST(ISTR:IEND,J,K) = photoFe_diat
          bSi_form_HIST(ISTR:IEND,J,K) = photoSi_diat

    !---------------------------------------------------------------------------
    !   calculate pChl, (used in photoadapt., GD98)
    !   3.0   max value of thetaN (Chl/N ratio) (mg Chl/mmol N)
    !   GD 98 Chl. synth. term
    !---------------------------------------------------------------------------

         WORK = parm_alphaChl * thetaC_diat * PAR_lay
       WHERE (WORK > c0)
           pChl = 3.3 * PCphoto_diat / WORK
          photoacc_diat = (pChl * VNC_diat / thetaC_diat) * diatChl_loc
        ELSEWHERE
       photoacc_diat = c0
       END WHERE


    !---------------------------------------------------------------------------
    !   get relative Fe uptake by diazotrophs
    !   get relative P uptake rates for diazotrophs
    !---------------------------------------------------------------------------

        Vfec_diaz = Fe_loc/(Fe_loc + diaz_kFe)
        diaz_Fe_lim_HIST(ISTR:IEND,J,K) = Vfec_diaz

        Vpo4_diaz = PO4_loc / (PO4_loc + diaz_kPO4)
        diaz_P_lim_HIST(ISTR:IEND,J,K) = Vpo4_diaz

        f_nut = MIN(Vpo4_diaz, Vfec_diaz)

    !---------------------------------------------------------------------------
    !   get diazotroph photosynth. rate, phyto C biomass change
    !---------------------------------------------------------------------------

        PCmax = PCrefDiaz * f_nut * Tfunc

        light_lim = (c1 - EXP((-c1 * parm_alphaDiaz * thetaC_diaz 
     &    * PAR_lay) / 
     &              (PCmax + epsTinv)))
          PCphoto_diaz = PCmax * light_lim
          diaz_light_lim_HIST(ISTR:IEND,J,K) = light_lim

         photoC_diaz = PCphoto_diaz * diazC_loc

    !---------------------------------------------------------------------------
    !   Get N fixation by diazotrophs based on C fixation,
    !   Diazotrophs fix more than they need then 30% is excreted
    !---------------------------------------------------------------------------

         diaz_Nfix = photoC_diaz * Q * 1.43
         diaz_Nfix_HIST(ISTR:IEND,J,K) = diaz_Nfix
    
         diaz_Nexcrete = diaz_Nfix * 0.3
         photoN_diaz   = diaz_Nfix - diaz_Nexcrete
         Vnc_diaz = PCphoto_diaz * Q
    
    !---------------------------------------------------------------------------
    !   Get Fe and po4 uptake by diazotrophs based on C fixation
    !---------------------------------------------------------------------------

         photoFe_diaz = photoC_diaz * gQfe_diaz
         photoFe_diaz_HIST(ISTR:IEND,J,K) = photoFe_diaz
   
    !---------------------------------------------------------------------------
    !   calculate pChl, (used in photoadapt., GD98)
    !   3.3   max value of thetaN (Chl/N ratio) (mg Chl/mmol N)
    !   GD 98 Chl. synth. term
    !---------------------------------------------------------------------------
     
         WORK = parm_alphaDiaz * thetaC_diaz * PAR_lay
        WHERE (WORK > c0)
            pChl = 3.4 * PCphoto_diaz / WORK
          photoacc_diaz = (pChl * Vnc_diaz / thetaC_diaz) * diazChl_loc
        ELSEWHERE
           photoacc_diaz = c0
         END WHERE


    !---------------------------------------------------------------------------
    !   CALCULATE GRAZING AND OTHER MORT
    !---------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    !   0.001 small phytoplankton threshold C concentration (mmol C/m^3)
    !   get small phyto loss(in C units)
    !   small phyto agg loss
    !   get grazing rate (graze_sp) on small phyto  (in C units)
    !---------------------------------------------------------------------------

         C_loss_thres = 0.001

         WHERE (-z_r(istr:iend,j,k) > thres_z1) 
            WHERE (-z_r(istr:iend,j,k) < thres_z2) 
              C_loss_thres = C_loss_thres * 
     &          (thres_z2 + z_r(istr:iend,j,k)) / 
     &               (thres_z2 - thres_z1)
             ELSE WHERE
              C_loss_thres = c0
             END WHERE
          END WHERE

             Pprime = MAX(spC_loc - C_loss_thres, c0)

           sp_loss = sp_mort * Pprime

          sp_agg = MIN((0.2 * dps) * Pprime, 
     &       sp_mort2 * Pprime * Pprime)

         reduceV = Pprime * Pprime
         z_grz_sqr = parm_z_grz * parm_z_grz
          graze_sp = z_umax * zooC_loc * (reduceV / 
     &            (reduceV + z_grz_sqr))

    !---------------------------------------------------------------------------
    !   routing of graze_sp & sp_loss
    !   sp_agg all goes to POC
    ! currently assumes that 33% of grazed caco3 is remineralized
    ! if z_ingest ever changes, coefficients on routing grazed sp must change!
    ! min.%C routed to POC from grazing for ballast requirements = 0.4 * Qcaco3
    ! min.%C routed from sp_loss = 0.59 * QCaCO3, or P_CaCO3%rho
    ! 
    !---------------------------------------------------------------------------

        graze_sp_zoo = z_ingest * graze_sp
! HF: this value used to be hard-coded (0.24), introduced the variable
! with a different value on Oct 3, 2005, following Keith Moore's changes
        spc_poc_fac = 0.22
        graze_sp_poc = graze_sp * MAX((0.4 * QCaCO3), 
     &   	MIN((0.18 * Pprime),spc_poc_fac)) 
    
         graze_sp_doc = 0.34 * graze_sp - graze_sp_poc
         graze_sp_dic = 0.36 * graze_sp

          sp_loss_poc = QCaCO3 * sp_loss
        sp_loss_doc = (c1 - parm_labile_ratio) *
     &                 (sp_loss - sp_loss_poc)
         sp_loss_dic = parm_labile_ratio * (sp_loss - sp_loss_poc)

    !---------------------------------------------------------------------------
    !   0.01 small diatom threshold C concentration (mmol C/m^3)
    !   get diatom loss(in C units)
    !   Diatom agg loss, min. 1%/day
    !   get grazing rate (graze_diat) on diatoms  (in C units)
    !---------------------------------------------------------------------------

          C_loss_thres = 0.01

         WHERE (-z_r(istr:iend,j,k) > thres_z1) 
            WHERE (-z_r(istr:iend,j,k) < thres_z2) 
              C_loss_thres = C_loss_thres * 
     &            (thres_z2 + z_r(istr:iend,j,k)) / 
     &               (thres_z2 - thres_z1)
             ELSE WHERE
              C_loss_thres = c0
           END WHERE
           END WHERE
          Pprime = MAX(diatC_loc - C_loss_thres, c0)

             diat_loss = diat_mort * Pprime

            diat_agg = MIN((0.2 * dps) * Pprime, 
     &               diat_mort2 * Pprime * Pprime)
           diat_agg = MAX((0.01 * dps) * Pprime, diat_agg)

    !----------------------------------------------------------------------------
    ! Lower z_grz term for diatoms and diazotrophs, larger, more mobile predators
    !----------------------------------------------------------------------------
    
          reduceV = Pprime * Pprime
          graze_diat = diat_umax *zooC_loc * 
     &       (reduceV / (reduceV + z_grz_sqr * 0.81))

    !---------------------------------------------------------------------------
    !   routing of graze_diat & diat_loss
    !   diat_agg all goes to POC
    !   NOTE: if z_ingest is changed, coeff.s for poc,doc and dic must change!
    !---------------------------------------------------------------------------

          graze_diat_zoo = z_ingest * graze_diat
         graze_diat_poc = 0.26 * graze_diat
         graze_diat_doc = 0.13 * graze_diat
         graze_diat_dic = 0.31 * graze_diat

         diat_loss_poc = 0.05 * diat_loss
         diat_loss_doc = (c1 - parm_labile_ratio) * 0.95 * diat_loss
         diat_loss_dic = parm_labile_ratio * 0.95 * diat_loss

    !---------------------------------------------------------------------------
    !   0.03 small diazotroph threshold C concentration (mmol C/m^3)
    !   Lower value used at temperatures < 16 deg. C, negligible biomass 
    !   get diazotroph loss(in C units)
    !   get grazing rate (graze_diaz) on diazotrophs  (in C units)
    !   no aggregation loss for diazotrophs
    !---------------------------------------------------------------------------

            C_loss_diaz = 0.01

         WHERE (temp .LT. 15.0) 
     &           C_loss_diaz = 0.001

         WHERE (-z_r(istr:iend,j,k) > thres_z1) 
            WHERE (-z_r(istr:iend,j,k) < thres_z2) 
             C_loss_diaz = C_loss_diaz *
     &             (thres_z2 + z_r(istr:iend,j,k))/
     &             (thres_z2-thres_z1)
             ELSE WHERE
              C_loss_diaz = c0
           END WHERE
           END WHERE

         Pprime = MAX(diazC_loc - C_loss_diaz, c0)
         diaz_loss = diaz_mort * Pprime
    
         reduceV = Pprime * Pprime
         graze_diaz = diaz_umax * zooC_loc * 
     &        (reduceV / (reduceV + z_grz_sqr))
    
    !---------------------------------------------------------------------------
    !   routing of graze_diaz & diaz_loss
    !   NOTE: if z_ingest is changed, coeff.s for poc,doc and dic must change!
    !   z_ingest for diaz = 0.21 based on O'Neil (1998)
    !---------------------------------------------------------------------------

         graze_diaz_zoo = 0.21 * graze_diaz
         graze_diaz_poc = 0.0 * graze_diaz
         graze_diaz_doc = 0.24 * graze_diaz
         graze_diaz_dic = 0.55 * graze_diaz

        diaz_loss_doc = (c1 - parm_labile_ratio) * diaz_loss
        diaz_loss_dic = parm_labile_ratio * diaz_loss
    
    !--------------------------------------------------------------------------
    ! Note as diazotrophs have different Qp, we must route enough P into zoopl
    ! and sinking detritus pools to fill their fixed p/C ratios.  The left over
    ! P (remaining_diazP) is split between DOP and DIP pools
    !--------------------------------------------------------------------------

        remaining_diazP =  ((graze_diaz + diaz_loss) * Qp_diaz)- 
     &    	     ((graze_diaz_poc+graze_diaz_zoo) * Qp)
        diaz_loss_dop = (c1 - parm_labile_ratio) * remaining_diazP
        diaz_loss_dip = parm_labile_ratio * remaining_diazP

    !---------------------------------------------------------------------------
    !   get fractional factor for routing of zoo losses, based on food supply
    !   more material is routed to large detrital pool when diatoms eaten
    !---------------------------------------------------------------------------

        f_zoo_detr = (0.1333 * (graze_diat + epsC * epsTinv) + 
     &    0.0333 * (graze_sp + epsC * epsTinv) + 
     &    0.0 * (graze_diaz + epsC * epsTinv)) / 
     &    (graze_diat + graze_sp + graze_diaz + 3.0 * epsC * epsTinv)

    !---------------------------------------------------------------------------
    !   0.01 small zoo threshold C concentration (mmol C/m^3)
    !   zoo losses
    !---------------------------------------------------------------------------

         C_loss_thres = 0.03

         WHERE (-z_r(istr:iend,j,k) > thres_z1) 
            WHERE (-z_r(istr:iend,j,k) < thres_z2) 
              C_loss_thres = C_loss_thres *
     &            ((-z_r(istr:iend,j,k)-thres_z1)/
     &                 (thres_z2-thres_z1))
             ELSE WHERE
              C_loss_thres = c0
           END WHERE
           END WHERE

        Zprime = MAX(zooC_loc - C_loss_thres, c0)

        zoo_loss = z_mort2 * Zprime * Zprime + z_mort * Zprime

        zoo_loss_doc = (c1 - parm_labile_ratio) * (c1 - f_zoo_detr)
     &                     * zoo_loss
         zoo_loss_dic = parm_labile_ratio * (c1 - f_zoo_detr)
     &                       * zoo_loss

    !---------------------------------------------------------------------------
    !   compute terms for DOM
    !   remineralization is faster at warmer temperatures,
    !---------------------------------------------------------------------------
         DOC_prod = sp_loss_doc + graze_sp_doc + zoo_loss_doc 
     &       + diat_loss_doc 
     &	     + graze_diat_doc + diaz_loss_doc + graze_diaz_doc
    		
         DON_prod = (DOC_prod * Q) + diaz_Nexcrete
         DOP_prod = (sp_loss_doc + graze_sp_doc + zoo_loss_doc 
     &       + diat_loss_doc 
     &		+ graze_diat_doc) * Qp + diaz_loss_dop
         DOFe_prod = (zoo_loss_doc * Qfe_zoo) 
     &	+ (Qfe_sp * (graze_sp_doc + sp_loss_doc)) 
     &	+ (Qfe_diat * (graze_diat_doc + diat_loss_doc)) 
     &	+ (Qfe_diaz * (graze_diaz_doc + diaz_loss_doc))

        DOC_remin = DOC_loc * DOM_remin
        DON_remin = DON_loc * DOM_remin
        DOFe_remin = DOFe_loc * DOM_remin
        DOP_remin = DOP_loc * DOM_remin

    !---------------------------------------------------------------------------
    !   large detritus C
    !---------------------------------------------------------------------------

        sinking_particle_POC(3,istr:iend,j)  = 
     &      sp_agg + graze_sp_poc 
     &      + sp_loss_poc + f_zoo_detr * zoo_loss + 
     &   diat_loss_poc + diat_agg + graze_diat_poc + graze_diaz_poc
        

    !---------------------------------------------------------------------------
    !   large detrital CaCO3
    !   33% of CaCO3 is remin when phyto are grazed
    !---------------------------------------------------------------------------

         sinking_particle_P_CaCO3(3,istr:iend,j)= 
     &        (0.67 * graze_sp + sp_loss + sp_agg) * QCaCO3

    !---------------------------------------------------------------------------
    !   large detritus SiO2
    !   grazed diatom SiO2, 60% is remineralized
    !---------------------------------------------------------------------------

        sinking_particle_P_SiO2(3,istr:iend,j) = 
     &      (0.5 * graze_diat 
     &      + diat_agg + 0.05 * diat_loss) * Qsi

        sinking_particle_dust(3,istr:iend,j) = c0

    !---------------------------------------------------------------------------
    !   Compute iron scavenging :
    !   1) compute in terms of loss per year per unit iron (%/year/fe)
    !   2) scale by sinking POC/Dust flux & a scaled total POC concentration,
    !	   the POC scale factor to convert to ~sink units, 
    ! 	   this estimate ignores remineralization of POC,
    !      so is an overestimate, accounting for adsorption onto non-POC stuff
    !      assumes 1gDust > 1molPOC, accounts for non-C parts of biomass/volume
    !   3) set min. scavenging rate in upper water column
    !   4) increase scavenging at higher iron (>0.6e-3mmol/m3)
    !        loss per year per Fe equal to 4.0 at Fe=2e-3mmol/m3
    !	5) decrease scavenging rates at low iron (<0.35e-3mmol/m3)
    !   6) convert to net loss per second
    !---------------------------------------------------------------------------

         Fe_scavenge_rate = 0.12
! HF: this value used to be hard-coded (4.0), introduced the variable
! with a different value on Oct 3, 2005, following Keith Moore's changes
         fe_max_scale1 = 3.0

! HF: corrections made in 2004, notification by Jeff Dusenberry Jan 19, 2005
! 0.833e8 nmol C/g dust = 8.33e4 mmol C/kg dust
! 0.002 cm/s reduced by Keith Moore to 0.0002 cm/s, converts to 2e-6 m/s

         Fe_scavenge_rate = Fe_scavenge_rate
     &    *MIN(((sinking_particle_POC(4,istr:iend,j) + 
     &	   sinking_particle_POC(5,istr:iend,j)+
     &       ((sinking_particle_dust(4,istr:iend,j)
     &    + sinking_particle_dust(5,istr:iend,j)) * 8.33e4) )
     &     / parm_POC_flux_ref), fe_max_scale1)

! HF removed 2nd to last line of Fe_scavenge_rate equation on Oct 3, 2005 (JKM change):
! &	+ ((spC_loc+diatC_loc+zooC_loc+diazC_loc) * 2.e-6)) 

         WHERE (Fe_loc > 0.6e-3) 
     &      Fe_scavenge_rate = Fe_scavenge_rate + (Fe_loc - 0.6e-3) 
     &    	* (6.0 / (1.4e-3))   	

! HF: this value used to be hard-coded (0.4e-3), introduced the variable
! with a different value on Oct 3, 2005, following Keith Moore's changes
         fe_scavenge_thres2 = 0.5e-3
        WHERE (Fe_loc < fe_scavenge_thres2) 
     &        Fe_scavenge_rate = Fe_scavenge_rate * 
     &        (Fe_loc / fe_scavenge_thres2)
 
          Fe_scavenge = 3.1709792e-8 * Fe_loc * Fe_scavenge_rate

         sinking_particle_P_iron(3,istr:iend,j) = 
     &          ((sp_agg + graze_sp_poc
     &       + sp_loss_poc) * Qfe_sp) 
     &      + (zoo_loss * f_zoo_detr * Qfe_zoo) 
     &	  + ((diat_agg + graze_diat_poc + diat_loss_poc) * Qfe_diat) 
     &	+ (graze_diaz_poc * Qfe_diaz) + (0.1 * Fe_scavenge)

! HF: changed on Oct 3, 2005, pass temp instead of Tfunc, 
!      following Keith Moore's changes
        CALL compute_particulate_terms(k, QA_dust_def, temp,istr,
     &               iend,j)
    !---------------------------------------------------------------------------
    !   nitrate & ammonium
    !   nitrification in low light
    !   use exponential decay of PAR across model level to compute taper factor
    !---------------------------------------------------------------------------

         IF (lrest_no3) THEN
           RESTORE = (NO3_CLIM(istr:iend,j,k) - NO3_loc) 
     &          * nutr_rest_time_inv(k)
         ELSE
           RESTORE = c0
         END IF

         NO3_RESTORE_HIST(ISTR:IEND,J,K) = RESTORE
         WHERE (PAR_out < parm_nitrif_par_lim)
#ifdef OXYLIM_BEC
         WHERE (O2_loc > parm_o2_min)
#endif
            NITRIF = parm_kappa_nitrif * NH4_loc
#ifdef OXYLIM_BEC
         ELSEWHERE
            NITRIF = c0
         END WHERE
#endif
            WHERE (PAR_in > parm_nitrif_par_lim)
#ifdef OXYLIM_BEC
            WHERE (O2_loc > parm_o2_min)
#endif
               NITRIF = NITRIF * LOG(PAR_out / parm_nitrif_par_lim) 
     &              / (-KPARdz)
#ifdef OXYLIM_BEC
            ELSEWHERE
               NITRIF = c0
            END WHERE
#endif
            END WHERE
         ELSEWHERE
            NITRIF = c0
         END WHERE
        NITRIF_HIST(ISTR:IEND,J,K) = NITRIF

#ifdef OXYLIM_BEC
        WHERE (O2_loc < parm_o2_min)
           DENITR_DOC = 1
           DENITR_POC = 1
        ELSEWHERE
           DENITR_DOC = c0
           DENITR_POC = c0
        END WHERE

        DENITR_DOC_HIST(ISTR:IEND,J,K) = DENITR_DOC
        DENITR_POC_HIST(ISTR:IEND,J,K) = DENITR_POC
#endif

       TRACER(istr:iend,j,K,no3_ind,curtime) = RESTORE + NITRIF-
     &                  NO3_V_diat - NO3_V_sp

         TRACER(istr:iend,j,K,nh4_ind,curtime) =
     &    -NH4_V_diat(istr:iend)
     &   - NH4_V_sp(istr:iend)-NITRIF(istr:iend) +
     &    Q * (zoo_loss_dic(istr:iend) + 
     &    sp_loss_dic(istr:iend) +
     &     graze_sp_dic(istr:iend) + 
     &     diat_loss_dic(istr:iend) + 
     &    graze_diat_dic (istr:iend)+ 
     &    Sinking_Particle_Poc(6,istr:iend,j) +
     &    diaz_loss_dic(istr:iend) +
     &    graze_diaz_dic(istr:iend)) + 
     &    DON_remin(istr:iend)
         
    !---------------------------------------------------------------------------
    !   dissolved iron
    !   DROPPED Feflux term
    !   When fe/C of phytoplankton prey is > fe/C of zooplankton, excess is remin
    !---------------------------------------------------------------------------

       TRACER(istr:iend,j,K,fe_ind,curtime) = 
     &    sinking_particle_P_iron(6,istr:iend,j) 
     &    - Fe_scavenge + 
     &    (Qfe_zoo * zoo_loss_dic) + DOFe_remin - photoFe_diaz 
     &     + (Qfe_sp * (sp_loss_dic + graze_sp_dic)) 
     &     + (Qfe_diat * (diat_loss_dic + graze_diat_dic)) 
     &     + (Qfe_diaz * (diaz_loss_dic + graze_diaz_dic)) 
     &     - photoFe_sp - photoFe_diat 
     &	  + graze_diaz_zoo *(Qfe_diaz-Qfe_zoo) 
     &     + graze_diat_zoo *(Qfe_diat-Qfe_zoo) 
     &     + graze_sp_zoo * (Qfe_sp-Qfe_zoo)

    !---------------------------------------------------------------------------
    !   dissolved SiO3
    !---------------------------------------------------------------------------

        IF (lrest_sio3) THEN
          RESTORE = (SiO3_CLIM(istr:iend,j,k) - SiO3_loc) 
     &             * nutr_rest_time_inv(k)
        ELSE
           RESTORE = c0
        END IF

         SiO3_RESTORE_HIST(ISTR:IEND,J,K) = RESTORE

        TRACER(istr:iend,j,K,sio3_ind,curtime) = RESTORE 
     &     + sinking_particle_P_SiO2(6,istr:iend,j) + 
     &      Qsi * (0.5 * graze_diat + 0.95 * diat_loss) 
     &      - photoSi_diat

    !---------------------------------------------------------------------------
    !   phosphate
    !---------------------------------------------------------------------------

        IF (lrest_po4) THEN
            RESTORE = (PO4_CLIM(istr:iend,j,k) - PO4_loc) 
     &            * nutr_rest_time_inv(k)
        ELSE
           RESTORE = c0
        END IF

        PO4_RESTORE_HIST(ISTR:IEND,J,K) = RESTORE

        TRACER(istr:iend,j,K,po4_ind,curtime) = RESTORE + (Qp * 
     &    (Sinking_Particle_Poc(6,istr:iend,j) + 
     &   zoo_loss_dic + sp_loss_dic + graze_sp_dic + diat_loss_dic + 
     &    graze_diat_dic - photoC_sp - photoC_diat)) 
     &    + DOP_remin + diaz_loss_dip - (photoC_diaz * Qp_diaz)

    !---------------------------------------------------------------------------
    !   small phyto Carbon
    !---------------------------------------------------------------------------

        TRACER(istr:iend,j,K,spC_ind,curtime) = photoC_sp - 
     &       graze_sp - sp_loss - sp_agg

    !---------------------------------------------------------------------------
    !   small phyto Chlorophyll
    !---------------------------------------------------------------------------

        TRACER(istr:iend,j,K,spChl_ind,curtime) = photoacc_sp - 
     &       thetaC_sp * (graze_sp + sp_loss + sp_agg)

    !---------------------------------------------------------------------------
    !   small phytoplankton CaCO3
    !---------------------------------------------------------------------------

       TRACER(istr:iend,j,K,spCaCO3_ind,curtime) = CaCO3_prod - 
     &    (graze_sp + sp_loss + sp_agg) * QCaCO3

    !---------------------------------------------------------------------------
    !   diatom Carbon
    !---------------------------------------------------------------------------

       TRACER(istr:iend,j,K,diatC_ind,curtime) = 
     &   photoC_diat - graze_diat - 
     &    diat_loss - diat_agg

    !---------------------------------------------------------------------------
    !   diatom Chlorophyll
    !---------------------------------------------------------------------------

       TRACER(istr:iend,j,K,diatChl_ind,curtime) = 
     &    photoacc_diat - 
     &    thetaC_diat * (graze_diat + diat_loss + diat_agg)
             
    !---------------------------------------------------------------------------
    !   zoo Carbon
    !---------------------------------------------------------------------------

       TRACER(istr:iend,j,K,zooC_ind,curtime) = graze_sp_zoo 
     &  + graze_diat_zoo 
     &	+ graze_diaz_zoo - zoo_loss

    !---------------------------------------------------------------------------
    !   dissolved organic Matter
    !---------------------------------------------------------------------------

       TRACER(istr:iend,j,K,doc_ind,curtime) = 
     &                 DOC_prod - DOC_remin

       TRACER(istr:iend,j,K,don_ind,curtime) = 
     &                 DON_prod - DON_remin
    
       TRACER(istr:iend,j,K,dop_ind,curtime) = 
     &                 DOP_prod - DOP_remin

       TRACER(istr:iend,j,K,dofe_ind,curtime) = 
     &                 DOFe_prod - DOFe_remin

    !---------------------------------------------------------------------------
    !   small phyto Fe
    !---------------------------------------------------------------------------

       TRACER(istr:iend,j,K,spFe_ind,curtime) =  photoFe_sp 
     &	  - (Qfe_sp * (graze_sp+sp_loss+sp_agg))  
    	
    !---------------------------------------------------------------------------
    !   Diatom Fe
    !---------------------------------------------------------------------------

       TRACER(istr:iend,j,K,diatFe_ind,curtime) =  photoFe_diat 
     &	- (Qfe_diat * (graze_diat+diat_loss+diat_agg))
    	
    !---------------------------------------------------------------------------
    !   Diatom Si
    !---------------------------------------------------------------------------

       TRACER(istr:iend,j,K,diatSi_ind,curtime) =  photoSi_diat 
     & 	- (Qsi * (graze_diat+diat_loss+diat_agg))

    !---------------------------------------------------------------------------
    !   Diazotroph C
    !---------------------------------------------------------------------------

       TRACER(istr:iend,j,K,diazC_ind,curtime) =  photoC_diaz - 
     &      graze_diaz - diaz_loss
    	
    !---------------------------------------------------------------------------
    !   diazotroph Chlorophyll
    !---------------------------------------------------------------------------

       TRACER(istr:iend,j,K,diazChl_ind,curtime) = photoacc_diaz
     &    - thetaC_diaz * (graze_diaz + diaz_loss)

    !---------------------------------------------------------------------------
    !   Diazotroph Fe
    !---------------------------------------------------------------------------

        TRACER(istr:iend,j,K,diazFe_ind,curtime) =  photoFe_diaz
     &   	- (Qfe_diaz * (graze_diaz + diaz_loss))

 		
    !---------------------------------------------------------------------------
    !   dissolved inorganic Carbon
    !---------------------------------------------------------------------------

        TRACER(istr:iend,j,K,dic_ind,curtime) = DOC_remin 
     &    + Sinking_Particle_Poc(6,istr:iend,j)
     &     + sinking_particle_P_CaCO3(6,istr:iend,j) + 
     &    0.33 * graze_sp * QCaCO3 + zoo_loss_dic + sp_loss_dic + 
     &    graze_sp_dic + diat_loss_dic + graze_diat_dic - 
     &    photoC_sp - photoC_diat 
     &   - CaCO3_prod + graze_diaz_dic + diaz_loss_dic - photoC_diaz

    !---------------------------------------------------------------------------
    !   alkalinity
    !---------------------------------------------------------------------------

       TRACER(istr:iend,j,K,alk_ind,curtime) =
     &     -TRACER(istr:iend,j,K,no3_ind,curtime) + 
     &       TRACER(istr:iend,j,K,nh4_ind,curtime) + 
     &    c2 * (sinking_particle_P_CaCO3(6,istr:iend,j)+ 
     &    0.33 * graze_sp * QCaCO3 - CaCO3_prod)

    !---------------------------------------------------------------------------
    !   oxygen
    !---------------------------------------------------------------------------

        TRACER(istr:iend,j,K,o2_ind,curtime) = 
     &      (photoC_sp + photoC_diat + photoC_diaz) 
     &  	/ parm_Red_D_C_O2

       WHERE (O2_loc > parm_o2_min)
          TRACER(istr:iend,j,K,o2_ind,curtime) = 
     &      TRACER(istr:iend,j,K,o2_ind,curtime)
     &     +((- Sinking_Particle_Poc(6,istr:iend,j) - 
     &       DOC_remin - zoo_loss_dic - sp_loss_dic - graze_sp_dic- 
     &       diat_loss_dic - graze_diat_dic - graze_diaz_dic 
     &       - diaz_loss_dic) / parm_Red_P_C_O2)
     &       - parm_nitrif_o2 * nitrif
       END WHERE

    !---------------------------------------------------------------------------
    !   various tavg/history variables
    !---------------------------------------------------------------------------
        PAR(ISTR:IEND,J,K)                  = PAR_lay
        graze_sp_HIST(ISTR:IEND,J,K)        = graze_sp
        graze_diat_HIST(ISTR:IEND,J,K)      = graze_diat
        graze_diaz_HIST(ISTR:IEND,J,K)	 = graze_diaz
        graze_tot_HIST(ISTR:IEND,J,K)       = graze_sp 
     &                        + graze_diat + graze_diaz
        sp_loss_HIST(ISTR:IEND,J,K)         = sp_loss
        diat_loss_HIST(ISTR:IEND,J,K)       = diat_loss
        diaz_loss_HIST(ISTR:IEND,J,K)       = diaz_loss
        zoo_loss_HIST(ISTR:IEND,J,K)        = zoo_loss
        sp_agg_HIST(ISTR:IEND,J,K)          = sp_agg
        diat_agg_HIST(ISTR:IEND,J,K)        = diat_agg
        photoC_sp_HIST(ISTR:IEND,J,K)       = photoC_sp
        photoC_diat_HIST(ISTR:IEND,J,K)     = photoC_diat
        photoC_diaz_HIST(ISTR:IEND,J,K)     = photoC_diaz
        tot_prod_HIST(ISTR:IEND,J,K)        = photoC_sp +
     &                    photoC_diat + photoC_diaz
        DOC_prod_HIST(ISTR:IEND,J,K)        = DOC_prod
        DOC_remin_HIST(ISTR:IEND,J,K)       = DOC_remin
        DON_prod_HIST(ISTR:IEND,J,K)        = DON_prod
        DON_remin_HIST(ISTR:IEND,J,K)       = DON_remin
        DOP_prod_HIST(ISTR:IEND,J,K)        = DOP_prod
        DOP_remin_HIST(ISTR:IEND,J,K)       = DOP_remin
        DOFe_prod_HIST(ISTR:IEND,J,K)        = DOFe_prod
        DOFe_remin_HIST(ISTR:IEND,J,K)       = DOFe_remin
        Fe_scavenge_HIST(ISTR:IEND,J,K)     = Fe_scavenge
        Fe_scavenge_rate_HIST(ISTR:IEND,J,K) = Fe_scavenge_rate

        TRACER(istr:iend,j,k,:,curtime)=
     &      TRACER(istr:iend,j,k,:,curtime)*dt2
     &      +TRACER(istr:iend,j,k,:,ctime)
        tracer(istr:iend,j,k,:,ctime)=
     &        tracer(istr:iend,j,k,:,curtime)

        ! Modif Jds 
        ! We prescribe tracers values
        ! at the domain bottom in a quasi one D
        ! configuration in order to avoid terms. 
        ! We set these values as being the initial values 
#ifdef ONE_DIM

        if(k==1) then

            if(iic==ntstart) then

             initial_temp(istr:iend,j)=t(istr:iend,j,k,nnew,itemp)
             initial_salt(istr:iend,j)=t(istr:iend,j,k,nnew,isalt)

             ! As initial value we take the one of the initial values file
             INITIAL_TRACER(istr:iend,j,o2_ind)=TRACER(istr:iend,j,k,o2_ind,curtime)
             INITIAL_TRACER(istr:iend,j,po4_ind)=TRACER(istr:iend,j,k,po4_ind,curtime)
             INITIAL_TRACER(istr:iend,j,spC_ind)=TRACER(istr:iend,j,k,spC_ind,curtime)
             INITIAL_TRACER(istr:iend,j,spChl_ind)=TRACER(istr:iend,j,k,spChl_ind,curtime)
             INITIAL_TRACER(istr:iend,j,spCaCO3_ind)=TRACER(istr:iend,j,k,spCaCO3_ind,curtime)
             INITIAL_TRACER(istr:iend,j,diatC_ind)=TRACER(istr:iend,j,k,diatC_ind,curtime)
             INITIAL_TRACER(istr:iend,j,diatChl_ind)=TRACER(istr:iend,j,k,diatChl_ind,curtime)
             INITIAL_TRACER(istr:iend,j,zooC_ind)=TRACER(istr:iend,j,k,zooC_ind,curtime)
             INITIAL_TRACER(istr:iend,j,sio3_ind)=TRACER(istr:iend,j,k,sio3_ind,curtime)
             INITIAL_TRACER(istr:iend,j,fe_ind)=TRACER(istr:iend,j,k,fe_ind,curtime)
             INITIAL_TRACER(istr:iend,j,no3_ind)=TRACER(istr:iend,j,k,no3_ind,curtime)
             INITIAL_TRACER(istr:iend,j,spFe_ind)=TRACER(istr:iend,j,k,spFe_ind,curtime)
             INITIAL_TRACER(istr:iend,j,diatFe_ind)=TRACER(istr:iend,j,k,diatFe_ind,curtime)
             INITIAL_TRACER(istr:iend,j,diatSi_ind)=TRACER(istr:iend,j,k,diatSi_ind,curtime)
             INITIAL_TRACER(istr:iend,j,diazC_ind)=TRACER(istr:iend,j,k,diazC_ind,curtime)
             INITIAL_TRACER(istr:iend,j,diazChl_ind)=TRACER(istr:iend,j,k,diazChl_ind,curtime)
             INITIAL_TRACER(istr:iend,j,diazFe_ind)=TRACER(istr:iend,j,k,diazFe_ind,curtime)
             INITIAL_TRACER(istr:iend,j,dic_ind)=TRACER(istr:iend,j,k,dic_ind,curtime)
             INITIAL_TRACER(istr:iend,j,alk_ind)=TRACER(istr:iend,j,k,alk_ind,curtime)

            end if

            t(istr:iend,j,k,nnew,itemp)=initial_temp(istr:iend,j)
            t(istr:iend,j,k,nnew,isalt)=initial_salt(istr:iend,j)

            TRACER(istr:iend,j,k,o2_ind,ctime)=INITIAL_TRACER(istr:iend,j,o2_ind)

            TRACER(istr:iend,j,k,po4_ind,ctime)=INITIAL_TRACER(istr:iend,j,po4_ind)

            TRACER(istr:iend,j,k,spC_ind,ctime)=INITIAL_TRACER(istr:iend,j,spC_ind)

            TRACER(istr:iend,j,k,spChl_ind,ctime)=INITIAL_TRACER(istr:iend,j,spChl_ind)
 
            TRACER(istr:iend,j,k,spCaCO3_ind,ctime)=INITIAL_TRACER(istr:iend,j,spCaCO3_ind)

            TRACER(istr:iend,j,k,diatC_ind,ctime)=INITIAL_TRACER(istr:iend,j,diatC_ind)

            TRACER(istr:iend,j,k,diatChl_ind,ctime)=INITIAL_TRACER(istr:iend,j,diatChl_ind)

            TRACER(istr:iend,j,k,zooC_ind,ctime)=INITIAL_TRACER(istr:iend,j,zooC_ind)

            TRACER(istr:iend,j,k,sio3_ind,ctime)=INITIAL_TRACER(istr:iend,j,sio3_ind)

            TRACER(istr:iend,j,k,fe_ind,ctime)=INITIAL_TRACER(istr:iend,j,fe_ind)

            TRACER(istr:iend,j,k,no3_ind,ctime)=INITIAL_TRACER(istr:iend,j,no3_ind)

            TRACER(istr:iend,j,k,spFe_ind,ctime)=INITIAL_TRACER(istr:iend,j,spFe_ind)

            TRACER(istr:iend,j,k,diatFe_ind,ctime)=INITIAL_TRACER(istr:iend,j,diatFe_ind)

            TRACER(istr:iend,j,k,diatSi_ind,ctime)=INITIAL_TRACER(istr:iend,j,diatSi_ind)

            TRACER(istr:iend,j,k,diazC_ind,ctime)=INITIAL_TRACER(istr:iend,j,diazC_ind)

            TRACER(istr:iend,j,k,diazChl_ind,ctime)=INITIAL_TRACER(istr:iend,j,diazChl_ind)

            TRACER(istr:iend,j,k,diazFe_ind,ctime)=INITIAL_TRACER(istr:iend,j,diazFe_ind)

            TRACER(istr:iend,j,k,dic_ind,ctime)=INITIAL_TRACER(istr:iend,j,dic_ind)

            TRACER(istr:iend,j,k,alk_ind,ctime)=INITIAL_TRACER(istr:iend,j,alk_ind)

      end if
#endif
         
         end subroutine ecosys_set_interior

!-------------------------------------------------------------------------
!
!     ************   SUBROUTINE INIT_PARTICULATE_TERMS   ****************
!
!-------------------------------------------------------------------------

         SUBROUTINE init_particulate_terms(QA_dust_def,istr,
     &      iend,j,net_dust_in)
        implicit none
#include "param.h" 
#include "param_bec.h"
#include "ecosys_bec.h"

    !---------------------------------------------------------------------------
    !   Set incoming fluxes (put into outgoing flux for first level usage).
    !   Set dissolution length, production fraction and mass terms.
    !---------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    !   arguments
    !---------------------------------------------------------------------------
         integer istr,iend,j
!MODHF            REAL, DIMENSION(istr:iend,jstr:jend), INTENT(OUT) :: 
            REAL, DIMENSION(istr:iend), INTENT(OUT) :: 
     &          QA_dust_def     ! incoming deficit in the QA(dust) POC flux

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------

!MODHF             REAL, DIMENSION(istr:iend,jstr:jend) :: 
             REAL, DIMENSION(istr:iend) :: 
     &         net_dust_in        ! net incoming dust flux

    !---------------------------------------------------------------------------
    !   parameters, from Armstrong et al. 2000
    !
    !   July 2002, length scale for excess POC and bSI modified by temperature
    !   Value given here is at Tref of 30 deg. C, JKM
    ! 
    !   diss       dissolution length for soft subclass
    !   gamma      fraction of production -> hard subclass
    !   mass       mass of 1e6 base units in kg    (WAS: 1e9 base units in g)
    !   rho        QA mass ratio of POC to this particle class
    !
    !   Base units:
    !     POC:        mmol C      (WAS: nmol C)
    !     P_CaCO3:    mmol CaCO3  (WAS: nmol CaCO3)
    !     P_SiO2:     mmol SiO2   (WAS: nmol SiO2)
    !     dust:       kg dust     (WAS: g dust)
    !     P_iron:     mmol Fe     (WAS: nmol Fe)
    !
    !   Value of first index in sinking_particle_POC and field in original code:
    !     1   sflux_in:    incoming flux of soft subclass (base units/m^2/sec)
    !     2   hflux_in:    incoming flux of hard subclass (base units/m^2/sec)
    !     3   prod:        production term (base units/m^3/sec)
    !     4   sflux_out:   outgoing flux of soft subclass (base units/m^2/sec)
    !     5   hflux_out:   outgoing flux of hard subclass (base units/m^2/sec)
    !     6   remin:       remineralization term (base units/m^3/sec)
    !    NOTE: Area/volume units were cm^2 and cm^3!
    !---------------------------------------------------------------------------
!zhc: for POC
! HF: changed from 120 on Oct 3, 2005, following Keith Moore's changes
             !diss(1)      = 130.0   ! diss. length (m), modified by TEMP
             diss(1)      = 130.0 * parm_diss   ! diss. length (m), modified by TEMP
             !gamma(1)     = 0.4       ! prod frac -> hard subclass, not used
             gamma(1)     = 0.4  * parm_gamma     ! prod frac -> hard subclass, not used
             mass(1)      = 12.01     ! molecular weight of POC
             rhoo(1)      = c1                 ! not used
!zhc: for p_caco3
             !diss(2)  = 600.0   ! diss. length (m)
             diss(2)  = 600.0 * parm_diss   ! diss. length (m)
! HF: changed from 0.6 on Oct 3, 2005, following Keith Moore's changes
             !gamma(2) = 0.55      ! prod frac -> hard subclass
             gamma(2) = 0.55 * parm_gamma       ! prod frac -> hard subclass
             mass(2)  = 100.09    ! molecular weight of CaCO3
             rhoo(2)   = 0.07 * mass(2) / mass(1)
                                       ! QA mass ratio for CaCO3
                                       ! This ratio is used in ecos_set_interior
!zhc:for p_sio2
! HF: changed from 100 on Oct 3, 2005, following Keith Moore's changes
           !diss(3)   = 22.0   ! diss. length (m), modified by TEMP
           diss(3)   = 22.0 * parm_diss   ! diss. length (m), modified by TEMP
! HF: changed from 0.5 on Oct 3, 2005, following Keith Moore's changes
           !gamma(3)  = 0.37      ! prod frac -> hard subclass
           gamma(3)  = 0.37 * parm_gamma       ! prod frac -> hard subclass
           mass(3)   = 60.08     ! molecular weight of SiO2
            rhoo(3)    = 0.035 * mass(3) / mass(1)
                                       ! QA mass ratio for SiO2
!zhc: for dust
           !diss(4)     = 600.0   ! diss. length (m)
           diss(4)     = 600.0 * parm_diss   ! diss. length (m)
           !gamma(4)     = 0.97      ! prod frac -> hard subclass
           gamma(4)     = 0.97 * parm_gamma       ! prod frac -> hard subclass
          mass(4)      = 1.0e6     ! base units are already kg
          rhoo(4)      = 0.07 * mass(4)  / mass(1) 
                                       ! QA mass ratio for dust
!zhc: for p_iron
            !diss(5)   = 600.0   ! diss. length (m) - not used
            diss(5)   = 600.0  * parm_diss  ! diss. length (m) - not used
            gamma(5)  = c0                 ! prod frac -> hard subclass - not used
            mass(5)   = c0                 ! not used
            rhoo(5)   = c0                 ! not used

    !---------------------------------------------------------------------------
    !   Set incoming fluxes
    !---------------------------------------------------------------------------

         sinking_particle_P_CaCO3(4,istr:iend,j) = c0
         sinking_particle_P_CaCO3(5,istr:iend,j) = c0

        sinking_particle_P_SiO2(4,istr:iend,j) = c0
        sinking_particle_P_SiO2(5,istr:iend,j) = c0

      if (ldust_flux) then
       net_dust_in = net_dust_in * (c1 - parm_fe_bioavail)

       sinking_particle_dust(4,istr:iend,j) = 
     &     (c1 - gamma(4)) * net_dust_in
       sinking_particle_dust(5,istr:iend,j) = 
     &     gamma(4) * net_dust_in 

      ELSE
            sinking_particle_dust(4,istr:iend,j) = c0
            sinking_particle_dust(5,istr:iend,j) = c0
      END IF

            sinking_particle_P_iron(4,istr:iend,j) = c0
            sinking_particle_P_iron(5,istr:iend,j) = c0

    !------------------------------------------------------------------------
    !   Hard POC is QA flux and soft POC is excess POC.
    !------------------------------------------------------------------------

           sinking_particle_POC(4,istr:iend,j) = c0
           sinking_particle_POC(5,istr:iend,j) = c0

    !------------------------------------------------------------------------
    !   Compute initial QA(dust) POC flux deficit.
    !------------------------------------------------------------------------

           QA_dust_def = rhoo(4) * 
     &    (sinking_particle_dust(4,istr:iend,j) + 
     &           sinking_particle_dust(5,istr:iend,j))

           END SUBROUTINE init_particulate_terms


!-------------------------------------------------------------------------
!
!     ************   SUBROUTINE COMPUTE_PARTICULATE_TERMS   ****************
!
!-------------------------------------------------------------------------

! HF: changed on Oct 3, 2005, pass in temp instead of Tfunc, 
!      following Keith Moore's changes
             SUBROUTINE compute_particulate_terms(k, QA_dust_def, 
     &                 temp,istr,iend,j)
        implicit none
#include "param.h"  
!need for im_eco,jm_eco
#include "param_bec.h"
#include "ecosys_bec.h"
#include "ocean3d.h"

    !---------------------------------------------------------------------------
    !   Compute outgoing fluxes and remineralization terms. Assumes that
    !   production terms have been set. Incoming fluxes are assumed to be the
    !   outgoing fluxes from the previous level.
    !
    !   It is assumed that there is no production of dust.
    !
    !   Instantaneous remineralization in the bottom cell is implemented by
    !   setting the outgoing flux to zero.
    !
    !   For POC, the hard subclass is the POC flux qualitatively associated
    !   with the ballast flux. The soft subclass is the excess POC flux.
    !
    !   Remineralization for the non-iron particulate pools is computed
    !   by first computing the outgoing flux and then computing the
    !   remineralization from conservation, i.e.
    !      flux_in - flux_out + prod * dz - remin * dz == 0.
    !
    !   For iron, remineralization is first computed from POC remineralization
    !   and then flux_out is computed from conservation. If the resulting
    !   flux_out is negative or should be zero because of the sea floor, the
    !   remineralization is adjusted.
    !   Note: all the sinking iron is in the P_iron%sflux pool, hflux Fe not 
    !      explicitly tracked, it is assumed that total iron remin is 
    !	     proportional to total POC remin.
    !
    !   Based upon Armstrong et al. 2000
    !
    !   July 2002, added temperature effect on remin length scale of 
    !	excess POC (all soft POM& Iron) and on SiO2.
    !   new variable passed into ballast, Tfunc, main Temperature function
    !   computed in ecosystem routine.  scaling factor for dissolution
    !   of excess POC, Fe, and Bsi now varies with location (f(temperature)).
    !
    !   Added diffusive iron flux from sediments at depths < 1100m,
    !   based on Johnson et al., 1999, value of 5 umolFe/m2/day, 
    !       this value too high, using 2 umolFe/m2/day here
    !
    !   Allow hard fraction of ballast to remin with long length scale 40,000m
    !	  thus ~ 10 of hard ballast remins over 4000m water column.
    !
    !   Sinking dust flux is decreased by assumed instant solubility/dissolution
    !	  at ocean surface from the parm_fe_bioavail.
    !---------------------------------------------------------------------------
    !---------------------------------------------------------------------------
    !   arguments
    !---------------------------------------------------------------------------

           INTEGER, INTENT(IN) :: k,istr,iend,j ! vertical model level

 !MODHF         REAL, DIMENSION(istr:iend,jstr:jend), INTENT(INOUT) :: 
          REAL, DIMENSION(istr:iend), INTENT(INOUT) :: 
     &       QA_dust_def     ! incoming deficit in the QA(dust) POC flux

!MODHF          REAL, DIMENSION(istr:iend,jstr:jend), INTENT(IN) :: 
          REAL, DIMENSION(istr:iend), INTENT(IN) :: 
     &        temp  	 ! temperature (used for scaling) from ecosystem routine
         

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------

          REAL:: 
     &    decay_CaCO3,         ! scaling factor for dissolution of CaCO3
     &    decay_dust,          ! scaling factor for dissolution of dust
     &    decay_POC_E,         ! scaling factor for dissolution of excess POC
     &    decay_SiO2,          ! scaling factor for dissolution of SiO2
     &    decay_Hard,	      ! scaling factor for dissolution of Hard Ballast
     &    POC_prod_avail,      ! POC production available for excess POC flux
     &    new_QA_dust_def       ! outgoing deficit in the QA(dust) POC flux

 ! HF new local variables
          REAL :: Tfunc_POC, Tfunc_SiO2

           INTEGER:: i                  ! loop index

    !---------------------------------------------------------------------------
    !   incoming fluxes are outgoing fluxes from previous level
    !---------------------------------------------------------------------------

         Sinking_Particle_P_Caco3(1,istr:iend,j) = 
     &         Sinking_Particle_P_Caco3(4,istr:iend,j)
         Sinking_Particle_P_Caco3(2,istr:iend,j) = 
     &         Sinking_Particle_P_Caco3(5,istr:iend,j)

          Sinking_Particle_P_Sio2(1,istr:iend,j) = 
     &           Sinking_Particle_P_Sio2(4,istr:iend,j)
          Sinking_Particle_P_Sio2(2,istr:iend,j) = 
     &           Sinking_Particle_P_Sio2(5,istr:iend,j)

          sinking_particle_dust(1,istr:iend,j) = 
     &             sinking_particle_dust(4,istr:iend,j)
           sinking_particle_dust(2,istr:iend,j) = 
     &              sinking_particle_dust(5,istr:iend,j)

        SINKING_PARTICLE_POC(1,istr:iend,j) = 
     &        SINKING_PARTICLE_POC(4,istr:iend,j)
        SINKING_PARTICLE_POC(2,istr:iend,j) = 
     &          SINKING_PARTICLE_POC(5,istr:iend,j)

          sinking_particle_p_iron(1,istr:iend,j) = 
     &              sinking_particle_p_iron(4,istr:iend,j)
           sinking_particle_p_iron(2,istr:iend,j) = 
     &               sinking_particle_p_iron(5,istr:iend,j)

    !---------------------------------------------------------------------------
    !   compute decay factors
    !---------------------------------------------------------------------------
         DO i = istr,iend

          IF (LANDMASK(i,j) ) THEN

             !-----------------------------------------------------------------
             ! decay_POC_E and decay_SiO2 set locally, modified by Tfunc
             !-----------------------------------------------------------------
! HF: compute Tfunc factors with different Q10 values for POC and SiO2
!     changed from Q10=2.0 following Keith Moore's changes
             !Tfunc_POC = 1.12 ** (0.1 * temp(i) - 3.0)
             !Tfunc_SiO2 = 4.0 ** (0.1 * temp(i) - 3.0)
             Tfunc_POC=(Q10_POC*1.12)**(0.1*temp(i)-3.0)
             Tfunc_SiO2=(Q10_POC*4.0)**(0.1*temp(i)-3.0)

    	     decay_POC_E = EXP(-Hz(i,j,k) / diss(1) * Tfunc_POC)
    	     decay_SiO2  = EXP(-Hz(i,j,k) / diss(3) * Tfunc_SiO2)
             decay_CaCO3 = EXP(-Hz(i,j,k) / diss(2))
             decay_dust  = EXP(-Hz(i,j,k) / diss(4))
             decay_Hard  = EXP(-Hz(i,j,k) * parm_decay_hard)

             !------------------------------------------------------------------
             !   Set outgoing fluxes for non-iron pools.
             !   The outoing fluxes for ballast materials are from the
             !   solution of the coresponding continuous ODE across the model
             !   level. The ODE has a constant source term and linear decay.
             !   It is assumed that there is no sub-surface dust production.
             !------------------------------------------------------------------

              Sinking_Particle_P_Caco3(4,i,j) = 
     &              Sinking_Particle_P_Caco3(1,i,j) * decay_CaCO3 + 
     &              Sinking_Particle_P_Caco3(3,i,j) * 
     &             ((c1 - Gamma(2)) * (c1 - decay_CaCO3) * diss(2))

              Sinking_Particle_P_Caco3(5,i,j) = 
     &               Sinking_Particle_P_Caco3(2,i,j) * decay_Hard + 
     &               Sinking_Particle_P_Caco3(3,i,j) * 
     &               (Gamma(2) * Hz(i,j,k))

               Sinking_Particle_P_Sio2(4,i,j) = 
     &               Sinking_Particle_P_Sio2(1,i,j) * decay_SiO2 + 
     &               Sinking_Particle_P_Sio2(3,i,j) * 
     &               ((c1 - Gamma(3)) * (c1 - decay_SiO2) 
     &               * (diss(3) / Tfunc_SiO2))

                Sinking_Particle_P_Sio2(5,i,j) = 
     &               Sinking_Particle_P_Sio2(2,i,j) * decay_Hard + 
     &               Sinking_Particle_P_Sio2(3,i,j) * 
     &              (Gamma(3) * Hz(i,j,k))

                sinking_particle_dust(4,i,j) = 
     &                  sinking_particle_dust(1,i,j) * decay_dust

                 sinking_particle_dust(5,i,j) = 
     &                  sinking_particle_dust(2,i,j) * decay_Hard

             !------------------------------------------------------------------
             !   Compute how much POC_prod is available for deficit reduction
             !   and excess POC flux after subtracting off fraction of non-dust
             !   ballast production from net POC_prod.
             !------------------------------------------------------------------

               POC_prod_avail = Sinking_Particle_POC(3,i,j) - 
     &             rhoo(2) * Sinking_Particle_P_Caco3(3,i,j) - 
     &             rhoo(3) * Sinking_Particle_P_Sio2(3,i,j)

             !------------------------------------------------------------------
             !   Check for POC production bounds violations
             !------------------------------------------------------------------
            	     
              IF (POC_prod_avail < c0) THEN
               print *,"subroutine compute_particulate_terms:mass 
     &            ratio of ballast production exceeds POC production"
               print *, 'POC_prod_avail: ', POC_prod_avail
             END IF

             !------------------------------------------------------------------
             !   Compute 1st approximation to new QA_dust_def, the QA_dust
             !   deficit leaving the cell. Ignore POC_prod_avail at this stage.
             !------------------------------------------------------------------

             IF (QA_dust_def(i) > 0) THEN
                new_QA_dust_def = QA_dust_def(i) * 
     &               (sinking_particle_dust(4,i,j) + 
     &                 sinking_particle_dust(5,i,j)) / 
     &                (sinking_particle_dust(1,i,j) + 
     &                 sinking_particle_dust(2,i,j))
             ELSE
                new_QA_dust_def = c0
             END IF

             !------------------------------------------------------------------
             !   Use POC_prod_avail to reduce new_QA_dust_def.
             !------------------------------------------------------------------

             IF (new_QA_dust_def > c0) THEN
                new_QA_dust_def = new_QA_dust_def - 
     &                    POC_prod_avail * Hz(i,j,k)

                IF (new_QA_dust_def < c0) THEN
                   POC_prod_avail = -new_QA_dust_def / Hz(i,j,k)
                   new_QA_dust_def = c0
                ELSE
                   POC_prod_avail = c0
                END IF
             END IF

               QA_dust_def(i) = new_QA_dust_def

             !------------------------------------------------------------------
             !   Compute outgoing POC fluxes. QA POC flux is computing using
             !   ballast fluxes and new_QA_dust_def. If no QA POC flux came in
             !   and no production occured, then no QA POC flux goes out. This
             !   shortcut is present to avoid roundoff cancellation errors from
             !   the dust%rho * dust_flux_out - QA_dust_def computation.
             !   Any POC_prod_avail still remaining goes into excess POC flux.
             !------------------------------------------------------------------

             IF (SINKING_PARTICLE_POC(2,i,j) == c0 .AND. 
     &                  SINKING_PARTICLE_POC(3,i,j) == c0) THEN
                SINKING_PARTICLE_POC(5,i,j) = c0
             ELSE
                SINKING_PARTICLE_POC(5,i,j) = rhoo(2) * 
     &                (Sinking_Particle_P_Caco3(4,i,j) + 
     &                Sinking_Particle_P_Caco3(5,i,j)) + rhoo(3) * 
     &                (Sinking_Particle_P_Sio2(4,i,j) + 
     &                Sinking_Particle_P_Sio2(5,i,j)) + 
     &                rhoo(4) * (sinking_particle_dust(4,i,j) + 
     &                sinking_particle_dust(5,i,j)) -new_QA_dust_def
                 SINKING_PARTICLE_POC(5,i,j) = 
     &                   MAX(SINKING_PARTICLE_POC(5,i,j), c0)
             END IF

             SINKING_PARTICLE_POC(4,i,j) = SINKING_PARTICLE_POC(1,i,j)
     &                * decay_POC_E + POC_prod_avail 
     &               *((c1 - decay_POC_E) * (diss(1) / Tfunc_POC))

             !------------------------------------------------------------------
             !   Compute remineralization terms. It is assumed that there is no
             !   sub-surface dust production.
             !------------------------------------------------------------------

             Sinking_Particle_P_Caco3(6,i,j) = 
     &              Sinking_Particle_P_Caco3(3,i,j) + 
     &             ((Sinking_Particle_P_Caco3(1,i,j) - 
     &             Sinking_Particle_P_Caco3(4,i,j)) + 
     &             (Sinking_Particle_P_Caco3(2,i,j) - 
     &             Sinking_Particle_P_Caco3(5,i,j))) / Hz(i,j,k)

             Sinking_Particle_P_Sio2(6,i,j) = 
     &              Sinking_Particle_P_Sio2(3,i,j) + 
     &             ((Sinking_Particle_P_Sio2(1,i,j) - 
     &             Sinking_Particle_P_Sio2(4,i,j)) + 
     &             (Sinking_Particle_P_Sio2(2,i,j) - 
     &             Sinking_Particle_P_Sio2(5,i,j))) / Hz(i,j,k)
                  
             SINKING_PARTICLE_POC(6,i,j) = 
     &              SINKING_PARTICLE_POC(3,i,j) + 
     &             ((SINKING_PARTICLE_POC(1,i,j) - 
     &             SINKING_PARTICLE_POC(4,i,j)) + 
     &             (SINKING_PARTICLE_POC(2,i,j) - 
     &             SINKING_PARTICLE_POC(5,i,j))) / Hz(i,j,k)
                  
             sinking_particle_dust(6,i,j) = 
     &            ((sinking_particle_dust(1,i,j) - 
     &            sinking_particle_dust(4,i,j)) + 
     &             (sinking_particle_dust(2,i,j) - 
     &             sinking_particle_dust(5,i,j))) / Hz(i,j,k)

             !------------------------------------------------------------------
             !   Compute iron remineralization and flux out.
             !------------------------------------------------------------------

             IF (SINKING_PARTICLE_POC(1,i,j) + 
     &                 SINKING_PARTICLE_POC(2,i,j) == c0) THEN
                sinking_particle_p_iron(6,i,j) = 
     &               (SINKING_PARTICLE_POC(6,i,j) * parm_Red_Fe_C)
             ELSE
                sinking_particle_p_iron(6,i,j) = 
     &               (SINKING_PARTICLE_POC(6,i,j) * 
     &                (sinking_particle_p_iron(1,i,j) + 
     &                sinking_particle_p_iron(2,i,j)) / 
     &                (SINKING_PARTICLE_POC(1,i,j) + 
     &                 SINKING_PARTICLE_POC(2,i,j)))
             END IF

             sinking_particle_p_iron(4,i,j) = 
     &            sinking_particle_p_iron(1,i,j) + Hz(i,j,k) * 
     &            ((c1 - gamma(5)) * sinking_particle_p_iron(3,i,j) 
     &           - sinking_particle_p_iron(6,i,j))
                  

             IF (sinking_particle_p_iron(4,i,j) < c0) THEN
                sinking_particle_p_iron(4,i,j) = c0
                sinking_particle_p_iron(6,i,j) = 
     &              sinking_particle_p_iron(1,i,j) / Hz(i,j,k) + 
     &               (c1 - gamma(5)) * sinking_particle_p_iron(3,i,j)
             END IF
             
	     !----------------------------------------------------------------
	     !  Compute iron release from dust remin/dissolution
	     !
             !   dust remin 1kg Dust = 35 / 55.847   * 1.0e3 = 626.712 mmolFe
             !			       gFe  molFe/gFe mmolFe/molFe
!HF             !   dust remin gDust = 0.035 / 55.847 * 1.0e9 = 626712.0 nmolFe
!HF             !			    gFe     molFe     nmolFe
             !----------------------------------------------------------------

             sinking_particle_p_iron(6,i,j) = 
     &           sinking_particle_p_iron(6,i,j) + 
     &           sinking_particle_dust(6,i,j) * 626.712


              sinking_particle_p_iron(5,i,j) = 
     &               sinking_particle_p_iron(2,i,j)
             

          ELSE
             Sinking_Particle_P_Caco3(4,i,j) = c0
             Sinking_Particle_P_Caco3(5,i,j) = c0
             Sinking_Particle_P_Caco3(6,i,j) = c0

             Sinking_Particle_P_Sio2(4,i,j) = c0
             Sinking_Particle_P_Sio2(5,i,j) = c0
             Sinking_Particle_P_Sio2(6,i,j) = c0

             sinking_particle_dust(4,i,j) = c0
             sinking_particle_dust(5,i,j) = c0
             sinking_particle_dust(6,i,j) = c0

             SINKING_PARTICLE_POC(4,i,j) = c0
             SINKING_PARTICLE_POC(5,i,j) = c0
             SINKING_PARTICLE_POC(6,i,j) = c0

             sinking_particle_p_iron(4,i,j) = c0
             sinking_particle_p_iron(5,i,j) = c0
             sinking_particle_p_iron(6,i,j) = c0
          END IF

          !---------------------------------------------------------------------
          !   Remineralize everything in bottom cell.
          !---------------------------------------------------------------------

          IF (LANDMASK(i,j) .AND. k == 1) THEN
             Sinking_Particle_P_Caco3(6,i,j) = 
     &            Sinking_Particle_P_Caco3(6,i,j) + 
     &            (Sinking_Particle_P_Caco3(4,i,j) + 
     &            Sinking_Particle_P_Caco3(5,i,j)) 
     &            / Hz(i,j,k)
             Sinking_Particle_P_Caco3(4,i,j) = c0
             Sinking_Particle_P_Caco3(5,i,j) = c0

             Sinking_Particle_P_Sio2(6,i,j) = 
     &             Sinking_Particle_P_Sio2(6,i,j) + 
     &             (Sinking_Particle_P_Sio2(4,i,j) + 
     &             Sinking_Particle_P_Sio2(5,i,j)) / Hz(i,j,k)
             Sinking_Particle_P_Sio2(4,i,j) = c0
             Sinking_Particle_P_Sio2(5,i,j) = c0

             sinking_particle_dust(6,i,j) = 
     &             sinking_particle_dust(6,i,j) + 
     &             (sinking_particle_dust(4,i,j) + 
     &            sinking_particle_dust(5,i,j)) / Hz(i,j,k)
             sinking_particle_dust(4,i,j) = c0
             sinking_particle_dust(5,i,j) = c0

             SINKING_PARTICLE_POC(6,i,j) = 
     &             SINKING_PARTICLE_POC(6,i,j) + 
     &             (SINKING_PARTICLE_POC(4,i,j) + 
     &             SINKING_PARTICLE_POC(5,i,j)) / Hz(i,j,k)
             SINKING_PARTICLE_POC(4,i,j) = c0
             SINKING_PARTICLE_POC(5,i,j) = c0

             sinking_particle_p_iron(6,i,j) = 
     &          sinking_particle_p_iron(6,i,j) + 
     &             (sinking_particle_p_iron(4,i,j) + 
     &         sinking_particle_p_iron(5,i,j)) / Hz(i,j,k)
             sinking_particle_p_iron(4,i,j) = c0
             sinking_particle_p_iron(5,i,j) = c0
             
          !-------------------------------------------------------------------
          ! Add diffusive iron flux if depth < 1100.0m, based on 
          ! Johnson et al.1999, value of 5.0 umolFe/m2/day.
          ! 
          ! eco2.42 - eco2.43, 0.5 umolFe/m2/day, 5.78704e-7 nmolFe/cm2/sec.
          ! begin run eco2.45, 1.0 umolFe/m2/day,  1.1574e-6 nmolFe/cm2/sec.
          ! begin run eco2.06, 2.0 umolFe/m2/day,  2.3148e-6 nmolFe/cm2/sec
          !
!HF       ! begin run eco2.06, 2.0 umolFe/m2/day,  2.3148e-8 mmolFe/m2/sec
          !-------------------------------------------------------------------
          
             IF (-z_r(i,j,k) < 1100.) THEN
               sinking_particle_p_iron(6,i,j) = 
     &             sinking_particle_p_iron(6,i,j) + 
     &        	    (2.3148e-8 / Hz(i,j,k))
!HF     &        	    (2.3148e-6 * dzr(i,j,k))
             ENDIF

           END IF

         END DO

    !---------------------------------------------------------------------------
    !   Set history variables.
    !---------------------------------------------------------------------------

         POC_FLUX_IN_hist(istr:iend,j,k) = 
     &               SINKING_PARTICLE_POC(1,istr:iend,j) + 
     &                SINKING_PARTICLE_POC(2,istr:iend,j)
         POC_PROD_hist(istr:iend,j,k)  = 
     &                SINKING_PARTICLE_POC(3,istr:iend,j)
         POC_REMIN_hist(istr:iend,j,k)       = 
     &                 SINKING_PARTICLE_POC(6,istr:iend,j)
         CaCO3_FLUX_IN_HIST(istr:iend,j,k)  = 
     &           Sinking_Particle_P_Caco3(1,istr:iend,j) + 
     &           Sinking_Particle_P_Caco3(2,istr:iend,j)
         CaCO3_PROD_hist(istr:iend,j,k)      = 
     &           Sinking_Particle_P_Caco3(3,istr:iend,j)
         CaCO3_REMIN_hist(istr:iend,j,k)     = 
     &            Sinking_Particle_P_Caco3(6,istr:iend,j)
         SiO2_FLUX_IN_hist(istr:iend,j,k)    = 
     &            Sinking_Particle_P_Sio2(1,istr:iend,j) + 
     &            Sinking_Particle_P_Sio2(2,istr:iend,j)
         SiO2_PROD_HIST(istr:iend,j,k)      = 
     &            Sinking_Particle_P_SiO2(3,istr:iend,j)
         SiO2_REMIN_HIST(istr:iend,j,k)     = 
     &             Sinking_Particle_P_SiO2(6,istr:iend,j)
         dust_FLUX_IN_hist(istr:iend,j,k)    = 
     &             sinking_particle_dust(1,istr:iend,j) + 
     &              sinking_particle_dust(2,istr:iend,j)
         dust_REMIN_hist(istr:iend,j,k)      = 
     &               Sinking_Particle_dust(6,istr:iend,j)
         P_iron_FLUX_IN_hist(istr:iend,j,k)  = 
     &             sinking_particle_p_iron(1,istr:iend,j) + 
     &             sinking_particle_p_iron(2,istr:iend,j)
         P_iron_PROD_HIST(istr:iend,j,k)    = 
     &              sinking_particle_p_iron(3,istr:iend,j)
         P_iron_REMIN_hist(istr:iend,j,k)    = 
     &             sinking_particle_p_iron(6,istr:iend,j)

          END SUBROUTINE compute_particulate_terms

  !***************************************************************************

!-------------------------------------------------------------------------
!
!     ************   SUBROUTINE WS            ****************************
!
!-------------------------------------------------------------------------

       subroutine WS(SMFTX, SMFTY,landmask,work,istr,iend,jstr,jend)
    !-------------------------------------------------------------------------
    !   result declaration
    !-------------------------------------------------------------------------
        implicit none

! needed for scalars.h
#include "param.h"  
! needed for rho0
#include "scalars.h"       
       integer::istr,iend,jstr,jend
        REAL, DIMENSION(istr:iend,jstr:jend) :: WORK ! 10m windspeed (m/s)

    !-------------------------------------------------------------------------
    !   Compute 10m windspeed from surface velocity fluxes where LAND_MASK is
    !   true. Give zero where LAND_MASK is false.
    !-------------------------------------------------------------------------

    !-------------------------------------------------------------------------
    !   Derivation
    !
    !   stress (N/m^2) is a function of surface velocity fluxes
    !   (taux,tauy) = (SMFTX,SMFTY) / momentum_factor
    !
    !   turbulent velocity scale (m/s) is a function of stress
    !   ustar^2 = sqrt(taux^2 + tauy^2) / rho_air
    !           = sqrt(SMFTX^2 + SMFTY^2) / (momentum_factor rho_air)
    !
    !   turbulent velocity scale (m/s) is a function of 10m windspeed (m/s)
    !   ustar^2 = coef_1 ws + coef_2 WS^2 + coef_3 WS^3
    !   ustar^2 = c_d WS^2
    !
    !   Compute ustar^2 from velocity fluxes and solve cubic equation
    !   for 10m windspeed. Initial guess for iterative solver comes
    !   from assuming a constant drag coefficient c_d.
    !
    !-----------------------------------------------------------------------
        real,parameter::
     &   rho_air   = 1.2            ! ambient air density (kg/m^3)
!HF not needed, use rho0     &   momentum_factor  = 10.0
!HF not needed     &   cmperm        = 100.        ! cm per meter

    !-----------------------------------------------------------------------
    !   arguments
    !-------------------------------------------------------------------------

        REAL, DIMENSION(istr:iend,jstr:jend), INTENT(IN) :: 
     &    SMFTX,  ! 'zonal' surface velocity flux (m^2/s^2)
     &    SMFTY    ! 'meridional' surface velocity flux (m^2/s^2)
! HF: both were in (cm^2/s^s)

    !------------------------------------------------------------------------
    !   local variables
    !-------------------------------------------------------------------------

       REAL, PARAMETER :: 
     &    coef_1  = 0.0027,     ! (m/s)
     &    coef_2  = 0.000142,   ! (non-dimensional)
     &    coef_3  = 0.0000764,  ! (s/m)
     &    c_d     = 1.7e-3      ! drag coefficient for initial u

        REAL, DIMENSION(istr:iend,jstr:jend) :: 
     &    ustar_squared  ! square of turbulent velocity scale (m^2/s^2)
        LOGICAL,DIMENSION(istr:iend,jstr:jend) :: landmask
    !------------------------------------------------------------------------
    !   solve for WS in (m/s)
    !-------------------------------------------------------------------------

       WHERE (landmask)
         ustar_squared = SQRT(SMFTX**2 + SMFTY**2) * rho0 / 
     &         rho_air

         WORK = SQRT(ustar_squared / c_d)

       !----------------------------------------------------------------------
       !   perform 3 Newton iterations
       !   loop constructs are not allowed inside WHERE constructs
       !----------------------------------------------------------------------

          WORK = WORK - (WORK*(coef_1 + WORK*(coef_2 + WORK*coef_3)) 
     &         - ustar_squared) / 
     &         (coef_1 + WORK*(2*coef_2 + WORK*3*coef_3))

          WORK = WORK - (WORK*(coef_1 + WORK*(coef_2 + WORK*coef_3)) 
     &          - ustar_squared) / 
     &         (coef_1 + WORK*(2*coef_2 + WORK*3*coef_3))

          WORK = WORK - (WORK*(coef_1 + WORK*(coef_2 + WORK*coef_3))
     &           - ustar_squared) / 
     &       (coef_1 + WORK*(2*coef_2 + WORK*3*coef_3))

        ELSEWHERE
           WORK = 0.0
        END WHERE

    !-------------------------------------------------------------------------
    !   HF: DO NOT convert to (cm/s)
    !-------------------------------------------------------------------------

        RETURN
        END subroutine WS

!-------------------------------------------------------------------------
!
!     ************   SUBROUTINE CSCHMIDT_O2   ****************************
!
!-------------------------------------------------------------------------

         subroutine CSCHMIDT_O2(SSTT,landmask,SCHMIDT_O2,istr,
     &          iend,jstr,jend)
        implicit none
! needed for c0
#include "param_bec.h"
        
       integer::istr,iend,jstr,jend
         LOGICAL,DIMENSION(istr:iend,jstr:jend) :: landmask

    !---------------------------------------------------------------------------
    !   Compute Schmidt number of O2 in seawater as function of SST
    !   where LAND_MASK is true. Give zero where LAND_MASK is false.
    !
    !   ref : Keeling et al, Global Biogeochem. Cycles, Vol. 12,
    !         No. 1, pp. 141-163, March 1998
    !---------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    !   arguments
    !---------------------------------------------------------------------------

          REAL, DIMENSION(istr:iend,jstr:jend), INTENT(IN) :: SSTT

    !---------------------------------------------------------------------------
    !   result declaration
    !---------------------------------------------------------------------------

          REAL, DIMENSION(istr:iend,jstr:jend) :: SCHMIDT_O2

    !---------------------------------------------------------------------------
    !   coefficients in expansion
    !---------------------------------------------------------------------------

          REAL, PARAMETER :: 
     &      a = 1638.0, 
     &      b = 81.83, 
     &      c = 1.483, 
     &      d = 0.008004

         WHERE (LANDMASK)
           SCHMIDT_O2 = a + SSTT* (-b + SSTT * (c + SSTT * (-d)))
         ELSEWHERE
            SCHMIDT_O2 = c0
          END WHERE
          return
          END subroutine CSCHMIDT_O2

  !*****************************************************************************

!-------------------------------------------------------------------------
!
!     ************   SUBROUTINE O2SATU   ****************************
!
!-------------------------------------------------------------------------

          subroutine O2SATU(SSTT, SSSS,landmask, O2SAT,istr,
     &         iend,jstr,jend)
        implicit none
! needed for c0
#include "param_bec.h"
       integer::istr,iend,jstr,jend
         LOGICAL,DIMENSION(istr:iend,jstr:jend) :: landmask

    !---------------------------------------------------------------------------
    !
    !   Computes oxygen saturation concentration at 1 atm total pressure
    !   in mmol/m^3 given the temperature (t, in deg C) and the salinity (s,
    !   in permil) where LAND_MASK is true. Give zero where LAND_MASK is false.
    !
    !   FROM GARCIA AND GORDON (1992), LIMNOLOGY and OCEANOGRAPHY.
    !   THE FORMULA USED IS FROM PAGE 1310, EQUATION (8).
    !
    !   *** NOTE: THE "A_3*TS^2" TERM (IN THE PAPER) IS INCORRECT. ***
    !   *** IT SHOULDN'T BE THERE.                                ***
    !
    !   O2SAT IS DEFINED BETWEEN T(freezing) <= T <= 40(deg C) AND
    !   0 permil <= S <= 42 permil
    !   CHECK VALUE:  T = 10.0 deg C, S = 35.0 permil,
    !   O2SAT = 282.015 mmol/m^3
    !
    !---------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    !   arguments
    !---------------------------------------------------------------------------

       REAL, DIMENSION(istr:iend,jstr:jend), INTENT(IN) :: 
     &    SSTT,  ! sea surface temperature (C)
     &    SSSS    ! sea surface salinity (psu)

    !---------------------------------------------------------------------------
    !   result declaration
    !---------------------------------------------------------------------------

        REAL, DIMENSION(istr:iend,jstr:jend) :: O2SAT

    !---------------------------------------------------------------------------
    !   local variables
    !---------------------------------------------------------------------------

        REAL, DIMENSION(istr:iend,jstr:jend) :: TS

    !---------------------------------------------------------------------------
    !   coefficients in expansion
    !---------------------------------------------------------------------------

        REAL, PARAMETER :: 
     &    a_0 = 2.00907, 
     &    a_1 = 3.22014, 
     &    a_2 = 4.05010, 
     &    a_3 = 4.94457, 
     &    a_4 = -2.56847E-1, 
     &    a_5 = 3.88767, 
     &    b_0 = -6.24523E-3, 
     &    b_1 = -7.37614E-3, 
     &    b_2 = -1.03410E-2, 
     &    b_3 = -8.17083E-3, 
     &    c_0 = -4.88682E-7

        WHERE (LANDMASK)
           TS = LOG( ((T0_Kelvin+25.0) - SSTT) / (T0_Kelvin + SSTT) )

         O2SAT = EXP(a_0+TS*(a_1+TS*(a_2+TS*(a_3+TS*(a_4+TS*a_5)))) + 
     &         SSSS*( (b_0+TS*(b_1+TS*(b_2+TS*b_3))) + SSSS*c_0 ))
        ELSEWHERE
          O2SAT = c0
       END WHERE
    !---------------------------------------------------------------------------
    !   Convert from ml/l to mmol/m^3
    !---------------------------------------------------------------------------

        O2SAT = O2SAT * 44.6596

        return
        END subroutine O2SATU

  !*****************************************************************************

!-------------------------------------------------------------------------
!
!     ************   SUBROUTINE CSCHMIDT_CO2   ****************************
!
!-------------------------------------------------------------------------

          subroutine CSCHMIDT_CO2(SSTT,landmask,SCHMIDT_CO2,istr,
     &                     iend,jstr,jend )
        implicit none
! needed for c0
#include "param_bec.h"
       integer::istr,iend,jstr,jend
         LOGICAL,DIMENSION(istr:iend,jstr:jend) :: landmask

    !---------------------------------------------------------------------------
    !   Compute Schmidt number of CO2 in seawater as function of SST
    !   where LAND_MASK is true. Give zero where LAND_MASK is false.
    !
    !   ref : Wanninkhof, J. Geophys. Res, Vol. 97, No. C5,
    !   pp. 7373-7382, May 15, 1992
    !---------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    !   arguments
    !---------------------------------------------------------------------------

          REAL, DIMENSION(istr:iend,jstr:jend), INTENT(IN) :: SSTT

    !---------------------------------------------------------------------------
    !   result declaration
    !---------------------------------------------------------------------------

          REAL, DIMENSION(istr:iend,jstr:jend) :: SCHMIDT_CO2

    !---------------------------------------------------------------------------
    !   coefficients in expansion
    !---------------------------------------------------------------------------

         REAL, PARAMETER :: 
     &    a = 2073.1, 
     &    b = 125.62, 
     &    c = 3.6276,
     &    d = 0.043219

         WHERE (LANDMASK)
           SCHMIDT_CO2 = a + SSTT * (-b + SSTT * (c + SSTT * (-d)))
         ELSEWHERE
           SCHMIDT_CO2 = c0
         END WHERE
         return
        END subroutine  CSCHMIDT_CO2

!-------------------------------------------------------------------------
!
!     ************   SUBROUTINE CO2CALC_ROW   ***************************
!-------------------------------------------------------------------------

# ifdef CH_CARBON_DEPTH
       SUBROUTINE co2calc_row(mask, t, s, p, dic_in, ta_in, pt_in,
     &    sit_in, phlo, phhi, ph, xco2_in, atmpres, co2star,
     &    dco2star, pCO2surf, dpco2, hco3, co3, istr,iend)
# else
       SUBROUTINE co2calc_row(mask, t, s, dic_in, ta_in, pt_in,
     &    sit_in, phlo, phhi, ph, xco2_in, atmpres, co2star,
     &    dco2star, pCO2surf, dpco2, istr,iend)
# endif /* CH_CARBON_DEPTH */

    !---------------------------------------------------------------------------
    !   SUBROUTINE CO2CALC
    !
    !   PURPOSE : Calculate delta co2*, etc. from total alkalinity, total CO2,
    !             temp, salinity (s), etc.
    !---------------------------------------------------------------------------
        implicit none
       real,parameter::c0=0.0, c1=1.0, c10=10.0, 
     &   c1000=1000.0, T0_Kelvin=273.16,rho_sw=4.1/3.996
       integer::istr,iend
    !---------------------------------------------------------------------------
    !   input arguments
    !---------------------------------------------------------------------------
       REAL, PARAMETER :: xacc = 1e-10
       INTEGER, PARAMETER :: maxit = 100

  !-----------------------------------------------------------------------------
  !   declarations for function coefficients & species concentrations
  !-----------------------------------------------------------------------------

        REAL, DIMENSION(istr:iend) :: 
     &  k0, k1, k2, kw, kb, ks, kf, k1p, k2p, k3p, ksi, ff, 
     &  bt, st, ft, dic, ta, pt, sit

         LOGICAL, DIMENSION(istr:iend), INTENT(IN) :: mask
         REAL, DIMENSION(istr:iend), INTENT(IN) :: 
     &    t,         ! temperature (degrees C)
     &    s,         ! salinity (PSU)
     &    dic_in,    ! total inorganic carbon (mmol/m^3)
     &    ta_in,     ! total alkalinity (meq/m^3)
     &    pt_in,     ! inorganic phosphate (mmol/m^3)
     &    sit_in,    ! inorganic silicate (mmol/m^3)
     &    phlo,      ! lower limit of pH range
     &    phhi,      ! upper limit of pH range
     &    xco2_in,   ! atmospheric mole fraction CO2 in dry air (ppmv)
     &    atmpres     ! atmospheric pressure (atmosphere)

#ifdef CH_CARBON_DEPTH
         REAL, DIMENSION(istr:iend), INTENT(IN) ::
     &    p         ! hydrostatic pressure (Pa)
#endif /*CH_CARBON_DEPTH*/


    !---------------------------------------------------------------------------
    !   output arguments
    !---------------------------------------------------------------------------

       REAL, DIMENSION(istr:iend), INTENT(OUT) :: 
     &    ph,        ! computed ph values, for initial guess on next time step
     &    co2star,   ! CO2*water (mmol/m^3)
     &    dco2star,  ! delta CO2 (mmol/m^3)
     &    pCO2surf,  ! oceanic pCO2 (ppmv)
     &    dpco2       ! Delta pCO2, i.e, pCO2ocn - pCO2atm (ppmv)

# if defined CH_CARBON_DEPTH
       REAL, DIMENSION(istr:iend), INTENT(OUT) ::
     &    hco3,         ! bicarbonate (mmol/m^3)
     &    co3          ! carbonate (mmol/m^3)
# endif /* CH_CARBON_DEPTH */

    !---------------------------------------------------------------------------
    !   local variable declarations
    !---------------------------------------------------------------------------

       INTEGER :: i

        REAL :: 
     &    mass_to_vol,   ! (mol/kg) -> (mmol/m^3)
     &    vol_to_mass,   ! (mmol/m^3) -> (mol/kg)
     &    tk,            ! temperature (K)
     &    is,            ! ionic strength
     &    scl,           ! chlorinity
     &    co2starair,    ! co2star saturation
     &    tk100, tk1002, invtk, dlogtk, is2, sqrtis, 
     &    s2, sqrts, s15, htotal2


#if  defined CH_CARBON_DEPTH
      !   Declaration of variables used to calculate pressure dependent k1,k2,kb,kw,ks,kf,k1p,k2p,k3p
        REAL ::
     &    pdbar, rt, saltrat,
     &    dvf, dkf, dv, dk, putponk1_mi, putponk2_mi, putponkb_mi, putponkw_mi,
     &    putponks_mi, putponkf_mi, putponk1p_mi, putponk2p_mi, putponk3p_mi
# endif /*CH_CARBON_DEPTH*/

        REAL, DIMENSION(istr:iend) :: 
     &    xco2,          ! atmospheric CO2 (atm)
     &    htotal,        ! free concentration of H ion
     &    x1, x2          ! bounds on htotal for solver

    !---------------------------------------------------------------------------
    !   check for existence of ocean points
    !---------------------------------------------------------------------------


       IF (COUNT(mask) == 0) THEN
         ph          = c0
         co2star     = c0
         dco2star    = c0
         pCO2surf    = c0
         dpCO2       = c0
       RETURN
      END IF

#if  defined CH_CARBON_DEPTH
       IF (COUNT(mask) == 0) THEN
         hco3        = c0
         co3         = c0
       RETURN
      END IF
# endif /*CH_CARBON_DEPTH*/

    !---------------------------------------------------------------------------
    !   set unit conversion factors
    !---------------------------------------------------------------------------

       mass_to_vol = 1e6 * rho_sw
       vol_to_mass = c1 / mass_to_vol

    !---------------------------------------------------------------------------
    !   convert tracer units to per mass  xco2 from uatm to atm
    !---------------------------------------------------------------------------

        DO i = istr,iend
          IF (mask(i)) THEN
            dic(i)  = dic_in(i)  * vol_to_mass
            ta(i)   = ta_in(i)   * vol_to_mass
            pt(i)   = pt_in(i)   * vol_to_mass
            sit(i)  = sit_in(i)  * vol_to_mass
           xco2(i) = xco2_in(i) * 1e-6

          !---------------------------------------------------------------------
          !   Calculate all constants needed to convert between various
          !   measured carbon species. References for each equation are
          !   noted in the code.  Once calculated, the constants are stored
          !   and passed in the common block "const". The original version
          !   of this code was based on the code by Dickson in Version 2 of
          !   "Handbook of Methods for the Analysis of the Various Parameters
          !   of the Carbon Dioxide System in Seawater", DOE, 1994 (SOP No. 3,
          !   p25-26).
          !   Derive simple terms used more than once
          !---------------------------------------------------------------------

            tk       = T0_Kelvin + t(i)
            tk100    = tk * 1e-2
            tk1002   = tk100 * tk100
            invtk    = c1 / tk
            dlogtk   = LOG(tk)

            is       = 19.924 * s(i) / (c1000 - 1.005 * s(i))
            is2      = is * is
            sqrtis   = SQRT(is)
            sqrts    = SQRT(s(i))
            s15      = s(i) ** 1.5
            s2       = s(i) ** 2
            scl      = s(i) / 1.80655

          !---------------------------------------------------------------------
          !   f = k0(1-pH2O)*correction term for non-ideality
          !   Weiss  Price (1980, Mar. Chem., 8, 347-359;
          !                 Eq 13 with table 6 values)
          !---------------------------------------------------------------------

            ff(i) = EXP(-162.8301 + 218.2968/tk100 + 
     &          90.9241*LOG(tk100) - 
     &          1.47696*tk1002 + s(i)*(.025695 - .025225*tk100 + 
     &          0.0049867*tk1002))

          !---------------------------------------------------------------------
          !   K0 from Weiss 1974
          !---------------------------------------------------------------------

            k0(i) = EXP(93.4517/tk100 - 60.2409 + 23.3585*LOG(tk100) + 
     &          s(i)*(0.023517 - 0.023656 * tk100 + 0.0047036 * tk1002))

          !---------------------------------------------------------------------
          !   k1 = [H][HCO3]/[H2CO3]
          !   k2 = [H][CO3]/[HCO3]
          !   Millero p.664 (1995) using Mehrbach et al. data on seawater scale
          !---------------------------------------------------------------------

          k1(i) = 10**(-1*(3670.7*invtk - 62.008 + 9.7944*dlogtk - 
     &         0.0118*s(i) + 0.000116*s2))

          k2(i) = 10**(-1*(1394.7*invtk + 4.777 - 
     &           0.0184*s(i) + 0.000118*s2))
 
          !---------------------------------------------------------------------
          !   kb = [H][BO2]/[HBO2]
          !   Millero p.669 (1995) using data from Dickson (1990)
          !---------------------------------------------------------------------

          kb(i) = EXP((-8966.90 - 2890.53*sqrts - 77.942*s(i) + 
     &          1.728*s15 - 0.0996*s2)*invtk + 
     &          (148.0248 + 137.1942*sqrts + 1.62142*s(i)) + 
     &          (-24.4344 - 25.085*sqrts - 0.2474*s(i)) * 
     &          dlogtk + 0.053105*sqrts*tk)

          !---------------------------------------------------------------------
          !   k1p = [H][H2PO4]/[H3PO4]
          !   DOE(1994) eq 7.2.20 with footnote using data from Millero (1974)
          !---------------------------------------------------------------------

          k1p(i) = EXP(-4576.752*invtk + 115.525 - 18.453 * dlogtk + 
     &         (-106.736*invtk + 0.69171) * sqrts + 
     &          (-0.65643*invtk - 0.01844) * s(i))

          !---------------------------------------------------------------------
          !   k2p = [H][HPO4]/[H2PO4]
          !   DOE(1994) eq 7.2.23 with footnote using data from Millero (1974))
          !---------------------------------------------------------------------

          k2p(i) = EXP(-8814.715*invtk + 172.0883 - 27.927 * dlogtk + 
     &          (-160.340*invtk + 1.3566) * sqrts + 
     &          (0.37335*invtk - 0.05778) * s(i))

          !---------------------------------------------------------------------
          !   k3p = [H][PO4]/[HPO4]
          !   DOE(1994) eq 7.2.26 with footnote using data from Millero (1974)
          !---------------------------------------------------------------------

          k3p(i) = EXP(-3070.75*invtk - 18.141 +  
     &          (17.27039*invtk + 2.81197) * sqrts + 
     &          (-44.99486*invtk - 0.09984) * s(i))

          !---------------------------------------------------------------------
          !   ksi = [H][SiO(OH)3]/[Si(OH)4]
          !   Millero p.671 (1995) using data from Yao and Millero (1995)
          !---------------------------------------------------------------------

          ksi(i) = EXP(-8904.2*invtk + 117.385 - 19.334 * dlogtk + 
     &         (-458.79*invtk + 3.5913) * sqrtis + 
     &         (188.74*invtk - 1.5998) * is + 
     &          (-12.1652*invtk + 0.07871) * is2 + 
     &          LOG(1.0-0.001005*s(i)))

          !---------------------------------------------------------------------
          !   kw = [H][OH]
          !   Millero p.670 (1995) using composite data
          !---------------------------------------------------------------------

          kw(i) = EXP(-13847.26*invtk + 148.9652 - 23.6521 * dlogtk + 
     &        (118.67*invtk - 5.977 + 1.0495 * dlogtk) * 
     &         sqrts - 0.01615 * s(i))

          !---------------------------------------------------------------------
          !   ks = [H][SO4]/[HSO4]
          !   Dickson (1990, J. chem. Thermodynamics 22, 113)
          !---------------------------------------------------------------------

          ks(i) = EXP(-4276.1*invtk + 141.328 - 23.093*dlogtk + 
     &          (-13856*invtk + 324.57 - 47.986*dlogtk) * 
     &          sqrtis + 
     &          (35474*invtk - 771.54 + 114.723*dlogtk) * is - 
     &          2698*invtk*is**1.5 + 1776*invtk*is2 + 
     &          LOG(1.0 - 0.001005*s(i)))

          !---------------------------------------------------------------------
          !   kf = [H][F]/[HF]
          !   Dickson and Riley (1979) -- change pH scale to total
          !---------------------------------------------------------------------

          kf(i) = EXP(1590.2*invtk - 12.641 + 1.525*sqrtis + 
     &         LOG(1.0 - 0.001005*s(i)) +  
     &          LOG(1.0 + (0.1400/96.062)*(scl)/ks(i)))

          !---------------------------------------------------------------------
          !   Calculate concentrations for borate, sulfate, and fluoride
          !   bt : Uppstrom (1974)
          !   st : Morris  Riley (1966)
          !   ft : Riley (1965)
          !---------------------------------------------------------------------

          bt(i) = 0.000232 * scl/10.811
          st(i) = 0.14 * scl/96.062
          ft(i) = 0.000067 * scl/18.9984

          x1(i) = c10 ** (-phhi(i))
          x2(i) = c10 ** (-phlo(i))


#if  defined CH_CARBON_DEPTH

          !----------------------------------------------------------------------
          !   Pressure effect on K's (Millero, 95), not dependent on salinity
          !   taken from calc_saturate_roms.F (provided by Gian-Kasper Plattner 
          !   (note that mistakes that were found in Millero 95 are corrected here!
          !----------------------------------------------------------------------

           pdbar=p(i)/100000.0            !pdbar = from pressure in Pa to bar
           rt=(273.15+t(i))*83.145        !RT in cm^3 bar mole^-1

          !---------------------------------------------------------------------
          !   pressure dependence on k1
          !---------------------------------------------------------------------

           dv  = -25.5 + (0.1271 *t(i)) + (0.0 *t(i)*t(i))
           dk  = -3.08e-3  +(( 0.0877e-3 *t(i)) + (0.0 *t(i)*t(i)))
           putponk1_mi  =dexp((-dv*pdbar /rt) + (0.5*dk*pdbar**2.0/rt))

           k1(i) = k1(i)*putponk1_mi

          !---------------------------------------------------------------------
          !   pressure dependence on k2
          !---------------------------------------------------------------------

           dv  = -15.82 + (-0.0219 *t(i)) + (0.0 *t(i)*t(i))
           dk  =  1.13e-3 +((-0.1475e-3 *t(i)) + (0.0 *t(i)*t(i)))
           putponk2_mi  =dexp((-dv*pdbar /rt) + (0.5*dk*pdbar**2.0/rt))

           k2(i) = k2(i)*putponk2_mi


          !---------------------------------------------------------------------
          !   pressure dependence on kb
          !---------------------------------------------------------------------

           dv  = -29.48 + (0.1622 *t(i)) + (2.608e-3 *t(i)*t(i))
           dk  = -2.84e-3  +((0.0 *t(i)) + (0.0 *t(i)*t(i)))
           putponkb_mi  =dexp((-dv*pdbar /rt) + (0.5*dk*pdbar**2.0/rt))

           kb(i) = kb(i)*putponkb_mi


          !---------------------------------------------------------------------
          !   pressure dependence on kw
          !---------------------------------------------------------------------

           dv  = -25.6 + (0.2324 *t(i)) + (-3.6246e-3 *t(i)*t(i))
           dk  = -5.13e-3 +((0.0794e-3 *t(i)) + (0.0 *t(i)*t(i)))
           putponkw_mi  =dexp((-dv*pdbar /rt) + (0.5*dk*pdbar**2.0/rt))

           kw(i) = kw(i)*putponkw_mi


          !---------------------------------------------------------------------
          !   pressure dependence on ks
          !---------------------------------------------------------------------

           dv  = -18.03 + (0.0466 *t(i)) + (0.316e-3 *t(i)*t(i))
           dk  = -4.53e-3 +((0.09e-3 *t(i)) + (0.0 *t(i)*t(i)))
           putponks_mi  =dexp((-dv*pdbar /rt) + (0.5*dk*pdbar**2.0/rt))

           ks(i) = ks(i)*putponks_mi


          !---------------------------------------------------------------------
          !   pressure dependence on kf
          !---------------------------------------------------------------------

           dv  = -9.78 + (-0.0090 *t(i)) + (-0.942e-3 *t(i)*t(i))
           dk  = -3.91e-3 +((0.054e-3 *t(i)) + (0.0 *t(i)*t(i)))
           putponkf_mi  =dexp((-dv*pdbar /rt) + (0.5*dk*pdbar**2.0/rt))

           kf(i) = kf(i)*putponkf_mi


          !---------------------------------------------------------------------
          !   pressure dependence on k1p
          !---------------------------------------------------------------------

           dv  = -14.51 + (0.1211 *t(i)) + (-0.321e-3 *t(i)*t(i))
           dk  = -2.67e-3 +((0.0427e-3 *t(i)) + (0.0 *t(i)*t(i)))
           putponk1p_mi  =dexp((-dv*pdbar /rt) + (0.5*dk*pdbar**2.0/rt))

           k1p(i) = k1p(i)*putponk1p_mi


          !---------------------------------------------------------------------
          !   pressure dependence on k2p
          !---------------------------------------------------------------------

           dv  = -23.12 + (0.1758 *t(i)) + (-2.647e-3 *t(i)*t(i))
           dk  = -5.15e-3 +((0.09e-3 *t(i)) + (0.0 *t(i)*t(i)))
           putponk2p_mi  =dexp((-dv*pdbar /rt) + (0.5*dk*pdbar**2.0/rt))

           k2p(i) = k2p(i)*putponk2p_mi


          !---------------------------------------------------------------------
          !   pressure dependence on k3p
          !---------------------------------------------------------------------

           dv  = -26.57 + (0.2020 *t(i)) + (-3.042e-3 *t(i)*t(i))
           dk  = -4.08e-3 +((0.0714e-3 *t(i)) + (0.0 *t(i)*t(i)))
           putponk3p_mi  =dexp((-dv*pdbar /rt) + (0.5*dk*pdbar**2.0/rt))

           k3p(i) = k3p(i)*putponk3p_mi


#endif /* CH_CARBON_DEPTH */


        END IF ! if mask

       END DO ! i loop


    !---------------------------------------------------------------------------
    !   If DIC and TA are known then either a root finding or iterative
    !   method must be used to calculate htotal. In this case we use
    !   the Newton-Raphson "safe" method taken from "Numerical Recipes"
    !   (function "rtsafe.f" with error trapping removed).
    !
    !   As currently set, this procedure iterates about 12 times. The
    !   x1 and x2 values set below will accomodate ANY oceanographic
    !   values. If an initial guess of the pH is known, then the
    !   number of iterations can be reduced to about 5 by narrowing
    !   the gap between x1 and x2. It is recommended that the first
    !   few time steps be run with x1 and x2 set as below. After that,
    !   set x1 and x2 to the previous value of the pH +/- ~0.5.
    !---------------------------------------------------------------------------

        CALL drtsafe_row(mask, x1, x2, xacc, htotal,istr,
     &    iend, k0, k1, k2, 
     &   kw, kb, ks, kf, k1p, k2p, k3p, ksi, 
     &    ff, bt, st, ft, dic, ta, pt, sit)
    !---------------------------------------------------------------------------
    !   Calculate [CO2*] as defined in DOE Methods Handbook 1994 Ver.2,
    !   ORNL/CDIAC-74, Dickson and Goyet, eds. (Ch 2 p 10, Eq A.49)
    !---------------------------------------------------------------------------

        DO i = istr,iend
          IF (mask(i)) THEN

          htotal2 = htotal(i) ** 2
          co2star(i) = dic(i) * htotal2 / 
     &          (htotal2 + k1(i)*htotal(i) + k1(i)*k2(i))
          co2starair = xco2(i) * ff(i) * atmpres(i)
          dco2star(i) = co2starair - co2star(i)
          ph(i) = -LOG10(htotal(i))

          !---------------------------------------------------------------------
          !   Add two output arguments for storing pCO2surf
          !   Should we be using K0 or ff for the solubility here?
          !---------------------------------------------------------------------

          pCO2surf(i) = co2star(i) / ff(i)
          dpCO2(i)    = pCO2surf(i) - xco2(i) * atmpres(i)

          !---------------------------------------------------------------------
          !   Convert units of output arguments
          !   Note: pCO2surf and dpCO2 are calculated in atm above.
          !---------------------------------------------------------------------

          co2star(i)  = co2star(i) * mass_to_vol
          dco2star(i) = dco2star(i) * mass_to_vol

          pCO2surf(i) = pCO2surf(i) * 1e6
          dpCO2(i)    = dpCO2(i) * 1e6

        ELSE ! if mask

          ph(i)       = c0
          co2star(i)  = c0
          dco2star(i) = c0
          pCO2surf(i) = c0
          dpCO2(i)    = c0


        END IF ! if mask
       END DO ! i loop

# if defined CH_CARBON_DEPTH
        DO i = istr,iend
          IF (mask(i)) THEN

          hco3(i) = k1(i) * co2star(i) / htotal(i)
          co3(i) = k2(i) * hco3(i) / htotal(i)


        ELSE ! if mask

          hco3(i)     = c0
          co3(i)      = c0

        END IF ! if mask
       END DO ! i loop
# endif /* CH_CARBON_DEPTH */

      END SUBROUTINE co2calc_row

!-------------------------------------------------------------------------
!
!     ************   SUBROUTINE TALK_ROW   ****************************
!
!-------------------------------------------------------------------------

       SUBROUTINE talk_row(mask, x, fn, df,istr,iend, k0, k1, k2, 
     &   kw, kb, ks, kf, k1p, k2p, k3p, ksi, 
     &    ff, bt, st, ft, dic, ta, pt, sit)

        implicit none
       REAL, PARAMETER :: xacc = 1e-10
       INTEGER, PARAMETER :: maxit = 100

  !-----------------------------------------------------------------------------
  !   declarations for function coefficients & species concentrations
  !-----------------------------------------------------------------------------
         integer :: istr,iend

        REAL, DIMENSION(istr:iend) :: 
     &  k0, k1, k2, kw, kb, ks, kf, k1p, k2p, k3p, ksi, ff, 
     &  bt, st, ft, dic, ta, pt, sit

    !---------------------------------------------------------------------------
    !   This routine computes TA as a function of DIC, htotal and constants.
    !   It also calculates the derivative of this function with respect to
    !   htotal. It is used in the iterative solution for htotal. In the call
    !   "x" is the input value for htotal, "fn" is the calculated value for
    !   TA and "df" is the value for dTA/dhtotal.
    !---------------------------------------------------------------------------
         real,parameter::c1=1.0, c2=2.0, c3=3.0 

    !---------------------------------------------------------------------------
    !   input arguments
    !---------------------------------------------------------------------------
        LOGICAL, DIMENSION(istr:iend), INTENT(IN) :: mask
        REAL, DIMENSION(istr:iend), INTENT(IN) :: x

    !---------------------------------------------------------------------------
    !   output arguments
    !---------------------------------------------------------------------------

         REAL, DIMENSION(istr:iend), INTENT(OUT) :: fn, df

    !---------------------------------------------------------------------------
    !   local variable declarations
    !---------------------------------------------------------------------------

         INTEGER :: i

          REAL :: 
     &    x1, x2, x3, k12, k12p, k123p, a, a2, da, b, b2, db, c

    !---------------------------------------------------------------------------

      DO i = istr,iend
        IF (mask(i)) THEN
          x1 = x(i)
          x2 = x1 * x1
          x3 = x2 * x1
          k12 = k1(i) * k2(i)
          k12p = k1p(i) * k2p(i)
          k123p = k12p * k3p(i)
          a = x3 + k1p(i) * x2 + k12p * x1 + k123p
          a2 = a * a
          da = c3 * x2 + c2 * k1p(i) * x1 + k12p
          b = x2 + k1(i) * x1 + k12
          b2 = b * b
          db = c2 * x1 + k1(i)
          c = c1 + st(i)/ks(i)
          !---------------------------------------------------------------------
          !   fn = hco3+co3+borate+oh+hpo4+2*po4+silicate-hfree-hso4-hf-h3po4-ta
          !---------------------------------------------------------------------

          fn(i) = k1(i) * x1 * dic(i)/b + 
     &          c2 * dic(i) * k12/b + 
     &          bt(i)/(c1 + x1/kb(i)) + 
     &          kw(i)/x1 + 
     &          pt(i) * k12p * x1/a + 
     &          c2 * pt(i) * k123p/a + 
     &          sit(i)/(c1 + x1/ksi(i)) - 
     &          x1/c - 
     &          st(i)/(c1 + ks(i)/x1/c) - 
     &          ft(i)/(c1 + kf(i)/x1) - 
     &          pt(i) * x3/a - 
     &          ta(i)

          !---------------------------------------------------------------------
          !   df = d(fn)/dx
          !---------------------------------------------------------------------

          df(i) = ((k1(i)*dic(i)*b) - k1(i)*x1*dic(i)*db)/b2 - 
     &          c2 * dic(i) * k12 * db/b2 - 
     &          bt(i)/kb(i)/(c1+x1/kb(i)) ** 2 - 
     &          kw(i)/x2 + 
     &          (pt(i) * k12p * (a - x1 * da))/a2 - 
     &          c2 * pt(i) * k123p * da/a2 - 
     &          sit(i)/ksi(i)/(c1+x1/ksi(i)) ** 2 - 
     &          c1/c + 
     &          st(i) * (c1 + ks(i)/x1/c)**(-2) * (ks(i)/c/x2) + 
     &          ft(i) * (c1 + kf(i)/x1)**(-2) * kf(i)/x2 - 
     &          pt(i) * x2 * (c3 * a - x1 * da)/a2

        END IF ! if mask
        END DO ! i loop

      END SUBROUTINE talk_row

!-------------------------------------------------------------------------
!
!     ************   SUBROUTINE DRTSAFE_ROW   ****************************
!
!-------------------------------------------------------------------------

        SUBROUTINE drtsafe_row(mask_in, x1, x2, xacc, soln,istr,
     &   iend, k0, k1, k2, 
     &   kw, kb, ks, kf, k1p, k2p, k3p, ksi, 
     &    ff, bt, st, ft, dic, ta, pt, sit)
        implicit none
       INTEGER, PARAMETER :: maxit = 100
       integer::istr,iend
  !-----------------------------------------------------------------------------
  !   declarations for function coefficients & species concentrations
  !-----------------------------------------------------------------------------

        REAL, DIMENSION(istr:iend) :: 
     &  k0, k1, k2, kw, kb, ks, kf, k1p, k2p, k3p, ksi, ff, 
     &  bt, st, ft, dic, ta, pt, sit

    !---------------------------------------------------------------------------
    !   Vectorized version of drtsafe, which was a modified version of
    !      Numerical Recipes algorithm.
    !   Keith Lindsay, Oct 1999
    !
    !   Algorithm comment :
    !      Iteration from Newton's method is used unless it leaves
    !      bracketing interval or the dx is > 0.5 the previous dx.
    !      In that case, bisection method is used.
    !---------------------------------------------------------------------------
        real,parameter::c0=0.0, c2=2.0
  
    !---------------------------------------------------------------------------
    !   input arguments
    !---------------------------------------------------------------------------
  
        LOGICAL, DIMENSION(istr:iend), INTENT(IN) :: mask_in
        REAL, DIMENSION(istr:iend), INTENT(IN) :: x1, x2
        REAL, INTENT(IN) :: xacc

    !---------------------------------------------------------------------------
    !   output arguments
    !---------------------------------------------------------------------------

       REAL, DIMENSION(istr:iend), INTENT(OUT) :: soln

    !---------------------------------------------------------------------------
    !   local variable declarations
    !---------------------------------------------------------------------------

       LOGICAL :: leave_bracket, dx_decrease
       LOGICAL, DIMENSION(istr:iend) :: mask
       INTEGER ::  i, it
       REAL :: temp
       REAL, DIMENSION(istr:iend) :: xlo, xhi, flo, fhi, f, 
     &     df, dxold, dx

    !---------------------------------------------------------------------------
    !   bracket root at each location and set up first iteration
    !---------------------------------------------------------------------------

       mask = mask_in

       CALL talk_row(mask, x1, flo, df,istr,iend, k0, k1, k2, 
     &   kw, kb, ks, kf, k1p, k2p, k3p, ksi, 
     &    ff, bt, st, ft, dic, ta, pt, sit)
       CALL talk_row(mask, x2, fhi, df,istr,iend, k0, k1, k2, 
     &   kw, kb, ks, kf, k1p, k2p, k3p, ksi, 
     &    ff, bt, st, ft, dic, ta, pt, sit)

       DO i = istr,iend
         IF (mask(i)) THEN
           IF (flo(i) .LT. c0) THEN
              xlo(i) = x1(i)
              xhi(i) = x2(i)
           ELSE
             xlo(i) = x2(i)
             xhi(i) = x1(i)
             temp = flo(i)
             flo(i) = fhi(i)
             fhi(i) = temp
          END IF
          soln(i) = 0.5 * (xlo(i) + xhi(i))
          dxold(i) = ABS(xlo(i) - xhi(i))
          dx(i) = dxold(i)
         END IF
        END DO

        CALL talk_row(mask, soln, f, df,istr,iend, k0, k1, k2, 
     &   kw, kb, ks, kf, k1p, k2p, k3p, ksi, 
     &    ff, bt, st, ft, dic, ta, pt, sit)

    !---------------------------------------------------------------------------
    !   perform iterations, zeroing mask when a location has converged
    !---------------------------------------------------------------------------

       DO it = 1,maxit
        DO i = istr,iend
           IF (mask(i)) THEN
             leave_bracket = ((soln(i)-xhi(i))*df(i)-f(i)) * 
     &             ((soln(i)-xlo(i))*df(i)-f(i)) .GE. 0
             dx_decrease = ABS(c2 * f(i)) .LE. ABS(dxold(i) * df(i))
             IF (leave_bracket .OR. .NOT. dx_decrease) THEN
                dxold(i) = dx(i)
                dx(i) = 0.5 * (xhi(i) - xlo(i))
                soln(i) = xlo(i) + dx(i)
                IF (xlo(i) .EQ. soln(i)) mask(i) = .FALSE.
             ELSE
                dxold(i) = dx(i)
                dx(i) = -f(i) / df(i)
                temp = soln(i)
                soln(i) = soln(i) + dx(i)
                IF (temp .EQ. soln(i)) mask(i) = .FALSE.
             END IF
             IF (ABS(dx(i)) .LT. xacc) mask(i) = .FALSE.
          END IF
        END DO

        IF (.NOT. ANY(mask)) RETURN

        CALL talk_row(mask, soln, f, df,istr,iend, k0, k1, k2, 
     &   kw, kb, ks, kf, k1p, k2p, k3p, ksi, 
     &    ff, bt, st, ft, dic, ta, pt, sit)

        DO i = istr,iend
          IF (mask(i)) THEN
             IF (f(i) .LT. c0) THEN
                xlo(i) = soln(i)
                flo(i) = f(i)
             ELSE
                xhi(i) = soln(i)
                fhi(i) = f(i)
             END IF
          END IF
        END DO

       END DO ! iteration loop

       END SUBROUTINE drtsafe_row
#else
       subroutine ecosys_tile__empty ()
       
         end
#endif  /*BIOLOGY_BEC*/
 
