!   $Log: CalcPar.F,v $
!   Revision 1.2  2002/08/27 00:17:42  frenzel
!   Restricted calculation to ocean cells, and added the option for
!   PAR_DIRECT_INPUT.
!
!   Revision 1.1  2002/04/19 00:42:11  frenzel
!   New routine to calculate PAR and pCO2 before the time-stepping loop
!   so that output of initial conditions to history file is correct. This
!   routine is an excerpt of biology.F. Note that parameters must be modified
!   here as well if they are modified in biology.F.
!
!
#include "cppdefs.h"
#ifdef BIOLOGY_NPZDOC
      subroutine calcpar(tile)
      implicit none
      integer tile
# include "param.h"
# include "compute_tile_bounds.h"
      call calcpar_tile (Istr,Iend,Jstr,Jend)
      return
      end
                                !
      subroutine calcpar_tile (Istr,Iend,Jstr,Jend)
      implicit none
      integer Istr,Iend,Jstr,Jend
#include "param.h"
#include "grid.h"
#include "ocean2d.h"
#include "ocean3d.h"
#include "scalars.h"
#include "forces.h"
#include "biopar.h"
#include "bgcflux.h"
      
#ifdef CARBON
      real pCO2air_loc
#endif
      integer i,j,k
                                !
      real tem(N), sal(N)
#ifdef OXYGEN
      real den(N)
#endif
      real NO3(N), Phyt(N), theta(N), chla(N)
                                !
      real thisPAR, attn
                                !
      real eos80
                                !
#ifdef OXYGEN
      real u10_loc,Sc
      real O2(N)
      real O2satu_loc,Kv_O2_loc
#  ifdef OCMIP_OXYGENSAT
      real o2sato   ! OCMIP function, calculates O2 saturation
#  else /* OCMIP_OXYGENSAT */
      real satpc         ! oxygen saturation in % (calculated, but unused)
      real AOU           ! Apparent oxygen utilization (calc., but unused)
#  endif /* OCMIP_OXYGENSAT */
#endif /* OXYGEN */

#ifdef CARBON
      real DIC(N),TALK(N)
      real TP,TSi,TA,TC,CO2sol_loc,pCO2sw_loc,Kv_CO2_loc
#  ifdef OCMIP_CARBON
      real pHlo, pHhi           ! bounds for the iterative solver
      parameter (pHlo = 6.0, pHhi = 9.0)
! output variables from OCMIP subroutines
      real pHsw_loc             ! pH values
      real CO2star
      real dCO2star
      real dpCO2
      real atmpres
#  else /* OCMIP_CARBON */
      real Pdbar
#  endif /* OCMIP_CARBON */
#endif /* CARBON */
                                !
#include "compute_auxiliary_bounds.h"
      
#ifdef CARBON
# ifdef VARIABLE_ATM_PCO2
      ! the factor deg2rad converts 360d into one full cycle of 2 pi
      ! the subtraction of 30d ensures that the maximum is reached
      ! at the end of April and the minimum at the end of October
      pCO2air_loc = 370. + 2.9 * sin(deg2rad*(tdays-30.))
# else /* VARIABLE_ATM_PCO2 */
       pCO2air_loc = 550.        ! concentration of atmospheric CO2 2050
!      pCO2air_loc = 370.        ! concentration of atmospheric CO2
!       pCO2air_loc = 278.
# endif /* VARIABLE_ATM_PCO2 */
#endif

#  ifdef EW_PERIODIC
#   define I_RANGE Istr,Iend
#  else
#   define I_RANGE IstrR,IendR
#  endif
#  ifdef NS_PERIODIC
#   define J_RANGE Jstr,Jend
#  else
#   define J_RANGE JstrR,JendR
#  endif
      
      j_loop: do j=J_RANGE
      i_loop: do i=I_RANGE
#  ifdef MASKING
        rmask_if: if (rmask(i,j) .eq. 1) then
#  endif /* MASKING */
                                !
                                ! Extract biological variables from tracer arrays; place them into
                                ! scratch variables; restrict their values to be positive definite.
                                !
      k_loop1: do k=1,N
      tem(k)   = max(t(i,j,k,nnew,itemp),0.) ! temperature;       [deg. C]
      sal(k)   = max(t(i,j,k,nnew,isalt),0.) ! salinity;          [PSU]
#ifdef OXYGEN
      den(k)   = 1000.+eos80(0.,tem(k),sal(k)) ! potential density; [kg m-3]
      O2(k)    = max(t(i,j,k,nnew,iO2),0.)     ! Oxygen;  [mmol O2 m-3]
#endif
#ifdef CARBON
      DIC(k)   = max(t(i,j,k,nnew,iDIC),0.) ! Dissolved Inorganic Carbon;  [mmol-C m-3]
      TALK(k)  = max(t(i,j,k,nnew,iTALK),0.) ! Total Alkalinity;        [meq m-3]
#endif /* CARBON */
      NO3(k)   = max(t(i,j,k,nnew,iNO3_),0.) ! Nitrate;                 [mmol-N m-3]
      Phyt(k)  = max(t(i,j,k,nnew,iPhyt),0.) ! Phytoplankton;           [mmol-N m-3]
      chla(k)  = max(t(i,j,k,nnew,iChla),0.) ! Chlor a;                 [mg Chl a m-3]
      if (Phyt(k) .gt. 0.001 .and. chla(k) .gt. 0.001) then
         theta(k) = chla(k)/(Phyt(k)*CN_P*12.) ! Chla/Phyt ratio; [mg Chla (mg C)-1]
      else
         theta(k) = theta_m
      endif
      end do k_loop1
                                ! PAR is the same for all iterations
# ifdef PAR_DIRECT_INPUT
      PARinc(i,j) = parflx(i,j)
# else /* PAR_DIRECT_INPUT */
      PARinc(i,j) = srflx(i,j)*rho0*Cp*0.43
# endif /* PAR_DIRECT_INPUT */
      thisPAR = PARinc(i,j) 
                                !
      PAR_if: if (thisPAR.gt.0.) then
                                !         *** SUN IS UP ***
                                !
         k_loop2: do k=N,1,-1   !<-- irreversible
                                !
                                !           attenuate the light to the center of the grid cell
         attn=exp(-0.5*(kwater+kChla*chla(k))*
     &        (z_w(i,j,k)-z_w(i,j,k-1)))
         thisPAR=thisPAR*attn
         PAR(i,j,k) = thisPAR   !           attenuate the light to the bottom of the grid cell
         thisPAR=thisPAR*attn
      end do k_loop2
      else PAR_if
         k_loop2a: do k = 1, N
         PAR(i,j,k) = 0.0
      end do k_loop2a
      end if PAR_if
                                !
#ifdef OXYGEN
#  ifdef OCMIP_OXYGEN_SC
!*********************************************************************
!  alternative formulation (Sc will be slightly smaller up to about 35 C)
!  Computes the Schmidt number of oxygen in seawater using the
!  formulation proposed by Keeling et al. (1998, Global Biogeochem.
!  Cycles, 12, 141-163).  Input is temperature in deg C.
!
        Sc = 1638.0 - 81.83*Tem(N) + 1.483*(Tem(N)**2) - 
     &       0.008004*(Tem(N)**3)
!*********************************************************************
#  else /* OCMIP_OXYGEN_SC */
!       calculate the Schmidt number for O2 in sea water [Wanninkhof, 1992]
        Sc=1953.4 - 128.0*Tem(N) + 3.9918*(Tem(N)**2) -
     &     0.050091*(Tem(N)**3)
#  endif /* OCMIP_OXYGEN_SC */
!
!       calculate the wind speed from the surface stress values
        u10_loc = sqrt(sqrt( (0.5*(sustr(i,j)+sustr(i+1,j)))**2
     &                         +(0.5*(svstr(i,j)+svstr(i,j+1)))**2)
     &       * rho0 * 550.)   ! 550 = 1 / (1.3 * 0.0014) (=rho_air * CD)
!       calculate the gas transfer coef for O2
        Kv_O2_loc=0.31*u10_loc*u10_loc*sqrt(660./Sc)/(100.*3600.) ! denominator: convert Kv from [cm/h] to [m/s]
!       calculate the saturation oxygen level
#  ifdef OCMIP_OXYGENSAT
        O2satu_loc = o2sato(Tem(N), Sal(N))
#  else /* OCMIP_OXYGENSAT */
        call O2sat(O2(N),Tem(N),Sal(N),den(N),O2satu_loc,satpc,AOU)
#  endif /* OCMIP_OXYGENSAT */
        O2satu(i,j) = O2satu_loc
        Kv_O2(i,j) = Kv_O2_loc
        u10(i,j) = u10_loc
#endif /* OXYGEN */

#ifdef CARBON
                                !       calculate the pCO2 at the surface of the ocean using the CO2 SYSTEM program
                                !       calculate the TP using a poly fit to the CalCOFI data set
      TP=0.334+0.0669*NO3(N)    ! PO4 conc. [mmol-P/m^3]
                                !       calculate the TSi using a poly fit to the CalCOFI data set
      TSi=2.1205 + 1.0146*NO3(N) - 0.0263*(NO3(N)*2) +
     &     0.0013*(NO3(N)**3)   ! SiO4 conc. [mmol-Si/m^3]
      TA=TALK(N)                ! total alkalinity [meq/m^3]
      TC=DIC(N)                 ! dissolved inorganic carbon [mmol C/m^3]
#  ifdef OCMIP_CARBON
      atmpres=1.                ! atmospheric pressure [atm]
                                ! co2calc is from the OCMIP code
                                ! it uses [mol/m^3] instead of [mmol/m^3]
      TA = TA * 1.d-3
      TC = TC * 1.d-3
      TP = TP * 1.d-3
      TSi = TSi * 1.d-3
      call co2calc_SWS(Tem(N),Sal(N),TC,TA,TP,TSi,pHLo,pHHi,pHsw_loc,
     &     pCO2air_loc, den(N),atmpres, CO2star, dCO2star, pCO2sw_loc,  
     &     dpCO2, CO2sol_loc)
#  else /* OCMIP_CARBON */
      Pdbar=0.                  ! water pressure [db]
      call CO2SYS(TP,TSi,Sal(N),Tem(N),den(N),Pdbar,TA,TC,
     &     pCO2sw_loc,CO2sol_loc)
#  endif /* OCMIP_CARBON */
!
!       calculate the Schmidt number for CO2 in sea water [Wanninkhof, 1992]
        Sc=2073.1 - 125.62*Tem(N) + 3.6276*(Tem(N)**2) -
     &     0.043219*(Tem(N)**3)
!       calculate the gas transfer coef from eq. 3 in [Wanninkhof, 1992]
        Kv_CO2_loc=0.31*u10_loc*u10_loc*sqrt(660./Sc)/(100.*3600.) !  denominator: convert Kv from [cm/h] to [m/s]

      pCO2sw(i,j)=pCO2sw_loc
      CO2sol(i,j) = CO2sol_loc
#  ifdef OCMIP_CARBON
      pH_hist(i,j) = pHsw_loc
      pCO2air(i,j) = pCO2air_loc
      Kv_CO2(i,j) = Kv_CO2_loc
#  endif /* OCMIP_CARBON */
#endif /* CARBON */
#  ifdef MASKING
      end if rmask_if
#  endif /* MASKING */
      end do i_loop
      end do j_loop
      
#else /* BIOLOGY_NPZDOC */
      subroutine calcpar_empty ()
#endif /* BIOLOGY_NPZDOC */
      return
      end
