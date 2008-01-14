! rho0      Boussinesque Approximation Mean density [kg/m^3].
! R0,T0,S0  Background constant density [kg/m^3], temperature [deg C]
!           and salinity [PSU] for analytical fields and linear EOS.
! Tcoef     Thermal expansion and sSaline contraction coefficients
!                                                   for linear EOS;


# ifndef NONLIN_EOS
      real R0, T0,Tcoef, S0, Scoef
      common /eos_pars/ R0, T0, Tcoef, S0, Scoef
# endif



 
#ifdef SOLVE3D
      real rho1(GLOBAL_2D_ARRAY,N)
CSDISTRIBUTE_RESHAPE rho1(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /eos_rho1/ rho1
# if defined NONLIN_EOS && defined SPLIT_EOS
      real qp1(GLOBAL_2D_ARRAY,N)
CSDISTRIBUTE_RESHAPE qp1(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /eos_qp1/ qp1
      real qp2
      parameter (qp2=0.0000172)
# else
      real rho(GLOBAL_2D_ARRAY,N)
CSDISTRIBUTE_RESHAPE rho(BLOCK_PATTERN,*) BLOCK_CLAUSE
      common /eos_rho/ rho
# endif
#endif
