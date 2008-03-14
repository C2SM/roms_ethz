! R0         Coefficients for linear Equation of State (EOS)
! T0,Tcoef  
! S0,Scoef          rho = R0 - Tcoef*(T-T0) + Scoef*(S-S0)

#ifdef SOLVE3D
# ifndef NONLIN_EOS
      real R0, T0, Tcoef
      common /eos_pars/ R0, T0, Tcoef
#  ifdef SALINITY
      real S0, Scoef 
      common /eos_pars/     S0, Scoef
#  endif
# endif
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
