!
!DL: gas exchange fluxes:
!
#ifdef SOLVE3D
!DL: for gas exchange fluxes:
# ifdef BIOLOGY_NPZDOC
      real GasExcFlux(GLOBAL_2D_ARRAY,NumGasExcTerms)
      common /gasexcflux/GasExcFlux

      integer hisGasExcFlux(NumGasExcTerms)
      common /gasexcflux/ hisGasExcFlux
      integer avgGasExcFlux(NumGasExcTerms)
      common /gasexcflux/ avgGasExcFlux
      integer slavgGasExcFlux(NumGasExcTerms)
      common /gasexcflux/ slavgGasExcFlux
      character*42  vname_GasExcFlux(4,NumGasExcTerms)
      common /gasexcflux/ vname_GasExcFlux
# endif /* BIOLOGY_NPZDOC */
#endif /* SOLVE3D */
