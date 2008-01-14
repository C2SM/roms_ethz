      implicit none
      real*8  beta,gamma,delta,epsil,  cff0,cff1,cff2,cff3


      beta=0.22
      epsil=0.0088



      cff1=1.5D0+beta
      cff2=-0.5D0-2.D0*beta
      cff3=beta

      write(*,'(4(9x,A,F10.6/))') 'cff1=', cff1, 'cff2=', cff2,
     &                                           'cff3=', cff3 


      gamma=1.D0/3.D0 -beta -2.D0*epsil


c      gamma=0.088


      delta=0.5D0 +gamma +2.D0*epsil
      cff3=epsil
      cff2=gamma
      cff1=1.D0-delta-gamma-epsil
      cff0=delta

      write(*,'(4(9x,A,F10.6/))') 'cff0=', cff0, 'cff1=', cff1,
     &                            'cff2=', cff2, 'cff3=', cff3

      stop 
      end

