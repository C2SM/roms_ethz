!--------------------------------------------------------------------
! Definition of flux operators: 1st, 2nd, 3rd, 4th order,
! used in advection schemes (and order degradation near 
! land masks). 
!--------------------------------------------------------------------
     
            function flux2(q_im1, q_i, ua)
            
            implicit none
      REAL    :: flux2
      REAL    :: q_im1, q_i, ua
            
        flux2= 0.5*( q_i + q_im1 ) - 0.5*sign(1.,ua)*(q_i-q_im1)
     
            return
            end
     
            function flux5_weno(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua)
!
!     WENO5: Weighted Essentially Non-Oscillatory scheme 
!            with 5th-order accuracy
!
! This function computes tracer reconstruction at the grid cell's left 
! edge (u-point i-1/2 or v-point j-1/2). WENO5 uses a convex combination 
! of the polynomials reconstructed on the three ENO3 stencils in order
! to achieve higher accuracy on smooth profiles. Both left and right 
! combinations of mirror symetric stencils around i-1/2 (j-1/2) points 
! are computed. For stability, the upstream stencil combination, 
! identified by the sign of ua (u or v), is selected. 
!
! The scheme includes improvements from Borges et al., 2008 (WENO_Z), i.e.,
! smoothness indicators of higher order (with new non-oscillatory weights) 
! that provide a scheme with less dissipation, higher resolution and better 
! monotonicity preservation than the classical WENO5. In the meantime, WENO_Z 
! removes the need to tune parameter Eps for added dissipation (needed in the 
! original scheme due to suboptimal performance on critical points). 
! 
! References:
!  Guang-Shan JIANG and Chi-Wang SHU, 1996: Efficient Implementation of 
!  Weighted ENO Schemes. JOURNAL OF COMPUTATIONAL PHYSICS 126, 202–228.
!
!  Rong WANG and Raymond J. SPITERI, 2007: Linear instability of the 
!  fifth-order WENO method, SIAM J. Numer. Anal., 45, 1871-1901.
!
!  Borges R., M. Carmona, B. Costa, W.S. Don, 2008: An improved weighted 
!  essentially non-oscillatory scheme for hyperbolic conservation laws.
!  Journal of Computational Physics 227 (2008) 3191–3211
!
!  Implementation: P. Marchesiello and J. Demange 2013
!
      implicit none
      REAL    :: flux5_weno
      REAL    :: q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua
      REAL    :: IS0, IS1, IS2
      REAl    :: d0, d1, d2
      REAl    :: a0, a1, a2
      REAL    :: sum_a
      REAL    :: w0, w1, w2
      REAL    :: p0, p1, p2
      REAL    :: Eps, cff1, cff2, T5

!#  define WENO_Z

! Epsilon value for computing weigths 
!#  ifdef WENO_Z
      Eps = 1.e-40
!#  else
!  --> from 1.e-7 (smoother) to 1.e-5 (sharper)
!      Eps = 1.e-6
!#  endif
! Weigths coefficients
!      d0=1./10.
!      d1=6./10.
!      d2=3./10.
      d0=0.1
      d1=0.6
      d2=0.3
! other coefficients
      cff1=13./12.
      cff2=1./6.

!
      if (ua .ge. 0.) then        ! Take upstream stencils for stability
!
! === Reconstruction at u_i-1/2 using LEFT stencils ===
!
! Nonlinear Smoothness Indicators
        IS0 = cff1*(q_im3 - 2.*q_im2 + q_im1)**2
     &                        + 0.25*(q_im3 - 4.*q_im2 + 3*q_im1)**2
        IS1 = cff1*(q_im2 - 2.*q_im1 + q_i)**2
     &                        + 0.25*(q_im2 - q_i)**2
        IS2 = cff1*(q_im1 - 2.*q_i + q_ip1)**2
     &                        + 0.25*(3.*q_im1 - 4.*q_i + q_ip1)**2
!#  ifdef WENO_Z
! Non-normalized stencil weights
! with parameter T5 of new smoothness indicator
        T5 = abs(IS2-IS0)
        a0  = d0*(1+T5/(Eps+IS0))
        a1  = d1*(1+T5/(Eps+IS1))
        a2  = d2*(1+T5/(Eps+IS2))
!#  else
!        a0  = d0/(Eps+IS0)**2
!        a1  = d1/(Eps+IS1)**2
!        a2  = d2/(Eps+IS2)**2
!#  endif
! Normalized Weigths
        sum_a=a0+a1+a2
        w0  = a0/sum_a
        w1  = a1/sum_a
        w2  = a2/sum_a
! Polynomials
        p0  = cff2*(2.*q_im3 - 7.*q_im2 + 11.*q_im1)
        p1  = cff2*(-q_im2   + 5.*q_im1 + 2.*q_i)
        p2  = cff2*(2.*q_im1 + 5.*q_i   - q_ip1)
! Combination
        flux5_weno = w0*p0 + w1*p1 +w2*p2
        

      else
!
! === Reconstruction at u_i-1/2 using RIGHT stencils ===
!
! Nonlinear Smoothness Indicators
        IS0 = cff1*(q_ip2 - 2.*q_ip1 + q_i)**2
     &                         + 0.25*(q_ip2 -4.*q_ip1 + 3*q_i)**2
        IS1 = cff1*(q_ip1 - 2.*q_i + q_im1)**2
     &                         + 0.25*(q_ip1-q_im1)**2
        IS2 = cff1*(q_i   - 2.*q_im1 + q_im2)**2
     &                         + 0.25*(3.*q_i -4.*q_im1 + q_im2)**2
!#  ifdef WENO_Z
! Non-normalized stencil weights
! with parameter T5 of new smoothness indicator
        T5 = abs(IS2-IS0)
        a0  = d0*(1+T5/(Eps+IS0))
        a1  = d1*(1+T5/(Eps+IS1))
        a2  = d2*(1+T5/(Eps+IS2))
!#  else
!        a0  = d0/(Eps+IS0)**2
!        a1  = d1/(Eps+IS1)**2
!        a2  = d2/(Eps+IS2)**2
!#  endif
! Normalized Weigths
        sum_a=a0+a1+a2
        w0  = a0/sum_a
        w1  = a1/sum_a
        w2  = a2/sum_a
! Polynomials
        p0  = cff2*(2.*q_ip2 - 7.*q_ip1 + 11.*q_i)
        p1  = cff2*(-q_ip1   + 5.*q_i   + 2.*q_im1)
        p2  = cff2*(2.*q_i   + 5.*q_im1 - q_im2)
! Combination
        flux5_weno = w0*p0 + w1*p1 +w2*p2
        
        endif
      return
      end
      
            function flux3_weno( q_im2, q_im1, q_i, q_ip1, ua)
!
!     WENO3: Weighted Essentially Non-Oscillatory of 3rd-order Accuracy
!
! This function computes tracer reconstruction at the grid cell's left 
! edge (u-point i-1/2 or v-point j-1/2).
!
! Implementation: P. Marchesiello and J. Demange 2013
!
      implicit none
      REAL    :: flux3_weno
      REAL    :: q_im2, q_im1, q_i, q_ip1, ua
      REAL    :: IS0, IS1
      REAl    :: a0, a1
      REAL    :: sum_a
      REAL    :: w0, w1
      REAL    :: p0, p1 
      REAL    :: Eps, d0,d1, T3

#  define WENO_Z
!
! Epsilon value for computing weigths 
!#  ifdef WENO_Z
      Eps = 1.e-40
!#  else
!  --> from 1.e-7 (smoother) to 1.e-5 (sharper)
!      Eps = 1.e-6
!#  endif
      d0=1./3.
      d1=2./3.
!
      if (ua .ge. 0.) then        ! Take upstream stencils for stability

        IS0 = (q_im1-q_im2)**2
        IS1 = (q_im1-q_i)**2
!#  ifdef WENO_Z
        T3 = abs(IS1-IS0)
        a0  = d0*(1+T3/(Eps+IS0))
        a1  = d1*(1+T3/(Eps+IS1))
!#  else
!        a0  = 1./(3.*(Eps+IS0)**2)
!        a1  = 2./(3.*(Eps+IS1)**2)
!#  endif
        sum_a=a0+a1
        w0  = a0/sum_a
        w1  = a1/sum_a
        p0  = 1./2.*(3.*q_im1-q_im2)
        p1  = 1./2.*(q_im1+q_i)

        flux3_weno = w0*p0 + w1*p1

      else

        IS0 = (q_i-q_ip1)**2
        IS1 = (q_im1-q_i)**2
!#  ifdef WENO_Z
        T3 = abs(IS1-IS0)
        a0  = d0*(1+T3/(Eps+IS0))
        a1  = d1*(1+T3/(Eps+IS1))
!#  else
!        a0  = 1./(3.*(Eps+IS0)**2)
!        a1  = 2./(3.*(Eps+IS1)**2)
!#  endif
        sum_a=a0+a1
        w0  = a0/sum_a
        w1  = a1/sum_a
        p0  = 1./2.*(3.*q_i-q_ip1)
        p1  = 1./2.*(q_im1+q_i)

        flux3_weno = w0*p0 + w1*p1

      endif

      return
      end
