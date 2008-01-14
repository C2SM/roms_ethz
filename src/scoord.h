! File "scoord.h": Control parameters associated with vertical
! coordinate transformation: "theta_s", "theta_b" are surface and
! bottom refinement parameters [non-dimensional, 0 =< theta_s < 10,
! 0 =< theta_b < 1], with theta_s=theta_b=0 corresponds to vertically
! uniform grid spacing (aka plain sigma-coordinate), while increase of
! "theta_s" yields a more concentrated resolution near the surface;
! "hc" is critical depth [meters].  This is the depth above which
! the vertical grid spacing remains approximately uniform, and
! becomes stretched below (hence, optimally chosen "hc" should be
! comparable to the expected thermocline depth.  "Cs_w" and "Cs_r"
! are stretching functions, Cs=Cs(s), where "s" is sigma-coordinate,
! -1 < s < 0, and "_w", "_r" stands for vertical W- and RHO-point
! locations.  Once (Cs) is specified, the unperturbed (zeta==0)
! vertical coordinate transformation z=z(s) is defined as either,
!
!               z = hc*s + (h-hc)*Cs(s) 
! or
!
!                        hc*s + h*Cs(s)
!               z = h * ---------------- 
!                            hc + h
!
! where the later version is introduced by CPP switch NEW_S_COORD
! defined here.  The upper transformation has the limitation of
!
!                       hc < hmin 
!
! otherwise z=z(s) looses monotonicity, resulting in negative grid
! spacing,  which severely restricts the choice of hc.  In the lower
! case "hc" can be selected independently from minimum depth "hmin".
!  
#define NEW_S_COORD

#ifdef SOLVE3D
      real theta_s,theta_b, hc, Cs_w(0:N), Cs_r(N)
      common /scoord_params/ theta_s,theta_b, hc, Cs_w,Cs_r
#endif
