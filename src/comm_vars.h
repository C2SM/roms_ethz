! This following common block contains a set of globally accessable
! variables in order to allow information exchange between parallel
! threads working on different subdomains.
!
! Global summation variables are declared with 16 byte precision
! to avoid accumulation of roundoff errors, since roundoff  error
! depends on the order of summation, which is undeterministic in
! the case of summation between the parallel threads; not doing so
! would make itimpossible to pass an ETALON CHECK test if there is
! a feedback of these sums into the dynamics of the model, such as
! in the case when global mass conservation is enforced.
!
! One sunny sping day, somewhen in 1989 an american tourist, who
! happened to be an attorney, was walking along a Moscow street.
! Because it was the period of 'Perestroika' (which literally means
! 'remodelling'), so that a lot of construction was going on in
! Moscow, dozens of holes and trenches were open on the street. He
! felt into one of them, broke his leg, ended up in a hospital and
! complaining: In my country if a construction firm would not place
! little red flags around the construction zone to warn passers-by
! about the danger, I will sue em for their negligence! The doctror,
! who was performing surgery on his leg replied to him: Did not you
! see the one big red flag above the whole country at the first time?
!
! WARNING: FRAGILE ALIGNMENT SEQUENCE: In the following common block:
! since real objects are grouped in pairs and integer*4 are grouped
! in quartets, it is guaranteed that 16 Byte objects are aligned
! in 16 Byte boundaries and 8 Byte objects are aligned in 8 Byte
! boundaries. Removing or introduction of variables with violation
! of parity, as well as changing the sequence of variables in the
! common block may cause violation of alignment.
!
      real*QUAD  area,   volume,  avzeta,  avke,  prev_ke,
     &     avpe, avke2b, avke3bc, avkesrf, avkp,  bc_crss
      common /comm_vars/
     &           area,   volume,  avzeta,  avke,  prev_ke,
     &     avpe, avke2b, avke3bc, avkesrf, avkp,  bc_crss
#ifdef OBC_VOLCONS
      real*QUAD bc_flux, ubar_xs
      common /comm_vars/ bc_flux, ubar_xs
#endif
#if defined BIOLOGY || defined BIOLOGY_NPZDOC
      real*QUAD global_sum(0:2*NT+1), global_srf_sum(0:NT)
      common /comm_vars_bio/ global_sum, global_srf_sum
#endif
      real hmin,hmax, grdmin,grdmax, rx0,rx1, Cu_min,Cu_max,
     &     Cu_Cor,    Cu_visc,   v2d_max, v3d_max, v3bc_max
      common /comm_vars/
     &     hmin,hmax, grdmin,grdmax, rx0,rx1, Cu_min,Cu_max,
     &     Cu_Cor,    Cu_visc,   v2d_max, v3d_max, v3bc_max

      real*4 CPU_time_ALL(4)
      integer trd_count, tile_count, bc_count, mcheck, first_time
      common /comm_vars/ CPU_time_ALL, trd_count, tile_count,
     &                               bc_count, mcheck, first_time

