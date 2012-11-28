#include "cppdefs.h"
#define SUM_BY_PAIRS
#define  ETALON_CHECK
 
      subroutine diag (tile)
      implicit none
      integer tile
#include "param.h"
#include "private_scratch.h"
#include "compute_tile_bounds.h"
      call diag_tile (istr,iend,jstr,jend,  A2d(1,1), A2d(1,2),
     &                                                 A2d(1,3)
#ifdef SOLVE3D
     &       , A2d(1,4), A2d(1,5), A2d(1,6), A2d(1,7), A2d(1,8)
#endif
     &                                                        )
      return
      end
 
      subroutine diag_tile (istr,iend,jstr,jend, dVol, ke,pe
#ifdef SOLVE3D
     &                           , ke2b, ke3bc, kesrf, ub,vb
#endif
     &                                                      )
      implicit none
      integer istr,iend,jstr,jend, i,j,k, nsubs, ierr, ie
#include "param.h"
      real dVol(PRIVATE_2D_SCRATCH_ARRAY),  cff,
     &       ke(PRIVATE_2D_SCRATCH_ARRAY),  dA,    v2, 
     &       pe(PRIVATE_2D_SCRATCH_ARRAY),  my_v2d_max
#ifdef SOLVE3D
      real ke2b(PRIVATE_2D_SCRATCH_ARRAY),  v2bc,
     &    ke3bc(PRIVATE_2D_SCRATCH_ARRAY),  my_v3d_max,
     &    kesrf(PRIVATE_2D_SCRATCH_ARRAY),  my_v3bc_max,
     &       ub(PRIVATE_2D_SCRATCH_ARRAY),
     &       vb(PRIVATE_2D_SCRATCH_ARRAY)
      real*QUAD  my_ke2b, my_ke3bc, my_kesrf
#endif
      real*QUAD my_avzeta, my_ke, my_pe
      logical lsumX, lsumY
      integer max_check_line, inc,jnc
      parameter (max_check_line=128)
      character check_line*(max_check_line), tstring*18 
#ifdef MPI
# include "mpif.h"
      integer size, step, status(MPI_STATUS_SIZE)
      real*QUAD buff(16)
# ifdef  COMPUTE_SPEED_DIAGNOSE
     &      , wtime0, wtime1, wtime2
      save wtime0, wtime1
# endif
      common /xyz/ buff
#endif
#include "grid.h"
#ifdef SOLVE3D
# include "ocean3d.h"
# include "eos_vars.h"
#else
# include "ocean2d.h"
#endif
#include "scalars.h"
#include "comm_vars.h"
#include "ncvars.h"
 
#ifdef ETALON_CHECK
      integer ncheck, npars
      parameter (ncheck=32, npars=8)
      integer icheck, check_point(ncheck)
      character fmt*8
      character*(max_check_line) etalon_line(ncheck)
      real    A0(npars), A1(npars)
      integer P0(npars), P1(npars)
 
      do icheck=1,ncheck          ! reset checklines to all-blanc
        check_point(icheck)=-1    ! status, before filling them with
        etalon_line(icheck)=' '   ! meaningful values. This is needed
      enddo                       ! because some of them may be left
                                  ! uninitialized.
# if defined SOLITON
#  include "etalon_data.SOLITON"
# elif defined WAVE_RAD 
#  include "etalon_data.WAVE_RAD"

# elif defined SEAMOUNT
#  include "etalon_data.SEAMOUNT"
# elif defined GRAV_ADJ
#  include "etalon_data.GRAV_ADJ"
# elif defined ISWAKE 
#  include "etalon_data.ISWAKE"
# elif defined UPWELLING
#  include "etalon_data.UPWELLING"
# elif defined DAMEE_B
#  include "etalon_data.DAMEE_B"
# elif defined HUMBOLDT
#  include "etalon_data.HUMBOLDT"
# elif defined USWEST
#  include "etalon_data.USWEST"
# elif defined PACIFIC
#  include "etalon_data.PACIFIC"
# elif defined BRAZIL
#  include "etalon_data.BRAZIL" 
# endif
#endif
!
! Compute and report various diagnostics: volume-averaged kinetic and
! potential energy, horizontally averaged free-surface perturbation;
! maximum velocity, etc. Since this operation involves computation of
! global sums, it is done in three stages: at first, summation within
! the tile [subdomain of indices (istr:iend,jstr:jend)] independently
! by individual threads. In the case of three dimensions also perform
! verical summation at this stage; then summation accross the threads
! of the same MPI process (if any), and, finally, MPI reduction to
! compute global integrals.
!
      if (mod(iic-1,ninfo) .eq. 0) then
        my_v2d_max=0.
#ifdef SOLVE3D
        my_v3d_max=0.
        my_v3bc_max=0.
        do j=jstr,jend+1
          do i=istr,iend+1
            ub(i,j)=(Hz(i,j,N)+Hz(i-1,j,N))*u(i,j,N,nstp)
            vb(i,j)=(Hz(i,j,N)+Hz(i,j-1,N))*v(i,j,N,nstp)
          enddo
          do k=N-1,2,-1
            do i=istr,iend+1
              ub(i,j)=ub(i,j)+(Hz(i,j,k)+Hz(i-1,j,k))*u(i,j,k,nstp)
              vb(i,j)=vb(i,j)+(Hz(i,j,k)+Hz(i,j-1,k))*v(i,j,k,nstp)
            enddo
          enddo
          do i=istr,iend+1
            ub(i,j)=(ub(i,j)+(Hz(i,j,1)+Hz(i-1,j,1))*u(i,j,1,nstp))
     &           /(z_w(i,j,N)+z_w(i-1,j,N)-z_w(i,j,0)-z_w(i-1,j,0))
            vb(i,j)=(vb(i,j)+(Hz(i,j,1)+Hz(i,j-1,1))*v(i,j,1,nstp))
     &           /(z_w(i,j,N)+z_w(i,j-1,N)-z_w(i,j,0)-z_w(i,j-1,0))
          enddo
        enddo
 
        cff=g/rho0
        do j=jstr,jend
          do i=istr,iend
            v2=0.5*(ub(i,j)**2+ub(i+1,j)**2 +vb(i,j)**2+vb(i,j+1)**2)
            my_v2d_max=max(my_v2d_max, v2)
 
            ke(i,j)=0.
            pe(i,j)=0.5*g*z_w(i,j,N)*z_w(i,j,N)
 
            ke2b(i,j)=0.5*(z_w(i,j,N)-z_w(i,j,0))*v2
            ke3bc(i,j)=0.
            kesrf(i,j)=0.25*( u(i,j,N,nstp)**2 + u(i+1,j,N,nstp)**2
     &                       +v(i,j,N,nstp)**2 + v(i,j+1,N,nstp)**2)
          enddo
          do k=N,1,-1
            do i=istr,iend
              v2=0.5*( u(i,j,k,nstp)**2 + u(i+1,j,k,nstp)**2
     &                +v(i,j,k,nstp)**2 + v(i,j+1,k,nstp)**2)
 
              v2bc=0.5*( (u(i  ,j,k,nstp)-ub(i  ,j))**2
     &                  +(u(i+1,j,k,nstp)-ub(i+1,j))**2
     &                  +(v(i,j  ,k,nstp)-vb(i,j  ))**2
     &                  +(v(i,j+1,k,nstp)-vb(i,j+1))**2)
 
              my_v3d_max  = max(my_v3d_max,  v2)
              my_v3bc_max = max(my_v3bc_max, v2bc)
 
              ke(i,j)=ke(i,j) + 0.5*v2*Hz(i,j,k)
 
              pe(i,j)=pe(i,j) + cff*Hz(i,j,k)
# ifdef SPLIT_EOS
     &      *(rho1(i,j,k)+qp1(i,j,k)*(z_w(i,j,N)-z_r(i,j,k)))
# else
     &                                            *rho(i,j,k)
# endif
     &                               *(z_r(i,j,k)-z_w(i,j,0))
 
              ke3bc(i,j)=ke3bc(i,j) + 0.5*v2bc*Hz(i,j,k)
            enddo
          enddo
          do i=istr,iend
# ifdef MASKING
            dA=rmask(i,j)/(pm(i,j)*pn(i,j))
# else
            dA=1./(pm(i,j)*pn(i,j))
# endif
            dVol(i,j) = dA * z_w(i,j,N)
            ke(i,j)   = dA * ke(i,j)
            pe(i,j)   = dA * pe(i,j)
            ke2b(i,j) = dA * ke2b(i,j)
            ke3bc(i,j)= dA * ke3bc(i,j)
            kesrf(i,j)= dA * kesrf(i,j)
          enddo
        enddo
#else
        cff=0.5*g
        do j=jstr,jend
          do i=istr,iend
            v2=0.5*( ubar(i,j,kstp)**2+ubar(i+1,j,kstp)**2
     &              +vbar(i,j,kstp)**2+vbar(i,j+1,kstp)**2)
            my_v2d_max=max(my_v2d_max, v2)
# ifdef MASKING
            dA=rmask(i,j)/(pm(i,j)*pn(i,j))
# else
            dA=1./(pm(i,j)*pn(i,j))
# endif
            dVol(i,j)=dA*zeta(i,j,kstp)
            ke(i,j)=dA*0.5*(zeta(i,j,kstp)+h(i,j))*v2
            pe(i,j)=cff*dVol(i,j)*zeta(i,j,kstp)
          enddo
        enddo
#endif /* SOLVE3D */
                                        ! Horizontal summation within
                                        ! the subdomain.
#define SUM_BY_PAIRS
#ifdef SUM_BY_PAIRS
        lsumX=.true.                    ! Compute partial sums within
        lsumY=.true.                    ! the subdomain tile using
        inc=1                           ! split-directional reduction
        jnc=1                           ! by pairs algorithm to avoid
                                        ! accumulation of roundoff
        do while (lsumX .or. lsumY)     ! errors.
          if (istr.le.iend-inc) then
            do j=jstr,jend
              do i=istr,iend-inc,2*inc
                 dVol(i,j) =  dVol(i,j) +  dVol(i+inc,j)
                   ke(i,j) =    ke(i,j) +    ke(i+inc,j)
                   pe(i,j) =    pe(i,j) +    pe(i+inc,j)
# ifdef SOLVE3D
                 ke2b(i,j) =  ke2b(i,j) +  ke2b(i+inc,j)
                ke3bc(i,j) = ke3bc(i,j) + ke3bc(i+inc,j)
                kesrf(i,j) = kesrf(i,j) + kesrf(i+inc,j)
# endif
              enddo
            enddo
            inc=2*inc
          else
            lsumX=.false.
          endif
          if (jstr.le.jend-jnc) then
            do j=jstr,jend-jnc,2*jnc
              do i=istr,iend
                 dVol(i,j) =  dVol(i,j) +  dVol(i,j+jnc)
                   ke(i,j) =    ke(i,j) +    ke(i,j+jnc)
                   pe(i,j) =    pe(i,j) +    pe(i,j+jnc)
# ifdef SOLVE3D
                 ke2b(i,j) =  ke2b(i,j) +  ke2b(i,j+jnc)
                ke3bc(i,j) = ke3bc(i,j) + ke3bc(i,j+jnc)
                kesrf(i,j) = kesrf(i,j) + kesrf(i,j+jnc)
# endif
              enddo
            enddo
            jnc=2*jnc                   ! Code segment below: two-
          else                          ! stage straight summation
            lsumY=.false.               ! algorithm with moderate
          endif                         ! effort to counter roundoff
        enddo                           ! errors.
#else
        do j=jstr+1,jend
          do i=istr,iend
            dVol(i,jstr)  = dVol(i,jstr)  + dVol(i,j)
            ke(i,jstr)    = ke(i,jstr)    + ke(i,j)
            pe(i,jstr)    = pe(i,jstr)    + pe(i,j)
# ifdef SOLVE3D
            ke2b(i,jstr)  = ke2b(i,jstr)  + ke2b(i,j)
            ke3bc(i,jstr) = ke3bc(i,jstr) + ke3bc(i,j)
            kesrf(i,jstr) = kesrf(i,jstr) + kesrf(i,j)
# endif
          enddo
        enddo
        do i=istr+1,iend
          dVol(istr,jstr)  = dVol(istr,jstr)  + dVol(i,jstr)
          ke(istr,jstr)    = ke(istr,jstr)    + ke(i,jstr)
          pe(istr,jstr)    = pe(istr,jstr)    + pe(i,jstr)
# ifdef SOLVE3D
          ke2b(istr,jstr)  = ke2b(istr,jstr)  + ke2b(i,jstr)
          ke3bc(istr,jstr) = ke3bc(istr,jstr) + ke3bc(i,jstr)
          kesrf(istr,jstr) = kesrf(istr,jstr) + kesrf(i,jstr)
# endif
        enddo
#endif
 
        my_avzeta=dVol(istr,jstr)
        my_ke=ke(istr,jstr)
        my_pe=pe(istr,jstr)
#ifdef SOLVE3D
        my_ke2b=ke2b(istr,jstr)
        my_ke3bc=ke3bc(istr,jstr)
        my_kesrf=kesrf(istr,jstr)
#endif
        if (SINGLE_TILE_MODE) then
          nsubs=1
        else
          nsubs=NSUB_X*NSUB_E
        endif
!
! Perform global summation: whoever gets first to the critical region
! resets global sums before global summation starts; after the global
! summation is completed, thread, which is the last one to enter the
! critical region, finalizes the computation of diagnostics and
! prints them out.
!
C$OMP CRITICAL (diag_cr_rgn)
        if (tile_count.eq.0) then
          avzeta=my_avzeta              ! Initialize global sums
          avke=my_ke                    ! for multithreaded shared
          avpe=my_pe                    ! memory summation.
          v2d_max=my_v2d_max
#ifdef SOLVE3D
          avke2b=my_ke2b
          avke3bc=my_ke3bc
          avkesrf=my_kesrf
          v3d_max=my_v3d_max
          v3bc_max=my_v3bc_max
#endif
        else                            ! Perform global summation
          avzeta=avzeta+my_avzeta       ! among the threads within
          avke=avke+my_ke               ! each MPI process.
          avpe=avpe+my_pe
          v2d_max=max(v2d_max, my_v2d_max)
#ifdef SOLVE3D
          avke2b=avke2b+my_ke2b
          avke3bc=avke3bc+my_ke3bc
          avkesrf=avkesrf+my_kesrf
          v3d_max=max(v3d_max, my_v3d_max)
          v3bc_max=max(v3bc_max,my_v3bc_max)
#endif
        endif
 
        tile_count=tile_count+1         ! This counter identifies
                                        ! the last thread, whoever
        if (tile_count.eq.nsubs) then   ! it is, not always master.
          tile_count=0
#ifdef MPI
          if (NNODES.gt.1) then         ! Perform global summation
            size=NNODES                 ! among MPI processes
   1         step=(size+1)/2
              if (mynode.ge.step .and. mynode.lt.size) then
                buff(1)=avzeta
                buff(2)=avke            ! This is MPI_Reduce
                buff(3)=avpe            ! operation. Note that
                buff(4)=avke2b          ! because buff() may be
                buff(5)=avke3bc         ! real*16, the size of
                buff(6)=avkesrf
                buff(7)=v2d_max         ! message in MPI_send
                buff(8)=v3d_max         ! is doubled.
                buff(9)=v3bc_max
 
!>              write(*,*) 'sending ', mynode, '-->', mynode-step
 
                call MPI_Send (buff,  18, MPI_DOUBLE_PRECISION,
     &               mynode-step, 17, ocean_grid_comm,      ierr)
              elseif (mynode .lt. size-step) then
                call MPI_Recv (buff,  18, MPI_DOUBLE_PRECISION,
     &            mynode+step, 17, ocean_grid_comm, status, ierr)
 
!<              write(*,*) 'received ',  mynode, '<--', mynode+step
 
                avzeta=avzeta+buff(1)
                avke=avke+buff(2)
                avpe=avpe+buff(3)
                avke2b=avke2b+buff(4)
                avke3bc=avke3bc+buff(5)
                avkesrf=avkesrf+buff(6)
                v2=buff(7)                  ! here v2 is used as 
                v2d_max=max(v2d_max, v2)    ! scratch variable to
                v2=buff(8)                  ! avoid type conflict
                v3d_max=max(v3d_max, v2)    ! in the case if buff 
                v2=buff(9)                  ! is 16-byte, while 
                v3bc_max=max(v3bc_max, v2)  ! v2, v2d_max, etc... 
              endif                         ! are 8-byte long.
             size=step
            if (size.gt.1) goto 1
          endif
          if (mynode.eq.0) then
# ifdef  COMPUTE_SPEED_DIAGNOSE
              wtime2=MPI_Wtime()
            if (first_time.eq.0) then
              wtime0=wtime2
              wtime1=wtime2
            endif
# endif
#endif
            avke=avke/(volume+avzeta)       ! Compute and print out
            avpe=avpe/(volume+avzeta)       ! global diagnostics
            avkp=avke+avpe                  ! (last thread of master
            avke2b=avke2b/(volume+avzeta)   ! MPI process only).
            avke3bc=avke3bc/(volume+avzeta)
            avkesrf=avkesrf/area
            v2d_max=sqrt(v2d_max)
            v3d_max=sqrt(v3d_max)
            v3bc_max=sqrt(v3bc_max)
#ifdef SOLVE3D
# if defined SEAMOUNT || defined PACIFIC || defined ISWAKE
            if (first_time.eq.0) then
              first_time=1
              write(stdout,2) 'STEP', 'time[DAYS]', 'KINETIC_ENRG',
     &                        'BAROTR_ENRG', 'MAX_U3D', 'MAX_UBAR',
     &                        'MAX_UBCL', 'POTEN_ENRG'
#  ifdef COMPUTE_SPEED_DIAGNOSE
     &               , ' WALL_CLOCK[s]', ' DELTA_WALL_CLOCK[ms]'
#  endif
C$   &                                                     , 'trd'
# ifdef COMPUTE_SPEED_DIAGNOSE
  2           format(1x,A,2x,A,1x,A,5x,A,4x,A,7x,A,6x,A,6x,A,3(4x,A))
# else
  2           format(1x,A,2x,A,1x,A,5x,A,4x,A,7x,A,6x,A,6x,A,4x,A)
# endif
            endif
            write(check_line,3)  avke, avke2b, v3d_max, v2d_max,
     &                                             v3bc_max, avpe
# ifdef COMPUTE_SPEED_DIAGNOSE
     &            , int(wtime2-wtime0), int(1000*(wtime2-wtime1))
# endif
C$   &                                                   , proc(2)
  3         format(1PE17.11, 1PE16.9, 3(1PE15.8), 1PE16.8,I3
# ifdef COMPUTE_SPEED_DIAGNOSE
                ,2(I7)
# endif
     $                )
# else
            if (first_time.eq.0) then
              first_time=1
              print '(2/3A)'
     &          ,'------------------------------'
     &          ,' Time-stepping Diagnostics '
     &          ,'------------------------------'
              write(stdout,'(/1x,A,2x,A,1x,A,5x,A,4x,A,4x,A,1x,3A)')
     &                 'STEP',       'time[DAYS]', 'KINETIC_ENRG',
     &                 'SURFACE_KE', 'POTEN_ENRG', 'FREE_SURFACE'
# ifdef COMPUTE_SPEED_DIAGNOSE
     &               , ' WALL_CLOCK[s]', ' DELTA_WALL_CLOCK[ms]'
# endif
C$   &                                                      ,'trd'
            endif
# ifdef COMPUTE_SPEED_DIAGNOSE
            write(check_line,'(1PE18.11,3(1PE15.8), I8,I6, I3)')
# else
            write(check_line,'(1PE18.11,3(1PE15.8), I3)')
# endif
     &                   avke, avkesrf, avpe, avzeta/area
# ifdef COMPUTE_SPEED_DIAGNOSE
     &            , int(wtime2-wtime0), int(1000*(wtime2-wtime1))
# endif
C$   &                                                   , proc(2)
# endif
#else
            if (first_time.eq.0) then
              first_time=1
              write(stdout,'(1x,A,2x,A,1x,A,5x,A,4x,A,9x,A,1x,A)')
     &                 'STEP',       'time[DAYS]'
# ifdef COMPUTE_SPEED_DIAGNOSE
     &               , 'WALLCLOCK', 'TIME', 'KINETIC_ENRG',
# endif
     &                 'POTEN_ENRG', 'KE+PE',   'FREE_SURFACE'
C$   &                                                      ,'trd'
              endif
              write(check_line,'(1PE17.11,3(1PE15.8),I3)')
     &                   avke, avpe, avke+avpe, avzeta/area
C$   &                                                   , proc(2)
#endif
              ie=max_check_line
              do while (check_line(ie:ie).eq.' ' .and. ie.gt.0)
                ie=ie-1
              enddo                                 ! Suppress FORTRAN
              i=0                                   ! floating point Es
              do while (i.lt.ie)                    ! to shorten the
                i=i+1                               ! diagnostic line.
                if (check_line(i:i).eq.'E' .or.
     &              check_line(i:i).eq.'e') then
                  check_line(i:ie-1)=check_line(i+1:ie)
                  check_line(ie:ie)=' '
                  ie=ie-1
                elseif (ichar(check_line(i:i)).lt.48 .or.
     &                  ichar(check_line(i:i)).gt.57) then
                  if (check_line(i:i).ne.' ' .and.
     &              check_line(i:i).ne.'+'  .and. ! Set may_day_flag
     &              check_line(i:i).ne.'-'  .and. ! to terminate the
     &              check_line(i:i).ne.'.') then  ! run in the case
                    may_day_flag=1                  ! of floating point
                  endif                             ! exception
                endif 
              enddo

              write(tstring,'(F18.8)') tdays
              i=1
              do while (i.lt.18 .and. tstring(i:i).eq.' ')
                i=i+1
              enddo
              write(stdout,'(I7,1x,A,1x,A)') iic-1, tstring(i:i+8),
     &                                          check_line(1:ie)
#ifdef COMPUTE_SPEED_DIAGNOSE
            wtime1=wtime2
#endif
              call flush(stdout)

#ifdef  ETALON_CHECK
# define WRITE_ETALON
!
! The following segment performs comparison of model output from
! current run (stored and formatted as "check_line") with pre-stored
! data ("etalon_line"). This version is designed to handle situation
! where the number of significant digits for each parameter to be
! compared is unknown (determined dynamically) and it is only assumed
! that format contains mantissa and power, which are recognized as
! A1,P1 and A0,P0 for current and etalon data respectively. This is
! necessary to rescale the mantissa of the difference (if occurs) in
! such a way that its power is the same as for the current output.
! This code organization also has the property that no adjustment is
! necessary, if output format (see above) is modified (i.e., number
! of controlled parameters or number of significant digits in each
! parameter is changed).
!
C$          ie=ie-3             ! <--  disregard thread number
            do icheck=1,ncheck
              if (iic-1 .eq. check_point(icheck)) then
# ifdef WRITE_ETALON
                if (mcheck.eq.0) open(unit=19,file='etalon_data.NEW')
                mcheck=mcheck+1
                write(19,'(8x,A,I2,A,I6/8x,A,I2,A/5x,A,3x,3A)')
     &             'check_point(', mcheck, ')=', check_point(icheck),
     &             'etalon_line(', mcheck, ')=',
     &                              '&', '''', check_line(1:ie), ''''
                if (icheck.lt.ncheck) then
                  call flush(19)
                else
                  close(19)
                  write(stdout,*) 'finished etalon_data.NEW.'
                endif
# endif
!
! Extract arrays of mantissa and exponent A1,P1 and A0,P0 from check
! and etalon lines respectively. Perform the comparison, choosing the
! number of controlled parameters to be the smaller of one from check
! and etalon line. Rescale "A0" of etalon line to match scale of
! mantissa of check line.
!
                call read_check_line (check_line,ie, A1,P1,j, npars)
                call read_check_line (etalon_line(icheck),
     &                                           ie, A0,P0,k, npars)
                k=min(j,k)
                if (k.gt.0) then
                  ierr=0
                  do i=1,k
                    if (P0(i).eq.P1(i) .and. A0(i).eq.A1(i)) then
                      A1(i)=0.
                    else
                      A1(i)=A1(i)-A0(i)*10.**(P0(i)-P1(i))
                      ierr=ierr+1
                    endif
                  enddo            !--> discard A0,P0,P1, keep A1
!
! Print out comparison results. If difference is discovered, print
! out only its mantissa, retaining the same number of digits after
! decimal point as in the original format. To do so, analyse the
! format of check line and save the location of decimal point as
! P0(j) and last digit of mantissa P1(j) for each controlled
! parameter indexed j=1,..,k.
!
                  if (ierr.eq.0) then
                    write(stdout,*) 'PASSED_ETALON_CHECK'
                  else
                    do j=1,k          ! P0(j) is location of decimal
                      P0(j)=0         ! point of parameter "j";
                      P1(j)=0         ! P1(j) is location of the last
                    enddo             ! digit of its mantissa
                    j=0
                    do i=1,ie
                      if (check_line(i:i).eq.'.'.and.j.lt.npars) then
                        j=j+1
                        P0(j)=i
                      elseif (check_line(i:i).eq.' '.and.j.gt.0) then
                        if (P0(j).gt.0 .and. P1(j).eq.0) P1(j)=i-1
                      endif
                    enddo                ! unless j.eq.0, "i" is
                                         ! location the first symbol
                    do j=1,k             ! after the last digit of
                      if (j.gt.1) then   ! mantissa of the previous
                        i=P1(j-1)+1      ! parameter. Note that the
                      else               ! whole string is shifted by
                        i=-5             ! 6 characters when printing.
                      endif
                      write(fmt,'(2(A,I2),A)') '(F', P1(j)-i+1, '.',
     &                                              P1(j)-P0(j), ')'
                      write(check_line(i+6:P1(j)+6),fmt) A1(j)
                    enddo
                                     ! For visial purposes replace 0s
                    j=0              ! after decimal point with dots. 
                    do i=2,P1(k)+6
                      if (check_line(i-1:i).eq.'0.') then
                        check_line(i:i)=':'
                        j=1
                      elseif (check_line(i:i).eq.'0' .and.
     &                                       j.eq.1) then
                        check_line(i:i)='.'
                      else
                        j=0
                      endif
                    enddo
                    write(stdout,'(1x,2A/1x,A)')    'difference:',
     &              check_line(1:P1(k)+6), 'DIFFERENCE DISCOVERED'
                  endif      !<-- ierr.eq.0
                else
                  write(stdout,*) 'WARNING: No parameters can be ',
     &                                 'compared by ETALON CHECK.'
                endif     !<-- k.gt.0
              endif     !<-- iic-1 .eq. check_point(icheck)
            enddo     !<-- icheck
#endif /* ETALON_CHECK */


c            if (prev_ke .gt. 1.D0-20) then
c              if (avke .gt. 1.002*prev_ke) then
c                nwrt=1
c                write(*,*) 'Abrupt change in KE: setting nwrt to 1.'
c              endif
c            endif
c            prev_ke=avke


#ifdef MPI
          endif    ! <-- mynode.eq.0

          buff(1)=may_day_flag
          buff(2)=nwrt

          call MPI_Bcast(buff, 4, MPI_DOUBLE_PRECISION,
     &                        0, ocean_grid_comm, ierr)
          may_day_flag=buff(1)
          nwrt=buff(2)

#endif

          if (ninfo.eq. 1   .and.  iic.gt.   8) ninfo=2
          if (ninfo.eq. 2   .and.  iic.gt.  16) ninfo=4
          if (ninfo.eq. 4   .and.  iic.gt.  64) ninfo=8
          if (ninfo.eq. 8   .and.  iic.gt. 256) ninfo=16
          if (ninfo.eq.16   .and.  iic.gt. 512) ninfo=32

#ifndef SOLVE3D
          if (ninfo.eq.32   .and.  iic.gt. 1024) ninfo=64
          if (ninfo.eq.64   .and.  iic.gt. 2048) ninfo=128
          if (ninfo.eq.128  .and.  iic.gt. 4096) ninfo=256
          if (ninfo.eq.256  .and.  iic.gt. 8192) ninfo=512
          if (ninfo.eq.512  .and.  iic.gt.16384) ninfo=1024
          if (ninfo.eq.1024 .and.  iic.gt.32768) ninfo=2048
#endif



        endif  ! <-- tile_count.eq.nsubs
C$OMP END CRITICAL (diag_cr_rgn)
      endif              ! <-- mod(iic-1,ninfo).eq.0
      return
      end

#ifdef ETALON_CHECK
      subroutine read_check_line (string,lstr, A,P,np, npars)
!
! For a given character string of length lstr, which is assumed to
! contain a sequence of floating point numbers written in FY.X, EY.X
! or 1PEY.X format (unknown in advance), determine how many numbers
! "np" are written and extract mantissa A and exponent P separately
! for each of number. If exponent is absent (FY.X format), the
! corresponding P is set to zero.
!
      implicit none
      character string*(*)
      integer  lstr, npars, P(npars), np,  i,k, ierr
      real A(npars)

      np=0
      i=0
      do while (i.lt.lstr .and. np.lt.npars)
        i=i+1
        if (string(i:i).eq.'.') then
          k=i-1
          do while (k.gt.1 .and. ichar(string(k:k)).ge.48
     &                     .and. ichar(string(k:k)).le.57)
            k=k-1
          enddo
          if (string(k:k).eq.' ') k=k+1
          i=i+1
          do while (ichar(string(i:i)).ge.48
     &        .and. ichar(string(i:i)).le.57)
            i=i+1
          enddo
          np=np+1
          A(np)=0.
          P(np)=0
          read(string(k:i-1),*,iostat=ierr) A(np)
          if (string(i:i).eq.'E' .or.
     &        string(i:i).eq.'e') then
            string(i:i)=' '
            i=i+1
          endif
          if (string(i:i).eq.'+' .or. string(i:i).eq.'-') then
            k=i
            i=i+1
            do while (ichar(string(i:i)).ge.48
     &          .and. ichar(string(i:i)).le.57)
              i=i+1
            enddo
            if (string(i:i).ne.'.') then
              read(string(k:i-1),*,iostat=ierr) P(np)
              string(k:i-1)=' '
            endif
          endif
c**       write(*,*) np, A(np),P(np)
        endif
      enddo
      return
      end
#endif
