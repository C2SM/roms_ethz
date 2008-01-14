#include "cppdefs.h"
#if defined SGI || defined __IFC || defined ALPHA || defined AIX \
 || defined PGI 
!
! Sort out machine-dependent implementation issues: on some
! platforms Open MP threads are kernel-level threads, which means
! that they have different pids and their CPU times can be measured
! separately (in this case summation of the CPU times is performed);
! on other platforms they are user-level threads (like mandated by
! POSIX standard), hence it is no longer possible to distinguish
! CPU time consumed by individual threads; no summation will be
! performed in this case. Additionally, Open MP (standard v. 2.0)
! function omp_get_wtime may not be implemented on some platforms,
! so its use is avoided.
!
# undef KERNEL_THREADS

# if defined SGI || defined PGI
#  undef OMP_GET_WTIME
# else
#  define OMP_GET_WTIME
# endif


                                        ! Initialize timers for all 
      subroutine start_timers           ! threads. NOTE: for some
      implicit none                     ! reason SGI does not support 
      integer getpid, trd               ! OpenMP wall clock timing
# include "param.h"
# include "scalars.h"
# include "comm_vars.h"
# ifdef OMP_GET_WTIME
C$    real*8 omp_get_wtime              ! function, so it is avoided.
# endif
      real*4 etime 
C$    integer omp_get_thread_num, omp_get_num_threads

      numthreads=1
C$    numthreads=omp_get_num_threads()
      trd=0
C$    trd=omp_get_thread_num()
      proc(1)=0
      proc(2)=trd
      proc(1)=getpid()
# ifdef OMP_GET_WTIME 
C$    WallClock=omp_get_wtime()
# endif
      CPU_time(1) = etime(CPU_time(2))
C$OMP CRITICAL (start_timers_cr_rgn)
      if (trd_count.eq.0) then
# ifdef MPI
        if (mynode.eq.0) then 
          write(stdout,'(/1x,2(A,I3,A,I2,A,I3),2(A,I4),A,I3)')
     &         'NUMBER OF NODES:', NNODES, '(',NP_XI, ' x', NP_ETA,
     &         ') THREADS:',  numthreads,  ' TILING:',
     &          NSUB_X,' x', NSUB_E, ' GRID:', LLm,' x',MMm,' x',N
        endif
# else
        write(stdout,'(/3(1x,A,I3),4x,2(A,I4),A,I3)')
     &         'NUMBER OF THREADS:',     numthreads,     'TILING:',
     &          NSUB_X,'x',NSUB_E, 'GRID SIZE:', Lm,' x',Mm,' x',N
# endif
      endif
      trd_count=trd_count+1
# ifdef MPI
      write(stdout,'(1x,A,I3,A,I3,A,I10,A)') 'Process ', mynode,
     &     ' thread', proc(2), ' (pid=', proc(1), ') is active.'
# else
      write(stdout,'(1x,A,I3,A,i10,A)') 'Thread #', proc(2),
     &                         ' (pid=', proc(1), ') is active.'
# endif
      if (trd_count.eq.numthreads) then
        trd_count=0
C$      MPI_master_only write(stdout,'(1x,A/)')
C$   &      'This code was compiled using Open MP library.'
      endif
C$OMP END CRITICAL (start_timers_cr_rgn)
      return
      end
 
      subroutine stop_timers()            ! Finalize timings
      implicit none                       ! for all threads.
# include "param.h"
# include "scalars.h"
# include "comm_vars.h"
# ifdef OMP_GET_WTIME 
C$    real*8 omp_get_wtime
# endif
      real*4 etime 

      if (proc(1).ne.0) then
        proc(1)=0
# ifdef OMP_GET_WTIME 
C$      WallClock = omp_get_wtime() - WallClock 
# endif
        CPU_time(1) = etime(CPU_time(2)) - CPU_time(1) 
C$OMP CRITICAL (stop_timers_cr_rgn)
        if (trd_count.eq.0) write(*,*)
# ifdef MPI
        write(stdout,'(1x,A,I3,2x,A,I3,2(2x,A,F12.2),2x,A,F10.2)')
     & 'Process ', mynode, 'thread', proc(2),  'net:', CPU_time(1),
     &                   'cpu:', CPU_time(2),  'sys:', CPU_time(3)

# else
        write(stdout,'(12x,A,I3,2(2x,A,F12.2),2x,A,F10.2)')
     &               'Thread #', proc(2),  'net:', CPU_time(1),
     &               'cpu:', CPU_time(2),  'sys:', CPU_time(3)
# endif

# ifdef KERNEL_THREADS
        CPU_time_ALL(1)=CPU_time_ALL(1) +CPU_time(1)
        CPU_time_ALL(2)=CPU_time_ALL(2) +CPU_time(2)
        CPU_time_ALL(3)=CPU_time_ALL(3) +CPU_time(3)
# else
        CPU_time_ALL(1)=max(CPU_time_ALL(1), CPU_time(1))
        CPU_time_ALL(2)=max(CPU_time_ALL(2), CPU_time(2))
        CPU_time_ALL(3)=max(CPU_time_ALL(3), CPU_time(3))
# endif
        trd_count=trd_count+1
        if (trd_count.eq.numthreads) then
          trd_count=0
C$        write(stdout,'(20x,A,F15.2,F18.2,F16.2)') 'TOTAL:',
C$   &      CPU_time_ALL(1), CPU_time_ALL(2), CPU_time_ALL(3)
# ifdef OMP_GET_WTIME 
C$        write(stdout,'(3x,2A,F12.2)') 'Wall Clock (elapsed) ',
C$   &                                    'Time:',  WallClock
# endif
        endif
C$OMP END CRITICAL (stop_timers_cr_rgn)
      endif
      return
      end
#else
      subroutine start_timers
      implicit none
#include "param.h"
      MPI_master_only write(stdout,'(/3(1x,A,I3),4x,2(1x,A,I4)/)')
     &       'NUMBER OF THREADS:',    numthreads,     'BLOCKING:',
     &        NSUB_X,'x',NSUB_E,   'HORIZ. GRID SIZE:', Lm,'x',Mm
      return
      end
      subroutine stop_timers()
      return
      end
#endif
!
! The following routine is to catch loss of synchronization in
! Open MP regime. Calls to "sync_trap" are not hardcoded into the
! model, but are rather inserted by "mpc" if the later is directed
! to do so.  The algorithm of trapping is that every thread
! advances its own private counter "priv_count", and then global
! counter "barr_count" (inside critical region) and compares the
! value of global counter with its own private.  Since each thread
! increments the global counter by 1, it grows numthreads-times
! faster than the private, hence within each synchronization region
! the global counter, after incremented by 1 by a thread must have
! values from "previous"+1 to last "previous"+numthreads inclussive
! (here "previous" means the final value after the previous
! synchronization event. As the result, "itest" computed as below
! must always match "priv_count". 
!
      subroutine sync_trap (ibarr)
      implicit none
      integer ibarr, indx, itest
#include "param.h"
#include "scalars.h"
      indx=1+mod(ibarr-1,16)
      priv_count(indx)=priv_count(indx)+1
C$OMP CRITICAL (trap_cr_rgn)
      barr_count(indx)=barr_count(indx)+1
      itest=1+(barr_count(indx)-1)/numthreads
      if (itest .ne. priv_count(indx)) then
        write(*,'(A,3I10)') 'sync error', ibarr,
     &      priv_count(indx),  barr_count(indx)
      elseif (mod(priv_count(indx),4001) .eq.0) then
        write(*,'(A,I12,2(2x,A,I3))') 'barrier count =',
     &              priv_count(indx), 'barr# =', ibarr,
     &                                'trd =',  proc(2) 
      endif
C$OMP END CRITICAL (trap_cr_rgn)
      return
      end
 
