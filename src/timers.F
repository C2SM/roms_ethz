#include "cppdefs.h"
#ifndef NO_COMPILER_SUPPORT_FOR_TIMING
! Most modern compilers recognize "etime" and Open MP "omp_get_wtime"
! functions as intrinsics, so the above switch should never be defined.
! However, of otherwise needed, stub functions for start/stop_timers
! are provided for compatibility at the end of this file.

! Sort out machine-dependent implementation issues: on some platforms
! Open MP threads are kernel-level threads, which means that they have
! distinct process IDs and their CPU times can be measured separately
! (in this case summation of the CPU times is performed); on other
! platforms they are user-level threads (like mandated by POSIX
! standard), hence it is no longer possible to distinguish CPU time
! consumed by individual threads; no summation will be performed in
! this case. Additionally, Open MP (standard v. 2.0) function
! omp_get_wtime may not be implemented on some platforms, so its
! use is avoided.

# undef KERNEL_THREADS

# if defined SGI || defined PGI
#  undef OMP_GET_WTIME
# else
#  define OMP_GET_WTIME
# endif
                                         ! Initialize timers for all
      subroutine start_timers            ! the threads. Note that for
      implicit none                      ! some reason SGI does not
      integer getpid, trd                ! support native OpenMP wall
# include "param.h"
# include "scalars.h"
# include "comm_vars.h"
# ifdef OMP_GET_WTIME
C$    real*8 omp_get_wtime               ! clock timing function,
# endif
# ifdef __IFC
      real*4 etime                       ! so its usage is avoided.
# endif
C$    integer omp_get_thread_num, omp_get_num_threads

      numthreads=1     ; trd=0           ! Note: "etime" can be either
C$    numthreads=omp_get_num_threads()   ! a function or a subroutine,
C$    trd=omp_get_thread_num()           ! depending on the compiler.
      proc(1)=getpid() ; proc(2)=trd     ! Its first argument is an
                                         ! array of two elements which
# ifdef OMP_GET_WTIME
C$    WallClock=omp_get_wtime()          ! are "user" CPU time and
# endif
# ifdef __IFC
      cpu_time(3) = etime(cpu_time)      ! "system" time respectively.
# else
      call etime(cpu_time,cpu_time(3))
# endif
C$OMP CRITICAL (start_timers_cr_rgn)
      if (trd_count==0) then
# ifdef MPI
        if (mynode==0) then
          write(*,'(/1x,2(A,I4,A,I2,A,I3),2(A,I4),A,I3)')
     &         'NUMBER OF NODES:', NNODES, '(',NP_XI, ' x', NP_ETA,
     &         ') THREADS:',  numthreads,  ' TILING:',
     &          NSUB_X,' x', NSUB_E, ' GRID:', LLm,' x',MMm,' x',N
        endif
# else
        write(*,'(/3(1x,A,I3),4x,2(A,I4),A,I3)')
     &         'NUMBER OF THREADS:',     numthreads,     'TILING:',
     &          NSUB_X,'x',NSUB_E, 'GRID SIZE:', Lm,' x',Mm,' x',N
# endif
      endif
      trd_count=trd_count+1
# ifdef MPI
#  ifndef MPI_SILENT_MODE
      write(*,'(4x,A,I4,1x,A,I3,1x,A,I10,A)') 'Process',  mynode,
     &        'thread', proc(2), '(pid=', proc(1), ') is active.'
#  endif
# else
      write(*,'(8x,A,I3,1x,A,i10,A)') 'Thread #', proc(2),
     &                           '(pid=', proc(1), ') is active.'
# endif
      if (trd_count==numthreads) then
        trd_count=0
C$      mpi_master_only write(*,'(1x,2A/)') 'This code was ',
C$   &               'built using Open MP enabled compiler.'
      endif
C$OMP END CRITICAL (start_timers_cr_rgn)
      end

      subroutine stop_timers()            ! Finalize timings
      implicit none                       ! for all threads.
# include "param.h"
# include "scalars.h"
# include "comm_vars.h"
# ifdef OMP_GET_WTIME
C$    real*8 omp_get_wtime
# endif
# ifdef __IFC
      real*4 etime
# endif
      if (proc(1)/=0) then
        proc(1)=0
# ifdef OMP_GET_WTIME
C$      WallClock = omp_get_wtime() - WallClock
# endif
# ifdef __IFC
        cpu_time(3) = etime(cpu_time) - cpu_time(3)
#else
        call etime(cpu_time,cpu_time(3))
#endif
C$OMP CRITICAL (stop_timers_cr_rgn)
# ifdef MPI
#  ifdef MPI_SILENT_MODE
        if (mynode==0) then
#  endif
          write(*,'(1x,A,I4,2x,A,I3,2(2x,A,F12.2),2x,A,F10.2)')
     &    'Process ', mynode, 'thread', proc(2),  'net:', cpu_time(3),
     &                      'cpu:', cpu_time(1),  'sys:', cpu_time(2)
#  ifdef MPI_SILENT_MODE
        endif
#  endif
# else
        if (trd_count==0) write(*,*)
        write(*,'(13x,A,I3,2(2x,A,F12.2),2x,A,F10.2)')
     &               'thread #', proc(2),  'net:', cpu_time(3),
     &               'cpu:', cpu_time(1),  'sys:', cpu_time(2)
# endif

# ifdef KERNEL_THREADS
        cpu_all(3)=cpu_all(3) +cpu_time(3)
        cpu_all(1)=cpu_all(1) +cpu_time(1)
        cpu_all(2)=cpu_all(2) +cpu_time(2)
# else
        cpu_all(3)=max(cpu_all(3), cpu_time(3))
        cpu_all(1)=max(cpu_all(1), cpu_time(1))
        cpu_all(2)=max(cpu_all(2), cpu_time(2))
# endif
        trd_count=trd_count+1
        if (trd_count==numthreads) then
          trd_count=0
#  ifdef MPI_SILENT_MODE
          if (mynode==0) then
#  endif
C$          write(*,'(24x,A,F12.2,F18.2,F16.2)')        'TOTAL:',
C$   &                        cpu_all(3), cpu_all(1), cpu_all(2)
# ifdef OMP_GET_WTIME
C$          write(*,'(4x,A,F12.2)') 'Wall Clock (elapsed) Time:',
C$   &                                                WallClock
# endif
#  ifdef MPI_SILENT_MODE
          endif
#  endif
        endif
C$OMP END CRITICAL (stop_timers_cr_rgn)
      endif
      end

! The following routine is to catch loss of synchronization in OpenMP
! mode.  Calls to "sync_trap" are not hardcoded into the model, but are
! inserted by "mpc" if the later is directed to do so.  The algorithm
! for trapping is that every thread advances its own private counter
! "priv_count", and then global counter "barr_count" (inside critical
! region) and compares the value of global counter with its private.
! Since each thread increments the global counter by 1, it grows
! numthreads-times faster than the private, hence within each
! synchronization region the global counter, after incremented by 1
! by a thread must have values from "previous"+1 to last
! "previous"+numthreads inclussive (here "previous" means the final
! value after the previous synchronization event.  As the result,
! "itest" computed below must always match "priv_count".

      subroutine sync_trap(ibarr)
      implicit none
      integer ibarr, indx, itest
#include "param.h"
#include "scalars.h"
      indx=1+mod(ibarr-1,16)
      priv_count(indx)=priv_count(indx)+1
C$OMP CRITICAL (trap_cr_rgn)
      barr_count(indx)=barr_count(indx)+1
      itest=1+(barr_count(indx)-1)/numthreads
      if (itest /= priv_count(indx)) then
        write(*,'(A,3I10)') 'sync error', ibarr,
     &        priv_count(indx),  barr_count(indx)
      elseif (mod(priv_count(indx),4001) ==0) then
        write(*,'(A,I12,2(2x,A,I3))') 'barrier count =',
     &              priv_count(indx), 'barr# =', ibarr,
     &                                'trd =',  proc(2)
      endif
C$OMP END CRITICAL (trap_cr_rgn)
      end

#else
                                       ! These are stub-routines for
      subroutine start_timers          ! compatibility with compilers
      implicit none                    ! without OpenMP support.
#include "param.h"
      mpi_master_only write(*,'(/2(1x,A,I3),4x,2(1x,A,I4)/)')
     &                        'BLOCKING:', NSUB_X, 'x', NSUB_E,
     &                        'HORIZ. GRID SIZE:', Lm, 'x', Mm
      end

      subroutine stop_timers()
      end
#endif
