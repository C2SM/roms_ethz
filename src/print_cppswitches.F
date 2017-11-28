#include "cppdefs.h"

      program print_cppswitches

! Analyze content of string "cpps" and print out all filenames
! (substrings enclosed in <..>) and CPP switches activated in these
! files as they were recorded in "cpps" by several other routines,
! starting with "check_switches1".    This routine is merely for
! reporting, and it does not perform any checks, nor does it raise
! the error flag.

      implicit none
      character(len=36) bffr(3)
      integer is,ie, i,nwd, lbffr(3)
      integer :: ierr = 0
#include "strings.h"
      call check_switches1(ierr)
      if (ierr /= 0 ) then
        print*,'Error in calling check_switches1'
      else
        write(*,'(/1x,A/)') 'Activated CPP configuration switches:'
        nwd=0
        is=1
        do while (is<max_opt_size)
          do while (is<max_opt_size .and. cpps(is:is)==' ')
            is=is+1
          enddo
          ie=is
          do while (ie<max_opt_size .and. cpps(ie:ie)/=' ')
            ie=ie+1
          enddo
          if (cpps(ie:ie)==' ') ie=ie-1
          if (cpps(is:is)=='<' .and. cpps(ie:ie)=='>') then
            if (nwd>0) then
              write(*,1) (bffr(i)(1:lbffr(i)), i=1,nwd)
              nwd=0
            endif
            write(*,'(1x,A)') cpps(is:ie)
          else
            nwd=nwd+1
            bffr(nwd)=cpps(is:ie)
            lbffr(nwd)=ie-is+1
            if (nwd==3) then
              write(*,1) (bffr(i)(1:lbffr(i)), i=1,nwd)
              nwd=0
            endif
          endif
          is=ie+1                      ! This format is designed to
        enddo                          ! accommodate three columns at
      endif
      print*
    1 format(T8,A,T40,A,T72,A)       ! most 24-character-wide and fit
      end                            ! it into 80-character width.

