! The following code takes one argument, which should contain two
! integer numbers separated by lowercase "x" (e.g., 4x32) decodes it,
! and generates parameter string, like
!
!    parameter (NSUB_X=4,  NSUB_E=16, Lm=LLm, Mm=MMm) 
!
! for the 4x32 example above.  This code is designed to work within 
! an automatic multi-compilation script to test performance of ROMS
! code as function of partitioning. An example of such script is
!
!        #/bin/csh
!        foreach CASE (2x2 2x4 2x8 2x16 3x4 3x8 3x16 3x32 3x38 3x46)
!          cp param.HEAD param.h
!          ncb $CASE >> param.h
!          cat param.TAIL >> param.h
!          make clean ; make ; mv roms A.$CASE
!        end
!
! where file "param.h" is generated from "param.HEAD", which contains
! all lines of "param.h", which normally preceed NSUB_X, NSUB_E
! setting, and "param.TAIL" contains all lines below it.  The use of
! this code facilitates compiling multiple executables, where is is
! not practical to make it by hands. 
! 
      program ncb
      implicit none
      integer iarg,iargc, is,ie, is1,ie1, max_case
      parameter (max_case=16)
      character case*(max_case)
      iarg=iargc()
      if (iarg.gt.0) then
        case='                '
        call getarg (1,case)
        is=1
        do while (case(is:is).eq.' ' .and. is.lt.max_case)
          is=is+1
        enddo
        ie=is
        do while (case(ie:ie).ne.'x' .and. ie.lt.max_case)
          ie=ie+1
        enddo
        is1=ie+1
        ie1=is1
        do while (case(ie1:ie1).ne.' ' .and. ie1.lt.max_case)
          ie1=ie1+1
        enddo
        write(*,'(6x,5A))') 'parameter (NSUB_X=', case(is:ie-1),
     &       ', NSUB_E=', case(is1:ie1-1), ',  Lm=LLm, Mm=MMm)'
      else
        write(*,'(A)') 'ERROR in ncb.F: no argument is given' 
      endif
      stop
      end
