      integer function lenstr (string)
!
! Returns the character position of the last nonblank character in
! input string after removing the leading blank characters, if any.
! Algorithm: Find length of input character string using intrinsic
! function "len", then find positions of  the first "is" and the
! last "ie" nonblank characters. Define the length of the string
! based on these bounds. In the case of there are leading blank
! characters, shift the whole string to the beginning.
!
! WARNING: if there are leading blank characters, user must ensure
! that the string is "writable", i.e., there is a character variable 
! in the calling program which holds the string: otherwise call to
! lenstr results in segmentation fault, i.e. passing directly typed
! argument like
!
!                     lstr=lenstr(' x...')
!
! is not allowed, however
!
!                      lstr=lenstr('x...') 
!
! is OK because lenstr makes no attempt to shift the string.
!
      implicit none                       ! In the code below there
      character string*(*)                ! are two possible outcomes
      integer is,ie                       ! of search for the first
      ie=len(string)                      ! non-blank character "is":
      is=1
      dowhile (string(is:is).eq.' ' .and. is.lt.ie)
        is=is+1
      enddo
      dowhile (string(ie:ie).eq.' ' .and. is.lt.ie)
        ie=ie-1
      enddo                               ! it either finds one, or 
      if (string(is:is).ne.' ') then      ! the search is terminated 
        if (is.gt.1) string=string(is:ie) ! by reaching the condition
        lenstr=ie-is+1                    ! (is.eq.ie), while the
      else                                ! character is still blank, 
        lenstr=0                          ! which  means that the 
      endif                               ! whole string consists of 
      return                              ! blank characters only. 
      end
