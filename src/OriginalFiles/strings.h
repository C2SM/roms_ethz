! Two long character strings to hold names of activated cpp-switches
! and list of soarse-code file names. Basically it is used to keep
! track of cpp-switches/sourse-names by placing them together and
! writing and global attribute into history file.
!
      integer max_opt_size
      parameter (max_opt_size=2048)
      character*(max_opt_size) cpps, srcs, kwds
      common /strings/ cpps, srcs, kwds
 
