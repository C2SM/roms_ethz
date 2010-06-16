      program cross_matrix
!
! Purpose: Analyze file dependencies and build "Make.depend".
!---------
! Usage:
!--------       cross_matrix list_of_files
! for example
!               cross_matrix *.F
! or
!               cross_matrix *.F *.h
! or
!               cross_matrix $(SRCS)            inside a makefile
!
! Algorithm:
!-----------
! cross_matrix reads all files from the list arguments it is given,
! and searches for the CPP command #include, FORTRAN-style INCLUDE,
! USE and MODULE statements. If any of these found, it looks further 
! and interprets the following word (enclosed or not into "...",
! '...') as the name of include file or F90 module. The new files to
! be open and searched for #include inside it and so on.
!
! cross_matrix investigates chained include statements, for example,
! if file1 contains statement #include "file2", and file2 contains
! statement #include "file3", therefore file3 will be included into
! list of dependencies of "file1"
!
!--> Optionally, files which are not in the list of arguments AND
! which are not in the present working directory can be excluded from
! the list of dependencies (CPP switch EXCLUDE). A warning message
! will be issued about each file excluded from analysis.
!
! #### WARNING: This practice is known to be unsafe because, unlike
! the old IRIX SGI CPP, most Linux C-preprocessors simply ignore
! #include command without a warning if the file cannot be included,
! for example its name is misspelled in the source code being
! analyzed.  If the included file contains variable declarations
! (which occurs in most not all cases) its absence triggers
! subsequent compilation error, so that the mishap is discovered.
! However, a file which contains a purely executable segment of code,
! the miss goes undetected at compiling time resulting in wrong code
! without any visible signal at the compilation time.  
!
! The other option, EXCLUDE_SELECTED, allows explicit specification
! of which files to be ignored in the dependency list. This option
! is safe against misspelled files.
!
c--#define EXCLUDE
#define EXCLUDE_SELECTED
c--#define XCOMM_FORMAT
#define WARN_NONSTANDARD
#define TRAP_DOUBLE_SLASH
c--#define VERBOSE
 
      implicit none
      integer max_name_size, max_string_size, max_names,
     &        max_modules, stdout, iin, iout, test
      parameter (max_name_size=32, max_names=256, max_modules=128,
     &     max_string_size=72, stdout=6, iin=11, iout=12, test=13)
      character*(max_name_size) testword, fname(max_names),
     &                                    mod_name(max_modules)
      character*(max_string_size) string
      character backslash*1,  single_quote*1, double_quote*1
      integer nsize(max_names), mod_src(max_names), nmax, nmods,
     &        size, line, lines_all, cpps_all, empty_all, comm_all,
     &        n_disc, last_arg, iocheck, i,j,k,l,m,n,  is,ie, itype
      logical matrix(max_names,max_names), new_name, not_end_of_file
      common /xxxx/nsize, matrix /yyyy/fname

                    ! Function "iargc" is viewed as intrinsic by
#if defined AIX  || defined PGI      /* ! most modern compilers and does not need to  */
      integer iargc ! be declared. IBM xlf95 is a notable exclusion.
#endif           /* ! So do 7.x and earlier versions of Intel IFC, */
                    ! but 8.x, 9.x IFORT recognize it as intrinsic.
#ifdef EXCLUDE
      integer exc_names,nexc
      parameter (exc_names=16)
      integer exc_size(exc_names)
      character*32 exc_name(exc_names)
#endif
#ifdef EXCLUDE_SELECTED
      integer exc_names                ! Specify list of files to be 
      parameter (exc_names=16)         ! excluded from the dependency
      integer exc_size(exc_names)      ! list. Typically this should
      character*16 exc_name(exc_names) ! be constrained to 3rd-party
      exc_name(1)='mpif.h'             ! files which do not evolve 
      exc_size(1)=6                    ! with the code, but needed as
      exc_name(2)='netcdf.inc'         ! input.
      exc_size(2)=10
#endif
      is=0
      ie=0
 
      last_arg=iargc()
      if (last_arg.eq.0) then
        write(stdout,'(/2(1x,A)/)') 'CROSS_MATRIX ERROR:',
     &                              'no files to process.'
        stop
      elseif (last_arg .gt. max_names) then
        goto 98                               !--> ERROR
      endif
 
      write(stdout,'(/2(1x,A)/)') 'This is CROSS_MATRIX:',
     &            'Creating a new version of Make.depend.'
      single_quote=char(39)
      double_quote=char(34)   ! Reset everything:
      backslash=char(92)      !
      lines_all=0             ! <-- line counter all files altogether
      cpps_all=0
      empty_all=0
      comm_all=0
      n_disc=0                ! <-- counter of newly discovered files
#ifdef EXCLUDE
      nexc=0                  ! <-- counter of excluded files
#endif
      do j=1,max_names        !
        nsize(j)=0            ! <-- array of sizes of filenames
        do i=1,max_name_size  !
          fname(j)(i:i)=' '   ! <-- character array of filenames
        enddo                 !
        do i=1,max_names      !
          matrix(i,j)=.false. ! <-- matrix of dependencies:
        enddo                 !
      enddo                   ! Note: matrix(i,j).eq..true. means
                              ! that file j depends on i.
!
! The following block creates the list of given names and their
! sizes without actually opening the files.
!
#ifdef VERBOSE
      write(stdout,'(1x,A)') 'Initial list:'
#endif
      do n=1,last_arg
        call getarg(n,fname(n))
        i=1
        do while (fname(n)(i:i).eq.' ' .and. i.lt.max_name_size)
          i=i+1
        enddo
        j=i
        k=j+1
        do while (fname(n)(k:k).ne.' ' .and. k.lt.max_name_size)
          j=k
          k=j+1
        enddo
        if (i.eq.1) then
          nsize(n)=j
        else
          nsize(n)=j-i+1
          fname(n)(1:j-i+1)=fname(n)(i:j)
        endif
#ifdef VERBOSE
        write(stdout,'(4I3,1x,3A)') n,i,j, nsize(n),
     &            'fname = ''', fname(n)(i:j), '''.'
#endif
      enddo
#ifdef VERBOSE
      write(stdout,*)
#endif
      nmax=last_arg
      nmods=0
!
! Create Make.depend file and write upper portion of the header.
! after which start processing of actual files.
!
      open(unit=iout, file='Make.depend', form='formatted')
      write(iout,'(3(A,1x,A/),A/A/A/A/A/A)')
     &         '# Make.depend: list of dependencies generated by',
     &         'cross_matrix.', '# WARNING: THIS IS A MACHINE',
     &         'GENERATED FILE: DO NOT EDIT !!!!!', '# To create',
     &         'or update this file use commands:', '#',
     &         '#        cross_matrix *.F',         '# or',
     &         '#        cross_matrix *.F *.h',     '# or',
     &         '#        make depend'


!
!      *****    *********    ******   *******    *********
!    ***   ***  *  ***  *   *   ***   ***   ***  *  ***  *
!    ***           ***     **   ***   ***   ***     ***
!      *****       ***    ***   ***   ***   **      ***
!          ***     ***    *********   ******        ***
!    ***   ***     ***    ***   ***   ***  **       ***
!      *****       ***    ***   ***   ***   ***     ***
!
! Reading of the input files starts here. 
!       
      n=0
  1    n=n+1
        line=0
        not_end_of_file=.true.
#ifdef VERBOSE
        write(stdout,'(1x,3A)') 'Analyzing ''', fname(n)(1:nsize(n)),
     &                                                       '''...'
#endif
        open(unit=iin, file=fname(n)(1:nsize(n)), form='formatted',
     &                                          status='old', err=7)
  2     string(1:1)=' '
         read(iin,'(A)',iostat=iocheck,end=3) string
         line=line+1
         goto 4
  3      not_end_of_file=.false.
!
! Reset "itype" and determine position of the staring and ending
! position non-blank characters "is:ie" for current string. These
! will remain unchanged during the rest of processing.
!
!
  4      if (iocheck.eq.0 ) then
           itype=0
           ie=max_string_size 
           do while (ie.gt.1 .and. string(ie:ie).eq.' ')
             ie=ie-1
           enddo
           if (string(ie:ie).eq.' ') ie=ie-1
           is=1
           do while (string(is:is).eq.' ' .and. is.lt.ie)
             is=is+1
           enddo

!
! The following code segment discriminates lines into several
! categories (empty lines, F77- and F90-style comments, CPP-lines
! starting with # and regular FORTRAN statements). Once it is
! determined that the line is not empty, it checks whether the first
! word is "include" (with or without # in front), "use", or "module". 
! If so, it sets flag "itype" to non-zero value to signal that the
! line needs further processing.  Since unlike CPP-style #include,
! FORTRAN include/use/module can appear both in upper- and lower-case
! letters, copy the first word of string into "testword", while 
! performing folding to uppercase. Additionally, this section of the
! code also increment line counters for various type of lines, which
! are just used for statistical purposes only do not affect matrix
! of dependencies
!
           if (ie.eq.0) then
             empty_all=empty_all+1
           elseif (is.eq.ie .and. string(is:is).eq.'!') then
               empty_all=empty_all+1
           elseif (is.eq.1 .and. ie.eq.1 .and. (string(1:1).eq.'c'
     &                              .or. string(1:1).eq.'C')) then
             empty_all=empty_all+1
           elseif (string(is:is).eq.'!') then
             comm_all=comm_all+1
           elseif (i.eq.1 .and. (string(1:1).eq.'c' .or.
     &                           string(1:1).eq.'C')) then
             comm_all=comm_all+1
           elseif (string(1:1).eq.'#') then
             cpps_all=cpps_all+1
             i=2
             do while (string(i:i).eq.' ' .and. i.lt.ie)
               i=i+1
             enddo
             j=i
             do while (string(j:j).ne.' ' .and. j.lt.ie)
               j=j+1
             enddo
             if (string(j:j).eq.' ') j=j-1
             m=j-i+1
             if (m.eq.7 .and. string(i:j).eq.'include') then
               itype=1
             endif
           else
             lines_all=lines_all+1
             i=is
             j=is
             do while (string(j:j).ne.' ' .and. j.lt.ie)
               j=j+1
             enddo
             if (string(j:j).eq.' ') j=j-1
             if (j-i+1 .lt.8) then
               do l=i,j
                 m=l-i+1
                 if (string(l:l).ge.'a' .and.string(l:l).le.'z') then
                   testword(m:m)=char(ichar(string(l:l))-32)
                 else
                   testword(m+1:m)=string(l:l)
                 endif
               enddo
               m=j-i+1
               if (m.eq.7 .and. testword(1:m).eq.'INCLUDE') then
                 itype=2
               elseif (m.eq.3  .and.  testword(1:m).eq.'USE') then
                 itype=3
               elseif (m.eq.6 .and. testword(1:m).eq.'MODULE') then
                 itype=4
               endif
             endif
           endif
!
! Once it is determined that the line begins with "[#]include",
! "use", or "module" statement, find bounds and read the second
! word, which is either name of include file or module. Copy it 
! into "testword". Strip out double/single quotes, and append with
! suffix ".mod" if necessary.
!
           if (itype.ne.0) then
             i=j+1
             do while (string(i:i).eq.' ' .and. i.lt.ie)
               i=i+1
             enddo
             j=i
             do while (string(j:j).ne.' ' .and. j.lt.ie)
               j=j+1
             enddo
             if (string(j:j).eq.' ') j=j-1
             if (string(i:i).eq.single_quote .and.
     &           string(j:j).eq.single_quote) then
               i=i+1
               j=j-1
             endif
             if (string(i:i).eq.double_quote .and.
     &           string(j:j).eq.double_quote) then
               i=i+1
               j=j-1
             endif
             size=j-i+1

             if (size.gt.0) then
               testword=string(i:j)
               if (itype.eq.3 .or. itype.eq.4) then
                 testword(size+1:size+4)='.mod'
                 size=size+4
               endif

c**            if (itype.eq.1) then
c**              write(*,*) 'CPP #include discovered ',
c**     &                                      testword(1:size)
c**            elseif (itype.eq.2) then
c**              write(*,*) 'FORTRAN-style include discovered ',
c**     &                                      testword(1:size) 
c**            elseif (itype.eq.3) then
c**              write(*,*) 'F90 use statement discovered ',
c**     &                                      testword(1:size) 
c**            elseif (itype.eq.4) then
c**              write(*,*) 'F90 module statement discovered ',
c**     &                                      testword(1:size) 
c**            endif



!
! At this moment "testword" contains name of an include file or
! module. Check whether this name already appeared in the list of
! files, and if not add it into the list. In both cases, "new" or
! "old", set element of dependency matrix. Also note that F90-style
! module statement, itype=4, results in REVERSE dependency, since
! it is the resultant .mod file depends on the original source, and
! not other way around, like in the case of include or use.
!
               new_name=.true.
#ifdef EXCLUDE_SELECTED
               do k=1,exc_names 
                 if (size .eq. exc_size(k)) then
                   if (exc_name(k)(1:exc_size(k)) .eq.
     &                         testword(1:size) ) then
                     new_name=.false.
# ifdef VERBOSE
                     write(stdout,'(8x,5A)')   'Excluded selected ',
     &                      'file ''', testword(1:size), ''' from ',
     &                                       'the dependency list.'
# endif
                   endif
                 endif
               enddo
#endif
               if (new_name) then
                 do k=1,nmax
                   if (size .eq. nsize(k)) then
                     if (testword(1:size) .eq. fname(k)(1:size)) then
                       new_name=.false.
                       if (itype.lt.4) then
                         matrix(k,n)=.true.
                       else
                         matrix(n,k)=.true.
                         nmods=nmods+1
                         mod_name(nmods)=testword(1:size)
                         mod_src(nmods)=n
                       endif
                     endif
                   endif
                 enddo
                 if (new_name) then
                   n_disc=n_disc+1
#ifdef EXCLUDE
                   if (itype.eq.1 .or. itype.eq.2) then
                     open(unit=test, file=testword(1:size),
     &                              status='old', err=5)
                     close(unit=test)
                   endif
#endif
                   nmax=nmax+1
                   if (nmax.gt.max_names) goto 98  !--> ERROR
                   nsize(nmax)=size
                   fname(nmax)(1:size)=testword(1:size)
                   if (itype.lt.4) then
                     matrix(nmax,n)=.true.
                   else
                     matrix(n,nmax)=.true.
                     nmods=nmods+1
                     mod_name(nmods)=testword(1:size)
                     mod_src(nmods)=n
                   endif
#ifdef VERBOSE
                   write(stdout,'(4x,3A)') 'Found new file ''',
     &                           fname(nmax)(1:nsize(nmax)), '''.'
#endif
#ifdef EXCLUDE
                   goto 6
   5               do k=1,nexc
                     if (size .eq. exc_size(k)) then
                      if (testword(1:size) .eq. exc_name(k)(1:size))
     &                                              new_name=.false.
                     endif
                   enddo
                   if (new_name) then
                     nexc=nexc+1
                     if (nexc.gt.exc_names) goto 99 !--> ERROR
                     exc_size(nexc)=size
                     exc_name(nexc)=testword(1:size)
# ifdef VERBOSE
                     write(stdout,'(I4,1x,3A)')   nexc, 'Exclude ''',
     &                       exc_name(nexc)(1:exc_size(nexc)), '''.'
# endif
                     write(iout,'(A/A,2x,A/2A)') '#',
     &                              '# WARNING: File is not found:',
     &                              exc_name(nexc)(1:exc_size(nexc)),
     &                              '# This file is excluded from ',
     &                               'the dependency list.'
                   endif
#endif
                 endif  !<-- new name
               endif  !<-- new name
             endif  ! <-- size .gt.0
           endif  ! <-- itype.ne.0 
         endif ! <-- iocheck.eq.0
#ifdef TRAP_DOUBLE_SLASH
         do i=is,ie-1
           if (string(i:i).eq.'/' .and. string(i+1:i+1).eq.'/') then
             write(*,'(A/2A,I5,1x,3A/A/5(2A/),A)') '*****',
     &        '***** WARNING!!! Double slash /',        '/ on line',
     &         line,    'in file ''',  fname(n)(1:nsize(n)),  '''.',
     &        '*****',  '***** Although it is legal in FORTRAN as ',
     &        'string concatenation operator, double',
     &        '***** slash is interpreted as C++ comment line ',
     &        'by GCC C-preprocessor leading to',
     &        '***** possible misinterpretation of Fortran code. ',
     &        'It shall be therefore policy',
     &        '***** of this code to eliminate all double slashes. ',
     &        'Use at least one blank ',
     &        '***** space between slashes in Fortran ',
     &        'concatenation operator.', '*****'
           endif
         enddo
#endif
#ifdef WARN_NONSTANDARD
         do i=is,ie
           k=ichar(string(i:i))
           if (string(i:i).eq.' ') then

           elseif (k.eq.9) then
             write(*,'(/1x,4A,I5,1x,A,I3,A/)') '***** WARNING!!! ',
     &         'file ', fname(n)(1:nsize(n)),  ' line',  line, ':',
     &          i, '  contains TAB character.'
           elseif (k.lt.33 .or. k.gt.126) then
             write(*,'(/1x,4A,I5,1x,A,I3,1x,A,I4,1X,2A/)') '***** ',
     &         'WARNING!!! file ',  fname(n)(1:nsize(n)),  ' line',
     &          line,':',i, 'nonstandard symbol:',k, string(i:i),'.'
           endif 
         enddo
#endif
#ifdef EXCLUDE
   6   continue
#endif
       if (not_end_of_file) goto 2
       close (unit=iin)
   7   if (n.lt.nmax) goto 1
!
!
!          *******       ****       *****    *********
!          ***   ***   ***  ***   ***   ***  *  ***  *
!          ***   ***  ***    ***  ***           ***   
!          ***   ***  ***    ***    ******      ***
!          *******    ***    ***         ***    ***
!          ***         ***  ***    ***   ***    ***
!          ***           ****        ****       ***
!
! Post-processing...
!                                  ! Suppress circular dependencies
      do m=1,nmods                 ! associated with the F90-module
        j=mod_src(m)               ! being introduced by and used
        do i=1,nmax                ! within the same file. In this
          matrix(i,j)=.false.      ! case .mod file is created
        enddo                      ! automatically at the compilation
      enddo                        ! without the need of pre-existing
                                   ! .mod file.

      do i=1,nmax                  ! Suppress main diagonal: this
        matrix(i,i)=.false.        ! is needed to avoid duplication
      enddo                        ! of file name in the case of
                                   ! self-inslusion.
 
      i=0                          ! Investigate possible secondary
      j=1                          ! dependencies. This is equivalent
      do while(j.gt.i)             ! to logical addition:
        i=j                        !
        do n=1,nmax                ! if
          do k=1,nmax              !      file1 depends on file2 
            if (matrix(k,n)) then  ! and
              do j=1,nmax          !      file2 depends on file3 
                if (matrix(j,k))   ! then 
     &         matrix(j,n)=.true.  !      file1 depends on file3 
              enddo                !
            endif                  ! This is an iterative procedure,
          enddo                    ! since staged include statements
        enddo                      ! possible. The number of .true.
        j=0                        ! elements in the matrix grows 
        do n=1,nmax                ! when new dependencies are 
          do k=1,nmax              ! discovered. The procedure 
            if (matrix(k,n)) j=j+1 ! repeats itself until the next
          enddo                    ! iteration does not discover
        enddo                      ! any new dependencies.
      enddo

#ifdef VERBOSE
      if (nmax.le.max_string_size) then
        write(*,'(/A)') 'Printing dependence matrix:'
        j=1
        do i=1,nmax
          if (j.gt.9) j=0
          string(i:i)=char(j+48)
          j=j+1
        enddo 
        write(*,'(32x,A)') string(1:nmax)
        do j=1,nmax
          do i=1,nmax
            if (matrix(i,j)) then
              if (i.eq.j) then
                string(i:i)='W'
               else
                string(i:i)='X'
              endif
            else
              string(i:i)='.'
            endif
          enddo 
          write(*,'(A,I3,1x,A)') fname(j)(1:28), j, string(1:nmax) 
        enddo
      else
        write(*,'(/1x,2A/)') 'Dependence matrix cannot be ',
     &             'printed because nmax > max_string_size.'
      endif
#endif
!
! Report statistics for all files:
!
      write(iout,'(A/2A,13x,I4/A/A,26x,I4)') '#', '# Number of ',
     &     'files given for dependency analysis:', last_arg, '#',
     &     '# Number of newly discovered files:',  n_disc
#ifdef EXCLUDE
      write(iout,'(A/2A,9x,I4)') '#', '# Number of files ',
     &           'excluded from dependency analysis:', nexc
#endif
      write(iout,'(A,13x,A/A,13x,A,9x,I4/A/A,6x,I6/2A/A/2A,5x,I6/A)')
     &  '#',   '---------------------------------------------------',
     &  '#',   'total files analyzed for dependencies:',  nmax,  '#',
     &  '# Total number of lines of FORTRAN code in all files:',
     &   lines_all, '# (excluding CPP-commands, empty, and comment ',
     &  'lines)',  '#',  '# Total number of CPP-preprocessor lines ',
     &  'in all files:',   cpps_all, '#'
      write(iout,'(A,14x,I6/A/A/A,16x,I6/A/A,38x,A/A,38x,A,7x,I7/A)')
     &  '# Total number of comment lines in all files:',   comm_all,
     &  '# (excluding empty and containing only "c", "!" and "C")',
     &  '#', '# Total number of empty lines in all files:',empty_all,
     &  '# (including lines containing  only "c", "!" and "C")', '#',
     &             '--------------------------', '#', 'total lines:',
     &                    lines_all+cpps_all+comm_all+empty_all, '#'
!
! Write dependency lines associated with F90 modules. This is
! done separately because of the special treatment: since .mod file
! is created after compilation of source file with module statement,
! the suffix of the source file name (usually .F or .f) needs to be
! changed into .o to initiate compilation of source with module
! statement first.
!
      do m=1,nmods
        i=1
        do while (mod_name(m)(i:i).ne.' ' .and. i.lt.max_name_size)
          i=i+1
        enddo
        if (mod_name(m)(i:i).eq.' ') i=i-1
        string=fname(mod_src(m))
        j=1
        do while (string(j:j).ne.' ' .and. j.lt.max_name_size)
          j=j+1
        enddo
        if (string(j:j).eq.' ') j=j-1
        if (string(j-1:j).eq.'.F') string(j:j)='o'   ! Special 
        if (string(j-1:j).eq.'.f') string(j:j)='o'   ! treatment:
        if (string(j-3:j).eq.'.f90') then            ! .f --> .o
          j=j-2
          string(j:j)='o'
        endif
        write(iout,'(2A,1x,A/)') mod_name(m)(1:i), ':', string(1:j)
      enddo


!
! Generate list of dependencies. Two styles are supported, both
! work just fine. If XCOMM_FORMAT is defined, the style of the list
! is similar to that of makefiles generated by imake utility
! recommended by $XConsortium. In this case the target file name
! and the column separator ':' are repeated in all lines which
! correspond to the target dependencies, and its name is also
! repeated one time more wit no characters after ':'.
! If XCOMM_FORMAT is NOT defined, the target file name and the
! column separator ':' are not repeated, while backslash symbol is
! used in the end of each line, if the line needs to be  continued
! on the next line.
!

      do n=1,last_arg !!!  nmax
        if (nsize(n).gt.0) then
#ifdef XCOMM_FORMAT
          write(iout,'(A1)') '#'
          k=0
  11      i=nsize(n)
           string(1:i)=fname(n)(1:i)
           if (string(i-1:i).eq.'.F') then
             if (k.eq.0) write(iout,'(2A,1x,2A)') string(1:i-1),'o:',
     &                                            string(1:i-1),'f'
             string(i:i)='f'
           endif
           i=i+1
           string(i:i)=':'
  12       i=i+1
            string(i:i)=' '
            if (k.eq.0) then
              if (fname(n)(nsize(n)-1:nsize(n)).eq.'.F') then
                string(i+1:i+nsize(n))=fname(n)(1:nsize(n))
                i=i+nsize(n)+1
                string(i:i)=' '
              endif
            endif
  13        k=k+1
            if (matrix(k,n)) then
              if (i+nsize(k).lt.max_string_size) then
                string(i+1:i+nsize(k))=fname(k)(1:nsize(k))
                i=i+nsize(k)
                goto 12
              else
                write(iout,'(A)') string(1:i)
                goto 11
              endif
            elseif (k.lt.nmax) then
              goto 13
            else
              write(iout,'(A)') string(1:i)
              if (i.gt.nsize(n)+2) then
                write(iout,'(A)') string(1:nsize(n)+1)
              endif
            endif
#else
          if (n.eq.1 .or. j.gt.0) write(iout,*)
          j=0
          i=nsize(n)
          string(1:i)=fname(n)(1:i)
          if (fname(n)(nsize(n)-1:nsize(n)).eq.'.F') then
            write(iout,'(2A,1x,2A/)') fname(n)(1:i-1), 'o:',
     &                                fname(n)(1:i-1), 'f'
            string(i:i)='f'
            i=i+1
            string(i:i)=':'
            j=1
            i=i+1
            string(i:i)=' '
            string(i+1:i+nsize(n))=fname(n)(1:nsize(n))
            i=i+nsize(n)
          else                       ! Do not print dependency line,
            i=i+1                    ! if the file does not depend on
            string(i:i)=':'          ! anything, except itself. To
          endif                      ! detect trivial dependencies,
          k=0                        ! set j=0, then set it to j=1,
  14       k=k+1                     ! when dependency is detected.
           if (matrix(k,n)) then     ! Do not print, if j remains 0.
             j=1
  15         if (i+nsize(k) .lt. max_string_size-2) then
               i=i+1
               string(i:i)=' '
               string(i+1:i+nsize(k))=fname(k)(1:nsize(k))
               i=i+nsize(k)
             else
               i=i+1
               string(i:i)=backslash
               write(iout,'(A)') string(1:i)
               i=0
               goto 15
             endif
           endif
           if (k.lt.nmax) goto 14
           if (j.gt.0) write(iout,'(A)') string(1:i)
#endif
        endif
      enddo
      goto 100
 
  98  write(stdout,'(/2(1x,A)/21x,A/)') 'CROSS_MATRIX ERROR:',
     &          'Too many files to process. To fix: increase',
     &   'parameter ''max_names'' in file ''cross_matrix.F''.'
      goto 100
#ifdef EXCLUDE
  99  write(stdout,'(/2(1x,A)/21x,A/)') 'CROSS_MATRIX ERROR:',
     &          'Too many files to process. To fix: increase',
     &   'parameter ''exc_names'' in file ''cross_matrix.F''.'
#endif
 100  close (iout)
      stop
      end
 
