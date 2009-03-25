      program cppcheck
!
! PURPOSE: Scan all existing CPP-switches in file "cppdefs.h" and
! automatomatically generate file check_switches1.F which contains
! subroutine check_switches1. When later this file is compiled and
! executed a part of SCRUM/ROMS model, it creates log of activated
! CPP-switches.
!
! Algorithm: this program reads line-by-line file "cppdefs.h" and
! creates catalog of CPP-switches found there. It does not matter,
! whether switches are in defined or undefined status, either way
! they are put into catalog. For the purpose of this algorithm
! CPP-switch (CPP-macro name) is a word which follows a command
! (reserved word) of C-preprocessor, such as "ifdef", "define" etc.
! Conversely, a word which follows another word which is not a
! CPP-command is not considered as a CPP-macro name.
!
! For the purpopse of preceeding paragraph "word" means a consecutive
! string of nonblank and nonspecial characters (i.e. letters, digits
! and ! underscore '_').
!
! The algorithm works as follows:
!---- --------- ----- -- --------
! 0. reset catalog: arrays of names of CPP-switches and their sizes;
! 1. read line from the file; ignore all lines which do not have #;
! 2. find non-trivial length of that line;
! 3. set all symbols within C-style comments to blank (' ');
! 4. set all special characters to blank;
! 5. identify words within the modified line;
! 6. identify words which are CPP commands and CPP-macro names.
! 7. for each CPP-macro name encountered in (6) check whether it
!     already listed in the catalog, and if not, place it there.
! 8. Once catalog is complete, generate code for checkdefs.
!
! Created by Alexander Shchepetkin <alex@atmos.ucla.edu> on May 2000.
!
      implicit none
      integer input,iout, maxstring, lstring
      parameter (input=11, iout=12, maxstring=80)
      character string*(maxstring)
      integer nwords, ifile,nfiles     , fswtch(8), nswtchs,nexample
     &                                  , max_switches
      parameter (nwords=16              , max_switches=1024)
      integer istart(nwords), is        , size(max_switches)
     &       ,  Iend(nwords), ie,ln     , line(max_switches)
      logical macro(nwords)             , example
      character fname(4)*32
      character switch(max_switches)*32
      logical status, state(max_switches)
      integer count, iocheck, i,k,n
      logical end_of_file, comment, word, command, new

      integer nstdpth 



 
      write(*,'(/1x,A,1x,A/)') 'This is CPPCHECK: Creating',
     &                   'new version of check_switches1.F.'
      example=.true.
      nexample=0                        ! Initialize catalog: reset/
      nswtchs=0                         ! blank out arrays of sizes
      fswtch(1)=1
      do i=1,max_switches               ! and names.
        size(i)=0
        switch(i)='                                '
      enddo

      nfiles=1
      fname(1)='cppdefs.h'
      fname(2)='set_global_definitions.h'
      fname(3)='compute_horiz_tracer_fluxes.h'

      do ifile=1,nfiles
        open(input,file=fname(ifile),status='old',form='formatted')
 
        count=0     ! <-- reset counter of lines within the file.
        end_of_file=.false.
        nstdpth=0
   1     count=count+1                  ! Read line from input file.
          do i=1,maxstring              ! Ignore all lines, which do
           string(i:i)=' '              ! not start with #.
          enddo
          read(input,'(A)',iostat=iocheck,end=2) string
          if (string(1:1).ne.'#') goto 1
          goto 3                        ! Find length of the string,
   2      end_of_file=.true.            ! which is equal to position
                                        ! of the most right nonblank
   3      lstring=maxstring             ! character.
          do while(string(lstring:lstring).eq.' ' .and. lstring.gt.1)
            lstring=lstring-1
          enddo
 
!
! Suppress C-style comments and special characters.
!
          n=0     ! <-- reset counter of comments within the string.
          comment=.false.
          i=1
          do while (i.lt.lstring)
            if (.not.comment .and. string(i:i+1).eq.'/*') then
              comment=.true.
              n=n+1
              istart(n)=i
            elseif (comment  .and. string(i:i+1).eq.'*/') then
              comment=.false.
              Iend(n)=i+1
            endif
            i=i+1
          enddo

          if (comment) then             ! If string ends as an open
            lstring=istart(n)-1         ! comment, restrict lstring
            n=n-1                       ! and disregard all symbols
          endif                         ! one right right from it.
          do k=1,n                      !
            do i=istart(k),Iend(k)      !
              string(i:i)=' '           ! blank out C-style comments
            enddo                       !
          enddo                         ! Suppress special characters
          do i=1,lstring
c*          if (string(i:i).eq.'(' .or. string(i:i).eq.')' .or.
c*   &        string(i:i).eq.'&' .or. string(i:i).eq.'|'  .or.
c*   &        string(i:i).eq.'!' .or. ichar(string(i:i)).eq.9)
c*   &      string(i:i)=' '
                                        ! Character 9 is TaB symbol.
            k=ichar(string(i:i))
            if (k.lt.48 .or. (k.gt.57.and.k.lt.65) .or. (k.gt.90.and.
     &           k.lt.95) .or. k.eq.96 .or. k.gt.122) string(i:i)=' '
          enddo
!
! Identify words within the string, find starting and ending
! characters of each word. Since all special characters have
! been removed, at this point word is a sequence of non-blank
! characters.
!
          n=0       ! <-- reset counter of words within the string.
          word=.false.
          i=1
   6       i=i+1
            if (string(i:i).ne.' ' .and. .not.word) then
              word=.true.
              n=n+1
              istart(n)=i
            elseif (string(i:i).eq.' ' .and.  word) then
              word=.false.
              Iend(n)=i-1
            endif
           if (i.lt.lstring) goto 6
          if (word) Iend(n)=i
 
c**     write(*,'(/,I4,I4,/)') count, n ! Print out words.
c**     do k=1,n
c**       write(*,'(10x,80A1)') (string(i:i), i=istart(k),Iend(k))
c**     enddo
                                        ! Identify CPP-commands (i.e.
          command=.false.               ! reserved words) and CPP-
          do k=1,n                      ! macro names among the words
            macro(k)=.false.            ! of the line. Cancel example
            is=istart(k)                ! switch when encounter first
            ie=Iend(k)                  ! conditional CPP-command.
            ln=ie-is+1
            status=.false.
 
            if (ln.eq.6 .and. string(is:ie).eq.'define') then
              command=.true.
              status=.true.
            elseif (ln.eq.5 .and. string(is:ie).eq.'undef') then
              command=.true.
            elseif (ln.eq.2 .and. string(is:ie).eq.'if') then
              command=.true.
              example=.false.
              nstdpth=nstdpth+1
            elseif (ln.eq.5 .and. string(is:ie).eq.'ifdef') then
              command=.true.
              example=.false.
              nstdpth=nstdpth+1
            elseif (ln.eq.7 .and. string(is:ie).eq.'defined') then
              command=.true.
              example=.false.
            elseif (ln.eq.4 .and. string(is:ie).eq.'elif') then
              command=.true.
              example=.false.
            elseif (ln.eq.4 .and. string(is:ie).eq.'else') then
 
            elseif (ln.eq.5 .and. string(is:ie).eq.'endif') then
              nstdpth=nstdpth-1
 
            elseif (ln.eq.7 .and. string(is:ie).eq.'include') then
 
            elseif (command) then
              command=.false.
              macro(k)=.true.
c**         elseif (string(istart(1):Iend(1)) .ne. 'include') then
c**          write (*,'(6x,A,1x,A,1x,I4,A1/8x,A)') 'CPPCHECK ERROR:',
c**  &       'Unknown CPP-command on line', count, ':', string(is:ie)
            endif
          enddo
c**       write(*,'(/2I4/)') count,n ! Print out CPP-macro names.
c**       do k=1,n
c**         if (macro(k)) then
c**           write(*,'(10x,80A1)') (string(i:i),i=istart(k),Iend(k))
c**         endif
c**       enddo
                                        ! Scan catalog of previously
          do k=1,n                      ! discovered switches to find 
            if (macro(k)) then          ! match with CPP-macro names 
              is=istart(k)              ! found in the present line. 
              ie=Iend(k)                ! If no match is found, add 
              ln=ie-is+1
              if (ifile.gt.1 .and. nstdpth.gt.0) then
                new=.false.
              else                       ! the new switch to the
                new=.true.               ! catalog. 
                do i=fswtch(ifile),nswtchs
                  if (ln.eq.size(i)) then
                   if (string(is:ie).eq.switch(i)(1:ln)) new=.false.
                  endif
                enddo
              endif
              if (new) then
                nswtchs=nswtchs+1
                size(nswtchs)=ln
                switch(nswtchs)(1:ln)=string(is:ie)
                state(nswtchs)=status
                line(nswtchs)=count
                if (example) nexample=nexample+1
              endif                       !
            endif                         ! CPP-switches found prior
          enddo                           ! to the first conditional
         if (.not.end_of_file) goto 1     ! CPP-command correspond to
        close(unit=input)                 ! predefined examples.

        fswtch(ifile+1)=nswtchs+1
      enddo  !<-- ifile
 
      write(*,'(/A,I4)')  'nswtchs =', nswtchs  ! Print out catalog.
      do ifile=1,nfiles
        write(*,'(/A)') fname(ifile) 
        do i=fswtch(ifile),nswtchs
          ln=size(i)
          write(*,'(10x,I4,I4,2x,A)') line(i), ln, switch(i)(1:ln)
        enddo
      enddo
!
! Generate CPP-checking subroutine.
!
      open (unit=iout,file='check_switches1.F',form='formatted')
      write(iout,'(A/)')  '#include "cppdefs.h"'
      write(iout,'(/6x,A/)') 'subroutine check_switches1 (ierr)'
      write(iout,'(4(A,1x,A/),A,14x,A/A,1x,A,3x,A/A,14x,A/A,22x,A)')
     &  '!!!!!! WARNING: THIS IS A MACHINE GENERATED',
     &                                   'CODE, DO NOT EDIT! !!!!!!',
     &  '!!!!!! This file needs to be updated only if',
     &                                    'new CPP-switches  !!!!!!',
     &  '!!!!!! were introduced into "cppdefs.h".',
     &                                ' NO ACTION IS NEEDED  !!!!!!',
     &  '!!!!!! if changes in "cppdefs.h" are limited',
     &                                    'to activation or  !!!!!!',
     &  '!!!!!! deactivation of previously known switches.','!!!!!!',
     &  '!!!!!! To refresh this file compile and execute',
     &                                      '"cppcheck.F"', '!!!!!!',
     &  '!!!!!! as an independent program, or use commands','!!!!!!',
     &  '!!!!!! "make checkdefs" or "make depend".',        '!!!!!!'
      write(iout,'(A,20x,I3,1x,A/A,23x,I3,1x,A)')
     &  '!!!!!! Number of Configuration Choices:',nexample, '!!!!!!',
     &  '!!!!!! Total number of CPP-switches:', nswtchs,  '!!!!!!'
      write(iout,'(2(/6x,A), 5(/A) /6x,A /5x,A1,6x,A, 5(/6x,A))')
     &  'implicit none',        'integer ierr, is,ie, iexample',
     &  '#include "param.h"',   '#include "strings.h"',
     &  '#ifdef MPI',           '# include "scalars.h"',    '#endif',
     &  'MPI_master_only write(stdout,''(/1x,A/)'')',       '&',
     &  '''Activated C-preprocessing Options:''',
     &  'do is=1,max_opt_size', '  cpps(is:is)='' ''',  'enddo',
     &                                    'iexample=0',  'is=1'


      do ifile=1,nfiles
        write(iout,'(A/A,1x,A/A)') '!', '!', fname(ifile), '!'

        ln=32
        do while(fname(ifile)(ln:ln).eq.' ')
          ln=ln-1 
        enddo
        write(iout,'(6x,A,1x,3A)')
     &         'MPI_master_only write(stdout,''(1x,3A)'')',
     &                     '''<', fname(ifile)(1:ln), '>'''

        write(iout,'(6x,A,I2/6x,A/6x,3A/6x,A/6x,A)')
     &       'ie=is +', ln+1, 'if (ie.ge.max_opt_size) goto 99',
     &       'cpps(is:ie)=''<', fname(ifile)(1:ln), '>''',
     &       'cpps(ie+1:ie+1)='' ''', 'is=ie+2'




        do i=fswtch(ifile),fswtch(ifile+1)-1 
          ln=size(i)

          if (ifile.eq.1) then
            write(iout,'(A,1x,A)') '#ifdef', switch(i)(1:ln)
          endif

          if (ifile.eq.1 .or. state(i)) then

            if (i.le.nexample) then
              write(iout,'(6x,A)') 'iexample=iexample+1'
            endif
            write(iout,'(6x,A,1x,3A)')
     &         'MPI_master_only write(stdout,''(10x,A)'')',
     &                       '''', switch(i)(1:ln), ''''
            write(iout,'(6x,A,I2/6x,A/6x,3A/6x,A/6x,A)')
     &         'ie=is +', ln-1, 'if (ie.ge.max_opt_size) goto 99',
     &         'cpps(is:ie)=''',  switch(i)(1:ln),  '''',
     &         'cpps(ie+1:ie+1)='' ''',  'is=ie+2'
          endif

          if (ifile.eq.1) then
            write(iout,'(A)')  '#endif'
          endif
        enddo
      enddo

      write(iout,'(6x,A/8x,A/5x,A1,1x,A/8x,A/6x,A)')
     &     'if (iexample.eq.0) then',
     &     'MPI_master_only write(stdout,''(/1x,A)'')', '&',
     &   '''ERROR in "cppdefs.h": no configuration is specified.''',
     &     'ierr=ierr+1',  'elseif (iexample.gt.1) then'
      write(iout,'(8x,A/5x,A1,2x,A/5x,A1,14x,A/8x,A/6x,A/6x,A)')
     &     'MPI_master_only write(stdout,''(/1x,A/12x,A)'')',
     &     '&', '''*** ERROR: More than one main configurational '',',
     &     '&',            '''switch is defined in "cppdefs.h".''',
     &     'ierr=ierr+1', 'endif', 'return'
      write(iout,'(2x,A/5x,A1,2x,A,1x,A/5x,A1,2x,A,1x,A)')
     &   '99  MPI_master_only write(stdout,''(/1x,A,A/14x,A)'')',
     &   '&', '''CHECKDEFS -- ERROR: Unsufficient size of string',
     &     'cpps'',', '&', '''in file "strings.h".'',',
     &                   '''Increase the size it and recompile.'''
      write(iout,'(6x,A,2(/6x,A))') 'ierr=ierr+1', 'return', 'end'
      close(unit=iout)
      stop
      end
 
