      implicit none


      integer max_name_size, max_string_size, max_names,
     &        max_modules, stdout, iin, iout, test
      parameter (max_name_size=32, max_names=256, max_modules=128,
     &     max_string_size=72, stdout=6, iin=11, iout=12, test=13)
      character*(max_name_size) testword, fname

      character*(max_string_size) string
      character backslash*1,  single_quote*1, double_quote*1
      integer nsize(max_names), mod_src(max_names), 
     &     size, line, lines_all, cpps_all, empty_all, comm_all,
     &  last_arg, iocheck, i,j,k,l,m,n,  is,ie, itype, narg
      logical not_end_of_file
c**   integer iargc  !<-- uncomment it only if compiler does not
                     !    recognize "iargc" as intrinsic function.

      integer lstr, lenstr




      last_arg=iargc()
      do narg=1,last_arg
        call getarg(narg,fname)
        lstr=lenstr(fname)
        write(*,'(1x,3A)') 'Processing ''', fname(1:lstr), '''...'
        open(unit=iin,file=fname,form='formatted',status='old',err=7)

        n=0
        line=0
        not_end_of_file=.true.

  1     do i=1,max_string_size
          string(i:i)=' '
         enddo
         read(iin,'(A)',iostat=iocheck,end=3) string
         goto 4
  3      not_end_of_file=.false.
  4      if (iocheck.eq.0 ) then
           line=line+1
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

           if (is.eq.1) then
             if (string(1:1).eq.'!' .or. string(1:1).eq.'C' .or.
     &           string(1:1).eq.'#' .or. string(1:1).eq.'c') goto 1
           else

           endif
                 



           write(*,'(I4,1x,3A)') is, '''', string(is:ie), ''''












           if (not_end_of_file) goto 1
         else
           write(*,'(/1x,A,I5,1x,3A)') '### ERROR in reading line ',
     &                     line, 'in file ''',  fname(1:lstr), '''.'
         endif ! <-- iocheck.eq.0
        close(unit=iin)
   7    continue

      enddo
      stop
      end

