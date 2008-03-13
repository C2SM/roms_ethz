      implicit none
      integer max_name_size, max_string_size, maxwords, iin, iout
      parameter (max_name_size=32,max_string_size=72, iin=11,iout=12)
      character fname*(max_name_size),  string*(max_string_size+8)
      integer nfname, line, n,i,ie,ierr, last_arg,iargc
      logical not_end_of_file
 
 
      last_arg=iargc()
      if (last_arg.gt.0) then
        do n=1,last_arg
          do i=1,max_name_size
            fname(i:i)=' '
          enddo
          call getarg(n,fname)
          nfname=1
          do while (fname(nfname:nfname).ne.' ')
            nfname=nfname+1
          enddo
          nfname=nfname-1
          string(1:4)='tmp/'
          string(5:4+nfname)=fname(1:nfname)
          write(*,'(I3,1x,A)') n, fname(1:nfname)
          open(unit=iin,  file=fname(1:nfname),    form='formatted',
     &                                                 status='old')
          open(unit=iout, file=string(1:4+nfname), form='formatted')
          line=0
          not_end_of_file=.true.
          do while (not_end_of_file)
            line=line+1
            do i=1,max_string_size
              string(i:i)=' '
            enddo
            read(iin,'(A)',iostat=ierr,end=1) string
            goto 2
  1         not_end_of_file=.false.
  2         ie=max_string_size
            do while(string(ie:ie).eq.' ' .and. ie.gt.1)
              ie=ie-1
            enddo
            if (string(ie:ie).eq.' ') ie=ie-1
            if (ie.gt.0) then
              i=1
              do while (i.lt.ie)
                if (string(i:i).eq.'I') then
                  if (string(i+1:i+3).eq.'str' .or.
     &                string(i+1:i+3).eq.'end') then
                    string(i:i)='i'
                    i=i+3
                  endif
                elseif (string(i:i).eq.'J') then
                  if (string(i+1:i+3).eq.'str' .or.
     &                string(i+1:i+3).eq.'end') then
                    string(i:i)='j'
                    i=i+3
                  endif
                endif
                i=i+1
              enddo
              write(iout,'(A)') string(1:ie)
            else
              write(iout,*)
            endif
          enddo
          close(iout)
          close(iin)
        enddo
      else
        write(*,*) 'ERROR: no file to process.'
      endif
      stop
      end
 
