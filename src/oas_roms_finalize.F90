   SUBROUTINE oas_roms_finalize

      !!---------------------------------------------------------------------
      !!              ***  ROUTINE oas_roms_finalize  ***
      !!
      !! ** Purpose : - Finalizes the coupling. If MPI_init has not been
      !!      called explicitly before oas_roms_init it will also close
      !!      MPI communication.
      !!----------------------------------------------------------------------
      USE oas_roms_vardef

      IMPLICIT NONE

      !write(6,*) 'call prism_terminate '
      call flush(6)
      !DEALLOCATE(exfld)
      CALL oasis_terminate ( nerror )         

   END SUBROUTINE oas_roms_finalize
