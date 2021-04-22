#include "cppdefs.h"
      MODULE oas_roms_set_cpl_grd
         ! Description
         ! -----------
         ! Set coupling grid strucutre using compute_starts_counts.h
         ! WARNING: This is very dirty, using compute_starts_counts.h ONLY works
         !          when PARALLEL_FILES is not activated.
         !          We should switch to a more robust solution in the future
         !          based on istr, istrU, etc... It would probably work except for
         !          global starting indices of local tile.
   
         ! Notes
         ! -----
         ! This is written in an .FF90 file and FIXED FORMAT in order to be able to
         ! include ROMS .h files and run mpc on them.
         !
         ! Unfortunamtely, we have to use the `type` and `count` variable names
         ! in order to USE compute_starts_counts.h.

         ! Authors
         ! -------
         ! Matthieu Leclair- ETHZ

         IMPLICIT NONE

         PRIVATE

         PUBLIC :: oas_roms_set_grd

      CONTAINS

         SUBROUTINE oas_roms_set_grd(grd, k_pt)


            USE oas_roms_data, ONLY: OAS_GRID, cpl_grd, k_rho, k_u, k_v

            ! Arguments
            TYPE(OAS_GRID) :: grd
            INTEGER :: k_pt

#include "param.h"

            IF (k_pt == k_rho) THEN
               grd%pt = 'rho'
               grd%grd_name = 'Rrho'
               grd%dims_g = (/LLm,MMm/)
            ELSEIF (k_pt == k_u) THEN
               grd%pt = 'u'
               grd%grd_name = 'R__u'
               grd%dims_g = (/LLm-1,MMm/)
            ELSEIF (k_pt == k_v) THEN
               grd%pt = 'v'
               grd%grd_name = 'R__v'
               grd%dims_g = (/LLm,MMm-1/)
            ELSE
               WRITE(*,*) 'ERROR in oas_roms_set_cpl_grd :'
               WRITE(*,*) '   unkown value of k_pt'
               CALL abort
            ENDIF

            CALL oas_roms_set_grd_local(grd)

         END SUBROUTINE oas_roms_set_grd

         ! ----------------------------------------------------------------------------------- !

         SUBROUTINE oas_roms_set_grd_local(grd)
            ! Description
            ! -----------
            ! Set coupling grid properties depending on the compute_starts_counts.h include

            USE oas_roms_data, ONLY: OAS_GRID, IOASISDEBUGLVL, k_rho, k_u, k_v

            ! Arguments
            TYPE(OAS_GRID) :: grd

#include "param.h"
! For debug info inode, jnode if IOOASISDEBUGLVL == 1
# include "hidden_mpi_vars.h"

            grd%imin = iwest
            grd%imax = ieast
            grd%jmin = jsouth
            grd%jmax = jnorth
            !if (type == k_u) grd%imax=grd%imax-1
            !if (type == k_v) grd%jmax=grd%jmax-1
            grd%start_g = (/max(iSW_corn+1,1), max(jSW_corn+1,1)/)
            if (TRIM(grd%pt) == 'u') grd%start_g = (/max(iSW_corn,1),max(jSW_corn+1,1)/)
            if (TRIM(grd%pt) == 'v') grd%start_g =(/max(iSW_corn+1,1),max(jSW_corn,1)/)

            if (TRIM(grd%pt) == 'u' .and. grd%start_g(1) == 1 ) grd%imax=grd%imax-1
            if (TRIM(grd%pt) == 'v' .and. grd%start_g(2) == 1 ) grd%jmax=grd%jmax-1
            grd%dims_l = (/grd%imax-grd%imin+1, grd%jmax-grd%jmin+1/)
     
            ALLOCATE(grd%exfld(grd%imin:grd%imax, grd%jmin:grd%jmax))

            IF ( IOASISDEBUGLVL >= 2 ) THEN
               PRINT *, '***********************************************'
               PRINT *, 'Mynode: ', mynode
               PRINT *, 'Node tile (inode, jnode) = (',inode,', ', jnode,')'
               PRINT *, 'Grid Variables ROMS', TRIM(grd%pt)
               PRINT *, 'start: ', grd%start_g
               PRINT *, 'global dims: ', grd%dims_g
               PRINT *, 'local_rho, local_eta: ', grd%dims_l
               PRINT *, 'jSW corn: ', jSW_corn
               PRINT *, 'iSW corn: ', iSW_corn
               PRINT *, 'imin,imax: ', grd%imin,grd%imax
               PRINT *, 'jmin, jmax: ', grd%jmin,grd%jmax
               PRINT *, '**********************************************'
            ENDIF

         END SUBROUTINE oas_roms_set_grd_local

      END MODULE oas_roms_set_cpl_grd
