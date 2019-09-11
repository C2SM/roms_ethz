#include "cppdefs.h"
      subroutine oas_roms_define
!
!**** *oas_roms_define*  - Define grids etc for OASIS
!
!     Purpose. 
!     --------
!        Define grid and field information for exchange

!
!**   Interface.
!     ----------
!       *CALL*  *oas_roms_define*
!
!     Input:
!     -----
!
!     Output:
!     ------
!
!     Method:
!     ------
!        
!     Externals:
!     ---------
!       prism_init, prism_init_comp, prism_get_localcomm : prism library
!
!     Author:
!     -------
!       D. Byrne : ETHZ 

      USE oas_roms_vardef
      !!USE mod_oasis_namcouple    ! OASIS3MCT namcouple variables: e.g. coupling time step
      !use mod_prism_grids_writing
      !use mod_prism_def_partition_proto
      use mod_oasis_grid
      use mod_oasis_part

      implicit none
# include "param.h"
# include "ncvars.h"
# include "netcdf.inc"

      !Grid definition variables
      integer ierr, ncid, varid, lstr, lenstr, il_flag, ist(2), icnt(2) !Grid definition variables      
      real oaslon_rho(xi_rho,eta_rho), oaslon_crn(xi_rho, eta_rho,4),
     &  oaslon_u(xi_u,eta_rho),oaslon_v(xi_rho,eta_v),
     &  oaslon_psi(xi_u,eta_v), oasarea_tot(xi_rho,eta_rho),
     &  oasarea_totu(xi_u,eta_rho), oasarea_totv(xi_rho,eta_v),
     &  oas_pm(xi_rho,eta_rho), oas_pn(xi_rho,eta_rho)
      real oaslat_rho(xi_rho,eta_rho), oaslat_crn(xi_rho, eta_rho,4),
     &  oaslat_u(xi_u,eta_rho), oaslat_v(xi_rho,eta_v), oaslat_psi(xi_u,eta_v)
      real r_earth,start_lonc,start_latc, dx_src(xi_rho, eta_rho), dy_src(xi_rho, eta_rho), 
     &  source_dx(xi_rho, eta_rho), source_dy(xi_rho, eta_rho),
     &  dx_srcu(xi_u, eta_rho), dy_srcu(xi_u, eta_rho), 
     &  source_dxu(xi_u, eta_rho), source_dyu(xi_u, eta_rho),
     &  dx_srcv(xi_rho, eta_v), dy_srcv(xi_rho, eta_v), 
     &  source_dxv(xi_rho, eta_v), source_dyv(xi_rho, eta_v)
      integer oasmask_rho(xi_rho,eta_rho), oasmask_u(xi_u,eta_rho), 
     &  oasmask_v(xi_rho,eta_v),oi,oj, ok

      !Partition Definition
      integer il_paralr(5), il_part_idr, i, record, oasgrid, var_shape_oasis3r(1)
      integer il_paralu(5), il_part_idu, var_shape_oasis3u(1)
      integer il_paralv(5), il_part_idv, var_shape_oasis3v(1)
      !Coupling Variable Definition
      integer var_nodims(2), var_type, ji, jj 
      integer imin,imax,jmin,jmax,  vert_type, horiz_type, tile,
     &                              start(4), count(4)

      
      tile=0
      !Set subdomain limits
      call oas_roms_computebounds(r2dvar)
      call oas_roms_computebounds(u2dvar)
      call oas_roms_computebounds(v2dvar)
!      call oas_roms_computetilebounds(tile)

      ! -----------------------------------------------------------------
      ! ... Master process writes info on OASIS3 auxillary files (if needed) 
      ! ----------------------------------------------------------------

      !Only Master node writes grid file
      if (mynode .eq. 0) then
        call oasis_start_grids_writing(il_flag)
        
        if (il_flag .EQ. 1) then
          oasmask_rho(1:xi_rho,1:eta_rho)=1.
          oasmask_u(1:xi_u,1:eta_rho)=1.
          oasmask_v(1:xi_rho,1:eta_v)=1.

          !Get rho
          ist(1)=1
          ist(2)=1
          icnt(1)=xi_rho
          icnt(2)=eta_rho
          lstr=lenstr(grdname)
          print *, 'OasisRoms:Opening Gridfile ',grdname(1:lstr)
          ierr=nf_open(grdname(1:lstr), nf_nowrite, ncid)
          ierr=NF_INQ_VARID(ncid, 'lon_rho' , varid)
          ierr=NF_GET_VARA_DOUBLE (ncid, varid, ist, icnt, oaslon_rho(:,:))
          ierr=NF_INQ_VARID(ncid, 'lat_rho' , varid)
          ierr=NF_GET_VARA_DOUBLE (ncid, varid, ist, icnt, oaslat_rho(:,:))
          ierr=NF_INQ_VARID(ncid, 'pm' , varid)
          !Get grid metric
          ierr=NF_GET_VARA_DOUBLE (ncid, varid, ist, icnt, oas_pm(:,:))
          ierr=NF_INQ_VARID(ncid, 'pn' , varid)
          ierr=NF_GET_VARA_DOUBLE (ncid, varid, ist, icnt, oas_pn(:,:))
          call oasis_write_grid('Rrho', icnt(1), icnt(2), oaslon_rho, oaslat_rho)
          ierr=NF_INQ_VARID(ncid, 'mask_rho' , varid)
          ierr=NF_GET_VARA_INT (ncid, varid, ist, icnt, oasmask_rho(:,:))
          oasmask_rho(1:xi_rho,1:eta_rho)=oasmask_rho(1:xi_rho,1:eta_rho)-1 
          where (oasmask_rho(1:xi_rho,1:eta_rho) ==-1) oasmask_rho(1:xi_rho,1:eta_rho) =1
          call oasis_write_mask('Rrho', icnt(1), icnt(2), oasmask_rho)

          !Get U
          icnt(1)=xi_u
          icnt(2)=eta_rho
          ierr=NF_INQ_VARID(ncid, 'lon_u' , varid)
          ierr=NF_GET_VARA_DOUBLE (ncid, varid, ist, icnt, oaslon_u(:,:))
          ierr=NF_INQ_VARID(ncid, 'lat_u' , varid)
          ierr=NF_GET_VARA_DOUBLE (ncid, varid, ist, icnt, oaslat_u(:,:))
          call oasis_write_grid('R__u', icnt(1), icnt(2), oaslon_u, oaslat_u)
          ierr=NF_INQ_VARID(ncid, 'mask_u' , varid)
          ierr=NF_GET_VARA_INT (ncid, varid, ist, icnt, oasmask_u(:,:))
          oasmask_u(1:xi_u,1:eta_rho)=oasmask_u(1:xi_u,1:eta_rho)-1 
          where (oasmask_u(1:xi_u,1:eta_rho) ==-1) oasmask_u(1:xi_u,1:eta_rho) =1
          call oasis_write_mask('R__u', icnt(1), icnt(2), oasmask_u)

          !Get V
          icnt(1)=xi_rho
          icnt(2)=eta_v
          ierr=NF_INQ_VARID(ncid, 'lon_v' , varid)
          ierr=NF_GET_VARA_DOUBLE (ncid, varid, ist, icnt, oaslon_v(:,:))
          ierr=NF_INQ_VARID(ncid, 'lat_v' , varid)
          ierr=NF_GET_VARA_DOUBLE (ncid, varid, ist, icnt, oaslat_v(:,:))
          call oasis_write_grid('R__v', icnt(1), icnt(2), oaslon_v, oaslat_v)
          ierr=NF_INQ_VARID(ncid, 'mask_v' , varid)
          ierr=NF_GET_VARA_INT (ncid, varid, ist, icnt, oasmask_v(:,:))
          oasmask_v(1:xi_rho,1:eta_v)=oasmask_v(1:xi_rho,1:eta_v)-1 
          where (oasmask_v(1:xi_rho,1:eta_v) ==-1) oasmask_v(1:xi_rho,1:eta_v) =1
          call oasis_write_mask('R__v', icnt(1), icnt(2), oasmask_v)

          !Get Psi grid for corners and area calculation
          icnt(1)=xi_u
          icnt(2)=eta_v
          ierr=NF_INQ_VARID(ncid, 'lon_psi' , varid)
          ierr=NF_GET_VARA_DOUBLE (ncid, varid, ist, icnt, oaslon_psi(:,:))
          ierr=NF_INQ_VARID(ncid, 'lat_psi' , varid)
          ierr=NF_GET_VARA_DOUBLE (ncid, varid, ist, icnt, oaslat_psi(:,:))


          print *, 'OASISROMS:Writing Corners',grdname(1:lstr)

          !Read in/Calculate Corners
          !1: lower left corner. 2: lower right corner 
          !3: upper right corner 4: upper left corner
          !(anti-clockwise, starting from the bottom left corner)

          !-----------------------------------------------------------------
          ! ... Define the corners of the unstaggered grid points
          ! -----------------------------------------------------------------
          start_lonc = startlon_tot - dlon * 0.5_ireals
          start_latc = startlat_tot - dlat * 0.5_ireals
          CALL oas_roms_constant_corners(oaslon_crn(:,:,1),oaslat_crn(:,:,1), start_lonc, start_latc,xi_rho,eta_rho)

          start_lonc = startlon_tot + dlon * 0.5_ireals
          start_latc = startlat_tot - dlat * 0.5_ireals
          CALL oas_roms_constant_corners(oaslon_crn(:,:,2),oaslat_crn(:,:,2), start_lonc, start_latc,xi_rho,eta_rho)

          start_lonc = startlon_tot + dlon * 0.5_ireals
          start_latc = startlat_tot + dlat * 0.5_ireals
          CALL oas_roms_constant_corners(oaslon_crn(:,:,3),oaslat_crn(:,:,3), start_lonc, start_latc,xi_rho,eta_rho)

          start_lonc = startlon_tot - dlon * 0.5_ireals
          start_latc = startlat_tot + dlat * 0.5_ireals
          CALL oas_roms_constant_corners(oaslon_crn(:,:,4),oaslat_crn(:,:,4), start_lonc, start_latc,xi_rho,eta_rho)

          call oasis_write_corner('Rrho',xi_rho, eta_rho,4,oaslon_crn,oaslat_crn)


          ! For U grid points
          start_lonc = startlon_tot
          start_latc = startlat_tot - dlat * 0.5_ireals
          CALL oas_roms_constant_corners(oaslon_crn(:,:,1),oaslat_crn(:,:,1), start_lonc, start_latc,xi_rho,eta_rho)

          start_lonc = startlon_tot + dlon
          start_latc = startlat_tot - dlat * 0.5_ireals
          CALL oas_roms_constant_corners(oaslon_crn(:,:,2),oaslat_crn(:,:,2), start_lonc, start_latc,xi_rho,eta_rho)

          start_lonc = startlon_tot + dlon
          start_latc = startlat_tot + dlat * 0.5_ireals
          CALL oas_roms_constant_corners(oaslon_crn(:,:,3),oaslat_crn(:,:,3), start_lonc, start_latc,xi_rho,eta_rho)

          start_lonc = startlon_tot 
          start_latc = startlat_tot + dlat * 0.5_ireals
          CALL oas_roms_constant_corners(oaslon_crn(:,:,4),oaslat_crn(:,:,4), start_lonc, start_latc,xi_rho,eta_rho)
          
          CALL oasis_write_corner('R__u',xi_u, eta_rho,4,
     &                            oaslon_crn(1:xi_u,1:eta_rho,1:4),oaslat_crn(1:xi_u,1:eta_rho,1:4))

          ! For V grid points
          start_lonc = startlon_tot - dlon * 0.5_ireals
          start_latc = startlat_tot
          CALL oas_roms_constant_corners(oaslon_crn(:,:,1),oaslat_crn(:,:,1), start_lonc, start_latc,xi_rho,eta_rho)

          start_lonc = startlon_tot + dlon * 0.5_ireals
          start_latc = startlat_tot
          CALL oas_roms_constant_corners(oaslon_crn(:,:,2),oaslat_crn(:,:,2), start_lonc, start_latc,xi_rho,eta_rho)

          start_lonc = startlon_tot + dlon * 0.5_ireals
          start_latc = startlat_tot + dlat
          CALL oas_roms_constant_corners(oaslon_crn(:,:,3),oaslat_crn(:,:,3), start_lonc, start_latc,xi_rho,eta_rho)

          start_lonc = startlon_tot - dlon * 0.5_ireals
          start_latc = startlat_tot + dlat
          CALL oas_roms_constant_corners(oaslon_crn(:,:,4),oaslat_crn(:,:,4), start_lonc, start_latc,xi_rho,eta_rho)

          CALL oasis_write_corner('R__v',xi_rho, eta_v,4,
     &                            oaslon_crn(1:xi_rho,1:eta_v,1:4),oaslat_crn(1:xi_rho,1:eta_v,1:4))




          !Rho Corners
!          oaslon_rho_crn(2:xi_rho,2:eta_rho,1)=oaslon_psi(1:xi_u,1:eta_v)
!          oaslon_rho_crn(1:xi_u,2:eta_rho,2)=oaslon_psi(1:xi_u,1:eta_v)
!          oaslon_rho_crn(1:xi_u,1:eta_v,3)=oaslon_psi(1:xi_u,1:eta_v)
!          oaslon_rho_crn(2:xi_rho,1:eta_v,4)=oaslon_psi(1:xi_u,1:eta_v)
          !U Corners
!          oaslon_u_crn(1:xi_u,2:eta_rho,1)=oaslon_v(1:xi_u,1:eta_v)
!          oaslon_u_crn(1:xi_u,2:eta_rho,2)=oaslon_v(1:xi_u,1:eta_v)
!          oaslon_u_crn(1:xi_u,1:eta_v,3)=oaslon_v(2:xi_rho,1:eta_v)
!          oaslon_u_crn(1:xi_u,1:eta_v,4)=oaslon_v(1:xi_u,1:eta_v)
          !V Corners
!          oaslon_v_crn(2:xi_rho,1:eta_v,1)=oaslon_u(1:xi_u,1:eta_v)
!          oaslon_v_crn(1:xi_u,1:eta_v,2)=oaslon_u(1:xi_u,1:eta_v)
!          oaslon_v_crn(1:xi_u,1:eta_v,3)=oaslon_u(1:xi_u,2:eta_rho)
!          oaslon_v_crn(2:xi_rho,1:eta_v,4)=oaslon_u(1:xi_u,2:eta_rho)

          !Rho Corners Lat
!          oaslat_rho_crn(2:xi_rho,2:eta_rho,1)=oaslat_psi(1:xi_u,1:eta_v)
!          oaslat_rho_crn(1:xi_u,2:eta_rho,2)=oaslat_psi(1:xi_u,1:eta_v)
!          oaslat_rho_crn(1:xi_u,1:eta_v,3)=oaslat_psi(1:xi_u,1:eta_v)
!          oaslat_rho_crn(2:xi_rho,1:eta_v,4)=oaslat_psi(1:xi_u,1:eta_v)
          !U Corners
!          oaslat_u_crn(1:xi_u,2:eta_rho,1)=oaslat_v(1:xi_u,1:eta_v)
!          oaslat_u_crn(1:xi_u,2:eta_rho,2)=oaslat_v(1:xi_u,1:eta_v)
!          oaslat_u_crn(1:xi_u,1:eta_v,3)=oaslat_v(2:xi_rho,1:eta_v)
!          oaslat_u_crn(1:xi_u,1:eta_v,4)=oaslat_v(1:xi_u,1:eta_v)
          !V Corners
!          oaslat_v_crn(2:xi_rho,1:eta_v,1)=oaslat_u(1:xi_u,1:eta_v)
!          oaslat_v_crn(1:xi_u,1:eta_v,2)=oaslat_u(1:xi_u,1:eta_v)
!          oaslat_v_crn(1:xi_u,1:eta_v,3)=oaslat_u(1:xi_u,2:eta_rho)
!          oaslat_v_crn(2:xi_rho,1:eta_v,4)=oaslat_u(1:xi_u,2:eta_rho)


          !Write Corners
          !1: lower left corner. 2: lower right corner 
          !3: upper right corner 4: upper left corner
          !(anti-clockwise, starting from the bottom left corner)
!          call prism_write_corner('Rrho',xi_rho, eta_rho,4,oaslon_rho_crn,oaslat_rho_crn)
!          call prism_write_corner('R__u',xi_u, eta_rho,4,oaslon_u_crn,oaslat_u_crn)
!          call prism_write_corner('R__v',xi_rho, eta_v,4,oaslon_v_crn,oaslat_v_crn)

          !Areas of cells <----calculated as in Cosmo UOI interface to be consistent
          r_earth  =  6371.229E3
          do ji=1,xi_rho -1
             dx_src(ji,:) = r_earth*sqrt((cos(oaslat_rho(ji,:))*(oaslon_rho(ji+1,:)-oaslon_rho(ji,:)))**2 + 
     &                                                 (oaslat_rho(ji+1,:)-oaslat_rho(ji,:))**2)
          end do
          source_dx(1:xi_rho-1,1:eta_rho) = dx_src(1:xi_rho-1,1:eta_rho)
          source_dx(xi_rho,:)            = 2*source_dx(xi_rho-1,:)-source_dx(xi_rho-2,:)
          
          do ji=1,eta_rho -1
             dy_src(:,ji) = r_earth*sqrt((cos(oaslat_rho(:,ji))*(oaslon_rho(:,ji+1)-oaslon_rho(:,ji)))**2 + 
     &                                                 (oaslat_rho(:,ji+1)-oaslat_rho(:,ji))**2)
          end do
          source_dy(1:xi_rho,1:eta_rho-1) = dy_src(1:xi_rho,1:eta_rho-1)
          source_dy(:,eta_rho)            = 2*source_dy(:,eta_rho-1)-source_dy(:,eta_rho-2)


          oasarea_tot=source_dx*source_dy
          call oasis_write_area ('Rrho', xi_rho, eta_rho, oasarea_tot)

          !U areas 
          do ji=1,xi_u -1
             dx_srcu(ji,:) = r_earth*sqrt((cos(oaslat_u(ji,:))*(oaslon_u(ji+1,:)-oaslon_u(ji,:)))**2 + 
     &                                                 (oaslat_u(ji+1,:)-oaslat_u(ji,:))**2)
          end do
          source_dxu(1:xi_u-1,1:eta_rho) = dx_srcu(1:xi_u-1,1:eta_rho)
          source_dxu(xi_u,:)            = 2*source_dxu(xi_u-1,:)-source_dxu(xi_u-2,:)
          
          do ji=1,eta_rho -1
             dy_srcu(:,ji) = r_earth*sqrt((cos(oaslat_u(:,ji))*(oaslon_u(:,ji+1)-oaslon_u(:,ji)))**2 + 
     &                                                 (oaslat_u(:,ji+1)-oaslat_u(:,ji))**2)
          end do
          source_dyu(1:xi_u,1:eta_rho-1) = dy_srcu(1:xi_u,1:eta_rho-1)
          source_dyu(:,eta_rho)            = 2*source_dyu(:,eta_rho-1)-source_dyu(:,eta_rho-2)

          oasarea_totu=source_dxu*source_dyu
          call oasis_write_area ('R__u', xi_u, eta_rho, oasarea_totu(1:xi_u,1:eta_rho))

          !V areas 
          do ji=1,xi_rho -1
             dx_srcv(ji,:) = r_earth*sqrt((cos(oaslat_v(ji,:))*(oaslon_v(ji+1,:)-oaslon_v(ji,:)))**2 + 
     &                                                 (oaslat_v(ji+1,:)-oaslat_v(ji,:))**2)
          end do
          source_dxv(1:xi_rho-1,1:eta_v) = dx_srcv(1:xi_rho-1,1:eta_v)
          source_dxv(xi_rho,:)            = 2*source_dxv(xi_rho-1,:)-source_dxv(xi_rho-2,:)
          
          do ji=1,eta_v -1
             dy_srcv(:,ji) = r_earth*sqrt((cos(oaslat_v(:,ji))*(oaslon_v(:,ji+1)-oaslon_v(:,ji)))**2 + 
     &                                                 (oaslat_v(:,ji+1)-oaslat_v(:,ji))**2)
          end do
          source_dyv(1:xi_rho,1:eta_v-1) = dy_srcv(1:xi_rho,1:eta_v-1)
          source_dyv(:,eta_v)            = 2*source_dyv(:,eta_v-1)-source_dyv(:,eta_v-2)

          oasarea_totv=source_dxv*source_dyv
          call oasis_write_area ('R__v', xi_rho, eta_v, oasarea_totv(1:xi_rho,1:eta_v))

          call oasis_terminate_grids_writing()
        endif
      endif
      
      


  !!!!!!!!!!!!!!!!!!!!!!! DEFINITION OF THE LOCAL SHAPE OF THE ARRAYS !!!!!!!!!!!!!!!!!!!!!
  !
  ! Allocate the fields sent and received by the model without Halo and overlaps between CPU's
  !
  !
  !Rho Grid   
!DL: these are not used any more. The Oasis User Guide recommends to
!     pass a vector of length 1
        var_shape_oasis3r(1) = 1
  !U grid
        var_shape_oasis3u(1) = 1
  !V grid
        var_shape_oasis3v(1) = 1


              
  !Define the partioning of the grid

        il_paralr ( CLIM_strategy ) = clim_box        !Box Partition
        il_paralr ( CLIM_offset   ) = (oas_start(1)-1)+((oas_start(2)-1)*xi_rho) !Offset of upper left corner of node in physical grid
        il_paralr ( CLIM_SizeX   ) = oas_xi         !Local size in x_i
        il_paralr ( CLIM_SizeY   ) = oas_eta         !Local size in eta
        il_paralr ( CLIM_Ldx   ) = xi_rho             !Global Size in X_i

  !Define the partioning of the grid
        il_paralu ( CLIM_strategy ) = clim_box        !Box Partition
        il_paralu ( CLIM_offset   ) = (oas_start_u(1)-1)+((oas_start_u(2)-1)*xi_u) !Offset of upper left corner of node in physical grid
        il_paralu ( CLIM_SizeX   ) = oas_xi_u        !Local size in x_i
        il_paralu ( CLIM_SizeY   ) = oas_eta_u         !Local size in eta
        il_paralu ( CLIM_Ldx   ) = xi_u             !Global Size in X_i 

  !Define the partioning of the grid
        il_paralv ( CLIM_strategy ) = clim_box        !Box Partition
        il_paralv ( CLIM_offset   ) = (oas_start_v(1)-1)+((oas_start_v(2)-1)*xi_rho) !Offset of upper left corner of node in physical grid
        il_paralv ( CLIM_SizeX   ) = oas_xi_v         !Local size in x_i
        il_paralv ( CLIM_SizeY   ) = oas_eta_v         !Local size in eta
        il_paralv ( CLIM_Ldx   ) = xi_rho             !Global Size in X_i

        call oasis_def_partition (il_part_idr, il_paralr, ierr)
        call oasis_def_partition (il_part_idu, il_paralu, ierr)
        call oasis_def_partition (il_part_idv, il_paralv, ierr)

  !
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  ! DEFINITION OF THE COUPLING FIELDS  
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

      ! Default values
      ssnd(:)%laction=.FALSE.  ; srcv(:)%laction=.FALSE.
      ssnd(:)%clgrid='ROMS'    ; srcv(:)%clgrid='ROMS'
      
      !Declare Send Variables
      ssnd(1)%clname='SO_SST_A'
      ssnd(1)%clgrid='Rrho'

      !Declare Receive Variables
      srcv(1)%clname='RO_UST_A'    ! u-momentum flux (surface)          ( N/m2)
      srcv(1)%clgrid='R__u'
      srcv(2)%clname='RO_VST_A'    ! v-momentum flux (surface)          ( N/m2)
      srcv(2)%clgrid='R__v'
      srcv(3)%clname='RO_NHF_A'    ! Net heat flux (surface)      ( W/m2)
      srcv(3)%clgrid='Rrho' 
      srcv(4)%clname='RO_SWR_A'    ! direct shortwave downward radiation (W/m2)
      srcv(4)%clgrid='Rrho'
      srcv(5)%clname='RO_TEP_A'  ! total Evaporation - precipitation rate     (kg/m2*s)
      srcv(5)%clgrid='Rrho'
 
      ! Variable selection
      ! in the final version, must be defined by user (for example through namelist)
      ssnd(1)%laction=.TRUE. 
      srcv(1:5)%laction=.TRUE.
      rcv_st=1

      var_nodims(1) = 2           ! Dimension number of exchanged arrays
      var_nodims(2) = 1           ! number of bundles (always 1 for OASIS3)

      ! ... Declare send variables. 
      !
      ksnd=0
      do ji = 1, nmaxfld
         if ( ssnd(ji)%laction ) then 
            call oasis_def_var( ssnd(ji)%nid, ssnd(ji)%clname, il_part_idr, var_nodims, 
     &                                OASIS_OUT, var_shape_oasis3r, OASIS_REAL, ierr )
         ksnd= ksnd+1
         endif
      end do
      !
      ! ... Declare received variables. 
      !
      krcv = 0
      do ji = 1, nmaxfld
         if ( srcv(ji)%laction ) then
            if (ji .eq. oas_UST) then
              call oasis_def_var( srcv(ji)%nid, srcv(ji)%clname, il_part_idu, var_nodims,  
     &                                 OASIS_In, var_shape_oasis3u, OASIS_REAL, ierr )  
            endif
            if (ji .eq. oas_VST) then
              call oasis_def_var( srcv(ji)%nid, srcv(ji)%clname, il_part_idv, var_nodims,
     &                                 OASIS_In, var_shape_oasis3v, OASIS_REAL, ierr )  
            endif
            if (ji .ge. 3) then
              call oasis_def_var( srcv(ji)%nid, srcv(ji)%clname, il_part_idr, var_nodims,
     &                                 OASIS_In, var_shape_oasis3r, OASIS_REAL, ierr )
            endif
            krcv = krcv + 1
         endif
      end do

      !Allocate Memory
      call oas_roms_allocate

  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !         TERMINATION OF DEFINITION PHASE AND SEARCH 
  ! OASIS3 : only the processes involved in the coupling must call prism_enddef_proto;
  ! here all processes are involved in coupling
  !+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  !
  !!!!!!!!!!!!!!!!!! OASIS_ENDDEF !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !

      call oasis_enddef (ierr)
      return

      end


