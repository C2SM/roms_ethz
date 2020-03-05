Full WENO:

The new WENO scheme is implemented into 3 files and 2 functions.

_ step3d_t_ISO_weno_3.F: which contains all the routines to perform a time step
on all the 3 dimensions

_ compute_horiz_tracer_fluxes_weno.h: that computes the horizontal flux using the
WENO procedure for all the tracers

_ compute_vert_tracer_fluxes_weno.h: that computes the vertical flux using the
WENO procedure for all the tracers.

The functions :
• flux5_weno : This function computes tracer reconstruction at the grid cell’s left
edge (u-point i − 1/2 or v-point j − 1/2) using the WENO5 procedure presented
in [2].
• flux3_weno : this function computes the same quantity except it uses the WENO3
procedure.

In the file step3d_t_ISO_weno_3.F, for each σ layers, a call is
made to compute_horiz_tracer_fluxes_weno.h.
• In this file, we first compute the flux FX in the ξ direction (i indices). We loop
over all the i indices of the MPI tile.

Later in step3d_t_ISO_weno_3.F, we compute the flux FC in the σ direction (k indices). We loop over j and
call compute_vert_tracer_fluxes_weno.h.
• We use the function flux5_weno to compute the flux of interior cell.
• We decay the order of accuracy (use flux3_weno) for cells at the top and bottom
of the columns of water because less ghost points are available to compute the flux.
• Finally, we apply the boundary conditions.


It is possible to activate the WENO scheme using the foolowing CPP-keys:
# define TS_HADV_WENO3
# define TS_VADV_WENO5

and the possibility to also activate the # define TS_HADV_WENO5 but is not recommended yet.


