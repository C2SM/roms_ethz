WENO:

It is possible to activate the WENO scheme using the following CPP-keys:

#define WENO
#ifdef WENO
# define T_HADV_WENO
# define T_VADV_WENO
#endif

The WENO scheme is implemented to compute the horizontal and vertical advection of all the tracers.
It can help to prevent the emergence of unphysical negative concentration by computing fluxes more accurately and without the creation of oscillations.

The implementation of the fluxes is in : weno_flux.F

In this file there is an implementation of a WENO 5th order, WENO 3rd order and a upstream 2nd order function to compute the flux.

flux5_weno : This function computes tracer reconstruction at the grid cell's left
edge (u-point i − 1/2 or v-point j − 1/2) using the WENO5 procedure

flux3_weno : this function computes the same quantity except it uses the WENO3
procedure.

These functions can be called in the following files:
compute_horiz_tracer_fluxes.h and compute_vert_tracer_fluxes.h.

Finally, these two procedures are used in the following files:
step3d_t_ISO.F and pre_step3d4S.F

Here is a bref explanation of the implementation:

For the horizontal flux:
In step3d_t_ISO.F, for each sigma layers, a call is made to compute_horiz_tracer_fluxes.h.
Then, in this file, we first compute the flux FX in the ksi direction (i indices). We loop
over all the i indices of the MPI tile.

For the vertical flux:
In step3d_t_ISO_weno_3.F, we compute the flux FC in the sigma direction (k indices). We loop over j and
call compute_vert_tracer_fluxes_weno.h. 
We use the function flux5_weno to compute the flux of interior cell. 
We decay the order of accuracy (use flux3_weno) for cells at the top and bottom
of the columns of water because less ghost points are available to compute the flux.
Finally, we apply the boundary conditions.
