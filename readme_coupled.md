Note about the unified coupled/uncoupled ROMS code base
-------------------------------------------------------

-   The coupled model (ROMSOC) now has to be compiled using

    > make COUPLED=1

    - I did not edited romsoc_tools accordingly.

-   The cppdefs.h files are now under `src/include/`.
    - Please use the cppdefs_PACTC_ROMSOC.h  and link to it:

    > cd src/include && ln -s cppdefs_PACTC_ROMSOC.h cppdefs.h

-   I use the following environment settings.  You, Gesa, may like to
    replace "ROMSOC" by "code" :

    > # ROMSOC environment variables
    > # ----------------------------
    > # OASIS
    > export ROMSOC_OASIS_HOME=${HOME}/ROMSOC/oasis3-mct
    > export ROMSOC_OASIS_INSTALL=${ROMSOC_OASIS_HOME}/install
    > # ECCODES
    > export ROMSOC_ECCODES_INSTALL=/cluster/home/loher/COSMOR/eccodes-2.7.3-install
    > export ROMSOC_LIBGRIB1=/cluster/home/loher/COSMOR/DWD-libgrib1
    > # COSMO
    > export ROMSOC_COSMO_HOME=${HOME}/ROMSOC/cosmo_v4.30
    > # ROMS
    > export ROMSOC_ROMS_HOME=${HOME}/ROMSOC/roms_src_ethz

-   The `Makefile` and `Makedefs` files are now supposed to be universal. Let's try
    to keep it this way.
-   I created two little test scripts `roms_compile.sh` and  `romsoc_compile.sh` 
    under `src/tests` to check if `roms` (uncoupled) and `roms_cpl` compiles.  
    The one for roms_cpl is set up for the `Euler` cluster.
-   There is still a lot of cleanup to do.





