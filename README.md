# ROMS_ETHZ
## Overview
This is repository contains the source code of the version of [Regional Ocean Modelling System](https://www.myroms.org/)
used by the [UP group at ETH Zurich](https://up.ethz.ch). 

The physics is mostly identical to the [UCLA version of ROMS](http://https://github.com/CESR-lab/ucla-roms).  Over the years, we made a small changes to the _UCLA_ code, like adding the _WENO_ advection scheme based on [CROCO's implementation](https://en.ird.fr/project-croco-coastal-and-regional-ocean-community-model) . 

It contains 2 biogeochemical moduls, a simple NPZD-model and the Biogeochemical Elemental Cycling (BEC) of the [CESM](https://www.cesm.ucar.edu/models/cesm3). 
We added several additional plankton functional types (PFT) to BEC:  Coccolithophores, Southern-Ocean Phaeocystis, DDA (Diazotroph, Diatom Assemplage), UCYN.


## Documentation

Some sometimes outdated documentation in form of READMEs can be found in the `src/Documentation` subfolder.

## Compilation
### Prerequisites
You need a working FORTRAN compiler, the [NetCDF library](https://www.unidata.ucar.edu/software/netcdf/) (both, NetCDF-c and NetCDF-FORTRAN) installed.  Usually the code is run in parallel using MPI, which mean you will also need an installed [MPI library](https://www.mpi-forum.org/)

With this at the top level a simple

    make 

should work.  It compiles the code for our default PACTC60 setup,  a coarse version for our _PACific TelesCopic_  US Westcoast.   For other setups you need to specify the configuration, e.g.,

    make config=HUMPAC

for our HUMboldt PACific setup.  Have a look in the `configs/` directory for other setups.  Each `cppdef_XXXX.h` selects the `cpp` switches and dimensions for configuration `XXXX`.

