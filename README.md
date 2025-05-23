# ROMS_ETHZ
## Overview
This is repository contains the source code of the version of _Regional Ocean Modelling System_ (https://www.myroms.org/, https://github.com/myroms/roms)
used by the UP group (https://up.ethz.ch) at  ETH Zurich. 

The physics is mostly identical to the _UCLA_ version of ROMS (http://https://github.com/CESR-lab/ucla-roms).  
Over the years, we made a small changes to the _UCLA_ code, like adding the _WENO_ advection scheme. 
It contains 2 biogeochemical moduls, a simple NPZD-model and the Biogeochemical Elemental Cycling (BEC) of the CESR model. 
We added several additional plankton functional types (PFT) to BEC:  Coccolithophores, Southern-Ocean Phaeocystis, DDA (Diazotroph, Diatom Assemplage), UCYN.


## Documentation

Some sometimes outdated documentation in form of READMEs can be found in the `src/Documentation` subfolder.

## Compilation

At the top level a simple

>  make 

may work.  It compiles the code in a coarse version for our _PACific TelesCopic_  US Westcoast setup .   For other setups you need to specify the configuration, e.g.,

> make config=HUMPAC

for our HUMboldt PACific setup.  Have a look in the `configs/` directory for other setups.  Each `cppdef_XXXX.h` selects the `cpp` switches and dimensions for configuration `XXXX`.

