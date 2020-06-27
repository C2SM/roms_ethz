#!/bin/bash
#
# Compile test for ROMSOC on Euler for various UP setups

# ROMSOC environment variables
# ----------------------------
# OASIS
export ROMSOC_OASIS_HOME=${HOME}/ROMSOC/oasis3-mct
export ROMSOC_OASIS_INSTALL=${ROMSOC_OASIS_HOME}/install
# ECCODES
export ROMSOC_ECCODES_INSTALL=/cluster/home/loher/COSMOR/eccodes-2.7.3-install
export ROMSOC_LIBGRIB1=/cluster/home/loher/COSMOR/DWD-libgrib1
# COSMO
export ROMSOC_COSMO_HOME=${HOME}/ROMSOC/cosmo_v4.30
# ROMS
export ROMSOC_ROMS_HOME=${HOME}/ROMSOC/roms_src_ethz
module purge
# standard modules
module load intel/15.0.0 netcdf/4.3.1 open_mpi/1.6.5
# new compiler and Intel MPI library
#    module load new intel/14.0.1 netcdf/4.3.2 parallel_studio_xe/2018.1i

echo Testing ROMSOC compilation for various ETH ROMS setups:
if [ ! -d logs ] ;  then
    mkdir logs
fi
for setup in AMACAN PACTC HUMPAC SO BENGT CANAMA ONEDIM # ATLTC SAEP OLDSTYLE MOONS
do
	cd ../include
	cp cppdefs_$setup.h cppdefs.h
    echo '#define COUP_OAS' >> cppdefs.h
	cd ..
	echo -n '   - '${setup} ...' '
	make distclean >/dev/null
    make roms_cpl COUPLED=1 Q='' > tests/logs/make_roms_cpl_${setup}.log 2>&1
    if [ $? -ne 0 ] ; then
         echo 'FAIL'
         exit 1
     fi
	echo OK
	mv roms_cpl tests/logs/roms_cpl_$setup
	rm include/cppdefs.h
	cd tests
done

