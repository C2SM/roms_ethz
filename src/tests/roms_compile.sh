#!/bin/bash
#
# Test script for various ETH Zurich ROMS setups

echo Testing ROMS compilation for various ETH ROMS setups:
if [ ! -d logs ] ;  then
    mkdir logs
fi
for setup in AMACAN PACTC HUMPAC SO BENGT CANAMA ONEDIM # ATLTC SAEP OLDSTYLE MOONS
do
    cd ..
	cp include/cppdefs_$setup.h include/cppdefs.h
	echo -n '   - '${setup} ...' '
	make clean > /dev/null
    make -j > tests/logs/make_$setup.log 2>&1 && echo OK
    if [ $? -ne 0 ] ; then
        echo FAIL
        echo Check tests/logs/make_$setup.log for more information
        exit 1
    fi
	mv roms tests/logs/roms_$setup
	rm include/cppdefs.h
	cd tests
done

