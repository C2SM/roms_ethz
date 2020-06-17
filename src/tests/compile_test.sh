#!/bin/bash
#
# Test script for various ETH Zurich ROMS setups

echo Testing compilation for various ETH ROMS setups:
for setup in AMACAN PACTC HUMPAC SO BENGT CANAMA ONEDIM # ATLTC SAEP OLDSTYLE MOONS
do
	cd ../include
	cp cppdefs_$setup.h cppdefs.h
	cd ..
	echo -n '   - '${setup} ...' '
	# make clean >/dev/null
	make -j > tests/make_$setup.log 2>&1 || exit 
	echo OK
	mv roms tests/roms_$setup 
	rm include/cppdefs.h
	cd tests
done

