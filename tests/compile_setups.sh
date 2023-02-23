#!/bin/bash
#
# Test compilation of ROMS and ROMSOC for various ETH Zurich setups
# By default both ROMS and ROMSOC are tried compile for each setup
# Only ROMS and ROMSOC are tried to compile if you call it with
# argmument ROMS or ROMSOC, respectively

SETUPS="PACTC60 AMACAN PACTC HUMPAC SO BENGT CANAMA ONEDIM" # ATLTC SAEP OLDSTYLE MOONS

echo '    ' Testing ROMS and/or ROMSOC compilation for various ETH setups:

testdir=$PWD
export SRCDIR=$testdir/../src

cd ..
for setup in $SETUPS ; do
	echo  '   - '${setup}': '
	if [ "$1" != "ROMSOC" ] ; then
        echo -n '         - ROMS ... '
		export BLDDIR=$testdir/${setup}_build
        mkdir $BLDDIR
		make -j config=$setup > $BLDDIR/build.log 2>&1 && echo SUCCESS
		if [ $? -ne 0 ] ; then
			echo FAIL
		fi
	fi
	if [ "$1" != "ROMS" ] ; then
		echo -n '         - ROMSOC ... '
		export BLDDIR=$testdir/${setup}_romsoc_build
        mkdir $BLDDIR
		make -j roms_cpl config=$setup > $BLDDIR/build.log 2>&1 && echo SUCCESS
		if [ $? -ne 0 ] ; then
			echo FAIL
		fi
	fi
done

