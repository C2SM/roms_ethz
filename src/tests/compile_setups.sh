#!/bin/bash
#
# Test compilation of ROMS and ROMSOC for various ETH Zurich setups
# By default both ROMS and ROMSOC are tried compile for each setup
# Only ROMS and ROMSOC are tried to compile if you call it with
# argmument ROMS or ROMSOC, respectively


echo '    ' Testing ROMS and/or ROMSOC compilation for various ETH setups:

# ROMS source directory:
export SRCDIR=$PWD/..

# Log make output in this directory
LOGDIR=$PWD/make_logs
if [ ! -d $LOGDIR ] ;  then
    mkdir $LOGDIR
fi
echo '    ' Log location:: $LOGDIR
echo

# Build in the directory:
export BLDDIR=$PWD/build
if [ ! -d $BLDDIR ] ; then
    mkdir $BLDDIR
fi

# echo SRCDIR: $SRCDIR
# echo BLDDIR: $BLDDIR


SETUPS="AMACAN PACTC" # HUMPAC SO BENGT CANAMA ONEDIM" # ATLTC SAEP OLDSTYLE MOONS
cd  $BLDDIR

for setup in $SETUPS ; do
	cp $SRCDIR/include/cppdefs_$setup.h $SRCDIR/include/cppdefs.h
	cp $SRCDIR/Makefile .
	echo  '   - '${setup}': '
	if [ "$1" != "ROMSOC" ] ; then
		echo -n '         - ROMS   ... '
		make clean > /dev/null
		make -j > $LOGDIR/make_$setup.log 2>&1 && echo OK
		if [ $? -ne 0 ] ; then
			echo FAIL
		fi
		mv $BLDDIR/roms $BLDDIR/roms_$setup
	fi
	if [ "$1" != "ROMS" ] ; then
		echo -n '         - ROMSOC ... '
		make clean > /dev/null
		make -j COUPLED=1 > $LOGDIR/make_romsoc_$setup.log 2>&1 && echo OK
		if [ $? -ne 0 ] ; then
			echo FAIL
		fi
		mv $BLDDIR/roms $BLDDIR/romsoc_$setup
	fi
	rm -f $SRCDIR/include/cppdefs.h
done

