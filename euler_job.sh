#!/bin/sh
# Create an chain of EULER SLURM batch jobs and submit it.
#
# M. Munnich 2009-11 2023-01

# USE
#   euler_job.sh  -h
#   for USAGE help



# run from a finished chain job add a 3rd argument to flag
# the the first job should not wait for its predesessor.

#-------job parameters---------------
# set -x
wall_clock=4 # Batch job max compute time [h], Choose 4, 24, or 120
runtag=test2  # May be overwrittgen using option "-t"
#runtag=g82om41r2  # May be overwrittgen using option "-t"
remarks="Using PACTCS/ANAMCA BEC parameters. Computing tendencies with absolute values"

# title string in netCDF output:
titl="Run name: $runtag; Remarks: $remarks; forcing: ERA-I, SODA, WOA13"

# Tag for input files:
setup=pactcs60
SETUP=PACTC60

# Base dir for running the model (model is actually run in rundir/run)
rundir=$SCRATCH/Roms/Output/$setup/$runtag
# model netCDF output is written into subdirectories under out_dir
out_dir=$rundir
# Sometime we like write the model netCDF output directly to our storage servers (kryo, meso)
#out_dir=/nfs/kryo/work/$LOGNAME/Roms/Output/$setup
store_dir/nfs/kryo/work/$LOGNAME/Roms/Output/$setup/runtag

# Source dir
srcdir=$HOME/../muennicm/ROMS/roms_src_ethz

# Build dir
#export BLDDIR=$srcdir

# Namelist file for BEC parameters:
# NOT USED: bio_nml=/nfs/malva/work/loher/ROMS/BEC_param/namelists/biopar

NP_XI=8
NP_ETA=18
tiling=${NP_XI}x${NP_ETA}
#tiling=.
let tiles=${NP_XI}*${NP_ETA}
tiles_per_core=1
let ncores=${NP_XI}*${NP_ETA}/${tiles_per_core}
echo $ncores

# Input files
#in_dir="$HOME/mmwork/roms_in/$setup/"    # linked later to "$rundir/Input"
in_dir=/cluster/work/climate/muennicm/roms_in/$setup
fin_grd=Input/grd/${tiling}/${setup}_grd.nc # Grid file
#fin_ini0=Input/ini/${tiling}/${setup}_spinup_hc_flora_y18_rst.nc
#fin_ini0=Input/ini/rest/${tiling}/${setup}_ini_from_rest.nc
fin_ini0=Input/ini/from_spinup/${tiling}/${setup}_ini_from_spinup.nc
fin_bry=Input/bry/${tiling}/${setup}_bry.nc


#------functions ---------------------------------------

function set_params() {
local runtag=$1
#-------derived parameters---------------
if (( $job > 0 )) ; then
   jobtag=$runtag$job
else
   jobtag=$runtag
fi
scriptdir=`pwd`
jobscript=$jobtag.bsub # batch script name
infile=$jobtag.in
outfile=$jobtag.out
postproc_log=${jobtag}_postproc.log
postproc_job=${jobtag}_postproc.bsub

echo '   jobtag: '$jobtag
echo '   jobscript: '$jobscript
echo '   postprocess script: ' $postproc_job
}

args=$*
script=`basename $0`

function parse_command_line() {
#
#defaults:
exact_restart=n
nstart=-1
job=1
#

# get command line options
#
while getopts "e:r:i:j:m:wchC" opt $args ; do
   case $opt in
      i)
         fin_ini=$OPTARG
         ;;
      e)
         exact_restart=$OPTARG
         if [[ $exact_restart == 'y' ]] ; then
            nstart=2
         fi
         ;;
      t)
          runtag=$OPTARG
          ;;
      j)
         job=$OPTARG
         ;;
      m)
         maxrun=$OPTARG
         ;;
      w)
         wt=1
         ;;
      C)
         compile=0
         ;;
      c)
         compile=1
         ;;
      h)
         echo romsjob USAGE:
         echo  romsjob -[ijmwcrh]
         echo       '-i <ini-file>, '
         echo       '-e <n|y>, try (y, default) do not try (n) exact restart'
         echo       '-j <N>, job number in a sequence'
         echo       '-m <M>:  max number of runs in sequence'
         echo       '-w : wait for previous job to finish'
         echo       '-C : do not  compile'
         echo       '-c : force compile'
         echo       '-t <tag>: append <tag> to run name'
         echo       '-h: This help line'
         exit
         ;;
   esac
done

if [[ $job == 1 ]] || [[ $exact_restart == 'n' ]] ; then
   nstart=1
else
   nstart=2
fi

if [[ $maxrun == '' ]] ; then
     maxrun=1
#     maxrun=2
fi
if [[ $job == '' ]] ; then
   job=1
fi
if (( "$job" <= 1 )) && [[ $fin_ini == '' ]] ; then
   fin_ini=$fin_ini0
fi
echo "Selected script options:"
echo "    # of chained jobs: $maxrun"
echo "    job number: $job"
echo "    fin_ini: $fin_ini"
echo "    wait for prior job to finish (0/1): $wt"
echo "    recompile roms (0/1): $compile"
echo "    run tag: $runtag"
echo "    exact_restart: $exact_restart"
echo
}

#------------------------------------------------------------------------------------------------
function set_fout_stump() {
  # set output file names stump
    fout=${setup}_${runtag}
}
#------------------------------------------------------------------------------------------------
function set_next_job() {
# optional command to submit the next batch job from within
# the current batch job
  if (( "$job" < $maxrun ))  ; then
    let jobpp=$job+1
    next_job_comment="# submit continuation run"
    next_job="$script -w -m $maxrun -j $jobpp"
  fi
}
#------------------------------------------------------------------------------------------------
#function set_join_job() {
# optional command to submit the ncjoin job from within
# the current batch job
  #join_job_comment="# submit ncjoin job"
  #join_job="$joinscript"
#}
#------------------------------------------------------------------------------------------------

# function set_wait_job() {
# optionally specify to wait for a job to finish
#   if [[ $wt == 1 ]] && (( "$job" > 1 )); then

#     let waitjob=$job-1
#     bsub_wait='BSUB -w "done('$runtag$waitjob')"'
#   fi
#   bsub_wait_postproc='BSUB -w "done('$runtag$job')"'
# }
#------------------------------------------------------------------------------------------------

function set_ini_file() {
# Set initial file
# if (( $job < 2 )) ; then
if [[ $fin_ini == '' ]] ; then
   # Start number of initial file
   #inis=-2
   #dini=2  # step ini file number by this
   # compute restart file number
   #let "ini = $inis + $dini*($job-1)"
   # Padd ini left to 5 digits
   #printf -v inif '%05d' $ini
   #fin_ini=../rst/${runtag}_rst.$inif.nc
   let "ini= ${job} - 1"
   fin_ini=../rst/${runtag}${ini}_rst.nc
fi
# fi
echo
echo '    Initial conditions: ' $fin_ini
}
#------------------------------------------------------------------------------------------------

function set_frc_files() {
    fin_frc=Input/frc/${tiling}/${setup}_daily_1979nyr_1979_2016clm_all_corr.nc
    fin_river=Input/rivload/${tiling}/${setup}_river_load_NP_Si_Alk_DIC.nc
    fin_ndep=Input/ndep/${tiling}/${setup}_ndep_frc_clim.nc
    fin_iron=Input/iron/${tiling}/${setup}_1day_365days_2001_normal_frc_corr_dfs_river_seaice_dust_iron_spinup.nc
echo '    Forcing: '
echo '        Atmospheric: ' $fin_frc
echo '        River:       ' $fin_river
echo '        Nutrient:    ' $fin_ndep
echo '        Iron:        ' $fin_iron
}

#------------------------------------------------------------------------------------------------

function get_exec() {
    # compile and copy executable to run directory
    # also keep source code and script in a tar-ball
    #	set -x
    rundir=$1
    echo '   Rundir: ' $rundir
    out_dir=$2
    if [ ! -d $rundir/run ] ; then
      mkdir -p $rundir/run
    fi
    if [ ! -f $rundir/$script ] ; then
       cp $script $rundir/run/.
    fi
    if (( $job <= 1 )) ; then
        if [ ! -d compile ] ; then
            compile=1
        fi
    fi
    if [ ! -f $rundir/run/roms ] ; then
    # compile and archive ROMS src
        cd $srcdir
        if [[ $compile == 1 ]] ; then
            make -j config=$SETUP
        fi
        cp build/roms $rundir/run/
        #  Archive roms executable and source
        tar -Jc --exclude '*.f' --exclude '*.o' --exclude '*.tar.xz' -f $rundir/run/roms_src.tar.xz src build/roms
    fi
    cd $rundir/run
    # make link for passive tracer
    #mm ln -s $in_dir/originalfiles/ad_tracer_001.nc ad_tracer_001.nc
    #mm ln -s $in_dir/originalfiles/ad_tracer_002.nc ad_tracer_002.nc
    # make link to input directory
    if [ ! -L Input ] ; then
      ln -s $in_dir Input
      echo '   Input/ -> ' $in_dir
      echo
    fi
    if [ ! -d $out_dir ] ; then
      mkdir -p $out_dir
    fi
    # Create output subdirectories for each type of output
    cd $rundir
    for dir in rst his avg phys_flux # bgcflux slavg
    do
      if [[ ! -d $dir ]] && [[ ! -L $dir ]] ; then
        mkdir -p $out_dir/$dir
        if [[ $out_dir != $rundir ]] ; then
          ln -s $out_dir/$dir $dir
          echo '    Output dir: ' $out_dir
        fi
      fi
    done
}

# -----------------------------------------------------------
function get_biopar_nml() {
    # Copy namelist file for BEC parameters:
    rundir=$1
    cd $rundir/run
    if [ ! -f biopar_bec.nml ] ; then
       cp -p $bio_nml .
       ln -s $bio_nml biopar_bec.nml
    fi
}


# ----------------------------------------------------------------------------------------
function write_infile() {
rundir=$1
cd ${rundir}/run
# ----------- Write ROMS input file:
cat > $infile <<EOINFILE

!--------------------------------------------------------------------------
!---------- Input parameters:

title:
   PACIFIC stretched 6.1km to 65km x2

time_stepping: NTIMES   dt[sec]  NDTFAST  NINFO
               35040    900     45        36     ! 1 years
!               192    900     45        36     ! testing 2 days?


S-coord: THETA_S,   THETA_B,    TCLINE (m)
           10.0d0      4.0d0       250.d0
!USWC           7.0d0      0.0d0       150.d0

age_dye: N_agedye
          2

rho0:
      1027

lateral_visc:   VISC2,
                 0.

tracer_diff2: TNU2(1:NT)           [m^2/sec for all]
              0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0.

bottom_drag:     RDRG [m/s],  RDRG2,  Zob [m],  Cdb_min, Cdb_max
                 0.E-04      0.0E-3   4.0E-2     1.E-4    1.E-1    !RDRG was 3.0E-04 Cdb_max was 1.E-2
!pactcs                 3.0E-04      0.d0     5.E-3     1.E-4    1.E-2

! Zob roughly following Cheng et al. 1999 (JGR) and Kumar et al. 2015 (JPO)

gamma2:
                 0.5

sponge:          N_SPONGE [-],    V_SPONGE [m^2/sec]
                   25           400.



nudg_cof: attnM2  TauT_out [days for all]
          0.004   30.

ubind: Open Boundary Binding Velocity [m/s]
	0.5

! not used at ETH by requested for COAMS bulk flux
SSS_correction: [cm/day]
        0.5

!---------- Input files:

grid:  filename
       $fin_grd

forcing: filename
         $fin_frc
         $fin_ndep
         $fin_river
         $fin_iron

initial: NRREC  filename
           $nstart
           $fin_ini

boundary: filename
          $fin_bry

!climatology:
!      $fin_clm

!---------- Output:

restart:          NRST, NRPFRST / filename
                  35040  2 !<--- yearly restart (2 for EXACT_RESTART)
           ../rst/${fout}_rst.nc  2

history: LDEFHIS, NWRT, NRPFHIS / filename   ! (write initial fields, every 5 day for dt=900, all records in 1 file)
           T      480    0
           ../his/${fout}_his.nc

averages: NTSAVG, NAVG, NRPFAVG / filename ! (from the beginning, every 30 day for dt=900, all records in 1 file) 
            1      2880       0
          ../avg/${fout}_avg.nc
!          file://../avg/${fout}_avg.za#mode=nczarr,file 

!Variable Names:                                   T S PO4 NO3 SiO3 NH4 Fe O2 DIC ALK DOC DON DOFe DOP DOPr DONr ZooC SPC SPChl SPFe SPCaCO3 DiatC DiatChl DiatFe DiatSi DiazC DiazChl DiazFe DUST_H POC_H PCACO3_H PSIO2_H PFE_H DUST_S POC_S PCACO3_S PSIO2_S PFE_S
primary_history_fields:    zeta UBAR VBAR  U  V   wrtT(1:NT)                     wrtTsed(1:NT_sed)
                              T    F    F   T  T   T T T   T   T    T   T  T  T   T   T   T   F    F   F    F    T    T   F     F    F       T             F       F      F      T       F     F      F      F     F        F       F     F      F     F        F       F

!Variable Names:                                   T S PO4 NO3 SiO3 NH4 Fe O2 DIC ALK DOC DON DOFe DOP DOPr DONr ZooC SPC SPChl SPFe SPCaCO3 DiatC         DiatChl DiatFe DiatSi DiazC DiazChl DiazFe DUST_H POC_H PCACO3_H PSIO2_H PFE_H DUST_S POC_S PCACO3_S PSIO2_S PFE_S
primary_averages:          zeta UBAR VBAR  U  V   wrtT(1:NT)                     wrtTsed(1:NT_sed)
                              T    T    T   T  T   T T T   T   T    T   T  T  T   T   T   T   F    F   F    F    T    T   F     F    F       T             F       F      F      T       F     F      F      F     F        F       F     F      F     F        F       F

auxiliary_history_fields:  rho Omega  W  Akv  Akt  Aks  HBL BBL
                             T    F    T   F    F    F    T   F      F F F F F F F F F F F F

auxiliary_averages:        rho Omega  W  Akv  Akt  Aks  HBL BBL
                             T    F    T   F    F    F    T   F      F F F F F F F F F F F F

EOINFILE
}
#--------------------------------------------------------------------------

function write_jobscript() {
rundir=$1
cd $rundir/run
# -----------BSUB script setup  begin
cat > $jobscript <<EOT
#!/bin/sh
#SBATCH --time=02:00:00       # Max compute time (hh:mm:ss)
#SBATCH -n $ncores		   # Number of processors
#SBATCH -o $jobtag.stdout	# stdout
#SBATCH -e $jobtag.stderr	# stderr
#SBATCH -J $jobtag         # job name

# old: $bsub_wait
#
# M. Munnich 2008

# Commands:
#
#$next_job_comment
#$next_job

module li

echo Dir: \`pwd\` >> $outfile
echo CPU info:
echo ---
cat /proc/cpuinfo | head -26 >> $outfile
echo ---
date >> $outfile
echo ----Start of roms output --- >>$outfile
# let nmpi="$ncores * $tiles_per_core"
echo Nunmber of MPI processes: \$nmpi
srun ./roms $infile >> $outfile
echo
echo ----End of roms output --- >>$outfile
date >> $outfile

EOT
}


#--------------------------------------------------------------------------

#------Prepare batch job and run it----------
parse_command_line
set_params $runtag
out_dir=$out_dir/$runtag
out_dir=$rundir
get_exec $rundir $out_dir
set_wait_job
set_next_job
set_ini_file

set_fout_stump
set_frc_files

write_infile $rundir
write_jobscript $rundir
# get_biopar_nml $rundir
echo Run directory: `pwd`

# Submit jobscript to queue:
sbatch  $jobscript

