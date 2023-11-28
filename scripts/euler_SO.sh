#!/bin/sh
# create sbatch queue script and submit it
#
# M. Munnich 2009-11

# USAGE:
#   romsjob.sh [-rej] [-m <n>] [-i <ini-file>]

# romsjob.sh submits a sbatch job of roms.
# with on argument it will restart the job maxrun times
#
# the 2nd arg specifies the continuation uns next job number

# Usually two job are in the queue on running, the next one
# wating in the queue for the first to finish. 


# run from a finished chain job add a 3rd argument to flag
# the the first job should not wait for its predesessor. 

#-------job parameters---------------
# set -x


run=spinup_std_v2  # run tag
tag=SO_d025 # model setup tag
srcdir=/cluster/home/mhague/roms_src_ethz-master/src

runtag=${run}_$tag
freq=monthly

tiling=16x36

# ROMS model input files
in_dir=/nfs/kryo/work/mhague/ROMS/input_AH/$tag/    # linked later to "$rundir/Input"
out_dir=/nfs/meso/work/mhague/ROMS/output/$tag/$runtag     
kryo_dir=${out_dir}
scr_dir=/cluster/scratch/mhague/${run}/
fin_grd=$in_dir${tag}_grd/${tiling}/${tag}_grd.nc # Grid file
fin_ini0=/nfs/kryo/work/mhague/ROMS/input/new_ini/SO_d025/hindcast/${tiling}/SO_d025_ini.nc
fin_bry=/nfs/kryo/work/mhague/ROMS/input/SO_d025/hindcast/${tiling}/SO_d025_bry.nc
fin_tsrc=$in_dir${tag}_tsrc/${tiling}/${tag}_tsrc_1979_rst.nc

#------functions ---------------------------------------

function set_params() {
#-------derived parameters---------------
if [[ $job > 1 ]] ; then
   jobtag=$runtag$job
else
   jobtag=$runtag
fi
scriptdir=`pwd`
#rundir=/nfs/kryo/work/martinfr/Roms/Output/$tag/$runtag
rundir=$out_dir
jobscript=$jobtag.sbatch # batch script name
infile=$jobtag.in
outfile=$jobtag.out

outfile_join=${jobtag}_ncjoin.out

joinscript=${jobtag}_ncjoin.sbatch
echo jobtag $jobtag, jobscript: $jobscript, joinscript: $joinscript
}

args=$*
script=`basename $0`

function parse_command_line() {
#
#defaults:
exact_restart=y
nstart=2
job=1
#

# get command line options
#
while getopts "e:r:i:j:m:wch" opt $args ; do
   case $opt in
      i)
         fin_ini=$OPTARG
         ;;
      e)
         exact_restart=$OPTARG
         if [[ $exact_restart == 'y' ]] ; then
            nstart=2
         else
            nstart=1
         fi
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
      c)
         compile=1
         ;;
      h)
         echo romsjob USAGE:
         echo  romsjob -[ijmwcrh]
         echo        '-i <ini-file>, '
         echo        '-e <n|y>, try (y, default) do not try (n) exact restart'
         echo        '-j <N>, job number in a sequence'
         echo       '-m <M>:  max number of runs in sequence'
         echo       '-w : wait for previous job to finish'
         echo       '-c : force compile'
         echo       '-h: This help line'
         exit
         ;;
   esac
done

# Set year of run
yr=$((0+${job}))
if [$job == 1] ; then
        yrb=0
else
        yrb=$((${job}-1))
fi

# Check if leap year and set ntimes for integration
#if  [[ $(($yr % 4)) == 0 ]] ; then
#   ntimes=19764 #52704 
#   navg=1647 #4383 #144
#else
ntimes=19710
navg=1642 #144


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
echo "   # of chained jobs: $maxrun" 
echo "   job number: $job"
echo "   fin_ini: $fin_ini"
echo "   wait for prior job to finish (0/1): $wt"
echo "   recompile roms (0/1): $compile"
echo "   run tag: $run"
echo "   exact_restart: $exact_restart"
}
#------------------------------------------------------------------------------------------------
function set_next_job() {
# optional command to submit the next batch job from within
# the current batch job
  if (( "$job" < $maxrun ))  ; then
    let jobpp=$job+1
    next_job_comment="# submit continuation run"
    next_job="./$script -w -m $maxrun -j $jobpp"
  fi
}
#------------------------------------------------------------------------------------------------
function set_join_job() {
# optional command to submit the ncjoin job from within
# the current batch job
  join_job_comment="# submit ncjoin job"
  join_job="$joinscript"
}
#------------------------------------------------------------------------------------------------

function set_wait_job() {
#  optionally specify to wait for a job to finish
  if [[ $wt == 1 ]] ; then
    let waitjob=$job-1
    if (( $waitjob  ==  1 )) ; then
      #jobid="$(sbatch --parsable -J $runtag)" 
      sbatch_wait='SBATCH -d "afterany:'$jobid'"'
    else
      #jobid="$(sbatch --parsable -J $runtag$waitjob)"
      sbatch_wait='SBATCH -d "afterany:'$jobid'"'
    fi
  fi
  if (( $job  ==  1 )) ; then
    #jobid_join="$(sbatch --parsable -J $runtag)"
    join_wait='SBATCH -d "afterany:'$jobid_join'"'
  else
    #jobid_join="$(sbatch --parsable -J $runtag$job)"
    join_wait='SBATCH -d "afterany:'$jobid_join'"'
  fi
}
#------------------------------------------------------------------------------------------------

function set_ini_file() {
# Set initial file
# if (( $job < 2 )) ; then
if [[ $fin_ini == '' ]] ; then
   # Start number of initial file
   inis=-2
   dini=2  # step ini file number by this 
   # compute restart file number
   let "ini = $inis + $dini*($job-1)"
   # Padd ini left to 6 digits
   if (( 1$ini < 20 ))  ; then
      ini=0000$ini
   elif (( 1$ini < 200 ))  ; then
      ini=000$ini
   elif (( 1$ini < 2000 ))  ; then
      ini=00$ini
   fi
   #fin_ini=../rst/${tag}_rst.$ini.nc
   fin_ini=${kryo_dir}/rst/${tag}_rst_${yrb}.$ini.nc
fi
# fi
}
#------------------------------------------------------------------------------------------------

function set_frc_files() {
    fin_frc=${in_dir}/SO_d025_frc/${tiling}/SO_d025_frc_norm_radcorr_2003_cryocorr_rivercorr_woa_restoring.nc
    fin_river=Input/frc/${tiling}/${tag}_river_NPload.nc
    fin_ndep=Input/frc/${tiling}/${tag}_ndep_frc_clim.nc
    fin_pCO2=/nfs/kryo/work/mhague/ROMS/input/atm_pco2/${tiling}/SO_d025_pCO2_frc_1978.nc
    #fin_pCO2=${fin_frc}
}

#------------------------------------------------------------------------------------------------

function set_bry_files() {
    fin_bry=/nfs/kryo/work/mhague/ROMS/input/SO_d025/hindcast/${tiling}/${tag}_bry.nc
}

#------------------------------------------------------------------------------------------------
function get_exec() {
# compile and copy executable to run directory
# also keep source code and script in a tar-ball
#	set -x
if [ ! -d $rundir ] ; then 
  mkdir $rundir
fi
if [ ! -d $rundir/run ] ; then
  mkdir $rundir/run
fi
if [ ! -f $rundir/run/$script ] ; then
   cp $script $rundir/run/.
fi
if (( $job <= 1 )) ; then
   compile=1
fi
if [[ $compile == 1 ]] ; then
# compile and archive ROMS src
    cd $srcdir
    make -j
    if [[ ! -f roms ]] ; then
        make -j # needed if Make.depend did not exist
    fi
#   mm    make tar
    tar -c --exclude '*.o' --exclude '*.tar' -f roms.tar  *

    cp roms.tar $rundir/run/${jobtag}_src.tar 
    cp roms $rundir/run/.
# biopar namelist is obsolete by now...
#    cp biopar_bec.nml $rundir/run/.

    cd $rundir/run
    tar -rf ${jobtag}_src.tar $script
fi
cd $rundir/run
#tar -rf ${jobtag}_src.tar $script
# make link to input directory
if [ ! -L Input ] ; then 
  ln -s $in_dir Input
fi

#### ADDED BY EK
cd $kryo_dir
if [ ! -d $kryo_dir ] ; then
  mkdir $kryo_dir
fi
if [ ! -L rst ] ; then
  mkdir $kryo_dir/rst
  ln -s $kryo_dir/rst rst
fi
if [ ! -L his ] ; then
  mkdir $kryo_dir/his
  ln -s $kryo_dir/his his
fi
if [ ! -L avg ] ; then
  mkdir $kryo_dir/avg
  ln -s $kryo_dir/avg avg
fi
###### DOWN TO HERE


cd $rundir
if [ ! -d $out_dir ] ; then
  mkdir $out_dir
fi
if [ ! -L rst ] ; then 
  mkdir $out_dir/rst
  ln -s $out_dir/rst rst
fi
if [ ! -L his ] ; then 
  mkdir $out_dir/his
  ln -s $out_dir/his his
fi
if [ ! -L avg ] ; then 
  mkdir $out_dir/avg
  ln -s $out_dir/avg avg
#  for y in {1979..2019}; do
#	mkdir $out_dir/avg/$y
#  done
fi
#if [ ! -L slavg ] ; then 
#  mkdir $out_dir/slavg
#  ln -s $out_dir/slavg slavg
#fi
#if [ ! -L bgc_flux ] ; then 
#  mkdir $out_dir/bgc_flux
#  ln -s $out_dir/bgc_flux bgc_flux
#fi
#if [ ! -L phys_flux ] ; then 
#  mkdir $out_dir/phys_flux
#  ln -s $out_dir/phys_flux phys_flux
#fi
}


# ----------------------------------------------------------------------------------------
function write_infile() {
cd ${rundir}/run
# ----------- Write ROMS input file:
cat > $infile <<EOINFILE

!--------------------------------------------------------------------------
!---------- Input parameters:

title:
   SOUTHERN OCEAN 25km grid - SO_d025

time_stepping: NTIMES   dt[sec]  NDTFAST  NINFO
               $ntimes   1600     70        1     ! 1 years (365 days)

S-coord: THETA_S,   THETA_B,    TCLINE (m)
           10.0d0      4.0d0       450.d0
rho0:
      1027.

lateral_visc:   VISC2,
                 0.

tracer_diff2: TNU2(1:NT)           [m^2/sec for all]
              0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0.

bottom_drag:     RDRG [m/s],  RDRG2,  Zob [m]
                 0.d0      0.d0     0.02    

! Zob roughly following Cheng et al. 1999 (JGR) and Kumar et al. 2015 (JPO)

gamma2:
                 1.d0

v_sponge:  [m^2/sec]
                400.


nudg_cof: TauM2_in/out  attnM2   TauM3_in/out  TauT_in/out [days for all]
             3.  365.    0.000      3. 365.      1.  365.

ubind: Open Boundary Binding Velocity [m/s]
	0.5

!---------- Input files:

grid:  filename
       $fin_grd

forcing: filename
         $fin_frc

initial: NRREC  filename
           $nstart 
           $fin_ini

boundary: filename
          $fin_bry

tracer_source: filename
          $fin_tsrc

!climatology:
!      $fin_clm

!---------- Output files:

restart:          NRST, NRPFRST / filename
                  $ntimes  2                       
           ${kryo_dir}/rst/${tag}_rst_${yr}.nc 
! 1/2 365.25 year@dt600                  26298   2 

history: LDEFHIS, NWRT, NRPFHIS / filename
           F      1000000000      0
           ../his/${tag}_his.nc

averages: NTSAVG, NAVG, NRPFAVG / filename
            1      $navg       0            
           ${out_dir}/avg/year_${yr}/${tag}_avg_${freq}_${yr}.nc

!slice_averages: K2D, NTSSLAVG, NSLAVG, NRPFSLAVG / filename
!                42   1         144       0
!       ../slavg/${tag}_slavg.nc


!bgc_flux_histories: newfile, nwrt, nrpfhis / filename
!                      T      21900000       0
!                          ../bgc_flux/${tag}_${yr}_bflx_his.nc

!bgc_flux_averages:  newfile, ntsavg, navg, nrpfavg / filename
!                      T       1      2880       0
!                          ../bgc_flux/${tag}_${yr}_avg.nc

!phys_flux_histories: newfile, nwrt, nrpfhis / filename
!                      T       21900000       0      
!                          ../phys_flux/${tag}_${yr}_pflx_his.nc

!phys_flux_averages: newfile, ntsavg, navg, nrpfavg / filename
!                      T       1      4380       0
!                          ../phys_flux/${tag}_${yr}_pflx_avg.nc


!Variable Names:                                  T S PO4 NO3 SiO3 NH4 Fe O2 DIC ALK DOC DON DOFe DOP DOPr DONr ZooC SPC SPChl SPFe SPCaCO3 DiatC DiatChl DiatFe DiatSi DiazC DiazChl DiazFe CoccoC CoccoChl CoccoCal CocccoFe CAL  

primary_history_fields:    zeta UBAR VBAR  U  V   wrtT(1:NT)                     wrtTsed(1:NT_sed)
                             T    F    F   T  T   T T T   T   F    T   T  T  F   F   F   F   F    F   F    F    T    F   F     F    F       F       T       F      F      F       T     F      F       T       F         F      F   

primary_averages:          zeta UBAR VBAR  U  V   wrtT(1:NT)                     wrtTsed(1:NT_sed)
                             T    F    F   T  T   T T F   F   F    F   F  F  T   F   F   F   F    F   F    F    F    F     F   F    F       F       F       F      F      F       F     F      F       F       F         F      F  

!primary_slice_avg:         zeta UBAR VBAR  U  V   wrtT(1:NT)                     wrtTsed(1:NT_sed)
!                             T    F    F   T  T   T T T   T   F    T   F  T  T   T   T   F   F    F   F    F    F    F     F   F    F       F       F     F      F      F       F     F     F F F F F F F F F F F F

auxiliary_history_fields:  rho Omega  W  Akv  Akt  Aks  HBL BBL 
                            F    F    F   F    F    F    F   F      F F F F F F F F F F F F

auxiliary_averages:        rho Omega  W  Akv  Akt  Aks  HBL BBL
                            T    F    T   F    F    F    T   F      T T T T T T T T T T T T

auxiliary_slice_avg:       rho Omega  W  Akv  Akt  Aks  HBL BBL
                            F    F    F   F    F    F    F   F      F F F F F F F F F F F F


EOINFILE

}
#--------------------------------------------------------------------------

function write_jobscript() {
cd $rundir/run
# -----------BSUB script setup  begin
cat > $jobscript <<EOT
#!/bin/sh
#SBATCH --time=24:00:00          # Max compute time (wall-clock, hh:mm:ss)
#SBATCH -n 576		# Number of processors 70x8 tiling
#SBATCH --output=$jobtag.stdout 	# stdout
#SBATCH --error=$jobtag.stderr	# stderr
#SBATCH --job-name=$run           # job name
#SBATCH --constraint=EPYC_7742
#SBATCH --constraint=EPYC_7H12
#SBATCH --constraint=EPYC_7763
# ----- #sbatch_wait
#
# M. Munnich 2008

# Commands:
#
echo --- submit the next year ---
$next_job_comment
$next_job

echo --- submit the ncjoin script ---
# Here the ncjoining of the output files on Euler is initialized:
#--join_job_comment
#--sbatch -d singleton join_job

echo Dir: \`pwd\` >> $outfile
date >> $outfile
echo ----Start of roms output --- >>$outfile
echo
#mpirun  --bind-to-core ./roms  $infile >>  $outfile
srun ./roms  $infile >> $outfile
echo
echo ----End of roms output --- >>$outfile
date >> $outfile

echo ---- Syncing run folder --- >>$rundir/run/$outfile
# Copy rundir to kryo
rsync -av  $rundir/run ${kryo_dir}/
date >> $outfile

EOT

}

# -----------------------------------------------------------------------------
function write_joinscript() {
cd $rundir/run
# -----------BSUB script setup  begin
cat > $joinscript <<EOT
#!/bin/sh
#SBATCH --time=4:00:00          # Max compute time (wall-clock, hh:mm)
#SBATCH -n 24             # Number of processors for joining the tiles
#SBATCH --output=${jobtag}_ncjoin.stdout         # stdout
#SBATCH --error=${jobtag}_ncjoin.stderr # stderr
#SBATCH --job-name=$run           # job name
#-----#join_wait
# M. Munnich 2008


# NCJOIN the avg file-tiles
echo ------ joining .nc files ----- >> $rundir/run/${outfile_join}
cd ${scr_dir}${yr}/
/cluster/home/muennicm/bin/ncjoin ${tag}_avg_${freq}_${yr}.*.nc >> $rundir/run/${outfile_join}
date >> $rundir/run/${outfile_join}

mv ${tag}_avg_${freq}_${yr}.nc ${kryo_dir}/avg/

EOT

}

#--------------------------------------------------------------------------

#------Prepare batch job and run it----------
parse_command_line
set_params
get_exec
#set_wait_job
set_next_job
set_join_job
set_ini_file

set_frc_files
set_bry_files

write_infile
write_jobscript
#write_joinscript

# Copy rundir to kryo
rsync -av  $rundir/run $out_dir/

# Submit sbatch job to queue
sbatch -d singleton $jobscript
# These following lines were suggested by Matt to also start the joining script
#sleep 5
#bsub < $joinscript

# testing: bash $jobscript


