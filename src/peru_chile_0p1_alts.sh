#!/bin/sh
# create bsub queue script and submit it
#
# M. Munnich 2009-11

# USAGE:
#   $0 [-wch] -s <first_year> -f <final_year> [-e <y|n>] [-j <n] [-m <m>] [-i <ini-file>]

# submit a bsub job of roms.
#
# Usually two job are in the queue one running, the next one
# waiting in the queue for the first to finish. 

# ACF: Script used to run the timeslices and adapted to telescopic. 

#-------job parameters adjust these---------------
args=$*
script=`basename $0`
nrun=60 # number of mpiruns in one job

tag=peru_chile_0p1  # model setup tag
run=$tag'_NYF_optsel4'
run_dir=$SCRATCH/RUNS/$run
tiling=30x16
ncores=480
nodes=20
taskpnode=24
srcdir=$HOME/roms_src_ethz_NitrOMZ/src
dir_frc=$SCRATCH/RUNS/frc/$tag/$tiling
#dir_clm=Input
in_dir=$SCRATCH/RUNS/frc/$tag/$tiling


#----------------------------------

function parse_command_line() {
#
#defaults:
exact_restart=y
first_yr=2000
final_yr=2060
# Time requested per job:
job_hours=48
job_no=2d
compile=1
#
# get command line options
#
while getopts "e:s:f:i:J:j:l:t:w:ch" opt $args ; do
   case $opt in
      c)
         compile=1
         ;;
      e)
         exact_restart=$OPTARG
         if [[ $exact_restart == 'y' ]] ; then
            nstart=2
         else
            nstart=1
         fi
         ;;
      f)
         final_yr=$OPTARG
         ;;
      i)
         fin_ini=$OPTARG
         ;;
      j)
         job_no=$OPTARG
         ;;
      s)
         first_yr=$OPTARG
         ;;
      t)
         job_hours=$OPTARG
         ;;
      w)
         wait_job=$OPTARG
         ;;
      h)
         echo romsjob USAGE:
         echo  romsjob -[cefijmstwh]
         echo       '-c : force compile'
         echo       '-e <n|y>, try (y, default) do not try (n) exact restart'
         echo       '-f <year>:  final year'
         echo       '-i <ini-file>, '
         echo       '-j <LSF job-number >, '
         echo       '-s <year>:  first year'
         echo       '-t <job_hours>, '
         echo       '-w : wait for previous job to finish'
         echo       '-h: This help line'
         exit
         ;;
   esac
done

let last_yr=$first_yr+$nrun-1
if (( $last_yr > $final_yr )) ; then
    last_yr=$final_yr
fi
run_name=${run}_${first_yr}_${last_yr}

echo "Selected script options:"
echo "   first year: $first_yr"
echo "   final year: $final_yr"
echo "   Wait for LSF job to finish: $wait_job"
echo "   recompile roms (0/1): $compile"
echo "   run tag: $run"
echo "   job-hours: $job_hours"
echo "   exact_restart: $exact_restart"
echo "   run_name: $run_name"

}

#----------------------------------

function set_next_job() {
# optional command to submit the next batch job from within
# the current batch job
  if (( $first_yr < $final_yr ))  ; then
    let jobpp=$job_no+1
    next_job_comment="# submit continuation run"
    next_job="$script -w $run_name -s $next_yr -f $final_yr -t $job_hours -J $jobpp"
  else
    next_job_comment='# Final LSF job'
    next_job=''
  fi
}

#----------------------------------

function set_wait_job() {
#  optionally specify to wait for a job to finish
    if [[ $wait_job != '' ]]  ; then
      bsub_wait='BSUB -w "done('$wait_job')"'
    else
      bsub_wait=''
    fi
}

#------------------------------------------------------------------------------------------------

# Turn on for the spinup
function set_frc_files() {
    fin_pCO2=${dir_frc}/${tag}_era_1979-2016_clim_frc_reduced.nc
    fin_sms=${dir_frc}/${tag}_365days_1979_normal_frc_corr_dfs_river.nc
}

# Turn on for the hindcast simulation
#function set_frc_files() {
#    fin_pCO2=${dir_frc}/8x48/humpac15_pCO2_frc_1979-2016.nc
#    fin_sms=${dir_frc}/8x48/humpac15_1day_1979-2016_frc_sms.nc
#    fin_shf=${dir_frc}/8x48/humpac15_1day_1979-2016_frc_shf.nc
#    fin_swf=${dir_frc}/8x48/humpac15_1day_1979-2016_frc_swf.nc
#    fin_srf=${dir_frc}/8x48/humpac15_1day_1979-2016_frc_srf.nc
#    fin_SST=${dir_frc}/8x48/humpac15_1day_1979-2016_frc_SST.nc
#    fin_dust_iron=${dir_frc}/8x48/humpac15_clim_frc_dust_iron.nc
#    fin_SSS=${dir_frc}/8x48/humpac15_clim_frc_SSS.nc
#}

#------------------------------------------------------------------------------------------------

function get_exec() {
# compile and copy executable to run directory
# also keep source code and script in a tar-ball
if [ ! -d $run_dir ] ; then 
  mkdir $run_dir
fi
if [ ! -f $run_dir/$script ] ; then
   cp $script $run_dir/.
fi

if [[ $compile == 1 ]] ; then
# compile and archive ROMS src
    cd $srcdir
    make -j
    if [[ ! -f roms ]] ; then
        make -j # needed if Make.depend did not exist
    fi
    tar -c --exclude '*.o' --exclude '*.tar' -f roms.tar  *

    cp roms.tar $run_dir/${run_name}_src.tar 
    cp roms $run_dir/roms_$run
fi
cd $run_dir
tar -rf ${run_name}_src.tar $script
# make link to input directory
if [ ! -L Input ] ; then 
  ln -s $in_dir Input
fi
#if [ ! -L atm_pco2.dat ] ; then 
#  ln -s Input/atm_pco2.dat .
#fi
# make run_dir subdirectories
for d in rst his avg slavg bgc_flux phys_flux ASCII_input LSF_scripts; do
  if [ ! -d $d ] ; then 
    mkdir $d
  fi
done
}

#----------------------------------

function set_ini_file() {
# Find name of ini file:                                                      #
#
# The ini file name is usually something like rst/rst_for_1969_2.000000.nc.
# This function derives the file name based on the assumption that for year 1969,
# half 2 the time index to insert is 0, and is increased by 2 for every half year.
#
# The two exceptions are the first halves of 1969 and 1979, the first year after
# the spin-up. In both cases an ini file is needed. Note that the very first job
# (1969, first half) is not handled by this funtion.


  #  $1: year of run
  #  ( $2: half of year (1 or 2) )
  
  #local this_yr=$1

  ref_yr=1929
  n_ini=1
    if [[ $this_yr == 1929 ]]; then
     fin_ini="${dir_frc}/${tag}_ini_hybrid_ini.nc"
  else
     local idx idx_t
    # let "idx = -4 + 4*($this_yr-${ref_yr})" # + 2*($2-1)"
     let "idx = -2 + 2*($this_yr-${ref_yr})" # + 2*($2-1)"
     # Format idx with 5 digits:
     printf -v idx_t '%05d' $idx
     fin_ini="rst/rst_for_$this_yr.${idx_t}.nc"
     n_ini=2
  fi

}

#----------------------

function days_per_year()
{

# Determine number of days in a year

  if [ $(expr $this_yr % 4) -gt 0 ]; then
     days_per_yr=365
  elif [ $(expr $this_yr % 100) -eq 0 ]; then
     if [ $(expr $this_yr % 400) -eq 0 ]; then
        days_per_yr=366
     else
        days_per_yr=365
     fi
  else
     days_per_yr=366
  fi
}

#----------------------

function set_infile_param() {
#
# Function to generate ASCII input file for a given year:
# If it already exists, then it will be overwritten.

  # 3D time step (seconds):
  dt_sec=800

  # ASCII input file path:
  infile="$run_dir/ASCII_input/$run"
  #rm -f $infile
  # Flag indicating whether we are in spin-up phase:
  local is_spinup
  #local this_yr_2 
  # Format this_yr with 2 digits:
  #printf -v this_yr_2 '%02d' $this_yr
  if [[ $this_yr -lt 2100 ]]; then
     is_spinup=true
  else
     is_spinup=false
  fi

  # Set name of restart file:
  let next_yr=$this_yr+1
  rst_file="rst_for_${next_yr}.nc"
  # Compute number of time steps (for 360 days):
  #local nr_tstep_1y nr_tstep_1m
  let nr_tstep_1y=$(awk "BEGIN{print (86400/$dt_sec)*$days_per_yr}")
  let nr_tstep_1m=$(awk "BEGIN{print (86400/$dt_sec*$days_per_yr)/12}")
  let nr_tstep_2m=$nr_tstep_1m*2
  let nr_tstep_6m=$nr_tstep_1m*6
  let nr_tstep_1d=$(awk "BEGIN{print (86400/$dt_sec)}")
  # Compute frequency of restart output (every 6m):
  #local rst_out
  rst_out=$nr_tstep_1y
  # Determine number that's high enough so nothing is written out:
  #local no_write
  let no_write=$nr_tstep_1y*100
  # Compute frequency of average output (1y during spinup, 1m else):
  #local avg_out his_out bgc_out
  if $is_spinup; then
     avg_out=$nr_tstep_1m
     slavg_out=$no_write
     #slavg_out=$nr_tstep_3d
     his_out=$no_write
     # Output bgc and physical fluxes every month:
     #bgc_out=$no_write
     bgc_out=$no_write
     phys_out=$no_write #I added this just to try
  else
     # higher frequency output from year 1 on:
     his_out=$no_write
     #slavg_out=$nr_tstep_3d
     slavg_out=$no_write
     avg_out=$nr_tstep_1m
     bgc_out=$no_write
     phys_out=$no_write
  fi
  his_file=his_${this_yr}.nc
  avg_file=avg_${this_yr}.nc
  slavg_file=slavg_${this_yr}.nc
}

#-----------------------------------------------

function write_infile() {
cat > ${infile}_${this_yr}.in <<EOF
title:
          ${tag}, 0.25 degree resolution

time_stepping: NTIMES   dt[sec]  NDTFAST  NINFO
              $nr_tstep_1y   $dt_sec    45      100

S-coord: THETA_S,   THETA_B,    TCLINE (m)
           6.5d0      1.5d0       150.d0

grid:  filename
          $dir_frc/${tag}_grd.nc

forcing: filename
       $fin_pCO2
       $fin_sms

initial: NRREC  filename
        $n_ini
        $fin_ini

!climatology:
!2
!         Input/SAWC_clm.nc 
!         Input/SAWC_clm2.nc

boundary: filename
           $dir_frc/${tag}_bry_hybrid_bry.nc

restart:          NRST,  NRPFRST / filename
                  $rst_out  2 
rst/$rst_file


history: LDEFHIS, NWRT, NRPFHIS / filename  
            T     $his_out  0
his/$his_file


averages: NTSAVG, NAVG, NRPFAVG / filename
            1     $avg_out  0 
avg/$avg_file

slice_averages: K2D, NTSSLAVG, NSLAVG, NRPFSLAVG / filename
                 42      1     $slavg_out    0
slavg/$slavg_file

!Variable Names:                                  T S PO4 NO3 SiO3 NH4 Fe O2 DIC ALK DOC DON DOFe DOP DOPr DONr ZooC SPChl SPC SPFe SPCaCO3 DiatChl DiatC DiatFe DiatSi DiazChl DiazC DiazFe NO2 N2O_AO1 N2O_AO2 N2O_SIDEN N2O_SODEN N2 N2O_ATM N2O N2_SED N2O_NEV
primary_history_fields:    zeta UBAR VBAR  U  V   wrtT(1:NT)                     wrtTsed(1:NT_sed)
                             T    T    T   T  T   T T F   F   F    F   F  F  F   F   F   F   F    F   F    F    F    F     F   F    F       F       F     F      F      F       F     F      F       F     F      F      F       F     F     F   F    F      F

auxiliary_history_fields:  rho Omega  W  Akv  Akt  Aks  HBL
                            F   T     F   F    T    T    T   F    F   F  F  F   F   F   F   F    F   F    F    F    F     F   F    F       F       F     F      F      F       F     F       F       F     F      F      F       F     F     F   F    F      F

primary_averages:          zeta UBAR VBAR  U  V   wrtT(1:NT)                     wrtTsed(1:NT_sed)
                             T    T    T   T  T   T T T   T   T    T   T  T  T   T   T   T   F    T   T    T    T    T     T   F    T       T       T     F      T      T       T     F      T       T     T      T       T      T      T    T   T   T       T

primary_slice_avg:         zeta UBAR VBAR  U  V   wrtT(1:NT)                     wrtTsed(1:NT_sed)
                             T    F    F   T  T   T T T   T   F    T   F  T  T   T   T   T   F    T   T    T    T    T     T   F    T       T       T     F      T      T       T     F      F       F     F      F      F       F     F     F   F    F      F

auxiliary_averages: rho Omega  W  Akv  Akt   Aks  HBL
                     F   F     T   F    T     F    T  F    F   F  F  F   F   F   F   F    F   F    F    F    F     F   F    F       F       F     F      F      F       F     F

auxiliary_slice_avg: rho Omega  W  Akv  Akt  Aks  HBL
                     F   F     F   F    F    F    T  F    F   F  F  F   F   F   F   F    F   F    F    F    F     F   F    F       F       F     F      F      F       F     F

bgc_flux_histories: newfile, nwrt, nrpfhis / filename
                      T      $no_write     0
bgc_flux/bgc_flux_his_${this_yr}.nc

bgc_flux_averages:  newfile, ntsavg, navg, nrpfavg / filename
                      T       1      $avg_out   0
bgc_flux/bgc_flux_avg_${this_yr}.nc

phys_flux_histories: newfile, nwrt, nrpfhis / filename
                      T      $no_write     0
phys_flux/phys_flux_his_${this_yr}.nc

phys_flux_averages:  newfile, ntsavg, navg, nrpfavg / filename
                      T       1      $phys_out   0
phys_flux/phys_flux_avg_${this_yr}.nc

rho0:
       1000.0d0

lateral_visc:   VISC2,   
                 0.     

tracer_diff2: TNU2(1:NT)           [m^2/sec for all]
              0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0.

bottom_drag:     RDRG [m/s],  RDRG2,  Zob [m],  Cdb_min, Cdb_max
                 3.0E-04      0.d0     0.E-4     1.E-4    1.E-2

gamma2:
                 1.d0

sponge:          N_SPONGE [grid points], V_SPONGE [m^2/sec]
                  12                      200.

nudg_cof: TauM2_in/out  attnM2   TauM3_in/out  TauT_in/out [days for all]
             3  360    0.0      3. 360.      1.  360.

ubind:  binding velocity [m/s]
            0.5 

EOF
}

#--------------------------------------------------------------------------

function write_jobscript() {

  # Name of jobs (to be used as argument to BSUB -J)
  # Flag indicating whether we are in spin-up phase:
  local is_spinup
  #local this_yr_2
  # Format this_yr with 2 digits:
  #printf -v this_yr_2 '02d' $this_yr
  if [[ $this_yr -lt 1929 ]]; then
     is_spinup=true
  else
     is_spinup=false
  fi

  # Path to run script:
  jobscript=${run_dir}/LSF_scripts/${run_name}.lsf


  # ROMS executable
  # Use roms_tr_$tiling, to compute bgc fluxes even during spinup:
  local ROMS_exe
  ROMS_exe=roms_$run

  if [[ -f $srcdir/roms ]] ; then
    mv $srcdir/roms $srcdir/$ROMS_exe
  else 
    cp -p $srcdir/$ROMS_exe .
  fi
    cp -p $srcdir/$ROMS_exe $run_dir/

  outfile="out.${run_name}"

  printf -v hh '%02d' $job_hours # hours 2-digits format

  cat > $jobscript <<EOF
#!/bin/bash
#SBATCH -A cla181
#SBATCH --job-name="$run"
#SBATCH --output="$run.out"
#SBATCH --partition=compute
#SBATCH --nodes=$nodes
#SBATCH --ntasks-per-node=$taskpnode
#SBATCH --export=ALL
#SBATCH -t 48:00:00
#SBATCH --mail-user=yangsi
#SBATCH --mail-type=begin  # email me when the job starts
#SBATCH --mail-type=end    # email me when the job finishes


  cd     $run_dir

  i=$first_yr 
  while [ \$i -le $last_yr ]; do
  	mpirun -np $ncores ./roms_$run ./ASCII_input/${run}_\$i.in > ${run}_\$i.out
  	let i+=1
  done

EOF

}
#--------------------------------------------------------------------------
#--------------------------------------------------------------------------


#------Prepare batch job and run it----------
parse_command_line
get_exec

for this_yr in `seq $first_yr $last_yr` ; do 
    # this_yr is used in all of these functions
    days_per_year
    set_infile_param
    set_ini_file
    set_frc_files
    write_infile
done
# set_wait_job
# set_next_job
write_jobscript

# Submit batch job to queue
# bsub < $jobscript
