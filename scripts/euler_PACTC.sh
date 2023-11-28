#!/bin/sh
# create bsub queue script and submit it
#
# M. Munnich 2009-11

# USAGE:
#   romsjob.sh [-rej] [-m <n>] [-i <ini-file>]

# romsjob.sh submits a bsub job of roms.
# with on argument it will restart the job maxrun times
#
# the 2nd arg specifies the continuation uns next job number

# Usually two job are in the queue on running, the next one
# wating in the queue for the first to finish. 


# run from a finished chain job add a 3rd argument to flag
# the the first job should not wait for its predesessor. 

#-------job parameters---------------
# set -x


run=hc006_daily  # run tag
tag=pactcs30 # model setup tag
srcdir=$HOME/Roms/src_hc006_river_load_corr/src
runtag=${run}_${tag}

tiling=8x48

# ROMS model input files
in_dir="/cluster/home/fdesmet/fdscratch/$tag/"    # linked later to "$rundir/Input"
out_dir=/cluster/scratch/fdesmet/roms/output/$tag/$runtag
kryo_dir=/nfs/meso/work/fdesmet/roms/output/$tag/$runtag
fin_grd=/cluster/home/fdesmet/fdscratch/$tag/${tiling}/grd_merged/grd/${tag}_grd.nc # Grid file
#fin_ini0=/cluster/home/fdesmet/fdscratch/$tag/${tiling}/bec2phys_rst_N64ts10tb4hc250_grd_merged_SiO3_PO4_fix/SPhys002_monthly_${tag}_rst_bec2phys_10yrs_halfDORG.nc
fin_ini0=/cluster/home/fdesmet/fdscratch/$tag/${tiling}/hindcast_1979_2016/hc_ini/hc004_ini/${tag}_rst.00018.nc
#fin_bry=/cluster/home/fdesmet/fdscratch/$tag/${tiling}/hindcast_1979_2016/bry/${tag}_bry_merged_DIC_spinup.nc
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
#rundir=$HOME/scratch/Roms/$tag/$runtag
#rundir=/nfs/kryo/work/fdesmet/roms/output/$tag/$runtag
rundir=$out_dir
jobscript=$jobtag.bsub # batch script name
infile=$jobtag.in
outfile=$jobtag.out

outfile_join=${jobtag}_ncjoin.out

joinscript=${jobtag}_ncjoin.bsub
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

# Set year of hindcast
#yr=$((1978+${job}))
yr=$((1978+${job}))

# Check if leap year and set ntimes for integration
if  [[ $(($yr % 4)) == 0 ]] ; then
   ntimes=52704
   navg=144
else
   ntimes=52560
   navg=144
fi


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
    next_job="$script -w -m $maxrun -j $jobpp"
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
      bsub_wait='BSUB -w "done('$runtag')"'
    else
      bsub_wait='BSUB -w "done('$runtag$waitjob')"'
    fi
  fi
  if (( $job  ==  1 )) ; then
    join_wait='BSUB -w "done('$runtag')"'
  else
    join_wait='BSUB -w "done('$runtag$job')"'
  fi
}
#------------------------------------------------------------------------------------------------

function set_ini_file() {
# Set initial file
# if (( $job < 2 )) ; then
if [[ $fin_ini == '' ]] ; then
   # Start number of initial file
   # because I am starting this run from T200_monthly rst.00018
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
   fin_ini=${kryo_dir}/rst/${tag}_rst.$ini.nc
fi
# fi
}
#------------------------------------------------------------------------------------------------

function set_frc_files() {
    fin_sms=Input/hcast_era5/${tiling}/sms/pactcs30_1day_1979-2019_frc_corr_sms.nc
    fin_shf=Input/hcast_era5/${tiling}/shf/pactcs30_1day_1979-2019_frc_corr_shf.nc
    fin_swf=Input/hcast_era5/${tiling}/swf/pactcs30_1day_1979-2019_frc_corr_swf.nc
    fin_srf=Input/hcast_era5/${tiling}/srf/pactcs30_1day_1979-2019_frc_corr_srf.nc
    fin_SST=Input/hcast_era5/${tiling}/SST/pactcs30_1day_1979-2019_frc_corr_SST.nc
    fin_pco2=Input/hcast_era5/${tiling}/pCO2/pactcs30_pCO2_frc_1979-2019.nc
    fin_dust_iron=Input/hcast_era5/${tiling}/dust_iron/pactcs30_1day_1979-2019_frc_corr_dust_iron.nc
    fin_SSS=Input/hcast_era5/${tiling}/SSS/pactcs30_clim_frc_SSS.nc
    fin_river=Input/hcast_era5/${tiling}/RivLoad/${tag}_river_load_NP_Si_Alk_DIC.nc
    fin_ndep=Input/hcast_era5/8x48/ndep/pactcs30_ndep_frc_clim.nc
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
    cd $rundir/run
    tar -rf ${jobtag}_src.tar $script
fi
cd $rundir/run
#tar -rf ${jobtag}_src.tar $script
# make link to input directory
if [ ! -L Input ] ; then 
  ln -s $in_dir Input
fi

#### ADDED BY EK for ncjoin on euler
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
   PACIFIC stretched 6.1km to 65km

time_stepping: NTIMES   dt[sec]  NDTFAST  NINFO
               $ntimes   600     45        36     ! 1 years (365 days)

S-coord: THETA_S,   THETA_B,    TCLINE (m)
           10.0d0      4.0d0       250.d0
!USWC           7.0d0      0.0d0       150.d0
rho0:
      1000.

lateral_visc:   VISC2,
                 0.

tracer_diff2: TNU2(1:NT)           [m^2/sec for all]
              0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0. 0.

bottom_drag:     RDRG [m/s],  RDRG2,  Zob [m],  Cdb_min, Cdb_max
                 0.d0      0.d0     4.E-3     1.E-4    1.E-2

! Zob roughly following Cheng et al. 1999 (JGR) and Kumar et al. 2015 (JPO)

gamma2:
                 0.5

sponge:          N_SPONGE [-],    V_SPONGE [m^2/sec]
                   25           400.
! sa3: blowup iic=40                   15           500.
! sa1: working!                  20           400.
! sao1 blowup after iic=40:                  15           500.
!                   10           200.



nudg_cof: attnM2  TauT_out [days for all]
          0.004   30.

ubind: Open Boundary Binding Velocity [m/s]
	0.5

!---------- Input files:

grid:  filename
       $fin_grd

forcing: filename
         $fin_sms
         $fin_shf
         $fin_swf
         $fin_srf
         $fin_SST
         $fin_dust_iron
         $fin_SSS
         $fin_pco2
         $fin_river
         $fin_ndep

initial: NRREC  filename
           $nstart 
           $fin_ini

boundary: filename
          /cluster/home/fdesmet/fdscratch/$tag/${tiling}/hindcast_1979_2016/bry/${yr}/pactcs30_bry_merged_${yr}.nc

!climatology:
!      $fin_clm

!---------- Output files:

restart:          NRST, NRPFRST / filename
                  $ntimes  2 
           ${kryo_dir}/rst/${tag}_rst.nc 
! 1/2 365.25 year@dt600                  26298   2 

history: LDEFHIS, NWRT, NRPFHIS / filename
           F      105120      0
           ../his/${tag}_${yr}_his.nc

averages: NTSAVG, NAVG, NRPFAVG / filename
            1     $navg       0
           ${kryo_dir}/avg/${tag}_${yr}_avg.nc

!slice_averages: K2D, NTSSLAVG, NSLAVG, NRPFSLAVG / filename
!                64   1         144       0
!       ../slavg/${tag}_${yr}_slavg.nc


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
!                      T       1      4383       0
!                          ../phys_flux/${tag}_${yr}_pflx_avg.nc


!Variable Names:                                  T S PO4 NO3 SiO3 NH4 Fe O2 DIC ALK DOC DON DOFe DOP DOPr DONr ZooC SPC SPChl SPFe SPCaCO3 DiatC DiatChl DiatFe DiatSi DiazC DiazChl DiazFe DUST_H POC_H PCACO3_H PSIO2_H PFE_H DUST_S POC_S PCACO3_S PSIO2_S PFE_S 

primary_history_fields:    zeta UBAR VBAR  U  V   wrtT(1:NT)                     wrtTsed(1:NT_sed)
                             T    F    F   T  T   T T T   T   F    T   T  T  F   F   F   F   F    F   F    F    F    F   F     F    F       F       F       F      F      F       F     F      F      F     F        F       F     F      F     F        F       F

primary_averages:          zeta UBAR VBAR  U  V   wrtT(1:NT)                     wrtTsed(1:NT_sed)
                             T    F    F   T  T   T T T   T   T    T   F  T  T   T   F   F   F    F   F    F    T    F   F     F    F       F     F       F      F      F       F     F      F      F     F        F       F     F      F     F        F       F

!primary_slice_avg:         zeta UBAR VBAR  U  V   wrtT(1:NT)                     wrtTsed(1:NT_sed)
!                             T    F    F   T  T   T T F   T   F    T   F  F  F   F   F   T   F    F   F    T    T    F     F   F    F       F       F     F      F      F       F     F      F      T     F        F       F     F      T     F        F       F 

auxiliary_history_fields:  rho Omega  W  Akv  Akt  Aks  HBL BBL 
                            F    F    F   F    F    F    F   T      F F F F F F F F F F F F

auxiliary_averages:        rho Omega  W  Akv  Akt  Aks  HBL BBL
                            T    F    T   F    F    F    T   F      F F F F F F F F F F F F

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
#BSUB -W 24:00          # Max compute time (wall-clock, hh:mm)
#BSUB -n 384		# Number of processors 8x48 tiling
#BSUB -o $jobtag.stdout 	# stdout
#BSUB -e $jobtag.stderr	# stderr
#BSUB -J $jobtag           # job name
#$bsub_wait
#
# M. Munnich 2008

# Commands:
#
echo --- submit the next year ---
$next_job_comment
$next_job

echo --- submit the ncjoin script ---
# Here the ncjoining of the output files on Euler is initialized:
$join_job_comment
bsub < $join_job

echo Dir: \`pwd\` >> $outfile
date >> $outfile
echo ----Start of roms output --- >>$outfile
echo
#mpirun  --bind-to-core ./roms  $infile >>  $outfile
mpirun ./roms  $infile >> $outfile
echo
echo ----End of roms output --- >>$outfile
date >> $outfile

echo ---- Syncing run folder --- >>$rundir/run/$outfile
# Copy rundir to kryo
rsync -av  $rundir/run ${kryo_dir}/
date >> $outfile

# Copy rundir to kryo
#rsync -av  $rundir/run $out_dir/

EOT

}
#--------------------------------------------------------------------------
function write_joinscript() {
cd $rundir/run
# -----------BSUB script setup  begin
cat > $joinscript <<EOT
#!/bin/sh
#BSUB -W 24:00          # Max compute time (wall-clock, hh:mm)
#BSUB -n 24             # Number of processors for joining the tiles
#BSUB -R "rusage[mem=4096]"              #Memory in MB per processor core (default is 1024MB (1GB))
#BSUB -o ${jobtag}_ncjoin.stdout         # stdout
#BSUB -e ${jobtag}_ncjoin.stderr # stderr
#BSUB -J ${jobtag}_ncjoin           # job name
#$join_wait
# M. Munnich 2008

# NCJOIN the avg files
echo ------ joining .nc files ----- >> $rundir/run/${outfile_join}
cd ${kryo_dir}/avg/
/cluster/home/muennicm/bin/ncjoin ${tag}_${yr}_avg.*.nc >> $rundir/run/${outfile_join}
echo -----.nc files joining done ---- >>$rundir/run/${outfile_join}
date >> $rundir/run/${outfile_join}

# move joined avgfile to kryo
#echo ---- Copying avg file --- >>$rundir/run/${outfile_join}
#mv ${out_dir}/avg/${tag}_${yr}_avg.nc ${kryo_dir}/avg/.
#echo ---- Copying avg file done --- >>$rundir/run/${outfile_join}
#date >> $rundir/run/${outfile_join}

EOT

}

#--------------------------------------------------------------------------

#------Prepare batch job and run it----------
parse_command_line
set_params
get_exec
set_wait_job
set_next_job
set_join_job
set_ini_file

set_frc_files

write_infile
write_jobscript
write_joinscript

# Copy rundir to kryo
rsync -av  $rundir/run $out_dir/

# Submit batch job to queue
bsub < $jobscript
# testing: bash $jobscript

