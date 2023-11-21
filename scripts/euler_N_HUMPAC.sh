#!/bin/sh
# create ROMS ascii input file and queue script
#
# M. Munnich 2009-11

# USAGE:
#   romsjob.sh [-rej] [-m <n>] [-i <ini-file>]

# Usually two job are in the queue one running, and
# another one waiting in the queue for the first to finish.


# run from a finished chain job add a 3rd argument to flag
# the the first job should not wait for its predesessor. 

#-------job parameters---------------


run=UCLA_params  # runtag
tag=humpac15_Ncycle # model setup tag
srcdir=/cluster/home/jahaerri/src/ETH_params_debug/roms_src_ethz/N_HUMPAC_Ncycle_build #this points to the compiled UCLA version
runtag=${run}_$tag
outfreq=monthly

tiling=8x48
let ncpus=8x48

# ROMS model input files
in_dir=/nfs/kryo/work/jahaerri/roms/inputs/$tag/$run    # linked later to "$rundir/Input"
outdir=/cluster/scratch/jahaerri/roms/output/$tag/ETH_params_8/${outfreq}     #/nfs/kryo/work/koehne/roms/output/$tag/hindcast_1979_2016/$runtag/$outfreq
fin_grd=$in_dir/grd/${tiling}/humpac15_grd.nc # Grid file
#fin_ini0=$in_dir/bec2phys_rst/${tiling}/newsrc_T168_${tag}_rst_bec2phys_10yrs_halfDORG.nc
#fin_ini0=$in_dir/bec2phys_rst/${tiling}/spinup_r004_humpac15_rst.nc
fin_ini0=/nfs/kryo/work/jahaerri/roms/inputs/humpac15_Ncycle/ETH_params/ini/8x48/spinup_r103_humpac15_rst_bec2phys_timereset.nc #spinup_r015_humpac15/hc_ini/humpac15_rst.00018.nc
#fin_bry=$in_dir/bry/${tiling}/humpac15_bry_replaced.nc


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
rundir=$outdir
jobscript=$jobtag.slurm # slurm script name
infile=$jobtag.in 
outfile=$jobtag.out
echo jobtag $jobtag, jobscript: $jobscript
}

args=$*
script=`basename $0`

function parse_command_line() {
#
#defaults:
exact_restart=y
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
         if [[ $job == 1 ]] ; then
            nstart=1
         else
            nstart=2
         fi
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
         echo       '-i <ini-file>, '
         echo       '-e <n|y>, try (y, default) do not try (n) exact restart'
         echo       '-j <N>, job number in a sequence'
         echo       '-m <M>:  max number of runs in sequence'
         echo       '-w : wait for previous job to finish'
         echo       '-c : force compile'
         echo       '-h: This help line'
         exit
         ;;
   esac
done

# Set year of hindcast
yr=$((1968+${job}))

# Check if leap year and set ntimes for integration
if  [[ $(($yr % 4)) == 0 ]] ; then
   ntimes=2   #210240 #52560 #52704 
   navg=4380 #4383 #144 for daily output 144
else
   ntimes=2 #210240 #52560
   navg=4380 #144
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
   fin_ini=../rst/${tag}_rst.$ini.nc
fi
# fi
}
#------------------------------------------------------------------------------------------------

function set_frc_files() {
#    fin_frc=Input/frc/normalyear/normalyear_all_corrected/${tiling}/humpac15_1day_365days_2001_normal_frc_double_noCO2_smscycle365_1969.nc 
#    fin_river=Input/frc/${tiling}/${tag}_river_NPload.nc
#    fin_ndep=Input/frc/${tiling}/${tag}_ndep_frc_clim.nc
#    fin_pCO2=Input/frc/normalyear/normalyear_all_corrected/${tiling}/humpac15_1day_365days_2001_normal_frc_shifted.nc
    fin_frc=$in_dir/frc/normalyear/normalyear_all_corrected/${tiling}/humpac15_1day_365days_2001_normal_frc_double_noCO2_smscycle365_1969.nc 
    fin_river=$in_dir/frc/${tiling}/humpac15_river_NPload.nc
    fin_ndep=$in_dir/frc/${tiling}/humpac15_ndep_frc_clim.nc
    fin_pCO2=$in_dir/frc/normalyear/normalyear_all_corrected/${tiling}/humpac15_1day_365days_2001_normal_frc_double_1969.nc
}

#------------------------------------------------------------------------------------------------

function set_bry_files() {
    #fin_bry=Input/bry/yearly/${tiling}/${tag}_bry_${yr}.nc
    #fin_bry=Input/bry/yearly/${tiling}/humpac15_bry_replaced.nc
    fin_bry=$in_dir/bry/${tiling}/humpac15_bry_replaced.nc
}

#------------------------------------------------------------------------------------------------
function get_exec() {
# compile and copy executable to run directory
# also keep source code and script in a tar-ball
#	set -x
if [ ! -d $rundir/run ] ; then
      mkdir -p $rundir/run
fi
    if [ ! -f $rundir/$script ] ; then
   cp $script $rundir/run/.
       # Archive script
       tar -Jc --exclude '*.o' --exclude '*.tar.xz' -f $rundir/run/roms_src.tar.xz $script
fi
if (( $job <= 1 )) ; then
   compile=1
fi
if [[ $compile == 1 ]] ; then
# compile and archive ROMS src
    cd $srcdir
#        make -j 12
#        if [[ ! -f roms ]] ; then
#            make -j 12 # needed if Make.depend did not exist
#        fi
    cp roms $rundir/run/.

    #  Add src, .git to archive
        cd ..
#        tar -Jc --exclude '*.o' --exclude '*.tar.xz' -f $rundir/run/roms_src.tar.xz src .git
fi
cd $rundir/run
    # make link for passive tracer
#    ln -s $in_dir/originalfiles/ad_tracer_001.nc ad_tracer_001.nc
 #   ln -s $in_dir/originalfiles/ad_tracer_002.nc ad_tracer_002.nc
# make link to input directory
if [ ! -L Input ] ; then 
  ln -s $in_dir Input
fi
   # if [ ! -L Input2 ] ; then #      ln -s $in_dir2 Input2
   #   ln -s $in_dir2 Input2
   # fi

    # Create output subdirectories for each type of output
cd $rundir
    for dir in rst his avg slavg phys_flux # bgcflux
    do
      if [[ ! -d $dir ]] && [[ ! -L $dir ]] ; then
        mkdir -p $outdir/$dir
        if [[ $outdir != $rundir ]] ; then
          ln -s $outdir/$dir $dir
fi
fi
    done
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
         $fin_frc
         $fin_river
         $fin_ndep
         $fin_pCO2

initial: NRREC  filename
           $nstart 
           $fin_ini

boundary: filename
          $fin_bry

!climatology: filename
!             $fin_clm

!---------- Output files:
restart:          NRST, NRPFRST / filename
                  210240  1
           ../rst/${tag}_rst.nc

history: LDEFHIS, NWRT, NRPFHIS / filename
           T       1      5
           ../his/${tag}_${yr}_his.nc

averages: NTSAVG, NAVG, NRPFAVG / filename
            1      $navg       12            
           ../avg/${tag}_${yr}_avg.nc

!slice_averages: K2D, NTSSLAVG, NSLAVG, NRPFSLAVG / filename
!                64   1         144       12
!       ../slavg/${tag}_${yr}_slavg.nc


!bgc_flux_histories: newfile, nwrt, nrpfhis / filename
!                      T      21900000       0
!                          ../bgc_flux/${tag}_${yr}_bflx_his.nc

!bgc_flux_averages:  newfile, ntsavg, navg, nrpfavg / filename
!                      T       1      4380       0
!                          ../bgc_flux/${tag}_${yr}_avg.nc

!phys_flux_histories: newfile, nwrt, nrpfhis / filename
!                      T       21900000       0      
!                          ../phys_flux/${tag}_${yr}_pflx_his.nc

!phys_flux_averages: newfile, ntsavg, navg, nrpfavg / filename
!                      T       1      4380       0
!                          ../phys_flux/${tag}_${yr}_pflx_avg.nc


!Variable Names:                                  T S PO4 NO3 SiO3 NH4 Fe O2 DIC ALK DOC DON DOFe DOP DOPr DONr ZooC SPC SPChl SPFe SPCaCO3 DiatC DiatChl DiatFe DiatSi DiazC DiazChl DiazFe NO2 N2 N2O N2O_AO N2O_SI N2O_SO N2O_AT N2_SedN2

primary_history_fields:    zeta UBAR VBAR  U  V   wrtT(1:NT)                     wrtTsed(1:NT_sed)
                             T    F    F   T  T   T T T   T   T    T   T  T  T   T   F   F   F    F   F    F    F    F   F     F    F       F       F       F      F      F       F     F      T  T   T    T     T       T     T       T        F       F

primary_averages:          zeta UBAR VBAR  U  V   wrtT(1:NT)                     wrtTsed(1:NT_sed)
                             T    F    F   T  T   T T T   T   T    T   T  T  T   T   F   F   F    F   F    F    F    F   F     F    F       F       F       F      F      F       F     F      T  T   T    T     T       T     T       T        F       F

!primary_slice_avg:         zeta UBAR VBAR  U  V   wrtT(1:NT)                     wrtTsed(1:NT_sed)
!                             T    F    F   T  T   T T T   T   F    T   F  T  T   T   T   F   F    F   F    F    F    F     F   F    F       F       F     F      F      F       F     F     F F F F F F F F F F F F

auxiliary_history_fields:  rho Omega  W  Akv  Akt  Aks  HBL BBL 
                            F    F    F   F    F    F    F   T      F F F F F F F F F F F F

auxiliary_averages:        rho Omega  W  Akv  Akt  Aks  HBL BBL
                            T    F    T   F    F    F    T   T      F F F F F F F F F F F F

!auxiliary_slice_avg:       rho Omega  W  Akv  Akt  Aks  HBL BBL
!                            F    F    F   F    F    F    F   F      F F F F F F F F F F F F


EOINFILE
}
#--------------------------------------------------------------------------

function write_jobscript() {
cd $rundir/run
    # -----------SBATCH script setup  begin
cat > $jobscript <<EOT
#!/bin/sh
#SBATCH --time=04:00:00  # max cpu-time (hh:mm)
#SBATCH -n 420     # no-of-cores
#SBATCH --mem-per-cpu=2G
#SBATCH -o $jobtag.stdout  # job stdout
#SBATCH -e $jobtag.stderr  # job stderr
#SBATCH -J $jobtag  # job name
#

# Commands:
#

module list >> $outfile
which nf-config >> $outfile
which nc-config >> $outfile


echo Dir: \`pwd\` >> $outfile
date >> $outfile
echo ----Start of roms output --- >>$outfile
echo
srun -n 384 ./roms  $infile >>  $outfile
echo
echo ----End of roms output --- >>$outfile
date >> $outfile
rsync * $outdir/run

EOT
}
#--------------------------------------------------------------------------

#------Prepare batch job and run it----------
parse_command_line
set_params
get_exec
#set_wait_job
#set_next_job
set_ini_file
set_frc_files
set_bry_files
write_infile
write_jobscript

#cd $rundir/run
# Submit batch job to queue
#sbatch < $jobscript

