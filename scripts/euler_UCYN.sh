#!/bin/sh
# create ROMS ascii input file and queue script and submit it
#
# M. Munnich 2009-11

# USAGE:
#   romsjob.sh [-rej] [-m <n>] [-i <ini-file>]

# Usually two job are in the queue one running, and
# another one waiting in the queue for the first to finish.


# run from a finished chain job add a 3rd argument to flag
# the the first job should not wait for its predesessor.

#-------job parameters---------------

run=amacanc_notion_5PFTs_test6_daily

# Remarks to be include in the title string
remarks="ROMS + BEC spinup"

# title string in netCDF output:
titl="Run: $run; Remarks: $remarks; forcing: ERA-5, SODA, WOA13"

tag=amacanc_Lp576
tiles=576  #tiling nb in the folder name
let ncpus=16*36

# ROMS model input files
in_dir=/cluster/home/dlouchar/meso_work/dlouchar/ROMS/inputs/NOTION
in_dir2="$HOME/meso_work/dlouchar/ROMS/outputs/NOTION"
srcdir=/cluster/home/dlouchar/uphome/ROMS/roms_src_ethz/UCYN_build

fin_bry=$in_dir/t${tiles}/${tag}_bry.nc
#fin_ini0=$in_dir/t${tiles}/${tag}_ini_spdom.nc
#fin_ini0=$in_dir2/amacanc_notion_spinup_new13/rst/amacanc_Lp576_rst.00019.nc
fin_ini0=$in_dir2/amacanc_notion_5PFTs_test6/rst/amacanc_Lp576_rst.00027.nc
fin_frc=$in_dir/t${tiles}/${tag}_frc.nc
fin_ndep=$in_dir/t${tiles}/${tag}_ndep_frc_clim.nc
fin_pdep=$in_dir/t${tiles}/${tag}_pdep_frc_clim.nc
fin_clm=$in_dir/t${tiles}/${tag}_clm.nc
fin_NPload=$in_dir/t${tiles}/${tag}_river_NPload.nc
fin_grd=$in_dir/t${tiles}/${tag}_grd2.nc

#------functions ---------------------------------------

function set_params() {
#-------derived parameters---------------
#runtag=${run}_$tag
runtag=$run
if [[ $job > 1 ]] ; then
   jobtag=$runtag$job
else
   jobtag=$runtag
fi
scriptdir=`pwd`
#rundir=/nfs/briza/work/dlouchar/ROMS/outputs/$tag/$runtag
rundir=$HOME/meso_work/$USER/ROMS/outputs/NOTION/$runtag
#rundir=$HOME/scratch/ROMS/$tag/$runtag
#outdir=/nfs/briza/work/dlouchar/ROMS/outputs/$tag/$runtag
outdir=$HOME/meso_work/$USER/ROMS/outputs/NOTION/$runtag
#mkdir $outdir2
#outdir=$HOME/scratch/ROMS/$tag/$runtag
jobscript=$jobtag.slurm # batch script name
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
   dini=8  # step ini file number by this
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
    fin_frc=$in_dir/t${tiles}/${tag}_frc.nc
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
       tar -Jc --exclude '*.o' --exclude '*.tar.xz' -f $rundir/run/roms_src.tar.xz /cluster/home/dlouchar/uphome/ROMS/roms_src_ethz/src
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
   $title

time_stepping: NTIMES   dt[sec]  NDTFAST  NINFO
                933120   200       71     48    ! 6 y (360d)

S-coord: THETA_S,   THETA_B,    TCLINE (m)
           7.5d0      1.7d0       150.d0

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

gamma2:
                 1.d0

v_sponge:          V_SPONGE [m^2/sec]
                   200.

sponge:          N_SPONGE [-],    V_SPONGE [m^2/sec]
                    40            200.                     !N_SPONGE was 20 and 400

nudg_cof: TauM2_in/out  attnM2   TauM3_in/out  TauT_in/out [days for all]
             1    2    0.000        1. 2.      1. 15.
ubind:  binding velocity [m/s]
            0.5

!---------- Input files:

grid:  filename
       $fin_grd

forcing: filename
         $fin_frc
         $fin_NPload
         $fin_pdep
         $fin_ndep

initial: NRREC  filename
           -1
           $fin_ini

boundary: filename
          $fin_bry

climatology: filename
             $fin_clm
      $fin_clm
      $fin_clm

!---------- Output files:
restart:          NRST, NRPFRST / filename
                  155520  1
           ../rst/${tag}_rst.nc

history: LDEFHIS, NWRT, NRPFHIS / filename
           F     999999999    31
           ../his/${tag}${job}_his.nc

averages: NTSAVG, NAVG, NRPFAVG / filename
            1      864    180
           ../avg/${tag}${job}_avg.nc

slice_averages: K2D, NTSSLAVG, NSLAVG, NRPFSLAVG / filename
                42   1         9999999999       365
       ../slavg/${tag}${job}_slavg.nc

!bgc_flux_histories: newfile, nwrt, nrpfhis / filename
!                      T      21900000       0
!                          ../bgc_flux/${tag}${job}_bflx_his.nc
!
!bgc_flux_averages:  newfile, ntsavg, navg, nrpfavg / filename
!                      T       1      3600      12
!                          ../bgc_flux/${tag}${job}_avg.nc
!
!phys_flux_histories: newfile, nwrt, nrpfhis / filename
!                      F       99999999   0
!                          ../phys_flux/${tag}${job}_pflx_his.nc
!
!phys_flux_averages: newfile, ntsavg, navg, nrpfavg / filename
!                      F       1      2025     24
!                          ../phys_flux/${tag}${job}_pflx_avg.nc


!Variable Names:                                  T S PO4 NO3 SiO3 NH4 Fe O2 DIC ALK DOC DON DOFe DOP DOPr DONr ZooC SPC SPChl SPFe SPCaCO3 DiatC DiatChl DiatFe DiatSi DiazC DiazChl DiazFe DUST_H POC_H PCACO3_H PSIO2_H PFE_H DUST_S POC_S PCACO3_S PSIO2_S PFE_S

primary_history_fields:    zeta UBAR VBAR  U  V   wrtT(1:NT)                     wrtTsed(1:NT_sed)
                             T    T    T   T  T   T T T   T   T    T   T  T  T   T   T   T   T    T   F    F    F    T   F     F    F       T       F       F      T      T       F     F    T   T  T   T   T  T T T  T T T T T T T T T T T T T T  T T T T T T T

primary_averages:          zeta UBAR VBAR  U  V   wrtT(1:NT)                     wrtTsed(1:NT_sed)
                             T    T    T   T  T   T T T   T   T    T   T  T  T   T   T   T   T    T   F    F    T    T   F     F    F       T       F       F      F      T       F     F    T   F  F   F   T T T T T  F F F  F F F F F F F F F F F F F F  T T T T T T T

primary_slice_avg:         zeta  UBAR  VBAR  U   V   wrtT(1:NT)
                             T     T     T   T   T   T  T   T T T T T T T T T T T T T T T T T T T T T T T T T T T T T T T

auxiliary_history_fields:  rho Omega  W  Akv  Akt  Aks  HBL  HBBL  Heatflx  RestflxTemp  RestflxSalt
                            F    F    F   F    F    F    F    F       F          F           F   F F F F F F

auxiliary_averages:        rho Omega  W  Akv  Akt  Aks  HBL  HBBL  Heatflx  RestflxTemp  RestflxSalt
                            T    T    T   T    T    T    T    T       T          F           F  F F F F F F

!auxiliary_slice_avg:       rho Omega  W  Akv  Akt  Aks  HBL  HBBL
!                            F    F    F   F    F    F    F    F  F F F F F F


EOINFILE
}
#--------------------------------------------------------------------------

function write_jobscript() {
    cd $rundir/run
    # -----------SBATCH script setup  begin
cat > $jobscript <<EOT
#!/bin/sh
#SBATCH --time=120:00:00  # max cpu-time (hh:mm)
#SBATCH -n 576     # no-of-cores
#SBATCH -o $jobtag.stdout  # job stdout
#SBATCH -e $jobtag.stderr  # job stderr
#SBATCH -J $jobtag  # job name

# Commands:
#

module list >> $outfile
which nf-config >> $outfile
which nc-config >> $outfile


echo Dir: \`pwd\` >> $outfile
date >> $outfile
echo ----Start of roms output --- >>$outfile
echo
mpirun ./roms  $infile >>  $outfile
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
write_infile
write_jobscript

#cd $rundir/run
# Submit batch job to queue
#sbatch < $jobscript

