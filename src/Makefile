# Universal machine independent makefile for ROMS model 
#========== ======= =========== ======== === ==== =====
# Set machine dependent definitions and rules.

#include Makedefs.Intel
#include Makedefs.IntelEM64T
#include Makedefs.Intel_MPI
#include  Makedefs.CMPI
#include  Makedefs.Tungsten
#include Makedefs.Altix
#include Makedefs.p690
#include Makedefs.PGI
#include Makedefs.GCC
#include Makedefs.g95
#include Makedefs.Fujitsu
#include Makedefs.Gonzales.ifort
#include Makedefs.IntelBrutus_ompi
#include Makedefs.PGIBrutus_ompi
include Makedefs.UP_ETH


#                                      All source code files are 
# Configuration for SCRUM/ROMS model:  sorted into eight  groups,
#============== === ========== ======  separated by blanc lines:
#
#  1) main driving part;          5) vertical mixing schemes; 
#  2) 2D time stepping engine;    6) on-fly model diagnostic routines;
#  3) 3D time stepping engine;    7) netCDF I/O routines;
#  4) sea-water EOS routines;     8) model forcing routines; 
#
# Group 9 contains the new files for the NPZD code, group 10 for the BEC code.

KRNSRC = main.F		step2D_FB.F	read_inp.F	sta_par.F\
	timers.F	setup_kwds.F	check_switches1.F\
	check_srcs.F	check_switches2.F\
	set_weights.F	set_scoord.F	init_scalars.F	init_arrays.F\
	setup_grid1.F	setup_grid2.F	visc_rescale.F	set_nudgcof.F\
	analytical.F	ana_grid.F	ana_initial.F\
\
	zetabc.F	u2dbc_im.F	v2dbc_im.F	obc_volcons.F\
\
	pre_step3d4S.F	step3d_t3S.F	step3d_uv1.F	step3d_uv2.F\
	prsgrd32AC1.F	rhs3d35S.F	set_depth.F	omega.F\
	visc3d_GP.F	t3dmix.F	ana_vmix.F\
	u3dbc_im.F	v3dbc_im.F	t3dbc_im.F\
\
	rho_eos.F	ab_ratio.F	alfabeta.F\
\
	lmd_vmix.F      lmd_kpp.F	lmd_kpp_init.F	lmd_swr_frac.F\
	bvf_mix.F\
\
	diag.F		wvlcty.F	grid_stiffness.F\
\
	exchange.F	mpi_setup.F	mpi_exchange8WA.F\
\
	lenstr.F	get_date.F\
\
	CalcPar.F  carbon.F  co2calc_SWS.F  def_bgc_flux.F\
        drtsafe.F  init_biopar.F  init_scalars_bgcflux.F  init_scalars_npzdoc.F\
        init_scalars_physflux.F o2sato.F set_bgc_flux_avg.F\
	ta_iter_SWS.F wrt_bgc_flux_avg.F wrt_bgc_flux_his.F\
	def_phys_flux.F\
	set_phys_flux_avg.F wrt_phys_flux_his.F wrt_phys_flux_avg.F\
	biology.F  bio_diag.F atm_pco2.F calendar.F\
\
	init_scalars_bec.F ecosys_bec.F ecosys_bec_init.F get_dust.F\
	get_iron.F set_bec_flux_avg.F wrt_bec_flux_avg.F wrt_bec_flux_his.F\
	init_scalars_becflux.F def_bec_flux.F


IOSRC = nf_fread.F	nf_read_bry.F	set_cycle.F	checkdims.F\
	insert_node.F	closecdf.F	put_global_atts.F\
	get_grid.F	get_initial.F	def_grid.F	wrt_grid.F\
	def_rst.F	wrt_rst.F	def_his.F	wrt_his.F\
	set_avg.F	wrt_avg.F	def_station.F	wrt_station.F\
\
	get_forces.F	get_all_bry.F	get_tclima.F	get_uclima.F\
	get_ssh.F	get_smflux.F	get_stflux.F	get_srflux.F\
	get_sst.F	get_dqdt.F	get_sss.F



FLTSRC = init_floats.F	init_arrays_floats.F	get_initial_floats.F\
	interp_rho.F	rhs_floats.F	step_floats.F\
	def_floats.F	wrt_floats.F

#
# non-parallelized/old/obsolete stuff
#
#       pp_vmix.F      get_wwave.F 
#	ri_number.F	sg_bbl96.F	sg_ubab.F\
#	my25_q.F	my25_vmix.F	my2_vmix.F\
#	smol_adv.F	smol_ups.F	smol_adiff.F\
#	get_rhosxe.F	get_bsedim.F
#
  SRCS = $(KRNSRC) $(IOSRC)
   RCS = $(SRCS:.F=.f)
  OBJS = $(RCS:.f=.o)
  SBIN = roms
 LROMS = libroms.a
 LROMS2 = 2/$(LROMS)
 LROMS3 = 2/$(LROMS) 3/$(LROMS)
 LROMS4 = 2/$(LROMS) 3/$(LROMS) 4/$(LROMS)
#
# Eecutable file.
#========== =====
#
$(SBIN): $(OBJS)
	$(LDR) $(FFLAGS) $(LDFLAGS) -o $@ $(OBJS) $(LCDF) $(LMPI)
#mm	mv a.out $(SBIN)
2level: $(OBJS) $(LROMS2)
	$(LDR) $(FFLAGS) $(LDFLAGS) -o a.out $(OBJS) $(LROMS2) $(LCDF) $(LMPI)
	mv a.out $(SBIN)
3level: $(OBJS) $(LROMS3)
	$(LDR) $(FFLAGS) $(LDFLAGS) -o a.out $(OBJS) $(LROMS3) $(LCDF) $(LMPI)
	mv a.out $(SBIN)
4level: $(OBJS) $(LROMS4)
	$(LDR) $(FFLAGS) $(LDFLAGS) -o a.out $(OBJS) $(LROMS4) $(LCDF) $(LMPI)
	mv a.out $(SBIN)
#
# Multilevel libraries
#
$(LROMS): $(OBJS)
	/bin/rm -f $(LROMS)
	ar r $(LROMS) $(OBJS)
2/$(LROMS): $(SRCS)
	cd 2; make -f ./Makefile $(LROMS); cd ..
#$(LROMS3): $(SRCS) $(LROMS2)
#cd 3; make $(LROMS); cd ..
#$(LROMS4): $(SRCS) $(LROMS3)
#cd 3; make $(LROMS); cd ..
#
# Everything
#===========
all: tools depend $(SBIN)
#
# Auxiliary utility programs and List of Dependecies:
# ========= ======= ======== === ==== == ============
#
 TOOLS = mpc cross_matrix cppcheck srcscheck checkkwds\
	 partit ncjoin ncrename nccp nccpstruct ncextract redefs

tools: $(TOOLS)

    TMP = .

mpc: mpc.F Makefile
	$(CPP) $(CPPFLAGS) -P mpc.F > $(TMP)/mpc.for
	$(LDR) $(FFLAGS) $(LDFLAGS) -o mpc $(TMP)/mpc.for
#	/bin/rm -rf $(TMP)/mpc.f
cross_matrix: cross_matrix.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o cross_matrix cross_matrix.o
inspect:  inspect.o lenstr.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o inspect inspect.o lenstr.o
partit: partit.o insert_node.o lenstr.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o partit partit.o insert_node.o lenstr.o $(LCDF)
ncjoin: ncjoin.o lenstr.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o ncjoin ncjoin.o lenstr.o $(LCDF)
ncrename: ncrename.o lenstr.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o ncrename ncrename.o lenstr.o $(LCDF)
nccp: nccp.o lenstr.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o nccp nccp.o lenstr.o $(LCDF)

nccpstruct: nccpstruct.o lenstr.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o nccpstruct nccpstruct.o lenstr.o $(LCDF)


ncextract: ncextract.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o ncextract ncextract.o
cppcheck: cppcheck.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o cppcheck cppcheck.o
srcscheck: srcscheck.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o srcscheck srcscheck.o
checkkwds: checkkwds.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o checkkwds checkkwds.o
redefs: redefs.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o redefs redefs.o



checkdefs: check_switches1.F setup_kwds.F

check_switches1.F: cppcheck cppdefs.h
	./cppcheck
check_srcs.F: srcscheck Makefile
	./srcscheck
setup_kwds.F: checkkwds read_inp.F
	./checkkwds

depend: checkdefs cross_matrix
	./cross_matrix *.F

avr: avr.F lenstr.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o avr avr.F lenstr.o $(LCDF)

plotter: plotter.o
	$(LDR) -o plotter plotter.o $(LIBNCAR)
#
# Target to create tar file.
# ====== == ====== === =====
#
tarfile: clean
	tar cvf roms.tar Make* *.F *.h *.inc etalon_data.* README.*  *.in *.in.*
#
# Cleaning targets
# ======== =======
#
rmtools:
	/bin/rm -f $(TOOLS)
clean:
	/bin/rm -rf *.f *.for *.o *.a *.s *.trace *~ $(COMP_FILES) 
allclean: clean
	cd 2; make -f ./Makefile clean; cd ..
	cd 3; make -f ./Makefile clean; cd ..
	cd 4; make -f ./Makefile clean; cd ..
clobber: clean
	/bin/rm -f check_switches1.F setup_kwds.F check_srcs.F
	/bin/rm -f $(SBIN) $(TOOLS)
	/bin/rm -f core core.*
#
# Include automatically generated dependency list:
#
include Make.depend

