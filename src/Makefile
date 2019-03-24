# Universal machine independent makefile for ROMS model
#========== ======= =========== ======== === ==== =====
# Set machine dependent definitions and rules.
SRCDIR ?= $(CURDIR)

-include $(SRCDIR)/Makedefs

BLDDIR ?= .
vpath %.f = $(BLDDIR)
vpath %.o = $(BLDDIR)
vpath %.h = $(INCDIR):$(BIODIR)
vpath %.F = $(SRCDIR):$(BIODIR):$(SRCDIR)/biology/bec:$(SRCDIR)/biology/npzdoc:$(SRCDIR)/biology/pre_2014_bec

# Configuration for ROMS model:  Source code files are sorted into
# ------------- --- ---- ------  groups:

# Main driver
DRVSRC := main.F 	step2d_FB.F	read_inp.F	read_sta_pos.F \
	set_weights.F	set_scoord.F	init_scalars.F	init_arrays.F \
	ana_init.F	setup_grid1.F	setup_grid2.F	visc_rescale.F \
	set_nudgcof.F	ana_grid.F	analytical.F

# Time stepping
STEPSRC := prsgrd32AC1.F	pre_step3d4S.F	step3d_uv1.F	step3d_uv2.F \
	step3d_t_ISO.F	set_depth.F	omega.F \
	visc3d_GP.F	t3dmix_GP.F	ana_vmix.F

# Lateral boundary
BRYSRC := zetabc.F	u2dbc_im.F	v2dbc_im.F	obc_volcons.F \
	u3dbc_im.F	v3dbc_im.F	t3dbc_im.F	exchange.F \
        t3dbc_closed_im.F

# Seawater EOS
EOSSRC := rho_eos.F	ab_ratio.F	alfabeta.F

# Vertical mixing
VMXSRC := lmd_vmix.F      lmd_kpp.F	lmd_swr_frac.F	bvf_mix.F

# Diagnostics and checks
CHKSRC := diag.F	timers.F	wvlcty.F	grid_stiffness.F \
        lenstr.F	setup_kwds.F	check_switches1.F \
        get_date.F	ext_copy_prv2shr.F		check_switches2.F\
        check_srcs.F	

# Message passing
MPISRC := mpi_setup.F	mpi_exchange8TA.F

# (Carbon) chemistry
CARBSRC := CalcPar.F	carbon.F	co2calc_SWS.F	o2sato.F \
	atm_pco2.F	drtsafe.F	 ta_iter_SWS.F

# Flux analysis
FLXSRC := \
	def_bgc_flux.F	init_scalars_bgcflux.F	 wrt_bgc_flux_his.F \
	set_bgc_flux_avg.F	wrt_bgc_flux_avg.F \
\
	def_bec_flux.F init_scalars_becflux.F	wrt_bec_flux_his.F \
	set_bec_flux_avg.F wrt_bec_flux_avg.F \
\
	def_phys_flux.F  init_scalars_physflux.F \
	wrt_phys_flux_his.F set_phys_flux_avg.F wrt_phys_flux_avg.F \
\
	bio_diag.F calendar.F vsink_BEC2.F \

#Biogeochemical models
NPZDSRC := biology.F bio_npzdoc.F init_scalars_npzdoc.F
BECSRC := init_scalars_bec2.F init_biopar.F  ecosys_bec2.F ecosys_bec2_init.F
BECSRCOLD := init_scalars_bec.F ecosys_bec.F ecosys_bec_init.F\

IOSRC := checkdims.F	find_record.F	insert_node.F	ncdf_read_write.F\
	get_grid.F	get_init.F	wrt_grid.F	put_global_atts.F\
	def_rst.F	wrt_rst.F	def_his.F	wrt_his.F\
	set_avg.F	wrt_avg.F	def_statn.F	wrt_statn.F\
	get_forces.F	get_stflux.F	get_bry_all.F	set_forces.F\
	set_smflux.F	set_stflux.F	nf_read_bry.F	bulk_flux_COAMS.F\
	set_seaice.F	get_tides.F	set_tides.F	set_srflux.F\
	get_tclima.F	get_uclima.F	closecdf.F\
	init_agedye.F   get_agedye.F    set_cycle.F     get_tsource.F

FLTSRC := init_floats.F	init_arrays_floats.F	get_init_flts.F\
	interp_rho.F	rhs_floats.F	step_floats.F\
	def_floats.F	wrt_floats.F

#       pp_vmix.F	get_wwave.F	smol_adv.F	get_bsedim.F

KRNSRC := $(DRVSRC) $(STEPSRC) $(BRYSRC) $(EOSSRC) $(VMXSRC) $(CHKSRC) $(MPISRC)
BIOSRC := $(FLXSRC) $(NPZDSRC) $(BECSRC) # $(BECSRCOLD)

SRCS := $(KRNSRC) $(IOSRC) $(BIOSRC)
OBJS := $(SRCS:.F=.o)
SBIN := roms

# ROMS excutable

$(BLDDIR)/$(SBIN): mpc print_cppswitches $(OBJS)
	$(Q)$(SRCDIR)/print_cppswitches
	$(Q)cd $(BLDDIR); $(LDR) $(FFLAGS) $(LDFLAGS) -o $@ $(OBJS) $(LCDF) $(LMPI)

check_forces: check_forces.o $(SBIN)
	cp -pv check_forces.o main.o 	
	$(LDR) $(FFLAGS) $(LDFLAGS) -o $@ $(OBJS) $(LCDF) $(LMPI)

# Everything
all: mpc tools $(SBIN)

# A program to test MPI halo exchange routines.
# - ------- -- ---- --- ---- -------- ---------
MPI_TEST = mpi_test.F	mpi_setup.F	exchange.F	mpi_exchange4.F\
							mpi_exchange8WA.F
MPI_TEST_RCS = $(MPI_TEST:.F=.for)
MPI_TEST_OBJ = $(MPI_TEST_RCS:.for=.o)

mpi_test: $(MPI_TEST_OBJ)
	$(LDR) $(FFLAGS) $(LDFLAGS) -o mpi_test $(MPI_TEST_OBJ) $(LCDF) $(LMPI)

# Auxiliary utility programs
# --------- ------- --------
TOOLS := mpc cppcheck srcscheck checkkwds ncextract redefs

tools: $(TOOLS)
mpc: mpc.F
	$(Q)cd $(SRCDIR); $(FC) $(CPPFLAGS) -o $@ mpc.F

# We do not optimise the tools
cppcheck: cppcheck.F
	$(Q)cd $(SRCDIR); $(FC) $(CPPFLAGS) -o $@ cppcheck.F
srcscheck: srcscheck.F
	$(Q)cd $(SRCDIR); $(FC) $(CPPFLAGS) -o $@ srcscheck.F
checkkwds: checkkwds.F
	$(Q)cd $(SRCDIR); $(FC) $(CPPFLAGS) -o $@ checkkwds.F
redefs: redefs.F
	$(Q)cd $(SRCDIR); $(FC) $(CPPFLAGS) -o $@ redefs.F
checkdefs: check_switches1.F setup_kwds.F
check_switches1.F: cppcheck cppdefs_UP.h cppdefs.h
	$(Q)cd $(SRCDIR); ./cppcheck $(INCDIR)/cppdefs_UP.h $(INCDIR)/cppdefs.h
check_srcs.F: srcscheck Makefile
	$(Q)cd $(SRCDIR); ./srcscheck
setup_kwds.F: checkkwds read_inp.F
	$(Q)cd $(SRCDIR); ./checkkwds
print_cppswitches: check_switches1.F print_cppswitches.F check_switches1.F 
	$(Q)cd $(SRCDIR); $(FC) $(CPPFLAGS) -o $@ print_cppswitches.F $(SRCDIR)/check_switches1.F

# Debugging target
print-%:
	@echo '$*=$($*)'

# Target to create tar file.
# ------ -- ------ --- -----
tarfile: clean
	tar zcvf roms.tgz $(SRCDIR)

.PHONY: all clean clobber distclean

# Cleaning targets
# -------- -------
clean:
	cd $(BLDDIR); /bin/rm -f *.f *.o *.a *.s *.trace *~ $(COMP_FILES)

clobber: distclean
distclean: clean
	cd $(SRCDIR); /bin/rm -f check_switches1.F setup_kwds.F check_srcs.F \
	$(SBIN) $(TOOLS) nsub core* *_genmod.mod *_genmod.f90 \
	$(DEPDIR)/*.d print_cppswitches

# Rule for dependency files *.d
# ---- --- ---------- ----- ---
# $(DEPDIR)/%.d:

.PRECIOUS: $(DEPDIR)/%.d

%.d: %.F
	$(MAKEDEPEND) 'dirname $<' $< > $@

# Include dependency files
# ------- ---------- -----
# see https://make.mad-scientist.net/papers/advanced-auto-dependency-generation/#output
-include $(wildcard $(patsubst %,$(DEPDIR)/%.d,$(basename $(SRCS))))


# ------- End --------
