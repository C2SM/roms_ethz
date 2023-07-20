# Source dir:
SRCDIR ?= $(CURDIR)/src

# Configurations (ROMS setups) directory:
CNFDIR ?=  $(CURDIR)/configs

# Default ROMS configuration:
config ?= PACTC

# Build directory:
BLDDIR ?= $(CURDIR)/$(config)_build

# Install prefix:
PREFIX ?= $(HOME)

# Create build dir if needed 
$(shell test -d $(BLDDIR) || mkdir -p $(BLDDIR) )

# and copy make and the configuration files into it:
$(shell cp $(SRCDIR)/00makefile $(BLDDIR)/Makefile) 
$(shell cp $(SRCDIR)/Makedefs $(BLDDIR)/Makedefs) 
$(shell cp $(CNFDIR)/cppdefs_UP.h $(BLDDIR)/ )
$(shell cp $(CNFDIR)/cppdefs_$(config).h $(BLDDIR)/cppdefs.h )

# All phony targets here since the actually make is done in BLDDIR:
.PHONY: print-% roms roms_cpl all clean distclean install

roms:
	$(MAKE) -C $(BLDDIR)
	EXECS += roms
	@echo
	@echo Config: $(config)
	@echo
roms_cpl:
	$(MAKE) -C $(BLDDIR) COUPLED=1
	EXECS += roms
	@echo
	@echo ROMSOC Config: $(config)
	@echo
nctools:
	$(MAKE) -C $(BLDDIR) nctools
	EXECS += partit ncjoin
all:
	$(MAKE) -C $(BLDDIR) all
	EXECS += roms partit ncjoin
clean:
	$(MAKE) -C $(BLDDIR) clean
distclean:
	rm -rf $(BLDDIR)
install:
	cd $(BLDDIR) && install $(EXECS) $(PREFIX)/bin/.

