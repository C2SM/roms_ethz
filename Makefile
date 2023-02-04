# Source dir
SRCDIR := $(CURDIR)/src
# Build dir
BLDDIR ?= $(CURDIR)/build
# Default ROMS configuration:
config ?= PACTC60

# Create build dir if needed and copy make and configuration files to it
$(shell test -d $(BLDDIR) || mkdir -p $(BLDDIR) )
$(shell cp $(SRCDIR)/00makefile $(BLDDIR)/Makefile) 
$(shell cp $(SRCDIR)/include/cppdefs_$(config).h $(BLDDIR)/cppdefs.h )

.PHONY: print-% roms roms_cpl all clean distclean install

roms:
	$(MAKE) -C $(BLDDIR)
	@echo
	@echo Config: $(config)
	@echo
roms_cpl:
	$(MAKE) -C $(BLDDIR) COUPLED=1
	@echo
	@echo Config: $(config)
	@echo
nctools:
	$(MAKE) -C $(BLDDIR) nctools
all:
	$(MAKE) -C $(BLDDIR) all
clean:
	$(MAKE) -C $(BLDDIR) clean
distclean:
	rm -rf $(BLDDIR)

install:
	install $(BLDDIR)/ncjoin $(BLDDIR)/partit $(HOME)/bin

