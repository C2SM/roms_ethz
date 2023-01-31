# Source dir
SRCDIR := $(CURDIR)/src
# Build dir
BLDDIR ?= $(CURDIR)/build
# Default ROMS configuration:
config ?= PACTC

# Create build dir if neede and copy Makefile to it
$(shell test -d $(BLDDIR) || mkdir -p $(BLDDIR) )
$(shell cp $(SRCDIR)/Makefile $(BLDDIR)) 
# $(shell cd $(SRCDIR)/include ; rm -f cppdefs.h ;  ln -s cppdefs_$(config).h cppdefs.h)
$(shell cp $(SRCDIR)/include/cppdefs_$(config).h $(BLDDIR) )

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
	$(MAKE) -C $(BLDDIR) distclean
	rm -rf $(BLDDIR)

install:
	install $(BLDDIR)/ncjoin $(BLDDIR)/partit $(HOME)/bin

