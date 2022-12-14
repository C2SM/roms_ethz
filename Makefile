# Source dir
SRCDIR := $(CURDIR)/src
# Build dir
BLDDIR ?= $(CURDIR)/build
# Default ROMS setup:
ROMS_SETUP ?= PACTC

# Create build dir if neede and copy Makefile to it
$(shell test -d $(BLDDIR) || mkdir -p $(BLDDIR) )
$(shell cp $(SRCDIR)/Makefile $(BLDDIR)) 
$(shell cd $(SRCDIR)/include ; rm -f cppdefs.h ;  ln -s cppdefs_$(ROMS_SETUP).h cppdefs.h)

.PHONY: print-% roms roms_cpl all clean distclean install

roms:
	$(MAKE) -C $(BLDDIR)
	rm -f $(SRCDIR)/include/cppdefs.h
roms_cpl:
	$(MAKE) -C $(BLDDIR) COUPLED=1
	rm -f $(SRCDIR)/include/cppdefs.h
all:
	$(MAKE) -C $(BLDDIR) all
clean:
	$(MAKE) -C $(BLDDIR) clean
distclean:
	$(MAKE) -C $(BLDDIR) distclean
	rm -rf $(BLDDIR)

install:
	install $(BLDDIR)/roms 

