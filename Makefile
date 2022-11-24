# Source dir
SRCDIR := $(CURDIR)/src
# Build dir
BLDDIR := $(CURDIR)/build

# Create build dir if neede and copy Makefile to it
$(shell test -d $(BLDDIR) || mkdir -p $(BLDDIR) )
$(shell cp $(SRCDIR)/Makefile $(BLDDIR)) 

.PHONY: print-% roms roms_cpl all clean distclean

roms:
	$(MAKE) -C $(BLDDIR)
roms_cpl:
	$(MAKE) -C $(BLDDIR) COUPLED=1
all:
	$(MAKE) -C $(BLDDIR) all
clean:
	$(MAKE) -C $(BLDDIR) clean
distclean:
	$(MAKE) -C $(BLDDIR) distclean
	rm -rf $(BLDDIR)

