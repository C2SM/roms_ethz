# Source dir
SRCDIR := $(CURDIR)/src
#
BLDDIR ?= $(CURDIR)/build

#-include $(SRCDIR)/Makefile

$(shell test -d $(BLDDIR) || mkdir -p $(BLDDIR) )
$(shell cp $(SRCDIR)/Makefile $(BLDDIR)) 

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

