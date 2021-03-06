#################################################################################
#                Chamo                                                          #
#                                                                               #
#    Copyright (C) 2003-2012 Institut National de Recherche en Informatique     #
#    et en Automatique. All rights reserved.                                    #
#                                                                               #
#    This program is free software; you can redistribute it and/or modify       #
#    it under the terms of the GNU Lesser General Public License version        #
#    3 as published by the Free Software Foundation.                            #
#                                                                               #
#    This program is distributed in the hope that it will be useful,            #
#    but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              #
#    GNU General Public License for more details.                               #
#                                                                               #
#    You should have received a copy of the GNU General Public License          #
#    along with this program; if not, write to the Free Software                #
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   #
#    02111-1307  USA                                                            #
#                                                                               #
#    Contact: Maxence.Guesdon@inria.fr                                          #
#                                                                               #
#################################################################################

include master.Makefile

# Compilation
#############

all: src

src: dummy
	cd src && $(MAKE) all

re : depend clean all

# Documentation :
#################
doc: dummy
	cd src && $(MAKE) doc

# myself

master.Makefile: master.Makefile.in config.status
	./config.status

config.status: configure
	./config.status --recheck

configure: configure.in
	autoconf

# backup, clean and depend :
############################

distclean: clean
	cd src && $(MAKE) distclean
	$(RM) config.cache config.log config.status master.Makefile \
	src/ed_installation.ml src/ed_config.ml

clean:: dummy
	$(RM) *~ \#*\#
	cd src && $(MAKE) clean

depend: dummy
	cd src && $(MAKE) depend

dummy:

###########
# Headers
###########
HEADEDFILES= configure.in configure \
	master.Makefile.in Makefile src/Makefile web/Makefile checkocaml.ml \
	src/*.ml src/*.mli src/ed_installation.ml.in src/ed_config.ml.in
headers: dummy
	headache -h header -c ~/.headache_config $(HEADEDFILES)

noheaders: dummy
	headache -r -c ~/.headache_config $(HEADEDFILES)

############
# Web site
############
webdoc: dummy
	cd web && $(MAKE) DEST_DIR=../../chamo-gh-pages
	$(MKDIR) ../chamo-gh-pages/refdoc
	$(CP) src/ocamldoc/* ../chamo-gh-pages/refdoc/

#################
# installation
#################

install: install-lib install-bin

install-lib: dummy
	cd src && $(MAKE) install-lib
	@$(MKDIR) $(PIXMAPSDIR)
	@for i in images/*.png ; do \
		$(CP) $$i $(PIXMAPSDIR)/ && echo Installed $(PIXMAPSDIR)/$$i; done

install-bin: dummy
	cd src && $(MAKE) install-bin

uninstall:
	cd src && $(MAKE) uninstall

uninstall-lib: dummy
	cd src && $(MAKE) uninstall-lib

uninstall-bin: dummy
	cd src && $(MAKE) uninstall-bin

###########
# archive
###########
archive:
	git archive --prefix=chamo-$(VERSION)/ HEAD | gzip > ../chamo-gh-pages/chamo-$(VERSION).tar.gz

###########################
# additional dependencies
###########################

# DO NOT DELETE
