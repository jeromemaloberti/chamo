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

STOGDIR=~/Personal/stog
STOG=$(STOGDIR)/stog.byte
DEST_DIR=/tmp/chamo
BASE_URL_OPTION=
STOG_OPTIONS=-d $(DEST_DIR) $(BASE_URL_OPTION)
LESSC=lessc

.PHONY: build site test

build:
	$(MAKE) site


site:
	$(STOG) $(STOG_OPTIONS) .
	$(LESSC) less/style.less > $(DEST_DIR)/style.css
#	cp -f *.svg $(DEST_DIR)/

test:
	$(MAKE) BASE_URL_OPTION="--site-url file://$(DEST_DIR)" site
