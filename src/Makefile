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

include ../master.Makefile

PACKAGES=str,threads.posix,pcre,config-file,xmlm,lablgtk2.glade,lablgtk2-extras.configwin,lablgtk2.init
# IMPORTANT: do not forget to update ed_eval.ml modifying the list of packages

OF_FLAGS=-package $(PACKAGES) -predicates mt
INCLUDES=-I +ocamldoc

COMPFLAGS=$(INCLUDES) -annot -thread
LINKFLAGS=$(INCLUDES)

# The executables and libraries to produce
EDITOR=chamo
EDITOR_BYTE=$(EDITOR).byte


LIB=chamo.cmxa
LIB_BYTE=chamo.cma

# Compilation
#############
EDITOR_CMXFILES=ed_main.cmx
EDITOR_CMOFILES=$(EDITOR_CMXFILES:.cmx=.cmo)
EDITOR_CMIFILES=$(EDITOR_CMXFILES:.cmx=.cmi)

LIB_CMXFILES= \
	ed_extern.cmx \
	ed_xml.cmx \
	ed_utf8.cmx \
	ed_gtk_misc.cmx \
	ed_annot.cmx \
	multiclip.cmx \
	multiclip_gui.cmx \
	ed_constant.cmx \
	ed_installation.cmx \
	ed_config.cmx \
	ed_dbg.cmx \
	ed_hooks.cmx \
	ed_rc.cmx \
	ed_com_history.cmx \
	ed_commands.cmx \
	ed_messages.cmx \
	ed_prefs.cmx \
	ed_gui_base.cmx \
	ed_keymaps.cmx \
	ed_gui_rc.cmx \
	ed_bookmarks.cmx \
	ed_fstack.cmx \
	ed_sourceview_rc.cmx \
	ed_minibuffer_rc.cmx \
	ed_minibuffer.cmx \
	ed_charsets.cmx \
	ed_core_rc.cmx \
	ed_outputs.cmx \
	ed_misc.cmx \
	ed_view_rc.cmx \
	ed_view.cmx \
	ed_multiclip_rc.cmx \
	ed_multiclip.cmx \
	ed_odoc_rc.cmx \
	ed_odoc.cmx \
	ed_sourceview.cmx \
	ed_sourceview_expand.cmx \
	ed_ocamloutput.cmx \
	ed_ocaml_lexer.cmx \
	ed_mode_ocaml_rc.cmx \
	ed_ocamlbuild.cmx \
	ed_mode_ocaml.cmx \
	ed_mode_makefile_rc.cmx \
	ed_mode_makefile.cmx \
	ed_mode_changelog_rc.cmx \
	ed_mode_changelog.cmx \
	ed_gui.cmx \
	ed_layout.cmx \
	ed_args.cmx \
	ed_log.cmx

LIB_CMOFILES= $(LIB_CMXFILES:.cmx=.cmo) ed_eval.cmo
LIB_CMIFILES= $(LIB_CMOFILES:.cmo=.cmi)

all: opt byte
opt: $(LIB) $(EDITOR)
byte: $(LIB_BYTE) $(EDITOR_BYTE)

$(LIB): $(LIB_CMIFILES) $(LIB_CMXFILES)
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -a -o $@ -thread $(LIB_CMXFILES)

$(LIB_BYTE): $(LIB_CMIFILES) $(LIB_CMOFILES)
	$(OCAMLFIND) ocamlc $(OF_FLAGS) -a -o $@ -thread $(LIB_CMOFILES)

$(EDITOR): $(LIB) $(EDITOR_CMIFILES) $(EDITOR_CMXFILES)
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) -linkpkg -o $@ -thread -linkall $(LINKFLAGS) \
	$(INCLUDES) odoc_info.cmxa $(LIB) $(EDITOR_CMXFILES)

$(EDITOR_BYTE): $(LIB_BYTE) $(EDITOR_CMIFILES) $(EDITOR_CMOFILES)
	$(OCAMLFIND) ocamlc $ $(OF_FLAGS) -linkpkg -o $@ -thread -linkall $(LINKFLAGS) \
	$(INCLUDES) toplevellib.cma odoc_info.cma $(LIB_BYTE) $(EDITOR_CMOFILES)


# Documentation :
#################
dump.odoc: *.mli *.ml
	$(OCAMLFIND) ocamldoc $(OF_FLAGS) $(OCAMLPP) $(INCLUDES) -sort -dump dump.odoc \
	`ls $^ | grep -v example`

doc: dump.odoc
	$(MKDIR) ocamldoc
	$(OCAMLFIND) ocamldoc $(OF_FLAGS) $(OCAMLPP) \
	-t "Chamo library" -load $^ \
	-d ocamldoc -html

dot: dep.dot
dep.dot: dump.odoc
	$(OCAMLDOC) -load $< -o $@ -dot -dot-reduce
dep.ps:dep.dot
	dot -Tps -o $@ $<

# backup, clean and depend :
############################

distclean: clean

clean:: dummy
	$(RM) *~ \#*\#
	$(RM) $(EDITOR) $(EDITOR_BYTE)
	$(RM) *.cm* *.a *.o *.annot ed_ocaml_lexer.ml

.depend depend:
	$(RM) .depend
	$(OCAMLDEP) `ls *.ml *.mli | grep -v ed_commands` > .depend

dummy:

include .depend

#################
# Installation
#################
install: all
	$(OCAMLFIND) install $(PACKAGE) dbf/META \
		$(LIB_DBF) $(LIB_DBF:.cmxa=.a) $(LIB_DBF_CMIFILES) \
		$(LIB_SQL) $(LIB_SQL:.cmxa=.a) $(LIB_SQL_CMIFILES) \
		$(LIB_MYSQL) $(LIB_MYSQL:.cmxa=.cmi) $(LIB_MYSQL:.cmxa=.a) $(LIB_MYSQL:.cmxa=.mli) \
		$(LIB_DBF_GTK) $(LIB_DBF_GTK:.cmxa=.a) $(LIB_DBF_GTK:.cmxa=.mli) $(LIB_DBF_GTK_CMIFILES) \
		$(LIB_DBF_BYTE) $(LIB_DBF_GTK_BYTE) $(LIB_SQL_BYTE) $(LIB_MYSQL_BYTE)
	$(CP) $(DBFORGE) $(DBFORGE_BYTE) $(OCAMLBIN)
	$(CP) $(DBFORGE_GTK) $(DFBORGE_GTK_BYTE) $(OCAMLBIN)
	$(OCAMLFIND) install sqml sqml/META \
		$(LIB_SQML) $(LIB_SQML:.cmxa=.cmi) $(LIB_SQML:.cmxa=.a) $(LIB_SQML_CMIFILES) \
		$(LIB_SQML_BYTE)
	$(MKDIR) $(DTDDIR) $(GLADEDIR)
	$(CP) dbf/db.dtd $(DTDDIR)/
	$(CP) dbf/dbforge.glade $(GLADEDIR)/

uninstall: dummy
	$(OCAMLFIND) remove $(PACKAGE)
	$(OCAMLFIND) remove sqml
	$(RM) $(OCAMLBIN)/$(DBFORGE)
	$(RM) $(OCAMLBIN)/$(DBFORGE_GTK)
	$(RM) $(OCAMLBIN)/$(DBFORGE_BYTE)
	$(RM) $(OCAMLBIN)/$(DBFORGE_GTK_BYTE)

###########################
# additional dependencies
###########################
ed_gui_base.ml: chamo.glade
	$(LABLGLADECC) -hide-default $< > $@

ed_commands.cmi: ed_com_history.cmi

ed_commands.cmo: ed_commands.ml ed_com_history.cmo
	$(OCAMLFIND) ocamlc $(OF_FLAGS) $(COMPFLAGS) -c -pp "$(OCAMLBIN)/camlp4o" $<

ed_commands.cmx: ed_commands.ml ed_com_history.cmx
	$(OCAMLFIND) ocamlopt $(OF_FLAGS) $(COMPFLAGS) -c -pp "$(OCAMLBIN)/camlp4o" $<
