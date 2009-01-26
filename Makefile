OCAMLMAKEFILE = ../OCamlMakefile

include ../Makefile.global

VERSION=0.6-alpha-20090127

include Makefile.conf

SOURCES = version.ml config.ml common.ml types.ml lang.ml hooks.ml iq.ml

SUBDIRS = lang

ifeq ($(PLUGIN_ADMIN),yes)
  SOURCES1 += plugin_admin.ml
endif
ifeq ($(PLUGIN_GOOGLE),yes)
  SOURCES1 += plugin_google.ml
  DEHTML = yes
  HTTP_SUCK = yes
endif
ifeq ($(PLUGIN_GOOGLE_TRANSLATE),yes)
   SOURCES1 += plugin_google_translate.ml
   HTTP_SUCK = yes
endif
ifeq ($(PLUGIN_CALC),yes)
  SOURCES1 += math.ml pcalc.mly pcalc_lexer.mll icalc.mly icalc_ulex.ml plugin_calc.ml
endif
ifeq ($(PLUGIN_MUELLER),yes)
  SOURCES1 += plugin_mueller.ml
  NETSTRING = yes
endif
ifeq ($(PLUGIN_PING),yes)
  SOURCES1 += plugin_ping.ml
endif
ifeq ($(PLUGIN_USERINFO),yes)
  SOURCES1 += plugin_userinfo.ml
  NETSTRING = yes
endif
ifeq ($(PLUGIN_MISC),yes)
  SOURCES1 += plugin_misc.ml
endif
ifeq ($(PLUGIN_DICT),yes)
  SOURCES1 += plugin_dict.ml
endif 
ifeq ($(PLUGIN_WEATHER),yes)
  SOURCES1 += plugin_weather.ml
  HTTP_SUCK = yes
endif
ifeq ($(PLUGIN_GLOBALSTATS),yes)
  SOURCES1 += plugin_globalstats.ml
endif
ifeq ($(PLUGIN_CURRENCY),yes)
  SOURCES1 += plugin_currency.ml
  XMLSTRING_NETSTRING = yes
  HTTP_SUCK = yes
endif
ifeq ($(PLUGIN_TLD),yes)
  SOURCES1 += plugin_tld.ml
  DBM_LIB =  yes
  SUBDIRS += tlds
endif
ifeq ($(PLUGIN_TRANSLATE),yes)
  SOURCES1 += plugin_translate.ml
  HTTP_SUCK = yes
endif
ifeq ($(PLUGIN_VOCABULARY),yes)
  SOURCES1 += plugin_vocabulary.ml
  SQLITE = yes
endif
ifeq ($(PLUGIN_VCARD),yes)
  SOURCES1 += plugin_vcard.ml
endif
ifeq ($(PLUGIN_XMLRPC),yes)
  SOURCES1 += plugin_xmlrpc.ml
endif
ifeq ($(PLUGIN_HOSTIP),yes)
   SOURCES1 += plugin_hostip.ml
   HTTP_SUCK = yes
endif
ifeq ($(PLUGIN_1APRIL),yes)
   SOURCES1 += plugin_1april.ml
endif
ifeq ($(PLUGIN_YANDEX),yes)
   SOURCES1 += plugin_yandex.ml
   DEHTML = yes
   HTTP_SUCK = yes
endif

ifeq ($(MUC),yes)
   SOURCES1 += muc_types.ml find_url.ml muc_log.ml muc.ml
   SQLITE = yes

  ifeq ($(PLUGIN_ADMIN),yes)
    SOURCES1 += plugin_admin_muc.ml
  endif

  ifeq ($(PLUGIN_CERBERUS),yes)
    SOURCES1 += plugin_cerberus.ml
  endif

  ifeq ($(PLUGIN_MARKOV),yes)
    SOURCES1 += plugin_markov.ml
    SQLITE = yes
  endif

  ifeq ($(PLUGIN_ROULETTE),yes)
    SOURCES1 += plugin_roulette.ml
  endif

  ifeq ($(PLUGIN_SEEN),yes)
    SOURCES1 += plugin_seen.ml
    SQLITE = yes
  endif

  ifeq ($(PLUGIN_TALKERS),yes)
    SOURCES1 += plugin_talkers.ml
    SQLITE = yes
  endif

  ifeq ($(PLUGIN_VOCABULARY),yes)
    SOURCES1 += plugin_vocabulary_muc.ml
    SQLITE = yes
  endif

  ifeq ($(PLUGIN_USERINFO),yes)
    SOURCES1 += plugin_userinfo_muc.ml
  endif
endif

LANGPACKS = lang/ru_time.ml lang/en_time.ml lang/es_time.ml

ifdef HTTP_SUCK
   SOURCES += http_suck.ml
   NETCLIENT = yes
endif

THREADS = yes

PACKS = ulex unix xmpp logger xmlstring strftime scheduler pcre

ifdef XMLSTRING_NETSTRING
   NETSTRING = yes
endif
ifdef NETSTRING
   PACKS += netstring
endif
ifdef NETCLIENT
   PACKS += netclient
endif
ifdef DBM_LIB
  PACKS += dbm
endif
ifdef DEHTML
   PACKS += dehtml
endif
ifdef XMLSTRING_NETSTRING
   PACKS += xmlstring_netstring
endif
ifeq ($(SQLITE), yes)
   SOURCES += sqlite_util.ml
   PACKS += sqlite3
endif

SOURCES += $(SOURCES1) $(LANGPACKS) sulci.ml

OCAMLDEP      = ocamldep -package ulex -syntax camlp4o
OCAMLFLAGS    = -syntax camlp4o

RESULT = sulci

.PHONY: subdirs $(SUBDIRS)

include ../Makefile.global

all: version.ml nc subdirs

version.ml: version.ml.src
	sed 's/VERSION/$(VERSION)/' version.ml.src > version.ml

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

SDIR=/tmp/sulci-$(VERSION)
LIB_PROJECTS = Makefile.inc xmpp xml xmlstring xmlstring_netstring dehtml cryptokit scheduler strftime

tarball::
	rm -rf $(SDIR)
	mkdir $(SDIR)
	cp -Rp ../packages $(SDIR)
	mkdir $(SDIR)/sulci
	cp *.ml *.mll *.mly Makefile Makefile.conf $(SDIR)/sulci
	cp -Rp tlds fcgi lang utils $(SDIR)/sulci
	mkdir $(SDIR)/libs
	for i in $(LIB_PROJECTS); do \
	   cp -Rp ../libs/$$i $(SDIR)/libs/; \
	done
	mkdir $(SDIR)/docs
	cp -Rp ../doc/sulci $(SDIR)/docs/
	cp README $(SDIR)/
	cp ../OCamlMakefile $(SDIR)
	cp ../misc/Makefile.sulci $(SDIR)/Makefile
	cp ../COPYING $(SDIR)/
	cp ../ChangeLog $(SDIR)/
	find -d $(SDIR) -name ".svn" -print0 | xargs -0 -- rm -rf
	find $(SDIR) -name "*~" -print0 | xargs -0 -- rm -rf
	tar jcf sulci-$(VERSION).tar.bz2 -C /tmp sulci-$(VERSION)
	tar zcf sulci-$(VERSION).tar.gz -C /tmp sulci-$(VERSION)

include $(OCAMLMAKEFILE)

.PHONY: clean
clean::
	rm -f version.ml
	rm -f $(TARGETS) $(TRASH)
	rm -rf $(BCDIDIR) $(NCDIDIR) $(MLDEPDIR)
	for dir in $(SUBDIRS); do \
	   $(MAKE) -C $$dir clean; \
	done


