OCAMLMAKEFILE = ../OCamlMakefile

include ../Makefile.global

VERSION=0.5-alpha-20081114

include Makefile.conf

SOURCES = version.ml config.ml logger.ml common.ml types.ml lang.ml muc.ml \
	  find_url.ml muc_log.ml hooks.ml iq.ml 

SUBDIRS = lang

#ifdef MUC_LOG
#  SOURCES += muc_log.ml
#endif

ifdef PLUGIN_GOOGLE
  SOURCES1 += plugin_google.ml
  DEHTML = yes
  HTTP_SUCK = yes
endif
ifdef PLUGIN_YANDEX
   SOURCES1 += plugin_yandex.ml
   DEHTML = yes
   HTTP_SUCK = yes
endif
ifdef PLUGIN_CALC
  SOURCES1 += math.ml pcalc.mly pcalc_lexer.mll icalc.mly icalc_ulex.ml plugin_calc.ml
endif
ifdef PLUGIN_MUELLER
  SOURCES1 += plugin_mueller.ml
  NETSTRING = yes
endif
ifdef PLUGIN_MARKOV
  SOURCES1 += plugin_markov.ml
  SQLITE = yes
endif
ifdef PLUGIN_VOCABULARY
  SOURCES1 += plugin_vocabulary.ml
  SQLITE = yes
endif
ifdef PLUGIN_PING
  SOURCES1 += plugin_ping.ml
endif
ifdef PLUGIN_USERINFO
  SOURCES1 += plugin_userinfo.ml
  NETSTRING = yes
endif
ifdef PLUGIN_MISC
  SOURCES1 += plugin_misc.ml
endif
ifdef PLUGIN_ADMIN
  SOURCES1 += plugin_admin.ml
endif
ifdef PLUGIN_DICT
  SOURCES1 += plugin_dict.ml
endif 
ifdef PLUGIN_WEATHER
  SOURCES1 += plugin_weather.ml
  HTTP_SUCK = yes
endif
ifdef PLUGIN_GLOBALSTATS
  SOURCES1 += plugin_globalstats.ml
endif
ifdef PLUGIN_CURRENCY
  SOURCES1 += plugin_currency.ml
  XMLSTRING_NETSTRING = yes
  HTTP_SUCK = yes
endif
ifdef PLUGIN_TLD
  SOURCES1 += plugin_tld.ml
  DBM_LIB =  yes
  SUBDIRS += tlds
endif
ifdef PLUGIN_ROULETTE
  SOURCES1 += plugin_roulette.ml
endif
ifdef PLUGIN_SEEN
  SOURCES1 += plugin_seen.ml
  SQLITE = yes
endif
ifdef PLUGIN_TALKERS
  SOURCES1 += plugin_talkers.ml
  SQLITE = yes
endif
ifdef PLUGIN_CERBERUS
  SOURCES1 += plugin_cerberus.ml
endif
ifdef PLUGIN_TRANSLATE
  SOURCES1 += plugin_translate.ml
  HTTP_SUCK = yes
endif
ifdef PLUGIN_GOOGLE_TRANSLATE
   SOURCES1 += plugin_google_translate.ml
   HTTP_SUCK = yes
endif
ifdef PLUGIN_VCARD
  SOURCES1 += plugin_vcard.ml
endif
ifdef PLUGIN_XMLRPC
  SOURCES1 += plugin_xmlrpc.ml
endif
ifdef PLUGIN_HOSTIP
   SOURCES1 += plugin_hostip.ml
   HTTP_SUCK = yes
endif
ifdef PLUGIN_1APRIL
   SOURCES1 += plugin_1april.ml
endif

LANGPACKS = lang/ru_time.ml lang/en_time.ml lang/es_time.ml

ifdef HTTP_SUCK
   SOURCES += http_suck.ml
   NETCLIENT = yes
endif

SOURCES += $(SOURCES1) $(LANGPACKS) sulci.ml

THREADS = yes

PACKS = ulex unix xmpp getopt xmlstring strftime scheduler pcre

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
ifdef SQLITE
  PACKS += sqlite_util
endif
ifdef DEHTML
   PACKS += dehtml
endif
ifdef XMLSTRING_NETSTRING
   PACKS += xmlstring_netstring
endif

OCAMLDEP      = ocamldep -package ulex -syntax camlp4o
OCAMLFLAGS    = -syntax camlp4o

RESULT = sulci

.PHONY: subdirs $(SUBDIRS)

include ../Makefile.global

all: nc subdirs

subdirs: $(SUBDIRS)

$(SUBDIRS):
	$(MAKE) -C $@

SDIR=/tmp/sulci-$(VERSION)
LIB_PROJECTS = Makefile.inc xmpp xml xmlstring xmlstring_netstring getopt dehtml cryptokit sqlite sqlite_util scheduler scheduler2 strftime

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
	rm -f $(TARGETS) $(TRASH)
	rm -rf $(BCDIDIR) $(NCDIDIR) $(MLDEPDIR)
	for dir in $(SUBDIRS); do \
	   $(MAKE) -C $$dir clean; \
	done

