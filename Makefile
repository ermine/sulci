OCAMLMAKEFILE = ../OCamlMakefile

VERSION=0.5-alpha-20060126

include Makefile.conf

SOURCES = version.ml config.ml logger.ml common.ml types.ml lang.ml muc.ml \
	  find_url.ml muc_log.ml hooks.ml iq.ml http_suck.ml

#ifdef MUC_LOG
#  SOURCES += muc_log.ml
#endif

ifdef PLUGIN_GOOGLE
  SOURCES += plugin_google.ml
endif
ifdef PLUGIN_YANDEX
   SOURCES += plugin_yandex.ml
    DEHTML = yes
endif
ifdef PLUGIN_CALC
  SOURCES += math.ml pcalc.mly pcalc_lexer.mll icalc.mly icalc_ulex.ml plugin_calc.ml
endif
ifdef PLUGIN_MUELLER
  SOURCES += plugin_mueller.ml
endif
ifdef PLUGIN_MARKOV
  SOURCES += plugin_markov.ml
  SQLITE = yes
endif
ifdef PLUGIN_VOCABULARY
  SOURCES += plugin_vocabulary.ml
endif
ifdef PLUGIN_PING
  SOURCES += plugin_ping.ml
  DBM_LIB = dbm
endif
ifdef PLUGIN_USERINFO
  SOURCES += plugin_userinfo.ml
endif
ifdef PLUGIN_MISC
  SOURCES += plugin_misc.ml
endif
ifdef PLUGIN_ADMIN
  SOURCES += plugin_admin.ml
endif
ifdef PLUGIN_DICT
  SOURCES += plugin_dict.ml
endif 
ifdef PLUGIN_WEATHER
  SOURCES += plugin_weather.ml
endif
ifdef PLUGIN_GLOBALSTATS
  SOURCES += plugin_globalstats.ml
endif
ifdef PLUGIN_CURRENCY
  SOURCES += plugin_currency.ml
  XMLSTRING_NETSTRING =  yes
endif
ifdef PLUGIN_TLD
  SOURCES += plugin_tld.ml
  DBM_LIB =  dbm
endif
ifdef PLUGIN_ROULETTE
  SOURCES += plugin_roulette.ml
endif
ifdef PLUGIN_SEEN
  SOURCES += plugin_seen.ml
  SQLITE = yes
endif
ifdef PLUGIN_TALKERS
  SOURCES += plugin_talkers.ml
  SQLITE = yes
endif
ifdef PLUGIN_CERBERUS
  SOURCES += plugin_cerberus.ml
endif
ifdef PLUGIN_TRANSLATE
  SOURCES += plugin_translate.ml
endif
ifdef PLUGIN_VCARD
  SOURCES += plugin_vcard.ml
endif
ifdef PLUGIN_XMLRPC
  SOURCES += plugin_xmlrpc.ml
endif

LANGPACKS = lang/ru_time.ml lang/en_time.ml lang/es_time.ml

SOURCES += $(LANGPACKS) sulci.ml

THREADS = yes
PACKS = ulex unix netstring netclient $(DBM_LIB)

INCDIRS = ../libs/getopt ../libs/xml ../xmpp ../libs/xmlstring ../libs/scheduler \
	  ../libs/strftime

OCAMLLDFLAGS =  nums.cmxa cryptokit.cmxa \
		getopt.cmxa xml.cmxa xmpp.cmxa xmlstring.cmxa strftime.cmxa \
		scheduler.cmxa $(CURR_LIB)

ifdef SQLITE
  INCDIRS += ../packages/ocaml-sqlite-0.3.5 ../libs/sqlite_util
  OCAMLLDFLAGS += sqlite.cmxa sqlite_util.cmxa
endif

ifdef DEHTML
   INCDIRS += ../libs/dehtml
    OCAMLLDFLAGS += dehtml.cmxa
endif

ifdef XMLSTRING_NETSTRING
  OCAMLLDFLAGS += xmlstring_netstring.cmxa
endif

USE_CAMLP4    = yes
OCAMLDEP      = ocamldep -package ulex -syntax camlp4o
OCAMLFLAGS    = -syntax camlp4o

RESULT = sulci

all: nc langcompile

SDIR=/tmp/sulci-$(VERSION)

tarball::
	rm -rf $(SDIR)
	mkdir $(SDIR)
	cp -Rp ../packages $(SDIR)
	cp -Rp ../xmpp $(SDIR)
	cp -Rp ../sulci $(SDIR)
	mkdir $(SDIR)/libs
	cp -Rp ../libs/xml $(SDIR)/libs
	cp -Rp ../libs/scheduler $(SDIR)/libs
	cp -Rp ../libs/sqlite_util $(SDIR)/libs
	cp -Rp ../libs/strftime $(SDIR)/libs
	cp -Rp ../libs/xmlstring $(SDIR)/libs
	cp -Rp ../libs/getopt $(SDIR)/libs
	mkdir $(SDIR)/docs
	cp -Rp ../doc/sulci $(SDIR)/docs/
	cp README $(SDIR)/
	cp ../OCamlMakefile $(SDIR)
	cp ../misc/Makefile.sulci $(SDIR)/Makefile
	cp ../COPYING $(SDIR)/
	cp ../ChangeLog $(SDIR)/
	find -d $(SDIR) -name ".svn" -print0 | xargs -0 -- rm -rf
	find $(SDIR) -name *.cm* -delete
	tar jcf sulci-$(VERSION).tar.bz2 -C /tmp sulci-$(VERSION)
	tar zcf sulci-$(VERSION).tar.gz -C /tmp sulci-$(VERSION)

langcompile: langcompile.ml
	ocamlopt langcompile.ml -o langcompile

include $(OCAMLMAKEFILE)

