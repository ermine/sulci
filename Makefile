
OCAMLMAKEFILE = ../OCamlMakefile

include Makefile.conf

SOURCES = version.ml config.ml common.ml types.ml lang.ml muc.ml muc_log.ml hooks.ml iq.ml

#ifdef MUC_LOG
#  SOURCES += muc_log.ml
#endif

ifdef PLUGIN_GOOGLE
  SOURCES += plugin_google.ml
  MIDGE = yes
endif
ifdef PLUGIN_CALC
  SOURCES += math.ml pcalc.mly pcalc_lexer.mll icalc.mly icalc_ulex.ml plugin_calc.ml
  USE_CAMLP4=yes
  OCAMLDEP = ocamldep -package ulex -syntax camlp4o
  OCAMLFLAGS = -syntax camlp4o
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
  MIDGE = yes
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
  USE_CAMLP4=yes
  OCAMLDEP = ocamldep -package ulex -syntax camlp4o
  OCAMLFLAGS = -syntax camlp4o
endif
ifdef PLUGIN_TRANSLATE
  SOURCES += plugin_translate.ml
  MIDGE = yes
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
PACKS = ulex unix str netstring $(DBM_LIB)

ifdef MIDGE
  PACKS += netclient
endif

INCDIRS = ../libs/getopt ../libs/xml ../xmpp ../libs/xmlstring ../libs/scheduler \
	  ../libs/strftime

ifdef SQLITE
  INCDIRS += ../packages/ocaml-sqlite-0.3.5 ../libs/sqlite_util
endif

ifdef MIDGE
  INCDIRS += ../libs/midge
endif

OCAMLLDFLAGS =  nums.cmxa cryptokit.cmxa \
		getopt.cmxa xml.cmxa xmpp.cmxa xmlstring.cmxa strftime.cmxa \
		scheduler.cmxa $(CURR_LIB)

ifdef XMLSTRING_NETSTRING
  OCAMLLDFLAGS += xmlstring_netstring.cmxa
endif

ifdef SQLITE
  OCAMLLDFLAGS += sqlite.cmxa sqlite_util.cmxa
endif

ifdef MIDGE
  OCAMLLDFLAGS += midge.cmxa
endif

OCAMLLDFLAGS += -linkall -linkpkg

RESULT = sulci

all: nc langcompile

langcompile: langcompile.ml
	ocamlopt langcompile.ml -o langcompile

include $(OCAMLMAKEFILE)

