OCAMLMAKEFILE = ../OCamlMakefile

#PRE_TARGETS := ../misc/strftime.cmx ../misc/timer.cmx
SOURCES := common.ml config.ml version.ml hooks.ml lang.ml \
	   iq.ml muc.ml \
	   plugin_ping.ml plugin_dict.ml plugin_mueller.ml \
	   plugin_weather.ml plugin_google.ml plugin_userinfo.ml \
	   plugin_roulette.ml plugin_currency.ml \
	   plugin_admin.ml plugin_vocabulary.ml \
	   pcalc.mly pcalc_lexer.mll \
           icalc.mly icalc_lexer.mll plugin_calc.ml \
	   plugin_markov.ml plugin_misc.ml \
	   plugin_globalstats.ml plugin_tld.ml \
	   sulci.ml
THREADS := yes
PACKS := ulex unix str netstring dbm sqlite
INCDIRS :=  ../xmpp ../xml ../misc
OCAMLLDFLAGS := nums.cmxa cryptokit.cmxa ../xml/xml.cmxa ../xmpp/xmpp.cmxa \
	     ../misc/strftime.cmxa ../misc/timer.cmxa \
	     ../misc/sqlite_util.cmxa ../misc/http_client.cmxa \
	     -linkall -linkpkg
RESULT := sulci

all:
	${MSKE} -C ../xml
	${MAKE} -C ../xmpp
	${MAKE} -C ../misc
	${MAKE} -f $(OCAMLMAKEFILE) native-code

include $(OCAMLMAKEFILE)

