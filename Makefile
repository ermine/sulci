OCAMLMAKEFILE = ../OCamlMakefile

SOURCES := common.ml config.ml version.ml hooks.ml lang.ml \
	   iq.ml muc.ml \
	   plugin_ping.ml plugin_dict.ml plugin_mueller.ml \
	   plugin_weather.ml plugin_google.ml plugin_userinfo.ml \
	   plugin_roulette.ml plugin_currency.ml \
	   plugin_admin.ml plugin_vocabulary.ml \
	   math.ml pcalc.mly pcalc_lexer.mll \
           icalc.mly icalc_ulex.ml plugin_calc.ml \
	   plugin_markov.ml plugin_misc.ml \
	   plugin_globalstats.ml plugin_tld.ml \
	   sulci.ml
THREADS := yes
PACKS := ulex unix str netstring dbm
INCDIRS :=  ../xmpp ../xml ../libs/timer ../libs/strftime ../libs/http ../libs/sqlite_util ../packages/ocaml-sqlite-0.3.5
OCAMLLDFLAGS := nums.cmxa cryptokit.cmxa ../xml/xml.cmxa ../xmpp/xmpp.cmxa \
	     strftime.cmxa timer.cmxa \
	     sqlite.cmxa sqlite_util.cmxa http_client.cmxa \
	     -linkall -linkpkg
RESULT := sulci

all: nc

include $(OCAMLMAKEFILE)

