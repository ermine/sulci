OCAMLMAKEFILE = ../OCamlMakefile

SOURCES := config.ml version.ml types.ml iq.ml muc.ml \
	   plugin_ping.ml plugin_userinfo.ml plugin_dict.ml \
	   plugin_google.ml plugin_mueller.ml plugin_weather.ml \
	   plugin_roulette.ml plugin_currency.ml \
	   sulci.ml
THREADS := yes
PACKS := ulex unix str netstring

OCAMLFLAGS := -I ../xmpp -I ../xml -I ../misc
OCAMLLDFLAGS := nums.cmxa cryptokit.cmxa ../xml/xml.cmxa ../xmpp/xmpp.cmxa \
	     ../misc/hex.cmx ../misc/http_client.cmx ../misc/strftime.cmx \
	     -linkall -linkpkg
RESULT := sulci

all: native-code

include $(OCAMLMAKEFILE)

