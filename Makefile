all:
	${MAKE} -C ../xml
	${MAKE} -C ../xmpp
	${MAKE} -C ../misc
	${MAKE} -f Makefile.sulci

