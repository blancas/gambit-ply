# +---------------------------------------------------------------------+
# |  GAMBIT PLY                                                         |
# |  A high-productivity function library for Gambit-C.                 |
# |                                                                     |
# |  Copyright 2012 Armando Blancas                                     |
# +---------------------------------------------------------------------+

GSC= /usr/local/Gambit-C/bin/gsc
FLAGS= -exe
BIN= bin
PLY= plylib
INC= ${PLY}/plylib.scm \
     ${PLY}/ply-io.scm \
     ${PLY}/ply-string.scm \
     ${PLY}/ply-sxml.scm

all : ${BIN}/fixws \
      ${BIN}/hits \
      ${BIN}/popclock \
      ${BIN}/plytest \
      ${BIN}/weather \
      ${BIN}/runpom \
      ${BIN}/runrepl \
      ${BIN}/catpom \
      ${BIN}/undouble

${BIN}/fixws : ${INC} fixws.scm
	${GSC} ${FLAGS} -o ${BIN}/fixws fixws.scm
	cp ${BIN}/fixws ~/bin

${BIN}/hits : ${INC} hits.scm
	${GSC} ${FLAGS} -o ${BIN}/hits hits.scm
	cp ${BIN}/hits ~/bin

${BIN}/popclock : ${INC} popclock.scm
	${GSC} ${FLAGS} -o ${BIN}/popclock popclock.scm
	cp ${BIN}/popclock ~/bin

${BIN}/plytest : ${INC} plytest.scm
	${GSC} ${FLAGS} -o ${BIN}/plytest plytest.scm
	cp ${BIN}/plytest ~/bin

${BIN}/weather : ${INC} weather.scm
	${GSC} ${FLAGS} -o ${BIN}/weather weather.scm
	cp ${BIN}/weather ~/bin

${BIN}/runpom : ${INC} runpom.scm
	${GSC} ${FLAGS} -o ${BIN}/runpom runpom.scm
	cp ${BIN}/runpom ~/bin

${BIN}/runrepl : ${INC} runrepl.scm
	${GSC} ${FLAGS} -o ${BIN}/runrepl runrepl.scm
	cp ${BIN}/runrepl ~/bin

${BIN}/catpom : ${INC} catpom.scm
	${GSC} ${FLAGS} -o ${BIN}/catpom catpom.scm
	cp ${BIN}/catpom ~/bin

${BIN}/undouble : ${INC} undouble.scm
	${GSC} ${FLAGS} -o ${BIN}/undouble undouble.scm
	cp ${BIN}/undouble ~/bin

clean :
	- rm -f ${BIN}/*
