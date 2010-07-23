#
# General Hash Function Algorithms Master MakeFile
# By Arash Partow - 2000
#
# URL: http://www.partow.net/programming/hashfunctions/index.html
#
# Copyright Notice:
# Free use of this library is permitted under the
# guidelines and in accordance with the most
# current version of the Common Public License.
# http://www.opensource.org/licenses/cpl1.0.php
#

COMPILER      = -cc
OPTIONS       = -std=c99 -pedantic -Wall -o
OPTIONS_LIBS  = -std=c99 -pedantic -Wall -c


all: GeneralHashFunctions.o HashVal

GeneralHashFunctions.o: GeneralHashFunctions.c GeneralHashFunctions.h
	$(COMPILER) $(OPTIONS_LIBS) GeneralHashFunctions.c

HashVal: GeneralHashFunctions.c HashVal.c
	$(COMPILER) $(OPTIONS) HashVal HashVal.c GeneralHashFunctions.o

clean:
	rm -f core *.o *.bak *stackdump *#

#
# The End !
#