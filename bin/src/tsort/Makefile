# Make file for tsort for CAMxRunner

ifeq ($(strip $(DESTINATION)),)
	DESTINATION = ../..
endif

# Compiler to use
CC = gcc
CFLAGS = -fno-builtin-exit

all: tsort

# Standard pbzip2 compile
tsort: tsort.c
	$(CC) $(CFLAGS) $^ -o $(DESTINATION)/$@-$(HOSTTYPE)$(SUFFIX)


clean:
	rm -f *.o $(DESTINATION)/tsort-$(HOSTTYPE)$(SUFFIX)
