# Make file for SQLite for CAMxRunner

ifeq ($(strip $(DESTINATION)),)
	DESTINATION = ../..
endif

# Compiler to use
CC = gcc
CFLAGS = -O2

all: sqlite

# Standard pbzip2 compile
sqlite: shell.c sqlite3.c
	$(CC) $(CFLAGS) $^ -o $(DESTINATION)/$@-$(HOSTTYPE)$(SUFFIX) -DDSQLITE_ENABLE_LOCKING_STYLE -DSQLITE_FIXED_LOCKING_STYLE=flockLockingStyle -ldl -lpthread


clean:
	rm -f *.o $(DESTINATION)/sqlite-$(HOSTTYPE)$(SUFFIX)
