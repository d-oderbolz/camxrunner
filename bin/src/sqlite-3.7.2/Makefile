# Make file for SQLite for CAMxRunner

ifeq ($(strip $(DESTINATION)),)
	DESTINATION = ../..
endif

# Compiler to use
CC = gcc
CFLAGS = -O2

all: sqlite3 libsqlitefunctions.so

# Standard compilation
sqlite3: shell.c sqlite3.c
	$(CC) $(CFLAGS) $^ -o $(DESTINATION)/$@-$(HOSTTYPE)$(SUFFIX) -DDSQLITE_ENABLE_LOCKING_STYLE -DSQLITE_FIXED_LOCKING_STYLE=flockLockingStyle -ldl -lpthread

# The math library
libsqlitefunctions.so: extension-functions.c
	$(CC) -fPIC -lm -shared $^ -o $(LIBDIR)/$@$(SUFFIX)

clean:
	rm -f *.o $(DESTINATION)/sqlite3-$(HOSTTYPE)$(SUFFIX) $(LIBDIR)/libsqlitefunctions.so$(SUFFIX)
