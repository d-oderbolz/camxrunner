#
#                              Makefile.UNX
#
# Makes dos2unix and unix2dos under Unix
#

CFLAGS = -O

all          : dos2unix unix2dos

dos2unix     : dos2unix.c
	cc $(CFLAGS) -o ../../dos2unix-${HOSTTYPE} dos2unix.c

unix2dos     : unix2dos.c
	cc $(CFLAGS) -o ../../unix2dos-${HOSTTYPE} unix2dos.c


