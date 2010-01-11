SHELL=/bin/sh

MPI_INST = /usr/local/mpich

LIBNAME = libparlib.a

CC = gcc

LIB = -L$(MPI_INST)/lib -lmpich

INC = $(MPI_INST)/include

#####CFLAGS = -O2 -m32 -I $(INC)
CFLAGS = -O2 -I $(INC)

OBJS= par_assoc_buff.o  \
      par_send_noblock.o  \
      par_get_noblock.o  \
      par_wait.o   \
      par_init_put.o \
      par_put_int.o  \
      par_get_int.o  \
      par_put_float.o  \
      par_get_float.o 

$(LIBNAME) : $(OBJS)
	$(CC) $(CFLAGS) -c $(?:.o=.c)
	ar -rv $@ $?
	chmod 644 $@
clean:
	rm -rf *.o $(LIBNAME)
