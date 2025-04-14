# tapestry Makefile; this project builds A-type projects (A-type generates its own headers) and their imports
CC      = gcc
PROJECT = tapestry
CFLAGS := $(CFLAGS) -I. -I./include -I../lib -fPIC \
	-Wno-incompatible-pointer-types -Wfatal-errors \
	-std=gnu11 -DMODULE="\"$(PROJECT)\""
OBJS    = tapestry.o A.o
TARGET  = tapestry

all: $(TARGET)

$(TARGET): $(OBJS)
	@mkdir -p bin
	$(CC) -o bin/tapestry $^ -lm 

A.o: ../A/lib/A.c
	$(CC) $(CFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(TARGET) *.o

install:
	@bash install.sh