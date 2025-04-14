# tapestry Makefile; this project builds A-type/silver projects and their imports
CC      = gcc
PROJECT = tapestry
CFLAGS := $(CFLAGS) -I. -I./include -I../lib -fPIC \
	-Wno-incompatible-pointer-types -Wfatal-errors \
	-std=gnu11 -DMODULE="\"$(PROJECT)\""
OBJS    = tapestry.o A.o
TARGET  = tapestry

all: $(TARGET)

$(TARGET): $(OBJS)
	$(CC) -o tapestry $^ -lm 

A.o: ../lib/A.c
	$(CC) $(CFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(TARGET) *.o

install:
	sudo install -m 755 $(TARGET) /usr/local/bin/$(TARGET)