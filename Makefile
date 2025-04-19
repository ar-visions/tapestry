CC      = gcc
PROJECT = tapestry
CFLAGS := $(CFLAGS) -I. -I./include -I../A/lib -fPIC \
	-Wno-incompatible-pointer-types -Wfatal-errors \
	-std=gnu11 -DMODULE="\"$(PROJECT)\""
ifeq ($(shell uname -s),Darwin)
	LIB_PRE = lib
	LIB_EXT = dylib
else
	LIB_PRE = lib
	LIB_EXT = so
endif
SHARED_OBJS = tapestry-shared.o A.o
SHARED_LIB  = lib/$(LIB_PRE)tapestry.$(LIB_EXT)
APP_OBJS    = tapestry.o
TARGET      = bin/tapestry

all: $(SHARED_LIB) $(TARGET)

tapestry-shared.o: tapestry-shared.c
	$(CC) $(CFLAGS) -c $< -o $@

$(SHARED_LIB): $(SHARED_OBJS)
	@mkdir -p lib
	$(CC) -shared -o $@ $^ -lm

$(TARGET): $(APP_OBJS)
	@mkdir -p bin
	$(CC) -o $@ $^ -Llib -ltapestry -lm

A.o: ../A/lib/A.c
	$(CC) $(CFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(TARGET) $(SHARED_LIB) $(SHARED_OBJS) $(APP_OBJS)

install:
	@bash install.sh