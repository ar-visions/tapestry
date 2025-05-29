CC      = gcc
PROJECT = tapestry
CFLAGS := $(CFLAGS) -I. -I./include -I../A/lib -fPIC \
	-Wno-incompatible-pointer-types -Wfatal-errors \
	-std=gnu11 -DMODULE="\"$(PROJECT)\""
ifeq ($(shell uname -s),Darwin)
	LIB_PRE = lib
	LIB_EXT = dylib
	APP_LDFLAGS := -Wl,-rpath,@executable_path/../lib
	LIB_LDFLAGS := -Wl,-install_name,@rpath/$(LIB_PRE)tapestry.$(LIB_EXT)
else
	LIB_PRE = lib
	LIB_EXT = so
	APP_LDFLAGS := -Wl,-rpath,'$$ORIGIN/../lib'
	LIB_LDFLAGS := -Wl,-soname,$(LIB_PRE)tapestry.$(LIB_EXT)
endif
SHARED_OBJS = tapestry-shared.o A.o
SHARED_LIB  = lib/$(LIB_PRE)tapestry.$(LIB_EXT)
APP_OBJS    = tapestry.o
TARGET      = bin/tapestry




# — pad your tokens so you only ever match whole names —  
DBG_PAD     := ,$(DBG),  
PROJECT_PAD := ,$(PROJECT),  
EXCLUDE_PAD := ,-$(PROJECT),   # “-foo” means “do not debug foo”

# — compute a single DEBUG_COND that is non-empty if and only if
#   1) we see ,project, OR a literal * in DBG_PAD,  AND
#   2) we do NOT see ,-project, in DBG_PAD   —
DEBUG_COND := $(and \
    $(or $(findstring $(PROJECT_PAD),$(DBG_PAD)) \
         $(findstring *,$(DBG_PAD))) \
  ,$(not $(findstring $(EXCLUDE_PAD),$(DBG_PAD))) \
)

DBG          := $(strip $(DBG))
PROJECT      := $(strip $(PROJECT))
DBG_PAD      := ,$(DBG),
PROJECT_PAD  := ,$(PROJECT),
EXCLUDE_PAD  := ,-$(PROJECT),
DEBUG        := false

ifneq ($(findstring $(EXCLUDE_PAD),$(DBG_PAD)),)
  DEBUG := false
else ifneq ($(findstring $(PROJECT_PAD),$(DBG_PAD)),)
  DEBUG := true
else ifneq ($(findstring *,$(DBG_PAD)),)
  DEBUG := true
endif

ifeq ($(DEBUG),true)
    CFLAGS := $(CFLAGS) -g2 -O0 
else
    CFLAGS := $(CFLAGS) -O2
endif

all: $(SHARED_LIB) $(TARGET)

tapestry-shared.o: tapestry-shared.c
	$(CC) $(CFLAGS) -c $< -o $@

$(SHARED_LIB): $(SHARED_OBJS)
	@mkdir -p lib
	$(CC) -shared -o $@ $^ -lm $(LIB_LDFLAGS)

$(TARGET): $(APP_OBJS)
	@mkdir -p bin
	$(CC) -o $@ $^ -Llib -lm $(APP_LDFLAGS) -ltapestry 

A.o: ../A/lib/A.c
	$(CC) $(CFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(TARGET) $(SHARED_LIB) $(SHARED_OBJS) $(APP_OBJS)

install:
	@bash install.sh