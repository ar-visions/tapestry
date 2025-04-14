MAKEFILE_PATH := $(abspath $(word $(shell expr $(words $(MAKEFILE_LIST)) - 1),$(MAKEFILE_LIST)))
ifeq ($(MAKEFILE_PATH),)
    MAKEFILE_PATH := $(abspath $(lastword $(MAKEFILE_LIST)))
endif

SRC_ROOT           := $(patsubst %/,%,$(dir $(MAKEFILE_PATH)))
MAKEFILE_PATH_ROOT := $(abspath $(lastword $(MAKEFILE_LIST)))
TAPESTRY           ?= $(patsubst %/,%,$(dir $(MAKEFILE_PATH_ROOT)))
TAPESTRY_FILE      := $(SRC_ROOT)/build

ifeq (,$(findstring $(PROJECT),$(DBG)))
BUILD_DIR := $(SRC_ROOT)/build
else
BUILD_DIR := $(SRC_ROOT)/debug
endif

TAPESTRY_BIN := $(TAPESTRY_ROOT)/tapestry
SRC     ?= $(SRC_ROOT)
DBG     ?=

.PHONY: all tapestry
all: $(TAPESTRY_BIN)
	@echo "==> Building project with tapestry"
	@SRC=$(SRC) TAPESTRY=$(TAPESTRY) DBG=$(DBG) $(TAPESTRY_BIN) $(TAPESTRY_FILE)

$(TAPESTRY_BIN):
	@echo "==> Bootstrapping tapestry..."
	@$(MAKE) -C $(TAPESTRY_ROOT)

.PHONY: clean
clean:
	rm -rf $(BUILD_DIR)