# Standard things

sp 		:= $(sp).x
dirstack_$(sp)	:= $(d)
d		:= $(dir)


## Subdirectories, in random order
#
#dir	:= $(d)/test
#include		$(dir)/Rules.mk
#


# Local variables

OBJS_$(d)	:= $(d)/Lock.o $(d)/Logger.o \
		   $(d)/WatchData.o $(d)/Win32FS.o \
		   $(d)/Win32FSHook.o $(d)/WinString.o
DEPS_$(d)	:= $(OBJS_$(d):%.o=%.d)

TGTS_$(d)	:= $(d)/$(TARGET)/winfs.a

CLEAN		:= $(CLEAN) $(OBJS_$(d)) $(DEPS_$(d)) \
		   $(TGTS_$(d))


# Local rules

$(OBJS_$(d)):	CF_TGT := -I$(d)

$(TGTS_$(d)):	TGT_DIR := $(d)/$(TARGET)
$(TGTS_$(d)):	$(OBJS_$(d))
		mkdir -p $(TGT_DIR)
		$(ARCH)

# Standard things

-include	$(DEPS_$(d))

d		:= $(dirstack_$(sp))
sp		:= $(basename $(sp))

