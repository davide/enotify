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

CLEAN		:= $(CLEAN) $(OBJS_$(d)) $(DEPS_$(d)) \
		   $(d)/winfs.a


# Local rules

$(OBJS_$(d)):	CF_TGT := -I$(d)

$(d)/winfs.a:	$(OBJS_$(d))
		$(ARCH)

# Standard things

-include	$(DEPS_$(d))

d		:= $(dirstack_$(sp))
sp		:= $(basename $(sp))

