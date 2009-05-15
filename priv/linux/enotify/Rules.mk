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

OBJS_$(d)	:= $(d)/erl_comm.o $(d)/inotify_driver.o \
		   $(d)/inotify_erlang.o
DEPS_$(d)	:= $(OBJS_$(d):%.o=%.d)

TGTS_$(d)	:= $(d)/$(TARGET)/enotify$(BIN_EXT)

CLEAN		:= $(CLEAN) $(OBJS_$(d)) $(DEPS_$(d)) \
		   $(TGTS_$(d))


# Local rules

$(OBJS_$(d)):	CF_TGT := -I$(d) -I$(ERL_INTERFACE_DIR)/include

TGT_BIN		:= $(TGT_BIN) $(TGTS_$(d))

$(TGTS_$(d)):	TGT_DIR := $(d)/$(TARGET)
$(TGTS_$(d)):	CF_TGT := 
$(TGTS_$(d)):	LL_TGT := $(S_LL_INET) $(ERL_INTERFACE_LIBRARY_BINDING)
$(TGTS_$(d)):	$(OBJS_$(d))
		mkdir -p $(TGT_DIR)
		$(CPP_LINK)

# Standard things

-include	$(DEPS_$(d))

d		:= $(dirstack_$(sp))
sp		:= $(basename $(sp))
