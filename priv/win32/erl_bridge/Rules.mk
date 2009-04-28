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

OBJS_$(d)	:= $(d)/enotify.o \
		   $(d)/erl_comm.o
DEPS_$(d)	:= $(OBJS_$(d):%.o=%.d)

CLEAN		:= $(CLEAN) $(OBJS_$(d)) $(DEPS_$(d)) \
		   $(d)/erl_bridge.a


# Local rules

$(OBJS_$(d)):	CF_TGT := -I$(d) -Ic:/erl5.7.1/lib/erl_interface-3.6.1/include

$(d)/erl_bridge.a:	$(OBJS_$(d))
		$(ARCH)

# Standard things

-include	$(DEPS_$(d))

d		:= $(dirstack_$(sp))
sp		:= $(basename $(sp))
