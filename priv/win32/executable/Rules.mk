# Standard things

sp 		:= $(sp).x
dirstack_$(sp)	:= $(d)
d		:= $(dir)


# Local rules and targets

TGTS_$(d)	:= $(d)/enotify
DEPS_$(d)	:= $(TGTS_$(d):%=%.d)

TGT_BIN		:= $(TGT_BIN) $(TGTS_$(d))
CLEAN		:= $(CLEAN) $(TGTS_$(d)) $(DEPS_$(d))

$(TGTS_$(d)):	$(d)/Rules.mk

$(TGTS_$(d)):	CF_TGT := -Ienotify_c -Ienotify_cpp
$(TGTS_$(d)):	LL_TGT := $(S_LL_INET) erl_bridge/erl_bridge.a winfs/winfs.a -lerl_interface -lei -Lc:/otp_src_R13B_/lib/erl_interface/obj.debug/i686-pc-mingw32 -lwsock32
$(TGTS_$(d)):	erl_bridge/erl_bridge.a winfs/winfs.a
		$(CPP_COMPLINK)


# Standard things

-include	$(DEPS_$(d))

d		:= $(dirstack_$(sp))
sp		:= $(basename $(sp))
