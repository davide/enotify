
ifeq ($(TYPE),debug)
PURIFY =
TYPEMARKER = .debug
TYPE_FLAGS = -g -DDEBUG
endif

COMPILER = gcc

WARNFLAGS = -Wall 
CFLAGS = -g -O2 $(WARNFLAGS) $(INCFLAGS) $(TYPE_FLAGS)
PROG_CFLAGS = -g -O2 $(WARNFLAGS) $(INCFLAGS) $(TYPE_FLAGS) -Ilegacy

BUILD_BASEDIR = c:/Projectos/eNotify/priv/win32
OBJDIR     = $(BUILD_BASEDIR)/obj$(TYPEMARKER)
BINDIR     = $(BUILD_BASEDIR)/../../bin
INCLUDE_DIR = $(BUILD_BASEDIR)/include
EXTERNAL_INCLUDES = -Ic:/erl5.7.1/lib/erl_interface-3.6.1/include

LIBS = -lerl_interface -lei -LC:/otp_src_R13B/lib/erl_interface/obj/i686-pc-mingw32 -lwsock32

###########################################################################
#  List targets
###########################################################################

###########################################################################
#  Specify targets names
###########################################################################

ENOTIFY_BIN   = $(BINDIR)/enotify.exe

###########################################################################
#  Specify targets to build
###########################################################################

TARGETS = \
	$(OBJDIR)

###########################################################################
#  List all source files
###########################################################################

HEADERS = \
	$(INCLUDE_DIR)/erl_comm.h

SOURCES = \
	enotify.c \
	erl_comm.c

OBJECTS  = $(addprefix $(OBJDIR)/,$(notdir $(SOURCES:.c=.o)))
TARGETS += $(OBJECTS)
###########################################################################
#  Main targets
###########################################################################

debug: $(TARGETS)

clean:
	rm -f $(OBJECTS)

###########################################################################
#  Default rules
###########################################################################

$(OBJDIR)/%.o: %.c $(HEADERS)
	$(COMPILER) $(CFLAGS) -c $< -o $@ -I$(INCLUDE_DIR) $(EXTERNAL_INCLUDES)

###########################################################################
#  Create directories
###########################################################################

$(OBJDIR):
	mkdir -p $(OBJDIR)

###########################################################################