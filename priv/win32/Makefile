
ifeq ($(TYPE),debug)
PURIFY =
TYPEMARKER = .debug
TYPE_FLAGS = -g -DDEBUG
endif

COMPILER = g++

WARNFLAGS = -Wall 

CFLAGS = -g -O2 $(WARNFLAGS) $(INCFLAGS) $(TYPE_FLAGS)
PROG_CFLAGS = -g -O2 $(WARNFLAGS) $(INCFLAGS) $(TYPE_FLAGS) -Ilegacy

BUILD_BASEDIR = .
OBJDIR     = $(BUILD_BASEDIR)/obj$(TYPEMARKER)
BINDIR     = $(BUILD_BASEDIR)/bin
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

EXE_TARGETS = \
	$(ENOTIFY_BIN)

TARGETS = \
	$(BINDIR) \
	$(EXE_TARGETS)

###########################################################################
#  List all source files
###########################################################################

HEADERS = \
	$(INCLUDE_DIR)/erl_comm.h

OBJECTS  = $(OBJDIR)/*.o

###########################################################################
#  Main targets
###########################################################################

all: dependencies $(TARGETS)

dependencies:
	cd src_c; make
	cd src_cpp; make

clean:
	rm -f $(ENOTIFY_BIN)

run:
	$(ENOTIFY_BIN) "local"

###########################################################################
#  Create directories
###########################################################################

$(BINDIR):
	mkdir -p $(BINDIR)

###########################################################################
#  enotify
###########################################################################

$(ENOTIFY_BIN): $(OBJECTS)
	$(COMPILER) -o $@ $(OBJECTS) $(LIBS)

###########################################################################