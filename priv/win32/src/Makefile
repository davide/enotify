
ifeq ($(TYPE),debug)
PURIFY =
TYPEMARKER = .debug
TYPE_FLAGS = -g -DDEBUG
endif

COMPILER = g++
#AR = ar
#RANLIB = ranlib

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

EXE_TARGETS = \
	$(ENOTIFY_BIN)

TARGETS = \
	$(BINDIR) \
	$(OBJDIR) \
	$(EXE_TARGETS)

###########################################################################
#  List all source files
###########################################################################

# FIXME do we need dummy here for XX.h that was needed before??

HEADERS = \
	$(INCLUDE_DIR)/Lock.h \
	$(INCLUDE_DIR)/WatchData.h \
	$(INCLUDE_DIR)/Logger.h \
	$(INCLUDE_DIR)/Win32FSHook.h \
	$(INCLUDE_DIR)/Win32FS.h \
	$(INCLUDE_DIR)/ENotify.h \
	$(INCLUDE_DIR)/erl_comm.h

SOURCES = \
	Lock.cpp \
	Logger.cpp \
	WatchData.cpp \
	Win32FS.cpp \
	Win32FSHook.cpp \
	ENotify.cpp \
	erl_comm.cpp

OBJECTS  = $(addprefix $(OBJDIR)/,$(notdir $(SOURCES:.cpp=.o)))

###########################################################################
#  Main targets
###########################################################################

debug: $(TARGETS)

clean:
	rm -f $(OBJECTS)

###########################################################################
#  Default rules
###########################################################################

$(OBJDIR)/%.o: %.cpp $(HEADERS)
	$(COMPILER) $(CFLAGS) -c $< -o $@ -I$(INCLUDE_DIR) $(EXTERNAL_INCLUDES)

###########################################################################
#  Create directories
###########################################################################

$(BINDIR):
	mkdir -p $(BINDIR)

$(OBJDIR):
	mkdir -p $(OBJDIR)

###########################################################################
#  Special rules
###########################################################################

# ... archive creation

###########################################################################
#  enotify
###########################################################################

$(ENOTIFY_BIN): $(OBJECTS)
	$(COMPILER) -o $@ $(OBJECTS) $(LIBS)

###########################################################################