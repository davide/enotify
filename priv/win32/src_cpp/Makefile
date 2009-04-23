
ifeq ($(TYPE),debug)
PURIFY =
TYPEMARKER = .debug
TYPE_FLAGS = -g -DDEBUG
endif

COMPILER = g++

WARNFLAGS = -Wall 
CFLAGS = -g -O2 $(WARNFLAGS) $(INCFLAGS) $(TYPE_FLAGS)
PROG_CFLAGS = -g -O2 $(WARNFLAGS) $(INCFLAGS) $(TYPE_FLAGS) -Ilegacy

BUILD_BASEDIR = c:/Projectos/eNotify/priv/win32
OBJDIR     = $(BUILD_BASEDIR)/obj$(TYPEMARKER)
BINDIR     = $(BUILD_BASEDIR)/../../bin
INCLUDE_DIR = $(BUILD_BASEDIR)/include

LIBS = 

###########################################################################
#  List targets
###########################################################################

###########################################################################
#  Specify targets to build
###########################################################################

TARGETS = \
	$(OBJDIR)

###########################################################################
#  List all source files
###########################################################################

HEADERS = \
	$(INCLUDE_DIR)/Lock.h \
	$(INCLUDE_DIR)/WatchData.h \
	$(INCLUDE_DIR)/Logger.h \
	$(INCLUDE_DIR)/Win32FSHook.h \
	$(INCLUDE_DIR)/Win32FS.h

SOURCES = \
	Lock.cpp \
	Logger.cpp \
	WatchData.cpp \
	Win32FS.cpp \
	Win32FSHook.cpp

OBJECTS  = $(addprefix $(OBJDIR)/,$(notdir $(SOURCES:.cpp=.o)))
TARGETS +=  $(OBJECTS)

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
	$(COMPILER) $(CFLAGS) -c $< -o $@ -I$(INCLUDE_DIR)

###########################################################################
#  Create directories
###########################################################################

$(OBJDIR):
	mkdir -p $(OBJDIR)

###########################################################################