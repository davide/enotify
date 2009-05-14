#ifndef _ENotify_Win32FS

#include <windows.h>
#include <winbase.h>
#include <winnt.h>

#include "Win32FSHook.h"
#include "Logger.h"
#include "Lock.h"
#include "WinString.h"

#define _ENotify_Win32FS

#define ENotify_Win32FS_FILE_ACTION_ADDED 1L
#define ENotify_Win32FS_FILE_ACTION_REMOVED 2L
#define ENotify_Win32FS_FILE_ACTION_MODIFIED 3L
#define ENotify_Win32FS_FILE_ACTION_RENAMED_OLD_NAME 4L
#define ENotify_Win32FS_FILE_ACTION_RENAMED_NEW_NAME 5L
#define ENotify_Win32FS_FILE_ACTION_ANY 7L
	
static int FILE_CREATED = 1L;
static int FILE_DELETED = 2L;
static int FILE_MODIFIED = 4L;
static int FILE_RENAMED = 8L;
static int FILE_ANY = FILE_CREATED | FILE_DELETED | FILE_MODIFIED | FILE_RENAMED;

extern "C"
{
  int eNotify_init(void);
  int eNotify_addWatch(const void* path, int pathLength, long notifyFilter, int watchSubdir);
  void eNotify_removeWatch(int);
  void eNotify_getErrorDesc(long, char*, int);
  extern void eNotifyCallback(int watchID, int action, const void* rootPath, int rootPathLength, const void* filePath, int filePathLength);
}

#endif
