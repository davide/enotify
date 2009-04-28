#include "Win32FS.h"

Win32FSHook *_win32FSHook;

void onFSEvent(int watchID, int action, const WCHAR* rootPath, const WCHAR* filePath)
{
  int rootPathLength = wcslen(rootPath) * sizeof(WCHAR);
  int filePathLength = wcslen(filePath) * sizeof(WCHAR);
  eNotifyCallback(watchID, action, rootPath, rootPathLength, filePath, filePathLength);
}

int eNotify_init(void)
{
  try
  {
    _win32FSHook = new Win32FSHook();
    _win32FSHook->init(&onFSEvent);
    return 0;
  }
  catch (int err) {
	  return err;
  }
}

wstring utf8BinaryToUtf16String(const char* bin, int binLength)
{
  // Convert to a regular string
  char str[binLength + 1];
  str[binLength] = '\0';
  for(int i=0; i < binLength; i++) {
    str[i] = bin[i];
  }
  
  // Check how many WCHAR characters are needed
  int len = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, str, binLength+1, NULL, 0);
  if (len == 0)
	return 0;

  // Convert data to UTF16
  WCHAR utf16[len + 1];
  int n = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, str, binLength+1, &utf16[0], len);
  if (n == 0)
	return 0;
  return wstring(utf16);
}

int eNotify_addWatch2(const char* path, int pathLength)
{
  const WCHAR* wPath = (utf8BinaryToUtf16String(path, pathLength)).c_str();
  bool watchSubdir = true;
  long notifyFilter = 7L;
  DWORD error = 0;
  int watchId = _win32FSHook->add_watch(wPath, notifyFilter, watchSubdir, error, onFSEvent);
  if (watchId == 0)
    return -error;
  return watchId;
}


int eNotify_addWatch(const void* path, int pathLength, long notifyFilter, int int_watchSubdir)
{
  bool watchSubdir = (int_watchSubdir != 0);
  DWORD error = 0;

  const WCHAR *castedPath = (WCHAR*)path;
  int wPathLength = pathLength / sizeof(WCHAR);
  WCHAR wPath[wPathLength + 1];
  wPath[wPathLength] = '\0';
  int i;
  for(i=0; i < wPathLength; i++)
    wPath[i] = castedPath[i];

  int watchId = _win32FSHook->add_watch(wPath, notifyFilter, watchSubdir, error, onFSEvent);

  if (watchId == 0)
    return -error;
  return watchId;
}

void eNotify_removeWatch(int watchId)
{
  _win32FSHook->remove_watch(watchId);
}

void getErrorDescription(int errorCode, WCHAR *buffer, int len);

/* WARNING: allocates memory! */
const WCHAR* getErrorDesc(long errorCode)
{
	WCHAR buffer[1024];
	getErrorDescription(errorCode, buffer, sizeof(buffer) / sizeof(WCHAR));
	return NULL; //buffer;
}

void getErrorDescription(int errorCode, WCHAR *buffer, int len)
{
	static Lock lock;
	lock.lock();
	
	LPVOID lpMsgBuf;
	FormatMessageW( 
	    FORMAT_MESSAGE_ALLOCATE_BUFFER | 
	    FORMAT_MESSAGE_FROM_SYSTEM | 
	    FORMAT_MESSAGE_IGNORE_INSERTS,
	    NULL,
	    errorCode,
	    0, // Default language
	    (LPWSTR) &lpMsgBuf,
	    0,
	    NULL 
	);

	_snwprintf(buffer, len, L"Error %d : %s", errorCode, (LPCTSTR)lpMsgBuf);
	int len1 = wcslen(buffer);
	if (len1 >= 2)
	{
		buffer[len1 - 2] = '\0';
	}
	
	LocalFree( lpMsgBuf );
	
	lock.unlock();
}
