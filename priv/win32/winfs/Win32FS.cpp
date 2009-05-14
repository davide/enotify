#include "Win32FS.h"

Win32FSHook *_win32FSHook;

void onFSEvent(int watchID, int action, const WCHAR* rootPathUTF16, const WCHAR* filePathUTF16)
{
  /* // Convert to UTF8 */
  string a = UTF16toUTF8(rootPathUTF16);
  string b = UTF16toUTF8(filePathUTF16);
  const char *rootPathUTF8 = a.c_str();
  const char *filePathUTF8 = b.c_str();
  int rootPathUTF8Length = strlen(rootPathUTF8);
  int filePathUTF8Length = strlen(filePathUTF8);
  if (0 == rootPathUTF8Length)
	return;
  if (0 == filePathUTF8Length)
    return;
  
  eNotifyCallback(watchID, action, rootPathUTF8, rootPathUTF8Length, filePathUTF8, filePathUTF8Length);
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

int eNotify_addWatch(const void* path, int pathLength, long notifyFilter, int int_watchSubdir)
{
  bool watchSubdir = (int_watchSubdir != 0);
  DWORD error = 0;

  wstring wcsPath = UTF8BinToUtf16Str((const char*)path, pathLength);
  const WCHAR* wPath = wcsPath.c_str();
  if (wcslen(wPath) == 0)
	return 0;

  int watchId = _win32FSHook->add_watch(wPath, notifyFilter, watchSubdir, error, onFSEvent);
  if (watchId == 0)
    return -error;
  return watchId;
}

void eNotify_removeWatch(int watchId)
{
  _win32FSHook->remove_watch(watchId);
}

void eNotify_getErrorDesc(long errorCode, char *buffer, int len)
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

	UTF16toUTF8_inBuffer((WCHAR*)lpMsgBuf, buffer, len);

	// Ditch the final \r\n in the message
	int len1 = strlen(buffer);
	if (len1 >= 2)
		buffer[len1 - 2] = '\0';
	
	LocalFree( lpMsgBuf );
	
	lock.unlock();
}
