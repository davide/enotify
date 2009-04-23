#include "Win32FS.h"

Win32FSHook *_win32FSHook;

void onFSEvent(int watchID, int action, const WCHAR* wchar_rootPath, const WCHAR* wchar_filePath)
{
  cout << "watchID " << watchID << " belonging to " << wchar_rootPath << " got action " << action << wchar_filePath << endl;
  // TODO: convert WCHAR* to char*
  char *rootPath = "rootPath";
  char *filePath = "filePath";
  eNotifyCallback(watchID, action, rootPath, filePath);
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

const WCHAR* string2wstring(const char *orig)
{
	const std::string input = orig;

	// null-call to get the size
	size_t needed = ::mbstowcs(NULL,&input[0],input.length());

	// allocate
	std::wstring output;
	output.resize(needed);

	// real call
	::mbstowcs(&output[0],&input[0],input.length());
	const WCHAR *pout = output.c_str();
	return pout;
}

int eNotify_addWatch(const char* char_path, long notifyFilter, bool watchSubdir)
{
  const WCHAR* path = L"c:/teste";//string2wstring(path);
  DWORD error = 0;
	//int add_watch(const WCHAR* path, long notifyFilter, bool watchSubdirs, DWORD &error,ChangeCallback changeCallback);
  int watchId = _win32FSHook->add_watch(path, notifyFilter, watchSubdir, error, onFSEvent);
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
