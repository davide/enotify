#include "Win32FS.h"

Win32FSHook *_win32FSHook;

#include <iostream>
#include <fstream>
void write(string a)
{
  std::ofstream myfile( "winfs.log", std::ios::out | std::ios::binary | std::ios::app ) ;
  myfile << a.c_str() << endl;
  myfile.close();
}

void writeN(char *x, int l)
{
  std::ofstream myfile( "winfs.log", std::ios::out | std::ios::binary | std::ios::app ) ;
  myfile.write(x, l);
  myfile.close();
}

void onFSEvent(int watchID, int action, const WCHAR* rootPathUTF16, const WCHAR* filePathUTF16)
{
  /* */
  write("IN: onFSEvent");
  /* */
  
  // Handle UTF16 by default
  int rootPathUTF16Length = wcslen(rootPathUTF16) * sizeof(WCHAR);
  int filePathUTF16Length = wcslen(filePathUTF16) * sizeof(WCHAR);
  
  /* // Convert to UTF8 */
  string a = UTF16toUTF8(rootPathUTF16);
  string b = UTF16toUTF8(filePathUTF16);
  const char *rootPathUTF8 = a.c_str();
  const char *filePathUTF8 = b.c_str();
  int rootPathUTF8Length = strlen(rootPathUTF8);
  int filePathUTF8Length = strlen(filePathUTF8);
  if ((rootPathUTF8Length == 0) or (filePathUTF8Length == 0))
    return;

  /* * /
  std::ofstream myfile( "winfs.log", std::ios::out | std::ios::binary | std::ios::app ) ;
  /* * /
  myfile << "IN: MIDDLE" << endl;
  myfile.write((char*)rootPathUTF8, rootPathUTF8Length) << endl;
  myfile.write((char*)filePathUTF8, filePathUTF8Length) << endl;
  /* * /
  myfile.write((char*)rootPathUTF16, rootPathUTF16Length) << endl;
  myfile.write((char*)filePathUTF16, filePathUTF16Length) << endl;
  myfile << "OUT: MIDDLE" << endl;
  /* * /
  myfile.close();
  /* */
  
  eNotifyCallback(watchID, action, rootPathUTF16, rootPathUTF16Length, filePathUTF16, filePathUTF16Length);
  eNotifyCallback(watchID, action, rootPathUTF8, rootPathUTF8Length, filePathUTF8, filePathUTF8Length);
  write("OUT: onFSEvent\n");
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

int eNotify_addWatch2(const char* path, int pathLength)
{
  const WCHAR* wPath = (UTF8BinToUtf16Str(path, pathLength)).c_str();
  if (wPath == NULL)
	return 0;

  bool watchSubdir = false;
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
		buffer[len1 - 2] = '\0';
	
	LocalFree( lpMsgBuf );
	
	lock.unlock();
}
