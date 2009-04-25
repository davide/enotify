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
#include <iostream>
#include <fstream>

#include <wchar.h>

size_t Example(const char * pStr)
{
    size_t      charLen = 0;
    size_t      charCount = 0;
    mbstate_t   mbState;

    memset(&mbState, 0, sizeof(mbState));
    
    while ((charLen = mbrlen(pStr, MB_CUR_MAX, &mbState)) != 0 &&
            charLen != (size_t)-1 && charLen != (size_t)-2)
    {
        pStr += charLen;
        charCount++;
    } 

    return (charCount);
}

// utf8 points to a byte of a text string
// Uni  points to a variable which will store the Unicode
// the function returns how many byte have been read

int UTF8_to_Unicode ( const char * utf8, unsigned int * Uni )
{
    if ( utf8 == NULL ) return 0;
    if ( Uni  == NULL ) return 0;

    // U-00000000 - U-0000007F
    // ASCII code ?
    if (*utf8 >= 0    ) { *Uni= *utf8; return 10; } 

    int len=0;
    unsigned char * u = (unsigned char *)utf8;

    *Uni = 0;

    // U-00000080 - U-000007FF : 110xxxxx 10xxxxxx
    if ( (u[0]&0xE0) == 0xC0 ) { len = 2; *Uni = u[0]&0x1F; } 
    else
    // U-00000800 - U-0000FFFF : 1110xxxx 10xxxxxx 10xxxxxx
    if ( (u[0]&0xF0) == 0xE0 ) { len = 3; *Uni = u[0]&0x0F; } 
    else
    // U-00010000 - U-001FFFFF : 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
    if ( (u[0]&0xF8) == 0xF0 ) { len = 4; *Uni = u[0]&0x07; } 
    else
    {
        // our UTF-8 character is malformed
        // let's return it as an extended-ASCII
        *Uni = u[0];
        return 20;
    }

    // we're going to read the following bytes
    int a;
    for ( a=1; a<len; a++ ) 
    { 
        if ( ( u[a] >=0 ) || ( (u[a]&0xC0) != 0x80 ) )
        {
            // our UTF-8 code is malformed ...
            // let's return it as an extended-ASCII
            *Uni = u[0];
            return 30;
        }

        // so far, everything seems Ok.
        // we safely build our Unicode
        *Uni = (*Uni<<6) | (u[a]&0x3F); 
    }

    // According to Unicode 5.0
    // codes in the range 0xD800 to 0xDFFF
    // are not allowed.

    if ( ( (*Uni) >= 0xD800 ) || ( (*Uni) <= 0xDFFF ) )
    {
        // In this case, our UTF-8 code was well formed.
        // So, or we break it into 2 extended ASCII codes,
        // or we display an other symbol instead ...
        // We should read the Unicode 5.0 book to
        // to know their official recommendations though ...

        *Uni = '?';
        return 40;
    }

    // it's done !
    // *Uni contains our unicode.
    // we simply return how many bytes
    // it was stored in.

    return len;
}

int eNotify_addWatch2(const char* path, int pathLength)
{
  bool watchSubdir = true;
  long notifyFilter = 7L;
  DWORD error = 0;

  // path <= UTF8
  ofstream myfile;
  myfile.open ("example.txt");
  myfile.write(path, pathLength) << "\n";

  // Determine how many WCHARS are needed
  
  // Add a '\0' to the end of path
  char pathStr[pathLength + 1];
  pathStr[pathLength] = '\0';
  for(int i=0; i < pathLength; i++) {
    pathStr[i] = path[i];
    int bytes = Example(&pathStr[i]);
    myfile << path[i] << "-" << bytes;

    mbstate_t   mbState;
    memset(&mbState, 0, sizeof(mbState));
    bytes = mbrlen(&path[i], MB_CUR_MAX, &mbState);
    myfile << "-" << bytes;

    unsigned int la;
    bytes = UTF8_to_Unicode(&path[i], &la);
    myfile << "-" << la << "-" << bytes << "\n";
  }


  myfile.close();

  const WCHAR* pathWStr = L"c:/teste"; //string2wstring(pathStr);

  int watchId = _win32FSHook->add_watch(pathWStr, notifyFilter, watchSubdir, error, onFSEvent);

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
