#ifndef _ENotify_WinString
#define _ENotify_WinString

#include <windows.h>
#include <winbase.h>
#include <winnt.h>
#include <cstdlib>
#include <string>

const WCHAR* string2wstring(const char* orig);
const char* wstring2string(const WCHAR* orig);

#endif
