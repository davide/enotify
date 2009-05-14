#include "WinString.h"

wstring UTF8BinToUtf16Str(const char* bin, int binLength)
{
  // Check how many WCHAR characters are needed
  int len = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, bin, binLength, NULL, 0);
  if (0 == len)
	return L"";
	
  // Convert data to UTF16
  WCHAR *utf16 = new WCHAR[len + 1];
  utf16[len-1] = '\0';
  int n = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, bin, binLength, &utf16[0], len);
  if (0 == n)
  {
    delete[] utf16;
	return L"";
  }
  wstring ret = wstring(utf16);
  delete[] utf16;
  return ret;
}

string UTF16toUTF8(const WCHAR* utf16)
{
  int len = WideCharToMultiByte(CP_UTF8, 0, utf16, -1, NULL, 0, 0, 0);
  if (0 == len)
    return "";
  char *utf8 = new char[len];
  utf8[len-1] = '\0';
  int n = WideCharToMultiByte(CP_UTF8, 0, utf16, -1, utf8, len, 0, 0);
  if (0 == n)
  {
    delete[] utf8;
	return "";
  }
  string ret = string(utf8);
  delete[] utf8;
  return ret;
}

void UTF16toUTF8_inBuffer(const WCHAR* utf16, char* utf8, int utf8Len)
{
  int len = WideCharToMultiByte(CP_UTF8, 0, utf16, -1, NULL, 0, 0, 0);
  if (0 == len) {
	utf8[0] = '\0';
    return;
  }
  int n = WideCharToMultiByte(CP_UTF8, 0, utf16, -1, utf8, utf8Len, 0, 0);
  if (0 == n) {
	utf8[0] = '\0';
    return;
  }
}
