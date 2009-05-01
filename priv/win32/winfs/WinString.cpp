#include "WinString.h"

#include <iostream>
#include <fstream>

wstring UTF8BinToUtf16Str(const char* bin, int binLength)
{
  // Check how many WCHAR characters are needed
  int len = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, bin, binLength, NULL, 0);
  if (0 == len)
	return L"";
	
  // Convert data to UTF16
  WCHAR utf16[len + 1];
  utf16[len] = '\0';
  
  /*std::ofstream myfile( "winfs.log", std::ios::out | std::ios::binary | std::ios::app ) ;
  //myfile << len;
  //myfile.write((char*)&utf16, (len+1) * sizeof(WCHAR));
  //myfile << endl;
  */
  int n = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, bin, binLength, &utf16[0], len);
  /*
  //myfile << n;
  myfile.write((char*)&utf16, (len+1) * sizeof(WCHAR));
  myfile << endl;
  myfile.close();
  */
  
  if (0 == n)
	return L"";
  return wstring(utf16);
}


string UTF16toUTF8_(const WCHAR* utf16)
{
  int len = WideCharToMultiByte(CP_UTF8, 0, utf16, -1, NULL, 0, 0, 0);
  if (0 == len)
    return "";

  char utf8[len+1];
  utf8[len] = '\0';
  
  std::ofstream myfile( "winfs.log", std::ios::out | std::ios::binary | std::ios::app ) ;
  myfile << wcslen(utf16);
  myfile.write((char*)&utf16, (wcslen(utf16)+1) * sizeof(WCHAR));
  myfile << endl;
  
  int n = WideCharToMultiByte(CP_UTF8, 0, utf16, -1, utf8, len, 0, 0);
  
  myfile << n;
  myfile.write((char*)&utf8, (len+1) * sizeof(char));
  myfile << endl;
  myfile.close();
  
  if (0 == n)
	return "";
  return string(utf8);
}


string UTF16toUTF8(const WCHAR* utf16)
{
  int len = WideCharToMultiByte(CP_UTF8, 0, utf16, -1, NULL, 0, 0, 0);
  if (0 == len)
    return "";
  char utf8[len];
  utf8[len-1] = '\0';

  WideCharToMultiByte(CP_UTF8, 0, utf16, -1, utf8, len, 0, 0);
  
  std::ofstream myfile( "winfs.log", std::ios::out | std::ios::binary | std::ios::app ) ;
  myfile << "UTF16toUTF8() " << len;
  myfile.write((char*)&utf8, (len) * sizeof(char));
  myfile << endl;
  myfile.close();
  
  return string(utf8);
}
