#include "WinString.h"

const WCHAR* string2wstring(const char* orig)
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

const char* wstring2string(const WCHAR* orig)
{
  const std::wstring input = orig;

  // null-call to get the size
  size_t needed = ::wcstombs(NULL,&input[0],input.length());

  // allocate
  std::string output;
  output.resize(needed);

  // real call
  ::wcstombs(&output[0],&input[0],input.length());
  const char *pout = output.c_str();
  return pout;
}
