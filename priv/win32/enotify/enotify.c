#include "enotify.h"
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#ifdef __WIN32__
#include <fcntl.h>
#endif

#define MAX_FILE_PATHNAME_LENGTH 32767

void local_add_watch(ETERM* args)
{
  ETERM *pathp = erl_element(1, args);
  int pathLength = ERL_BIN_SIZE(pathp);
  char *path = ERL_BIN_PTR(pathp);

  ETERM *notifyFilterp = erl_element(2, args);
  unsigned int notifyFilter_ui = ERL_INT_UVALUE(notifyFilterp);
  long notifyFilter = (long)notifyFilter_ui;

  ETERM *watchSubdirsp = erl_element(3, args);
  int watchSubdirs = ERL_INT_VALUE(watchSubdirsp);

  int watchID = eNotify_addWatch(path, pathLength, notifyFilter, watchSubdirs);

  // Prepare response
  ETERM *tuplep;
  ETERM *tupleArray[2];

  if (watchID < 0)
  {
	  long errorCode = (long)(-watchID);
	  char errorDesc[1024];
	  eNotify_getErrorDesc(errorCode, errorDesc, 1024);
	  tupleArray[0] = erl_mk_atom("error");
	  tupleArray[1] = erl_mk_string(errorDesc);
  }
  else if (0 == watchID)
  {
	  // this may happen if there's a problem converting
	  // a path from utf8 to UTF16
	  tupleArray[0] = erl_mk_atom("error");
	  tupleArray[1] = erl_mk_string("internal_error");
  }
  else
  {
	  tupleArray[0] = erl_mk_atom("ok");
	  tupleArray[1] = erl_mk_int(watchID);
  }

  char buf[1024];
  tuplep = erl_mk_tuple(tupleArray, 2);
  erl_encode(tuplep, buf);
  write_cmd(buf, erl_term_len(tuplep));

  erl_free_array(tupleArray, 2); // free contents from tupleArray
}

void local_remove_watch(ETERM* args)
{
  byte buf[100];
  ETERM *resp;
  ETERM *watchIDp = erl_element(1, args);
  int watchID = ERL_INT_VALUE(watchIDp);

  eNotify_removeWatch(watchID);

  // Build response
  resp = erl_mk_atom("ok"); // alloc resp
  erl_encode(resp, buf);
  write_cmd(buf, erl_term_len(resp));
  erl_free_term(resp); // free resp
}

void local_get_error_desc(ETERM* args)
{
  byte buf[100];
  ETERM *resp;
  ETERM *errorCodep = erl_element(1, args);
  long errorCode = (long)ERL_INT_VALUE(errorCodep);

  char errorDesc[1024];
  eNotify_getErrorDesc(errorCode, errorDesc, 1024);

  // Build response
  resp = erl_mk_string(errorDesc); // alloc resp
  erl_encode(resp, buf);
  write_cmd(buf, erl_term_len(resp));
  erl_free_term(resp); // free resp
}

int main()
{
  ETERM *tuplep;
  ETERM *fnp;
  ETERM *args;
  byte buf[100];
  const char* func_name;

#ifdef __WIN32__
  /* Attention Windows programmers: you need [to pay Serge Aleynikov a
   * beer because he's the one who figured out how to get this to work
   * on windows :)] explicitly set mode of stdin/stdout to binary or
   * else the port program won't work.
   */
  _setmode(_fileno(stdout), _O_BINARY);
  _setmode(_fileno(stdin), _O_BINARY);
#endif

  eNotify_init();
  erl_init(NULL, 0);

  while (read_cmd(buf) > 0) {
    tuplep = erl_decode(buf);
    fnp = erl_element(1, tuplep);

    func_name =  (const char*)ERL_ATOM_PTR(fnp);
    args = erl_element(2, tuplep);

    // MATCH FIRST! -> REMEMBER THAT!
    if (strncmp(func_name, "get_error_desc", 14) == 0)
      {
		local_get_error_desc(args);
      }
    else if (strncmp(func_name, "add_watch", 9) == 0)
      {
	local_add_watch(args);
      }
    else if (strncmp(func_name, "remove_watch", 12) == 0)
      {
	local_remove_watch(args);
      }
    else
      {
	byte buf[10];
	ETERM *nok = erl_mk_atom("undef"); // alloc nok
	erl_encode(nok, buf);
	write_cmd(buf, erl_term_len(nok));
	erl_free_term(nok); // free nok
      }
    erl_free_compound(tuplep);
    erl_free_term(fnp);
  }
  return 0;
}

void eNotifyCallback(int watchID, int action, const void* rootPath, int rootPathLength, const void* filePath, int filePathLength)
{
 // MAX_FILE_PATHNAME_LENGTH * 2 because we are passing 2 paths
  byte buf[MAX_FILE_PATHNAME_LENGTH*2];
  ETERM *tuplep;

  // Build response
  ETERM *tupleArray[4];
  tupleArray[0] = erl_mk_int(watchID);
  tupleArray[1] = erl_mk_int(action);
  tupleArray[2] = erl_mk_binary(rootPath, rootPathLength);
  tupleArray[3] = erl_mk_binary(filePath, filePathLength);
  tuplep = erl_mk_tuple(tupleArray, 4);

  erl_encode(tuplep, buf);
  write_cmd(buf, erl_term_len(tuplep));

  // free contents from tupleArray
  erl_free_array(tupleArray, 4);
}
