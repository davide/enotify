#include "enotify.h"
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#ifdef __WIN32__
#include <fcntl.h>
#endif

#define MAX_FILE_PATHNAME_LENGTH 32767

void ping()
{
  // Build response
  byte buf[100];
  ETERM *resp = erl_mk_atom("pong"); // alloc resp
  erl_encode(resp, buf);
  write_cmd(buf, erl_term_len(resp));
  erl_free_term(resp); // free resp
}

void bar(ETERM *args)
{
  ETERM *intp = erl_element(1, args);
  int value = ERL_INT_VALUE(intp);

  int res = value + 1;

  // Build response
  byte buf[100];
  ETERM *resp = erl_mk_int(res); // alloc resp
  erl_encode(resp, buf);
  write_cmd(buf, erl_term_len(resp));
  erl_free_term(resp); // free resp
}

void foo(ETERM* args)
{
  ETERM *intp = erl_element(1, args); // alloc 2nd elem
  int value = ERL_INT_VALUE(intp);

  int res = value * 2;

  // Build response
  byte buf[100];
  ETERM *resp = erl_mk_int(res); // alloc resp
  erl_encode(resp, buf);
  write_cmd(buf, erl_term_len(resp));
  erl_free_term(resp); // free resp
}

void echo_string(ETERM* args)
{
  ETERM *pathp = erl_element(1, args); // alloc path
  char *path = erl_iolist_to_string(pathp);
 
  // Build response
  byte buf[100];
  ETERM *resp = erl_mk_string(path); // alloc resp
  erl_encode(resp, buf);
  write_cmd(buf, erl_term_len(resp));
  erl_free_term(resp); // free resp
}

void echo_binary(ETERM* args)
{
  ETERM *pathp = erl_element(1, args);
  int pathLen = ERL_BIN_SIZE(pathp);
  void *path = ERL_BIN_PTR(pathp);
  
  // Build response
  byte buf[100];
  ETERM *resp = erl_mk_binary(path, pathLen);; // alloc resp
  erl_encode(resp, buf);
  write_cmd(buf, erl_term_len(resp));
  erl_free_term(resp); // free resp
}

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

  // Build response
  byte buf[100];
  ETERM *resp = erl_mk_int(watchID); // alloc resp
  erl_encode(resp, buf);
  write_cmd(buf, erl_term_len(resp));
  erl_free_term(resp); // free resp
}

void local_remove_watch(ETERM* args)
{
  ETERM *watchIDp = erl_element(1, args);
  int watchID = ERL_INT_VALUE(watchIDp);

  eNotify_removeWatch(watchID);

  // Build response
  byte buf[100];
  ETERM *resp = erl_mk_atom("ok"); // alloc resp
  erl_encode(resp, buf);
  write_cmd(buf, erl_term_len(resp));
  erl_free_term(resp); // free resp
}

int main()
{

#ifdef __WIN32__
  /* Attention Windows programmers: you need [to pay Serge Aleynikov a
   * beer because he's the one who figured out how to get this to work
   * on windows :)] explicitly set mode of stdin/stdout to binary or
   * else the port program won't work.
   */
  _setmode(_fileno(stdout), O_BINARY);
  _setmode(_fileno(stdin), O_BINARY);
#endif

  eNotify_init();
  
  ETERM *tuplep;
  ETERM *fnp;
  ETERM *args;
  byte buf[100];
  const char* func_name;

  erl_init(NULL, 0);

  while (read_cmd(buf) > 0) {
    tuplep = erl_decode(buf);
    fnp = erl_element(1, tuplep);

    func_name =  (const char*)ERL_ATOM_PTR(fnp);
    args = erl_element(2, tuplep);

    // MATCH FIRST! -> REMEMBER THAT!
    if (strncmp(func_name, "add_watch", 9) == 0)
      {
	local_add_watch(args);
      }
    else if (strncmp(func_name, "remove_watch", 12) == 0)
      {
	local_remove_watch(args);
      }
    else if (strncmp(func_name, "echo_string", 11) == 0)
      {
	echo_string(args);
      }
    else if (strncmp(func_name, "echo_binary", 11) == 0)
      {
	echo_binary(args);
      }
    else if (strncmp(func_name, "foo", 3) == 0)
      {
	foo(args);
      }
    else if (strncmp(func_name, "bar", 3) == 0)
      {
	bar(args);
      }
    else if (strncmp(func_name, "ping", 4) == 0)
      {
	ping();
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

  // Build response
  int tupleArrayLen = 4;
  ETERM *tupleArray[tupleArrayLen];
  tupleArray[0] = erl_mk_int(watchID);
  tupleArray[1] = erl_mk_int(action);
  tupleArray[2] = erl_mk_binary(rootPath, rootPathLength);
  tupleArray[3] = erl_mk_binary(filePath, filePathLength);
  ETERM *tuplep = erl_mk_tuple(tupleArray, tupleArrayLen);

  erl_encode(tuplep, buf);
  write_cmd(buf, erl_term_len(tuplep));

  // free contents from tupleArray
  erl_free_array(tupleArray, tupleArrayLen);
}
