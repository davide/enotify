#include "enotify.h"
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>

#define MAX_FILE_PATHNAME_LENGTH 32767

void bar(ETERM* rcvTuple)
{
  ETERM *argp = erl_element(2, rcvTuple); // alloc 2nd elem
  int a = ERL_INT_VALUE(argp);
  erl_free_term(argp); // free 2nd elem

  int res = a;
  // Build response
  byte buf[100];
  ETERM *intp = erl_mk_int(res);
  erl_encode(intp, buf); // alloc intp
  write_cmd(buf, erl_term_len(intp));
  erl_free_term(intp); // free intp
}

void foo(ETERM* rcvTuple)
{
  ETERM *argp = erl_element(2, rcvTuple); // alloc 2nd elem
  //  int int_arg = ERL_INT_VALUE(argp);
  erl_free_term(argp); // free 2nd elem

  int watchID = 0;//eNotify_addWatch("c:/Davide Marquês", 7L, 1);

  // Build response
  byte buf[100];
  ETERM *intp = erl_mk_int(watchID);
  erl_encode(intp, buf); // alloc intp
  write_cmd(buf, erl_term_len(intp));
  erl_free_term(intp); // free intp
}

void local_add_watch(ETERM* rcvTuple)
{
  ETERM *pathp = erl_element(2, rcvTuple); // alloc path
  int pathLength = ERL_BIN_SIZE(pathp);
  char *path = ERL_BIN_PTR(pathp);

  ETERM *notifyFilterp = erl_element(3, rcvTuple); // alloc notifyFilter
  unsigned int notifyFilter_ui = ERL_INT_UVALUE(notifyFilterp);
  long notifyFilter = (long)notifyFilter_ui;
  erl_free_term(notifyFilterp); // free notifyFilter

  ETERM *watchSubdirsp = erl_element(4, rcvTuple); // alloc watchSubdirs
  int watchSubdirs = ERL_INT_VALUE(watchSubdirsp);
  erl_free_term(watchSubdirsp); // free watchSubdirs

  int watchID = eNotify_addWatch(path, pathLength, notifyFilter, watchSubdirs);

  erl_free_term(pathp); // free path

  // Build response
  byte buf[100];
  ETERM *intp = erl_mk_int(watchID);
  erl_encode(intp, buf); // alloc intp
  write_cmd(buf, erl_term_len(intp));
  erl_free_term(intp); // free intp
}

void local_remove_watch(ETERM* rcvTuple)
{
  ETERM *watchIDp = erl_element(2, rcvTuple); // alloc watchID
  int watchID = ERL_INT_VALUE(watchIDp);
  erl_free_term(watchIDp); // free watchID

  eNotify_removeWatch(watchID);

  // No response
}

void echo2(ETERM* rcvTuple)
{
  ETERM *pathp = erl_element(2, rcvTuple); // alloc path
  int pathLen = ERL_BIN_SIZE(pathp);
  void *path = ERL_BIN_PTR(pathp);

  FILE *pFile = fopen ("myfile.txt","wb+");
  if (pFile!=NULL)
    {
      fprintf(pFile, "\nGot a bin of length: %d\n", pathLen);
      fwrite(path, 1, pathLen, pFile);
      fclose(pFile);
    }
  
  eNotify_addWatch2(path, pathLen);

  // reply with ok
  byte buf[10];
  ETERM *okp = erl_mk_atom("ok"); // alloc okp
  erl_encode(okp, buf);
  write_cmd(buf, erl_term_len(okp));
  erl_free_term(okp); // free okp

  // finally free path
  erl_free_term(pathp);

}

void echo1(ETERM* rcvTuple)
{
    ETERM *pathp = erl_element(2, rcvTuple); // alloc path
  //char *path = erl_iolist_to_string(pathp);
  // can't really free path here because ERL_BIN_PTR hasn't copied it
 
  //eNotify_addWatch(path, strlen(path), 7L, 1);

  byte buf[10];
  ETERM *okp = erl_mk_atom("ok"); // alloc okp
  erl_encode(okp, buf);
  write_cmd(buf, erl_term_len(okp));
  erl_free_term(okp); // free okp

  // finally free path
  erl_free_term(pathp);
}


void echo3(ETERM* rcvTuple)
{
  FILE *pFile = fopen ("myfile.txt","ab+");
  if (pFile!=NULL)
    {
      fprintf(pFile, "\nEverything ok so far!\n");
      fclose(pFile);
    }

  ETERM *pathp = erl_element(2, rcvTuple); // alloc path
  int pathLen = ERL_BIN_SIZE(pathp);
  void *path = ERL_BIN_PTR(pathp);
  // can't really free path here because ERL_BIN_PTR hasn't copied it

  pFile = fopen ("myfile.txt","ab+");
  if (pFile!=NULL)
    {
      // fprintf(pFile, "Got a bin of length: %d\n", pathLen);
      // fwrite(path, 1, pathLen, pFile);
      fclose(pFile);
    }
  
  eNotify_addWatch(path, pathLen, 7L, 1);

  byte buf[10];
  ETERM *okp = erl_mk_atom("ok"); // alloc okp
  erl_encode(okp, buf);
  write_cmd(buf, erl_term_len(okp));
  erl_free_term(okp); // free okp

  // finally free path
  erl_free_term(pathp);

  /*
  // Build response
  byte buf[MAX_FILE_PATHNAME_LENGTH];
  int tupleArrayLen = 1;
  ETERM *tupleArray[tupleArrayLen];
  tupleArray[1] = erl_mk_string(path);
  ETERM *tuplep = erl_mk_tuple(tupleArray, tupleArrayLen);

  erl_encode(tuplep, buf);
  write_cmd(buf, erl_term_len(tuplep));

  // free contents from tupleArray
  erl_free_array(tupleArray, tupleArrayLen);
  */
}

int main()
{
  eNotify_init();
  
  ETERM *tuplep;
  ETERM *fnp;
  byte buf[100];

  erl_init(NULL, 0);

  //  eNotify_addWatch("c:/teste", 7, 1);
	
  while (read_cmd(buf) > 0) {
    tuplep = erl_decode(buf);
    fnp = erl_element(1, tuplep);
    const char* func_name =  (const char*)ERL_ATOM_PTR(fnp);

    // MATCH FIRST! -> REMEMBER THAT!
    if (strncmp(func_name, "foo", 3) == 0)
      {
	foo(tuplep);
      }
    else if (strncmp(func_name, "bar", 3) == 0)
      {
	bar(tuplep);
      }
    else if (strncmp(func_name, "add_watch", 9) == 0)
      {
	local_add_watch(tuplep);
      }
    else if (strncmp(func_name, "remove_watch", 12) == 0)
      {
	local_remove_watch(tuplep);
      }
    else if (strncmp(func_name, "echo1", 5) == 0)
      {
	echo1(tuplep);
      }
    else if (strncmp(func_name, "echo2", 5) == 0)
      {
	echo2(tuplep);
      }
    else if (strncmp(func_name, "echo3", 5) == 0)
      {
	echo3(tuplep);
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
