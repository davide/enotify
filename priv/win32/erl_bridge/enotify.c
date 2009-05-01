#include "enotify.h"
#include <windows.h>
#include <stdio.h>
#include <stdlib.h>

#define MAX_FILE_PATHNAME_LENGTH 32767

void bar(ETERM* args)
{
  ETERM *argp = erl_element(1, args); // alloc 2nd elem
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

void foo(ETERM* args)
{
  ETERM *argp = erl_element(1, args); // alloc 2nd elem
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

void local_add_watch(ETERM* args)
{
  ETERM *pathp = erl_element(1, args); // alloc path
  int pathLength = ERL_BIN_SIZE(pathp);
  char *path = ERL_BIN_PTR(pathp);

  ETERM *notifyFilterp = erl_element(3, args); // alloc notifyFilter
  unsigned int notifyFilter_ui = ERL_INT_UVALUE(notifyFilterp);
  long notifyFilter = (long)notifyFilter_ui;
  erl_free_term(notifyFilterp); // free notifyFilter

  ETERM *watchSubdirsp = erl_element(4, args); // alloc watchSubdirs
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

void local_remove_watch(ETERM* args)
{
  ETERM *watchIDp = erl_element(1, args); // alloc watchID
  int watchID = ERL_INT_VALUE(watchIDp);
  erl_free_term(watchIDp); // free watchID

  eNotify_removeWatch(watchID);

  // No response
}

void echo2(ETERM* args)
{
  ETERM *pathp = erl_element(1, args); // alloc path
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

void echo1(ETERM* args)
{
    ETERM *pathp = erl_element(1, args); // alloc path
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


void echo3(ETERM* args)
{
  FILE *pFile = fopen ("myfile.txt","ab+");
  if (pFile!=NULL)
    {
      fprintf(pFile, "\nEverything ok so far!\n");
      fclose(pFile);
    }

  ETERM *pathp = erl_element(1, args); // alloc path
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
}

void test()
{
  byte buf[100];
  ETERM *okp = erl_mk_atom("New Folder"); // alloc okp
  erl_encode(okp, buf);
  write_cmd(buf, erl_term_len(okp));
  erl_free_term(okp); // free okp
}

int main()
{
  eNotify_init();
  
  ETERM *tuplep;
  ETERM *fnp;
  ETERM *args;
  byte buf[100];

  erl_init(NULL, 0);

  //  eNotify_addWatch("c:/teste", 7, 1);
	
  while (read_cmd(buf) > 0) {
    tuplep = erl_decode(buf);
    fnp = erl_element(1, tuplep);
    const char* func_name =  (const char*)ERL_ATOM_PTR(fnp);
	args = erl_element(2, tuplep);

    // MATCH FIRST! -> REMEMBER THAT!
    if (strncmp(func_name, "foo", 3) == 0)
      {
		foo(args);
      }
    else if (strncmp(func_name, "bar", 3) == 0)
      {
		bar(args);
      }
    else if (strncmp(func_name, "add_watch", 9) == 0)
      {
		local_add_watch(args);
      }
    else if (strncmp(func_name, "remove_watch", 12) == 0)
      {
		local_remove_watch(args);
      }
    else if (strncmp(func_name, "echo1", 5) == 0)
      {
		echo1(args);
      }
    else if (strncmp(func_name, "echo2", 5) == 0)
      {
		echo2(args);
      }
    else if (strncmp(func_name, "echo3", 5) == 0)
      {
		echo3(args);
      }
	else if (strncmp(func_name, "test", 5) == 0)
      {
		test();
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
    erl_free_compound(tuplep);
    erl_free_term(fnp);
  }
  return 0;
}

void eNotifyCallback(int watchID, int action, const void* rootPath, int rootPathLength, const void* filePath, int filePathLength)
{
 // MAX_FILE_PATHNAME_LENGTH * 2 because we are passing 2 paths
  byte buf[MAX_FILE_PATHNAME_LENGTH*2];

  FILE *f ;

  f = fopen("winfs.log", "a");
  fprintf (f, "watchID: %d | rootPath (%d): ",watchID, rootPathLength);
  fwrite((char*)rootPath,1,rootPathLength+1,f);
  fwrite(" | ",1,3,f);
  fprintf (f, "filePath (%d): ",filePathLength);
  fwrite((char*)filePath,1,filePathLength+1,f);
  fprintf (f, " | action: %d\n",action);
  fclose(f);
  
  // Build response
  int i=0, tupleArrayLen = 4;
  ETERM *tupleArray[tupleArrayLen];
  //tupleArray[i++] = erl_mk_int(watchID);
  //tupleArray[i++] = erl_mk_binary(rootPath, rootPathLength);
  tupleArray[i++] = erl_mk_binary(filePath, filePathLength);
  //tupleArray[i++] = erl_mk_int(action);
  tupleArrayLen = i;
  ETERM *tuplep = erl_mk_tuple(tupleArray, tupleArrayLen);

  ETERM *out = erl_mk_binary(filePath, filePathLength);
  erl_encode(out, buf);
  
  f = fopen("winfs.log", "a");
  fprintf(f, "ERLANGBUFFER");
  fwrite((char*)buf,1,erl_term_len(tuplep),f);
  fprintf(f, "ERLANGBUFFER\n");
  fclose(f);

  write_cmd(buf, erl_term_len(out));
  
  // free contents from tupleArray
  erl_free_array(tupleArray, tupleArrayLen);
}

int read_exact(byte *buf, int len)
{
  int i, got=0;

  do {
    if ((i = read(0, buf+got, len-got)) <= 0)
      return(i);
    got += i;
  } while (got<len);

  return(len);
}

int write_exact(byte *buf, int len)
{
  int i, wrote = 0;

  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return (i);
    wrote += i;
  } while (wrote<len);

  return (len);
}

int read_cmd(byte *buf)
{
  int len;

  if (read_exact(buf, 2) != 2)
    return(-1);
  len = (buf[0] << 8) | buf[1];
  return read_exact(buf, len);
}

int write_cmd(byte *buf, int len)
{
  byte li;

  li = (len >> 8) & 0xff;
  write_exact(&li, 1);
  
  li = len & 0xff;
  write_exact(&li, 1);

  return write_exact(buf, len);
}

