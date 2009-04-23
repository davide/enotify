#include "ENotify.h"

int foo(int a) { return a; }
int bar(int a) { return a; }

int main()
{
	eNotify_init();
	
	ETERM *tuplep, *intp;
	ETERM *fnp, *argp;
	int res;
	byte buf[100];
	long allocated, freed;

	erl_init(NULL, 0);
	
	while (read_cmd(buf) > 0) {
		tuplep = erl_decode(buf);
		fnp = erl_element(1, tuplep);
		argp = erl_element(2, tuplep);
		const char* func_name =  (const char*)ERL_ATOM_PTR(fnp);
		int int_arg = ERL_INT_VALUE(argp);
		if (strncmp(func_name, "foo", 3) == 0) {
			res = foo(int_arg);
		} else if (strncmp(func_name, "bar", 17) == 0) {
			res = bar(int_arg);
		}

		intp = erl_mk_int(res);
		erl_encode(intp, buf);
		write_cmd(buf, erl_term_len(intp));

		erl_free_compound(tuplep);
		erl_free_term(fnp);
		erl_free_term(argp);
		erl_free_term(intp);
	}
}

void eNotifyCallback(int watchID, int action, const char* rootPath, const char* filePath)
{
	
}
