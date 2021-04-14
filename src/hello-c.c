#include <R.h>
#include <Rinternals.h>
#include <R_ext/RS.h>
#include <string.h>


SEXP hello_c (SEXP name) {
	char greeting[] = "Hello ";

	const char* c_name = CHAR(STRING_ELT(name, 0));

  char* res = strcat(greeting, c_name);

	SEXP r_res = PROTECT(allocVector(STRSXP, 1));
	SET_STRING_ELT(r_res, 0, mkChar(res));
	UNPROTECT(1);

	return r_res;
}

