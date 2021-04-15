#include <R.h>
#include <Rinternals.h>
#include <R_ext/RS.h>
#include <string.h>


SEXP hello (SEXP name) {
	const char* c_name = CHAR(STRING_ELT(name, 0));

  /* printf("hello %s\n", c_name); */

  int l = strlen(c_name);
  const int size = l + 7;

  char greeting[size];
  greeting[0] = 0;

  strcat(greeting, "hello ");
  strcat(greeting, c_name);

  /* printf("result is %s\n", greeting); */

	SEXP r_res = PROTECT(allocVector(STRSXP, 1));
	SET_STRING_ELT(r_res, 0, mkChar(greeting));
	UNPROTECT(1);

	return r_res;
}

