#define R_NO_REMAP

#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>

SEXP hello_c (SEXP name);

static const R_CallMethodDef callMethods[] = {
	{"hello_c", 		(DL_FUNC) &hello_c, 1},
	{NULL, NULL, 0}
};

void R_init_signatr(DllInfo* dll) {
	R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
}
