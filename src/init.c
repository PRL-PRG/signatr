#define R_NO_REMAP

#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>

SEXP hello (SEXP name);

static const R_CallMethodDef callMethods[] = {
	{"hello", 		(DL_FUNC) &hello, 1},
	{NULL, NULL, 0}
};

void R_init_signatr(DllInfo* dll) {
	R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
}
