#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include "val-tracer.h"
#include <instrumentr/instrumentr.h>
// #include <lazr/utilities.h>

INSTRUMENTR_DEFINE_API()

extern "C" {

  static const R_CallMethodDef callMethods[] = {
    {"val_tracer_create", (DL_FUNC) &r_val_tracer_create, 0},
    {NULL, NULL, 0}
  };

  void R_init_signatr(DllInfo* dll) {
    R_registerRoutines(dll, NULL, callMethods, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);

    INSTRUMENTR_INITIALIZE_API()
  }
}
