#include <R.h>
#include <Rinternals.h>

#ifndef RCRD_HELLO_H
#define RCRD_HELLO_H

#ifdef __cplusplus
extern "C" {
#endif

  SEXP hello(SEXP name);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // RCRD_HELLO_H
