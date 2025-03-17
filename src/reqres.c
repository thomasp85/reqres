#include <stdio.h>
#include <time.h>

#define STRICT_R_HEADERS
#include <Rinternals.h>
#include <R_ext/Rdynload.h> // Included by default in R (>= 3.4).

SEXP fmt_http_time(SEXP input_time)
{
  time_t t;
  const char fmt[25] = "%a, %d %b %Y %H:%M:%S";
  char buffer[64];
  if (Rf_length(input_time) != 0) {
    t = INTEGER(input_time)[0];
  } else {
    time(&t);
  }

  struct tm* tm_info;
  tm_info = gmtime(&t);

  int written = strftime(buffer, 64, fmt, tm_info);
  if (!written) {
    Rf_error("Failed to format time.");
    return R_NilValue;
  }
  buffer[written++] = ' ';
  buffer[written++] = 'G';
  buffer[written++] = 'M';
  buffer[written++] = 'T';

  return Rf_ScalarString(Rf_mkCharLen(buffer, written));
}

static const R_CallMethodDef CallEntries[] = {
  {"fmt_http_time_c", (DL_FUNC) &fmt_http_time, 1},
  {NULL, NULL, 0}
};

void R_init_reqres(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, TRUE);
}
