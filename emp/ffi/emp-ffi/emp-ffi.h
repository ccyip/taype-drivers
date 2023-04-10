#ifndef TAYPE_DRIVER_EMP_FFI_H__
#define TAYPE_DRIVER_EMP_FFI_H__

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

#define DRIVER_INT_SIZE (32)

void setup_driver(const char *addr, int port, int party, bool quiet);

void finalize_driver(void);

typedef void *obliv_int;

obliv_int obliv_int_new(int n, int party);

void obliv_int_destroy(obliv_int n);

int obliv_int_reveal(obliv_int m);

bool obliv_bool_reveal(obliv_int b);

obliv_int obliv_int_add(obliv_int m, obliv_int n);

obliv_int obliv_int_sub(obliv_int m, obliv_int n);

obliv_int obliv_int_mul(obliv_int m, obliv_int n);

obliv_int obliv_int_div(obliv_int m, obliv_int n);

obliv_int obliv_int_eq(obliv_int m, obliv_int n);

obliv_int obliv_int_le(obliv_int m, obliv_int n);

obliv_int obliv_bool_not(obliv_int m);

obliv_int obliv_bool_and(obliv_int m, obliv_int n);

obliv_int obliv_bool_or(obliv_int m, obliv_int n);

obliv_int obliv_int_mux(obliv_int s, obliv_int m, obliv_int n);

#ifdef __cplusplus
}
#endif

#endif
