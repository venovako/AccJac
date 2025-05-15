#if (defined(__INTEL_CLANG_COMPILER) || defined(__INTEL_LLVM_COMPILER))
#include <mathimf.h>
#else /* !Intel */
#ifdef __cplusplus
#include <complex>
#include <cmath>
#else /* !__cplusplus */
#include <complex.h>
#include <math.h>
#endif /* ?__cplusplus */
#endif /* ?Intel */

#ifndef NDEBUG
#ifdef __cplusplus
#include <cassert>
#else /* !__cplusplus */
#include <assert.h>
#endif /* ?__cplusplus */
#endif /* !NDEBUG */
#include "gmp.h"
#include "mpfr.h"
#include "mpc.h"

static mpc_t A, B, C, D, E;

extern void mpc_start_(int *const p)
{
#ifndef NDEBUG
  assert(p);
#endif /* !NDEBUG */
  mpc_init2(A, *p);
  mpc_init2(B, *p);
  mpc_init2(C, *p);
  mpc_init2(D, *p);
  mpc_init2(E, *p);
}

extern void mpc_stop_()
{
  mpc_clear(E);
  mpc_clear(D);
  mpc_clear(C);
  mpc_clear(B);
  mpc_clear(A);
}

extern void mpc_wfma_(LONG_DOUBLE_COMPLEX *const e, const LONG_DOUBLE_COMPLEX *const a, const LONG_DOUBLE_COMPLEX *const b, const LONG_DOUBLE_COMPLEX *const c, const long double *const d)
{
#ifndef NDEBUG
  assert(e);
  assert(a);
  assert(b);
  assert(c);
  assert(d);
#endif /* !NDEBUG */
  (void)mpc_set_ldc(A, *a, MPC_RNDNN);
  (void)mpc_set_ldc(B, *b, MPC_RNDNN);
  (void)mpc_set_ldc(C, *c, MPC_RNDNN);
  (void)mpc_fma(E, A, B, C, MPC_RNDNN);
  (void)mpc_set_ld(D, *d, MPC_RNDNN);
  (void)mpc_mul(E, E, D, MPC_RNDNN);
  *e = mpc_get_ldc(E, MPC_RNDNN);
}
