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

static mpc_t A, B, C, D;

extern void mpc_start_(int *const p)
{
#ifndef NDEBUG
  assert(p);
#endif /* !NDEBUG */
  mpc_init2(A, *p);
  mpc_init2(B, *p);
  mpc_init2(C, *p);
  mpc_init2(D, *p);
}

extern void mpc_stop_()
{
  mpc_clear(D);
  mpc_clear(C);
  mpc_clear(B);
  mpc_clear(A);
}

extern void mpc_wfma_(LONG_DOUBLE_COMPLEX *const d, const LONG_DOUBLE_COMPLEX *const a, const LONG_DOUBLE_COMPLEX *const b, const LONG_DOUBLE_COMPLEX *const c)
{
#ifndef NDEBUG
  assert(d);
  assert(a);
  assert(b);
  assert(c);
#endif /* !NDEBUG */
  (void)mpc_set_ldc(A, *a, MPC_RNDNN);
  (void)mpc_set_ldc(B, *b, MPC_RNDNN);
  (void)mpc_set_ldc(C, *c, MPC_RNDNN);
  (void)mpc_fma(D, A, B, C, MPC_RNDNN);
  *d = mpc_get_ldc(D, MPC_RNDNN);
}
