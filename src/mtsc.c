#ifndef MPFR_WANT_FLOAT128
#define MPFR_WANT_FLOAT128
#endif /* !MPFR_WANT_FLOAT128 */
#if (defined(__INTEL_CLANG_COMPILER) || defined(__INTEL_LLVM_COMPILER))
#ifndef _Float128
#define _Float128 __float128
#endif /* !_Float128 */
#endif /* Intel compiler */
#include "gmp.h"
#include "mpfr.h"

static mpfr_t O, D, T, C, S;

extern void mpfr_start_(int *const p)
{
#ifndef NDEBUG
  assert(p);
#endif /* !NDEBUG */
  if (*p < MPFR_PREC_MIN) {
    *p = -1;
    return;
  }
  if (*p > MPFR_PREC_MAX) {
    *p = -1;
    return;
  }
  if (*p != mpfr_get_default_prec())
    mpfr_set_default_prec(*p);
  const mpfr_exp_t emin = mpfr_get_emin_min();
  if ((emin < mpfr_get_emin()) && mpfr_set_emin(emin)) {
    *p = 1;
    return;
  }
  const mpfr_exp_t emax = mpfr_get_emax_max();
  if ((emax > mpfr_get_emax()) && mpfr_set_emax(emax)) {
    *p = 2;
    return;
  }
  if (mpfr_init_set_d(O, 1.0, MPFR_RNDN)) {
    *p = 3;
    return;
  }
  if (mpfr_init_set_d(D, 0.0, MPFR_RNDN)) {
    *p = 4;
    return;
  }
  if (mpfr_init_set_d(T, 0.0, MPFR_RNDN)) {
    *p = 5;
    return;
  }
  if (mpfr_init_set_d(C, 0.0, MPFR_RNDN)) {
    *p = 6;
    return;
  }
  if (mpfr_init_set_d(S, 0.0, MPFR_RNDN)) {
    *p = 7;
    return;
  }
  *p = 0;
}

extern void mpfr_stop_()
{
  mpfr_clear(S);
  mpfr_clear(C);
  mpfr_clear(T);
  mpfr_clear(D);
  mpfr_clear(O);
  mpfr_free_cache();
}

extern void mpfr_tcs_(const __float128 *const d, __float128 *const t, __float128 *const c, __float128 *const s)
{
#ifndef NDEBUG
  assert(d);
  assert(t);
  assert(c);
  assert(s);
#endif /* !NDEBUG */
  (void)mpfr_set_float128(D, *d, MPFR_RNDN);

  (void)mpfr_neg(C, D, MPFR_RNDN);
  (void)mpfr_fma(S, C, D, O, MPFR_RNDN);
  (void)mpfr_sqrt(S, S, MPFR_RNDN);
  (void)mpfr_add(S, S, O, MPFR_RNDN);
  (void)mpfr_div(T, D, S, MPFR_RNDN);

  (void)mpfr_neg(C, T, MPFR_RNDN);
  (void)mpfr_fma(S, C, T, O, MPFR_RNDN);
  (void)mpfr_rec_sqrt(C, S, MPFR_RNDN);

  (void)mpfr_mul(S, C, T, MPFR_RNDN);

  *t = mpfr_get_float128(T, MPFR_RNDN);
  *c = mpfr_get_float128(C, MPFR_RNDN);
  *s = mpfr_get_float128(S, MPFR_RNDN);
}

extern void mpfr_re_(const __float128 *const exac, __float128 *const comp, const __float128 *const eps)
{
#ifndef NDEBUG
  assert(exac);
  assert(comp);
  assert(eps);
#endif /* !NDEBUG */
  (void)mpfr_set_float128(D, *exac, MPFR_RNDN);
  (void)mpfr_set_float128(T, *comp, MPFR_RNDN);
  (void)mpfr_set_float128(C, *eps, MPFR_RNDN);

  (void)mpfr_sub(T, D, T, MPFR_RNDN);
  (void)mpfr_abs(T, T, MPFR_RNDN);
  (void)mpfr_mul(S, D, C, MPFR_RNDN);
  (void)mpfr_div(D, T, S, MPFR_RNDN);

  *comp = mpfr_get_float128(D, MPFR_RNDN);
}
