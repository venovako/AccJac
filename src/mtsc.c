#ifndef NDEBUG
#include <assert.h>
#endif /* !NDEBUG */
#include "gmp.h"
#include "mpfr.h"

extern float cr_rsqrtf(float x);
//extern double cr_rsqrt(double x);
//extern long double cr_rsqrtl(long double x);

static mpfr_t O, D, T, C, S;

extern void tcs_new_(const float *const d, float *const t, float *const c, float *const s)
{
#ifndef NDEBUG
  assert(d);
  assert(t);
  assert(c);
  assert(s);
#endif /* !NDEBUG */
  *t = *d / (1.0f + __builtin_sqrtf(__builtin_fmaf(-*d, *d, 1.0f)));
  *c = cr_rsqrtf(__builtin_fmaf(-*t, *t, 1.0f));
  *s = (*c) * (*t);
}

extern void tcs_old_(const float *const d, float *const t, float *const c, float *const s)
{
#ifndef NDEBUG
  assert(d);
  assert(t);
  assert(c);
  assert(s);
#endif /* !NDEBUG */
  *t = *d / (1.0f + __builtin_sqrtf((1.0f - *d) * (1.0f + *d)));
  *c = 1.0f / __builtin_sqrtf((1.0f - *t) * (1.0f + *t));
  *s = (*c) * (*t);
}

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

extern void mpfr_tcs_(const double *const d, double *const t, double *const c, double *const s)
{
#ifndef NDEBUG
  assert(d);
  assert(t);
  assert(c);
  assert(s);
#endif /* !NDEBUG */
  (void)mpfr_set_d(D, *d, MPFR_RNDN);

  (void)mpfr_neg(C, D, MPFR_RNDN);
  (void)mpfr_fma(S, C, D, O, MPFR_RNDN);
  (void)mpfr_sqrt(S, S, MPFR_RNDN);
  (void)mpfr_add(S, S, O, MPFR_RNDN);
  (void)mpfr_div(T, D, S, MPFR_RNDN);

  (void)mpfr_neg(C, T, MPFR_RNDN);
  (void)mpfr_fma(S, C, T, O, MPFR_RNDN);
  (void)mpfr_rec_sqrt(C, S, MPFR_RNDN);

  (void)mpfr_mul(S, C, T, MPFR_RNDN);

  *t = mpfr_get_d(T, MPFR_RNDN);
  *c = mpfr_get_d(C, MPFR_RNDN);
  *s = mpfr_get_d(S, MPFR_RNDN);
}

extern void mpfr_re_(const double *const exac, double *const comp, const double *const eps)
{
#ifndef NDEBUG
  assert(exac);
  assert(comp);
  assert(eps);
#endif /* !NDEBUG */
  (void)mpfr_set_d(D, *exac, MPFR_RNDN);
  (void)mpfr_set_d(T, *comp, MPFR_RNDN);
  (void)mpfr_set_d(C, *eps, MPFR_RNDN);

  (void)mpfr_sub(T, D, T, MPFR_RNDN);
  (void)mpfr_abs(T, T, MPFR_RNDN);
  (void)mpfr_mul(S, D, C, MPFR_RNDN);
  (void)mpfr_div(D, T, S, MPFR_RNDN);

  *comp = mpfr_get_d(D, MPFR_RNDN);
}
