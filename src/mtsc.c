#ifndef NDEBUG
#ifdef __cplusplus
#include <cassert>
#else /* !__cplusplus */
#include <assert.h>
#endif /* ?__cplusplus */
#endif /* !NDEBUG */
#include "gmp.h"
#include "mpfr.h"

extern float cr_rsqrtf(float x);

static mpfr_t O, D, T, C, S, E, F;

extern void mpfr_start_(int *const p, const float *const eps)
{
#ifndef NDEBUG
  assert(p);
  assert(eps);
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
  if (mpfr_init_set_d(E, (double)*eps, MPFR_RNDN)) {
    *p = 8;
    return;
  }
  if (mpfr_init_set_d(F, 0.0, MPFR_RNDN)) {
    *p = 9;
    return;
  }
  *p = 0;
}

extern void mpfr_stop_()
{
  mpfr_clear(F);
  mpfr_clear(E);
  mpfr_clear(S);
  mpfr_clear(C);
  mpfr_clear(T);
  mpfr_clear(D);
  mpfr_clear(O);
  mpfr_free_cache();
}

extern void tcs_new_(const float *const d, float *const t, float *const c, float *const s, double *const et, double *const ec, double *const es)
{
#ifndef NDEBUG
  assert(d);
  assert(t);
  assert(c);
  assert(s);
  assert(et);
  assert(ec);
  assert(es);
#endif /* !NDEBUG */
  *t = *d / (1.0f + __builtin_sqrtf(__builtin_fmaf(-*d, *d, 1.0f)));
  *c = cr_rsqrtf(__builtin_fmaf(-*t, *t, 1.0f));
  *s = (*c) * (*t);

  (void)mpfr_set_flt(D, *d, MPFR_RNDN);

  /* T */
  (void)mpfr_neg(C, D, MPFR_RNDN);
  (void)mpfr_fma(S, C, D, O, MPFR_RNDN);
  (void)mpfr_sqrt(S, S, MPFR_RNDN);
  (void)mpfr_add(S, S, O, MPFR_RNDN);
  (void)mpfr_div(T, D, S, MPFR_RNDN);

  /* C */
  (void)mpfr_neg(C, T, MPFR_RNDN);
  (void)mpfr_fma(S, C, T, O, MPFR_RNDN);
  (void)mpfr_rec_sqrt(C, S, MPFR_RNDN);

  /* S */
  (void)mpfr_mul(S, C, T, MPFR_RNDN);

  /* relerr T */
  (void)mpfr_set_flt(D, *t, MPFR_RNDN);
  (void)mpfr_sub(D, T, D, MPFR_RNDN);
  (void)mpfr_abs(D, D, MPFR_RNDN);
  (void)mpfr_mul(F, E, T, MPFR_RNDN);
  (void)mpfr_div(D, D, F, MPFR_RNDN);
  *et = mpfr_get_d(D, MPFR_RNDN);

  /* relerr C */
  (void)mpfr_set_flt(D, *c, MPFR_RNDN);
  (void)mpfr_sub(D, C, D, MPFR_RNDN);
  (void)mpfr_abs(D, D, MPFR_RNDN);
  (void)mpfr_mul(F, E, C, MPFR_RNDN);
  (void)mpfr_div(D, D, F, MPFR_RNDN);
  *ec = mpfr_get_d(D, MPFR_RNDN);

  /* relerr S */
  (void)mpfr_set_flt(D, *s, MPFR_RNDN);
  (void)mpfr_sub(D, S, D, MPFR_RNDN);
  (void)mpfr_abs(D, D, MPFR_RNDN);
  (void)mpfr_mul(F, E, S, MPFR_RNDN);
  (void)mpfr_div(D, D, F, MPFR_RNDN);
  *es = mpfr_get_d(D, MPFR_RNDN);
}

extern void tcs_old_(const float *const d, float *const t, float *const c, float *const s, double *const et, double *const ec, double *const es)
{
#ifndef NDEBUG
  assert(d);
  assert(t);
  assert(c);
  assert(s);
  assert(et);
  assert(ec);
  assert(es);
#endif /* !NDEBUG */
  *t = *d / (1.0f + __builtin_sqrtf((1.0f - *d) * (1.0f + *d)));
  *c = 1.0f / __builtin_sqrtf((1.0f - *t) * (1.0f + *t));
  *s = (*c) * (*t);

  /* relerr T */
  (void)mpfr_set_flt(D, *t, MPFR_RNDN);
  (void)mpfr_sub(D, T, D, MPFR_RNDN);
  (void)mpfr_abs(D, D, MPFR_RNDN);
  (void)mpfr_mul(F, E, T, MPFR_RNDN);
  (void)mpfr_div(D, D, F, MPFR_RNDN);
  *et = mpfr_get_d(D, MPFR_RNDN);

  /* relerr C */
  (void)mpfr_set_flt(D, *c, MPFR_RNDN);
  (void)mpfr_sub(D, C, D, MPFR_RNDN);
  (void)mpfr_abs(D, D, MPFR_RNDN);
  (void)mpfr_mul(F, E, C, MPFR_RNDN);
  (void)mpfr_div(D, D, F, MPFR_RNDN);
  *ec = mpfr_get_d(D, MPFR_RNDN);

  /* relerr S */
  (void)mpfr_set_flt(D, *s, MPFR_RNDN);
  (void)mpfr_sub(D, S, D, MPFR_RNDN);
  (void)mpfr_abs(D, D, MPFR_RNDN);
  (void)mpfr_mul(F, E, S, MPFR_RNDN);
  (void)mpfr_div(D, D, F, MPFR_RNDN);
  *es = mpfr_get_d(D, MPFR_RNDN);
}
