#ifndef MPFR_WANT_FLOAT128
#define MPFR_WANT_FLOAT128
#endif /* !MPFR_WANT_FLOAT128 */
#if (defined(__INTEL_CLANG_COMPILER) || defined(__INTEL_LLVM_COMPILER))
#ifndef _Float128
#define _Float128 __float128
#endif /* !_Float128 */
#endif /* Intel compiler */
#include <assert.h>
#include <stdarg.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <gmp.h>
#include <mpfr.h>

static mpfr_t c, sr, si, mo;

int init_mpfr(const mpfr_prec_t *const p)
{
#ifndef NDEBUG
  assert(p);
#endif /* !NDEBUG */
  if (*p < MPFR_PREC_MIN)
    return -1;
  if (*p > MPFR_PREC_MAX)
    return -1;
  if (*p != mpfr_get_default_prec())
    mpfr_set_default_prec(*p);
  const mpfr_exp_t emin = mpfr_get_emin_min();
  if ((emin < mpfr_get_emin()) && mpfr_set_emin(emin))
    return 1;
  const mpfr_exp_t emax = mpfr_get_emax_max();
  if ((emax > mpfr_get_emax()) && mpfr_set_emax(emax))
    return 2;
  if (mpfr_init_set_d(c, 1.0, MPFR_RNDN))
    return 3;
  if (mpfr_init_set_d(sr, 0.0, MPFR_RNDN))
    return 4;
  if (mpfr_init_set_d(si, 0.0, MPFR_RNDN))
    return 5;
  if (mpfr_init_set_d(mo, -1.0, MPFR_RNDN))
    return 6;
  return 0;
}

void init_mpfr_(int *const info)
{
#ifndef NDEBUG
  assert(info);
#endif /* !NDEBUG */
  const mpfr_prec_t p = 256;
  *info = init_mpfr(&p);
}

__float128 ndetm1_(const __float128 *const cs1, const __float128 *const sn1, const __float128 *const e)
{
#ifndef NDEBUG
  assert(cs1);
  assert(sn1);
  assert(e);
#endif /* !NDEBUG */
  (void)mpfr_set_float128(sr, *sn1, MPFR_RNDN);
  (void)mpfr_fma(sr, sr, sr, mo, MPFR_RNDN);
  (void)mpfr_set_float128(c, *cs1, MPFR_RNDN);
  (void)mpfr_fma(c, c, c, sr, MPFR_RNDN);
  (void)mpfr_set_float128(si, *e, MPFR_RNDN);
  (void)mpfr_div(sr, c, si, MPFR_RNDN);
  return mpfr_get_float128(sr, MPFR_RNDN);
}

__float128 mdetm1_(const __float128 *const cs1, const __float128 *const sn1, const __float128 *const e)
{
#ifndef NDEBUG
  assert(cs1);
  assert(sn1);
  assert(e);
#endif /* !NDEBUG */
  (void)mpfr_set_float128(si, sn1[1], MPFR_RNDN);
  (void)mpfr_fma(si, si, si, mo, MPFR_RNDN);
  (void)mpfr_set_float128(sr, sn1[0], MPFR_RNDN);
  (void)mpfr_fma(sr, sr, sr, si, MPFR_RNDN);
  (void)mpfr_set_float128(c, *cs1, MPFR_RNDN);
  (void)mpfr_fma(c, c, c, sr, MPFR_RNDN);
  (void)mpfr_set_float128(si, *e, MPFR_RNDN);
  (void)mpfr_div(sr, c, si, MPFR_RNDN);
  return mpfr_get_float128(sr, MPFR_RNDN);
}

void fini_mpfr_()
{
  mpfr_clear(mo);
  mpfr_clear(si);
  mpfr_clear(sr);
  mpfr_clear(c);
  mpfr_free_cache();
}

#ifdef MTEST_MAIN
int main(int argc, char *argv[])
{
  if (argc != 2) {
    (void)fprintf(stderr, "%s p\n", *argv);
    return EXIT_FAILURE;
  }
  (void)printf("sizeof(mpfr_t)=%zu\n", sizeof(mpfr_t));
  const mpfr_prec_t p = (mpfr_prec_t)atoi(argv[1]);
  (void)printf("MPFR_PREC_MIN=%zu\n", (size_t)MPFR_PREC_MIN);
  (void)printf("MPFR_PREC_MAX=%zu\n", (size_t)MPFR_PREC_MAX);
  (void)printf("mpfr_get_emin=%zd\n", (ssize_t)mpfr_get_emin());
  (void)printf("mpfr_get_emax=%zd\n", (ssize_t)mpfr_get_emax());
  (void)printf("init_mpfr=%d\n", init_mpfr(&p));
  (void)printf("mpfr_get_emin=%zd\n", (ssize_t)mpfr_get_emin());
  (void)printf("mpfr_get_emax=%zd\n", (ssize_t)mpfr_get_emax());
  fini_mpfr_();
  return EXIT_SUCCESS;
}
#endif /* MTEST_MAIN */
