#include "pvn.h"
#include "cgic.h"

extern int cgiMain();
extern void cjsvdf_(const unsigned *const m, const unsigned *const n, void *const G, const unsigned *const ldG, void *const V, const unsigned *const ldV, const unsigned *const jpos, void *const sv, int *const gs, unsigned *const ix, void *const wrk, void *const rwrk, int *const info);
extern void sjsvdf_(const unsigned *const m, const unsigned *const n, void *const G, const unsigned *const ldG, void *const V, const unsigned *const ldV, const unsigned *const jpos, void *const sv, int *const gs, unsigned *const ix, void *const wrk, void *const rwrk, int *const info);
extern void zjsvdf_(const unsigned *const m, const unsigned *const n, void *const G, const unsigned *const ldG, void *const V, const unsigned *const ldV, const unsigned *const jpos, void *const sv, int *const gs, unsigned *const ix, void *const wrk, void *const rwrk, int *const info);
extern void djsvdf_(const unsigned *const m, const unsigned *const n, void *const G, const unsigned *const ldG, void *const V, const unsigned *const ldV, const unsigned *const jpos, void *const sv, int *const gs, unsigned *const ix, void *const wrk, void *const rwrk, int *const info);
extern void wjsvdf_(const unsigned *const m, const unsigned *const n, void *const G, const unsigned *const ldG, void *const V, const unsigned *const ldV, const unsigned *const jpos, void *const sv, int *const gs, unsigned *const ix, void *const wrk, void *const rwrk, int *const info);
extern void xjsvdf_(const unsigned *const m, const unsigned *const n, void *const G, const unsigned *const ldG, void *const V, const unsigned *const ldV, const unsigned *const jpos, void *const sv, int *const gs, unsigned *const ix, void *const wrk, void *const rwrk, int *const info);
typedef void (*fptr)(const unsigned *const, const unsigned *const, void *const, const unsigned *const, void *const, const unsigned *const, const unsigned *const, void *const, int *const, unsigned *const, void *const, void *const, int *const);

int cgiMain()
{
  void *G = NULL;
  void *V = NULL;
  void *sv = NULL;
  unsigned *ix = (unsigned*)NULL;
  void *wrk = NULL;
  void *rwrk = NULL;
  int ret = EXIT_FAILURE;
  char buf[40] = { '\0' };
  char job[11] = { '0', '1', '2', '3', '4', '5', '6', '7', '.', '?', '\0' };
  if (cgiFormSuccess != cgiFormStringNoNewlines("job", job, 9))
    goto err;
  char *fxt = job;
  while (isalnum(*fxt))
    ++fxt;
  if (*fxt)
    goto err;

  unsigned m = 0u;
  if (cgiFormSuccess != cgiFormIntegerBounded("m", (int*)&m, 1, 999, 0))
    goto err;
  if (!m || (m > 999u))
    goto err;

  unsigned n = 0u;
  if (cgiFormSuccess != cgiFormIntegerBounded("n", (int*)&n, 1, (int)m, 0))
    goto err;
  if (!n || (n > m))
    goto err;

  unsigned jpos = 0u;
  if (cgiFormSuccess != cgiFormIntegerBounded("jpos", (int*)&jpos, 0, (int)n, -1))
    goto err;
  if (jpos > n)
    goto err;

  int gs = 0;
  if (cgiFormSuccess != cgiFormIntegerBounded("cycles", &gs, 0, INT_MAX, 0))
    goto err;
  if (gs < 0)
    goto err;

  char *prec[6] = { "complex32", "real32", "complex64", "real64", "complex80", "real80" };
  unsigned s = 0u, r = 0u;
  if (cgiFormSuccess != cgiFormRadio("prec", prec, 6, (int*)&s, 2))
    goto err;
  fptr f = (fptr)NULL;
  switch (s) {
  case 0u:
    s = (unsigned)sizeof(float _Complex);
    r = (unsigned)sizeof(float);
    f = cjsvdf_;
    break;
  case 1u:
    s = (unsigned)sizeof(float);
    r = s;
    f = sjsvdf_;
    break;
  case 2u:
    s = (unsigned)sizeof(double _Complex);
    r = (unsigned)sizeof(double);
    f = zjsvdf_;
    break;
  case 3u:
    s = (unsigned)sizeof(double);
    r = s;
    f = djsvdf_;
    break;
  case 4u:
    s = (unsigned)sizeof(long double _Complex);
    r = (unsigned)sizeof(long double);
    f = wjsvdf_;
    break;
  case 5u:
    s = (unsigned)sizeof(long double);
    r = s;
    f = xjsvdf_;
    break;
  default:
    s = 0u;
  }
  if (!s)
    goto err;

  char *strat[2] = { "deRijk", "rowcyc" };
  unsigned c = 0u;
  if (cgiFormSuccess != cgiFormRadio("strat", strat, 2, (int*)&c, 0))
    goto err;
  if (c > 1u)
    goto err;

  char *colup[3] = { "fast", "slow", "ruti" };
  unsigned o = 0u;
  if (cgiFormSuccess != cgiFormRadio("colup", colup, 3, (int*)&o, 0))
    goto err;
  if (o > 2u)
    goto err;
  int info = 0;
  switch (o) {
  case 0u: *ix = c; break;
  case 1u: *ix = c; info = 1; break;
  case 2u: *ix = (c | 2u); break;
  default: goto err;
  }

  cgiFilePtr fp = (cgiFilePtr)NULL;
  if (cgiFormSuccess != cgiFormFileOpen("inp", &fp))
    goto err;
  if (!fp)
    goto err;
  const unsigned bG = m * n * s;
  if (!(G = malloc(bG)))
    goto err;
  if (cgiFormSuccess != cgiFormFileRead(fp, (char*)G, (int)bG, (int*)buf))
    goto err;
  if (bG != *(const unsigned*)buf)
    goto err;
  if (cgiFormSuccess != cgiFormFileClose(fp))
    goto err;

  const unsigned bV = n * n * s;
  if (!(V = malloc(bV)))
    goto err;
  const unsigned bsv = n * s;
  if (!(sv = malloc(bsv)))
    goto err;
  if (!(ix = malloc(n * sizeof(unsigned))))
    goto err;
  if (!(wrk = malloc(bG)))
    goto err;
  if (!(rwrk = malloc(n * r)))
    goto err;

  c = *ix;
  f(&m, &n, G, &m, V, &n, &jpos, sv, &gs, ix, wrk, rwrk, &info);
  if (info < 0)
    goto err;
  if (r == (unsigned)sizeof(float)) {
    if (gs)
      for (unsigned i = 0u; i < n; ++i)
        ((float*)sv)[i] = scalbnf(((const float*)sv)[i], -gs);
    o = (unsigned)(((const float*)rwrk)[n - 1u]);
  }
  else if (r == (unsigned)sizeof(double)) {
    if (gs)
      for (unsigned i = 0u; i < n; ++i)
        ((double*)sv)[i] = scalbn(((const double*)sv)[i], -gs);
    o = (unsigned)(((const double*)rwrk)[n - 1u]);
  }
  else {
    if (gs)
      for (unsigned i = 0u; i < n; ++i)
        ((long double*)sv)[i] = scalbnl(((const long double*)sv)[i], -gs);
    o = (unsigned)(((const long double*)rwrk)[n - 1u]);
  }
  (void)sprintf(buf, "%2u,%11u,%11d,%11u\n", c, gs, info, o);

  const int fd = fileno(cgiOut);
  if (fd < 0)
    goto err;
  cgiHeaderContentType("application/x-tar");
  *fxt = '.';
  ++fxt;

  *fxt = 'U';
  if (pvn_tar_add_file_(&fd, job, &bG, G) < 0)
    goto end;
  *fxt = 'V';
  if (pvn_tar_add_file_(&fd, job, &bV, V) < 0)
    goto end;
  *fxt = 'S';
  if (pvn_tar_add_file_(&fd, job, &bsv, sv) < 0)
    goto end;
  *fxt = 'T';
  c = 40u;
  if (pvn_tar_add_file_(&fd, job, &c, buf) < 0)
    goto end;
  if (pvn_tar_terminate_(&fd) < 0)
    goto end;
  ret = EXIT_SUCCESS;
  goto end;

 err:
  cgiHeaderStatus(400, "Bad Request");
 end:
  free(rwrk);
  free(wrk);
  free(ix);
  free(sv);
  free(V);
  free(G);
  return ret;
}
