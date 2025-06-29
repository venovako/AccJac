#include "pvn.h"
#include "cgic.h"

extern int cgiMain(const int u, const int v);
extern void cjsvdf_(const unsigned *const m, const unsigned *const n, void *const G, const unsigned *const ldG, void *const V, const unsigned *const ldV, const unsigned *const jpos, void *const sv, int *const gs, unsigned *const ix, void *const wrk, void *const rwrk, int *const info);
extern void sjsvdf_(const unsigned *const m, const unsigned *const n, void *const G, const unsigned *const ldG, void *const V, const unsigned *const ldV, const unsigned *const jpos, void *const sv, int *const gs, unsigned *const ix, void *const wrk, void *const rwrk, int *const info);
extern void zjsvdf_(const unsigned *const m, const unsigned *const n, void *const G, const unsigned *const ldG, void *const V, const unsigned *const ldV, const unsigned *const jpos, void *const sv, int *const gs, unsigned *const ix, void *const wrk, void *const rwrk, int *const info);
extern void djsvdf_(const unsigned *const m, const unsigned *const n, void *const G, const unsigned *const ldG, void *const V, const unsigned *const ldV, const unsigned *const jpos, void *const sv, int *const gs, unsigned *const ix, void *const wrk, void *const rwrk, int *const info);
extern void wjsvdf_(const unsigned *const m, const unsigned *const n, void *const G, const unsigned *const ldG, void *const V, const unsigned *const ldV, const unsigned *const jpos, void *const sv, int *const gs, unsigned *const ix, void *const wrk, void *const rwrk, int *const info);
extern void xjsvdf_(const unsigned *const m, const unsigned *const n, void *const G, const unsigned *const ldG, void *const V, const unsigned *const ldV, const unsigned *const jpos, void *const sv, int *const gs, unsigned *const ix, void *const wrk, void *const rwrk, int *const info);
typedef void (*fptr)(const unsigned *const, const unsigned *const, void *const, const unsigned *const, void *const, const unsigned *const, const unsigned *const, void *const, int *const, unsigned *const, void *const, void *const, int *const);

int cgiMain(const int u, const int v)
{
  void *G = NULL;
  void *V = NULL;
  void *sv = NULL;
  unsigned *ix = (unsigned*)NULL;
  void *wrk = NULL;
  void *rwrk = NULL;
  char *buf = (char*)NULL;
  int ret = EXIT_FAILURE;

  char job[13] = { '0', '1', '2', '3', '4', '5', '6', '7', '.', 't', 'a', 'r', '\0' };
  if (cgiFormSuccess != cgiFormStringNoNewlines("job", job, 9))
    goto err;
  char *fxt = job;
  while (isalnum(*fxt))
    ++fxt;
  if (*fxt)
    goto err;

  unsigned m = 0u;
  if (cgiFormSuccess != cgiFormIntegerBounded("m", (int*)&m, 1, 512, 0))
    goto err;
  if (!m || (m > 512u))
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
  c >>= 1u;

  char *colup[3] = { "fast", "slow", "ruti" };
  unsigned o = 0u;
  if (cgiFormSuccess != cgiFormRadio("colup", colup, 3, (int*)&o, 0))
    goto err;
  if (o > 2u)
    goto err;

  cgiFilePtr fp = (cgiFilePtr)NULL;
  if (cgiFormSuccess != cgiFormFileOpen("inp", &fp))
    goto err;
  if (!fp)
    goto err;
  const unsigned bG = m * n * s;
  if (!(G = malloc(bG)))
    goto err;
  if (r == (unsigned)sizeof(long double)) {
    (void)memset(G, 0, bG);
    goto err;
  }
  int t = 0;
  if (cgiFormSuccess != cgiFormFileRead(fp, (char*)G, (int)bG, &t))
    goto err;
  if (bG != (unsigned)t)
    goto err;
  if (cgiFormSuccess != cgiFormFileClose(fp))
    goto err;

  const unsigned bV = n * n * s;
  if (!(V = malloc(bV)))
    goto err;
  const unsigned bsv = n * r;
  if (!(sv = malloc(bsv)))
    goto err;
  if (!(ix = malloc(n * sizeof(unsigned))))
    goto err;
  if (!(wrk = malloc(bG)))
    goto err;
  if (!(rwrk = malloc(bsv)))
    goto err;

  if (r == (unsigned)sizeof(long double)) {
    (void)memset(V, 0, bV);
    (void)memset(sv, 0, bsv);
  }

  int info = 0;
  switch (o) {
  case 0u: info = (int)c; break;
  case 1u: info = (int)(c | 1u);  break;
  case 2u: info = (int)(c | 4u); break;
  default: goto err;
  }
  *ix = u;
  c = (unsigned)info;
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

  const int fd = fileno(cgiOut);
  if (fd < 0)
    goto err;
  *fxt = '.';
  ++fxt;
  *fxt = 't';
  ++fxt;
  *fxt = 'g';
  ++fxt;
  *fxt = 'z';
  ++fxt;
  *fxt = '\0';
  if (dprintf(fd, "Content-Type: application/x-tar\nContent-Transfer-Encoding: binary\nContent-Disposition: attachment; filename=\"%s\"\n\n", job) < 0)
    goto err;
  (void)fsync(fd);
  --fxt;
  *fxt = '\0';
  --fxt;
  *fxt = '\0';
  --fxt;
  FILE *const gzf = popen("/usr/bin/gzip -9cq -", "w");
  if (!gzf)
    goto end;
  const int gz = fileno(gzf);
  if (gz < 0)
    goto end;
  *fxt = 'U';
  if (pvn_tar_add_file_(&gz, job, &bG, G) < 0)
    goto end;
  *fxt = 'V';
  if (pvn_tar_add_file_(&gz, job, &bV, V) < 0)
    goto end;
  *fxt = 'S';
  if (pvn_tar_add_file_(&gz, job, &bsv, sv) < 0)
    goto end;
  *fxt = 't';
  ++fxt;
  *fxt = 'x';
  ++fxt;
  *fxt = 't';
  (void)fsync(v);
  t = (int)lseek(v, 0, SEEK_CUR);
  if (t < 0)
    goto end;
  if (lseek(v, 0, SEEK_SET))
    goto end;
  buf = (char*)malloc((size_t)(t + 39));
  if (!buf)
    goto end;
  if (t != (int)read(v, buf, (size_t)t))
    goto end;
  if (sprintf((buf + t), "%2u,%11u,%11d,%11u", c, gs, info, o) < 0)
    goto end;
  buf[t += 38] = '\n';
  c = (unsigned)++t;
  if (pvn_tar_add_file_(&gz, job, &c, buf) < 0)
    goto end;
  if (pvn_tar_terminate_(&gz))
    goto end;
  (void)fsync(gz);
  if (pclose(gzf) < 0)
    goto end;
  ret = EXIT_SUCCESS;
  goto end;

 err:
  cgiHeaderStatus(400, "Bad Request");
 end:
  free(buf);
  free(rwrk);
  free(wrk);
  free(ix);
  free(sv);
  free(V);
  free(G);
  return ret;
}
