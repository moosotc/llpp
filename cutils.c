#include <stdio.h>
#include <errno.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <inttypes.h>
#include <sys/time.h>

#include "cutils.h"

void NORETURN_ATTR GCC_FMT_ATTR (2, 3) err (int exitcode, const char *fmt, ...)
{
    va_list ap;
    int savederrno;

    savederrno = errno;
    va_start (ap, fmt);
    vfprintf (stderr, fmt, ap);
    va_end (ap);
    fprintf (stderr, ": %s\n", strerror (savederrno));
    fflush (stderr);
    _exit (exitcode);
}

void NORETURN_ATTR GCC_FMT_ATTR (2, 3) errx (int exitcode, const char *fmt, ...)
{
    va_list ap;

    va_start (ap, fmt);
    vfprintf (stderr, fmt, ap);
    va_end (ap);
    fputc ('\n', stderr);
    fflush (stderr);
    _exit (exitcode);
}

void *parse_pointer (const char *cap, const char *s)
{
    int ret;
    void *ptr;

    ret = sscanf (s, "%" SCNxPTR, (uintptr_t *) &ptr);
    if (ret != 1) {
        errx (1, "%s: cannot parse pointer in `%s'", cap, s);
    }
    return ptr;
}

double now (void)
{
    struct timeval tv;
    gettimeofday (&tv, NULL);
    return tv.tv_sec + tv.tv_usec*1e-6;
}

void fmt_linkn (char *s, const char *c, unsigned int l, int n)
{
    int nn = n;
    do { div_t d = div (nn, l); s++; nn -= l; } while (nn > 0);
    *s = 0;
    do { div_t d = div (n, l); *--s = c[d.rem]; n -= l; } while (n > 0);
}

char *ystrdup (const char *s)
{
    size_t len = strlen (s);
    if (len > 0) {
        char *r = malloc (len+1);
        if (!r) errx (1, "malloc %zu", len+1);
        memcpy (r, s, len+1);
        return r;
    }
    return NULL;
}
