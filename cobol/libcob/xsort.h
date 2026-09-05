/* xsort.h -- a budgeted external sort of fixed-size entries.
 *
 * An entry is a fixed-width KEY followed by a fixed-width RECORD; the
 * key's unsigned byte order is the sort order (the caller builds keys
 * that way -- libcob's normalized SORT key, or a DFSORT-style control
 * card's fields later).  Entries arrive one at a time; the engine keeps
 * as many as its memory budget holds, sorts that run, writes it beside
 * the caller's work file, and repeats.  A sort that fits in the budget
 * never touches a file.  At the end the runs are merged k ways -- in
 * more than one pass if there are more runs than the fan-in -- and the
 * caller reads the merged order one entry at a time.
 *
 * Single translation unit: include from the runtime.  No dependency on
 * libcob; the fatal callback is the only way out. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define XS_MIN_CAP 16u

typedef struct {
    FILE *fp;
    unsigned char *b;             /* read-ahead buffer: bcap entries, bn filled, bi next */
    unsigned bcap, bn, bi;
} xs_src;

typedef struct {
    unsigned esize, klen;         /* entry = klen key bytes + record */
    size_t budget;                /* bytes the sort may use for entries */
    char base[240];               /* run files: base.rNNNN */
    void (*fatal)(const char *);
    unsigned char *buf; unsigned cap, n;    /* the run being collected */
    unsigned *order, *tmp;
    char **runs; unsigned nruns, runcap, runseq;
    unsigned fan;                 /* merge fan-in */
    xs_src *src; unsigned nsrc;   /* the merge in progress */
    unsigned *heap; unsigned hn;
    int finished, inmem;
    unsigned pos;                 /* in-memory cursor */
} xsort;

static void xs_die(xsort *xs, const char *m) { xs->fatal(m); exit(3); }

static void xs_merge_sort(xsort *xs, unsigned *v, unsigned *tmp, unsigned lo, unsigned hi)
{
    if (hi - lo < 2) return;
    unsigned mid = lo + (hi - lo) / 2;
    xs_merge_sort(xs, v, tmp, lo, mid); xs_merge_sort(xs, v, tmp, mid, hi);
    unsigned i = lo, j = mid, k = lo;
    const unsigned char *b = xs->buf; unsigned es = xs->esize, kl = xs->klen;
    while (i < mid && j < hi)
        tmp[k++] = memcmp(b + (size_t)v[i] * es, b + (size_t)v[j] * es, kl) <= 0 ? v[i++] : v[j++];
    while (i < mid) tmp[k++] = v[i++];
    while (j < hi) tmp[k++] = v[j++];
    memcpy(v + lo, tmp + lo, (size_t)(hi - lo) * sizeof *v);
}

/* the run buffer: as many entries as the budget holds, fewer if the
 * heap will not give that much (no sbrk here: what is free is free) */
static void xs_alloc_run(xsort *xs)
{
    size_t per = (size_t)xs->esize + 2 * sizeof(unsigned);
    size_t cap = xs->budget / per;
    if (cap < XS_MIN_CAP) cap = XS_MIN_CAP;
    for (;;) {
        xs->buf = malloc(cap * xs->esize);
        xs->order = malloc((cap + 1) * sizeof *xs->order);
        xs->tmp = malloc((cap + 1) * sizeof *xs->tmp);
        if (xs->buf && xs->order && xs->tmp) break;
        free(xs->buf); free(xs->order); free(xs->tmp);
        xs->buf = 0; xs->order = xs->tmp = 0;
        if (cap <= XS_MIN_CAP) xs_die(xs, "SORT: out of memory");
        cap /= 2;
        if (cap < XS_MIN_CAP) cap = XS_MIN_CAP;
    }
    xs->cap = (unsigned)cap;
}

static void xs_init(xsort *xs, unsigned esize, unsigned klen, size_t budget, unsigned fan, const char *base, void (*fatal)(const char *))
{
    memset(xs, 0, sizeof *xs);
    xs->esize = esize; xs->klen = klen; xs->budget = budget; xs->fatal = fatal;
    xs->fan = fan < 2 ? 2 : fan;
    size_t bl = strlen(base); if (bl > sizeof xs->base - 1) bl = sizeof xs->base - 1;
    memcpy(xs->base, base, bl); xs->base[bl] = 0;
    xs_alloc_run(xs);
}

static void xs_sort_run(xsort *xs)
{
    for (unsigned i = 0; i < xs->n; i++) xs->order[i] = i;
    xs_merge_sort(xs, xs->order, xs->tmp, 0, xs->n);
}

static char *xs_run_name(xsort *xs)
{
    char *name = malloc(strlen(xs->base) + 16);
    if (!name) xs_die(xs, "SORT: out of memory");
    sprintf(name, "%s.r%04u", xs->base, xs->runseq++);
    if (xs->nruns == xs->runcap) {
        xs->runcap = xs->runcap ? xs->runcap * 2 : 16;
        char **nr = realloc(xs->runs, xs->runcap * sizeof *nr);
        if (!nr) xs_die(xs, "SORT: out of memory");
        xs->runs = nr;
    }
    xs->runs[xs->nruns++] = name;
    return name;
}

static void xs_flush_run(xsort *xs)
{
    if (!xs->n) return;
    xs_sort_run(xs);
    char *name = xs_run_name(xs);
    FILE *fp = fopen(name, "wb");
    if (!fp) xs_die(xs, "SORT: cannot create a work file");
    for (unsigned i = 0; i < xs->n; i++)
        if (fwrite(xs->buf + (size_t)xs->order[i] * xs->esize, xs->esize, 1, fp) != 1) xs_die(xs, "SORT: work file write failed");
    if (fclose(fp)) xs_die(xs, "SORT: work file write failed");
    xs->n = 0;
}

/* one entry in: key then record */
static void xs_put(xsort *xs, const void *key, const void *rec)
{
    if (xs->n == xs->cap) xs_flush_run(xs);
    unsigned char *e = xs->buf + (size_t)xs->n * xs->esize;
    memcpy(e, key, xs->klen);
    memcpy(e + xs->klen, rec, xs->esize - xs->klen);
    xs->n++;
}

/* ---- the k-way merge over runs[first .. first+count) ---- */

static const unsigned char *xs_head(xsort *xs, unsigned s)
{
    xs_src *r = &xs->src[s];
    return r->b + (size_t)r->bi * xs->esize;
}

static int xs_fill(xsort *xs, unsigned s)      /* 1 if the source has an entry at its head */
{
    xs_src *r = &xs->src[s];
    if (r->bi < r->bn) return 1;
    if (!r->fp) return 0;
    r->bn = (unsigned)fread(r->b, xs->esize, r->bcap, r->fp); r->bi = 0;
    if (!r->bn) { fclose(r->fp); r->fp = 0; return 0; }
    return 1;
}

static int xs_less(xsort *xs, unsigned a, unsigned b)
{
    return memcmp(xs_head(xs, a), xs_head(xs, b), xs->klen) < 0;
}

static void xs_heap_down(xsort *xs, unsigned i)
{
    unsigned *h = xs->heap, n = xs->hn;
    for (;;) {
        unsigned l = 2 * i + 1, r = l + 1, m = i;
        if (l < n && xs_less(xs, h[l], h[m])) m = l;
        if (r < n && xs_less(xs, h[r], h[m])) m = r;
        if (m == i) return;
        unsigned t = h[i]; h[i] = h[m]; h[m] = t; i = m;
    }
}

static void xs_merge_open(xsort *xs, unsigned first, unsigned count)
{
    xs->src = calloc(count, sizeof *xs->src);
    xs->heap = malloc(count * sizeof *xs->heap);
    if (!xs->src || !xs->heap) xs_die(xs, "SORT: out of memory");
    /* the run buffer is free during a merge: share it out as read-ahead,
     * count sources x per entries, never more than cap in total.  If it
     * cannot give every source one entry, grow it to exactly that. */
    size_t per = xs->cap / count;
    if (per < 1) {
        per = 1;
        unsigned char *nb = realloc(xs->buf, (size_t)count * xs->esize);
        if (!nb) xs_die(xs, "SORT: out of memory");
        xs->buf = nb; xs->cap = count;
    }
    for (unsigned i = 0; i < count; i++) {
        xs_src *r = &xs->src[i];
        r->fp = fopen(xs->runs[first + i], "rb");
        if (!r->fp) xs_die(xs, "SORT: cannot reopen a work file");
        r->bcap = (unsigned)per;
        r->b = xs->buf + (size_t)i * per * xs->esize;
        r->bn = r->bi = 0;
    }
    xs->nsrc = count; xs->hn = 0;
    for (unsigned i = 0; i < count; i++) if (xs_fill(xs, i)) xs->heap[xs->hn++] = i;
    for (int i = (int)xs->hn / 2 - 1; i >= 0; i--) xs_heap_down(xs, (unsigned)i);
}

static const unsigned char *xs_merge_next(xsort *xs)   /* NULL at end */
{
    if (!xs->hn) return 0;
    unsigned s = xs->heap[0];
    const unsigned char *e = xs_head(xs, s);
    xs->src[s].bi++;
    /* the entry stays valid until the next call: advance the heap first,
     * refilling only when the head buffer is spent -- and a refill would
     * overwrite e, so a spent buffer defers: copy out */
    static unsigned char *hold; static unsigned holdsz;
    if (xs->src[s].bi >= xs->src[s].bn) {
        if (holdsz < xs->esize) { free(hold); hold = malloc(xs->esize); holdsz = hold ? xs->esize : 0; if (!hold) xs_die(xs, "SORT: out of memory"); }
        memcpy(hold, e, xs->esize); e = hold;
    }
    if (xs_fill(xs, s)) xs_heap_down(xs, 0);
    else { xs->heap[0] = xs->heap[--xs->hn]; if (xs->hn) xs_heap_down(xs, 0); }
    return e;
}

static void xs_merge_close(xsort *xs, unsigned first, unsigned count)
{
    for (unsigned i = 0; i < count; i++) if (xs->src[i].fp) fclose(xs->src[i].fp);
    for (unsigned i = 0; i < count; i++) { remove(xs->runs[first + i]); free(xs->runs[first + i]); }
    memmove(xs->runs + first, xs->runs + first + count, (xs->nruns - first - count) * sizeof *xs->runs);
    xs->nruns -= count;
    free(xs->src); free(xs->heap); xs->src = 0; xs->heap = 0; xs->nsrc = 0;
}

/* input is complete: sort the last run; merge down to one pass */
static void xs_finish(xsort *xs)
{
    xs->finished = 1;
    if (!xs->nruns) { xs_sort_run(xs); xs->inmem = 1; xs->pos = 0; return; }
    xs_flush_run(xs);
    while (xs->nruns > xs->fan) {            /* intermediate pass: the first fan runs -> one new run */
        xs_merge_open(xs, 0, xs->fan);
        char *name = xs_run_name(xs);
        FILE *fp = fopen(name, "wb");
        if (!fp) xs_die(xs, "SORT: cannot create a work file");
        const unsigned char *e;
        while ((e = xs_merge_next(xs)) != 0)
            if (fwrite(e, xs->esize, 1, fp) != 1) xs_die(xs, "SORT: work file write failed");
        if (fclose(fp)) xs_die(xs, "SORT: work file write failed");
        xs_merge_close(xs, 0, xs->fan);
    }
    xs_merge_open(xs, 0, xs->nruns);
}

/* the next entry in order (key then record), valid until the next call; NULL at end */
static const unsigned char *xs_next(xsort *xs)
{
    if (xs->inmem) {
        if (xs->pos >= xs->n) return 0;
        return xs->buf + (size_t)xs->order[xs->pos++] * xs->esize;
    }
    return xs_merge_next(xs);
}

static void xs_free(xsort *xs)
{
    if (xs->src) { for (unsigned i = 0; i < xs->nsrc; i++) if (xs->src[i].fp) fclose(xs->src[i].fp); free(xs->src); }
    for (unsigned i = 0; i < xs->nruns; i++) { remove(xs->runs[i]); free(xs->runs[i]); }
    free(xs->runs); free(xs->heap); free(xs->buf); free(xs->order); free(xs->tmp);
    memset(xs, 0, sizeof *xs);
}
