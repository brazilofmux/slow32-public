/* machine.c -- the PC a QuickBASIC program looks at through PEEK, POKE,
 * INP, OUT and WAIT: not a CPU (no machine code runs here: SLOW-32 is
 * W^X, and CALL ABSOLUTE stays "Advanced feature unavailable"), but the
 * devices at the addresses and ports programs use --
 *
 *   A000:0000  SCREEN 13's pixels, a byte each (other modes: nothing)
 *   B800:0000  the text screen, a cell's character then its attribute
 *   0040:0000  a little of the BIOS's data: the keyboard's flags, the
 *              video mode, the columns, the rows, the clock's ticks
 *   ports 3C7-3C9  the VGA DAC: read index, write index, data (6 bits
 *              a component, red green blue, the index moving on)
 *   port 3DA   the status: vertical retrace (bit 3) 70 times a second
 *              by the clock, so WAIT &H3DA, 8 paces a game as it did
 *
 * An address is the segment times 16 plus the offset, wrapped at 1MB.
 * Conventional memory, the first 640K, keeps what is POKEd there (QBasic:
 * DEF SEG = &H1000: POKE 5, 77 reads back 77), made when first touched;
 * the rest reads 0 and takes nothing -- QBasic never refuses an address
 * (an offset past 65535 is Overflow).
 *
 * The program's own storage has addresses too, as QBasic lays it out
 * (probed): VARSEG and VARPTR give them, and PEEK, POKE, BSAVE and BLOAD
 * then reach the variable itself, as the bytes QuickBASIC keeps it in --
 *
 *   an INTEGER 2 bytes, a LONG 4, a SINGLE and a DOUBLE their IEEE 4 and
 *   8, a STRING * n its n characters, a record its fields one after
 *   another with nothing between;
 *   a variable-length string a descriptor of 4: its length, then the
 *   offset of its characters in the same segment (SADD);
 *   an array its elements with the first subscript varying fastest, in
 *   a segment of its own from offset 0;
 *   the variables, the characters of strings and the descriptors of an
 *   array of them in one segment, DEF SEG alone's and the one a program
 *   starts in.
 *
 * Nothing here moves or copies a variable: an address is a window onto
 * it, made when a program first asks (few programs ever do).  Which
 * segment is whose is ours; a program only ever gets them from VARSEG. */
#include "machine.h"
#include "graphics.h"
#include "eval.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#define CONVENTIONAL 0xA0000L
#define HOME_SEG 0x7000         /* the variables' segment (VARSEG is positive as a program holds it) */
#define HOME_LINEAR ((long)HOME_SEG << 4)
#define HOME_FIRST 0x100L       /* (their offsets stay small: VARPTR(x) + 1 must not overflow) */
#define HOME_SIZE 0x10000L
#define FAR_FIRST 0x500L        /* arrays: down from the variables, past the BIOS's */
static unsigned char *conv;     /* (made at the first POKE: a PEEK before it reads 0) */
int def_seg = HOME_SEG;         /* DEF SEG's segment */

void pc_def_seg_home(void) { def_seg = HOME_SEG; }

static int window_peek(long a, int *out);
static int window_poke(long a, int v);

static unsigned char bios_kept[256];    /* what was POKEd into 0040:xx (kept) */

static long linear(int off) {
    return ((((long)def_seg & 0xFFFF) << 4) + ((long)off & 0xFFFF)) & 0xFFFFF;
}

static error_t check_offset(int off) {
    return off >= -32768 && off <= 65535 ? ERR_NONE : ERR_OVERFLOW;
}

/* the clock's ticks since midnight, 18.2 a second, as the BIOS counts */
static unsigned long bios_ticks(void) {
    struct timespec ts;
    time_t now;
    struct tm *tm;
    double s;
    if (clock_gettime(CLOCK_REALTIME, &ts) != 0) return 0;
    now = ts.tv_sec;
    tm = localtime(&now);
    if (!tm) return 0;
    s = tm->tm_hour * 3600.0 + tm->tm_min * 60.0 + tm->tm_sec + ts.tv_nsec / 1e9;
    return (unsigned long)(s * 1193180.0 / 65536.0);
}

static int video_mode(void) {
    switch (sb_gfx_mode()) {
        case 1: return 4;    case 2: return 6;    case 7: return 0x0D;
        case 8: return 0x0E; case 9: return 0x10; case 11: return 0x11;
        case 12: return 0x12;
        case 13: return 0x13;
        default: return 3;
    }
}

static int bios_byte(int k) {
    switch (k) {
        case 0x17: return bios_kept[0x17];          /* the shift flags: none held */
        case 0x49: return video_mode();
        case 0x4A: return eval_text_cols() & 0xFF;
        case 0x4B: return eval_text_cols() >> 8;
        case 0x84: return eval_text_rows() - 1;
        case 0x6C: case 0x6D: case 0x6E: case 0x6F:
            return (int)(bios_ticks() >> (8 * (k - 0x6C)) & 0xFF);
        default: return bios_kept[k];
    }
}

static int linear_peek(long a) {
    int v;
    if (pc_windows && window_peek(a, &v)) return v;
    if (a >= 0x400 && a < 0x500) return bios_byte((int)(a - 0x400));
    if (a < CONVENTIONAL) return conv ? conv[a] : 0;
    if (a >= 0xA0000 && a < 0xB0000) { v = sb_gfx_fb_get((int)(a - 0xA0000)); return v < 0 ? 0 : v; }
    if (a >= 0xB8000 && a < 0xC0000) return eval_text_byte((int)(a - 0xB8000), -1);
    return 0;
}

error_t pc_peek(int off, int *out) {
    long a;
    EVAL_CHECK(check_offset(off));
    *out = linear_peek(linear(off));
    return ERR_NONE;
}

static error_t linear_poke(long a, int v) {
    if (pc_windows && window_poke(a, v)) return ERR_NONE;
    if (a >= 0x400 && a < 0x500) bios_kept[a - 0x400] = (unsigned char)v;
    else if (a < CONVENTIONAL) {
        if (!conv && !(conv = calloc(1, CONVENTIONAL))) return ERR_OUT_OF_MEMORY;
        conv[a] = (unsigned char)v;
    }
    else if (a >= 0xA0000 && a < 0xB0000) sb_gfx_fb_set((int)(a - 0xA0000), v);
    else if (a >= 0xB8000 && a < 0xC0000) eval_text_byte((int)(a - 0xB8000), v);
    return ERR_NONE;
}

error_t pc_poke(int off, int v) {
    long a;
    EVAL_CHECK(check_offset(off));
    a = linear(off);
    return linear_poke(a, v & 0xFF);
}

/* --- ports --- */

static int dac_windex, dac_rindex, dac_wcomp, dac_rcomp, dac_latch[3];

/* the retrace: 70 a second, about a twentieth of each in it */
#define FRAME_US 14286L
#define RETRACE_US 700L

/* where in its frame the screen is now, in microseconds (64 bits: a
 * long is 32 on SLOW-32, and seconds times a million overflow it) */
static long frame_us(void) {
    struct timespec ts;
    if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) return 0;
    return (long)(((long long)ts.tv_sec * 1000000LL + ts.tv_nsec / 1000) % FRAME_US);
}

static int status_3da(void) {
    static int toggle;
    int in_retrace = frame_us() < RETRACE_US;
    toggle ^= 1;                        /* (bit 0, display not enabled: changes as it is read) */
    return (in_retrace ? 8 : 0) | (in_retrace ? 1 : toggle);
}

/* QBasic's ranges (probed): a port from -65536 to 65535, a value or a
 * mask an INTEGER (its low byte is what goes out); past them, Overflow */
static error_t check_port(int port) {
    return port >= -65536 && port <= 65535 ? ERR_NONE : ERR_OVERFLOW;
}

static error_t check_byte(int v) {
    return v >= -32768 && v <= 32767 ? ERR_NONE : ERR_OVERFLOW;
}

error_t pc_inp(int port, int *out) {
    EVAL_CHECK(check_port(port));
    port &= 0xFFFF;
    switch (port) {
        case 0x3C9:
            *out = sb_gfx_dac_get(dac_rindex, dac_rcomp);
            if (++dac_rcomp == 3) { dac_rcomp = 0; dac_rindex = (dac_rindex + 1) & 255; }
            return ERR_NONE;
        case 0x3C8: *out = dac_windex; return ERR_NONE;
        case 0x3DA: *out = status_3da(); return ERR_NONE;
        default: *out = 255; return ERR_NONE;       /* (nothing there: the bus floats high) */
    }
}

error_t pc_out(int port, int v) {
    EVAL_CHECK(check_port(port));
    EVAL_CHECK(check_byte(v));
    v &= 0xFF;
    switch (port & 0xFFFF) {
        case 0x3C7: dac_rindex = v; dac_rcomp = 0; break;
        case 0x3C8: dac_windex = v; dac_wcomp = 0; break;
        case 0x3C9:
            dac_latch[dac_wcomp++] = v;
            if (dac_wcomp == 3) {
                sb_gfx_dac_set(dac_windex, dac_latch[0], dac_latch[1], dac_latch[2]);
                dac_wcomp = 0;
                dac_windex = (dac_windex + 1) & 255;
            }
            break;
        default: break;                 /* (no other device here) */
    }
    return ERR_NONE;
}

/* WAIT port, and[, xor]: until (INP(port) XOR xor) AND and is not 0 --
 * at 3DA by the clock (asleep until the retrace begins or ends); any
 * other port's value never changes here, so it is looked at once */
error_t pc_wait(int port, int and_mask, int xor_mask) {
    int v;
    EVAL_CHECK(check_port(port));
    EVAL_CHECK(check_byte(and_mask));
    EVAL_CHECK(check_byte(xor_mask));
    and_mask &= 0xFF;
    xor_mask &= 0xFF;
    if ((port & 0xFFFF) != 0x3DA) {
        return pc_inp(port, &v);
    }
    for (int tries = 0; tries < 100000; tries++) {
        EVAL_CHECK(pc_inp(port, &v));
        if ((v ^ xor_mask) & and_mask) return ERR_NONE;
        if (and_mask & 8) {             /* the retrace bit: sleep to its change */
            long t = frame_us();
            long left = (v & 8) ? RETRACE_US - t : FRAME_US - t;
            if (left > 0) usleep((unsigned)left);
        }
    }
    return ERR_NONE;
}

void pc_reset(void) {
    memset(bios_kept, 0, sizeof bios_kept);
    if (conv) memset(conv, 0, CONVENTIONAL);
    dac_windex = dac_rindex = dac_wcomp = dac_rcomp = 0;
    def_seg = HOME_SEG;
    pc_forget(NULL, NULL);
}

/* --- the program's own storage --------------------------------------- */

enum { W_ARRAY = 16, W_CHARS };         /* (beyond the PC_ kinds; W_CHARS's sub says whose) */

typedef struct {
    long base, size;            /* its linear addresses */
    int kind, sub;
    void *p;                    /* the variable, or the array */
    int fixlen;                 /* a STRING * n's n */
    int on_stack;               /* compiled code's: gone when its frame is */
} window_t;

static window_t *win;           /* in order of base */
int pc_windows;
static int win_cap;

static sb_string_t *str_at(int kind, void *p) {
    return kind == PC_STRING ? *(sb_string_t **)p : ((value_t *)p)->sval;
}

static void str_set(int kind, void *p, sb_string_t *s) {
    string_unref(str_at(kind, p));
    if (kind == PC_STRING) *(sb_string_t **)p = s;
    else ((value_t *)p)->sval = s;
}

/* the size of what a value of type t keeps in memory */
static long value_size(const value_t *v, val_type_t t, int fixlen) {
    if (t == VAL_STRING) return fixlen ? fixlen : 4;
    if (t == VAL_RECORD) {
        long n = 0;
        if (v->type != VAL_RECORD || !v->rval) return 0;
        for (int i = 0; i < v->rval->nfields; i++) {
            const value_t *f = &v->rval->fields[i];
            n += f->type == VAL_STRING ? (f->sval ? f->sval->len : 0) : value_size(f, f->type, 0);
        }
        return n;
    }
    return elem_bytes(t);
}

int pc_field_at(const sb_record_t *r, int f) {
    long n = 0;
    for (int i = 0; i < f && i < r->nfields; i++) {
        const value_t *x = &r->fields[i];
        n += x->type == VAL_STRING ? (x->sval ? x->sval->len : 0) : value_size(x, x->type, 0);
    }
    return (int)n;
}

static long array_elem_size(const sb_array_t *a) {
    return a->total > 0 ? value_size(&a->data[0], a->elem_type, a->fixlen) : elem_bytes(a->elem_type);
}

/* a string's descriptor: its length, then where its characters are */
static int descriptor_byte(int kind, void *p, int k) {
    sb_string_t *s = str_at(kind, p);
    int n = s ? s->len : 0, seg, off = 0;
    if (k < 2) return (n >> (8 * k)) & 0xFF;
    if (n && pc_addr_chars(kind, p, &seg, &off) != ERR_NONE) off = 0;
    return (off >> (8 * (k - 2))) & 0xFF;
}

/* a POKE into the length: the characters past a shorter length are still
 * there to come back, as they are in QBasic (one string's, the last so
 * cut); a POKE into where they are takes nothing here */
static const void *cut_key;
static sb_string_t *cut;

static void descriptor_poke(int kind, void *p, int k, int v) {
    sb_string_t *s = str_at(kind, p), *ns;
    int n = s ? s->len : 0, m, i;
    if (k >= 2) return;
    m = k == 0 ? (n & 0xFF00) | v : (n & 0xFF) | (v << 8);
    if (m > 32767 || !(ns = string_alloc(NULL, m))) return;
    for (i = 0; i < m; i++)
        ns->data[i] = (char)(i < n ? s->data[i] : cut_key == p && cut && i < cut->len ? cut->data[i] : 0);
    if (s && (cut_key != p || !cut || n >= cut->len)) {
        if (cut) string_unref(cut);
        string_ref(s);
        cut = s;
        cut_key = p;
    }
    str_set(kind, p, ns);
}

/* a string's character k changed: a string is never changed in place (it
 * may be shared), so the variable gets a string of its own */
static void chars_poke(int kind, void *p, int k, int v) {
    sb_string_t *s = str_at(kind, p), *ns;
    if (!s || k >= s->len || !(ns = string_alloc(s->data, s->len))) return;
    ns->data[k] = (char)v;
    str_set(kind, p, ns);
}

/* byte k of the value at v, as type t keeps it; key, how a string's
 * descriptor names the variable (kind PC_VALUE or PC_STRING) */
static int value_peek(value_t *v, val_type_t t, int fixlen, int k, int kind, void *key) {
    unsigned char b[8];
    if (t == VAL_STRING) {
        if (!fixlen) return descriptor_byte(kind, key, k);
        return v->sval && k < v->sval->len ? (unsigned char)v->sval->data[k] : 0;
    }
    if (t == VAL_RECORD) {
        for (int i = 0; v->type == VAL_RECORD && i < v->rval->nfields; i++) {
            value_t *f = &v->rval->fields[i];
            int str = f->type == VAL_STRING;
            int n = str ? (f->sval ? f->sval->len : 0) : (int)value_size(f, f->type, 0);
            if (k < n) return value_peek(f, f->type, n, k, PC_VALUE, f);
            k -= n;
        }
        return 0;
    }
    elem_to_bytes(v, t, b);
    return b[k];
}

static void value_poke(value_t *v, val_type_t t, int fixlen, int k, int x, int kind, void *key) {
    unsigned char b[8];
    if (t == VAL_STRING) {
        if (!fixlen) descriptor_poke(kind, key, k, x);
        else chars_poke(kind, key, k, x);
        return;
    }
    if (t == VAL_RECORD) {
        for (int i = 0; v->type == VAL_RECORD && i < v->rval->nfields; i++) {
            value_t *f = &v->rval->fields[i];
            int str = f->type == VAL_STRING;
            int n = str ? (f->sval ? f->sval->len : 0) : (int)value_size(f, f->type, 0);
            if (k < n) { value_poke(f, f->type, n, k, x, PC_VALUE, f); return; }
            k -= n;
        }
        return;
    }
    elem_to_bytes(v, t, b);
    b[k] = (unsigned char)x;
    elem_from_bytes(v, t, b);
}

/* a compiled variable as a value, and back */
static void slot_read(int kind, void *p, value_t *v) {
    switch (kind) {
    case PC_INT: v->type = VAL_INTEGER; v->ival = *(int *)p; break;
    case PC_LONG: v->type = VAL_LONG; v->ival = *(int *)p; break;
    case PC_FLOAT: v->type = VAL_SINGLE; v->dval = *(float *)p; break;
    case PC_DOUBLE: v->type = VAL_DOUBLE; memcpy(&v->dval, p, 8); break;
    case PC_STRING: v->type = VAL_STRING; v->sval = *(sb_string_t **)p; break;
    default: v->type = VAL_RECORD; v->rval = *(sb_record_t **)p; break;
    }
}

static void slot_write(int kind, void *p, const value_t *v) {
    switch (kind) {
    case PC_INT: case PC_LONG: *(int *)p = v->ival; break;
    case PC_FLOAT: *(float *)p = (float)v->dval; break;
    case PC_DOUBLE: memcpy(p, &v->dval, 8); break;
    default: break;             /* (a string's and a record's change where they are) */
    }
}

static void window_drop(int i) {
    memmove(&win[i], &win[i + 1], (size_t)(pc_windows - i - 1) * sizeof(window_t));
    pc_windows--;
}

void pc_forget(const void *lo, const void *hi) {
    if (!lo) {
        pc_windows = 0;
        if (cut) string_unref(cut);
        cut = NULL;
        cut_key = NULL;
        return;
    }
    for (int i = pc_windows - 1; i >= 0; i--)
        if ((const char *)win[i].p >= (const char *)lo && (const char *)win[i].p < (const char *)hi)
            window_drop(i);
    if (cut && (const char *)cut_key >= (const char *)lo && (const char *)cut_key < (const char *)hi) {
        string_unref(cut);
        cut = NULL;
        cut_key = NULL;
    }
}

/* the window over address a: compiled locals whose frame has returned are
 * gone (the stack grows down, and a frame below this one's is dead) */
static window_t *window_at(long a) {
    int lo = 0, hi = pc_windows - 1;
    char here;
    while (lo <= hi) {
        int m = (lo + hi) / 2;
        window_t *w = &win[m];
        if (a < w->base) hi = m - 1;
        else if (a >= w->base + w->size) lo = m + 1;
        else {
            if (w->on_stack && (char *)w->p < &here) { window_drop(m); return NULL; }
            return w;
        }
    }
    return NULL;
}

static int window_peek(long a, int *out) {
    window_t *w = window_at(a);
    long k;
    value_t t;
    if (!w) return 0;
    k = a - w->base;
    if (w->kind == W_ARRAY) {
        sb_array_t *arr = w->p;
        long sz = array_elem_size(arr);
        value_t *e = &arr->data[array_row_index(arr, (int)(k / sz))];
        *out = value_peek(e, arr->elem_type, arr->fixlen, (int)(k % sz), PC_VALUE, e);
    } else if (w->kind == W_CHARS) {
        sb_string_t *s = str_at(w->sub, w->p);
        *out = s && k < s->len ? (unsigned char)s->data[k] : 0;
    } else if (w->kind == PC_VALUE) {
        value_t *v = w->p;
        *out = value_peek(v, v->type, w->fixlen, (int)k, PC_VALUE, v);
    } else {
        slot_read(w->kind, w->p, &t);
        *out = value_peek(&t, t.type, w->fixlen, (int)k, PC_STRING, w->p);
    }
    return 1;
}

static int window_poke(long a, int x) {
    window_t *w = window_at(a);
    long k;
    value_t t;
    if (!w) return 0;
    k = a - w->base;
    if (w->kind == W_ARRAY) {
        sb_array_t *arr = w->p;
        long sz = array_elem_size(arr);
        value_t *e = &arr->data[array_row_index(arr, (int)(k / sz))];
        value_poke(e, arr->elem_type, arr->fixlen, (int)(k % sz), x, PC_VALUE, e);
    } else if (w->kind == W_CHARS) {
        chars_poke(w->sub, w->p, (int)k, x);
    } else if (w->kind == PC_VALUE) {
        value_t *v = w->p;
        value_poke(v, v->type, w->fixlen, (int)k, x, PC_VALUE, v);
    } else {
        slot_read(w->kind, w->p, &t);
        value_poke(&t, t.type, w->fixlen, (int)k, x, PC_STRING, w->p);
        slot_write(w->kind, w->p, &t);
    }
    return 1;
}

/* size bytes free among the windows: an array's from the top of the far
 * memory down, on a segment's boundary, at offset 0; a variable's from the
 * bottom of the variables' segment up.  -1: no room */
static long room(long size, int far) {
    if (far) {
        long top = HOME_LINEAR;
        for (int i = pc_windows - 1; i >= 0; i--) {
            long want = (top - size) & ~15L;
            if (win[i].base >= top) continue;
            if (want >= win[i].base + win[i].size) return want >= FAR_FIRST ? want : -1;
            top = win[i].base;
        }
        top = (top - size) & ~15L;
        return top >= FAR_FIRST ? top : -1;
    }
    long at = HOME_LINEAR + HOME_FIRST;
    for (int i = 0; i < pc_windows; i++) {
        if (win[i].base + win[i].size <= at) continue;
        if (win[i].base >= at + size) break;
        at = (win[i].base + win[i].size + 1) & ~1L;
    }
    return at + size <= HOME_LINEAR + HOME_SIZE ? at : -1;
}

static error_t window_make(int kind, int sub, void *p, int fixlen, long size, int far, int on_stack, window_t **out) {
    long base;
    int i;
    if (size <= 0) size = 1;
    base = room(size, far);
    if (base < 0) return ERR_OUT_OF_MEMORY;
    if (pc_windows == win_cap) {
        int cap = win_cap ? win_cap * 2 : 32;
        window_t *t = realloc(win, (size_t)cap * sizeof(window_t));
        if (!t) return ERR_OUT_OF_MEMORY;
        win = t;
        win_cap = cap;
    }
    for (i = pc_windows; i > 0 && win[i - 1].base > base; i--) win[i] = win[i - 1];
    win[i].base = base;
    win[i].size = size;
    win[i].kind = kind;
    win[i].sub = sub;
    win[i].p = p;
    win[i].fixlen = fixlen;
    win[i].on_stack = on_stack;
    pc_windows++;
    *out = &win[i];
    return ERR_NONE;
}

static window_t *window_of(int kind, const void *p) {
    for (int i = 0; i < pc_windows; i++)
        if (win[i].p == p && win[i].kind == kind) return &win[i];
    return NULL;
}

/* a variable's address, in the variables' segment */
static void home_addr(const window_t *w, long byte, int *seg, int *off) {
    *seg = HOME_SEG;
    *off = (int)(w->base - HOME_LINEAR + byte);
}

error_t pc_addr_var(int kind, void *p, int fixlen, int byte, int *seg, int *off) {
    window_t *w = window_of(kind, p);
    if (!w) {
        char here;
        value_t t;
        const value_t *v = p;
        long size;
        if (kind != PC_VALUE) { slot_read(kind, p, &t); v = &t; }
        size = value_size(v, v->type, fixlen);
        EVAL_CHECK(window_make(kind, 0, p, fixlen, size, 0,
                               kind != PC_VALUE && (char *)p > &here, &w));
    }
    home_addr(w, byte, seg, off);
    return ERR_NONE;
}

error_t pc_addr_elem(sb_array_t *arr, int k, int byte, int *seg, int *off) {
    window_t *w = window_of(W_ARRAY, arr);
    long sz = array_elem_size(arr), at;
    int near = arr->elem_type == VAL_STRING && !arr->fixlen;
    if (!w) EVAL_CHECK(window_make(W_ARRAY, 0, arr, 0, (long)arr->total * sz, !near, 0, &w));
    at = (long)array_mem_index(arr, k) * sz + byte;
    if (near) {
        home_addr(w, at, seg, off);
        return ERR_NONE;
    }
    /* (an array past 64K, which QBasic has not: the segment moves on) */
    *seg = (int)((w->base >> 4) + (at >> 16) * 0x1000);
    *off = (int)(at & 0xFFFF);
    return ERR_NONE;
}

error_t pc_addr_chars(int kind, void *p, int *seg, int *off) {
    sb_string_t *s = str_at(kind, p);
    window_t *w = window_of(W_CHARS, p);
    long n = s ? s->len : 0;
    *seg = HOME_SEG;
    *off = 0;
    if (!n) return ERR_NONE;
    if (w && w->size < n) {     /* (it has grown: QBasic moves it too) */
        window_drop((int)(w - win));
        w = NULL;
    }
    if (!w) {
        char here;
        EVAL_CHECK(window_make(W_CHARS, kind, p, 0, n, 0, kind != PC_VALUE && (char *)p > &here, &w));
    }
    home_addr(w, 0, seg, off);
    return ERR_NONE;
}

/* --- BSAVE and BLOAD: a file of FD, the segment, the offset, the length,
 * each a word, then the bytes.  A length of -1 is 65535 (an INTEGER's
 * bits); BLOAD with no offset goes back where BSAVE took it from, DEF SEG
 * as it was.  What is not such a file is Bad file mode -- and so is one
 * shorter than it says, its bytes loaded first (QBasic, probed). */
error_t pc_bsave(const char *file, int offset, int length) {
    unsigned char h[7];
    long a;
    FILE *fp;
    EVAL_CHECK(check_offset(offset));
    EVAL_CHECK(check_offset(length));
    if (!*file) return ERR_BAD_FILE_NUMBER;
    if (!(fp = fopen(file, "wb"))) return ERR_PATH_NOT_FOUND;
    length &= 0xFFFF;
    h[0] = 0xFD;
    h[1] = (unsigned char)def_seg; h[2] = (unsigned char)(def_seg >> 8);
    h[3] = (unsigned char)offset; h[4] = (unsigned char)(offset >> 8);
    h[5] = (unsigned char)length; h[6] = (unsigned char)(length >> 8);
    fwrite(h, 1, 7, fp);
    a = linear(offset);
    for (long i = 0; i < length; i++) putc(linear_peek((a + i) & 0xFFFFF), fp);
    fclose(fp);
    return ERR_NONE;
}

error_t pc_bload(const char *file, int has_offset, int offset) {
    unsigned char h[7];
    long a, n, i;
    int c;
    FILE *fp;
    error_t e = ERR_NONE;
    if (has_offset) EVAL_CHECK(check_offset(offset));
    if (!*file) return ERR_BAD_FILE_NUMBER;
    if (!(fp = fopen(file, "rb"))) return ERR_FILE_NOT_FOUND;
    if (fread(h, 1, 7, fp) != 7 || h[0] != 0xFD) { fclose(fp); return ERR_BAD_FILE_MODE; }
    n = h[5] | (long)h[6] << 8;
    a = has_offset ? linear(offset)
                   : (((long)(h[1] | h[2] << 8) << 4) + (h[3] | (long)h[4] << 8)) & 0xFFFFF;
    for (i = 0; i < n && e == ERR_NONE; i++) {
        if ((c = getc(fp)) == EOF) { e = ERR_BAD_FILE_MODE; break; }
        e = linear_poke((a + i) & 0xFFFFF, c);
    }
    fclose(fp);
    return e;
}
