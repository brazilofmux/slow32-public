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
 * (an offset past 65535 is Overflow). */
#include "machine.h"
#include "graphics.h"
#include "eval.h"
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#define CONVENTIONAL 0xA0000L
static unsigned char *conv;     /* (made at the first POKE: a PEEK before it reads 0) */
int def_seg = 0;                /* DEF SEG's segment */

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
        case 8: return 0x0E; case 9: return 0x10; case 12: return 0x12;
        case 13: return 0x13;
        default: return 3;
    }
}

/* the BIOS's columns: the text width, or in graphics the mode's own --
 * 40 in the 320-pixel modes (1, 7, 13), 80 in the rest (QBasic on a VGA) */
static int bios_cols(void) {
    switch (sb_gfx_mode()) {
        case 0: return eval_text_cols();
        case 1: case 7: case 13: return 40;
        default: return 80;
    }
}

static int bios_byte(int k) {
    switch (k) {
        case 0x17: return bios_kept[0x17];          /* the shift flags: none held */
        case 0x49: return video_mode();
        case 0x4A: return bios_cols() & 0xFF;
        case 0x4B: return bios_cols() >> 8;
        case 0x84: return eval_text_rows() - 1;
        case 0x6C: case 0x6D: case 0x6E: case 0x6F:
            return (int)(bios_ticks() >> (8 * (k - 0x6C)) & 0xFF);
        default: return bios_kept[k];
    }
}

error_t pc_peek(int off, int *out) {
    long a;
    EVAL_CHECK(check_offset(off));
    a = linear(off);
    if (a >= 0x400 && a < 0x500) *out = bios_byte((int)(a - 0x400));
    else if (a < CONVENTIONAL) *out = conv ? conv[a] : 0;
    else if (a >= 0xA0000 && a < 0xB0000) { int v = sb_gfx_fb_get((int)(a - 0xA0000)); *out = v < 0 ? 0 : v; }
    else if (a >= 0xB8000 && a < 0xC0000) *out = eval_text_byte((int)(a - 0xB8000), -1);
    else *out = 0;
    return ERR_NONE;
}

error_t pc_poke(int off, int v) {
    long a;
    EVAL_CHECK(check_offset(off));
    a = linear(off);
    v &= 0xFF;
    if (a >= 0x400 && a < 0x500) bios_kept[a - 0x400] = (unsigned char)v;
    else if (a < CONVENTIONAL) {
        if (!conv && !(conv = calloc(1, CONVENTIONAL))) return ERR_OUT_OF_MEMORY;
        conv[a] = (unsigned char)v;
    }
    else if (a >= 0xA0000 && a < 0xB0000) sb_gfx_fb_set((int)(a - 0xA0000), v);
    else if (a >= 0xB8000 && a < 0xC0000) eval_text_byte((int)(a - 0xB8000), v);
    return ERR_NONE;
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
    def_seg = 0;
}
