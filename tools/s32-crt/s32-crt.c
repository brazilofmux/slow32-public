/* s32-crt — attach to a SLOW-32 tube and render (or dump) vec frames.
 *
 * The emulator never renders. This process is the glass.
 * No SDL, no Cocoa: a terminal grid, or --text for the wire dump.
 *
 *   s32-crt                      wait for ./tube.port, draw in the terminal
 *   s32-crt --port 12345         connect to that port
 *   s32-crt --port-file PATH     read the port number from a file
 *   s32-crt --wait --once --text print one VSEG as text and exit
 */
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

#define TAG_HELO 0x4F4C4548u
#define TAG_VSEG 0x47455356u
#define TAG_KEYE 0x4559454Bu
#define TAG_BYE  0x00455942u

#define SPACE 4096

static int opt_once;
static int opt_text;
static int opt_frames = 1;
static int opt_port = -1;
static const char *opt_port_file = "tube.port";
static int opt_draw = 1;

static int read_port_file(const char *path) {
    FILE *f = fopen(path, "r");
    unsigned p = 0;
    if (!f) {
        return -1;
    }
    if (fscanf(f, "%u", &p) != 1 || p == 0 || p > 65535) {
        fclose(f);
        return -1;
    }
    fclose(f);
    return (int)p;
}

static int connect_port(int port) {
    int fd;
    struct sockaddr_in addr;
    fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) {
        return -1;
    }
#ifdef SO_NOSIGPIPE
    {
        int one = 1;
        setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &one, sizeof(one));
    }
#endif
    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    addr.sin_port = htons((uint16_t)port);
    if (connect(fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        close(fd);
        return -1;
    }
    return fd;
}

/* timeout_ms < 0 waits forever. A successful attach is a connect, not
   just a file: leftover tube.port from a crashed run is ignored. */
static int attach_port_file(const char *path, int timeout_ms) {
    int waited = 0;
    int announced = 0;
    for (;;) {
        int p = read_port_file(path);
        if (p > 0) {
            int fd = connect_port(p);
            if (fd >= 0) {
                return fd;
            }
        }
        if (timeout_ms >= 0 && waited >= timeout_ms) {
            return -1;
        }
        if (!announced) {
            fprintf(stderr, "s32-crt: waiting for %s\n", path);
            announced = 1;
        }
        usleep(20 * 1000);
        waited += 20;
    }
}

static int recvn(int fd, void *buf, size_t n) {
    uint8_t *p = (uint8_t *)buf;
    while (n > 0) {
        ssize_t r = recv(fd, p, n, 0);
        if (r < 0) {
            if (errno == EINTR) {
                continue;
            }
            return -1;
        }
        if (r == 0) {
            return -1;
        }
        p += (size_t)r;
        n -= (size_t)r;
    }
    return 0;
}

static int sendn(int fd, const void *buf, size_t n) {
    const uint8_t *p = (const uint8_t *)buf;
    while (n > 0) {
        ssize_t w = send(fd, p, n, 0);
        if (w < 0) {
            if (errno == EINTR) {
                continue;
            }
            return -1;
        }
        p += (size_t)w;
        n -= (size_t)w;
    }
    return 0;
}

/* S32_CRT_KEYLOG=<path> appends raw stdin bytes and sent events —
 * for diagnosing what a particular terminal actually emits. */
static FILE *keylog;

static int send_key(int fd, uint16_t code, uint8_t down) {
    uint32_t hdr[2];
    uint8_t ev[4];
    if (keylog) {
        fprintf(keylog, "send %#x %s\n", code, down ? "down" : "up");
        fflush(keylog);
    }
    hdr[0] = 8;
    hdr[1] = TAG_KEYE;
    ev[0] = (uint8_t)(code & 0xFF);
    ev[1] = (uint8_t)(code >> 8);
    ev[2] = down;
    ev[3] = 0;
    if (sendn(fd, hdr, 8) < 0) {
        return -1;
    }
    return sendn(fd, ev, 4);
}

typedef struct {
    uint16_t x0, y0, x1, y1;
    uint8_t r, g, b, i;
} seg_t;

static void print_text(const seg_t *segs, uint32_t count) {
    uint32_t i;
    for (i = 0; i < count; i++) {
        const seg_t *s = &segs[i];
        printf("S %u %u %u %u %u %u %u %u\n",
               s->x0, s->y0, s->x1, s->y1, s->r, s->g, s->b, s->i);
    }
    fflush(stdout);
}

static int term_cols = 80;
static int term_rows = 24;
static uint8_t *phos;
static struct termios raw_saved;
static int raw_on;

static void phos_resize(void) {
    struct winsize ws;
    int cols = 80, rows = 24;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == 0 && ws.ws_col > 4 && ws.ws_row > 3) {
        cols = ws.ws_col;
        rows = ws.ws_row - 1;
    }
    if (cols == term_cols && rows == term_rows && phos) {
        return;
    }
    term_cols = cols;
    term_rows = rows;
    free(phos);
    phos = (uint8_t *)calloc((size_t)term_cols * (size_t)term_rows, 1);
}

static void plot(int x, int y, uint8_t add) {
    size_t i;
    unsigned v;
    if (x < 0 || y < 0 || x >= term_cols || y >= term_rows || !phos) {
        return;
    }
    i = (size_t)y * (size_t)term_cols + (size_t)x;
    v = phos[i] + add;
    phos[i] = v > 255 ? 255 : (uint8_t)v;
}

static void line(int x0, int y0, int x1, int y1, uint8_t add) {
    int dx = abs(x1 - x0), sx = x0 < x1 ? 1 : -1;
    int dy = -abs(y1 - y0), sy = y0 < y1 ? 1 : -1;
    int err = dx + dy;
    for (;;) {
        plot(x0, y0, add);
        if (x0 == x1 && y0 == y1) {
            break;
        }
        {
            int e2 = 2 * err;
            if (e2 >= dy) {
                err += dy;
                x0 += sx;
            }
            if (e2 <= dx) {
                err += dx;
                y0 += sy;
            }
        }
    }
}

static void map_pt(uint16_t x, uint16_t y, int *ox, int *oy) {
    /* vec is y-up; terminal is y-down. Letterbox to a square. */
    int side = term_cols < term_rows * 2 ? term_cols : term_rows * 2;
    int xoff = (term_cols - side) / 2;
    int yoff = (term_rows - side / 2) / 2;
    *ox = xoff + (int)((uint32_t)x * (uint32_t)side / SPACE);
    *oy = yoff + (int)((uint32_t)(SPACE - 1 - y) * (uint32_t)(side / 2) / SPACE);
}

static void draw_segs(const seg_t *segs, uint32_t count) {
    static const char ramp[] = " .:-=+*#%@";
    uint32_t i;
    int r, c;
    size_t n;

    phos_resize();
    if (!phos) {
        return;
    }
    n = (size_t)term_cols * (size_t)term_rows;
    for (i = 0; i < n; i++) {
        phos[i] = (uint8_t)((phos[i] * 180u) / 256u);
    }
    for (i = 0; i < count; i++) {
        int x0, y0, x1, y1;
        uint8_t add = segs[i].i > 16 ? segs[i].i : 16;
        map_pt(segs[i].x0, segs[i].y0, &x0, &y0);
        map_pt(segs[i].x1, segs[i].y1, &x1, &y1);
        line(x0, y0, x1, y1, add);
    }
    fputs("\033[H\033[2J", stdout);
    for (r = 0; r < term_rows; r++) {
        for (c = 0; c < term_cols; c++) {
            unsigned v = phos[(size_t)r * (size_t)term_cols + (size_t)c];
            unsigned idx = (v * (sizeof(ramp) - 2)) / 255;
            fputc(ramp[idx], stdout);
        }
        fputc('\n', stdout);
    }
    fflush(stdout);
}

static void raw_enter(void) {
    struct termios t;
    if (!isatty(STDIN_FILENO) || raw_on) {
        return;
    }
    if (tcgetattr(STDIN_FILENO, &raw_saved) < 0) {
        return;
    }
    t = raw_saved;
    t.c_lflag &= ~(ICANON | ECHO);
    t.c_cc[VMIN] = 0;
    t.c_cc[VTIME] = 0;
    tcsetattr(STDIN_FILENO, TCSANOW, &t);
    raw_on = 1;
}

static void raw_leave(void) {
    if (raw_on) {
        tcsetattr(STDIN_FILENO, TCSANOW, &raw_saved);
        raw_on = 0;
    }
    free(phos);
    phos = NULL;
}

/* A terminal has no key-up, and escape sequences can arrive split
 * across reads. So: bytes accumulate in a carry buffer; a lone ESC is
 * only reported as the ESC key after a short quiet gap; and arrow
 * keys become synthesized make/break — down on first sight, held
 * while terminal autorepeat keeps them coming, up after the repeats
 * stop. Everything else stays an instant down+up pair. */

#define ESC_GAP_MS    150
#define REPEAT_GAP_MS 500

static unsigned char ibuf[32];
static int ifill;
static uint16_t held_code;
static long long held_last_ms;
static long long esc_since_ms;

static long long now_ms(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (long long)ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}

static int is_arrow(uint16_t code) {
    return code >= 0x100 && code <= 0x103;
}

static int release_held(int fd) {
    int rc = 0;
    if (held_code) {
        if (fd >= 0 && send_key(fd, held_code, 0) < 0) {
            rc = -1;
        }
        held_code = 0;
    }
    return rc;
}

/* Returns 1 on local quit, -1 on send failure, else 0. */
static int emit_key(int fd, uint16_t code) {
    if (code == 'q' || code == 'Q') {
        release_held(fd);
        return 1;
    }
    if (is_arrow(code)) {
        if (code == held_code) {
            held_last_ms = now_ms();     /* autorepeat: still held */
            return 0;
        }
        if (release_held(fd) < 0) {
            return -1;
        }
        if (fd >= 0 && send_key(fd, code, 1) < 0) {
            return -1;
        }
        held_code = code;
        held_last_ms = now_ms();
        return 0;
    }
    if (fd >= 0 &&
        (send_key(fd, code, 1) < 0 || send_key(fd, code, 0) < 0)) {
        return -1;
    }
    return 0;
}

static int pump_keys(int fd) {
    long long now;
    if (!raw_on) {
        return 0;
    }
    if (ifill < (int)sizeof(ibuf)) {
        ssize_t n = read(STDIN_FILENO, ibuf + ifill, sizeof(ibuf) - (size_t)ifill);
        if (n > 0) {
            if (keylog) {
                ssize_t k;
                fprintf(keylog, "read");
                for (k = 0; k < n; k++) {
                    fprintf(keylog, " %02x", ibuf[ifill + k]);
                }
                fprintf(keylog, "\n");
                fflush(keylog);
            }
            ifill += (int)n;
        }
    }
    now = now_ms();
    while (ifill > 0) {
        uint16_t code = 0;
        int used = 1;
        unsigned char b0 = ibuf[0];
        if (b0 == 0x1b) {
            /* Arrows arrive as CSI (ESC [ ... final) or SS3 (ESC O final)
             * depending on the terminal's cursor-key mode. Parse both;
             * never fabricate an ESC key out of a partial sequence. */
            if (ifill >= 2 && (ibuf[1] == '[' || ibuf[1] == 'O')) {
                int fin = 2;
                while (fin < ifill &&
                       (ibuf[fin] < 0x40 || ibuf[fin] > 0x7E)) {
                    fin++;              /* CSI parameter/intermediate bytes */
                }
                if (fin >= ifill) {
                    if (ifill == (int)sizeof(ibuf)) {
                        used = ifill;   /* runaway sequence: drop it */
                        code = 0;
                    } else {
                        break;          /* incomplete: wait for the rest */
                    }
                } else {
                    used = fin + 1;
                    if (ibuf[fin] == 'A') code = 0x100;
                    else if (ibuf[fin] == 'B') code = 0x101;
                    else if (ibuf[fin] == 'C') code = 0x103;
                    else if (ibuf[fin] == 'D') code = 0x102;
                    /* other finals: swallow silently */
                }
            } else if (ifill >= 2) {
                code = 27;              /* ESC then an ordinary key */
            } else {
                /* Lone ESC: wait out the gap before deciding the user
                 * really meant the ESC key. */
                if (esc_since_ms == 0) {
                    esc_since_ms = now;
                }
                if (now - esc_since_ms < ESC_GAP_MS) {
                    break;
                }
                code = 27;
            }
        } else if (b0 == '\r' || b0 == '\n') {
            code = 13;
        } else if (b0 == 0x7f) {
            code = 8;
        } else {
            code = b0;
        }
        esc_since_ms = 0;
        memmove(ibuf, ibuf + used, (size_t)(ifill - used));
        ifill -= used;
        if (code) {
            int rc = emit_key(fd, code);
            if (rc != 0) {
                return rc;
            }
        }
    }
    if (held_code && now - held_last_ms > REPEAT_GAP_MS) {
        if (release_held(fd) < 0) {
            return -1;
        }
    }
    return 0;
}

static void usage(void) {
    fprintf(stderr,
            "usage: s32-crt [--port N | --port-file PATH] [--once]\n"
            "               [--text] [--frames N]\n"
            "  Port-file attach waits for the emulator to write the file.\n"
            "  --once gives up after 5s if nothing appears (for tests).\n");
}

int main(int argc, char **argv) {
    int i, fd, got = 0, quit = 0;
    uint8_t *payload = NULL;
    size_t paycap = 0;

    for (i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--once") == 0) {
            opt_once = 1;
        } else if (strcmp(argv[i], "--text") == 0) {
            opt_text = 1;
            opt_draw = 0;
        } else if (strcmp(argv[i], "--wait") == 0) {
            /* Default for port-file attach; accepted for old command lines. */
        } else if (strcmp(argv[i], "--port") == 0 && i + 1 < argc) {
            opt_port = atoi(argv[++i]);
        } else if (strcmp(argv[i], "--port-file") == 0 && i + 1 < argc) {
            opt_port_file = argv[++i];
        } else if (strcmp(argv[i], "--frames") == 0 && i + 1 < argc) {
            opt_frames = atoi(argv[++i]);
            if (opt_frames < 1) {
                opt_frames = 1;
            }
        } else if (strcmp(argv[i], "--help") == 0) {
            usage();
            return 0;
        } else {
            usage();
            return 2;
        }
    }

    if (opt_port > 0) {
        fd = connect_port(opt_port);
        if (fd < 0) {
            fprintf(stderr, "s32-crt: connect %d failed\n", opt_port);
            return 1;
        }
    } else {
        /* Interactive glass waits forever. --once is for tests. */
        int attach_ms = opt_once ? 5000 : -1;
        fd = attach_port_file(opt_port_file, attach_ms);
        if (fd < 0) {
            fprintf(stderr, "s32-crt: no listener at %s\n", opt_port_file);
            return 1;
        }
    }

    {
        const char *kl = getenv("S32_CRT_KEYLOG");
        if (kl && kl[0]) {
            keylog = fopen(kl, "a");
        }
    }
    if (opt_draw && !opt_text) {
        raw_enter();
        atexit(raw_leave);
    }

    while (!quit) {
        uint32_t length = 0, tag = 0;

        if (recvn(fd, &length, 4) < 0 || recvn(fd, &tag, 4) < 0) {
            break;
        }
        if (length < 4 || length > 16u * 1024u * 1024u) {
            break;
        }
        {
            uint32_t plen = length - 4;
            if (plen > paycap) {
                uint8_t *nbuf = (uint8_t *)realloc(payload, plen);
                if (!nbuf) {
                    break;
                }
                payload = nbuf;
                paycap = plen;
            }
            if (plen && recvn(fd, payload, plen) < 0) {
                break;
            }
            if (tag == TAG_VSEG && plen >= 8) {
                uint32_t gen, count;
                memcpy(&gen, payload, 4);
                memcpy(&count, payload + 4, 4);
                (void)gen;
                if (8u + count * 12u <= plen) {
                    seg_t *segs = (seg_t *)malloc((size_t)count * sizeof(seg_t));
                    uint32_t s;
                    if (!segs && count) {
                        break;
                    }
                    for (s = 0; s < count; s++) {
                        const uint8_t *p = payload + 8 + s * 12;
                        memcpy(&segs[s].x0, p + 0, 2);
                        memcpy(&segs[s].y0, p + 2, 2);
                        memcpy(&segs[s].x1, p + 4, 2);
                        memcpy(&segs[s].y1, p + 6, 2);
                        segs[s].r = p[8];
                        segs[s].g = p[9];
                        segs[s].b = p[10];
                        segs[s].i = p[11];
                    }
                    if (opt_text) {
                        print_text(segs, count);
                    } else if (opt_draw) {
                        draw_segs(segs, count);
                    }
                    free(segs);
                    got++;
                    if (opt_once && got >= opt_frames) {
                        quit = 1;
                    }
                }
            } else if (tag == TAG_BYE) {
                /* Guest closed the tube. Hold the last picture unless --once. */
                if (opt_once) {
                    quit = 1;
                } else {
                    break;
                }
            }
        }

        if (!opt_text && opt_draw && !quit) {
            int k = pump_keys(fd);
            if (k > 0) {
                quit = 1;
            }
            if (k < 0) {
                break;
            }
        }
    }

    if (!opt_once && !opt_text && opt_draw && got > 0 && !quit) {
        fprintf(stderr, "s32-crt: guest halted — press q\n");
        while (pump_keys(-1) == 0) {
            usleep(50 * 1000);
        }
    }

    {
        uint32_t bye[2] = { 4, TAG_BYE };
        sendn(fd, bye, 8);
    }
    close(fd);
    free(payload);
    return got > 0 ? 0 : 1;
}
