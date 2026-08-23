/* s32-crt-mac — the good glass. A native macOS viewer for the SLOW-32
 * tube: phosphor persistence and additive beam crossings for vec,
 * nearest-neighbor with scanlines for fb/ppu, and real NSEvent
 * make/break keys (no terminal synthesis).
 *
 *   s32-crt-mac [--port N | --port-file PATH] [--green] [--no-scanlines]
 *
 * Attaches to tube.port in the cwd by default, holds the last picture
 * when the guest halts, and quietly reattaches when a new one appears.
 * The emulator never renders; this process is where the glow lives.
 */

#import <Cocoa/Cocoa.h>

#include <arpa/inet.h>
#include <errno.h>
#include <netinet/in.h>
#include <stdint.h>
#include <unistd.h>

#define TAG_HELO 0x4F4C4548u
#define TAG_VSEG 0x47455356u
#define TAG_VFRM 0x4D524656u
#define TAG_KEYE 0x4559454Bu
#define TAG_BYE  0x00455942u

#define PHOS_SIZE 1024          /* vec accumulation bitmap, square */

typedef struct {
    uint16_t x0, y0, x1, y1;
    uint8_t r, g, b, i;
} seg_t;

static int opt_port = -1;
static const char *opt_port_file = "tube.port";
static int opt_green = 0;
static int opt_scanlines = 1;

/* ---- socket ------------------------------------------------------- */

static int g_fd = -1;           /* guarded by g_send_q for writes */
static dispatch_queue_t g_send_q;

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
    int fd = socket(AF_INET, SOCK_STREAM, 0);
    struct sockaddr_in addr;
    if (fd < 0) {
        return -1;
    }
    {
        int one = 1;
        setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &one, sizeof(one));
    }
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

static void send_key(uint16_t code, uint8_t down) {
    dispatch_async(g_send_q, ^{
        uint8_t pkt[12];
        uint32_t len = 8, tag = TAG_KEYE;
        ssize_t off = 0;
        if (g_fd < 0) {
            return;
        }
        memcpy(pkt, &len, 4);
        memcpy(pkt + 4, &tag, 4);
        pkt[8] = (uint8_t)(code & 0xFF);
        pkt[9] = (uint8_t)(code >> 8);
        pkt[10] = down;
        pkt[11] = 0;
        while (off < 12) {
            ssize_t w = send(g_fd, pkt + off, (size_t)(12 - off), 0);
            if (w <= 0) {
                return;
            }
            off += w;
        }
    });
}

/* ---- the glass ---------------------------------------------------- */

@interface GlassView : NSView
@property (atomic) uint32_t mode;       /* 0 none, 1 vec, 2 fb, 3 ppu */
@property (atomic) uint32_t guestW, guestH;
@property (atomic) BOOL attached;
- (void)decayTick;
- (void)takeSegs:(NSData *)segs;
- (void)takeFrame:(NSData *)rgba width:(uint32_t)w height:(uint32_t)h;
@end

@implementation GlassView {
    CGContextRef phos;          /* vec accumulation, PHOS_SIZE^2 RGBA */
    CGImageRef lastFrame;       /* fb/ppu raster */
    NSData *pendingSegs;        /* latest unconsumed VSEG payload */
}

- (instancetype)initWithFrame:(NSRect)r {
    self = [super initWithFrame:r];
    if (self) {
        CGColorSpaceRef cs = CGColorSpaceCreateDeviceRGB();
        phos = CGBitmapContextCreate(NULL, PHOS_SIZE, PHOS_SIZE, 8,
                                     PHOS_SIZE * 4, cs,
                                     (CGBitmapInfo)kCGImageAlphaPremultipliedLast);
        CGColorSpaceRelease(cs);
        CGContextSetRGBFillColor(phos, 0, 0, 0, 1);
        CGContextFillRect(phos, CGRectMake(0, 0, PHOS_SIZE, PHOS_SIZE));
        [self setWantsLayer:YES];
    }
    return self;
}

- (BOOL)acceptsFirstResponder { return YES; }
- (BOOL)isOpaque { return YES; }

- (void)takeSegs:(NSData *)segs {
    pendingSegs = segs;
    [self setNeedsDisplay:YES];
}

- (void)takeFrame:(NSData *)rgba width:(uint32_t)w height:(uint32_t)h {
    CGDataProviderRef prov = CGDataProviderCreateWithCFData(
        (__bridge CFDataRef)rgba);
    CGColorSpaceRef cs = CGColorSpaceCreateDeviceRGB();
    CGImageRef img = CGImageCreate(w, h, 8, 32, w * 4, cs,
                                   (CGBitmapInfo)kCGImageAlphaNoneSkipLast,
                                   prov, NULL, false,
                                   kCGRenderingIntentDefault);
    CGColorSpaceRelease(cs);
    CGDataProviderRelease(prov);
    if (img) {
        CGImageRef old = lastFrame;
        lastFrame = img;
        if (old) {
            CGImageRelease(old);
        }
    }
    [self setNeedsDisplay:YES];
}

/* Fade the phosphor toward black; called at 60Hz whether or not the
 * guest is presenting, so trails keep decaying on a halted tube. */
- (void)decayTick {
    if (self.mode == 1) {
        CGContextSetBlendMode(phos, kCGBlendModeNormal);
        CGContextSetRGBFillColor(phos, 0, 0, 0, 0.16);
        CGContextFillRect(phos, CGRectMake(0, 0, PHOS_SIZE, PHOS_SIZE));
        [self setNeedsDisplay:YES];
    }
}

- (void)strokeSegs:(const seg_t *)s count:(uint32_t)n
             width:(CGFloat)wd alpha:(CGFloat)al {
    uint32_t i;
    CGFloat sc = (CGFloat)PHOS_SIZE / 4096.0;
    CGContextSetLineWidth(phos, wd);
    CGContextSetLineCap(phos, kCGLineCapRound);
    for (i = 0; i < n; i++) {
        CGFloat inten = (CGFloat)s[i].i / 255.0;
        CGFloat r = (CGFloat)s[i].r / 255.0;
        CGFloat g = (CGFloat)s[i].g / 255.0;
        CGFloat b = (CGFloat)s[i].b / 255.0;
        if (opt_green) {
            r = 0.25 * r;
            b = 0.30 * b;
            g = g < 0.4 ? 0.4 : g;      /* P31 leans everything green */
        }
        CGContextSetRGBStrokeColor(phos, r, g, b, al * inten);
        CGContextBeginPath(phos);
        CGContextMoveToPoint(phos, s[i].x0 * sc, s[i].y0 * sc);
        if (s[i].x0 == s[i].x1 && s[i].y0 == s[i].y1) {
            CGContextAddLineToPoint(phos, s[i].x1 * sc + 0.6,
                                    s[i].y1 * sc);
        } else {
            CGContextAddLineToPoint(phos, s[i].x1 * sc, s[i].y1 * sc);
        }
        CGContextStrokePath(phos);
    }
}

- (void)burnSegs {
    NSData *d = pendingSegs;
    const seg_t *s;
    uint32_t n;
    if (!d) {
        return;
    }
    pendingSegs = nil;
    s = (const seg_t *)d.bytes;
    n = (uint32_t)(d.length / sizeof(seg_t));
    /* Additive: crossings brighten, like ink and unlike toner. */
    CGContextSetBlendMode(phos, kCGBlendModePlusLighter);
    [self strokeSegs:s count:n width:7.0 alpha:0.10];   /* halo */
    [self strokeSegs:s count:n width:3.0 alpha:0.30];   /* bloom */
    [self strokeSegs:s count:n width:1.3 alpha:0.95];   /* beam  */
}

- (NSRect)fitRect:(CGFloat)aw by:(CGFloat)ah {
    NSRect b = self.bounds;
    CGFloat sc = MIN(b.size.width / aw, b.size.height / ah);
    CGFloat w = aw * sc, h = ah * sc;
    return NSMakeRect((b.size.width - w) / 2,
                      (b.size.height - h) / 2, w, h);
}

- (void)drawRect:(NSRect)dirty {
    CGContextRef ctx = [[NSGraphicsContext currentContext] CGContext];
    (void)dirty;
    CGContextSetRGBFillColor(ctx, 0, 0, 0, 1);
    CGContextFillRect(ctx, self.bounds);

    if (self.mode == 1) {
        CGImageRef img;
        [self burnSegs];
        img = CGBitmapContextCreateImage(phos);
        if (img) {
            CGContextSetInterpolationQuality(ctx, kCGInterpolationMedium);
            CGContextDrawImage(ctx, [self fitRect:PHOS_SIZE by:PHOS_SIZE],
                               img);
            CGImageRelease(img);
        }
    } else if (lastFrame) {
        NSRect fit = [self fitRect:(CGFloat)CGImageGetWidth(lastFrame)
                                by:(CGFloat)CGImageGetHeight(lastFrame)];
        CGContextSetInterpolationQuality(ctx, kCGInterpolationNone);
        CGContextDrawImage(ctx, fit, lastFrame);
        if (opt_scanlines) {
            CGFloat rows = (CGFloat)CGImageGetHeight(lastFrame);
            CGFloat rh = fit.size.height / rows;
            if (rh >= 2.0) {
                CGFloat y;
                CGContextSetRGBFillColor(ctx, 0, 0, 0, 0.22);
                for (y = fit.origin.y; y < fit.origin.y + fit.size.height;
                     y += rh) {
                    CGContextFillRect(ctx, CGRectMake(fit.origin.x, y,
                                                      fit.size.width,
                                                      rh * 0.35));
                }
            }
        }
    }
}

/* ---- keys: the real thing ----------------------------------------- */

static uint16_t map_key(NSEvent *ev) {
    NSString *ch;
    switch (ev.keyCode) {
        case 126: return 0x100;     /* up */
        case 125: return 0x101;     /* down */
        case 123: return 0x102;     /* left */
        case 124: return 0x103;     /* right */
        case 53:  return 27;        /* esc */
        case 36:  return 13;        /* return */
        case 51:  return 8;         /* delete */
        default: break;
    }
    ch = ev.charactersIgnoringModifiers.lowercaseString;
    if (ch.length == 1) {
        unichar c = [ch characterAtIndex:0];
        if (c >= 32 && c < 127) {
            return (uint16_t)c;
        }
    }
    return 0;
}

- (void)keyDown:(NSEvent *)ev {
    uint16_t code;
    if (ev.modifierFlags & NSEventModifierFlagCommand) {
        [super keyDown:ev];
        return;
    }
    if (ev.isARepeat) {
        return;                     /* the guest holds its own keys */
    }
    code = map_key(ev);
    if (code) {
        send_key(code, 1);
    }
}

- (void)keyUp:(NSEvent *)ev {
    uint16_t code = map_key(ev);
    if (code) {
        send_key(code, 0);
    }
}

- (void)flagsChanged:(NSEvent *)ev {
    static NSEventModifierFlags prev = 0;
    NSEventModifierFlags now = ev.modifierFlags;
    struct {
        NSEventModifierFlags flag;
        uint16_t code;
    } mods[] = {
        { NSEventModifierFlagShift, 0x104 },
        { NSEventModifierFlagControl, 0x106 },
        { NSEventModifierFlagOption, 0x107 },
    };
    for (size_t i = 0; i < sizeof(mods) / sizeof(mods[0]); i++) {
        BOOL was = (prev & mods[i].flag) != 0;
        BOOL is = (now & mods[i].flag) != 0;
        if (was != is) {
            send_key(mods[i].code, is ? 1 : 0);
        }
    }
    prev = now;
}

- (void)dealloc {
    if (phos) {
        CGContextRelease(phos);
    }
    if (lastFrame) {
        CGImageRelease(lastFrame);
    }
}
@end

/* ---- app ---------------------------------------------------------- */

@interface Glass : NSObject <NSApplicationDelegate>
@property (strong) NSWindow *window;
@property (strong) GlassView *view;
@end

@implementation Glass

- (void)applicationDidFinishLaunching:(NSNotification *)n {
    NSRect r = NSMakeRect(0, 0, 800, 800);
    (void)n;
    self.window = [[NSWindow alloc]
        initWithContentRect:r
                  styleMask:NSWindowStyleMaskTitled |
                            NSWindowStyleMaskClosable |
                            NSWindowStyleMaskMiniaturizable |
                            NSWindowStyleMaskResizable
                    backing:NSBackingStoreBuffered
                      defer:NO];
    if (opt_port > 0) {
        self.window.title = [NSString stringWithFormat:
            @"s32-crt — waiting for port %d", opt_port];
    } else {
        char abs[1024];
        if (opt_port_file[0] != '/' && getcwd(abs, sizeof(abs) - 64)) {
            strlcat(abs, "/", sizeof(abs));
            strlcat(abs, opt_port_file, sizeof(abs));
        } else {
            strlcpy(abs, opt_port_file, sizeof(abs));
        }
        self.window.title = [NSString stringWithFormat:
            @"s32-crt — waiting for %s (start the guest in that directory)",
            abs];
    }
    self.view = [[GlassView alloc] initWithFrame:r];
    self.window.contentView = self.view;
    [self.window makeFirstResponder:self.view];
    [self.window center];
    [self.window makeKeyAndOrderFront:nil];
    [NSApp activateIgnoringOtherApps:YES];

    [NSTimer scheduledTimerWithTimeInterval:1.0 / 60.0
                                    repeats:YES
                                      block:^(NSTimer *t) {
        (void)t;
        [self.view decayTick];
    }];

    [self startReader];
}

- (BOOL)applicationShouldTerminateAfterLastWindowClosed:(NSApplication *)a {
    (void)a;
    return YES;
}

- (void)setTitle:(NSString *)t {
    dispatch_async(dispatch_get_main_queue(), ^{
        self.window.title = t;
    });
}

/* Reader thread: attach, stream frames, and on loss hold the picture
 * and quietly try again — leave the glass up, relaunch guests at will. */
- (void)startReader {
    dispatch_async(dispatch_get_global_queue(QOS_CLASS_USER_INTERACTIVE, 0), ^{
        for (;;) {
            int fd = -1;
            if (opt_port > 0) {
                fd = connect_port(opt_port);
            } else {
                int p = read_port_file(opt_port_file);
                if (p > 0) {
                    fd = connect_port(p);
                }
            }
            if (fd < 0) {
                usleep(300 * 1000);
                continue;
            }
            dispatch_sync(g_send_q, ^{ g_fd = fd; });
            self.view.attached = YES;

            for (;;) {
                uint32_t len = 0, tag = 0;
                NSMutableData *pay;
                if (recvn(fd, &len, 4) < 0 || recvn(fd, &tag, 4) < 0) {
                    break;
                }
                if (len < 4 || len > 16u * 1024u * 1024u) {
                    break;
                }
                pay = [NSMutableData dataWithLength:len - 4];
                if (len > 4 && recvn(fd, pay.mutableBytes, len - 4) < 0) {
                    break;
                }
                if (tag == TAG_HELO && pay.length >= 16) {
                    const uint32_t *h = (const uint32_t *)pay.bytes;
                    uint32_t mode = h[1], w = h[2], hh = h[3];
                    self.view.mode = mode;
                    self.view.guestW = w;
                    self.view.guestH = hh;
                    [self setTitle:[NSString stringWithFormat:
                        @"s32-crt — %@ %ux%u",
                        mode == 1 ? @"vec" : mode == 2 ? @"fb" : @"ppu",
                        w, hh]];
                } else if (tag == TAG_VSEG && pay.length >= 8) {
                    const uint8_t *p = pay.bytes;
                    uint32_t count;
                    memcpy(&count, p + 4, 4);
                    if (8u + count * 12u <= pay.length) {
                        NSMutableData *segs =
                            [NSMutableData dataWithLength:count * sizeof(seg_t)];
                        seg_t *s = segs.mutableBytes;
                        for (uint32_t i = 0; i < count; i++) {
                            const uint8_t *e = p + 8 + i * 12;
                            memcpy(&s[i], e, 8);
                            s[i].r = e[8];
                            s[i].g = e[9];
                            s[i].b = e[10];
                            s[i].i = e[11];
                        }
                        dispatch_async(dispatch_get_main_queue(), ^{
                            [self.view takeSegs:segs];
                        });
                    }
                } else if (tag == TAG_VFRM && pay.length >= 12) {
                    const uint8_t *p = pay.bytes;
                    uint32_t w, hh;
                    memcpy(&w, p + 4, 4);
                    memcpy(&hh, p + 8, 4);
                    if (w && hh && 12u + w * hh * 4u <= pay.length) {
                        NSData *rgba =
                            [pay subdataWithRange:NSMakeRange(12, w * hh * 4u)];
                        dispatch_async(dispatch_get_main_queue(), ^{
                            [self.view takeFrame:rgba width:w height:hh];
                        });
                    }
                } else if (tag == TAG_BYE) {
                    break;
                }
            }

            dispatch_sync(g_send_q, ^{ g_fd = -1; });
            close(fd);
            self.view.attached = NO;
            [self setTitle:[NSString stringWithFormat:
                @"s32-crt — guest halted (watching %s)",
                opt_port > 0 ? "the port" : opt_port_file]];
            usleep(300 * 1000);
        }
    });
}
@end

int main(int argc, char **argv) {
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--port") == 0 && i + 1 < argc) {
            opt_port = atoi(argv[++i]);
        } else if (strcmp(argv[i], "--port-file") == 0 && i + 1 < argc) {
            opt_port_file = argv[++i];
        } else if (strcmp(argv[i], "--green") == 0) {
            opt_green = 1;
        } else if (strcmp(argv[i], "--no-scanlines") == 0) {
            opt_scanlines = 0;
        } else {
            fprintf(stderr,
                    "usage: s32-crt-mac [--port N | --port-file PATH] "
                    "[--green] [--no-scanlines]\n");
            return 2;
        }
    }
    g_send_q = dispatch_queue_create("s32.crt.send", DISPATCH_QUEUE_SERIAL);

    @autoreleasepool {
        NSApplication *app = [NSApplication sharedApplication];
        Glass *glass = [[Glass alloc] init];
        [app setActivationPolicy:NSApplicationActivationPolicyRegular];
        app.delegate = glass;
        [app run];
    }
    return 0;
}
