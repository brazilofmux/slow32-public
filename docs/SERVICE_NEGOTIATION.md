# Service Negotiation Protocol

## Motivation

SLOW-32's MMIO layer currently hard-codes a fixed set of host services (stdio, files,
time, etc.) at fixed opcode numbers. This works, but doesn't scale:

- **New services** (terminal control, crypto, compression) require editing the opcode
  map, rebuilding emulators, and coordinating guest/host updates.
- **Sandboxing** is all-or-nothing. There's no way to say "this guest gets file I/O
  but not networking."
- **Portability** suffers. A binary that assumes networking is available crashes on
  a host that doesn't provide it, rather than degrading gracefully.

The service negotiation protocol solves these by making services **discoverable,
optional, and policy-controlled**.

## Design Principles

1. **MMIO is the syscall layer.** Code that makes sense as guest code (libc, math,
   string handling) gets compiled to SLOW-32. Code that needs host capabilities or
   would be impractical to port (crypto, terminal control, compression, networking)
   is exposed as MMIO services. The guest calls the API; the emulator does the work.

2. **One fixed thing: the negotiation channel.** Everything else is dynamic. The
   guest discovers services at runtime, not compile time.

3. **Capability-based.** The guest requests services. The host grants or denies
   based on policy. Denial is not an error — it's expected. Guests degrade gracefully.

4. **Stateful sessions with deterministic cleanup.** Each granted service creates a
   session with state (open files, terminal mode, buffers). When the guest exits —
   normally or abnormally — the host walks all sessions and calls cleanup. Terminal
   restored, files closed, no leaks.

5. **Guest-driven addressing.** The guest tells the host where to map each service
   in the MMIO region. The guest manages its own address space. No hard-coded offsets.

## Architecture

```
+------------------+
|   Guest Binary   |
|                  |
|  term_init() ----+---> mmio_request("term") ---> negotiation channel
|  fs_open()   ----+---> fs service region     ---> granted service
|  net_send()  ----+---> mmio_request("net")   ---> DENIED (sandbox)
|                  |
+------------------+
        |
        | MMIO writes/reads
        v
+------------------+
|   Host Emulator  |
|                  |
|  Policy engine   |  --allow term,fs --deny net
|  Service registry|  term.1, fs.1, net.1, crypto.1, ...
|  Session manager |  per-guest state, cleanup on exit
|                  |
+------------------+
```

## Negotiation Channel

Negotiation is **not** a separate MMIO region. It's new opcodes (0xF0-0xFF) on the
existing request/response ring — the same ring used for `OPEN`, `READ`, `GETTIME`,
and every other host operation. No new infrastructure needed.

### SVC_REQUEST Flow

```
Guest                              Host
  |                                  |
  |  Request ring descriptor:        |
  |  word0 = 0xF0 (SVC_REQUEST)     |
  |  word1 = 4    (name_len)        |
  |  word2 = 0    (data_offset)     |
  |  word3 = 0x80 (desired base)    |
  |  data = "term"                   |
  |  YIELD                           |
  |  -------------------------------->
  |                                  |  Check policy: "term" allowed?
  |                                  |  Check conflicts: 0x80 range free?
  |                                  |  Create session, initialize state
  |  <--------------------------------
  |  Response ring descriptor:       |
  |  word0 = 0xF0 (SVC_REQUEST)     |
  |  word1 = 8    (opcode_count)    |
  |  word2 = 1    (version)         |
  |  word3 = 0x00 (SVC_OK)         |
  |                                  |
  |  (opcodes 0x80-0x87 now route   |
  |   to the term service handler)  |
```

After negotiation, the guest uses term by submitting ring descriptors with
opcodes 0x80-0x87 — same mechanism as everything else.

### Response Codes

| Code | Name              | Meaning |
|------|-------------------|---------|
| 0x00 | `SVC_OK`          | Service granted at requested opcode base |
| 0x01 | `SVC_DENIED`      | Policy forbids this service |
| 0x02 | `SVC_UNKNOWN`     | Service name not recognized |
| 0x03 | `SVC_CONFLICT`    | Requested opcode range overlaps existing service |
| 0x04 | `SVC_LIMIT`       | Too many active services |
| 0x05 | `SVC_VERSION_ERR` | Requested version not supported |

## Session Lifecycle

```
  SVC_REQUEST("term")
        |
        v
  +-- SESSION CREATED --+
  |  service: "term"    |
  |  guest_id: 47       |
  |  state: { raw=false,|
  |    saved_termios,    |
  |    cursor_pos, ... } |
  |  cleanup: term_fini  |
  +---------------------+
        |
        | (guest uses service via MMIO)
        |
        v
  Guest exits / crashes / SVC_RELEASE
        |
        v
  cleanup() called:
    - restore terminal to cooked mode
    - flush output buffers
    - free state
        |
        v
  SESSION DESTROYED
```

Each service implementation provides:

```c
typedef struct {
    const char *name;
    uint32_t version;
    void *(*create)(int guest_id);           // allocate session state
    void  (*destroy)(void *state);           // cleanup on exit
    void  (*handle)(void *state,             // process an opcode
                    uint32_t opcode,
                    uint32_t *descriptor,
                    uint8_t *data_buffer);
} service_def_t;
```

## Host Policy

### Command Line

```bash
# Explicit allow/deny
slow32-fast program.s32x --allow term,fs --deny net,crypto

# Default-deny (safest)
slow32-fast program.s32x --sandbox --allow fs

# Default-allow (development)
slow32-fast program.s32x --sandbox-off
```

### Policy File

```ini
# /etc/slow32/sandbox.conf or ~/.slow32/sandbox.conf
[default]
policy = deny

[services]
fs = allow
term = allow
time = allow
net = deny
crypto = deny
exec = deny
```

### Per-Binary Policy (Future)

The `.s32x` executable header could include a service manifest — "this binary
needs fs and term" — so the host can pre-check before execution and give a clear
error instead of a runtime denial.

## Planned Services

| Service  | What it wraps                | Priority |
|----------|------------------------------|----------|
| `fs`     | File I/O (migrate existing)  | -        |
| `term`   | Terminal control             | High     |
| `time`   | Clock/timers (migrate)       | -        |
| `env`    | Environment vars (migrate)   | -        |
| `net`    | Sockets                     | Low      |
| `crypto` | Hashing, encryption          | Low      |
| `zlib`   | Compression                  | Low      |
| `exec`   | Subprocess spawning          | Low      |

Existing fixed services (fs, time, env) can migrate incrementally. They keep
working at their current opcodes. New negotiated versions run alongside until
the old ones are retired.

## Terminal Service (`term`)

The first service to implement via negotiation. Unlocks dBase III and INKEY$.

### Opcodes (offsets from negotiated base)

| Offset | Name              | Args                     | Description |
|--------|-------------------|--------------------------|-------------|
| +0     | `TERM_SET_MODE`   | mode (0=cooked, 1=raw)   | Switch input mode |
| +1     | `TERM_GET_SIZE`   | -                        | Returns rows, cols |
| +2     | `TERM_MOVE_CURSOR`| row, col                 | Position cursor |
| +3     | `TERM_CLEAR`      | region (0=all, 1=eol, 2=eos) | Clear screen/line |
| +4     | `TERM_SET_ATTR`   | attr (normal/bold/reverse/underline) | Set text attribute |
| +5     | `TERM_READ_KEY`   | -                        | Blocking key read |
| +6     | `TERM_KEY_AVAIL`  | -                        | Non-blocking poll (INKEY$) |
| +7     | `TERM_SET_COLOR`  | fg, bg                   | Set text color |

If the guest negotiated `term` at base 0x80, then `TERM_SET_MODE` is opcode
0x80, `TERM_READ_KEY` is 0x85, etc.

### Host Implementation

The emulator implements these using `tcsetattr()` for mode switching and ANSI
escape sequences for cursor/screen control. No curses dependency required on the
host, though one could be used as an implementation detail.

### Guest Library (`term.h`)

```c
int  term_init(void);              // negotiate service, returns 0 or -1
void term_cleanup(void);           // release service
void term_set_raw(int raw);        // 0=cooked, 1=raw
void term_get_size(int *rows, int *cols);
void term_gotoxy(int row, int col);
void term_clear(void);
void term_clear_eol(void);
void term_bold(int on);
void term_reverse(int on);
int  term_getkey(void);            // blocking
int  term_kbhit(void);             // non-blocking (INKEY$)
```

### Graceful Degradation

```c
if (term_init() < 0) {
    // No terminal service — fall back to line mode
    // dBase: disable BROWSE, use LIST instead
    // BASIC: INKEY$ returns -1 (no key available, ever)
}
```

## Negotiation Format (Resolved)

Negotiation uses the **existing ring mechanism**. `SVC_REQUEST` is an opcode on the
request ring, just like `OPEN` or `GETTIME`. No new MMIO regions, no new
synchronization mechanism. What gets negotiated is an **opcode range** — the guest
picks its base opcode, the host validates and confirms.

```
Guest writes to request ring:
  word0 = SVC_REQUEST (0xF0)
  word1 = name_len
  word2 = data_offset    (service name string in data buffer)
  word3 = base_opcode    (guest's desired opcode base, e.g. 0x80)

Host writes to response ring:
  word0 = SVC_REQUEST
  word1 = opcode_count   (e.g. 8 — so term is 0x80-0x87)
  word2 = version        (e.g. 1)
  word3 = status         (SVC_OK, SVC_DENIED, SVC_CONFLICT, ...)
```

If there's a conflict, host returns `SVC_CONFLICT`, guest picks another base.
Guest manages its own opcode namespace, host validates.

The negotiation opcodes themselves live at **0xF0-0xFF** — top of the opcode map,
clearly separated from both legacy services and negotiated service ranges.

| Opcode | Name            | Description |
|--------|-----------------|-------------|
| 0xF0   | `SVC_REQUEST`   | Request and map a service at a chosen opcode base |
| 0xF1   | `SVC_RELEASE`   | Release a previously mapped service |
| 0xF2   | `SVC_QUERY`     | Check if a service is available (without mapping) |
| 0xF3   | `SVC_LIST`      | List available services |
| 0xF4   | `SVC_VERSION`   | Query negotiation protocol version |

## Versioning (Resolved)

Single integer. Guest requests `"term"` and optionally a minimum version. Host
returns the granted version. If the host has term v2 and guest wants v1+, it gets
v2. If the guest requires v2 but host only has v1, `SVC_VERSION_ERR`.

Adding new opcodes at the end of a service doesn't bump the version — old guests
don't use new opcodes, so they don't break.

## Opcode Count (Resolved)

Service dictates. The service definition includes its opcode count. The host returns
`opcode_count` in the SVC_REQUEST response so the guest knows the range. Guest
picked the base, host confirmed the count. No region sizing needed.

## Legacy Migration

There is no installed base. Fixed opcodes that bypass negotiation are services that
can't be denied by policy — every one is an attack surface that the sandbox can't
control. Migration is therefore a security requirement, not a convenience.

**Timeline:**

1. **Now**: Fixed opcodes are all that exist. They work.
2. **Negotiation lands**: New services (`term`) use negotiation. Old services
   still work at their fixed opcodes.
3. **Policy gate**: Fixed opcodes get routed through the same policy engine.
   `--deny fs` blocks fixed-opcode `OPEN`/`READ`/`WRITE` too. This is the
   critical step — the sandbox becomes real.
4. **Negotiated alternatives**: `fs`, `time`, `env` become requestable through
   negotiation. Internally, both paths converge to the same service implementation.
5. **Deprecate fixed opcodes**: Guest runtime libraries switch to negotiated
   services. Fixed opcodes remain as a thin redirect but are no longer the
   primary path. Can be removed when no binaries depend on them.

Step 3 can and should ship the same day as step 2.

## Relationship to Other Docs

- `docs/host-interface-design.md` — Low-level MMIO architecture, ring buffers,
  queue design, multi-instance model
- `docs/mmio/opcode-map.md` — Current fixed opcode assignments
- `docs/mmio/ring-design.md` — Ring buffer protocol details
- `docs/CALLING_CONVENTION.md` — Guest-side ABI (unrelated but often referenced)

## Design Decisions (Resolved)

| Question | Decision |
|----------|----------|
| Negotiation format | Existing ring mechanism, SVC_* opcodes at 0xF0-0xFF |
| Service addressing | Opcode ranges on the shared ring, guest picks base |
| Versioning | Single integer per service |
| Opcode count | Service defines it, returned in SVC_REQUEST response |
| Legacy migration | Policy gate first (security), full migration over time |
