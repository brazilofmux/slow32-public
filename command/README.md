# COMMAND.COM

A DOS-shaped shell for SLOW-32. The prompt is theater. `DIR` is the product.

v0.2 is file commands plus launching a `.s32x` (host `exec`, waits).
No `.BAT` yet.

```bash
./build.sh
../tools/emulator/slow32-fast command.s32x
./tests/run-tests.sh
```

| Command | Aliases |
|---|---|
| `DIR [path]` | |
| `TYPE file` | |
| `COPY src dst` | |
| `REN old new` | `RENAME` |
| `DEL file` | `ERASE` |
| `CD [path]` | `CHDIR` |
| `MD path` | `MKDIR` |
| `RD path` | `RMDIR` |
| `ECHO text` | |
| `CLS` | |
| `VER` | |
| `HELP` | `?` |
| `EXIT` | `QUIT` |

Backslashes in paths are accepted and turned into `/` for the host.
Commands are case-insensitive. Unknown input gets the period-correct
`Bad command or file name`.
