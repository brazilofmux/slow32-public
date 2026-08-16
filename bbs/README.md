# SLOW-32 BBS

A dBase-backed board. The user file is a real `.DBF`. The modem is
`127.0.0.1`.

v0.5 is logon, DBF mail, doors, and ZMODEM download. `[T]ransfer`
sends a file from `files/` (or the cwd) with a CRC-16 ZMODEM
subset. `[F]iles` lists them.

`[D]oor` runs any `.s32x` dropped in `doors/` with the caller's
socket as its stdio; `?` at the prompt lists what's installed.
Full-screen term programs work through the door — copy
`../rogue/rogue.s32x` into `doors/` and callers get a door game
whose scores land in the board's `rogue.scr`.

```bash
./tests/make_users.py USERS.DBF   # or CREATE in dBase
./build.sh
../tools/emulator/slow32-fast bbs.s32x
# then: nc 127.0.0.1 $(cat bbs.port)
./tests/run-tests.sh
```

`USERS.DBF` needs `NAME` and `PASS`. `MSG.DBF` is created on first
run (`FROM`, `TO`, `SUBJ`, `TEXT`). dBase can `USE` either file.

```
SLOW-32 BBS  v0.1

Name: alice
Password: secret

Welcome, alice.

[L]ist  [R]ead  [P]ost  [D]oor  [F]iles  [T]ransfer  [W]ho  [G]oodbye
```
