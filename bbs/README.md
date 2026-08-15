# SLOW-32 BBS

A dBase-backed board. The user file is a real `.DBF`. The modem is
`127.0.0.1`.

v0.3 is logon plus a message base. Post is multi-line (blank line
ends). Both files are `.DBF`. No doors, no ZMODEM.

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

[L]ist  [R]ead  [P]ost  [W]ho  [G]oodbye
```
