# SLOW-32 Kermit

The civilized file protocol. More polite than ZMODEM, and not faster:
every packet is printable, ctl-quoted with `#`, carries a type-1
checksum, and is individually acknowledged. SEQ mod 64, stop-and-wait.

This is item #5 on the 1987-desk plan, and it exists because the `net`
hose gave the machine a second endpoint. The two guests below share a
TCP connection and **no directory** — which is the only situation where
a file protocol earns its keep.

```bash
./build.sh

# In one directory:
mkdir inbox && cd inbox
../../tools/emulator/slow32-fast ../kermit.s32x -r     # writes kermit.port

# In another:
../tools/emulator/slow32-fast kermit.s32x -s $(cat inbox/kermit.port) notes.txt data.bin

./tests/run-tests.sh
```

`-s [-h A.B.C.D] PORT FILE...` sends one or more files (binary-safe;
names travel as basenames and the receiver refuses paths and dotfiles).
`-r` listens on `127.0.0.1:0`, writes the port to `kermit.port`, and
receives one session into the cwd.

Protocol subset: S/F/D/Z/B/Y/N/E packets, MAXL 94, ctl-quoting.
No 8th-bit quoting (the hose is 8-bit clean), no repeat counts, no
sliding windows — long packets and windows are what made real Kermit
fast in 1988, and they are the natural next fight if anyone ever
ships something big over this wire.
