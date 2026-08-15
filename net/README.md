# SLOW-32 networking (v1)

IPv4 TCP only. No DNS, no UDP, no Unix sockets. The guest talks
`socket`/`bind`/`listen`/`accept`/`connect`/`send`/`recv`; the host
opens a real host socket. Addresses are numeric (`127.0.0.1`, or
`htonl(INADDR_LOOPBACK)`).

```bash
# rebuild libc + emulators first
make -C ../runtime libc_mmio.s32a
make -C ../tools/emulator

./build.sh
./run-tests.sh
```

`run-tests.sh` starts `echo_server.s32x` in one emulator, then
`echo_client.s32x` in another. They share a loopback port, not a
filesystem (except `echo.port`, which is just rendezvous).

See `docs/plans/hose.md` for why this is a hose and not a cluster.
