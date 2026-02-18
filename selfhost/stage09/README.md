# Stage 09: Subset-C Fixed-Point Proof

## What This Does

Proves that cc-min (the subset-C compiler from Stage 08) is self-hosting by
demonstrating a fixed point in the bootstrap chain:

1. **Gen1** (cc-min compiled by cc.fth) compiles cc-min source → Gen2 assembly
2. **Gen2** (cc-min compiled by Gen1) compiles cc-min source → Gen3 assembly
3. **Fixed-point check**: gen2.s == gen3.s (byte-identical)

If Gen2 and Gen3 produce identical assembly, the compiler is self-consistent:
it faithfully reproduces itself.

## What It Needs

- All tools from Stages 01-08 (bootstrapped automatically)
- Stage 08 cc-min source (pass1, pass2, pass3, main)

## What It Produces

- Gen2 cc-min executable (self-compiled compiler)
- Proof that gen2.s == gen3.s (fixed point)
- Smoke test verification (Gen2 compiles and runs a trivial program)

## Running

```bash
bash selfhost/stage09/run-spike.sh
bash selfhost/stage09/run-spike.sh --keep-artifacts   # preserve temp files
bash selfhost/stage09/run-spike.sh --emu /path/to/emu  # use specific emulator
```

## Status

Complete. Self-compilation fixed point proven.
