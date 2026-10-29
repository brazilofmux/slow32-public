# Selfhost SHA256 Checksums

> Stages 05-08 regenerated 2026-10-29 (tree at b8942204), after the in-place
> repairs since 2026-08-27 (stage05/06 `hir_opt.h`, stage07 selfhost
> ISSUES-76 and ISSUES-92, stage08's C11 work).  Stages 01-08 rebuilt to
> these sums under `slow32-dbt`, `slow32-fast` and `slow32` on an Apple
> M5 Max.  stage08 is the moving head: its sums change with its sources,
> so refresh them (run `verify-emu-sums.sh`, take the new sums) when
> stage08 changes.

All platforms and emulators produce bit-identical artifacts.

## Verified Platforms & Emulators

| Emulator | Platform |
|----------|----------|
| s32-emu | Raspberry Pi 4 |
| s32-emu | AMD Ryzen 5 3600 6-Core Processor |
| slow32-fast | Intel(R) Celeron(R) N4500 @ 1.10GHz |
| slow32-dbt (stage 4) | Intel(R) Celeron(R) N4500 @ 1.10GHz |
| slow32-dbt (stage 4) | Raspberry Pi 4 |
| slow32-dbt (stage 5) | Intel(R) Celeron(R) N4500 @ 1.10GHz |
| slow32-dbt (stage 5) | Lenovo ARM Cortex-A720/Cortex-X4 |
| slow32-dbt (stage 6) | Lenovo ARM Cortex-A720/Cortex-X4 |
| slow32-dbt (stage 7) | Lenovo ARM Cortex-A720/Cortex-X4 |
| slow32-fast (stage 7) | Intel(R) Xeon(R) Platinum 8259CL CPU @ 2.50GHz |
| slow32-dbt (stage 7) | Intel(R) Xeon(R) Platinum 8259CL CPU @ 2.50GHz |
| s32fast-hir (stage 7) | Intel(R) Xeon(R) Platinum 8259CL CPU @ 2.50GHz |
| slow32-dbt (stages 1–8) | Intel(R) Xeon(R) Platinum 8259CL CPU @ 2.50GHz |
| slow32 | Apple M5 Max (MacBook Pro) |
| slow32-fast | Apple M5 Max (MacBook Pro) |
| slow32-dbt | Apple M5 Max (MacBook Pro) |
| s32fast-hir-a64 (cc-a64 cross-compiled) | Lenovo ARM Cortex-A720/Cortex-X4 |

## Stage 01

- 081b1f594ba3ec86e38364fb913ab7bace50424171133c015257e9540da5b768  hello_minimal.s32x
- 7044d917c708f6bdf42dba144ed1a02b509cecaaa056875befc7bb4aa2a8ba61  test1.s32x

## Stage 02

- 065f0dc9a5e1dbc9036ef4de6ff081fb7eed26ecaf44018ef82fa2dc0f545b06  s32-as.s32x
- 2c47fa3a938c59c32307769ce9ec52472e9f8acbe1819046b3594f8cf79067c8  s32-ld.s32x
- 6c298e2c9113b3cb039a27917ea296a7d6ff03c26e0cee46b90075aaca9def2d  cc-min.s32x
- a6dd7618d8ee55e0c980bae209e47f6111bfddbab5c120163a058cc75940db2c  s32-ar.s32x

## Stage 03

- 02d85ec0b2a6617a8452377100efa2b9c426acbc122b40c057897f983a826e8e  s32-as.s32x
- 104d8da70aadcab2947d53f726f1f4bd29c7309d407800f132a87003fc9ff9d2  cc.s32x
- 4bf0114d1e0c8492902d5dad1c3cde86fb20b842b98ea976ba14e626d9f5acf8  s32-ar.s32x
- 5fcf46d419fc041861d36aa57b7727a9dea182535ba870f29a3a8f93e6c560d2  s32-ld.s32x

## Stage 04

- 988eb98efca23aba678fefabc264f7a366e3b99569c0687ff76d5c618ba3f434  cc.s32x
- a8cf3a4235abd1cbc0813da41f275085a6e0d19756ab76f5d7dd0414a9c7edc0  s32-ar.s32x
- b2c8d626283a9b7ae4823413b1f370df52468ae670af555395e5b330eaf835c9  s32-as.s32x
- eb2b532b8b50df34fc02d4772b01387709633756e09d3cf0c5edbcba17073fc5  s32-ld.s32x

## Stage 05

- d4015589b90ebacf001fb2f2dbd2e36b88c606bcdac605115b22f73c96e8faeb  s32-as.s32x
- 38e80138287bc650c827a5fe60baf92bda839b182eac27714b5a2ca7c464745b  s32-ld.s32x
- 159d5c7e25d2f42921ff0d9fcb915c5475fb02c2030827fa5589c702e9e92b9f  cc.s32x
- 5f1ff425e5145f1320037e15795efeaa4855da2a6ec26336702f8ca804c73f6c  slow32dump.s32x
- 8eb1944677a4aecd62915bbb9798e1f306fa08747439807fb009573031464c80  slow32dis.s32x
- b9277ad1321c76d47b8a7af2f793f39d422d19fcaa90272020b388ebb2e9101c  s32-ar.s32x

## Stage 06 (fixed-point proven)

- ab90fb01e7e45b884bbfb82121d6d929e9a900862533d94da67565cf7067014e  cc.s32x
- 4d4289a4be0981ac94747b6e1f2367d1299792e8b942608888f77465dcc5a71b  slow32dis.s32x
- a66aac753b78debc7c3c22b33a03c7412ea6308d4a8331bac0bfee22ff592349  s32-ar.s32x
- cab6553b0519090179ef327fc0a196d29b70378db07d42b0a797c346f90af6ce  s32-as.s32x
- a30c0d101153888f82926cc44ce1c85aa71dc9586343c59fb89f53e661c4c4d0  s32-ld.s32x
- e67b2f1f6519ce643bc81ff3ba202cc064dfeba74fbce6e0945e1937cb237f20  slow32dump.s32x

## Stage 07

- 40f93ea67db2ccc3f956fd186af21827427553fb5cfc1a37e908a73a544cdbfc  s32-as.s32x
- ee0926dd0909618c76fd9c88088663f307fe6802bbc4585b86f4c55605d0c512  cc.s32x
- da8f1c429a599b9bba1553f79140ef05000d4c902354e34f3c06fb072144e2d7  slow32dump.s32x
- 60a7333e3dbd142c509ebf877f23e7ae8435651cb3a35027c402a3f6e4e0b827  slow32dis.s32x
- afe81bfc1ca56575152c5bd40b8475295cca6cdc89b5748dc847470b8558c483  s32-ar.s32x
- ef348118cf1e5b525b81445c43a4d8d74781f24ab541a650e0de8454b1334a21  s32-ld.s32x

## Stage 08

- 30871cc5aed8da8180f0493a748669fed107417836d6ba64f4669f3557362c03  cc.s32x
- 6ca483fa8dab1ede122fe59f3a1ab7c13b96fb423be2fefff692107e9ae376cb  s32-ar.s32x
- 6781571cdbd0d8050e222abc6b0e78795929c4c03499ca8cf8005fba6c1d0feb  s32-as.s32x
- e859937912bad591af48361ea3b24897664b12fbb40213b87aa2b6180c81ee99  s32-ld.s32x
- f93ce93d495631e6f8e1b74124bdd4f8aaa7333fbf81006e6a5688735f2c5272  slow32dis.s32x
- dc64f040340a581765953d486c5d804c958e9c5f77a0a380b27ed221ef772b1d  slow32dump.s32x
