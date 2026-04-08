# Selfhost SHA256 Checksums

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

- 195cee7ba2e53740534733877f7b68577e1f405ec0a490b210404ef5a47d6931  s32-as.s32x
- 38e80138287bc650c827a5fe60baf92bda839b182eac27714b5a2ca7c464745b  s32-ld.s32x
- 4e2553f7fe2247fe7263218ac87c176cd288348ac5704c325727843e22c8aeb8  cc.s32x
- 5f1ff425e5145f1320037e15795efeaa4855da2a6ec26336702f8ca804c73f6c  slow32dump.s32x
- 8eb1944677a4aecd62915bbb9798e1f306fa08747439807fb009573031464c80  slow32dis.s32x
- b9277ad1321c76d47b8a7af2f793f39d422d19fcaa90272020b388ebb2e9101c  s32-ar.s32x

## Stage 06 (fixed-point proven)

- 161e64e61846a0a7eb0473794807cac081cc55657266c1535dd54667867cd873  cc.s32x
- 3a0458fc54522bf237130716ec6c5220c473b0be5cdd3e4fa606632576d85de0  s32-ld.s32x
- 4d4289a4be0981ac94747b6e1f2367d1299792e8b942608888f77465dcc5a71b  slow32dis.s32x
- 6145e162904dfd7708ac80dd8a2e50bc4f3a16695978e3c130798018cec178cf  s32-ar.s32x
- 8672a3b8e44e71bb7675652ae4df90374fc93349fd3b3d4ec36c03b82d70b7f3  s32-as.s32x
- e67b2f1f6519ce643bc81ff3ba202cc064dfeba74fbce6e0945e1937cb237f20  slow32dump.s32x
