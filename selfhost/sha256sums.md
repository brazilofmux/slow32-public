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
| slow32-dbt (stage 6) | Lenovo ARM Cortex-A720/Cortex-X4 |
| slow32-dbt (stage 7) | Lenovo ARM Cortex-A720/Cortex-X4 |
| slow32-fast (stage 7) | Intel(R) Xeon(R) Platinum 8259CL CPU @ 2.50GHz |
| slow32-dbt (stage 7) | Intel(R) Xeon(R) Platinum 8259CL CPU @ 2.50GHz |
| s32fast-hir (stage 7) | Intel(R) Xeon(R) Platinum 8259CL CPU @ 2.50GHz |
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

- 195cee7ba2e53740534733877f7b68577e1f405ec0a490b210404ef5a47d6931  s32-as.s32x
- 38e80138287bc650c827a5fe60baf92bda839b182eac27714b5a2ca7c464745b  s32-ld.s32x
- 4e2553f7fe2247fe7263218ac87c176cd288348ac5704c325727843e22c8aeb8  cc.s32x
- 5f1ff425e5145f1320037e15795efeaa4855da2a6ec26336702f8ca804c73f6c  slow32dump.s32x
- 8eb1944677a4aecd62915bbb9798e1f306fa08747439807fb009573031464c80  slow32dis.s32x
- b9277ad1321c76d47b8a7af2f793f39d422d19fcaa90272020b388ebb2e9101c  s32-ar.s32x

## Stage 06 (fixed-point proven)

- 161e64e61846a0a7eb0473794807cac081cc55657266c1535dd54667867cd873  cc.s32x
- 4d4289a4be0981ac94747b6e1f2367d1299792e8b942608888f77465dcc5a71b  slow32dis.s32x
- 6145e162904dfd7708ac80dd8a2e50bc4f3a16695978e3c130798018cec178cf  s32-ar.s32x
- 8672a3b8e44e71bb7675652ae4df90374fc93349fd3b3d4ec36c03b82d70b7f3  s32-as.s32x
- 8cab38cb9a77ca5fc140cba589eacf07103e6406671bb2c53614ba3a0cb1a657  s32-ld.s32x
- e67b2f1f6519ce643bc81ff3ba202cc064dfeba74fbce6e0945e1937cb237f20  slow32dump.s32x

## Stage 07

- 868208e7fc76d11fce2b1d70187eacb0d033105efdd266d3e1549ca169b5de91  s32-as.s32x
- fbdd70c3ab719450a06b8a7d276c6d66e05b7df7eac201dd14b1a01748921a8a  cc.s32x
- 8bf82cf422aa55662bf0b004797aa5cd9301c815a083ba7116a948a9c9f68878  slow32dump.s32x
- a73f8673f628a427ee2c23e5c1ce9bf6cccdfe80f0391600a3cb4862263a8bbd  slow32dis.s32x
- bfdaf67b543f0bb0a29065dece1a00d5d1026cb76e3f2105f266c2dab198516b  s32-ar.s32x
- e1b2cc3a441ab9a560756f332115f49842133476647025fcd24fe6386904415c  s32-ld.s32x

## Stage 08

- 71d363897d7d857c922d58733075cd40718247fd41eba3adb09ce136c933f58a  cc.s32x
- 4566160d593c7e06d7badc34f564098691e0231bfcc5abb7db624e5f08334e65  s32-ar.s32x
- 8fbecbf03df4177c5c5d4dd777fcf89d4aa2c5e44399b46e5cbd21464001a216  s32-as.s32x
- a6a24b6de21c480ec1c8df3a229213f844bb3256a824ad5c3dd8187d34660145  s32-ld.s32x
- c600079c60b6b171d0585f037b2015c22b0644dbd0054f39e3e92d1f3fa14656  slow32dis.s32x
- 45b491441ee249e0c822146abb5159b78e27be8c9a9859f822ac619de2eb7b52  slow32dump.s32x
