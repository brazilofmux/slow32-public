/* Nesting past lowering's old fixed stacks, which grow now (tables.h):
 * 70 nested loops (64, and the 65th was written past the end without a
 * bound), 40 nested switches (32), and a K&R definition of 127
 * parameters (64; C11 5.2.4.1 asks for 127).  Generated. */
static int loops(int n) {
    int t = 0;
    for (int i0 = 0; i0 < 1; i0++) {
    for (int i1 = 0; i1 < 1; i1++) {
    for (int i2 = 0; i2 < 1; i2++) {
    for (int i3 = 0; i3 < 1; i3++) {
    for (int i4 = 0; i4 < 1; i4++) {
    for (int i5 = 0; i5 < 1; i5++) {
    for (int i6 = 0; i6 < 1; i6++) {
    for (int i7 = 0; i7 < 1; i7++) {
    for (int i8 = 0; i8 < 1; i8++) {
    for (int i9 = 0; i9 < 1; i9++) {
    for (int i10 = 0; i10 < 1; i10++) {
    for (int i11 = 0; i11 < 1; i11++) {
    for (int i12 = 0; i12 < 1; i12++) {
    for (int i13 = 0; i13 < 1; i13++) {
    for (int i14 = 0; i14 < 1; i14++) {
    for (int i15 = 0; i15 < 1; i15++) {
    for (int i16 = 0; i16 < 1; i16++) {
    for (int i17 = 0; i17 < 1; i17++) {
    for (int i18 = 0; i18 < 1; i18++) {
    for (int i19 = 0; i19 < 1; i19++) {
    for (int i20 = 0; i20 < 1; i20++) {
    for (int i21 = 0; i21 < 1; i21++) {
    for (int i22 = 0; i22 < 1; i22++) {
    for (int i23 = 0; i23 < 1; i23++) {
    for (int i24 = 0; i24 < 1; i24++) {
    for (int i25 = 0; i25 < 1; i25++) {
    for (int i26 = 0; i26 < 1; i26++) {
    for (int i27 = 0; i27 < 1; i27++) {
    for (int i28 = 0; i28 < 1; i28++) {
    for (int i29 = 0; i29 < 1; i29++) {
    for (int i30 = 0; i30 < 1; i30++) {
    for (int i31 = 0; i31 < 1; i31++) {
    for (int i32 = 0; i32 < 1; i32++) {
    for (int i33 = 0; i33 < 1; i33++) {
    for (int i34 = 0; i34 < 1; i34++) {
    for (int i35 = 0; i35 < 1; i35++) {
    for (int i36 = 0; i36 < 1; i36++) {
    for (int i37 = 0; i37 < 1; i37++) {
    for (int i38 = 0; i38 < 1; i38++) {
    for (int i39 = 0; i39 < 1; i39++) {
    for (int i40 = 0; i40 < 1; i40++) {
    for (int i41 = 0; i41 < 1; i41++) {
    for (int i42 = 0; i42 < 1; i42++) {
    for (int i43 = 0; i43 < 1; i43++) {
    for (int i44 = 0; i44 < 1; i44++) {
    for (int i45 = 0; i45 < 1; i45++) {
    for (int i46 = 0; i46 < 1; i46++) {
    for (int i47 = 0; i47 < 1; i47++) {
    for (int i48 = 0; i48 < 1; i48++) {
    for (int i49 = 0; i49 < 1; i49++) {
    for (int i50 = 0; i50 < 1; i50++) {
    for (int i51 = 0; i51 < 1; i51++) {
    for (int i52 = 0; i52 < 1; i52++) {
    for (int i53 = 0; i53 < 1; i53++) {
    for (int i54 = 0; i54 < 1; i54++) {
    for (int i55 = 0; i55 < 1; i55++) {
    for (int i56 = 0; i56 < 1; i56++) {
    for (int i57 = 0; i57 < 1; i57++) {
    for (int i58 = 0; i58 < 1; i58++) {
    for (int i59 = 0; i59 < 1; i59++) {
    for (int i60 = 0; i60 < 1; i60++) {
    for (int i61 = 0; i61 < 1; i61++) {
    for (int i62 = 0; i62 < 1; i62++) {
    for (int i63 = 0; i63 < 1; i63++) {
    for (int i64 = 0; i64 < 1; i64++) {
    for (int i65 = 0; i65 < 1; i65++) {
    for (int i66 = 0; i66 < 1; i66++) {
    for (int i67 = 0; i67 < 1; i67++) {
    for (int i68 = 0; i68 < 1; i68++) {
    for (int i69 = 0; i69 < 1; i69++) {
    t = t + n; if (t > 100) break; continue;
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    return t;
}
static int switches(int k) {
    int r = -1;
    switch (k > 0 ? 1 : 0) { case 0: r = 0; break; default:
    switch (k > 1 ? 1 : 0) { case 0: r = 1; break; default:
    switch (k > 2 ? 1 : 0) { case 0: r = 2; break; default:
    switch (k > 3 ? 1 : 0) { case 0: r = 3; break; default:
    switch (k > 4 ? 1 : 0) { case 0: r = 4; break; default:
    switch (k > 5 ? 1 : 0) { case 0: r = 5; break; default:
    switch (k > 6 ? 1 : 0) { case 0: r = 6; break; default:
    switch (k > 7 ? 1 : 0) { case 0: r = 7; break; default:
    switch (k > 8 ? 1 : 0) { case 0: r = 8; break; default:
    switch (k > 9 ? 1 : 0) { case 0: r = 9; break; default:
    switch (k > 10 ? 1 : 0) { case 0: r = 10; break; default:
    switch (k > 11 ? 1 : 0) { case 0: r = 11; break; default:
    switch (k > 12 ? 1 : 0) { case 0: r = 12; break; default:
    switch (k > 13 ? 1 : 0) { case 0: r = 13; break; default:
    switch (k > 14 ? 1 : 0) { case 0: r = 14; break; default:
    switch (k > 15 ? 1 : 0) { case 0: r = 15; break; default:
    switch (k > 16 ? 1 : 0) { case 0: r = 16; break; default:
    switch (k > 17 ? 1 : 0) { case 0: r = 17; break; default:
    switch (k > 18 ? 1 : 0) { case 0: r = 18; break; default:
    switch (k > 19 ? 1 : 0) { case 0: r = 19; break; default:
    switch (k > 20 ? 1 : 0) { case 0: r = 20; break; default:
    switch (k > 21 ? 1 : 0) { case 0: r = 21; break; default:
    switch (k > 22 ? 1 : 0) { case 0: r = 22; break; default:
    switch (k > 23 ? 1 : 0) { case 0: r = 23; break; default:
    switch (k > 24 ? 1 : 0) { case 0: r = 24; break; default:
    switch (k > 25 ? 1 : 0) { case 0: r = 25; break; default:
    switch (k > 26 ? 1 : 0) { case 0: r = 26; break; default:
    switch (k > 27 ? 1 : 0) { case 0: r = 27; break; default:
    switch (k > 28 ? 1 : 0) { case 0: r = 28; break; default:
    switch (k > 29 ? 1 : 0) { case 0: r = 29; break; default:
    switch (k > 30 ? 1 : 0) { case 0: r = 30; break; default:
    switch (k > 31 ? 1 : 0) { case 0: r = 31; break; default:
    switch (k > 32 ? 1 : 0) { case 0: r = 32; break; default:
    switch (k > 33 ? 1 : 0) { case 0: r = 33; break; default:
    switch (k > 34 ? 1 : 0) { case 0: r = 34; break; default:
    switch (k > 35 ? 1 : 0) { case 0: r = 35; break; default:
    switch (k > 36 ? 1 : 0) { case 0: r = 36; break; default:
    switch (k > 37 ? 1 : 0) { case 0: r = 37; break; default:
    switch (k > 38 ? 1 : 0) { case 0: r = 38; break; default:
    switch (k > 39 ? 1 : 0) { case 0: r = 39; break; default:
    r = 99;
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    }
    return r;
}
static int kr(p000, p001, p002, p003, p004, p005, p006, p007, p008, p009, p010, p011, p012, p013, p014, p015, p016, p017, p018, p019, p020, p021, p022, p023, p024, p025, p026, p027, p028, p029, p030, p031, p032, p033, p034, p035, p036, p037, p038, p039, p040, p041, p042, p043, p044, p045, p046, p047, p048, p049, p050, p051, p052, p053, p054, p055, p056, p057, p058, p059, p060, p061, p062, p063, p064, p065, p066, p067, p068, p069, p070, p071, p072, p073, p074, p075, p076, p077, p078, p079, p080, p081, p082, p083, p084, p085, p086, p087, p088, p089, p090, p091, p092, p093, p094, p095, p096, p097, p098, p099, p100, p101, p102, p103, p104, p105, p106, p107, p108, p109, p110, p111, p112, p113, p114, p115, p116, p117, p118, p119, p120, p121, p122, p123, p124, p125, p126)
    int p000, p001, p002, p003, p004, p005, p006, p007, p008, p009, p010, p011, p012, p013, p014, p015, p016, p017, p018, p019, p020, p021, p022, p023, p024, p025, p026, p027, p028, p029, p030, p031, p032, p033, p034, p035, p036, p037, p038, p039, p040, p041, p042, p043, p044, p045, p046, p047, p048, p049, p050, p051, p052, p053, p054, p055, p056, p057, p058, p059, p060, p061, p062, p063, p064, p065, p066, p067, p068, p069, p070, p071, p072, p073, p074, p075, p076, p077, p078, p079, p080, p081, p082, p083, p084, p085, p086, p087, p088, p089, p090, p091, p092, p093, p094, p095, p096, p097, p098, p099, p100, p101, p102, p103, p104, p105, p106, p107, p108, p109, p110, p111, p112, p113, p114, p115, p116, p117, p118, p119, p120, p121, p122, p123, p124, p125, p126;
{
    return p000 + p063 + p126;
}
int main(void) {
    if (loops(3) != 3) return 1;
    if (switches(5) != 5 || switches(100) != 99) return 2;
    if (kr(0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0) != 0 + 0 + 0) return 3;
    return 0;
}
