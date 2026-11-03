/* Dead-static removal past its old tables: 16,384 references, past
 * which it kept every function (hir_codegen.h, TAB_CG_REF/TAB_CG_EDGE).
 * 20,000 references from 100 statics, a chain of statics reached only
 * through other statics (dropping one live is an undefined symbol at
 * link time), and one dead.  Generated. */
int printf(const char *fmt, ...);
static unsigned acc;
/* a loop, so the inliner leaves the 20,000 calls as calls */
static void tick(unsigned k) {
    do { acc = acc * 31u + (k & 7u); k = k >> 3; } while (k);
}
#define T10(k) tick(k); tick(k + 1); tick(k + 2); tick(k + 3); tick(k + 4); \
    tick(k + 5); tick(k + 6); tick(k + 7); tick(k + 8); tick(k + 9);
#define T100(k) T10(k) T10(k + 10) T10(k + 20) T10(k + 30) T10(k + 40) \
    T10(k + 50) T10(k + 60) T10(k + 70) T10(k + 80) T10(k + 90)
static unsigned dead_one(unsigned x) { return x * 7u; }
static void w99(void) { T100(297) T100(298)  }
static void w98(void) { T100(294) T100(295) w99(); }
static void w97(void) { T100(291) T100(292) w98(); }
static void w96(void) { T100(288) T100(289) w97(); }
static void w95(void) { T100(285) T100(286) w96(); }
static void w94(void) { T100(282) T100(283) w95(); }
static void w93(void) { T100(279) T100(280) w94(); }
static void w92(void) { T100(276) T100(277) w93(); }
static void w91(void) { T100(273) T100(274) w92(); }
static void w90(void) { T100(270) T100(271) w91(); }
static void w89(void) { T100(267) T100(268) w90(); }
static void w88(void) { T100(264) T100(265) w89(); }
static void w87(void) { T100(261) T100(262) w88(); }
static void w86(void) { T100(258) T100(259) w87(); }
static void w85(void) { T100(255) T100(256) w86(); }
static void w84(void) { T100(252) T100(253) w85(); }
static void w83(void) { T100(249) T100(250) w84(); }
static void w82(void) { T100(246) T100(247) w83(); }
static void w81(void) { T100(243) T100(244) w82(); }
static void w80(void) { T100(240) T100(241) w81(); }
static void w79(void) { T100(237) T100(238) w80(); }
static void w78(void) { T100(234) T100(235) w79(); }
static void w77(void) { T100(231) T100(232) w78(); }
static void w76(void) { T100(228) T100(229) w77(); }
static void w75(void) { T100(225) T100(226) w76(); }
static void w74(void) { T100(222) T100(223) w75(); }
static void w73(void) { T100(219) T100(220) w74(); }
static void w72(void) { T100(216) T100(217) w73(); }
static void w71(void) { T100(213) T100(214) w72(); }
static void w70(void) { T100(210) T100(211) w71(); }
static void w69(void) { T100(207) T100(208) w70(); }
static void w68(void) { T100(204) T100(205) w69(); }
static void w67(void) { T100(201) T100(202) w68(); }
static void w66(void) { T100(198) T100(199) w67(); }
static void w65(void) { T100(195) T100(196) w66(); }
static void w64(void) { T100(192) T100(193) w65(); }
static void w63(void) { T100(189) T100(190) w64(); }
static void w62(void) { T100(186) T100(187) w63(); }
static void w61(void) { T100(183) T100(184) w62(); }
static void w60(void) { T100(180) T100(181) w61(); }
static void w59(void) { T100(177) T100(178) w60(); }
static void w58(void) { T100(174) T100(175) w59(); }
static void w57(void) { T100(171) T100(172) w58(); }
static void w56(void) { T100(168) T100(169) w57(); }
static void w55(void) { T100(165) T100(166) w56(); }
static void w54(void) { T100(162) T100(163) w55(); }
static void w53(void) { T100(159) T100(160) w54(); }
static void w52(void) { T100(156) T100(157) w53(); }
static void w51(void) { T100(153) T100(154) w52(); }
static void w50(void) { T100(150) T100(151) w51(); }
static void w49(void) { T100(147) T100(148) w50(); }
static void w48(void) { T100(144) T100(145) w49(); }
static void w47(void) { T100(141) T100(142) w48(); }
static void w46(void) { T100(138) T100(139) w47(); }
static void w45(void) { T100(135) T100(136) w46(); }
static void w44(void) { T100(132) T100(133) w45(); }
static void w43(void) { T100(129) T100(130) w44(); }
static void w42(void) { T100(126) T100(127) w43(); }
static void w41(void) { T100(123) T100(124) w42(); }
static void w40(void) { T100(120) T100(121) w41(); }
static void w39(void) { T100(117) T100(118) w40(); }
static void w38(void) { T100(114) T100(115) w39(); }
static void w37(void) { T100(111) T100(112) w38(); }
static void w36(void) { T100(108) T100(109) w37(); }
static void w35(void) { T100(105) T100(106) w36(); }
static void w34(void) { T100(102) T100(103) w35(); }
static void w33(void) { T100(99) T100(100) w34(); }
static void w32(void) { T100(96) T100(97) w33(); }
static void w31(void) { T100(93) T100(94) w32(); }
static void w30(void) { T100(90) T100(91) w31(); }
static void w29(void) { T100(87) T100(88) w30(); }
static void w28(void) { T100(84) T100(85) w29(); }
static void w27(void) { T100(81) T100(82) w28(); }
static void w26(void) { T100(78) T100(79) w27(); }
static void w25(void) { T100(75) T100(76) w26(); }
static void w24(void) { T100(72) T100(73) w25(); }
static void w23(void) { T100(69) T100(70) w24(); }
static void w22(void) { T100(66) T100(67) w23(); }
static void w21(void) { T100(63) T100(64) w22(); }
static void w20(void) { T100(60) T100(61) w21(); }
static void w19(void) { T100(57) T100(58) w20(); }
static void w18(void) { T100(54) T100(55) w19(); }
static void w17(void) { T100(51) T100(52) w18(); }
static void w16(void) { T100(48) T100(49) w17(); }
static void w15(void) { T100(45) T100(46) w16(); }
static void w14(void) { T100(42) T100(43) w15(); }
static void w13(void) { T100(39) T100(40) w14(); }
static void w12(void) { T100(36) T100(37) w13(); }
static void w11(void) { T100(33) T100(34) w12(); }
static void w10(void) { T100(30) T100(31) w11(); }
static void w09(void) { T100(27) T100(28) w10(); }
static void w08(void) { T100(24) T100(25) w09(); }
static void w07(void) { T100(21) T100(22) w08(); }
static void w06(void) { T100(18) T100(19) w07(); }
static void w05(void) { T100(15) T100(16) w06(); }
static void w04(void) { T100(12) T100(13) w05(); }
static void w03(void) { T100(9) T100(10) w04(); }
static void w02(void) { T100(6) T100(7) w03(); }
static void w01(void) { T100(3) T100(4) w02(); }
static void w00(void) { T100(0) T100(1) w01(); }
int main(void) {
    w00();
    printf("%u\n", acc);
    return acc == 1310642220u ? 0 : 1;   /* the host's answer */
}
