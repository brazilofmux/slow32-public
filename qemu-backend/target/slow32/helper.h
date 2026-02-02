DEF_HELPER_FLAGS_2(slow32_debug, 0, void, env, i32)
DEF_HELPER_FLAGS_1(slow32_yield, 0, void, env)
DEF_HELPER_FLAGS_1(slow32_halt, 0, void, env)

DEF_HELPER_FLAGS_1(slow32_native_memcpy, TCG_CALL_NO_WG, void, env)
DEF_HELPER_FLAGS_1(slow32_native_memset, TCG_CALL_NO_WG, void, env)
DEF_HELPER_FLAGS_1(slow32_native_memmove, TCG_CALL_NO_WG, void, env)
DEF_HELPER_FLAGS_1(slow32_native_strlen, TCG_CALL_NO_WG, void, env)
DEF_HELPER_FLAGS_1(slow32_native_memswap, TCG_CALL_NO_WG, void, env)

/* Floating-point helper: opcode in i32 arg, rd/rs1/rs2 encoded in instruction fields */
DEF_HELPER_FLAGS_2(slow32_fp_op, 0, void, env, i32)
