int ccmin_get_ret_imm(void);

int pass2_validate_ir(void) {
    int ret_imm = ccmin_get_ret_imm();
    if (ret_imm < -2048 || ret_imm > 2047) return 0;
    return 1;
}
