int abi_probe10(int a1, int a2, int a3, int a4, int a5,
                int a6, int a7, int a8, int a9, int a10);

int abi_call_probe(void) {
    return abi_probe10(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
}
