/* 7.21.6.1 fprintf: %ls converts a wide string as if by wcrtomb; in the C locale ASCII passes through */
#include <stdio.h>
#include <stddef.h>

int main(void) {
    const wchar_t *ws = L"wide";
    printf("[%ls][%.2ls][%6ls][%-6ls]\n", ws, ws, ws, ws);
    return 0;
}
