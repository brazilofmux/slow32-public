: RUN-KERNEL
    ." Loading assembler..." CR
    S" forth/kernel.s" S" /tmp/kernel-forth.s32o"
    ." About to call ASSEMBLE..." CR
    ['] ASSEMBLE CATCH
    ?DUP IF ." ASSEMBLE threw error: " . CR ELSE ." ASSEMBLE completed." CR THEN ;
RUN-KERNEL
BYE
