\ fib.fth — the M2 gate: recursion, EXIT, comparison, IF.
: FIB DUP 2 < IF EXIT THEN DUP 1- RECURSE SWAP 2 - RECURSE + ;
: MAIN 32 FIB . CR ;
