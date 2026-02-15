# Test 2: loop printing "ABCDE\n" using branches and labels
.text
    li r3, 65       # 'A'
    li r4, 70       # 'E' + 1 (stop value)
loop:
    debug r3
    addi r3, r3, 1
    blt r3, r4, loop
    li r3, 10       # '\n'
    debug r3
    halt
