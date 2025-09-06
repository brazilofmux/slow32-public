        # Fibonacci sequence calculator
        # Calculates the 10th Fibonacci number
        
        .section .text
        .globl _start
_start:
        li      r3, 10          # Calculate fib(10)
        li      r4, 0           # fib(0) = 0
        li      r5, 1           # fib(1) = 1
        
.loop:
        beq     r3, r0, .done   # If counter == 0, we're done
        add     r6, r4, r5      # next = current + previous
        add     r4, r0, r5      # previous = current (mv r4, r5)
        add     r5, r0, r6      # current = next (mv r5, r6)
        addi    r3, r3, -1      # counter--
        jal     r0, .loop       # Continue loop
        
.done:
        add     r1, r0, r5      # Return value in r1 (should be 55)
        halt                    # Stop execution