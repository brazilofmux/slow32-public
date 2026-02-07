.text
.global main

# Register assignments
# r0: Zero
# r1-r2: Temp
# r3-r10: C Args (volatile)
# r11-r25: Saved/General
# r26: IP (Instruction Pointer)
# r27: RSP (Return Stack Pointer)
# r28: DSP (Data Stack Pointer)
# r29: System Stack (SP)
# r30: Frame Pointer
# r31: Link Register

# Direct Threaded Code implementation

# Entry point
main:
    # Initialize stacks
    lui r28, %hi(dstack_top)
    addi r28, r28, %lo(dstack_top)
    
    lui r27, %hi(rstack_top)
    addi r27, r27, %lo(rstack_top)

    # Set IP to the start of our Forth program (COLD_START)
    lui r26, %hi(cold_start_body)
    addi r26, r26, %lo(cold_start_body)

    # Start the interpreter
    jal r0, next

# The Inner Interpreter
next:
    ldw r25, r26, 0    # W = *IP (XT pointer)
    addi r26, r26, 4   # IP++
    ldw r24, r25, 0    # code pointer = *XT
    jalr r0, r24, 0    # Jump to code pointer

# ----------------------------------------------------------------------
# Primitives with Dictionary Headers
# ----------------------------------------------------------------------
# Link format: Points to previous word's header (start)
# Header: Link(4), Len(1), Name(N), Padding(Align 2), XT(4)

# Word: EXIT
.text
    .align 2
head_exit:
    .word 0            # Link (First word)
    .byte 4
    .ascii "EXIT"
    .align 2
xt_exit:
    .word exit_word

exit_word:
    ldw r26, r27, 0    # IP = *RSP
    addi r27, r27, 4   # RSP++
    jal r0, next

# Word: DOCOL (Internal)
docol_word:
    addi r27, r27, -4  # RSP--
    stw r27, r26, 0    # *RSP = IP (Save old IP)
    addi r26, r25, 4   # IP = XT + 4 (body)
    jal r0, next

# Word: DOCREATE (Internal) - runtime for CREATE'd words
# Layout: [docreate] [does-cell] [data...]
#          W          W+4         W+8 = PFA
docreate:
    addi r1, r25, 8    # PFA = W + 8 (skip code-ptr + does-cell)
    addi r28, r28, -4
    stw r28, r1, 0     # push PFA
    jal r0, next

# Word: DODOES (Internal) - runtime for DOES> modified words
# Layout: [dodoes] [does-thread-addr] [data...]
#          W        W+4                W+8 = PFA
dodoes:
    addi r1, r25, 8    # PFA = W + 8
    addi r28, r28, -4
    stw r28, r1, 0     # push PFA
    addi r27, r27, -4
    stw r27, r26, 0    # push IP to return stack
    ldw r26, r25, 4    # IP = does-thread (from XT+4)
    jal r0, next

# Word: LIT ( -- x ) runtime: push next cell from instruction stream
.text
    .align 2
head_lit:
    .word head_exit
    .byte 3
    .ascii "LIT"
    .align 2
xt_lit:
    .word lit_word
lit_word:
    ldw r1, r26, 0     # r1 = *IP
    addi r26, r26, 4   # IP++
    addi r28, r28, -4  # DSP--
    stw r28, r1, 0     # *DSP = r1
    jal r0, next

# Word: EXECUTE ( xt -- )
.text
    .align 2
head_execute:
    .word head_lit
    .byte 7
    .ascii "EXECUTE"
    .align 2
xt_execute:
    .word execute_word

execute_word:
    ldw r25, r28, 0     # xt = *DSP
    addi r28, r28, 4    # DSP++
    ldw r24, r25, 0     # code pointer
    jalr r0, r24, 0

# Word: DUP ( a -- a a )
.text
    .align 2
head_dup:
    .word head_execute
    .byte 3
    .ascii "DUP"
    .align 2
xt_dup:
    .word dup_word

dup_word:
    ldw r1, r28, 0     # r1 = TOS
    addi r28, r28, -4  # DSP--
    stw r28, r1, 0     # Push r1
    jal r0, next

# Word: DROP ( a -- )
.text
    .align 2
head_drop:
    .word head_dup
    .byte 4
    .ascii "DROP"
    .align 2
xt_drop:
    .word drop_word

drop_word:
    addi r28, r28, 4   # DSP++
    jal r0, next

# Word: SWAP ( a b -- b a )
.text
    .align 2
head_swap:
    .word head_drop
    .byte 4
    .ascii "SWAP"
    .align 2
xt_swap:
    .word swap_word

swap_word:
    ldw r1, r28, 0     # r1 = b (TOS)
    ldw r2, r28, 4     # r2 = a (TOS+4)
    stw r28, r2, 0     # *DSP = a (New TOS)
    stw r28, r1, 4     # *(DSP+4) = b
    jal r0, next

# Word: EMIT ( c -- )
.text
    .align 2
head_emit:
    .word head_swap
    .byte 4
    .ascii "EMIT"
    .align 2
xt_emit:
    .word emit_word

emit_word:
    ldw r3, r28, 0     # r3 = TOS
    addi r28, r28, 4   # DSP++
    debug r3
    jal r0, next

# Word: KEY ( -- c )
.text
    .align 2
head_key:
    .word head_emit
    .byte 3
    .ascii "KEY"
    .align 2
xt_key:
    .word key_word

key_word:
    jal getchar
    addi r28, r28, -4
    stw r28, r1, 0
    jal r0, next

# Word: FIND ( c-addr -- xt | 0 )
.text
    .align 2
head_find:
    .word head_key
    .byte 4
    .ascii "FIND"
    .align 2
xt_find:
    .word find_word

# FIND ( c-addr -- xt flag | 0 0 )
# flag = 1 if the word is IMMEDIATE, 0 otherwise
find_word:
    ldw r4, r28, 0     # r4 = search string address
    addi r28, r28, 4   # pop input
    
    # Load LATEST
    lui r5, %hi(var_latest)
    addi r5, r5, %lo(var_latest)
    ldw r5, r5, 0      # r5 = current dictionary head

find_loop:
    beq r5, r0, find_fail

    # Compare length (mask out IMMEDIATE bit)
    ldbu r6, r5, 4     # dict len/raw flags
    addi r7, r0, 0x7F
    and r8, r6, r7     # dict_len = len & 0x7F
    ldbu r7, r4, 0     # search len
    bne r8, r7, find_next

    # Compare bytes
    add r9, r0, r0     # i = 0
find_str_loop:
    bge r9, r8, find_match
    
    add r10, r5, r9
    ldbu r10, r10, 5     # dict char (offset 5 + i)
    
    add r11, r4, r9
    ldbu r11, r11, 1     # search char (offset 1 + i)
    
    bne r10, r11, find_next
    
    addi r9, r9, 1
    jal r0, find_str_loop

find_next:
    ldw r5, r5, 0      # Load link
    jal r0, find_loop

find_match:
    # Found. Calculate XT address.
    # XT is at header + aligned(5 + len)
    addi r5, r5, 5
    add r5, r5, r8     # r5 = end of name string
    
    # Align r5 to 4 bytes
    # mask = -4 (0xFFFFFFFC)
    addi r10, r0, -4
    addi r5, r5, 3
    and r5, r5, r10
    
    # Push xt pointer and immediate flag
    addi r28, r28, -4
    stw r28, r5, 0     # xt

    addi r10, r0, 0x80
    and r6, r6, r10
    beq r6, r0, find_not_immediate
    addi r6, r0, 1
find_not_immediate:
    addi r28, r28, -4
    stw r28, r6, 0     # flag

    jal r0, next

find_fail:
    addi r28, r28, -4
    stw r28, r0, 0     # xt = 0
    addi r28, r28, -4
    stw r28, r0, 0     # flag = 0
    jal r0, next


# Word: BYE
.text
    .align 2
head_bye:
    .word head_find
    .byte 3
    .ascii "BYE"
    .align 2
xt_bye:
    .word bye_word

bye_word:
    halt

# Word: ACCEPT ( c-addr +n -- +n' )
# Read at most +n chars into c-addr. Stop at newline. Return num chars read.
.text
    .align 2
head_accept:
    .word head_bye
    .byte 6
    .ascii "ACCEPT"
    .align 2
xt_accept:
    .word accept_word

accept_word:
    ldw r4, r28, 0     # r4 = max length (+n)
    ldw r5, r28, 4     # r5 = buffer address (c-addr)
    addi r28, r28, 8   # Pop both args; DSP now points above inputs
    
    add r6, r0, r0     # r6 = count (i)
    
accept_loop:
    beq r6, r4, accept_done   # If count == max, done
    
    # We need to save regs before calling C? 
    # r4, r5, r6 are caller-saved in C ABI?
    # Yes, r3-r10 are volatile.
    # We must save our loop state (r4, r5, r6).
    # Easier: Use saved registers r11-r25 for the loop state.
    # r11 = max len
    # r12 = buffer addr
    # r13 = count
    
    add r11, r4, r0
    add r12, r5, r0
    add r13, r6, r0
    
    jal getchar        # r1 = char
    
    # Restore/Move back to loop regs
    add r4, r11, r0
    add r5, r12, r0
    add r6, r13, r0
    
    # Check EOF (-1)
    addi r2, r0, -1
    beq r1, r2, accept_eof

    # Check Newline (10)
    addi r2, r0, 10
    beq r1, r2, accept_done

    # Check CR (13) - treat as newline
    addi r2, r0, 13
    beq r1, r2, accept_done
    
    # Store char
    add r7, r5, r6     # addr + i
    stb r7, r1, 0
    
    addi r6, r6, 1     # i++
    jal r0, accept_loop

accept_eof:
    # EOF with no chars read: return -1 to signal EOF
    bne r6, r0, accept_done  # if we already read something, return count
    addi r6, r0, -1           # signal EOF
accept_done:
    addi r28, r28, -4  # push result
    stw r28, r6, 0     # Store count

    # Update #TIB and reset >IN (use count or 0 for EOF)
    add r7, r6, r0      # save count
    blt r6, r0, accept_eof_tib  # if EOF (-1), set #TIB=0
    lui r8, %hi(var_source_len)
    addi r8, r8, %lo(var_source_len)
    stw r8, r6, 0
    jal r0, accept_reset_in
accept_eof_tib:
    lui r8, %hi(var_source_len)
    addi r8, r8, %lo(var_source_len)
    stw r8, r0, 0
accept_reset_in:
    lui r8, %hi(var_to_in)
    addi r8, r8, %lo(var_to_in)
    stw r8, r0, 0
    jal r0, next


# Word: PARSE-WORD ( -- c-addr u )
# Skips leading delimiters (space=32)
# Parses until next delimiter
# Returns string address (in TIB) and length
# Updates >IN
.text
    .align 2
head_parse_word:
    .word head_accept
    .byte 10
    .ascii "PARSE-WORD"
    .align 2
xt_parse_word:
    .word parse_word_word

parse_word_word:
    # Load TIB, #TIB, >IN
    lui r1, %hi(tib)
    addi r1, r1, %lo(tib)   # r1 = TIB base
    
    lui r2, %hi(var_source_len)
    addi r2, r2, %lo(var_source_len)
    ldw r2, r2, 0           # r2 = #TIB (Limit)
    
    lui r3, %hi(var_to_in)
    addi r3, r3, %lo(var_to_in)
    ldw r4, r3, 0           # r4 = >IN (Current Offset)
    
    # 1. Skip leading spaces
parse_skip:
    bge r4, r2, parse_fail  # If >IN >= #TIB, fail (return 0 0)
    
    add r5, r1, r4          # current addr = TIB + >IN
    ldbu r6, r5, 0          # load char
    
    addi r7, r0, 32         # Space
    bne r6, r7, parse_found # If not space, found start
    
    addi r4, r4, 1          # >IN++
    jal r0, parse_skip

parse_found:
    add r11, r1, r4         # r11 = Start Address
    add r12, r4, r0         # r12 = Start Index
    
    # 2. Scan until space or end
parse_scan:
    bge r4, r2, parse_end   # If >IN >= #TIB, end
    
    add r5, r1, r4
    ldbu r6, r5, 0
    
    addi r7, r0, 32         # Space
    beq r6, r7, parse_end   # If space, end
    
    # Also check for invisible control chars? For now just space.
    
    addi r4, r4, 1          # >IN++
    jal r0, parse_scan

parse_end:
    # r4 is now at the delimiter (or end)
    # Length = r4 - r12
    sub r13, r4, r12
    
    # Update >IN variable
    stw r3, r4, 0
    
    # Push ( addr len )
    addi r28, r28, -4
    stw r28, r11, 0    # Push addr
    addi r28, r28, -4
    stw r28, r13, 0    # Push len
    jal r0, next

parse_fail:
    # Return ( 0 0 )
    addi r28, r28, -4
    stw r28, r0, 0
    addi r28, r28, -4
    stw r28, r0, 0
    jal r0, next


# Word: TYPE ( c-addr u -- )
.text
    .align 2
head_type:
    .word head_parse_word
    .byte 4
    .ascii "TYPE"
    .align 2
xt_type:
    .word type_word

type_word:
    ldw r4, r28, 0     # r4 = len
    ldw r5, r28, 4     # r5 = addr
    addi r28, r28, 8   # Pop both
    
    add r6, r0, r0     # i = 0
type_loop:
    bge r6, r4, type_done

    add r7, r5, r6     # addr + i
    ldbu r3, r7, 0     # char
    debug r3

    addi r6, r6, 1
    jal r0, type_loop

type_done:
    jal r0, next

# Word: DOT ( n -- ) print number in current BASE with trailing space
.text
    .align 2
head_dot:
    .word head_type
    .byte 1
    .ascii "."
    .align 2
xt_dot:
    .word dot_word
dot_word:
    jal dot_print_sub
    jal r0, next

dot_print_sub:
    add r16, r31, r0     # save caller return
    ldw r4, r28, 0     # n
    addi r28, r28, 4   # pop

    # Load BASE
    lui r5, %hi(var_base)
    addi r5, r5, %lo(var_base)
    ldw r5, r5, 0      # base

    add r6, r0, r0     # sign flag
    add r10, r4, r0    # work = n

    # If base==10 and n<0, remember sign and negate
    addi r7, r0, 10
    bne r5, r7, dot_no_sign
    blt r4, r0, dot_make_positive
    jal r0, dot_no_sign
dot_make_positive:
    sub r10, r0, r10   # work = -n
    addi r6, r0, 1     # sign = 1
dot_no_sign:

    # Set buffer end (use top half of PAD)
    lui r8, %hi(pad)
    addi r8, r8, %lo(pad)
    addi r8, r8, 127   # buffer end
    add r9, r8, r0     # keep end pointer

    # Special case n==0
    bne r10, r0, dot_loop
    addi r8, r8, -1
    addi r7, r0, 48    # '0'
    stb r8, r7, 0
    jal r0, dot_digits_done

dot_loop:
    div r11, r10, r5   # quot = work / base
    rem r12, r10, r5   # rem  = work % base
    add r10, r11, r0   # work = quot

    # Convert digit to ASCII
    addi r7, r0, 10
    blt r12, r7, dot_digit_numeric
    addi r12, r12, 55  # 'A' = 65 -> 10 -> +55
    jal r0, dot_store_digit
dot_digit_numeric:
    addi r12, r12, 48
dot_store_digit:
    addi r8, r8, -1
    stb r8, r12, 0

    bne r10, r0, dot_loop

dot_digits_done:
    # Add sign if needed
    beq r6, r0, dot_build_done
    addi r8, r8, -1
    addi r7, r0, 45    # '-'
    stb r8, r7, 0

dot_build_done:
    sub r13, r9, r8    # len = end - start

    # Append trailing space
    addi r7, r0, 32
    stb r9, r7, 0
    addi r13, r13, 1   # include space

    # Emit string manually (non-recursive)
    add r14, r8, r0     # ptr
    add r15, r13, r0    # remaining
dot_emit_loop:
    beq r15, r0, dot_emit_done
    ldbu r3, r14, 0
    debug r3
    addi r14, r14, 1
    addi r15, r15, -1
    jal r0, dot_emit_loop
dot_emit_done:
    add r31, r16, r0
    jalr r0, r31, 0


# Word: HELLO ( -- )
.text
    .align 2
head_hello:
    .word head_dot    # Linked to DOT
    .byte 5
    .ascii "HELLO"
    .align 2
xt_hello:
    .word docol_word
    .word xt_lit, 72, xt_emit  # H
    .word xt_lit, 101, xt_emit # e
    .word xt_lit, 108, xt_emit # l
    .word xt_lit, 108, xt_emit # l
    .word xt_lit, 111, xt_emit # o
    .word xt_lit, 10, xt_emit  # \n
    .word xt_exit


# Word: STATE ( -- a-addr )
.text
    .align 2
head_state:
    .word head_hello
    .byte 5
    .ascii "STATE"
    .align 2
xt_state:
    .word state_word
state_word:
    lui r1, %hi(var_state)
    addi r1, r1, %lo(var_state)
    addi r28, r28, -4
    stw r28, r1, 0
    jal r0, next

# Word: BASE ( -- a-addr )
.text
    .align 2
head_base:
    .word head_state
    .byte 4
    .ascii "BASE"
    .align 2
xt_base:
    .word base_word
base_word:
    lui r1, %hi(var_base)
    addi r1, r1, %lo(var_base)
    addi r28, r28, -4
    stw r28, r1, 0
    jal r0, next

# Word: BASE! ( x -- ) store into BASE without needing addr
.text
    .align 2
head_base_store:
    .word head_base
    .byte 5
    .ascii "BASE!"
    .align 2
xt_base_store:
    .word base_store_word
base_store_word:
    ldw r1, r28, 0     # x
    addi r28, r28, 4   # pop
    lui r2, %hi(var_base)
    addi r2, r2, %lo(var_base)
    stw r2, r1, 0      # *var_base = x
    jal r0, next

# Word: LATEST ( -- a-addr ) address of latest dictionary pointer
.text
    .align 2
head_latest:
    .word head_base_store
    .byte 6
    .ascii "LATEST"
    .align 2
xt_latest:
    .word latest_word
latest_word:
    lui r1, %hi(var_latest)
    addi r1, r1, %lo(var_latest)
    addi r28, r28, -4
    stw r28, r1, 0
    jal r0, next

# Word: HERE ( -- a-addr )
.text
    .align 2
head_here:
    .word head_latest
    .byte 4
    .ascii "HERE"
    .align 2
xt_here:
    .word here_word
here_word:
    lui r1, %hi(var_here)
    addi r1, r1, %lo(var_here)
    addi r28, r28, -4
    stw r28, r1, 0
    jal r0, next

# Word: TIB ( -- a-addr )
.text
    .align 2
head_tib:
    .word head_here
    .byte 3
    .ascii "TIB"
    .align 2
xt_tib:
    .word tib_word
tib_word:
    lui r1, %hi(tib)
    addi r1, r1, %lo(tib)
    addi r28, r28, -4
    stw r28, r1, 0
    jal r0, next

# Word: TOIN ( -- a-addr )
.text
    .align 2
head_to_in:
    .word head_tib
    .byte 4
    .ascii "TOIN"
    .align 2
xt_to_in:
    .word to_in_word
to_in_word:
    lui r1, %hi(var_to_in)
    addi r1, r1, %lo(var_to_in)
    addi r28, r28, -4
    stw r28, r1, 0
    jal r0, next

# Word: NTIB ( -- a-addr ) - Length of TIB
.text
    .align 2
head_num_tib:
    .word head_to_in
    .byte 4
    .ascii "NTIB"
    .align 2
xt_num_tib:
    .word num_tib_word
num_tib_word:
    lui r1, %hi(var_source_len)
    addi r1, r1, %lo(var_source_len)
    addi r28, r28, -4
    stw r28, r1, 0
    jal r0, next

# Word: ! ( x a-addr -- )
.text
    .align 2
head_store:
    .word head_num_tib
    .byte 1
    .ascii "!"
    .align 2
xt_store:
    .word store_word

store_word:
    ldw r1, r28, 0     # addr
    ldw r2, r28, 4     # x
    addi r28, r28, 8   # Pop
    stw r1, r2, 0      # *addr = x (Remember: stw base, src, offset)
    jal r0, next

# Word: @ ( a-addr -- x )
.text
    .align 2
head_fetch:
    .word head_store
    .byte 1
    .ascii "@"
    .align 2
xt_fetch:
    .word fetch_word

fetch_word:
    ldw r1, r28, 0     # addr
    ldw r2, r1, 0      # x = *addr
    stw r28, r2, 0     # Replace TOS
    jal r0, next

# Word: CR ( -- )
.text
    .align 2
head_cr:
    .word head_fetch
    .byte 2
    .ascii "CR"
    .align 2
xt_cr:
    .word cr_word
cr_word:
    addi r3, r0, 10
    debug r3
    jal r0, next

# Word: C@ ( c-addr -- x ) unsigned byte fetch
.text
    .align 2
head_cfetch:
    .word head_cr
    .byte 2
    .ascii "C@"
    .align 2
xt_cfetch:
    .word cfetch_word
cfetch_word:
    ldw r1, r28, 0     # addr
    ldbu r2, r1, 0     # byte
    stw r28, r2, 0     # replace TOS
    jal r0, next

# Word: OVER ( a b -- a b a )
.text
    .align 2
head_over:
    .word head_cfetch
    .byte 4
    .ascii "OVER"
    .align 2
xt_over:
    .word over_word
over_word:
    ldw r1, r28, 4     # fetch second stack item
    addi r28, r28, -4  # push
    stw r28, r1, 0
    jal r0, next

# Word: >R ( x -- )  move to return stack
.text
    .align 2
head_to_r:
    .word head_over
    .byte 2
    .ascii ">R"
    .align 2
xt_to_r:
    .word to_r_word
to_r_word:
    ldw r1, r28, 0
    addi r28, r28, 4   # pop data
    addi r27, r27, -4
    stw r27, r1, 0
    jal r0, next

# Word: R> ( -- x )  move from return stack
.text
    .align 2
head_r_from:
    .word head_to_r
    .byte 2
    .ascii "R>"
    .align 2
xt_r_from:
    .word r_from_word
r_from_word:
    ldw r1, r27, 0
    addi r27, r27, 4
    addi r28, r28, -4
    stw r28, r1, 0
    jal r0, next

# Word: R@ ( -- x )  copy return stack top
.text
    .align 2
head_r_fetch:
    .word head_r_from
    .byte 2
    .ascii "R@"
    .align 2
xt_r_fetch:
    .word r_fetch_word
r_fetch_word:
    ldw r1, r27, 0
    addi r28, r28, -4
    stw r28, r1, 0
    jal r0, next

# Word: + ( a b -- sum )
.text
    .align 2
head_plus:
    .word head_r_fetch
    .byte 1
    .ascii "+"
    .align 2
xt_plus:
    .word plus_word
plus_word:
    ldw r1, r28, 0     # b
    ldw r2, r28, 4     # a
    add r2, r2, r1
    addi r28, r28, 4   # pop one
    stw r28, r2, 0     # replace TOS
    jal r0, next

# Word: - ( a b -- a-b )
.text
    .align 2
head_minus:
    .word head_plus
    .byte 1
    .ascii "-"
    .align 2
xt_minus:
    .word minus_word
minus_word:
    ldw r1, r28, 0     # b
    ldw r2, r28, 4     # a
    sub r2, r2, r1
    addi r28, r28, 4
    stw r28, r2, 0
    jal r0, next

# Word: * ( a b -- a*b )
.text
    .align 2
head_mul:
    .word head_minus
    .byte 1
    .ascii "*"
    .align 2
xt_mul:
    .word mul_word
mul_word:
    ldw r1, r28, 0     # b
    ldw r2, r28, 4     # a
    mul r2, r2, r1
    addi r28, r28, 4
    stw r28, r2, 0
    jal r0, next

# Word: AND ( a b -- a&b )
.text
    .align 2
head_and:
    .word head_mul
    .byte 3
    .ascii "AND"
    .align 2
xt_and:
    .word and_word
and_word:
    ldw r1, r28, 0
    ldw r2, r28, 4
    and r2, r2, r1
    addi r28, r28, 4
    stw r28, r2, 0
    jal r0, next

# Word: OR ( a b -- a|b )
.text
    .align 2
head_or:
    .word head_and
    .byte 2
    .ascii "OR"
    .align 2
xt_or:
    .word or_word
or_word:
    ldw r1, r28, 0
    ldw r2, r28, 4
    or r2, r2, r1
    addi r28, r28, 4
    stw r28, r2, 0
    jal r0, next

# Word: = ( a b -- flag )
.text
    .align 2
head_equals:
    .word head_or
    .byte 1
    .ascii "="
    .align 2
xt_equals:
    .word equals_word
equals_word:
    ldw r1, r28, 0
    ldw r2, r28, 4
    addi r3, r0, 1
    bne r1, r2, equals_zero
    jal r0, equals_push
equals_zero:
    addi r3, r0, 0
equals_push:
    addi r28, r28, 4
    stw r28, r3, 0
    jal r0, next

# Word: < ( a b -- flag ) signed
.text
    .align 2
head_less:
    .word head_equals
    .byte 1
    .ascii "<"
    .align 2
xt_less:
    .word less_word
less_word:
    ldw r1, r28, 0     # b
    ldw r2, r28, 4     # a
    addi r3, r0, 0
    blt r2, r1, less_set
    jal r0, less_push
less_set:
    addi r3, r0, 1
less_push:
    addi r28, r28, 4
    stw r28, r3, 0
    jal r0, next

# Word: BRANCH ( -- ) (runtime for compiled loops)
.text
    .align 2
head_branch:
    .word head_less
    .byte 6
    .ascii "BRANCH"
    .align 2
xt_branch:
    .word branch_word
branch_word:
    ldw r1, r26, 0     # offset
    addi r26, r26, 4   # skip offset cell
    add r26, r26, r1   # IP += offset (offset is relative to next cell)
    jal r0, next

# Word: 0BRANCH ( flag -- ) branches if flag == 0
.text
    .align 2
head_0branch:
    .word head_branch
    .byte 7
    .ascii "0BRANCH"
    .align 2
xt_0branch:
    .word zbranch_word
zbranch_word:
    ldw r1, r28, 0     # flag
    addi r28, r28, 4   # pop flag
    bne r1, r0, zbranch_fallthrough
    ldw r1, r26, 0     # offset
    add r26, r26, r1   # IP += offset
    addi r26, r26, 4   # skip offset cell
    jal r0, next
zbranch_fallthrough:
    addi r26, r26, 4   # skip offset cell
    jal r0, next

# Word: WORD ( -- c-addr ) parses next token into PAD as counted string
.text
    .align 2
head_word:
    .word head_0branch
    .byte 4
    .ascii "WORD"
    .align 2
xt_word:
    .word word_word
word_word:
    # Load TIB base, #TIB, >IN
    lui r1, %hi(tib)
    addi r1, r1, %lo(tib)   # r1 = TIB base
    lui r2, %hi(var_source_len)
    addi r2, r2, %lo(var_source_len)
    ldw r2, r2, 0           # r2 = #TIB
    lui r3, %hi(var_to_in)
    addi r3, r3, %lo(var_to_in)
    ldw r4, r3, 0           # r4 = >IN

    # Skip leading spaces
word_skip:
    bge r4, r2, word_empty
    add r5, r1, r4
    ldbu r6, r5, 0
    addi r7, r0, 32
    bne r6, r7, word_found
    addi r4, r4, 1
    jal r0, word_skip

word_found:
    add r12, r4, r0         # start index
    add r11, r1, r4         # start addr

    # Scan until space or end
word_scan:
    bge r4, r2, word_end
    add r5, r1, r4
    ldbu r6, r5, 0
    addi r7, r0, 32
    beq r6, r7, word_end
    addi r4, r4, 1
    jal r0, word_scan

word_end:
    sub r13, r4, r12        # len
    stw r3, r4, 0           # update >IN

    # Build counted string in PAD (uppercased)
    lui r14, %hi(pad)
    addi r14, r14, %lo(pad)
    stb r14, r13, 0         # length byte
    addi r15, r14, 1        # dest pointer
    add r16, r11, r0        # src pointer
    add r17, r0, r0         # i = 0

word_copy_loop:
    bge r17, r13, word_copy_done
    ldbu r18, r16, 0
    addi r19, r0, 97        # 'a'
    blt r18, r19, word_no_upper
    addi r19, r0, 122       # 'z'
    bgt r18, r19, word_no_upper
    addi r18, r18, -32      # uppercase
word_no_upper:
    stb r15, r18, 0
    addi r15, r15, 1
    addi r16, r16, 1
    addi r17, r17, 1
    jal r0, word_copy_loop

word_copy_done:
    addi r28, r28, -4
    stw r28, r14, 0         # push PAD address
    jal r0, next

word_empty:
    lui r14, %hi(pad)
    addi r14, r14, %lo(pad)
    stb r14, r0, 0          # len = 0
    addi r28, r28, -4
    stw r28, r14, 0
    jal r0, next

# Word: NUMBER ( c-addr -- n flag ) convert counted string using BASE
.text
    .align 2
head_number:
    .word head_word
    .byte 6
    .ascii "NUMBER"
    .align 2
xt_number:
    .word number_word
number_word:
    ldw r4, r28, 0      # c-addr
    addi r28, r28, 4    # pop input
    ldbu r5, r4, 0      # len
    beq r5, r0, number_fail

    lui r13, %hi(var_base)
    addi r13, r13, %lo(var_base)
    ldw r13, r13, 0     # base

    addi r6, r4, 1      # ptr to chars
    add r7, r0, r0      # i
    add r10, r0, r0     # accumulator
    add r11, r0, r0     # sign flag

    ldbu r8, r6, 0
    addi r9, r0, 45     # '-'
    bne r8, r9, number_loop
    addi r11, r0, 1     # negative
    addi r7, r7, 1
    addi r6, r6, 1

number_loop:
    bge r7, r5, number_done
    ldbu r8, r6, 0
    addi r9, r0, 97         # 'a'
    blt r8, r9, number_no_upper
    addi r9, r0, 122        # 'z'
    bgt r8, r9, number_no_upper
    addi r8, r8, -32        # uppercase
number_no_upper:
    addi r9, r0, 58         # '9'+1
    blt r8, r9, number_digit
    addi r9, r0, 65         # 'A'
    blt r8, r9, number_fail
    addi r9, r0, 91         # 'Z'+1
    bge r8, r9, number_fail
    addi r9, r8, -55        # digit = c - 'A' + 10
    jal r0, number_have_digit

number_digit:
    addi r9, r8, -48        # digit = c - '0'

number_have_digit:
    bge r9, r13, number_fail
    mul r10, r10, r13       # acc *= base
    add r10, r10, r9        # acc += digit
    addi r6, r6, 1
    addi r7, r7, 1
    jal r0, number_loop

number_fail:
    addi r28, r28, -4
    stw r28, r0, 0          # n = 0
    addi r28, r28, -4
    stw r28, r0, 0          # flag = 0
    jal r0, next

number_done:
    beq r11, r0, number_push
    sub r10, r0, r10        # negate if needed
number_push:
    addi r28, r28, -4
    stw r28, r10, 0         # push n
    addi r28, r28, -4
    addi r1, r0, 1
    stw r28, r1, 0          # flag = 1
    jal r0, next

# Word: INTERPRET ( -- ) processes current TIB
.text
    .align 2
head_interpret:
    .word head_number
    .byte 9
    .ascii "INTERPRET"
    .align 2
xt_interpret:
    .word interpret_word
interpret_word:
interpret_loop:
    # Parse next token via WORD
    lui r7, %hi(xt_word)
    addi r7, r7, %lo(xt_word)
    lui r13, %hi(interpret_after_word)
    addi r13, r13, %lo(interpret_after_word)
    jal r0, interpret_run_xt

interpret_after_word:
    ldw r4, r28, 0           # c-addr
    ldbu r5, r4, 0           # len
    beq r5, r0, interpret_done

    # Duplicate address for number path (addr -> addr addr)
    addi r28, r28, -4
    stw r28, r4, 0

    # FIND on top address -> xt flag
    lui r7, %hi(xt_find)
    addi r7, r7, %lo(xt_find)
    lui r13, %hi(interpret_after_find)
    addi r13, r13, %lo(interpret_after_find)
    jal r0, interpret_run_xt

interpret_after_find:
    ldw r6, r28, 0           # flag
    ldw r7, r28, 4           # xt
    bne r7, r0, interpret_found

    # Not found: drop flag+xt to expose addr
    addi r28, r28, 8         # pop flag and xt
    lui r7, %hi(xt_number)
    addi r7, r7, %lo(xt_number)
    lui r13, %hi(interpret_after_number)
    addi r13, r13, %lo(interpret_after_number)
    jal r0, interpret_run_xt

interpret_after_number:
    ldw r6, r28, 0           # flag
    ldw r7, r28, 4           # n
    beq r6, r0, interpret_error

    # Number succeeded
    lui r8, %hi(var_state)
    addi r8, r8, %lo(var_state)
    ldw r9, r8, 0
    beq r9, r0, interpret_number_interp

    # Compiling: compile LIT and literal
    addi r28, r28, 8         # drop n and flag
    lui r10, %hi(var_here)
    addi r10, r10, %lo(var_here)
    ldw r11, r10, 0          # HERE
    lui r12, %hi(xt_lit)
    addi r12, r12, %lo(xt_lit)
    stw r11, r12, 0
    addi r11, r11, 4
    stw r11, r7, 0
    addi r11, r11, 4
    stw r10, r11, 0          # update HERE
    jal r0, interpret_loop

interpret_number_interp:
    addi r28, r28, 4         # drop flag, keep n
    jal r0, interpret_loop

interpret_found:
    # Stack: flag (TOS), xt
    addi r28, r28, 12        # drop flag, xt, and original c-addr
    lui r8, %hi(var_state)
    addi r8, r8, %lo(var_state)
    ldw r9, r8, 0
    beq r9, r0, interpret_execute

    # Compiling
    bne r6, r0, interpret_found_immediate

    # Compile xt
    lui r10, %hi(var_here)
    addi r10, r10, %lo(var_here)
    ldw r11, r10, 0
    stw r11, r7, 0
    addi r11, r11, 4
    stw r10, r11, 0
    jal r0, interpret_loop

interpret_found_immediate:
    add r13, r0, r0          # resume = interpret_loop
    jal r0, interpret_run_xt

interpret_execute:
    add r13, r0, r0          # resume = interpret_loop
    jal r0, interpret_run_xt

interpret_run_xt:
    # r7 = XT to run
    # r13 = resume target (0 => interpret_loop)
    beq r13, r0, interpret_default_resume
    jal r0, interpret_resume_set
interpret_default_resume:
    lui r13, %hi(interpret_loop)
    addi r13, r13, %lo(interpret_loop)
interpret_resume_set:
    lui r12, %hi(interp_resume_target)
    addi r12, r12, %lo(interp_resume_target)
    stw r12, r13, 0          # save resume target

    # Save caller IP so we can resume after executing XT
    lui r10, %hi(interp_saved_ip)
    addi r10, r10, %lo(interp_saved_ip)
    stw r10, r26, 0

    # Build tiny thread: XT, interpret_resume
    lui r11, %hi(interp_exec_thread)
    addi r11, r11, %lo(interp_exec_thread)
    stw r11, r7, 0           # thread[0] = XT

    add r26, r11, r0         # IP = &thread[0]
    jal r0, next             # run XT, then interpret_resume

interpret_resume:
    lui r1, %hi(interp_saved_ip)
    addi r1, r1, %lo(interp_saved_ip)
    ldw r26, r1, 0           # restore caller IP

    lui r2, %hi(interp_resume_target)
    addi r2, r2, %lo(interp_resume_target)
    ldw r2, r2, 0
    jalr r0, r2, 0           # jump to resume target

interpret_error:
    addi r28, r28, 8         # drop n and flag
    addi r3, r0, 63          # '?'
    debug r3
    addi r3, r0, 10          # newline
    debug r3
    jal r0, interpret_loop

interpret_done:
    addi r28, r28, 4         # drop c-addr when len==0
    jal r0, next

# Word: COMMA ( x -- )  store cell at HERE and advance
.text
    .align 2
head_comma:
    .word head_interpret
    .byte 1
    .ascii ","
    .align 2
xt_comma:
    .word comma_word
comma_word:
    ldw r1, r28, 0     # x
    addi r28, r28, 4   # pop
    lui r2, %hi(var_here)
    addi r2, r2, %lo(var_here)
    ldw r3, r2, 0      # HERE
    stw r3, r1, 0
    addi r3, r3, 4
    stw r2, r3, 0      # HERE += 4
    jal r0, next

# Word: ALLOT ( n -- )  adjust HERE by n bytes
.text
    .align 2
head_allot:
    .word head_comma
    .byte 5
    .ascii "ALLOT"
    .align 2
xt_allot:
    .word allot_word
allot_word:
    ldw r1, r28, 0     # n
    addi r28, r28, 4
    lui r2, %hi(var_here)
    addi r2, r2, %lo(var_here)
    ldw r3, r2, 0
    add r3, r3, r1     # HERE += n
    stw r2, r3, 0
    jal r0, next

# Word: [ ( -- )  enter interpret state (IMMEDIATE)
.text
    .align 2
head_lbrac:
    .word head_allot
    .byte 0x81
    .ascii "["
    .align 2
xt_lbrac:
    .word lbrac_word
lbrac_word:
    lui r1, %hi(var_state)
    addi r1, r1, %lo(var_state)
    stw r1, r0, 0      # STATE := 0
    jal r0, next

# Word: ] ( -- )  enter compile state
.text
    .align 2
head_rbrac:
    .word head_lbrac
    .byte 1
    .ascii "]"
    .align 2
xt_rbrac:
    .word rbrac_word
rbrac_word:
    lui r1, %hi(var_state)
    addi r1, r1, %lo(var_state)
    addi r2, r0, 1
    stw r1, r2, 0      # STATE := 1
    jal r0, next

# Word: IMMEDIATE ( -- )  set IMMEDIATE bit on latest
.text
    .align 2
head_immediate:
    .word head_rbrac
    .byte 0x89
    .ascii "IMMEDIATE"
    .align 2
xt_immediate:
    .word immediate_word
immediate_word:
    lui r1, %hi(var_latest)
    addi r1, r1, %lo(var_latest)
    ldw r2, r1, 0      # latest header
    beq r2, r0, immediate_done
    addi r2, r2, 4     # len byte
    ldbu r3, r2, 0
    addi r4, r0, 0x80
    or r3, r3, r4
    stb r2, r3, 0
immediate_done:
    jal r0, next

# Word: ; ( -- )  IMMEDIATE, finish current definition
.text
    .align 2
head_semicolon:
    .word head_immediate
    .byte 0x81
    .ascii ";"
    .align 2
xt_semicolon:
    .word semicolon_word
semicolon_word:
    # Compile EXIT
    lui r1, %hi(var_here)
    addi r1, r1, %lo(var_here)
    ldw r2, r1, 0      # HERE
    lui r3, %hi(xt_exit)
    addi r3, r3, %lo(xt_exit)
    stw r2, r3, 0
    addi r2, r2, 4
    stw r1, r2, 0      # HERE += 4

    # STATE := 0
    lui r4, %hi(var_state)
    addi r4, r4, %lo(var_state)
    stw r4, r0, 0
    jal r0, next

# Word: : ( -- )  IMMEDIATE, start a new colon definition
.text
    .align 2
head_colon:
    .word head_semicolon
    .byte 0x81
    .ascii ":"
    .align 2
xt_colon:
    .word colon_word
colon_word:
    # Parse next name directly (similar to WORD but no stack effect)
    lui r1, %hi(tib)
    addi r1, r1, %lo(tib)   # r1 = TIB base
    lui r2, %hi(var_source_len)
    addi r2, r2, %lo(var_source_len)
    ldw r2, r2, 0           # #TIB
    lui r3, %hi(var_to_in)
    addi r3, r3, %lo(var_to_in)
    ldw r4, r3, 0           # >IN

colon_skip:
    bge r4, r2, colon_done  # no name available
    add r5, r1, r4
    ldbu r6, r5, 0
    addi r7, r0, 32
    bne r6, r7, colon_found
    addi r4, r4, 1
    jal r0, colon_skip

colon_found:
    add r11, r1, r4         # start addr
    add r12, r4, r0         # start index

colon_scan:
    bge r4, r2, colon_end
    add r5, r1, r4
    ldbu r6, r5, 0
    addi r7, r0, 32
    beq r6, r7, colon_end
    addi r4, r4, 1
    jal r0, colon_scan

colon_end:
    sub r13, r4, r12        # len
    stw r3, r4, 0           # update >IN
    beq r13, r0, colon_done

    # Build counted string in PAD (uppercased)
    lui r14, %hi(pad)
    addi r14, r14, %lo(pad)
    stb r14, r13, 0
    addi r15, r14, 1        # dest pointer
    add r16, r11, r0        # src pointer
    add r17, r0, r0         # i = 0

colon_copy_loop:
    bge r17, r13, colon_copy_done
    ldbu r18, r16, 0
    addi r19, r0, 97        # 'a'
    blt r18, r19, colon_no_upper
    addi r19, r0, 122       # 'z'
    bgt r18, r19, colon_no_upper
    addi r18, r18, -32      # uppercase
colon_no_upper:
    stb r15, r18, 0
    addi r15, r15, 1
    addi r16, r16, 1
    addi r17, r17, 1
    jal r0, colon_copy_loop

colon_copy_done:
    # HERE pointer
    lui r3, %hi(var_here)
    addi r3, r3, %lo(var_here)
    ldw r4, r3, 0       # r4 = HERE
    add r12, r4, r0     # remember header start

    # Link
    lui r5, %hi(var_latest)
    addi r5, r5, %lo(var_latest)
    ldw r6, r5, 0       # previous latest
    stw r4, r6, 0
    addi r4, r4, 4

    # Length byte (mask off immediate bit)
    addi r7, r0, 0x7F
    and r2, r13, r7
    stb r4, r2, 0
    addi r4, r4, 1

    # Copy name characters from PAD+1
    addi r8, r14, 1     # src = PAD+1
    add r9, r0, r0      # i = 0
colon_header_copy:
    bge r9, r13, colon_header_done
    ldbu r10, r8, 0
    stb r4, r10, 0
    addi r4, r4, 1
    addi r8, r8, 1
    addi r9, r9, 1
    jal r0, colon_header_copy
colon_header_done:
    # Align to 4-byte boundary
    addi r10, r0, 3
    add r4, r4, r10
    addi r10, r0, -4
    and r4, r4, r10

    # Codeword = DOCOL
    lui r11, %hi(docol_word)
    addi r11, r11, %lo(docol_word)
    stw r4, r11, 0
    addi r4, r4, 4

    # Update HERE and LATEST
    stw r3, r4, 0       # HERE = body start
    stw r5, r12, 0      # LATEST = new header

    # Enter compile state
    lui r13, %hi(var_state)
    addi r13, r13, %lo(var_state)
    addi r14, r0, 1
    stw r13, r14, 0

colon_done:
    jal r0, next

# Word: IF ( -- patch ) IMMEDIATE
.text
    .align 2
head_if:
    .word head_colon
    .byte 0x82
    .ascii "IF"
    .align 2
xt_if:
    .word if_word
if_word:
    lui r8, %hi(var_state)
    addi r8, r8, %lo(var_state)
    ldw r8, r8, 0
    beq r8, r0, if_done
    # HERE
    lui r1, %hi(var_here)
    addi r1, r1, %lo(var_here)
    ldw r2, r1, 0          # r2 = HERE
    # Compile 0BRANCH XT
    lui r3, %hi(xt_0branch)
    addi r3, r3, %lo(xt_0branch)
    stw r2, r3, 0
    addi r2, r2, 4
    # Placeholder
    stw r2, r0, 0
    addi r2, r2, 4
    stw r1, r2, 0          # HERE = r2
    # Push patch address (offset cell)
    addi r4, r2, -4
    addi r28, r28, -4
    stw r28, r4, 0
if_done:
    jal r0, next

# Word: ELSE ( patch -- patch2 ) IMMEDIATE
.text
    .align 2
head_else:
    .word head_if
    .byte 0x84
    .ascii "ELSE"
    .align 2
xt_else:
    .word else_word
else_word:
    lui r8, %hi(var_state)
    addi r8, r8, %lo(var_state)
    ldw r8, r8, 0
    beq r8, r0, else_done
    ldw r4, r28, 0     # old patch addr
    addi r28, r28, 4

    # current HERE
    lui r1, %hi(var_here)
    addi r1, r1, %lo(var_here)
    ldw r2, r1, 0

    # Compile BRANCH with placeholder
    lui r3, %hi(xt_branch)
    addi r3, r3, %lo(xt_branch)
    stw r2, r3, 0
    addi r2, r2, 4
    stw r2, r0, 0      # placeholder
    addi r2, r2, 4
    stw r1, r2, 0      # update HERE

    # Patch old IF to jump here (after branch placeholder)
    addi r5, r2, 0     # target = new HERE
    addi r6, r4, 4
    sub r5, r5, r6     # offset
    stw r4, r5, 0

    # Push new patch address (branch placeholder)
    addi r7, r2, -4
    addi r28, r28, -4
    stw r28, r7, 0
else_done:
    jal r0, next

# Word: THEN ( patch -- ) IMMEDIATE
.text
    .align 2
head_then:
    .word head_else
    .byte 0x84
    .ascii "THEN"
    .align 2
xt_then:
    .word then_word
then_word:
    lui r8, %hi(var_state)
    addi r8, r8, %lo(var_state)
    ldw r8, r8, 0
    beq r8, r0, then_done
    ldw r4, r28, 0     # patch addr
    addi r28, r28, 4
    lui r1, %hi(var_here)
    addi r1, r1, %lo(var_here)
    ldw r2, r1, 0      # target = HERE
    addi r3, r4, 4
    sub r2, r2, r3     # offset = target - (patch+4)
    stw r4, r2, 0
then_done:
    jal r0, next

# Word: BEGIN ( -- addr ) IMMEDIATE
.text
    .align 2
head_begin:
    .word head_then
    .byte 0x85
    .ascii "BEGIN"
    .align 2
xt_begin:
    .word begin_word
begin_word:
    lui r8, %hi(var_state)
    addi r8, r8, %lo(var_state)
    ldw r8, r8, 0
    beq r8, r0, begin_done
    lui r1, %hi(var_here)
    addi r1, r1, %lo(var_here)
    ldw r2, r1, 0
    addi r28, r28, -4
    stw r28, r2, 0
begin_done:
    jal r0, next

# Word: AGAIN ( addr -- ) IMMEDIATE
.text
    .align 2
head_again:
    .word head_begin
    .byte 0x85
    .ascii "AGAIN"
    .align 2
xt_again:
    .word again_word
again_word:
    lui r8, %hi(var_state)
    addi r8, r8, %lo(var_state)
    ldw r8, r8, 0
    beq r8, r0, again_done
    ldw r4, r28, 0     # target addr (begin)
    addi r28, r28, 4
    lui r1, %hi(var_here)
    addi r1, r1, %lo(var_here)
    ldw r2, r1, 0      # HERE
    lui r3, %hi(xt_branch)
    addi r3, r3, %lo(xt_branch)
    stw r2, r3, 0
    addi r2, r2, 4
    # offset back
    addi r5, r2, 4     # patch addr +4
    sub r5, r4, r5     # offset = target - (patch+4)
    stw r2, r5, 0
    addi r2, r2, 4
    stw r1, r2, 0
again_done:
    jal r0, next

# Word: UNTIL ( addr -- ) IMMEDIATE
.text
    .align 2
head_until:
    .word head_again
    .byte 0x85
    .ascii "UNTIL"
    .align 2
xt_until:
    .word until_word
until_word:
    lui r8, %hi(var_state)
    addi r8, r8, %lo(var_state)
    ldw r8, r8, 0
    beq r8, r0, until_done
    ldw r4, r28, 0     # begin addr
    addi r28, r28, 4
    lui r1, %hi(var_here)
    addi r1, r1, %lo(var_here)
    ldw r2, r1, 0      # HERE
    lui r3, %hi(xt_0branch)
    addi r3, r3, %lo(xt_0branch)
    stw r2, r3, 0
    addi r2, r2, 4
    addi r5, r2, 4
    sub r5, r4, r5
    stw r2, r5, 0
    addi r2, r2, 4
    stw r1, r2, 0
until_done:
    jal r0, next

# Word: WHILE ( addr -- addr patch ) IMMEDIATE
.text
    .align 2
head_while:
    .word head_until
    .byte 0x85
    .ascii "WHILE"
    .align 2
xt_while:
    .word while_word
while_word:
    lui r8, %hi(var_state)
    addi r8, r8, %lo(var_state)
    ldw r8, r8, 0
    beq r8, r0, while_done
    ldw r4, r28, 0     # begin addr
    addi r28, r28, 4   # pop begin
    lui r1, %hi(var_here)
    addi r1, r1, %lo(var_here)
    ldw r2, r1, 0      # HERE
    lui r3, %hi(xt_0branch)
    addi r3, r3, %lo(xt_0branch)
    stw r2, r3, 0
    addi r2, r2, 4
    stw r2, r0, 0      # placeholder
    addi r2, r2, 4
    stw r1, r2, 0      # HERE update
    # push begin then patch addr (stack: begin patch)
    addi r28, r28, -4
    stw r28, r4, 0     # begin
    addi r5, r2, -4
    addi r28, r28, -4
    stw r28, r5, 0     # patch
while_done:
    jal r0, next

# Word: REPEAT ( begin patch -- ) IMMEDIATE
.text
    .align 2
head_repeat:
    .word head_while
    .byte 0x86
    .ascii "REPEAT"
    .align 2
xt_repeat:
    .word repeat_word
repeat_word:
    lui r8, %hi(var_state)
    addi r8, r8, %lo(var_state)
    ldw r8, r8, 0
    beq r8, r0, repeat_done
    ldw r5, r28, 0     # patch addr
    ldw r4, r28, 4     # begin addr
    addi r28, r28, 8
    lui r1, %hi(var_here)
    addi r1, r1, %lo(var_here)
    ldw r2, r1, 0      # HERE
    # Compile BRANCH back to begin
    lui r3, %hi(xt_branch)
    addi r3, r3, %lo(xt_branch)
    stw r2, r3, 0
    addi r2, r2, 4
    addi r6, r2, 4
    sub r6, r4, r6
    stw r2, r6, 0
    addi r2, r2, 4
    stw r1, r2, 0
    # Patch forward placeholder to HERE
    addi r7, r5, 4
    sub r7, r2, r7
    stw r5, r7, 0
repeat_done:
    jal r0, next

# Word: XOR ( a b -- a^b )
.text
    .align 2
head_xor:
    .word head_repeat
    .byte 3
    .ascii "XOR"
    .align 2
xt_xor:
    .word xor_word
xor_word:
    ldw r1, r28, 0
    ldw r2, r28, 4
    xor r2, r2, r1
    addi r28, r28, 4
    stw r28, r2, 0
    jal r0, next

# Word: INVERT ( x -- ~x )
.text
    .align 2
head_invert:
    .word head_xor
    .byte 6
    .ascii "INVERT"
    .align 2
xt_invert:
    .word invert_word
invert_word:
    ldw r1, r28, 0
    addi r2, r0, -1    # r2 = 0xFFFFFFFF
    xor r1, r1, r2     # bitwise NOT (full 32-bit)
    stw r28, r1, 0
    jal r0, next

# Word: 0= ( x -- flag )
.text
    .align 2
head_zero_equal:
    .word head_invert
    .byte 2
    .ascii "0="
    .align 2
xt_zero_equal:
    .word zero_equal_word
zero_equal_word:
    ldw r1, r28, 0
    addi r2, r0, 0
    beq r1, r0, zero_equal_set
    jal r0, zero_equal_push
zero_equal_set:
    addi r2, r0, 1
zero_equal_push:
    stw r28, r2, 0
    jal r0, next

# Word: 0< ( x -- flag ) signed
.text
    .align 2
head_zero_less:
    .word head_zero_equal
    .byte 2
    .ascii "0<"
    .align 2
xt_zero_less:
    .word zero_less_word
zero_less_word:
    ldw r1, r28, 0
    addi r2, r0, 0
    blt r1, r0, zero_less_set
    jal r0, zero_less_push
zero_less_set:
    addi r2, r0, 1
zero_less_push:
    stw r28, r2, 0
    jal r0, next

# Word: > ( a b -- flag ) signed
.text
    .align 2
head_greater:
    .word head_zero_less
    .byte 1
    .ascii ">"
    .align 2
xt_greater:
    .word greater_word
greater_word:
    ldw r1, r28, 0     # b
    ldw r2, r28, 4     # a
    addi r3, r0, 0
    blt r1, r2, greater_set
    jal r0, greater_push
greater_set:
    addi r3, r0, 1
greater_push:
    addi r28, r28, 4
    stw r28, r3, 0
    jal r0, next

# Word: <> ( a b -- flag )
.text
    .align 2
head_not_equals:
    .word head_greater
    .byte 2
    .ascii "<>"
    .align 2
xt_not_equals:
    .word not_equals_word
not_equals_word:
    ldw r1, r28, 0
    ldw r2, r28, 4
    addi r3, r0, 1
    bne r1, r2, not_equals_push
    addi r3, r0, 0
not_equals_push:
    addi r28, r28, 4
    stw r28, r3, 0
    jal r0, next

# Word: / ( a b -- a/b ) signed
.text
    .align 2
head_div:
    .word head_not_equals
    .byte 1
    .ascii "/"
    .align 2
xt_div:
    .word div_word
div_word:
    ldw r1, r28, 0     # b
    ldw r2, r28, 4     # a
    div r2, r2, r1
    addi r28, r28, 4
    stw r28, r2, 0
    jal r0, next

# Word: MOD ( a b -- a mod b )
.text
    .align 2
head_mod:
    .word head_div
    .byte 3
    .ascii "MOD"
    .align 2
xt_mod:
    .word mod_word
mod_word:
    ldw r1, r28, 0     # b
    ldw r2, r28, 4     # a
    rem r2, r2, r1
    addi r28, r28, 4
    stw r28, r2, 0
    jal r0, next

# Word: /MOD ( a b -- rem quot )
.text
    .align 2
head_divmod:
    .word head_mod
    .byte 4
    .ascii "/MOD"
    .align 2
xt_divmod:
    .word divmod_word
divmod_word:
    ldw r1, r28, 0     # b
    ldw r2, r28, 4     # a
    div r3, r2, r1     # quot
    rem r4, r2, r1     # rem
    stw r28, r3, 0     # replace top with quot
    stw r28, r4, 4     # second slot with rem
    jal r0, next

# Word: .S ( -- ) print stack contents without altering
.text
    .align 2
head_dot_s:
    .word head_divmod
    .byte 2
    .ascii ".S"
    .align 2
xt_dot_s:
    .word dot_s_word
dot_s_word:
    # Save original DSP
    add r21, r28, r0

    # Load dstack_top into a preserved register (dot_print_sub clobbers r1)
    lui r20, %hi(dstack_top)
    addi r20, r20, %lo(dstack_top)

    # Compute depth = (dstack_top - DSP)/4
    sub r2, r20, r21
    srai r2, r2, 2     # divide by 4

    # Print depth (stack will be restored by DOT)
    addi r28, r21, -4
    stw r28, r2, 0
    jal dot_print_sub

    # Walk from top (current DSP) to dstack_top
    add r22, r21, r0   # cursor = DSP
dot_s_loop:
    bge r22, r20, dot_s_done
    ldw r6, r22, 0     # value
    addi r28, r21, -4  # temporary push at original DSP
    stw r28, r6, 0
    jal dot_print_sub  # prints value, restores r28 to r21
    addi r22, r22, 4
    jal r0, dot_s_loop

dot_s_done:
    add r28, r21, r0   # ensure DSP unchanged
    jal r0, next

# Word: DEPTH ( -- n )
.text
    .align 2
head_depth:
    .word head_dot_s
    .byte 5
    .ascii "DEPTH"
    .align 2
xt_depth:
    .word depth_word
depth_word:
    lui r1, %hi(dstack_top)
    addi r1, r1, %lo(dstack_top)
    sub r2, r1, r28
    srai r2, r2, 2
    addi r28, r28, -4
    stw r28, r2, 0
    jal r0, next

# Word: DSP@ ( -- addr ) return data stack pointer
.text
    .align 2
head_dsp_fetch:
    .word head_depth
    .byte 4
    .ascii "DSP@"
    .align 2
xt_dsp_fetch:
    .word dsp_fetch_word
dsp_fetch_word:
    add r1, r28, r0     # save DSP
    addi r28, r28, -4
    stw r28, r1, 0
    jal r0, next

# Word: DSTKTOP ( -- addr ) address of data stack top
.text
    .align 2
head_dstack_top:
    .word head_dsp_fetch
    .byte 7
    .ascii "DSTKTOP"
    .align 2
xt_dstack_top:
    .word dstack_top_word
dstack_top_word:
    lui r1, %hi(dstack_top)
    addi r1, r1, %lo(dstack_top)
    addi r28, r28, -4
    stw r28, r1, 0
    jal r0, next

# Word: C! ( c addr -- ) store byte
.text
    .align 2
head_cstore:
    .word head_dstack_top
    .byte 2
    .ascii "C!"
    .align 2
xt_cstore:
    .word cstore_word
cstore_word:
    ldw r1, r28, 0     # addr
    ldw r2, r28, 4     # c
    addi r28, r28, 8   # pop both
    stb r1, r2, 0      # *addr = c (byte)
    jal r0, next

# Word: LSHIFT ( x n -- x<<n )
.text
    .align 2
head_lshift:
    .word head_cstore
    .byte 6
    .ascii "LSHIFT"
    .align 2
xt_lshift:
    .word lshift_word
lshift_word:
    ldw r1, r28, 0     # n
    ldw r2, r28, 4     # x
    sll r2, r2, r1
    addi r28, r28, 4
    stw r28, r2, 0
    jal r0, next

# Word: RSHIFT ( x n -- x>>n ) logical
.text
    .align 2
head_rshift:
    .word head_lshift
    .byte 6
    .ascii "RSHIFT"
    .align 2
xt_rshift:
    .word rshift_word
rshift_word:
    ldw r1, r28, 0     # n
    ldw r2, r28, 4     # x
    srl r2, r2, r1
    addi r28, r28, 4
    stw r28, r2, 0
    jal r0, next

# Word: NEGATE ( n -- -n )
.text
    .align 2
head_negate:
    .word head_rshift
    .byte 6
    .ascii "NEGATE"
    .align 2
xt_negate:
    .word negate_word
negate_word:
    ldw r1, r28, 0
    sub r1, r0, r1
    stw r28, r1, 0
    jal r0, next

# Word: 1+ ( n -- n+1 )
.text
    .align 2
head_one_plus:
    .word head_negate
    .byte 2
    .ascii "1+"
    .align 2
xt_one_plus:
    .word one_plus_word
one_plus_word:
    ldw r1, r28, 0
    addi r1, r1, 1
    stw r28, r1, 0
    jal r0, next

# Word: 1- ( n -- n-1 )
.text
    .align 2
head_one_minus:
    .word head_one_plus
    .byte 2
    .ascii "1-"
    .align 2
xt_one_minus:
    .word one_minus_word
one_minus_word:
    ldw r1, r28, 0
    addi r1, r1, -1
    stw r28, r1, 0
    jal r0, next

# Word: PROMPTS-ON ( -- ) enable interactive prompts
.text
    .align 2
head_prompts_on:
    .word head_one_minus
    .byte 10
    .ascii "PROMPTS-ON"
    .align 2
xt_prompts_on:
    .word prompts_on_word
prompts_on_word:
    lui r1, %hi(var_prompt_enabled)
    addi r1, r1, %lo(var_prompt_enabled)
    addi r2, r0, 1
    stw r1, r2, 0
    jal r0, next

# Word: (DO) ( limit start -- ) runtime for DO
# Word: (DOES>) (Internal) - runtime for DOES>
# Called during defining word execution. Patches LATEST word for DOES> behavior.
# IP points to the does-thread (the XTs after DOES> in the defining word).
.text
    .align 2
xt_does_runtime:
    .word does_runtime_word
does_runtime_word:
    # Find LATEST word's XT address
    lui r1, %hi(var_latest)
    addi r1, r1, %lo(var_latest)
    ldw r2, r1, 0          # r2 = LATEST header
    ldbu r3, r2, 4         # length byte (with possible IMMEDIATE flag)
    addi r4, r0, 0x7F
    and r3, r3, r4         # mask off IMMEDIATE bit
    addi r3, r3, 5         # skip: link(4) + len(1) + name_len
    add r3, r2, r3         # past end of name
    addi r3, r3, 3         # align up
    addi r4, r0, -4
    and r3, r3, r4         # r3 = XT address
    # Patch code field to dodoes
    lui r4, %hi(dodoes)
    addi r4, r4, %lo(dodoes)
    stw r3, r4, 0          # XT[0] = dodoes
    # Store does-thread address (current IP) into does-cell at XT+4
    stw r3, r26, 4         # XT[4] = does-thread address
    # EXIT: pop IP from return stack (don't execute the does-thread now)
    ldw r26, r27, 0
    addi r27, r27, 4
    jal r0, next

# Word: (S") (Internal) - runtime for string literals
# Reads inline [length][string...padded] from IP, pushes ( c-addr u )
.text
    .align 2
xt_sliteral:
    .word sliteral_word
sliteral_word:
    ldw r1, r26, 0         # r1 = length (from inline cell)
    addi r2, r26, 4        # r2 = string start (IP + 4)
    # Advance IP past length cell + padded string
    addi r3, r1, 3         # round up to 4-byte boundary
    addi r4, r0, -4
    and r3, r3, r4         # r3 = padded string length
    addi r26, r26, 4       # skip length cell
    add r26, r26, r3       # skip string data
    # Push ( c-addr u )
    addi r28, r28, -4
    stw r28, r2, 0         # push c-addr
    addi r28, r28, -4
    stw r28, r1, 0         # push u (length, TOS)
    jal r0, next

# Pushes limit then index onto return stack
.text
    .align 2
head_do_runtime:
    .word head_prompts_on
    .byte 4
    .ascii "(DO)"
    .align 2
xt_do_runtime:
    .word do_runtime_word
do_runtime_word:
    ldw r1, r28, 4         # r1 = limit (second on stack)
    ldw r2, r28, 0         # r2 = start/index (top of stack)
    addi r28, r28, 8       # pop both
    addi r27, r27, -4
    stw r27, r1, 0         # push limit onto return stack
    addi r27, r27, -4
    stw r27, r2, 0         # push index onto return stack
    jal r0, next

# Word: (LOOP) ( -- ) runtime for LOOP, inline offset follows
# Increments index by 1. If boundary crossed, exit loop.
.text
    .align 2
head_loop_runtime:
    .word head_do_runtime
    .byte 6
    .ascii "(LOOP)"
    .align 2
xt_loop_runtime:
    .word loop_runtime_word
loop_runtime_word:
    ldw r1, r27, 0         # r1 = index
    ldw r2, r27, 4         # r2 = limit
    sub r3, r1, r2         # r3 = old_index - limit
    addi r4, r1, 1         # r4 = new_index = index + 1
    sub r5, r4, r2         # r5 = new_index - limit
    xor r6, r3, r5         # sign-flip test
    slt r6, r6, r0         # r6 = 1 if sign bit set (boundary crossed)
    bne r6, r0, loop_exit
    # Continue loop: store new index, branch back
    stw r27, r4, 0         # update index on return stack
    ldw r1, r26, 0         # load offset
    addi r26, r26, 4       # skip offset cell
    add r26, r26, r1       # IP += offset (branch back)
    jal r0, next
loop_exit:
    # Exit loop: drop loop params from return stack, skip offset
    addi r27, r27, 8       # drop index and limit
    addi r26, r26, 4       # skip offset cell
    jal r0, next

# Word: (+LOOP) ( n -- ) runtime for +LOOP, inline offset follows
# Adds n to index. If boundary crossed, exit loop.
.text
    .align 2
head_ploop_runtime:
    .word head_loop_runtime
    .byte 7
    .ascii "(+LOOP)"
    .align 2
xt_ploop_runtime:
    .word ploop_runtime_word
ploop_runtime_word:
    ldw r7, r28, 0         # r7 = increment n
    addi r28, r28, 4       # pop n
    ldw r1, r27, 0         # r1 = index
    ldw r2, r27, 4         # r2 = limit
    sub r3, r1, r2         # r3 = old_index - limit
    add r4, r1, r7         # r4 = new_index = index + n
    sub r5, r4, r2         # r5 = new_index - limit
    xor r6, r3, r5         # sign-flip test
    slt r6, r6, r0         # r6 = 1 if sign bit set (boundary crossed)
    bne r6, r0, ploop_exit
    # Continue loop: store new index, branch back
    stw r27, r4, 0         # update index on return stack
    ldw r1, r26, 0         # load offset
    addi r26, r26, 4       # skip offset cell
    add r26, r26, r1       # IP += offset (branch back)
    jal r0, next
ploop_exit:
    # Exit loop: drop loop params from return stack, skip offset
    addi r27, r27, 8       # drop index and limit
    addi r26, r26, 4       # skip offset cell
    jal r0, next

# Word: I ( -- n ) push current loop index to data stack
.text
    .align 2
head_i:
    .word head_ploop_runtime
    .byte 1
    .ascii "I"
    .align 2
xt_i:
    .word i_word
i_word:
    ldw r1, r27, 0         # index is at RSP[0]
    addi r28, r28, -4
    stw r28, r1, 0
    jal r0, next

# Word: J ( -- n ) push outer loop index to data stack
.text
    .align 2
head_j:
    .word head_i
    .byte 1
    .ascii "J"
    .align 2
xt_j:
    .word j_word
j_word:
    ldw r1, r27, 8         # outer index is at RSP[8] (skip inner index + inner limit)
    addi r28, r28, -4
    stw r28, r1, 0
    jal r0, next

# Word: UNLOOP ( -- ) drop loop params from return stack
.text
    .align 2
head_unloop:
    .word head_j
    .byte 6
    .ascii "UNLOOP"
    .align 2
xt_unloop:
    .word unloop_word
unloop_word:
    addi r27, r27, 8       # drop index and limit from return stack
    jal r0, next

# Word: DO ( -- addr ) IMMEDIATE compile-time
# Compiles (DO) xt, pushes HERE as loop-back target
.text
    .align 2
head_do:
    .word head_unloop
    .byte 0x82
    .ascii "DO"
    .align 2
xt_do:
    .word do_word
do_word:
    lui r8, %hi(var_state)
    addi r8, r8, %lo(var_state)
    ldw r8, r8, 0
    beq r8, r0, do_done
    lui r1, %hi(var_here)
    addi r1, r1, %lo(var_here)
    ldw r2, r1, 0          # r2 = HERE
    lui r3, %hi(xt_do_runtime)
    addi r3, r3, %lo(xt_do_runtime)
    stw r2, r3, 0          # compile (DO) xt
    addi r2, r2, 4
    stw r1, r2, 0          # update HERE
    # Save current leave-list head, reset for this loop
    lui r4, %hi(var_leave_list)
    addi r4, r4, %lo(var_leave_list)
    ldw r5, r4, 0          # r5 = old leave-list head
    stw r4, r0, 0          # var_leave_list = 0 (empty for new loop)
    addi r28, r28, -4
    stw r28, r5, 0         # push old leave-list head
    addi r28, r28, -4
    stw r28, r2, 0         # push HERE (loop-back target)
do_done:
    jal r0, next

# Word: LOOP ( addr -- ) IMMEDIATE compile-time
# Compiles (LOOP) xt + backward offset to loop-back target
.text
    .align 2
head_loop:
    .word head_do
    .byte 0x84
    .ascii "LOOP"
    .align 2
xt_loop:
    .word loop_word
loop_word:
    lui r8, %hi(var_state)
    addi r8, r8, %lo(var_state)
    ldw r8, r8, 0
    beq r8, r0, loop_done
    ldw r4, r28, 0         # r4 = loop-back target addr
    ldw r10, r28, 4        # r10 = old leave-list head (saved by DO)
    addi r28, r28, 8       # pop both
    lui r1, %hi(var_here)
    addi r1, r1, %lo(var_here)
    ldw r2, r1, 0          # r2 = HERE
    lui r3, %hi(xt_loop_runtime)
    addi r3, r3, %lo(xt_loop_runtime)
    stw r2, r3, 0          # compile (LOOP) xt
    addi r2, r2, 4         # r2 = offset cell addr
    addi r5, r2, 4         # r5 = offset cell addr + 4
    sub r5, r4, r5         # offset = target - (offset_cell + 4)
    stw r2, r5, 0          # compile offset
    addi r2, r2, 4
    stw r1, r2, 0          # update HERE; r2 = new HERE (past LOOP)
    # Patch all LEAVE forward branches from var_leave_list
    lui r8, %hi(var_leave_list)
    addi r8, r8, %lo(var_leave_list)
    ldw r9, r8, 0          # r9 = current leave-list head
loop_patch_leaves:
    beq r9, r0, loop_patch_done
    ldw r5, r9, 0          # r5 = next link from placeholder
    addi r6, r9, 4         # r6 = patch_addr + 4
    sub r6, r2, r6         # offset = HERE - (patch_addr + 4)
    stw r9, r6, 0          # overwrite placeholder with real offset
    add r9, r5, r0         # advance to next
    jal r0, loop_patch_leaves
loop_patch_done:
    # Restore outer loop's leave-list
    stw r8, r10, 0         # var_leave_list = old head
loop_done:
    jal r0, next

# Word: +LOOP ( addr -- ) IMMEDIATE compile-time
# Compiles (+LOOP) xt + backward offset to loop-back target
.text
    .align 2
head_ploop:
    .word head_loop
    .byte 0x85
    .ascii "+LOOP"
    .align 2
xt_ploop:
    .word ploop_word
ploop_word:
    lui r8, %hi(var_state)
    addi r8, r8, %lo(var_state)
    ldw r8, r8, 0
    beq r8, r0, ploop_done
    ldw r4, r28, 0         # r4 = loop-back target addr
    ldw r10, r28, 4        # r10 = old leave-list head (saved by DO)
    addi r28, r28, 8       # pop both
    lui r1, %hi(var_here)
    addi r1, r1, %lo(var_here)
    ldw r2, r1, 0          # r2 = HERE
    lui r3, %hi(xt_ploop_runtime)
    addi r3, r3, %lo(xt_ploop_runtime)
    stw r2, r3, 0          # compile (+LOOP) xt
    addi r2, r2, 4         # r2 = offset cell addr
    addi r5, r2, 4         # r5 = offset cell addr + 4
    sub r5, r4, r5         # offset = target - (offset_cell + 4)
    stw r2, r5, 0          # compile offset
    addi r2, r2, 4
    stw r1, r2, 0          # update HERE; r2 = new HERE (past +LOOP)
    # Patch all LEAVE forward branches from var_leave_list
    lui r8, %hi(var_leave_list)
    addi r8, r8, %lo(var_leave_list)
    ldw r9, r8, 0          # r9 = current leave-list head
ploop_patch_leaves:
    beq r9, r0, ploop_patch_done
    ldw r5, r9, 0          # r5 = next link from placeholder
    addi r6, r9, 4         # r6 = patch_addr + 4
    sub r6, r2, r6         # offset = HERE - (patch_addr + 4)
    stw r9, r6, 0          # overwrite placeholder with real offset
    add r9, r5, r0         # advance to next
    jal r0, ploop_patch_leaves
ploop_patch_done:
    # Restore outer loop's leave-list
    stw r8, r10, 0         # var_leave_list = old head
ploop_done:
    jal r0, next

# Word: LEAVE ( -- ) IMMEDIATE compile-time
# Compiles UNLOOP + BRANCH + placeholder offset, chains into leave-list
.text
    .align 2
head_leave:
    .word head_ploop
    .byte 0x85
    .ascii "LEAVE"
    .align 2
xt_leave:
    .word leave_word
leave_word:
    lui r8, %hi(var_state)
    addi r8, r8, %lo(var_state)
    ldw r8, r8, 0
    beq r8, r0, leave_done
    lui r1, %hi(var_here)
    addi r1, r1, %lo(var_here)
    ldw r2, r1, 0          # r2 = HERE
    # Compile xt_unloop at HERE
    lui r3, %hi(xt_unloop)
    addi r3, r3, %lo(xt_unloop)
    stw r2, r3, 0          # [HERE] = xt_unloop
    # Compile xt_branch at HERE+4
    lui r3, %hi(xt_branch)
    addi r3, r3, %lo(xt_branch)
    stw r2, r3, 4          # [HERE+4] = xt_branch
    # HERE+8 = offset placeholder cell
    addi r4, r2, 8         # r4 = address of placeholder cell
    # Chain into var_leave_list: placeholder stores old head, variable gets new head
    lui r6, %hi(var_leave_list)
    addi r6, r6, %lo(var_leave_list)
    ldw r5, r6, 0          # r5 = old leave-list head
    stw r4, r5, 0          # [placeholder] = old head (forward link)
    stw r6, r4, 0          # var_leave_list = this placeholder addr
    # Update HERE past the 3 cells
    addi r2, r2, 12
    stw r1, r2, 0          # update HERE
leave_done:
    jal r0, next

# Word: CREATE ( "name" -- ) parse name, build header with docreate
.text
    .align 2
head_create:
    .word head_leave
    .byte 6
    .ascii "CREATE"
    .align 2
xt_create:
    .word create_word
create_word:
    # Parse next name (same logic as colon_word)
    lui r1, %hi(tib)
    addi r1, r1, %lo(tib)
    lui r2, %hi(var_source_len)
    addi r2, r2, %lo(var_source_len)
    ldw r2, r2, 0
    lui r3, %hi(var_to_in)
    addi r3, r3, %lo(var_to_in)
    ldw r4, r3, 0
create_skip:
    bge r4, r2, create_done
    add r5, r1, r4
    ldbu r6, r5, 0
    addi r7, r0, 32
    bne r6, r7, create_found
    addi r4, r4, 1
    jal r0, create_skip
create_found:
    add r11, r1, r4
    add r12, r4, r0
create_scan:
    bge r4, r2, create_end
    add r5, r1, r4
    ldbu r6, r5, 0
    addi r7, r0, 32
    beq r6, r7, create_end
    addi r4, r4, 1
    jal r0, create_scan
create_end:
    sub r13, r4, r12
    stw r3, r4, 0          # update >IN
    beq r13, r0, create_done
    # Build counted string in PAD (uppercased)
    lui r14, %hi(pad)
    addi r14, r14, %lo(pad)
    stb r14, r13, 0
    addi r15, r14, 1
    add r16, r11, r0
    add r17, r0, r0
create_copy_loop:
    bge r17, r13, create_copy_done
    ldbu r18, r16, 0
    addi r19, r0, 97
    blt r18, r19, create_no_upper
    addi r19, r0, 122
    bgt r18, r19, create_no_upper
    addi r18, r18, -32
create_no_upper:
    stb r15, r18, 0
    addi r15, r15, 1
    addi r16, r16, 1
    addi r17, r17, 1
    jal r0, create_copy_loop
create_copy_done:
    # HERE pointer
    lui r3, %hi(var_here)
    addi r3, r3, %lo(var_here)
    ldw r4, r3, 0
    add r12, r4, r0        # remember header start
    # Link
    lui r5, %hi(var_latest)
    addi r5, r5, %lo(var_latest)
    ldw r6, r5, 0
    stw r4, r6, 0
    addi r4, r4, 4
    # Length byte
    addi r7, r0, 0x7F
    and r2, r13, r7
    stb r4, r2, 0
    addi r4, r4, 1
    # Copy name characters from PAD+1
    addi r8, r14, 1
    add r9, r0, r0
create_header_copy:
    bge r9, r13, create_header_done
    ldbu r10, r8, 0
    stb r4, r10, 0
    addi r4, r4, 1
    addi r8, r8, 1
    addi r9, r9, 1
    jal r0, create_header_copy
create_header_done:
    # Align to 4-byte boundary
    addi r10, r0, 3
    add r4, r4, r10
    addi r10, r0, -4
    and r4, r4, r10
    # Codeword = docreate
    lui r11, %hi(docreate)
    addi r11, r11, %lo(docreate)
    stw r4, r11, 0
    addi r4, r4, 4
    # Reserve does-cell (initialized to 0)
    stw r4, r0, 0
    addi r4, r4, 4
    # Update HERE and LATEST (no compile state change)
    stw r3, r4, 0          # HERE = after does-cell
    stw r5, r12, 0         # LATEST = new header
create_done:
    jal r0, next

# Word: DOES> ( -- ) IMMEDIATE compile-time
# Compiles (DOES>) runtime token into current definition
.text
    .align 2
head_does:
    .word head_create
    .byte 0x85
    .ascii "DOES>"
    .align 2
xt_does:
    .word does_compile_word
does_compile_word:
    lui r8, %hi(var_state)
    addi r8, r8, %lo(var_state)
    ldw r8, r8, 0
    beq r8, r0, does_compile_done
    lui r1, %hi(var_here)
    addi r1, r1, %lo(var_here)
    ldw r2, r1, 0          # r2 = HERE
    lui r3, %hi(xt_does_runtime)
    addi r3, r3, %lo(xt_does_runtime)
    stw r2, r3, 0          # compile xt_does_runtime at HERE
    addi r2, r2, 4
    stw r1, r2, 0          # update HERE
does_compile_done:
    jal r0, next

# Word: COUNT ( c-addr -- c-addr+1 u ) convert counted string to addr+len
.text
    .align 2
head_count:
    .word head_does
    .byte 5
    .ascii "COUNT"
    .align 2
xt_count:
    .word count_word
count_word:
    ldw r1, r28, 0         # c-addr
    ldbu r2, r1, 0         # length byte
    addi r1, r1, 1         # c-addr + 1
    stw r28, r1, 0         # replace TOS with c-addr+1 (becomes second)
    addi r28, r28, -4
    stw r28, r2, 0         # push length (new TOS)
    jal r0, next

# Word: S" ( -- c-addr u ) IMMEDIATE compile-time
# Parses to closing ", compiles xt_sliteral + length + string data (padded)
.text
    .align 2
head_squote:
    .word head_count
    .byte 0x82             # length 2 + IMMEDIATE flag (0x80)
    .ascii "S\""
    .align 2
xt_squote:
    .word squote_word
squote_word:
    # Only works in compile mode
    lui r8, %hi(var_state)
    addi r8, r8, %lo(var_state)
    ldw r8, r8, 0
    beq r8, r0, squote_done

    # Compile xt_sliteral at HERE
    lui r14, %hi(var_here)
    addi r14, r14, %lo(var_here)
    ldw r15, r14, 0        # r15 = HERE
    lui r1, %hi(xt_sliteral)
    addi r1, r1, %lo(xt_sliteral)
    stw r15, r1, 0         # compile xt_sliteral
    addi r15, r15, 4       # advance past xt
    add r16, r15, r0       # r16 = address where length will go (fill later)
    addi r15, r15, 4       # skip length cell (will fill after parsing)

    # Parse from TIB: skip one leading space, then copy chars until "
    lui r1, %hi(tib)
    addi r1, r1, %lo(tib)
    lui r2, %hi(var_source_len)
    addi r2, r2, %lo(var_source_len)
    ldw r2, r2, 0          # r2 = #TIB
    lui r3, %hi(var_to_in)
    addi r3, r3, %lo(var_to_in)
    ldw r4, r3, 0          # r4 = >IN

    # Skip exactly one leading space (standard: S" <space>string")
    bge r4, r2, squote_end_parse
    add r5, r1, r4
    ldbu r6, r5, 0
    addi r7, r0, 32        # space
    bne r6, r7, squote_parse_loop  # no leading space, start parsing
    addi r4, r4, 1         # skip the space

squote_parse_loop:
    bge r4, r2, squote_end_parse   # end of input
    add r5, r1, r4
    ldbu r6, r5, 0
    addi r7, r0, 34        # '"' (ASCII 34)
    beq r6, r7, squote_found_quote
    # Copy char to HERE
    stb r15, r6, 0
    addi r15, r15, 1
    addi r4, r4, 1
    jal r0, squote_parse_loop

squote_found_quote:
    addi r4, r4, 1         # skip past closing "

squote_end_parse:
    # Update >IN
    stw r3, r4, 0

    # Calculate string length: r15 (current) - r16 (length cell) - 4
    sub r1, r15, r16
    addi r1, r1, -4        # r1 = string length
    stw r16, r1, 0         # store length at the reserved cell

    # Pad to 4-byte alignment
    addi r2, r15, 3
    addi r3, r0, -4
    and r15, r2, r3        # r15 = aligned HERE

    # Update HERE
    stw r14, r15, 0

squote_done:
    jal r0, next

# Word: ." ( -- ) IMMEDIATE compile-time
# Like S" but also compiles xt_type after the string data
.text
    .align 2
head_dotquote:
    .word head_squote
    .byte 0x82             # length 2 + IMMEDIATE flag (0x80)
    .ascii ".\""
    .align 2
xt_dotquote:
    .word dotquote_word
dotquote_word:
    # Only works in compile mode
    lui r8, %hi(var_state)
    addi r8, r8, %lo(var_state)
    ldw r8, r8, 0
    beq r8, r0, dotquote_done

    # Compile xt_sliteral at HERE
    lui r14, %hi(var_here)
    addi r14, r14, %lo(var_here)
    ldw r15, r14, 0        # r15 = HERE
    lui r1, %hi(xt_sliteral)
    addi r1, r1, %lo(xt_sliteral)
    stw r15, r1, 0         # compile xt_sliteral
    addi r15, r15, 4
    add r16, r15, r0       # r16 = length cell address
    addi r15, r15, 4       # skip length cell

    # Parse from TIB: skip one leading space, then copy chars until "
    lui r1, %hi(tib)
    addi r1, r1, %lo(tib)
    lui r2, %hi(var_source_len)
    addi r2, r2, %lo(var_source_len)
    ldw r2, r2, 0
    lui r3, %hi(var_to_in)
    addi r3, r3, %lo(var_to_in)
    ldw r4, r3, 0

    # Skip exactly one leading space
    bge r4, r2, dotquote_end_parse
    add r5, r1, r4
    ldbu r6, r5, 0
    addi r7, r0, 32
    bne r6, r7, dotquote_parse_loop
    addi r4, r4, 1

dotquote_parse_loop:
    bge r4, r2, dotquote_end_parse
    add r5, r1, r4
    ldbu r6, r5, 0
    addi r7, r0, 34        # '"'
    beq r6, r7, dotquote_found_quote
    stb r15, r6, 0
    addi r15, r15, 1
    addi r4, r4, 1
    jal r0, dotquote_parse_loop

dotquote_found_quote:
    addi r4, r4, 1

dotquote_end_parse:
    stw r3, r4, 0          # update >IN

    # Calculate and store string length
    sub r1, r15, r16
    addi r1, r1, -4
    stw r16, r1, 0

    # Pad to 4-byte alignment
    addi r2, r15, 3
    addi r3, r0, -4
    and r15, r2, r3

    # Also compile xt_type after the string data
    lui r1, %hi(xt_type)
    addi r1, r1, %lo(xt_type)
    stw r15, r1, 0
    addi r15, r15, 4

    # Update HERE
    stw r14, r15, 0

dotquote_done:
    jal r0, next

# Word: ' ( "name" -- xt )
# Parse the next word and return its execution token
.text
    .align 2
head_tick:
    .word head_dotquote
    .byte 1
    .ascii "'"
    .align 2
xt_tick:
    .word docol_word
    .word xt_word
    .word xt_find
    .word xt_drop
    .word xt_exit

# Word: ['] ( "name" -- ) IMMEDIATE compile-time
# At compile time: parse next word, find its XT, compile LIT <xt>
.text
    .align 2
head_bracket_tick:
    .word head_tick
    .byte 0x83             # length 3 + IMMEDIATE flag (0x80)
    .ascii "[']"
    .align 2
xt_bracket_tick:
    .word docol_word
    .word xt_word
    .word xt_find
    .word xt_drop
    .word xt_lit
    .word xt_lit           # pushes xt_lit itself as a literal
    .word xt_comma         # compile xt_lit at HERE
    .word xt_comma         # compile the XT at HERE
    .word xt_exit

# Word: CHAR ( "name" -- char )
# Parse the next word and push its first character
.text
    .align 2
head_char:
    .word head_bracket_tick
    .byte 4
    .ascii "CHAR"
    .align 2
xt_char:
    .word docol_word
    .word xt_word
    .word xt_one_plus
    .word xt_cfetch
    .word xt_exit

# Word: [CHAR] ( "name" -- ) IMMEDIATE compile-time
# At compile time: parse next word, get first char, compile LIT <char>
.text
    .align 2
head_bracket_char:
    .word head_char
    .byte 0x86             # length 6 + IMMEDIATE flag (0x80)
    .ascii "[CHAR]"
    .align 2
xt_bracket_char:
    .word docol_word
    .word xt_word
    .word xt_one_plus
    .word xt_cfetch
    .word xt_lit
    .word xt_lit           # pushes xt_lit itself
    .word xt_comma         # compile xt_lit at HERE
    .word xt_comma         # compile the char value at HERE
    .word xt_exit

# Word: RECURSE ( -- ) IMMEDIATE compile-time
# Compile a call to the word currently being defined.
# Computes XT from LATEST header: aligned(header + 5 + (len & 0x7F))
.text
    .align 2
head_recurse:
    .word head_bracket_char
    .byte 0x87             # length 7 + IMMEDIATE flag (0x80)
    .ascii "RECURSE"
    .align 2
xt_recurse:
    .word docol_word
    .word xt_latest        # push &var_latest
    .word xt_fetch         # read header address
    .word xt_dup           # header header
    .word xt_lit
    .word 4
    .word xt_plus          # header (header+4)
    .word xt_cfetch        # header len_byte
    .word xt_lit
    .word 127
    .word xt_and           # header clean_len
    .word xt_plus          # (header + clean_len)
    .word xt_lit
    .word 8                # 5 (link+len) + 3 (alignment round-up)
    .word xt_plus          # (header + clean_len + 8)
    .word xt_lit
    .word -4               # 0xFFFFFFFC alignment mask
    .word xt_and           # aligned XT
    .word xt_comma         # compile XT at HERE
    .word xt_exit

# Word: POSTPONE ( "name" -- ) IMMEDIATE compile-time
# If next word is IMMEDIATE, compile its XT directly.
# If non-immediate, compile code that will compile it later: LIT <xt> ,
.text
    .align 2
head_postpone:
    .word head_recurse
    .byte 0x88             # length 8 + IMMEDIATE flag (0x80)
    .ascii "POSTPONE"
    .align 2
xt_postpone:
    .word docol_word
    .word xt_word          # parse next token
    .word xt_find          # ( xt flag )
    .word xt_0branch       # if flag=0 (not immediate), jump to else
    .word 12               # skip 3 cells to ELSE
    # IF (immediate word): just compile its XT
    .word xt_comma
    .word xt_branch
    .word 28               # skip 7 cells to EXIT
    # ELSE (non-immediate): compile LIT <xt> ,
    .word xt_lit
    .word xt_lit           # push xt_lit as literal
    .word xt_comma         # compile xt_lit at HERE
    .word xt_comma         # compile the XT at HERE
    .word xt_lit
    .word xt_comma         # push xt_comma as literal
    .word xt_comma         # compile xt_comma at HERE
    # THEN:
    .word xt_exit

# Word: C, ( char -- ) store byte at HERE, advance HERE by 1
.text
    .align 2
head_ccomma:
    .word head_postpone
    .byte 2
    .ascii "C,"
    .align 2
xt_ccomma:
    .word ccomma_word
ccomma_word:
    ldw r1, r28, 0         # char
    addi r28, r28, 4       # pop
    lui r2, %hi(var_here)
    addi r2, r2, %lo(var_here)
    ldw r3, r2, 0          # HERE
    stb r3, r1, 0          # store byte
    addi r3, r3, 1         # HERE += 1
    stw r2, r3, 0          # update HERE
    jal r0, next

# Word: U< ( u1 u2 -- flag ) unsigned less-than
.text
    .align 2
head_ult:
    .word head_ccomma
    .byte 2
    .ascii "U<"
    .align 2
xt_ult:
    .word ult_word
ult_word:
    ldw r1, r28, 0         # b (TOS)
    ldw r2, r28, 4         # a (second)
    addi r3, r0, 0
    bltu r2, r1, ult_set
    jal r0, ult_push
ult_set:
    addi r3, r0, 1
ult_push:
    addi r28, r28, 4
    stw r28, r3, 0
    jal r0, next

# Word: <# ( -- ) Begin pictured numeric output
.text
    .align 2
head_less_sharp:
    .word head_ult
    .byte 2
    .ascii "<#"
    .align 2
xt_less_sharp:
    .word less_sharp_word
less_sharp_word:
    lui r1, %hi(pad)
    addi r1, r1, %lo(pad)
    addi r1, r1, 128          # pad + 128 (one past end)
    lui r2, %hi(var_hld)
    addi r2, r2, %lo(var_hld)
    stw r2, r1, 0             # HLD = pad + 128
    jal r0, next

# Word: HOLD ( char -- ) Prepend char to pictured output buffer
.text
    .align 2
head_hold:
    .word head_less_sharp
    .byte 4
    .ascii "HOLD"
    .align 2
xt_hold:
    .word hold_word
hold_word:
    ldw r1, r28, 0            # char
    addi r28, r28, 4          # pop
    lui r2, %hi(var_hld)
    addi r2, r2, %lo(var_hld)
    ldw r3, r2, 0             # HLD
    addi r3, r3, -1           # --HLD
    stb r3, r1, 0             # *HLD = char
    stw r2, r3, 0             # save updated HLD
    jal r0, next

# Word: #> ( u -- addr len ) End pictured numeric output
.text
    .align 2
head_sharp_greater:
    .word head_hold
    .byte 2
    .ascii "#>"
    .align 2
xt_sharp_greater:
    .word sharp_greater_word
sharp_greater_word:
    lui r2, %hi(var_hld)
    addi r2, r2, %lo(var_hld)
    ldw r3, r2, 0             # HLD = start addr
    lui r4, %hi(pad)
    addi r4, r4, %lo(pad)
    addi r4, r4, 128          # pad + 128 = end
    sub r5, r4, r3            # len = end - HLD
    stw r28, r3, 0            # replace TOS (was u) with addr
    addi r28, r28, -4         # push
    stw r28, r5, 0            # TOS = len
    jal r0, next

# ----------------------------------------------------------------------
# Variables
# ----------------------------------------------------------------------
.data
    .align 2
var_state:      .word 0
var_base:       .word 10
var_here:       .word user_dictionary
var_latest:
    .word head_sharp_greater   # Point to last defined word
var_to_in:      .word 0
var_source_id:  .word 0            # 0 = Console
var_source_len: .word 0
var_prompt_enabled: .word 0        # 0 = suppress prompts (prelude), 1 = show prompts
var_leave_list:     .word 0        # compile-time leave-list head for DO...LOOP
var_hld:            .word 0            # Pictured numeric output pointer into PAD

    .align 2
interp_exec_thread:
    .word 0                  # Placeholder for XT
    .word interp_resume_xt

interp_saved_ip: .word 0     # Saved caller IP for interpreter dispatch
interp_resume_target: .word interpret_loop

interp_resume_xt:
    .word interpret_resume

    .align 2
tib:            .space 128         # Terminal Input Buffer
user_dictionary: .space 65536      # Space for new words
pad:            .space 128         # Scratch pad for strings/numbers


# ----------------------------------------------------------------------
# Boot Program
# ----------------------------------------------------------------------
.data
str_prompt:
    .ascii "ok> "
str_banner:
    .ascii "SLOW-32 Forth\n"

.text
cold_start_body:
    # Initialize BASE to 10
    .word xt_lit, 10
    .word xt_base
    .word xt_store

    # Print banner
    .word xt_lit, str_banner, xt_lit, 14, xt_type

    # REPL loop
cold_loop:
    # Conditional prompt: check var_prompt_enabled
    .word xt_lit, var_prompt_enabled
    .word xt_fetch
    .word xt_0branch, 20     # skip 5 cells if prompts disabled
    .word xt_lit, str_prompt, xt_lit, 4, xt_type

cold_after_prompt:
    .word xt_tib       # TIB address
    .word xt_lit, 128  # Max length
    .word xt_accept    # Returns count (-1 = EOF)

    # Check EOF: count == -1?
    .word xt_dup, xt_lit, -1, xt_equals
    .word xt_0branch, 8   # skip 2 cells if NOT EOF
    .word xt_drop, xt_bye  # EOF: exit

    # Check blank line: count == 0?
    .word xt_dup, xt_zero_equal
    .word xt_0branch, 12   # skip 3 cells if NOT blank
    .word xt_drop
    .word xt_branch, -116  # blank: loop back to cold_loop

    # Normal line: interpret
    .word xt_num_tib   # #TIB address
    .word xt_store     # #TIB !
    .word xt_lit, 0
    .word xt_to_in
    .word xt_store     # >IN !
    .word xt_interpret
    .word xt_branch, -152  # back to cold_loop


.data
    .align 2
dstack_bottom:
    .space 1024
dstack_top:

    .align 2
rstack_bottom:
    .space 1024
rstack_top:
