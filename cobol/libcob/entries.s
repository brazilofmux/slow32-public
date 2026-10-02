# PERFORM's push and the exit's pop (libcob.c, "PERFORM's push and the
# exit's pop are small entries"): written out, no frame, the general
# routine a jump away with the arguments where they were.  libcob/build.sh
# appends this file to the assembly either C compiler makes of libcob.c --
# the self-hosted cc has no file-scope asm, which is where these were
# until it was asked to build the runtime and could not.
#
#   void  cob_perform_push(int *cell, void *ret);
#   void *cob_perform_exit(int *cell);     the place to return to, or 0
#
# cob_pf is { stk, sp, cap, base } and a frame { cell, ret, prev, - }:
# 16 bytes, which the shift below is (the _Static_asserts in libcob.c).
	.text
	.globl	cob_perform_push
	.p2align	2
	.type	cob_perform_push,@function
cob_perform_push:
	lui r5, %hi(cob_pf)
	addi r5, r5, %lo(cob_pf)
	ldw r1, r3+0                       # what the exit's cell holds: a frame of its own, or none
	ldw r6, r5+12                      # base
	ldw r7, r5+4                       # sp
	bgt r1, r6, .Lcob_push_rest        # the range is under way in this activation
	ldw r8, r5+8                       # cap
	beq r7, r8, .Lcob_push_rest        # the stack is full
	ldw r9, r5+0
	slli r2, r7, 4
	add r2, r9, r2
	stw r2+0, r3                       # the frame: the cell, where to return, what the cell held
	stw r2+4, r4
	stw r2+8, r1
	addi r7, r7, 1
	stw r5+4, r7
	stw r3+0, r7
	jalr r0, r31, 0
.Lcob_push_rest:
	jal r0, cob_perform_push_rest
	.size	cob_perform_push, .-cob_perform_push
	.globl	cob_perform_exit
	.p2align	2
	.type	cob_perform_exit,@function
cob_perform_exit:
	lui r5, %hi(cob_pf)
	addi r5, r5, %lo(cob_pf)
	ldw r2, r3+0
	ldw r6, r5+12                      # base
	ldw r7, r5+4                       # sp
	ble r2, r6, .Lcob_exit_none        # another activation's frame is not this one's exit
	bne r2, r7, .Lcob_exit_rest        # not the innermost frame: some are abandoned
	ldw r9, r5+0
	addi r7, r7, -1
	slli r1, r7, 4
	add r9, r9, r1
	stw r5+4, r7
	ldw r1, r9+8                       # what the cell held before this frame
	stw r3+0, r1
	ldw r1, r9+4                       # where to return
	jalr r0, r31, 0
.Lcob_exit_none:
	addi r1, r0, 0
	jalr r0, r31, 0
.Lcob_exit_rest:
	jal r0, cob_perform_exit_rest
	.size	cob_perform_exit, .-cob_perform_exit
