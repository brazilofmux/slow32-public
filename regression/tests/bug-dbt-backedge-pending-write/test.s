	.text
	.globl main
	.p2align 2
main:
	# Seed registers to bias reg allocator away from r7
	addi r1, r0, 1
	addi r2, r0, 2
	addi r3, r0, 3
	addi r4, r0, 4
	addi r5, r0, 5
	addi r6, r0, 6
	addi r7, r0, 0

loop:
	# Force an early ALU op that clobbers host temp (RAX) each iteration
	sltiu r9, r1, 1

	# Loop condition at top (reads r7 before it is rewritten)
	sltiu r10, r7, 200
	beq r10, r0, done

	# Heavy use of r1-r6 to keep them in the reg cache instead of r7
	add r1, r1, r2
	add r1, r1, r3
	add r2, r2, r3
	add r3, r3, r4
	add r4, r4, r5
	add r5, r5, r6
	add r6, r6, r1

	# Update r7 at the end of the loop (pending write candidate)
	addi r7, r7, 1

	# Back-edge
	beq r0, r0, loop

done:
	addi r11, r0, 200
	bne r7, r11, fail
	addi r3, r0, 0x4F  # 'O'
	debug r3
	addi r3, r0, 0x4B  # 'K'
	debug r3
	addi r3, r0, 10    # '\n'
	debug r3
	addi r1, r0, 0     # exit code = 0
	halt

fail:
	addi r3, r0, 0x46  # 'F'
	debug r3
	addi r3, r0, 0x41  # 'A'
	debug r3
	addi r3, r0, 0x49  # 'I'
	debug r3
	addi r3, r0, 0x4C  # 'L'
	debug r3
	addi r3, r0, 10    # '\n'
	debug r3
	addi r1, r0, 0     # exit code = 0
	halt
