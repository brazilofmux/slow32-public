# SLTIU zero-extends its 12-bit immediate (docs/SPEC.md 4.2), like ORI,
# ANDI and XORI.  qemu sign-extended it, so an immediate of 2048 or more
# compared against 0xFFFFF800 and up; no compiler emits one, and
# regression/fuzz/isafuzz.py found it on its first run (2026-10-27).
# Prints one digit per comparison: 0 1 0 1 1 0, then a newline.
	.text
	.globl main
	.p2align 2
main:
	lui r10, 0x3f800        # 0x3F800000
	sltiu r5, r10, 3723     # 0x3F800000 < 0xE8B: no
	addi r6, r0, 48
	add r6, r6, r5
	debug r6
	addi r11, r0, 2047
	sltiu r5, r11, 2048     # 2047 < 2048: yes
	addi r6, r0, 48
	add r6, r6, r5
	debug r6
	addi r11, r0, -1
	sltiu r5, r11, 4095     # 0xFFFFFFFF < 0xFFF: no
	addi r6, r0, 48
	add r6, r6, r5
	debug r6
	ori r11, r0, 4094       # ori: zero-extended
	sltiu r5, r11, 4095     # 4094 < 4095: yes
	addi r6, r0, 48
	add r6, r6, r5
	debug r6
	sltiu r5, r0, 2048      # 0 < 2048: yes
	addi r6, r0, 48
	add r6, r6, r5
	debug r6
	lui r11, 0x80000
	sltiu r5, r11, 2048     # 0x80000000 < 2048: no
	addi r6, r0, 48
	add r6, r6, r5
	debug r6
	addi r6, r0, 10
	debug r6
	addi r1, r0, 0
	halt
