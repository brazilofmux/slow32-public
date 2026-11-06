# A quoted name is a name wherever a name goes: LLVM quotes any it does not like,
# flang's "_QFEXnX/s1/" for one, and `.word "name"` was refused as no expression
# while the label and `.weak` of the same name were accepted (2026-11-06, flang -O0).
# Prints 3 3 7: through %hi/%lo of the name, a word holding it, one with an offset.
	.text
	.globl main
	.p2align 2
main:
	lui r3, %hi("a/b")
	addi r3, r3, %lo("a/b")
	ldw r5, r3+0
	debug r5
	lui r3, %hi(ptrs)
	addi r3, r3, %lo(ptrs)
	ldw r4, r3+0
	ldw r5, r4+0
	debug r5
	ldw r4, r3+4
	ldw r5, r4+0
	debug r5
	addi r5, r0, 10
	debug r5
	addi r1, r0, 0
	jalr r0, r31, 0
	.data
	.p2align 2
"a/b":
	.word 51
	.word 55
ptrs:
	.word "a/b"
	.word "a/b"+4
