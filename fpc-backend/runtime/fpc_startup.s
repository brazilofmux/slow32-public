# FPC startup shim for SLOW-32 embedded target
# Provides symbols expected by the FPC system unit that aren't
# available from the C runtime (crt0).
#
# When using the C crt0 as entry point (recommended for MMIO programs),
# this shim provides:
#   _haltproc - exit handler that reads FPC's exit code and calls C exit()
#   _stack_top - stack top address (matches what crt0/linker provides)
#
# The FPC system unit declares _haltproc as external. The RTL's
# slow32_start.inc provides a weak _haltproc (infinite loop) for
# bare-metal targets. This shim overrides it with a proper exit.

.section .text
    .balign 4

# _haltproc: called by SYSTEM_$$_SYSTEM_EXIT via tail call
# FPC stores exit code in operatingsystem_result before reaching here
.globl _haltproc
_haltproc:
    la r3, operatingsystem_result
    ldw r3, r3+0
    call exit
    halt    # fallback if exit returns

.section .data
    .balign 4

# _stack_top: referenced by FPC's LowlevelStartup (not used when
# linking with C crt0, but must be defined to satisfy the linker)
.globl _stack_top
_stack_top:
    .long 0x0FFFFFF0
