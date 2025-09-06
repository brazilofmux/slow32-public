# SLOW-32 Tools Master Makefile

.PHONY: all clean assembler linker emulator utilities

all: assembler linker emulator utilities

assembler:
	$(MAKE) -C assembler

linker:
	$(MAKE) -C linker

emulator:
	$(MAKE) -C emulator

utilities:
	$(MAKE) -C utilities

clean:
	$(MAKE) -C assembler clean
	$(MAKE) -C linker clean
	$(MAKE) -C emulator clean
	$(MAKE) -C utilities clean