# Makefile — Verilator build for SLOW-32 CPU simulation

VERILATOR ?= verilator
VERILATOR_FLAGS = --cc --exe --build -j 0 \
                  --top-module slow32_cpu \
                  -Wall -Wno-fatal \
                  --trace \
                  -CFLAGS "-O2 -std=c++17"

RTL_DIR = ../rtl
SIM_DIR = .

# RTL source files (order matters for package)
RTL_SRCS = $(RTL_DIR)/slow32_pkg.sv \
           $(RTL_DIR)/slow32_regfile.sv \
           $(RTL_DIR)/slow32_decode.sv \
           $(RTL_DIR)/slow32_alu.sv \
           $(RTL_DIR)/slow32_cpu.sv

TB_SRC = $(SIM_DIR)/slow32_tb.cpp

# Build the simulator
.PHONY: build
build: obj_dir/Vslow32_cpu

obj_dir/Vslow32_cpu: $(RTL_SRCS) $(TB_SRC)
	$(VERILATOR) $(VERILATOR_FLAGS) $(RTL_SRCS) $(TB_SRC)

# Run a single test
# Usage: make run TEST=path/to/test.s32x
.PHONY: run
run: build
	./obj_dir/Vslow32_cpu $(TEST)

# Regression test targets (DEBUG-only tests, no MMIO needed)
REGRESSION_DIR = ../../regression
TESTS = asm-branch-alias asm-li-la asm-asciz-long asm-stw-base-imm \
        feature-arithmetic feature-branches feature-logical feature-shifts \
        feature-loops feature-control-flow feature-function-calls \
        feature-negative-offset feature-switch \
        bug001-varargs-clobbered bug005-getelementptr

.PHONY: test
test: build
	@pass=0; fail=0; skip=0; total=0; \
	for t in $(TESTS); do \
		total=$$((total + 1)); \
		s32x="$(REGRESSION_DIR)/results/$$t/test.s32x"; \
		expected="$(REGRESSION_DIR)/tests/$$t/expected.txt"; \
		if [ ! -f "$$s32x" ]; then \
			echo "SKIP $$t (no .s32x)"; \
			skip=$$((skip + 1)); \
			continue; \
		fi; \
		if [ ! -f "$$expected" ]; then \
			echo "SKIP $$t (no expected.txt)"; \
			skip=$$((skip + 1)); \
			continue; \
		fi; \
		actual=$$(timeout 10 ./obj_dir/Vslow32_cpu "$$s32x" 2>/dev/null); \
		rc=$$?; \
		exp=$$(cat "$$expected"); \
		if [ "$$actual" = "$$exp" ] && [ $$rc -eq 0 ]; then \
			echo "PASS $$t"; \
			pass=$$((pass + 1)); \
		else \
			echo "FAIL $$t (rc=$$rc)"; \
			if [ "$$actual" != "$$exp" ]; then \
				echo "  expected: $$(echo "$$exp" | head -3)"; \
				echo "  actual:   $$(echo "$$actual" | head -3)"; \
			fi; \
			fail=$$((fail + 1)); \
		fi; \
	done; \
	echo ""; \
	echo "Results: $$pass passed, $$fail failed, $$skip skipped / $$total total"

.PHONY: clean
clean:
	rm -rf obj_dir
