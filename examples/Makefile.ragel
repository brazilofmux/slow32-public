SLOW32CC ?= ../slow32cc
EMULATOR ?= ../tools/emulator/slow32-fast
RAGEL ?= ragel
MMIO_SIZE ?= 0x10000

BIN := validatecsv_ragel.s32x
SRC := validatecsv_ragel.c

.PHONY: all clean regen run

all: $(BIN)

$(BIN): $(SRC)
	$(SLOW32CC) --libc=mmio -O2 $< -o $@

# Rebuild the generated parser if ragel is installed.
regen:
	@command -v $(RAGEL) >/dev/null 2>&1 || { echo "ragel not found; install it to regenerate $(SRC)"; exit 1; }
	$(RAGEL) -G2 validatecsv.rl -o $(SRC)

# Run the validator under the fast emulator. Pass ARGS="file.csv".
run: $(BIN)
	$(EMULATOR) --mmio $(MMIO_SIZE) $(BIN) $(ARGS)

clean:
	rm -f $(BIN)
