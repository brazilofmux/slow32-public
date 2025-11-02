CXX = clang++-19
LLVM_CONFIG = llvm-config-19

CXXFLAGS = $(shell $(LLVM_CONFIG) --cxxflags)
LDFLAGS = $(shell $(LLVM_CONFIG) --ldflags --system-libs --libs core irreader support)

TARGETS = slow32-compile

all: $(TARGETS)

slow32-compile: slow32-compile.cpp
	$(CXX) $(CXXFLAGS) $< -o $@ $(LDFLAGS)

test: slow32-compile
	# Create a simple test
	@echo '; Simple LLVM IR test' > test.ll
	@echo 'define i32 @main() {' >> test.ll
	@echo '  ret i32 42' >> test.ll
	@echo '}' >> test.ll
	@echo "Generated test.ll:"
	@cat test.ll
	@echo ""
	@echo "Compiling to SLOW-32 assembly:"
	./slow32-compile test.ll

clean:
	rm -f $(TARGETS) test.ll *.o

.PHONY: all test clean