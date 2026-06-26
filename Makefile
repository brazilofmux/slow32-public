CXX      ?= c++
CXXFLAGS ?= -O2 -std=c++17 -Wall -Wextra
INC       = -Isrc -Igen
PY        = .venv/bin/python

.PHONY: all data test clean

all: tz

# --- C++ runtime --------------------------------------------------------------
tz: src/tz_cli.cpp src/tzlookup.cpp src/tzlookup.h gen/tz_data.h
	$(CXX) $(CXXFLAGS) $(INC) -o $@ src/tz_cli.cpp src/tzlookup.cpp

test: tests/test_tzlookup
	./tests/test_tzlookup

tests/test_tzlookup: tests/test_tzlookup.cpp src/tzlookup.cpp src/tzlookup.h gen/tz_data.h
	$(CXX) $(CXXFLAGS) $(INC) -o $@ tests/test_tzlookup.cpp src/tzlookup.cpp

# --- Data pipeline (regenerates gen/tz_data.h) --------------------------------
data:
	$(PY) build/download.py
	$(PY) build/build.py
	$(PY) build/emit_c.py

clean:
	rm -f tz tests/test_tzlookup *.o
