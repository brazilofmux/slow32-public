// libc++ on SLOW-32, the parts that live in libc++.a and libc++abi.a:
// strings, the exceptions the library throws, exception_ptr, unordered
// containers, charconv, format, chrono, system_error.
// Self-checking; one line per case, the same as a host build prints.
#include <algorithm>
#include <charconv>
#include <chrono>
#include <cstdio>
#include <exception>
#include <format>
#include <new>
#include <stdexcept>
#include <string>
#include <system_error>
#include <unordered_map>
#include <unordered_set>
#include <vector>

static int fails;
static void check(const char *what, bool ok) {
    std::printf("%-38s %s\n", what, ok ? "PASS" : "FAIL");
    if (!ok) fails++;
}

struct Watch {
    int seen = -1;
    ~Watch() { seen = std::uncaught_exceptions(); }
};
static int dtor_saw;
struct Probe { ~Probe() { dtor_saw = std::uncaught_exceptions(); } };

int main() {
    // ---- std::string: short (in place) and long (on the heap)
    std::string small = "kettle";
    std::string big(200, 'z');
    big.replace(10, 5, "lantern");
    check("string short and long",
          small.size() == 6 && big.size() == 202 && big.substr(10, 7) == "lantern");
    std::string words = "the quick brown fox jumps";
    check("string find/rfind",
          words.find("brown") == 10 && words.rfind('o') == 17 &&
          words.find("cat") == std::string::npos);
    std::string built;
    for (int i = 0; i < 40; i++) built += std::to_string(i * i) + ",";
    check("to_string + append", built.size() == 154 && built.substr(0, 10) == "0,1,4,9,16");
    check("stoi / stod", std::stoi("  -812") == -812 && std::stod("2.5e3") == 2500.0);

    int caught = 0;
    try { (void)std::stoi("bananas"); } catch (std::invalid_argument &) { caught = 1; }
    check("stoi throws invalid_argument", caught == 1);
    caught = 0;
    try { (void)std::stoi("99999999999999"); } catch (std::out_of_range &) { caught = 1; }
    check("stoi throws out_of_range", caught == 1);

    // ---- what the containers throw
    std::vector<int> v{1, 2, 3};
    caught = 0;
    try { (void)v.at(7); } catch (std::out_of_range &e) { caught = e.what()[0] != 0; }
    check("vector::at throws out_of_range", caught == 1);
    caught = 0;
    try { throw std::runtime_error(big); } catch (std::exception &e) {
        caught = std::string(e.what()) == big;
    }
    check("runtime_error carries a long message", caught == 1);
    caught = 0;
    try {
        void *p = ::operator new(std::size_t(-1) / 2);   // half the address space
        ::operator delete(p);
    } catch (std::bad_alloc &) { caught = 1; }
    check("operator new throws bad_alloc", caught == 1);
    void *nt = ::operator new(std::size_t(-1) / 2, std::nothrow);
    check("nothrow new returns null", nt == nullptr);

    // ---- exception_ptr, nested exceptions, uncaught_exceptions
    std::exception_ptr ep;
    try { throw std::logic_error("saved for later"); } catch (...) { ep = std::current_exception(); }
    caught = 0;
    try { std::rethrow_exception(ep); } catch (std::logic_error &e) {
        caught = std::string(e.what()) == "saved for later";
    }
    check("current_exception / rethrow_exception", caught == 1);
    auto made = std::make_exception_ptr(std::out_of_range("made"));
    caught = 0;
    try { std::rethrow_exception(made); } catch (std::out_of_range &) { caught = 1; }
    check("make_exception_ptr", caught == 1);
    caught = 0;
    try {
        try { throw std::runtime_error("inner"); }
        catch (...) { std::throw_with_nested(std::logic_error("outer")); }
    } catch (std::logic_error &e) {
        try { std::rethrow_if_nested(e); }
        catch (std::runtime_error &in) { caught = std::string(in.what()) == "inner"; }
    }
    check("nested exceptions", caught == 1);
    dtor_saw = -1;
    try { Probe p; throw 5; } catch (int) {}
    check("uncaught_exceptions during unwind", dtor_saw == 1 && std::uncaught_exceptions() == 0);

    // ---- unordered containers, sort with a comparator
    std::unordered_map<std::string, int> counts;
    for (const char *w : {"fig", "pear", "fig", "plum", "fig", "pear"}) counts[w]++;
    check("unordered_map", counts.size() == 3 && counts["fig"] == 3 && counts["plum"] == 1);
    std::unordered_set<int> seen;
    for (int i = 0; i < 1000; i++) seen.insert((i * 7919) % 211);
    check("unordered_set rehashes", seen.size() == 211);
    std::vector<std::string> names{"walnut", "ash", "birch", "cedar", "alder"};
    std::sort(names.begin(), names.end(),
              [](const std::string &a, const std::string &b) { return a.size() < b.size() || (a.size() == b.size() && a < b); });
    check("sort strings by a lambda", names.front() == "ash" && names.back() == "walnut");

    // ---- charconv, format
    char buf[64];
    auto r1 = std::to_chars(buf, buf + sizeof buf, -123456789);
    check("to_chars int", std::string(buf, r1.ptr) == "-123456789");
    auto r2 = std::to_chars(buf, buf + sizeof buf, 0.1);
    check("to_chars double (shortest)", std::string(buf, r2.ptr) == "0.1");
    double d = 0;
    const char *in = "6.02214076e23";
    auto r3 = std::from_chars(in, in + 13, d);
    check("from_chars double", r3.ec == std::errc() && d == 6.02214076e23);
    std::string f = std::format("{:>6}|{:04x}|{:.3f}|{}", "ab", 255, 3.14159, true);
    check("std::format", f == "    ab|00ff|3.142|true");

    // ---- chrono, system_error
    auto now = std::chrono::system_clock::now();
    auto secs = std::chrono::duration_cast<std::chrono::seconds>(now.time_since_epoch()).count();
    check("system_clock is after 2020", secs > 1577836800);
    std::error_code ec = std::make_error_code(std::errc::no_such_file_or_directory);
    check("error_code", ec.value() != 0 && ec.category() == std::generic_category());

    std::printf("%d failed\n", fails);
    return fails;
}
