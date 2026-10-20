// libc++ on the named locales (runtime/locales.s32o): std::locale by
// name, numpunct and grouped output, put_time, collate and sorting with a
// locale, moneypunct and put_money, a locale combined from two.  The
// values are CLDR 46's as runtime/gen_locales.py renders them, so
// locales.expected is pinned, not a host build's output: a host's C
// library has locale data of its own.  (locales.locales tells
// run-libcxx.sh to link the named locales.)
#include <algorithm>
#include <ctime>
#include <iomanip>
#include <iostream>
#include <locale>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

static int fails;
static void check(const char *what, bool ok) {
    std::cout << what << (ok ? " PASS" : " FAIL") << '\n';
    if (!ok) fails++;
}

static std::tm when() {
    std::tm tm{};
    tm.tm_year = 128; tm.tm_mon = 10; tm.tm_mday = 23; tm.tm_wday = 4; tm.tm_yday = 327;
    tm.tm_hour = 9; tm.tm_min = 41; tm.tm_sec = 30;
    return tm;
}

static std::string stamp(const std::locale &loc, const char *fmt) {
    std::ostringstream os;
    os.imbue(loc);
    std::tm tm = when();
    os << std::put_time(&tm, fmt);
    return os.str();
}

int main() {
    std::locale de("de_DE.UTF-8");
    std::cout << "name: " << de.name() << '\n';
    const auto &np = std::use_facet<std::numpunct<char>>(de);
    std::cout << "numpunct: [" << np.decimal_point() << "] [" << np.thousands_sep() << "] grouping "
              << int(np.grouping()[0]) << '\n';

    std::ostringstream num;
    num.imbue(de);
    num << std::fixed << std::setprecision(2) << 1234567.891 << ' ' << 98765432;
    std::cout << "de numbers: " << num.str() << '\n';
    std::istringstream in("4.321,5");
    in.imbue(de);
    double back = 0;
    in >> back;
    check("de number read back", back == 4321.5);

    std::cout << "de time: " << stamp(de, "%c | %x | %A %d. %B") << '\n';
    std::cout << "pl time: " << stamp(std::locale("pl_PL.UTF-8"), "%A %B %Y") << '\n';
    std::cout << "en time: " << stamp(std::locale("en_US.UTF-8"), "%X | %p") << '\n';

    std::vector<std::string> words = { "zebra", "\xc3\xa4pple", "apple", "\xc3\xb6l", "orm", "\xc3\xa5ker" };
    for (const char *name : { "C", "de_DE.UTF-8", "sv_SE.UTF-8" }) {
        std::locale loc(name);
        std::sort(words.begin(), words.end(), loc);         // a locale is its collate's comparison
        std::cout << name << ":";
        for (auto &w : words) std::cout << ' ' << w;
        std::cout << '\n';
    }
    std::locale sv("sv_SE.UTF-8");                          // the facet lives as long as a locale holds it
    const auto &co = std::use_facet<std::collate<char>>(sv);
    std::string a = "\xc3\xa4pple", z = "zebra";
    std::string ka = co.transform(a.data(), a.data() + a.size()), kz = co.transform(z.data(), z.data() + z.size());
    check("sv transform keys order as compare", (ka < kz) == (co.compare(a.data(), a.data() + a.size(), z.data(), z.data() + z.size()) < 0));

    const auto &mp = std::use_facet<std::moneypunct<char>>(de);
    std::cout << "moneypunct: [" << mp.curr_symbol() << "] frac " << mp.frac_digits() << " [" << mp.decimal_point() << "]\n";
    const auto &mpi = std::use_facet<std::moneypunct<char, true>>(de);
    std::cout << "international: [" << mpi.curr_symbol() << "]\n";
    std::ostringstream money;
    money.imbue(de);
    money << std::showbase << std::put_money(123456) << " | " << std::put_money(-75);
    std::cout << "de money: " << money.str() << '\n';
    std::ostringstream usd;
    usd.imbue(std::locale("en_US.UTF-8"));
    usd << std::showbase << std::put_money(123456);
    std::cout << "en money: " << usd.str() << '\n';

    std::locale mixed(std::locale::classic(), de, std::locale::time);
    std::ostringstream mx;
    mx.imbue(mixed);
    std::tm tm = when();
    mx << std::put_time(&tm, "%B") << ' ' << 1.5;
    std::cout << "mixed: " << mx.str() << " | " << mixed.name() << '\n';

    bool threw = false;
    try { std::locale nowhere("xx_QQ.UTF-8"); } catch (std::runtime_error &) { threw = true; }
    check("an unknown name still throws", threw);
    threw = false;
    try { std::locale latin1("de_DE.ISO-8859-1"); } catch (std::runtime_error &) { threw = true; }
    check("a codeset other than UTF-8 throws", threw);

    std::cout << fails << " failed\n";
    return fails;
}
