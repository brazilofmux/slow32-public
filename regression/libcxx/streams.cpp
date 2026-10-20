// libc++ iostreams and <locale> on SLOW-32, the C locale: cout and the
// manipulators, string streams both ways, the standard facets, put_time,
// regex, and a locale name nobody has.  (fstream is files.cpp's, with
// std::filesystem.)
// Self-checking; prints what a host build of this file prints.
#include <cstdio>
#include <ctime>
#include <iomanip>
#include <iostream>
#include <locale>
#include <regex>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

static int fails;
static void check(const char *what, bool ok) {
    std::cout << std::left << std::setw(36) << what << (ok ? "PASS" : "FAIL") << '\n';
    if (!ok) fails++;
}

int main() {
    // ---- cout and the manipulators
    std::cout << "plain " << 42 << ' ' << -7 << ' ' << 2.5 << ' ' << true << '\n';
    std::cout << std::boolalpha << true << ' ' << std::noboolalpha << false << '\n';
    std::cout << std::hex << 255 << ' ' << std::showbase << 255 << std::dec << std::noshowbase << '\n';
    std::cout << std::fixed << std::setprecision(3) << 3.14159 << ' '
              << std::scientific << 12345.678 << std::defaultfloat << '\n';
    std::cout << '[' << std::right << std::setw(8) << std::setfill('.') << 99 << ']'
              << std::setfill(' ') << '\n';

    // ---- ostringstream / istringstream
    std::ostringstream os;
    os << "row " << std::setw(4) << std::setfill('0') << 7 << ": " << std::setprecision(4) << 1.0 / 3;
    check("ostringstream formats", os.str() == "row 0007: 0.3333");

    std::istringstream is("  17 -3.25 lantern 0x1F\nsecond line here\n");
    int n = 0; double d = 0; std::string w; int h = 0;
    is >> n >> d >> w >> std::hex >> h;
    check("istringstream parses", n == 17 && d == -3.25 && w == "lantern" && h == 31);
    std::string line;
    std::getline(is >> std::ws, line);
    check("getline after >>", line == "second line here");
    std::istringstream bad("forty");
    int x = 5;
    bad >> x;
    check("failed extraction sets failbit", bad.fail() && x == 0);

    std::stringstream round;
    for (int i = 1; i <= 5; i++) round << i * i << ' ';
    int sum = 0, v;
    while (round >> v) sum += v;
    check("stringstream round trip", sum == 55 && round.eof());

    // ---- locales and facets (the C locale)
    std::locale c = std::locale::classic();
    check("classic locale is named C", c.name() == "C");
    check("global locale is C", std::locale().name() == "C");
    const auto &np = std::use_facet<std::numpunct<char>>(c);
    check("numpunct", np.decimal_point() == '.' && np.grouping().empty());
    const auto &ct = std::use_facet<std::ctype<char>>(c);
    check("ctype facet",
          ct.is(std::ctype_base::alpha, 'q') && !ct.is(std::ctype_base::alpha, '7') &&
          ct.toupper('m') == 'M' && !ct.is(std::ctype_base::alpha, (char)0xC3));
    const auto &co = std::use_facet<std::collate<char>>(c);
    std::string a1 = "apple", a2 = "apricot";
    check("collate facet", co.compare(a1.data(), a1.data() + a1.size(), a2.data(), a2.data() + a2.size()) < 0);
    std::tm tm{};
    tm.tm_year = 126; tm.tm_mon = 9; tm.tm_mday = 19; tm.tm_hour = 14; tm.tm_min = 5; tm.tm_wday = 1;
    std::ostringstream ts;
    ts << std::put_time(&tm, "%A %B %d %Y %H:%M");
    check("put_time", ts.str() == "Monday October 19 2026 14:05");

    bool threw = false;
    try { std::locale nowhere("xx_QQ.nowhere"); } catch (std::runtime_error &) { threw = true; }
    check("an unknown locale name throws", threw);

    // ---- regex, which runs on the locale machinery
    std::regex date(R"((\d{4})-(\d{2})-(\d{2}))");
    std::smatch m;
    std::string text = "shipped on 2026-10-19, arrived later";
    check("regex_search + groups", std::regex_search(text, m, date) && m[1] == "2026" && m[3] == "19");
    check("regex_replace", std::regex_replace(std::string("a1b22c333"), std::regex("[0-9]+"), "#") == "a#b#c#");

    std::cout << fails << " failed\n";
    return fails;
}
