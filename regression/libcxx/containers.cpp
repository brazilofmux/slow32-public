// libc++ on SLOW-32: containers, algorithms and vocabulary types.
// Self-checking; prints PASS/FAIL per part like the other C++ probes.
#include <algorithm>
#include <array>
#include <bitset>
#include <cstdio>
#include <functional>
#include <map>
#include <memory>
#include <numeric>
#include <optional>
#include <set>
#include <span>
#include <string_view>
#include <tuple>
#include <utility>
#include <variant>
#include <vector>

static int fails;
static void check(const char *what, bool ok) {
    std::printf("%-34s %s\n", what, ok ? "PASS" : "FAIL");
    if (!ok) fails++;
}

struct Tally {
    static int live;
    int v;
    explicit Tally(int x) : v(x) { live++; }
    Tally(const Tally &o) : v(o.v) { live++; }
    ~Tally() { live--; }
};
int Tally::live;

int main() {
    std::vector<int> v;
    for (int i = 0; i < 50; i++) v.push_back((i * 37) % 53);
    std::sort(v.begin(), v.end());
    check("vector + sort", std::is_sorted(v.begin(), v.end()) && v.size() == 50);
    check("accumulate", std::accumulate(v.begin(), v.end(), 0) == 1282);

    std::array<int, 5> a{5, 3, 9, 1, 7};
    std::reverse(a.begin(), a.end());
    auto mx = std::max_element(a.begin(), a.end());
    check("array, reverse, max_element", a[0] == 7 && *mx == 9);

    std::map<std::string_view, int> m;
    for (std::string_view w : {"pear", "fig", "plum", "fig", "pear", "fig"}) m[w]++;
    check("map counts", m["fig"] == 3 && m["pear"] == 2 && m.size() == 3);

    std::set<int> s(v.begin(), v.end());
    check("set from a range", s.size() == 50 && *s.begin() == 0);

    std::optional<int> none, some = 42;
    check("optional", !none && some.value_or(0) == 42 && none.value_or(7) == 7);

    std::variant<int, double, std::string_view> var = 2.5;
    double got = std::visit([](auto x) -> double {
        if constexpr (std::is_same_v<decltype(x), std::string_view>) return (double)x.size();
        else return (double)x;
    }, var);
    var = std::string_view("seven");
    check("variant + visit", got == 2.5 && std::get<std::string_view>(var).size() == 5);

    auto [q, r] = std::make_tuple(17 / 5, 17 % 5);
    check("tuple + structured bindings", q == 3 && r == 2);

    {
        auto up = std::make_unique<Tally>(5);
        std::vector<std::unique_ptr<Tally>> owned;
        owned.push_back(std::move(up));
        owned.push_back(std::make_unique<Tally>(6));
        check("unique_ptr in a vector", Tally::live == 2 && owned[1]->v == 6 && !up);
    }
    check("unique_ptr destroys", Tally::live == 0);

    {
        auto sp = std::make_shared<Tally>(9);
        std::shared_ptr<Tally> sp2 = sp;
        std::weak_ptr<Tally> wp = sp;
        check("shared_ptr counts", sp.use_count() == 2 && wp.lock()->v == 9);
        sp.reset();
        sp2.reset();
        check("shared_ptr frees, weak expires", Tally::live == 0 && wp.expired());
    }

    std::function<int(int)> twice = [](int x) { return x * 2; };
    int base = 100;
    std::function<int(int)> plus = [base](int x) { return x + base; };
    check("std::function", twice(21) == 42 && plus(1) == 101);

    int arr[6] = {1, 2, 3, 4, 5, 6};
    std::span<int> sp(arr);
    check("span", sp.subspan(2, 3).size() == 3 && sp.subspan(2, 3)[0] == 3);

    std::bitset<20> bits;
    bits.set(3).set(17);
    check("bitset", bits.count() == 2 && bits.test(17) && !bits.test(4));

    std::string_view text = "the quick brown fox";
    check("string_view find/substr",
          text.find("brown") == 10 && text.substr(4, 5) == "quick");

    std::printf("%d failed\n", fails);
    return fails;
}
