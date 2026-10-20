// libc++'s thread support on SLOW-32, which has one thread
// (runtime/include/__external_threading): what a single-threaded program
// gets from <mutex>, <condition_variable>, <thread> and steady_clock.
// Every case is one a host build answers the same way, so the expected
// output is a host's: std::thread either runs its function (a host) or
// throws resource_unavailable_try_again (SLOW-32), and both pass; a held
// mutex refuses try_lock everywhere.  Self-checking; one line per case.
#include <chrono>
#include <condition_variable>
#include <cstdio>
#include <mutex>
#include <system_error>
#include <thread>

static int fails;
static void check(const char *what, bool ok) {
    std::printf("%-38s %s\n", what, ok ? "PASS" : "FAIL");
    if (!ok) fails++;
}

static int once_calls;
static void count_once() { once_calls++; }

int main() {
    std::mutex m;
    m.lock();
    bool second = m.try_lock();
    m.unlock();
    check("mutex lock, try_lock refused, unlock", !second);
    {
        std::lock_guard<std::mutex> g(m);
        check("lock_guard takes a free mutex", true);
    }
    check("try_lock after the guard is gone", m.try_lock());
    m.unlock();

    std::recursive_mutex rm;
    rm.lock();
    bool again = rm.try_lock();
    if (again) rm.unlock();
    rm.unlock();
    check("recursive_mutex locks twice", again);

    auto t0 = std::chrono::steady_clock::now();
    std::this_thread::sleep_for(std::chrono::milliseconds(30));
    auto slept = std::chrono::steady_clock::now() - t0;
    check("sleep_for 30ms on steady_clock", slept >= std::chrono::milliseconds(30));

    std::condition_variable cv;
    std::unique_lock<std::mutex> lk(m);
    t0 = std::chrono::steady_clock::now();
    std::cv_status st = cv.wait_for(lk, std::chrono::milliseconds(40));
    auto waited = std::chrono::steady_clock::now() - t0;
    check("wait_for with no notify times out", st == std::cv_status::timeout);
    check("and waited its 40ms", waited >= std::chrono::milliseconds(40));
    check("the lock is held again after it", lk.owns_lock());
    bool pred_seen = cv.wait_for(lk, std::chrono::milliseconds(5), [] { return true; });
    check("wait_for with a true predicate", pred_seen);
    lk.unlock();

    std::once_flag flag;
    std::call_once(flag, count_once);
    std::call_once(flag, count_once);
    check("call_once calls once", once_calls == 1);

    int ran = 0;
    bool outcome_ok = false;
    try {
        std::thread t([&ran] { ran = 1; });
        t.join();
        outcome_ok = ran == 1;
    } catch (const std::system_error &e) {
        outcome_ok = e.code() == std::errc::resource_unavailable_try_again;
    }
    check("std::thread runs, or cannot start", outcome_ok);

    check("this_thread::get_id is not the null id", std::this_thread::get_id() != std::thread::id());

    bool caught = false;
    try {
        throw std::system_error(std::make_error_code(std::errc::timed_out), "still unwinds");
    } catch (const std::system_error &e) {
        caught = e.code() == std::errc::timed_out;
    }
    check("exceptions still thrown and caught", caught);

    std::printf("%d failed\n", fails);
    return fails;
}
