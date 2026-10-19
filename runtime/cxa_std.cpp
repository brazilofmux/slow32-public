// The C++ half of the SLOW-32 C++ run time: the standard classes the
// headers in include/ (<exception>, <typeinfo>, <new>) declare, whose
// vtables and type_info objects the compiler emits here, and the two ABI
// entry points that throw them.  Everything else is C (cxxabi.c, cxa_*.c).

#include <exception>
#include <new>
#include <typeinfo>

extern "C" unsigned int __cxa_uncaught_exceptions(void);

namespace std {

type_info::~type_info() {}

exception::~exception() noexcept {}
const char *exception::what() const noexcept { return "std::exception"; }

bad_exception::~bad_exception() noexcept {}
const char *bad_exception::what() const noexcept { return "std::bad_exception"; }

bad_cast::~bad_cast() noexcept {}
const char *bad_cast::what() const noexcept { return "std::bad_cast"; }

bad_typeid::~bad_typeid() noexcept {}
const char *bad_typeid::what() const noexcept { return "std::bad_typeid"; }

int uncaught_exceptions() noexcept { return (int)__cxa_uncaught_exceptions(); }

const nothrow_t nothrow{};

} // namespace std

// operator new does not throw here (no std::bad_alloc), so the nothrow
// forms are the plain ones.
void *operator new(__SIZE_TYPE__ size, const std::nothrow_t &) noexcept {
    return ::operator new(size);
}
void *operator new[](__SIZE_TYPE__ size, const std::nothrow_t &) noexcept {
    return ::operator new[](size);
}

// dynamic_cast<T &> that fails, and typeid of *p with p null.
extern "C" [[noreturn]] void __cxa_bad_cast() { throw std::bad_cast(); }
extern "C" [[noreturn]] void __cxa_bad_typeid() { throw std::bad_typeid(); }
