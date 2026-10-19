// C++ run-time type information on SLOW-32: typeid, dynamic_cast, and the
// catch matching built on them (base classes, pointer conversions).
// Self-checking; expected.txt is what a host build of this file prints.
#include <exception>
#include <typeinfo>

extern "C" int printf(const char *, ...);

static int fails;
static void check(const char *what, bool ok) {
    printf("%-40s %s\n", what, ok ? "PASS" : "FAIL");
    if (!ok) fails++;
}

// A small zoo: single inheritance, multiple inheritance with a non-zero
// second-base offset, a virtual-inheritance diamond, an ambiguous base,
// and a private base.
struct Animal { virtual ~Animal() {} int legs = 4; };
struct Dog : Animal { int barks = 3; };
struct Puppy : Dog { int naps = 9; };
struct Cat : Animal {};

struct Swimmer { virtual ~Swimmer() {} int strokes = 7; };
struct Otter : Animal, Swimmer { int fish = 2; };

struct Base { virtual ~Base() {} int b = 10; };
struct Left : virtual Base { int l = 20; };
struct Right : virtual Base { int r = 30; };
struct Diamond : Left, Right { int d = 40; };

struct Plain { int p = 1; };
struct LeftP : Plain { virtual ~LeftP() {} };
struct RightP : Plain { virtual ~RightP() {} };
struct Extra { virtual ~Extra() {} };
struct Ambig : LeftP, RightP, Extra {};        // two Plain subobjects

struct Hidden : private Animal { int h = 5; };  // Animal not public

struct Oops : std::exception {
    const char *what() const noexcept override { return "oops from the zoo"; }
};

int main() {
    // ---- typeid
    Puppy pup;
    Animal &ar = pup;
    check("typeid of fundamentals",
          typeid(int) == typeid(int) && typeid(int) != typeid(long) &&
          typeid(const char *) != typeid(char *));
    check("typeid through a base reference", typeid(ar) == typeid(Puppy));
    check("typeid of the static type", typeid(Animal) != typeid(ar));
    printf("names: %s %s %s %s\n", typeid(int).name(), typeid(ar).name(),
           typeid(Diamond).name(), typeid(const char *).name());
    check("hash_code follows equality",
          typeid(ar).hash_code() == typeid(Puppy).hash_code());
    check("before is an order",
          typeid(Dog).before(typeid(Cat)) != typeid(Cat).before(typeid(Dog)));
    int caught = 0;
    try {
        Animal *none = nullptr;
        (void)typeid(*none);
    } catch (std::bad_typeid &e) {
        caught = 1;
        printf("bad_typeid: %s\n", e.what());
    }
    check("typeid(*null) throws bad_typeid", caught == 1);

    // ---- dynamic_cast
    Animal *ap = &pup;
    check("downcast to the dynamic type", dynamic_cast<Puppy *>(ap) == &pup);
    check("downcast to an intermediate", dynamic_cast<Dog *>(ap) == &pup);
    Cat cat;
    Animal *cp = &cat;
    check("downcast to the wrong type is null", dynamic_cast<Dog *>(cp) == nullptr);

    Otter otter;
    Animal *oa = &otter;
    Swimmer *os = dynamic_cast<Swimmer *>(oa);        // cross-cast
    check("cross-cast to the second base",
          os == static_cast<Swimmer *>(&otter) && os->strokes == 7);
    check("cross-cast back down", dynamic_cast<Otter *>(os) == &otter);
    check("dynamic_cast<void *> finds the whole object",
          dynamic_cast<void *>(os) == static_cast<void *>(&otter));

    Diamond dia;
    Base *bp = &dia;                                  // the one virtual Base
    Diamond *dp = dynamic_cast<Diamond *>(bp);
    check("downcast from a virtual base", dp == &dia && dp->d == 40);
    Right *rp = dynamic_cast<Right *>(static_cast<Left *>(&dia));
    check("cross-cast in a diamond", rp == static_cast<Right *>(&dia) && rp->r == 30);

    Ambig amb;
    LeftP *lp = &amb;
    check("cross-cast to a sibling base", dynamic_cast<RightP *>(lp) == static_cast<RightP *>(&amb));
    Extra *ep = &amb;
    check("cast to an ambiguous base is null", dynamic_cast<Plain *>(ep) == nullptr);

    caught = 0;
    try {
        Dog &dr = dynamic_cast<Dog &>(*cp);
        (void)dr;
    } catch (std::bad_cast &e) {
        caught = 1;
        printf("bad_cast: %s\n", e.what());
    }
    check("failed reference cast throws bad_cast", caught == 1);

    // ---- catching by base class and by pointer conversion
    caught = 0;
    try { throw Puppy(); } catch (Animal &a) { caught = a.legs; }
    check("catch derived by base reference", caught == 4);

    caught = 0;
    try { throw Otter(); } catch (Swimmer &s) { caught = s.strokes; }
    check("catch by a second base (adjusted)", caught == 7);

    caught = 0;
    try { throw Diamond(); } catch (Base &b) { caught = b.b; }
    check("catch by a virtual base", caught == 10);

    caught = 0;
    try { throw Ambig(); } catch (Plain &) { caught = 1; } catch (...) { caught = 2; }
    check("ambiguous base is not a match", caught == 2);

    caught = 0;
    try { throw Hidden(); } catch (Animal &) { caught = 1; } catch (...) { caught = 2; }
    check("private base is not a match", caught == 2);

    caught = 0;
    try { throw &otter; } catch (Swimmer *s) { caught = s == static_cast<Swimmer *>(&otter); }
    check("catch derived* by base*", caught == 1);

    caught = 0;
    try { throw &pup; } catch (const Animal *a) { caught = a->legs; }
    check("catch adding const", caught == 4);

    caught = 0;
    const Dog *cd = &pup;
    try { throw cd; } catch (Dog *) { caught = 1; } catch (const Dog *) { caught = 2; }
    check("const is not dropped", caught == 2);

    caught = 0;
    try { throw &cat; } catch (void *v) { caught = v == static_cast<void *>(&cat); }
    check("catch T* as void*", caught == 1);

    caught = 0;
    try { throw "words"; } catch (const char *s) { caught = s[0] == 'w'; }
    check("catch a string literal", caught == 1);

    caught = 0;
    try { throw nullptr; } catch (Dog *d) { caught = d == nullptr ? 1 : 3; }
    check("nullptr caught as a pointer", caught == 1);

    caught = 0;
    try { throw Oops(); } catch (std::exception &e) {
        caught = 1;
        printf("std::exception: %s\n", e.what());
    }
    check("user exception by std::exception", caught == 1);

    caught = 0;
    try { throw Puppy(); } catch (Dog d) { caught = d.barks; }
    check("catch by value, sliced to a base", caught == 3);

    printf("%d failed\n", fails);
    return fails;
}
