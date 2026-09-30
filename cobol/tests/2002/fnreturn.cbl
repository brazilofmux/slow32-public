*> The intrinsic functions' returned values (2023 15.x), every implemented
*> function over a grid of correct arguments -- generated, one COMPUTE or
*> DISPLAY a call, each line labelled with the call.  The oracle agrees
*> on all but four (.oracle-expected; docs/oracles.md): EXP(20) and
*> EXP10(10.5) past the 15 significant digits computed here in double
*> (native arithmetic: an implementor-defined approximation, 15.34.4,
*> 15.35.4); ANNUITY(0.1, 1), exactly 1.1 here, 1.0999999999 there; and
*> NUMVAL-F("1.5E3"), whose exponent has no sign, which 15.69.3 requires:
*> 0 here (EC-ARGUMENT-FUNCTION), 1500 there.
identification division.
program-id. fnreturn.
data division.
working-storage section.
01 r pic -9(20).9(10).
01 w8 pic x(8).
01 h1 pic s9(3)v99.
01 h2 pic 9(4).
procedure division.
    compute r = function abs(-3.5)
    display "abs(-3.5) " r
    compute r = function abs(0)
    display "abs(0) " r
    compute r = function abs(7)
    display "abs(7) " r
    compute r = function acos(-1)
    display "acos(-1) " r
    compute r = function acos(-0.5)
    display "acos(-0.5) " r
    compute r = function acos(0)
    display "acos(0) " r
    compute r = function acos(0.3)
    display "acos(0.3) " r
    compute r = function acos(1)
    display "acos(1) " r
    compute r = function asin(-1)
    display "asin(-1) " r
    compute r = function asin(0.5)
    display "asin(0.5) " r
    compute r = function asin(1)
    display "asin(1) " r
    compute r = function atan(-100)
    display "atan(-100) " r
    compute r = function atan(0)
    display "atan(0) " r
    compute r = function atan(1)
    display "atan(1) " r
    compute r = function atan(2.5)
    display "atan(2.5) " r
    compute r = function annuity(0, 5)
    display "annuity(0, 5) " r
    compute r = function annuity(0.05, 10)
    display "annuity(0.05, 10) " r
    compute r = function annuity(0.1, 1)
    display "annuity(0.1, 1) " r
    compute r = function annuity(1, 3)
    display "annuity(1, 3) " r
    compute r = function ord(function char(1))
    display "ord(function char(1)) " r
    compute r = function ord(function char(66))
    display "ord(function char(66)) " r
    compute r = function ord(function char(256))
    display "ord(function char(256)) " r
    compute r = function cos(0)
    display "cos(0) " r
    compute r = function cos(1)
    display "cos(1) " r
    compute r = function cos(-2.5)
    display "cos(-2.5) " r
    compute r = function cos(100)
    display "cos(100) " r
    compute r = function sin(0)
    display "sin(0) " r
    compute r = function sin(1)
    display "sin(1) " r
    compute r = function sin(-2.5)
    display "sin(-2.5) " r
    compute r = function sin(100)
    display "sin(100) " r
    compute r = function tan(0)
    display "tan(0) " r
    compute r = function tan(1)
    display "tan(1) " r
    compute r = function tan(-2.5)
    display "tan(-2.5) " r
    compute r = function date-of-integer(1)
    display "date-of-integer(1) " r
    compute r = function date-of-integer(100000)
    display "date-of-integer(100000) " r
    compute r = function date-of-integer(3067671)
    display "date-of-integer(3067671) " r
    compute r = function day-of-integer(1)
    display "day-of-integer(1) " r
    compute r = function day-of-integer(100000)
    display "day-of-integer(100000) " r
    compute r = function day-of-integer(3067671)
    display "day-of-integer(3067671) " r
    compute r = function date-to-yyyymmdd(851231, 50, 2026)
    display "date-to-yyyymmdd(851231, 50, 2026) " r
    compute r = function date-to-yyyymmdd(851231, 10, 1990)
    display "date-to-yyyymmdd(851231, 10, 1990) " r
    compute r = function date-to-yyyymmdd(101, 50, 2026)
    display "date-to-yyyymmdd(101, 50, 2026) " r
    compute r = function date-to-yyyymmdd(991231, 0, 2000)
    display "date-to-yyyymmdd(991231, 0, 2000) " r
    compute r = function day-to-yyyyddd(85365, 50, 2026)
    display "day-to-yyyyddd(85365, 50, 2026) " r
    compute r = function day-to-yyyyddd(1, 50, 2026)
    display "day-to-yyyyddd(1, 50, 2026) " r
    compute r = function year-to-yyyy(85, 50, 2026)
    display "year-to-yyyy(85, 50, 2026) " r
    compute r = function year-to-yyyy(5, 10, 1994)
    display "year-to-yyyy(5, 10, 1994) " r
    compute r = function year-to-yyyy(0, 0, 2000)
    display "year-to-yyyy(0, 0, 2000) " r
    compute r = function year-to-yyyy(99, 99, 1700)
    display "year-to-yyyy(99, 99, 1700) " r
    compute r = function exp(0)
    display "exp(0) " r
    compute r = function exp(1)
    display "exp(1) " r
    compute r = function exp(-3)
    display "exp(-3) " r
    compute r = function exp(20)
    display "exp(20) " r
    compute r = function exp10(0)
    display "exp10(0) " r
    compute r = function exp10(2)
    display "exp10(2) " r
    compute r = function exp10(-3)
    display "exp10(-3) " r
    compute r = function exp10(10.5)
    display "exp10(10.5) " r
    compute r = function factorial(0)
    display "factorial(0) " r
    compute r = function factorial(1)
    display "factorial(1) " r
    compute r = function factorial(5)
    display "factorial(5) " r
    compute r = function factorial(20)
    display "factorial(20) " r
    compute r = function factorial(25)
    display "factorial(25) " r
    compute r = function fraction-part(3.75)
    display "fraction-part(3.75) " r
    compute r = function fraction-part(-3.75)
    display "fraction-part(-3.75) " r
    compute r = function fraction-part(0)
    display "fraction-part(0) " r
    compute r = function integer(3.7)
    display "integer(3.7) " r
    compute r = function integer(-3.7)
    display "integer(-3.7) " r
    compute r = function integer(-4)
    display "integer(-4) " r
    compute r = function integer-part(3.7)
    display "integer-part(3.7) " r
    compute r = function integer-part(-3.7)
    display "integer-part(-3.7) " r
    compute r = function integer-of-date(16010101)
    display "integer-of-date(16010101) " r
    compute r = function integer-of-date(20000229)
    display "integer-of-date(20000229) " r
    compute r = function integer-of-date(99991231)
    display "integer-of-date(99991231) " r
    compute r = function integer-of-day(1601001)
    display "integer-of-day(1601001) " r
    compute r = function integer-of-day(2000366)
    display "integer-of-day(2000366) " r
    compute r = function integer-of-day(9999365)
    display "integer-of-day(9999365) " r
    compute r = function log(1)
    display "log(1) " r
    compute r = function log(2.718281828)
    display "log(2.718281828) " r
    compute r = function log(1000)
    display "log(1000) " r
    compute r = function log(0.001)
    display "log(0.001) " r
    compute r = function log10(1)
    display "log10(1) " r
    compute r = function log10(1000)
    display "log10(1000) " r
    compute r = function log10(0.5)
    display "log10(0.5) " r
    compute r = function max(3 1 2)
    display "max(3 1 2) " r
    compute r = function max(-1.5 2.25)
    display "max(-1.5 2.25) " r
    compute r = function min(3 1 2)
    display "min(3 1 2) " r
    compute r = function min(-1.5 2.25)
    display "min(-1.5 2.25) " r
    compute r = function mean(1 2 3 4)
    display "mean(1 2 3 4) " r
    compute r = function mean(-1 1.5)
    display "mean(-1 1.5) " r
    compute r = function median(1 5 3)
    display "median(1 5 3) " r
    compute r = function median(4 1 3 2)
    display "median(4 1 3 2) " r
    compute r = function midrange(1 9 4)
    display "midrange(1 9 4) " r
    compute r = function midrange(-3 4)
    display "midrange(-3 4) " r
    compute r = function mod(11, 5)
    display "mod(11, 5) " r
    compute r = function mod(-11, 5)
    display "mod(-11, 5) " r
    compute r = function mod(11, -5)
    display "mod(11, -5) " r
    compute r = function mod(-11, -5)
    display "mod(-11, -5) " r
    compute r = function mod(0, 3)
    display "mod(0, 3) " r
    compute r = function numval(" 12.5 ")
    display "numval("" 12.5 "") " r
    compute r = function numval("-3.25")
    display "numval(""-3.25"") " r
    compute r = function numval("4.5-")
    display "numval(""4.5-"") " r
    compute r = function numval("7 CR")
    display "numval(""7 CR"") " r
    compute r = function numval(" +.5")
    display "numval("" +.5"") " r
    compute r = function numval("12DB")
    display "numval(""12DB"") " r
    compute r = function numval-c("$1,234.56")
    display "numval-c(""$1,234.56"") " r
    compute r = function numval-c("1,234.56-")
    display "numval-c(""1,234.56-"") " r
    compute r = function numval-c("$12.00CR")
    display "numval-c(""$12.00CR"") " r
    compute r = function numval-c(" - $ 7")
    display "numval-c("" - $ 7"") " r
    compute r = function numval-f("1.5E3")
    display "numval-f(""1.5E3"") " r
    compute r = function numval-f("-2.5E-2")
    display "numval-f(""-2.5E-2"") " r
    compute r = function numval-f(" 12 ")
    display "numval-f("" 12 "") " r
    compute r = function ord("A")
    display "ord(""A"") " r
    compute r = function ord-max(3 9 1 9)
    display "ord-max(3 9 1 9) " r
    compute r = function ord-min(3 9 1 1)
    display "ord-min(3 9 1 1) " r
    compute r = function pi
    display "pi " r
    compute r = function e
    display "e " r
    compute r = function present-value(0.1, 100, 200)
    display "present-value(0.1, 100, 200) " r
    compute r = function present-value(0, 5, 5)
    display "present-value(0, 5, 5) " r
    compute r = function range(3 9 1)
    display "range(3 9 1) " r
    compute r = function range(-2.5 2.5)
    display "range(-2.5 2.5) " r
    compute r = function rem(11, 5)
    display "rem(11, 5) " r
    compute r = function rem(-11, 5)
    display "rem(-11, 5) " r
    compute r = function rem(11, -5)
    display "rem(11, -5) " r
    compute r = function rem(5.5, 2)
    display "rem(5.5, 2) " r
    compute r = function rem(-5.5, 2)
    display "rem(-5.5, 2) " r
    compute r = function sign(-2)
    display "sign(-2) " r
    compute r = function sign(0)
    display "sign(0) " r
    compute r = function sign(3)
    display "sign(3) " r
    compute r = function sqrt(0)
    display "sqrt(0) " r
    compute r = function sqrt(2)
    display "sqrt(2) " r
    compute r = function sqrt(10000000000)
    display "sqrt(10000000000) " r
    compute r = function sqrt(0.25)
    display "sqrt(0.25) " r
    compute r = function standard-deviation(2 4 4 4 5 5 7 9)
    display "standard-deviation(2 4 4 4 5 5 7 9) " r
    compute r = function standard-deviation(1 2)
    display "standard-deviation(1 2) " r
    compute r = function sum(1 2 3.5)
    display "sum(1 2 3.5) " r
    compute r = function sum(-1 -2)
    display "sum(-1 -2) " r
    compute r = function variance(1 2 3 4)
    display "variance(1 2 3 4) " r
    compute r = function variance(5)
    display "variance(5) " r
    compute r = function test-date-yyyymmdd(20230229)
    display "test-date-yyyymmdd(20230229) " r
    compute r = function test-date-yyyymmdd(20240229)
    display "test-date-yyyymmdd(20240229) " r
    compute r = function test-date-yyyymmdd(16001231)
    display "test-date-yyyymmdd(16001231) " r
    compute r = function test-date-yyyymmdd(20231301)
    display "test-date-yyyymmdd(20231301) " r
    compute r = function test-date-yyyymmdd(20230431)
    display "test-date-yyyymmdd(20230431) " r
    compute r = function test-day-yyyyddd(2023366)
    display "test-day-yyyyddd(2023366) " r
    compute r = function test-day-yyyyddd(2024366)
    display "test-day-yyyyddd(2024366) " r
    compute r = function test-day-yyyyddd(1600001)
    display "test-day-yyyyddd(1600001) " r
    compute r = function test-numval(" 12.5")
    display "test-numval("" 12.5"") " r
    compute r = function test-numval("1 2")
    display "test-numval(""1 2"") " r
    compute r = function test-numval("abc")
    display "test-numval(""abc"") " r
    compute r = function test-numval("12.5-")
    display "test-numval(""12.5-"") " r
    compute r = function test-numval-c("$1,234")
    display "test-numval-c(""$1,234"") " r
    compute r = function test-numval-c("1,,2")
    display "test-numval-c(""1,,2"") " r
    compute r = function test-numval-f("1.5E3")
    display "test-numval-f(""1.5E3"") " r
    compute r = function test-numval-f("1.5E")
    display "test-numval-f(""1.5E"") " r
    compute r = function length("abcde")
    display "length(""abcde"") " r
    compute r = function length(w8)
    display "length(w8) " r
    compute r = function byte-length(w8)
    display "byte-length(w8) " r
    compute r = function highest-algebraic(h1)
    display "highest-algebraic(h1) " r
    compute r = function lowest-algebraic(h1)
    display "lowest-algebraic(h1) " r
    compute r = function highest-algebraic(h2)
    display "highest-algebraic(h2) " r
    compute r = function lowest-algebraic(h2)
    display "lowest-algebraic(h2) " r
    display "upper-case(""MiXed 1"") [" function upper-case("MiXed 1") "]"
    display "lower-case(""MiXed 1"") [" function lower-case("MiXed 1") "]"
    display "reverse(""abc "") [" function reverse("abc ") "]"
    display "char(66) [" function char(66) "]"
    display "max(""ab"" ""b"" ""a"") [" function max("ab" "b" "a") "]"
    display "min(""ab"" ""b"" ""a"") [" function min("ab" "b" "a") "]"
    stop run.
end program fnreturn.
