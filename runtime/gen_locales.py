#!/usr/bin/env python3
"""Generate runtime/locales_data.h from CLDR: the named locales of
locales.c, one record each, in the form locale_impl.h declares.

    runtime/gen_locales.py [CLDR_DIR] > runtime/locales_data.h

The locales are the COBOL runtime's: POSIX's 53 companions in
cobol/libcob/locale_names.h, libutf's CLDR 46 collators.  CLDR_DIR is the
same cache that runtime uses (default ~/utf/gen/data/cldr), and the CLDR
reading -- inheritance through parentLocales and truncation, aliases, the
region's currency -- is cobol/libcob/gen_locale_data.py's, imported, so
that the two runtimes cannot disagree on a value both of them have.

What a record holds, by POSIX's names:
  LC_TIME     d_fmt, t_fmt     gregorian dateFormats / timeFormats, medium,
                               turned into strftime conversions (below)
              d_t_fmt          the medium dateTimeFormat gluing those two
              t_fmt_ampm       t_fmt when it has a 12-hour clock, else
                               POSIX's %I:%M:%S %p
              the names        format context, wide and abbreviated
              am_str, pm_str   format, abbreviated
  LC_NUMERIC  decimal_point, thousands_sep    numbers/symbols (latn)
              grouping         the decimal pattern's group sizes
  LC_MONETARY the currency     the region's (likelySubtags -> currencyData):
                               int_curr_symbol is its ISO code and a space,
                               currency_symbol its symbol in the locale
              where it goes    the standard currency pattern: before or
                               after the number, with a space or without
              mon_*            the currency symbols (CLDR's currencyDecimal
                               and currencyGroup where it has them) and the
                               currency pattern's group sizes
  LC_CTYPE    codeset "UTF-8": CLDR's strings are UTF-8, and so are the
              strings these names are compared with
  LC_MESSAGES yesexpr, noexpr: POSIX's -- CLDR 46 has no yes/no strings

A CLDR date pattern becomes a strftime format letter by letter: y and
yyyy %Y, yy %y; M %-m, MM %m, MMM (and L...) %b, MMMM %B; d %-d, dd %d;
E..EEE %a, EEEE %A; H %-H, HH %H; h %-I, hh %I; m %-M, mm %M; s %-S,
ss %S; a %p.  Quoted text is literal ('' is a quote) and a literal % is
%%.  Any other letter stops the generator: a format that silently lost a
field would be worse than none.
"""
import os, sys, re

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(HERE, "..", "cobol", "libcob"))
import gen_locale_data as G          # reads sys.argv[1], the CLDR directory, as this does

CONV = {
    "y": "%Y", "yy": "%y", "yyyy": "%Y",
    "M": "%-m", "MM": "%m", "MMM": "%b", "MMMM": "%B",
    "L": "%-m", "LL": "%m", "LLL": "%b", "LLLL": "%B",
    "d": "%-d", "dd": "%d",
    "E": "%a", "EE": "%a", "EEE": "%a", "EEEE": "%A",
    "H": "%-H", "HH": "%H", "h": "%-I", "hh": "%I",
    "m": "%-M", "mm": "%M", "s": "%-S", "ss": "%S",
    "a": "%p",
}

def strftime_of(pat, loc):
    out, i = [], 0
    while i < len(pat):
        c = pat[i]
        if c == "'":
            j = pat.find("'", i + 1)
            if j < 0: raise SystemExit("%s: unclosed quote in %r" % (loc, pat))
            out.append("'" if j == i + 1 else pat[i + 1:j].replace("%", "%%"))
            i = j + 1
        elif c.isascii() and c.isalpha():
            j = i
            while j < len(pat) and pat[j] == c: j += 1
            run = pat[i:j]
            if run not in CONV: raise SystemExit("%s: no strftime form for %r in %r" % (loc, run, pat))
            out.append(CONV[run])
            i = j
        else:
            out.append("%%" if c == "%" else c)
            i += 1
    return "".join(out)

def glue(pat, date, time, loc):
    """the dateTimeFormat: {1} the date, {0} the time, quoted text literal"""
    out, i = [], 0
    while i < len(pat):
        if pat.startswith("{0}", i): out.append(time); i += 3
        elif pat.startswith("{1}", i): out.append(date); i += 3
        elif pat[i] == "'":
            j = pat.find("'", i + 1)
            if j < 0: raise SystemExit("%s: unclosed quote in %r" % (loc, pat))
            out.append("'" if j == i + 1 else pat[i + 1:j].replace("%", "%%"))
            i = j + 1
        else:
            out.append("%%" if pat[i] == "%" else pat[i]); i += 1
    return "".join(out)

def grouping(pat):
    """a number pattern's group sizes as a C grouping string: "#,##0.###"
    groups by threes ("\\3"), "#,##,##0.###" by three then twos ("\\3\\2")"""
    num = pat.split(";")[0]
    num = re.sub(r"[^#0,.]", "", num).split(".")[0]
    parts = num.split(",")
    if len(parts) < 2: return ""
    primary = len(parts[-1])
    if len(parts) > 2 and len(parts[-2]) != primary:
        return "\\%o\\%o" % (primary, len(parts[-2]))
    return "\\%o" % primary

def cstr(s):
    return G.cstr(s)

def placement(pat, loc):
    """(cs_precedes, sep_by_space) of the positive currency pattern: which
    side of the number the symbol is on, and whether a space parts them"""
    p = pat.split(";")[0]
    k = p.find("\u00a4")
    if k < 0: raise SystemExit("%s: no currency sign in %r" % (loc, pat))
    digits = [m.start() for m in re.finditer(r"[#0]", p)]
    between = p[k + 1:digits[0]] if k < digits[0] else p[digits[-1] + 1:k]
    return (1 if k < digits[0] else 0), (1 if re.search("[ \u00a0\u202f]", between) else 0)

def sym(loc, what, fallback):
    return G.find(loc, "numbers/symbols[@numberSystem='latn']/" + what, default=fallback)

def record(loc):
    r = G.record(loc)
    date = strftime_of(r["date"], loc)
    time = strftime_of(r["time"], loc)
    dt = G.find(loc, G.GREG + "dateTimeFormats/dateTimeFormatLength[@type='medium']/dateTimeFormat/pattern")
    decfmt = G.find(loc, "numbers/decimalFormats[@numberSystem='latn']/decimalFormatLength/decimalFormat/pattern")
    if len(r["decimal"].encode()) != 1:
        raise SystemExit("%s: a decimal point of more than one byte (%r): printf takes one" % (loc, r["decimal"]))
    has_cur = bool(r["curcode"])          # eo has no region, so no currency
    cs_pre, sep = placement(r["curfmt"], loc) if has_cur else (127, 127)
    info = {
        "CODESET": "UTF-8",
        "D_T_FMT": glue(dt, date, time, loc),
        "D_FMT": date,
        "T_FMT": time,
        "T_FMT_AMPM": time if "%p" in time else "%I:%M:%S %p",
        "AM_STR": r["am"], "PM_STR": r["pm"],
        "RADIXCHAR": r["decimal"], "THOUSEP": r["group"],
        "YESEXPR": "^[yY]", "NOEXPR": "^[nN]",
        "CRNCYSTR": (("-" if cs_pre else "+") + r["cursym"]) if has_cur else "",
    }
    for i in range(7):
        info["DAY_%d" % (i + 1)] = r["day_wide"][i]
        info["ABDAY_%d" % (i + 1)] = r["day_abbr"][i]
    for i in range(12):
        info["MON_%d" % (i + 1)] = r["mon_wide"][i]
        info["ABMON_%d" % (i + 1)] = r["mon_abbr"][i]
    conv = dict(
        decimal_point=r["decimal"], thousands_sep=r["group"], grouping=grouping(decfmt),
        int_curr_symbol=r["curcode"] + " " if has_cur else "", currency_symbol=r["cursym"],
        mon_decimal_point=sym(loc, "currencyDecimal", r["decimal"]),
        mon_thousands_sep=sym(loc, "currencyGroup", r["group"]),
        mon_grouping=grouping(r["curfmt"]), positive_sign="", negative_sign="-",
    )
    # without a currency, CHAR_MAX (127) for "not available", as in the C locale
    frac = r["frac"] if has_cur else 127
    nums = dict(int_frac_digits=frac, frac_digits=frac,
                p_cs_precedes=cs_pre, p_sep_by_space=sep, n_cs_precedes=cs_pre, n_sep_by_space=sep,
                p_sign_posn=1 if has_cur else 127, n_sign_posn=1 if has_cur else 127,
                int_p_cs_precedes=cs_pre, int_p_sep_by_space=sep,
                int_n_cs_precedes=cs_pre, int_n_sep_by_space=sep,
                int_p_sign_posn=1 if has_cur else 127, int_n_sign_posn=1 if has_cur else 127)
    return info, conv, nums

def main():
    names = G.names[1:]                  # [0] is POSIX, which is libc's own C locale
    print("/* locales_data.h -- GENERATED by runtime/gen_locales.py from CLDR 46")
    print(" * (Unicode License V3: utf/LICENSE-UNICODE), through")
    print(" * cobol/libcob/gen_locale_data.py's reading of it: the named locales")
    print(" * of locales.c, in cobol/libcob/locale_names.h's order. */")
    print("static const struct __s32_locdata named_locales[] = {")
    for loc in names:
        info, conv, nums = record(loc)
        print("    { %s, {" % cstr(loc))
        for k, v in info.items():
            print("        [%s] = %s," % (k, cstr(v)))
        print("      }, {")
        for k, v in conv.items():
            # a grouping is already escaped: its digits are octal escapes
            print("        .%s = %s," % (k, '"%s"' % v if k.endswith("grouping") else cstr(v)))
        print("        " + ", ".join(".%s = %d" % kv for kv in nums.items()) + ",")
        print("      } },")
    print("};")
    print("#define N_NAMED_LOCALES ((int)(sizeof named_locales / sizeof named_locales[0]))")

if __name__ == "__main__":
    main()
