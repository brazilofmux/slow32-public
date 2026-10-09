#!/usr/bin/env python3
"""Generate libcob/locale_data.h from CLDR: the LC_TIME and LC_MONETARY
fields of each locale in locale_names.h (docs/plans/locale.md, steps 2 and 5).

    libcob/gen_locale_data.py [CLDR_DIR] > libcob/locale_data.h

CLDR_DIR holds common/main/<locale>.xml for every locale named plus root,
and common/supplemental/{supplementalData,likelySubtags}.xml, as
<dir>/main/*.xml and <dir>/supplemental/*.xml -- the libutf checkout's
gen/data/cldr (default ~/utf/gen/data/cldr; gen/fetch_cldr.py there fills
and checks the cache), the same release (46) its collators come from.  Same Unicode license; no C library's locale files.

What each locale gives (the standard's 8.2.2 fields, by CLDR's names):
  LC_TIME     d_fmt, t_fmt     gregorian dateFormats / timeFormats, medium
              day and month names (format context, abbreviated and wide),
              the am and pm strings (format, abbreviated)
  LC_MONETARY decimal, group    numbers/symbols (latn)
              currency pattern  currencyFormats standard (latn)
              the currency      likelySubtags -> region -> currencyData,
                                its symbol in the locale, its fraction digits
An element written as the inheritance marker (three up arrows) or missing
is taken from the parent: supplementalData's parentLocales, else the
locale truncated a subtag at a time, down to root.  Elements with an alt
attribute are variants and skipped.

The POSIX locale (index 0) is the standard's own: %m/%d/%y and %H:%M:%S
written in CLDR's pattern letters (MM/dd/yy, HH:mm:ss), English names,
no currency.
"""
import os, sys, re
import xml.etree.ElementTree as ET

CLDR = sys.argv[1] if len(sys.argv) > 1 else os.path.expanduser("~/utf/gen/data/cldr")
HERE = os.path.dirname(os.path.abspath(__file__))

# the locale list, from locale_names.h
names = []
for line in open(os.path.join(HERE, "locale_names.h")):
    if line.strip().startswith('"'):
        names += re.findall(r'"([^"]+)"', line)
assert names[0] == "POSIX", names[:3]

INHERIT = "↑↑↑"
trees = {}
def tree(loc):
    if loc not in trees:
        trees[loc] = ET.parse(os.path.join(CLDR, "main", loc + ".xml")).getroot()
    return trees[loc]

supp = ET.parse(os.path.join(CLDR, "supplemental", "supplementalData.xml")).getroot()
likely = ET.parse(os.path.join(CLDR, "supplemental", "likelySubtags.xml")).getroot()
parents = {}
for pl in supp.iter("parentLocale"):
    if pl.get("component"): continue          # collation/segmentation parents are not ours
    for l in pl.get("locales").split():
        parents[l] = pl.get("parent")
likely_to = {e.get("from"): e.get("to") for e in likely.iter("likelySubtag")}

def chain(loc):
    out = [loc]
    while out[-1] != "root":
        l = out[-1]
        p = parents.get(l)
        if p is None:
            p = l.rsplit("_", 1)[0] if "_" in l else "root"
        out.append(p)
    return out

def alias_of(root, cpath):
    """a CLDR <alias source="locale" path="../x[@type='y']"/> on the
    container at cpath: the path it stands for, relative to the requesting
    locale (that is what source="locale" means: root's "abbreviated months
    are the wide ones" is each locale's own wide months); None when the
    container is absent or holds values"""
    c = root.find(cpath)
    if c is None: return None
    a = c.find("alias")
    if a is None or a.get("source") != "locale": return None
    parts = cpath.split("/")
    for step in a.get("path").split("/"):
        if step == "..": parts.pop()
        else: parts.append(step)
    return "/".join(parts)

def find(loc, path, default=None, depth=0):
    """the first inherited value of an element path: a missing element or
    the inheritance marker means the parent's; alt variants are skipped;
    an alias met on the way restarts the search from the requesting locale
    along the aliased path"""
    if depth > 6: raise SystemExit("alias loop at %s for %s" % (path, loc))
    cpath, leaf = path.rsplit("/", 1)
    for l in chain(loc):
        root = tree(l)
        a = alias_of(root, cpath)
        if a: return find(loc, a + "/" + leaf, default, depth + 1)
        for e in root.findall(cpath + "/" + leaf):
            if e.get("alt"): continue
            t = e.text or ""
            if t == INHERIT: break          # explicit: the parent's
            return t
    if default is not None: return default
    raise SystemExit("no %s for %s" % (path, loc))

GREG = "dates/calendars/calendar[@type='gregorian']/"
MONTHS = [str(i) for i in range(1, 13)]
DAYS = ["sun", "mon", "tue", "wed", "thu", "fri", "sat"]

def region(loc):
    t = likely_to.get(loc) or likely_to.get(loc.split("_")[0])
    if "_" in loc and loc.rsplit("_", 1)[1].isupper() and len(loc.rsplit("_", 1)[1]) == 2:
        return loc.rsplit("_", 1)[1]
    return t.split("_")[-1] if t else None

def currency_of(reg):
    for r in supp.iter("region"):
        if r.get("iso3166") != reg: continue
        cur = [c for c in r.findall("currency") if not c.get("to") and c.get("tender", "true") != "false"]
        if cur: return cur[0].get("iso4217")
    return None

def digits_of(code):
    d = None
    for i in supp.iter("info"):
        if i.get("iso4217") == code: return int(i.get("digits"))
        if i.get("iso4217") == "DEFAULT": d = int(i.get("digits"))
    return d

def cstr(s):
    out = []
    for ch in s:
        b = ch.encode("utf-8")
        if ch == '"' or ch == "\\": out.append("\\" + ch)
        elif 32 <= ord(ch) < 127: out.append(ch)
        else: out.append("".join("\\%03o" % x for x in b))
    # an octal escape followed by a digit would be misread: split the literal there
    return '"' + re.sub(r'(\\[0-7]{3})(?=[0-9])', r'\1" "', "".join(out)) + '"'

def record(loc):
    if loc == "POSIX":
        return dict(date="MM/dd/yy", time="HH:mm:ss", am="AM", pm="PM",
                    mon_abbr="Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec".split(),
                    mon_wide="January February March April May June July August September October November December".split(),
                    day_abbr="Sun Mon Tue Wed Thu Fri Sat".split(),
                    day_wide="Sunday Monday Tuesday Wednesday Thursday Friday Saturday".split(),
                    decimal=".", group=",", curfmt="", curcode="", cursym="", frac=2)
    r = dict(
        date=find(loc, GREG + "dateFormats/dateFormatLength[@type='medium']/dateFormat/pattern"),
        time=find(loc, GREG + "timeFormats/timeFormatLength[@type='medium']/timeFormat/pattern"),
        am=find(loc, GREG + "dayPeriods/dayPeriodContext[@type='format']/dayPeriodWidth[@type='abbreviated']/dayPeriod[@type='am']"),
        pm=find(loc, GREG + "dayPeriods/dayPeriodContext[@type='format']/dayPeriodWidth[@type='abbreviated']/dayPeriod[@type='pm']"),
        mon_abbr=[find(loc, GREG + "months/monthContext[@type='format']/monthWidth[@type='abbreviated']/month[@type='%s']" % m) for m in MONTHS],
        mon_wide=[find(loc, GREG + "months/monthContext[@type='format']/monthWidth[@type='wide']/month[@type='%s']" % m) for m in MONTHS],
        day_abbr=[find(loc, GREG + "days/dayContext[@type='format']/dayWidth[@type='abbreviated']/day[@type='%s']" % d) for d in DAYS],
        day_wide=[find(loc, GREG + "days/dayContext[@type='format']/dayWidth[@type='wide']/day[@type='%s']" % d) for d in DAYS],
        decimal=find(loc, "numbers/symbols[@numberSystem='latn']/decimal"),
        group=find(loc, "numbers/symbols[@numberSystem='latn']/group"),
        curfmt=find(loc, "numbers/currencyFormats[@numberSystem='latn']/currencyFormatLength/currencyFormat[@type='standard']/pattern"),
    )
    reg = region(loc)
    code = currency_of(reg) if reg else None
    r["curcode"] = code or ""
    r["cursym"] = find(loc, "numbers/currencies/currency[@type='%s']/symbol" % code, default=code) if code else ""   # no symbol: the code is the symbol (CLDR's rule)
    r["frac"] = digits_of(code) if code else 2
    return r

print("/* locale_data.h -- GENERATED by libcob/gen_locale_data.py from CLDR 46")
print(" * (Unicode License V3: NOTICE, libcob/utf/LICENSE-UNICODE; libutf's\n * cache, gen/data/cldr): the LC_TIME and")
print(" * LC_MONETARY fields of each locale of locale_names.h, in its order.")
print(" * Patterns use CLDR's letters (y M d E H h m s a); see docs/plans/locale.md. */")
print("typedef struct {")
print("    const char *name;")
print("    const char *date_fmt, *time_fmt, *am, *pm;    /* LC_TIME: d_fmt, t_fmt (gregorian, medium) */")
print("    const char *mon_abbr[12], *mon_wide[12];     /* month names, format context */")
print("    const char *day_abbr[7], *day_wide[7];       /* Sunday first */")
print("    const char *decimal, *group;                 /* LC_MONETARY (and LC_NUMERIC): the symbols */")
print("    const char *cur_fmt, *cur_code, *cur_symbol; /* the standard currency pattern, the region's currency, its symbol here */")
print("    int frac_digits;")
print("} cob_locale_data;")
print("static const cob_locale_data cob_locale_data_tab[] = {")
for loc in names:
    r = record(loc)
    print("    { %s," % cstr(loc))
    print("      %s, %s, %s, %s," % (cstr(r["date"]), cstr(r["time"]), cstr(r["am"]), cstr(r["pm"])))
    print("      { %s }," % ", ".join(cstr(x) for x in r["mon_abbr"]))
    print("      { %s }," % ", ".join(cstr(x) for x in r["mon_wide"]))
    print("      { %s }," % ", ".join(cstr(x) for x in r["day_abbr"]))
    print("      { %s }," % ", ".join(cstr(x) for x in r["day_wide"]))
    print("      %s, %s, %s, %s, %s, %d }," % (cstr(r["decimal"]), cstr(r["group"]), cstr(r["curfmt"]), cstr(r["curcode"]), cstr(r["cursym"]), r["frac"]))
print("};")
