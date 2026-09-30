import random, sys
seed = int(sys.argv[1]); n = int(sys.argv[2]); random.seed(seed)
def pic(total, scale, signed=True):
    ip = total - scale
    p = ("s" if signed else "") + "9(%d)" % ip if ip > 0 else ("s" if signed else "")
    if scale: p += "v9(%d)" % scale
    return p
items = []
lines = ["identification division.", "program-id. wd.", "data division.", "working-storage section."]
for i in range(8):
    tot = random.randint(1, 31); sc = random.randint(0, min(tot, 12)) if random.random() < .6 else 0
    if tot - sc < 1: sc = tot - 1
    usage = random.choice(["", " binary", " packed-decimal"]) if sc == 0 or True else ""
    v = random.randint(0, 10**tot - 1) * random.choice([1, -1])
    s = str(abs(v)).rjust(tot, "0"); lit = ("-" if v < 0 else "") + (s[:tot-sc] + ("." + s[tot-sc:] if sc else ""))
    lines.append("01 v%d pic %s%s value %s." % (i, pic(tot, sc), usage, lit))
    items.append((tot, sc))
recv = []
for i in range(6):
    tot = random.randint(1, 31); sc = random.randint(0, min(tot - 1, 12))
    u = random.choice(["", " binary", " packed-decimal"]); u = "" if (u == " binary" and tot > 18) else u
    lines.append("01 r%d pic %s%s." % (i, pic(tot, sc), u))
    recv.append((tot, sc))
lines.append("procedure division.")
def comp(ops):
    ints = max(t - s for t, s in ops); fr = max(s for t, s in ops); return ints + fr
k = 0
while k < n:
    a, b = random.sample(range(8), 2); c = random.randrange(8)
    op = random.choice(["+", "-", "*"])
    r = random.randrange(6)
    expr = "v%d %s v%d" % (a, op, b)
    if random.random() < .4: expr = "(%s) %s v%d" % (expr, random.choice("+-"), c)
    rnd = " rounded" if random.random() < .5 else ""
    lines.append("    compute r%d%s = %s on size error display \"%d size\" not on size error display \"%d \" r%d end-compute" % (r, rnd, expr, k, k, r))
    k += 1
lines.append("    stop run.")
print("\n".join(lines))
