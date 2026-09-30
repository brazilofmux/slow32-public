#!/usr/bin/env python3
"""nist-sql-schema.py DIR FILE AUTHID -- load one NIST SQL Test Suite schema
file into the SQLite databases libcob/esql.c uses (docs/esql.md).

Each schema (authorization id) is DIR/<AUTHID>.db, listed in DIR/schemas;
the owner's database is `main`, the others are attached by name, and the
owner's own qualifier (HU.STAFF) becomes main. -- as the runtime does it.
A schema file's elements are not separated by semicolons: each begins with
CREATE, GRANT, ALTER, INSERT or DROP at the top level.  CREATE SCHEMA,
GRANT, CREATE DOMAIN, ASSERTION, CHARACTER SET, COLLATION and TRANSLATION
are skipped (SQLite has none of them: behavior points).  Each element that
SQLite refuses is reported on stdout as "refused: <line>: <first words>:
<error>", and the count ends the output.  The host's sqlite3 module and
the guest's SQLite share one file format.
"""
import os, re, sqlite3, sys

STARTS = ('create', 'grant', 'revoke', 'alter', 'drop', 'insert', 'commit')
SKIP = (('create', 'schema'), ('grant',), ('revoke',), ('create', 'domain'), ('create', 'assertion'),
        ('create', 'character'), ('create', 'collation'), ('create', 'translation'), ('commit',))


def elements(text):
    """(line, text) for each top-level element"""
    out, cur, start, depth, q, i, line = [], [], 1, 0, None, 0, 1
    n = len(text)
    while i < n:
        c = text[i]
        if q:
            cur.append(c)
            if c == q: q = None
            if c == '\n': line += 1
            i += 1; continue
        if c in "'\"": q = c; cur.append(c); i += 1; continue
        if c == '-' and text[i:i + 2] == '--':
            while i < n and text[i] != '\n': i += 1
            continue
        if c == '(': depth += 1
        elif c == ')': depth -= 1
        elif c == ';' and depth == 0:
            s = ''.join(cur).strip()
            if s: out.append((start, s))
            cur = []; start = line; i += 1; continue
        if c.isalpha() and depth == 0 and (i == 0 or not (text[i - 1].isalnum() or text[i - 1] in '_.')):
            m = re.match(r'[A-Za-z_]+', text[i:])
            w = m.group(0).lower()
            prev = ''.join(cur).strip().lower()
            pw = prev.split()[-1] if prev.split() else ''
            first = prev.split()[0] if prev.split() else ''
            inside = pw in ('with', 'revoke', 'grant', ',') or pw.endswith(',') or \
                (first in ('grant', 'revoke') and w in ('insert', 'alter', 'drop')) or \
                (first == 'alter' and w == 'drop') or (first == 'create' and w == 'insert')
            if w in STARTS and prev and not inside:
                s = ''.join(cur).strip()
                if s: out.append((start, s))
                cur = []; start = line
        if c == '\n': line += 1
        cur.append(c); i += 1
    s = ''.join(cur).strip()
    if s: out.append((start, s))
    return out


def own(sql, user):
    """the owner's qualifier as main. outside quotes"""
    out, q, i = [], None, 0
    pat = re.compile(re.escape(user) + r'\.', re.I)
    while i < len(sql):
        c = sql[i]
        if q:
            out.append(c)
            if c == q: q = None
            i += 1; continue
        if c == "'": q = c; out.append(c); i += 1; continue
        m = pat.match(sql, i)
        if m and (i == 0 or not (sql[i - 1].isalnum() or sql[i - 1] in '_.')):
            out.append('main.'); i = m.end(); continue
        out.append(c); i += 1
    return ''.join(out)


def main():
    d, path, user = sys.argv[1], sys.argv[2], sys.argv[3].upper()
    cat = os.path.join(d, 'schemas')
    names = [l.strip() for l in open(cat)] if os.path.exists(cat) else []
    if user not in names:
        names.append(user)
        open(cat, 'w').write(''.join(n + '\n' for n in names))
    db = sqlite3.connect(os.path.join(d, user + '.db'))
    attached = []

    def attach_for(sql):
        """the other schemas this element names (the host's SQLite attaches
        at most 10 at once; the guest's is built for 125)"""
        want = [n for n in names if n != user and re.search(r'(?<![\w.])' + re.escape(n) + r'\.', sql, re.I)]
        for n in [a for a in attached if a not in want]:
            db.execute('DETACH "%s"' % n); attached.remove(n)
        for n in want:
            if n not in attached:
                db.execute('ATTACH ? AS "%s"' % n, (os.path.join(d, n + '.db'),)); attached.append(n)
    text = open(path, encoding='latin-1').read().replace('\r', '')
    refused = 0
    for line, s in elements(text):
        words = tuple(s.lower().split()[:2])
        if any(words[:len(k)] == k for k in SKIP):
            continue
        try:
            db.commit()
            attach_for(s)
            db.execute(own(s, user))
        except sqlite3.Error as e:
            refused += 1
            print('refused: %d: %s: %s' % (line, ' '.join(s.split()[:4]), e))
    db.commit()
    print('%s %s: %d refused' % (os.path.basename(path), user, refused))


if __name__ == '__main__':
    main()
