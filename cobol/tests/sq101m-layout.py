#!/usr/bin/env python3
# tests/sq101m-layout.py REPORT: render CCVS-85 SQ101M's print file as a line
# printer would and check the layout the program claims for itself (every
# "THIS LINE SHOULD BE a LINES BELOW AND b LINES ABOVE", top of page, last
# line, next line, and each overprint jumbling exactly five A/B cells).
# Harness gate 5 runs it; exit 0 when every claim holds (cobol ISSUES-46).
import re, sys

"""Render a print file as a line printer would and check SQ101M's own
claims about where each line lands.

Printer model: '\n' moves to the next line, column 0; '\r' returns to
column 0 of the same line (the next text overprints); '\f' starts a new
page at line 0.  A cell keeps every character struck on it, so an
overprint is visible as a cell with two different non-space characters.
"""
def render(data):
    pages = [[]]
    line = 0; col = 0
    def cell(l, c):
        page = pages[-1]
        while len(page) <= l: page.append([])
        row = page[l]
        while len(row) <= c: row.append('')
        return row
    for ch in data:
        if ch == '\n': line += 1; col = 0
        elif ch == '\r': col = 0
        elif ch == '\f': pages.append([]); line = 0; col = 0
        else:
            row = cell(line, col)
            if ch != ' ': row[col] += ch
            col += 1
    out = []
    for page in pages:
        out.append([''.join((c[-1] if c else ' ') for c in row).rstrip() for row in page])
    return pages, out

TEST = re.compile(r'^(?!.*THIS LINE).*(WRT-TEST-(?:GF-)?\d+(?![\d/])|AFTER-LAST-TEST)')

def check(path):
    data = open(path, 'rb').read().decode('latin-1')
    raw, pages = render(data)
    fails = []; checked = 0
    for pn, page in enumerate(pages):
        for i, text in enumerate(page):
            m = re.search(r'THIS LINE SHOULD BE (\d+) +LINES? BELOW AND (\d+) +LINES? ABOVE', text)
            if m:
                a, b = int(m.group(1)), int(m.group(2)); checked += 1
                up = next((i - j for j in range(i - 1, -1, -1) if TEST.search(page[j])), None)
                dn = next((j - i for j in range(i + 1, len(page)) if TEST.search(page[j])), None)
                if up != a or (dn is not None and dn != b):
                    fails.append(f'p{pn} l{i}: says {a} below/{b} above, is {up} below/{dn} above: {text.strip()[:60]}')
                continue
            if 'SHOULD APPEAR AT THE TOP OF A NEW PAGE' in text:
                checked += 1
                if i != 0: fails.append(f'p{pn} l{i}: should be at the top of a new page')
            if 'ALSO BE THE LAST LINE' in text:
                checked += 1
                if any(t.strip() for t in page[i + 1:]): fails.append(f'p{pn} l{i}: should be the last line on its page')
            if 'SHOULD FOLLOW IMMEDIATELY ON THE NEXT LINE' in text:
                checked += 1
                if i + 1 >= len(page) or not TEST.search(page[i + 1]): fails.append(f'p{pn} l{i}: a WRT-TEST line should follow on the next line')
            if re.search(r'WRT-TEST-\d+/ THIS LINE SHOULD BE OVERPRINTED', text):
                checked += 1
                row = raw[pn][i]
                ab = sum(1 for c in row if len(set(c)) > 1 and set(c) <= {'A', 'B'})
                if ab != 5: fails.append(f'p{pn} l{i}: overprint jumbles {ab} A/B cells, the test says five')
    return checked, fails

if __name__ == "__main__":
    checked, fails = check(sys.argv[1])
    for f in fails[:20]: print(f)
    print(f"{checked - len(fails)} of {checked} layout claims hold")
    sys.exit(0 if checked and not fails else 1)
