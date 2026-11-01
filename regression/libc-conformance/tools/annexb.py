#!/usr/bin/env python3
"""List every identifier Annex B (the library summary) gives each header.

usage: annexb.py n1570.txt > annexb.tsv     (n1570.txt: pdftotext -layout
of WG14's N1570; it is not in the tree -- see ../README.md)
Output: header, kind (func = a prototype, name = a macro, type or object),
identifier.  Only names are taken; the standard's text stays out.
"""
import re, sys, json
text=open(sys.argv[1]).read().split('\n')
start=[i for i,l in enumerate(text) if l.strip()=='Annex B'][-1]
end=[i for i,l in enumerate(text) if l.strip()=='Annex C'][-1]
lines=text[start:end]
out=[]; hdr=None; clause=None; buf=''
def flush_proto(s):
    m=re.search(r'([A-Za-z_][A-Za-z0-9_]*)\s*\((?:[^()]|\([^()]*\))*\)\s*;\s*$', s)
    return m.group(1) if m else None
for ln in lines:
    if re.match(r'\s*(§B\.|\d+\s+Library summary|N1570|ISO/IEC)', ln) or 'Library summary' in ln or 'Committee Draft' in ln: continue
    m=re.match(r'B\.(\d+)\s+.*<([a-z0-9/]+\.h)>', ln.strip())
    if m:
        hdr=m.group(2); clause=int(m.group(1)); buf=''; continue
    if hdr is None: continue
    s=ln.strip().replace('_ _','__').replace('_ _','__')
    if not s or s.startswith('#pragma') or s.startswith('(informative)') or s=='Annex B': continue
    if buf or '(' in s:
        buf=(buf+' '+s).strip()
        if buf.endswith(';'):
            n=flush_proto(buf)
            if n: out.append((hdr,'func',n))
            buf=''
        elif not re.search(r'[(,]\s*$|[A-Za-z0-9_*]\s*$', buf) : buf=''
        continue
    for tok in re.split(r'\s{2,}', s):
        tok=tok.strip()
        if re.fullmatch(r'[A-Za-z_][A-Za-z0-9_]*', tok): out.append((hdr,'name',tok))
        elif re.fullmatch(r'struct [A-Za-z_][A-Za-z0-9_]*', tok): out.append((hdr,'name',tok))
        elif re.fullmatch(r'[A-Za-z_][A-Za-z0-9_ ]*', tok):
            ws=tok.split(); i=0
            while i<len(ws):
                if ws[i]=='struct' and i+1<len(ws): out.append((hdr,'name','struct '+ws[i+1])); i+=2
                else: out.append((hdr,'name',ws[i])); i+=1
seen=set(); res=[]
for h,k,n in out:
    if (h,n) in seen: continue
    seen.add((h,n)); res.append((h,k,n))
for r in res: print('\t'.join(r))
