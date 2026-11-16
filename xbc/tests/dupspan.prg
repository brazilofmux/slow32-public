* Runs of equal keys that span index pages: SEEK finds the run's first
* record (it landed part-way, right of the separator), and changing a
* key removes the old entry wherever in the run it is (a removal routed
* right of the separator missed it and left it in the index)
* -- the walk in key order; equal keys keep the order they came in, as
* dBASE keeps them, so a record REPLACEd into a run is not in record order
* -- the 2,000 records' keys, one letter each (a for "aaaaaaaa"), in a
* string: a program of 4,600 straight lines took stage08 cc six minutes
x = DBCREATE("DSP", {{"K", "C", 8, 0}, {"T", "N", 5, 0}})
keys = ""
keys = keys + "bcbbccaacbccaabbaacccacbbccccacacaaaaacabbbcacacbbacabcbbcacbbacbaacababbaacaaaabbcbbaccacbbabbabaaa"
keys = keys + "caaabbaccabcacabcbabbaabcbaaabcccaaaabbacbbbaaaaccaacbbcbacbcabacabacacaacccacbbcabaaaacbaccbbbcbbca"
keys = keys + "caaabccccabaacaabbbaaaabbccaababbcccccacaabbcbaaccabacbbaaabcbcacccabbacbababcaccbbcacaaacabbcbbcccb"
keys = keys + "bbcaccabcabaacabccbbabbaccacabcbbabbcbacbaabacbaaabbacabaabbbabbccbcbccbbaabcaccbcbbccacabbaabaaacab"
keys = keys + "accccbacaabbacabccaaabbcccccabcaabcbabbacabccccbbbcaacccaaababcbbcabcaabbcbbbaaabaacbcabacccccbbccaa"
keys = keys + "bccbcccacccbbcbcabaccbcbabcacacbacacbccabacaacbbbbabbabbbcbaabcabacaaaaaacbbcbacbabccaccbcaccbabcbcc"
keys = keys + "acacccacacccaaccbabcbaaaaaaacabbabbccaabccaaaccbbacccbccaabaaaccccbaaaaaaabcacaacbaabbbbcbbacbaccaaa"
keys = keys + "bbbcbbacaacabaccbccaccbbbaccababccbbabacaabbbaabccaabcbbacbcacabcbcacabcaacacabbbcaacbaabbbbcccbccac"
keys = keys + "bcacccababbabcccabbbbcbcbbcbbcbcbcccbaaacacabcbabcbaccbaabcbbaacacbaaccaacbbaaabcaabbacbbcbcccbbccab"
keys = keys + "caabacbccbbbccccccacbaaaccacacaabbbacaaacbaacacacbbbbabbbcaaabcabbcbbabaccbbccabcbcabbacabccbcbbaaba"
keys = keys + "ccacbbccbbaabccbbabcbcccbbcbbbabccaabbabcbcbaccbbcaccaaabbaacacacbacbaccbcababcabaaccaaccbacabcbbbbb"
keys = keys + "bcabbccaccabcbababccacbcaabbaacacbcababbcbbabbcabcaaabacbabaaabacaccbccababbabcccbbbbbccababaaabccba"
keys = keys + "caabbbacbcabababcacccbccaacaaaaaabbcccabcbaccccbaabacabaacbcbabbbbacbbccabacaacbcbcacbabccbabcabacac"
keys = keys + "abcbccbbbccabaabcabbbcbccbbbaaacaacccaaababccaaaaaabbabbcacaacbbbbcacbccbacccabbabbcbacbacbbaccabcbc"
keys = keys + "abcccaccacccacaccaaaabcbcbbabbbabcaaccccacbacbcaaabcbacbaaccababacabccbcccaaabbaacbacbabcabacbacacca"
keys = keys + "bbcabbbcccbbbbcbbacacbccbbababccbcbcbbcaaabacbabcacacbcbaaccbcbcaaccbcaaacaaccbabacbabaabbcbabccaaba"
keys = keys + "bbbbbabaabcaabbbaacccaccaaacabcbbbcabcbbaaabaccabacaccbbbccaccabacabaccababcacaaccbcbbcaabaaabccaaaa"
keys = keys + "cbcbabaaaccbbcbacbccccccacaacbacbcbccabbcaccbbaaccacbabacacbbcbcbaaaacccccacaabbacbcccbcbaccabbaabcc"
keys = keys + "cbabbbbcacaabcaacbccaaabcaabcbbbaaccccabbacabbaabbbcaaabccacbcccabbabbaabbaacababacbbbbbccabacbbaacb"
keys = keys + "bbcacacbcbcccababaacbaacaabbbabbbccabbcabaaaccaaabbbbccacccbbcbcbaabccbbabcbaabbbcaaaaccbcacbccaaccb"
USE DSP
i = 1
DO WHILE i <= 2000
   APPEND BLANK
   REPLACE K WITH REPLICATE(SUBSTR(keys, i, 1), 8), T WITH i
   i = i + 1
ENDDO
INDEX ON K TO DSPN
SEEK "aaaaaaaa"
? "first a:", RECNO()
SEEK "bbbbbbbb"
? "first b:", RECNO()
SEEK "cccccccc"
? "first c:", RECNO()
* every seventh record's key turned a to c, b to a, c to b
i = 7
DO WHILE i <= 1995
   GO i
   REPLACE K WITH IIF(K = "a", "cccccccc", IIF(K = "b", "aaaaaaaa", "bbbbbbbb"))
   i = i + 7
ENDDO
n = 0
ok = .T.
prev = ""
GO TOP
DO WHILE .NOT. EOF()
   IF K < prev
      ok = .F.
   ENDIF
   prev = K
   n = n + 1
   SKIP
ENDDO
? "entries:", n, "in order:", ok
SEEK "aaaaaaaa"
? "first a now:", RECNO()
SEEK "bbbbbbbb"
? "first b now:", RECNO()
SEEK "cccccccc"
? "first c now:", RECNO()
USE
ERASE DSP.DBF
ERASE DSPN.NDX
