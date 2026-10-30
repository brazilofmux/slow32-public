* CA: SAMPLE/ENVIRON.PRG SAMPLE/STRING.PRG
? "[" + FilePath("C:\DIR\SUB\FILE.TXT") + "]", "[" + FileBase("C:\DIR\SUB\FILE.TXT") + "]"
? "[" + FileExt("C:\DIR\SUB\FILE.TXT") + "]", "[" + FileDrive("C:\DIR\SUB\FILE.TXT") + "]"
? "[" + FileBase("NOEXT") + "]", "[" + FileExt("NOEXT") + "]", "[" + FilePath("NOEXT") + "]"
