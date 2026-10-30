// FREAD(handle, @buffer, bytes): the bytes read into the buffer passed by
// reference, the count returned; FREADSTR beside it.
PROCEDURE Main
   LOCAL h, cBuf, n
   h := FCREATE("frtest.tmp")
   FWRITE(h, "alpha beta gamma")
   FCLOSE(h)
   h := FOPEN("frtest.tmp")
   cBuf := SPACE(5)
   n := FREAD(h, @cBuf, 5)
   ? "read", n, "[" + cBuf + "]"
   n := FREAD(h, @cBuf, 5)
   ? "next", n, "[" + cBuf + "]"
   ? "str", FREADSTR(h, 3)
   FCLOSE(h)
   FERASE("frtest.tmp")
   RETURN
