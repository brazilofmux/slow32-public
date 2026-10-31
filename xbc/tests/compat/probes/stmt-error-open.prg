// A failed USE is an open error that can be retried or defaulted: the
// default handler offers Quit, Retry and Default, and quitting ends the
// program with the file named in the message.
PROCEDURE Main
   ? "open"
   USE nosuch
   ? "not reached"
   RETURN
