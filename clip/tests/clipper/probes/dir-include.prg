* Directives: #include, #translate, #xcommand with two markers,
* block and line comments
#include "dir-include.ch"
#translate DOUBLED(<v>) => ((<v>) + (<v>))
/* a block comment
   across two lines */
? GREETING          // a line comment
? HALF(10), DOUBLED(4)
SHOW "left" AND "right"
#ifndef GREETING
? "not shown"
#else
? "the header was read"
#endif
