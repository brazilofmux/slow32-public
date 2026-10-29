* Directives: #ifdef / #ifndef / #undef
#define DEBUGGING
#ifdef DEBUGGING
? "on"
#else
? "off"
#endif
#undef DEBUGGING
#ifndef DEBUGGING
? "now off"
#endif
