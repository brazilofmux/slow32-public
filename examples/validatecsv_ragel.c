
#line 1 "validatecsv.rl"
// SLOW-32 CSV Validator using Ragel -G2 goto-driven parsing
// Generate with: ragel -G2 validatecsv.rl -o validatecsv_ragel.c
//
// This is adapted from the original validatecsv.rl for SLOW-32:
// - No mmap (not available)
// - Uses fopen/fread/fseek/ftell/fclose
// - No fprintf to stderr (uses printf to stdout)
// - Simplified argument parsing

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    const char* filename;
    bool isValid;
    int expectedFields;
    int lineNumber;
} FileResult;

static volatile int gVerbose = 0;

typedef struct {
    FileResult* result;
    bool haveExpected;
    int fields;
    uint8_t state;
    int cs;
    const unsigned char* ts;
    const unsigned char* te;
    int act;
    bool initialized;
} ParserContext;

static void finalizeRecord(ParserContext* ctx) {
    if (ctx->haveExpected) {
        if (ctx->fields != ctx->result->expectedFields) {
            ctx->result->isValid = false;
            if (gVerbose) {
                printf("%s: Line %d contains %d fields instead of the expected %d fields.\n",
                       ctx->result->filename, ctx->result->lineNumber, ctx->fields,
                       ctx->result->expectedFields);
            }
        }
    } else {
        ctx->result->expectedFields = ctx->fields;
        ctx->haveExpected = true;
    }
    ctx->fields = 0;
    ++ctx->result->lineNumber;
}

static void errorUnexpectedChar(ParserContext* ctx, unsigned char ch) {
    ctx->result->isValid = false;
    if (gVerbose) {
        printf("%s: Unexpected character '%c' (decimal %d) on line %d.\n",
               ctx->result->filename, (char)ch, (int)ch, ctx->result->lineNumber);
    }
}

static void unexpectedEof(ParserContext* ctx) {
    ctx->result->isValid = false;
    if (gVerbose) {
        printf("%s: Unexpected end of file while parsing line %d.\n",
               ctx->result->filename, ctx->result->lineNumber);
    }
}


#line 144 "validatecsv.rl"



#line 69 "validatecsv_ragel.c"
static const int csv_loose_c_start = 0;
static const int csv_loose_c_first_final = 0;
static const int csv_loose_c_error = -1;

static const int csv_loose_c_en_main = 0;
static const int csv_loose_c_en_quoted = 1;
static const int csv_loose_c_en_after_quote = 2;
static const int csv_loose_c_en_save_quote = 3;
static const int csv_loose_c_en_unquoted = 4;
static const int csv_loose_c_en_after_field = 5;
static const int csv_loose_c_en_cr0 = 6;
static const int csv_loose_c_en_cr1 = 7;
static const int csv_loose_c_en_after_field_cr = 8;


#line 147 "validatecsv.rl"


#line 226 "validatecsv.rl"



#line 86 "validatecsv_ragel.c"
static const int csv_strict_c_start = 0;
static const int csv_strict_c_first_final = 0;
static const int csv_strict_c_error = -1;

static const int csv_strict_c_en_main = 0;
static const int csv_strict_c_en_quoted = 1;
static const int csv_strict_c_en_after_quote = 2;
static const int csv_strict_c_en_save_quote = 3;
static const int csv_strict_c_en_unquoted = 4;
static const int csv_strict_c_en_after_field = 5;
static const int csv_strict_c_en_cr0 = 6;
static const int csv_strict_c_en_cr1 = 7;
static const int csv_strict_c_en_after_field_cr = 8;


#line 229 "validatecsv.rl"

static void runLoose(const char* data, size_t len, bool at_eof, ParserContext* ctx) {
    const unsigned char* p = (const unsigned char*)data;
    const unsigned char* pe = p + len;
    const unsigned char* eof = at_eof ? pe : 0;
    const unsigned char* ts = ctx->ts;
    const unsigned char* te = ctx->te;
    int act = ctx->act;
    int cs = ctx->cs;

    
#line 240 "validatecsv.rl"
    
#line 110 "validatecsv_ragel.c"
	{
	if (!ctx->initialized) {
	cs = csv_loose_c_start;
	ts = 0;
	te = 0;
	act = 0;
	ctx->initialized = true;
	}
	}

#line 241 "validatecsv.rl"
    
#line 118 "validatecsv_ragel.c"
	{
	if ( p == pe )
		goto _test_eof;
	switch ( cs )
	{
tr0:
#line 76 "validatecsv.rl"
	{ ctx->state = 6; }
#line 93 "validatecsv.rl"
	{te = p+1;{ {goto st4;} }}
	goto st0;
tr1:
#line 80 "validatecsv.rl"
	{ ctx->state = 10; finalizeRecord(ctx); }
#line 89 "validatecsv.rl"
	{te = p+1;{ {goto st0;} }}
	goto st0;
tr2:
#line 73 "validatecsv.rl"
	{ ctx->state = 3; }
#line 90 "validatecsv.rl"
	{te = p+1;{ {goto st6;} }}
	goto st0;
tr3:
#line 71 "validatecsv.rl"
	{ ctx->state = 1; }
#line 92 "validatecsv.rl"
	{te = p+1;{ {goto st1;} }}
	goto st0;
tr4:
#line 78 "validatecsv.rl"
	{ ctx->state = 8; ++ctx->fields; }
#line 91 "validatecsv.rl"
	{te = p+1;{ {goto st5;} }}
	goto st0;
st0:
#line 1 "NONE"
	{ts = 0;}
	if ( ++p == pe )
		goto _test_eof0;
case 0:
#line 1 "NONE"
	{ts = p;}
#line 147 "validatecsv_ragel.c"
	switch( (*p) ) {
		case 10u: goto tr1;
		case 13u: goto tr2;
		case 34u: goto tr3;
		case 44u: goto tr4;
	}
	goto tr0;
tr5:
#line 77 "validatecsv.rl"
	{ ctx->state = 7; }
#line 98 "validatecsv.rl"
	{te = p+1;{ {goto st1;} }}
	goto st1;
tr6:
#line 72 "validatecsv.rl"
	{ ctx->state = 2; }
#line 97 "validatecsv.rl"
	{te = p+1;{ {goto st2;} }}
	goto st1;
st1:
#line 1 "NONE"
	{ts = 0;}
	if ( ++p == pe )
		goto _test_eof1;
case 1:
#line 1 "NONE"
	{ts = p;}
#line 168 "validatecsv_ragel.c"
	if ( (*p) == 34u )
		goto tr6;
	goto tr5;
tr7:
#line 76 "validatecsv.rl"
	{ ctx->state = 6; }
#line 106 "validatecsv.rl"
	{te = p+1;{ {goto st4;} }}
	goto st2;
tr8:
#line 81 "validatecsv.rl"
	{ ctx->state = 11; ++ctx->fields; finalizeRecord(ctx); }
#line 102 "validatecsv.rl"
	{te = p+1;{ {goto st0;} }}
	goto st2;
tr9:
#line 74 "validatecsv.rl"
	{ ctx->state = 4; }
#line 103 "validatecsv.rl"
	{te = p+1;{ {goto st7;} }}
	goto st2;
tr10:
#line 75 "validatecsv.rl"
	{ ctx->state = 5; }
#line 105 "validatecsv.rl"
	{te = p+1;{ {goto st3;} }}
	goto st2;
tr11:
#line 78 "validatecsv.rl"
	{ ctx->state = 8; ++ctx->fields; }
#line 104 "validatecsv.rl"
	{te = p+1;{ {goto st5;} }}
	goto st2;
st2:
#line 1 "NONE"
	{ts = 0;}
	if ( ++p == pe )
		goto _test_eof2;
case 2:
#line 1 "NONE"
	{ts = p;}
#line 197 "validatecsv_ragel.c"
	switch( (*p) ) {
		case 10u: goto tr8;
		case 13u: goto tr9;
		case 34u: goto tr10;
		case 44u: goto tr11;
	}
	goto tr7;
tr12:
#line 77 "validatecsv.rl"
	{ ctx->state = 7; }
#line 111 "validatecsv.rl"
	{te = p+1;{ {goto st1;} }}
	goto st3;
tr13:
#line 72 "validatecsv.rl"
	{ ctx->state = 2; }
#line 110 "validatecsv.rl"
	{te = p+1;{ {goto st2;} }}
	goto st3;
st3:
#line 1 "NONE"
	{ts = 0;}
	if ( ++p == pe )
		goto _test_eof3;
case 3:
#line 1 "NONE"
	{ts = p;}
#line 218 "validatecsv_ragel.c"
	if ( (*p) == 34u )
		goto tr13;
	goto tr12;
tr14:
#line 76 "validatecsv.rl"
	{ ctx->state = 6; }
#line 119 "validatecsv.rl"
	{te = p+1;{ {goto st4;} }}
	goto st4;
tr15:
#line 81 "validatecsv.rl"
	{ ctx->state = 11; ++ctx->fields; finalizeRecord(ctx); }
#line 115 "validatecsv.rl"
	{te = p+1;{ {goto st0;} }}
	goto st4;
tr16:
#line 74 "validatecsv.rl"
	{ ctx->state = 4; }
#line 116 "validatecsv.rl"
	{te = p+1;{ {goto st7;} }}
	goto st4;
tr17:
#line 76 "validatecsv.rl"
	{ ctx->state = 6; }
#line 118 "validatecsv.rl"
	{te = p+1;{ {goto st4;} }}
	goto st4;
tr18:
#line 78 "validatecsv.rl"
	{ ctx->state = 8; ++ctx->fields; }
#line 117 "validatecsv.rl"
	{te = p+1;{ {goto st5;} }}
	goto st4;
st4:
#line 1 "NONE"
	{ts = 0;}
	if ( ++p == pe )
		goto _test_eof4;
case 4:
#line 1 "NONE"
	{ts = p;}
#line 247 "validatecsv_ragel.c"
	switch( (*p) ) {
		case 10u: goto tr15;
		case 13u: goto tr16;
		case 34u: goto tr17;
		case 44u: goto tr18;
	}
	goto tr14;
tr19:
#line 76 "validatecsv.rl"
	{ ctx->state = 6; }
#line 127 "validatecsv.rl"
	{te = p+1;{ {goto st4;} }}
	goto st5;
tr20:
#line 81 "validatecsv.rl"
	{ ctx->state = 11; ++ctx->fields; finalizeRecord(ctx); }
#line 123 "validatecsv.rl"
	{te = p+1;{ {goto st0;} }}
	goto st5;
tr21:
#line 79 "validatecsv.rl"
	{ ctx->state = 9; ++ctx->fields; }
#line 124 "validatecsv.rl"
	{te = p+1;{ {goto st8;} }}
	goto st5;
tr22:
#line 71 "validatecsv.rl"
	{ ctx->state = 1; }
#line 126 "validatecsv.rl"
	{te = p+1;{ {goto st1;} }}
	goto st5;
tr23:
#line 78 "validatecsv.rl"
	{ ctx->state = 8; ++ctx->fields; }
#line 125 "validatecsv.rl"
	{te = p+1;{ {goto st5;} }}
	goto st5;
st5:
#line 1 "NONE"
	{ts = 0;}
	if ( ++p == pe )
		goto _test_eof5;
case 5:
#line 1 "NONE"
	{ts = p;}
#line 280 "validatecsv_ragel.c"
	switch( (*p) ) {
		case 10u: goto tr20;
		case 13u: goto tr21;
		case 34u: goto tr22;
		case 44u: goto tr23;
	}
	goto tr19;
tr24:
#line 82 "validatecsv.rl"
	{
    ctx->state = 12;
    errorUnexpectedChar(ctx, *p);
    {p++; cs = 6; goto _out;}
}
#line 132 "validatecsv.rl"
	{te = p+1;{ /* stay in error */ }}
	goto st6;
tr25:
#line 80 "validatecsv.rl"
	{ ctx->state = 10; finalizeRecord(ctx); }
#line 131 "validatecsv.rl"
	{te = p+1;{ {goto st0;} }}
#line 82 "validatecsv.rl"
	{
    ctx->state = 12;
    errorUnexpectedChar(ctx, *p);
    {p++; cs = 6; goto _out;}
}
	goto st6;
st6:
#line 1 "NONE"
	{ts = 0;}
	if ( ++p == pe )
		goto _test_eof6;
case 6:
#line 1 "NONE"
	{ts = p;}
#line 310 "validatecsv_ragel.c"
	if ( (*p) == 10u )
		goto tr25;
	goto tr24;
tr26:
#line 82 "validatecsv.rl"
	{
    ctx->state = 12;
    errorUnexpectedChar(ctx, *p);
    {p++; cs = 7; goto _out;}
}
#line 137 "validatecsv.rl"
	{te = p+1;{ /* stay in error */ }}
	goto st7;
tr27:
#line 81 "validatecsv.rl"
	{ ctx->state = 11; ++ctx->fields; finalizeRecord(ctx); }
#line 136 "validatecsv.rl"
	{te = p+1;{ {goto st0;} }}
#line 82 "validatecsv.rl"
	{
    ctx->state = 12;
    errorUnexpectedChar(ctx, *p);
    {p++; cs = 7; goto _out;}
}
	goto st7;
st7:
#line 1 "NONE"
	{ts = 0;}
	if ( ++p == pe )
		goto _test_eof7;
case 7:
#line 1 "NONE"
	{ts = p;}
#line 336 "validatecsv_ragel.c"
	if ( (*p) == 10u )
		goto tr27;
	goto tr26;
tr28:
#line 82 "validatecsv.rl"
	{
    ctx->state = 12;
    errorUnexpectedChar(ctx, *p);
    {p++; cs = 8; goto _out;}
}
#line 142 "validatecsv.rl"
	{te = p+1;{ /* stay in error */ }}
	goto st8;
tr29:
#line 80 "validatecsv.rl"
	{ ctx->state = 10; finalizeRecord(ctx); }
#line 141 "validatecsv.rl"
	{te = p+1;{ {goto st0;} }}
#line 82 "validatecsv.rl"
	{
    ctx->state = 12;
    errorUnexpectedChar(ctx, *p);
    {p++; cs = 8; goto _out;}
}
	goto st8;
st8:
#line 1 "NONE"
	{ts = 0;}
	if ( ++p == pe )
		goto _test_eof8;
case 8:
#line 1 "NONE"
	{ts = p;}
#line 362 "validatecsv_ragel.c"
	if ( (*p) == 10u )
		goto tr29;
	goto tr28;
	}
	_test_eof0: cs = 0; goto _test_eof; 
	_test_eof1: cs = 1; goto _test_eof; 
	_test_eof2: cs = 2; goto _test_eof; 
	_test_eof3: cs = 3; goto _test_eof; 
	_test_eof4: cs = 4; goto _test_eof; 
	_test_eof5: cs = 5; goto _test_eof; 
	_test_eof6: cs = 6; goto _test_eof; 
	_test_eof7: cs = 7; goto _test_eof; 
	_test_eof8: cs = 8; goto _test_eof; 

	_test_eof: {}
	_out: {}
	}

#line 242 "validatecsv.rl"

    ctx->ts = ts;
    ctx->te = te;
    ctx->act = act;
    ctx->cs = cs;

    if (at_eof) {
        if (!ctx->result->isValid) return;
        switch (ctx->state) {
            case 6:
            case 7:
            case 8:
            case 9:
                ++ctx->fields;
                finalizeRecord(ctx);
                break;
            case 0:
            case 10:
            case 11:
                break;
            default:
                unexpectedEof(ctx);
                break;
        }
    }
}

static void runStrict(const char* data, size_t len, bool at_eof, ParserContext* ctx) {
    const unsigned char* p = (const unsigned char*)data;
    const unsigned char* pe = p + len;
    const unsigned char* eof = at_eof ? pe : 0;
    const unsigned char* ts = ctx->ts;
    const unsigned char* te = ctx->te;
    int act = ctx->act;
    int cs = ctx->cs;

    
#line 274 "validatecsv.rl"
    
#line 410 "validatecsv_ragel.c"
	{
	if (!ctx->initialized) {
	cs = csv_strict_c_start;
	ts = 0;
	te = 0;
	act = 0;
	ctx->initialized = true;
	}
	}

#line 275 "validatecsv.rl"
    
#line 416 "validatecsv_ragel.c"
	{
	if ( p == pe )
		goto _test_eof;
	switch ( cs )
	{
tr0:
#line 158 "validatecsv.rl"
	{ ctx->state = 6; }
#line 175 "validatecsv.rl"
	{te = p+1;{ {goto st4;} }}
	goto st0;
tr1:
#line 162 "validatecsv.rl"
	{ ctx->state = 10; finalizeRecord(ctx); }
#line 171 "validatecsv.rl"
	{te = p+1;{ {goto st0;} }}
	goto st0;
tr2:
#line 155 "validatecsv.rl"
	{ ctx->state = 3; }
#line 172 "validatecsv.rl"
	{te = p+1;{ {goto st6;} }}
	goto st0;
tr3:
#line 153 "validatecsv.rl"
	{ ctx->state = 1; }
#line 174 "validatecsv.rl"
	{te = p+1;{ {goto st1;} }}
	goto st0;
tr4:
#line 160 "validatecsv.rl"
	{ ctx->state = 8; ++ctx->fields; }
#line 173 "validatecsv.rl"
	{te = p+1;{ {goto st5;} }}
	goto st0;
st0:
#line 1 "NONE"
	{ts = 0;}
	if ( ++p == pe )
		goto _test_eof0;
case 0:
#line 1 "NONE"
	{ts = p;}
#line 447 "validatecsv_ragel.c"
	switch( (*p) ) {
		case 10u: goto tr1;
		case 13u: goto tr2;
		case 34u: goto tr3;
		case 44u: goto tr4;
	}
	goto tr0;
tr5:
#line 159 "validatecsv.rl"
	{ ctx->state = 7; }
#line 180 "validatecsv.rl"
	{te = p+1;{ {goto st1;} }}
	goto st1;
tr6:
#line 154 "validatecsv.rl"
	{ ctx->state = 2; }
#line 179 "validatecsv.rl"
	{te = p+1;{ {goto st2;} }}
	goto st1;
st1:
#line 1 "NONE"
	{ts = 0;}
	if ( ++p == pe )
		goto _test_eof1;
case 1:
#line 1 "NONE"
	{ts = p;}
#line 468 "validatecsv_ragel.c"
	if ( (*p) == 34u )
		goto tr6;
	goto tr5;
tr7:
#line 164 "validatecsv.rl"
	{
    ctx->state = 12;
    errorUnexpectedChar(ctx, *p);
    {p++; cs = 2; goto _out;}
}
#line 188 "validatecsv.rl"
	{te = p+1;{ /* stay in error */ }}
	goto st2;
tr8:
#line 163 "validatecsv.rl"
	{ ctx->state = 11; ++ctx->fields; finalizeRecord(ctx); }
#line 184 "validatecsv.rl"
	{te = p+1;{ {goto st0;} }}
	goto st2;
tr9:
#line 156 "validatecsv.rl"
	{ ctx->state = 4; }
#line 185 "validatecsv.rl"
	{te = p+1;{ {goto st7;} }}
	goto st2;
tr10:
#line 157 "validatecsv.rl"
	{ ctx->state = 5; }
#line 187 "validatecsv.rl"
	{te = p+1;{ {goto st3;} }}
	goto st2;
tr11:
#line 160 "validatecsv.rl"
	{ ctx->state = 8; ++ctx->fields; }
#line 186 "validatecsv.rl"
	{te = p+1;{ {goto st5;} }}
	goto st2;
st2:
#line 1 "NONE"
	{ts = 0;}
	if ( ++p == pe )
		goto _test_eof2;
case 2:
#line 1 "NONE"
	{ts = p;}
#line 501 "validatecsv_ragel.c"
	switch( (*p) ) {
		case 10u: goto tr8;
		case 13u: goto tr9;
		case 34u: goto tr10;
		case 44u: goto tr11;
	}
	goto tr7;
tr12:
#line 159 "validatecsv.rl"
	{ ctx->state = 7; }
#line 193 "validatecsv.rl"
	{te = p+1;{ {goto st1;} }}
	goto st3;
tr13:
#line 154 "validatecsv.rl"
	{ ctx->state = 2; }
#line 192 "validatecsv.rl"
	{te = p+1;{ {goto st2;} }}
	goto st3;
st3:
#line 1 "NONE"
	{ts = 0;}
	if ( ++p == pe )
		goto _test_eof3;
case 3:
#line 1 "NONE"
	{ts = p;}
#line 522 "validatecsv_ragel.c"
	if ( (*p) == 34u )
		goto tr13;
	goto tr12;
tr14:
#line 158 "validatecsv.rl"
	{ ctx->state = 6; }
#line 201 "validatecsv.rl"
	{te = p+1;{ {goto st4;} }}
	goto st4;
tr15:
#line 163 "validatecsv.rl"
	{ ctx->state = 11; ++ctx->fields; finalizeRecord(ctx); }
#line 197 "validatecsv.rl"
	{te = p+1;{ {goto st0;} }}
	goto st4;
tr16:
#line 156 "validatecsv.rl"
	{ ctx->state = 4; }
#line 198 "validatecsv.rl"
	{te = p+1;{ {goto st7;} }}
	goto st4;
tr17:
#line 164 "validatecsv.rl"
	{
    ctx->state = 12;
    errorUnexpectedChar(ctx, *p);
    {p++; cs = 4; goto _out;}
}
#line 200 "validatecsv.rl"
	{te = p+1;{ /* stay in error */ }}
	goto st4;
tr18:
#line 160 "validatecsv.rl"
	{ ctx->state = 8; ++ctx->fields; }
#line 199 "validatecsv.rl"
	{te = p+1;{ {goto st5;} }}
	goto st4;
st4:
#line 1 "NONE"
	{ts = 0;}
	if ( ++p == pe )
		goto _test_eof4;
case 4:
#line 1 "NONE"
	{ts = p;}
#line 555 "validatecsv_ragel.c"
	switch( (*p) ) {
		case 10u: goto tr15;
		case 13u: goto tr16;
		case 34u: goto tr17;
		case 44u: goto tr18;
	}
	goto tr14;
tr19:
#line 158 "validatecsv.rl"
	{ ctx->state = 6; }
#line 209 "validatecsv.rl"
	{te = p+1;{ {goto st4;} }}
	goto st5;
tr20:
#line 163 "validatecsv.rl"
	{ ctx->state = 11; ++ctx->fields; finalizeRecord(ctx); }
#line 205 "validatecsv.rl"
	{te = p+1;{ {goto st0;} }}
	goto st5;
tr21:
#line 161 "validatecsv.rl"
	{ ctx->state = 9; ++ctx->fields; }
#line 206 "validatecsv.rl"
	{te = p+1;{ {goto st8;} }}
	goto st5;
tr22:
#line 153 "validatecsv.rl"
	{ ctx->state = 1; }
#line 208 "validatecsv.rl"
	{te = p+1;{ {goto st1;} }}
	goto st5;
tr23:
#line 160 "validatecsv.rl"
	{ ctx->state = 8; ++ctx->fields; }
#line 207 "validatecsv.rl"
	{te = p+1;{ {goto st5;} }}
	goto st5;
st5:
#line 1 "NONE"
	{ts = 0;}
	if ( ++p == pe )
		goto _test_eof5;
case 5:
#line 1 "NONE"
	{ts = p;}
#line 588 "validatecsv_ragel.c"
	switch( (*p) ) {
		case 10u: goto tr20;
		case 13u: goto tr21;
		case 34u: goto tr22;
		case 44u: goto tr23;
	}
	goto tr19;
tr24:
#line 164 "validatecsv.rl"
	{
    ctx->state = 12;
    errorUnexpectedChar(ctx, *p);
    {p++; cs = 6; goto _out;}
}
#line 214 "validatecsv.rl"
	{te = p+1;{ /* stay in error */ }}
	goto st6;
tr25:
#line 162 "validatecsv.rl"
	{ ctx->state = 10; finalizeRecord(ctx); }
#line 213 "validatecsv.rl"
	{te = p+1;{ {goto st0;} }}
#line 164 "validatecsv.rl"
	{
    ctx->state = 12;
    errorUnexpectedChar(ctx, *p);
    {p++; cs = 6; goto _out;}
}
	goto st6;
st6:
#line 1 "NONE"
	{ts = 0;}
	if ( ++p == pe )
		goto _test_eof6;
case 6:
#line 1 "NONE"
	{ts = p;}
#line 618 "validatecsv_ragel.c"
	if ( (*p) == 10u )
		goto tr25;
	goto tr24;
tr26:
#line 164 "validatecsv.rl"
	{
    ctx->state = 12;
    errorUnexpectedChar(ctx, *p);
    {p++; cs = 7; goto _out;}
}
#line 219 "validatecsv.rl"
	{te = p+1;{ /* stay in error */ }}
	goto st7;
tr27:
#line 163 "validatecsv.rl"
	{ ctx->state = 11; ++ctx->fields; finalizeRecord(ctx); }
#line 218 "validatecsv.rl"
	{te = p+1;{ {goto st0;} }}
#line 164 "validatecsv.rl"
	{
    ctx->state = 12;
    errorUnexpectedChar(ctx, *p);
    {p++; cs = 7; goto _out;}
}
	goto st7;
st7:
#line 1 "NONE"
	{ts = 0;}
	if ( ++p == pe )
		goto _test_eof7;
case 7:
#line 1 "NONE"
	{ts = p;}
#line 644 "validatecsv_ragel.c"
	if ( (*p) == 10u )
		goto tr27;
	goto tr26;
tr28:
#line 164 "validatecsv.rl"
	{
    ctx->state = 12;
    errorUnexpectedChar(ctx, *p);
    {p++; cs = 8; goto _out;}
}
#line 224 "validatecsv.rl"
	{te = p+1;{ /* stay in error */ }}
	goto st8;
tr29:
#line 162 "validatecsv.rl"
	{ ctx->state = 10; finalizeRecord(ctx); }
#line 223 "validatecsv.rl"
	{te = p+1;{ {goto st0;} }}
#line 164 "validatecsv.rl"
	{
    ctx->state = 12;
    errorUnexpectedChar(ctx, *p);
    {p++; cs = 8; goto _out;}
}
	goto st8;
st8:
#line 1 "NONE"
	{ts = 0;}
	if ( ++p == pe )
		goto _test_eof8;
case 8:
#line 1 "NONE"
	{ts = p;}
#line 670 "validatecsv_ragel.c"
	if ( (*p) == 10u )
		goto tr29;
	goto tr28;
	}
	_test_eof0: cs = 0; goto _test_eof; 
	_test_eof1: cs = 1; goto _test_eof; 
	_test_eof2: cs = 2; goto _test_eof; 
	_test_eof3: cs = 3; goto _test_eof; 
	_test_eof4: cs = 4; goto _test_eof; 
	_test_eof5: cs = 5; goto _test_eof; 
	_test_eof6: cs = 6; goto _test_eof; 
	_test_eof7: cs = 7; goto _test_eof; 
	_test_eof8: cs = 8; goto _test_eof; 

	_test_eof: {}
	_out: {}
	}

#line 276 "validatecsv.rl"

    ctx->ts = ts;
    ctx->te = te;
    ctx->act = act;
    ctx->cs = cs;

    if (at_eof) {
        if (!ctx->result->isValid) return;
        switch (ctx->state) {
            case 6:
            case 7:
            case 8:
            case 9:
                ++ctx->fields;
                finalizeRecord(ctx);
                break;
            case 0:
            case 10:
            case 11:
                break;
            default:
                unexpectedEof(ctx);
                break;
        }
    }
}

static FileResult ValidateFile(const char* filename, bool strict) {
    FileResult result;
    result.filename = filename;
    result.isValid = true;
    result.expectedFields = 0;
    result.lineNumber = 1;

    FILE* f = fopen(filename, "rb");
    if (!f) {
        result.isValid = false;
        if (gVerbose) {
            printf("Unable to open file: %s\n", filename);
        }
        return result;
    }

    ParserContext ctx;
    ctx.result = &result;
    ctx.haveExpected = false;
    ctx.fields = 0;
    ctx.state = 0;
    ctx.cs = 0;
    ctx.ts = 0;
    ctx.te = 0;
    ctx.act = 0;
    ctx.initialized = false;

    char chunk[1024];
    bool saw_data = false;
    bool fed_eof = false;

    while (1) {
        size_t n = fread(chunk, 1, sizeof(chunk), f);
        if (n == 0) {
            break;
        }
        saw_data = true;
        bool final_chunk = (n < sizeof(chunk));
        if (strict) {
            runStrict(chunk, n, final_chunk, &ctx);
        } else {
            runLoose(chunk, n, final_chunk, &ctx);
        }
        if (final_chunk) {
            fed_eof = true;
        }
        if (!result.isValid || final_chunk) {
            break;
        }
    }
    fclose(f);

    if (!saw_data) {
        return result;
    }

    // If end-of-file was never delivered to the parser (exact chunk multiple),
    // flush an empty chunk with at_eof=true.
    if (result.isValid && !fed_eof) {
        if (strict) {
            runStrict(chunk, 0, true, &ctx);
        } else {
            runLoose(chunk, 0, true, &ctx);
        }
        fed_eof = true;
    }

    if (gVerbose) {
        printf("%s: File is %sly %s with %d columns and %d rows.\n",
               filename,
               strict ? "strict" : "loose",
               result.isValid ? "valid" : "invalid",
               result.expectedFields,
               result.lineNumber);
    }
    return result;
}

int main(int argc, char* argv[]) {
    bool strict = false;

    if (argc < 2) {
        printf("Usage: validatecsv [-s|--strict] [-v|--verbose] <filename1> [filename2] ...\n");
        return 1;
    }

    int argi = 1;
    const char* files[64];
    int fileCount = 0;

    for (; argi < argc && fileCount < 64; ++argi) {
        if (strcmp(argv[argi], "-s") == 0 || strcmp(argv[argi], "--strict") == 0) {
            strict = true;
        } else if (strcmp(argv[argi], "-v") == 0 || strcmp(argv[argi], "--verbose") == 0) {
            gVerbose = 1;
        } else if (argv[argi][0] != '-') {
            files[fileCount++] = argv[argi];
        }
    }

    if (fileCount == 0) {
        printf("Usage: validatecsv [-s|--strict] [-v|--verbose] <filename1> [filename2] ...\n");
        return 1;
    }

    bool allValid = true;
    for (int i = 0; i < fileCount; ++i) {
        FileResult r = ValidateFile(files[i], strict);
        if (!r.isValid) {
            allValid = false;
        }
    }

    return allValid ? 0 : 1;
}
