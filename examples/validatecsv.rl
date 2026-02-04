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

%%{
machine csv_loose_c;
alphtype unsigned char;

action to_f0              { ctx->state = 0; }
action to_f2              { ctx->state = 1; }
action to_q2              { ctx->state = 2; }
action to_cr0             { ctx->state = 3; }
action to_cr1             { ctx->state = 4; }
action to_save_quote      { ctx->state = 5; }
action to_save_byte       { ctx->state = 6; }
action to_save_byte_quote { ctx->state = 7; }
action to_save_field      { ctx->state = 8; ++ctx->fields; }
action to_save_field_cr   { ctx->state = 9; ++ctx->fields; }
action to_save_record     { ctx->state = 10; finalizeRecord(ctx); }
action to_save_field_rec  { ctx->state = 11; ++ctx->fields; finalizeRecord(ctx); }
action to_error {
    ctx->state = 12;
    errorUnexpectedChar(ctx, *p);
    fbreak;
}

main := |*
    '\n'        @to_save_record     { fgoto main; };
    '\r'        @to_cr0             { fgoto cr0; };
    ','         @to_save_field      { fgoto after_field; };
    '"'         @to_f2              { fgoto quoted; };
    [^,\n\r"]   @to_save_byte       { fgoto unquoted; };
*|;

quoted := |*
    '"'         @to_q2              { fgoto after_quote; };
    [^"]        @to_save_byte_quote { fgoto quoted; };
*|;

after_quote := |*
    '\n'        @to_save_field_rec  { fgoto main; };
    '\r'        @to_cr1             { fgoto cr1; };
    ','         @to_save_field      { fgoto after_field; };
    '"'         @to_save_quote      { fgoto save_quote; };
    [^,\n\r"]   @to_save_byte       { fgoto unquoted; };
*|;

save_quote := |*
    '"'         @to_q2              { fgoto after_quote; };
    [^"]        @to_save_byte_quote { fgoto quoted; };
*|;

unquoted := |*
    '\n'        @to_save_field_rec  { fgoto main; };
    '\r'        @to_cr1             { fgoto cr1; };
    ','         @to_save_field      { fgoto after_field; };
    '"'         @to_save_byte       { fgoto unquoted; };
    [^,\n\r]    @to_save_byte       { fgoto unquoted; };
*|;

after_field := |*
    '\n'        @to_save_field_rec  { fgoto main; };
    '\r'        @to_save_field_cr   { fgoto after_field_cr; };
    ','         @to_save_field      { fgoto after_field; };
    '"'         @to_f2              { fgoto quoted; };
    [^,\n\r"]   @to_save_byte       { fgoto unquoted; };
*|;

cr0 := |*
    '\n'        @to_save_record     { fgoto main; };
    any         @to_error           { /* stay in error */ };
*|;

cr1 := |*
    '\n'        @to_save_field_rec  { fgoto main; };
    any         @to_error           { /* stay in error */ };
*|;

after_field_cr := |*
    '\n'        @to_save_record     { fgoto main; };
    any         @to_error           { /* stay in error */ };
*|;
}%%

%% write data;

%%{
machine csv_strict_c;
alphtype unsigned char;

action to_f0              { ctx->state = 0; }
action to_f2              { ctx->state = 1; }
action to_q2              { ctx->state = 2; }
action to_cr0             { ctx->state = 3; }
action to_cr1             { ctx->state = 4; }
action to_save_quote      { ctx->state = 5; }
action to_save_byte       { ctx->state = 6; }
action to_save_byte_quote { ctx->state = 7; }
action to_save_field      { ctx->state = 8; ++ctx->fields; }
action to_save_field_cr   { ctx->state = 9; ++ctx->fields; }
action to_save_record     { ctx->state = 10; finalizeRecord(ctx); }
action to_save_field_rec  { ctx->state = 11; ++ctx->fields; finalizeRecord(ctx); }
action to_error {
    ctx->state = 12;
    errorUnexpectedChar(ctx, *p);
    fbreak;
}

main := |*
    '\n'        @to_save_record     { fgoto main; };
    '\r'        @to_cr0             { fgoto cr0; };
    ','         @to_save_field      { fgoto after_field; };
    '"'         @to_f2              { fgoto quoted; };
    [^,\n\r"]   @to_save_byte       { fgoto unquoted; };
*|;

quoted := |*
    '"'         @to_q2              { fgoto after_quote; };
    [^"]        @to_save_byte_quote { fgoto quoted; };
*|;

after_quote := |*
    '\n'        @to_save_field_rec  { fgoto main; };
    '\r'        @to_cr1             { fgoto cr1; };
    ','         @to_save_field      { fgoto after_field; };
    '"'         @to_save_quote      { fgoto save_quote; };
    [^,\n\r"]   @to_error           { /* stay in error */ };
*|;

save_quote := |*
    '"'         @to_q2              { fgoto after_quote; };
    [^"]        @to_save_byte_quote { fgoto quoted; };
*|;

unquoted := |*
    '\n'        @to_save_field_rec  { fgoto main; };
    '\r'        @to_cr1             { fgoto cr1; };
    ','         @to_save_field      { fgoto after_field; };
    '"'         @to_error           { /* stay in error */ };
    [^,\n\r"]   @to_save_byte       { fgoto unquoted; };
*|;

after_field := |*
    '\n'        @to_save_field_rec  { fgoto main; };
    '\r'        @to_save_field_cr   { fgoto after_field_cr; };
    ','         @to_save_field      { fgoto after_field; };
    '"'         @to_f2              { fgoto quoted; };
    [^,\n\r"]   @to_save_byte       { fgoto unquoted; };
*|;

cr0 := |*
    '\n'        @to_save_record     { fgoto main; };
    any         @to_error           { /* stay in error */ };
*|;

cr1 := |*
    '\n'        @to_save_field_rec  { fgoto main; };
    any         @to_error           { /* stay in error */ };
*|;

after_field_cr := |*
    '\n'        @to_save_record     { fgoto main; };
    any         @to_error           { /* stay in error */ };
*|;
}%%

%% write data;

static void runLoose(const char* data, size_t len, bool at_eof, ParserContext* ctx) {
    const unsigned char* p = (const unsigned char*)data;
    const unsigned char* pe = p + len;
    const unsigned char* eof = at_eof ? pe : 0;
    const unsigned char* ts = ctx->ts;
    const unsigned char* te = ctx->te;
    int act = ctx->act;
    int cs = ctx->cs;

    %% machine csv_loose_c;
    if (!ctx->initialized) {
        %% write init;
        ctx->initialized = true;
    }
    %% write exec;

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

    %% machine csv_strict_c;
    if (!ctx->initialized) {
        %% write init;
        ctx->initialized = true;
    }
    %% write exec;

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

    size_t chunk_size = 65536;
    char* chunk = malloc(chunk_size);
    if (!chunk) {
        result.isValid = false;
        fclose(f);
        return result;
    }
    bool saw_data = false;
    bool fed_eof = false;

    while (1) {
        size_t n = fread(chunk, 1, chunk_size, f);
        if (n == 0) {
            break;
        }
        saw_data = true;
        bool final_chunk = (n < chunk_size);
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
        free(chunk);
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
    free(chunk);

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
