/* lex.rl -- the COBOL token scanner, Ragel -G2 (docs/plans/standard-queue.md
 * item 15, ruled 2026-10-06): one grammar for 8.3's lexical elements that
 * both the text-word scanner of copy.h (COPY and REPLACE work on
 * text-words, 7.2) and the token scanner of tokenizer.h read through.
 * Until then the two scanned by hand, each its own copy of the rules.
 *
 * The machine knows one line of source at a time -- the reference format,
 * continuation and comment lines are reader.h's -- and returns one lexeme
 * from a given position: the finest division (a word, a number with its
 * sign and exponent, a literal with its prefix, each punctuation).  What
 * is context -- a PICTURE after PIC, EXEC SQL text, the boundary that
 * lets a sign or a leading point begin a number, DECIMAL-POINT IS
 * COMMA's swap -- stays with the callers, as the ruling has it.  The
 * callers differ in how they assemble: the text-word scanner runs
 * adjacent lexemes together between separators, the tokenizer keeps them
 * apart.
 *
 * Build: ./gen_lex.sh   (ragel -G2 -o lex_scan.c lex.rl); the output is
 * checked in, so the build needs no ragel. */
#include <string.h>
#include "lex.h"

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wimplicit-fallthrough"
#pragma GCC diagnostic ignored "-Wunused-const-variable"
#endif

%%{
    machine lexscan;
    # Bytes, not chars. Without this Ragel takes plain char, which it treats
    # as signed, and bakes the UTF-8 ranges in as negative constants
    # (0xC2..0xDF becomes -62..-33). That only works where char is signed --
    # x86-64 and Apple arm64 -- and on Linux aarch64, where char is unsigned,
    # no byte above 0x7F ever matched: every extended letter fell through to
    # LX_OTHER and the tokenizer died on "unexpected character".
    alphtype unsigned char;
    write data;
}%%

/* is the text at q a separator's tail: the line's end, a space, a tab, or
 * a closing pseudo-text delimiter (==... PIC 9(5).==) */
static int lx_sep_tail(const unsigned char *q, const unsigned char *pe)
{
    return q >= pe || *q == ' ' || *q == '\t' || (q + 1 < pe && q[0] == '=' && q[1] == '=');
}

/* the lexeme at p (p < pe): kind, text and length; the literal's prefix
 * length; whether a number carries an exponent.  Every byte is some
 * lexeme (LX_OTHER when nothing else), so the callers always advance. */
int lx_next(const char *p0, const char *pe0, Lexeme *out)
{
    /* the interface is char, as its callers' buffers are; the machine reads
     * bytes (alphtype unsigned char above) */
    const unsigned char *p = (const unsigned char *)p0;
    const unsigned char *pe = (const unsigned char *)pe0, *eof = pe;
    const unsigned char *ts, *te;
    int cs, act;
    memset(out, 0, sizeof *out);
    out->s = p0; out->len = 1; out->kind = LX_OTHER;

    %%{
        quote1 = '"' ( [^"] | '""' )* '"';
        quote2 = "'" ( [^'] | "''" )* "'";
        unterm = ( '"' [^"]* ) | ( "'" [^']* );
        prefix = ( [nN] [xX]? | [bB] [xX]? | [gG] [xX]? | [xX] | [zZ] | [uU] );
        # an extended letter (2023 8.1.3, Annex B): any UTF-8 sequence of two to
        # four bytes; the tokenizer checks the code point against Annex B
        ext    = ( 0xC2..0xDF 0x80..0xBF ) | ( 0xE0..0xEF 0x80..0xBF 0x80..0xBF ) | ( 0xF0..0xF4 0x80..0xBF 0x80..0xBF 0x80..0xBF );
        letter = alpha | ext;
        wordch = alnum | ext | '-' | '_';
        fixed  = ( digit+ ( '.' digit+ )? ) | ( '.' digit+ );
        expo   = [eE] [+\-]? digit+;
        # 0100-MAIN, 9000-END: digits followed by a word character are a word
        word   = ( letter wordch* ) | ( digit+ ( letter | '-' | '_' ) wordch* );

        action lx_space   { out->kind = LX_SPACE;   out->len = (int)(te - ts); fbreak; }
        action lx_comment { out->kind = LX_COMMENT; out->len = (int)(te - ts); fbreak; }
        action lx_pdelim  { out->kind = LX_PDELIM;  out->len = 2; fbreak; }
        action lx_lp      { out->kind = LX_LP;    out->len = 1; fbreak; }
        action lx_rp      { out->kind = LX_RP;    out->len = 1; fbreak; }
        action lx_colon   { out->kind = LX_COLON; out->len = 1; fbreak; }
        action lx_period {
            /* a period separates before a space, the line's end or ==; a
             * doubled one is one separator (RM's reader let "12370121.."
             * through); otherwise it is a period inside something */
            if (lx_sep_tail(te, pe)) { out->kind = LX_PERIOD; out->len = (int)(te - ts); }
            else if (te - ts == 2 && lx_sep_tail(ts + 1, pe)) { out->kind = LX_PERIOD; out->len = 1; }
            else { out->kind = LX_DOT; out->len = 1; }
            fbreak;
        }
        action lx_sep {
            /* a comma or semicolon separates before a space; otherwise it
             * is tight to what follows (the tokenizer says what that means) */
            out->kind = lx_sep_tail(te, pe) ? LX_SEP : LX_COMMA; out->len = 1; fbreak;
        }
        action lx_lit {
            out->kind = LX_LIT; out->len = (int)(te - ts);
            const unsigned char *q = ts; while (*q != '"' && *q != '\'') q++;
            out->prefix = (int)(q - ts);
            fbreak;
        }
        action lx_unterm {
            out->kind = LX_LIT; out->len = (int)(te - ts); out->bad = 1;
            const unsigned char *q = ts; while (*q != '"' && *q != '\'') q++;
            out->prefix = (int)(q - ts);
            fbreak;
        }
        action lx_num {
            out->kind = LX_NUM; out->len = (int)(te - ts);
            for (const unsigned char *q = ts; q < te; q++) if (*q == 'e' || *q == 'E') { out->exp = 1; break; }
            fbreak;
        }
        action lx_word    { out->kind = LX_WORD; out->len = (int)(te - ts); fbreak; }
        action lx_op      { out->kind = LX_OP;   out->len = (int)(te - ts); fbreak; }
        action lx_other   { out->kind = LX_OTHER; out->len = 1; fbreak; }

        main := |*
            ( ' ' | '\t' )+                  => lx_space;
            '*>' any*                        => lx_comment;
            '=='                             => lx_pdelim;
            '('                              => lx_lp;
            ')'                              => lx_rp;
            ':'                              => lx_colon;
            '.' '.'?                         => lx_period;
            ( ',' | ';' )                    => lx_sep;
            prefix? ( quote1 | quote2 )      => lx_lit;
            prefix? unterm                   => lx_unterm;
            # a floating-point literal's significand has a point (8.3.3.3.3 rule 2): 3E5 is a word
            ( [+\-]? fixed ) | ( [+\-]? ( digit+ '.' digit+ | '.' digit+ ) expo ) => lx_num;
            word                             => lx_word;
            ( '**' | '>=' | '<=' | '<>' | [=<>+\-*/&] ) => lx_op;
            any                              => lx_other;
        *|;
    }%%

    %% write init;
    %% write exec;

    (void)act; (void)eof; (void)cs;
    return out->kind;
}
