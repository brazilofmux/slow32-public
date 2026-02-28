// phonetic.c - NATO phonetic alphabet converter for SLOW-32
// Usage: phonetic <text>    (from command line argument)
//        phonetic            (reads lines from stdin)

#include <stdio.h>
#include <ctype.h>
#include <string.h>

static const char *phonetic_table[128] = {
    // 0x00-0x1F: control characters (NULL entries)
    0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0, 0,0,0,0,0,0,0,0,
    // 0x20-0x2F
    "(Space)",          // 0x20 space
    "Exclamation Mark", // 0x21 !
    "Double Quote",     // 0x22 "
    "Hash",             // 0x23 #
    "Dollar",           // 0x24 $
    "Percent",          // 0x25 %
    "Ampersand",        // 0x26 &
    "Single Quote",     // 0x27 '
    "Left Parenthesis", // 0x28 (
    "Right Parenthesis",// 0x29 )
    "Asterisk",         // 0x2A *
    "Plus",             // 0x2B +
    "Comma",            // 0x2C ,
    "Dash",             // 0x2D -
    "Dot",              // 0x2E .
    "Forward Slash",    // 0x2F /
    // 0x30-0x39: digits
    "Zero", "One", "Two", "Three", "Four",
    "Five", "Six", "Seven", "Eight", "Nine",
    // 0x3A-0x40
    "Colon",            // 0x3A :
    "Semicolon",        // 0x3B ;
    "Less Than",        // 0x3C <
    "Equals",           // 0x3D =
    "Greater Than",     // 0x3E >
    "Question Mark",    // 0x3F ?
    "At",               // 0x40 @
    // 0x41-0x5A: uppercase letters (A-Z)
    "Alpha", "Bravo", "Charlie", "Delta", "Echo",
    "Foxtrot", "Golf", "Hotel", "India", "Juliet",
    "Kilo", "Lima", "Mike", "November", "Oscar",
    "Papa", "Quebec", "Romeo", "Sierra", "Tango",
    "Uniform", "Victor", "Whiskey", "X-ray", "Yankee", "Zulu",
    // 0x5B-0x60
    "Left Bracket",     // 0x5B [
    "Backslash",        // 0x5C backslash
    "Right Bracket",    // 0x5D ]
    "Caret",            // 0x5E ^
    "Underscore",       // 0x5F _
    "Backtick",         // 0x60 `
    // 0x61-0x7A: lowercase letters (a-z) — mapped via toupper, but fill anyway
    "Alpha", "Bravo", "Charlie", "Delta", "Echo",
    "Foxtrot", "Golf", "Hotel", "India", "Juliet",
    "Kilo", "Lima", "Mike", "November", "Oscar",
    "Papa", "Quebec", "Romeo", "Sierra", "Tango",
    "Uniform", "Victor", "Whiskey", "X-ray", "Yankee", "Zulu",
    // 0x7B-0x7F
    "Left Curly Brace", // 0x7B {
    "Pipe",             // 0x7C |
    "Right Curly Brace",// 0x7D }
    "Tilde",            // 0x7E ~
    0                   // 0x7F DEL
};

static void convert(const char *s) {
    int first = 1;
    while (*s) {
        unsigned char c = (unsigned char)*s++;
        if (c == '\n' || c == '\r')
            continue;
        if (!first)
            putchar(' ');
        first = 0;
        if (c < 128 && phonetic_table[c]) {
            printf("%s", phonetic_table[c]);
        } else {
            putchar(c);
        }
    }
    putchar('\n');
}

int main(int argc, char *argv[]) {
    if (argc > 1) {
        // Concatenate all arguments with spaces (like the original)
        int first_arg = 1;
        int first_phonetic = 1;
        for (int i = 1; i < argc; i++) {
            if (!first_arg) {
                // Space between arguments
                if (!first_phonetic)
                    putchar(' ');
                first_phonetic = 0;
                printf("(Space)");
            }
            first_arg = 0;
            const char *s = argv[i];
            while (*s) {
                unsigned char c = (unsigned char)*s++;
                if (!first_phonetic)
                    putchar(' ');
                first_phonetic = 0;
                if (c < 128 && phonetic_table[c]) {
                    printf("%s", phonetic_table[c]);
                } else {
                    putchar(c);
                }
            }
        }
        putchar('\n');
    } else {
        char line[4096];
        while (fgets(line, sizeof(line), stdin)) {
            convert(line);
        }
    }
    return 0;
}
