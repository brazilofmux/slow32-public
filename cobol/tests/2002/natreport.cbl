identification division.
program-id. natreport.
*> National report fields (cobol ISSUES-92). A PIC N(n) field is n
*> columns of the line, its text laid out by display width: an East
*> Asian wide character takes two columns, a combining mark rides with
*> the letter before it, and a character that would cross the field's
*> last column is dropped with the rest, spaces standing instead. So the
*> "|" after the field lands in the same column on every line, whatever
*> the text. Also: a national VALUE with no PICTURE (as wide as it
*> shows), USAGE NATIONAL numeric-edited, a numeric USAGE NATIONAL
*> SOURCE, and an alphanumeric SOURCE into a national field. The print
*> file is read back so the layout is in the output. No oracle: the
*> column rule is this implementation's (docs/national.md).
environment division.
input-output section.
file-control.
    select print-file assign to "tmp/natreport.prn"
        organization is line sequential.
    select back-file assign to "tmp/natreport.prn"
        organization is line sequential.
data division.
file section.
fd  print-file report is r.
fd  back-file.
01  back-line pic x(40).
working-storage section.
01  tag   pic x.
01  nm    pic n(6).
01  amt   pic 9(3)v99.
01  cnt   pic 999 usage national.
01  alnm  pic x(6).
01  eof   pic x value "n".
report section.
rd  r page limit 9.
01  type page heading line 1.
    05 column 1 value "t".
    05 column 5 value n"名前".
    05 column 12 value "|".
    05 column 14 value n"金額".
    05 column 21 value "|".
01  d type detail line plus 1.
    05 column 1 pic x source tag.
    05 column 5 pic n(6) source nm.
    05 column 12 pic x value "|".
    05 column 14 pic zz9.99 usage national source amt.
    05 column 21 pic x value "|".
    05 column 23 pic zz9 source cnt.
01  d2 type detail line plus 1.
    05 column 1 pic x source tag.
    05 column 5 pic n(6) source alnm.
    05 column 12 pic x value "|".
procedure division.
    open output print-file
    initiate r
    move "A" to tag move n"東京" to nm move 1.5 to amt move 7 to cnt generate d
    move "B" to tag move n"café" to nm move 12.25 to amt move 42 to cnt generate d
    move "C" to tag move n"日本語テ" to nm move 999.99 to amt move 100 to cnt generate d
    move "D" to tag move n"abcdef" to nm move 0 to amt move 0 to cnt generate d
    move "E" to tag move n"ab東京" to nm generate d
    move "F" to tag move n"abc東京" to nm generate d
    move "G" to tag move "héllo" to alnm generate d2
    terminate r
    close print-file
    open input back-file
    perform until eof = "y"
        read back-file
            at end move "y" to eof
            not at end display back-line
        end-read
    end-perform
    close back-file
    stop run.
