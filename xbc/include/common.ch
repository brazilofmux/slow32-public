// common.ch -- the common definitions a Clipper program includes
#define TRUE   .T.
#define FALSE  .F.
#define YES    .T.
#define NO     .F.
#translate ISNIL( <x> )       => ( VALTYPE( <x> ) == "U" )
#translate ISARRAY( <x> )     => ( VALTYPE( <x> ) == "A" )
#translate ISBLOCK( <x> )     => ( VALTYPE( <x> ) == "B" )
#translate ISCHARACTER( <x> ) => ( VALTYPE( <x> ) == "C" )
#translate ISDATE( <x> )      => ( VALTYPE( <x> ) == "D" )
#translate ISLOGICAL( <x> )   => ( VALTYPE( <x> ) == "L" )
#translate ISMEMO( <x> )      => ( VALTYPE( <x> ) == "M" )
#translate ISNUMBER( <x> )    => ( VALTYPE( <x> ) == "N" )
#translate ISOBJECT( <x> )    => ( VALTYPE( <x> ) == "O" )
#xcommand DEFAULT <v1> TO <x1> [, <vn> TO <xn> ] => IF <v1> == NIL ; <v1> := <x1> ; END [; IF <vn> == NIL ; <vn> := <xn> ; END ]
#command UPDATE <v1> IF <exp> TO <v2> => IF <exp> ; <v1> := <v2> ; END
