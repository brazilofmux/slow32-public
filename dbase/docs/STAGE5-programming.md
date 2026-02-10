# Stage 5: Programming Language

## Goal

Execute `.PRG` program files with control flow, procedures, parameters,
and user-defined functions. This turns dBase from an interactive tool
into a programmable application platform.

## Prerequisites

- Stage 4 complete (terminal UI)

## Program Execution

| Command | Syntax | Description |
|---------|--------|-------------|
| DO | `DO <program>` | Execute .PRG file |
| SET PROCEDURE TO | `SET PROCEDURE TO <file>` | Load procedure library |
| CANCEL | `CANCEL` | Abort program, return to dot prompt |
| RETURN | `RETURN [<expr>]` | Return from procedure/program |
| SUSPEND | `SUSPEND` | Pause program, enter dot prompt |
| RESUME | `RESUME` | Continue suspended program |

## Control Flow

```
IF <condition>
   ...
[ELSE]
   ...
ENDIF

DO WHILE <condition>
   ...
   [LOOP]        && restart iteration
   [EXIT]        && break out
ENDDO

DO CASE
   CASE <condition1>
      ...
   CASE <condition2>
      ...
   [OTHERWISE]
      ...
ENDCASE

FOR <var> = <start> TO <end> [STEP <inc>]
   ...
   [LOOP]
   [EXIT]
NEXT
```

## Procedures & Functions

```
* In a .PRG file:
PROCEDURE procname
   PARAMETERS p1, p2, p3
   ...
   RETURN

FUNCTION funcname
   PARAMETERS p1, p2
   ...
   RETURN <expr>
```

- `DO procname` or `DO procname WITH arg1, arg2`
- Functions callable in expressions: `? funcname(1, 2)`
- Parameters passed by reference (can modify caller's variables)
- PRIVATE/PUBLIC variable scoping

## Program File Handling

```c
// Program loaded as array of lines
typedef struct {
    char **lines;       // Source lines
    int line_count;
    int current_line;   // Execution pointer
    char filename[64];
} program_t;

// Call stack for DO nesting
#define MAX_CALL_DEPTH 32
typedef struct {
    program_t *program;
    int return_line;
    memvar_t *saved_privates;
} call_frame_t;
```

## Preprocessor

Before execution, handle:
- `&` macro substitution: `USE &mFile` expands variable
- `&&` comments (rest of line)
- `*` comments (full line)
- Line continuation with `;`

## Error Handling

- `ON ERROR DO <procedure>` — custom error handler
- `ERROR()` — last error number
- `MESSAGE()` — last error message
- `LINENO()` — current line number
- `PROGRAM()` — current program name

## Source Files

```
src/
  program.c       # PRG file loader + executor
  program.h
  control.c       # IF/WHILE/CASE/FOR flow control
  control.h
  procedure.c     # PROCEDURE/FUNCTION + call stack
  procedure.h
```

## Success Criteria

- `DO myprog` executes a .PRG file
- IF/ELSE/ENDIF, DO WHILE/ENDDO, DO CASE/ENDCASE all work
- Nested DO calls with proper variable scoping
- PARAMETERS passes values to procedures
- User-defined FUNCTION callable in expressions
- `&variable` macro substitution works
- ON ERROR handler catches runtime errors
