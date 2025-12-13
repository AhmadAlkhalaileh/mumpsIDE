# MUMPS (M) Language Syntax Reference

**Standard:** ANSI X11.1-1995 / ISO/IEC 11756:1999
**Implementation:** YottaDB/GT.M Compatible
**Last Updated:** December 2024

---

## Line Structure

### Maximum Line Length
- **Standard Limit:** 255 characters (including label and comments)
- **Recommendation:** Keep lines under 200 characters for readability
- **Error:** Lines exceeding 255 characters violate MUMPS standard

### Line Format
```
[label] [linestart] [level] [command [arguments]] [;comment]
```

**Components:**
- `label` - Optional line label (1-16 characters)
- `linestart` - Space or tab (required after label)
- `level` - Optional period(s) for structured code
- `command` - MUMPS command keyword
- `arguments` - Command arguments (space-separated)
- `comment` - Optional comment (starts with semicolon)

---

## Naming Rules

### Labels
- **Length:** 1-16 characters
- **First Character:** Must be A-Z or %
- **Subsequent Characters:** A-Z, 0-9
- **Case:** Case-sensitive (though uppercase is convention)
- **Position:** Must start at column 1 (no leading spaces)

**Valid Examples:**
```mumps
MAIN        ; Valid label
TEST123     ; Valid label
%UTIL       ; Valid label (% prefix allowed)
```

**Invalid Examples:**
```mumps
 MAIN       ; ERROR: Label has leading space
test        ; WARNING: Lowercase (should be uppercase)
VERYLONGLABELNAME  ; ERROR: Exceeds 16 characters
123TEST     ; ERROR: Starts with digit
```

### Variables

**Local Variables:**
- **Length:** 1-16 characters
- **First Character:** A-Z or %
- **Subsequent Characters:** A-Z, 0-9
- **Scope:** Current routine/stack level

**Global Variables:**
- **Prefix:** Must start with `^` (caret)
- **Name:** Follows same rules as local variables
- **Scope:** Persistent across routines and sessions

**Valid Examples:**
```mumps
SET NAME="John"          ; Local variable
SET AGE=30               ; Local variable
SET ^PATIENT(ID)="Data"  ; Global variable
SET %TEMP=123            ; System variable (% prefix)
```

**Invalid Examples:**
```mumps
SET 1VAR=10              ; ERROR: Starts with digit
SET MY-VAR=20            ; ERROR: Contains hyphen
SET VERYLONGVARIABLENAME=30  ; ERROR: Exceeds 16 characters
```

### Routine Names
- **Length:** 1-16 characters
- **First Character:** A-Z (uppercase)
- **Subsequent Characters:** A-Z, 0-9
- **Case:** Must be uppercase
- **File Extension:** `.m`

---

## Commands

### Standard Commands (ANSI X11.1)

| Command | Abbrev | Description | Example |
|---------|--------|-------------|---------|
| BREAK | B | Enter debug mode | `BREAK` |
| CLOSE | C | Close I/O device | `CLOSE device` |
| DO | D | Execute subroutine | `DO LABEL^ROUTINE` |
| ELSE | E | Conditional branch | `ELSE  WRITE "No"` |
| FOR | F | Loop construct | `FOR I=1:1:10` |
| GOTO | G | Jump to label | `GOTO LABEL` |
| HALT | H | Terminate program | `HALT` |
| HANG | H | Pause execution | `HANG 5` |
| IF | I | Conditional | `IF X>5` |
| JOB | J | Start background job | `JOB ROUTINE` |
| KILL | K | Delete variable | `KILL VAR` |
| LOCK | L | Lock global | `LOCK +^LOCK:10` |
| MERGE | M | Copy array | `MERGE A=B` |
| NEW | N | Create new scope | `NEW VAR` |
| OPEN | O | Open I/O device | `OPEN device` |
| QUIT | Q | Exit routine/block | `QUIT value` |
| READ | R | Read input | `READ VAR` |
| SET | S | Assign variable | `SET X=10` |
| USE | U | Select I/O device | `USE device` |
| VIEW | V | System function | `VIEW` |
| WRITE | W | Output data | `WRITE "Hello"` |
| XECUTE | X | Execute string | `XECUTE CODE` |

### Transaction Processing Commands

| Command | Abbrev | Description |
|---------|--------|-------------|
| TCOMMIT | TC | Commit transaction |
| TRESTART | TRE | Restart transaction |
| TROLLBACK | TRO | Rollback transaction |
| TSTART | TS | Start transaction |

### YottaDB/GT.M Extensions (Z-Commands)

| Command | Description |
|---------|-------------|
| ZALLOCATE | Allocate shared resource |
| ZBREAK | Set breakpoint |
| ZCOMPILE | Compile routine |
| ZCONTINUE | Continue after ZBREAK |
| ZDEALLOCATE | Release shared resource |
| ZEDIT | Edit routine |
| ZGOTO | Stack-level jump |
| ZHALT | Halt with status |
| ZHELP | Display help |
| ZKILL | Delete node (keep descendants) |
| ZLINK | Link/compile routine |
| ZMESSAGE | Signal error |
| ZPRINT | Print routine source |
| ZRUPDATE | Update routine cache |
| ZSHOW | Display information |
| ZSTEP | Step debugger |
| ZSYSTEM | Execute shell command |
| ZTCOMMIT | Z-transaction commit |
| ZTRIGGER | Manage triggers |
| ZTSTART | Z-transaction start |
| ZWITHDRAW | Withdraw from transaction |
| ZWRITE | Write variable tree |

---

## Operators

### Arithmetic Operators
| Operator | Description | Example |
|----------|-------------|---------|
| `+` | Addition | `5+3` → `8` |
| `-` | Subtraction | `5-3` → `2` |
| `*` | Multiplication | `5*3` → `15` |
| `/` | Division | `15/3` → `5` |
| `\` | Integer division | `7\2` → `3` |
| `#` | Modulo | `7#3` → `1` |
| `**` | Exponentiation | `2**3` → `8` |

### String Operators
| Operator | Description | Example |
|----------|-------------|---------|
| `_` | Concatenation | `"AB"_"CD"` → `"ABCD"` |

### Logical Operators
| Operator | Description | Example |
|----------|-------------|---------|
| `&` | AND | `1&1` → `1` |
| `!` | OR | `0!1` → `1` |
| `'` | NOT | `'1` → `0` |

### Relational Operators
| Operator | Description | Example |
|----------|-------------|---------|
| `=` | Equal | `5=5` → `1` |
| `'=` | Not equal | `5'=3` → `1` |
| `>` | Greater than | `5>3` → `1` |
| `<` | Less than | `3<5` → `1` |
| `'>` | Not greater | `3'>5` → `1` |
| `'<` | Not less | `5'<3` → `1` |

### Pattern Match Operators
| Operator | Description | Example |
|----------|-------------|---------|
| `?` | Pattern match | `"ABC"?3A` → `1` |
| `[` | Contains | `"ABCD"["BC"` → `1` |
| `]` | Follows | `"ABC"]"AB"` → `1` |
| `]]` | Sorts after | `"B"]]"A"` → `1` |

### Operator Precedence
**All binary operators have equal precedence and evaluate left-to-right**

```mumps
; These are evaluated left-to-right:
SET X=5+3*2        ; (5+3)*2 = 16 (not 11!)
SET Y=10/2-1       ; (10/2)-1 = 4

; Use parentheses for clarity:
SET X=5+(3*2)      ; 11
SET Y=10/(2-1)     ; 10
```

---

## Intrinsic Functions

All intrinsic functions start with `$` and are case-insensitive.

### String Functions
| Function | Description | Example |
|----------|-------------|---------|
| `$ASCII(str,pos)` | ASCII code of character | `$ASCII("A")` → `65` |
| `$CHAR(code,...)` | Character from ASCII | `$CHAR(65)` → `"A"` |
| `$EXTRACT(str,from,to)` | Substring | `$EXTRACT("HELLO",2,4)` → `"ELL"` |
| `$FIND(str,substr,pos)` | Find position | `$FIND("HELLO","LL")` → `4` |
| `$JUSTIFY(str,width,dec)` | Right-justify | `$JUSTIFY(123,5)` → `"  123"` |
| `$LENGTH(str,delim)` | String/piece length | `$LENGTH("A,B,C",",")` → `3` |
| `$PIECE(str,delim,from,to)` | Extract pieces | `$PIECE("A,B,C",",",2)` → `"B"` |
| `$REVERSE(str)` | Reverse string | `$REVERSE("ABC")` → `"CBA"` |
| `$TRANSLATE(str,from,to)` | Translate characters | `$TRANSLATE("ABC","AB","12")` → `"12C"` |

### Numeric Functions
| Function | Description |
|----------|-------------|
| `$FNUMBER(num,format)` | Format number |
| `$RANDOM(limit)` | Random integer 0 to limit-1 |

### Database Functions
| Function | Description |
|----------|-------------|
| `$DATA(var)` | Variable status (0/1/10/11) |
| `$GET(var,default)` | Get value or default |
| `$INCREMENT(var,delta)` | Atomic increment |
| `$NAME(var,sub)` | Construct variable name |
| `$ORDER(var,dir)` | Next/previous subscript |
| `$QUERY(var,dir)` | Next node in tree |

### Utility Functions
| Function | Description |
|----------|-------------|
| `$SELECT(cond:val,...)` | Conditional selection |
| `$STACK(level,info)` | Call stack information |
| `$TEXT(label+offset^routine)` | Routine source line |
| `$VIEW(...)` | System state |

### YottaDB/GT.M Extensions
- `$ZBIT*` functions - Bitwise operations
- `$ZDATE()` - Date formatting
- `$ZJUSTIFY()` - Extended justify
- `$ZPARSE()` - File path parsing
- `$ZPIECE()` - Extended piece
- Plus many more Z-functions

---

## String Literals

### Syntax
- Enclosed in double quotes: `"string"`
- Embedded quotes: Use double-double-quotes: `"He said ""Hello"""`
- Empty string: `""`

### Examples
```mumps
SET MSG="Hello World"                     ; Simple string
SET QUOTE="She said ""Hi"""               ; Contains quotes
SET EMPTY=""                              ; Empty string
SET MULTI="Line1"_$CHAR(13,10)_"Line2"   ; Multi-line (CR+LF)
```

---

## Comments

### Syntax
- Start with semicolon `;`
- Extend to end of line
- Can appear after code or on separate line

### Examples
```mumps
; This is a full-line comment
SET X=10  ; This is an inline comment
```

---

## Indentation and Structure

### Label Lines
- **Must start at column 1** (no leading spaces)
- Can have parameters: `LABEL(param1,param2)`
- Can have comment: `LABEL ; comment`

### Executable Lines
- **Must be indented** (at least one space)
- Use dots (`.`) for nested levels

### Examples
```mumps
MAIN    ; Entry point - label at column 1
 SET X=10           ; Indented executable line
 IF X>5 DO          ; Conditional
 . WRITE "Yes",!    ; Nested with dot
 . SET Y=20         ; Nested with dot
 QUIT
```

---

## Postconditionals

Commands can have conditions using colon syntax:

```mumps
WRITE:X>5 "Greater"     ; Write only if X>5
SET:FLAG X=10           ; Set only if FLAG is true
QUIT:DONE               ; Quit only if DONE is true
```

---

## Indirection

Use `@` for runtime evaluation:

```mumps
SET VAR="NAME"
SET @VAR="John"         ; Sets NAME="John"

SET CMD="WRITE ""Hello"""
XECUTE @CMD             ; Executes the WRITE command
```

---

## Common Validation Rules

### Line-Level Rules
1. **M013:** Line must not exceed 255 characters
2. **M021:** Labels must start at column 1 (no leading spaces)
3. **M022:** Executable lines must be indented

### Label Rules
4. **M028:** Label names must be 1-16 alphanumeric characters starting with letter or %

### Command Rules
5. **M026:** Unknown or invalid command
6. **M027:** Ambiguous command abbreviation
7. **M003:** Commands should be uppercase (warning)

### Syntax Rules
8. **M002:** Unclosed string literal (odd number of quotes)
9. **M007:** Mismatched parentheses
10. **M023:** Trailing comma at end of command
11. **M024:** Empty argument between commas

### Best Practices
12. **M001:** Variable used without NEW (info)
13. **M009:** GOTO considered harmful (use DO instead)
14. **M016:** LOCK without timeout may cause deadlock

---

## References

- **ANSI Standard:** X11.1-1995 (MUMPS Programming Language)
- **ISO Standard:** ISO/IEC 11756:1999
- **YottaDB Docs:** https://docs.yottadb.com/ProgrammersGuide/
- **Learn M:** https://learnxinyminutes.com/m/

---

**Generated for Ahmad IDE**
**Last Updated:** December 2024
