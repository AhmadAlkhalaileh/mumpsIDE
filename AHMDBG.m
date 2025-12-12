AHMDBG  ; Ahmad JSON Debugger - stdin/stdout only
        ; This routine is designed to be injected into any YottaDB/GT.M env
        ; and used via: mumps -run AHMDBGJSON^AHMDBG <routine> [<tag>]
        ;
        ; Protocol (stdin -> M, stdout <- M):
        ;   Events sent by M (one JSON per line):
        ;     {"event":"started","routine":"AAAA","tag":""}
        ;     {"event":"stopped","routine":"AAAA","line":3,"pos":"^AAAA","+...","depth":1,"tag":"AAAA","offset":0}
        ;     {"event":"bp-set","routine":"AAAA","tag":"AAAA","offset":0}
        ;     {"event":"bp-cleared","routine":"AAAA","line":3}
        ;     {"event":"bp-cleared-routine","routine":"AAAA"}
        ;     {"event":"bp-cleared-all"}
        ;     {"event":"error","message":"<ZSTATUS>"}
        ;     {"event":"exit"}
        ;
        ;   Commands from debugger (one per line on stdin):
        ;     INTO
        ;     OVER
        ;     OUTOF
        ;     CONTINUE
        ;     GETVARS
        ;     HALT | EXIT | QUIT
        ;     SETBP;<routine>;<tag>;<offset>
        ;     SETBPJSON;{"routine":"TMPDBG","tag":"TAG","offset":N}
        ;     CLEARBP;<routine>;<line>
        ;     CLEARBP;<routine>;
        ;     CLEARBP;;
        ;
        QUIT  ; do not fall through
        ;
        ; -------------------------------------------------------------------
        ; JSON-based entry point (no TCP)
        ; -------------------------------------------------------------------
        ;
AHMDBGJSON(entryRoutine,entryTag) ;
        NEW ROUT,TAG,CMDLINE
        ; basic error handler for this mode
        SET $ETRAP="DO ERRJSON^AHMDBG"
        ; resolve routine & tag from params or $ZCMDLINE
        SET ROUT=$GET(entryRoutine)
        SET TAG=$GET(entryTag)
        IF ROUT="" DO
        . SET CMDLINE=$ZCMDLINE
        . SET ROUT=$PIECE(CMDLINE," ",1)
        . SET TAG=$PIECE(CMDLINE," ",2)
        ; fallback routine name if nothing passed
        IF ROUT="" SET ROUT="TMPDBG"
        ; store for reference (optional)
        SET ^%AHMDBG($J,"JSON","ROUT")=ROUT
        SET ^%AHMDBG($J,"JSON","TAG")=TAG
        ; configure $ZSTEP to call STEPJSON and then use ZST/ZC
        ; %STP will be one of: "I","O","F","C","H"
        ;  I -> ZSTEP INTO   (step into)
        ;  O -> ZSTEP OVER   (step over)
        ;  F -> ZSTEP OUTOF  (step out)
        ;  C -> ZCONTINUE    (run / continue)
        ;  H -> HALT         (stop program)
        ; Capture variables before calling STEPJSON (while in user scope)
        SET $ZSTEP="ZSHOW ""V"":^%AHMDBG($J,""VARS"") SET %STP=$$STEPJSON^AHMDBG() ZSTEP:%STP=""I"" INTO ZSTEP:%STP=""O"" OVER ZSTEP:%STP=""F"" OUTOF ZCONTINUE:%STP=""C""  HALT:%STP=""H"""
        ; notify debugger that we are ready (waiting for first command)
        USE $PRINCIPAL
        WRITE "{""event"":""ready"",""routine"":""",ROUT,""",""tag"":""",TAG,"""}",!
        ; Wait for debugger command before starting execution
        NEW STARTCMD
        SET STARTCMD=$$READCMDJSON()
        ; If user sent HALT before starting, exit
        IF STARTCMD="H" QUIT
        ; IMPORTANT: First command must ALWAYS use INTO to enter the routine
        ; OVER/OUTOF only work when already executing inside a routine
        ; So we ignore the command type and always use INTO for startup
        ; Validate entry tag exists; if missing, report a friendly error and fall back to routine entry
        IF TAG'="",($TEXT(@(TAG_"^"_ROUT))="") DO
        . USE $PRINCIPAL
        . WRITE "{""event"":""error"",""message"":""Entry label not found: ",TAG,"^",ROUT,". Falling back to routine entry.""}",!
        . SET TAG=""
        ; Build call: use extrinsic $$ for tags (may return value), DO for routine entry
        NEW CALL
        IF TAG'="" DO
        . SET CALL=TAG_"^"_ROUT_"()"
        . ZSTEP INTO SET ^%AHMDBG($J,"JSON","RET")=$$@CALL
        ELSE  DO
        . SET CALL="^"_ROUT
        . ZSTEP INTO DO @CALL
        QUIT
        QUIT
        ;
        ; -------------------------------------------------------------------
        ; $ZSTEP handler: send stopped event & wait for next command
        ; Returns "I","O","F","C","H" to drive ZST/ZC in $ZSTEP string
        ; -------------------------------------------------------------------
        ;
STEPJSON()
        NEW USR,POS,ROU,TAG,OFF,LIN
        ; user frame is one below this routine
        SET USR=$SELECT($STACK>0:$STACK-1,1:$STACK)
        SET POS=$STACK(USR,"PLACE")
        SET ROU=$PIECE(POS,"^",2)
        ; Skip if we're stepping through AHMDBG itself (prevents stepping into debugger code)
        IF ROU="AHMDBG" QUIT "I"
        SET TAG=$PIECE($PIECE(POS,"^"),"+",1)
        SET OFF=+$PIECE($PIECE(POS,"^"),"+",2)
        ; if TAG not found from $STACK, fallback to $ZPOSITION
        IF TAG="" DO
        . NEW ZP SET ZP=$ZPOSITION
        . SET TAG=$PIECE($PIECE(ZP,"^"),"+",1)
        . SET OFF=+$PIECE($PIECE(ZP,"^"),"+",2)
        ; compute 1-based source line number
        SET LIN=$$LINENUM(TAG,OFF,ROU)
        ; send stopped event
        USE $PRINCIPAL
        WRITE "{""event"":""stopped"",""routine"":""",ROU,""",""line"":",LIN,","
        WRITE """pos"":""",POS,""",""depth"":",USR,","
        WRITE """tag"":""",TAG,""",""offset"":",OFF,"}",!
        ; wait for stepping / breakpoint command, return mode letter
        QUIT $$READCMDJSON()
        ;
        ; -------------------------------------------------------------------
        ; Command reader for JSON mode
        ;   Returns:
        ;     "I" - INTO
        ;     "O" - OVER
        ;     "F" - OUTOF
        ;     "C" - CONTINUE
        ;     "H" - HALT / EXIT / error / EOF
        ; -------------------------------------------------------------------
        ;
READCMDJSON() ;
        NEW CMD
READLP   ; loop until we get a stepping command
        READ CMD:999
        ELSE  DO  QUIT "H"
        . USE $PRINCIPAL
        . WRITE "{""event"":""error"",""message"":""READ timeout or stdin closed""}",!
        ; trim trailing CR / LF if any
        FOR  QUIT:CMD=""  QUIT:$EXTRACT(CMD,$LENGTH(CMD))'?1C  DO
        . IF $EXTRACT(CMD,$LENGTH(CMD))=$CHAR(10)!($EXTRACT(CMD,$LENGTH(CMD))=$CHAR(13)) DO
        . . SET CMD=$EXTRACT(CMD,1,$LENGTH(CMD)-1)
        . ELSE  QUIT
        ; EOF or empty line -> treat as HALT
        IF CMD="" QUIT "H"
        ; JSON-style breakpoint command
        IF $EXTRACT(CMD,1,9)="SETBPJSON" DO SETBPJSON(CMD) GOTO READLP
        ; legacy semicolon-style breakpoint command
        IF $EXTRACT(CMD,1,6)="SETBP;" DO SETBPCMD(CMD) GOTO READLP
        ; breakpoint clear commands
        IF $EXTRACT(CMD,1,8)="CLEARBP;" DO CLEARBPJSON(CMD) GOTO READLP
        ; eval command
        IF $EXTRACT(CMD,1,5)="EVAL;" DO EVJSON(CMD) GOTO READLP
        ; variable query command
        IF CMD="GETVARS" DO SENDVARS GOTO READLP
        ; stepping commands
        IF CMD="INTO"     QUIT "I"  ; ZSTEP INTO
        IF CMD="OVER"     QUIT "O"  ; ZSTEP OVER
        IF CMD="OUTOF"    QUIT "F"  ; ZSTEP OUTOF
        IF CMD="CONTINUE" QUIT "C"  ; ZCONTINUE
        ; stop / exit commands
        IF (CMD="HALT")!(CMD="EXIT")!(CMD="QUIT") QUIT "H"
        ; unknown command -> ignore and continue waiting
        GOTO READLP
        ;
        ; -------------------------------------------------------------------
        ; Breakpoint handling (JSON + legacy protocol)
        ;
        ; 1) Legacy:
        ;    SETBPCMD;
        ;      CMD = "SETBP;<routine>;<tag>;<offset>"
        ;
        ; 2) JSON:
        ;    SETBPJSON;
        ;      CMD = "SETBPJSON;{""routine"":""TMPDBG"",""tag"":""TAG"",""offset"":N}"
        ;
        ;    Both forms end up doing: ZBREAK TAG+offset^ROUTINE
        ; -------------------------------------------------------------------
        ;
SETBPCMD(CMD) ;
        NEW R,TAG,OFF,BP,$ETRAP
        SET $ETRAP="GOTO BPERR^AHMDBG"
        SET R=$PIECE(CMD,";",2)
        SET TAG=$PIECE(CMD,";",3)
        SET OFF=$PIECE(CMD,";",4)
        QUIT:R=""
        ; if tag is empty, use routine name as label
        IF TAG="" SET TAG=R
        SET OFF=+$GET(OFF)
        ; TAG+offset^ROUTINE
        SET BP=TAG_"+"_OFF_"^"_R
        ; Try to set breakpoint - may fail if location invalid
        ZBREAK @BP:"N"
        USE $PRINCIPAL
        WRITE "{""event"":""bp-set"",""routine"":""",R,""",""tag"":""",TAG,""",""offset"":",OFF,"}",!
        QUIT
BPERR   ; Error handler for breakpoint setting
        USE $PRINCIPAL
        WRITE "{""event"":""bp-error"",""message"":""Could not set breakpoint at ",TAG,"+",$GET(OFF),"^",R,"""}",!
        SET $ECODE=""
        QUIT
SETBPJSON(CMD) ;
        NEW JSON,R,TAG,OFF,BP
        ; everything after first ';' is the JSON payload
        SET JSON=$PIECE(CMD,";",2,999)
        IF JSON="" QUIT
        ; extract "routine","tag","offset" from the JSON
        SET R=$$JSONSTR(JSON,"routine")
        SET TAG=$$JSONSTR(JSON,"tag")
        SET OFF=$$JSONNUM(JSON,"offset")
        ; routine is mandatory
        IF R="" QUIT
        ; fallback: if tag empty, use routine as label
        IF TAG="" SET TAG=R
        SET OFF=+$GET(OFF)
        ; TAG+offset^ROUTINE
        SET BP=TAG_"+"_OFF_"^"_R
        ; Use action "N" (no-op) to prevent drop to direct mode
        ZBREAK @BP:"N"
        USE $PRINCIPAL
        WRITE "{""event"":""bp-set"",""routine"":""",R,""",""tag"":""",TAG,""",""offset"":",OFF,"}",!
        QUIT
CLEARBPJSON(CMD) ;
        NEW R,L,BP
        SET R=$PIECE(CMD,";",2)
        SET L=$PIECE(CMD,";",3)
        ; CLEARBP;;  -> clear all breakpoints
        IF R="" DO  QUIT
        . ZBREAK -*
        . USE $PRINCIPAL
        . WRITE "{""event"":""bp-cleared-all""}",!
        ; CLEARBP;<routine>;  -> clear all in that routine
        IF L="" DO  QUIT
        . NEW BPS,P,SPEC
        . ZSHOW "B":BPS
        . SET P=""
        . FOR  SET P=$ORDER(BPS("B",P)) QUIT:P=""  DO
        . . SET SPEC=$PIECE(BPS("B",P),">",1)
        . . IF SPEC[("^"_R) ZBREAK -@SPEC
        . USE $PRINCIPAL
        . WRITE "{""event"":""bp-cleared-routine"",""routine"":""",R,"""}",!
        ; CLEARBP;<routine>;<line>
        SET L=+$GET(L)
        SET BP="-+"_L_"^"_R
        ZBREAK @BP
        USE $PRINCIPAL
        WRITE "{""event"":""bp-cleared"",""routine"":""",R,""",""line"":",L,"}",!
        QUIT
        ;
        ; -------------------------------------------------------------------
        ; SENDVARS: Send all local variables as JSON
        ; Uses ZSHOW "V" to capture variable state
        ; -------------------------------------------------------------------
        ;
SENDVARS ;
        NEW I,LINE,VAR,VAL,FIRST,SKIP
        ; Use variables captured in $ZSTEP action (stored in global)
        USE $PRINCIPAL
        WRITE "{""event"":""vars"",""vars"":{"
        SET I="",FIRST=1
        FOR  SET I=$ORDER(^%AHMDBG($J,"VARS","V",I)) QUIT:I=""  DO
        . SET LINE=^%AHMDBG($J,"VARS","V",I)
        . ; Parse ZSHOW output: VAR=value
        . SET VAR=$PIECE(LINE,"=",1)
        . ; Skip system variables and debugger internal variables
        . SET SKIP=0
        . IF $EXTRACT(VAR,1,1)="%" SET SKIP=1
        . IF VAR="SKIP" SET SKIP=1
        . IF VAR="I" SET SKIP=1
        . IF VAR="LINE" SET SKIP=1
        . IF VAR="VAR" SET SKIP=1
        . IF VAR="VAL" SET SKIP=1
        . IF VAR="FIRST" SET SKIP=1
        . IF VAR="CMD" SET SKIP=1
        . IF VAR="CMDLINE" SET SKIP=1
        . IF VAR="USR" SET SKIP=1
        . IF VAR="POS" SET SKIP=1
        . IF VAR="ROU" SET SKIP=1
        . IF VAR="ROUT" SET SKIP=1
        . IF VAR="TAG" SET SKIP=1
        . IF VAR="OFF" SET SKIP=1
        . IF VAR="LIN" SET SKIP=1
        . IF VAR="STARTCMD" SET SKIP=1
        . QUIT:SKIP
        . ; Extract value (everything after first =)
        . SET VAL=$PIECE(LINE,"=",2,999)
        . ; Remove quotes if present
        . IF $EXTRACT(VAL,1,1)="""" SET VAL=$EXTRACT(VAL,2,$LENGTH(VAL)-1)
        . ; Output JSON key-value pair
        . IF 'FIRST WRITE ","
        . WRITE """",VAR,""":""",VAL,""""
        . SET FIRST=0
        WRITE "}}",!
        QUIT
        ;
        ; -------------------------------------------------------------------
        ; EVJSON: Evaluate arbitrary M code while paused, capture output safely
        ; Input format: EVAL;<m-code>
        ; Sends: {"event":"eval","ok":1,"output":"...","locals":{...}}
        ; -------------------------------------------------------------------
EVJSON(CMD) ;
        NEW CODE,FN,OLDIO,LINE,OUT,I,JSON,ERR,LOCALS
        SET CODE=$PIECE(CMD,";",2,999)
        IF CODE="" DO  QUIT
        . USE $PRINCIPAL
        . WRITE "{""event"":""eval"",""ok"":0,""error"":""Empty command""}",!
        SET FN="/tmp/ahmad_dbg/eval_"_$J_".tmp"
        SET OLDIO=$IO,ERR=""
        SET $ETRAP="GOTO EVAERR^AHMDBG"
        ; capture output to file
        OPEN FN:NEWVERSION
        USE FN
        XECUTE CODE
        USE OLDIO
        CLOSE FN
        ; read captured output
        OPEN FN:READONLY
        USE FN
        SET I=0
        FOR  USE FN READ LINE QUIT:$ZEOF  DO
        . SET I=I+1,OUT(I)=LINE
        CLOSE FN
        ; cleanup temp file (best-effort)
        ZSYSTEM "rm -f "_FN
        ; capture locals after eval
        NEW I,LINEVAR,VAR,VAL,FIRST,SKIP
        K ^%AHMDBG($J,"VARS")
        ZSHOW "V":^%AHMDBG($J,"VARS")
        SET LOCALS=""
        SET I="",FIRST=1
        FOR  SET I=$ORDER(^%AHMDBG($J,"VARS","V",I)) QUIT:I=""  DO
        . SET LINEVAR=^%AHMDBG($J,"VARS","V",I)
        . SET VAR=$PIECE(LINEVAR,"=",1)
        . SET SKIP=0
        . IF $EXTRACT(VAR,1,1)="%" SET SKIP=1
        . IF VAR="SKIP" SET SKIP=1
        . IF VAR="I" SET SKIP=1
        . IF VAR="LINE" SET SKIP=1
        . IF VAR="VAR" SET SKIP=1
        . IF VAR="VAL" SET SKIP=1
        . IF VAR="FIRST" SET SKIP=1
        . IF VAR="CMD" SET SKIP=1
        . IF VAR="CMDLINE" SET SKIP=1
        . IF VAR="USR" SET SKIP=1
        . IF VAR="POS" SET SKIP=1
        . IF VAR="ROU" SET SKIP=1
        . IF VAR="ROUT" SET SKIP=1
        . IF VAR="TAG" SET SKIP=1
        . IF VAR="OFF" SET SKIP=1
        . IF VAR="LIN" SET SKIP=1
        . IF VAR="STARTCMD" SET SKIP=1
        . QUIT:SKIP
        . SET VAL=$PIECE(LINEVAR,"=",2,999)
        . IF $EXTRACT(VAL,1,1)="""" SET VAL=$EXTRACT(VAL,2,$LENGTH(VAL)-1)
        . IF 'FIRST SET LOCALS=LOCALS_","
        . SET LOCALS=LOCALS_""""_$$ESC(VAR)_""":"""_$$ESC(VAL)_""""
        . SET FIRST=0
        USE $PRINCIPAL
        SET JSON="{""event"":""eval"",""ok"":1,""output"":"""_$$JOINOUT(.OUT)_""",""locals"":{"_LOCALS_"}}"
        WRITE JSON,!
        QUIT
EVAERR  ; error during eval
        USE OLDIO
        SET ERR=$ZSTATUS
        ZSYSTEM "rm -f "_FN
        USE $PRINCIPAL
        WRITE "{""event"":""eval"",""ok"":0,""error"":"""_$$ESC(ERR)_"""}",!
        SET $ECODE=""
        QUIT
        ;
JOINOUT(ARR) ; join captured output lines with \n and escape JSON
        NEW I,RES
        SET RES=""
        SET I=0
        FOR  SET I=$ORDER(ARR(I)) QUIT:'I  DO
        . IF RES'="" SET RES=RES_"\n"
        . SET RES=RES_$$ESC(ARR(I))
        QUIT RES
        ;
ESC(STR) ; basic JSON string escape for " and \
        NEW OUT,I,C
        SET OUT=""
        SET STR=$GET(STR)
        FOR I=1:1:$LENGTH(STR) DO
        . SET C=$EXTRACT(STR,I)
        . IF C="""" SET OUT=OUT_"\\"_"""" QUIT
        . IF C="\\" SET OUT=OUT_"\\"_"\\" QUIT
        . SET OUT=OUT_C
        QUIT OUT
        ;
        ; -------------------------------------------------------------------
        ; Tiny JSON helpers (very limited, for our own payload only)
        ; -------------------------------------------------------------------
        ;
JSONSTR(JSON,KEY) ;
        NEW SEARCH,P1,P2,RES
        SET SEARCH=""""_KEY_""":"""
        SET P1=$FIND(JSON,SEARCH)
        IF 'P1 QUIT ""
        SET P2=$FIND(JSON,"""",P1)
        IF 'P2 QUIT ""
        SET RES=$EXTRACT(JSON,P1,P2-2)
        QUIT RES
JSONNUM(JSON,KEY) ;
        NEW SEARCH,P1,P2,CH,RES
        SET SEARCH=""""_KEY_""":"
        SET P1=$FIND(JSON,SEARCH)
        IF 'P1 QUIT 0
        SET P2=P1
        FOR  SET CH=$EXTRACT(JSON,P2) QUIT:CH=""!(CH=",")!(CH="}")  SET P2=P2+1
        SET RES=$EXTRACT(JSON,P1,P2-1)
        QUIT +RES
        ;
        ; -------------------------------------------------------------------
        ; Map <tag,offset,routine> to 1-based line number
        ; Your zstep adapter already skips comment-only lines when mapping,
        ; so here we keep it simple and just return tag+offset as-is.
        ; -------------------------------------------------------------------
        ;
LINENUM(TAG,OFF,ROU) ;
        NEW BASE,TXT,FOUND,FIRST,LINE
        SET TAG=$GET(TAG),OFF=+$GET(OFF),ROU=$GET(ROU)
        IF ROU="" QUIT OFF+1
        SET BASE=0,FOUND=0
        FOR  SET TXT=$TEXT(@("+"_BASE_"^"_ROU)) Q:TXT=""  DO  Q:FOUND
        . SET FIRST=$PIECE($PIECE(TXT,";",1)," ",1)
        . IF FIRST="" SET BASE=BASE+1 QUIT
        . IF TAG="" SET TAG=FIRST,FOUND=1 QUIT
        . IF (FIRST=TAG)!($PIECE(FIRST,"(",1)=TAG) SET FOUND=1 QUIT
        . SET BASE=BASE+1
        IF 'FOUND QUIT OFF+1
        SET LINE=BASE+1+OFF
        QUIT LINE
        ;
        ; -------------------------------------------------------------------
        ; Error handler for JSON mode
        ; -------------------------------------------------------------------
        ;
ERRJSON ;
        USE $PRINCIPAL
        WRITE "{""event"":""error"",""message"":""",$ZSTATUS,"""}",!
        QUIT
