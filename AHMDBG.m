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
        SET $ZSTEP="SET %STP=$$STEPJSON^AHMDBG() ZSTEP:%STP=""I"" INTO ZSTEP:%STP=""O"" OVER ZSTEP:%STP=""F"" OUTOF ZCONTINUE:%STP=""C""  HALT:%STP=""H"""
        ; notify debugger that we started
        USE $PRINCIPAL
        WRITE "{""event"":""started"",""routine"":""",ROUT,""",""tag"":""",TAG,"""}",!
        ; begin execution with ZSTEP enabled
        IF TAG'="" ZSTEP INTO DO @(TAG_"^"_ROUT) QUIT
        ELSE  ZSTEP INTO DO @("^"_ROUT) QUIT
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
