AHMDBG	; Ahmad IDE Enhanced Debugger - YottaDB/GT.M
	; Advanced debugging engine with intelligent features
	; Supports: breakpoints, stepping, variable inspection, syntax validation
	; TCP communication on port 9200
	; Initialize stepping behavior and start command loop
	QUIT  ; Prevent fall-through
	;
MAIN	; Main entry point with stepping enabled
	SET $ZSTEP="ZSHOW ""VIS"":^%AHMDBG($J,""VARS"") SET %STP=$$CMD^AHMDBG($ZPOS) ZST:%STP=""I"" INTO ZST:%STP=""O"" OVER ZST:%STP=""F"" OUTOF ZC:%STP=""C""  HALT:%STP=""H"""
	DO START
	FOR  SET %STP=$$CMD("") QUIT:%STP["^"
	; Handle hot reload
	DO RELOAD(%STP)
	QUIT
	;
RELOAD(PROG)	; Reload routine and restart execution
	SET %PROG=PROG
	KILL (%PROG)
	SET $ZSTATUS=""
	ZGOTO 1:RELOAD1
RELOAD1	; Entry after stack cleanup
	DO SAVEBP
	IF $PIECE(%PROG,"^",2)=$TEXT(+0) GOTO ENDPROG
	ZLINK:%PROG["^" $TRANSLATE($PIECE(%PROG,"^",2),"%","_")
	ZLINK:%PROG'["^" $TRANSLATE(%PROG,"%","_")
	DO LOADBP
	DO @%PROG XECUTE $ZSTEP
	QUIT
	;
START	; Initialize TCP listener
	NEW %IO,%DEV,%PORT,%SOCK
	USE $PRINCIPAL
	SET %IO=$IO,%PORT=9200,%DEV="|TCP|"_%PORT_"|"_$JOB
	OPEN %DEV:(ZLISTEN=%PORT_":TCP":NODELIMITER:ATTACH="listener"):5:"SOCKET"
	ELSE  USE $PRINCIPAL WRITE "Debug port unavailable",! HALT
	USE %DEV
	WRITE /LISTEN(1)
	FOR  WRITE /WAIT QUIT:$KEY]""
	SET %SOCK=$PIECE($KEY,"|",2)
	KILL ^%AHMDBG($JOB)
	SET ^%AHMDBG($JOB,"SOCK")=%SOCK
	SET ^%AHMDBG($JOB,"DEV")=%DEV
	DO SEND("Ready")
	; Auto error handling if none set
	SET:($ZTRAP="B")!($ZTRAP="")&($ETRAP="") $ETRAP="ZSHOW ""VIS"":^%AHMDBG($J,""VARS"") SET %STP=$$CMD^AHMDBG($ZPOS) ZST:%STP=""I"" INTO ZST:%STP=""O"" OVER ZST:%STP=""F"" OUTOF ZC:%STP=""C"""
	SET $ZSTATUS=""
	SET $ECODE=""
	USE %IO
	QUIT
	;
CMD(POS)	; Wait for debugger command
	NEW %DEV,%IO,%CMD,%CMDS,%LINE,%SOCK,%I,%VAR,%MI
	SET %CMDS="START;QUIT;EXIT;INTO;OUTOF;OVER;CONTINUE;SETBP;VARS;INTERNALS;CLEARBP;REQUESTBP;RESET;GETGBL;GETVAR;ERRCHK;RESTART;SEARCHGBL"
	SET %IO=$IO
	SET %DEV=^%AHMDBG($JOB,"DEV"),%SOCK=^%AHMDBG($JOB,"SOCK")
	USE %DEV:(SOCKET=%SOCK:DELIM=$CHAR(10):EXCEPTION="HALT")
	; Auto error handling
	SET:($ZTRAP="B")!($ZTRAP="")&($ETRAP="") $ETRAP="ZSHOW ""VIS"":^%AHMDBG($J,""VARS"") SET %STP=$$CMD^AHMDBG($ZPOS) ZST:%STP=""I"" INTO ZST:%STP=""O"" OVER ZST:%STP=""F"" OUTOF ZC:%STP=""C"""
	; Prevent stepping into debugger itself
	IF POS[("^"_$TEXT(+0))&($ZSTATUS'="")!($GET(^%AHMDBG($JOB,"VARS","S",1))[("^"_$TEXT(+0))) DO
	. SET %I="" FOR  SET %I=$ORDER(^%AHMDBG($JOB,"VARS","I",%I)) QUIT:%I=""  DO
	. . SET %VAR=^%AHMDBG($JOB,"VARS","I",%I) SET:$EXTRACT(%VAR,1,10)="$ZPOSITION" %MI=%I
	. . SET:$EXTRACT(%VAR,1,8)="$ZSTATUS" ^%AHMDBG($JOB,"VARS","I",%MI)="$ZPOSITION="""_$PIECE(%VAR,",",2)_""""
	;
	; Send variables to debugger
	IF $DATA(^%AHMDBG($JOB,"VARS","I")) DO
	. WRITE "***STARTVAR",!
	. SET %I="" FOR  SET %I=$ORDER(^%AHMDBG($JOB,"VARS","I",%I)) QUIT:%I=""  SET %VAR=^%AHMDBG($JOB,"VARS","I",%I) SET:%VAR[$CHAR(10) %VAR=$$REPLACE(%VAR,$CHAR(10),"_$C(10)_") WRITE "I:",%VAR,!
	. SET %I="" FOR  SET %I=$ORDER(^%AHMDBG($JOB,"VARS","V",%I)) QUIT:%I=""  SET %VAR=^%AHMDBG($JOB,"VARS","V",%I) SET:%VAR[$CHAR(10) %VAR=$$REPLACE(%VAR,$CHAR(10),"_$C(10)_") WRITE "V:",%VAR,!
	. SET %I="" FOR  SET %I=$ORDER(^%AHMDBG($JOB,"VARS","S",%I)) QUIT:%I=""  SET %VAR=^%AHMDBG($JOB,"VARS","S",%I) WRITE:%VAR'[("^"_$TEXT(+0)) "S:",%VAR,!
	. WRITE "***ENDVAR",!
CMDLOOP	; Read next command
	USE %DEV:(SOCKET=%SOCK:DELIM=$CHAR(10))
	FOR  READ %LINE SET %CMD=$PIECE(%LINE,";",1) QUIT:$PIECE(%CMD,";",1)'=""&(%CMDS[$TRANSLATE(%CMD,";"))
	IF %CMD="REQUESTBP" WRITE "***STARTBP",! DO LISTBP WRITE "***ENDBP",! GOTO CMDLOOP
	IF %CMD="GETVAR" DO GETVAR($PIECE(%LINE,";",2,999)) GOTO CMDLOOP
	IF %CMD="GETGBL" DO GETGBL($PIECE(%LINE,";",2,999)) GOTO CMDLOOP
	IF %CMD="SEARCHGBL" DO SEARCHGBL($PIECE(%LINE,";",2),$PIECE(%LINE,";",3,999)) GOTO CMDLOOP
	IF %CMD="SETBP" DO SETBP($PIECE(%LINE,";",2),$PIECE(%LINE,";",3),$PIECE(%LINE,";",4)) GOTO CMDLOOP
	IF %CMD="CLEARBP" DO CLEARBP($PIECE(%LINE,";",2),$PIECE(%LINE,";",3)) GOTO CMDLOOP
	IF %CMD="RESET" GOTO RESET
	IF %CMD="ERRCHK" DO LINT GOTO CMDLOOP
	USE %IO
	; Return stepping command, avoid double-stop on breakpoint
	IF %CMD="INTO" QUIT:$DATA(^%AHMDBG($JOB,"BP",$$CONVPOS(POS))) "O" QUIT "I"
	QUIT:%CMD="INTO" "I"
	QUIT:%CMD="OUTOF" "F"
	QUIT:%CMD="OVER" "O"
	QUIT:%CMD="CONTINUE" "C"
	QUIT:%CMD="START"&(%LINE'["/"&(%LINE'["\")) $PIECE(%LINE,";",2)
	QUIT:%CMD="START" "^"_$$RNAME($PIECE(%LINE,";",2))
	DO:%CMD="RESTART" RELOAD("^"_$$RNAME($PIECE(%LINE,";",2)))
	IF %CMD="EXIT"!(%CMD="QUIT") GOTO CLEANUP
	HALT
	;
CLEANUP	; Shutdown debugger
	SET %DEV=$GET(^%AHMDBG($JOB,"DEV"))
	CLOSE:%DEV'="" %DEV
	KILL ^%AHMDBG($JOB)
	HALT
	;
SETBP(FILE,LINE,COND)	; Set breakpoint with optional condition
	NEW RTN,POS,ZCMD,$ZTRAP
	IF FILE'["/"&(FILE'["\") SET POS=$PIECE($PIECE(FILE,"^",1)_"+"_LINE_"^"_$PIECE(FILE,"^",2),"(",1)
	ELSE  SET RTN=$$RNAME(FILE) QUIT:RTN=""  SET:LINE=0 POS="^"_RTN SET:LINE'=0 POS="+"_LINE_"^"_RTN
	QUIT:POS[("^"_$TEXT(+0))
	SET COND=$$REPLACE(COND,$CHAR(34),$CHAR(34,34))
	SET ^%AHMDBG($JOB,"BP",POS)=COND
	SET:COND'="" COND="IF ("_COND_") "
	SET ZCMD="ZBREAK "_POS_":"""_COND_"ZSHOW """"VIS"""":^%AHMDBG($J,""""VARS"""") SET %STP=$$CMD^AHMDBG($ZPOS) ZST:%STP=""""I"""" INTO ZST:%STP=""""O"""" OVER ZST:%STP=""""F"""" OUTOF ZC:%STP=""""C""""  HALT:%STP=""""H"""""""
	SET $ZTRAP="B"
	XECUTE ZCMD
	DO SAVEBP
	QUIT
	;
LOADBP	; Restore breakpoints after relink
	NEW BP,ZCMD,COND
	ZBREAK -*
	SET BP="" FOR  SET BP=$ORDER(^%AHMDBG($JOB,"BP",BP)) QUIT:BP=""  DO
	. SET COND=^%AHMDBG($JOB,"BP",BP)
	. SET:COND'="" COND="IF ("_COND_") "
	. SET ZCMD="ZBREAK "_BP_":"""_COND_"ZSHOW """"VIS"""":^%AHMDBG($J,""""VARS"""") SET %STP=$$CMD^AHMDBG($ZPOS) ZST:%STP=""""I"""" INTO ZST:%STP=""""O"""" OVER ZST:%STP=""""F"""" OUTOF ZC:%STP=""""C""""  HALT:%STP=""""H"""""""
	. XECUTE ZCMD
	DO SAVEBP
	QUIT
	;
CLEARBP(FILE,LINE)	; Remove breakpoint
	NEW RTN,POS,BP
	SET RTN=$$RNAME(FILE)
	IF RTN="" ZBREAK -* DO SAVEBP QUIT
	IF LINE="" DO  QUIT
	. DO SAVEBP
	. SET BP="" FOR  SET BP=$ORDER(^%AHMDBG($JOB,"BP",BP)) QUIT:BP=""  DO
	. . IF BP[("^"_RTN) SET POS="-"_BP ZBREAK @POS
	. DO SAVEBP
	SET POS="-+"_LINE_"^"_RTN
	ZBREAK:$TEXT(@$EXTRACT(POS,2,99))'="" @POS
	DO SAVEBP
	QUIT
	;
GETGBL(EXPR)	; Retrieve global variable values
	NEW $ZTRAP,I,VAR,KCNT,KEY,GNAME,GREF
	SET $ZTRAP="GOTO GBLEND^"_$TEXT(+0)
	WRITE "***STARTGBL",!
	IF EXPR="" SET I="^%" FOR  SET I=$ORDER(@I) QUIT:I=""  DO
	. WRITE $DATA(@I)+10 SET VAR=I_"="_$GET(@I) SET:VAR[$CHAR(10) VAR=$$REPLACE(VAR,$CHAR(10),"_$C(10)_") WRITE VAR,!
	IF EXPR'="" DO
	. SET KEY=""
	. IF EXPR["("&($$LAST(EXPR)'=")") DO  IF 1
	. . SET EXPR=EXPR_")"
	. . SET KCNT=$QLENGTH(EXPR),KEY=$QSUBSCRIPT(EXPR,KCNT)
	. . SET GNAME=$PIECE(EXPR,KEY,1,$LENGTH($EXTRACT(EXPR,1,$LENGTH(EXPR)-1),KEY)-1)
	. . SET:$$LAST(GNAME)="""" GNAME=$EXTRACT(GNAME,1,$LENGTH(GNAME)-1)
	. . SET GNAME=GNAME_"KEY)"
	. ELSE  SET KCNT=$QLENGTH(EXPR) DO
	. . SET:KCNT=0 GNAME=EXPR_"(KEY)"
	. . SET:KCNT>0 GNAME=$EXTRACT(EXPR,1,$LENGTH(EXPR)-1)_",KEY"_")"
	. FOR I=1:1:1000 SET KEY=$ORDER(@GNAME) QUIT:KEY=""  DO
	. . SET GREF=$EXTRACT(GNAME,1,$LENGTH(GNAME)-4)_""""_KEY_""""_")"
	. . WRITE $DATA(@GNAME)+10+(40*(I=1000)) SET VAR=GREF_"="_$GET(@GNAME) SET:VAR[$CHAR(10) VAR=$$REPLACE(VAR,$CHAR(10),"_$C(10)_") WRITE VAR,!
GBLEND	; Error recovery for global access
	WRITE "***ENDGBL",!
	SET $ZSTATUS=""
	QUIT
	;
SEARCHGBL(GBL,SRCH)	; Search global for matching entries
	NEW $ZTRAP,I,VAL,VAR
	WRITE "***STARTGBL",!
	SET $ZTRAP="GOTO GBLEND^"_$TEXT(+0)
	SET SRCH=$ZCONVERT(SRCH,"L")
	SET I=0
	FOR  SET GBL=$QUERY(@GBL) QUIT:GBL=""  DO  QUIT:I=1000
	. SET VAL=@GBL IF $ZCONVERT(GBL,"L")[SRCH!($ZCONVERT(VAL,"L")[SRCH) DO
	. . WRITE $DATA(@GBL)+10+(40*(I=1000))
	. . SET VAR=GBL_"="_VAL
	. . SET:VAR[$CHAR(10) VAR=$$REPLACE(VAR,$CHAR(10),"_$C(10)_")
	. . WRITE VAR,!
	. . SET I=I+1
	GOTO GBLEND
	;
GETVAR(EXPR)	; Evaluate and return variable/expression value
	NEW $ZTRAP
	SET $ZTRAP="GOTO VAREND^"_$TEXT(+0)
	WRITE "***SINGLEVAR",!
	WRITE EXPR,!
	WRITE "***SINGLEVARCONTENT",!
	WRITE @EXPR,!
VAREND	; Error recovery for variable access
	WRITE "***SINGLEEND",!
	SET $ZSTATUS=""
	QUIT
	;
ENDPROG	; Stop debugging and wait for new connection
	NEW %DEV
	SET %DEV=$GET(^%AHMDBG($JOB,"DEV")),%SOCK=$GET(^%AHMDBG($JOB,"SOCK"))
	IF %DEV'="" DO
	. USE %DEV:(SOCKET=%SOCK:DELIM=$CHAR(10))
	. WRITE "***ENDPROGRAM",!
	DO RESET
	QUIT
	;
RESET	; Clear state and reconnect
	SET %DEV=$GET(^%AHMDBG($JOB,"DEV"))
	KILL ^%AHMDBG($JOB,"VARS")
	CLOSE:%DEV'="" %DEV
	ZGOTO 1:AHMDBG
	QUIT

; ---------------------------------------------------------------------------
; JSON-based stepping entry point for Ahmad IDE (used by bridge.js zstep)
; ---------------------------------------------------------------------------
; Usage: mumps -run ^AHMDBGJSON <entryRoutine>
; Communicates over stdin/stdout with JSON lines:
;   {event:"stopped",routine:<>,line:<>,pos:<>,depth:<>,tag:<>,offset:<>}
; Commands from stdin: INTO | OVER | OUTOF | CONTINUE
; Breakpoints: SETBP;<routine>;<line> , CLEARBP;<routine>;<line>
;
AHMDBGJSON(entryRoutine,entryTag) ; entry point
	NEW $ETRAP,$ESTACK,ROUT,TAG,CMDLINE
	SET $ZTRAP="DO ERRJSON^AHMDBG"
	SET $ZSTEP="DO STEPJSON^AHMDBG"
	; Parse command line arguments if not passed as parameters
	SET CMDLINE=$ZCMDLINE
	IF $GET(entryRoutine)="" DO
	. SET ROUT=$PIECE(CMDLINE," ",1)
	. SET TAG=$PIECE(CMDLINE," ",2)
	ELSE  DO
	. SET ROUT=$GET(entryRoutine)
	. SET TAG=$GET(entryTag)
	; Normalize inputs so we never get undefined variable errors
	IF ROUT="" SET ROUT="TMPDBG"
	SET entryRoutine=ROUT,entryTag=TAG
	; Initialize
	SET ^%AHMDBG($JOB,"JSON","ROUTINE")=ROUT
	SET ^%AHMDBG($JOB,"JSON","TAG")=TAG
	; Start execution with stepping enabled
	; Use ZSTEP INTO to enable line-by-line stepping, then call routine
	IF $L(TAG) ZSTEP INTO DO @(TAG_"^"_ROUT) QUIT
	ELSE  ZSTEP INTO DO @("^"_ROUT) QUIT

STEPJSON ; $ZSTEP handler
	NEW POS,ROU,TAG,OFF,LIN,DEP,USR
	; Stack top is this handler; caller lives at $STACK-1
	SET USR=$SELECT($STACK>0:$STACK-1,1:$STACK)
	SET POS=$STACK(USR,"PLACE")
	SET ROU=$PIECE(POS,"^",2)
	SET TAG=$PIECE($PIECE(POS,"^"),"+",1)
	SET OFF=+$PIECE($PIECE(POS,"^"),"+",2)
	; If no tag came from $STACK, try $ZPOSITION
	IF TAG="" DO
	. NEW ZP SET ZP=$ZPOSITION
	. SET TAG=$PIECE($PIECE(ZP,"^"),"+",1)
	. SET OFF=+$PIECE($PIECE(ZP,"^"),"+",2)
	SET LIN=$$LINENUM^AHMDBG(TAG,OFF,ROU)
	; Depth should reflect user stack, not including handler frame
	SET DEP=USR
	WRITE "{""event"":""stopped"",""routine"":""",ROU,""",""line"":",LIN,",""pos"":""",POS,""",""depth"":",DEP,",""tag"":""",TAG,""",""offset"":",OFF,"}",!
	DO READCMDJSON
	QUIT

READCMDJSON ; wait for command from stdin
	NEW CMD
READLP
	; Use READ with error handling - if stdin closes, gracefully exit
	READ CMD:30 ELSE  WRITE "{""event"":""error"",""message"":""READ timeout or stdin closed""}",! HALT
	; Check if we got EOF or empty input
	IF $LENGTH(CMD)=0 WRITE "{""event"":""error"",""message"":""Empty command or EOF""}",! HALT
	IF CMD["SETBP" DO SETBPJSON(CMD) GOTO READLP
	IF CMD["CLEARBP" DO CLEARBPJSON(CMD) GOTO READLP
	IF CMD="INTO" SET $ZSTEP="DO STEPJSON^AHMDBG" QUIT
	IF CMD="OVER" SET $ZSTEP="DO STEPJSON^AHMDBG" QUIT
	IF CMD="OUTOF" SET $ZSTEP="DO STEPJSON^AHMDBG" QUIT
	IF CMD="CONTINUE" SET $ZSTEP="DO STEPJSON^AHMDBG" QUIT
	; default: step into
	SET $ZSTEP="DO STEPJSON^AHMDBG"
	QUIT

SETBPJSON(CMD)
	NEW R,TAG,OFF,BP
	SET R=$PIECE(CMD,";",2)
	SET TAG=$PIECE(CMD,";",3)
	SET OFF=$PIECE(CMD,";",4)
	QUIT:R=""
	; Use TAG+offset format for better compatibility
	IF TAG="" SET TAG=R  ; If no tag provided, use routine name
	SET BP=TAG_"+"_OFF_"^"_R
	ZBREAK @BP:"DO STEPJSON^AHMDBG"
	QUIT

CLEARBPJSON(CMD)
	NEW R,L,BP
	SET R=$PIECE(CMD,";",2),L=$PIECE(CMD,";",3)
	IF R="" ZB -* QUIT
	IF L="" DO
	. NEW BPS,P
	. ZSHOW "B":BPS
	. SET P="" FOR  SET P=$ORDER(BPS("B",P)) Q:P=""  DO
	. . NEW SPEC SET SPEC=$PIECE(BPS("B",P),">",1)
	. . IF SPEC[("^"_R) ZB -@SPEC
	. QUIT
	SET BP="-+"_L_"^"_R
	ZB @BP
	QUIT

LINENUM(TAG,OFF,ROU) ; resolve tag+offset to line number
	NEW IDX,TXT,FOUND,LINE,BASE
	SET TAG=$GET(TAG),OFF=+$GET(OFF),ROU=$GET(ROU)
	IF ROU="" QUIT OFF+1
	; If no tag provided, pick the first non-empty line's tag as entry
	SET BASE=0,FOUND=0
	FOR  SET TXT=$TEXT(@("+"_BASE_"^"_ROU)) Q:TXT=""  DO  Q:FOUND
	. NEW STR SET STR=$PIECE(TXT,";",1)
	. FOR  Q:$EXTRACT(STR,1)'=" "&($EXTRACT(STR,1)'=$CHAR(9))  SET STR=$EXTRACT(STR,2,$LENGTH(STR))
	. IF STR="" SET BASE=BASE+1 QUIT
	. NEW FIRST SET FIRST=$PIECE(STR," ",1)
	. IF TAG="" SET TAG=FIRST,FOUND=1 QUIT
	. IF FIRST=TAG!(($PIECE(FIRST,"(",1))=TAG) SET FOUND=1 QUIT
	. SET BASE=BASE+1
	IF 'FOUND QUIT OFF+1
	SET LINE=BASE+1+OFF
	; Skip comment-only/blank lines after applying offset
	FOR  SET TXT=$TEXT(@("+"_(LINE-1)_"^"_ROU)) Q:TXT=""  DO  Q:$TRANSLATE($PIECE(TXT,";",1)," "_$CHAR(9),"")'=""
	. SET LINE=LINE+1
	QUIT LINE

ERRJSON ; error handler for JSON mode
	WRITE "{""event"":""error"",""message"":""",$ZSTATUS,"""}",!
	QUIT
	;
SAVEBP	; Synchronize breakpoint list
	NEW BP,BPS,BPNEW
	ZSHOW "B":BPS
	SET BP="" FOR  SET BP=$ORDER(BPS("B",BP)) QUIT:BP=""  DO
	. SET BPS("B",BP)=$PIECE(BPS("B",BP),">",1)
	. SET BPNEW(BPS("B",BP))=$GET(^%AHMDBG($JOB,"BP",BPS("B",BP)))
	KILL ^%AHMDBG($JOB,"BP")
	MERGE:$DATA(BPNEW) ^%AHMDBG($JOB,"BP")=BPNEW
	QUIT
	;
LISTBP	; Display active breakpoints
	NEW BPS,BP
	ZSHOW "B":BPS
	SET BP="" FOR  SET BP=$ORDER(BPS("B",BP)) QUIT:BP=""  DO
	. WRITE $PIECE(BPS("B",BP),">",1),!
	QUIT
	;
CONVPOS(POS)	; Convert LABEL+offset to +line form
	QUIT:$EXTRACT(POS,1)="+" POS
	NEW OFF,LBL,RTN,LBLLEN,I
	SET LBL=$PIECE(POS,"+",1)
	IF LBL["^" SET LBL=$PIECE(LBL,"^",1),OFF=0
	ELSE  SET OFF=+$PIECE(POS,"+",2)
	SET RTN=$PIECE(POS,"^",2),LBLLEN=$LENGTH(LBL)
	FOR I=1:1 QUIT:$EXTRACT($TEXT(@("+"_I_"^"_RTN)),1,LBLLEN)=LBL
	QUIT "+"_(I+OFF)_"^"_RTN
	;
RNAME(FILE)	; Extract routine name from file path
	SET FILE=$TRANSLATE(FILE,"\","/")
	SET:$PIECE(FILE,".",$LENGTH(FILE,"."))'="m" FILE=""
	QUIT $TRANSLATE($PIECE($PIECE(FILE,"/",$LENGTH(FILE,"/")),".",1),"_","%")
	;
SEND(MSG)	; Debug output helper
	NEW IO
	SET IO=$IO
	USE 0
	WRITE MSG,!
	USE IO
	QUIT
	;
LAST(STR)	; Get last character of string
	QUIT $EXTRACT(STR,$LENGTH(STR))
	;
REPLACE(STR,OLD,NEW)	; Replace all occurrences
	QUIT:OLD="" STR
	NEW POS
	SET POS=0
	FOR  SET POS=$FIND(STR,OLD,POS) QUIT:POS=0  DO
	. SET POS=POS-$LENGTH(OLD)
	. SET STR=$EXTRACT(STR,1,POS-1)_NEW_$EXTRACT(STR,POS+$LENGTH(OLD),$LENGTH(STR))
	. SET POS=POS+$LENGTH(NEW)
	QUIT STR
	;
LINT	; Read and validate MUMPS syntax
	NEW LINE,LINES,I,RES
	FOR I=10000:1 READ LINE:5 QUIT:'$TEST  QUIT:LINE="***ENDPROGRAM"  DO
	. SET LINES(I)=LINE
	WRITE "***BEGINERRCHK",!
	DO COMPILE(.LINES,.RES)
	SET I="" FOR  SET I=$ORDER(RES(I)) QUIT:I=""  WRITE RES(I),!
	WRITE "***ENDERRCHK",!
	QUIT
	;
COMPILE(LINES,RES)	; Validate MUMPS code via compiler
	NEW LN,DEV,IO
	SET IO=$IO
	SET DEV="/tmp/ahmide_lint"_$JOB_".m"
	OPEN DEV USE DEV
	SET LN="" FOR  SET LN=$ORDER(LINES(LN)) QUIT:LN=""  DO
	. WRITE LINES(LN),!
	CLOSE DEV
	DO SYNTAX(DEV,.RES)
	OPEN DEV CLOSE DEV:DELETE
	USE IO
	QUIT
	;
SYNTAX(FILE,RES)	; Run compiler and parse errors
	NEW IO,PIPE,I,LINE
	SET IO=$IO
	SET PIPE="PIPE"
	KILL RES
	OPEN PIPE:(shell="/bin/bash":command="$gtm_dist/mumps -noobject "_FILE_" && echo '***READY'":readonly)::"PIPE"
	USE PIPE
	SET I=1000
	FOR  QUIT:$ZEOF  READ LINE QUIT:LINE="***READY"  DO
	. IF $EXTRACT(LINE,1,12)=($CHAR(9,9)_"At column ") SET RES(I)=$PIECE($PIECE(LINE,"At column ",2),",",1)_";"_$PIECE($PIECE(LINE,"line ",2),",",1)_";"
	. IF $EXTRACT(LINE,1,4)="%YDB"!($EXTRACT(LINE,1,4)="%GTM") SET:$GET(RES(I))="" RES(I)=";;" SET RES(I)=RES(I)_LINE,I=I+1
	CLOSE PIPE
	USE IO
	QUIT
	;
