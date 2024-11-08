! (c) 2016 by Carl Barnes released under MIT License
      Member()
      Include('Equates.CLW'),ONCE
      Map
!        module ('win32')
!          OutputDebugString(*CString), raw, pascal, name('OutputDebugStringA'),Dll(dll_mode)
!        end
      End ! map
      Include('CBCodeParse.inc'),ONCE
!      INCLUDE('CBWndPreview.INC'),ONCE  !for .QueueReflection() download from https://github.com/CarlTBarnes/WindowPreview
!--------------------------------------------------------------------------------------------------
CBCodeFlattenClass.Init    PROCEDURE  ()
  CODE  
  Message('CodeFlattenCls.Init')   
  RETURN
!--------------------------------------------------------------------------------------------------
CBCodeFlattenClass.TrimText1310     PROCEDURE(*STRING Txt, BOOL AddOne1310toEnd=1)  !Remove 13,10,32,9 Leading or Trailing
Ndx LONG,AUTO
    CODE
    LOOP Ndx=1 TO SIZE(Txt)                     !10/14/17 remove all tabs seems like a good idea
         IF VAL(Txt[Ndx]) = 9 THEN Txt[Ndx]=''. !10/14/17 I think this is done later and NOT needed here
    END

    LOOP Ndx=1 TO SIZE(Txt)             !Remove leading 13,10,9,spaces
         IF VAL(Txt[Ndx]) > 32 THEN     !Found first non-space,9,13,10
            Ndx -= 1                    !back up to last 13,10,9,32
            IF Ndx > 0 THEN
               CLEAR(Txt[1 : Ndx])      !Blank out leading  13,10,9,32
            END
            BREAK
         END
    END
    Txt=LEFT(Txt)                       !Remove leading spaces
    LOOP Ndx=SIZE(Txt) TO 1 BY -1       !Remove trailing 13,10
         IF VAL(Txt[Ndx]) > 32 THEN     !Found the last good character > Space
            Ndx += 1                    !move to the space after
            IF AddOne1310toEnd AND Ndx < SIZE(Txt) THEN
               Txt[Ndx : SIZE(Txt)]='<13,10>'
            ELSIF Ndx <= SIZE(Txt) THEN         !11/08/24 add IF <= SIZE check else Ndx+1 can be Index Out of Range
               Txt[Ndx : SIZE(Txt)]='' 
            END
            BREAK
         END
    END
    RETURN
!--------------------------------------------------------------------------------------------------
CBCodeFlattenClass.Flatten    PROCEDURE(*STRING WinTxt)
LnMapQ  QUEUE(CBFlatLineMapQType)
        END
    CODE
    SELF.Flatten(WinTxt, LnMapQ)
!    DO ShowQ
    RETURN
!ShowQ ROUTINE
!    DATA
!PrvCls CbWndPreviewClass    
!    CODE
!    PrvCls.QueueReflection(LnMapQ,'LnMapQ CBFlatLineMapQType')
!--------------------------------------------------------------------------------------------------
CBCodeFlattenClass.Flatten    PROCEDURE(*STRING WinTxt, CBFlatLineMapQType LnMapQ )
LenTxt     LONG,AUTO
Ndx        LONG,AUTO
OutNdx        LONG
InQuote             BOOL            !Am I between 'Quotes'?
InContinuation      BOOL            !Did I run into a |
InComment           BOOL            !Did I run into a !    
NdxLine1    LONG(1)          !10/25 this will not get good text because overwrites input, would need to save original
Is13        BOOL            !10/25 
WinOrig     &STRING !was--> LIKE(WinTxtInput),AUTO          !KILL ME
    CODE
    WinOrig &= NEW(STRING(SIZE(WinTxt)))
    WinOrig = WinTxt                       !KILL ME
    FREE(LnMapQ)                !10/25
    CLEAR(LnMapQ)               !10/25
    LnMapQ.Line1 = 1
    LnMapQ.Line2 = 1
    IF VAL(WinTxt[1])<=32 THEN SELF.TrimText1310(WinTxt).   !10/25 be sure no leading 13,10,32,9
    
    !--Prepare
    LenTxt = LEN(CLIP(WinTxt))

    !--Flatten out the Line Continuations and extra spaces
    InQuote = 0
    OutNdx  = 0  
    LOOP Ndx = 1 TO LenTxt
         IF VAL(WinTxt[Ndx]) = 9 THEN WinTxt[Ndx]=''.       !Changes tabs to spaces

         CASE WinTxt[Ndx]
         OF '|' 
            IF InQuote THEN GOTO BreakCaseLabel: .          !20171017 have | in FORMAT('|')
            InContinuation = True
         OF '!'
            IF InQuote THEN GOTO BreakCaseLabel: .          !20171017 comment in quotes is not a comment
            InComment = True

!wrong place, Flat should have &
!         OF '&'
!            !Crude, toss out & of hot keys, gets some & it should not but who cares, this is for spell check
!            IF InQuote AND Ndx < LenTxt AND VAL(WinTxt[Ndx+1]) > 32 THEN CYCLE.   !if & is not next to space then toss it
!            !Better might be to do this from just PROMPT RADIO OPTION BUTTON

         OF CHR(39)                     !a Quote e.g. PROMPT('Don''t fear the reaper'),AT(1,1),USE(?feq1),HIDE
            IF InComment THEN CYCLE.    ! a comment with Quotes just confuses
            IF ~InQuote                 !Not currently inbetween 'quotes'
                InQuote = True          !   then set flag to true
                IF OutNdx > 1 THEN
                   !db('flatten',WinTxt[OutNdx - 3 : OutNdx] )
                   IF WinTxt[OutNdx - 1 : OutNdx] = '''&' THEN      !change 'xxxx'&'yyyyy' to 'xxxxyyyyy'
                      OutNdx -= 2
                      CYCLE
                   END
                END
            ELSE                    !I am in 'quotes now'
                IF Ndx < LenTxt AND WinTxt[Ndx+1] = CHR(39) THEN   !is is a Doubled quote e.g.  PROMPT('Don''t fear the reaper')
                   OutNdx += 1
                   WinTxt[OutNdx] = WinTxt[Ndx]                    !Take this Quote
                   Ndx += 1                                        !and below keep the next quote
                   InQuote = True                                  !just to make it clear we remain in quotes
                ELSE
                   InQuote = False
                END
            END

         OF CHR(32)
            IF ~InQuote                               !do not keep some spaces outside quotes
                IF OutNdx THEN
                   CASE VAL(WinTxt[OutNdx])     !look at Last character we saved
                   OF 32  ; CYCLE               !Double space then toss it
                   OF 2Ch ; CYCLE               !comma space then toss it
                   OF 28h ; CYCLE               !( space then toss it
                   OF 29h ; CYCLE               !) space then toss it
                   OF 39  ; CYCLE               !' space then toss it
                   OF 38  ; CYCLE               !& space then toss it
                   !preserve 1 space so indented  OF 10  ; CYCLE               !<13,10> space then toss it
                   END
                ELSE
                    CYCLE   !leading spaces get trimmed
                END
            END

         OF CHR(13) OROF CHR(10)
            InComment = False
            InQuote = False                                         !Quotes cannot span lines so force this off cause it must be base code
            IF ~OutNdx THEN Message('Leading 13,10 cannot happen??').                                  !Leading 13,10s get trimmed
            IF ~OutNdx THEN CYCLE.                                  !Leading 13,10s get trimmed

            Is13 = CHOOSE(VAL(WinTxt[Ndx])= 13)           !10/25
            IF Is13 THEN                                  !10/25
               LnMapQ.Txt = LEFT(SUB(WinTxt,NdxLine1,255))       !10/25
!KillMe               LnMapQ.Txt = LEFT(SUB(WinOrig,NdxLine1,255))      !10/25   !KILLME
               NdxLine1 = Ndx+2
               ADD(LnMapQ)                                !10/25
               LnMapQ.Line1  += 1                         !10/25
               LnMapQ.Line2  += 1                         !10/25
            END 
            IF VAL(WinTxt[Ndx])= 10 THEN NdxLine1 = Ndx+1. 
          
            IF OutNdx > 2 AND Ndx + 1 < LenTxt THEN
                IF  WinTxt[OutNdx-1 : OutNdx] = '<13,10>' |         !have 13,10 at end of good text
                AND WinTxt[Ndx      : Ndx+1 ] = '<13,10>' THEN      ! and 13,10 at start
                   Ndx += 1                                         !Skip this 13,10 and catch it next time 
                   NdxLine1 = Ndx+1
                   CYCLE
                END
            END

            IF InContinuation THEN                          !I had a |
                InContinuation = False
                IF Ndx < LenTxt THEN                        !is there more text
                   CASE VAL(WinTxt[Ndx+1])                  ! look at Next character
                   OF 13 OROF 10                            !   is it the 10 after the 13
                      Ndx += 1                              !     then skip it too
                   END
                END
                NdxLine1 = Ndx+1
                LnMapQ.Line2  -= 1                         !10/25
                CYCLE                                       !toss the 13/10 character after the |
            END
            
            !LnMapQ.Line2 += 1       !10/25
         END
         
BreakCaseLabel:
         IF InContinuation OR InComment THEN CYCLE.             !toss these characters
        

         OutNdx += 1
         WinTxt[OutNdx] = WinTxt[Ndx]                    !Take this Quote

    END !LOOP LOOP Ndx

    IF OutNdx < LenTxt THEN CLEAR( WinTxt[OutNdx+1 : LenTxt]).   

    LnMapQ.Txt = LEFT(SUB(WinTxt,NdxLine1,255))     !10/25
!KillMe    LnMapQ.Txt = LEFT(SUB(WinOrig,NdxLine1,255))    !10/25   !KILLME
    ADD(LnMapQ)                                     !10/25  !The Last Line
    
    SORT(LnMapQ,LnMapQ.Line2,LnMapQ.Line1)               !10/25
    DISPOSE(WinOrig)
    RETURN

!====================================================================================
!CBCodeParseClass    Class(),Type,Module('CBCodeParse.Clw'),DLL(0),Link('CBCodeParse.Clw',1) 
CBCodeParseClass.FindAttrParen   PROCEDURE(STRING CodeTxt, STRING AttName, *LONG BegAttPos, *LONG BegParen, *LONG EndParen, *LONG BegQuote, *LONG EndQuote) !,BOOL
LenCodeTxt  LONG,AUTO 
LenAttName  LONG,AUTO 
RetBool     BOOL
InQuote     LONG
XX          LONG,AUTO
AttBeg      LONG,AUTO
AttEnd      LONG,AUTO
    CODE
    CodeTxt=UPPER(CodeTxt)
    AttName=UPPER(AttName)
    LenCodeTxt=LEN(CLIP(CodeTxt))
    LenAttName=LEN(CLIP(AttName))
    BegAttPos=0 ; BegParen=0 ; EndParen=0 ; BegQuote=0 ; EndQuote=0
    
! LIST,AT(4,16,447,257),USE(?Browse:1),IMM,HVSCROLL,MSG('Browsing Records'),FORMAT('25R(6)|M~Code~C(0)@n3@[160L(2)|M@s40@/160L(2)|M@s40@](167)|M~Description~44C|M~A' &|
!     'ssess Pen.~@s1@32C(2)|M~Pen. Cd.~C(0)@n3@35C|M~Rev. Cat.~@s1@41C|M~Pymt Alloc~@s' &|
!     '1@26C|M~Active~@s1@4C|M~Add To Bal.~@s1@'),FROM(Queue:Browse:1),#SEQ(1),#ORIG(?List),#FIELDS(BillCds:BillCode,BillCds:Desc1,BillCds:Desc2,BillCds:AssessPenalty,BillCds:PenaltyCode,BillCds:RevCategory,PymtAlloc,BillCds:Active,BillCds:AddToBalan
       
    
    LOOP XX = 1 TO LenCodeTxt
        ! IF _Line[LnX] = ')' AND ~InQuote THEN BREAK.   
         IF CodeTxt[XX] = CHR(39) THEN       !is it a Quote
            InQuote = 1 - InQuote
            IF BegAttPos AND ~BegQuote THEN BegQuote = XX.
            IF BegAttPos               THEN EndQuote = XX.
         END
!         EndPos = LnX
         IF InQuote THEN CYCLE. 
         
         IF ~BegAttPos THEN                     ! 1234567
             IF CodeTxt[XX]<>'(' THEN CYCLE.    !,FORMAT( 
             AttEnd = XX     - 1                !      ^  -1 
             LOOP
                IF AttEnd<1 OR CodeTxt[AttEnd]<>'' THEN BREAK.  !move past space between ATTNAME and (  e.g  FORMAT (
                AttEnd -= 1
             END 
             AttBeg = AttEnd + 1 - LenAttName           ! 654321 
             IF AttBeg < 1 THEN CYCLE.          !^        need 2 so room for the Delim               
             IF LEFT( CodeTxt[ AttBeg : AttEnd ] ) <> AttName THEN CYCLE.
             IF AttBeg=1 OR CodeTxt[AttBeg-1]=',' OR CodeTxt[AttBeg-1]=' ' THEN
                BegAttPos = AttBeg 
                BegParen  = XX
             END 
             CYCLE 
         END 
         !We are inside (
         IF CodeTxt[XX]<>')' THEN CYCLE. 
         EndParen = XX
         RetBool = True 
         BREAK
    END !Loop

    RETURN RetBool
!--------------------------------------------------------------------------------------------------
!Example Input: LIST,AT(11,55,656,169),MSG('Browsing Records'),FROM(Queue:Browse:1)  
!       Output: LIST,AT(11,55,656,169),MSG('                '),FROM(Queue:Browse:1)  
!TODO maybe should have option to clear between Parens (). 
!     The original purpose was to find letters so having AT(1,2,3,4) was no problem
CBCodeParseClass.MakeCodeOnlyLine  PROCEDURE(CONST *STRING ALine, *CSTRING COLine) !Keep only Keywords, mask 'text'
X         LONG
LenLn     LONG
InQuote   BOOL
    CODE
    LenLn = LEN(CLIP(ALine)) 
    IF ~LenLn THEN
        CoLine=''
        RETURN
    END
    CoLine = UPPER(ALine[1 : LenLn] & ' ')     !Leave trailing space
    LOOP X=1 TO LenLn
         IF VAL(CoLine[x])=39 THEN 
            InQuote=1-InQuote
            CYCLE
         END   
         IF InQuote THEN CoLine[x]=' '.      !Keep nothig between quotes
         CASE CoLine[x] 
         OF CHR(9)       ; CoLine[x]=' '
         OF '.' OROF ';' ; CoLine[x]=' '     !blank . ; to avoid no space aftef THEH EXIT. 
         OF '!' OROF '|'                     !Comment or Continue ends line
            CoLine[x]=' '
            CoLine=SUB(CoLine,1,X)
            BREAK
         END
    END
    RETURN
!--------------------------------------------------------------------------------------------------
CBCodeParseClass.ParseCOLineAttrib2Q  PROCEDURE(CONST *STRING ALine, CONST *CSTRING COLine, QUEUE LinesQ) !Split at Commas between Attribs
X         LONG
LenLn     LONG
InQuote   SHORT
InParen   SHORT
SegBegin  LONG
SegEND    LONG
    CODE             
    FREE(LinesQ) 
    LenLn = LEN(COLine) 
    IF ~LenLn THEN RETURN.
    SegBegin = 1
    LOOP X=1 TO LenLn+1
         IF VAL(CoLine[x])=39 THEN    !in 'Quotes' Ignore
            InQuote=1-InQuote
            CYCLE
         END   
         IF InQuote THEN CYCLE.
         IF InParen                           !in () ignore commas (1,2,3,4) 
            IF CoLine[x]=')' THEN InParen -= 1.  !Can there be nested () assume yes
            CYCLE             
         END 
         IF CoLine[x]='(' THEN    InParen += 1.

         IF X=LenLn+1 THEN 
            SegEND = X-1
         ELSIF CoLine[x]=',' THEN 
            SegEND = X 
         ELSE
            CYCLE
         END
         IF SegBegin <= SegEND AND SegBegin THEN     !is valid slice?
            LinesQ = ALine[SegBegin : SegEND] 
            ADD(LinesQ)
         END    
         SegBegin = X+1
    END
    RETURN 
!--------------------------------------------------------------------------------------------------


