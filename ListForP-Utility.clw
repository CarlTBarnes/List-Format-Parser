MsgLineBreak        PROCEDURE(STRING Txt)!,STRING 
RT      ANY
ChX     LONG,AUTO
    CODE
    LOOP ChX= 1 TO LEN(CLIP(Txt)) BY 80
         RT = CHOOSE(CHX=1,'',RT & '<13,10>') & SUB(Txt,ChX,80) 
    END 
    RETURN RT     
!================================================ 
GetExample        PROCEDURE(BYTE ExpNo, <*STRING GenQueFmtExample>)!,STRING
Exp1 STRING('     LIST,AT(3,22,360,121),USE(?BrowseMeters),IMM,HVSCROLL,' &|
     'FORMAT(''[27R(2)|M~Cust.~C(0)@n_6@41R(4)|M~Meter~C(0)@n_9@4C|M~P' &|
     'ort~@n1@](85)|M~I.D. Numb'' &|' &|
     '<13,10>           ''ers~[38R(2)|M~Previous~C(0)@d1@47R(2)|M~Current~C(0)@d1' &|
     '@](76)|M~Reading Dates~[4'' &|' &|
     '<13,10>           ''1R(2)|M~Current~C(0)@n_9@41R(2)|M~Prior~C(0)@n_9@39R(2)' &|
     '|M~Usage~C(0)@n-9@22C|M~M'' &|' &|
     '<13,10>           ''eter~@s1@21C|M~Read~@s1@4C|M~Final~@s1@](224)|M~Readings~''),' &|
     'FROM(Queue:BrowseMeters),#SEQ(1), |' &|
     '<13,10>           #FIELDS(CustMeter:CustIdNo,CustMeter:MeterId' &|
     'No,CustMeter:PortNo,CustMeter:BegReadingDate,CustMeter:CurReadingDate,CustM' &|
     'eter:CurReading,CustMeter:BegReading,CustMeter:Usage,Meters:MeterType,CustM' &|
     'eter:ReadingType,CustMeter:FinalReading)' )
Exp2  STRING('          LIST,AT(139,11,191,167),USE(?List:EmpTypes),HVSCROLL,FORMAT(''15R(1)|M' &|
     '~Cd.~L@n2@15C(1'' & |' &|
     '<13,10>            '')|MI@s10@120L(1)|M~Employee Type~@s30@''),FROM(EmpTypeQ),IMM, |' &|
     '<13,10>            #FIELDS(TYP:Code,Tag,TYP:Desc),#SEQ(7)' )
Exp3  STRING(' LIST,AT(11,55,656,169),USE(?Browse:Employees),VSCROLL,FORMAT(''126L(2)|*~Employee Name'' & |' &|
     '<13,10>   ''~@s30@?[19C~Type~L(0)@n2@132L(2)|~Description~@s30@](134)|[19R(2)~Bldg.~C('' & |' &|
     '<13,10>   ''0)@n2@100L(2)|~Description~@s25@]|20C|~ Pay<<0DH,0AH>Grp~@n_2@13C|*~St.~@s1'' & |' &|
     '<13,10>   ''@[30R(2)|~Board~C(0)@n6.2~%~b@24L(2)|~Empl.~C(0)@n6.5b@](55)|_~TRS Data~40R(3)|~SSN~C(0)@'' & |' &|  !@Pic with ~%~
     '<13,10>   ''P###-##-####P@20R(3)|~Emp<<0DH,0AH>No~C(0)@N_5@24R(2)|~Clock~C(0)@n_6@33R(2)|M~EIN~C'' & |' &|
     '<13,10>   ''(0)@n_9b@15R(5)|M~EIS<<0DH,0AH>Exempt~C(0)@n3~Exm~B@''),FROM(Queue:Browse:Employees),IMM, |' &|       !@Pic with ~Exm~ Trick to convert Byte to String
     '<13,10>   #FIELDS(MST:LastFirstMiddleName,MST:EmpType,TYP:Desc,MST:BldgNo,LOC:BldgDesc,MST:PayGroup, |' &|
     '<13,10>   MST:PayStatus,MST:TrsBrdRate[1],MST:TrsEmpRate[1],MST:SSN,MST:EmpNo,MST:ClockNo, |' &|
     '<13,10>   MST:StateEIN,MST:EISExempt)' )  
Exp4 STRING('Window WINDOW(''LIST Everything''),AT(,,395,224),GRAY,FONT(''Segoe UI'',9)' &|
     '<13,10> LIST,AT(3,2,379,206),USE(?List:Everything),FROM(EverythingQ),FORMAT(''45L(2)|_FM*IY'' & |' &|
     '<13,10>         ''PT(LRB)~Tree List~@s55@Z(5)/?#1#43L(2)|_FM*YPT(LRB)~Everything~'' & |' &|
     '<13,10>         ''@s55@Z(55)Q''''DefaaultTip''''/?#2#56L(2)B(00008000H)|_FM*YPT(LRB)~'' & |' &|
     '<13,10>         ''Everything with Colors~@s55@Z(55)Q''''DefaultTip''''E(00000000H,000'' & |' &|
     '<13,10>         ''00080H,00FFFFFFH,00FF0000H)/?#3#[20L(4)F~SimpleCol~L(2)@s55@#4#'' & |' &|
     '<13,10>         '']|_FMHT(0000FFFFH)HB(00008000H)~Group Head~L(4)S(100)'')' &|
     '<13,10>    END' ) 
Exp5 STRING('ModTest WINDOW(''All Modifiers and Pictures''),AT(,,586,90),GRAY,FONT(''Segoe UI'',9),RESIZE' &|
     '<13,10> LIST,AT(1,1,583,81),USE(?AllMods_and_Pics),FORMAT(''36L(2)|FM*~Colored *~C(0)@s10@16L'' & |' &|
     '<13,10>     ''(2)I@p p@34L(2)|M~Icon~@s8@16L(2)J@p p@36L(2)|M~Tran Icon~@s9@7'' & |' &|
     '<13,10>     ''0L(2)|MT(1)~Tree T(1)~C(0)@s12@38R(2)|M~Style Z(3)~C(0)@n-9.2@Z'' & |' &|
     '<13,10>     ''(3)40R(2)|MY~Col Style Y~C(0)@N08@40R(2)|MP~Tooltip P~C(0)@n-13.2@'' & |' &|
     '<13,10>     ''26R(2)|M~Tip Q~C(0)@n5.1~%~@Q''''Tip in Format() as Q Modifier for All Rows<<0Dh,0Ah>Also picture with Tildes ~%~'''''' & |' &|
     '<13,10>     ''40R(2)|M~Euro n9`2~C(0)@n-10`2@42L(2)|M~SSN  P # P~C(0)@p###'' & |' &|
     '<13,10>     ''-##-####p@30R(2)|M~Time~C(0)@t3@40L(2)|M~Date d4 ~@d4@48L(2)|M~'' & |' &|
     '<13,10>     ''E12.1 Picture~L(1)@E12.1@77L(2)|MS(100)~Text Scroll S(100)~@s255@'')' &|
     '<13,10>  END' )
Exp6 STRING(' LIST,AT(180,14,90,11),USE(TransWanted),VSCROLL,DROP(14),FROM(''ALL Types|'' & |' &|
     '<13,10>   ''#ALL|TRS|THIS|Surcharge|#SURCH|T.R.I.P.|#TRIP|Federal TR'' & |' &|
     '<13,10>   ''S|#FEDTRS|SSP ** ALL Types **|#SSP_ALL|SSP Standard |#SSPSTD|SSP Catchup |'' & |' &|
     '<13,10>   ''#SSPCAT|SSP Special |#SSPSPE|SSP Roth Standard |#ROTHSTD|SSP Roth Catchup '' & |' &|
     '<13,10>   ''|#ROTHCAT|SSP Roth Special |#ROTHSPE|Board Paid SSP|#SSPBRD'')' )
    CODE
    IF ~OMITTED(GenQueFmtExample) THEN DO GenQueRtn.
    IF ~ExpNo THEN !    1              2                  3               4          5                     6
        ExpNo=POPUP('Customer Meter|Simple Emp Type Tag|Employee Browse|Everything|All Modifiers and Pictures|FROM("string") Test')
    END
    EXECUTE ExpNo
    RETURN Exp1
    RETURN Exp2
    RETURN Exp3 
    RETURN Exp4
    RETURN Exp5
    RETURN Exp6
    END
    RETURN Exp3
GenQueRtn ROUTINE
    GenQueFmtExample = |
           'DirQ  QUEUE,PRE(DirQ)   !Example of File:Queue' &|
    '<13,10>Name       STRING(255)' &|
    '<13,10>ShortName  STRING(13)  !Hide' &|
         '<13,10>![' &|
    '<13,10>Date       LONG        !@d8' &|
    '<13,10>Time       LONG' &|
         '<13,10>!]Last Modified' &|
    '<13,10>Size       LONG' &|
    '<13,10>Attrib     BYTE        !Omit' &|
    '<13,10>      END' & | 
    '<13,10,13,10>!1. Click "Process Queue" to parse this text into Fields List ===>' & | 
          '<13,10>!2. Click "Generate Format" to build FORMAT from the Fields.' & | 
    '<13,10,13,10>!Paste your QUEUE or FILE here <13,10>!then cick "Process Queue" then "Generate Format"' 
    EXIT
    OMIT('**END**') 
ModTest WINDOW('Modifiers and Picutres'),AT(,,586,90),GRAY,FONT('Segoe UI',9),RESIZE
 LIST,AT(1,1,583,81),USE(?List1),FORMAT('36L(2)|M*~Colored *~C(0)@s10@16L' & |
         '(2)I@p p@34L(2)|M~Icon~@s8@16L(2)J@p p@36L(2)|M~Tran Icon~@s9@7' & |
         '0L(2)|MT(1)~Tree T(1)~C(0)@s12@38R(2)|M~Style Z(3)~C(0)@n-9.2@Z' & |
         '(3)40R(2)|MY~Col Style Y~C(0)@N08@48R(2)|MP~Tooltip P~C(0)@n-13' & |
         '.2@40R(2)|M~Euro n9`2~C(0)@n-10`2@42L(2)|M~SSN  P # P~C(0)@p###' & |
         '-##-####p@30R(2)|M~Time~C(0)@t3@40L(2)|M~Date d4 ~@d4@48L(2)|M~' & |
         'E12.1 Picture~L(1)@E12.1@')
    END  
    !end of OMIT('**END**')
!====================================================
ModifierHelpPopup PROCEDURE(STRING XPos, STRING YPos, STRING HelpBtnFEQ)
DoNotOpenThis BYTE,STATIC
P LONG,DIM(4),STATIC
ModWin WINDOW('Modifiers'),AT(,,180,228),GRAY,SYSTEM,ICON('LFmtIcon.ico'),FONT('Segoe UI',8,COLOR:Black), |
            COLOR(0E1FFFFH),RESIZE
        CHECK('Don''t Open'),AT(121,1),USE(DoNotOpenThis),SKIP,TRN,TIP('Do not open Modifiers window' & |
                ' when Help is shown')
        TEXT,AT(0,15,,213),FULL,USE(ModifierHelp),FLAT,HVSCROLL,FONT('Consolas',9),READONLY
    END
    CODE
    !03/31/21 ThreadSafe  .... IF ThreadOpen ==========0   !ICE() will set ThreadOpen=THREAD() IF it was =0
    IF InterlockedCompareExchange(ModifierHelp_ThreadOpen,THREAD(), 0) <> 0 THEN  !Returns initial value of ThreadOpen so IF <>0 is already running
!12/29/21 not with new CwHelp  POST(EVENT:User+1,,ThreadOpen)                     !Possible Race Condition that TheadOPen was set to Zero by a closing window in this time slice
       RETURN
    END
    !03/31/21 This "ThreadOpen=THREAD()" is done in InterlockedCompareExchange() only when ThreadOpen=0
    OPEN(ModWin)
    IF P[3] THEN SETPOSITION(0,P[1],P[2],P[3],P[4]) ELSE SETPOSITION(0,Xpos,YPos).
!    ?ModifierHelp{PROP:Color}    =80000018h !COLOR:InfoBackground  12/29/21 colors now on Window
!    ?ModifierHelp{PROP:FontColor}=80000017h !COLOR:InfoText
    ACCEPT
        CASE EVENT()
        OF EVENT:OpenWindow  ; POST(EVENT:Accepted,HelpBtnFEQ,1)
        OF EVENT:User+1
           0{PROP:Iconize}=0 ; 0{PROP:Active}=1
        END
        CASE ACCEPTED()
        OF ?DoNotOpenThis ; BREAK
        END
    END
    GETPOSITION(0,P[1],P[2],P[3],P[4])
    IF ~DoNotOpenThis THEN InterlockedExchange(ModifierHelp_ThreadOpen,0).   !03/31/21 does ThreadOpen=0 Thread Safe
    RETURN
!====================================================
ChrCount PROCEDURE(STRING Text2Scan, STRING ChrList)!LONG 
X LONG,AUTO
CntChr LONG
    CODE
    LOOP X=1 TO SIZE(Text2Scan)
        IF INSTRING(Text2Scan[X],ChrList) THEN CntChr += 1.
    END
    RETURN CntChr
!====================================================
CharNumeric PROCEDURE(STRING FmtChar, LONG CharPosition=0)!,BOOL   !Is Char Numeric or '-' in Pos 1? - Format Width is Numeric, allow for '-1'
IsNumber BOOL
    CODE
    IF CharPosition=1 AND FmtChar[1]='-' THEN
       IsNumber=True                            !Allow '-1' to be seen as Negative
    ELSE
       IsNumber=NUMERIC(FmtChar[1])
    END
    RETURN IsNumber 
!====================================================
InBetween PROCEDURE(STRING FindLeft,STRING FindRight, CONST *STRING SearchTxt, <*LONG OutLeftPos>, <*LONG OutRightPos>, <*STRING OutBetweenStr>, BOOL IncludeLeftRight=0)!,LONG,PROC  !Returns -1 Not Found or Length Between, may =Zero
LengthOut LONG,AUTO   !-1 = Not Found, 0=No Data Between
PosLeft   LONG,AUTO
PosRight  LONG,AUTO
NoUpper   BOOL,AUTO 
    CODE
    PosRight=0 ; LengthOut = -1   !-1 = Not Found 
    FindLeft=UPPER(FindLeft) ; FindRight=UPPER(FindRight)
    NoUpper=CHOOSE(FindLeft  & FindRight = lower(FindLeft & FindRight))   !No UPPER() a bit Faster with Large SearchTxt
    IF NoUpper THEN PosLeft=INSTRING(FindLeft,      SearchTxt ,1,1) |                               !e.g. find "PRE(" on Left
    ELSE            PosLeft=INSTRING(FindLeft,UPPER(SearchTxt),1,1) .
    IF PosLeft THEN
       IF NoUpper THEN PosRight=INSTRING(FindRight,      SearchTxt ,1,PosLeft+LEN(FindLeft)) |      !e.g. find ")" on Right
       ELSE            PosRight=INSTRING(FindRight,UPPER(SearchTxt),1,PosLeft+LEN(FindLeft)) .
       IF PosRight THEN
          IF ~IncludeLeftRight THEN 
              PosLeft  += LEN(FindLeft)      ! Exclude so LEFT  is ^ in  "PRE(^"
              PosRight -= 1                  !            RIGHT is ^ in  "PRE(  ^)"
          ELSE                               ! Include so LEFT  is ^ in "^PRE("
              PosRight += (LEN(FindRight) -1)         !   RIGHT is ^ in  "PRE(  )^"
          END 
          LengthOut   = PosRight - PosLeft + 1  !May =0 if Left+1=Right i.e. PRE() with nothing between
       END 
    END 
    IF ~OMITTED(OutLeftPos)  THEN OutLeftPos  = PosLeft.
    IF ~OMITTED(OutRightPos) THEN OutRightPos = PosRight.    
    IF ~OMITTED(OutBetweenStr) THEN 
        OutBetweenStr=CHOOSE(LengthOut<1,'',SearchTxt[PosLeft : PosRight])
    END                     !~PosLeft OR PosLeft>PosRight
    RETURN LengthOut    

InBetween PROCEDURE(STRING FindLeft,STRING FindRight, STRING InLiteral, <*LONG OutLeftPos>, <*LONG OutRightPos>, <*STRING OutBetweenStr>, BOOL IncludeLeftRight=0)!,LONG,PROC  !Returns -1 Not Found or Length Between, may =Zero
    CODE
    RETURN InBetween(FindLeft,FindRight, InLiteral, OutLeftPos,OutRightPos, OutBetweenStr)
!====================================================
LenSizeText  PROCEDURE(STRING VariableName, *STRING StringVar)  
    CODE
    RETURN LenSizeText(VariableName,StringVar,SIZE(StringVar))
LenSizeText  PROCEDURE(STRING VariableName, *CSTRING StringVar)
    CODE
    RETURN LenSizeText(VariableName,StringVar,SIZE(StringVar))
LenSizeText  PROCEDURE(STRING VariableName, *? StringVar, LONG StringSize) !Used by LengthsTextRtn ROUTINE  to fill in LengthsText
L  LONG,AUTO
S  LONG,AUTO
F  LONG,AUTO
FPct LONG,AUTO
StrPart STRING('String(12345)')
UsedPart STRING(6),AUTO
FreePart STRING(6),AUTO
    CODE
    L=LEN(CLIP(StringVar))
    S=StringSize    !SIZE(StringVar)
    F=S-L
    StrPart='String('& S &')'
    UsedPart = L
    FreePart = F
    FPct=INT(F/S*100)
    IF FPct < LenMinFreePct THEN LenMinFreePct=FPct.
    RETURN VariableName &' <9>' & StrPart &' used '& UsedPart & FORMAT(INT(L/S*100),@n3) &'% = '& FreePart &'free '& FORMAT(FPct,@n3) &'% <13,10>'
!====================================================
ListMakeOver PROCEDURE(LONG ListFEQ, LONG ExtraLineHt, BOOL NoGridColor=False)   !Set LIST Grid Color and Line Height
    CODE
    IF ~NoGridColor THEN ListFEQ{PROPLIST:Grid}=COLOR:3DLight.   !or Color:Silver
    IF ExtraLineHt
       ListFEQ{PROP:LineHeight} = ExtraLineHt + ListFEQ{PROP:LineHeight}
    END
    ListFEQ{PROP:NoTheme}=1  !Makes Heading Gray not White
    RETURN
!====================================================
MessagePipeFix PROCEDURE(STRING MsgTxt)!,STRING   !Change Pipes in Msg that cause Line Breaks to !
L LONG,AUTO
X LONG,AUTO
    CODE
    L=LEN(CLIP(MsgTxt))
    LOOP X=1 TO L
        CASE MsgTxt[X]
        OF '|' ; MsgTxt[X]=CHR(166)  ! 166 = a kind of Pipe, or 161=! upside down
        END
    END
    IF ~L THEN RETURN ''.
    RETURN MsgTxt[1 : L]
!====================================================
No1310 PROCEDURE(STRING Txt)
N USHORT,AUTO
U USHORT
    CODE
    LOOP N=1 TO LEN(CLIP(Txt))
        CASE VAL(Txt[N])
        OF 10 OROF 13 ; CYCLE
        OF 9 ; Txt[N]=''
        END
        U+=1
        IF U<N THEN Txt[U]=Txt[N].
    END
    RETURN SUB(Txt,1,U)
NoTabs PROCEDURE(*STRING Txt)
N LONG,AUTO
    CODE
    LOOP N=1 TO LEN(CLIP(Txt))
        CASE VAL(Txt[N])
        OF 9 OROF 160 ; Txt[N]=''    !Hard Space 160 A0h
        END
    END
    RETURN
PopupUnder PROCEDURE(LONG CtrlFEQ, STRING PopMenu)!,LONG
X LONG,AUTO
Y LONG,AUTO
H LONG,AUTO
    CODE
    GETPOSITION(CtrlFEQ,X,Y,,H)
    IF CtrlFEQ{PROP:InToolBar} THEN Y -= (0{PROP:ToolBar}){PROP:Height}.
    RETURN POPUP(PopMenu,X,Y+H+1,1) 
ReplaceInto PROCEDURE(*STRING Into, STRING Find,STRING Repl,BYTE ClipInto=0)!,LONG,PROC !Return Count
X   LONG,AUTO
L   LONG,AUTO
szI LONG,AUTO   
szF LONG,AUTO   
szR LONG,AUTO
FindCnt LONG   
  CODE !From ABError, tweaked a LOT. Supports replace the same char e.g. 10 to 13,10
  Find=lower(Find) ; L=1 ; szI=SIZE(Into) ; szF=SIZE(Find) ; szR=SIZE(Repl)  
  IF NOT(Find=lower(Repl) AND szF=szR) THEN 
     LOOP
       IF ClipInto THEN X=INSTRING(Find,CLIP(lower(Into)),1,L) ELSE X=INSTRING(Find,lower(Into),1,L).
       IF ~X THEN BREAK.
       Into=SUB(Into,1,X-1) & Repl & SUB(Into,X+szF,szI) ; L=X+szR ; FindCnt+=1
     END
  END
  RETURN FindCnt
!---------------
UppLow PROCEDURE(*STRING Txt, LONG HowUPloUplo=3)  !1=Upper or 2=Lower or 3=Up[1] + Low[2:end]
    CODE
    CASE HowUPloUplo
    OF 1        ; Txt=UPPER(Txt)
    OF 2 OROF 3 ; Txt=lower(Txt)
           IF 3 = HowUPloUplo AND SIZE(Txt) THEN Txt[1]=UPPER(Txt[1]).
    END
    RETURN
!================================
Picture_N_Width  PROCEDURE(SHORT pDigitsTotal, SHORT pDecimals, BOOL pMinus, BOOL pCommas, STRING pBlankB, *STRING OutPicture )!,SHORT,PROC 
PicWidth SHORT,AUTO
IntegerDigits SHORT,AUTO
    CODE
    IntegerDigits = pDigitsTotal - pDecimals 
    PicWidth = IntegerDigits    
    IF pCommas   THEN PicWidth += INT( (IntegerDigits-1)/3 ).
    IF pMinus    THEN PicWidth += 1.
    IF pDecimals THEN PicWidth += (1 + pDecimals).  ! 1+ Room for Decimal point
    OutPicture='n' & |                                       !@n
                CHOOSE(~pMinus, '','-') & |                  !  - 
                CHOOSE(~pCommas,'_','') & |                  !   _
                PicWidth                & |                  !    12
                CHOOSE(~pDecimals,'','.' & pDecimals ) & |   !      .2
                pBlankB                                      !        b
!  DB('Picture_N_Width pDigitsTotal=' & pDigitsTotal &' IntegerDigits=' & IntegerDigits &' pDecimals=' & pDecimals & |
!        ' pMinus='& pMinus &' pCommas='& pCommas  &' pBlankB='& pBlankB  &' PicWidth='& PicWidth &'  Out=' & OutPicture )

    RETURN PicWidth                
!====================================================

