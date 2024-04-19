!---- Data and WINDOW for List Format Parser ----
!Region -- Data for List Parse
Ndx             LONG
FlattenCls      CBCodeFlattenClass
ParserCls       CBCodeParseClass
ListControl     STRING(8000)
ListFlat        STRING(6000)
ListParsed      STRING(32000)  !List Lines tab with Attribs and Format one per line
DebugTabs        BYTE
DebugMsgs        BYTE
LengthsText      STRING(1200)

FormatGrp  GROUP,PRE()    !Pre(Fmt)
Fmt:Found           BOOL
Fmt:BegPos  LONG
Fmt:Paren1  LONG
Fmt:Paren2  LONG
Fmt:Quote1  LONG
Fmt:Quote2  LONG
Fmt:IsLast  BOOL             !10/02/18 FORMAT() is last so do not append Comma+Pipe
Fmt:Format  STRING(4000)     !Format string extracted from LIST
Fmt:TokFmt  STRING(4000)     !Format in my token format
Fmt:InLines STRING(10000)    !Format() one per line in my token format
Fmt:Explain STRING(24000)    !Comments about the Format
        END

FromGrp  GROUP,PRE()         !11/08/21 Add a FROM Tab
From:Found   BOOL
From:BegPos  LONG
From:Paren1  LONG
From:Paren2  LONG
From:Quote1  LONG
From:Quote2  LONG
From:FROM    STRING(2000)
From:InLines STRING(3000)
From:CASE    STRING(4000)
        END

FieldsGrp  GROUP,PRE()    !Pre(Flds)
Flds:Found   BOOL
Flds:Records LONG
Flds:BegPos  LONG
Flds:Paren1  LONG
Flds:Paren2  LONG
Flds:FieldsFlat STRING(2400)  !Flat without returns
Flds:InLines    STRING(3000)  !For TEXT aligned with Fmt:InLines
Flds:FieldsCode CSTRING(5000) !In Lines with Pipes
        END

FormatQ     QUEUE,PRE(FmtQ)     !         FORMAT( parsed into Fields
Pos1            USHORT          !FmtQ:Pos1
Pos2            USHORT          !FmtQ:Pos2
LenSpec         SHORT           !FmtQ:LenSpec
FldNo           SHORT           !FmtQ:FldNo      !This is really Column Number, the "Field #" pertains to Queue
GrpNo           SHORT           !FmtQ:GrpNo      ! +# for [ open ... and -# for ] close i.e. Groups have 2 records
InGrpNo         SHORT           !FmtQ:InGrpNo    !04/05/24 can tell if Field inside group
FieldSpec       STRING(256)     !FmtQ:FieldSpec
TokenSpec       STRING(256)     !FmtQ:TokenSpec
            END
FieldsQ     QUEUE,PRE(FldsQ)
Name            STRING(96)      !FldsQ:Name
            END
Format2QCls CLASS
Parse2Q         PROCEDURE(CONST *STRING Fmt, *STRING OutTokFmt)
Format2Token    PROCEDURE(CONST *STRING Fmt, *STRING OutTokFmt, BYTE BlankNoise=1)
ParseSegment    PROCEDURE(CONST *STRING Fmt, CONST *STRING TokFmt, Long SegPosBeg, Long SegPosEnd, *SHORT NxtFldNo, SHORT ThisGrpNo, SHORT InsideGrpNo=0 )
GetLinesFmt     PROCEDURE(*STRING FldsLines),STRING
GetExplainLines PROCEDURE(),STRING
TokFmtCmd       PROCEDURE(STRING FindTxt, STRING CmdName),STRING !If Instring FmtTok
AssignSLM       PROCEDURE(*STRING ToStr, STRING FromStr, *USHORT OutLen, *USHORT InOutMaxLen)   !Assign String with length
            END

ExplainQ    QUEUE,PRE(ExpQ)
FmtLen        USHORT        !ExpQ:FmtLen
FmtTxt        STRING(256)   !ExpQ:FmtTxt      19C~Type~L(0)@n2@
GrpLen        USHORT        !ExpQ:GrpLen
GrpTxt        STRING(8)     !ExpQ:GrpTxt      Gr[##
InGrp         STRING(1)     !ExpQ:InGrp       : or Black
ColLen        USHORT        !ExpQ:ColLen
ColTxt        STRING(8)     !ExpQ:ColTxt      ##. or #1#
ModLen        USHORT        !ExpQ:ModLen
ModTxt        STRING(64)    !ExpQ:ModTxt      !Modifiers Explained  *Colors<Q4*Long>
FieLen        USHORT        !ExpQ:FieLen
FieTxt        STRING(64)    !ExpQ:FieTxt      !#Fields Variable  Pre:Var
            END

HistoryQ    QUEUE,PRE(HisQ)
Time            STRING(5)               !HisQ:Time
Date            STRING(5)               !HisQ:Date
List255         STRING(255)             !HisQ:List255
Format255       STRING(255)             !HisQ:Format255       From Fmt:Format
ListControl     LIKE(ListControl)       !HisQ:ListControl
            END

HelpCls CLASS
Init        PROCEDURE()
Init2       PROCEDURE()
Add1Q       PROCEDURE(STRING _Char, STRING _Type, STRING _Prop, STRING _Name, STRING _Desc)
Add1ModExQ  PROCEDURE(STRING _Char, BYTE _ExtraCnt, STRING _Desc)  !Add 1 Modifier Extra to  ModExtraQ
Set1QDesc   PROCEDURE(STRING _Char, STRING _Desc)
        END
HelpSyntax      STRING(200)
HelpWidth       STRING(600)
HelpModOrder    STRING(1600)

ModifierQ   QUEUE,PRE(ModQ)     !LIST Modifiers
Char            STRING(4)       !ModQ:Char
Prop            STRING(100)     !ModQ:Prop      !w/o PROPLIST:
Type            STRING(5)       !ModQ:Type (Category) Align Data Head Group Color Tree Style
Name            STRING(60)      !ModQ:Name
PropFull        STRING(60)      !ModQ:PropFull  !with PROPLIST:
Desc            STRING(1024)    !ModQ:Desc
Sort            STRING(7)       !ModQ:Sort = lower(Char) + Rec# so Unique Key
Lookup          STRING(2)       !ModQ:Lookup - 1 for most, or 2 for HB HT - For GET on Mod Tips
            END
ModExtraQ   QUEUE,PRE(ModExQ)   !LIST Modifiers that add Extra Fields to the Queue
Order           BYTE            !ModExQ:Order       123456
Char            STRING(1)       !ModExQ:Char        *IJTYP
ExtraCnt        BYTE            !ModExQ:ExtraCnt    4 or 1
Desc            STRING(60)      !ModExQ:Desc
            END

PrefixFieldInExplain  SHORT(1)
ShowExplainSpaces     SHORT(0)
ListParsedHScrollOff  SHORT     !AKA Wrap lines
!EndRegion -- Data for List Parse
!Region -- Data for Generate Format Simple
GenFmt_Simple   GROUP,PRE(GenSim)
Columns             BYTE(9)                 !GenSim:Columns
Width               USHORT(80)              !GenSim:Width
JustLCR             STRING('L')             !GenSim:JustLCR
Indent              SHORT(2)                !GenSim:Indent          ()
Picture             STRING('s255 {8}')      !GenSim:Picture         @ @
RightBorder         BYTE(1)                 !GenSim:RightBorder     |
Underline           BYTE                    !GenSim:Underline       _
Fixed               BYTE                    !GenSim:Fixed           F
Resize              BYTE(1)                 !GenSim:Resize          M
Colored             BYTE                    !GenSim:Colored         *
CellStyle           BYTE                    !GenSim:CellStyle       Y
FieldNumbered       BYTE                    !GenSim:FieldNumbered   # #
HeaderRow           BYTE(1)                 !GenSim:HeaderRow
HeaderText          STRING('Column {16}')   !GenSim:HeaderText   ~ ~
HdrJustLCR          STRING('L')             !GenSim:HdrJustLCR
HdrIndent           SHORT(2)                !GenSim:HdrIndent    ()
AutoGenerate        BYTE(1)                 !GenSim:AutoGenerate
OnePerLine          BYTE(1)                 !GenSim:OnePerLine
                END
GenFmt_Simple_Defaults  LIKE(GenFmt_Simple)
GenSim_Format   STRING(2000)
GenSim_FIELDS   STRING(2000)

GenFmt  CLASS
SimpleGen        PROCEDURE()
SimplePreviewBtn PROCEDURE()
SimpleParseBtn   PROCEDURE()
SimpleLoadConfig PROCEDURE()
ConfigGetPut     PROCEDURE(BYTE Get1_Put2, STRING CfgSection, *GROUP ConfigGrp)
CopyFormatBtn    PROCEDURE(LONG FeqFormatText, LONG FeqFieldsText, BOOL ReturnCode=0),STRING,PROC
CopyWindowAndListBtn  PROCEDURE(LONG FeqFmt, LONG FeqFields)   !01/05/22 Generate WINDOW + LIST + Format
CopyFieldsEqualBtn    PROCEDURE()                              !01/05/22 Generate Queue.Field= for all fields
PictureAccepted  PROCEDURE(LONG FEQ, STRING DorT)
PicturePopup     PROCEDURE(LONG FEQ, STRING DorT)

QueueTextParse   PROCEDURE()    !Parse GenQue_TextQ TEXT into GQFieldsQ Queue
QueueAutoGenerate PROCEDURE()   !Generate if GenQue:AutoGenerate checked and have RECORDS GQFieldsQ
QueueGenFormat   PROCEDURE()    !Build Format() using GenFmt_Queue and GQFieldsQ queue
QueuePreviewBtn  PROCEDURE()
QueueParseBtn    PROCEDURE()
QueueLoadConfig  PROCEDURE()
BangPictureBtn   PROCEDURE()
List:GQFieldsQ_TakeEvent    PROCEDURE()
        END
!EndRegion -- Data for Generate Format Simple
!Region -- Data for Generate Queue to Format 
GenFmt_Queue   GROUP,PRE(GenQue)
WidthMin            USHORT(20)              !GenQue:WidthMin
WidthMax            USHORT(200)             !GenQue:WidthMax
JustLCR             STRING('L')             !GenQue:JustLCR
Indent              BYTE(2)                 !GenQue:Indent          ()
Pic_Date            STRING('d17 {6}')       !GenQue:Pic_Date
Pic_Time            STRING('t7  {6}')       !GenQue:Pic_Time
Pic_Int_Minus       BYTE(1)                 !GenQue:Pic_Int_Minus   Long Short have Sign
Pic_Int_Commas      BYTE(1)                 !GenQue:Pic_Int_Commas
Pic_Int_Blank       STRING(1)               !GenQue:Pic_Int_Blank
Pic_Dec_Minus       BYTE(1)                 !GenQue:Pic_Dec_Minus   Decimals have Sign
Pic_Dec_Commas      BYTE(1)                 !GenQue:Pic_Dec_Commas  Decimals have commas
Pic_Dec_Blank       STRING(1)               !GenQue:Pic_Dec_Blank
Digits_BYTE         BYTE(3)                 !GenQue:Digits_BYTE
Digits_SHORT        BYTE(5)                 !GenQue:Digits_SHORT
Digits_LONG         BYTE(10)                !GenQue:Digits_LONG
Digits_BOOL         BYTE(1)                 !GenQue:Digits_BOOL
LongLook4DateTime   BYTE(1)                 !GenQue:LongLook4DateTime
RightBorder         BYTE(1)                 !GenQue:RightBorder     |
Underline           BYTE                    !GenQue:Underline       _
Fixed               BYTE                    !GenQue:Fixed           F
Resize              BYTE(1)                 !GenQue:Resize          M
Colored             BYTE                    !GenQue:Colored         *
CellStyle           BYTE                    !GenQue:CellStyle       Y
FieldNumbered       BYTE                    !GenQue:FieldNumbered   # #
HeaderRow           BYTE(1)                 !GenQue:HeaderRow
HdrJustLCR          STRING('L')             !GenQue:HdrJustLCR
HdrIndent           BYTE(2)                 !GenQue:HdrIndent    ()
HdrCenterDataRight  BYTE(1)                 !GenQue:HdrCenterDataRight  if Data is Right then Center Heading
AutoGenerate        BYTE(1)                 !GenQue:AutoGenerate
OnePerLine          BYTE(1)                 !GenQue:OnePerLine
SelectTabAtOpen     BYTE                    !GenQue:SelectTabAtOpen
                END
GenFmt_Queue_Defaults  LIKE(GenFmt_Queue)
GenQue_Format   STRING(2000)
GenQue_FIELDS   STRING(2000)
GenQue_Width    LONG          !01/05/22 sum of Column Widths to size LIST AT() for CopyWindowAndListBtn

GenQue_TextQ    STRING(5000)    !Queue Text control
GenQue_IsFILE   BOOL            !Was QUEUE a FILE?
GenQue_Name     PSTRING(65)
GenQue_Pre      PSTRING(65)    !Pre: or Queue.

GQFieldsQ   QUEUE,PRE(GQFldQ)
FieldNo         SHORT         !GQFldQ:FieldNo
Label           STRING(48)    !GQFldQ:Label
OmitHow         STRING(4)     !GQFldQ:OmitHow       !Case Sensitive  'OMIT' must, 'Omit' can be changed  N/A
Type            STRING(32)    !GQFldQ:Type
TypeNums        STRING(32)    !GQFldQ:TypeNums      e.g. STRING( nums )
BangPic         STRING(34)    !GQFldQ:BangPic       !@override picture
Picture         STRING(32)    !GQFldQ:Picture
CharsWide       STRING(5)     !GQFldQ:CharsWide
Bracket         STRING(32)    !GQFldQ:Bracket       !Group '[ '=Begin ' ]'=End  ' ]Header Text
TypeCode        STRING(48)    !GQFldQ:TypeCode
Pre_Label       STRING(48)    !GQFldQ:Pre_Label     !Label with PRE or Q.
Line            STRING(255)   !GQFldQ:Line
Debug           STRING(255)   !GQFldQ:Debug
            END
Bracket_Over  GROUP,PRE(),OVER(GQFldQ:Bracket)
GQFldQ:Bracket_1_  STRING(1)                        ![  Open
GQFldQ:Bracket_2_  STRING(1)                        ! ] Close
GQFldQ:BracketText STRING(30)                       !  ~Header Text~
              END
eOmit_Omit EQUATE('Omit')  !Case sensitive for GQFldQ:OmitHow
eOmit_Hide EQUATE('Hide')
eOmit_NA_  EQUATE('N/A')
!EndRegion -- Data for Generate Format

!Region -- Columns View on tab using ColumnzQ and Class new 04/05/24
ColumnzCls CLASS   !04/05/24 added new ColumnZ Queue and Tab so these methods create that Queue
ColumnzQ_AddFromExplainAndFormat  PROCEDURE(*SHORT inColz_LastColX, *SHORT inColz_LastFldX)  !Fill in ColumnzQ fields from Format
ColumnzQ_From_Format    PROCEDURE(STRING FmtSpec, STRING FmtTokn)  !Fill in ColumnzQ from Format
List:ColzQ_TakeEvent    PROCEDURE() !Handle Event for LIST like Right click
ColumnzQ_ToClipboard    PROCEDURE() !Copy ColumnzQ to Clip
FmtParse_WidthAlign     PROCEDURE(CONST *STRING FmtSpec, CONST *STRING FmtTokn, <*STRING OutWidth>, <*STRING OutAlignLRCD>),LONG,PROC !Return End Paren of ###L(##)
FmtParse_AlignOffset_Len PROCEDURE(CONST *STRING FmtTokn, LONG AlignPos, BOOL IsGroup=0),LONG !Returns Length of L(##) or ](##)
FmtParse_Between        PROCEDURE(CONST *STRING FmtSpec, CONST *STRING FmtTokn,STRING LeftToken, STRING RightToken, <*LONG OutLeftPos>, <*LONG OutRightPos>),STRING,PROC
FmtParse_NumberCnt      PROCEDURE(CONST *STRING FmtSpec, <*STRING OutNumberString>),LONG
FmtParse_GetModifiers   PROCEDURE(STRING inFmtSpec,*STRING outModifiers, *BYTE outXFieldCnt)
            END
!--
ColumnzQ QUEUE,PRE(ColzQ)
ColNo       STRING(6)   !1  ColzQ:ColNo      <--  String ## or Grp## a STRING see ColX below a Numeric SHORT
Level       LONG        !-     ColzQ:Level      Tree 1=Column or Group, 2=Column in Group
FieldNo     SHORT       !2  ColzQ:FieldNo       PROPLIST:FieldNo  *only* when <>ColNo so #Fld#
GroupNo     SHORT       !3  ColzQ:GroupNo       PL:GroupNo
Variable    STRING(96)  !4  ColzQ:Variable              #FIELD(var) Actually Queue
Header      STRING(32)  !5  ColzQ:Header        PL:Header
Picture     STRING(32)  !6  ColzQ:Picture       PL:Picture
Width       STRING(8)   !7  ColzQ:Width         PL:width
Align       STRING(5)   !8  ColzQ:Align         PL:Left (PL:LeftOffset) etc  L(2)
HeadAlign   STRING(5)   !9  ColzQ:HeadAlign     PL:HeaderLeft
Mods        STRING(16)  !10 ColzQ:Mods          Modifiers e.g. |FM_*
Mods_Tip    STRING(516) !--    ColzQ:Mods_Tip   Lookup in ModifierQ of help text
ModXFields  BYTE        !12 ColzQ:ModXFields    Extra Fields Required for Modifier 
ModXF_Tip   STRING(516) !--    ColzQ:ModXF_Tip  Lookup in ModExtraQ of Extra help
QFieldX     SHORT       !13 ColzQ:QFieldX       PROPLIST:FieldNo  *Always* even if =ColNo  Cannot do Here? 
ColX        SHORT       !14 ColzQ:ColX          <-- ColNo as a Numeric SHORT vs above ColNo a STRING
InGroup     SHORT       !15 ColzQ:InGroup       Group # the Column is in, =Zero if it is a group
FmtSource   STRING(256) !16 ColzQ:FmtSource     PL:Format for Column = FmtQ:FieldSpec
Level2      LONG        !-- ColzQ:Level2     <-- Tree Level for Format Column duplicates Level above
FmtString   STRING(256) !-- ColzQ:FmtString  <-- Tool TIP for Format =UnQuoted(FmtSource)
FmtTokn     STRING(256) !   ColzQ:FmtTokn   Format just the Tokens for parsing so ignore () inside ~Heading~ 
      END    
!EndRegion -- Columns View
                    
Window WINDOW('LIST FORMAT() 411 - Parse to Fields and Explain - FROM() Parser - Format Generator'),AT(,,505,360),GRAY,SYSTEM,MAX, |
            ICON('LFmtIcon.ico'),FONT('Segoe UI',8),RESIZE
        SHEET,AT(6,2),FULL,USE(?Sheet1),JOIN
            TAB(' L&IST Code... '),USE(?TabInput)
                PROMPT('L&IST Code:'),AT(6,26),USE(?ListControl:Prompt)
                BUTTON('P&aste Code<13,10>and Process'),AT(45,21,74,22),USE(?PasteBtn),SKIP, |
                        ICON(ICON:Paste),TIP('Paste Clipboard into List Code entry and Process'),LEFT
                BUTTON('&Process<13,10>LIST'),AT(127,21,58,22),USE(?ProcessBtn),ICON(ICON:VCRplay),LEFT
                TEXT,AT(7,49,,226),FULL,USE(ListControl),HVSCROLL,FONT('Consolas',10)
                BUTTON('&Close'),AT(193,21,42,22),USE(?CloseBtn),SKIP,STD(STD:Close)
                BUTTON('&ReRun'),AT(425,21,35,18),USE(?RunAgainBtn),SKIP,TIP('Run Another Instance')        
                CHECK('Debug Tabs'),AT(245,21),USE(DebugTabs),SKIP,TIP('Show debug Message() during ' & |
                        'process')
                CHECK('Dbg Messages'),AT(245,32),USE(DebugMsgs),SKIP,TIP('Show debug Message() durin' & |
                        'g process')
                CHECK('Explain Prefix with Field'),AT(315,21),USE(PrefixFieldInExplain),SKIP, |
                        TIP('In Explain comments insert Field at front of each line<13,10>Requires #' & |
                        'FIELDS() in LIST Code...Helpful!')
                CHECK('Explain Show Spaces'),AT(315,32),USE(ShowExplainSpaces),SKIP,TIP('Put + chara' & |
                        'cter in spaces so they can be seen to debug coding alignment')
                BUTTON('Load Test Code...'),AT(391,280),USE(?GetExpPickBtn),SKIP,TIP('Pick Text Exam' & |
                        'ple Code')
                PROMPT('Paste your LIST,FORMAT(),#FIELD() code above and press the Process List butt' & |
                        'on to see it parsed'),AT(43,283),USE(?Instrux)
                BUTTON,AT(6,279,18,18),USE(?CopyListBtn),SKIP,ICON(ICON:Copy),TIP('Copy List Code') 
                BUTTON('Fix <91h>Typesetter<92h> to Code <39>Quotes<39>'),AT(6,304,133,18), |
                        USE(?QuoteFixBtn),SKIP,TIP('Fix Typesetter Quotes <91h,92h,93h,94h> 91h,92h,' & |
                        '93h,94h <13,10>to be Code ''Single'' / "Double" Quotes 27h,22h<13,10><13>' & |
                        '<10>When code is pasted into ClarionHub <13,10>it has Quotes changed, this ' & |
                        'fixes that.<13,10>Markdown may remove other formating like * [ ]')
            END
            TAB(' &FORMAT Lines '),USE(?TabFormatLines),TIP('FORMAT() and #FIELDS() parsed to one co' & |
                    'lumn per line')
                PROMPT('FORMAT()<13,10>in Lines:'),AT(140,25,40,18),USE(?Fmt:InLines:Prompt),CENTER
                BUTTON('Copy Format'),AT(194,23,57,22),USE(?CopyLineFmtBtn),SKIP,ICON(ICON:Copy), |
                        TIP('Copy FORMAT() to Clipboard<13,10>Parsed one column per line as shown below'), |
                        LEFT
                BUTTON('Copy Format and Explain'),AT(257,23,74,22),USE(?CopyLineFmtPlusExplainBtn),SKIP, |
                        ICON(ICON:Copy),TIP('Copy FORMAT + Explain to Clipboard'),LEFT
                BUTTON('Preview<13,10>Format()'),AT(344,23,62,22),USE(?PreviewListBtn),SKIP, |
                        ICON(ICON:Zoom),TIP('Preview Format in a LIST on a Window'),LEFT
                BUTTON,AT(419,23,22,22),USE(?ModHelpBtn),SKIP,ICON(ICON:Help),TIP('LIST and Modifier Help')
                BUTTON('&ReRun'),AT(461,23,35,22),USE(?RunAgainFmtBtn),SKIP,TIP('Run Another Instance')
                TEXT,AT(139,52),FULL,USE(Fmt:InLines),SKIP,HVSCROLL,FONT('Consolas',9)
                PROMPT('Fields'),AT(8,27),USE(?PROMPT1)
                BUTTON('Copy Fields'),AT(39,23,51,22),USE(?CopyLineFieldsBtn),SKIP,ICON(ICON:Copy), |
                        TIP('Copy #Fields() to Clipboard<13,10>Parsed one field per line'),LEFT
                TEXT,AT(8,52,126),FULL,USE(Flds:InLines),HVSCROLL,FONT('Consolas',9)
            END
            TAB(' &Explain '),USE(?TabExplain),TIP('Format columns as comments with some explainatio' & |
                    'n of modifiers')
                PROMPT('Explain'),AT(7,23),USE(?Fmt:Explain:Prompt)
                BUTTON,AT(12,35,18,18),USE(?CopyExplainBtn),SKIP,ICON(ICON:Copy),TIP('Copy Format Ex' & |
                        'plain to Clipboard')
                BUTTON,AT(12,60,18,18),USE(?PreviewList2Btn),SKIP,ICON(ICON:Zoom),TIP('Preview LIST')
                BUTTON,AT(12,85,18,18),USE(?ModHelp2Btn),SKIP,ICON(ICON:Help),TIP('LIST and Modifier Help')
                TEXT,AT(39,26),FULL,USE(Fmt:Explain),HVSCROLL,FONT('Consolas',9)
            END
            TAB(' FR&OM '),USE(?TabFROM),TIP('LIST FROM()')
                PROMPT(' LIST with FROM(''String 1|String 2|#2'') is parsed into continuation Lines ' & |
                        'for view and edit. '),AT(6,22,,10),USE(?FromFYI),FONT('Consolas',9, |
                        COLOR:Black),COLOR(COLOR:White)
                BUTTON('FROM Help'),AT(449,20,52,14),USE(?CwHelpForFROM),SKIP,TIP('View Clarion Help on FROM')                        
                PROMPT('FROM( )'),AT(7,41),USE(?From:Format:Prompt)
                TEXT,AT(40,40,460,58),USE(From:From),VSCROLL,FONT('Consolas',10)
                BUTTON,AT(12,61,18,18),USE(?FromFromCopyBtn),SKIP,ICON(ICON:Copy),TIP('Copy FORM() t' & |
                        'o Clipboard')
                PROMPT('FROM( )<13,10>Lines'),AT(7,108,,30),USE(?From:InLines:Prompt)
                TEXT,AT(40,107,230),FULL,USE(From:InLines),HVSCROLL,FONT('Consolas',10)
                TEXT,AT(280,107),FULL,USE(From:CASE),HVSCROLL,FONT('Consolas',10)
                BUTTON,AT(12,142,18,18),USE(?FromInLinesCopyBtn),SKIP,ICON(ICON:Copy),TIP('Copy FORM' & |
                        '() in Lines to Clipboard')
                BUTTON('Align #'),AT(4,185,31,18),USE(?FromAlignValueBtn),SKIP,TIP('Align #Values')
                BUTTON('Align "'),AT(4,210,31,18),USE(?FromAlignQuoteBtn),SKIP,TIP('Undo Align #Values by Align Left Quotes')
                BUTTON('Split #'),AT(4,245,31,18),USE(?FromSplitValueBtn),SKIP,TIP('#Values Split into separate String and Aligned')
                BUTTON('&ReRun'),AT(4,290,31,18),USE(?RunAgainFromBtn),SKIP,TIP('Run Another Instance')
            END
            TAB('Columns'),USE(?TabColumns),TIP('List of Columns in the Format()')
                BUTTON,AT(8,23,22,20),USE(?CopyColumnzBtn),SKIP,ICON(ICON:Copy),TIP('Copy Columns to' & |
                        ' Clipboard')
                BUTTON('Preview<0Dh,0Ah>Format()'),AT(254,23,62,20),USE(?PreviewListBtn_Colz),SKIP, |
                        ICON(ICON:Zoom),TIP('Preview Format in a LIST on a Window'),LEFT
                BUTTON('Modifier<0Dh,0Ah>Help'),AT(354,23,62,20),USE(?ModHelpBtn_Colz),SKIP, |
                        ICON(ICON:Help),LEFT
                BUTTON('&ReRun'),AT(454,23,35,20),USE(?RunAgainBtn_Colz),SKIP            
                LIST,AT(8,49),FULL,USE(?LIST:ColumnzQ),VSCROLL,FONT(,9),HLP('x_noQtip'),FROM(ColumnzQ), |
                        FORMAT('40L(2)|FMT(B)~Column~C(0)@s6@18R(4)|FM~Fld~C(0)@n4b@15R(4)|FM~Gr' & |
                        '<0Dh,0Ah>No~C(0)@n3b@Q''PL_GroupNo''100L(2)|FM~#Field(Variable)~C(0)@s96@80' & |
                        'L(2)|FM~Header~C(0)@s32@Q''PL_Header''?37L(2)|FM~Picture~L(1)@s32@Z(6)Q''PL' & |
                        '_Picture''20L(2)|FM~Wid<0Dh,0Ah>th~C(0)@s8@Q''PL_Width''22L(2)|FM~Data<0Dh>' & |
                        '<0Ah>Algn~C(0)@s5@Q''PL_Left''22L(2)|FM~Hdr<0Dh,0Ah>Algn~C(0)@s5@Q''PL_Head' & |
                        'erLeft''28L(3)|FMP~Mods~C(0)@s16@Z(6)Q''Modifiers''14R(4)|FMP~Md<0Dh,0Ah>#~' & |
                        'C(0)@n1b@Q''Modifier Extra Queue Fields''19R(4)|M~Que<0Dh,0Ah>Fld~L(1)@n3b@' & |
                        'Q''PL_FieldNo ''15R(4)|M~Co<0Dh,0Ah>#~C(0)@n-4@Q''Column Number''15R(4)|M~G' & |
                        'r<0Dh,0Ah>#~C(0)@n-4b@Q''Column Group''80L(2)|FMPT(B)~Format String~@s255@Z' & |
                        '(6)Q''PL_Format''')            
            END
            TAB(' LIST Lines '),USE(?TabListLines),TIP('All LIST attributes parsed as one per line')
                BUTTON('Copy LIST'),AT(61,22,55,22),USE(?CopyListLineFmtBtn),SKIP,ICON(ICON:Copy), |
                        TIP('Copy LIST below to Clipboard'),LEFT
                PROMPT('LIST Parsed<13,10>into Lines:'),AT(8,24,47,18),USE(?ListParsed:Prompt)
                CHECK('Wrap lines'),AT(136,26),USE(ListParsedHScrollOff),TIP('Remove HScroll so line' & |
                        's wrap')
                BUTTON('Preview<13,10>Format()'),AT(344,23,62,22),USE(?PreviewList3Btn),SKIP, |
                        ICON(ICON:Zoom),TIP('Preview Format in a LIST on a Window'),LEFT
                BUTTON,AT(419,23,22,22),USE(?ModHelp3Btn),SKIP,ICON(ICON:Help),TIP('LIST and Modifier Help')
                TEXT,AT(8,52),FULL,USE(ListParsed),HVSCROLL,FONT('Consolas',9)
            END
            TAB(' History '),USE(?TabHistory),TIP('History of processed code to reload previous')
                LIST,AT(7,23),FULL,USE(?LIST:HistoryQ),VSCROLL,FONT('Consolas',9),FROM(HistoryQ), |
                        FORMAT('[66L(1)|FM~Time~@s5@/20R(1)|_FM~Date~@s5@](33)|F[20L(2)|FM~LIST ~@s2' & |
                        '55@/20L(2)|_FM~    FORMAT    (Double-Click on a Line to Reload)~@s255@]|F')
            END
            TAB(' LIST &Help '),USE(?TabHelp),TIP('List Format and Modifiers Help')
                TEXT,AT(10,22,440,20),USE(HelpSyntax),SKIP,FONT('Consolas',9),READONLY                        
                BUTTON('CW Help'),AT(458,22,42,20),USE(?CwHelpForList),SKIP,TIP('View Clarion Help on LIST')
                TEXT,AT(10,48,489,64),USE(HelpWidth),SKIP,FONT('Consolas',9),READONLY
                LIST,AT(10,118,310),FULL,USE(?List:ModifierQ),VSCROLL,FROM(ModifierQ),FORMAT('22L(3)' & |
                        '|M~Mod~C(0)@s4@Z(1)71L(3)|M~PROPLIST:~L(2)@s100@Z(2)29L(3)|M~Type~C(0)@s5@1' & |
                        '49L(3)~Modifier Description (click to sort)~L(2)@s60@'),ALRT(CtrlC), |
                         ALRT(CtrlShiftC)
                TEXT,AT(328,118,171,150),USE(ModQ:Desc),VSCROLL,FONT('Consolas',9),READONLY
                TEXT,AT(328,274,280),FULL,USE(HelpModOrder),SKIP,HVSCROLL,FONT('Consolas',9),READONLY
                LIST,AT(616,118,,203),FULL,USE(?LIST:ModifierQDbg),HIDE,VSCROLL,TIP('Debug ModifierQ' & |
                        ' and LookUp column used to add Help to ColumnzQ Modifier Tips'),FROM(ModifierQ), |
                        FORMAT('30L(2)|FM~Lookup~@s2@#8#19L(2)|FM~Char ~@s4@#1#70L(2)|FM~Prop STRING' & |
                        '(40)~@s40@25L(2)|M~Type ~@s5@134L(2)|M~Name STRING(60) ~@s60@90L(2)|M~PropF' & |
                        'ull STRING(60) ~@s60@147L(2)|M~Desc STRING(54) ~@s54@30L(2)|M~Sort 7~@s7@')
                LIST,AT(616,330,400,70),USE(?LIST:ModExtraQDbg),HIDE,VSCROLL,FONT('Consolas',10), |
                        TIP('Debug ModExtraQ used for ColumnzQ Modifier Extra'),FROM(ModExtraQ), |
                        FORMAT('29C(0)|M~Order~@s1@24C(0)|M~Char~@s1@28C(0)|M~XtraQ~@n1@200L(2)|M~De' & |
                        'sc~L(2)@s60@')
            END
            TAB('&Queue 2 Format'),USE(?TabGenQueue),TIP('Generate a Format from a Queue')
                PROMPT('QUEUE Declaration<13,10>or FILE(s)'),AT(10,206,66,20),USE(?GenQue_TextQ:Prompt)
                BUTTON('P&aste &&<13,10>Process'),AT(12,230,60,22),USE(?GenQue_PasteBtn),SKIP, |
                        ICON(ICON:Paste),TIP('Paste clipboard into Queue text and Process'),LEFT
                BUTTON('&Process<13,10>Queue'),AT(12,258,60,22),USE(?GenQue_ProcessBtn),ICON(ICON:VCRplay), |
                        TIP('Parse the Queue code to generate Format'),LEFT
                BUTTON('!@xx comment<13,10>forces Picture'),AT(12,318,60,26),USE(?GenQue_BangPicBtn),SKIP, |
                        TIP('Click for info message')
                TEXT,AT(86,206,220),FULL,USE(GenQue_TextQ),HVSCROLL,FONT('Consolas',9)
                PROMPT('#1 ====><13,10>Paste Q'),AT(22,290,41,20),USE(?Do1stHint),FONT(,9,COLOR:Red, |
                        FONT:bold),COLOR(COLOR:Aqua),CENTER
                PROMPT('#2 ==>'),AT(12,110,30,11),USE(?Do2ndHint),FONT(,9,COLOR:Red,FONT:bold), |
                        COLOR(COLOR:Aqua)
                ENTRY(@s64),AT(317,206,80),USE(GenQue_Name),SKIP,COLOR(COLOR:BTNFACE),TIP('Queue Name'), |
                        READONLY
                ENTRY(@s64),AT(402,206,66),USE(GenQue_Pre),SKIP,COLOR(COLOR:BTNFACE),TIP('For Q.Fiel' & |
                        'd dot syntax remove PRE()'),READONLY
                STRING('Right-Click on Fields List for Popup Options to Move or Remove Fields'), |
                        AT(480,208),USE(?GQFieldsFYI)
                LIST,AT(317,223),FULL,USE(?List:GQFieldsQ),VSCROLL,FONT('Consolas',9),FROM(GQFieldsQ), |
                        DRAGID('GQFieldsQ'),DROPID('GQFieldsQ'),FORMAT('19R(5)|M~No.~C(0)@n3@60L(2)|' & |
                        'M~Label~C(0)26L(2)|M~Omit~C(0)@s4@Q''Omit from List with !Omit<13,10>Hide a' & |
                        's Zero Width with !Hide''34L(2)|M~TYPE~C(0)34L(2)|M~(nums)~C(0)32L(2)|M~!@P' & |
                        'ic~C(0)Q'' !@picture comment with on lines overrides calculated ''41L(2)|M~' & |
                        'Picture~C(0)32R(2)|M~Chars~C(0)@s6@17L(2)|M~[G]~C(0)Q''Group [ or ]''54L(2)' & |
                        '|M~Type Code~60L(2)|M~Pre_Label~60L(2)|M~Code Line~60L(2)|M~Debug~'), |
                        ALRT(CtrlUp), ALRT(CtrlDown), ALRT(DeleteKey), ALRT(CtrlDelete), |
                         ALRT(InsertKey), ALRT(CtrlHome), ALRT(CtrlEnd)
                PANEL,AT(10,201,,2),FULL,USE(?PanelAboveFmtQue),BEVEL(0,0,0600H)
                GROUP('List Format Preferences'),AT(14,20,333,82),USE(?GemQue_Group),BOXED
                    GROUP,AT(17,30,92,64),USE(?GQUprLeftGROUP)
                        PROMPT('Width'),AT(19,32),USE(?GenQue:WidthMin:Pmt)
                        ENTRY(@n3),AT(49,32,20,10),USE(GenQue:WidthMin),TIP('MINimum Width')
                        PROMPT(' - '),AT(72,32),USE(?GenQue:WidthMax:Pmt)
                        ENTRY(@n3),AT(81,32,20,10),USE(GenQue:WidthMax),TIP('MAXimum Width')
                        PROMPT('Date *'),AT(19,45),USE(?GenQue:Pic_Date:Pmt)
                        BUTTON('@'),AT(92,45,12,10),USE(?GenQue:Pic_Date:Popup),SKIP,FONT(,8), |
                                TIP('Date Picker')
                        ENTRY(@s9),AT(49,45,38,10),USE(GenQue:Pic_Date),TIP('Date Picture for DATE t' & |
                                'ype<13,10>and LONG contains "Date"')
                        PROMPT('Time *'),AT(19,57),USE(?GenQue:Pic_Time:Pmt)
                        BUTTON('@'),AT(92,57,12,10),USE(?GenQue:Pic_Time:Popup),SKIP,FONT(,8), |
                                TIP('Time Picker')
                        ENTRY(@s9),AT(49,57,38,10),USE(GenQue:Pic_Time),TIP('Time Picture for TIME t' & |
                                'ype<13,10>and LONG contains "Time"')
                        PROMPT('Integer'),AT(19,69),USE(?GenQue:Pic_Int:Pmt)
                        CHECK('-'),AT(48,69),USE(GenQue:Pic_Int_Minus),FONT('Consolas'),TIP('Integer' & |
                                's Pictures include Minus @N-')
                        CHECK(','),AT(70,69),USE(GenQue:Pic_Int_Commas),FONT('Consolas'),TIP('Intege' & |
                                'r Pictures include Commas uncheck for @N_')
                        CHECK('B'),AT(92,69),USE(GenQue:Pic_Int_Blank),VALUE('b',''),TIP('Integer Bl' & |
                                'ank when Zero @N#b')
                        PROMPT('Decmal'),AT(19,79),USE(?GenQue:Pic_Dec:Pmt)
                        CHECK('-'),AT(48,79),USE(GenQue:Pic_Dec_Minus),FONT('Consolas'),TIP('Decimal' & |
                                ' Pictures include Minus @N-')
                        CHECK(','),AT(70,79),USE(GenQue:Pic_Dec_Commas),FONT('Consolas'),TIP('Decima' & |
                                'l Pictures include Commas uncheck for @N_')
                        CHECK('B'),AT(92,79),USE(GenQue:Pic_Dec_Blank),VALUE('b',''),TIP('Decimal Bl' & |
                                'ank when Zero @N#b')
                    END
                    CHECK('* LONG find "Date" "Time" uses @d @t'),AT(19,90),USE(GenQue:LongLook4DateTime), |
                            TIP('If LONG Field Label contains "Date" or "Time"<13,10>use @d / @t pictures')
                    PANEL,AT(113,28,1,60),USE(?PanelLeftOfWidth),BEVEL(0,0,6000H)
                    PANEL,AT(166,28,1,68),USE(?PanelRightOfWidth),BEVEL(0,0,6000H)
                    PANEL,AT(266,28,1,68),USE(?PanelRightOfHeader),BEVEL(0,0,6000H)
                    GROUP,AT(118,28,43,60),USE(?GQDigitsGROUP)
                        PROMPT('Max Digits'),AT(120,28),USE(?Digits:Lit)
                        PROMPT('BYTE'),AT(137,40),USE(?GenQue:DigByte:Pmt)
                        ENTRY(@n2),AT(120,40,14,10),USE(GenQue:Digits_BYTE),RIGHT
                        PROMPT('SHORT'),AT(137,53),USE(?GenQue:DigSHORT:Pmt)
                        ENTRY(@n2),AT(120,53,14,10),USE(GenQue:Digits_SHORT),RIGHT,TIP('Max digits 5' & |
                                ' -- SHORT USHORT')
                        PROMPT('LONG'),AT(137,66),USE(?GenQue:DigLONG:Pmt)
                        ENTRY(@n2),AT(120,66,14,10),USE(GenQue:Digits_LONG),RIGHT,TIP('Max digits 10' & |
                                ' -- LONG ULONG SIGNED UNSIGNED COUNT_T POINTER_T')
                        PROMPT('BOOL'),AT(137,79),USE(?GenQue:DigBOOL:Pmt)
                        ENTRY(@n2),AT(120,79,14,10),USE(GenQue:Digits_BOOL),RIGHT
                    END
                    GROUP,AT(173,31,85,68),USE(?GQHeaderGROUP)
                        OPTION('Header Justification'),AT(175,31,83,45),USE(GenQue:HdrJustLCR),BOXED
                            RADIO('Left'),AT(180,42),USE(?GenQue:HdrJustLCR:Left),VALUE('L')
                            RADIO('Center'),AT(180,52),USE(?GenQue:HdrJustLCR:Center),VALUE('C')
                            RADIO('Right'),AT(180,62),USE(?GenQue:HdrJustLCR:Right),VALUE('R')
                        END
                        PROMPT('Indent'),AT(223,44),USE(?GenQue:HdrIndent:Pmt)
                        ENTRY(@n2),AT(223,55,21,11),USE(GenQue:HdrIndent),TIP('Center ignores Indent')
                        CHECK('Center Header'),AT(182,77),USE(GenQue:HdrCenterDataRight),TRN, |
                                TIP('If Data is Right Justified, e.g. Numbers and Dates,<13,10>then ' & |
                                'Center the Heading  Text')
                        PROMPT('for Right Data'),AT(195,86),USE(?GenQue:HdrCenterDataRight:2),TRN
                    END
                    CHECK('Resizable M'),AT(275,27),USE(GenQue:Resize)
                    CHECK('Right Line |'),AT(275,37),USE(GenQue:RightBorder)
                    CHECK('Underline _'),AT(275,47),USE(GenQue:Underline)
                    CHECK('Fixed F'),AT(275,57),USE(GenQue:Fixed),TIP('Fixed No H Scroll Column')
                    CHECK('Colored *'),AT(275,69),USE(GenQue:Colored),TIP('Color All Columns require' & |
                            's 4 x LONG in Queue')
                    CHECK('Style Cell Y'),AT(275,79),USE(GenQue:CellStyle),TIP('Cell Style for requi' & |
                            'res 1 x LONG in Queue<13,10>Column Style Z(#) is only specified in Format.')
                    CHECK('Field No. #'),AT(275,89),USE(GenQue:FieldNumbered),TIP('Add # Field No # ' & |
                            'to all columns')
                    CHECK('1 Column / Line'),AT(279,116,68),USE(GenQue:OnePerLine),SKIP,LEFT, |
                            TIP('One Column per Line is much easier to read')
                    CHECK('Auto Generate'),AT(279,105,68),USE(GenQue:AutoGenerate),LEFT,TIP('Generat' & |
                            'e Format on any chnage to format specs')
                END
                BUTTON('Save as<13,10>Default'),AT(356,25,48,22),USE(?GenQueueDefaultSaveBtn),SKIP, |
                        TIP('Save Config')
                BUTTON('Load<13,10>Defaults'),AT(356,50,48,22),USE(?GenQueueDefaultLoadBtn),SKIP, |
                        TIP('Load Saved Config')
                BUTTON('Clear<13,10>Settings'),AT(356,76,48,22),USE(?GenQueueClearBtn),SKIP, |
                        TIP('Clear to Program Defaults')
                BUTTON(' &Generate<13,10> Format'),AT(52,105,66,23),USE(?GenQueueFormatBtn), |
                        ICON('LFmtIcon.ico'),TIP('Generate format using above parameters'),LEFT
                BUTTON('Pre&view<13,10>Format'),AT(125,105,66,23),USE(?GenQueuePreviewBtn),ICON(ICON:Zoom), |
                        TIP('Preview Format in a LIST on a Window'),LEFT
                BUTTON('Pa&rse<13,10>Format'),AT(197,105,66,23),USE(?GenQueueParseBtn),ICON(ICON:VCRplay), |
                        TIP('Put Format into "LIST Code" tab then parse Columns to "FORMAT Lines" tab'), |
                        LEFT
                STRING('FORMAT'),AT(349,144,14,45),USE(?FormatLitQ),TRN,FONT(,12),ANGLE(2700)
                TEXT,AT(38,132,308,63),USE(GenQue_Format),SKIP,VSCROLL,FONT('Consolas',9)
                STRING('FIELDS'),AT(373,115),USE(?FieldsLitQ),TRN,FONT(,12)
                TEXT,AT(372,132,308,63),USE(GenQue_FIELDS),SKIP,VSCROLL,FONT('Consolas',9)
                BUTTON,AT(12,132,18,18),USE(?GenQueueCopyFormatBtn),SKIP,ICON(ICON:Copy),TIP('Copy F' & |
                        'ORMAT() to Clipboard')
                BUTTON,AT(12,155,18,18),USE(?ModHelp4Btn),SKIP,ICON(ICON:Help),TIP('LIST and Modifier Help')                        
                BUTTON,AT(12,178,18,18),USE(?GenQueueCopyField2Btn),SKIP,ICON(ICON:Copy),TIP('Copy F' & |
                        'ORMAT()<13,10>and #FIELDS() to Clipboard')
                BUTTON,AT(412,110,18,18),USE(?GenQueueCopyFieldsBtn),SKIP,ICON(ICON:Copy), |
                        TIP('Copy #FIELDS() to Clipboard')
                BUTTON('+Window+List'),AT(438,110,,18),USE(?GenQueueCopyWindowBtn),SKIP,TIP('Generate WINDOW Code with LIST and FORMAT') 
                BUTTON('Q Fields ='),AT(510,110,,18),USE(?GenQueueFieldsEqualBtn),SKIP,TIP('Generate code with Queue Field= for all fields') 
                PROMPT('What''s This:<13,10>Creating a new List Format for a Queue can be tedious ad' & |
                        'ding each column. This generates a FORMAT for your QUEUE definition you pas' & |
                        'te into the Text control on the lower left.'),AT(420,20,110,86),USE(?GenQueueWhatsThis) |
                        ,FONT(,10)
                BUTTON('&ReRun'),AT(550,27,35,18),USE(?RunAgainGFQBtn),SKIP,TIP('Run Another Instance')
                CHECK('Select This Tab at Open'),AT(550,77),USE(GenQue:SelectTabAtOpen)
            END
            TAB('Simple Format Gen'),USE(?TabGenSimple),TIP('Generate a Simple Format with all colum' & |
                    'ns the same')
                GROUP('List Format Specifications for All Columns'),AT(14,22,289,110),USE(?GemSim_Group), |
                        BOXED
                    PROMPT('&Width'),AT(21,40),USE(?GenSim:Width:Pmt)
                    ENTRY(@n3),AT(51,40,25,10),USE(GenSim:Width)
                    PROMPT('Picture'),AT(21,57),USE(?GenSim:Picture:Pmt)
                    COMBO(@s12),AT(51,56,49,11),USE(GenSim:Picture),VSCROLL,TIP('Can be Blank or Non' & |
                            'e which allows over 255 bytes'),DROP(9),FROM(' |s40|s80|s99|s255|n-9|n-' & |
                            '11.2|None'),FORMAT('1L(2)')
                    OPTION('Data Justification'),AT(109,33,91,42),USE(GenSim:JustLCR),BOXED
                        RADIO('Left'),AT(114,43),USE(?GenSim:JustLCR:Left),VALUE('L')
                        RADIO('Center'),AT(114,53),USE(?GenSim:JustLCR:Center),VALUE('C')
                        RADIO('Right'),AT(114,63),USE(?GenSim:JustLCR:Right),VALUE('R')
                    END
                    PROMPT('Indent'),AT(161,46),USE(?GenSim:Indent:Pmt)
                    ENTRY(@n-3),AT(162,56,,11),USE(GenSim:Indent),TIP('Center ignores Indent')
                    PROMPT('Header Text'),AT(21,91),USE(?GenSim:HeaderText:Pmt)
                    ENTRY(@s20),AT(22,102,78,11),USE(GenSim:HeaderText)
                    OPTION('Header Justification'),AT(109,79,91,45),USE(GenSim:HdrJustLCR),BOXED
                        RADIO('Left'),AT(114,89),USE(?GenSim:HdrJustLCR:Left),VALUE('L')
                        RADIO('Center'),AT(114,99),USE(?GenSim:HdrJustLCR:Center),VALUE('C')
                        RADIO('Right'),AT(114,109),USE(?GenSim:HdrJustLCR:Right),VALUE('R')
                    END
                    PROMPT('Indent'),AT(161,91),USE(?GenSim:HdrIndent:Pmt)
                    ENTRY(@n-3),AT(162,102,,11),USE(GenSim:HdrIndent),TIP('Center ignores Indent')
                    CHECK('Resizable M'),AT(218,35),USE(GenSim:Resize)
                    CHECK('Right Border |'),AT(218,45),USE(GenSim:RightBorder)
                    CHECK('Underline _'),AT(218,55),USE(GenSim:Underline)
                    CHECK('Fixed (No Scroll) F'),AT(218,65),USE(GenSim:Fixed)
                    CHECK('Colored *'),AT(218,80),USE(GenSim:Colored),TIP('Color All Columns require' & |
                            's 4 x LONG')
                    CHECK('Style Cell Y'),AT(218,90),USE(GenSim:CellStyle),TIP('Style for All Column' & |
                            's requires 1 LONG in Queue<13,10>Column Style Z(#) is only in Format.')
                    CHECK('Field Number #'),AT(218,101),USE(GenSim:FieldNumbered),TIP('Add # Field N' & |
                            'o # to all columns')
                    CHECK('1 Column / Line'),AT(283,164,68),USE(GenSim:OnePerLine),SKIP,LEFT, |
                            TIP('One Column Per Line')
                    CHECK('Auto Generate'),AT(218,118),USE(GenSim:AutoGenerate),TIP('Generate Format' & |
                            ' on any chnage to format specs')
                END
                BUTTON('Save as<13,10>Default'),AT(315,41,48,22),USE(?GenSimpleDefaultSaveBtn),SKIP, |
                        TIP('Save Config')
                BUTTON('Load<13,10>Defaults'),AT(315,70,48,22),USE(?GenSimpleDefaultLoadBtn),SKIP, |
                        TIP('Load Saved Config')
                BUTTON('Clear<13,10>Settings'),AT(315,100,48,22),USE(?GenSimpleClearBtn),SKIP, |
                        TIP('Clear to Program Defaults')
                PROMPT('Columns'),AT(14,140),USE(?GenSim:Columns:Pmt)
                ENTRY(@n2),AT(15,152,,11),USE(GenSim:Columns)
                BUTTON(' &Generate<13,10> Format'),AT(56,140,70,23),USE(?GenSimpleFormatBtn), |
                        ICON('LFmtIcon.ico'),TIP('Generate format using above parameters'),LEFT
                BUTTON('Pre&view<13,10>Format'),AT(136,140,66,23),USE(?GenSimplePreviewBtn),SKIP, |
                        ICON(ICON:Zoom),TIP('Preview Format in a LIST on a Window'),LEFT
                BUTTON('&Parse<13,10>Format'),AT(212,140,64,23),USE(?GenSimpleParseBtn),ICON(ICON:VCRplay), |
                        TIP('Put Format into "LIST Code" tab then parse Columns to "FORMAT Lines" tab'), |
                        LEFT
                STRING('FORMAT'),AT(355,215,14,45),USE(?FormatLit),FONT(,12),ANGLE(2700)
                TEXT,AT(42,178,308,119),USE(GenSim_Format),SKIP,VSCROLL,FONT('Consolas',9)
                STRING('FIELDS'),AT(380,160),USE(?FieldsLit),FONT(,12)
                TEXT,AT(381,178,308,119),USE(GenSim_FIELDS),SKIP,VSCROLL,FONT('Consolas',9)
                BUTTON,AT(12,178,18,18),USE(?GenSimpleCopyFormatBtn),SKIP,ICON(ICON:Copy), |
                        TIP('Copy Format to Clipboard') 
                BUTTON,AT(12,226,18,18),USE(?ModHelp5Btn),SKIP,ICON(ICON:Help),TIP('LIST and Modifier Help')
                BUTTON,AT(12,279,18,18),USE(?GenSimpleCopyFieldsBtn),SKIP,ICON(ICON:Copy), |
                        TIP('Copy Format and #Fields() to Clipboard')
                PROMPT('"Simple Format" was <0DH,0AH>my first idea ...but...<0DH,0AH>"Queue 2 Format' & |
                        '" is <0DH,0AH>the tool to try first.'),AT(380,27,85,45),USE(?GenSimpleSeeQue2Fmt), |
                        FONT(,10,COLOR:Blue),COLOR(COLOR:White)
                PROMPT('What''s This:<13,10>Creating a new List Format for a Queue can be tedious ad' & |
                        'ding each column. This generates a FORMAT for the specified number of colum' & |
                        'ns that are all the same. Then just edit for changes.'),AT(380,79,133,76), |
                        USE(?GenSimpleWhatsThis),FONT(,10)
            END
            TAB(' Flat '),USE(?TabFlat),TIP('LIST code flattened to one line')
                BUTTON('Copy Flat'),AT(58,22,,14),USE(?CopyListFlatBtn),SKIP,TIP('Copy Flat Code to ' & |
                        'Clipboard')
                PROMPT('Flat Code:'),AT(9,25),USE(?ListFlat:Prompt)
                TEXT,AT(9,40),FULL,USE(ListFlat),VSCROLL,FONT('Consolas',10)
            END
            TAB(' Parsed '),USE(?TabParsed)
                PROMPT('Format'),AT(7,22),USE(?Fmt:Format:Prompt)
                TEXT,AT(40,24,,90),FULL,USE(Fmt:Format),VSCROLL,FONT('Consolas',10),TIP('Fmt:Format')
                PROMPT('Format<13,10>Tokens<13,10>Only'),AT(7,118,,30),USE(?Fmt:TokFmt:Prompt)
                TEXT,AT(40,119,,90),FULL,USE(Fmt:TokFmt),VSCROLL,FONT('Consolas',10),TIP('Fmt:TokFmt')
                PROMPT('#Fields'),AT(7,214),USE(?Flds:FieldsFlat:Prompt)
                TEXT,AT(40,215,,90),FULL,USE(Flds:FieldsFlat),VSCROLL,FONT('Consolas',10), |
                        TIP('Flds:FieldsFlat')
                STRING('Above are Strings parsed from the original input used to extract the data in' & |
                        'to Queues. Below are the length and size to help decide if they need to be ' & |
                        'longer.'),AT(9,309),USE(?WtfIsParse)
                STRING('Lengths'),AT(9,324),USE(?Lengths:Prompt)
                TEXT,AT(40,322),FULL,USE(LengthsText),VSCROLL,FONT('Consolas',10)
            END
            TAB('FormatQ'),USE(?TabFormatQ),TIP('Debug Format() Parse')
                TEXT,AT(8,20,,55),FULL,USE(Fmt:Format,, ?Fmt:Format:2),VSCROLL,FONT('Consolas',10), |
                        TIP('Fmt:Format')
                LIST,AT(8,82),FULL,USE(?LIST:FormatQ),VSCROLL,FONT('Consolas',9),FROM(FormatQ), |
                        FORMAT('28L(2)|M~Beg<0Dh,0Ah>Pos~C(0)@n4@28L(2)|M~End<0Dh,0Ah>Pos~C(0)@n4@26' & |
                        'L(2)|M~Fmt<0Dh,0Ah>Len~C(0)@n-4@24L(2)|M~Col<0Dh,0Ah>No.~C(0)@n2b@30L(2)|M~' & |
                        'Grp<0Dh,0Ah>No.~C(0)@n-3b@30L(2)|M~In<0Dh,0Ah>Grp~C(0)@n-3b@200L(2)|M~Forma' & |
                        't Source~@s255@200L(2)|M~Format Tokens~@s255@')
            END
            TAB('FieldQ'),USE(?TabFieldsQ),TIP('Debug #Fields() Parse')
                TEXT,AT(8,26,,40),FULL,USE(Flds:FieldsFlat,, ?Flds:FieldsFlat:2),VSCROLL, |
                        FONT('Consolas',9),TIP('Flds:FieldsFlat')
                LIST,AT(8,74),FULL,USE(?LIST:FieldsQ),VSCROLL,FONT(,9),FROM(FieldsQ),FORMAT('200L(2)' & |
                        '~#Fields~@s96@')
            END
            TAB('ExplainQ'),USE(?TabExplainQ),TIP('Debug Explain Tab')
                STRING('Max Lengths:'),AT(50,26),USE(?ExplainMAXstring)
                LIST,AT(6,40),FULL,USE(?LIST:ExplainQ),VSCROLL,FONT(,9),FROM(ExplainQ), |
                        FORMAT('20R(2)|M~Len~C(0)@n-3@101L(2)|M~Format Txt~L(1)@s255@20R(2)|M~Len ~C' & |
                        '(0)@n-3b@27L(2)|M~GrpNo~L(1)@s8@12C|M~IG~@s1@Q''In Group is Colon''20R(2)|M' & |
                        '~Len~C(0)@n-3b@27L(2)|M~Col No~L(1)@s8@20R(2)|M~Len~C(0)@n-3b@97L(2)|M~Modi' & |
                        'fiers~L(1)@s64@20R(2)|M~Len~C(0)@n-3b@80L(2)|M~#Field~L(1)@s64@')
            END
        END
    END
