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
                    
