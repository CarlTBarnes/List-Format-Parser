!---- Data and WINDOW for List Format Parser ----   
!Region -- Data for List Parse
Ndx             LONG     
FlattenCls      CBCodeFlattenClass     
ParserCls       CBCodeParseClass     
ListControl     STRING(6000)
ListFlat        STRING(6000)
ListParsed      STRING(9000)  !List Lines tab with Attribs and Format one per line
DebugTabs        BYTE
DebugMsgs        BYTE
                                                             
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
Fmt:InLines STRING(4000)     !Format() one per line in my token format
Fmt:Explain STRING(9000)     !Comments about the Format
        END

FieldsGrp  GROUP,PRE()    !Pre(Flds)
Flds:Found   BOOL
Flds:Records LONG
Flds:BegPos  LONG 
Flds:Paren1  LONG 
Flds:Paren2  LONG  
Flds:FieldsFlat STRING(2000)  !Flat without returns
Flds:InLines    STRING(2000)  !For TEXT aligned with Fmt:InLines
Flds:FieldsCode CSTRING(3000) !In Lines with Pipes
        END

FormatQ     QUEUE,PRE(FmtQ)     !         FORMAT( parsed into Fields
Pos1            USHORT          !FmtQ:Pos1         
Pos2            USHORT          !FmtQ:Pos2     
LenSpec         SHORT           !FmtQ:LenSpec     
FldNo           SHORT           !FmtQ:FldNo    
GrpNo           SHORT           !FmtQ:GrpNo    
FieldSpec       STRING(256)     !FmtQ:FieldSpec
TokenSpec       STRING(256)     !FmtQ:TokenSpec
            END
FieldsQ     QUEUE,PRE(FldsQ) 
Name            STRING(96)      !FldsQ:Name
            END            
Format2QCls CLASS
Parse2Q         PROCEDURE(CONST *STRING Fmt, *STRING OutTokFmt) 
Format2Token    PROCEDURE(CONST *STRING Fmt, *STRING OutTokFmt, BYTE BlankNoise=1) 
ParseSegment    PROCEDURE(CONST *STRING Fmt, CONST *STRING TokFmt, Long SegPosBeg, Long SegPosEnd, *SHORT NxtFldNo, SHORT ThisGrpNo )  
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
Add1Q       PROCEDURE(STRING _Char, STRING _Prop, STRING _Name, STRING _Desc) 
Set1QDesc   PROCEDURE(STRING _Char, STRING _Desc) 
        END
HelpSyntax      STRING(2000)        
HelpModOrder    STRING(1600)
        
ModifierQ   QUEUE,PRE(ModQ)     !LIST Modifiers 
Char            STRING(4)       !ModQ:Char
Prop            STRING(100)     !ModQ:Prop      !w/o PROPLIST:
Name            STRING(60)      !ModQ:Name
PropFull        STRING(60)      !ModQ:PropFull  !with PROPLIST:
Desc            STRING(1024)    !ModQ:Desc  
Sort            STRING(7)       !ModQ:Sort = lower(Char) + Rec# so Unique Key
            END 
            
PrefixFieldInExplain  SHORT(1)
ShowExplainSpaces     SHORT(0)
ListParsedHScrollOff  SHORT     !AKA Wrap lines
!EndRegion -- Data for List Parse
!Region -- Data for Generate Format
GenFmt_Simple   GROUP,PRE(GenSim)
Columns             BYTE(9)                 !GenSim:Columns
Width               USHORT(80)              !GenSim:Width
JustLCR             STRING('L')             !GenSim:JustLCR
Indent              SHORT(2)                !GenSim:Indent          ()
Picture             STRING(16)              !GenSim:Picture         @ @  
RightBorder         BYTE(1)                 !GenSim:RightBorder     |
Underline           BYTE                    !GenSim:Underline       _
Fixed               BYTE                    !GenSim:Fixed           F
Resize              BYTE(1)                 !GenSim:Resize          M
Colored             BYTE                    !GenSim:Colored         *
StyleY              BYTE                    !GenSim:StyleY          Y
FieldNo             BYTE                    !GenSim:FieldNo         # #
HeaderRow           BYTE(1)                 !GenSim:HeaderRow    
HeaderText          STRING('Column {16}')   !GenSim:HeaderText   ~ ~
HdrJustLCR          STRING('L')             !GenSim:HdrJustLCR
HdrIndent           SHORT(2)                !GenSim:HdrIndent    ()
AutoGenerate        BYTE(1)                 !GenSim:AutoGenerate
OnePerLine          BYTE                    !GenSim:OnePerLine
                END
GenFmt_Simple_Defaults  LIKE(GenFmt_Simple)                
GenSim_Format   STRING(2000)                
GenSim_FIELDS   STRING(2000) 

GenFmt  CLASS
SimpleGen        PROCEDURE()
SimplePreviewBtn PROCEDURE()
SimpleParseBtn   PROCEDURE()
SimpleCopyBtn    PROCEDURE(BYTE CopyType)
SimpleLoadConfig PROCEDURE()
ConfigGetPut     PROCEDURE(BYTE Get1_Put2, STRING CfgSection, *GROUP ConfigGrp)
        END
!EndRegion -- Data for Generate Format 

Window WINDOW('LIST FORMAT() - Parse to Fields and Explainer'),AT(,,470,360),GRAY,SYSTEM,MAX, |
            ICON('LFmtIcon.ico'),FONT('Segoe UI',8),RESIZE
        SHEET,AT(6,2),FULL,USE(?Sheet1),JOIN
            TAB(' L&IST Code... '),USE(?TabInput)
                PROMPT('L&IST Code:'),AT(6,26),USE(?ListControl:Prompt)
                BUTTON('P&aste Code<13,10>and Process'),AT(45,21,74,22),USE(?PasteBtn),SKIP, |
                        ICON(ICON:Paste),TIP('Paste Clipboard into List Code entry and Process'),LEFT
                BUTTON('&Process<13,10>LIST'),AT(127,21,58,22),USE(?ProcessBtn),ICON(ICON:VCRplay),LEFT
                TEXT,AT(7,49,,226),FULL,USE(ListControl),HVSCROLL,FONT('Consolas',9)
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
                BUTTON,AT(419,23,22,22),USE(?ModHelpBtn),SKIP,ICON(ICON:Help),TIP('Modifier Letter Help')
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
                BUTTON,AT(12,85,18,18),USE(?ModHelp2Btn),SKIP,ICON(ICON:Help),TIP('Modifier Letter Help')
                TEXT,AT(39,26),FULL,USE(Fmt:Explain),HVSCROLL,FONT('Consolas',9)
            END
            TAB(' &LIST Lines '),USE(?TabListLines),TIP('All LIST attributes parsed as one per line')
                BUTTON('Copy LIST'),AT(61,22,55,22),USE(?CopyListLineFmtBtn),SKIP,ICON(ICON:Copy), |
                        TIP('Copy LIST below to Clipboard'),LEFT
                PROMPT('LIST Parsed<13,10>into Lines:'),AT(8,24,47,18),USE(?ListParsed:Prompt)
                CHECK('Wrap lines'),AT(136,26),USE(ListParsedHScrollOff),TIP('Remove HScroll so line' & |
                        's wrap')
                BUTTON('Preview<13,10>Format()'),AT(344,23,62,22),USE(?PreviewList3Btn),SKIP, |
                        ICON(ICON:Zoom),TIP('Preview Format in a LIST on a Window'),LEFT
                BUTTON,AT(419,23,22,22),USE(?ModHelp3Btn),SKIP,ICON(ICON:Help),TIP('Modifier Letter Help')
                TEXT,AT(8,52),FULL,USE(ListParsed),HVSCROLL,FONT('Consolas',9)
            END
            TAB(' History '),USE(?TabHistory),TIP('History of processed code to reload previous')
                LIST,AT(7,23),FULL,USE(?LIST:HistoryQ),VSCROLL,FONT('Consolas',9),FROM(HistoryQ), |
                        FORMAT('[66L(1)|FM~Time~@s5@/20R(1)|_FM~Date~@s5@](33)|F[20L(2)|FM~LIST ~@s2' & |
                        '55@/20L(2)|_FM~    FORMAT    (Double-Click on a Line to Reload)~@s255@]|F')
            END
            TAB(' LIST &Help '),USE(?TabHelp),TIP('List Format and Modifiers Help')
                PROMPT('Group: [Cells] (GroupSize) Modifiers ~GroupHead~Justification(IndentHead)'), |
                        AT(9,22),USE(?HelpSyntaxGroup),FONT('Consolas',9)
                PROMPT('Cell:  Width Justification LRCD (Indent) Modifiers ~CellHead~ Justify(Indent' & |
                        'Head) @picture@'),AT(9,32),USE(?HelpSyntaxCell),FONT('Consolas',9)
                TEXT,AT(10,48,454,64),USE(HelpSyntax),SKIP,VSCROLL,FONT('Consolas',9),READONLY
                LIST,AT(10,118,275),FULL,USE(?List:ModifierQ),VSCROLL,FROM(ModifierQ),FORMAT('21L(2)' & |
                        '|M~Mod~C(0)@s4@Z(1)70L(2)|M~PROPLIST:~@s100@Z(2)149L(2)~Modifier Descriptio' & |
                        'n (click to sort)~@s60@'),ALRT(CtrlC), ALRT(CtrlShiftC)
                TEXT,AT(293,118,171,150),USE(ModQ:Desc),VSCROLL,FONT('Consolas',9),READONLY
                TEXT,AT(293,274),FULL,USE(HelpModOrder),SKIP,HVSCROLL,FONT('Consolas',9),READONLY
            END
            TAB(' Generate Format '),USE(?TabGenSimple),TIP('Generate a Simple Format')
                GROUP('List Format Specifications for All Columns'),AT(14,22,289,110),USE(?GemSim_Group), |
                        BOXED
                    PROMPT('&Width'),AT(21,40),USE(?GenSim:Width:Pmt)
                    ENTRY(@n3),AT(51,40,25,10),USE(GenSim:Width)
                    PROMPT('Picture'),AT(21,57),USE(?GenSim:Picture:Pmt)
                    COMBO(@s12),AT(51,56,49,11),USE(GenSim:Picture),VSCROLL,TIP('Can be blank'),DROP(9), |
                            FROM(' |s40|s80|s99|s255|n-9|n-11.2')
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
                    CHECK('Right Border'),AT(218,35),USE(GenSim:RightBorder)
                    CHECK('Resizable'),AT(218,45),USE(GenSim:Resize)
                    CHECK('Fixed (No Scroll)'),AT(218,55),USE(GenSim:Fixed)
                    CHECK('Underline'),AT(218,65),USE(GenSim:Underline)
                    CHECK('Colored *'),AT(218,80),USE(GenSim:Colored),TIP('Color All Columns require' & |
                            's 4 x LONG')
                    CHECK('Style (Cell)'),AT(218,90),USE(GenSim:StyleY),TIP('Style for All Columns r' & |
                            'equires 1 LONG in Queue ')
                    CHECK('Field Number'),AT(218,101),USE(GenSim:FieldNo),TIP('Add # Field No # to a' & |
                            'll columns')
                    CHECK('1 Column / Line'),AT(316,164,68),USE(GenSim:OnePerLine),SKIP,LEFT, |
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
                BUTTON(' &Generate<13,10> Format'),AT(59,140,70,23),USE(?GenSimpleFormatBtn), |
                        ICON('LFmtIcon.ico'),TIP('Generate format using above parameters'),LEFT
                BUTTON('Pre&view<13,10>Format'),AT(148,140,66,23),USE(?GenSimplePreviewBtn),SKIP, |
                        ICON(ICON:Zoom),TIP('Preview Format in a LIST on a Window'),LEFT
                BUTTON('&Parse<13,10>Format'),AT(228,140,64,23),USE(?GenSimpleParseBtn),ICON(ICON:VCRplay), |
                        TIP('Put Format into "LIST Code" tab then parse Columns to "FORMAT Lines" tab'), |
                        LEFT
                STRING('FORMAT'),AT(389,196,14,45),USE(?FormatLit),FONT(,12),ANGLE(2700)
                TEXT,AT(42,178,342,76),USE(GenSim_Format),SKIP,VSCROLL,FONT('Consolas',9)
                STRING('FIELDS'),AT(389,261,14,45),USE(?FieldsLit),FONT(,12),ANGLE(2700)
                TEXT,AT(42,261,342,38),USE(GenSim_FIELDS),SKIP,VSCROLL,FONT('Consolas',9)
                BUTTON,AT(12,178,18,18),USE(?GenSimpleCopyFormatBtn),SKIP,ICON(ICON:Copy), |
                        TIP('Copy Format to Clipboard')
                BUTTON,AT(12,247,18,18),USE(?GenSimpleCopyFieldsBtn),SKIP,ICON(ICON:Copy), |
                        TIP('Copy Format and #Fields() to Clipboard')
                PROMPT('What''s This:<13,10>Creating a new List Format for a Queue can be tedious ad' & |
                        'ding each column. This generates a FORMAT for the specified number of colum' & |
                        'ns that are all the same. Then just edit for changes.'),AT(380,27,133,90), |
                        USE(?GenSimpleWhatsThis),FONT(,10)
            END
            TAB(' Flat '),USE(?TabFlat),TIP('LIST code flattened to one line')
                BUTTON('Copy Flat'),AT(58,22,,14),USE(?CopyListFlatBtn),SKIP,TIP('Copy Flat Code to ' & |
                        'Clipboard')
                PROMPT('Flat Code:'),AT(9,25),USE(?ListFlat:Prompt)
                TEXT,AT(9,40),FULL,USE(ListFlat),VSCROLL,FONT('Consolas',9)
            END
            TAB(' Parsed '),USE(?TabParsed)
                STRING('Lengths'),AT(7,24),USE(?Lengths)
                PROMPT('Format'),AT(7,39),USE(?Fmt:Format:Prompt)
                TEXT,AT(40,41,,90),FULL,USE(Fmt:Format),VSCROLL,FONT('Consolas',9)
                PROMPT('Format<13,10>Tokens<13,10>Only'),AT(7,135,,30),USE(?Fmt:TokFmt:Prompt)
                TEXT,AT(40,136,,90),FULL,USE(Fmt:TokFmt),VSCROLL,FONT('Consolas',9)
                PROMPT('#Fields'),AT(7,231),USE(?Flds:FieldsFlat:Prompt)
                TEXT,AT(40,232,,90),FULL,USE(Flds:FieldsFlat),VSCROLL,FONT('Consolas',9)
                STRING('Strings parsed from the original input used to extract the data into Queues.'), |
                        AT(40,335),USE(?WtfIsParse)
            END
            TAB('FormatQ'),USE(?TabFormatQ),TIP('Debug Format() Parse')
                TEXT,AT(8,20,,55),FULL,USE(Fmt:Format,, ?Fmt:Format:2),VSCROLL,FONT('Consolas',9)
                LIST,AT(8,82),FULL,USE(?LIST:FormatQ),VSCROLL,FONT(,9),FROM(FormatQ),FORMAT('26L(2)|' & |
                        'M~Pos 1~@n4@26L(2)|M~Pos 2~@n4@26L(2)|M~Len~@n-4@20L(2)|M~Fld#~@n2b@20L(2)|' & |
                        'M~Grp~@n-3b@200L(2)|M~FieldSpec~@s255@200L(2)|M~Token Spec~@s255@')
            END
            TAB('FieldQ'),USE(?TabFieldsQ),TIP('Debug #Fields() Parse')
                TEXT,AT(8,26,,40),FULL,USE(Flds:FieldsFlat,, ?Flds:FieldsFlat:2),VSCROLL, |
                        FONT('Consolas',9)
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
