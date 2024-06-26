  PROGRAM   ! (c) 2016-2021 by Carl Barnes released under MIT License
!--------------------------------------
! 24-Sep-2016  Original idea
! 24-Mar-2021  Published on GitHub https://github.com/CarlTBarnes/List-Format-Parser
! 26-Mar-2021  Showed on Clarion Live episode #604 Magical GitHub Mystery Tour
! 28-Mar-2021  Add Sample data rows in List Preview, change Font, and more 
! 29-Mar-2021  Sample decimals as .1234
! 31-Mar-2021  ModifierHelpPopup STATIC variables Thread Safe with Interlocked. Example, not much risk, or none.
! 18-Sep-2021  Change Help Tab so can drag window taller and see all modifiers.
! 21-Sep-2021  Generate Format Tab to create Simple format for multiple columns to avoids the tedious adding
! 25-Sep-2021  Auto Generate Format checkbox, 1 column / line checkbox
! 28-Sep-2021  Generate Format from Queue
! 03-Oct-2021  Generate Format from Queue allow FILE for making Browse Template Format
! 04-Oct-2021  List Help tab improvements. Add TYPE column. Add separate Header alignment lines ~L ~C ~R ~D ~()
! 05-Oct-2021  Add Manifest
! 28-Oct-2021  Que2Fmt allow multiple FILE for Browse Tpl
!              Drag and Drop in GQ Fields Queue
! 12-Nov-2021  New FROM parsing of FROM('Choice 1|#1') to lines shows on new Tab('FROM')
! 19-Nov-2021  Que2Fmt finds @Picture in NAME('xxx | @Picture')
! 21-Nov-2021  Que2Fmt new List [Group] by having lines in source with [ ] or ![ !]
! 11-Dec-2021  FROM make first and last code lines '' to allow moving lines easier, thanks Mike
!              FROM Tab 'Align #' button aligns #Values, 'Align "' undoes
!              FROM Tab 'Split #' button splits 'Item|#Value' into 'Item' &'|#Value' and aligns
! 15-Dec-2021  Que2Fmt Picture Popup for Date @d / Time @t added @n to show Serial Number e.g. 12/15/21=80706
! 28-Dec-2021  Enhance LastOnLine Help
! 29-Dec-2021  Help buttons open CW Help for List with CwHelpListPopup(). Tabs "From, Help, Que2Fmt, SimpleFmt" add Help button.
! 05-Jan-2022  Que2Fmt new "+Window+List" button generates WINDOW and LIST with FORMAT see GenFmt.CopyWindowAndListBtn()
! 05-Jan-2022  Que2Fmt new "Q Fields =" button generates Que:Field= for all fields see GenFmt.CopyFieldsEqualBtn()
! 07-Jan-2022  Que2Fmt "Q Fields =" button now has 3 more options: "1_Que:Field = 2_Que:Field" and "Field LIKE(Que:Field)" 2 ways
! 05-May-2022  QuoteFix CODE pasted in ClarionHub changes Single 'Quotes' CHR(39) to 91h,92h TypeSetter Quotes, also "Double" to 93h,94h
! 15-Nov-2022  Parsed tab improved LengthsText to help size variables big enough for code (check Debug tabs to see Parsed tab) see LengthsTextRtn ROUTINE 
! 05-Apr-2024  Refactor code around Open(Window) to use new ListMakeOver(). Move most code to new PrepareWindowRtn routine
! 05-Apr-2024  FormatQ correct Group Numbers to be correct not a Sequence number
! 09-Apr-2024  Column Width could be '-1' which fails NUMERIC('-')= False so added CharNumeric(Char, Pos) 
! 19-Apr-2024  Columnz Tab and Queue and Class ColumnzCls
!---------------------- TODO ----------  
![ ] Help add column for "Category or Type" (Header,Data,Flags,General,Style and Colors,Tree)
![ ] Generate Format() with @Pics for GQ LIST? - Copy Widths of current list - or not, all have NO Pic but that's ok
![ ] FILE optional CHECK('skip fields do NOT affect # #')
![ ] Double Click on List opens Form so can edit without  !@n ... Rt  Click Popup needs 'Edit <9> Double Click'
!    [ ] then add to Queue fields for Color,CellStyle,Right,Resize,etc as CHECk or RADIO(Y,N,Default=' ')  ,ColStyleZ string(2)
!        those can appear in the FORM        
!--------------------------------------
! Tips: Can be used to Copy Columns (Duplicate) because each column is on one line
!       If a LIST changes FORMAT comparing the Source with the Columns parsed 1 per Line works better. Also with Fields
!       Learning to code the FORMAT string Modifier Letters improves your skills, can be much faster to edit
!       See all columns as rows to view all pictures at once, maybe change all @n9.2 to have "b"
!       See all columns as rows to change Modifiers e.g. remove * off multiple fields 
!       If Format does not work (Invalid Error) this view can help see the issues,
!           Can be like "reserved characters" inside others e.g. [ inside ~heading~ sometimes. 
!           I use it to answer forum quesitons for Invalid LIST Formats
!           A way to attack the problem is to 
!           Split into lines you can cut HALF the lines out and see if that works, repeat
!
!       Line 594 Invalid Format String? need more work to spot bad strings     
! 
![ ] Could in Explain Footer show Tree Supress (1 B L I R) if a Tree is present. Explain has Tree<QLong> 

  INCLUDE('KEYCODES.CLW'),ONCE          
  INCLUDE('CBCodeParse.INC'),ONCE          
  INCLUDE('CBString.INC'),ONCE          

OmitWndPrv EQUATE(0)                 !Set to (1) if you do not have CBWndPreviewClass
  INCLUDE('CBWndPreview.INC'),ONCE  !Download from https://github.com/CarlTBarnes/WindowPreview
WndPrvCls   CBWndPreviewClass       !At least download the LibSrc files and put in this folder
    
  MAP
ListFormatParser    PROCEDURE()

    MODULE('ListForP-Utility.clw') 
MsgLineBreak        PROCEDURE(STRING Txt),STRING
GetExample          PROCEDURE(BYTE ExpNo, <*STRING GenQueFmtExample>),STRING
ModifierHelpPopup   PROCEDURE(STRING XPos, STRING YPos, STRING HelpBtnFEQ) 
ChrCount            PROCEDURE(STRING Text2Scan, STRING ChrList),LONG
CharNumeric         PROCEDURE(STRING FmtChar, LONG CharPosition=0),BOOL   !Is Char Numeric or '-' in Pos 1? - Format Width is Numeric, allow for '-1'
InBetween           PROCEDURE(STRING FindLeft,STRING FindRight,        STRING InLiteral, <*LONG OutLeftPos>, <*LONG OutRightPos>, <*STRING OutBetweenStr>, BOOL IncludeLeftRight=0),LONG,PROC !Returns -1 Not Found or Length Between may =0
InBetween           PROCEDURE(STRING FindLeft,STRING FindRight, CONST *STRING SearchTxt, <*LONG OutLeftPos>, <*LONG OutRightPos>, <*STRING OutBetweenStr>, BOOL IncludeLeftRight=0),LONG,PROC !Returns -1 Not Found or Length Between, may =Zero
LenSizeText         PROCEDURE(STRING VariableName, *STRING StringVar),STRING  !to fill in LengthsText
LenSizeText         PROCEDURE(STRING VariableName, *CSTRING StringVar),STRING  !to fill in LengthsText
LenSizeText         PROCEDURE(STRING VariableName, *? StringVar, LONG StringSize),STRING  !to fill in LengthsText
ListMakeOver        PROCEDURE(LONG ListFEQ, LONG ExtraLineHt, BOOL NoGridColor=False)   !Set LIST Grid Color and Line Height
MessagePipeFix      PROCEDURE(STRING InMsgText),STRING   !Change Pipes in Msg that cause Line Breaks to !
No1310              PROCEDURE(STRING Text2Clean),STRING  !Remove 13,10 return Clipped
NoTabs              PROCEDURE(*STRING Txt)               !Change Tabs 09 to Space
PopupUnder          PROCEDURE(LONG CtrlFEQ, STRING PopMenu),LONG
Picture_N_Width     PROCEDURE(SHORT pDigitsTotal, SHORT pDecimals, BOOL pMinus, BOOL pCommas, STRING pBlankB, *STRING OutPicture ),SHORT,PROC 
ReplaceInto         PROCEDURE(*STRING Into, STRING FindTxt,STRING ReplaceTxt,BYTE ClipInto=0),LONG,PROC !Return Count
UppLow              PROCEDURE(*STRING InOutText, LONG How_Up1_Lo2_UpLow3=3)  !1=Upper or 2=Lower or 3=Up[1] + Low[2:end]
    END

    MODULE('ListForP-PreviewList.clw')
DB                  PROCEDURE(STRING DbTxt) 
PreviewFormatWindow PROCEDURE(STRING pListFormat, STRING pCaption)
    END
        MODULE('Win')
OutputDebugString   PROCEDURE(*cstring Msg),PASCAL,RAW,NAME('OutputDebugStringA'),DLL(1)
InterlockedCompareExchange PROCEDURE(*LONG Destination, LONG ExChangeToValue, LONG CompareDestToValue),LONG,PASCAL,DLL(1)
InterlockedExchange        PROCEDURE(*LONG Target, LONG SetToValue),LONG,PROC,PASCAL,DLL(1)

        END    
  END
!Region Global data
ModifierHelp_ThreadOpen LONG,STATIC
ModifierHelp STRING(' <166> = Right Border <13,10> M = Resizable ' &|
     '<13,10> F = Fixed (cannot scroll) ' &|
     '<13,10> _ = Underline ' &|
     '<13,10> / = LastOnLine ' &|
     '<13,10> ? = Locator ' &|
     '<13,10> # = FieldNo ' &|
     '<13,10>' &|
     '<13,10> * = Colors in Q ' &|
     '<13,10> B(c) = BarFrame Color ' &|
     '<13,10> E(fbsb) = Color Defaults ' &|
     '<13,10> Y = Cell Style No. in Q ' &|
     '<13,10> Z(#) = Column Style' &|
     '<13,10>  ' &|
     '<13,10> I = Icon ' &|
     '<13,10> J = Icon Transparent ' &|
     '<13,10> M = Resizable     |=RightBorder ' &|
     '<13,10> P = Tip Cell QText' &|
     '<13,10> Q = Tip "Default" Column ' &|
     '<13,10> S(w)=ScrollBar ' &|
     '<13,10> T(s)=Tree (Surpress: B=Boxes I=Indent L=Lines R=Root 1=Offset) ' &|
     '<13,10>' &|
     '<13,10> Queue Order: *Color - Icon index - Tree level - Y style code - P tip' )
LenMinFreePct LONG     
!endRegion

  CODE  
  ListFormatParser()
  RETURN
!---------------------
ListFormatParser    PROCEDURE()   
    INCLUDE('ListForP-Main_Data.clw')
    INCLUDE('ListForP-Main_Wind.clw')
DoResizePosted  BOOL
Tabs1Line       BOOL
    CODE
    ListControl = GetExample(3,GenQue_TextQ)
    GenFmt.SimpleLoadConfig()
    OPEN(Window)
    DO PrepareWindowRtn
    HelpCls.Init()    
    IF GenQue:SelectTabAtOpen THEN SELECT(?TabGenQueue).
    ACCEPT  
        CASE EVENT()
        OF EVENT:CloseWindow ; LOOP Ndx=2 TO 64 ; POST(EVENT:CloseWindow,,Ndx) ; END
!        OF EVENT:Sized      ; IF ~DoResizePosted THEN POST(EVENT:DoResize). ; DoResizePosted=1
!        OF EVENT:DoResize   ; DoResizePosted=0
!           IF ~Tabs1Line AND ?Sheet1{PROP:TabRows} > 1 THEN 
!               ?Sheet1{PROP:Join} = 1 ; Tabs1Line=1
!           END 
        END 
        CASE ACCEPTED() 
        OF ?PasteBtn      ; IF ~CLIPBOARD() THEN Message('Nothing on CLIPBOARD') ; CYCLE .
                            ListControl=CLIPBOARD() ; POST(EVENT:Accepted,?ProcessBtn)
        OF ?CopyListBtn   ; SETCLIPBOARD(ListControl)                            
        OF ?GetExpPickBtn ; ListControl = GetExample(0)  
                            POST(EVENT:Accepted,?ProcessBtn) 
        OF ?ProcessBtn       ; DO ProcessBtn_ListControlRtn
        OF ?QuoteFixBtn      ; DO QuoteFixTypeSetterRtn
        OF ?CopyLineFmtBtn   ; SETCLIPBOARD(Fmt:InLines)
        OF ?CopyExplainBtn   ; SETCLIPBOARD(Fmt:Explain)  
        OF ?CopyColumnzBtn   ; ColumnzCls.ColumnzQ_ToClipboard()
        OF ?CopyLineFmtPlusExplainBtn ; SETCLIPBOARD(CLIP(Fmt:InLines) &'<13,10>' & Fmt:Explain ) 
        OF   ?ModHelp2Btn
        OROF ?ModHelp3Btn
        OROF ?ModHelp4Btn
        OROF ?ModHelp5Btn
        OROF ?ModHelpBtn_Colz
        OROF ?ModHelpBtn     ;  IF ~ModifierHelp_ThreadOpen THEN !New IDE Help Popup conflicts with Start Window
                                    RESUME(START(ModifierHelpPopup,,0{PROP:XPos}+0{PROP:Width},0{PROP:YPos}+20,ACCEPTED()))
                                ELSE
                                    WndPrvCls.CwHelpListPopup(?)
                                END

        OF ?CopyListLineFmtBtn    ; SETCLIPBOARD(ListParsed)
        OF ?CopyListFlatBtn       ; SETCLIPBOARD(ListFlat)
        OF ?CopyLineFieldsBtn     ; SETCLIPBOARD(Flds:FIELDScode)
        OF ?ListParsedHScrollOff  ; ?ListParsed{PROP:HScroll}=CHOOSE(~ListParsedHScrollOff,'1','') ; DISPLAY 
        OF ?FromFromCopyBtn       ; SETCLIPBOARD(From:From)
        OF ?FromInLinesCopyBtn    ; SETCLIPBOARD(From:InLines)
        OF ?FromAlignValueBtn     ; DO FROM_AlignValue_Rtn
        OF ?FromAlignQuoteBtn     ; DO FROM_AlignQuote_Rtn
        OF ?FromSplitValueBtn     ; DO FROM_SplitValue_Rtn

        OF ?LIST:HistoryQ
            IF KEYCODE()=MouseLeft2 THEN 
               GET(HistoryQ, CHOICE(?LIST:HistoryQ))
               ListControl = HisQ:ListControl
               POST(EVENT:Accepted,?ProcessBtn) 
            END
        OF ?CwHelpForFROM ; WndPrvCls.CwHelpOpenTopic('~From__set_listbox_data_source_.htm')
        OF ?CwHelpForList ; WndPrvCls.CwHelpListPopup(?)
        OF ?RunAgainBtn OROF ?RunAgainGFQBtn OROF ?RunAgainFmtBtn OROF ?RunAgainFromBtn OROF ?RunAgainBtn_Colz
            RUN(COMMAND('0'))
        OF ?DebugTabs   ; DO TabHideSyncRtn 
        OF ?PreviewListBtn OROF ?PreviewList2Btn OROF ?PreviewList3Btn OROF ?PreviewListBtn_Colz
            IF Fmt:Format THEN 
               START(PreviewFormatWindow,,Fmt:Format,0{'Prop_Caption_Details'}) 
            ELSE
               SELECT(?TabInput)
               Message('You must Process the LIST Code before Preview.','List Format')
            END

        OF ?GenSimpleFormatBtn  ; GenFmt.SimpleGen()
        OF ?GenSimplePreviewBtn ; GenFmt.SimplePreviewBtn()
        OF ?GenSimpleParseBtn   ; GenFmt.SimpleParseBtn()
        OF ?GenSimpleCopyFormatBtn  ; GenFmt.CopyFormatBtn(?GenSim_Format,0)
        OF ?GenSimpleCopyFieldsBtn  ; GenFmt.CopyFormatBtn(?GenSim_Format,?GenSim_FIELDS)
        OF ?GenSimpleDefaultSaveBtn ; GenFmt.ConfigGetPut(2,'GenSimple',GenFmt_Simple)
        OF ?GenSimpleDefaultLoadBtn ; GenFmt.SimpleLoadConfig() ; DISPLAY
        OF ?GenSimpleClearBtn       ; GenFmt_Simple=GenFmt_Simple_Defaults ; DISPLAY

        OF ?GenQue_PasteBtn   ; IF ~CLIPBOARD() THEN Message('Nothing on CLIPBOARD') ; CYCLE .
                                GenQue_TextQ=CLIPBOARD() ; POST(EVENT:Accepted,?GenQue_ProcessBtn)
        OF ?GenQue_ProcessBtn ; GenFmt.QueueTextParse()  ; IF GenQue:AutoGenerate AND RECORDS(GQFieldsQ) THEN POST(EVENT:Accepted,?GenQueueFormatBtn).

        OF ?GenQueueFormatBtn  ; IF ~GenQue_TextQ THEN 
                                    ?GenQue_TextQ{PROP:Background}=Color:Yellow ; SELECT(?GenQue_TextQ) ; CYCLE
                                 END
                                 IF ~RECORDS(GQFieldsQ) THEN GenFmt.QueueTextParse().
                                 GenFmt.QueueGenFormat()
        OF ?GenQueuePreviewBtn ; GenFmt.QueuePreviewBtn()
        OF ?GenQueueParseBtn   ; GenFmt.QueueParseBtn()

        OF ?GenQueueCopyFormatBtn  ; GenFmt.CopyFormatBtn(?GenQue_Format,0)    
        OF ?GenQueueCopyFieldsBtn  ; GenFmt.CopyFormatBtn(0,?GenQue_FIELDS)
        OF ?GenQueueCopyField2Btn  ; GenFmt.CopyFormatBtn(?GenQue_Format,?GenQue_FIELDS)       
        OF ?GenQueueCopyWindowBtn  ; GenFmt.CopyWindowAndListBtn(?GenQue_Format,?GenQue_FIELDS)        
        OF ?GenQueueFieldsEqualBtn ; GenFmt.CopyFieldsEqualBtn() 
        OF ?GenQueueDefaultSaveBtn ; GenFmt.ConfigGetPut(2,'GenQueue',GenFmt_Queue)
        OF ?GenQueueDefaultLoadBtn ; GenFmt.QueueLoadConfig() ; DISPLAY
        OF ?GenQueueClearBtn       ; GenFmt_Queue=GenFmt_Queue_Defaults ; DISPLAY
        OF ?GenSim:Picture  ; GenFmt.PictureAccepted(?,' ')
        OF ?GenQue:Pic_Date ; GenFmt.PictureAccepted(?,'D')
        OF ?GenQue:Pic_Time ; GenFmt.PictureAccepted(?,'T') 

        OF ?GenQue:Pic_Date:Popup ; GenFmt.PicturePopup(?GenQue:Pic_Date,'D') ; cycle
        OF ?GenQue:Pic_Time:Popup ; GenFmt.PicturePopup(?GenQue:Pic_Time,'T') ; cycle
        OF ?GenQue_BangPicBtn     ; GenFmt.BangPictureBtn()
        END !Case Accepted()

        CASE Accepted()
        OF ?GemSim_Group TO ?GenSim:AutoGenerate OROF ?GenSim:Columns
           IF GenSim:AutoGenerate THEN GenFmt.SimpleGen(). 
        OF ?GemQue_Group TO ?GenQue:AutoGenerate
           IF GenQue:AutoGenerate THEN GenFmt.QueueGenFormat().         
        END !Case Accepted()

        CASE FIELD()
        OF ?List:ModifierQ  
            GET(ModifierQ,CHOICE(?List:ModifierQ))            
            CASE EVENT()
            OF EVENT:HeaderPressed
               CASE ?List:ModifierQ{PROPList:MouseDownField} !FYI PROPLIST:SortColumn is the current Sort not the new one
               OF 1 ; SORT(ModifierQ,ModQ:Sort)  ; ?{PROPLIST:Locator,1}=1
               OF 2 ; SORT(ModifierQ,ModQ:Prop)  ; ?{PROPLIST:Locator,1}=2
               OF 3 ; SORT(ModifierQ,ModQ:Type,ModQ:Prop)  ; ?{PROPLIST:Locator,1}=3
               OF 4 ; SORT(ModifierQ,ModQ:Name)  ; ?{PROPLIST:Locator,3}=1
               END
               GET(ModifierQ, ModQ:Sort) 
               ?List:ModifierQ{PROP:Selected}=POINTER(ModifierQ) 
               DISPLAY
            
            OF EVENT:AlertKey                           
               CASE KEYCODE()
               OF CtrlC
                  SETCLIPBOARD(CLIP(ModQ:Char) &'  '& CLIP(ModQ:PropFULL) &'  '&CLIP(ModQ:Name)) 
               OF CtrlShiftC  ; Message('CtrlShiftC TODO Copy ALL')
               END
            OF EVENT:NewSelection
               DISPLAY(?ModQ:Desc) 
               IF KEYCODE()=MouseRight THEN
                  CASE POPUP('Copy PROP and Description|Copy PROPLIST|Copy Long Description|-|Copy All|-|Open CW Help')
                  OF 1 ; SETCLIPBOARD(CLIP(ModQ:Char) &'  '& CLIP(ModQ:PropFULL) &'  '&CLIP(ModQ:Name))
                  OF 2 ; SETCLIPBOARD(ModQ:PropFULL)
                  OF 3 ; SETCLIPBOARD(ModQ:Desc)
                  OF 4 ; WndPrvCls.QueueReflection(ModifierQ,'ModifierQ',1)
                  OF 5 ; WndPrvCls.CwHelpOpenTopic('~proplist_properties_index.htm',0) !('PROPLIST',0) or (ModQ:Prop)
                  END
               ELSE 
                  
               END    
            END 
        OF ?List:GQFieldsQ      ; GenFmt.List:GQFieldsQ_TakeEvent()
        OF ?List:ColumnzQ       ; ColumnzCls.List:ColzQ_TakeEvent()
        END 
    END
    CLOSE(Window)
    RETURN
!---------------------------
ProcessBtn_ListControlRtn ROUTINE   !When ProcessBtn is pressed
    ListFlat=ListControl
    FlattenCls.TrimText1310(ListFlat,0)   !Incase pasted format alone with extra CRLF
    IF INSTRING('<13>',ListFlat) THEN FlattenCls.Flatten(ListFlat) .
    DISPLAY
    DO ParseRtn
    DO AddHistoryRtn
    Fmt:InLines = Format2QCls.GetLinesFmt(Flds:InLines)
    Fmt:Explain = Format2QCls.GetExplainLines()
    DO ParseListParsedIntoListLinesRtn
    DO WindowCaptionRtn
    DO LengthsTextRtn
    SELECT(CHOOSE(Fmt:Format OR ~From:From,?TabFormatLines,?TabFROM))
    DISPLAY
    EXIT
!---------------------------
PrepareWindowRtn ROUTINE    !Window is Open, get it all squared away
    DATA
FldX LONG
TTip CSTRING(500)
    CODE
    SYSTEM{PROP:PropVScroll}=1                                    !make Thumb Proportional
    SYSTEM{PROP:FontName}='Segoe UI' ; SYSTEM{PROP:FontSize}=10   !Message Font
        COMPILE('**C11**', _C110_)
    SYSTEM{PROP:MsgModeDefault}=MSGMODE:CANCOPY             !Added in C11 for Default Message() Mode
                 **C11**

    0{PROP:MinWidth} =400 ; 0{PROP:MinHeight}=0{PROP:Height}/2

    ?Sheet1{PROP:TabSheetStyle}=1
    ?Sheet1{PROP:BrokenTabs}=1        !this does not seem to work with the office style tabs
    ?Sheet1{PROP:NoSheet}=1
    ?Sheet1{PROP:Below}=1
    ?Sheet1{PROP:NoTheme}=1  !For Manifest
    IF EXISTS('_ShowDebugTabs_.txt') THEN DebugTabs=1.
    DO TabHideSyncRtn

    ListMakeOver(?LIST:HistoryQ,2)
    ListMakeOver(?LIST:ModifierQ,0)     ; ?List:ModifierQ{PROPLIST:HasSortColumn}=1      
    ListMakeOver(?LIST:GQFieldsQ,0) 
    ListMakeOver(?LIST:FormatQ,2) ; ListMakeOver(?LIST:FieldsQ,1) ; ListMakeOver(?LIST:ExplainQ,1) 
    ListMakeOver(?LIST:ColumnzQ,1)
                 ?LIST:ColumnzQ{PROPSTYLE:FontName,6}='Consolas'  !Format Column
                 ?LIST:ColumnzQ{PROPSTYLE:FontSize,6}=9

    TTip='Run Another Instance<13,10,13,10>'& Command('0') 
    ?RunAgainBtn{PROP:Tip}=TTip ; ?RunAgainFmtBtn{PROP:Tip}=TTip ; ?RunAgainFromBtn{PROP:Tip}=TTip ; ?RunAgainGFQBtn{PROP:Tip}=TTip
    ?RunAgainBtn_Colz{PROP:Tip}=TTip
    
    0{PROP:Status,1}=-1 ! Remoted into Windows 11 have trouble resizing Window due to FULL lists, so hope Status bar helps
    0{PROP:Status,2}=70 ; 0{PROP:StatusText,2}='EXE RTL ' & SYSTEM{PROP:ExeVersion,2} &'.'& SYSTEM{PROP:ExeVersion,3}
    0{PROP:Status,3}=70 ; 0{PROP:StatusText,3}='DLL RTL ' & SYSTEM{PROP:LibVersion,2} &'.'& SYSTEM{PROP:LibVersion,3}
    0{PROP:Status,4}=0
    FldX=0
    LOOP    !Assign Tip to all MSG so it shows in new Status Bar
        FldX=0{PROP:NextField,FldX} ; IF ~FldX THEN BREAK.
        IF ~FldX{PROP:Msg} AND FldX{PROP:Tip} THEN FldX{PROP:Msg}=FldX{PROP:Tip}.
    END
    OMIT('!**END WndPrv Omit**', OmitWndPrv)   
        WndPrvCls.Init(1)     !WndPreview secret button hover upper left corner and pops up
        WndPrvCls.InitList(?LIST:HistoryQ ,HistoryQ ,'HistoryQ')    !Not required in 11.13505, but below does show Queue Name in WndPreview 
        WndPrvCls.InitList(?LIST:ColumnzQ ,ColumnzQ ,'ColumnzQ')
        WndPrvCls.InitList(?LIST:FormatQ  ,FormatQ  ,'FormatQ')
    !**END WndPrv Omit**
    EXIT
!---------------------------
TabHideSyncRtn ROUTINE 
    ?TabFlat{PROP:Hide}   =1-DebugTabs
    ?TabParsed{PROP:Hide} =1-DebugTabs
    ?TabFormatQ{PROP:Hide}=1-DebugTabs
    ?TabFieldsQ{PROP:Hide}=1-DebugTabs
    ?TabExplainQ{PROP:Hide}=1-DebugTabs
    ?LIST:ModExtraQDbg{PROP:Hide}=1-DebugTabs
    ?LIST:ModifierQDbg{PROP:Hide}=1-DebugTabs
    IF DebugTabs AND 0{PROP:Width}<700 THEN 0{PROP:Width}=700.
!---------------------------
WindowCaptionRtn ROUTINE
    DATA
UseLst    STRING(30)
FromLst   STRING(30)
FormatLst STRING(46)
LF LONG
    CODE
    InBetween('USE('     ,')'     ,ListFlat,,,UseLst ,   1)  ; UppLow(UseLst[1:3])
    InBetween('FROM('    ,')'     ,ListFlat,,,FromLst,   1)  ; UppLow(FromLst[1:4])
    LF=InBetween('FORMAT(''',''')',ListFlat,,,FormatLst, 1)  ; UppLow(FormatLst[1:6])
    IF LF > 45 THEN FormatLst=SUB(FormatLst,1,40) &'...'')'.
    0{PROP:Text}='LIST 411: '& lower(FORMAT(CLOCK(),@t3))   & |
                          CLIP('  '& UseLst) & CLIP('  '& FromLst) & CLIP('  '& FormatLst) !&' Ln=' & Ln
    0{'Prop_Caption_Details'} = CLIP(UseLst) & CLIP('  '& FromLst) & CLIP('  '& FormatLst)
    EXIT
!---------------------------
LengthsTextRtn ROUTINE  !Debug info on screen to know STRING's are big enough
    LenMinFreePct = 100
    LengthsText = |
     LenSizeText('ListControl'    , ListControl    ) & |
     LenSizeText('Fmt:InLines'    , Fmt:InLines    ) & |
     LenSizeText('Flds:InLines'   , Flds:InLines   ) & |
     LenSizeText('Flds:FieldsCod', Flds:FieldsCode) & |
     LenSizeText('Fmt:Explain'    , Fmt:Explain    ) & |
     LenSizeText('ListParsed'     , ListParsed     ) & |
     LenSizeText('ListFlat'       , ListFlat       ) & |
     LenSizeText('Fmt:Format'     , Fmt:Format     ) & |
     LenSizeText('Flds:FieldsFla', Flds:FieldsFlat) & |     
     LenSizeText('Fmt:TokFmt'     , Fmt:TokFmt     ) & |
     LenSizeText('From:From'      , From:From      ) & |
     LenSizeText('From:InLines'   , From:InLines   ) & |
     LenSizeText('From:CASE'      , From:CASE      )
    
    LengthsText = CLIP(LengthsText) &'<13,10>Old: ' &  |
                     'LIST ' & LEN(CLIP(ListControl))         &', ' & |
                     'Fmt Lines ' & LEN(CLIP(Fmt:InLines))    &', ' & |
                     'FldsLines ' & LEN(CLIP(Flds:InLines))   &', ' & |
                     'FldsCode ' & LEN(CLIP(Flds:FieldsCode)) &', ' & |
                     'Explain ' & LEN(CLIP(Fmt:Explain))      &', ' & |
                     'ListParsed ' & LEN(CLIP(ListParsed))    &', ' & |
                     'Flat ' & LEN(CLIP(ListFlat))            &', ' & |
                     'Format ' & LEN(CLIP(Fmt:Format))        &', ' & |
                     'FldsFlat ' & LEN(CLIP(Flds:FieldsFlat)) &', ' & |           
                     'TokFmt ' & LEN(CLIP(Fmt:TokFmt))        &' -- ' & |
                     'FROM ' & LEN(CLIP(From:From))           &', ' & | 
                     'Lines ' & LEN(CLIP(From:InLines))       &', ' & |
                     'Case ' & LEN(CLIP(From:CASE))           &' bytes'
   ?Lengths:Prompt{PROP:Tip}= LenSizeText('LengthsText' , LengthsText )
   IF LenMinFreePct < 5 THEN 
      UNHIDE(?TabParsed) 
      ?LengthsText{PROP:Color}=0E6E6FFh
      SELECT(?LengthsText)
      DISPLAY
      Message('The lenghts of some variables may be too small for the LIST you pasted.|Review the bottom of the Parsed tab.||'& CLIP(LengthsText),'LengthsTextRtn ROUTINE')
   END 
    EXIT

ParseRtn ROUTINE
    CLEAR(FormatGrp) ;   FREE(FormatQ)    
    CLEAR(FieldsGrp) ;   FREE(FieldsQ) 
    CLEAR(FromGrp)
    IF ~ListFlat THEN EXIT.              !Flaten failed
    DO Parse_FROM_Rtn    
    DO Parse2Rtn
    DISPLAY
    IF ~Fmt:Format  THEN      !did not find FORMAT, maybe they pasted Format string only
       Fmt:Format = LEFT(ListFlat)
       Ndx=LEN(CLIP(Fmt:Format)) 
       IF DebugMsgs THEN 
          Message('ParseRtn ROUTINE did not find Fmt:Format.||Length of Flat=' & Ndx & |
                  '|Fmt:Format[1]=' & Fmt:Format[1] & '|Fmt:Format[Ndx]=' & Fmt:Format[Ndx] ) 
       END
            !message('Fmt:Format[1]=' & Fmt:Format[1] & '||Fmt:Format[Ndx]=' & Fmt:Format[Ndx] )        
       IF Fmt:Format[1]=CHR(39) AND Ndx > 2 AND Fmt:Format[Ndx]=CHR(39) THEN !Is it in 'Quotes' ?
          Fmt:Format = Fmt:Format[2 : Ndx-1]
       END 
       IF Fmt:Format[1]<>'[' AND ~CharNUMERIC(Fmt:Format[1],1) THEN   !Valid foramt has '[' or Number 
          IF DebugMsgs THEN message('First byte not [ or #||  Fmt:Format[1]=' & Fmt:Format[1]).
          Fmt:Format=''
          EXIT
       END 
       IF DebugMsgs THEN 
          Message('ParseRtn ROUTINE did not find Fmt:Format.||But did find format string||' & Fmt:Format )
       END        
    END
    Format2QCls.Parse2Q(Fmt:Format, Fmt:TokFmt )
    DISPLAY
    EXIT

Parse2Rtn ROUTINE

    Fmt:Found = ParserCls.FindAttrParen(ListFlat, 'FORMAT', Fmt:BegPos, Fmt:Paren1, Fmt:Paren2, Fmt:Quote1, Fmt:Quote2)  
    IF DebugMsgs THEN MESSAGE('Find FORMAT  Fmt:Found=' & Fmt:Found &|
            '||BegPos=' & Fmt:BegPos &', Paren1=' & Fmt:Paren1  &', Paren2=' & Fmt:Paren2 &', Quote1=' & Fmt:Quote1 &', Quote2=' & Fmt:Quote2 & |
            '||' & SUB(ListFlat, Fmt:BegPos, Fmt:Paren2 - Fmt:BegPos+1) )  .
    IF ~Fmt:Found OR Fmt:Quote1<1 OR Fmt:Quote2 < 2 OR Fmt:Quote2 <= Fmt:Quote1 THEN 
        Fmt:Found = 0
        EXIT
    END    

    Fmt:Format = LEFT(ListFlat[Fmt:Quote1 + 1 : Fmt:Quote2 - 1] )
    DISPLAY
    IF SUB(ListFlat,Fmt:Paren2+1,9999)<='' THEN Fmt:IsLast=True.      !10/02/18 Nothing after ")"

    Flds:Found = ParserCls.FindAttrParen(ListFlat, '#FIELDS', Flds:BegPos, Flds:Paren1, Flds:Paren2, Q1#, Q2#)  
    IF DebugMsgs THEN  MESSAGE('Find #FIELDS  Flds:Found=' & Flds:Found &|
            '||BegPos=' & Flds:BegPos &', Paren1=' & Flds:Paren1  &', Paren2=' & Flds:Paren2 & |
            '||' & MsgLineBreak(SUB(ListFlat, Flds:BegPos, Flds:Paren2 - Flds:BegPos+1)) ) .

    IF ~Flds:Found OR Flds:Paren1<1 OR Flds:Paren2 < 2 OR Flds:Paren2 <= Flds:Paren1   THEN
       Flds:Found = 0 
       EXIT
    END   
    Flds:FieldsFlat = LEFT(ListFlat[Flds:Paren1 + 1 : Flds:Paren2 - 1]) 
    IF Flds:FieldsFlat THEN DO ParseFieldsRtn.   
    EXIT
!----------------------
ParseListParsedIntoListLinesRtn ROUTINE
    DATA
CbCodeParCls    CBCodeParseClass
COLine          CSTRING( SIZE(ListParsed)+1) 
PLinesQ     QUEUE,PRE(PLinesQ)
ALine           STRING(512)    !PLinesQ:ALine
            END 
PNdx        LONG
InX        LONG
pIndent     PSTRING(5)             
Fmt:4Insert LIKE(Fmt:InLines)
    CODE
    Fmt:4Insert  = Fmt:InLines     !I want to change FORMAT in one line to my multi line
    InX=INSTRING('FORMAT',UPPER(Fmt:4Insert[1:255]),1)  !Find FORMAT, not begnning  , |<13,10>
    Fmt:4Insert = SUB(Fmt:4Insert,InX,9999)             !Cut it off 
    InX=INSTRING('), |',Fmt:4Insert,1)                  !Find ending  ' ), |<13,10,9>'
    IF InX THEN Fmt:4Insert=SUB(Fmt:4Insert,1,Inx+1).   !Leave Just ),
    
    CLEAR(ListParsed) 
    CbCodeParCls.MakeCodeOnlyLine(ListFlat,COLine)  !
    !ListParsed = COLine 
    !CbCodeParCls.ParseCOLineAttrib2Q  PROCEDURE(CONST *CSTRING COLine, QUEUE LinesQ) !Split at Commas between Attribs
    CbCodeParCls.ParseCOLineAttrib2Q(ListFlat, COLine, PLinesQ) !Split at Commas between Attribs
    LOOP PNdx = 1 TO RECORDS(PLinesQ)
         GET(PLinesQ,PNdx)
         ListParsed = CHOOSE(PNdx=1,' ', CLIP(ListParsed) &' |<13,10> ') & |
            CHOOSE(PLinesQ:ALine[1:6]<>'FORMAT', ' '&PLinesQ:ALine, Fmt:4Insert) 
    END
    ListParsed = CLIP(ListParsed) &' <13,10>' & Fmt:Explain
    !Debug SELECT(?TabFormatLines) ; display ; message('is TabFormatLines filled?')
    EXIT
!----------------------    
ParseFieldsRtn ROUTINE
    DATA
StrUtilCls  CBStringUtilityClass
Ndx     LONG    
LenFld  LONG    
MaxFld  LONG    
    CODE
    StrUtilCls.ParseTokenStringIntoQueue(Flds:FieldsFlat, ',', FieldsQ, 0) 
    Flds:Records = RECORDS(FieldsQ)
    LOOP Ndx=1 TO Flds:Records
         GET(FieldsQ,Ndx)
         LenFld=LEN(CLIP(FldsQ:Name))
         IF LenFld > MaxFld THEN MaxFld=LenFld.
    END
    Flds:FieldsCode=' #FIELDS('  !Note: CString
    LOOP Ndx=1 TO Flds:Records
         GET(FieldsQ,Ndx)  
         Flds:FieldsCode = Flds:FieldsCode & SUB(FldsQ:Name,1,MaxFld) & |
                       CHOOSE(Ndx=Flds:Records,' ) ,',' , |<13,10> {9}')
    END     
    EXIT
!--------------------
Parse_FROM_Rtn ROUTINE !FROM('Name|#Value') Parsing added 11/10/21
   DATA
FX LONG
F1 LONG
CX LONG
LenFrom LONG             !' FROM('
FNewLine STRING(' &|<13,10>      ')
FromIsLast BOOL
FItem PSTRING(256)
FCaseQ QUEUE,PRE(FCaseQ)
Item     PSTRING(256) !FCaseQ:Item
ItemLen  SHORT        !FCaseQ:ItemLen
LabelLen SHORT        !FCaseQ:LabelLen
ValuePos SHORT        !FCaseQ:ValuePos  |# or 1
ValueLen SHORT        !FCaseQ:ValueLen
      END
MaxValLen BYTE
Use:BegPos LONG
Use:Paren1 LONG
Use:Paren2 LONG
CsvLabels  CSTRING(1000)
CsvValues  CSTRING(1000)
    CODE
    From:Found = ParserCls.FindAttrParen(ListFlat, 'FROM', From:BegPos, From:Paren1, From:Paren2, From:Quote1, From:Quote2)
    IF DebugMsgs THEN MESSAGE('Find FROM  From:Found=' & From:Found &|
            '||BegPos=' & From:BegPos &', Paren1=' & From:Paren1  &', Paren2=' & From:Paren2 &', Quote1=' & From:Quote1 &', Quote2=' & From:Quote2 & |
            '||' & SUB(ListFlat, From:BegPos, From:Paren2 - From:BegPos+1) )  .
    IF ~From:Found OR From:Quote1<1 OR From:Quote2 < 2 OR From:Quote2 <= From:Quote1 THEN
        From:Found = 0
        EXIT
    END
    From:FROM = LEFT(ListFlat[From:Quote1 + 1 : From:Quote2 - 1] )
    IF SUB(ListFlat,From:Paren2+1,9999)<='' THEN FromIsLast=True.
    IF ~From:FROM THEN EXIT.
    From:InLines=' , |<13,10> FROM(<39,39>'
    F1=1
    LenFrom=LEN(CLIP(From:FROM))
    LOOP FX=2 TO LenFrom+1
         IF FX=LenFrom+1              |     !The End of the From
         OR (From:FROM[FX]='|'        |     !The End of an Element
         AND From:FROM[FX+1] <> '|'   |     !Two || are compressed to 1
         AND From:FROM[FX+1] <> '#' ) THEN  !The |#Value remains on the same line
             FItem=From:FROM[F1 : FX-1]
             From:InLines=CLIP(From:InLines) & |             ! Prior |strings
                          FNewLine & CHOOSE(F1=1,' ','') & | ! &|<13,10>
                          ''''& FItem &''''   !  |Next String|#Value
             IF F1>1 THEN FItem=SUB(FItem,2,LEN(FItem)-1). !'|String' now 'String'
             FCaseQ:Item = FItem                !Build CaseQ so can generate CASE OF #Value
             FCaseQ:ItemLen=LEN(FItem)
             CX=INSTRING('|#',FItem,1)          !Look for |#Value
             IF ~CX THEN                        !No |#Value
                FCaseQ:ValuePos=1
                FCaseQ:ValueLen=FCaseQ:ItemLen
                FCaseQ:LabelLen=FCaseQ:ItemLen
             ELSE
                FCaseQ:ValuePos=CX+2              !Take |# as Value
                FCaseQ:ValueLen=FCaseQ:ItemLen-CX-1
                FCaseQ:LabelLen=CX-1
             END
             IF MaxValLen < FCaseQ:ValueLen THEN MaxValLen=FCaseQ:ValueLen.
             ADD(FCaseQ)
             F1 = FX
         END
    END
    From:InLines=CLIP(From:InLines) & FNewLine &'<39,39>)'& CHOOSE(~FromIsLast,', |','')
    From:CASE='CASE ListUseVariable'
    IF ParserCls.FindAttrParen(ListFlat, 'USE', Use:BegPos, Use:Paren1, Use:Paren2, CX, CX) |
    AND Use:Paren2 > Use:Paren1 THEN
        FItem=CLIP(LEFT(ListFlat[Use:Paren1 + 1 : Use:Paren2 - 1]))
        IF FItem>' ' AND FItem[1]<>'?' THEN From:CASE='CASE '& FItem.
    END
    LOOP CX=1 TO RECORDS(FCaseQ)
         GET(FCaseQ,CX)
         From:CASE=CLIP(From:CASE)&'<13,10>'& |
            'OF '''& SUB(FCaseQ:Item,FCaseQ:ValuePos, FCaseQ:ValueLen) &'''' & |
            ALL(' ',MaxValLen-FCaseQ:ValueLen+10) &'!'& FORMAT(CX,@n3) &'  '& FCaseQ:Item
         CsvLabels=CHOOSE(CX=1,'',CsvLabels&',') &''''& SUB(FCaseQ:Item,1, FCaseQ:LabelLen) &''''
         CsvValues=CHOOSE(CX=1,'',CsvValues&',') &''''& SUB(FCaseQ:Item,FCaseQ:ValuePos, FCaseQ:ValueLen) &''''
    END
    From:CASE = CLIP(From:CASE) &   '<13,10>END' &|
                             '<13,10><13,10>!Choices: '& CsvLabels  & |       !For INLIST or CHOOSE
      CHOOSE(CsvValues=CsvLabels,'','<13,10>!Values:  '& CsvValues) &'<13,10>'
    EXIT
!--------------------
FROM_AlignValue_Rtn ROUTINE !12/11/21 Align the # signs  ?FromAlignValueBtn
   DATA
LX LONG
PndPos  LONG
MaxPnd  LONG
ALine   STRING(255)
NewFrom LIKE(From:InLines)
   CODE
   LOOP LX=1 TO ?From:InLines{PROP:LineCount}
        PndPos=INSTRING('|#',?From:InLines{PROP:Line,LX} ,1)
        IF PndPos > MaxPnd THEN MaxPnd = PndPos.
   END
   IF MaxPnd=0 THEN EXIT.
   LOOP LX=1 TO ?From:InLines{PROP:LineCount}
        ALine=?From:InLines{PROP:Line,LX}
        PndPos=INSTRING('|#',ALine,1)
        NewFrom=CHOOSE(LX=1,'',CLIP(NewFrom)&'<13,10>') & |
                CHOOSE(~PndPos OR PndPos >= MaxPnd,'', ALL(' ',MaxPnd-PndPos) ) & |
                ALine
   END
   From:InLines=NewFrom ; DISPLAY
   EXIT
!--------------------
FROM_AlignQuote_Rtn ROUTINE !12/11/21 Undo Align # by Align Quotes  ?FromAlignValueBtn
   DATA
LX LONG
Spaces  LONG
ALine   STRING(255)
NewFrom LIKE(From:InLines)
   CODE
   LOOP LX=1 TO ?From:InLines{PROP:LineCount}
        ALine=?From:InLines{PROP:Line,LX}
        IF LEFT(ALine,1)='''' THEN   !Quote line
           CASE LEFT(ALine,2)
           OF '''|'     ; Spaces=6       !Assume '|xxxx
           OF ''''''    ; Spaces=6       !Final  '')
           ELSE         ; Spaces=7
           END
           ALine=ALL(' ',Spaces) & LEFT(ALine) 
        END
        NewFrom=CHOOSE(LX=1,'',CLIP(NewFrom)&'<13,10>') & ALine
   END
   From:InLines=NewFrom ; DISPLAY
   EXIT
!--------------------
FROM_SplitValue_Rtn ROUTINE !12/11/21 Align the # signs  ?FromAlignValueBtn
   DATA
LX LONG
PndPos  LONG
MaxPnd  LONG
ALine   STRING(255)
NewFrom LIKE(From:InLines)
   CODE
   DO FROM_AlignQuote_Rtn
   LOOP LX=1 TO ?From:InLines{PROP:LineCount}
        ALine=?From:InLines{PROP:Line,LX}
        PndPos=INSTRING('|#',ALine,1)                   !Find |#  in 'Single|#S'
        IF ~PndPos THEN CYCLE.
        IF SUB(ALine,PndPos-2,4)='&''|#' THEN CYCLE.    !Already done
        IF PndPos > MaxPnd THEN MaxPnd = PndPos.
   END
   IF MaxPnd=0 THEN EXIT.
   LOOP LX=1 TO ?From:InLines{PROP:LineCount}
        ALine=?From:InLines{PROP:Line,LX}
        PndPos=INSTRING('|#',ALine,1)
        IF PndPos AND SUB(ALine,PndPos-2,4) <> '&''|#' THEN
           ALine=SUB(ALine,1,PndPos-1) & |
                 '''' & ALL(' ',MaxPnd-PndPos) & ' &''' & | 
                 SUB(ALine,PndPos,255) 
        END
        NewFrom=CHOOSE(LX=1,'',CLIP(NewFrom)&'<13,10>') & ALine
   END
   From:InLines=NewFrom ; DISPLAY
   EXIT
!--------------------
AddHistoryRtn ROUTINE
    DATA
ChIn    LONG,AUTO    
ChOut   LONG,AUTO
    CODE 
    IF ~ListControl THEN EXIT.
    LOOP Ndx=RECORDS(HistoryQ) TO 1 BY -1
        GET(HistoryQ,Ndx)
        IF HisQ:ListControl=ListControl THEN DELETE(HistoryQ).
    END
    HisQ:Time        = FORMAT(CLOCK(),@t1)
    HisQ:Date        = LEFT(SUB(FORMAT(today(),@d1),1,5))
    HisQ:ListControl = ListControl
    HisQ:List255     = ListFlat
    HisQ:Format255   = '    ' & Fmt:Format
    ADD(HistoryQ,1)
    ?TabHistory{PROP:text}=' History (' & RECORDS(HistoryQ) &') '
    EXIT 
!--------------------
QuoteFixTypeSetterRtn ROUTINE !05/28/22 Change Typesetter quotes to normal so code works here and in Clarion
   DATA
X  LONG
   CODE
   LOOP X=1 TO SIZE(ListControl)
        CASE VAL(ListControl[X])
        OF 91h OROF 92h ; ListControl[X]=''''     !Typesetter single quotes �� to CHR(39) 27h '
        OF 93h OROF 94h ; ListControl[X]='"'      !Typesetter double quotes �� to CHR(34) 22h "
        END
   END
   DISPLAY
   EXIT
!FYI have seen some *[] removed. Assume it is Markdown formatting applied then hidden. Nothing I can do. 
!The * is Color so not likely to break format. 
!The [] are Groups so it will likely break the Format. Could try to find unmatched pairs and fix or remove.
!Saw this ")@s50@](200)|F~" have ](200) removed so "@s50@|F~"
!------------------------------------------------------------------------
Format2QCls.Parse2Q         PROCEDURE(CONST *STRING Fmt, *STRING TokFmt) 
LnFmt   LONG,AUTO
FX      LONG,AUTO 
SegBeg      LONG 
SegEnd      LONG 
InGroup     BOOL
GroupCount  SHORT           !For Grp SEQ 1,2,3 ... Not used as much 04/05/24
GroupNoNow  SHORT           !Begin of Group #
FieldNo     SHORT
SegmentQ    QUEUE,PRE(SegQ) !figure out the Group [segments]
SegBeg        SHORT         ! SegQ:SegBeg
SegEnd        SHORT         ! SegQ:SegEnd
GrpSeqNo      SHORT         ! SegQ:GrpSeqNo         !It's 1,2,3 Sequence and NOT the Group No used for PROPLIST:Group
GrpEnd        SHORT         ! SegQ:GrpEnd  "]" pos
            END      ! SegQ:SegBeg= ; SegQ:SegEnd= ; SegQ:GrpNo= ; SegQ:GrpEnd=
    CODE 
    ![ multiple field-specifiers ] [ (size) ] [ modifiers ]     So Group can have (size) after it:
    !   and can have multiple modifiers so the group ends after the "]" at the next number or [
    !
    !The "field-group" modifiers act on the entire group of fields.
    !These are the same modifiers listed above for a field (except the *, I, T ,  *=Colorsx4  I=Icon J=TrnIcon  T=Tree
    !and #number# modifiers which are not appropriate to groups). 
    !Add PROPLIST:Group to the appropriate field property to affect the group properties. PROPLIST:GroupNo can be used to return the group number of a target column.

    FREE(FormatQ) 
    CLEAR(FormatQ) 
    LnFmt = LEN(CLIP(Fmt))  
    IF LnFmt < 5 THEN RETURN.  
    SELF.Format2Token(Fmt, TokFmt)  !Build TokFmt that is easy to find tokens
    SegBeg  = 1
    InGroup = CHOOSE(Fmt[1]='[')
    LOOP FX=2 TO LnFmt+1
         IF InGroup THEN
            IF TokFmt[FX]=']' THEN
               GroupCount += 1
               CLEAR(SegmentQ) ; SegQ:GrpSeqNo=GroupCount ; SegQ:GrpEnd=Fx
               LOOP !find where [FX+1] is the Start of a new group [] or 1 = Column
                    IF FX=LnFmt OR TokFmt[FX+1]='[' OR TokFmt[FX+1]='1' THEN BREAK.
                    FX += 1
               END
               SegQ:SegBeg=SegBeg ; SegQ:SegEnd=FX ; ADD(SegmentQ)   !Add a Group Begin [, Segment, Group End ]
               InGroup = 0 
               SegBeg = FX + 1
               CYCLE
            END 
         END
         IF FX > LnFmt OR TokFmt[FX]='[' THEN        !a new group finishes the prior one
            IF SegBeg <= Fx-1 THEN     !Could have [group][group] with nothing between
               CLEAR(SegmentQ) ; SegQ:SegBeg=SegBeg ; SegQ:SegEnd=FX-1 ; ADD(SegmentQ)
               !SELF.ParseSegment( Fmt[SegBeg : Fx-1] , SegBeg, Fx-1 ) 
            END
            SegBeg = FX
            InGroup = 1
            CYCLE
         END       
    END
    !WndPrvCls.QueueReflection(SegmentQ,'SegmentQ in Format2QCls.Parse2Q')   !Debug SegmentQ
    LOOP FX=1 TO RECORDS(SegmentQ)   !Add the segments to see them
         GET(SegmentQ,FX)
         IF ~SegQ:GrpSeqNo THEN ! Fmt TokFmt, SegPosBeg   SegPosEnd  *NxtFldNo  GroupNo  InsideGrpNo 
            SELF.ParseSegment( Fmt, TokFmt, SegQ:SegBeg, SegQ:SegEnd, FieldNo , 0 )         !Non group segment of columns
            CYCLE
         END 
         GroupNoNow = FieldNo + 1
         SELF.ParseSegment   ( Fmt, TokFmt, SegQ:SegBeg, SegQ:SegBeg, FieldNo , GroupNoNow , 0 )        !Begin Group "[" only 
         IF SegQ:SegBeg+1 <= SegQ:GrpEnd-1 THEN    
            SELF.ParseSegment( Fmt, TokFmt, SegQ:SegBeg+1 , SegQ:GrpEnd-1, FieldNo , 0 , GroupNoNow )   !Between the [brackets] columns in group
         END
         SELF.ParseSegment   ( Fmt, TokFmt, SegQ:GrpEnd, SegQ:SegEnd, FieldNo , -GroupNoNow , 0 )       !Ending ](size)modifiers         
    END       
    RETURN
!-----------------------    
Format2QCls.ParseSegment  PROCEDURE(CONST *STRING Fmt, CONST *STRING TokFmt, Long SegPosBeg, Long SegPosEnd, *SHORT NxtFldNo, SHORT GroupNo, SHORT InsideGrpNo=0 )  
FldBeg      LONG 
FldEnd      LONG 
FX          LONG,AUTO
PndPos1     USHORT,AUTO    !#5#             !10/01/18 spot #11#
PndPos2     USHORT,AUTO    !  #          
PndFldNo    SHORT,AUTO     ! 5 
    CODE
    IF GroupNo THEN
       CLEAR(FormatQ)
       FmtQ:Pos1 = SegPosBeg
       FmtQ:Pos2 = SegPosEnd
       FmtQ:LenSpec = SegPosEnd - SegPosBeg + 1
       FmtQ:GrpNo = GroupNo  
       FmtQ:FldNo=0
       FmtQ:FieldSpec = Fmt[SegPosBeg : SegPosEnd]    
       FmtQ:TokenSpec = TokFmt[SegPosBeg : SegPosEnd]    
       ADD(FormatQ)
       RETURN 
    END
    FldBeg = SegPosBeg 
    LOOP FX = SegPosBeg+1 TO SegPosEnd
         IF FX = SegPosEnd OR TokFmt[FX+1]='1' THEN 
            NxtFldNo += 1 
            FldEnd = FX
            CLEAR(FormatQ)
            FmtQ:Pos1  = FldBeg
            FmtQ:Pos2  = FldEnd 
            FmtQ:LenSpec = FldEnd - FldBeg + 1
            FmtQ:GrpNo = 0  
            FmtQ:InGrpNo = InsideGrpNo
            FmtQ:FldNo = NxtFldNo
            FmtQ:FieldSpec = Fmt[FldBeg : FldEnd]
            FmtQ:TokenSpec = TokFmt[FldBeg : FldEnd] 
            
            !Find Field Number                      !10/01/18 begin spot #5#
            PndPos1=INSTRING('#',FmtQ:TokenSpec,1)
            IF PndPos1 THEN
               PndPos2=INSTRING('#',FmtQ:TokenSpec,1,PndPos1)  
               IF PndPos2+1 >= PndPos1+1 THEN              !not ##
                  PndFldNo=FmtQ:FieldSpec[PndPos1+1 : PndPos2-1]
                  IF PndFldNo > 0 THEN
                     NxtFldNo   = PndFldNo
                     FmtQ:FldNo = NxtFldNo
                  END
               END
            END 
            
            ADD(FormatQ) 
            FldBeg = FldEnd + 1
         END
    END
    RETURN
!----------------------------------------------    
Format2QCls.GetLinesFmt     PROCEDURE(*STRING FldsLines)!,STRING  Build string with Format( with 1 column per line
FmtLns      ANY
QX          LONG,AUTO
Indent1 EQUATE(' FORMAT(''')
Indent2 EQUATE('        ''') 
gIndent  PSTRING(6)          
LineSpec STRING(512)
LastLen  LONG
MaxLen1  LONG
MaxLen2  LONG
pAlignCont  PSTRING(128) 
Col12345    STRING('     "         111111111122222222223333333333444444<13,10>' & |
                   '     "123456789012345678901234567890123456789012345<13,10>')  
    CODE   
    DO Try2AlignContinuationsRtn  !Lets try to align the 
    RETURN FmtLns
Try2AlignContinuationsRtn ROUTINE    !Align the & | 
    DO Try2AlignLoopConcatRtn
    MaxLen2 = MaxLen1 + 4 - LEN(Indent2) + 5 
    !   FmtLns = Col12345 & CLIP(FmtLns) &'<13,10>'& Col12345 & 'MaxLen1=' & MaxLen1 &'  MaxLen2='& MaxLen2 !Debug
    DO Try2AlignLoopConcatRtn
    FldsLines=CLIP(FldsLines) & '<13,10>'    
    EXIT
Try2AlignLoopConcatRtn ROUTINE
    FmtLns='' ; CLEAR(LastLen) ; CLEAR(pAlignCont)
    FldsLines='#FIELDS(<13,10>'
    LOOP QX=1 TO RECORDS(FormatQ) 
   !IF MaxLen2 AND QX > 1 THEN pAlignCont = ALL(' ',MaxLen2 - LastLen).         IF MaxLen2 AND QX > 1 THEN pAlignCont = ALL(' ',MaxLen2 - LastLen).    
         GET(FormatQ,QX)
         IF Flds:Records THEN 
            IF FmtQ:GrpNo THEN        !10/01/18 add Fields in Lines
               CLEAR(FieldsQ)
            ELSE 
               GET(FieldsQ,FmtQ:FldNo)
               IF ERRORCODE() THEN FldsQ:Name='Unknown #' & FmtQ:FldNo.
            END
            FldsLines=CLIP(FldsLines) & CLIP(FldsQ:Name) & '<13,10>'
         END        
         IF FmtQ:GrpNo < 0 THEN gIndent=''.   
         LineSpec = CHOOSE(QX=1, Indent1 , gIndent & Indent2) & SUB(FmtQ:FieldSpec,1,FmtQ:LenSpec) &''''  
         IF  FmtQ:FieldSpec[1]=']' THEN             !Push End Grp ] out to align with prior line
             LineSpec = ALL(' ',LastLen-4-5) & LineSpec
         END   
         LastLen=LEN(CLIP(LineSpec)) 
         IF MaxLen1 < LastLen THEN MaxLen1 = LastLen .
         IF MaxLen2  !False=Debug
           FmtLns = CHOOSE(QX=1, ' , |<13,10>' , FmtLns & pAlignCont &' &|<13,10>') & CLIP(LineSpec)
         ELSE  !Debug version
           FmtLns = CHOOSE(QX=1, ' , |<13,10>' , FmtLns & pAlignCont &' &|<13,10>') & LastLen &' '& MaxLen1 &' '& CLIP(LineSpec)
         END
         IF FmtQ:GrpNo > 0 THEN gIndent='  '.
         IF MaxLen2 AND QX > 1 THEN pAlignCont = ALL(' ',MaxLen2 - LastLen).            
    END 
    FmtLns = FmtLns & pAlignCont & CHOOSE(~Fmt:IsLast,' ), |<13,10,9>',')')   !10/02/18 was--> & ' ), |<13,10,9>'    
!----------------------------------------------    
Format2QCls.GetExplainLines     PROCEDURE()!,STRING 
ExpLns      ANY  
QX          LONG,AUTO
gIndBefore  PSTRING(3)
gIndAfter   PSTRING(3)
InGrpGap    STRING(4)        !'    ' or '  : '
Spacz       STRING(3)   
TokFmtCmds  STRING(256) 
MaxLens     GROUP(ExplainQ),PRE(MaxE)
ModLen4Col          USHORT              ! MaxE:ModLen4Col
LastFmtLen          USHORT              ! MaxE:LastFmtLen
            END
IsInGrp     STRING(1)         
Colz_LastColX SHORT(1)     !Groups are same Column# as the 1st Column so this helps         
Colz_LastFldX SHORT(0)     !Fields in Queue, they are adjusted for Modifier Extras
    CODE 
    FREE(ExplainQ) 
    FREE(ColumnzQ) 
    LOOP QX=1 TO RECORDS(FormatQ)
         GET(FormatQ,QX) 
         CLEAR(ExplainQ)          
         IF FmtQ:GrpNo < 0 THEN  !FmtQ:FieldSpec[1]=']' THEN              !Group End] move out length of prior line
            FmtQ:FieldSpec = ALL(' ',3+MaxE:LastFmtLen) & FmtQ:FieldSpec 
         END
         
         SELF.AssignSLM(ExpQ:FmtTxt, FmtQ:FieldSpec,        ExpQ:FmtLen, MaxE:FmtLen)
         MaxE:LastFmtLen = ExpQ:FmtLen
         IF FmtQ:GrpNo >= 1 THEN
            SELF.AssignSLM(ExpQ:GrpTxt ,'Gr['& FmtQ:GrpNo , ExpQ:GrpLen, MaxE:GrpLen )
            IsInGrp=':'
         ELSIF FmtQ:GrpNo < 0 THEN
            SELF.AssignSLM(ExpQ:GrpTxt ,'  ]'& -FmtQ:GrpNo, ExpQ:GrpLen, MaxE:GrpLen ) 
            IsInGrp=''
         ELSE 
            SELF.AssignSLM(ExpQ:ColTxt ,FmtQ:FldNo &'.' ,   ExpQ:ColLen, MaxE:ColLen )
            ExpQ:InGrp = IsInGrp
         END
         DO Set_TokFmtCmds_Rtn
         SELF.AssignSLM(ExpQ:ModTxt ,TokFmtCmds,            ExpQ:ModLen, MaxE:ModLen ) 
         IF FmtQ:GrpNo=0 AND MaxE:ModLen4Col < ExpQ:ModLen THEN 
                             MaxE:ModLen4Col = ExpQ:ModLen
         END
         IF FmtQ:FldNo AND Flds:Records THEN
            FldsQ:Name = '?Unknown #Fields#' & FmtQ:FldNo
            GET(FieldsQ,FmtQ:FldNo) 
            IF ~ERRORCODE() THEN ExpQ:FieTxt = FldsQ:Name.
            SELF.AssignSLM(ExpQ:FieTxt, ExpQ:FieTxt, ExpQ:FieLen, MaxE:FieLen ) 
         END   
         ADD(ExplainQ)
         ColumnzCls.ColumnzQ_AddFromExplainAndFormat(Colz_LastColX,Colz_LastFldX)
    END 
    IF MaxE:GrpLen THEN MaxE:GrpLen += 1.   !Leave space after, else leave zero for no Group

    ?ExplainMAXstring{PROP:Text}='MAX Lengths: Fmt=' & MaxE:FmtLen &'  GrpNo='& MaxE:GrpLen  &'  ColNo='& MaxE:ColLen &'  Mod='& MaxE:ModLen   &'  #Fie='& MaxE:FieLen 
    IF ShowExplainSpaces THEN Spacz=ALL('`'). 
    gIndBefore='' ; gIndAfter=Spacz    

    ExpLns = '!----------------------------------------------------------------------------'
    LOOP QX=1 TO RECORDS(FormatQ)
         GET(FormatQ,QX)
         GET(ExplainQ,QX)
         IF FmtQ:GrpNo < 0 THEN gIndBefore='' ; gIndAfter=Spacz ; InGrpGap='' ; end
         ! DO Set_TokFmtCmds_Rtn 
         ExpQ:ColTxt = ALL(' ',MaxE:ColLen-ExpQ:ColLen ) & ExpQ:ColTxt   !Right just ###.
         IF ~ExpQ:GrpTxt[3] THEN ExpQ:GrpTxt[3]=ExpQ:InGrp.
             IF ShowExplainSpaces
                ExpQ:FmtTxt=CLIP(ExpQ:FmtTxt)&'+{99}'    !Debug view spacing
                ExpQ:GrpTxt=CLIP(ExpQ:GrpTxt)&';{99}'    !Debug view spacing
                ExpQ:ColTxt=CLIP(ExpQ:ColTxt)&',{99}'    !Debug view spacing
                END
         ExpLns = CLIP(ExpLns &'<13,10>' & |
                  '! ' & |
                  CHOOSE(~PrefixFieldInExplain,'', SUB(ExpQ:FieTxt,1,MaxE:FieLen) &' ! ') & |      !Put #Fields before format
                  gIndBefore & SUB(ExpQ:FmtTxt,1,MaxE:FmtLen) & gIndAfter & |    !gIndent so [group] indents had &' '&
                  '! '& |
                  SUB(ExpQ:GrpTxt,1,MaxE:GrpLen) &|
                  SUB(ExpQ:ColTxt,1,MaxE:ColLen) &|
                  ' ' & |
                  CHOOSE(FmtQ:GrpNo=0, '',ExpQ:ModTxt) & |
                  CHOOSE(FmtQ:GrpNo<>0,'','  '&SUB(ExpQ:ModTxt,1,MaxE:ModLen4Col)) & |
                  '  ' & ExpQ:FieTxt )
!                  CHOOSE(FmtQ:GrpNo <1,'','Gr['   & ABS(FmtQ:GrpNo) ) & | 
!                  CHOOSE(FmtQ:GrpNo>-1,'','  ]'   & ABS(FmtQ:GrpNo) &'') & | 
!                  CHOOSE(FmtQ:GrpNo<>0,'   ',InGrpGap & FORMAT(FmtQ:FldNo,@n3) &'.') & | 
!                  ' ' & TokFmtCmds ) 
                  
         IF FmtQ:GrpNo > 0 THEN gIndBefore=Spacz ; gIndAfter='' ; InGrpGap='  : ' ; end
    END
    !FYI Alignment in the IDE is called Justification
!            '! ###=Width LCRD(#)=Alignment(indent) |=Right| M=Resize  *IJTYP /?_|*BEFMQSZ' &|
!     '<13,10>! ~Header~  LCRD(#)=AlignHead(indent) Modifiers  @picture@  #ColumnNo#' &|
!     '<13,10>! [Group Columns](GrpWidth)|MF_S HB HT ~Group Head~ LCRD(#)=AlignGrp(indent)  ' &|
!     '<13,10>!    /=LastOnLine ?=Locator _=Underline |=RightBorder  ~Header Text~' &|
!     '<13,10>!    *=ColorsInQ  B()=ColorBarFrame E()=ColorDefaults  F=Fixed' &|
!     '<13,10>!    I=Icon J=IconTrn M=Resizable P=CellTipQField Q=TipDefault S()=ScrollBar' &|
!     '<13,10>!    T(s)=Tree Y=CellStyleInQ Z(#)=ColumnStyle  Align:Left Center Right Decimal' &|

    ExpLns = CLIP(ExpLns & '<13,10>' & |
            '!---------------------------------------------------------------------------<13,10>' & |
            '! ##A(#)=Width AlignLCRD(Indent) |=Right| M=Resize  *IJTYP /?_|*BEFMQSZ' &|
     '<13,10>! ~Header~ AlignHeadLCRD(Indent) Modifiers  @picture@  #ColumnNo#' &|
     '<13,10>! [Group Columns](GroupWidth)|MF_S HB HT ~Group Head~ AlignHeadLCRD(indent)' &|
     '<13,10>!    /=LastOnLine ?=Locator _=Underline |=RightBorder  ~Header Text~' &|
     '<13,10>!    *=QColors  B(c)=ColorBarFrame E(fbsb)=ColorDefaults  F=Fixed' &|
     '<13,10>!    I=QIcon J=QIconTrn M=Resizable P=TipCellQText Q=TipDefault S(w)=ScrollBar' &|
     '<13,10>!    T(s)=QTree Y=QCellStyle Z(#)=ColumnStyle  Align:Left Center Right Decimal' &|
     '<13,10>!    Group Modifiers: |MF_S and HB=Head BG Color, HT=Head Text Color '     & |
     '<13,10>' )

    RETURN ExpLns                             

Set_TokFmtCmds_Rtn ROUTINE 
    TokFmtCmds =  SELF.TokFmtCmd('HB','HBHeadBackColor') & |  !Must comes first to remove HB HT 
                  SELF.TokFmtCmd('HT','HBHeadTextColor')      ! or else B T get found
    TokFmtCmds =  ' '&SELF.TokFmtCmd('B','BSelectBarColor') & |
                   SELF.TokFmtCmd('E','ECellColors') & |
                   TokFmtCmds
    TokFmtCmds  = '' & | 
             SELF.TokFmtCmd('*','*Colors<<Q4*Long>') & |   !This is kind of Wrong, should read in Sequence
             SELF.TokFmtCmd('I','IIcon<<QLong>') & |
             SELF.TokFmtCmd('J','JTrnIcon<<QLong>') & |
             SELF.TokFmtCmd('T','Tree<<QLong>') & |
             SELF.TokFmtCmd('Y','YCellStyle<<QLong>') & |
             SELF.TokFmtCmd('Z','ZColumnStyle') & |                  
             SELF.TokFmtCmd('P','PTip<<string>') & |
             SELF.TokFmtCmd('Q','QTipDefault') & |
             SELF.TokFmtCmd('?','?Locate') & |    
             SELF.TokFmtCmd('F','FFixed') & |       !nope     SELF.TokFmtCmd('|','|') & |  SELF.TokFmtCmd('M','MResize') & |
             SELF.TokFmtCmd('_','_Underline') & |
             SELF.TokFmtCmd('/','/Last') & |
             SELF.TokFmtCmd('S','Scroll') & |
                   TokFmtCmds 
    EXIT                   

!----------------------------------------------- 
Format2QCls.TokFmtCmd     PROCEDURE(STRING FndLetter, STRING CmdName)!,STRING 
LetPos  USHORT,AUTO 
ModBeg  USHORT,AUTO 
ModEnd  USHORT,AUTO 
MoreSpec    PSTRING(64)
    CODE
    LetPos = INSTRING(UPPER(FndLetter),UPPER(FmtQ:TokenSpec),1) 
    IF ~LetPos THEN RETURN('').
    ModBeg = LetPos + SIZE(FndLetter)  
    FmtQ:TokenSpec[ LetPos : ModBeg-1 ] = ''    !Must blank cause of 2 letter codes HB HT  
    IF FmtQ:TokenSpec[ModBeg]='(' THEN
       ModEnd = INSTRING(')',FmtQ:TokenSpec,1,ModBeg)
       IF ModEnd THEN
          MoreSpec = FmtQ:FieldSpec[ModBeg : ModEnd]
       END
    END 
    RETURN(' ' & CmdName & MoreSpec)
!----------------------------------------------- 
!Make the TokFmt string that is easiler for me to read later
Format2QCls.Format2Token    PROCEDURE(CONST *STRING Fmt, *STRING TokFmt, BYTE BlankNoise=1)
LnFmt   LONG,AUTO
FX      LONG,AUTO 
TokBeg      LONG 
!SegEnd      LONG 
InGroup     BOOL
InToken     LONG 
FormatDelim1    STRING('''~(@#')       !~Heading~  (Offest#)  @picture@  #FieldNo#  quotes? for tool tip
FormatDelim2    STRING('''~)@#')       !Note Q '' must be first to spot double
!TODO Quotes, figure them out
    CODE
    TokFmt = Fmt
    LnFmt = LEN(CLIP(TokFmt))  
    InGroup = CHOOSE(Fmt[1]='[')      !do I care ?
    LOOP FX=1 + InGroup TO LnFmt  
        IF InToken THEN
           IF Fmt[FX] = FormatDelim2[InToken] THEN
              IF InToken=1 AND Fmt[FX+1] = FormatDelim2[InToken] THEN FX += 1.  !Quotes ''   doubled
              InToken = 0 
              CYCLE
           END
           CASE Fmt[FX]
           OF 'A' TO 'Z' OROF 'a' TO 'z'  ; IF BlankNoise THEN TokFmt[FX]=''.  !a letter =a-z
           OF '0' TO '9' ; TokFmt[FX]='n' ; IF BlankNoise THEN TokFmt[FX]=''.  !a number=n=x
           ELSE          ; TokFmt[FX]='x' ; IF BlankNoise THEN TokFmt[FX]=''.  !special char
           END
           CYCLE       !chars between toke are set to x
        END
        InToken=INSTRING(Fmt[FX],FormatDelim1)   !It it 1st delimeter? 
            IF InToken THEN
               TokBeg = FX
               IF InToken=1 AND Fmt[FX+1]=FormatDelim2[InToken] THEN FX += 1.  !Quotes ''   doubled
               CYCLE
            END   
        IF CharNUMERIC(Fmt[FX],FX) THEN
           TokFmt[FX]=CHOOSE(FX=1 OR ~CharNUMERIC(Fmt[FX-1],FX-1),'1','2')
        END       
    END 
    !IF InGroup THEN  TODO Count [ ] 
!I need to count () to know they are not matched
!Inside "(" should watch for "(", or a lone ")" without "("?
    IF InToken THEN
        Message('The format string ended inside a delimiter "' & FormatDelim1[InToken] &'"'& |
                ' without an ending "' & FormatDelim2[InToken] &'".'& |
                '','Invalid Format String?')
!                '||Token began at position ' & TokBeg &':'& |  
!                '|-{40}|' & CLIP(SUB(Fmt,TokBeg,300)) & |
!                '','Invalid Format String?')
    END
    RETURN 
!-----------------------------------------------------
Format2QCls.AssignSLM       PROCEDURE(*STRING ToStr, STRING FromStr, *USHORT OutLen, *USHORT InOutMaxLen)   !Assign String with length
    CODE
    ToStr = FromStr
    OutLen = LEN(CLIP(ToStr))
    IF OutLen > InOutMaxLen THEN InOutMaxLen = OutLen .
    RETURN
!================================================    
!Region -- ColumnzCls Class
ColumnzCls.ColumnzQ_AddFromExplainAndFormat  PROCEDURE(*SHORT Colz_LastColX, *SHORT Colz_LastFldX)  !Fill in ColumnzQ fields from Format
    CODE
    DB('Add_ColumnzQ_FromExplainAndFormat_Rtn TOP #{30} '& Pointer(FormatQ) &' FmtSpec=' & FmtQ:FieldSpec )
    IF ~FmtQ:GrpNo THEN Colz_LastColX = FmtQ:FldNo.     !So Groups get Last ColNo
    CLEAR(ColumnzQ)                                   !'Grp '
    ColzQ:ColNo     = CHOOSE(~FmtQ:GrpNo,FmtQ:FldNo&'','Gr '& Colz_LastColX)  !was ExpQ:ColTxt  ! STRING(5)   !1  ColzQ:ColNo  
    ColzQ:Level     = CHOOSE(FmtQ:InGrpNo=0,1,2)    ! LONG        !-  ColzQ:Level
    ColzQ:FieldNo   = FmtQ:FldNo                    ! USHORT      !2  ColzQ:FieldNo  
    ColzQ:GroupNo   = FmtQ:GrpNo                    ! USHORT      !3  ColzQ:GroupNo 
    ColzQ:Variable  = ExpQ:FieTxt                   ! STRING(64)  

!    ColzQ:Header    = ''      ! STRING(32)  !4  ColzQ:Header   Parsed from Format
!    ColzQ:Picture   = ''      ! STRING(16)  !5  ColzQ:Picture  
!    ColzQ:Width     = ''      ! STRING(8)   !6  ColzQ:Width    
!    ColzQ:Align     = ''      ! STRING(5)   !7  ColzQ:Align    
!    ColzQ:HeadAlign = ''      ! STRING(5)   !8  ColzQ:HeadAlign
!    ColzQ:Mods      = ''      ! STRING(16)  !9  ColzQ:Mods
!    ColzQ:ModXFields=0        ! BYTE done below  
    ColzQ:FmtSource = FmtQ:FieldSpec
    ColzQ:FmtString = UNQUOTE(FmtQ:FieldSpec)
    ColzQ:FmtTokn   = FmtQ:TokenSpec
    ColzQ:Level2    = ColzQ:Level           ! LONG        !-  ColzQ:Level2 
    ColzQ:ColX      = Colz_LastColX         ! SHORT       !12 ColzQ:ColX
    ColzQ:InGroup   = FmtQ:InGrpNo          ! SHORT
    IF ~FmtQ:GrpNo THEN
        Colz_LastFldX += 1
        ColzQ:qFieldX = Colz_LastFldX    ! USHORT      !   ColzQ:FieldX    Queue Field No
    END
    ColumnzCls.ColumnzQ_From_Format(ColzQ:FmtSource, ColzQ:FmtTokn)

    IF FmtQ:GrpNo < 0 THEN                  !End ] of Group?
       DO EndofGroupApplyToBeginGroupRtn    ! update [ Begin
    ELSE     
       ADD(ColumnzQ)
       Colz_LastColX += 1
       Colz_LastFldX += ColzQ:ModXFields
    END
    RETURN 
EndofGroupApplyToBeginGroupRtn ROUTINE
    DATA
EndColzGroup    GROUP(ColumnzQ),Pre(EndColz)
                END 
    CODE
    EndColzGroup = ColumnzQ         !Save the End stuff where all the setting exist
    ColzQ:GroupNo = -1 * FmtQ:GrpNo
    GET(ColumnzQ,ColzQ:GroupNo)
    IF ERRORCODE() THEN 
         If DebugTabs OR DebugMsgs THEN Message('For End "]" Group did not find Begin |FmtQ:GrpNo='& FmtQ:GrpNo &'|ColzQ:FmtSource='& ColzQ:FmtSource,'EndofGroupApplyToBeginGroupRtn').
         EXIT
    END 
    ColzQ:Variable  = '[List Group]'
    ColzQ:Header    = EndColz:Header     ![Open Grp='' ... Close] can have ~Header~
    ColzQ:Picture   = EndColz:Picture    ![ ] Never in Group
    ColzQ:Width     = EndColz:Width      !  ] only close
    ColzQ:Align     = EndColz:Align      !  ] only
    ColzQ:HeadAlign = EndColz:HeadAlign  !  ] only
    ColzQ:Mods      = CLIP(ColzQ:Mods)    &''& EndColz:Mods          !Should be [
    ColzQ:FmtSource = CLIP(ColzQ:FmtSource)  &''& EndColz:FmtSource  !Typical [ ] Mods
    ColzQ:FmtString = CLIP(UNQUOTE(ColzQ:FmtSource))  &''& UNQUOTE(EndColz:FmtSource)   !Typical [ ] Mods
    ColzQ:FmtTokn   = CLIP(ColzQ:FmtTokn) &''& EndColz:FmtTokn
    PUT(ColumnzQ) 
    EXIT
!------------------------------    
ColumnzCls.ColumnzQ_From_Format  PROCEDURE(STRING FmtSpec, STRING FmtTokn)  !Fill in ColumnzQ from Format
EndParenCW  LONG
EndTildePos LONG            !End of ~header~ 
HeadSpec STRING(64),AUTO    !Header Extras after End Tilder~ should be L(#) 
HeadTokn STRING(64),AUTO
JunkOut  STRING(64),AUTO
    CODE
    IF ~CharNUMERIC(FmtSpec[1],1) THEN     !Must be [group]
        IF FmtSpec[1]=']' THEN 
           DO EndGroupRtn
        END
        RETURN
    END
    !Input  FmtSpec: 126L(2)|*~Employee Name~@s30@?
    !Input  FmtTokn: 122L( )|*~             ~@   @?    Just the tokens that matter
    EndParenCW   =SELF.FmtParse_WidthAlign(FmtSpec, FmtTokn, ColzQ:Width, ColzQ:Align)
    DO HeaderRtn
    ColzQ:Picture=SELF.FmtParse_Between(FmtSpec, FmtTokn,'@','@') 
    !IF ColzQ:Picture THEN ColzQ:Picture='@' & ColzQ:Picture.
    SELF.FmtParse_GetModifiers(FmtSpec,ColzQ:Mods,ColzQ:ModXFields)
    RETURN

HeaderRtn ROUTINE
    ColzQ:Header =SELF.FmtParse_Between(FmtSpec, FmtTokn,'~','~',,EndTildePos)
    IF EndTildePos AND SUB(FmtTokn,EndTildePos+1,1)<>'@' THEN     !Probably Header Align ~header~L(##)@Picture
       HeadSpec='1' & SUB(FmtSpec,EndTildePos+1,99)     !1 is fake width needed by function
       HeadTokn='1' & SUB(FmtTokn,EndTildePos+1,99)
       IF INSTRING(HeadSpec[2],'LRCD',1) THEN        !Looks like L(###)
          SELF.FmtParse_WidthAlign(HeadSpec, HeadTokn, JunkOut, ColzQ:HeadAlign)
       END       
    END
    EXIT 
                        !E.g. ](55)|_~TRS Data~C(#)
EndGroupRtn ROUTINE     !Special for End ] that may have  ~GroupHead~Justification(IndentHead)
    DB('EndGroupRtn FmtSpec=' & FmtSpec )
    IF FmtSpec[2]='(' THEN                  !Must have ](Width)
       ColzQ:Width =SELF.FmtParse_Between(FmtSpec, FmtTokn,'(',')',,EndParenCW)
       DB('Find Width in EndParenCW=' & EndParenCW &' W='& ColzQ:Width )
    END 
    DO HeaderRtn
    DB('EndGroupRtn DO HeaderRtn EndTildePos=' & EndTildePos &' ColzQ:Header='& ColzQ:Header&' ColzQ:HeadAlign='& ColzQ:HeadAlign )
    SELF.FmtParse_GetModifiers(FmtSpec,ColzQ:Mods,ColzQ:ModXFields)
    EXIT
!-----------------------------
ColumnzCls.FmtParse_WidthAlign PROCEDURE(CONST *STRING FmtSpec, CONST *STRING FmtTokn, <*STRING OutWidth>, <*STRING OutLRCD>)!,LONG,PROC !Return End Paren of ###L(##)
Numbers_Cnt  LONG
AlignOff_Len LONG
    CODE              !Should be ####L(###)     Column Width (offset)  if (0) is omitted sometimes
    Numbers_Cnt=SELF.FmtParse_NumberCnt(FmtSpec)
    IF Numbers_Cnt THEN 
       AlignOff_Len = Self.FmtParse_AlignOffset_Len(FmtTokn, Numbers_Cnt+1)  !Returns Length of L(##)
    END
    IF ~OMITTED(OutWidth) THEN OutWidth=SUB(FmtSpec,1,Numbers_Cnt).
    IF ~OMITTED(OutLRCD)  THEN OutLRCD =SUB(FmtSpec,  Numbers_Cnt+1, AlignOff_Len).
    RETURN Numbers_Cnt + AlignOff_Len 
!-----------------------------
ColumnzCls.FmtParse_AlignOffset_Len PROCEDURE(CONST *STRING FmtTokn, LONG AlignPos, BOOL IsGroup=0)!,LONG !Returns Length of L(##) or ](##)
LenOfAO  LONG
EndParen LONG,AUTO
    CODE              !Should be ####L(###)     Column Width (offset)  if (0) is omitted sometimes
    IF (~IsGroup AND INSTRING(SUB(FmtTokn,AlignPos,1),'LCRD') ) |      !Normal LCRD before (#)
    OR ( IsGroup AND SUB(FmtTokn,AlignPos,1)=']'              ) THEN   !Group can have ](Width) similar format to AO
       LenOfAO = 1 
       IF SUB(FmtTokn,AlignPos+1,1)='(' THEN                           !It has   '('
          EndParen=INSTRING(')',FmtTokn,1,AlignPos)                    !Is there ')' so (offset)
          IF EndParen THEN                                             !Yes its '()'
             LenOfAO = EndParen - AlignPos + 1
          END 
       END
    END
    RETURN LenOfAO
!----------------------------
ColumnzCls.FmtParse_NumberCnt PROCEDURE(CONST *STRING FmtSpec, <*STRING OutNumberString>)!,LONG
N LONG,AUTO
    CODE
    LOOP N=1 TO LEN(FmtSpec)
        IF ~CharNUMERIC(FmtSpec[N],N) THEN BREAK.
    END                         !FYI Clarion ends Loop at +1
    IF N THEN N -= 1.           !incase Len is zero then N=1 
    IF N AND ~OMITTED(OutNumberString) THEN 
       OutNumberString = FmtSpec[1: N]
    END
    RETURN N
!--------------------------
ColumnzCls.FmtParse_Between PROCEDURE(CONST *STRING FmtSpec, CONST *STRING FmtTokn,STRING LeftToken, STRING RightToken, <*LONG OutLeftPos>, <*LONG OutRightPos>)!,STRING,PROC
L LONG,AUTO
R LONG,AUTO
    CODE
    L=INSTRING(LeftToken,FmtTokn,1,1)
    IF L THEN R=INSTRING(RightToken,FmtTokn,1,L+1)  ELSE R=0 . 
    IF L+1 > R-1 THEN L=0 ; R=0.    !Was @@ 2 tokens then no return
    IF ~OMITTED(OutLeftPos)  THEN OutLeftPos = L.
    IF ~OMITTED(OutRightPos) THEN OutRightPos= R.
    IF L AND R AND R-1 >= L+1  THEN
       RETURN FmtSpec[L+1 : R-1]
    END
    RETURN ''
!-------------------------------
ColumnzCls.FmtParse_GetModifiers  PROCEDURE(STRING Fmt,*STRING outModz, *BYTE XFieldCnt)  !From WndPreview
!Column: 33R(2)|*~Rate~C(0)@n7.3b@ 
!Group]: (94)|FM~TRS Information~L(0)  Group End Starts with (width) ends with Align LCRD (Offset)
Modz    STRING(SIZE(Fmt)+9)
MCnt    USHORT
LenFmt  USHORT,AUTO
FX      SHORT,AUTO
MX      USHORT,AUTO
InThing   BYTE
ThingEnd  STRING(1),AUTO 
    CODE 
    Fmt=UPPER(UNQUOTE(Fmt))    !Fix Q''tip'' to be Q'Tip' ... could use Fmt Tokens, but this is more usable
    LenFmt=Len(Clip(Fmt))
    FX=STRPOS(Fmt,'[^0-9()LRCD]',0) - 1    !Bypass  ####(LRCD)
    IF FX < 0 OR FX > LenFmt THEN RETURN.
    LOOP WHILE FX < LenFmt AND MCnt < SIZE(Modz)
        FX += 1
        IF InThing THEN 
           IF Fmt[FX]<>ThingEnd THEN CYCLE.
           InThing=0 ; CYCLE
        END 
!FormatDelim1    STRING('''~(@#')       !~Heading~  (Offset#)  @picture@  #FieldNo#  quotes? for tool tip
!FormatDelim2    STRING('''~)@#')       !Note Q '' must be first to spot double
        CASE Fmt[FX]
        OF '~' OROF '''' OROF '@' ; InThing=1 ; ThingEnd=Fmt[FX] 
        OF '('                    ; InThing=1 ; ThingEnd=')'     
        OF '#'                    ; InThing=2 ; ThingEnd=Fmt[FX]  !Show the #
        OF 'L' OROF 'R' OROF 'C' OROF 'D' ; CYCLE  !Align after ~Header~ 
        OF ',' OROF ')' OROF '[' OROF ']' ; CYCLE  !Never posible
        OF '0' TO '9' ; CYCLE
        OF '<0>' TO '<32>' ; CYCLE  !No spaces, tabs, 13,10 or Low Ascii 
!        OF 'H'
!            IF INLIST(SUB(Fmt,FX,2),'HT','HB') THEN     !HT is Header Text color will
        END
        IF InThing=1 THEN CYCLE.
        CASE UPPER(Fmt[FX])
        OF '*' ; XFieldCnt += 4    !Color 4*LONG
        OF   'I' !Icon
        OROF 'J' !IconTrn
        OROF 'T' !Tree Level
        OROF 'Y' !Style Number
        OROF 'P' !Tool Tip
            XFieldCnt += 1
        END
        IF MCnt >= SIZE(Modz) THEN BREAK.
        MCnt += 1
        Modz[MCnt] = Fmt[FX]
    END
    ColzQ:Mods_Tip  = ''
    ColzQ:ModXF_Tip = ''
    LOOP MX=1 TO MCnt
         CASE SUB(Modz,MX,2)
         OF 'HT' OROF 'HB'
            ModQ:Lookup = SUB(Modz,MX,2) 
            Modz[MX+1]=LOWER(Modz[MX+1])     !Make it Ht Hb so "T" Not confused for Tree
         ELSE
            ModQ:Lookup = SUB(Modz,MX,1) 
         END
         GET(ModifierQ,ModQ:Lookup) ; IF ERRORCODE() THEN CYCLE.
         ColzQ:Mods_Tip=CLIP(ColzQ:Mods_Tip) & | 
                     CLIP(ModQ:Lookup) &' - '& CLIP(ModQ:Name) &' - '& CLIP(ModQ:PropFull) & |
                     '<13,10>'
    END
    XFieldCnt = 0
    LOOP FX=1 TO RECORDS(ModExtraQ)
        GET(ModExtraQ,FX)
        IF ~INSTRING(ModExQ:Char[1],Modz,1) THEN CYCLE.
        XFieldCnt += ModExQ:ExtraCnt
        ColzQ:ModXF_Tip=CLIP(ColzQ:ModXF_Tip) & | 
                     CLIP(ModExQ:Char) &' - '& CLIP(ModExQ:Desc) &'<13,10>'  
    END
    outModz = Modz 
    RETURN 
!----------------------------------------------- 
ColumnzCls.List:ColzQ_TakeEvent PROCEDURE() !Handle Event for LIST like Right click
    CODE
    GET(ColumnzQ,CHOICE(?LIST:ColumnzQ))
    IF ERRORCODE() THEN RETURN.
    CASE EVENT()
    OF EVENT:NewSelection
       CASE KEYCODE()
       OF MouseRight ; DO PopupColzRtn
       OF MouseLeft2 ; DO Click2ColzRtn
       END       
    END
    RETURN
PopupColzRtn ROUTINE
    SETKEYCODE(0)
    CASE POPUP('Copy #Field Variable|-|Copy Format() Source|Copy Format() Tokens|Copy Format() String|Copy Heading|-|Copy All Columns' & |
                '|-||Part Split TEXT Format() && Tokens|Part Split CODE  ')
    OF 1 ; SETCLIPBOARD(ColzQ:Variable)
    OF 2 ; SETCLIPBOARD(ColzQ:FmtSource)
    OF 3 ; SETCLIPBOARD(ColzQ:FmtTokn) 
    OF 4 ; SETCLIPBOARD(ColzQ:FmtString)
    OF 5 ; SETCLIPBOARD(ColzQ:Header)
    OF 6 ; Self.ColumnzQ_ToClipboard()

    OF 7 ; SETCLIPBOARD(       CLIP(ColzQ:FmtSource) &'<13,10>'&  CLIP(ColzQ:FmtTokn) )
    OF 8 ; SETCLIPBOARD('''' & CLIP(ColzQ:FmtSource) &'<<13,10>'& CLIP(ColzQ:FmtTokn) &'''')
    END
    EXIT
Click2ColzRtn ROUTINE
    DATA
Mods_Tip STRING(500)
ModXF_Tip STRING(500)
    CODE
    IF ColzQ:Mods_Tip THEN 
       Mods_Tip='<13,10>' & ColzQ:Mods_Tip
       ReplaceInto(Mods_Tip, '<13,10>','<13,10,9>')
       Mods_Tip='<13,10>Modifiers Help:' & Mods_Tip
    END 
    IF ColzQ:ModXF_Tip THEN 
       ModXF_Tip='<13,10>' & ColzQ:ModXF_Tip
       ReplaceInto(ModXF_Tip, '<13,10>','<13,10,9>')
       ModXF_Tip='<13,10>Modifier Xtra Queue Fields: ('& ColzQ:ModXFields &')'& ModXF_Tip
    END
    
    Message('List Column Information from the FORMAT()<13,10>={50}' & |  !Data for ColumnzQ '& ColzQ:ColNo & |
       MessagePipeFix(  |
          '<13,10>Column Number:  <9>' & CLIP(ColzQ:ColNo     ) & |     !1  ColzQ:ColNo           !STRING(6)  
          '<13,10>Field Number::<9>' & CLIP(ColzQ:FieldNo   ) & |     !2  ColzQ:FieldNo         !SHORT      
          '<13,10>Group Number::<9>' & CLIP(ColzQ:GroupNo   ) & |     !3  ColzQ:GroupNo         !SHORT      
          '<13,10>Variable:<9><9>' & CLIP(ColzQ:Variable  ) & |     !4  ColzQ:Variable        !STRING(96) 
   '<13,10><13,10>Header:  <9><9>' & CLIP(ColzQ:Header    ) & |     !4  ColzQ:Header          !STRING(32) 
          '<13,10>Head Align: <9>' & CLIP(ColzQ:HeadAlign ) & |     !8  ColzQ:HeadAlign       !STRING(5)  
   '<13,10><13,10>Picture: <9><9>' & CLIP(ColzQ:Picture   ) & |     !5  ColzQ:Picture         !STRING(32) 
          '<13,10>Data Width: <9>' & CLIP(ColzQ:Width     ) & |     !6  ColzQ:Width           !STRING(8)  
          '<13,10>Data Align: <9>' & CLIP(ColzQ:Align     ) & |     !7  ColzQ:Align           !STRING(5)  
   '<13,10><13,10>Modifiers:  <9>' & CLIP(ColzQ:Mods      ) & |     !9  ColzQ:Mods            !STRING(16) 
          '<13,10>Xtra Q Fields:<9>' & CLIP(ColzQ:ModXFields) & |     !9  ColzQ:ModXFields      !BYTE       
          '<13,10>Queue Number:<9>' & CLIP(ColzQ:QFieldX   ) & |     !   ColzQ:QFieldX         !SHORT      
          '<13,10>Col Number X:   <9>' & CLIP(ColzQ:ColX      ) & |     !12 ColzQ:ColX            !SHORT      
          '<13,10>In Group No:<9>' & CLIP(ColzQ:InGroup   ) & |     !13 ColzQ:InGroup         !SHORT      
    '<13,10><13,10>FORMAT(): ' & CLIP(ColzQ:FmtSource    ) & |     !11 ColzQ:FmtSource          !STRING(256)
            '<13,10>' & |
          CLIP(Mods_Tip) & |
          CLIP(ModXF_Tip) & |
          '')  ,'List Column '& ColzQ:ColNo) 
    EXIT 
!----------------------------------------------- 
ColumnzCls.ColumnzQ_ToClipboard    PROCEDURE() !Copy ColumnzQ to Clip
QX      LONG,AUTO
eTb     EQUATE('<9>')
ColzCB  ANY
    CODE
    ColzCB='ColNo <9>FieldNo <9>GroupNo <9>Variable <9>Header <9>Picture <9>Width <9>Align <9>HeadAlign <9>Mods <9>xM# ' & |
            '<9>Format Source <9>Format String <9>Format Tokens <9>ColX <9>QField <9>InGroup'
    LOOP QX=1 TO RECORDS(ColumnzQ)
        GET(ColumnzQ,QX)
        ColzCB=ColzCB & LEFT('<13,10>'& |
                CLIP(ColzQ:ColNo    ) & |
          eTb & CHOOSE(~ColzQ:FieldNo,'',''&ColzQ:FieldNo) & |
          eTb & CHOOSE(~ColzQ:GroupNo,'',''&ColzQ:GroupNo) & |
          eTb & CLIP(ColzQ:Variable ) & |
          eTb & CLIP(ColzQ:Header   ) & |
          eTb & CLIP(ColzQ:Picture  ) & |
          eTb & CLIP(ColzQ:Width    ) & |
          eTb & CLIP(ColzQ:Align    ) & |
          eTb & CLIP(ColzQ:HeadAlign) & |
          eTb & CLIP(ColzQ:Mods     ) & |
          eTb & CHOOSE(~ColzQ:ModXFields,'',''&ColzQ:ModXFields) & |  !BYTE Cnt
          eTb & CLIP(ColzQ:FmtSource   ) & |
          eTb & CLIP(ColzQ:FmtString) & |
          eTb & CLIP(ColzQ:FmtTokn  ) & |
          eTb & ColzQ:ColX            & |
          eTb & CHOOSE(~ColzQ:qFieldX ,'',''&ColzQ:qFieldX)  & |
          eTb & CHOOSE(~ColzQ:InGroup,'',''&ColzQ:InGroup) & |
          '')
    END        
    SETCLIPBOARD(ColzCB)
    RETURN      
!-----------------------------------------------
!EndRegion -- ColumnzCls Class
!================================================    
!Region -- HelpCls Class to load ModifierQ ModExtraQ. Setup Tips on some buttons
HelpCls.Init   PROCEDURE()

    CODE !( _Char,  _Name,  _Prop,  _Desc) 
    ?ModHelpBtn{PROP:Tip}=ModifierHelp
    ?ModHelp2Btn{PROP:Tip}=ModifierHelp
    ?ModHelp3Btn{PROP:Tip}=ModifierHelp
    ?ModHelp4Btn{PROP:Tip}=ModifierHelp
    ?ModHelp5Btn{PROP:Tip}=ModifierHelp
    ?ModHelpBtn_Colz{PROP:Tip}=ModifierHelp

    SELF.Add1Q('L'  ,'Align','PROPLIST:Left   ','Justification Left of Data'      ,'Left alignment of column Data. This may be offset by an (Indent).')
    SELF.Add1Q('R'  ,'Align','PROPLIST:Right  ','Justification Right of Data'    ,'Right alignment of column Data. This may be offset by an (Indent).')
    SELF.Add1Q('C'  ,'Align','PROPLIST:Center ','Justification Center of Data'  ,'Center alignment of column Data. This may be offset by an (Indent).')
    SELF.Add1Q('D'  ,'Align','PROPLIST:Decimal','Justification Decimal of Data','Decimal alignment of column Data. The offset to the decimal point is specified with an (Indent).')

    SELF.Add1Q('~L'  ,'Head' ,'PROPLIST:HeaderLeft'     ,'Justification Left of Heading'      ,'Left alignment of Heading text. This may be offset by an (Indent).'&      ' Appears in Format after ~Header Text~L(#). May be omitted if matches Data alignment and indent.')
    SELF.Add1Q('~R'  ,'Head' ,'PROPLIST:HeaderRight'    ,'Justification Right of Heading'    ,'Right alignment of Heading text. This may be offset by an (Indent).'&      ' Appears in Format after ~Header Text~R(#). May be omitted if matches Data alignment and indent.')
    SELF.Add1Q('~C'  ,'Head' ,'PROPLIST:HeaderCenter'   ,'Justification Center of Heading'  ,'Center alignment of Heading text. This may be offset by an (Indent).'&      ' Appears in Format after ~Header Text~C(#). May be omitted if matches Data alignment and indent.')
    SELF.Add1Q('~D'  ,'Head' ,'PROPLIST:HeaderDecimal'  ,'Justification Decimal of Heading','Decimal alignment of Heading text. Decimal point may be offset by an (Indent). Appears in Format after ~Header Text~D(#). May be omitted if matches Data alignment and indent.')

    SELF.Add1Q('~()' ,'Head','PROPLIST:HeaderLeftOffset :HeaderRightOffset :HeaderCenterOffset :HeaderDecimalOffset of [Group]' ,'Heading Indent from Justification', |
        'An optional (Indent) integer, enclosed in parentheses, that specifies in DLUs the indent from the justification. This may be negative. With Left justification indent defines a left margin ; with Right or Decimal, it defines a right margin; and with Center it defines an offset from the center of the field (negative = left offset).' & |
        ' Header Offset is specified after ~Heading~ and Alignment LRCD as (Offset).  May be omitted if matches Data alignment and indent.')    

    SELF.Add1Q('0-9','Align','PROPLIST:Width'       ,'Width DLUs of Cell or Group in Digits','Numbers in the FORMAT string outside delimiters ()~~@@## denote column width. Numbers are the delimiter that marks the beginning of a new column, as well as a "[" to mark the start of a Group.')
    SELF.Add1Q('()' ,'Align','PROPLIST:LeftOffset :RightOffset :CenterOffset :DecimalOffset' ,'Indent of Column Data from Justification', |
        'An optional (Indent) integer, enclosed in (parentheses), that specifies in DLUs the indent from the justification LRCD. This may be negative. With Left justification indent defines a left margin ; with Right or Decimal, it defines a right margin; and with Center it defines an offset from the center of the field (negative = left offset).')
    SELF.Add1Q('*'  ,'Color','PROPLIST:Color'       ,'Color of Cell  in Queue as 4x LONG'   ,'An asterisk "*" indicates color information a cell is contained in 4 LONG fields after the data field in the QUEUE. The four colors are Foreground, Background, Selected Foreground, and Selected Background (in that order in the queue).')
    SELF.Add1Q('B()','Color','PROPLIST:BarFrame'    ,'Color of Selection Bar Frame'         ,'A "B(color)" specifies the color of the selection bar frame.')
    SELF.Add1Q('E()','Color','PROPLIST:TextColor :BackColor :TextSelected :BackSelected','Color of Column and Cell Default','' & |
                                            'An "E(f,b,sf,sb)" specifies the column default colors (foreground, background, selected foreground, selected background). If the column also includes the * modifier the default colors are used for an cell when the Queue specifies COLOR:None.' & |
                                            '<13,10,13,10>The default column colors can be set or changed using the properties: PROPLIST:TextColor, PROPLIST:BackColor, PROPLIST:TextSelected and PROPLIST:BackSelected.')
    SELF.Add1Q('I'  ,'Icon','PROPLIST:Icon'        ,'Icon of Cell from Queue','' & |
                                             '"I" indicates an icon displays in the column, at the left edge of the column (prepended ' &|
                                             'to the data). An icon number is contained in a LONG after the data field in the QUEUE ' &|
                                             'that contains a number that refers to the list of icons set with {{PROP:IconList,#}=''~icon.ico'' runtime property.' &|
                                             '<13,10>' &|
                                             '<13,10>To display the icon only, and not the contents of the data field, make the display picture @P_PB.' &|
                                             '<13,10>' &|
                                             '<13,10>To detect a click on the icon check for PROPLIST:MouseDownZone = LISTZONE:Icon.')  ! Length = 462    
    SELF.Add1Q('J'  ,'Icon','PROPLIST:IconTrn'     ,'Icon (transparent) of Cell from Queue','A "J" indicates a transparent icon displays in the column. The same information for "I" applies to "J".')
    SELF.Add1Q('T()','Tree','PROPLIST:Tree'        ,'Tree Control  (1)(B)(L)(I)(R)','' & | 
                                             '"T()" indicates the LIST is a tree control. The tree level is contained in a LONG field ' &|
                                             'after the data field in the QUEUE. The expanded/contracted state of the tree level is ' &|
                                             'determined by the sign of the tree level LONG field''s value (positive value=expanded ' &|
                                             'and negative value=contracted).' &|
                                             '<13,10>' &|
                                             '<13,10>The optional (suppress) parameter can contain any of: 1 B L I R' &|
                                             '<13,10>' &|
                                             '<13,10>T(1) PROPLIST:TreeOffset to indicate the root is level number one not zero i.e. "One Based Tree"' &|
                                             '<13,10>T(B) PROPLIST:TreeBoxes to suppress expansion boxes' &|
                                             '<13,10>T(L) PROPLIST:TreeLines to suppress the connecting lines between all levels' &|
                                             '<13,10>T(I) PROPLIST:TreeIndent to suppress level indentation (also implicitly suppresses both Boxes and Lines)' &|
                                             '<13,10>T(R) PROPLIST:TreeRoot to suppress the connecting lines to the root level')  ! Length = 770    
    SELF.Add1Q('Y'  ,'Style','PROPLIST:CellStyle'   ,'Style of Cell in Queue as LONG'   ,'"Y" indicates a Style Number for the cell is contained in a LONG field after the data field in the QUEUE. The Style Number LONG refers to an entry in an array of styles associated with the LIST control defined through the {{PROPSTYLE:xxxx, Number} runtime properties. This overrides a Z() column default style.')
    SELF.Add1Q('Z()','Style','PROPLIST:ColStyle'    ,'Style default of Column in Format as (number)','"Z" followed by a Style number in (parens) sets the default style for an entire column.<13,10>Overriden by a "Y" Cell Style in the Queue (PROPLIST:CellStyle).<13,10>See PROPSTYLE: for more details.')
    SELF.Add1Q('~~' ,'Head' ,'PROPLIST:Header'      ,'Heading of Column ~Header~Justification(Indent)','A string enclosed in ~tildes~  displays the header at the top of the list. The header displays with the same alignment as the field data unless the tilde is followed by a header justification parameter LRCD (PROPLIST:HeaderLeft, :HeaderRight, :HeaderCenter, :HeaderDecimal) and/or indent value in (parentheses).')
    SELF.Add1Q('@@' ,'Data' ,'PROPLIST:Picture'     ,'Picture Format for Data as @picture@','The picture formats the field for display. The trailing @ is required to define the end of the picture, so that display pictures such as @N12~Kr~ can be used in the format string without creating ambiguity.')
    SELF.Add1Q('?'  ,'Flag' ,'PROPLIST:Locator'     ,'Locator Column'                   ,'' & |  
                                             'A question mark "?" defines the locator field for a COMBO or LIST box with a selector ' &|
                                             'field. For a drop-down multi-column list box, this is the value displayed in the current-selec' &|
                                             'tion box. Only one column can have the locator (?) modifier. If no column has the locator ' &|
                                             'modifier, the 1st column is the default.' &|
                                             '<13,10>' &|
                                             '<13,10>The locator column is used for the following purposes:' &|
                                             '<13,10>1. The value of the locator field of current record is copied to the LIST/COMBO''s ' &|
                                             'USE variable. ' &|
                                             '<13,10>2. The runtime library locates a record upon typing when the LIST has focus, or ' &|
                                             'the COMBO''s list is dropped down and the USE variable is changed. ' &|
                                             '<13,10>3. The value of the locator column of the current COMBO''s record is displayed ' &|
                                             'in the entry sub-control.')  ! Length = 719
                                             
    SELF.Add1Q('_'  ,'Flag','PROPLIST:Underline'   ,'Line Under Cell'                  ,'An underscore "_" underlines the field.')
    SELF.Add1Q('/'  ,'Flag','PROPLIST:LastOnLine'  ,'Last Column on Line for Group of a Multi-Line List ' ,'A slash "/" causes the next field to appear on a new line. Only used on a field in a Group [] to create a Multi-Line List.')
    SELF.Add1Q('|'  ,'Flag','PROPLIST:RightBorder' ,'Line on Right Side of Cell'       ,'A pipe "|" places a vertical line to the right of the field.')
    SELF.Add1Q('M'  ,'Flag','PROPLIST:Resize'      ,'Resizable Cell or Group'          ,'An "M" allows the column or group to be re-sized at runtime. This allows the user to drag the right vertical bar (if present) or right edge of the data area.')
    SELF.Add1Q('F'  ,'Flag','PROPLIST:Fixed'       ,'Fixed Column does not Scroll'     ,'An "F" creates a fixed column that stays on screen when the user horizontally pages through the fields (by the HSCROLL attribute). Fixed fields or groups must be at the start of the list. This is ignored if placed on a field within a group.')
    SELF.Add1Q('S()','Data' ,'PROPLIST:Scroll'      ,'Scroll bar on Column or Group'    ,'An S(integer) adds a scroll bar to the field or group. The (integer) defines the total dialog units to scroll. Ignored if field in group.')
    SELF.Add1Q('P'  ,'Tip'  ,'PROPLIST:Tip'         ,'Tool Tip for Cell in Queue as STRING' ,'A "P" adds a column tool tip. The tip text is in the next QUEUE field that follows the QUEUE data field. If the designated queue field is empty, the "Q" modifier designates a string value to use as a default tool tip.')
  SELF.Add1Q('Q''''','Tip'  ,'PROPLIST:DefaultTip'  ,'Tool Tip Column Default as ''String'' in FORMAT','A "Q" followed by a ''''string'''' designates the default column tip text. If "P" Column Tip is also specified the default tip shows when the Queue tip field is blank. Default tips can my the Format() unruly long so are best assigned at runtime.')
    SELF.Add1Q('#'  ,'Data' ,'PROPLIST:FieldNo'     ,'Field Number from Queue as #number#','A #number# enclosed in pound signs indicates the QUEUE field to display. Following fields in the format string without an explicit #number# are taken in order from the fields following the #number# field. For example, #2# on the first field in the format string indicates starting with the second field in the QUEUE, skipping the first. If the number of fields specified in the format string are >= the number of fields in the QUEUE, the format "wraps around" to the start of the QUEUE.')

    SELF.Add1Q('[]' ,'Group','PROPLIST:Group + '    ,'Group Columns '                     ,'"[]" indicate multiple columns grouped. A group may specify header text that spans the columns after the ending "]" using the syntax: ~header text~ Justification LRCD (Indent).' & |
        '<13,10><13,10>Groups can create a Multi-Line List by using the LastOnLine "/" modifier.' & |
        '<13,10><13,10>{{PROPLIST:GroupNo,Column} returns the group number of a column.' & |
        '<13,10><13,10>{{PROPLIST:GroupNo + PROPLIST:Group,Column} returns the number of columns in the group or zero if not a group.<13,10>')
    SELF.Add1Q('[]()' ,'Group','PROPLIST:Width of [Group]' ,'Group Width', |
        'An optional (Group Width) integer, enclosed in parentheses after the group closing square bracket, that specifies the width of the group (PROPLIST:Group + PROPLIST:Width). If omitted, the size is calculated from the enclosed fields.')    

    SELF.Add1Q(']~ ~' ,'Group','PROPLIST:Header of [Group]' ,'Group Heading Text', |
        'A string enclosed in ~tildes~  displays the header at the top of the Group. Heading is specified after [](width). Alignment may be specified after the second tilde as a header justification parameter LRCD and (Indent).')    

!    SELF.Add1Q('~~' ,'tttt','PROPLIST:Header'      ,'Heading of Column ~Header~Justification(Indent)','A string enclosed in ~tildes~  displays the header at the top of the list. The header displays with the same alignment as the field data unless the tilde is followed by a header justification parameter LRCD (PROPLIST:HeaderLeft, :HeaderRight, :HeaderCenter, :HeaderDecimal) and/or indent value in (parentheses).')
    
    SELF.Add1Q(']~()' ,'Group','PROPLIST:HeaderLeftOffset :HeaderRightOffset :HeaderCenterOffset :HeaderDecimalOffset of [Group]' ,'Group Heading Indent from Justification', |
        'An optional (Indent) integer, enclosed in parentheses, that specifies in DLUs the indent from the justification. This may be negative. With Left justification indent defines a left margin ; with Right or Decimal, it defines a right margin; and with Center it defines an offset from the center of the field (negative = left offset).' & |
        ' Group Header Offset is specified after []~Heading~ and Alignment LRCD e.g. [field-columns](group width)~Group Head~L(offset).')    

    SELF.Add1Q('HB()' ,'Head','PROPLIST:HdrBackColor','Color of Header Background'     ,'An "HB(color)" specifies the column header background color.')
    SELF.Add1Q('HT()' ,'Head','PROPLIST:HdrTextColor','Color of Header Text'           ,'An "HT(color)" specifies the column header text color. Confused by the templates as a Tree so best to assign at runtime.')
!    SELF.Add1Q('','prop','','')

!TODO add column for "Category or Type" (Header,Data,Flags,General,Style and Colors,Tree) from the See ListBoxFormatterPRJ.PNG so can sort by Cat? Must be Wider
!Header     HB HT S()   new TYPE of Group for []
!Data       # LRCD @
!Flags      F * /
    
    SORT(ModifierQ, ModQ:Sort) 
    GET(ModifierQ, 1) ; ?List:ModifierQ{PROP:Selected}=1
    SELF.Init2() 
    RETURN 
HelpCls.Add1Q   PROCEDURE(STRING _Char, STRING _Type, STRING _Prop, STRING _Name, STRING _Desc)  
P4D     STRING(128),AUTO
Cln     LONG,AUTO
LookUp2 STRING(2),AUTO
    CODE
    ModQ:Char = _Char
    ModQ:Name = _Name
    ModQ:Type = _Type
    ModQ:Prop = _Prop ; IF ModQ:Prop[1:8]='PROPLIST' THEN ModQ:Prop=SUB(ModQ:Prop,10,99).
    ModQ:Desc = _Desc  
    ModQ:Sort = LOWER(ModQ:Char) & FORMAT(RECORDS(ModifierQ)+1,@n02)   
    CASE ModQ:Sort[1]
    OF 'a' TO 'z' OROF '0' TO '9' 
    ELSE ; ModQ:Sort=' '&ModQ:Sort      !put special characters first
    END 
    P4D = _Prop
    LOOP 
        Cln=INSTRING(' :',P4D,1) ; IF ~Cln THEN BREAK.
        P4D=SUB(P4D,1,Cln-1) &'<13,10>PROPLIST:'& SUB(P4D,Cln+2,999)
    END 
    ModQ:PropFull = P4D 
!dbg    IF ~_DESC THEN  ModQ:Name='####### no desc ###' & _Name.
    ModQ:Desc =  CLIP(_Desc) & '<13,10>'& P4D
    
    CASE ModQ:Char
    OF '()' 
    OF '[]()'   ; LookUp2='](' 
    OF ']~()'   ; LookUp2='~(' 
    ELSE        ; LookUp2 = SUB(ModQ:Char,1,2)
    END
    IF    NUMERIC(LookUp2[1])                       THEN LookUp2   =''          !Numbers 0-9 
    ELSIF INLIST(LookUp2[2],'(','<39>','@','#','[') THEN LookUp2[2]=''          !Easy fix S( E(     
    ELSIF INLIST(LookUp2[1],'~','~')                THEN LookUp2=LookUp2[2]     !~L~R~C~D -> LRCD        
    ELSIF INLIST(LookUp2,'HT','HB')                 THEN    !HT HB Keep 2 bytes
    ELSIF LEN(CLIP(LookUp2))=1                      THEN    !Len=1 Keep 1 bytes 
    ELSE                                                    !Len=2 Keep 2 bytes
            !Keep 2 bytes
    END 
    ModQ:Lookup = LookUp2
    ADD(ModifierQ)
    RETURN
!------------------------------------------------
HelpCls.Add1ModExQ PROCEDURE(STRING _Char, BYTE _ExtraCnt, STRING _Desc)
    CODE
    CLEAR(ModExtraQ)
    ModExQ:Order    = RECORDS(ModExtraQ) +1
    ModExQ:Char     = _Char
    ModExQ:ExtraCnt = _ExtraCnt
    ModExQ:Desc     = _Desc
    ADD(ModExtraQ,ModExQ:Order)
    RETURN
!------------------------------------------------
HelpCls.Set1QDesc   PROCEDURE(STRING _Char, STRING _Desc) 
    CODE 
    ModQ:Char = _Char
    GET(ModifierQ, ModQ:Char) 
    ModQ:Desc =  CLIP(ModQ:Char) &'  '&  CLIP(ModQ:Prop) &'<13,10>'& _Desc
    PUT(ModifierQ)
    RETURN 
!------------------------------------------------
HelpCls.Init2   PROCEDURE()
ListFEQ LONG,AUTO
X       LONG,AUTO
QTip    CSTRING(500),AUTO
QProp   STRING(128),AUTO
    CODE 
    ?List:ModifierQ{PROP:LineHeight} = 1 + ?List:ModifierQ{PROP:LineHeight}
    ?List:ModifierQ{PROPSTYLE:FontName,1} = 'Consolas'
    ?List:ModifierQ{PROPSTYLE:FontSize,1} = 1 + ?List:ModifierQ{PROP:FontSize} 
    ?List:ModifierQ{PROPSTYLE:FontStyle,1} = FONT:Bold

    ?List:ModifierQ{PROPSTYLE:FontName,2} = 'Consolas'
    ?List:ModifierQ{PROPSTYLE:FontSize,2} = 1 + ?List:ModifierQ{PROP:FontSize} 
    ?List:ModifierQ{PROPSTYLE:FontStyle,2} = FONT:Regular

    self.Add1ModExQ('*', 4, 'Colors - 4 x LONG - PROPLIST:Color')
    self.Add1ModExQ('I', 1, 'Icon Index in {{Prop:IconList,#} - LONG - PROPLIST:Icon')
    self.Add1ModExQ('J', 1, 'Icon Index in {{Prop:IconList,#} - LONG - PROPLIST:IconTrn')
    self.Add1ModExQ('T', 1, 'Tree Level - LONG - PROPLIST:Tree')
    self.Add1ModExQ('Y', 1, 'Style Number for Cell - LONG - PROPLIST:CellStyle')
    self.Add1ModExQ('P', 1, 'Tool Tip for Cell - STRING - PROPLIST:Tip')

    HelpModOrder = 'Six modifiers require separate fields in the QUEUE to ' &|
            '<13,10>hold formatting data, the following is the order in ' &|
            '<13,10>which those fields MUST appear in the QUEUE:' &|
            '<13,10>-------------------------------------------------------' &|
     '<13,10>1. LIST Column data to          Various' &|
     '<13,10>2. * Color Foreground           Long PROPLIST:Color' &|
     '<13,10>3. * Color Background           Long PROPLIST:Color' &|
     '<13,10>4. * Color Selected Foreground  Long PROPLIST:Color' &|
     '<13,10>5. * Color Selected Background  Long PROPLIST:Color' &|
     '<13,10>6. I Icon in {{Prop:IconList,#}  Long PROPLIST:Icon' &|
     '<13,10>6. J Icon in {{Prop:IconList,#}  Long PROPLIST:IconTrn' &|
     '<13,10>7. T Tree Level                 Long PROPLIST:Tree' &|
     '<13,10>8. Y Style Number for Cell      Long PROPLIST:CellStyle' &|
     '<13,10>9. P Tool Tip for Cell          String PROPLIST:Tip' & |  ! Length = 729
     '<13,10>-------------------------------------------------------' &|
     '<13,10>Style Properties --  ?LIST{{ PROPSTYLE:xxx, Style# }' &|
     '<13,10>-------------------------------------------------------' &|
     '<13,10>PropStyle:BackColor     Background Color' &|
     '<13,10>PropStyle:BackSelected  Background Color Selected' &|
     '<13,10>PropStyle:BarFrame      Bar frame Color' &|
     '<13,10>PropStyle:CharSet       Font Character set' &|
     '<13,10>PropStyle:FontColor     Font Color ' &|
     '<13,10>PropStyle:FontName      Font Name' &|
     '<13,10>PropStyle:FontSize      Font Size' &|
     '<13,10>PropStyle:FontStyle     Font Style' &|
     '<13,10>PropStyle:Picture       Display Picture' &|
     '<13,10>PropStyle:TextColor     Text Color' &|
     '<13,10>PropStyle:TextSelected  Text Color Selected' &|
     '<13,10>' &|
     '<13,10>If BarFrame color matches the Selection Bar color, ' &|
     '<13,10>the focus rectangle is not drawn if List has focus.'  ! Length = 718

    HelpSyntax=' Column: Width Justification LRCD (Indent) Modifiers ~CellHead~ LRCD(IndentHead) @picture@ ' &|
        '<13,10> Group:  [Columns] (GroupWidth) Modifiers ~GroupHead~Justification(IndentHead)'
    HelpWidth = ' Width      PROPLIST:Width    Width of the Column or Group in DLUs' &|
     '<13,10> Justify L  PROPLIST:Left     Left align    (Indent) PROPLIST:LeftOffset' &|
     '<13,10> Justify R  PROPLIST:Right    Right align   (Indent) PROPLIST:RightOffset' &|
     '<13,10> Justify C  PROPLIST:Center   Center align  (Indent) PROPLIST:CenterOffset' &|
     '<13,10> Justify D  PROPLIST:Decimal  Decimal align (Indent) PROPLIST:DecimalOffset' &|
     '<13,10> Indent ()  PROPLIST:xOffset  Indent / Offset from Justification' &|
     '<13,10> Modifiers  PROPLIST:xxxxxxx  Characters (listed below) to modify display format'  ! Length = 509 

    ListFEQ = ?LIST:ColumnzQ
    IF ListFEQ{PROP:Hlp}<>'noQtip' THEN DO ListTipsModQRtn. 
    RETURN

ListTipsModQRtn ROUTINE     !Find Column PropList:DefaultTip like 'PL_Xxxx' found in ModifierQ and set Tip to its Name
    Loop X=1 To ListFEQ{PropList:Exists,0}
       QTip=ListFEQ{PROPLIST:DefaultTip, x}
       QProp=''
       CASE QTip  
       OF 'PL_GroupNo'    ; QProp='PROPLIST:GroupNo'  ; QTip='Group Number (if group)'
       OF 'PL_Width'      ; QProp='PROPLIST:Width'    ; QTip='Width of Column' 
       OF 'PL_Left'       ; QProp='PROPLIST:Left and LeftOffset'    
                                   QTip='Data Alignment and (Offset)' 
       OF 'PL_HeaderLeft' ; QProp='PROPLIST:HeaderLeft and HeaderLeftOffset'    ; 
                                   QTip='Header Alignment and (Offset) <13,10>If blank same as Data Align' 
       ELSE
            IF      SUB(QTip,1,3)='PL_' THEN 
                QProp = 'PROPLIST:' & SUB(QTip,4,30)  
            ELSIF   SUB(QTip,1,9)='PROPLIST:' THEN
                QProp = QTip 
            ELSE
                CYCLE 
            END
            MODQ:PropFull = QProp
            GET(ModifierQ,ModQ:PropFull)
            IF ERRORCODE() THEN CYCLE.
            QTip=CLIP(MODQ:NAME) 
       END 
       ListFEQ{PROPLIST:DefaultTip, x} = QTip & CHOOSE(~QProp,'','<13,10><13,10>'& QProp)
    END
    EXIT
!EndRegion -- HelpCls Class     

!================================================ 
!Region -- GenFmt Generate Format Class
GenFmt.SimpleGen PROCEDURE()
ColX USHORT,AUTO
Fmt  PSTRING(256)
DataIndent PSTRING(6)
HdrText    PSTRING(32)
HdrJustify PSTRING(2)
HdrIndent  PSTRING(6)
    CODE
    GenSim_Format='' ; GenSim_FIELDS='#FIELDS('
    IF ~GenSim:Columns THEN GenSim:Columns=5. ; IF GenSim:Columns > 40 THEN GenSim:Columns=40.
    IF ~GenSim:Width THEN GenSim:Width=80.    ; IF GenSim:Width > 300 THEN GenSim:Width=300.
    IF ~GenSim:JustLCR THEN GenSim:JustLCR='L'.
    GenSim:Picture=LEFT(GenSim:Picture)
    IF GenSim:Picture[1]='@' THEN GenSim:Picture=LEFT(SUB(GenSim:Picture,2,99)).
    DataIndent=CHOOSE(GenSim:JustLCR   ='C','(0)','('&GenSim:Indent&')')    !Center Indent Zero
    HdrIndent =CHOOSE(GenSim:HdrJustLCR='C','(0)','('&GenSim:HdrIndent&')')
    HdrJustify=GenSim:HdrJustLCR
    IF HdrJustify=GenSim:JustLCR AND HdrIndent=DataIndent THEN !If Header Same as DATA ?
!No    HdrJustify='' ; HdrIndent=''                            ! then no need for Header Just
!This way when split all the Justify are there to edit and make some L(2) C(0)
    END
!No IF DataIndent='(0)' THEN DataIndent=''.
    HdrText=CLIP(GenSim:HeaderText)
    LOOP ColX=1 TO GenSim:Columns
        Fmt=GenSim:Width & |
            GenSim:JustLCR & DataIndent & |
            CHOOSE(~GenSim:RightBorder,'','|') & |
            CHOOSE(~GenSim:Underline  ,'','_') & |
            CHOOSE(~GenSim:Fixed      ,'','F') & |
            CHOOSE(~GenSim:Resize     ,'','M') & |
            CHOOSE(~GenSim:Colored    ,'','*') & |
            CHOOSE(~GenSim:CellStyle  ,'','Y') & |
            CHOOSE(~HdrText ,'','~'& HdrText &'_'& ColX &'~'& HdrJustify & HdrIndent) & |
            CHOOSE(~GenSim:Picture OR lower(GenSim:Picture)='none','','@'& CLIP(GenSim:Picture) &'@') & |
            CHOOSE(~GenSim:FieldNumbered,'','#'& ColX &'#') & |
            CHOOSE(~GenSim:OnePerLine,'','<13,10>')
        GenSim_Format = CLIP(GenSim_Format) & Fmt
        GenSim_FIELDS = CLIP(GenSim_FIELDS) & CHOOSE(ColX=1,'',', ') &'Field_'& ColX
    END
    GenSim_FIELDS = CLIP(GenSim_FIELDS) & ')'
    DISPLAY
    RETURN
!---------------------------
GenFmt.SimplePreviewBtn PROCEDURE()
    CODE
    IF ~GenSim_Format THEN GenFmt.SimpleGen().
    START(PreviewFormatWindow,,No1310(GenSim_Format),'Simple Format Gen')
    RETURN
GenFmt.SimpleParseBtn   PROCEDURE()
    CODE
    IF ~GenSim_Format THEN GenFmt.SimpleGen().
    ListControl=' LIST,AT(4,4),USE(?List:Que),FROM(Que),VSCROLL,VCR, |' & |
                '<13,10> FORMAT(''' & QUOTE(No1310(GenSim_Format)) &''') ,|' & |
                '<13,10> ' & GenSim_FIELDS
    SELECT(?TabInput)
    POST(EVENT:Accepted,?ProcessBtn)
    RETURN
!----------------------------------
GenFmt.SimpleLoadConfig PROCEDURE()
    CODE
    GenFmt_Simple=GenFmt_Simple_Defaults
    GenFmt.ConfigGetPut(1,'GenSimple',GenFmt_Simple)
    RETURN
GenFmt.ConfigGetPut PROCEDURE(BYTE Get1_Put2, STRING CfgSection, *GROUP ConfigGrp)
FldX    LONG,AUTO
CfgWho  CSTRING(32),AUTO
CfgAny  ANY
ConfigIni STRING('.\ListFmtConfig.INI')
    CODE
    LOOP FldX=1 TO 99
         CfgWho=WHO(ConfigGrp,FldX) ; IF ~CfgWho THEN BREAK.
         CfgAny &= WHAT(ConfigGrp,FldX)
         IF CfgAny &= NULL THEN Message('CfgAny &= NULL ' & FldX) ; CYCLE.
         IF Get1_Put2 = 2 THEN
            PUTINI(CfgSection,CfgWho,CfgAny,ConfigIni)
         ELSE
            CfgAny = GETINI(CfgSection,CfgWho,CfgAny,ConfigIni)
         END
    END
    RETURN
!----------------------------------
GenFmt.CopyFormatBtn    PROCEDURE(LONG FeqFmt, LONG FeqFields, BOOL ReturnCode=0)!,STRING,PROC
FmtCB ANY
CL CSTRING(1025),AUTO 
LX USHORT,AUTO 
    CODE
    IF FeqFmt THEN
       FmtCB=' FORMAT(' 
       DO GetFormatRtn
       FmtCB = FmtCB & ')'
    END
    IF FeqFields THEN
       FmtCB=CHOOSE(~FmtCB,'',FmtCB&', |<13,10>') &' '& CLIP(CONTENTS(FeqFields))
!       FmtCB=CHOOSE(~FmtCB,'',FmtCB&', |<13,10>') &' #FIELDS('
!       DO GetFieldsRtn
!       FmtCB = FmtCB & ')'
    END
    IF ReturnCode THEN 
       RETURN FmtCB
    END 
    SETCLIPBOARD(FmtCB &'<13,10>')
    RETURN ''
GetFormatRtn ROUTINE
    LOOP LX=1 TO FeqFmt{PROP:LineCount}
         CL=CLIP(FeqFmt{PROP:Line,LX}) ; IF ~CL AND LX>1 THEN CYCLE.
         IF LX>1 THEN FmtCB=FmtCB &' &|<13,10> {8}'.
         FmtCB=FmtCB &''''& QUOTE(CLIP( CL )) &''''
    END
    EXIT
GetFieldsRtn ROUTINE  !TODO Split into lines? use above code
!    LOOP LX=1 TO FeqFields{PROP:LineCount}
!         CL=CLIP(FeqFields{PROP:Line,LX}) ; IF ~CL AND LX>1 THEN CYCLE.
!         IF LX>1 THEN FmtCB=FmtCB &' &|<13,10> {9}'.
!         FmtCB=FmtCB &CLIP( CL )
!    END
    EXIT   
!----------------------------------
GenFmt.CopyWindowAndListBtn  PROCEDURE(LONG FeqFmt, LONG FeqFields)  !01/05/22 Generate WINDOW + LIST + Format
CodeCB ANY
FormatCode ANY
FieldsCode ANY
ListWidth  LONG
_ListUSE_  PSTRING(96)
    CODE
    FormatCode=GenFmt.CopyFormatBtn(FeqFmt,0         , True)
    FieldsCode=GenFmt.CopyFormatBtn(0     , FeqFields, True)
    ListWidth = GenQue_Width + 10       !10=VScroll Width
    _ListUSE_ = '?LIST:'& GenQue_Name   !dup in USE(?List below

    CodeCB =    '!ViewQueue_' & GenQue_Name &' PROCEDURE()' & |
         '<13,10>!ViewQueue_' & GenQue_Name &' ROUTINE <13,10>!   DATA <13,10>' & |
    '<13,10>ListWindow WINDOW(''LIST Queue '& GenQue_Name & '''),' & |
                  'AT(,,' & 4+ListWidth & ',154),GRAY,SYSTEM,FONT(''Segoe UI'',10),RESIZE  ! MDI CENTER ICON(ICON:Clarion) ' &|
    '<13,10>' &|
    '<13,10>! LIST,AT(2,2 ), FULL, hVSCROLL,VCR, | ! ' &|
    '<13,10>  LIST,AT(2,2,' & ListWidth & ',150),hVSCROLL,VCR, | ' &|
    '<13,10>       USE('&       _ListUSE_   &' ), | ' &|
    '<13,10>        FROM(    '& GenQue_Name &' ), | ' &|
    '<13,10>' & FormatCode & ' ! ,| ' &|
    '<13,10>! '& FieldsCode & ' <13,10>' &|     !Expect to use this for hand code so comment ! #fields
    '<13,10>    END' &|
    '<13,10>    ' &|
    '<13,10>    CODE' &|
    '<13,10>  OPEN(ListWindow)' &|
    '<13,10>  ACCEPT ' &|
    '<13,10>    CASE EVENT()' &|
    '<13,10>    END' &|
    '<13,10>    CASE ACCEPTED()' &|
    '<13,10>    OF ' & _ListUSE_ &|
    '<13,10>       GET(' & GenQue_Name &',CHOICE(' & _ListUSE_ &'))' & |
    '<13,10>    END' &|
    '<13,10>    CASE FIELD()' &|
    '<13,10>    OF ' & _ListUSE_ &|
    '<13,10>       GET(' & GenQue_Name &',CHOICE(' & _ListUSE_ &'))' & |
    '<13,10>       CASE EVENT()' &|
    '<13,10>       OF EVENT:NewSelection' & |
    '<13,10>          ! CASE KEYCODE()' & |     !??? Event:Accepted gets these the same, is simpler code, so use that ???
    '<13,10>          ! OF MouseLeft2' & |
    '<13,10>          ! OF MouseRight' & |
    '<13,10>          ! END' & |
    '<13,10>       END' &|
    '<13,10>    END' &|
    '<13,10>  END !ACCEPT' &|
    '<13,10>  CLOSE(ListWindow)' &|
    '<13,10>' & |
    '<13,10>!LOOP QNdx=1 TO RECORDS('& GenQue_Name &')  ! TO 1 BY -1' & |   !Gen Common Looping Code
    '<13,10>!   GET('& GenQue_Name &',QNdx)' & |
    '<13,10>!   !PUT DELETE ('& GenQue_Name &')' & |
    '<13,10>!END' & |
    '<13,10>'
    SETCLIPBOARD(CodeCB)
    RETURN
!----------------------------------
GenFmt.CopyFieldsEqualBtn PROCEDURE() !01/05/22 Generate Queue.Field= for all fields
CodeCB ANY
QX USHORT,AUTO
LenFld USHORT,AUTO
AField PSTRING(256)
MaxLen USHORT 
CodeType BYTE,AUTO
    CODE
    CodeType=PopupUnder(?,'PreQ:Field = |PreQ:Field = 2_PreQ:Field |-|Field LIKE(PreQ:Field)|NewPre::Field LIKE(PreQ:Field)')
    IF ~CodeType THEN RETURN.
    LOOP QX=1 TO RECORDS(GQFieldsQ)
        GET(GQFieldsQ,QX)
        LenFld=LEN(CLIP(GQFldQ:Pre_Label))
        IF LenFld > MaxLen THEN MaxLen = LenFld.
    END

    CASE CodeType 
    OF 1 ; CodeCB = 'CLEAR(' & GenQue_Name &')' !    ! FREE(' & GenQue_Name &')'
    OF 2 ; CodeCB = 'CLEAR(1__' & GenQue_Name &')'
    OF 3 ; CodeCB = 'Like_' & GenQue_Name &'  QUEUE GROUP , PRE(1_Pre)'
    OF 4 ; CodeCB = 'Like_' & GenQue_Name &'  QUEUE GROUP , PRE()     !For IDE that can have trouble with PRE(xxx) e.g. BRw1::xxx'
    END    
    LOOP QX=1 TO RECORDS(GQFieldsQ)
        GET(GQFieldsQ,QX)
        LenFld=LEN(CLIP(GQFldQ:Pre_Label))  
        IF ~LenFld THEN CYCLE.
        AField = GQFldQ:Pre_Label[1 : LenFld] & ALL(' ',MaxLen-LenFld) 
        CASE CodeType 
        OF 1 ; AField = AField &' = '
        OF 2 ; AField = '1_'& AField &' = 2_'& AField           ! 1_Pre:xxx = 2_Pre:xxx
        OF 3 ; AField = CLIP(GQFldQ:Label) & ALL(' ',MaxLen-LenFld) &' LIKE('& GQFldQ:Pre_Label[1 : LenFld] & ALL(' ',MaxLen-LenFld) &')' 
        OF 4 ; AField = '1_Pre::' & CLIP(GQFldQ:Label) & ALL(' ',MaxLen-LenFld) &' LIKE('& GQFldQ:Pre_Label[1 : LenFld] & ALL(' ',MaxLen-LenFld) &')' 
        END
        IF GQFldQ:OmitHow OR UPPER(GQFldQ:Type)='GROUP' THEN 
           AField = AField & '    ! ' & GQFldQ:OmitHow &'  '& GQFldQ:Type
        END 
        CodeCB = CodeCB & CLIP('<13,10>' & AField )
    END
    CASE CodeType 
    OF 1   ; CodeCB = CodeCB & '<13,10>!ADD('& GenQue_Name & ') '
    OF 2   ; CodeCB = CodeCB & '<13,10>!ADD(1__'& GenQue_Name & ') '
    OF   3 
    OROF 4 ; CodeCB = CodeCB & '<13,10> {5}END<13,10>'
    END 
    CASE CodeType 
    OF 1 OROF 2    
       CodeCB = CodeCB & |
       '<13,10>' & |
       '<13,10>!LOOP QNdx=1 TO RECORDS('& GenQue_Name &')  ! TO 1 BY -1' & |   !Gen Common Looping Code
       '<13,10>!   GET('& GenQue_Name &',QNdx)' & |
       '<13,10>!   !PUT DELETE ('& GenQue_Name &')' & |
       '<13,10>!END' & |
       '<13,10>'
    END    
    SETCLIPBOARD(CodeCB)
    RETURN    
!----------------------------------
GenFmt.PictureAccepted PROCEDURE(LONG FEQ, STRING DorT)
Pic STRING(40),AUTO
InX SHORT,AUTO
TipLong LONG
    CODE
    Pic=CONTENTS(FEQ)
    IF Pic[1]='@' THEN Pic=LEFT(SUB(Pic,2,99)).
    InX=LEN(CLIP(Pic))
    IF Inx AND Pic[InX]='@' THEN Pic[InX]=''.   !he typed @n22@ with trailing @
    CASE DorT
    OF 'D' ; IF ~Pic THEN Pic='d17'. ; TipLong=DATE(01,22,3333)          !; TipLong=TODAY()
    OF 'T' ; IF ~Pic THEN Pic='t7' . ; TipLong=DEFORMAT('01:23:45',@t4)  !; TipLong=CLOCK()
    END
    IF ~Pic THEN RETURN.
    CHANGE(FEQ,Pic)
    IF TipLong THEN FEQ{PROP:Tip}='  @'&CLIP(Pic) &'<13,10>  '& FORMAT(TipLong, '@' & Pic)&'  '.
    RETURN
!----------------------------------
GenFmt.PicturePopup PROCEDURE(LONG FEQ, STRING DorT)
NewPic STRING(20)
PPic PSTRING(20)
PX SHORT,AUTO
FmtLong LONG
FmtStr  STRING(96),AUTO
S1      &STRING
X       USHORT,AUTO
NowLong LONG,AUTO
PupTxt ANY
PupNo  BYTE
PupPic STRING(8),DIM(19)    !@Picture for each Pop Item
BtnX LONG,AUTO
BtnY LONG,AUTO
DateZero  PSTRING(2),STATIC
DateSlash PSTRING(2),STATIC
DateBlank PSTRING(2),STATIC
TimeZero  PSTRING(2),STATIC
TimeColon PSTRING(2),STATIC
TimeBlank PSTRING(2),STATIC
MrkDn ANY      
GenMD EQUATE(0)  !Gen Markdown Table of Pictures for my GitHub
    CODE
    MrkDn='| Picture | Format | Example |<13,10>|--|--|--|'
    GETPOSITION(?,BtnX,BtnY)
    LOOP
       NewPic=''
       CASE DorT
       OF 'D' ; DO DateRtn
       OF 'T' ; DO TimeRtn
       END
    WHILE NewPic='?'
    IF NewPic THEN
       CHANGE(FEQ,NewPic)
       POST(EVENT:Accepted,FEQ)
    END
    IF GenMD THEN SETCLIPBOARD(MrkDn &'<13,10,13,10>'& PupTxt) ; Message('MarkDown on Clip','GenMD').
    RETURN
DateRtn ROUTINE
    FmtLong=DATE(11,22,3333)
    PupTxt='Date Modifiers{{' & |
               'Leading{{Space|Zero}' & |
              '|Separator{{Slash <9>/|Period <9>.|Comma <9>,|Hyphen <9>-|Space <9>_}' & |
              '|Blank{{00/00/00|Blank <9>B}' & |
            '}|-' 
    NowLong=TODAY() ; IF GenMD THEN NowLong=DATE(12,25,2021).
    LOOP PX=1 TO 19
        pPic='@d' & DateZero & PX & DateSlash  & DateBlank
        IF PX=19 THEN  !12/15/21 show @n with Date Serial Number last
           pPic='@n_6'  
           FmtStr='-|Date Serial<9>'&NowLong
        ELSIF PX=4 OR PX=18 THEN 
           FmtStr=FORMAT(TODAY(),pPic)
           MrkDn=MrkDn &'<13,10>| '& pPic &' | Long Date | '& FORMAT(NowLong,pPic) &' |'
        ELSE
           FmtStr=FORMAT(FmtLong,pPic)
           DO mmddyyyyRtn
           MrkDn=MrkDn &'<13,10>| '& pPic &' | '& CLIP(FmtStr) &' | '& FORMAT(NowLong,PPic) &' |'
           IF LEN(CLIP(FmtStr))<=5 THEN FmtStr=CLIP(FmtStr) &'<9>'. !mm/yy and yy/mm need extra tab to align
           FmtStr=CLIP(FmtStr) &'<9>'& FORMAT(NowLong,PPic)
        END
        PupTxt=PupTxt & CHOOSE(Px=17,'|-|','|') &  CLIP(FmtStr) &'<9>'& pPic !&' = '& CLIP(FORMAT(FmtLong,PPic))
        PupPic[PX]=pPic
    END
    PupNo=POPUP(PupTxt,BtnX,BtnY,1)
    IF ~PupNo THEN EXIT.
    CASE PupNo
    OF 1 TO 2 ; DateZero =CHOOSE(PupNo,'','0') ; NewPic='?' ; EXIT 
    OF 3 TO 7 ; DateSlash=CHOOSE(PupNo-2,'','.','`','-','_') ; NewPic='?' ; EXIT
    OF 8 TO 9 ; DateBlank=CHOOSE(PupNo-7,'','b') ; NewPic='?' ; EXIT
    END    
    NewPic=PupPic[PupNo-9] !12/15/21 was '@d' & DateZero & PupNo-9 & DateSlash & DateBlank
    EXIT

mmddyyyyRtn ROUTINE
    LOOP X=1 TO LEN(CLIP(FmtStr))
        S1 &= FmtStr[X]
        CASE S1
        OF '1' ; S1='m'
        OF '2' ; S1='d'
        OF '3' ; S1='y'
        ELSE
            IF    IsUPPER(S1) THEN S1='M'   !OF 'A' TO 'Z' ; S1='M'  !'NOV'
            ELSIF IsLower(S1) THEN S1='m'   !OF 'a' TO 'z' ; S1='m'  !'Nov'
            END
        END
   END
   EXIT

TimeRtn ROUTINE 
    FmtLong=DEFORMAT('11:22:33',@t4)
    PupTxt='Time Modifiers{{' & |
               'Leading{{Space|Zero}' & |
              '|Separator{{Colon <9>:|Period <9>.|Comma <9>,|Hyphen <9>-|Space <9>_}' & |
              '|Blank{{00:00:00|Blank <9>B}' & |
            '}|-'
    NowLong=CLOCK()  ; IF GenMD THEN NowLong=4529601.   ! 372301=1:02:03am  4319901=11:59:59 4529601=12:34:56 4692301=1:02:03pm
    LOOP PX=1 TO 9
        IF PX=9 THEN    
           pPic='@n9'  
           FmtStr='-|Time Serial'
        ELSE
           pPic='@t' & TimeZero & PX & TimeColon  & TimeBlank
           FmtStr=FORMAT(FmtLong,PPic)
           DO hhmmssRtn
           MrkDn=MrkDn &'<13,10>| '& pPic &' | '& CLIP(FmtStr) &' | '& FORMAT(NowLong,PPic) &' |'
           IF LEN(CLIP(FmtStr))<=6 THEN FmtStr=CLIP(FmtStr) &'<9>'. !some need extra tab to align           
        END
        FmtStr=CLIP(FmtStr) &'<9>'& FORMAT(NowLong,PPic)
        PupTxt=PupTxt & CHOOSE(Px=7,'|-|','|') & CLIP(FmtStr) &'<9>'& pPic !&' = '& CLIP(FORMAT(FmtLong,PPic))
        PupPic[PX]=pPic
    END 
    PupNo=POPUP(PupTxt,BtnX,BtnY,1)
    IF ~PupNo THEN EXIT.
    CASE PupNo
    OF 1 TO 2 ; TimeZero =CHOOSE(PupNo,'','0') ; NewPic='?' ; EXIT
    OF 3 TO 7 ; TimeColon=CHOOSE(PupNo-2,'','.','`','-','_') ; NewPic='?' ; EXIT
    OF 8 TO 9 ; TimeBlank=CHOOSE(PupNo-7,'','b') ; NewPic='?' ; EXIT
    END     
    NewPic=PupPic[PupNo-9] !12/15/21 was '@t' & TimeZero & PupNo-9 & TimeColon & TimeBlank
    EXIT
hhmmssRtn ROUTINE
    LOOP X=1 TO LEN(CLIP(FmtStr))
        S1 &= FmtStr[X]
        CASE S1
        OF '1' ; S1='h'
        OF '2' ; S1='m'
        OF '3' ; S1='s'
        OF 'A' ; S1='X' !AM/PM
        END
   END
   EXIT
!================================================
GenFmt.QueueTextParse PROCEDURE()   !Parse GenQue_TextQ TEXT into GQFieldsQ Queue
TxtLineNo USHORT,AUTO
QX      SHORT,AUTO
XX      SHORT,AUTO
BangX   SHORT,AUTO
dDigits SHORT,AUTO
dDecimals SHORT,AUTO
Space USHORT,AUTO
Comma USHORT,AUTO
Paren1 LONG,AUTO
Paren2 LONG,AUTO
ALine STRING(256),AUTO
PPart STRING(255),AUTO
TPart STRING(255),AUTO
TPartCO CSTRING(256),AUTO
NamePart STRING(255),AUTO
NamePic  STRING(64),AUTO
FType LIKE(GQFldQ:Type),AUTO
Label_UPR STRING(64),AUTO
FTypeOrig LIKE(GQFldQ:Type),AUTO
LikeWarned BYTE
GQF_Prefix LIKE(GenQue_Pre)  !Normally from line 1 Queue/File but can change if 2nd file
BracketOpening  LIKE(GQFldQ:Bracket)  !is '[heading'
BracketOpenQueX SHORT                 !GQFieldsQ Index
    CODE
    FREE(GQFieldsQ)
    IF ~GenQue_TextQ THEN 
        ?GenQue_TextQ{PROP:Background}=Color:Yellow
        SELECT(?GenQue_TextQ) 
        RETURN
    END
    ?GenQue_TextQ{PROP:Background}=Color:None
    GenQue_Name='' ; GenQue_Pre=''
    NoTabs(GenQue_TextQ)
!fix for spaces?    FlattenCls.Flatten(GenQue_TextQ) ; DISPLAY  !TODO move to accepted?

    QX=-1
    LOOP TxtLineNo=1 TO ?GenQue_TextQ{PROP:LineCount}
        ALine=?GenQue_TextQ{PROP:Line,TxtLineNo}
        IF LEFT(ALine,1)='!' THEN ALine=LEFT(ALine).                !Put !Comment in Column 1 for ![
        DO TakeBracketGroupInAlineRtn
        IF ALine[1]='' OR LEFT(ALine,2)='. ' OR LEFT(ALine,1)='!' |
        OR UPPER(LEFT(ALine,4))='END ' THEN CYCLE.
        IF QX = -1 THEN
           DO TakeFirstLineRtn
        ELSE
           DO TakeFieldLineRtn
        END
    END
    IF BracketOpenQueX THEN
       ALine=']Left Open' ; DO TakeBracketGroupInAlineRtn
    END
    DISPLAY
    RETURN

TakeFirstLineRtn ROUTINE
    DO TakeFieldLineRtn 
    CASE UPPER(GQFldQ:Type)
    OF 'FILE'  ; GenQue_IsFILE=1
    OF 'QUEUE' ; GenQue_IsFILE=0
    ELSE
       TxtLineNo=999
       SELECT(?GenQue_TextQ)
       MESSAGE('The First line must be the "Label QUEUE" or FILE.' & |
               '||The Prompt says "Queue Declaration" ?|Oh no! Seven years of college down the drain!' & |
               '||' & ALine,'Parse - No QUEUE or FILE')
       EXIT
    END    
    QX=0
    GenQue_Name = CLIP(GQFldQ:Label)
    GenQue_Pre  = CLIP(GQFldQ:Label) & '.'
    IF InBetween('PRE(',')',TPart,Paren1,Paren2,PPart) >=0 THEN     !Find PRE(xxxx)
       IF PPart THEN GenQue_Pre=CLIP(PPart) & ':'.                  !Not PRE()
!    ELSIF ~GenQue_IsFILE THEN             !For FILE is no PRE() do NOT use Dot Syntax
!        GenQue_Pre = CLIP(GQFldQ:Label) & '.'   !No PRE() use Dot. Syntax
    END
    GQF_Prefix = GenQue_Pre
    EXIT

TakeFieldLineRtn ROUTINE
    CLEAR(GQFieldsQ)
    GQFldQ:FieldNo = QX+1
    GQFldQ:Line = ALine
    Space=INSTRING(' ',ALine,1)
    GQFldQ:Label=SUB(ALine,1,Space) ; Label_UPR=UPPER(GQFldQ:Label)
    GQFldQ:Pre_Label = GQF_Prefix & GQFldQ:Label

    TPart=LEFT(SUB(ALine,Space,999))    !ALine now TYPE()
    IF ~TPart THEN EXIT.                !No Type ???? just LABEL 
    
    !See: https://clarionhub.com/t/a-proposed-convention-for-the-extended-use-of-the-name-attribute/2792?u=carlbarnes
    !     A Proposed Convention for the Extended use of the Name Attribute -- 10.8 Picture Formats - ... attribute that starts with the @ character.
    !     
    XX=STRPOS(TPart,'NAME *(.*\| *@',True)          !Does it have NAME('name | @picture | extended') ?
    IF XX THEN
       NamePart=SUB(TPart,XX,999)
       XX=STRPOS(NamePart,'\| *@')                  !Find |@picture inside NAME()
       NamePic=LEFT(SUB(NamePart,XX+1,99))          !Keep  @pic ...|')
       XX=INSTRING('|',NamePic)                     !Find end Pipe |
       IF ~XX THEN XX=INSTRING('''',NamePic).       !No Pipe find last Quote
       IF XX THEN NamePic=SUB(NamePic,1,XX-1).      !Cutoff to leave just @pic
       GQFldQ:BangPic='!'&NamePic                   !Format as !@pic
    END                                             !Note: a !@pic below will superceded this

    LOOP 4 TIMES                      !Allow !Hide or !Omit and !@Picture
       BangX=INSTRING('!',TPart,1)    !A comment of !@xxxx overrides my Picture
       IF ~BangX THEN BREAK.
       CASE lower(SUB(TPart,BangX,6))
       OF '!@omit' OROF '!omit' ; GQFldQ:OmitHow=eOmit_Omit
       OF '!@hide' OROF '!hide' ; GQFldQ:OmitHow=eOmit_Hide
       ELSE 
          IF TPart[BangX : BangX+1]='!@'  THEN 
             GQFldQ:BangPic=SUB(TPart,BangX,32)              !so = !@xxxx            
             IF UPPER(GQFldQ:BangPic[1:3])='!@P' THEN 
                XX=INSTRING(GQFldQ:BangPic[3],GQFldQ:BangPic,1,4)   !@P ends at P
                IF ~XX THEN XX=INSTRING(' ',GQFldQ:BangPic,1,1).
             ELSE
                XX=INSTRING(' ',GQFldQ:BangPic,1,1)       !@x ends at space, yah could have "@n12~% ~" with space, sorry
             END
             IF ~XX THEN XX=12.
             GQFldQ:BangPic = SUB(GQFldQ:BangPic,1,XX)    !Trim after !@picture  Trim Junk Here
          END !if !@
       END !case
       TPart[BangX]=' '  !Remove ! Bang or will find again 
    END

    ParserCls.MakeCodeOnlyLine(TPart,TPartCO)
    GQFldQ:TypeCode=TPartCO        !e.g.  DECIMAL(9,2,) or
 !TODO Smash Aline or TPart
    IF INSTRING('OVER(',UPPER(TPartCO),1) THEN 
       Message('A Field appears to be an OVER() on line ' & TxtLineNo & |
               '. Change that to not have an OVER().' & |
               '||John Blutarsky:|<9>"Over? Did you say OVER? Nothing is over until we decide it is!|<9>Was it over when the Germans bombed Pearl Harbor? Hell no!..."' & |
               '||Line: ' & ALine,'Queue Parse -- OVER Hello No!',Icon:Asterisk)        
    END 
    FType='?'
    Space=STRPOS(TPart,'[ ,(!]') !End of TYPE,xxx or TYPE( or TYPE xxx
    IF Space THEN FType=SUB(TPart,1,Space-1) ELSE FType=TPart.
    
    Space=INSTRING(' ',TPart,1)    !e.g. BYTE  but what about STRING('hi there')
    Comma=INSTRING(',',TPart,1)    !e.g. QUEUE,PRE
    Paren1=INSTRING('(',TPart,1)            !e.g. STRING(22)
    Paren2=INSTRING(')',TPart,1,Paren1+1)   !e.g. STRING(22)
    FTypeOrig = FType
    FType = UPPER(FType)
    GQFldQ:Type = UPPER(FType)

    IF Paren1 AND Paren2-1 >= Paren1 AND (~Comma OR Paren1 < Comma)  THEN
       PPart=LEFT(TPart[Paren1+1 :Paren2-1 ] ) !Now ="digits, deci" was ="(digits, deci)"
    ELSE
       PPart=''
    END

    CASE FType
    OF 'DECIMAL' OROF 'PDECIMAL'
        IF PPart THEN
           Comma=INSTRING(',',PPart,1)             !e.g. (9,2)
           IF Comma THEN 
              !PPart[Comma]='.'                          !chnage 9,2 to 9.2 so decimal makes it number
              Comma=INSTRING(',',PPart,1,Comma+1)        !   e.g. (9.2, 17.5)   a default value
              IF Comma THEN PPart=SUB(PPart,1,Comma-1).  !cutoff default
           END 
           GQFldQ:TypeNums = PPart
        END

    OF 'STRING'  OROF 'CSTRING' OROF 'PSTRING' OROF 'MEMO'
        IF PPart THEN           
           XX = PPart + 0
           IF PPart[1]='@' THEN                 !Is it STRING(@d2) ?
              GQFldQ:TypeNums = PPart 
           ELSIF PPart[1]='''' THEN  
                Message('Don''t quote me in this but I cannot handle a STRING(''literal'')|Change to a STRING(Number). ||' & TPart)
                !must Deformat to handle ' {#} << {{'
           ELSIF NUMERIC(PPart) AND XX > 0 THEN            !Is it STRING( ## )
              IF InLIST(FType,'CSTRING','PSTRING') THEN XX -= 1.
              GQFldQ:TypeNums = XX
           ELSE
               ! Message('STRING PPart=' & PPart )
                CASE lower(PPart)
                OF 'file:maxfilename' ; GQFldQ:TypeNums=255 !EQUATE(256) 
                OF 'file:maxfilepath' ; GQFldQ:TypeNums=260 !EQUATE(260) 
                END 
           END
           IF FType='MEMO' AND INSTRING('BINARY',UPPER(TPart),1) THEN 
              GQFldQ:OmitHow=eOmit_Omit
           END
        END
    OF 'ANY'    ; GQFldQ:OmitHow=eOmit_NA_
    OF 'GROUP'  ; GQFldQ:OmitHow=eOmit_Omit
        IF GenQue_IsFILE THEN EXIT.  !FILE is for BROWSE template so just skip these
        IF PPart THEN
           Message('There is a derived GROUP( Base Type ) in line ' & TxtLineNo & |
                   '||You need to paste in the fields from GROUP( '& CLIP(PPart) &' )'& |
                   '||Mark this won''t do Q.Group.Field dot syntax for the #FIELDS...yet' & |
                   '||Line: ' & ALine,'Queue Parse - Derived GROUP', Icon:Asterisk) 
        END 

    OF 'QUEUE'
        IF QX <> -1 THEN MESSAGE('Cannot have a 2nd Queue, but you can for FILE.||'& ALine,'2 Queues').
        IF PPart THEN
           CASE Message('This a derived QUEUE( Base Type ).|You need to paste in the fields from '& CLIP(PPart) & |
                '||Line: ' & ALine,'Queue Parse - Derived Queue', Icon:Asterisk, |
                CHOOSE(UPPER(PPart)='FILE:QUEUE','Close|FILE:Queue','Ok'))
           OF 2 ; SETCLIPBOARD('Name      STRING(256)<13,10>ShortName STRING(13)<13,10>Date      LONG<13,10>Time      LONG<13,10>Size      LONG<13,10>Attrib    BYTE') 
                  Message('Fields Clipboard for Paste|You must remove (File:Queue)|={30}|' & CLIPBOARD(),PPart,ICON:Paste,,,MSGMODE:FIXEDFONT+MSGMODE:CANCOPY)
           END 
        END
    OF 'FILE'       !10/28/21 allow Secondary files that would be a Browse VIEW related file
        IF QX <> -1 THEN                            !Not the 1st file?
           GQF_Prefix = CLIP(GQFldQ:Label) & '.'    !2nd FILE change the Prefix being prepended
           IF InBetween('PRE(',')',TPart,Paren1,Paren2,PPart) >=0 THEN     !Find PRE(xxxx)
              IF PPart THEN GQF_Prefix=CLIP(PPart) & ':'.                  !Not PRE()
           END 
           EXIT
        END 
        
    OF 'LIKE'
        IF ~LikeWarned THEN 
           Message('I cannot deal with LIKE() on line ' & TxtLineNo & |
                '|You must change LIKE() to a Data Type e.g. LONG' & |
                '|or it will be treated as a String(123).' & |
                '||Kramer: "Why don''t you just Tell Movie Phone the Name of the Movie..."' & |
                '||Line: ' & ALine,'Queue Parse - LIKE ... Really?',Icon:Asterisk) ; LikeWarned=1
        END 
    OF 'KEY' OROF 'INDEX' OROF 'RECORD' OROF 'BLOB'
        IF GenQue_IsFILE THEN EXIT.                 !for FILE just skip these
    ELSE !Non-Standard type
        GQFldQ:Type = FTypeOrig  !Not Upper
        IF FTypeOrig='&' THEN GQFldQ:OmitHow=eOmit_NA_.
    END
    !GQFldQ:Picture = '-?-' !was =Pic_Fld   

    IF QX=-1 THEN EXIT.      !The Queue Line 1st in
    IF BracketOpening[1] AND ~BracketOpenQueX THEN   !Inside [Bracket is this the Next Variable Line? i.e. QueX=0
       GQFldQ:Bracket = BracketOpening               !Store the [ in the GQ Field record
       BracketOpenQueX = QX+1
    END
    ADD(GQFieldsQ)
    QX += 1
    EXIT
TakeBracketGroupInAlineRtn ROUTINE !Does ALine have [Group]
    CASE ALine[1:2]
    OF '![' OROF '!]' ; ALine=SUB(ALine,2,99)   !Change ![ to [ , or !] to ]
    ELSE
        IF ~INSTRING(ALine[1],'[]') THEN EXIT.  !Line is NOT a ![ !] [ ]
    END
    TPart=ALine ; ALine=''
    CASE TPart[1]
    OF '['
        IF INSTRING(']',TPart) THEN EXIT.     !Was ![xxxx] assume Pasted Embed NOT Group [
        BracketOpening=TPart[1] &' '& SUB(TPart,2,99)
        BracketOpenQueX=0
    OF ']'
        IF BracketOpenQueX THEN                                     !Must have had open [
           IF TPart=']' THEN TPart=']' & SUB(BracketOpening,2,99).  !Line is =']' add open's "[Heading text"
           ReplaceInto(TPart,'~',' ') !No Tildes in ~Head~
           GQFldQ:Bracket_2_ =']'
           GQFldQ:BracketText=LEFT(SUB(TPart,2,99))
           PUT(GQFieldsQ)
        END
        BracketOpenQueX=0 ; BracketOpening=''
    END
    EXIT
!------------------------------------------------
GenFmt.QueueAutoGenerate PROCEDURE()  !Generate if GenQue:AutoGenerate checked and have RECORDS GQFieldsQ
    CODE
    IF GenQue:AutoGenerate AND GenQue_TextQ AND RECORDS(GQFieldsQ) THEN
       GenFmt.QueueGenFormat()
    END
    RETURN
!------------------------------------------------
GenFmt.QueueGenFormat PROCEDURE()   !Build Format() using GenFmt_Queue and GQFieldsQ queue
ColX USHORT,AUTO
Fmt  PSTRING(256)
DataIndent PSTRING(6)
HdrText    PSTRING(32)
HdrJustify PSTRING(2)
HdrIndent  PSTRING(6)
DataJustify PSTRING(2) 
ColWidth   USHORT
LastFieldNo SHORT !GQFldQ:FieldNo
ThisFieldNo SHORT !GQFldQ:FieldNo
    CODE
    GenQue_Format='' ; GenQue_FIELDS='#FIELDS(' ; GenQue_Width=0
    IF GenQue:WidthMin < 4   THEN GenQue:WidthMin=20.
    IF GenQue:WidthMax > 500 THEN GenQue:WidthMax=500.
    IF ~InRange(GenQue:Digits_BYTE ,1,3 ) THEN GenQue:Digits_BYTE =3. 
    IF ~InRange(GenQue:Digits_SHORT,1,5 ) THEN GenQue:Digits_SHORT=5.   
    IF ~InRange(GenQue:Digits_LONG ,1,10) THEN GenQue:Digits_LONG =10.  
    IF ~InRange(GenQue:Digits_BOOL ,1,10) THEN GenQue:Digits_BOOL =1.    
    IF ~GenQue:JustLCR THEN GenQue:JustLCR='L'.
    HdrText='Column_'
    LOOP ColX=1 TO RECORDS(GQFieldsQ)
        GET(GQFieldsQ,ColX)                      !Get Next GQ record
        IF GQFldQ:Bracket_1_ = '[' THEN          !Is [Group] open?
           GenQue_Format = CLIP(GenQue_Format) &'['& CHOOSE(~GenQue:OnePerLine,'','<13,10>')
        END
        IF GQFldQ:OmitHow AND GQFldQ:OmitHow<>eOmit_Hide THEN GOTO OmitFieldLabel:.   !No output GROUP or &REF when 'N/A' or 'Omit'
        DO FillInGQFieldsQRtn
        PUT(GQFieldsQ)
        CASE LOWER(GQFldQ:Picture[1]) 
!!!        OF 'd' ;
!!!        OF 't' ;
        OF 'n' ; ColWidth = GQFldQ:CharsWide * 3.3 + GenQue:Indent
        ELSE   ; ColWidth = GQFldQ:CharsWide * 4.0 + GenQue:Indent
        END         
        IF ColWidth < GenQue:WidthMin THEN ColWidth=GenQue:WidthMin.
        IF ColWidth > GenQue:WidthMax THEN ColWidth=GenQue:WidthMax.
        IF GQFldQ:OmitHow=eOmit_Hide THEN ColWidth=0.
        IF ColWidth THEN GenQue_Width += (ColWidth + 4).     !12/30/21 Need Width of all columns, fudge of +4 for column sep

        IF ~DataJustify THEN DataJustify='L'.
        DataIndent='('&GenQue:Indent&')'    !Center Indent Zero
        HdrIndent =CHOOSE(GenQue:HdrJustLCR='C','(0)','('&GenQue:HdrIndent&')')   
        HdrJustify=GenQue:HdrJustLCR
        IF GenQue:HdrJustLCR AND DataJustify='R' THEN
            HdrJustify='C'
            HdrIndent='(0)'
        END 
        IF HdrJustify=DataJustify AND HdrIndent=DataIndent THEN !If Header Same as DATA ?
!No        HdrJustify='' ; HdrIndent=''                            ! then no need for Header Just
        END
!No     IF DataIndent='(0)' THEN DataIndent=''.
        
        IF GenQue_IsFILE THEN                             !File assumed to be Browse so Queue always contigous
           ThisFieldNo = CHOOSE(~GenQue:FieldNumbered,0,ColX)   !Config wants ## ?
!TODO - Add a checkbox to allow FILE to be # # numbered like Queue
        ELSE !a queue
           ThisFieldNo = CHOOSE(GQFldQ:FieldNo = 1+LastFieldNo, 0, GQFldQ:FieldNo)  !If Contigius no Need
           ThisFieldNo = CHOOSE(~GenQue:FieldNumbered,ThisFieldNo,GQFldQ:FieldNo)         !Config wants ## ?
        END 
        Fmt=ColWidth & |
            DataJustify & DataIndent & |
            CHOOSE(~GenQue:RightBorder,'','|') & |
            CHOOSE(~GenQue:Underline  ,'','_') & |
            CHOOSE(~GenQue:Fixed      ,'','F') & |
            CHOOSE(~GenQue:Resize     ,'','M') & |
            CHOOSE(~GenQue:Colored    ,'','*') & |
            CHOOSE(~GenQue:CellStyle  ,'','Y') & | !            CHOOSE(~HdrText ,'','~'& HdrText &'_'& ColX &' ~'& HdrJustify & HdrIndent) & |
            '~'& CLIP(GQFldQ:Label) &'~'& HdrJustify & HdrIndent & |
            CHOOSE(~GQFldQ:Picture,'','@'& CLIP(GQFldQ:Picture) &'@') & |
            CHOOSE(~ThisFieldNo,'','#'& ThisFieldNo &'#') & |  !!! CHOOSE(~GenQue:FieldNo AND GQFldQ:FieldNo=1+LastFieldNo,'','#'& GQFldQ:FieldNo &'#') & |
            CHOOSE(~GenQue:OnePerLine,'','<13,10>')
        GenQue_Format = CLIP(GenQue_Format) & Fmt
        GenQue_FIELDS = CLIP(GenQue_FIELDS) & CHOOSE(ColX=1,'',', ') & GQFldQ:Pre_Label
        LastFieldNo = GQFldQ:FieldNo
OmitFieldLabel:
        IF GQFldQ:Bracket_2_=']' THEN !End ] List Group?
           Fmt=CLIP(LEFT(GQFldQ:BracketText))
           IF ~Fmt THEN Fmt='Group'.
           Fmt=']' & |                                  !FYI "](123)" specifies Group Width=123, best NOT used
               CHOOSE(~GenQue:Underline  ,'','_') & |   !FYI "|" "M" also possible but better on Field
               CHOOSE(~GenQue:Fixed      ,'','F') & |
               '~'& Fmt &'~'& |                         !FYI align L(2) R(2) possible after ~Group Head~
               CHOOSE(~GenQue:OnePerLine,'','<13,10>')
           GenQue_Format = CLIP(GenQue_Format) & Fmt
        END
    END
    GenQue_FIELDS = CLIP(GenQue_FIELDS) & ')'
    DISPLAY
    RETURN
!-------------------------
FillInGQFieldsQRtn ROUTINE 
    DATA 
XX SHORT,AUTO
dDigits SHORT,AUTO
dDecimals SHORT,AUTO
Comma USHORT,AUTO
Paren1 SHORT,AUTO
Paren2 SHORT,AUTO
PPart STRING(255),AUTO
TPart STRING(255),AUTO
FType LIKE(GQFldQ:Type),AUTO
Label_UPR STRING(64),AUTO
Pic_Fld  STRING(32)
    CODE
    IF ~GQFldQ:Pre_Label THEN GQFldQ:Pre_Label='Field_'& ColX.

    Pic_Fld='s123'         !when cannot figure it out this is the @pic
    Label_UPR=UPPER(GQFldQ:Label)
    FType = UPPER(GQFldQ:Type)
    CASE FType
    OF 'BOOL'  ; Pic_Fld='n' & GenQue:Digits_BOOL & GenQue:Pic_Int_Blank 
    OF 'BYTE'  ; Pic_Fld='n' & GenQue:Digits_BYTE & GenQue:Pic_Int_Blank
    OF 'SHORT' OROF 'USHORT'
             Picture_N_Width(GenQue:Digits_SHORT, 0, |
                             CHOOSE(GenQue:Pic_Int_Minus AND FType[1]<>'U'), |
                             GenQue:Pic_Int_Commas, GenQue:Pic_Int_Blank, Pic_Fld )
  
    OF 'LONG'      OROF 'ULONG'
    OROF 'SIGNED'  OROF 'UNSIGNED'         ! 123456789
    OROF 'COUNT_T' OROF 'POINTER_T'        ! 1,345,789,123
             Picture_N_Width(GenQue:Digits_LONG, 0, |
                             CHOOSE(GenQue:Pic_Int_Minus AND FType[1]<>'U'), |
                             GenQue:Pic_Int_Commas, GenQue:Pic_Int_Blank, Pic_Fld )

                IF INSTRING('DATE',Label_UPR,1) AND GenQue:LongLook4DateTime THEN
                   Pic_Fld=GenQue:Pic_Date
             ELSIF INSTRING('TIME',Label_UPR,1) AND GenQue:LongLook4DateTime THEN
                   Pic_Fld=GenQue:Pic_Time
                END
    OF 'DATE'   ; Pic_Fld=GenQue:Pic_Date
    OF 'TIME'   ; Pic_Fld=GenQue:Pic_Time

    OF 'SREAL' OROF 'REAL' OROF 'BFLOAT4' OROF 'BFLOAT8' ; Pic_Fld='s20'

    OF 'DECIMAL' OROF 'PDECIMAL'
            PPart=GQFldQ:TypeNums
            Comma=INSTRING(',',PPart,1)         !e.g. (9,2)
            IF ~Comma THEN                      !DECIMAL(9) 1/o ,decimal
               dDigits=PPart
               dDecimals=0
            ELSE 
               dDigits  =SUB(PPart,1,Comma-1)
               dDecimals=SUB(PPart,Comma+1,99)
               IF dDecimals < 0 THEN dDecimals=0.
            END
            IF dDigits < dDecimals THEN dDigits=dDecimals+1.    !TODO can this happens ?
            Picture_N_Width(dDigits, dDecimals, |
                             GenQue:Pic_Dec_Minus, |
                             GenQue:Pic_Dec_Commas, GenQue:Pic_Dec_Blank, Pic_Fld )

!        DB('Decimal #1 TypeNums='& CLIP(GQFldQ:TypeNums) &' dDigits='& dDigits &' dDecimals=' & dDecimals  &' Pic_Fld='& CLIP(Pic_Fld) &' PPart='& CLIP(PPart) )        
 
    OF 'STRING'  OROF 'CSTRING' OROF 'PSTRING' OROF 'MEMO'
        IF GQFldQ:TypeNums[1]='@' THEN         !Was STRING(@type)
           Pic_Fld=SUB(GQFldQ:TypeNums,2,99)
        ELSIF NUMERIC(GQFldQ:TypeNums) THEN
            Pic_Fld='s'& ( 0 + GQFldQ:TypeNums )
            IF GQFldQ:TypeNums > 255 THEN Pic_Fld=''.   !Cannot do @s256
        END
        
    OF 'ASTRING' OROF 'BSTRING' OROF 'ANY'  OROF 'VARIANT' ; Pic_Fld='s123'
    OF 'GROUP' ; Pic_Fld='s123' !Group needs to be skipped? then must # fld #
    OF 'QUEUE'
    OF 'LIKE' ; Pic_Fld='s123'
    ELSE !Non-Standard type
        Pic_Fld=''
    END
    !IF Pic_Fld='@' then stop('@ in Pic_Fld=' & Pic_Fld ) ; Pic_Fld=SUB(Pic_Fld,2,99) .
    IF GQFldQ:BangPic THEN 
        Pic_Fld=SUB(GQFldQ:BangPic,3,99)
        CASE lower(Pic_Fld)
        OF 'd' ; Pic_Fld=GENQUE:PIC_Date
        OF 't' ; Pic_Fld=GENQUE:PIC_Time
        END
    END
    GQFldQ:Picture = Pic_Fld
    
    !-- Chars Width of data -----------------
    
    GQFldQ:CharsWide=''
    GQFldQ:Debug=''
    DataJustify='R'
    CASE LOWER(Pic_Fld[1]) 
    OF 'd' ; GQFldQ:Debug=FORMAT(DATE(10,31,2020),Pic_Fld)  ; IF LEFT(GQFldQ:Debug,1)>='A' THEN DataJustify='L'.
    OF 't' ; GQFldQ:Debug=FORMAT(8123123,Pic_Fld) 
    OF 'n' ; GQFldQ:Debug=FORMAT('123456789012.12',Pic_Fld) 
    OF 'e' ; GQFldQ:Debug=FORMAT('123456789012.12',Pic_Fld) 
    OF 'p' ; GQFldQ:Debug=SUB(Pic_Fld,2,LEN(CLIP(Pic_Fld))-2) ; DataJustify='L'
    ELSE   ; DataJustify='L'
    END 
    IF GQFldQ:Debug THEN 
       XX=LEN(CLIP(GQFldQ:Debug))
    ELSE
       XX=INT(ABS(DEFORMAT(Pic_Fld)))
    END
    IF Pic_Fld='' THEN XX=0 + GQFldQ:TypeNums .
    IF XX < 1 THEN XX=1.
    GQFldQ:CharsWide = XX
    EXIT
!================================
GenFmt.QueuePreviewBtn PROCEDURE()
    CODE
    IF ~GenQue_Format THEN GenFmt.QueueGenFormat().
    START(PreviewFormatWindow,,No1310(GenQue_Format),'Queue 2 Format ' & GENQUE_NAME)
    RETURN
GenFmt.QueueParseBtn   PROCEDURE()
    CODE
    IF ~GenQue_Format THEN GenFmt.QueueGenFormat().
    ListControl=' LIST,AT(4,4),FULL,USE(?List:QueName),FROM(QueName),VSCROLL, |' & |
                '<13,10> FORMAT(''' & QUOTE(No1310(GenQue_Format)) &''') ,|' & |
                '<13,10> ' & GenQue_FIELDS
    SELECT(?TabInput)
    POST(EVENT:Accepted,?ProcessBtn)
    RETURN
!---------------------------------    
GenFmt.QueueLoadConfig PROCEDURE()
    CODE
    GenFmt_Queue=GenFmt_Queue_Defaults
    GenFmt.ConfigGetPut(1,'GenQueue',GenFmt_Queue)
    RETURN
!---------------------------------- 
GenFmt.BangPictureBtn PROCEDURE()
 CODE
 MESSAGE('Pictures are calculated based on Data Type and Preferences.'&|
     '||Override the picture with a !@ comment e.g. !@n_4b !@n7.2~%~'&|
      '|Use !@D for the default Date or !@T for the default Time.'&|
     '||Extended Attribute pictures can be used: NAME(''name ! @picture '')' &|
     '|-{40}|Append !@OMIT or !OMIT to exclude the field from the format.'&|
     '|Append !@HIDE or !HIDE to make field width Zero to hide it.'&|
     '|-{40}|List Groups can be speficied by putting [ and ] in the declarion.'&|
     '|    Put [ or ![ in column 1 on the line BEFORE the FIRST data field.'&|
     '|    Put ] or !] in column 1 on the line AFTER the LAST data field.'&|
     '|    Put "Heading Text" after the ] e.g. ]Last Modified'&|
     '|-{40}|Example:'&|
     '|LastRunTime  LONG  !@T4b'&|
     '|MeaningLess  LONG  !OMIT', 'Format Generation Picture', ICON:Help,,,MSGMODE:CANCOPY)    
!----------------------------------    
GenFmt.List:GQFieldsQ_TakeEvent PROCEDURE()
GQChoice LONG,AUTO
GQMax LONG,AUTO
NATilde PSTRING(2)
PopNo SHORT,AUTO 
DragRow SHORT,AUTO 
DropRow SHORT,AUTO 
ChangesGQF LONG,AUTO 
    MAP
GQMoveLine  PROCEDURE(SHORT UpOrDown)
GQHideLine  PROCEDURE()
GQOmitLine  PROCEDURE()
GQAllLines  PROCEDURE(SHORT AllWhat)
    END    
    CODE
    GQMax = RECORDS(GQFieldsQ) ; IF ~GQMax THEN RETURN.
    GQChoice = CHOICE(?List:GQFieldsQ)
    ChangesGQF = CHANGES(GQFieldsQ) 
    IF ~GQChoice THEN GQChoice=1 ; ?List:GQFieldsQ{PROP:Selected}=1.
    GET(GQFieldsQ,GQChoice)  ; IF ERRORCODE() THEN STOP('GET(GQFieldsQ ' & Error()) ; RETURN.
    NATilde=CHOOSE(GQFldQ:OmitHow=eOmit_NA_,'~','')
    CASE EVENT()
    OF EVENT:NewSelection
       IF KEYCODE()=MouseRight THEN DO PopupRtn.
    OF EVENT:AlertKey
       CASE KEYCODE() 
       OF CtrlHome  ; GQMoveLine(-2)
       OF CtrlUp    ; GQMoveLine(-1)
       OF CtrlDown  ; GQMoveLine(1) 
       OF CtrlEnd   ; GQMoveLine(2)
       OF InsertKey ; GQHideLine()
       OF DeleteKey ; GQOmitLine()
       OF CtrlDelete ; DELETE(GQFieldsQ)
       END
    OF EVENT:Drop
       DragRow = ?List:GQFieldsQ{PROPLIST:MouseDownRow}
       DropRow = ?List:GQFieldsQ{PROPLIST:MouseUpRow}
       !0{PROP:Text}='EVENT:Drop DragID()='& DragID() &' DropID()='& DropID() & ' MouseDownRow=' & DragRow  &' UpRow=' & DropRow  
       CASE DropRow
       OF 0  ; GQMoveLine(-2) 
       OF -1 ; GQMoveLine(2)
       OF DragRow !Same row
       ELSE !Move to row after dropped on row
           GET(GQFieldsQ,DragRow) 
           IF ~ERRORCODE() THEN 
              DELETE(GQFieldsQ)
              IF DropRow < DragRow THEN DropRow += 1.
              ADD(GQFieldsQ,DropRow)
           END 
       END
    END
    IF ChangesGQF <> CHANGES(GQFieldsQ) THEN GenFmt.QueueAutoGenerate().
    RETURN
PopupRtn ROUTINE
    SETKEYCODE(0)
    PopNo=POPUP(CHOOSE(GQChoice=1    , '~', '')  & 'Move Field to Top<9>Ctrl+Home' & |
                CHOOSE(GQChoice=1    ,'|~','|') & 'Move Field Up<9>Ctrl+Up' & |
                CHOOSE(GQChoice=GQMax,'|~','|') & 'Move Field Down<9>Ctrl+Down' & |
                CHOOSE(GQChoice=GQMax,'|~','|') & 'Move Field to Bottom<9>Ctrl+End' & |
                '|-' & |
                '|'& NATilde & CHOOSE(GQFldQ:OmitHow<>eOmit_Hide,'Hide Field (zero width)','Un-Hide Field') &'<9>Insert' & |
                '|'& NATilde & CHOOSE(GQFldQ:OmitHow<>eOmit_Omit,'Omit Field from List'   ,'Un-Omit Field') &'<9>Delete' & |
                '|-|ALL Fields{{Hide All|Un-Hide All|-|Omit All|Un-Omit All|-|Original Sequence}' & |
                '|-|' & |
                '|Remove from List<9>Ctrl+Delete')
    IF ~PopNo THEN EXIT.
    EXECUTE PopNo
      GQMoveLine(-2)
      GQMoveLine(-1)
      GQMoveLine(1)
      GQMoveLine(2)
      GQHideLine()
      GQOmitLine()
      GQAllLines(1)  !Hide All
      GQAllLines(-1) !UnHide
      GQAllLines(2)  !Omit All
      GQAllLines(-2) !UnOmit
      SORT(GQFieldsQ,GQFldQ:FieldNo)  !Orig Sequence
      DELETE(GQFieldsQ)
    END
    EXIT
GQMoveLine  PROCEDURE(SHORT UpOrDown)
NewPoz LONG,AUTO
    CODE
    CASE UpOrDown
    OF -2 ; NewPoz=1
    OF  2 ; NewPoz=GQMax
    ELSE  ; NewPoz=GQChoice+UpOrDown
    END
    IF ~INRANGE(NewPoz,1,GQMax) OR NewPoz=GQChoice THEN RETURN.
    DELETE(GQFieldsQ)
    GQChoice = NewPoz
    ADD(GQFieldsQ,GQChoice)
    ?List:GQFieldsQ{PROP:Selected}=GQChoice
    DISPLAY
    RETURN
GQHideLine  PROCEDURE()
    CODE
    IF ~NATilde THEN
       GQFldQ:OmitHow=CHOOSE(GQFldQ:OmitHow=eOmit_Hide,'',eOmit_Hide)
       PUT(GQFieldsQ)
    END
    RETURN
GQOmitLine  PROCEDURE()
    CODE
    IF ~NATilde THEN
       GQFldQ:OmitHow=CHOOSE(GQFldQ:OmitHow=eOmit_Omit,'',eOmit_Omit)
       PUT(GQFieldsQ)
    END
    RETURN
GQAllLines  PROCEDURE(SHORT AllWhat)
XQ LONG,AUTO
    CODE
    LOOP XQ=1 TO RECORDS(GQFieldsQ)
        GET(GQFieldsQ,XQ)
        IF GQFldQ:OmitHow=eOmit_NA_ THEN CYCLE.
        CASE AllWhat
        OF  1 ;    GQFldQ:OmitHow=eOmit_Hide
        OF -1 ; IF GQFldQ:OmitHow=eOmit_Hide THEN GQFldQ:OmitHow=''.
        OF  2 ;    GQFldQ:OmitHow=eOmit_Omit
        OF -2 ; IF GQFldQ:OmitHow=eOmit_Omit THEN GQFldQ:OmitHow=''.
        END
        PUT(GQFieldsQ)
    END
    RETURN
!----------------------------------
!EndRegion -- GenFmt Generate Format Class

!================================================    

