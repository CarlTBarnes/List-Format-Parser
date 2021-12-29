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
MsgLineBreak        PROCEDURE(STRING Txt),STRING
GetExample          PROCEDURE(BYTE ExpNo, <*STRING GenQueFmtExample>),STRING
ModifierHelpPopup   PROCEDURE(STRING XPos, STRING YPos, STRING HelpBtnFEQ) 
ChrCount            PROCEDURE(STRING Text2Scan, STRING ChrList),LONG
InBetween           PROCEDURE(STRING FindLeft,STRING FindRight, STRING SearchTxt, *LONG OutLeftPos, *LONG OutRightPos, <*STRING OutBetweenStr>),LONG,PROC !Returns -1 Not Found or Length Between may =0
No1310              PROCEDURE(STRING Text2Clean),STRING  !Remove 13,10 return Clipped
NoTabs              PROCEDURE(*STRING Txt)               !Change Tabs 09 to Space
ReplaceInto         PROCEDURE(*STRING Into, STRING FindTxt,STRING ReplaceTxt,BYTE ClipInto=0),LONG,PROC !Return Count
Picture_N_Width     PROCEDURE(SHORT pDigitsTotal, SHORT pDecimals, BOOL pMinus, BOOL pCommas, STRING pBlankB, *STRING OutPicture ),SHORT,PROC 
PreviewList         PROCEDURE(STRING pListFormat)
DB                  PROCEDURE(STRING DbTxt) 
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
!endRegion

  CODE  
  ListFormatParser()
  RETURN
!---------------------
ListFormatParser    PROCEDURE()   
    INCLUDE('ListFormatParser_DATA.clw')
DoResizePosted  BOOL
Tabs1Line       BOOL
    CODE
    SYSTEM{PROP:PropVScroll}=1 ; SYSTEM{PROP:MsgModeDefault}=MSGMODE:CANCOPY
    ListControl = GetExample(3,GenQue_TextQ)
    GenFmt.SimpleLoadConfig()
    OPEN(Window)
    0{PROP:MinWidth} =400
    0{PROP:MinHeight}=0{PROP:Height}/2  !300
    OMIT('**END**', OmitWndPrv)   
        WndPrvCls.Init(1)     !WndPreview secret button hover upper left corner and pops up
        WndPrvCls.InitList(?LIST:HistoryQ    ,HistoryQ    ,'HistoryQ')    !Not required in 11.13505, but below does show Queue Name in WndPreview 
    !end of OMIT('**END**', OmitWndPrv)
    
    ?Sheet1{PROP:TabSheetStyle}=1   
    ?Sheet1{PROP:BrokenTabs}=1        !this does not seem to work with the office style tabs
    ?Sheet1{PROP:NoSheet}=1 ; ?Sheet1{PROP:Below}=1
    DO TabHideSyncRtn
    ?LIST:HistoryQ{PROP:LineHeight} = 2 + ?LIST:HistoryQ{PROP:LineHeight}
    ?List:ModifierQ{PROPLIST:HasSortColumn}=1 
    ?LIST:ModifierQ{PROPLIST:Grid}=Color:Silver ; ?LIST:GQFieldsQ{PROPLIST:Grid}=Color:Silver
    ?Sheet1{PROP:NoTheme}=1  !For Manifest
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
        OF ?ProcessBtn  
            ListFlat=ListControl  
            FlattenCls.TrimText1310(ListFlat,0)   !Incase pasted format alone with extra CRLF
            IF INSTRING('<13>',ListFlat) THEN FlattenCls.Flatten(ListFlat) .
            DISPLAY  
            DO ParseRtn
            DO AddHistoryRtn
            Fmt:InLines = Format2QCls.GetLinesFmt(Flds:InLines)
            Fmt:Explain = Format2QCls.GetExplainLines()
            DO ParseListParsedIntoListLinesRtn      
            DO LengthsTextRtn
            SELECT(CHOOSE(Fmt:Format OR ~From:From,?TabFormatLines,?TabFROM)) 
            DISPLAY

        OF ?CopyLineFmtBtn   ; SETCLIPBOARD(Fmt:InLines)
        OF ?CopyExplainBtn   ; SETCLIPBOARD(Fmt:Explain)  
        OF ?CopyLineFmtPlusExplainBtn ; SETCLIPBOARD(CLIP(Fmt:InLines) &'<13,10>' & Fmt:Explain ) 
        OF   ?ModHelp2Btn
        OROF ?ModHelp3Btn
        OROF ?ModHelp4Btn
        OROF ?ModHelp5Btn
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
        OF ?RunAgainBtn OROF ?RunAgainGFQBtn OROF ?RunAgainFmtBtn OROF ?RunAgainFromBtn ; RUN(COMMAND('0'))
        OF ?DebugTabs   ; DO TabHideSyncRtn 
        OF ?PreviewListBtn OROF ?PreviewList2Btn OROF ?PreviewList3Btn
            IF Fmt:Format THEN 
               START(PreviewList,,Fmt:Format) 
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
                  CASE POPUP('Copy PROP and Description|Copy PROPLIST|Copy Long Description|-|Copy All')
                  OF 1 ; SETCLIPBOARD(CLIP(ModQ:Char) &'  '& CLIP(ModQ:PropFULL) &'  '&CLIP(ModQ:Name))
                  OF 2 ; SETCLIPBOARD(ModQ:PropFULL)
                  OF 3 ; SETCLIPBOARD(ModQ:Desc)
                  OF 4 ;  ; Message('TODO Copy ALL')
                  END
               ELSE 
                  
               END    
            END 
        OF ?List:GQFieldsQ
            GenFmt.List:GQFieldsQ_TakeEvent()
        END 
    END
    CLOSE(Window)
    RETURN

TabHideSyncRtn ROUTINE 
    ?TabFlat{PROP:Hide}   =1-DebugTabs
    ?TabParsed{PROP:Hide} =1-DebugTabs
    ?TabFormatQ{PROP:Hide}=1-DebugTabs
    ?TabFieldsQ{PROP:Hide}=1-DebugTabs
    ?TabExplainQ{PROP:Hide}=1-DebugTabs
    IF DebugTabs AND 0{PROP:Width}<620 THEN 0{PROP:Width}=620.

LengthsTextRtn ROUTINE  !Debug info on screen to know STRING's are big enough
    ?Lengths{PROP:Text}='Lengths: LIST ' & LEN(CLIP(ListControl)) &' , ' & |
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
       IF Fmt:Format[1]<>'[' AND ~NUMERIC(Fmt:Format[1]) THEN   !Valid foramt has '[' or Number 
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
     
!------------------------------------------------------------------------
Format2QCls.Parse2Q         PROCEDURE(CONST *STRING Fmt, *STRING TokFmt) 
LnFmt   LONG,AUTO
FX      LONG,AUTO 
SegBeg      LONG 
SegEnd      LONG 
InGroup     BOOL
GroupNo     SHORT
FieldNo     SHORT
SegmentQ    QUEUE,PRE(SegQ)     !figure out the [segments]
SegBeg        SHORT         ! SegQ:SegBeg
SegEnd        SHORT         ! SegQ:SegEnd
GrpNo         SHORT         ! SegQ:GrpNo
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
               GroupNo += 1
               CLEAR(SegmentQ) ; SegQ:GrpNo=GroupNo ; SegQ:GrpEnd=Fx
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
    LOOP FX=1 TO RECORDS(SegmentQ)   !Add the segments to see them
         GET(SegmentQ,FX)
         IF ~Segq:GrpNo THEN
            SELF.ParseSegment( Fmt, TokFmt, SegQ:SegBeg, SegQ:SegEnd, FieldNo , 0 )         !Non group segment of columns
            CYCLE
         END 
         SELF.ParseSegment( Fmt, TokFmt, SegQ:SegBeg, SegQ:SegBeg, FieldNo , Segq:GrpNo )   !Begin Group "[" only
         IF SegQ:SegBeg+1 <= SegQ:GrpEnd-1 THEN    
            SELF.ParseSegment( Fmt, TokFmt, SegQ:SegBeg+1 , SegQ:GrpEnd-1, FieldNo , 0 )    !Between the [brackets] columns in group
         END
         SELF.ParseSegment( Fmt, TokFmt, SegQ:GrpEnd, SegQ:SegEnd, FieldNo , -Segq:GrpNo )  !Ending ](size)modifiers         
    END       
    RETURN
!-----------------------    
Format2QCls.ParseSegment  PROCEDURE(CONST *STRING Fmt, CONST *STRING TokFmt, Long SegPosBeg, Long SegPosEnd, *SHORT NxtFldNo, SHORT GroupNo )  
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
    CODE 
    FREE(ExplainQ) 
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
        IF NUMERIC(Fmt[FX]) THEN
           TokFmt[FX]=CHOOSE(FX=1 OR ~NUMERIC(Fmt[FX-1]),'1','2')
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
HelpCls.Init   PROCEDURE()

    CODE !( _Char,  _Name,  _Prop,  _Desc) 
    ?ModHelpBtn{PROP:Tip}=ModifierHelp
    ?ModHelp2Btn{PROP:Tip}=ModifierHelp
    ?ModHelp3Btn{PROP:Tip}=ModifierHelp
    ?ModHelp4Btn{PROP:Tip}=ModifierHelp
    ?ModHelp5Btn{PROP:Tip}=ModifierHelp

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
    ADD(ModifierQ)
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
    CODE 
    ?List:ModifierQ{PROP:LineHeight} = 1 + ?List:ModifierQ{PROP:LineHeight}
    ?List:ModifierQ{PROPSTYLE:FontName,1} = 'Consolas'
    ?List:ModifierQ{PROPSTYLE:FontSize,1} = 1 + ?List:ModifierQ{PROP:FontSize} 
    ?List:ModifierQ{PROPSTYLE:FontStyle,1} = FONT:Bold

    ?List:ModifierQ{PROPSTYLE:FontName,2} = 'Consolas'
    ?List:ModifierQ{PROPSTYLE:FontSize,2} = 1 + ?List:ModifierQ{PROP:FontSize} 
    ?List:ModifierQ{PROPSTYLE:FontStyle,2} = FONT:Regular

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
   
!================================================ 
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
    START(PreviewList,,No1310(GenSim_Format))
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
GenFmt.CopyFormatBtn    PROCEDURE(LONG FeqFmt, LONG FeqFields)
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
    SETCLIPBOARD(FmtCB &'<13,10>')
    RETURN
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
    IF GenMD THEN SETCLIPBOARD(MrkDn &'<13,10,13,10>'& PupTxt) ; Message('MD on Clip').
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
    GenQue_Format='' ; GenQue_FIELDS='#FIELDS('
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
    START(PreviewList,,No1310(GenQue_Format))
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
     '|MeaningLess  LONG  !OMIT', 'Format Generation Picture', ICON:Help)    
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


!================================================    
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
Exp1 STRING('     LIST,AT(3,22,360,121),USE(?Browse:1),IMM,HVSCROLL,MSG(''Browsing ' &|
     'Records''),FORMAT(''[27R(2)|M~Cust.~C(0)@n_6@41R(4)|M~Meter~C(0)@n_9@4C|M~P' &|
     'ort~@n1@](85)|M~I.D. Numb'' &|' &|
     '<13,10>           ''ers~[38R(2)|M~Previous~C(0)@d1@47R(2)|M~Current~C(0)@d1' &|
     '@](76)|M~Reading Dates~[4'' &|' &|
     '<13,10>           ''1R(2)|M~Current~C(0)@n_9@41R(2)|M~Prior~C(0)@n_9@39R(2)' &|
     '|M~Usage~C(0)@n-9@22C|M~M'' &|' &|
     '<13,10>           ''eter~@s1@21C|M~Read~@s1@4C|M~Final~@s1@](224)|M~Reading' &|
     's~''),FROM(Queue:Browse:1),#SEQ(1), |' &|
     '<13,10>           #ORIG(?List),#FIELDS(CustMeter:CustIdNo,CustMeter:MeterId' &|
     'No,CustMeter:PortNo,CustMeter:BegReadingDate,CustMeter:CurReadingDate,CustM' &|
     'eter:CurReading,CustMeter:BegReading,CustMeter:Usage,Meters:MeterType,CustM' &|
     'eter:ReadingType,CustMeter:FinalReading)' )
Exp2  STRING('          LIST,AT(139,11,191,167),USE(?List:2),HVSCROLL,FORMAT(''15R(1)|M' &|
     '~Cd.~L@n2@15C(1'' & |' &|
     '<13,10>            '')|MI@s10@120L(1)|M~Employee Type~@s30@''),FROM(Queue:B' &|
     'rowse:1),IMM, |' &|
     '<13,10>            #FIELDS(TYP:Code,Tag,TYP:Desc),#ORIG(?List),#SEQ(7)' )
Exp3  STRING(' LIST,AT(11,55,656,169),USE(?Browse:1),VSCROLL,FORMAT(''126L(2)|*~Employee Name'' & |' &|
     '<13,10>   ''~@s30@?[19C~Type~L(0)@n2@132L(2)|~Description~@s30@](134)|[19R(2)~Bldg.~C('' & |' &|
     '<13,10>   ''0)@n2@100L(2)|~Description~@s25@]|20C|~ Pay<<0DH,0AH>Grp~@n_2@13C|*~St.~@s1'' & |' &|
     '<13,10>   ''@[30C|~Brd.~@n6.5b@24L|~Emp.~C(0)@n6.5b@](55)|_~TRS Data~46R(3)|~SSN~C(0)@'' & |' &|
     '<13,10>   ''P###-##-####P@30R(2)|~Emp No~C(0)@N4@28R(2)|~Clock~C(0)@n_6@31R(2)|M~EIN~C'' & |' &|
     '<13,10>   ''(0)@n_9b@15R(5)|M~EIS<<0DH,0AH>Exempt~C(0)@n3B@''),FROM(Queue:Browse:1),IMM, |' &|
     '<13,10>   #FIELDS(MST:LastFirstMiddleName,MST:EmpType,TYP:Desc,MST:BldgNo,LOC:BldgDesc,MST:PayGroup, ' &|
                '|' &|
     '<13,10>   MST:PayStatus,MST:TrsBrdRate[1],MST:TrsEmpRate[1],MST:SSN,MST:EmpNo,MST:ClockNo, |' &|
     '<13,10>   MST:StateEIN,MST:EISExempt)' )  
Exp4 STRING('Window WINDOW(''LIST Everything''),AT(,,395,224),GRAY,FONT(''Microsoft Sans Serif'',8)' &|
     '<13,10> LIST,AT(3,2,379,206),USE(?LIST1),FROM(LIST1:Queue),FORMAT(''45L(2)|_FM*IY'' & |' &|
     '<13,10>         ''PT(LRB)~Tree List~@s55@Z(5)/?#1#43L(2)|_FM*YPT(LRB)~Everything~'' & |' &|
     '<13,10>         ''@s55@Z(55)Q''''DefaaultTip''''/?#2#56L(2)B(00008000H)|_FM*YPT(LRB)~'' & |' &|
     '<13,10>         ''Everything with Colors~@s55@Z(55)Q''''DefaultTip''''E(00000000H,000'' & |' &|
     '<13,10>         ''00080H,00FFFFFFH,00FF0000H)/?#3#[20L(4)F~SimpleCol~L(2)@s55@#4#'' & |' &|
     '<13,10>         '']|_FMHT(0000FFFFH)HB(00008000H)~Group Head~L(4)S(100)'')' &|
     '<13,10>    END' ) 
Exp5 STRING('ModTest WINDOW(''Modifiers and Picutres''),AT(,,586,90),GRAY,FONT(''Segoe UI'',9),RESIZE' &|
     '<13,10> LIST,AT(1,1,583,81),USE(?List1),FORMAT(''36L(2)|M*~Colored *~C(0)@s10@16L'' & |' &|
     '<13,10>     ''(2)I@p p@34L(2)|M~Icon~@s8@16L(2)J@p p@36L(2)|M~Tran Icon~@s9@7'' & |' &|
     '<13,10>     ''0L(2)|MT(1)~Tree T(1)~C(0)@s12@38R(2)|M~Style Z(3)~C(0)@n-9.2@Z'' & |' &|
     '<13,10>     ''(3)40R(2)|MY~Col Style Y~C(0)@N08@48R(2)|MP~Tooltip P~C(0)@n-13'' & |' &|
     '<13,10>     ''.2@40R(2)|M~Euro n9`2~C(0)@n-10`2@42L(2)|M~SSN  P # P~C(0)@p###'' & |' &|
     '<13,10>     ''-##-####p@30R(2)|M~Time~C(0)@t3@40L(2)|M~Date d4 ~@d4@48L(2)|M~'' & |' &|
     '<13,10>     ''E12.1 Picture~L(1)@E12.1@'')' &|
     '<13,10>  END' )
Exp6 STRING(' LIST,AT(180,14,90,11),USE(TransWanted),VSCROLL,DROP(14),FROM(''ALL Types|'' & |' &|   !,FORMAT(''90L(2)'')
     '<13,10>   ''#ALL|TRS|THIS|Surcharge|#SURCH|T.R.I.P.|#TRIP|Federal TR'' & |' &|
     '<13,10>   ''S|#FEDTRS|SSP ** ALL Types **|#SSP_ALL|SSP Standard |#SSPSTD|SSP Catchup |'' & |' &|
     '<13,10>   ''#SSPCAT|SSP Special |#SSPSPE|SSP Roth Standard |#ROTHSTD|SSP Roth Catchup '' & |' &|
     '<13,10>   ''|#ROTHCAT|SSP Roth Special |#ROTHSPE|Board Paid SSP|#SSPBRD'')' )
    CODE
    IF ~OMITTED(GenQueFmtExample) THEN DO GenQueRtn.
    IF ~ExpNo THEN 
        ExpNo=POPUP('Customer Meter|Simple Emp Type Tag|Employee Browse|Everything|Preview All Modifiers|FROM Test')
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
InBetween PROCEDURE(STRING FindLeft,STRING FindRight, STRING SearchTxt, *LONG OutLeftPos, *LONG OutRightPos, <*STRING OutBetweenStr>)!,LONG,PROC !Returns -1 Not Found or Length Between may be Zero
OutLength LONG,AUTO
PosLeft  LONG,AUTO
PosRight LONG,AUTO
    CODE
    OutLeftPos=0 ; OutRightPos=0 ; OutLength = -1   !-1 = Not Found 
    PosLeft=INSTRING(UPPER(FindLeft),UPPER(SearchTxt),1,1)             !find "PRE("
    IF PosLeft THEN
       PosLeft += LEN(FindLeft)                          !is ^ in "PRE(^"
       PosRight=INSTRING(UPPER(FindRight),UPPER(SearchTxt),1,PosLeft)  !find ")"
       IF PosRight THEN
          PosRight -=1                                   !is ^ in "PRE(  ^)"
          OutLength   = PosRight - PosLeft + 1
          OutLeftPos  = PosLeft
          OutRightPos = PosRight
       END
    END
    IF ~OMITTED(OutBetweenStr) THEN 
        OutBetweenStr=CHOOSE(OutLength<1,'',SearchTxt[PosLeft : PosRight])
    END
    RETURN OutLength
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
PreviewList  PROCEDURE(STRING pListFormat)
Fmt:Format  STRING(4000) 
ColumnQ QUEUE,PRE(ColQ)            !Queue feeds VLB based on Format()
ColNo     SHORT           !ColQ:ColNo     List Column
FldNo     SHORT           !ColQ:FldNo     Queue Field No
Mod       STRING(1)       !ColQ:Mod       Modifier or Blank for Data 
GPic      GROUP,PRE()     !ColQ:GPic
PicAT       STRING(1)     !ColQ:PicAT   '@'
PicType     STRING(1)     !ColQ:PicType  n e s d t p P
PicSpec     STRING(22)    !ColQ:PicSpec  -9.2 
          END
Picture   STRING(24),OVER(GPic) !ColQ:Picture OVER(Pic Group)
IsLong    BOOL            !ColQ:IsLong
DataLong  LONG            !ColQ:DataLong   Data LONG for VLB
DataText  STRING(60)      !ColQ:DataText   Data STRING for VLB
Format    STRING(96)      !ColQ:Format     Col Format for Debug
        END !ColQ   
                    
!Ugly but common: FONT('Microsoft Sans Serif',8)   Better: ,FONT('Segoe UI',9)  tighter 8
ViewWindow WINDOW('Preview FORMAT()'),AT(,,450,150),GRAY,SYSTEM,MAX,ICON('LFmtIcon.ico'),FONT('Segoe UI',9),RESIZE
        LIST,AT(4,58),FULL,USE(?LIST:View),NOBAR,HVSCROLL
        BUTTON('Format'),AT(3,4,34,14),USE(?FmtBtn),SKIP,TIP('Format() view using CB Window Preview Class to see PROPLIS' & |
                'T: and more...')
        BUTTON('Menu...'),AT(3,21,34,14),USE(?MenuBtn),SKIP,TIP('Menu and Tricks')
        BUTTON('Undo'),AT(3,38,34,14),USE(?UndoBtn),DISABLE,SKIP,TIP('Undo changes to Format()')
        TEXT,AT(47,4,,48),FULL,USE(Fmt:Format),SKIP,VSCROLL,FONT('Consolas',10)
    END
ListFEQ LONG        !VLB LIST = ?LIST:View
VlbView CLASS       !Class to fill VLB List - TOOD Split into External Class
FEQ         LONG    !Set to ListFEQ
RowCnt      LONG
ClmCnt      USHORT
Changed     BOOL
LoadQ       PROCEDURE() 
Number1234  PROCEDURE(STRING pPicture),STRING
Init        PROCEDURE(SIGNED xFEQ, LONG xRowCnt, USHORT xClmCnt)
VLBprc      PROCEDURE(LONG xRow, USHORT xCol),STRING
       END
Date5   LONG,DIM(5)  !Date Data for VLB
Time5   LONG,DIM(5)  !Time Date

P LONG,DIM(4),STATIC     !Window Pos Saved
PreviewCls CBWndPreviewClass
DOO   CLASS              !DOO. instead of DO Routine
ListSetup  PROCEDURE()   !Setup LIST and VLB with current Format entered
MenuPopup  PROCEDURE()   !Menu... Button 
PreviewCls PROCEDURE(BYTE CallType)  !Call PreviewCls. then ListSetup of change
      END
    CODE 
    Fmt:Format=pListFormat
    ListFEQ=?LIST:View
    OPEN(ViewWindow)
    IF P[3] THEN SETPOSITION(0,P[1],P[2],P[3],P[4]).
    PreviewCls.Init() ; IF PreviewCls.ReflectionBtn THEN HIDE(PreviewCls.ReflectionBtn).
    0{PROP:MinWidth}=100 ; 0{PROP:MinHeight}=100 
    ListFEQ{PROP:LineHeight}=1+ListFEQ{PROP:LineHeight}
    0{PROP:Text}=0{PROP:Text} &' Length=' & LEN(CLIP(pListFormat)) &' @ '& FORMAT(CLOCK(),@t6) 
    DISPLAY 
    DOO.ListSetup()
    ACCEPT
        IF FIELD() THEN PreviewCls.SelectedLast=?LIST:View.
        CASE ACCEPTED()
        OF ?Fmt:Format ; DOO.ListSetup()
        OF ?FmtBtn     ; DOO.PreviewCls(3) 
        OF ?UndoBtn    ; Fmt:Format=pListFormat ; DOO.ListSetup()
        OF ?MenuBtn    ; DOO.MenuPopup()
        END 
    END
    GETPOSITION(0,P[1],P[2],P[3],P[4])
    RETURN 
!------------------------
DOO.MenuPopup PROCEDURE()
PopNo BYTE,AUTO
FontPU BYTE,AUTO
    CODE
    PopNo=POPUP('CB Wnd Preview Class' & |  !SubMenu            
                  '{{' & |
                     'Window Introspection' & |         !#1
                    '|LIST Control PROP:s' & |          !#2
                    '|LIST Format and LISTPROP:s' & |   !#3
                    '|LIST Re-Format' & |               !#4
                  '|-|VLB ColumnQ Reflection' & |       !#5
                  '}' & |
                '|-' & |
                '|Select Font...' & |               !#6
                '|Change Font' & |  !SubMenu
                    '{{' & |
                       'Sans Serif' & '{{8|9|10}' & |  !#7 - #9
                      '|Segoe UI'   & '{{8|9|10}' & |  !#10 - #12
                    '}') 
    CASE PopNo
    OF 1 ; DOO.PreviewCls(1)                             ! #1  Window Introspection
    OF 2 ; DOO.PreviewCls(2)                             ! #2  LIST Format and LISTPROP:s
    OF 3 ; DOO.PreviewCls(3)                             ! #2  LIST Format and LISTPROP:s
    OF 4 ; DOO.PreviewCls(4)                             ! #2  LIST Format and LISTPROP:s
    OF 5 ; PreviewCls.QueueReflection(ColumnQ,'ColumnQ') ! #2  View ColumnQ that feeds VLB
    OF 6 TO 12 ; FontPU = PopNo-5 ; DO FontRtn           ! #6 Select Font...  #12 Segoe UI 10  => 1 to 7
    END !CASE Popup
   
FontRtn ROUTINE     !TODO is this finished ?
    DATA
FFace   STRING(32)  ![ ] TODO keep STATIC Font to use on next Preview
FSize   LONG
FColor  LONG
FStyle  LONG
FCHar   LONG
  CODE
  GETFONT(ListFEQ,FFace,FSize,FColor,FStyle,FChar)
  CASE FontPU
  OF 1 ; IF ~FONTDIALOGa('Select List Font',FFace,FSize,FColor,FStyle,FChar) THEN EXIT.
  OF 2 TO 4 ; FSize=CHOOSE(FontPU-1,8,9,10) ; FFace='Microsoft Sans Serif'
  OF 5 TO 7 ; FSize=CHOOSE(FontPU-4,8,9,10) ; FFace='Segoe UI'
  ELSE      ; Message('FontRtn FontPU=' & FontPU) ; EXIT
  END    
  ListFEQ{PROP:Format}='' ; DISPLAY
  SETFONT(0,      FFace,FSize,FColor,FStyle,FChar)
  SETFONT(ListFEQ,FFace,FSize,FColor,FStyle,FChar) ; DISPLAY
  DOO.ListSetup()
  ?MenuBtn{PROP:Tip}='Font:<13,10>' & FFace &|
              '<13,10>Size: '& FSize &'  Color: '& FColor&'  Style: '& FStyle&'  CharSet: '& FChar 
  EXIT
!---------------------------------------
DOO.PreviewCls  PROCEDURE(BYTE CallType)
Fmt:Before LIKE(Fmt:Format),AUTO
Fmt:After  LIKE(Fmt:Format),AUTO
    CODE
    Fmt:Before=ListFEQ{PROP:Format}
    CASE CallType
    OF 1 ; PreviewCls.Reflection()
    OF 2 ; PreviewCls.SideDoorCall(1, ListFEQ, 14,'LIST','Preview LIST')  !ControlPROPs
    OF 3 ; PreviewCls.SideDoorCall(3, ListFEQ, 14,'LIST','Preview LIST')  !ListPROPs
    OF 4 ; PreviewCls.SideDoorCall(4, ListFEQ, 14,'LIST','Preview LIST')  !ListReFORMAT
    END 
    Fmt:After=ListFEQ{PROP:Format}
    IF Fmt:After=Fmt:Before THEN RETURN.
    Fmt:Format=QUOTE(CLIP(Fmt:After)) 
    DOO.ListSetup()
    RETURN  
!------------------------
DOO.ListSetup PROCEDURE()
Yr  USHORT
E   LONG
F   LONG
Fmt LIKE(Fmt:Format)
    CODE
    IF Date5[1] THEN VlbView.Changed=1.
    Date5[1]=TODAY()  ; Yr=YEAR(Date5[1])
    Date5[2]=Date( 2, 3,Yr)            ! variety with 1 and 2 digits
    Date5[3]=Date( 6,21,Yr)
    Date5[4]=Date(10, 4,Yr)
    Date5[5]=Date(12,25,Yr)
    Time5[1]=DEFORMAT('07:02:11',@t4)  
    Time5[2]=DEFORMAT('09:23:22',@t4)
    Time5[3]=DEFORMAT('10:04:13',@t4)
    Time5[4]=DEFORMAT('11:45:24',@t4)
    Time5[5]=DEFORMAT('22:56:09',@t4) 
    LOOP E=1 TO LEN(CLIP(Fmt:Format))  !Remove 13,10s in TEXT Format
        CASE VAL(Fmt:Format[E])
        OF 9 OROF 10 OROF 13
        ELSE
           F += 1 ; Fmt[F] = Fmt:Format[E]
        END
    END
    IF F=0 OR Fmt<=' ' THEN 
       Fmt=pListFormat 
       Fmt:Format=pListFormat
    END
    ?LIST:View{PROP:Format}=UNQUOTE(Fmt)
    VlbView.LoadQ() 
    VlbView.Init(?LIST:View, 5, Records(ColumnQ))
    ?UndoBtn{PROP:Disable}=CHOOSE(Fmt:Format=pListFormat)
    DISPLAY 
    SELECT(?LIST:View)
    RETURN

!==============================================================
!Region VlbView Class
VlbView.LoadQ    PROCEDURE()
VlbCnt  USHORT(1)
ColX    USHORT,AUTO
FieldX  USHORT,AUTO
InX     LONG,AUTO
Fmt     STRING(1024),AUTO 
Date1   LONG 
AZ      STRING('abcdefghijklmnopqrstuvwxyz') 
StyleZ  LONG
  CODE
  FREE(ColumnQ)
  Date1=TODAY()
  LOOP ColX=1 TO 1024  !ListFEQ{PROPLIST:Exists,0}
     IF ~ListFEQ{PROPList:Exists, ColX} THEN BREAK.
     CLEAR(ColumnQ)
     ColQ:ColNo   = ColX 
     FieldX       = ListFEQ{PROPLIST:FieldNo, ColX} 
     ColQ:Picture = ListFEQ{PROPLIST:Picture, ColX}
     ColQ:Format  = ListFEQ{PROPLIST:Format, ColX}  !for Debug in Q view
     IF ColQ:PicType<>'P' THEN ColQ:PicType=lower(ColQ:PicType).
     CASE ColQ:PicType
     OF 'n' ; ColQ:DataText = SELF.Number1234(ColQ:Picture)
     OF 'e' ; ColQ:IsLong=1 ; ColQ:DataLong=1234500
     OF 'd' ; ColQ:IsLong=1 ; ColQ:DataLong=Date5[1] ; Date5[1] += 64
     OF 't' ; ColQ:IsLong=1 ; ColQ:DataLong=Time5[1]
     OF   'P'   !<-- Upper P 
     OROF 'p' ; ColQ:DataText=ALL('1234567890',60)
                ColQ:DataText=SUB(ColQ:DataText,1,ChrCount(ColQ:Picture,'<#'))
     ELSE ; ColQ:DataText=UPPER(AZ[1]) & AZ[2:26] & AZ ; AZ=AZ[2:26] & AZ[1] !'AbcdeFghijKlmnoPqrstUvwxyz'
     END 
     DO AddColumnQ
    !    ---Sequence in Queue-------------------------------
    !    1. LIST Column data to          Various
    !    2. * Color Foreground           LONG  PROPLIST:Color
    !    3. * Color Background           LONG  PROPLIST:Color
    !    4. * Color Selected Foreground  LONG  PROPLIST:Color
    !    5. * Color Selected Background  LONG  PROPLIST:Color
    !  = 6. I Icon in {Prop:IconList,#}  LONG  PROPLIST:Icon
    !  = 6. J Icon in {Prop:IconList,#}  LONG  PROPLIST:IconTrn
    !    7. T Tree Level                 LONG  PROPLIST:Tree
    !    8. Y Style Number for Cell      LONG  PROPLIST:CellStyle
    !    9. P Tool Tip for Cell        STRING  PROPLIST:Tip
     
     IF ListFEQ{PROPLIST:Color, ColX} THEN 
        CLEAR(ColumnQ)
        ColQ:Mod='*'
        ColQ:IsLong=1  !Color like a Red Shirt washed in HOT Water
        ColQ:DataLong=COLOR:Maroon ; DO AddColumnQ  !FG
        ColQ:DataLong=0FAFAFFH     ; DO AddColumnQ  !BG  !Pinkish White
        ColQ:DataLong=COLOR:None   ; DO AddColumnQ  !SFG
        ColQ:DataLong=COLOR:None   ; DO AddColumnQ  !SBG        
     END
     IF ListFEQ{PROPLIST:Icon, ColX} THEN        !Icon
        CLEAR(ColumnQ)
        ColQ:Mod='I'
        ColQ:IsLong=1
        ColQ:DataLong=1    ; DO AddColumnQ
     ELSIF ListFEQ{PROPLIST:IconTrn, ColX} THEN  !Tran Icon
        CLEAR(ColumnQ)
        ColQ:Mod='J'
        ColQ:IsLong=1
        ColQ:DataLong=2    ; DO AddColumnQ
     END
     IF ListFEQ{PROPLIST:Tree, ColX} THEN        !Tree
        CLEAR(ColumnQ)
        ColQ:Mod='T'
        ColQ:IsLong=1
        ColQ:DataLong=1    ; DO AddColumnQ
     END 
     
     StyleZ=ListFEQ{PROPLIST:ColStyle,ColX}             !Z(#) Col Style
     IF StyleZ THEN 
        ListFEQ{PROPSTYLE:TextColor,StyleZ}=Color:Green !Olive 
        ListFEQ{PROPSTYLE:BackColor,StyleZ}=0E0FFFFH    !Lt Yellow
     END 
     IF ListFEQ{PROPLIST:CellStyle, ColX} THEN          !Y Cell Style
        CLEAR(ColumnQ)
        ColQ:Mod='Y'
        ColQ:IsLong=1
        ColQ:DataLong=255    ; DO AddColumnQ
        ListFEQ{PROPSTYLE:TextColor,255}=Color:Teal     !Teal on
        ListFEQ{PROPSTYLE:BackColor,255}=0E0FFFFH       !Lt Yellow
     END
     IF ListFEQ{PROPLIST:Tip, ColX} THEN                !P Tip 
        CLEAR(ColumnQ)
        ColQ:Mod='P'
        ColQ:DataText='Tip for Column ' & ColX ; DO AddColumnQ
     END
  END
  LOOP InX=1 TO 5
        ListFEQ{PROP:IconList,InX}=CHOOSE(InX,ICON:Print,ICON:Copy,ICON:Cut,ICON:Paste,ICON:Save)
  END
  RETURN 
AddColumnQ ROUTINE     
    ColQ:FldNo=FieldX ; FieldX += 1
    Add(ColumnQ)
    EXIT
!------------------------------------
VlbView.Number1234 PROCEDURE(STRING sPicture)!,STRING    
N       DECIMAL(21)
I       DECIMAL(2)
NewTry      CSTRING(32)
Worked_N    LIKE(N)     !Last Try w/o ####
Commas_N    LIKE(N)     !Last try #,###,### with most Commas (Grouping ,._)
CommaCnt    BYTE        !Want Max Commas, RTL will use Comma space for #
NewCount    BYTE
CPicture    CSTRING(32)
picFill     PSTRING(2)  ! _=Space *=**** 0=0000 kills Grouping
picGrouping PSTRING(2)  !1000s Grouping . _=Space   Default Comma
picPlaceSep PSTRING(2)  !Pennies . , v
picDecimals PSTRING(3)  !Number Decimal Digits e.g. 2 if @n9.2
    CODE
    sPicture=lower(sPicture)
    CPicture=CLIP(sPicture) 
    DO ParseForCommaPeriodRtn 
    N=1 ; I=2
    LOOP 20 TIMES
        NewTry=CLIP(LEFT(FORMAT(N,CPicture))) 
        IF INSTRING('#',NewTry) THEN BREAK.   !Overflow shows #'s
        Worked_N = N
        IF LEN(picGrouping) THEN !Want Most Commas ','
           NewCount=ChrCount(NewTry,picGrouping) ! ',' 
           IF NewCount >= CommaCnt THEN 
              Commas_N = Worked_N ; CommaCnt = NewCount 
           END
        END
        N *= 10 ; N += I ; I += 1 ; IF I > 9 THEN I=0.
    END
    RETURN CHOOSE(~Commas_N,Worked_N,Commas_N) + .12345
!------------------------------------
ParseForCommaPeriodRtn ROUTINE
    DATA
PX        SHORT
PicChr    STRING(1)
InTilde   BOOL        ! ~ currency ~
b4Numbers BOOL(1)     ! Before Numbers is Fill
    CODE
!@N [currency] [sign] [ fill ]  size  [ grouping ] [places] [sign] [currency] [B] 
!       $ ~~     -      0 _ *    12    . _(space)   .`v 2      -      ~xxx~          

    LOOP PX=3 TO LEN(CPicture) BY 1   !Forwards
        PicChr=CPicture[PX]
        IF PicChr='~' THEN InTilde=1-InTilde ; CYCLE.
        IF InTilde THEN CYCLE.                          !Ignore Inside ~ currency ~ 
        IF b4Numbers THEN      !Before Numbers is Fill
           ![fill]  Specifies leading zeros, spaces, or asterisks (*) in any leading zero positions, and suppressesdefault grouping. If the fill is omitted, leading zeros are suppressed.       
           CASE PicChr
           OF '_'        ; picFill='_'                   !_ (underscore) Produces leading spaces
           OF '*'        ; picFill='*'                   !* (asterisk) Produces leading asterisks
           OF '0'        ; picFill='0' ; b4Numbers=False !0 (zero) Produces leading zeroes
           OF '1' TO '9' ; b4Numbers=False
           END 
           CYCLE
        END

        ! [size]  The size is required to specify the total number of significant digits to display, including the number of digits in the places indicator and any formatting characters.
        ! [grouping] A grouping symbol, other than a comma (the default), can appear right of the size indicator to specify a three digit group separator.
        !   . (period) Produces periods
        !   _ (underscore) Produces spaces
        !
        ! [places]  Specifies the decimal separator symbol and the number of decimal digits. The number of decimal digits must be less than the size. The decimal separator may be a period (.), grave accent (' ) (produces periods grouping unless overridden), or the letter 
        !  v (used only for STRING field storage declarations--not for display).
        !  . (period) Produces a period
        !  ` (grave accent) Produces
        !Not implemented is double .. `` __ that make Grouping and Decimal the same
        CASE PicChr
        OF '.'  ; IF ~picGrouping THEN picGrouping='.' ELSE picPlaceSep='.'.  !Canhave .. nonsense
        OF '_'  ; picGrouping='_'  
        OF '`'  ; picPlaceSep=',' ; IF ~picGrouping THEN picGrouping='.'.     !Comma decimal defualts to decimal grouping
        OF 'v'  ; picPlaceSep='v'
        OF '0' TO '9' 
                  IF ~picPlaceSep THEN          !No Decimal Point ... but
                     IF picGrouping='.' |       !Period alone then numbers is Decimal
                     OR picGrouping='_' THEN    !Underscore alone is Space for Decimal
                           picPlaceSep=picGrouping   !So decimal is '.' or '_'
                           picGrouping=','                        !
                     END
                  END
                  IF picGrouping OR picPlaceSep THEN                   
                     picDecimals=picDecimals & PicChr
                  END
        END
        
    END
    IF    picFill THEN         picGrouping=''   !Any Fill _*0 Removes Grouping
    ELSIF ~picGrouping THEN    picGrouping=','  !Default Group is comma
    ELSIF picGrouping='_' THEN picGrouping=' '  !_ is ' ' Space
    END    
    IF picDecimals=0 THEN picPlaceSep=''.
    EXIT

!------------------------------------  
VlbView.Init PROCEDURE(SIGNED xFEQ, LONG xRowCnt, USHORT xClmCnt)
  CODE
  SELF.FEQ=xFEQ
  SELF.RowCnt=xRowCnt
  SELF.ClmCnt=xClmCnt
  xFEQ{PROP:VLBval} =ADDRESS(SELF)
  xFEQ{PROP:VLBproc}=ADDRESS(SELF.VLBprc) 
  RETURN
VlbView.VLBprc PROCEDURE(LONG xRow, USHORT xCol)
  CODE  
  CASE xRow
  OF -1 ; RETURN SELF.RowCnt !Rows
  OF -2 ; RETURN SELF.ClmCnt !Columns
  OF -3 ; RETURN SELF.Changed
  END
  ColQ:FldNo = xCol 
  GET(ColumnQ,ColQ:FldNo)   
  !IDEA: Build a Q of (Row,Col,Data) for this to GET and Return. So the logic below is used to create that queue
  CASE ColQ:Mod
  OF 'T' OROF 'I' OROF 'J' ; RETURN xRow  !Tree Level, Icons
  END
  CASE ColQ:PicType
  OF 'd' ; IF xRow > 1 THEN ColQ:DataLong=Date5[xRow].
  OF 't' ; ColQ:DataLong=Time5[xRow] 
  OF 'n' ; IF xRow=5 THEN
              IF INSTRING('-',ColQ:Picture) OR INSTRING('(',ColQ:Picture) THEN RETURN(-1 * ColQ:DataText).
           END
  END
  IF ColQ:IsLong THEN RETURN ColQ:DataLong.
  RETURN CLIP(ColQ:DataText) 
!EndRegion  
!-------------------------     
DB PROCEDURE(STRING DbTxt)
CStr CSTRING(SIZE(DbTxt)+12)
  CODE
  CStr='ListFmt: ' & CLIP(DbTxt)&'<13,10>' ; OutputDebugString(CStr) ; RETURN