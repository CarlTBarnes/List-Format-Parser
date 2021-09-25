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
GetExample          PROCEDURE(BYTE ExpNo),STRING
ModifierHelpPopup   PROCEDURE(STRING XPos, STRING YPos) 
ChrCount            PROCEDURE(STRING Text2Scan, STRING ChrList),LONG
No1310              PROCEDURE(STRING Text2Clean),STRING  !Remove 13,10 return Clipped
PreviewList         PROCEDURE(STRING pListFormat)
DB                  PROCEDURE(STRING DbTxt) 
        MODULE('Win')
OutputDebugString   PROCEDURE(*cstring Msg),PASCAL,RAW,NAME('OutputDebugStringA'),DLL(1)
InterlockedCompareExchange PROCEDURE(*LONG Destination, LONG ExChangeToValue, LONG CompareDestToValue),LONG,PASCAL,DLL(1)
InterlockedExchange        PROCEDURE(*LONG Target, LONG SetToValue),LONG,PROC,PASCAL,DLL(1)

        END    
  END
!Region Global data
ModifierHelp STRING(' Pipe = Right Border   M=Resizable ' &|
     '<13,10> _ = Underline ' &|
     '<13,10> / = LastOnLine ' &|
     '<13,10> ? = Locator ' &|
     '<13,10> # = FieldNo ' &|
     '<13,10>' &|
     '<13,10> * = Colors in Q ' &|
     '<13,10> B(c)=BarFrame Color ' &|
     '<13,10> E(fbsb)=Color Defaults ' &|
     '<13,10>  ' &|
     '<13,10> F = Fixed (cannot scroll) ' &|
     '<13,10> I = Icon ' &|
     '<13,10> J = Icon Transparent ' &|
     '<13,10> M = Resizable     |=RightBorder ' &|
     '<13,10> P = Tip Cell QText' &|
     '<13,10> Q = Tip "Default" Column ' &|
     '<13,10> S(w)=ScrollBar ' &|
     '<13,10> T(s)=Tree (Surpress: B=Boxes I=Indent L=Lines R=Root 1=Offset) ' &|
     '<13,10> Y = Cell Style No. in Q ' &|
     '<13,10> Z(#) = Column Style' &|
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
    SYSTEM{PROP:PropVScroll}=1
    ListControl = GetExample(3)
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
    HelpCls.Init()    
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
        OF ?PasteBtn      ; ListControl=CLIPBOARD() 
                            POST(EVENT:Accepted,?ProcessBtn)
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
            SELECT(?TabFormatLines) 
            DISPLAY

        OF ?CopyLineFmtBtn   ; SETCLIPBOARD(Fmt:InLines)
        OF ?CopyExplainBtn   ; SETCLIPBOARD(Fmt:Explain)  
        OF ?CopyLineFmtPlusExplainBtn ; SETCLIPBOARD(CLIP(Fmt:InLines) &'<13,10>' & Fmt:Explain ) 
        OF   ?ModHelp2Btn
        OROF ?ModHelp3Btn
        OROF ?ModHelpBtn     ; START(ModifierHelpPopup,,0{PROP:XPos}+0{PROP:Width},0{PROP:YPos}+20)

        OF ?CopyListLineFmtBtn    ; SETCLIPBOARD(ListParsed)
        OF ?CopyListFlatBtn       ; SETCLIPBOARD(ListFlat)
        OF ?CopyLineFieldsBtn     ; SETCLIPBOARD(Flds:FIELDScode)
        OF ?ListParsedHScrollOff  ; ?ListParsed{PROP:HScroll}=CHOOSE(~ListParsedHScrollOff,'1','') ; DISPLAY 
                
        OF ?LIST:HistoryQ
            IF KEYCODE()=MouseLeft2 THEN 
               GET(HistoryQ, CHOICE(?LIST:HistoryQ))
               ListControl = HisQ:ListControl
               POST(EVENT:Accepted,?ProcessBtn) 
            END 
        OF ?RunAgainBtn ; RUN(COMMAND('0'))
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
        OF ?GenSimpleCopyFormatBtn  ; GenFmt.SimpleCopyBtn(1)
        OF ?GenSimpleCopyFieldsBtn  ; GenFmt.SimpleCopyBtn(3)
        OF ?GenSimpleDefaultSaveBtn ; GenFmt.ConfigGetPut(2,'GenSimple',GenFmt_Simple)
        OF ?GenSimpleDefaultLoadBtn ; GenFmt.SimpleLoadConfig() ; DISPLAY
        OF ?GenSimpleClearBtn       ; GenFmt_Simple=GenFmt_Simple_Defaults ; DISPLAY
        OF ?GemSim_Group TO ?GenSim:AutoGenerate OROF ?GenSim:Columns
           IF GenSim:AutoGenerate THEN GenFmt.SimpleGen().
        END !Case Accepted()

        CASE FIELD()
        OF ?List:ModifierQ  
            GET(ModifierQ,CHOICE(?List:ModifierQ))            
            CASE EVENT()
            OF EVENT:HeaderPressed
               CASE ?List:ModifierQ{PROPList:MouseDownField} !FYI PROPLIST:SortColumn is the current Sort not the new one
               OF 1 ; SORT(ModifierQ,ModQ:Sort)  ; ?{PROPLIST:Locator,1}=1
               OF 2 ; SORT(ModifierQ,ModQ:Prop)  ; ?{PROPLIST:Locator,1}=2
               OF 3 ; SORT(ModifierQ,ModQ:Name)  ; ?{PROPLIST:Locator,3}=1
               END
               GET(ModifierQ, ModQ:Sort) 
               ?List:ModifierQ{PROP:Selected}=POINTER(ModifierQ) 
               DISPLAY
            
            OF EVENT:AlertKey                           
               CASE KEYCODE()
               OF CtrlC
                  SETCLIPBOARD(CLIP(ModQ:Char) &'  '& CLIP(ModQ:PropFULL) &'  '&CLIP(ModQ:Name)) 
               OF CtrlShiftC  ; Message('CtrlShiftC todo copy all, or maybe put this on right click')
               END
            OF EVENT:NewSelection
               DISPLAY(?ModQ:Desc) 
               IF KEYCODE()=MouseRight THEN
                  CASE POPUP('Copy PROP and Description|Copy PROPLIST|Copy Long Description|-|Copy All')
                  OF 1 ; SETCLIPBOARD(CLIP(ModQ:Char) &'  '& CLIP(ModQ:PropFULL) &'  '&CLIP(ModQ:Name))
                  OF 2 ; SETCLIPBOARD(ModQ:PropFULL)
                  OF 3 ; SETCLIPBOARD(ModQ:Desc)
                  END
               ELSE 
                  
               END    
            END 
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

LengthsTextRtn ROUTINE  !Debug info on screen to know STRING's are big enough
    ?Lengths{PROP:Text}='Lengths: LIST ' & LEN(CLIP(ListControl)) &' bytes, ' & |
                     'Fmt Lines ' & LEN(CLIP(Fmt:InLines))    &' , ' & |
                     'FldsLines ' & LEN(CLIP(Flds:InLines))   &' , ' & |
                     'FldsCode ' & LEN(CLIP(Flds:FieldsCode)) &' , ' & |
                     'Explain ' & LEN(CLIP(Fmt:Explain))      &' , ' & |
                     'LIST Lines ' & LEN(CLIP(ListParsed))    &' , ' & |
                     'Flat ' & LEN(CLIP(ListFlat))            &' , ' & |
                     'Format ' & LEN(CLIP(Fmt:Format))        &' , ' & |
                     'FldsFlat ' & LEN(CLIP(Flds:FieldsFlat)) &' , ' & |           
                     'TokFmt ' & LEN(CLIP(Fmt:TokFmt)) &' bytes'

ParseRtn ROUTINE
    CLEAR(FormatGrp) ;   FREE(FormatQ)    
    CLEAR(FieldsGrp) ;   FREE(FieldsQ) 
    IF ~ListFlat THEN EXIT.              !Flaten failed
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
    SELF.Add1Q('L'  ,'PROPLIST:Left :HeaderLeft'        ,'Justification Left of Data or Heading'     ,'Left alignment of column data, or heading text. This may be offset by an (Indent).')
    SELF.Add1Q('R'  ,'PROPLIST:Right :HeaderRight'      ,'Justification Right of Data or Heading'    ,'Right alignment of column data, or heading text. This may be offset by an (Indent).')
    SELF.Add1Q('C'  ,'PROPLIST:Center  :HeaderCenter'   ,'Justification Center of Data or Heading'   ,'Center alignment of column data, or heading text. This may be offset by an (Indent).')
    SELF.Add1Q('D'  ,'PROPLIST:Decimal :HeaderDecimal'  ,'Justification Decimal of Data or Heading'  ,'Decimal alignment of column data, or heading text. The offset to the decimal point is specified with an (Indent).')
    SELF.Add1Q('0-9','PROPLIST:Width'       ,'Width DLUs of Cell or Group in Digits','Numbers in the FORMAT string outside delimiters ()~~@@## denote column width. Numbers are the delimiter that marks the beginning of a new column, as well as a "[" to mark the start of a Group.')
    SELF.Add1Q('()' ,'PROPLIST:LeftOffset :RightOffset :CenterOffset :DecimalOffset' ,'Indent of Column or Heading from Justification', |
        'An optional (Indent) integer, enclosed in (parentheses), that specifies in DLUs the indent from the justification LRCD. This may be negative. With Left justification indent defines a left margin ; with Right or Decimal, it defines a right margin; and with Center it defines an offset from the center of the field (negative = left offset).')
    SELF.Add1Q('*'  ,'PROPLIST:Color'       ,'Color of Cell  in Queue as 4x LONG'   ,'An asterisk "*" indicates color information a cell is contained in 4 LONG fields after the data field in the QUEUE. The four colors are Foreground, Background, Selected Foreground, and Selected Background (in that order in the queue).')
    SELF.Add1Q('B()','PROPLIST:BarFrame'    ,'Color of Selection Bar Frame'         ,'A "B(color)" specifies the color of the selection bar frame.')
    SELF.Add1Q('E()','PROPLIST:TextColor :BackColor :TextSelected :BackSelected','Color of Column and Cell Default','' & |
                                            'An "E(f,b,sf,sb)" specifies the column default colors (foreground, background, selected foreground, selected background). If the column also includes the * modifier the default colors are used for an cell when the Queue specifies COLOR:None.' & |
                                            '<13,10,13,10>The default column colors can be set or changed using the properties: PROPLIST:TextColor, PROPLIST:BackColor, PROPLIST:TextSelected and PROPLIST:BackSelected.')
    SELF.Add1Q('I'  ,'PROPLIST:Icon'        ,'Icon of Cell from Queue','' & |
                                             '"I" indicates an icon displays in the column, at the left edge of the column (prepended ' &|
                                             'to the data). An icon number is contained in a LONG after the data field in the QUEUE ' &|
                                             'that contains a number that refers to the list of icons set with {{PROP:IconList,#}=''~icon.ico'' runtime property.' &|
                                             '<13,10>' &|
                                             '<13,10>To display the icon only, and not the contents of the data field, make the display picture @P_PB.' &|
                                             '<13,10>' &|
                                             '<13,10>To detect a click on the icon check for PROPLIST:MouseDownZone = LISTZONE:Icon.')  ! Length = 462    
    SELF.Add1Q('J'  ,'PROPLIST:IconTrn'     ,'Icon (transparent) of Cell from Queue','A "J" indicates a transparent icon displays in the column. The same information for "I" applies to "J".')
    SELF.Add1Q('T()','PROPLIST:Tree'        ,'Tree Control','' & | 
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
    SELF.Add1Q('Y'  ,'PROPLIST:CellStyle'   ,'Style of Cell in Queue as LONG'   ,'"Y" indicates a Style Number for the cell is contained in a LONG field after the data field in the QUEUE. The Style Number LONG refers to an entry in an array of styles associated with the LIST control defined through the {{PROPSTYLE:xxxx, Number} runtime properties. This overrides a Z() column default style.')
    SELF.Add1Q('Z()','PROPLIST:ColStyle'    ,'Style default of Column in Format as (number)','"Z" followed by a Style number in (parens) sets the default style for an entire column.<13,10>Overriden by a "Y" Cell Style in the Queue (PROPLIST:CellStyle).<13,10>See PROPSTYLE: for more details.')
    SELF.Add1Q('~~' ,'PROPLIST:Header'      ,'Heading of Column ~Header~Justification(Indent)','A string enclosed in ~tildes~  displays the header at the top of the list. The header displays with the same alignment as the field data unless the tilde is followed by a header justification parameter LRCD (PROPLIST:HeaderLeft, :HeaderRight, :HeaderCenter, :HeaderDecimal) and/or indent value in (parentheses).')
    SELF.Add1Q('@@' ,'PROPLIST:Picture'     ,'Picture Format for Data as @picture@','The picture formats the field for display. The trailing @ is required to define the end of the picture, so that display pictures such as @N12~Kr~ can be used in the format string without creating ambiguity.')
    SELF.Add1Q('?'  ,'PROPLIST:Locator'     ,'Locator Column'                   ,'' & |  
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
                                             
    SELF.Add1Q('_'  ,'PROPLIST:Underline'   ,'Line Under Cell'                  ,'An underscore "_" underlines the field.')
    SELF.Add1Q('/'  ,'PROPLIST:LastOnLine'  ,'Last Column on Line for Group'    ,'A slash "/" causes the next field to appear on a new line. Only used on a field in a Group [].')
    SELF.Add1Q('|'  ,'PROPLIST:RightBorder' ,'Line on Right Side of Cell'       ,'A pipe "|" places a vertical line to the right of the field.')
    SELF.Add1Q('M'  ,'PROPLIST:Resize'      ,'Resizable Cell or Group'          ,'An "M" allows the column or group to be re-sized at runtime. This allows the user to drag the right vertical bar (if present) or right edge of the data area.')
    SELF.Add1Q('F'  ,'PROPLIST:Fixed'       ,'Fixed Column does not Scroll'     ,'An "F" creates a fixed column that stays on screen when the user horizontally pages through the fields (by the HSCROLL attribute). Fixed fields or groups must be at the start of the list. This is ignored if placed on a field within a group.')
    SELF.Add1Q('S()','PROPLIST:Scroll'      ,'Scroll bar on Column or Group'    ,'An S(integer) adds a scroll bar to the field or group. The integer defines the total dialog units to scroll. Ignored if field in group.')
    SELF.Add1Q('P'  ,'PROPLIST:Tip'         ,'Tool Tip for Cell in Queue as STRING' ,'A "P" adds a column tool tip. The tip text is in the next QUEUE field that follows the QUEUE data field. If the designated queue field is empty, the "Q" modifier designates a string value to use as a default tool tip.')
  SELF.Add1Q('Q''''','PROPLIST:DefaultTip'  ,'Tool Tip Column Default as ''String'' in FORMAT','A "Q" followed by a ''''string'''' designates the default column tip text. If "P" Column Tip is also specified the default tip shows when the Queue tip field is blank. Default tips can my the Format() unruly long so are best assigned at runtime.')
    SELF.Add1Q('#'  ,'PROPLIST:FieldNo'     ,'Field Number from Queue as #number#','A #number# enclosed in pound signs indicates the QUEUE field to display. Following fields in the format string without an explicit #number# are taken in order from the fields following the #number# field. For example, #2# on the first field in the format string indicates starting with the second field in the QUEUE, skipping the first. If the number of fields specified in the format string are >= the number of fields in the QUEUE, the format "wraps around" to the start of the QUEUE.')

    SELF.Add1Q('[]' ,'PROPLIST:Group + '    ,'Group Columns '                     ,'"[]" indicate multiple columns grouped. A group may specify header text after the ending "]" using the syntax: ~header text~ Justification LRCD (Indent).' & |
        '<13,10><13,10>{{PROPLIST:GroupNo,Column} returns the group number of a column.' & |
        '<13,10><13,10>{{PROPLIST:GroupNo + PROPLIST:Group,Column} returns the number of columns in the group or zero if not a group.<13,10>')
    SELF.Add1Q('[]()' ,'PROPLIST:Width of [Group]' ,'Group Width', |
        'An optional (Group Width) integer, enclosed in parentheses after the group closing square bracket, that specifies the width of the group (PROPLIST:Group + PROPLIST:Width). If omitted, the size is calculated from the enclosed fields.')    

    SELF.Add1Q(']~ ~' ,'PROPLIST:Header of [Group]' ,'Group Heading Text', |
        'A string enclosed in ~tildes~  displays the header at the top of the Group. Heading is specified after [](width). Alignment may be specified after the second tilde as a header justification parameter LRCD and (Indent).')    

!    SELF.Add1Q('~~' ,'PROPLIST:Header'      ,'Heading of Column ~Header~Justification(Indent)','A string enclosed in ~tildes~  displays the header at the top of the list. The header displays with the same alignment as the field data unless the tilde is followed by a header justification parameter LRCD (PROPLIST:HeaderLeft, :HeaderRight, :HeaderCenter, :HeaderDecimal) and/or indent value in (parentheses).')
    
    SELF.Add1Q(']~()' ,'PROPLIST:HeaderLeftOffset :HeaderRightOffset :HeaderCenterOffset :HeaderDecimalOffset of [Group]' ,'Group Heading Indent from Justification', |
        'An optional (Indent) integer, enclosed in parentheses, that specifies in DLUs the indent from the justification. This may be negative. With Left justification indent defines a left margin ; with Right or Decimal, it defines a right margin; and with Center it defines an offset from the center of the field (negative = left offset).' & |
        ' Group Header Offset is specified after []~Heading~ and Alignment LRCD e.g. [field-columns](group width)~Group Head~L(offset).')    

    SELF.Add1Q('HB()' ,'PROPLIST:HdrBackColor','Color of Header Background'     ,'An "HB(color)" specifies the column header background color.')
    SELF.Add1Q('HT()' ,'PROPLIST:HdrTextColor','Color of Header Text'           ,'An "HT(color)" specifies the column header text color. Confused by the templates as a Tree so best to assign at runtime.')
!    SELF.Add1Q('','prop','','')

!TODO assign the category from the   See ListBoxFormatterPRJ.PNG
!Header     HB HT S()
!Data       # LRCD @
!Flags      F * /
    
    SORT(ModifierQ, ModQ:Sort) 
    GET(ModifierQ, 1) ; ?List:ModifierQ{PROP:Selected}=1
    SELF.Init2() 
    RETURN 
HelpCls.Add1Q   PROCEDURE(STRING _Char, STRING _Prop, STRING _Name, STRING _Desc)  
P4D     STRING(128),AUTO
Cln     LONG,AUTO
    CODE
    ModQ:Char = _Char
    ModQ:Name = _Name
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
     
    !HelpSyntax 
    HelpSyntax = 'Width      PROPLIST:Width    Width of the Column or Group in DLUs' &|
     '<13,10>Justify L  PROPLIST:Left     Left align    (Indent) PROPLIST:LeftOffset' &|
     '<13,10>Justify R  PROPLIST:Right    Right align   (Indent) PROPLIST:RightOffset' &|
     '<13,10>Justify C  PROPLIST:Center   Center align  (Indent) PROPLIST:CenterOffset' &|
     '<13,10>Justify D  PROPLIST:Decimal  Decimal align (Indent) PROPLIST:DecimalOffset' &|
     '<13,10>Indent ()  PROPLIST:xOffset  Indent / Offset from Justification' &|
     '<13,10>Modifiers  PROPLIST:xxxxxxx  Characters (listed below) to modify display format'  ! Length = 509 
   
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
    DataIndent=CHOOSE(~GenSim:Indent    OR GenSim:JustLCR   ='C','','('&GenSim:Indent&')')    !Center ignoreIndent
    HdrIndent =CHOOSE(~GenSim:HdrIndent OR GenSim:HdrJustLCR='C','','('&GenSim:HdrIndent&')')
    HdrJustify=GenSim:HdrJustLCR
    IF HdrJustify=GenSim:HdrJustLCR AND HdrIndent=DataIndent THEN !If Header Same no Need
       HdrJustify='' ; HdrIndent=''
    END
    HdrText=CLIP(GenSim:HeaderText)
    LOOP ColX=1 TO GenSim:Columns
        Fmt=GenSim:Width & |
            GenSim:JustLCR & DataIndent & |
            CHOOSE(~GenSim:RightBorder,'','|') & |
            CHOOSE(~GenSim:Underline  ,'','_') & |
            CHOOSE(~GenSim:Fixed      ,'','F') & |
            CHOOSE(~GenSim:Resize     ,'','M') & |
            CHOOSE(~GenSim:Colored    ,'','*') & |
            CHOOSE(~GenSim:StyleY     ,'','Y') & |
            CHOOSE(~HdrText ,'','~'& HdrText &'_'& ColX &' ~'& HdrJustify & HdrIndent) & |
            CHOOSE(~GenSim:Picture,'','@'& CLIP(GenSim:Picture) &'@') & |
            CHOOSE(~GenSim:FieldNo,'','#'& ColX &'#') & |
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
GenFmt.SimpleCopyBtn    PROCEDURE(BYTE CopyType)
FmtCB ANY
    CODE 
    FmtCB=' FORMAT(''' & QUOTE(No1310(GenSim_Format)) &''')'
    CASE CopyType
    OF 1 ; SETCLIPBOARD(FmtCB)
    OF 2 ; SETCLIPBOARD(GenSim_FIELDS)
    OF 3 ; SETCLIPBOARD(FmtCB &' ,|<13,10> '& GenSim_FIELDS)
    END
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
GetExample        PROCEDURE(BYTE ExpNo)!,STRING
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
    CODE 
    IF ~ExpNo THEN 
        ExpNo=POPUP('Customer Meter|Simple Emp Type Tag|Employee Browse|Everything|Preview All Modifiers')
    END
    EXECUTE ExpNo
    RETURN Exp1
    RETURN Exp2
    RETURN Exp3 
    RETURN Exp4
    RETURN Exp5
    END
    RETURN Exp3

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
ModifierHelpPopup PROCEDURE(STRING XPos, STRING YPos) 
ModWin WINDOW('Modifiers'),AT(,,180,210),GRAY,RESIZE,SYSTEM,FONT('Segoe UI',8),ICON('LFmtIcon.ico')
        TEXT,AT(0,0),USE(ModifierHelp),FLAT,HVSCROLL,READONLY,FULL,FONT('Consolas',9)
    END
P LONG,DIM(4),STATIC
ThreadOpen LONG,STATIC
    CODE  
    !03/31/21 ThreadSafe  .... IF ThreadOpen ==========0             !ICE() will set ThreadOpen=THREAD() IF it was =0
    IF InterlockedCompareExchange(ThreadOpen,THREAD(), 0) <> 0 THEN  !Returns initial value of ThreadOpen so IF <>0 is already running
       POST(EVENT:User+1,,ThreadOpen)                                !Possible Race Condition that TheadOPen was set to Zero by a closing window in this time slice
       RETURN
    END
    !03/31/21 This "ThreadOpen=THREAD()" is done in InterlockedCompareExchange() only when ThreadOpen=0
    OPEN(ModWin) 
    IF P[3] THEN SETPOSITION(0,P[1],P[2],P[3],P[4]) ELSE SETPOSITION(0,Xpos,YPos).
    ?ModifierHelp{PROP:Color}    =80000018h !COLOR:InfoBackground
    ?ModifierHelp{PROP:FontColor}=80000017h !COLOR:InfoText
    ACCEPT 
        CASE EVENT()
        OF EVENT:User+1
           0{PROP:Iconize}=0 ; 0{PROP:Active}=1
        END
    END
    GETPOSITION(0,P[1],P[2],P[3],P[4])
    InterlockedExchange(ThreadOpen,0)   !03/31/21 does ThreadOpen=0 Thread Safe
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
   
FontRtn ROUTINE
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