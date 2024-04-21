PreviewFormatWindow PROCEDURE(STRING pListFormat, STRING pCaption)
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
    IF ~pListFormat THEN Message('The Format is blank','Preview Format') ; RETURN.
    Fmt:Format=pListFormat
    ListFEQ=?LIST:View
    OPEN(ViewWindow)
    IF P[3] THEN SETPOSITION(0,P[1],P[2],P[3],P[4]).
    PreviewCls.Init() ; IF PreviewCls.ReflectionBtn THEN HIDE(PreviewCls.ReflectionBtn).
    0{PROP:MinWidth}=100 ; 0{PROP:MinHeight}=100 
    ListFEQ{PROP:LineHeight}=1+ListFEQ{PROP:LineHeight}
    0{PROP:Text}='LIST 411 Preview: '& lower(FORMAT(CLOCK(),@t3)) &'  '& CLIP(pCaption)    
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


