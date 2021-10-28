!Quick way to see contents of Temp Folder
  PROGRAM
  INCLUDE 'KEYCODES.CLW'
  INCLUDE('CBWndPreview.INC'),ONCE

    OMIT('**END**')
!### Test Exampes !!!!!!!!!!!!   Findclean 

IOStatsType GROUP,TYPE
CntIN        LONG
BytesIN      LONG
CntOUT       LONG
BytesOUT     LONG
            END
            
CleanQ  QUEUE,PRE(ClnQ)
FromQ       STRING(7)           !CleanQ.FromQ     = 
SubFolder   STRING(20)          !CleanQ.SubFolder = 
Find        GROUP!(IOStatsType). !CleanQ.Find      
CntIN        LONG
BytesIN      LONG
CntOUT       LONG
BytesOUT     LONG
            END
Repl        GROUP!(IOStatsType). !CleanQ.Repl 
CntIN        LONG
BytesIN      LONG
CntOUT       LONG
BytesOUT     LONG
            END     
PathBS      PSTRING(256)        !CleanQ.PathBS    = 
PathTip     STRING(256)         !CleanQ.PathTip   = 
CleanMsg    STRING(512)         !CleanQ.CleanMsg  = 
CleanTip    STRING(512)         !CleanQ.CleanTip  = 
VersionNo   DECIMAL(5,2)        !CleanQ.VersionNo =  
PathLwrBS   PSTRING(256)        !CleanQ.PathLwrBS = 
        END

FORMAT('[' &|
          '75L(2)|FM~<13,10>From Tab~@s8@/'                                                                                                    &|
          '75R(2)|_FM~Folder~@s255@'                                                                                                           &|
                                   ']|'                                                                                                        &|
        '['                                                                                                                                &|
          '24R(2)|M~Cnt~C(0)@n4@#4#'                                                                                                           &|
          '34R(2)|M~Bytes~C(0)@n7@/'                                                                                                           &|
          '24R(2)|_M@n4b@Q''Find Count After'''                                                                                                &|
          '34R(2)|_M@n7b@'                                                                                                                     &|
                         ']|~Find Patterns<13,10>Before/After~'                                                                                &|
        '['                                                                                                                                &|
          '24R(2)|M~Cnt~C(0)@n4@#9#'                                                                                                           &|
          '34R(2)|M~Bytes~C(0)@n7@/'                                                                                                           &|
          '24R(2)|_M@n4b@Q''Replace Count After'''                                                                                             &|
          '34R(2)|_M@n7b@'                                                                                                                     &|
                         ']|~Replace Pattrns<13,10>Before/After~'                                                                              &|
        '['                                                                                                                                &|
          '257L(2)P~Delete Key will remove rows to omit from shrink<13,10>Path  -  Double Click to Open - Right Click for Options~@s255@Z(1)/' &|
          '257R(2)_P~Clean message~@s255@'                                                                                                     &|
                                         ']'            

Students        FILE,DRIVER('TOPSPEED'),PRE(STU),CREATE,BINDABLE,THREAD                     
KeyStudentNumber    KEY(STU:Number),NOCASE,OPT,PRIMARY                     
MajorKey            KEY(STU:Major,STU:LastName,STU:FirstName),DUP,NOCASE,OPT                     
KeyLastName         KEY(STU:LastName),DUP,NOCASE                          
KeyGradYear         KEY(-STU:GradYear,STU:LastName,STU:FirstName),DUP,NOCASE,OPT                     
DynoKey             INDEX,NOCASE                   
Photograph             BLOB                        
Record              RECORD,PRE()
Number                 LONG        !@P###-##-####P                   
FirstName              STRING(20)                  
LastName               STRING(20)                  
Address                STRING(20)                  
Address2               STRING(20)                  
City                   STRING(20)                  
State                  STRING(2)                   
Zip                    LONG        !@n05                  
Telephone              STRING(12)                  
Major                  LONG        !@n02b
GradYear               LONG        !@n4b                        
                    END
                END
                
!### Test Exampes !!!!!!!!!!!!  
    !end of OMIT('**END**')

  MAP
TempPurgeTmp    PROCEDURE() 
Picture_N_Width  PRoCEDURE(SHORT pDigitsTotal, SHORT pDecimals, BOOL pMinus, BOOL pCommas, STRING pBlankB, *STRING OutPicture ),SHORT,PROC 
    MODULE('Win32')
        GetTempPath(LONG nBufferLength,*CSTRING lpTempPath),LONG,PASCAL,DLL(1),RAW,NAME('GetTempPathA'),PROC
    END          
  END

  CODE
 ! message('Test Pic @n_-8 = ' & Format(Clock(),@n-_8) ) ; return 
  TempPurgeTmp()
!-----------
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
    RETURN PicWidth            

           OMIT('**END**')
           dDigits=SUB(PPart,1,Comma-1)      !           Is = just "9"
           dDecimals=PPart              ; IF dDecimals < 0 THEN dDecimals=0.
           dDigits -= dDecimals
              IF ~NoCommaCnt THEN 
                  dDigits += INT( (dDigits-1)/3 )  !room for commas???
              END 
              Pic_Fld='n' & MinusSgn &  |
                            (MinusCnt + dDigits+dDecimals + CHOOSE(dDecimals=0,0,dDecimals+1)) & |   
                            CHOOSE(dDecimals=0,'','.'& dDecimals) & |
                            GenQue:Pic_Dec_Blank
           
                Pic_Fld='n' & MinusSgn & NoComma_ & | !bad  ( (3+GenQue:Digits_LONG + MinusCnt - NoCommaCnt * 3) & |   !was (13 + MinusCnt - NoCommaCnt * 3)
                             (13 + MinusCnt - NoCommaCnt * 3) & |
                             GenQue:Pic_Int_Blank
    !end of OMIT('**END**')
                             
!-----------
TempPurgeTmp    PROCEDURE()
WinTempBS  CSTRING(256)
QNdx    LONG,AUTO

DirQ    QUEUE(FILE:Queue),PRE(DirQ)
AnyTest     ANY
        END  ! DirQ:Name  DirQ:ShortName(8.3?)  DirQ:Date  DirQ:Time  DirQ:Size  DirQ:Attrib

    OMIT('**END**')     !Based on FILE:Queue
DirQ   QUEUE,PRE(DirQ)
Name           STRING (255)
ShortName      STRING (13)
Date           LONG
Time           LONG
Size           LONG
Attrib         BYTE
Amount         DECIMAL(9 ,2)
             END
    !end of OMIT('**END**')
             
PicTestQ QUEUE,PRE(PicQ)
S300        STRING(1200)  !PicQ:s300
         END 
WndPreview  CBWndPreviewClass         
    CODE
    PicQ:s300=ALL('I',255-4) & '_255-7890123456789012345678901234567890'
    PicQ:s300[1000: SIZE(PicQ:s300) ]='One Thousand ' & ALL('1234567890',200)
    LOOP 1 times ; ADD(PicTestQ) ; END 
    IF ~GetTempPath(SIZE(WinTempBS),WinTempBS) THEN RETURN.
    DO LoadFilesRtn 
!--------
LoadFilesRtn    ROUTINE
    DATA
!QNdx    LONG,AUTO
!DirQ    QUEUE(FILE:Queue),PRE(DirQ)
!        END ! DirQ:Name  DirQ:ShortName(8.3?)  DirQ:Date  DirQ:Time  DirQ:Size  DirQ:Attrib
SortNow     SHORT(1)
SortLast    SHORT(1)
SortNowWho  STRING(128)
SortLastWho STRING(128)
_CompileDirTestWnd_ EQUATE(1)
!  FORMAT('80L(2)|_M~Column_1 ~C80L(2)|_M~Column_2 ~C80L(2)|_M~Column_3 ~C80L(2)|_M~Column_4 ~C80L(2)|_M~Column_5 ~C80L(2)|_M~Column_6 ~C')
FilesWindow WINDOW('Test Format Blank Picture'),AT(,,600,350),CENTER,GRAY,SYSTEM,ICON(ICON:Pick), |
            FONT('Segoe UI',9,,FONT:regular),RESIZE
        SHEET,AT(1,1),FULL,USE(?Sheet1),JOIN
            TAB('Format Test'),USE(?Tab:Dir),HIDE
                LIST,AT(1,21,,100),FULL,USE(?ListFiles),VSCROLL,TIP('Click heads to sort, Right Clic' & |
                        'k to reverse, Ctrl+C to Copy'),FROM(DirQ),FORMAT('120L(1)|M~Name~@s255@52L(' & |
                        '1)|M~Short Name~@s13@40R(1)|M~Date~C(0)@d1@40R(1)|M~Time~C(0)@T4@56R(1)|M~S' & |
                        'ize~C(0)@n13@16L(1)|M~Attr~@n3@'),ALRT(MouseLeft), ALRT(CtrlC)
                LIST,AT(1,130,,120),FULL,USE(?ListFiles2),VSCROLL,FROM(DirQ),FORMAT('200L(2)|M~Name ' & |
                        '~L(2)@s255@0L(2)|M~ShortName ~L(2)@s13@46R(2)|M~Date ~C(0)@d8@34R(2)|M~Time' & |
                        ' ~C(0)@t7@48R(2)|M~Size ~C(0)@n-14@')
            END
            TAB('List Column 256+'),USE(?Tab:Pics)
                ENTRY(@s255),AT(27,275,,11),FULL,USE(?xxx)
                LIST,AT(8,22,,77),FULL,USE(?List:PicTestQ),VSCROLL,FROM(PicTestQ),FORMAT('[80L(2)~Fo' & |
                        'rmat Picture:  Blank ~/80L(2)~Format Picture: @s255~@s255@/#1#80L(2)~Format' & |
                        ' Picture: @s200 -- Runtime set Picture=@s1029~@s200@#1#]|')
                PROMPT('@s255 Entry'),AT(7,108),USE(?PROMPT1)
                ENTRY(@s255),AT(52,108,,11),FULL,USE(PicQ:s300,, ?PicQ:s300_255)
                PROMPT('@s1024 Entry'),AT(7,122),USE(?PROMPT2)
                ENTRY(@s255),AT(52,122,,11),FULL,USE(PicQ:s300,, ?PicQ:s300_1024)
                STRING('ENTRY PROP:Text = @s1024 at runtime works'),AT(7,135),USE(?PROMPT2:2), |
                        FONT('Consolas')
                LIST,AT(8,153,,77),FULL,USE(?List:PicTestQ2),VSCROLL,VCR,FROM(PicTestQ), |
                        FORMAT('80L(2)~Format Picture:  Blank ~S(1200)/')
            END
        END
    END
Dir2Clp     ANY
ColNo USHORT
!    FORMAT('200L(2)|M~Name ' & |
!                        '~@s255@48L(2)|M~ShortName ~@s13@37R(2)|M~Date ~L(2)@d17@29R(2)|M~Time ~L(2)' & |
!                        '@t7@48R(2)|M~Size ~L(2)@n13@20R(2)|M~Attrib ~L(2)@n3@51R(2)|M~Amount ~L(2)@' & |
!                        'n14.2@')
                        
    CODE
    FREE(DirQ)
    CLEAR(DirQ)
    IF 1 THEN  
       DIRECTORY(DirQ,WinTempBS&'*.*',ff_:NORMAL)
    ELSE 
       DIRECTORY(DirQ,WinTempBS&'*.tmp',ff_:NORMAL)
       DIRECTORY(DirQ,WinTempBS&'*.wmf',ff_:NORMAL)
    END 

    !--Keep directories, Delete files, Case Name
    LOOP QNdx = RECORDS(DirQ) TO 1 BY -1
         GET(DirQ,QNdx)
         IF BAND(DirQ:Attrib,FF_:Directory) OR DirQ:Name='.' OR DirQ:Name='..'   !. or .. Dirs
            DELETE(DirQ)
         ELSE
             DirQ:Name=UPPER(DirQ:Name[1]) & DirQ:Name[2 : SIZE(DirQ:Name) ] 
             DirQ:AnyTest = DirQ:Name
             PUT(DirQ)
         END
    END 
    SortNow=1
    SortNowWho=WHO(DirQ,SortNow) 
    SORT(DirQ , SortNowWho)  !   , DirQ:ShortName , DirQ:Date , DirQ:Time , DirQ:Size , DirQ:Attrib )    
    
    !===== Test Window to View Directory() ========================================
    SYSTEM{PROP:PropVScroll}=1   
    OPEN(FilesWindow)
    ?List:PicTestQ{PROP:LineHeight} = 2 + ?List:PicTestQ{PROP:LineHeight}
    WndPreview.Init() 

    ?List:PicTestQ{PROPLIST:Picture,3}='@s1029'  !Set Column Picture at runtime > 255 ?
    LOOP ColNo=1 TO 3
        ?List:PicTestQ{PROPLIST:Header, ColNo} = ?List:PicTestQ{PROPLIST:Header,ColNo} & |
              ' -- Runtime PROPLIST:Picture="' & ?List:PicTestQ{PROPLIST:Picture,ColNo} &'"'
    END 
    ?PicQ:s300_1024{PROP:Text}='@s1024'  !set ENTRY at runtime

    ACCEPT
       CASE FIELD()
       OF ?ListFiles
          CASE EVENT() !Click Header to Sort, Right Click Reverses
          OF EVENT:NewSelection !DblClick to View 1 File
             IF KEYCODE()=MouseLeft2 THEN
                GET(DirQ,CHOICE(?ListFiles))
                MESSAGE(CLIP(DirQ:Name) &'|'& DirQ:ShortName &'|'& DirQ:Date &'|'& DirQ:Time &'|'& DirQ:Size |
                        &'|'& DirQ:Attrib,'File '&CHOICE(?ListFiles),,,,2)
             END
          OF EVENT:PreAlertKey
             IF KEYCODE()=CtrlC THEN
                Dir2Clp='Name<9>Short<9>Date<9>Time<9>Size<9>Attr'
                LOOP QNdx = 1 TO RECORDS(DirQ)
                   GET(DirQ,QNdx)
                   Dir2Clp=Dir2Clp & CLIP('<13,10>' & |
                           CLIP(DirQ:Name) &'<9>'& DirQ:ShortName &'<9>'& FORMAT(DirQ:Date,@d02) &'<9>'& FORMAT(DirQ:Time,@t04) |
                           &'<9>'& DirQ:Size &'<9>'& DirQ:Attrib)
                END !LOOP
                SETCLIPBOARD(Dir2Clp)
             END !IF CtrlC
             IF ?ListFiles{PROPList:MouseDownRow} > 0 THEN CYCLE.
             IF ?ListFiles{PROPList:MouseDownZone} = LISTZONE:right THEN CYCLE.
          OF EVENT:AlertKey
             IF ?ListFiles{PROPList:MouseDownZone} = LISTZONE:right THEN CYCLE.
             SortNow=?ListFiles{PROPList:MouseDownField}
             IF ?ListFiles{PROPList:MouseDownRow} = 0  AND SortNow THEN
                IF SortNow<>ABS(SortLast) THEN SortLastWho=',' & SortNowWho.
                SortNowWho=CHOOSE(SortNow=SortLast,'-','+') & WHO(DirQ,SortNow)
                0{PROP:Text}='Directory SORT (' & SortNow &' /'& SortLast &') ' & CLIP(SortNowWho) & SortLastWho
                SORT(DirQ,CLIP(SortNowWho) & SortLastWho)
                SortLast = CHOOSE(SortNow=ABS(SortLast),-1*SortLast,SortNow)
                DISPLAY
             END
          END ! Case EVENT()
       END ! Case FIELD()
    END !ACCEPT
    CLOSE(FilesWindow)
    EXIT
    