!------------------------------------------------------------
! CBWndPreviewClass (c) Carl Barnes 2018-2021 - MIT License
! Download: https://github.com/CarlTBarnes/WindowPreview
!------------------------------------------------------------
! Disclaimer: This has no intended purpose, if it appears to ... that was coincidental.
! In a Preview generated from the Window Designer an invisbile button at the top allows
! changing Hide,Disable,ReadOnly,Skip. Properties cana be viewed. Controls Resized. More.
! ?????????????????????????????????????????????????????????????????????????????
! ??? See below if ... Window Previewer will not run (displays nothing)     ???
! ???                  or message "could not display window, report problem ???
! ?????????????????????????????????????????????????????????????????????????????
!Region ########## How to Use and Implement this Class ##########
!--------------------- How to Use Class in the Window Previewer ------------------------
! 1. In Accessory\LibSrc\Win add file file named "WindowPreviewer.INC" with the below code:
!=======================================================================================!
!=======================================================================================!

  OMIT('##### How to in WindowPreviewer. INC #####')
!**** File: WindowPreviewer.INC for LibSrc or Project folder ****  
  SECTION('Includes')  !File 
  !Put variable and equate declarations here
  INCLUDE('CBWndPreview.INC'),ONCE
FixNum      BYTE              !Must wait to fix number until InAccept. There is no SECTION for BeforeAccept or AfterData
WndPrvCls   CBWndPreviewClass
!---------------------------------
  SECTION('AfterOpen')
  !Put code that needs to be executed after the window is opened, but before entering the ACCEPT loop
  WndPrvCls.Init(2)             !2=Secret Button Across Top; 1=Upper Left Corner 8x8 (default)
!Optional--> WndPrvCls.AllOffHDRS(1,0,0,0) !All controls 1=Turn off (Hide,Disable,Readonly,Skip) - Not done if Ctrl key down
  0{PROP:System} = 1            !Carl likes a SYSTEM Menu to close window because below set: windowPreviewerCanExit__=False
  LOOP K#=1 TO 255 ; 0{PROP:Alrt,K#}=0 ; END  !Let ESC close window and use Mouse if Window ALRTs. ALRTs do NOT work in a Preview.
!---------------------------------  
  SECTION('InAccept')
  windowPreviewerCanExit__ = FALSE  !Carl likes button click NOT closing window to test Alt+Keys. Escape or SYSTEM Red X will Close. 
 !Optional-->  IF ~FixNum THEN FixNum=1 ; WndPrvCls.AllFixNumbers() ; END !Fix Previewer ####.## numbers to 9999.99
!Put code here that is executed at the top of the ACCEPT loop
!if you set windowPreviewerCanExit__ to FALSE, the generated code will not
!check to see if a button was pressed and exit the preview
!Note that windowPreviewerCanExit__ is reset to TRUE every iteration of the ACCEPT loop
    !end of OMIT('##### How to in WindowPreviewer. INC #####')

!=======================================================================================!
!=======================================================================================!

!------------ How to Use Class in Running EXE Windows to Reflect Properties --------------
! Use at Runtime on any Window by press hot key CtrlShiftW to Init() class that adds the Secret Button.
!
    OMIT('##### ** Add to Any Window Simpler ** #####')
    
    INCLUDE('CBWndPreview.INC'),ONCE            !<-- Add in: Global Includes
CBWndPrv    CBWndPreviewClass                   !<-- Add in: Procedure Data
WINDOW ... ,ALRT(CtrlShiftW)                    !<-- Add in: WINDOW  or after Open(Window) ALERT(CtrlShiftW)
  IF KEYCODE()=CtrlShiftW AND UserBackDoor THEN !<-- Add in Event:AlertKey
     CBWndPrv.Init()                            !    Secret Button: (1)=Top Left, (2)=Full Width
  END

    !end of OMIT('##### ** Add to Any Window Simpler ** #####')

!!-------------------------------------------     

    OMIT('##### ** Add to Any Window SAFER ** #####')
                                 ! Safer to NEW class only when needed so no risk of affect on production code
    INCLUDE('CBWndPreview.INC'),ONCE        !<-- Add in: Global Includes
CBWndPrv    &CBWndPreviewClass              !<-- Add in: Procedure Data
WINDOW ... ,ALRT(CtrlShiftW)                !<-- Add in: WINDOW  or after Open(Window) ALERT(CtrlShiftW)
  OF EVENT:AlterKey                         !<-- Add in Event:AlertKey below lines:
  IF  DeveloperRunningSecretFlag  |         !        Way to be secret, e.g. EXISTS('C:\Clarion11')
  AND KEYCODE()=CtrlShiftW        |         !        Secret Alerted key on Window
  AND CBWndPrv &= NULL THEN       THEN      !        Have not done this already?
        CBWndPrv &= NEW(CBWndPreviewClass)
        CBWndPrv.Init()                     !    Secret Button: (1)=Top Left, (2)=Full Width
  END

  IF NOT CBWndPrv &= NULL THEN DISPOSE(CBWndPrv).   !<-- Add after Close(Window) else may be small leak
!
!   !End of OMIT('##### ** Add to Any Window SAFER ** #####')

!------------------------------------------
! At runtime in the Window press Ctrl+Shift+W and the Class will Init and add a Secret Button at the Top Left. Click that button.
!-----------------------------------------------------------------------------------------
!EndRegion ########## How to Use and Implement this Class ##########


    OMIT('_EndOfInclude_',_IFDef_CBWndPreview_)
_IFDef_CBWndPreview_  EQUATE(1)

!----------------- CBWndPreviewClass -----------------
CBWndPreviewClass    CLASS,TYPE,MODULE('CBWndPreview.CLW'),LINK('CBWndPreview.CLW')

!You Only Call Init(Window). 
Init            PROCEDURE(BYTE SecretBtnType=1, LONG SecretKey=1879) !1879=Alt+Ctrl+Shift+W
Init            PROCEDURE(Window W, BYTE SecretBtnType=2, LONG SecretKey=1879) !1879=Alt+Ctrl+Shift+W
InitList        PROCEDURE(LONG ListFEQ, *QUEUE ListFromQueue, <STRING NameOfQ>) !Set List{'FromQ'}
AllOffHDRS      PROCEDURE(BOOL UnHide=1, BOOL UnDisable=1, BOOL UnReadOnly=1, BOOL UnSkip=1) !1=Turn Off (Hide,Disable,Readonly,Skip) - UNLESS Control Key is down
AllFixNumbers   PROCEDURE() !Fix #####.## numbers to 9999.99 - CANNOT do in SECTION('AfterOpen'), must do in Once SECTION('InAccept')
Start           PROCEDURE() ! START HERE Open the Controls List i.e. just calls .Reflection()
QueueReflection PROCEDURE(*QUEUE ListFromQueue, STRING NameOfQueue, BYTE ViewQRecords=0) !View Queue
ClassReflection PROCEDURE(*GROUP ClassRef, STRING NameOfClass) !03/14/21
GroupReflection PROCEDURE(*GROUP GroupRef, STRING NameOfGroup) !03/14/21
FileReflection  PROCEDURE(*FILE FileRef, <STRING NameOfFile>)  !View File RECORD
ShowReflection  PROCEDURE(Window W, SIGNED SelectedLast)  !MG Call into .Reflection
InitWorker      PROCEDURE(Window W) 
Glo_IsPreviewEXE  &BYTE    !&=Glo:IsPreviewEXE
Glo_Built         &PSTRING !&=Glo:Built (38)
GloT_Caption      &PSTRING !&=GloT:Caption (200)

!Internal Properties
!---------- 
ReflectionBtn           LONG 
ButtonShowsMsg          BYTE        !Buttons normally close preview. Instead show message
WndRef                  &WINDOW
WndTypeNo               BYTE
WndTypeName             PSTRING(11)
WndLabel                PSTRING(8)
MenuItemShows           BYTE
SelectedLast            LONG
!Internal Methods
!-------
Construct      PROCEDURE()
Destruct       PROCEDURE()         !Btn 1=UpperLeft 2=FullWidth
Kill           PROCEDURE()
TakeSelected   PROCEDURE(),BYTE
TakeAccepted   PROCEDURE(),BYTE
Reflection     PROCEDURE() !LIST of Controls on WndRef. Unhide/Enable. See SYS Window Ctrl Props. Resize!
WindowPROPs    PROCEDURE()
ControlPROPs   PROCEDURE(LONG FEQ, LONG FeqTypeNo, STRING FeqTypeName, STRING FeqName)
PROPsEditor    PROCEDURE(LONG FEQ, LONG FeqTypeNo, STRING FeqTypeName, STRING FeqName)
ResizeControl  PROCEDURE(LONG FEQ, LONG FeqTypeNo, STRING FeqTypeName, STRING FeqName)
ResizeWindow   PROCEDURE()
SnapToPreview  PROCEDURE() 
ListPROPs      PROCEDURE(LONG ListFEQ, LONG FeqTypeNo, STRING FeqTypeName, STRING FeqName) 
ListReFORMAT   PROCEDURE(LONG FEQ, LONG FeqTypeNo, STRING FeqTypeName, STRING FeqName) 
SystemPROPs    PROCEDURE()
SystemMetrics  PROCEDURE()
SideDoorCall   PROCEDURE(LONG CallDoorNo, LONG FEQ, LONG FeqTypeNo, STRING FeqTypeName, STRING FeqName)
GuideLines     PROCEDURE(LONG FEQ, LONG FeqTypeNo, STRING FeqTypeName, STRING FeqName)
PropPickList   PROCEDURE(*STRING OutHexProp, <Parse7QType InP7Q>),BOOL,PROC
Win32PropsAdd  PROCEDURE(PropQType PrpQ, LONG HndWnd, LONG FEQ, LONG FeqTypeNo)     !Windows API properties
PropDescribe   PROCEDURE(LONG PropEqt,*STRING InOutValue, BYTE BigLong2Hex),BOOL,PROC
PropQAdd       PROCEDURE(PropQType PrpQ, LONG PQEquate, STRING PQName, STRING PQVal, <STRING FilterVals>, BYTE AddBlank=0)
PropQCopy2Clip PROCEDURE(PropQType PrpQ)   !Copy PQ to Clipboard
ConfigGetAll   PROCEDURE() 
ConfigPut      PROCEDURE(*? CfgValue)
!ConfigPutLONG PROCEDURE(STRING ValueName, LONG ValueLONG)
!ConfigPutSTRING PROCEDURE(STRING ValueName, STRING ConfigValue)
!Internal Data
!-------------
AtSetOrSave    PROCEDURE(BYTE Set1Save2, *LONG[] AtXYWHArray, BYTE NoSetXY=0),PRIVATE  !TODO move inside?
FeqNmQ         &FeqNameQType,PRIVATE
GetFEQName     PROCEDURE(LONG FEQ),STRING
FeqCreatedByCB PROCEDURE(LONG Feq2Check),LONG
GGLines     GROUP
TRNControls   BYTE
GdHide        BYTE  !HideGuides    BYTE
GdFEQ         LONG,DIM(2)
GrdArea       USHORT
            END
GridQ       &GridQType,PRIVATE
BoxIt       PROCEDURE(LONG FEQ=0)
BoxItFEQ    LONG
!------------ Event Logging 06/26/20
EvtLog  GROUP
Started   BYTE
LogName   CSTRING(36)
        END
EvtLogStart   PROCEDURE(BYTE Ask2Confirm=0)
EvtLogStop    PROCEDURE()
EvtLogRegEvts PROCEDURE(BYTE UnRegister=0),PROTECTED
EvtLogTakeEvt PROCEDURE(),BYTE
EvtLogWrite   PROCEDURE(STRING LogLine)
EvtLogEVENT   PROCEDURE(),STRING
EvtLogField   PROCEDURE(<LONG F>),STRING
       END

!??????????????????????????????????????????????????????????????????????????????????????????????
! Window Previewer will NOT run, displays nothing: Install the .Net 2.0 and 3.5 redistributable
!
!   Read: https://clarionhub.com/t/how-to-solve-the-window-previewer-could-not-display-the-window/3545?u=carlbarnes
!
!   sv.clarion.clarion10 (sv.clarion.clarion10) - Subject: Re: Cannot Preview any windows on C10 
!   On 12.05.2016 00:49, robert.zaunere wrote:
!   You need to install the .Net 2.0 redistributable to resolve that error. The IDE uses .Net 3.5
!   so that its backward compatible to use on XP machines, but the MSBuild still requires .Net 2 FW.
!----------------------
! ? Message: The Window Previewer could not display the window. ... Report problem to SV ... ?
! Best bet to to grab the source of the generated previewer and compile it yourself.
!   1. Save the below PrvGrab.BAT to your Project folder
!   2. Run the BAT file and it will show a DOS window repeatly trying to copy
!   3. Try to Preview the window and get the message
!   4. In the PrvGrab DOS Window press Ctrl+C and Terminate 
!   5. In the Sub Folder "PrvGrab" you will have the WinPreview####3 CLW and other files
!   6. I always make a new project and past the WinPreview CLW code into it. I have not been able to compile that CWProj
!----------------- Save below as PrvGrab.BAT -------------
!    ECHO OFF
!    Md PrvGrb
!    :Again
!    Copy WinPreview????????-*.* PrvGrb
!    ECHO Copy WinPreview - Press Ctrl+C to Stop
!    GOTO :Again 
!----------------- Save above as PrvGrab.BAT -------------
!????????????????????????????????????????????????????????????????????????????????????????????
    _EndOfInclude_