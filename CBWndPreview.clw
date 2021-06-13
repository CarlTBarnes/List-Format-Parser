 MEMBER()
!------------------------------------------------------------
! CBWndPreviewClass (c) Carl Barnes 2018-2021 - MIT License
! Download: https://github.com/CarlTBarnes/WindowPreview
!------------------------------------------------------------
VersionWndPrv EQUATE('WndPrv 06-12-21.0111')  !11.1
    INCLUDE('KEYCODES.CLW'),ONCE
    INCLUDE('EQUATES.CLW'),ONCE
CREATE:Slider_MIA   EQUATE(36)      !Not defined in Equates until C11 sometime
PROP:Precedes_C10   EQUATE(7A7EH)   !Not in C9.1 - Inverse of PROP:Follows read-only, runtime only 
!Region Global TYPE's
PropQType  QUEUE,TYPE 
EqtHex      STRING(5)      !PQ:EqtHex
Name        STRING(32)     !PQ:Name
Value       STRING(255)    !PQ:Value        !TODO Have RawValue without Description
ValueBIG    &STRING        !PQ:ValueBIG 
EqtLong     LONG           !PQ:EqtLong    
          END
FeqNameQType    QUEUE,PRIVATE     
FEQ               LONG          !FeqNmQ:FEQ
Name              CSTRING(64)   !FeqNmQ:Name
                END
Parse7QType  QUEUE,TYPE
EqtLong     LONG           !P7Q:EqtLong 
EqtHex      STRING(5)      !P7Q:EqtHex
Name        STRING(32)     !P7Q:Name
          END
GridQType QUEUE,TYPE
LnFEQ       LONG
          END
QueDeclareType QUEUE,TYPE  !05/25/20
FieldX   USHORT
Name     STRING(64)
DType    STRING(16)
DTypeNo  BYTE
Size     LONG
HowMany  USHORT               
HasValue BYTE !An ANY can take
DValue   STRING(255)
      END
FrmFldQtype QUEUE,TYPE
Column   STRING(3)
FieldX   USHORT
Name     STRING(64)
DType    STRING(16)
Value    STRING(256)
HasValue BYTE
DTypeNo  BYTE
   END         
UFOType INTERFACE,TYPE,PRIVATE
_Type PROCEDURE(LONG _UfoAddr),LONG  !+00h Type of UFO
_2  PROCEDURE
_3  PROCEDURE
_4  PROCEDURE
_5  PROCEDURE
_6  PROCEDURE
_7  PROCEDURE
_8  PROCEDURE
_9  PROCEDURE
_10 PROCEDURE
_11 PROCEDURE
_12 PROCEDURE
_Address PROCEDURE(),LONG !+30h Address of a variable - IFF &Ref and not Null
_14 PROCEDURE
_15 PROCEDURE
_16 PROCEDURE
_17 PROCEDURE
_18 PROCEDURE
_19 PROCEDURE
_20 PROCEDURE
_Max  PROCEDURE(LONG),LONG !+50h Elements in first dimension of an array
_Size PROCEDURE(LONG),LONG !+54h Size of an object
BaseType PROCEDURE(LONG),LONG
      END
!EndRegion Global TYPE's             
    INCLUDE('CBWndPreview.INC'),ONCE
    MAP
ClaFeqName          PROCEDURE(LONG Feq),STRING,PRIVATE
ClaColorEquate      PROCEDURE(LONG ColorRGB, BYTE Verbose=1),STRING,PRIVATE
ClaColorEquate      PROCEDURE(*STRING InOutColorRGB, BYTE Verbose=1),PRIVATE
ClaControlTypeName  PROCEDURE(LONG CtrlPropType),STRING,PRIVATE     !Pass FEQ{PROP:Type} to get name e.g. Button
ClaCursorEquate     PROCEDURE(*STRING InOutCursorProp),PRIVATE
ClaKeyCodeExplain   PROCEDURE(LONG KeyCodeEquate, BYTE Terse=0),STRING,PRIVATE    !Explain KeyCode() Alt+CtrL+Shift
ClaAlign            PROCEDURE(LONG CtrlFEQ, LONG CtrlType),STRING,PRIVATE   !Pass FEQ,{PROP:Type} to get Left,R,C,D
ClaDataType         PROCEDURE(*? DataAny, *STRING OutTypeName, *BYTE OutHasValueForANY, *LONG OutSize),LONG,PROC,PRIVATE
ClaFont4            PROCEDURE(LONG CtrlFEQ, *PSTRING[] Font4),PRIVATE
ClaFont4Net         PROCEDURE(*PSTRING[] WFont, *PSTRING[] CFont),STRING,PRIVATE
ClaFont             PROCEDURE(LONG CtrlFEQ),STRING,PRIVATE
ClaFontStyle        PROCEDURE(LONG FontSyle),STRING,PRIVATE
ClaIconEquate       PROCEDURE(*STRING InOutIconProp),PRIVATE
ClaListColAlign     PROCEDURE(LONG ListFEQ, LONG ColX, BOOL IsGroup=0, BOOL IsHead=0),STRING
ClaListColResizable PROCEDURE(WINDOW WndRef, LONG ListFEQ)
ClaPicture          PROCEDURE(LONG CtrlFEQ, LONG CtrlType),STRING,PRIVATE
ClaSTDprop          PROCEDURE(LONG PropStd),STRING,PRIVATE

HelpCW              PROCEDURE(STRING HelpTxt, BYTE IsPROP=1),PRIVATE
Hex8                PROCEDURE(LONG LongNum, SHORT Digits=0, BYTE AddH=1,BYTE SpaceAt5=0),STRING,PRIVATE
Binary8             PROCEDURE(LONG  Num),STRING,PRIVATE
HexEval             PROCEDURE(*string HexNumber, byte Digits),LONG    !Convert HEX String to LONG,PRIVATE
DropListColor       PROCEDURE(LONG ListFEQ) !Color Drop Yellow (like Tip) to see better
EquateStringFind    PROCEDURE(STRING FindHex, *STRING EqtHexLabel),STRING,PRIVATE
Equate7StringParse  PROCEDURE(Parse7QType Parse7Q, *STRING Prop7String),PRIVATE
EquateXStringParse  PROCEDURE(Parse7QType P7Q, BYTE HexLen, *STRING CP, BYTE IsDecimal=0),PRIVATE
FeqNameUpLow        PROCEDURE(STRING FeqName),STRING,PRIVATE
FmtNumSM            PROCEDURE(STRING Numbr,BYTE pWidth),STRING,PRIVATE  !To align SeeMore Numbers "sort of" add 2 spaces per blank
If1Clear            PROCEDURE(*? TFV, LONG AcceptFEQ),BOOL,PROC,PRIVATE
GroupMoveChildren   PROCEDURE(LONG FromGroup, LONG ToControl, LONG XAdjust=0, LONG YAdjust=0),PRIVATE 
KeyStateSCA         PROCEDURE(BYTE Shft1_Ctrl2_Alt4),BYTE,PRIVATE
LineHt              PROCEDURE(LONG ListFEQ, SHORT LineHeightAdd=1),PRIVATE !List Prop:LineHeight +=1
ListColumnPositions PROCEDURE(LONG ListFEQ, *QUEUE OutColumnPosQ)
ListDrop            PROCEDURE(LONG ListFEQ, BYTE Down0_Up1=0),PRIVATE
ListFormatDejaVu    PROCEDURE(LONG ListFEQ, STRING WindowID, BYTE Closing=0)
ListHelpCW          PROCEDURE(LONG SetBtnTip=0),PRIVATE
ListHelpMods        PROCEDURE(),STRING,PRIVATE
LocateInLIST        PROCEDURE(QUEUE QRef, LONG ListFEQ, LONG TextFEQ, LONG NextBtn, LONG PrevBtn),PRIVATE
MakeOverLIST        PROCEDURE(LONG ListFEQ, BYTE CfgChg=0),PRIVATE
MakeOverWindow      PROCEDURE(),PRIVATE
MaxNum              PROCEDURE(LONG M1, *STRING S2, LONG M3=0, LONG M4=0),LONG,PRIVATE
MaxNum              PROCEDURE(LONG M1, LONG M2, LONG M3=0, LONG M4=0),LONG,PRIVATE
ParenWrap           PROCEDURE(STRING PropVal),STRING,PRIVATE  !If not blank wraps in Parens (#) 
PopItem             PROCEDURE(STRING ItemTxt, <LONG Checked>, <LONG Disabled>,BYTE Pipe012=1),STRING,PRIVATE
PopClean            PROCEDURE(STRING PopItemText),STRING,PRIVATE
PopupBeside         PROCEDURE(LONG CtrlFEQ, STRING PopMenu),LONG,PRIVATE
PopupUnder          PROCEDURE(LONG CtrlFEQ, STRING PopMenu),LONG,PRIVATE            
PropHuntLoadP7Q     PROCEDURE(Parse7QType OutP7Q, LONG FeqTypeNo),PRIVATE
PropText1           PROCEDURE(LONG FEQ, LONG FType, *STRING AltText, BOOL QuoteIt=0),STRING,PRIVATE
PropTFName          PROCEDURE(LONG CtrlFEQ, LONG PROPNumber, STRING TrueName,<STRING FalseName>),STRING,PRIVATE
PropTrashDelete     PROCEDURE(PropQType PQ),PRIVATE
PropViewWindow      PROCEDURE(STRING CapTxt, PropQType PQ, STRING ValueOnly),PRIVATE
PropEditWindow      PROCEDURE(LONG FEQ, STRING FeqName, PropQType PQ, STRING PropValue, BYTE PropIndex=0),PRIVATE
QueueDeclareGet     PROCEDURE(QUEUE inQueue, QueDeclareType DeclareQ),PRIVATE
QueueViewListVLB    PROCEDURE(QUEUE ViewQ, STRING QName),PRIVATE
QueueViewListVLB    PROCEDURE(QUEUE ViewQ, STRING QName, QueDeclareType FrmFldQ),PRIVATE
ReflectDeclareGet   PROCEDURE(*GROUP inGroupClassOrQueue, QueDeclareType DeclareQ, STRING LevelPrefix),LONG,PROC,PRIVATE !03/11/21
ReflectGroupOrQueue PROCEDURE(CBWndPreviewClass PrvCls,BYTE GrpClsFilQue1234,*GROUP GrpClsQueRef, STRING NameOfGCQ,<*QUEUE FromQ>,BYTE ViewQRecords=0),PRIVATE  !03/11/21
ReplaceInto         PROCEDURE(*string Into, string FindTxt,string ReplaceTxt, BYTE ClipInto=0),LONG,PROC,PRIVATE
ReplaceText         PROCEDURE(string InText, string Find,string Repl, BYTE ClipInto=0),STRING
SeeMore             PROCEDURE(LONG PropMore, LONG CtrlFEQ, LONG CtrlTypeNo),STRING,PRIVATE
SetClip2Queue       PROCEDURE(QUEUE Q2Copy, BYTE HeadUL=1, <STRING Head9Text>, <STRING QueWhoPrefix>, USHORT ColMin=1, USHORT ColMax=999),PRIVATE
SetClip2Tab2Space PROCEDURE(STRING TabDelimText, BYTE GapClm=1, BYTE HeadDash=0, BYTE FootDash=0, BYTE NoAsk=0),PRIVATE
TextViewWindow      PROCEDURE(STRING CapTxt, STRING Txt2See, STRING ValueOnly),PRIVATE
HexDumpString       PROCEDURE (*STRING SrcStr, BOOL ClipSpaces=0),STRING,PRIVATE
HexDumpMemory       PROCEDURE(long SrcAddr, Long SrcSize, Byte ShowAddr=0),STRING,PRIVATE
SysMenuCls_SubClassFunc  FUNCTION(LONG hWnd,LONG wMsg,LONG wParam,LONG lParam),LONG,PASCAL,PRIVATE
SysMenuCls_SYSCOMMAND    FUNCTION(LONG hWnd,LONG SMCmd_wParam,LONG lParam=0),PRIVATE
TypeHasAltKey       PROCEDURE(LONG PropType, LONG FEQ=0),BOOL
TypeIsLIST          PROCEDURE(LONG PropType),BYTE ! 1=List 2=Combo Drop +10h
DB                  PROCEDURE(STRING DbTxt),PRIVATE
        MODULE('RTL')
ClaFieldNameRTL  PROCEDURE(LONG pFEQ),CSTRING,RAW,NAME('Cla$FIELDNAME'),DLL(dll_mode)
LenFastClip      PROCEDURE(CONST *STRING Text2Measure),LONG,NAME('Cla$FASTCLIP'),DLL(dll_mode)
!C11 ClaEventNameRTL  PROCEDURE(LONG EventPlusA000h),*CSTRING,RAW,NAME('WslDebug$MsgName'),DLL(dll_mode)
ClaEventNameRTL5 PROCEDURE(*CSTRING OutName, LONG EventPlusA000h)RAW,NAME('WslDebug$NameMessage'),DLL(dll_mode)
    OMIT('**END**', _C100_)  !C9.1 for sure, some C10
C5LogSetName     PROCEDURE(CONST *CSTRING),NAME('_WslDebug$SetLogFile'),DLL(dll_mode)
    !end of COMPILE('**END**', _C110_)
!10.12799 has below, BUT before that 10.12567 was above with _Underscore. So if you use older 10 you'll have to modify this. Proper fix would be to LoadLibrary
    COMPILE('**END**', _C100_)  !C11 for sure, some C10
C5LogSetName     PROCEDURE(CONST *CSTRING),NAME('WslDebug$SetLogFile'),DLL(dll_mode)
    !end of COMPILE('**END**', _C110_)
C5LogPrint       PROCEDURE(STRING),NAME('WslDebug$Print'),DLL(dll_mode)
        END
        MODULE('Win32') !Win API at line 320
AppendMenu      PROCEDURE(LONG hMenu, LONG uFlags, LONG uIDNewItem, <*CSTRING lpNewItem> ),BOOL,RAW,PASCAL,DLL(1),PROC,NAME('AppendMenuA')
GetWindowRect   PROCEDURE(LONG hWnd, LONG lpRect ),PASCAL,DLL(1),RAW,BOOL,PROC
ClientToScreen  PROCEDURE(LONG hWnd, LONG lpPoint),PASCAL,DLL(1),RAW,BOOL,PROC
GetClassName    PROCEDURE(LONG hWnd, *CSTRING lpClassName, LONG nMaxCount ),PASCAL,DLL(1),RAW,LONG,name('GetClassNameA')
GetClientRect   PROCEDURE(LONG hWnd, LONG lpRect ),PASCAL,DLL(1),RAW,BOOL
GetCurrentProcessId PROCEDURE(),LONG,PASCAL,DLL(1)
GetLastError    PROCEDURE(),LONG,PASCAL,DLL(1)
GetSysColor     PROCEDURE(LONG nIndex),LONG,PASCAL,DLL(1)
GetSystemMenu   PROCEDURE(LONG hWnd, BOOL bRevert),LONG,RAW,PASCAL,DLL(1) !Returns HMENU
GetSystemMetrics PROCEDURE(LONG),LONG,PASCAL,DLL(1)
GetWindowLong   PROCEDURE(LONG hWnd, LONG nIndex ),PASCAL,DLL(1),RAW,LONG,name('GetWindowLongA')
CallWindowProc  PROCEDURE(LONG lpPrevWndProc, LONG hWnd, LONG nMsg, LONG wParam, LONG lParam),LONG,PASCAL,DLL(1),NAME('CallWindowProcA')
OutputDebugString   PROCEDURE(*cstring Msg),PASCAL,RAW,NAME('OutputDebugStringA'),DLL(1)
PostMessage     PROCEDURE(LONG hWnd, LONG nMsg, LONG wParam, LONG lParam),BOOL,PASCAL,DLL(1),PROC,NAME('PostMessageA')
HtmlHelp        PROCEDURE(Long hWndCaller, *cstring pszFile, long uCommand, long dwData=0),LONG,PROC,PASCAL,rAW,DLL(_fp_),name('HtmlHelpA')
GetProcAddress  PROCEDURE(long HINSTANCE,*cstring ProcName),LONG,PASCAL,RAW,DLL(1)
LoadLibrary     PROCEDURE(*CSTRING pszModuleFileName),UNSIGNED,PASCAL,RAW,NAME('LoadLibraryA'),DLL(1)
        END
    END    
!Region Global Data                           Global Data
HtmlHelp_fp LONG,NAME('HtmlHelpA')
PWnd &WINDOW,THREAD,PRIVATE     !MOst data should be THREAD incase this is Run in a Real EXE
GloThreaded  GROUP,PRE(),THREAD,PRIVATE
GloT:Caption       PSTRING(200) !Caption of Preview window
GloT:ResizeControl LONG
GloT:ReFormatList  LONG
GloT:Hide          BYTE(1)      !Hide before Open(w)
            END
Globals     GROUP,PRE(),PRIVATE
Glo:IsPreviewEXE  BYTE         !Is WinPreview or Test EXE?
Glo:Built         PSTRING(38)
AtWndReflect      LONG,DIM(4)  !Just W,H
AtCtrlProps       LONG,DIM(4)  !For
AtListPROPs       LONG,DIM(4)  !For ListPROPs
AtPropView        LONG,DIM(4)  !For PropViewWindow
AtReszCont        LONG,DIM(4)  !For ResizeControl
AtReFmtList       LONG,DIM(4)  !For ReFormat List Col
AtReszWind        LONG,DIM(4)  !For ResizeWindow
AtPickProp        LONG,DIM(4)  !For PropPickList
AtSM              LONG,DIM(4)  !For SysMets
            END     
ConfigGrp_DidGET BYTE,PRIVATE !,THREAD
ConfigGrp   GROUP,PRIVATE,PRE(CFG) !,THREAD
ResizeOnMouse2  BYTE !Cfg:ResizeOnMouse2
ResizeSnapTo    BYTE(1) !Cfg:ResizeSnapTo
HideUnders      BYTE !Hide before open a New window
FEQUpLow        BYTE(1)
MakeOvrWnd      BYTE(1)
MakeOvrList     BYTE(1)
GridInc         BYTE(10)
Grid5th         BYTE(5)
GuideClr        LONG(0FFh)
GridClr         LONG(8080h)
BoxItShows      BYTE(1)
            END            
ConfigKey   EQUATE('Software\CarlBarnes\WinPreview')
!EndRegion Global Data
!Region System Menu Class SysMenuClass
SysMenuClsQ     QUEUE,PRE(SysMnQ),THREAD,PRIVATE
WinRef              &WINDOW !SysMnQ:WinRef
hWindow             LONG    !SysMnQ:hWindow
OrigWndProc         LONG    !SysMnQ:OrigWndProc
WasHide12           BYTE    !SysMnQ:WasHide12         !1=Yes 2=No 0=Unknown
DoinHide            BYTE    !SysMnQ:DoinHide
CloseMe             BYTE    !SysMnQ:CloseMe
ZOrder              BYTE    !SysMnQ:ZOrder
HidePWnd            BYTE    !SysMnQ:HidePWnd
                END
SysMenuClass CLASS,TYPE,PRIVATE  
bInited         BYTE
WindowRef       &WINDOW
hWindow         LONG
OrigWndProc     LONG     !SysMenuCls_SubClassFunc FUNCTION(LONG hWnd,LONG wMsg,LONG wParam,LONG lParam),LONG,PASCAL
Init            Procedure(WINDOW WndRef)  
DESTRUCT        Procedure(),VIRTUAL
           END
                ITEMIZE(101)  
SMCmd_Close2Prv   EQUATE    !CLOSE Windows back to the Preview
SMCmd_HideUnder   EQUATE    !HIDE Under Windows
SMCmd_MoveUnder   EQUATE    !MOVE Under Windows under this One
SMCmd_HidePWnd    EQUATE    !Hide Preview
SMCmd_HaltThis    EQUATE    !HALT Me
SMCmd_HaltCapt    EQUATE    !Kill WINDOW Previews with this Caption
SMCmd_HaltALL     EQUATE    !Kill ALL PREVIEW EXEs
SMCmd_RunAgain    EQUATE    !RUN this Preview.exe again
                END           
!EndRegion SysMenuClass
!Region Classes
CwHelpCls CLASS,TYPE,PRIVATE
OpenHelp    PROCEDURE(string sHlp)  !Open a "~Context.htm" or "Keyword"
bInit       BYTE
HHLoaded    BYTE
ChmFile     CSTRING(256)      
IsInited    PROCEDURE(),BYTE
HHOcxLoad   PROCEDURE(),BOOL
HHCaller    PROCEDURE(long HH_Command, long dwData=0)
          END
CBSortClass CLASS,TYPE,PRIVATE
QRef        &QUEUE
FEQ         LONG
ColumnNow   SHORT            
ColumnLast  SHORT            
QFieldNow   SHORT
QFieldLast  SHORT
Who1st      STRING(128)
Who2nd      STRING(129)
Init        PROCEDURE(QUEUE ListQueue, LONG ListFEQ, SHORT SortColumnNow=0)
SetSortCol  PROCEDURE(SHORT SortColNow)
HeaderPressed PROCEDURE(SHORT ForceSortByColumn=0) !Call in OF EVENT:HeaderPressed for LIST
    END
BevClass    CLASS,TYPE,PRIVATE
Style  &LONG
StyFEQ LONG
BOFeq  LONG
Edgz   BYTE,DIM(8)  !Edge Style: 1=No 2=Raise 3=Lower 4=Gray
Init         PROCEDURE(LONG BtnBOFeq, *LONG BevelStyle, LONG StyleFEQ)
StyleChanged PROCEDURE()
BtnClick     PROCEDURE(LONG BtnFEQ)
AllBtn       PROCEDURE()
BtnConfig    PROCEDURE(BYTE Edge1To8, BYTE StyleNRLG),PROTECTED
Edge2Style   PROCEDURE(),PROTECTED
                    END
CBLocateCls CLASS,TYPE,PRIVATE
Init          PROCEDURE(QUEUE QRef, LONG ListFEQ, LONG FindTextFEQ, LONG BtnNextFEQ, LONG BtnPrevFEQ, BYTE Hack=0)
Kill          PROCEDURE()
DisableIfNone PROCEDURE()
Locate        PROCEDURE(SHORT NextPrev=1, BOOL IsButton=0)  !1=Next -1=Prev
TakeAccepted  PROCEDURE(),BYTE,PROC,PROTECTED
TakeAlertKey  PROCEDURE(),BYTE,PROC,PROTECTED
IsInit  BYTE
QRef    &QUEUE 
ListFEQ LONG
TextFEQ LONG
NextBtn LONG
PrevBtn LONG
            END
!EndRegion Classes
!----------------------------------------
CBWndPreviewClass.Construct        PROCEDURE()
!----------------------------------------
    CODE
    SELF.FeqNmQ &= NEW(FeqNameQType) ; SELF.GridQ &= NEW(GridQType)
    SELF.Glo_IsPreviewEXE &= Glo:IsPreviewEXE ; SELF.Glo_Built &= Glo:Built ; SELF.GloT_Caption &= GloT:Caption
    RETURN
!---------------------------------------
CBWndPreviewClass.Destruct PROCEDURE()
!---------------------------------------
    CODE
    IF SELF.EvtLog.Started THEN SELF.EvtLogStop().
    IF NOT SELF.FeqNmQ &= NULL THEN FREE(SELF.FeqNmQ) ; DISPOSE(SELF.FeqNmQ).
    IF NOT SELF.GridQ &= NULL THEN FREE(SELF.GridQ) ; DISPOSE(SELF.GridQ).
    RETURN
!-----------------------------------
CBWndPreviewClass.Init PROCEDURE(BYTE BtnType, LONG SecretKey)
W &WINDOW
    CODE
    IF SYSTEM{PROP:Target}=0 THEN 
       MESSAGE('Failed CBWndPreviewClass.Init because SYSTEM Prop:Target=' & SYSTEM{PROP:Target}) 
    ELSE
       W &= (SYSTEM{PROP:Target})
       SELF.INIT(W,BtnType,SecretKey)
    END
    RETURN
CBWndPreviewClass.Init PROCEDURE(window W, BYTE BtnType=2, LONG SecretKey=1879) !1879=Alt+Ctrl+Shift+W)
Btn LONG
!XQ  QUEUE(FILE:Queue),PRE(XQ).
!EXE STRING(260),AUTO
!Now LONG,AUTO
    CODE
    IF SELF.ReflectionBtn THEN RETURN.
!    SELF.SelectedLast=SELECTED()
!    SELF.WndRef &= W ; PWnd &= W
!    SELF.WndTypeNo=W{PROP:Type}
!    SELF.WndTypeName=CHOOSE(SELF.WndTypeNo-23,'APPLICATION','WINDOW','REPORT','WIN#' & SELF.WndTypeNo )
!    SELF.WndLabel=CHOOSE(SELF.WndTypeNo-23,'AppFrame','Window','Report','WindowUnk')       
!    Now=CLOCK() ; EXE=COMMAND('0') ; DIRECTORY(XQ,EXE,0) ; GET(XQ,1)
!    SELF.MenuItemShows = CHOOSE(W{PROP:Type}=CREATE:application)
!    GloT:Caption=W{PROP:Text}
    !TODO AppFrame Special handling ???? The buttons is on the Toolbar. If no TB will it work. Make a Menu?
    Btn = CREATE(0,CREATE:button)       !Make a Flat Button invisible until hover 
    SELF.ReflectionBtn = Btn 
    IF BtnType=2 THEN 
       SETPOSITION(Btn,0,0,,5)          !Place at 0,0 -- Full Width x 4 Hi 
       Btn{PROP:Full}=1                 !2=Full width at top of window 
    ELSE
       SETPOSITION(Btn,0,0,9,9)         !Place at 0,0 -- 8 x 8
    END
    Btn{PROP:Flat}=1                    !Flat so invsible until hover
!    Btn{PROP:Tip}='Carl''s Window Inspect & Perfect ... or Reflect & Correct - ' & VersionWndPrv & |
!                  '<13,10,13,10>' & CLIP(EXE) & |
!                  '<13,10,13,10>' & CHOOSE(Now-XQ:Time<999,'','Run: ' & FORMAT(Now,@t1)&'   --   ') & |
!                    'Built: ' & FORMAT(XQ:Time,@t4)&' on '&FORMAT(XQ:Date,@d01-) & |
!                  '   --   Process ID: ' & GetCurrentProcessId() &'<13,10>'
!    Glo:Built='(Built: ' & FORMAT(XQ:Time,@t4) &' '& SUB(FORMAT(XQ:Date,@d01),1,5) &CHOOSE(Now-XQ:Time<999,'',' - Run: ' & FORMAT(Now,@t1)) &')'
    Btn{PROP:Skip}=1                    !Skip so no focus 
    Btn{PROP:Key}=SecretKey             !Press Alt+Ctrl+Shift+W for Preview Features
    SELF.InitWorker(W)
    IF BtnType THEN UNHIDE(Btn).
    REGISTEREVENT(EVENT:Accepted,ADDRESS(SELF.TakeAccepted),ADDRESS(SELF)) 
    REGISTEREVENT(EVENT:Selected,ADDRESS(SELF.TakeSelected),ADDRESS(SELF)) 
!    XQ:Name=lower(XQ:Name) ; GLO:IsPreviewEXE=CHOOSE(XQ:Name[1:10]='winpreview' OR INSTRING('test',XQ:Name,1)) 
    RETURN
!--------------
CBWndPreviewClass.InitWorker PROCEDURE(Window W) !For MG to bypass .Init
Btn LONG
XQ  QUEUE(FILE:Queue),PRE(XQ).
EXE STRING(260),AUTO
Now LONG,AUTO
    CODE
    Btn=SELF.ReflectionBtn  !Set to -1 for no button
    SELF.SelectedLast=SELECTED()    
    SELF.WndRef &= W ; PWnd &= W
    SELF.WndTypeNo=W{PROP:Type}
    SELF.WndTypeName=CHOOSE(SELF.WndTypeNo-23,'APPLICATION','WINDOW','REPORT','WIN#' & SELF.WndTypeNo )
    SELF.WndLabel=CHOOSE(SELF.WndTypeNo-23,'AppFrame','Window','Report','WindowUnk')       
    Now=CLOCK() ; EXE=COMMAND('0') ; DIRECTORY(XQ,EXE,0) ; GET(XQ,1)
    SELF.MenuItemShows = CHOOSE(W{PROP:Type}=CREATE:application)
    GloT:Caption=W{PROP:Text}
    IF Btn AND Btn<>-1 THEN 
       Btn{PROP:Tip}='Carl''s Window Inspect & Perfect ... or Reflect & Correct - ' & VersionWndPrv & |
                  '<13,10,13,10>' & CLIP(EXE) & |
                  '<13,10,13,10>' & CHOOSE(Now-XQ:Time<999,'','Run: ' & FORMAT(Now,@t1)&'   --   ') & |
                    'Built: ' & FORMAT(XQ:Time,@t4)&' on '&FORMAT(XQ:Date,@d01-) & |
                  '   --   Process ID: ' & GetCurrentProcessId() &'<13,10>' 
    END                   
    Glo:Built='(Built: ' & FORMAT(XQ:Time,@t4) &' '& SUB(FORMAT(XQ:Date,@d01),1,5) &CHOOSE(Now-XQ:Time<999,'',' - Run: ' & FORMAT(Now,@t1)) &')'
    XQ:Name=lower(XQ:Name) ; GLO:IsPreviewEXE=CHOOSE(XQ:Name[1:10]='winpreview' OR INSTRING('test',XQ:Name,1)) 
    RETURN
!--------------
CBWndPreviewClass.ShowReflection PROCEDURE(Window W, SIGNED SelectedLast)
HoldTarget &Window 
   CODE 
    HoldTarget &= SYSTEM{PROP:Target}
    SETTARGET(W) 
    W{PROP:Active} = TRUE 
    SELF.InitWorker(W)
    SELF.SelectedLast=SelectedLast
    SELF.Reflection()
    SETTARGET(HoldTarget)
    RETURN
!--------------
CBWndPreviewClass.InitList PROCEDURE(LONG FEQ,*QUEUE FrmQ,<STRING NameQ>)
Ref GROUP,AUTO
Q    &QUEUE
L    LONG,OVER(Q)
  END
  CODE
  Ref.Q &=FrmQ ; FEQ{'FromQ'}=Ref.L  !can=INSTANCE(FrmQ,THREAD())
  FEQ{'FromWho'}=CHOOSE(~OMITTED(NameQ),NameQ,'Queue' & Ref.L)
  RETURN
!-----------------------------------
CBWndPreviewClass.AllOffHDRS  PROCEDURE(BOOL UnHide=1, BOOL UnDisa=1, BOOL UnRead=1, BOOL UnSkip=1) !Turn off Hide,Disable,Readonly,Skip - Save clicking
F LONG
    CODE
    IF KeyStateSCA(2) THEN RETURN. !Ctrl is down
    LOOP 2 TIMES  !With GROUPs Disable required 2 passes
      LOOP ; F=0{PROP:NextField,F} ; IF ~F THEN BREAK. ; IF SELF.FeqCreatedByCB(F) THEN CYCLE.
         IF UnDisa THEN F{PROP:Disable}=''. ; IF UnRead THEN F{PROP:ReadOnly}=''. 
         IF UnSkip THEN F{PROP:Skip}=''.    ; IF UnHide THEN F{PROP:Hide}=''.
    END ; END
    DISPLAY ; RETURN
!----------------------------------- 
CBWndPreviewClass.AllFixNumbers  PROCEDURE() !Fix numbers with #####.## to be 9999
F LONG(0)
X USHORT,AUTO
NPic  CSTRING(12),AUTO
Scrn  CSTRING(32),AUTO
Valu  STRING(32),AUTO
V USHORT,AUTO
  CODE
  LOOP ; F=0{PROP:NextField,F} ; IF ~F THEN BREAK. ; IF SELF.FeqCreatedByCB(F) THEN CYCLE.
     NPic=ClaPicture(f,f{PROP:Type}) ; IF NPic[1]<>'n' THEN CYCLE.     
     Scrn=f{PROP:ScreenText} ; X=INSTRING('#',Scrn) ; IF ~X THEN CYCLE.
     V=0 ; Valu=''
     LOOP X=1 TO LEN(Scrn) 
       IF Scrn[X]='#' THEN 
          V+=1 ; Valu[V]='9' !(10-V)%10
       ELSIF X=1 AND ~Scrn[1] AND NPic[2]='-' THEN !lead minus is space, make - so know
          V+=1 ; Valu[V]='-'
       ELSIF Scrn[X]='.' THEN 
          V+=1 ; Valu[V]='.'
       END
     END
     IF V THEN CHANGE(F,Valu[1: V]). !; DB('Pic:' & NPic &'  Scrn:' & Scrn &'  Change:' & Valu)
  END
  DISPLAY ; RETURN    
!-----------------------------------
CBWndPreviewClass.Kill     PROCEDURE()
    CODE
    RETURN
!-----------------------------
CBWndPreviewClass.TakeSelected  PROCEDURE()!,BYTE
S LONG,AUTO 
    CODE  !TODO could keep an Array of last 10 so could have a Popup of recent selects - Change GridQ to FEQQueue for this
    S=SELECTED() ; IF S<>SELF.ReflectionBtn THEN SELF.SelectedLast=S.
    RETURN 0 !Benign
!-----------------------------------
CBWndPreviewClass.TakeAccepted     PROCEDURE()!,BYTE 
RetLvl BYTE(0) !Benign
!A LONG,AUTO
F LONG,AUTO
  CODE
  F=FIELD() ; IF F=SELF.ReflectionBtn THEN SELF.Reflection() ; RETURN Level:Notify.
  IF ~SELF.ButtonShowsMsg OR ~Glo:IsPreviewEXE THEN RETURN 0. !Benign  !Let buttons close window
  !I prefer Buttons to not close the preview, I want to test ALT+Key presses to check for conflicts
  CASE F{PROP:Type} 
  OF Create:Button
     CASE Message('<0A0h>{80}|FEQ Numer: <9>'& F &'|FEQ Name: <9>'& ClaFeqName(F) &|
         '|Button Text: <9>'& F{PROP:Text} &'|Button Icon: <9>'& F{PROP:Icon} & '|Button KEY: <9>'& F{PROP:Key} &|
         '|KeyCode(): <9>' & KeyCode(),'Button Pressed',,'&Ok|E&xit Preview')
     OF 1 ; RetLvl = Level:Notify
     OF 2 ; RetLvl = Level:Fatal  ; POST(Event:CloseWindow)
     END 
  OF CREATE:item
     CASE Message('FEQ Numer: <9>'& F&'|FEQ Name: <9>'& ClaFeqName(F) &'|Item Text: <9>'& F{PROP:Text} &|
         '|Item Icon: <9>'& F{PROP:Icon} &'|KeyCode(): <9>'& KeyCode(),'Menu ITEM Selected',,'&Ok|E&xit Preview')
     OF 1 ; RetLvl = Level:Notify
     OF 2 ; RetLvl = Level:Fatal ; POST(Event:CloseWindow)
     END 
  END     
  RETURN RetLvl
!-----------------------------------
CBWndPreviewClass.SideDoorCall PROCEDURE(LONG CallDoorNo, LONG FEQ, LONG FeqTypeNo, STRING FeqTypeName, STRING FeqName)
SysMenuCls SysMenuClass
Save_GHide LIKE(GloT:Hide)
  CODE  !03/28/21 For ListFormatParser to call ListPROPs directly takes setup of SysMenu. Code from .Reflection() setup
  SYSTEM{7A58h}=1 ; SYSTEM{7A7Dh}=MSGMODE:CANCOPY !PROP:PropVScroll PROP:MsgModeDefault
  PWnd &= SELF.WndRef
  IF ~ConfigGrp_DidGet THEN SELF.ConfigGetAll(). 
  Save_GHide=GloT:Hide ; GloT:Hide=1
  FREE(SysMenuClsQ) ; CLEAR(SysMenuClsQ) ; SysMnQ:WinRef &= PWnd ; SysMnQ:hWindow=PWnd{PROP:Handle} ; SysMnQ:ZOrder=1 ; ADD(SysMenuClsQ)
  CASE CallDoorNo
  OF 1 ; Self.ControlPROPs (FEQ, FeqTypeNo, FeqTypeName, FeqName)
  OF 2 ; Self.ResizeControl(FEQ, FeqTypeNo, FeqTypeName, FeqName)
  OF 3 ; Self.ListPROPs    (FEQ, FeqTypeNo, FeqTypeName, FeqName) !ListFormatParser
  OF 4 ; Self.ListReFORMAT (FEQ, FeqTypeNo, FeqTypeName, FeqName) 
  OF 5 ; Self.WindowPROPs()
  OF 6 ; Self.ResizeWindow()
  OF 7 ; Self.SystemPROPs()    
  END
  GloT:Hide=Save_GHide
  RETURN 
!----------------------------------------------------------- 
CBWndPreviewClass.BoxIt PROCEDURE(LONG FEQ=0)
P LONG,DIM(4),AUTO
F LONG,AUTO
  CODE
  IF ~FEQ AND ~SELF.BoxItFEQ THEN RETURN.
  SETTARGET(PWnd)
  IF SELF.BoxItFEQ THEN DESTROY(SELF.BoxItFEQ) ; SELF.BoxItFEQ=0.
  IF FEQ THEN 
    IF ~FEQ{PROP:Visible} THEN DO VisibleRtn.
    GETPOSITION(FEQ,P[1],P[2],P[3],P[4])
    F=CREATE(0,Create:Box) ; SELF.BoxItFEQ=F 
    SETPOSITION(F,P[1]-2,P[2]-2,P[3]+4,P[4]+4) 
    F{PROP:Fill}=COLOR:Yellow ; F{PROP:Color}=COLOR:Red ; UNHIDE(F)
  END
  SETTARGET() ; RETURN
VisibleRtn ROUTINE!Recurse Parents and Unhide, select Tab on Sheet
    F=FEQ ; UNHIDE(F)
    LOOP  ; F=F{PROP:Parent} ; IF F=0 THEN BREAK.
      UNHIDE(F)
      IF F{PROP:Type}<>CREATE:Tab OR F{PROP:Visible} THEN CYCLE.
      (F{PROP:Parent}){PROP:Selected}=F{PROP:ChildIndex} !Sheet Select TAB
    END    
!-----------------------------------------------------------
! Parent control can Hide or Disable Children. See PROP:Enabled Prop:Visible. Show different Icon?
!   Maybe Checkbox to [ ] Show only Hide/Disable/ReadOnly that also shows parent PROP:Enabled Prop:Visible
!================================================== WindowReflection ==========================
CBWndPreviewClass.Start PROCEDURE() ! For Bruce XOXO
 CODE ; SELF.Reflection()
CBWndPreviewClass.Reflection PROCEDURE() 
FieldQ  QUEUE,PRE(FldQ) 
!PGroup    GROUP,PRE()
PHide     STRING(1)   !FldQ:PHide       1
PDisable  STRING(1)   !FldQ:PDisable    2
PReadOnly STRING(1)   !FldQ:PReadOnly   3
PSkip     STRING(1)   !FldQ:PSkip       4
FolSEQ    LONG        !FldQ:FolFEQ      5
FeqNo     LONG        !FldQ:FeqNo       6
Type      STRING(16)  !FldQ:Type        7
FeqName   CSTRING(64) !FldQ:FeqName     8
Text      STRING(256) !FldQ:Text        9
TabName   CSTRING(32) !FldQ:TabName     10
AtX       LONG        !FldQ:AtX         11
AtY       LONG        !FldQ:AtY         12
AtW       STRING(5)   !FldQ:AtW         13
AtH       STRING(5)   !FldQ:AtH         14
Align     STRING(8)   !FldQ:Align       15
SeeMore   STRING(256) !FldQ:SeeMore     16  !see below SeeMore:ColNum     
TypeNo    BYTE        !FldQ:TypeNo  BAND(,FFh)
Type16    USHORT      !FldQ:Type16
QMark     PSTRING(2)  !FldQ:QMark       
AtWGet    LONG        !FldQ:AtWGet       GetPos W, if NoWidth this = Default Full Width
AtHGet    LONG        !FldQ:AtHGet
Parent    LONG        !FldQ:Parent
TabFEQ    LONG        !FldQ:TabFEQ
SheetFEQ  LONG        !FldQ:SheetFEQ
Precedes  LONG        !FldQ:Precedes
Follows   LONG        !FldQ:Follows

        END
SeeMore:ColNum EQUATE(16)
Check0n EQUATE('<163>')   !"n"=Now Checked Wingdings 2  !These sort ascending to put checked first
Check1n EQUATE('<84>')            
Check0w EQUATE('<153>')   !"w"=was but you unchk
Check1w EQUATE('<86>')    !"w"=Was checked originally                    
FldQ:HDRSchk STRING(1),DIM(4),OVER(FieldQ)   ![1]...[4] Hide Disable ReadOnly Skip
Prop:HDRS    LONG,DIM(4),AUTO                !Values Prop:Hide :Disable :ReadOnly :Skip
Count:HDRS   SHORT,DIM(4)
SheetCnt BYTE     
TabQ QUEUE,PRE(TabQ)
ShFEQ LONG        !TabQ:ShFEQ
TbFEQ LONG        !TabQ:TbFEQ
TbNum BYTE        !TabQ:TbNum
PopNo BYTE        !TabQ:PopNo
Hide BYTE         !TabQ:Hide
Text  CSTRING(32) !TabQ:Text
FName CSTRING(32) !TabQ:FName
Name  CSTRING(32) !TabQ:Name
     END    
F LONG,AUTO
R LONG,AUTO
P LONG,AUTO
A ANY
SrtOne LONG(5),THREAD
SrtTwo LONG(5),THREAD
SrtTr3 LONG,THREAD
SrtOneWho PSTRING(16)
SrtTwoWho PSTRING(16)
SrtTr3Who PSTRING(16)
FieldQ_LastFEQNo LONG,THREAD  !Last Selection overrides...
Format_ListF ANY,THREAD
AnyPropH STRING('7C00h {25}'),THREAD
ReOpenSELECT  BYTE(1),THREAD    !Close and Reopen this Wnd so can Select(Ctrl)
ReOpenHOW     LONG              !Am I doing the ReOpen now  1=CtrlProps 2=List 3=Resize
ReOpenFEQNo   LONG              !Field being reopen
DevTpNoLIST BYTE
DevTipEIP LONG
AllColsHide USHORT,THREAD
FindTxt STRING(64) 
ConsolasFQ BYTE,THREAD          
!    BUTTON('G'),AT(14,2,10,10),USE(?SysMenuTip),SKIP,FONT('Wingdings',12),TIP('TIP: There are 8 special items on System Menu. This is on EVERY window.'),flat
!    BUTTON('<235>'),AT(2,2,10,10),USE(?UnderBtn),SKIP,FONT('Wingdings'),TIP('Move Preview under this Window')
!    BUTTON('<74>'),AT(14,2,10,10),USE(?SysMenuTip),flat,SKIP,FONT('Wingdings 2',13),TIP('TIP: There are 8 special items on System Menu. This is on EVERY window.')
Window WINDOW('WindowReflection'),AT(,,600,220),GRAY,SYSTEM,MAX,ICON(ICON:JumpPage),FONT('Segoe UI',9), |
            RESIZE
        BUTTON('<50>'),AT(2,2,10,10),USE(?UnderBtn),SKIP,FONT('Webdings'),TIP('Move Preview under th' & |
                'is Window'),FLAT
        BUTTON('<45>'),AT(14,2,10,10),USE(?SysMenuTip),SKIP,FONT('Wingdings 3',13),TIP('TIP: There a' & |
                're 8 special items on System Menu. This is on EVERY window.'),FLAT
        BUTTON('?'),AT(32,2,12,11),USE(?HelpBtn),KEY(F2Key),SKIP,FONT(,,,FONT:bold),TIP('Clarion Hel' & |
                'p on selected Control - F2'),FLAT
        BUTTON('<37h>'),AT(63,2,13,10),USE(?AltKeyAnalBtn),SKIP,FONT('Wingdings',12),TIP('ALT+Key An' & |
                'alysis<13,10>Show controls that can or have Alt+Key or KEY().<13,10>After can use R' & |
                't Mouse Delete TAB Controls'),FLAT 
        CHECK('<0a8h>'),AT(79,3,9,8),USE(Cfg:BoxItShows),SKIP,TRN,FLAT,FONT('Wingdings 2',,COLOR:Maroon),ICON(ICON:None), |
                TIP('Box selected Control in Red to find it on Window')
        BUTTON('8'),AT(225,14,13,10),USE(?RightTip),SKIP,FONT('Wingdings',12),TIP('TIP: Right-Click ' & |
                'on List for Popup of Options'),FLAT
        BUTTON('T'),AT(225,2,13,10),USE(?SettingsBtn),SKIP,FONT('Wingdings',12),TIP('Settings...'),FLAT
        BUTTON('&ALL...'),AT(2,13,26,12),USE(?AllUncheckBtn),SKIP,TIP('Selectively Uncheck All, or C' & |
                'trl+Click to Uncheck Everything')
        BUTTON('Cl&ose'),AT(32,13,26,12),USE(?CancelBtn),SKIP,STD(STD:Close)
        BUTTON('&Tricks'),AT(63,13,25,12),USE(?TricksBtn),SKIP,TIP('Tricks Menu')
        BUTTON('S&ystem'),AT(320,13,29,12),USE(?SystemBtn),SKIP,TIP('System PROP:<13,10>PRINTER PROP' & |
                ':<13,10>System Metrics')
        BUTTON('&Win Prop:'),AT(242,13,37,12),USE(?WndPropBtn),SKIP,TIP('Window PROPs')
        BUTTON('W R&esize'),AT(283,13,34,12),USE(?WndResizeBtn),SKIP,TIP('Resize Window')
        BUTTON('&Con Prop:'),AT(99,13,40,12),USE(?ConPropBtn),SKIP,TIP('Control PROP: List<13,10>Ctr' & |
                'l + or Alt+ Double Click on List<13,10>or Double Click on List ("2M Resize" unchecked) ')
        BUTTON('&Resize'),AT(142,13,30,12),USE(?ConResizeBtn),SKIP,TIP('Resize Control and Change Pr' & |
                'operties - WYSIWYG Designer <13,10>Shift + Double Click on List<13,10>or Double Cli' & |
                'ck on List ("2M Resize" checked) ')
        BUTTON('&LIST'),AT(175,13,22,12),USE(?LISTsBtn),SKIP,TIP('LIST FORMAT() & PropList')
        BUTTON('&Find'),AT(357,2,22,10),USE(?FindNext),SKIP,FONT(,8)
        BUTTON('Pre&v'),AT(421,2,22,10),USE(?FindPrev),SKIP,FONT(,8)
        ENTRY(@s64),AT(357,14,85,11),USE(FindTxt),SKIP,FONT('Consolas',9),TIP('Text to Locate, (Ctrl' & |
                ' F) to Select.<13><10>Prefix with % for RegEx MATCH'),KEY(CtrlF),ALRT(EnterKey)
        BUTTON('Ta&b'),AT(201,13,20,12),USE(?TabPickBtn),SKIP,TIP('Change Tab')
        BUTTON('ALRTs'),AT(567,14,29,11),USE(?ALRTsBtn),DISABLE,SKIP,HIDE,TIP('See all ALRT() and KEY()'), |
                FLAT
        BUTTON('See &More...'),AT(447,13,42,12),USE(?SeeMoreButs),SKIP,TIP('Select the See More Colu' & |
                'mn Data<13,10>Ctrl+Click on Popup Item to Add To (Prepend)'),LEFT
        ENTRY(@s30),AT(492,15,39,9),USE(AnyPropH),SKIP,TIP('Type any PROP in Hex "h", Decimal or Expression <13,10>E.g. ' & |
                '7C00h is PROP:Text - Defined Range 7200-7DFF<13,10><13,10>For {{''UserProp''} enter a leading <39> quote'), |
                ALRT(EnterKey)
        BUTTON('...'),AT(533,13,13,12),USE(?SeeMorePickBtn),SKIP,TIP('Property Pick List')
        BUTTON('&Halt'),AT(557,13,22,12),USE(?HaltBtn),SKIP
        BUTTON,AT(586,13,12,12),USE(?CopyBtn),SKIP,ICON(ICON:Copy),TIP('Copy Field Q'),FLAT
        CHECK('Hide'),AT(175,2,26),USE(GloT:Hide),SKIP,TRN,FONT(,8),TIP('Hide this Window when open o' & |
                'ther window')
        CHECK('Select'),AT(99,2,30),USE(ReOpenSELECT),SKIP,TRN,FONT(,8),TIP('SELECT Control on Previ' & |
                'ew window before open Control Properties<13,10>Requires Close and ReOpen Window.')
        CHECK('2mRsz'),AT(141,2,30),USE(Cfg:ResizeOnMouse2),SKIP,TRN,FONT(,8),TIP('Check for Control' & |
                ' Resize on Double Click<13><10>Uncheck to show Control PROPs<13,10>Context menu off' & |
                'ers either')
        CHECK('Consola&s'),AT(553,2,43),USE(ConsolasFQ),SKIP,LEFT,FONT(,8),TIP('Field LIST in Consol' & |
                'as font')
        CHECK('Menus'),AT(317,2),USE(?MenuItems),SKIP,DISABLE,HIDE,FONT(,8),TIP('TODO - MENU ITEM')
        LIST,AT(1,28),FULL,USE(?ListF),HVSCROLL,VCR,FROM(FieldQ),FORMAT('[12C|F~H~@s1@Z(1)12C|F~D~@s' & |
                '1@Z(1)12C|F~RO~@s1@Z(1)13L(2)F~Skip~L(0)@s1@Z(1)]|M~Hide Disable...~16R(2)|FM~Flw' & |
                '<13,10>Seq~C(0)@n3@22R(2)|FM~FEQ<13,10>No.~C(0)@n-_6@36L(2)|FM~Control<13><10> Type' & |
                '~C(0)@s16@76L(2)|M~FEQ Name~C(0)@s63@?112L(2)|M~Text / Description~@s255@40L(2)|M~T' & |
                'ab Name~@s31@[16R(2)|M~X~C(0)@n-7@16R(2)|M~Y~C(0)@n-7@16R(2)|M~Wd~C(0)@s5@16R(2)|M~' & |
                'Ht~C(0)@s8@18L(2)|M~Aln~C(0)@s8@]|~Position~120L(2)|M~See More....~@s255@'), |
                ALRT(EnterKey), ALRT(MouseRight2), ALRT(DeleteKey), ALRT(CtrlMouseLeft2), |
                 ALRT(ShiftMouseLeft2), ALRT(AltMouseLeft2), ALRT(CtrlEnter), ALRT(ShiftEnter), |
                 ALRT(AltEnter), ALRT(CtrlC)
    END
SysMenuCls SysMenuClass
AtNoSetXY   BYTE(1)
EVENT       ITEMIZE(Event:User),PRE
ControlPROPs  EQUATE()
ListPROPs     EQUATE()
ResizeControl EQUATE()
DoMouseRight  EQUATE()
            END
MouseRightSent SHORT
SortCls CBSortClass
!------------------
  CODE
  SYSTEM{7A58h}=1 ; SYSTEM{7A7Dh}=MSGMODE:CANCOPY !PROP:PropVScroll PROP:MsgModeDefault
  PWnd &= SELF.WndRef
  SELF.EvtLogWrite('** WndPreview Reflection Enter **')
  IF ~ConfigGrp_DidGet THEN SELF.ConfigGetAll().    
  SETTARGET(SELF.WndRef) ;  DO LoadFieldQRtn ; SETTARGET()   !Must SETTARGET so reports work
  IF SELF.SelectedLast THEN FieldQ_LastFEQNo=SELF.SelectedLast.
ReOpenLOOP:Label:    
  FREE(SysMenuClsQ) ; CLEAR(SysMenuClsQ) ; SysMnQ:WinRef &= PWnd ; SysMnQ:hWindow=PWnd{PROP:Handle} ; SysMnQ:ZOrder=1 ; ADD(SysMenuClsQ)
  FldQ:FeqName = 0{PROP:Text}     
  OPEN(Window) ; SysMenuCls.Init(Window) ; SELF.AtSetOrSave(1, AtWndReflect[], AtNoSetXY) ; AtNoSetXY=0 
  ?MenuItems{PROP:Use}=SELF.MenuItemShows
  0{PROP:Text} = 'CB wInspect - Controls: ' & CLIP(FldQ:FeqName) &' '& Glo:Built &' - '& VersionWndPrv
  IF PWnd{'Proc_Name'} THEN 0{PROP:Text}=PWnd{'Proc_Name'} &' - '& 0{PROP:Text}. !Tpl Procedure Name
  IF Format_ListF THEN ?ListF{PROP:Format}=Format_ListF.
  LineHt(?ListF)
  ?ListF{PROPSTYLE:FontName,1}='Wingdings 2'
  ?ListF{PROPSTYLE:FontSize,1}=12
  ?ListF{PROPSTYLE:TextSelected,1} = COLOR:WindowText  !COLOR:Black   don't let selecting inverse the Wingding character
  ?ListF{PROPSTYLE:BackSelected,1} = COLOR:Window      !COLOR:White   or it can appear opposite desired
  MakeOverList(?ListF) 
  FldQ:FeqNo=FieldQ_LastFEQNo ; GET(FieldQ,FldQ:FeqNo) ; ?ListF{PROP:Selected}=POINTER(FieldQ)
  IF ConsolasFQ THEN POST(EVENT:Accepted,?ConsolasFQ).
  
  DO AcceptLoopRtn ! ACCEPT .. CASE EVENT()  CASE ACCEPTED() END 
  SELF.BoxIt()
  SELF.AtSetOrSave(2, AtWndReflect[])
  GET(FieldQ,CHOICE(?ListF)) ; FieldQ_LastFEQNo=FldQ:FeqNo
  Format_ListF=?ListF{PROP:Format} 
  CLOSE(Window)   
!TODO ??? can I just make sure Parent Tab is Visible i.e. Sheet has it selected and ~Hide    
  IF ReOpenSELECT AND ReOpenHOW AND ReOpenFEQNo THEN  !Cannot seem to Select control on Under Window Preview witout closing
     F=FldQ:FeqNo
     LOOP While F !Walk up the Parents and Unhide[1]/Enable[2]
        FldQ:FeqNo=F ; GET(FieldQ,FldQ:FeqNo) 
        IF FldQ:HDRSchk[1]=Check1n  THEN FldQ:HDRSchk[1]=Check0n ; Count:HDRS[1]-=1.
        IF FldQ:HDRSchk[1]=Check1w  THEN FldQ:HDRSchk[1]=Check0w ; Count:HDRS[1]-=1.
        IF FldQ:HDRSchk[2]=Check1n  THEN FldQ:HDRSchk[2]=Check0w ; Count:HDRS[2]-=1.
        IF FldQ:HDRSchk[2]=Check1w  THEN FldQ:HDRSchk[2]=Check0n ; Count:HDRS[2]-=1.
        PUT(FieldQ)
        UNHIDE(F) ; ENABLE(F)
        F=F{PROP:Parent} 
     END
     FldQ:FeqNo=ReOpenFEQNo ; GET(FieldQ,FldQ:FeqNo) ; SELECT(FldQ:FeqNo) !Select Control on Preview
     DISPLAY ; AtNoSetXY=0 ; GOTO ReOpenLOOP:Label:
  END
  SELF.EvtLogWrite('** WndPreview Reflection Exit **')
  RETURN
AcceptLoopRtn ROUTINE !--------------------
  ACCEPT
    CASE EVENT()
    OF EVENT:OpenWindow
        DO SortListOpenWindowRtn
        IF ReOpenHOW THEN
           FldQ:FeqNo=ReOpenFEQNo ; GET(FieldQ,FldQ:FeqNo) ; ?ListF{PROP:Selected}=POINTER(FieldQ) ; POST(ReOpenHOW)
        END
    OF   Event:ControlPROPs
    OROF EVENT:ListPROPs
    OROF EVENT:ResizeControl            
         IF ReOpenSELECT AND ~PWnd$FldQ:FeqNo{PROP:Visible} AND ReOpenFEQNo<>FldQ:FeqNo THEN
            ReOpenHOW=EVENT()
            ReOpenFEQNo=FldQ:FeqNo 
            BREAK !Break out of ACCEPT to close this window, select Prv Window control, then come back here
         END
         ReOpenFEQNo=0 ; ReOpenHOW=0 ; IF GloT:Hide THEN HIDE(0).
         CASE EVENT()
         OF Event:ControlPROPs  ; SELF.ControlPROPs(FldQ:FeqNo, FldQ:TypeNo, FldQ:Type, FldQ:QMark & FldQ:FeqName) ; DO SyncFldQ:HDRSRtn
         OF EVENT:ListPROPs     ; SELF.ListPROPs(FldQ:FeqNo, FldQ:TypeNo, FldQ:Type, FldQ:QMark & FldQ:FeqName) ; UNHIDE(0); DO SyncFldQ:HDRSRtn ; CYCLE
         OF EVENT:ResizeControl ; SELF.ResizeControl(FldQ:FeqNo, FldQ:TypeNo, FldQ:Type, FldQ:QMark & FldQ:FeqName) ; UNHIDE(0); DO SyncFldQ:HDRSRtn ; CYCLE       
         END
         UNHIDE(0) ; CYCLE 
    END
    CASE ACCEPTED() 
    OF ?AllUncheckBtn ; DO AllUncheckRtn
    OF ?AltKeyAnalBtn ; DO Trick_AltKeyAnalRtn
    OF ?Cfg:ResizeOnMouse2 ; SELF.ConfigPut(Cfg:ResizeOnMouse2)
    OF ?ConPropBtn ; POST(Event:ControlPROPs) 
    OF ?ConResizeBtn ; POST(EVENT:ResizeControl) 
    OF ?ConsolasFQ ; IF ConsolasFQ THEN SETFONT(?ListF,'Consolas',9) ELSE SETFONT(?ListF,'Segoe UI',9). ; DISPLAY
    OF ?CopyBtn ; F=5 ; R=14 ; P=POPUPunder(?,'Visible List|Debug Queue|TabQ|-|FieldQ VLB View') ; IF P=2 THEN F=1 ; R=99.
                  IF P=4 THEN QueueViewListVLB(FieldQ,'FieldQ') ; CYCLE.
                  IF P<3 THEN SetClip2Queue(FieldQ,1,,'FldQ:',F,R) ELSE SetClip2Queue(TabQ).
    OF ?HaltBtn ; HALT
    OF ?HelpBtn ; GET(FieldQ,CHOICE(?ListF)) ; HelpCW(UPPER(ClaControlTypeName(FldQ:TypeNo)))
    OF ?LISTsBtn ; DO LISTsBtnRtn
    OF ?RightTip   ; DO MouseRightMenuRtn ; MouseRightSent=0  
    OF ?SeeMoreButs ; DO SeeMoreButsRtn
    OF ?SeeMorePickBtn ; IF SELF.PropPickList(AnyPropH) THEN DO SeeMoreButsRtn.
    OF ?SettingsBtn ; DO SettingsRtn
    OF ?SysMenuTip ; MESSAGE(?{PROP:Tip},'FYI') ; PressKey(AltSpace)
    OF ?SystemBtn  ; SELF.SystemPROPs()
    OF ?TabPickBtn ; DO TabPickRtn
    OF ?TricksBtn ; DO TricksBtnRtn 
    OF ?UnderBtn   ; SysMenuCls_SYSCOMMAND(0{PROP:Handle},SMCmd_MoveUnder)
    OF ?WndPropBtn ; SELF.WindowPROPs()
    OF ?WndResizeBtn ; SELF.ResizeWindow() 
    OF ?Cfg:BoxItShows ; IF ~Cfg:BoxItShows THEN SELF.BoxIt(0) ELSE POST(EVENT:NewSelection,?ListF). ; SELF.ConfigPut(Cfg:BoxItShows)
   END
    CASE FIELD()
    OF ?ListF
       GET(FieldQ,CHOICE(?ListF))
       CASE EVENT()
       OF EVENT:AlertKey
          CASE KEYCODE()
          OF MouseRight2 ; SetKeycode(0) ; MouseRightSent=0 ; POST(EVENT:ResizeControl) ; CYCLE
          OF EnterKey OROF CtrlEnter OROF ShiftEnter OROF AltEnter OROF CtrlMouseLeft2 OROF ShiftMouseLeft2 OROF AltMouseLeft2 ; POST(EVENT:NewSelection,?ListF ) 
          OF AppsKey  ; DO MouseRightMenuRtn ; MouseRightSent=0 !IF ~MouseRightSent THEN POST(EVENT:NewSelection,?ListF).
          OF DeleteKey; DELETE(FieldQ) ; DISPLAY  
          OF CtrlC ; SETCLIPBOARD(FldQ:FeqName)
          END
       OF EVENT:DoMouseRight ; IF MouseRightSent THEN DO MouseRightMenuRtn ; MouseRightSent=0.
       OF EVENT:NewSelection                !Check Uncheck boxes
          IF Cfg:BoxItShows THEN SELF.BoxIt(FldQ:FeqNo).
          CASE KEYCODE()              
          OF MouseRight ;  MouseRightSent+=1 ; IF MouseRightSent=1 THEN POST(EVENT:DoMouseRight,?ListF).
          OF MouseLeft2 OROF CtrlMouseLeft2 OROF ShiftMouseLeft2 OROF AltMouseLeft2 
          OROF EnterKey OROF CtrlEnter OROF ShiftEnter OROF AltEnter ; SetKeycode(0)
             IF KeyStateSCA(1) THEN POST(EVENT:ResizeControl) ; CYCLE.
             IF KeyStateSCA(2+4) THEN POST(Event:ControlPROPs)  ; CYCLE.
             POST(CHOOSE(~Cfg:ResizeOnMouse2,Event:ControlPROPs,EVENT:ResizeControl) ) ; CYCLE             
          END
          P=?ListF{PROPLIST:MouseDownField}
          IF P>=1 AND P<= 4 AND ?ListF{PROPLIST:MouseDownZone} = LISTZONE:field THEN
             R=INLIST(FldQ:HDRSchk[P],Check0w,Check1n,Check1w)+1 !1=Check0n
             FldQ:HDRSchk[P]=CHOOSE(R,Check1n,Check1w,Check0n,Check0w)
             PUT(FieldQ) ; Count:HDRS[P]+=CHOOSE(R<3,1,-1) ; PWnd$FldQ:FeqNo{Prop:HDRS[P]}=CHOOSE(R<3)
          END
       OF EVENT:HeaderPressed ; DO SortListHeaderPressedRtn
       END          
    OF ?FindTxt OROF ?FindNext OROF ?FindPrev ; LocateInList(FieldQ, ?ListF,?FindTxt,?FindNext,?FindPrev)
    OF ?AnyPropH ; IF EVENT()=EVENT:AlertKey AND KEYCODE()=EnterKey THEN UPDATE ; POST(EVENT:Accepted,?SeeMoreButs).
    END
  END
  EXIT
!----------------
SettingsRtn ROUTINE
  EXECUTE POPUPunder(?, |
     PopItem('Convert FEQ Names to Up:Low text',CFG:FEQUpLow,,0) & | 
     PopItem('Make Over WINDOWs (next open)',CFG:MakeOvrWnd,,2) & | 
     PopItem('Make Over LISTs',CFG:MakeOvrList,,2) & | 
     PopItem('Button press on Preview shows Message()',SELF.ButtonShowsMsg,,2) ) 
   BEGIN ; CFG:FEQUpLow=1-CFG:FEQUpLow ; SELF.ConfigPut(CFG:FEQUpLow) ; END
   BEGIN ; CFG:MakeOvrWnd=1-CFG:MakeOvrWnd ; SELF.ConfigPut(CFG:MakeOvrWnd) ; END
   BEGIN ; CFG:MakeOvrList=1-CFG:MakeOvrList ; SELF.ConfigPut(CFG:MakeOvrList) ; MakeOverList(?ListF,1) ; END  
   BEGIN ; SELF.ButtonShowsMsg=CHOOSE(~SELF.ButtonShowsMsg) ; END  
  END      
SortListOpenWindowRtn ROUTINE 
    ?ListF{PROPLIST:HdrSortTextColor} = COLOR:CaptionText ; ?ListF{PROPLIST:HdrSortBackColor} = 8000001Bh  !COLOR:GradientActiveCaption
    ?ListF{PROPLIST:HasSortColumn}=1 
    ?ListF{PROPLIST:SortColumn}=ABS(SrtOne)
    ?ListF{PROPList:MouseDownField}=ABS(SrtOne)
    SrtOne *= -1 ; DO SortListHeaderPressedRtn
SortListHeaderPressedRtn ROUTINE
    GET(FieldQ, CHOICE(?ListF))
    F=?ListF{PROPList:MouseDownField} ; F=?ListF{PROPLIST:FieldNo,F} !Column and NOT Q Field, use F={PROPLIST:FieldNo,F}
    IF F=ABS(SrtOne) THEN SrtOne *= -1 ELSE SrtTr3=SrtTwo ; SrtTwo=SrtOne ; SrtOne=F.  
    SrtOneWho=CHOOSE(SrtOne<0,'-','+') & WHO(FieldQ,ABS(SrtOne))
    IF ~SrtTwo OR SrtTwo=SrtOne THEN SrtTwoWho='' ELSE SrtTwoWho=CHOOSE(SrtTwo<0,',-',',+') & WHO(FieldQ,ABS(SrtTwo)).
    IF ~SrtTr3 OR SrtTr3=SrtTwo THEN SrtTr3Who='' ELSE SrtTr3Who=CHOOSE(SrtTr3<0,',-',',+') & WHO(FieldQ,ABS(SrtTr3)).
    SORT(FieldQ,SrtOneWho & SrtTwoWho & SrtTr3Who &',+FldQ:FeqNo') 
    GET(FieldQ, FldQ:FeqNo)
    P=CHOOSE(F<=4,1,POINTER(FieldQ) ) ; ?ListF{PROP:Selected}=P  !For chkbx 1-4 select 1st row
    ?ListF{PROPLIST:Locator,CHOOSE(F<= 5,7,F)}=1   !Step Locator in sort column or FEQ Name
    A=0{PROP:Text} ; R=INSTRING(' Sort(',A,1) ; IF R THEN A=SUB(A,1,R-1).
    IF NOT(SrtOne=5 AND SrtTwo=5) THEN 0{PROP:Text}=A & ReplaceText(' Sort(' & SrtOneWho&SrtTwoWho&SrtTr3Who &')','FLDQ:','').
    DISPLAY
!----------------
MouseRightMenuRtn ROUTINE
    DATA
TypNo LONG
TypNm PSTRING(16)    
TabNm PSTRING(24)
NoTab PSTRING('~')
TabFEQ LONG   
ShtFEQ LONG   
    CODE
    R=0 ; TypNo=FldQ:TypeNo ; TypNm=UPPER(ClaControlTypeName(TypNo)) 
    TabFEQ=FldQ:TabFEQ ; ShtFEQ=FldQ:SheetFEQ
    IF TabFEQ THEN NoTab='' ; TabNm=PopClean(CHOOSE(TabFEQ<>FldQ:FeqNo, FldQ:TabName,CLIP(FldQ:text) &' ?'&FldQ:FeqName)).
    SETKEYCODE(0)
    EXECUTE POPUP('Copy ' & CLIP(FldQ:Type) &'  '& FldQ:FeqName & |
                '{{Copy FEQ Name<9>Ctrl+C|Copy Text|Copy Text Quoted <<Hex>|Copy See More|Copy All}|-' & |
                '|PROP: List Control Properties<9>' & CHOOSE(~Cfg:ResizeOnMouse2,'Mouse 2','Ctrl+Mouse 2') & |
                '|Resize Control / WYSIWYG Designer<9>' & CHOOSE(~Cfg:ResizeOnMouse2,'Shift+Mouse 2','Mouse 2') & |
            CHOOSE(~TypeIsList(FldQ:TypeNo),'|~','|') & 'LIST FORMAT() && PropList Properties' & |
              '|-|Delete Control (in list)<9>Delete' & |
                '|Delete All "' & CLIP(TypNm)&'" Controls' & |
                '|Delete All Except "' & CLIP(TypNm)&'" Controls' & |
              '|-|' & NoTab & 'Delete Controls ON Tab: ' & TabNm & |
                '|' & NoTab & 'Delete Controls NOT on Tab: ' & TabNm & |
                '|-|Clarion Help on ' & TypNm )
                !&'|-|Dbg MouseRightSent=' & MouseRightSent &' Key=' & KeyCode() &' Event=' &Event() ) 
      SETCLIPBOARD(FldQ:FeqName)
      SETCLIPBOARD(PropText1(FldQ:FeqNo,FldQ:TypeNo,FldQ:Text,0)) !SETCLIPBOARD(CHOOSE(~PWnd$FldQ:FeqNo{PROP:Text},FldQ:Text,PWnd$FldQ:FeqNo{PROP:Text})) !Copy text
      SETCLIPBOARD(PropText1(FldQ:FeqNo,FldQ:TypeNo,FldQ:Text,1))
      SETCLIPBOARD(FldQ:SeeMore)
      SETCLIPBOARD('Feq#'&FldQ:FeqNo &' '& FldQ:Type &'<13,10>'& FldQ:FeqName &'<13,10>'& CLIP(FldQ:Text) &'<13,10>'& CLIP(FldQ:SeeMore))
      POST(EVENT:ControlPROPs)
      POST(EVENT:ResizeControl) 
      POST(EVENT:ListPROPs)       
      BEGIN ; SETKEYCODE(DeleteKey) ; POST(EVENT:AlertKey,?ListF) ; END
      R=1
      R=-1 
      R=2  !On Tab
      R=-2 !NOT Tab
      HelpCW(TypNm)                
    END
    IF ~R THEN EXIT.
    LOOP F=RECORDS(FieldQ) TO 1 BY -1
        GET(FieldQ,F)
        CASE R
        OF 1 OROF -1 ; IF R<>CHOOSE(TypNo=FldQ:TypeNo,1,-1) THEN CYCLE.
        OF 2  ; IF FldQ:TabFEQ <> TabFEQ OR FldQ:FeqNo=TabFEQ THEN CYCLE.  !Delete ON tab
        OF -2 ; IF FldQ:TabFEQ=TabFEQ OR FldQ:TabFEQ=0 OR FldQ:Parent=ShtFEQ THEN CYCLE.  !Delete NOT on tab
        ELSE ; CYCLE
        END 
        DELETE(FieldQ) 
    END ; DISPLAY   
!----------------
TricksBtnRtn ROUTINE 
  EXECUTE POPUPunder(?TricksBtn, |
     'Add Developer Tool Tips to Controls and LIST Columns' & |
     '|Add Developer Tool Tips to Controls Only' & | 
     '|Event() Logging - ' & CHOOSE(~SELF.EvtLog.Started,'Start','Stop') & |
    '|-|Fix ####.## Numbers' & |
    '|Remove Blank from Pictures  @nB  @dB  @tB' & | !TODO ALl Pictures @d @t
    '|Make all LIST Columns Resizable' & |
    '|-|ALT + Key Analysis' & |
    '|-|Delete PROMPT and STRING from below List of Controls' & |
    '|-|Edit Window Caption' & |
    '|-|Save Preview EXE so not lost on Close')
   DO Trick_DevTipsRtn
   BEGIN ; DevTpNoLIST=1 ; DO Trick_DevTipsRtn ; DevTpNoLIST=0 ; END 
   IF ~SELF.EvtLog.Started THEN SELF.EvtLogStart(1) ELSE SELF.EvtLogStop().
   BEGIN ; SETTARGET(PWnd) ; SELF.AllFixNumbers() ; SETTARGET() ; END
   DO Trick_NoBlankPictureRtn
   DO Trick_ListColsResizeRtn
   DO Trick_AltKeyAnalRtn
   DO Trick_PromptStringDeleteRtn
   DO Trick_EditCaptionRtn
   DO Trick_SaveExeRtn
  END
  DISPLAY
!----------------================================= DEV TIPS
Trick_DevTipsRtn ROUTINE
    DATA
TpGroup GROUP,PRE()    
DvTp        CSTRING(900)    
CMore       CSTRING(500)
Picture     CSTRING(28)  !Cleared every time
BtnStd      CSTRING(24)
Follows     CSTRING(100) 
Precedes    CSTRING(100) 
Parent_     CSTRING(100) 
AT_         CSTRING(192) 
       END
CRLFSp      STRING('<13,10,32>')       
  CODE
  SYSTEM{PROP:TipDelay}=6000
  SETTARGET(PWnd)
  LOOP R=1 TO RECORDS(FieldQ)
     GET(FieldQ,R) ; CLEAR(TpGroup)
     F=FldQ:FeqNo
     Picture=ClaPicture(f,FldQ:TypeNo) ; IF Picture THEN Picture=CRLFSp &'Picture: '&Picture.
     CMore=SeeMore(-1*PROP:Value,F,FieldQ:TypeNo)
     AT_ = 'AT(' & FldQ:AtX &','& FldQ:AtY &','& CLIP(FldQ:AtW) &','& CLIP(FldQ:AtH) &'),'& CLIP(FldQ:Align)&'  '& |
                CHOOSE(FldQ:AtW=FldQ:AtWGet,'',' W=' & FldQ:AtWGet) & CHOOSE(FldQ:AtH=FldQ:AtHGet,'',' H=' & FldQ:AtHGet) & |
                '  RightX,BottomY=' & FldQ:AtX+FldQ:AtWGet &',' & FldQ:AtY+FldQ:AtHGet & |
                '  +4=' & FldQ:AtX+FldQ:AtWGet+4 &',' & FldQ:AtY+FldQ:AtHGet+4
    IF TypeIsLIST(FldQ:TypeNo) THEN  |
        AT_=AT_ & CRLFSp & 'LIST Items=' & F{PROP:Items} &' LineHt=' & F{PROP:LineHeight} &' HdrHt=' & F{PROP:HeaderHeight} & |
                           ' LineCount=' & F{PROP:LineCount} & CRLFSp & 'Columns have Tips Too!'.
    IF FldQ:TypeNo=CREATE:Text THEN AT_=AT_ & '  LineHt=' & F{PROP:LineHeight} & ' LineCount=' & F{PROP:LineCount} .
    IF FldQ:Follows THEN
       FldQ:FeqNo=FldQ:Follows ; GET(FieldQ,FldQ:FeqNo) 
       Follows=CRLFSp &'Follows: ' & CLIP(FldQ:FeqName)&' - '&CLIP(FldQ:Type)
       GET(FieldQ,R) 
    END
    IF FldQ:Precedes THEN
       FldQ:FeqNo=FldQ:Precedes ; GET(FieldQ,FldQ:FeqNo)  
       Precedes=CRLFSp &'Precedes: ' & CLIP(FldQ:FeqName)&' - '&CLIP(FldQ:Type)
       GET(FieldQ,R) 
    END
    IF FldQ:Parent THEN
       FldQ:FeqNo=FldQ:Parent ; GET(FieldQ,FldQ:FeqNo)  
       Parent_=CRLFSp &'Parent: ' & CLIP(FldQ:FeqName)&' - '&CLIP(FldQ:Type)
       GET(FieldQ,R) 
    END
     DvTp=' '& FldQ:FeqName & |
        CRLFSp& CLIP(FldQ:Type) &'  FEQ: ' & F &|
        Picture &|
        CHOOSE(~CMore,'',CRLFSp&CMore) &|
        BtnStd &|
        CRLFSp & AT_ & |
        CRLFSp & Follows & Precedes & Parent_
!            CRLFSp &'AT(' & FldQ:AtX &','& FldQ:AtY &','& CLIP(FldQ:AtW) &','& CLIP(FldQ:AtH) &'),'& FldQ:Align & |
!                    CHOOSE(FldQ:AtW=FldQ:AtWGet,'',' W=' & FldQ:AtWGet) & CHOOSE(FldQ:AtH=FldQ:AtHGet,'',' H=' & FldQ:AtHGet) & |
       
     F{PROP:Tip}=DvTp 
     FldQ:SeeMore=SUB(DvTp,2,256)
     PUT(FieldQ)
     IF ~DevTpNoLIST AND TypeIsLIST(FldQ:TypeNo) THEN DO Trick_DevTips_LIST_Rtn. 
  END
  IF DevTipEIP THEN DESTROY(DevTipEIP) ; DevTipEIP=0.
  SETTARGET()
  ?ListF{PROPLIST:Header,SeeMore:ColNum}='Control Developer Tool Tips'
  DISPLAY
Trick_DevTips_LIST_Rtn ROUTINE
    DATA
ColX        USHORT,AUTO
InX         LONG,AUTO
GrpNo       SHORT
LastGrpNo   SHORT
ColsInGrp   SHORT
AlnCol      PSTRING(8)
AlnHdr      PSTRING(8)
Fmt         STRING(1024),AUTO
ListFEQ     LONG
DvTp        STRING(900)    
CRLFSp      STRING('<13,10,32>')  
GrpTp       STRING(900)
GrpWidth    PSTRING(6)
ListXPos    LONG 
ColXPos    LONG 
WidthCol    LONG 
WidthSum    LONG 
TipSaved    BOOL
  CODE
  IF ~DevTipEIP THEN DevTipEIP=CREATE(0,CREATE:Button). 
  ListFEQ=FldQ:FeqNo ; ListXPos=ListFEQ{PROP:XPos}  
  IF ListFEQ{PROP:Msg}='*DevToolTips*' THEN TipSaved=1.  
  LOOP ColX=1 TO 1024
     IF ~F{PROPList:Exists, ColX} THEN BREAK.
     CLEAR(GrpTp)
     GrpNo =  ListFEQ{PROPLIST:GroupNo, ColX}   
     IF GrpNo <> LastGrpNo THEN
        LastGrpNo = GrpNo
        ColsInGrp = ListFEQ{PROPLIST:GroupNo + PROPLIST:Group, ColX}
        IF ColsInGrp THEN
           GrpWidth=ListFEQ{PROPLIST:width + PROPLIST:Group, ColX} ; IF ~GrpWidth THEN GrpWidth='(none)'.
           Fmt = ListFEQ{PROPLIST:Format+ PROPLIST:Group, ColX}
           InX=INSTRING(']',Fmt)   !Group Fmt includes all member columns
           Fmt = QUOTE(CLIP(SUB(Fmt,Inx+1,9999)))
           GrpTp=CRLFSp&'={40}'&CRLFSp&'Start of GROUP #' & GrpNo &' with '& ColsInGrp &' Columns' & |
                 CRLFSp&'Width: ' & GrpWidth &'  Aln: ' & ClaListColAlign(F,ColX,1,1) &|
                 CRLFSp&'Format: '& Fmt 
        END
     END
     AlnCol = ClaListColAlign(F,ColX) 
     AlnHdr = ClaListColAlign(F,ColX,,1)
     IF ~TipSaved THEN F{'WndPrvSave_DefaultTip#' & ColX}=F{PROPLIST:DefaultTip, ColX}.  
     F{PROPLIST:DefaultTip, ColX}=''  !Clear existing Tip so not in format string
     F{PROP:Edit,ColX}=DevTipEIP ; GETPOSITION(DevTipEIP,ColXPos,cY#,cw#,ch#) ; F{PROP:Edit,ColX}=0
     WidthCol=ListFEQ{PROPLIST:width, ColX} ; WidthSum+=WidthCol  
!DB('DevTipCol ColX=' & ColX &' AT '& ColXPos&','&cY#&','&cw#&','&ch# &'  WidthCol=' & WidthCol &' WidthSum=' & WidthSum )
     Fmt = QUOTE( F{PROPLIST:Format, ColX} ) !Quote?
     DvTp='DevTip Column: ' & ColX &'  Field: ' &  ListFEQ{PROPLIST:FieldNo, ColX} &' Group: ' & GrpNo & |
        CRLFSp&'Picture: ' & ListFEQ{PROPLIST:Picture, ColX} &|
        CRLFSp&'Width: ' & WidthCol &'  '& AlnCol & CHOOSE(~AlnHdr OR AlnHdr=AlnCol,'','  Header: '&AlnHdr) & |
            '    Cumm Width: ' & WidthSum &'  XPos: ' & ListXPos+ColXPos &'  '& |   
        CRLFSp&CRLFSp&'Format: ' & CRLFSp & CLIP(Fmt) & CLIP(GrpTp)
    CLEAR(GrpTp)
    ReplaceInto(DvTp,CHR(39),'"') !Tip Delim is ' so must remove.
    F{PROPLIST:DefaultTip, ColX}=DvTp  ! ; DB('F' & F & ' Col#' & ColX &' Tip='& DvTp )            
  END
  IF ~TipSaved THEN ListFEQ{PROP:Msg}='*DevToolTips*'. !So can warn
  EXIT    
!----------------
Trick_NoBlankPictureRtn ROUTINE
  DATA
PIC CSTRING(32)
L   BYTE
  CODE
  SETTARGET(PWnd)
  LOOP R=1 TO RECORDS(FieldQ) ; GET(FieldQ,R)
       PIC=clip(lower(ClaPicture(FldQ:FeqNo,FldQ:TypeNo))) 
       L=LEN(PIC) ; IF L AND PIC[L]='b' AND INSTRING(PIC[1],'ndtekp') THEN PWnd$FldQ:FeqNo{PROP:Text}=PIC[1 : L-1].
  END
  DISPLAY
  SETTARGET()
!----------------    
Trick_ListColsResizeRtn ROUTINE
  LOOP R=1 TO RECORDS(FieldQ)
     GET(FieldQ,R) ; IF TypeIsLIST(FldQ:TypeNo) THEN ClaListColResizable(PWnd,FldQ:FeqNo).
  END
!----------------
Trick_AltKeyAnalRtn ROUTINE !Alt+Key possible, or has KEY()
  DATA
AAP BYTE
  CODE
  GET(FieldQ,CHOICE(?ListF))
  CLEAR(TabQ) ; TabQ:TbFEQ=FldQ:TabFEQ ; IF TabQ:TbFEQ THEN GET(TabQ,TabQ:TbFEQ) ; TabQ:FName=' <9>?'&TabQ:FName .  
  AAP=PopupUnder(?,'Alt+Key Analysis Options|-|Show Only Controls that CAN have an Alt+Key' & | !2 
                                             '|Show Only Controls that DO have an Alt+Key' & | !3
        '|-|' & CHOOSE(~FldQ:TabFEQ,'~','') & 'Analyze Controls on TAB: ' & PopClean(TabQ:Name) ) ! 4
  IF AAP<2 THEN EXIT. 
  IF AAP=4 THEN !Delete NOT on tab, keep Sht and 0=Window 
    LOOP R=RECORDS(FieldQ) TO 1 BY -1 ; GET(FieldQ,R)
        IF NOT(FldQ:TabFEQ=TabQ:TbFEQ OR FldQ:TabFEQ=0 OR FldQ:Parent=TabQ:ShFEQ) THEN DELETE(FieldQ).  
    END ; DISPLAY     
  END
  FieldQ:SeeMore='>Alt+Key<' ; DO SeeMoreButsRtn
  SETTARGET(PWnd)
  LOOP R=RECORDS(FieldQ) TO 1 BY -1
     GET(FieldQ,R)
     IF INLIST(FldQ:TypeNo,CREATE:sheet,CREATE:Tab) OR FieldQ:SeeMore>' ' THEN CYCLE.
     IF AAP<>3 THEN 
       CASE FldQ:TypeNo
       OF CREATE:entry OROF CREATE:list OROF CREATE:combo OROF CREATE:spin OROF CREATE:text OROF CREATE:singleline
          IF ~FldQ:FeqNo{PROP:Key} THEN CYCLE.
       ELSE ; IF TypeHasAltKey(FldQ:TypeNo,FldQ:FeqNo) THEN CYCLE.
       END
     END
     DELETE(FieldQ)
  END 
  SETTARGET() ; SELECT(?ListF,1) ; DISPLAY
!----------------
Trick_PromptStringDeleteRtn ROUTINE
  F=CHOICE(?ListF) 
  LOOP R=RECORDS(FieldQ) TO 1 BY -1
     GET(FieldQ,R)
     CASE FldQ:TypeNo
     OF CREATE:string OROF CREATE:prompt ; DELETE(FieldQ) ; IF R<= F THEN F-=1.
     END
  END
  IF F THEN SELECT(?ListF,F).
!----------------    
Trick_SaveExeRtn ROUTINE
    DATA
Exe STRING(260)
SvX STRING(260)
    CODE
    Exe=COMMAND('0') ; SvX=SUB(Exe,1,LenFastClip(Exe)-4)&'-Save.EXE'
    IF ~FILEDIALOGA('Save Preview EXE',SvX,'EXE Files|*.EXE|All Files|*.*',1+2+10h+80h) THEN EXIT.
    COPY(Exe,SvX) ; IF ERRORCODE() THEN Message('Failed Copy ' & ERROR()&'|'&Exe&'|'&SvX).
!----------------
Trick_EditCaptionRtn ROUTINE 
    DATA 
WinFont     STRING(60)
CapAppend   STRING(60)      !Glo:Caption PSTRING(200)
EdCapRESIZE LONG
ProcName    STRING(64)      !I set Window{'Proc_Name'}=%Procedure in templates
EdCapWnd WINDOW('Edit Caption'),AT(,,290,95),GRAY,SYSTEM,FONT('Segoe UI',9)
        PROMPT('Original Caption:'),AT(5,5),USE(?EdCap:Cap:Pmt)
        ENTRY(@s127),AT(61,5,219),USE(GloT:Caption,, ?EdCap:Cap),SKIP,COLOR(COLOR:BTNFACE),READONLY
        PROMPT('Window Font:'),AT(5,20),USE(?EdCap:Font:Pmt)
        ENTRY(@s60),AT(61,20,219),USE(WinFont),SKIP,COLOR(COLOR:BTNFACE),READONLY
        PROMPT('&Text to Append:'),AT(5,36),USE(?EdCap:Apd:Pmt)
        ENTRY(@s60),AT(61,36,219),USE(CapAppend)
        BUTTON('&OK'),AT(61,53,45),USE(?EdCap:OkBtn),DEFAULT
        BUTTON('Cancel'),AT(113,53,45),USE(?EdCap:CanBtn),STD(STD:Close)
        BUTTON('Save EXE'),AT(183,53,45),USE(?EdCap:Save),TIP('Save Preview EXE so don''t lose on close')
        BUTTON('&Stamp'),AT(250,53,30,11),USE(?EdCap:Stamp),SKIP
        CHECK('RESIZE on Window'),AT(5,76),USE(EdCapRESIZE),SKIP,TIP('Make Window Resizable')
        ENTRY(@s64),AT(114,76,167),USE(ProcName),SKIP,TRN,READONLY        
    END
    CODE 
    WinFont=PWnd{PROP:Font} &' '& PWnd{PROP:FontSize}
    CapAppend=SUB(PWnd{PROP:Text},LEN(GloT:Caption)+5,32)
    EdCapRESIZE=PWnd{PROP:Resize}
    ProcName=PWnd{'Proc_Name'}
    !CapAppend=SUB(SELF.WndRef{PROP:Text},LEN(Glo:Caption)+5,32)
    OPEN(EdCapWnd)
    ACCEPT
        CASE ACCEPTED()
        OF ?EdCap:Stamp ; CapAppend=CLIP(CapAppend)&' '&FORMAT(CLOCK(),@t1)&' '&FORMAT(TODAY(),@d1-) ; DISPLAY
        OF ?EdCapRESIZE ; PWnd{PROP:Resize}=EdCapRESIZE
        OF ?EdCap:Save  ; DO Trick_SaveExeRtn
        OF ?EdCap:OkBtn
            PWnd{PROP:Text}=GloT:Caption & CHOOSE(~CapAppend,'',' -- '& CapAppend)
            !SELF.WndRef{PROP:Text}=Glo:Caption & CHOOSE(~CapAppend,'',' -- '& CapAppend)
            BREAK
        END 
    END
    CLOSE(EdCapWnd)
    EXIT     
!----------------
SeeMoreButsRtn ROUTINE
    DATA
SeeX    LONG,AUTO
PropX   LONG,AUTO
PropUsr PSTRING(31)
CaseX   LONG,AUTO
Lng     LONG,AUTO
FQSM    &STRING
WFont   PSTRING(32),DIM(4)
CFont   PSTRING(32),DIM(4)
WHlp    PSTRING(64) 
N          USHORT
X          USHORT
L          LONG
NXY     LONG,DIM(4)
PXY     LONG,DIM(4)
First1  USHORT
PriorFldQ   GROUP(FieldQ),PRE(PrFQ).
PopMenu    CSTRING(34*17)
PUName     PSTRING(32),DIM(16)
PUProp            LONG,DIM(16)
AK STRING('ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'',-./;=')
K1 STRING(1)
IsAdds2 BYTE !Ctrl+ Prepends (Adds 2)
FQSMadd2  LIKE(FieldQ:SeeMore)
  CODE
  FQSM &= FieldQ:SeeMore    
  N += 1 ; PUProp[N]=-2           ; PUName[N]='Property: ' & AnyPropH
  N += 1 ; PUProp[N]=PROP:CAP     ; PUName[N]='Entry Modifiers CAP UPR INS OVR IMM'
  N += 1 ; PUProp[N]=PROP:Font    ; PUName[N]='Font'
  N += 1 ; PUProp[N]=PROP:HLP     ; PUName[N]='Help HLP()'     
  N += 1 ; PUProp[N]=PROP:Icon    ; PUName[N]='ICON()'
  N += 1 ; PUProp[N]=PROP:Key     ; PUName[N]='KEY() and ALRT()'
  N += 1 ; PUProp[N]=-1*PROP:Key  ; PUName[N]='KEY() and ALT + Key' 
           IF FieldQ:SeeMore='>Alt+Key<' THEN SeeX=N ; GOTO SkipPopLabel: .
  N += 1 ; PUProp[N]=PROP:MSG     ; PUName[N]='MSG() Text'
  N += 1 ; PUProp[N]=-1*PROP:Text ; PUName[N]='@ Picture @'
  N += 1 ; PUProp[N]=PROP:REQ     ; PUName[N]='REQ Required'
  N += 1 ; PUProp[N]=PROP:Tip     ; PUName[N]='TIP() Text'
  N += 1 ; PUProp[N]=PROP:Trn     ; PUName[N]='TRN Transparent'
  N += 1 ; PUProp[N]=-1*PROP:Value; PUName[N]='VALUE() or FROM()'
  N += 1 ; PUProp[N]=PROP:Value   ; PUName[N]='Value PROP:Value'
  N += 1 ; PUProp[N]=PROP:Visible ; PUName[N]='Visible PROP:Visible'
  N += 1 ; PUProp[N]=-1*PROP:XPos ; PUName[N]='X / Y Delta from Preceding'
!**MUST change DIM(16)** to Add New 
  PopMenu=PUName[1] ; LOOP P=2 TO N ; PopMenu=PopMenu &'|'& PUName[P] ; END
  SeeX=POPUPunder(?SeeMoreButs, PopMenu) ; IF ~SeeX THEN EXIT. 
  IsAdds2=CHOOSE(KeyStateSCA(2))  !; IF IsAdds2 THEN message('IsAdds2=' & IsAdds2 ).
SkipPopLabel:  PropX=PUProp[SeeX]
    CaseX=PropX
    CASE CaseX
    OF -2
       IF AnyPropH[1]=CHR(39) THEN
          PropUsr=CLIP(LEFT(SUB(AnyPropH,2,99)))
          L=LEN(PropUsr) ; IF L>1 AND PropUsr[L]=CHR(39) THEN PropUsr=CLIP(PropUsr[1 : L-1]).
          CaseX=-3
       ELSE
          PropX=EVALUATE(AnyPropH)
          IF ERRORCODE() OR PropX < 7000h OR PropX > 7FFFh THEN
             CASE Message('EVALUATE(' & CLIP(AnyPropH) &') = ' & PropX &' is not a property 7000h-7FFFFh (defined 7200-7DFF)' , |
                          'See More',,'Close|''User Prop''')
             OF 2 ; AnyPropH=CHR(39) & AnyPropH ; POST(EVENT:Accepted,?SeeMoreButs)
             END ; SELECT(?AnyPropH) ; EXIT
          END
       END
    END
    SETTARGET(PWnd)
    ClaFont4(0, WFont[]) 
    WHlp=CLIP(0{PROP:Hlp})
    LOOP R=1 TO RECORDS(FieldQ)
         GET(FieldQ,R) ; FQSMadd2=FQSM ; FQSM=''
         F=FldQ:FeqNo
         CASE CaseX
         OF -3 ; FQSM=F{PropUsr} ; L=FQSM+0 ; IF NUMERIC(FQSM) AND (L<-256 OR L>256) THEN FQSM=CLIP(FQSM)&' = '& Hex8(L,-4). 
         OF PROP:Font ; ClaFont4(F, CFont[])  ; FQSM=ClaFont4Net(WFont[],CFont[])
         OF -1*PROP:Text  ; FQSM=ClaPicture(F,FldQ:TypeNo) ; IF FQSM THEN FQSM=CLIP(FQSM) &' {9}'& FldQ:Type &'  '&FldQ:FeqName.
!TODO *-1 YPOS would be Delta from Control After (succeeds)
         OF -1*PROP:XPos ; !GETPOSITION(F,NXY[1],NXY[2],NXY[3],NXY[4]) ; FQSM='X=' & NXY[1]-PXY[1] & '  Y=' & NXY[2]-PXY[2] & '  W=' & NXY[3]-PXY[3]  & '  H=' & NXY[4]-PXY[4] ; PXY=NXY
                           DO FieldQGetPositionRtn
 !FQSM=LEFT('X=' & FldQ:AtX-PrFQ:AtX,12) & LEFT('  Y=' & FldQ:AtY-PrFQ:AtY,12) & '  W=' & FldQ:AtWGet-PrFQ:AtWGet & '  H=' & FldQ:AtHGet-PrFQ:AtHGet
 FQSM='dX=' & FmtNumSM(FldQ:AtX-PrFQ:AtX,5) & '  dY=' & FmtNumSM(FldQ:AtY-PrFQ:AtY,5) & '  dW=' & FmtNumSM(FldQ:AtWGet-PrFQ:AtWGet,5) & '  dH=' & FmtNumSM(FldQ:AtHGet-PrFQ:AtHGet,5)
    !TODO Change in Align
    
     OF -1*PROP:Value ; FQSM=SeeMore(CaseX,F,FieldQ:TypeNo)
         OF PROP:Key ! ; Lng=F{PROP:Key} ; IF Lng THEN FQSM=ClaKeyCodeExplain(Lng). 
                 LOOP X=1 TO 255 ; L=F{PROP:Alrt,X} ; IF L THEN FQSM=CLIP(FQSM)&' '& ClaKeyCodeExplain(L,1). ; END
                        L=F{PROP:Key} ; IF L THEN FQSM=' Key('&ClaKeyCodeExplain(L,1)&')'&FQSM ELSE FQSM=LEFT(FQSM).

     OF -1*PROP:Key ; IF ~TypeHasAltKey(FldQ:TypeNo) THEN X=0 ELSE X=STRPOS(FldQ:Text,'&[^& ]'). ; L=F{PROP:Key} 
                      IF X+L=0 THEN CYCLE. 
                      IF X THEN 
                         K1=UPPER(FldQ:Text[X+1]) ; N=INSTRING(K1,AK) ; IF N THEN AK=SUB(AK,1,N-1)&SUB(AK,N+1,48). 
                      END
                      FQSM=CHOOSE(~X,'','ALT + '& K1 &' ') & CHOOSE(~L,'','Key('&ClaKeyCodeExplain(L,1)&')') &|
                           CHOOSE(~FldQ:TabName,'','   Tab: '& FldQ:TabName)

         OF PROP:Hlp  ; FQSM=F{PropX} ; IF UPPER(FQSM)=UPPER(WHlp) THEN FQSM=''.
                        IF FQSM AND UPPER(FQSM)=UPPER((F{PROP:Parent}){PropX}) THEN FQSM=''.
         OF PROP:CAP  ; FQSM=PropTFName(F,PROP:CAP,'CAP ') & PropTFName(F,PROP:UPR,'UPR ') &|
                             PropTFName(F,PROP:INS,'INS ') & PropTFName(F,PROP:OVR,'OVR ') & PropTFName(F,PROP:MASK,'MASK') &|
                             PropTFName(F,PROP:IMM,'IMM ') & PropTFName(F,PROP:REQ,'REQ ')
         ELSE ; FQSM=F{PropX} ; SELF.PropDescribe(PropX,FQSM,1) !PROP:Icon PROP:Tip PROP:MSG PROP:Value etc     
         END
         If IsAdds2 AND FQSMadd2 THEN FQSM=CHOOSE(~FQSM,FQSMadd2,CLIP(FQSM) &' -- '& CLIP(FQSMadd2)).
         PUT(FieldQ) ; PriorFldQ=FieldQ
         IF ~First1 AND FQSM THEN First1=R.
    END
    SETTARGET()
    If IsAdds2 THEN FQSMadd2=?ListF{PROPLIST:Header,SeeMore:ColNum}.
    FQSM=PUName[SeeX]
    CASE PropX
    OF PROP:Font ; CFont=WFont ; CLEAR(WFont[]) ; FQSM='Font: ' & ClaFont4Net(WFont[],CFont[]) 
    OF PROP:HLP  ; IF WHlp[1]='~' THEN WHlp[1]=' '. ; FQSM='Help - Window HLP( ' & WHlp &')'  !FYI ~in Header BAD
    OF -1*PROP:Key ; FQSM=CLIP(FQSM)&'<13,10>Unused: ' & AK
    END     
    ?ListF{PROPLIST:Header,SeeMore:ColNum}=CHOOSE(~First1,'(None) ','') & CLIP(FQSM) & CHOOSE(~IsAdds2,'',' -- '& FQSMadd2)
    GET(FieldQ,CHOICE(?ListF)) ; IF ~FQSM AND First1 THEN ?ListF{Prop:Selected}=First1. !Selected SeeMore='' then change to first
    DISPLAY
    EXIT
!----------------
LoadFieldQRtn ROUTINE
    DATA
FType   LONG
    CODE
    Prop:HDRS[1]=PROP:Hide ; Prop:HDRS[2]=PROP:Disable ; Prop:HDRS[3]=PROP:ReadOnly ; Prop:HDRS[4]=PROP:Skip
    FREE(SELF.FeqNmQ) 
    F=0
    LOOP
      F=0{PROP:NextField,F} ; IF ~F THEN BREAK. ; IF SELF.FeqCreatedByCB(F) THEN CYCLE.
      IF F < 0 AND ~SELF.MenuItemShows THEN CYCLE.  !TODO keep Menus in queue to quickly add 
      !TODO Check PROP:InToolBar or BETTER if Parent is Negative
      IF F{PROP:Parent} < 0 THEN CYCLE.     !TODO Right? 
      CLEAR(FieldQ)
      FldQ:FeqNo = F 
      FldQ:FeqName=FeqNameUpLow(ClaFeqName(F)) !04/30/20 UpLow !; message('#1 FldQ:FeqName=' & FldQ:FeqName )
      SELF.FeqNmQ.FEQ = F ; SELF.FeqNmQ.Name=CLIP(FldQ:FeqName) ; ADD(SELF.FeqNmQ,SELF.FeqNmQ.FEQ)  !Store for use by 
      IF FldQ:FeqName[1]='?' THEN FldQ:FeqName=FldQ:FeqName[2 : SIZE(FldQ:FeqName)] ; FldQ:QMark='?'; END
      FldQ:Text=F{PROP:Text} ; IF FldQ:FeqName[1]='V' AND FldQ:Text[1:16]='The TEXT control' THEN FldQ:Text=SUB(FldQ:Text,1,47).
      FType=F{Prop:Type}       
      FldQ:TypeNo=BAND(FType,0FFh) ; FldQ:Type16=FType 
      FldQ:Type = UPPER(ClaControlTypeName(FType))
      CASE FldQ:TypeNo
      OF CREATE:BUTTON
         IF ~FldQ:Text THEN  !Btn No Text then show Icon
             FldQ:Text = F{PROP:Icon} ; ClaIconEquate(FldQ:Text)
             FldQ:Text = LEFT(CLIP(FldQ:Text) &' '& F{PROP:Tip})
         END
      OF CREATE:LIST OROF CREATE:COMBO  
         FldQ:Text=LEFT(CLIP(FldQ:Text) &' '& QUOTE(F{PROP:Format}) &' '& F{PROP:From})
         R=F{PROP:Drop} ; IF R THEN FldQ:Type=CLIP(FldQ:Type)&'-Drop ' & R. 
      END 
      LOOP P=1 TO MAXIMUM(Prop:HDRS[],1) !Read Checkbox for Hide,Disable,ReadOnly,Skip PROPs
           FldQ:HDRSchk[P]=CHOOSE(F{Prop:HDRS[P]}=True,Check1w,Check0n)
           Count:HDRS[P]+=CHOOSE(FldQ:HDRSchk[P]=Check1w)
      END
      GETPOSITION(F,FldQ:AtX,FldQ:AtY, FldQ:AtWGet,FldQ:AtHGet) 
  !FldQ:SeeMore='GET ' & FldQ:AtWGet &','& FldQ:AtHGet &' WHProp=' & F{PROP:Width} &','& F{PROP:Height} &'  NoW='&   F{PROP:NoWidth}  &'  NoH='& F{PROP:NoHeight}
      IF FldQ:AtX=_nopos THEN FldQ:AtX=0. ; IF FldQ:AtY=_nopos THEN FldQ:AtY=0.
      FldQ:AtH=CHOOSE(~F{PROP:Full},'','full')
      FldQ:AtW=CHOOSE(~F{PROP:NoWidth} ,''&FldQ:AtWGet,FldQ:AtH)
      FldQ:AtH=CHOOSE(~F{PROP:NoHeight},''&FldQ:AtHGet,FldQ:AtH)
      FldQ:Align=ClaAlign(F,FType)
      FldQ:Parent=F{PROP:Parent} 
         
  COMPILE('!*C100*',_C100_)
      FldQ:Precedes=F{PROP:Precedes}
  !*C100*     
      FldQ:Follows=F{PROP:Follows}       
  !FldQ:SeeMore=CHOOSE(~F{PROP:Visible},'~Viz ','Vis1 ') &' '& CHOOSE(~F{PROP:Enabled},'~Enb ','Ena1 ') & |
      !       CHOOSE(~F{PROP:Hide},' ','Hide ') &' '& CHOOSE(~F{PROP:Disable},'','Disb ') & |
      !        ' Flw'& FldQ:Follows &' Prc'& FldQ:Precedes &'  Par'& FldQ:Parent   !find tab order DEBUG 
      CASE FldQ:TypeNo
      OF CREATE:Line
         IF FldQ:AtWGet=0 THEN
            FldQ:Type='LINE-Vert'
            FldQ:Text='LINE VERTICAL' & CHOOSE(FldQ:AtHGet<0,' UP','') ; FldQ:Align='V '&FldQ:Align 
         ELSIF FldQ:AtHGet=0 THEN
            FldQ:Type='LINE-Horz'
            FldQ:Text='LINE HORIZONTAL' & CHOOSE(FldQ:AtWGet<0,' LEFT','') ; FldQ:Align='H '&FldQ:Align
         ELSE
            FldQ:Type='LINE-Diag'
            FldQ:Text='LINE DIAGONAL (' & FldQ:AtWGet &','& FldQ:AtHGet &')'
         END
      END        
      ADD(FieldQ,FldQ:FeqNo)
    END ; DO LoadParentsRtn
LoadParentsRtn ROUTINE
  DATA
PT   BYTE
ParQ QUEUE,PRE(ParQ) !Parent Stack
PFEQ  LONG
ShFEQ LONG
TbFEQ LONG
Name  STRING(32)
     END
PopCnt BYTE
ZeroFlwFEQ LONG
FlwSeqNo LONG
FlwNextFEQ LONG(0) !1st follows 0=Window
  CODE
  !Hidden companion controls for Drop list/combo have Follow=0 are problem
  F=10^INT(LOG10(RECORDS(FieldQ)+2)+.999)-1 !F=99 999
  SORT(FieldQ,-FldQ:FeqNo)
  LOOP R=1 TO RECORDS(FieldQ) ; GET(FieldQ,R) ; FldQ:FolSEQ=F ; PUT(FieldQ) ; F-=1
  !  FldQ:SeeMore='Feq:' & FldQ:FeqNo &' Fol:' & FldQ:Follows &' Pre:' & FldQ:Precedes &' Type16:' & FldQ:Type16 &' F=' & F ; PUT(FieldQ)
    IF FldQ:Follows=0 AND (FldQ:Precedes<>0 OR FldQ:Type16<100h) THEN ZeroFlwFEQ=FldQ:FeqNo. !First Control Follows zero and ...
  END
  IF ZeroFlwFEQ THEN !First "user" control that Follows Zero
     FldQ:FeqNo=ZeroFlwFEQ
     GET(FieldQ,FldQ:FeqNo) ; FlwSeqNo += 1 ; FldQ:FolSEQ = FlwSeqNo ; PUT(FieldQ)
     FlwNextFEQ=FldQ:FeqNo !Who Follows ME?
  END
  LOOP 32000 TIMES  !Build FieldQ FldQ:FolSEQ in Follows order, controls w/o FEQ Name get 30000+ and are out of FEQ order
     FldQ:Follows=FlwNextFEQ
     GET(FieldQ,FldQ:Follows) ; IF ERRORCODE() THEN BREAK.
     FlwSeqNo += 1 ; FldQ:FolSEQ = FlwSeqNo ; PUT(FieldQ)
     FlwNextFEQ=FldQ:FeqNo !Who Follows ME?
  END  !PROP:NextField is NOT in this order
  SORT(FieldQ,FldQ:FolSEQ,FldQ:FeqNo)
  CLEAR(ParQ) ; ADD(ParQ,1) ; FREE(TabQ) ; SheetCnt=0
  LOOP R=1 TO RECORDS(FieldQ) ; GET(FieldQ,R) ; F=FldQ:FeqNo 
    IF FldQ:Parent <> ParQ:PFeq THEN  !next Control has diff parent, Pop Stack 
       LOOP ; GET(ParQ,1) ; IF ParQ:PFeq=FldQ:Parent OR ParQ:PFeq=0 OR ERRORCODE() THEN BREAK. ; DELETE(ParQ) ; END 
    END
    FldQ:TabName=ParQ:Name ; FldQ:TabFEQ=ParQ:TbFEQ ; FldQ:SheetFEQ=ParQ:ShFEQ ; PUT(FieldQ) 
    PT=INLIST(FldQ:TypeNo,CREATE:sheet,CREATE:tab,CREATE:toolbar,CREATE:group,CREATE:option) ; IF ~PT THEN CYCLE.
    ParQ:PFeq=F 
    CASE PT
    OF 1 ; CLEAR(TabQ) ; TabQ:ShFEQ=F ; TabQ:FName=FldQ:FeqName ; TabQ:Name=' SHEET ?'&TabQ:FName ; ADD(TabQ) ; SheetCnt+=1
    OF 2 ; TabQ:TbFEQ=F ; TabQ:TbNum+=1 ; PopCnt+=1 ; TabQ:PopNo=PopCnt ; 
           TabQ:Text=CLIP(LEFT(F{PROP:Value})) ; IF ~CLIP(TabQ:Text) THEN TabQ:Text=CLIP(FldQ:Text).
           TabQ:FName=FldQ:FeqName ; TabQ:Name=TabQ:Text &' ?' & TabQ:FName ; ADD(TabQ)
           FldQ:TabFEQ=F ; PUT(FieldQ)
    END
    IF PT<3 THEN ParQ:Name=TabQ:Name ; ParQ:ShFEQ=TabQ:ShFEQ ; ParQ:TbFEQ=TabQ:TbFEQ . ; ADD(ParQ,1) !Push
  END !; SetClip2Queue(FieldQ) ; Message('CB FieldQ') 
TabPickRtn ROUTINE
  DATA
PU ANY
ChkNo BYTE
Hyde PSTRING(16)
SavFQ LONG
  CODE
  SETTARGET(PWnd)
  LOOP R=1 TO RECORDS(TabQ) ; GET(TabQ,R) ; TabQ:Name=PopClean(TabQ:Name) 
     IF TabQ:TbFEQ=0 THEN
        TabQ:Hide=TabQ:ShFEQ{PROP:Hide} ; PUT(TabQ) ; Hyde=CHOOSE(~TabQ:Hide,'','  <9>(hide)')
        IF R=1 AND SheetCnt>1 THEN PU=TabQ:Name &Hyde &'{{' ; ELSIF R>1 THEN PU=PU&'}|' & TabQ:Name &Hyde &'{{'. 
        ChkNo=TabQ:ShFEQ{PROP:Selected} ; CYCLE
     END
     TabQ:Hide=TabQ:TbFEQ{PROP:Hide} ; PUT(TabQ) ; Hyde=CHOOSE(~TabQ:Hide,'','(hide)      ')
     PU=PU& CHOOSE(TabQ:TbNum=1,'','|') & CHOOSE(ChkNo=TabQ:TbNum,'+','') & PopClean(TabQ:Text) &'<9>'& Hyde &'?'& PopClean(TabQ:FName)  
  END
  SETTARGET() ; IF SheetCnt>1 THEN PU=PU&'}'. ; P=POPUPunder(?,PU) ; IF ~P THEN EXIT.
  TabQ:PopNo=P ; GET(TabQ,TabQ:PopNo) ; FldQ:FeqNo=TabQ:TbFEQ ; GET(FieldQ,FldQ:FeqNo) 
  IF TabQ:Hide THEN PWnd$TabQ:TbFEQ{PROP:Hide}='' ; DO SyncFldQ:HDRSRtn. ; PWnd$TabQ:ShFEQ{PROP:Selected}=TabQ:TbNum
  SELECT(?ListF,POINTER(FieldQ)) ; DISPLAY 
!----------------
SyncFldQ:HDRSRtn ROUTINE !Sync FieldQ to the Preview - Caller must GET(FieldQ,F)
  DATA
Q_HDRS BYTE !FldQ Checked
P_HDRS BYTE !Prvw Checked
CircleW BYTE !"w" Icons
  CODE   
  LOOP P=4 TO 1 BY -1
    Q_HDRS=BAND(1,INLIST(FldQ:HDRSchk[P],Check1n,CHR(0),Check1w))
    P_HDRS=PWnd$FldQ:FeqNo{Prop:HDRS[P]}   
    IF P_HDRS=Q_HDRS THEN CYCLE. ;  CircleW=BAND(1,INLIST(FldQ:HDRSchk[P],Check1w,CHR(0),Check0w))
    FldQ:HDRSchk[P]=CHOOSE(1+P_HDRS+CircleW*2, Check0n,Check1n, Check0w,Check1w) ; PUT(FieldQ) 
    Count:HDRS[P] += CHOOSE(~P_HDRS,-1,1)
  END         
!----------------
FieldQGetPositionRtn ROUTINE
    GETPOSITION(FldQ:FeqNo,FldQ:AtX,FldQ:AtY, FldQ:AtWGet,FldQ:AtHGet) 
!FldQ:SeeMore='GET ' & FldQ:AtWGet &','& FldQ:AtHGet &' WHProp=' & F{PROP:Width} &','& F{PROP:Height} &'  NoW='&   F{PROP:NoWidth}  &'  NoH='& F{PROP:NoHeight}
    IF FldQ:AtX=_nopos THEN FldQ:AtX=0. ; IF FldQ:AtY=_nopos THEN FldQ:AtY=0.
    FldQ:AtH=CHOOSE(~FldQ:FeqNo{PROP:Full},'','full')
    FldQ:AtW=CHOOSE(~FldQ:FeqNo{PROP:NoWidth} ,''&FldQ:AtWGet,FldQ:AtH)
    FldQ:AtH=CHOOSE(~FldQ:FeqNo{PROP:NoHeight},''&FldQ:AtHGet,FldQ:AtH)
    FldQ:Align=ClaAlign(FldQ:FeqNo,FldQ:TypeNo)
!----------------
AllUncheckRtn ROUTINE
    DATA
P1 BYTE    
P2 BYTE
X2 BYTE
PLWG EQUATE(PROPLIST:Width+PROPLIST:Group)  
  CODE
  IF KeyStateSCA(2) THEN 
     P=5
  ELSE
     P=POPUPunder(?AllUncheckBtn,|
       'Uncheck all HIDE<9>'     & Count:HDRS[1]& |
      '|Uncheck all DISABLE<9>'  & Count:HDRS[2]& |
      '|Uncheck all READONLY<9>' & Count:HDRS[3]& |
      '|Uncheck all SKIP<9>'     & Count:HDRS[4]& |
      '|-|Everything - Hide Disable Readonly Skip<9>Ctrl+Click+ALL'& |
      '|-|Clear all ALERT Keys' & |
      '|-|' & CHOOSE(~AllColsHide,'-','+') &'Hide the "Hide Disable" Check Columns') !7                                                       !7
  END        
  IF ~P THEN EXIT. ; P1=P ; P2=P ; IF P=5 THEN P1=1 ; P2=4.
  IF P=6 THEN
     SETTARGET(PWnd)
     LOOP F=0 TO RECORDS(FieldQ) ; GET(FieldQ,F) ; IF ~F THEN FldQ:FeqNo=0. 
       LOOP R=1 TO 255 ; FldQ:FeqNo{Prop:Alrt,R}='' ; END
     END ; SETTARGET() ; EXIT
  END
  IF P=7 OR (P=5 AND AllColsHide=0) THEN
     IF ~AllColsHide THEN AllColsHide=?ListF{PLWG,1} ; ?ListF{PLWG,1}=0 |  !On put back loses Right and Resize?
     ELSE ?ListF{PLWG,1}=AllColsHide ; AllColsHide=0 ; ?ListF{PROPLIST:RightBorder,4}=1 ; ?ListF{PROPLIST:Resize+PROPLIST:Group,1}=1 .
!       ELSE ?ListF{PLWG,1}=AllColsHide ; AllColsHide=0 ; ; ?ListF{PROPLIST:Resize+PROPLIST:Group,1}=1 ; ?ListF{PROPLIST:RightBorder+PROPLIST:Group,1}=1 .
        IF P=7 THEN EXIT.                       
  END
  SETTARGET(PWnd)
  LOOP X2=1 TO 2 !Seen take 2 times when GROUPs Disable
    LOOP F=1 TO RECORDS(FieldQ)
      GET(FieldQ,F) ; IF FldQ:Type16 > 0FFh THEN CYCLE. !FFh are Drop List and Buttons
      LOOP P=P2 TO P1 BY -1
        R=INLIST(FldQ:HDRSchk[P],Check1n,Check1w)
        IF R THEN
           FldQ:HDRSchk[P]=CHOOSE(R,Check0n,Check0w)
           IF X2=2 THEN Count:HDRS[P] -= 1.
           FldQ:FeqNo{Prop:HDRS[P]}=''
        END            
      END
      IF X2=2 THEN PUT(FieldQ).
    END ; DISPLAY
  END ; SETTARGET() ; DISPLAY
  EXIT
!----------------
LISTsBtnRtn ROUTINE
    DATA
LcQ QUEUE,PRE(LcQ)
F    LONG !LcQ:F
    END
PU  ANY
    CODE
    LOOP F=1 TO RECORDS(FieldQ)
        GET(FieldQ,F) ; IF ~TypeIsLIST(FldQ:TypeNo) THEN CYCLE. ; IF FldQ:FeqNo>32000 THEN CYCLE. ; IF ~PWnd$FldQ:FeqNo{PROP:Format} THEN CYCLE.
        CLEAR(TabQ) ; TabQ:TbFEQ=FldQ:TabFEQ ; IF TabQ:TbFEQ THEN GET(TabQ,TabQ:TbFEQ) ; TabQ:FName=' <9>?'&TabQ:FName .
        P=INSTRING('-',FldQ:Type) ; IF P THEN FldQ:Type=SUB(FldQ:Type,1,P-1).
        PU=CHOOSE(~LcQ:F,'',PU&'|') & FldQ:FeqName &'  <9>'& CLIP(FldQ:Type) & CLIP(TabQ:FName) & |
            '<9>'&'  AT('& FldQ:AtX &','& FldQ:AtY &' , '& FldQ:AtWGet&','& FldQ:AtHGet &')  '& FldQ:FeqNo
        LcQ:F=F ; ADD(LcQ)            
    END
    IF ~LcQ:F THEN PU='~No LIST on Window'.
    IF RECORDS(LcQ)=1 THEN F=1 ELSE F=POPUPunder(?LISTsBtn,PU). ; IF ~F THEN EXIT.
    GET(LcQ,F) ; GET(FieldQ,LcQ:F)
    ?ListF{PROP:Selected}=POINTER(FieldQ)
    POST(EVENT:ListPROPs) 
!---------------------------------------
FmtNumSM PROCEDURE(STRING Numbr,BYTE pWidth)!,STRING  !To align SeeMore Numbers "sort of" add 2 spaces per blank
LenNum SHORT
    CODE
    LenNum=LenFastClip(Numbr)               
    IF LenNum>=pWidth THEN RETURN Numbr[1 : LenNum].
    RETURN Clip(Numbr) & ALL(' ',(pWidth-LenNum)*2)
!============================
CBWndPreviewClass.AtSetOrSave   PROCEDURE(BYTE Set1Save2, *LONG[] A, BYTE NoSetXY=0)
!TODO Split into 2 procedure AtSet and AtSave. At() passes  MinW MinH  0=None 1=Current, 
    CODE
    IF KeyStateSCA(1) THEN  !Shift Down no action
    ELSIF Set1Save2=1 THEN      !1=Window Open
        0{PROP:MinWidth}  = 0{PROP:Width}  / 3  !TODO better way
        0{PROP:MinHeight} = 0{PROP:Height} / 3
        IF A[3] THEN
           IF NoSetXY THEN 
              GETPOSITION(0,A[1],A[2])   !X,Y of NonMDI can be relative to MDI and end up way off
           END
           SETPOSITION(0,A[1],A[2],A[3],A[4])
        END
    ELSE 
        GETPOSITION(0,A[1],A[2],A[3],A[4])   !Retain in class so reopen is the same
    END
    RETURN
!----------------
CBWndPreviewClass.Win32PropsAdd  PROCEDURE(PropQType PQ, LONG hWnd, LONG FEQ, LONG FeqTypeNo) 
CStr CSTRING(256),AUTO
A LONG,DIM(4)
L LONG,AUTO
    CODE
    IF ~hWnd THEN RETURN.     !TIDI Check if Vaid Window?
    IF GetWindowRect(hWnd,ADDRESS(A)) THEN
       PQ:Name = ' At Win Rect LU x RB' 
       LOOP L=1 TO 2 
           SELF.PropQAdd(PQ,-32,PQ:Name, A[1] &','& A[2] &' x '& A[3] &','& A[4] & '  Width,Height=' & A[3]-A[1] &','& A[4]-A[2] )
           IF L=2 OR ~GetClientRect(hWnd,ADDRESS(A)) THEN BREAK.
           PQ:Name = ' At Win Client Rect LU x RB'  
       END
    END
    !ClientToScreen
    IF GetClassName(hWnd,CStr,SIZE(CStr)) THEN 
       SELF.PropQAdd(PQ, -32, 'Win ClassName',CStr)
    END 
    L=GetWindowLong(hWnd, -16) ;  SELF.PropQAdd(PQ, -32, 'Win Style GWL_STYLE',  Hex8(L) &'  '& Binary8(L) &'  '& L)  
    L=GetWindowLong(hWnd, -20) ;  SELF.PropQAdd(PQ, -32, 'Win Style GWL_EXSTYLE',Hex8(L) &'  '& Binary8(L) &'  '& L)
    !L=GetWindowLong(hWnd,  -6) ;  SELF.PropQAdd(PQ, -32, 'Win Style GWL_HINSTANCE',Hex8(L) &'  '& Binary8(L) &'  '& L) !Retrieves a handle to the application instance.
    !L=GetWindowLong(hWnd, -12) ;  SELF.PropQAdd(PQ, -32, 'Win Style GWL_ID',Hex8(L) &'  '& Binary8(L) &'  '& L)        !Retrieves the identifier of the window.
    !L=GetWindowLong(hWnd, -21) ;  SELF.PropQAdd(PQ, -32, 'Win Style GWL_USERDATA',Hex8(L) &'  '& Binary8(L) &'  '& L)  !Retrieves the user data associated with the window.
    RETURN   
!----------------
CBWndPreviewClass.WindowPROPs   PROCEDURE()
    CODE
    SELF.ControlPROPs(0,SELF.WndTypeNo,SELF.WndTypeName,SELF.WndLabel)
    RETURN   
CBWndPreviewClass.ControlPROPs  PROCEDURE(LONG FEQ, LONG FeqTypeNo, STRING FeqTypeName, STRING FeqName) 
PQ PropQType
X  LONG,AUTO
PE LONG,AUTO
TN LONG,AUTO
L  LONG,AUTO
Val STRING(255),AUTO 
ValLng LONG,AUTO
FindCls CBLocateCls
FindTxt STRING(64),STATIC 
Window WINDOW('ControlPROPs'),AT(,,250,250),GRAY,SYSTEM,FONT('Segoe UI',9),RESIZE
        BUTTON('<50>'),AT(2,2,12,12),USE(?UnderBtn),SKIP,FONT('Webdings'),TIP('Move Preview under th' & |
                'is Window'),FLAT
        BUTTON('Cl&ose'),AT(17,2,27,12),USE(?CloseBtn),SKIP,STD(STD:Close)
        BUTTON('LIST'),AT(78,2,27,12),USE(?LISTBtn),DISABLE,SKIP,TIP('PROPLIST Viewer')
        BUTTON('Resize'),AT(47,2,27,12),USE(?ResizeBtn),SKIP,TIP('Reposition i.e. Move or Resize')
        BUTTON('&Scan'),AT(154,2,27,12),USE(?ScanBtn),SKIP,TIP('Scan for undocumented PROPs 7900h-7DFF?')
        BUTTON('&Copy'),AT(123,2,27,12),USE(?CopyBtn),SKIP
        BUTTON('?'),AT(108,2,12,12),USE(?HelpBtn),KEY(F2Key),SKIP,FONT(,,,FONT:bold),TIP('Clarion He' & |
                'lp - F2')
        BUTTON('3'),AT(186,2,12,12),USE(?TrashBtn),SKIP,FONT('WingDings 2',14),TIP('Delete Clutter' & |
                '<13,10>Ctrl+Click to leave only Clutter')
        CHECK('Hide'),AT(205,4,25,8),USE(GloT:Hide),SKIP,TIP('Hide this Window when open others')
        BUTTON('Test'),AT(229,2,17,12),USE(?TestsBtn),SKIP,TIP('Carl Tests'),FLAT
        ENTRY(@s64),AT(31,19,182,10),USE(FindTxt),SKIP,FONT('Consolas')
        BUTTON('&Find'),AT(2,18,25,11),USE(?FindNext),SKIP
        BUTTON('Pre&v'),AT(216,18,26,11),USE(?FindPrev),SKIP
        LIST,AT(0,33),FULL,USE(?LIST:PQ),VSCROLL,FONT('Consolas',10),FROM(PQ),FORMAT('27L(3)|FM~Equa' & |
                'te~L(1)@s5@80L(2)|FM~Property~@s32@?20L(2)F~Value  (Double Click for more)~@s255@'), |
                ALRT(DeleteKey)
    END
SysMenuCls SysMenuClass
SortCls CBSortClass
    CODE
    PWnd &= SELF.WndRef
    SETTARGET(PWnd)
    DO AddPropsRtn
    SETTARGET()
    SORT(PQ,PQ:Name)
    OPEN(Window) ; SysMenuCls.Init(Window) ;SELF.AtSetOrSave(1, AtCtrlProps[])
    0{PROP:Text} = CLIP(FeqTypeName) & ' Prop: ' &'  Type: ' & FeqTypeNo  &'  Feq: '& FEQ &'  '& FeqName
    MakeOverList(?List:PQ)
    SortCls.Init(PQ,?LIST:PQ,2)
    FindCls.Init(PQ, ?LIST:PQ, ?FindTxt, ?FindNext, ?FindPrev)
    IF TypeIsLIST(FeqTypeNo) THEN ENABLE(?LISTBtn).
    ACCEPT 
        CASE ACCEPTED()
        OF ?UnderBtn ; SysMenuCls_SYSCOMMAND(0{PROP:Handle},SMCmd_MoveUnder)   
        OF ?CopyBtn ; SELF.PropQCopy2Clip(PQ)
        OF ?ScanBtn ; DISABLE(?) ; DO ScanRtn
        OF ?LISTBtn ; HIDE(0) ; SELF.ListPROPs(FEQ,FeqTypeNo,FeqTypeName,FeqName) ; UNHIDE(0) 
        OF ?ResizeBtn ; SELF.ResizeControl(FEQ, FeqTypeNo, FeqTypeName, FeqName) 
        OF ?HelpBtn ; GET(PQ,CHOICE(?LIST:PQ)) ; HelpCW(PQ:Name,1)
        OF ?TrashBtn  ; PropTrashDelete(PQ)
        OF ?TestsBtn  ; DO TestsRtn
        END
        IF FIELD()=?LIST:PQ THEN 
           GET(PQ,CHOICE(?LIST:PQ))
           CASE EVENT()
           OF EVENT:AlertKey ; IF KEYCODE()=DeleteKey THEN DELETE(PQ).
           OF EVENT:NewSelection
              IF KeyCode()=MouseLeft2 THEN 
                 PropViewWindow('View ' & CLIP(FeqTypeName) & ' FEQ '& FEQ &' '& PQ:Name, PQ, |
                                CHOOSE(PQ:EqtLong <1, PQ:Value, PWnd$Feq{PQ:EqtLong}) )
                    
              END
           OF EVENT:HeaderPressed ; SortCls.HeaderPressed()              
           END !Case Event
        END !IF List PreAlert       
    END !Accept
    SELF.AtSetOrSave(2, AtCtrlProps[])
    CLOSE(Window)
    RETURN
!-----------------    
TestsRtn ROUTINE
    DATA
F   LONG    
FQ  QUEUE(PQ),PRE(FQ).
AQ  QUEUE(PQ),PRE(AQ).
SaveFEQ  LONG  
TNo  LONG  
    CODE 
    Tno=POPUP('KeyCodes|Keys Small|All Fields|-|Halt')
    CASE TNo
    OF 1 OROF 2 ! Debug Keycodes
        FREE(PQ)     
        LOOP M#=1 TO 4 ; S#=CHOOSE(M#,0,100h,200h,400h,300h)
            LOOP K#=1 + S# TO S# + 7Fh ;  SELF.PropQAdd(PQ,K#,'K# ' & K# &' '& Hex8(K#,2), CHOOSE(TNo=2,ClaKeyCodeExplain(K#,1),ClaKeyCodeExplain(K#)  &'  '& Binary8(K#))) 
        . . 
    OF 3 !---- All Fields and Props 
        SaveFEQ=FEQ ; SETTARGET(PWnd) 
        FEQ=0 ; LOOP ; FEQ=0{PROP:NextField,FEQ} ; IF ~FEQ THEN BREAK.
            SELF.PropQAdd(FQ, FEQ, ClaControlTypeName(FEQ{PROP:Type}) &' ={30}' , ClaFeqName(FEQ) )
        END 
        LOOP F=1 TO RECORDS(FQ) 
            GET(FQ,F) ; FEQ=FQ:EqtLong ; FREE(PQ) ; DO AddPropsRtn 
            AQ=FQ ; ADD(AQ) ; LOOP X=1 TO RECORDS(PQ) ; GET(PQ,X) ; AQ=PQ ; ADD(AQ) ; END 
        END 
        FREE(PQ) ; LOOP X=1 TO RECORDS(AQ) ; GET(AQ,X) ; PQ=AQ ; ADD(PQ) ; END
        SETTARGET()  ; FEQ=SaveFEQ
    OF 3 ; HALT
    END
!-----------------
AddPropsRtn ROUTINE
    DATA
A   LONG,DIM(4)
    CODE
    SELF.PropQAdd(PQ, PROP:Type, 'Type', FeqTypeName )  
    PQ:Name=' At Pos XYWH' ; PWnd{PROP:Pixels}=0
    LOOP 2 TIMES
        GETPOSITION(FEQ,A[1],A[2],A[3],A[4])
        SELF.PropQAdd(PQ,PROP:At, PQ:Name, A[1] &','& A[2] &' x '& A[3] &','& A[4] & |
                                   '  BottomX,RightY=' & A[1]+A[3] &','& A[2]+A[4] & CHOOSE(~Feq{PROP:Full},'','  FULL') )    
        PQ:Name=CLIP(PQ:Name) & '-Pixels' ; PWnd{PROP:Pixels}=1
    END
    PQ:Name=' Client Pos XYWH' ; PWnd{PROP:Pixels}=0
    LOOP 2 TIMES
        A[1]=Feq{PROP:ClientX} ; A[2]=Feq{PROP:ClientY} ; A[3]=Feq{PROP:ClientWidth} ; A[4]=Feq{PROP:ClientHeight} 
        IF A[3]+A[4]=0 THEN BREAK.
        SELF.PropQAdd(PQ,PROP:ClientX, PQ:Name, A[1] &','& A[2] &' x '& A[3] &','& A[4] & |
                                        '  BottomX,RightY=' & A[1]+A[3] &','& A[2]+A[4] )            
        PQ:Name=CLIP(PQ:Name) & '-Pixels' ; PWnd{PROP:Pixels}=1
    END
    IF FeqTypeNo=CREATE:Sheet THEN
       PQ:Name=' TabRect LT x RB' ; PWnd{PROP:Pixels}=0
       LOOP 2 TIMES
           LOOP X=1 TO 4 ; A[X] = Feq{PROP:TabRect,X} ; END 
           SELF.PropQAdd(PQ,PROP:TabRect, PQ:Name, A[1] &','& A[2] &' x '& A[3] &','& A[4] & |
                                        '  Width,Height=' & A[3]-A[1] &','& A[4]-A[2] )     
           PQ:Name=CLIP(PQ:Name) & '-Pixels' ; PWnd{PROP:Pixels}=1
       END
    END !If Sheet    
    PWnd{PROP:Pixels}=0

    LOOP X=1 TO 255
         L = Feq{PROP:Alrt,X}  
         IF L THEN PQ:Value = ClaKeyCodeExplain(L) ; SELF.PropQAdd(PQ,PROP:Alrt,'Alrt,' & X,PQ:Value).
    END
    L = Feq{PROP:Key} ; IF L THEN SELF.PropQAdd(PQ,PROP:Key,'KEY',ClaKeyCodeExplain(L) ).
    LOOP X=1 TO 16 ;  SELF.PropQAdd(PQ,PROP:DropID,'DropID,' & X, Feq{PROP:DropID,X} ) ; END    
    LOOP X=1 TO 16 ;  SELF.PropQAdd(PQ,PROP:DragID,'DragID,' & X, Feq{PROP:DragID,X} ) ; END
    LOOP X=1 TO 255
         L = Feq{PROP:Child,X}       !TODO show HEX
         IF ~L THEN BREAK.
         !IF X>5 AND Feq=0 THEN BREAK.  !HACK Feq=0 just a handful of Window Children
         TN=L{PROP:Type}
         Val=L &' '& ClaControlTypeName(TN)  &'  '& SELF.GetFEQName(L)
         CASE TN !Sheet(String) is Like Option that TAB('Value') assigned
         OF CREATE:Radio OROF Create:TAB ; Val=CLIP(Val)&'  Value(''' & L{PROP:Value} &''')'
         END 
         SELF.PropQAdd(PQ,PROP:Child,'Child,' & X,Val)
    END
    IF FeqTypeNo=CREATE:List OR FeqTypeNo=CREATE:Combo ! Feq{PROP:Format} THEN 
        Val='{{PROPLIST:Exists,0} = ' & Feq{PROPLIST:Exists,0} & '  (Column Count)'
        SELF.PropQAdd(PQ,PROPLIST:Exists,'Columns Format',Val) ; SELF.PropQAdd(PQ,PROPLIST:Exists,'Format Columns',Val) 
        LOOP X=1 TO 255
             PQ:Value=Feq{PROP:IconList,X} ; IF ~PQ:Value THEN CYCLE. ; ClaIconEquate(PQ:Value)
             SELF.PropQAdd(PQ,PROP:IconList,'IconList,'& FORMAT(X,@n3),PQ:Value,,1) 
        END 
    END    
    DO PropHuntRtn  !Hunt thru a list of many properties
    SELF.Win32PropsAdd(PQ, FEQ{PROP:Handle}, FEQ, FeqTypeNo)     !Add Windows API properties
    EXIT
!-----------------
PropHuntRtn ROUTINE
    DATA
P7Q Parse7QType          
TB  STRING('<9>') !Tab shows in list
  CODE
  PropHuntLoadP7Q(P7Q,FeqTypeNo)
  LOOP X=1 TO RECORDS(P7Q) ; GET(P7Q, X)    
    PE=P7Q:EqtLong  
    Val=Feq{PE}
    IF ~Val THEN 
       CASE PE !TODO Better way to have MAP for Type/PRP to always show
       OF PROP:Checked ; IF FeqTypeNo=CREATE:Check OR FeqTypeNo=CREATE:Radio THEN Val=TB.
       END
       IF ~Val THEN CYCLE.
    END 
    IF Val='0' THEN
       CASE PE
       OF PROP:Std OROF PROP:UseAddress 
       !not sure OROF PROP:TrueValue OROF PROP:FalseValue    !PROP:Value=PROP:TrueValue
          CYCLE
       ELSE
            !Keep all '0'  CYCLE !Maybe some Props let '0' show ??? It lets me know the prop exists
       END   
    END !IF 0
    SELF.PropDescribe(PE,Val,0)
    SELF.PropQAdd(PQ,PE,P7Q:Name,Val)
  END !Loop
  EXIT 
!------------------ 
ScanRtn     ROUTINE !Scan for undocumented props 7900h-7DFFh 
  X=RECORDS(PQ)
  SORT(PQ, PQ:EqtHex) 
  SETTARGET(SELF.WndRef)
  LOOP PE = 7900h to 7E00h - 1   !7E00h=PropList
    CASE PE
    OF   PROP:ClientY OROF PROP:ClientWidth OROF PROP:ClientHeight ! OROF PROP:ClientX 
    OROF PROP:FEQ OROF PROP:Target OROF PROP:Selected OROF PROP:OldTreeColor
    OROF PROP:NextTabStop OROF PROP:PrevTabStop OROF PROP:IconList OROF PROP:NextField
        CYCLE 
    OF PROPLIST:MouseDownRow TO PROPLIST:MouseUpZone ; CYCLE
    END
    Val=Feq{PE}
    IF ~Val THEN CYCLE.    
    IF Val='0' THEN CYCLE.
    PQ:EqtHex = Hex8(PE,-4) 
    GET(PQ, PQ:EqtHex) ; IF ~ERRORCODE() THEN CYCLE.  !Already Got it
    PQ:Name = '{{' & PQ:EqtHex & '} Scan'
    SELF.PropQAdd(PQ,PE,PQ:Name,Val)
  END 
  SETTARGET()
  DISPLAY ; Message(RECORDS(PQ)-X & ' Added as {{####h} Scan')
  EXIT
!=====================================
CBWndPreviewClass.PROPsEditor  PROCEDURE(LONG FEQ, LONG FeqTypeNo, STRING FeqTypeName, STRING FeqName) 
PQ QUEUE(PropQType),PRE(PQ)    
Index BYTE !PQ:Index
   END
Last_EqtLong LONG,STATIC   
Subset BYTE,STATIC   
X  LONG,AUTO
PE LONG,AUTO
L  LONG,AUTO
Val STRING(255),AUTO 
VLg LONG,AUTO
FindCls CBLocateCls
FindTxt STRING(64),STATIC 
Window WINDOW('Edit PROPs'),AT(,,247,250),GRAY,SYSTEM,FONT('Segoe UI',9),RESIZE
        BUTTON('<50>'),AT(2,2,12,12),USE(?UnderBtn),SKIP,FONT('Webdings'),TIP('Move Preview under this Window'),FLAT
        BUTTON('Cl&ose'),AT(17,2,27,12),USE(?CloseBtn),SKIP,STD(STD:Close)
        LIST,AT(54,3,60,10),USE(Subset),SKIP,DROP(9),FROM('Normal PROP|#0|ALL PROP|#1|Alert Keys|#2|Drag & Drop|#3|Icon ' & |
                'List|#9')
        BUTTON('&Copy'),AT(216,2,27,12),USE(?CopyBtn),SKIP
        BUTTON('?'),AT(153,2,12,12),USE(?HelpBtn),KEY(F2Key),SKIP,FONT(,,,FONT:bold),TIP('Clarion Help - F2')
        BUTTON('3'),AT(182,2,12,12),USE(?TrashBtn),SKIP,FONT('WingDings 2',14),TIP('Delete Clutter<13,10>Ctrl+Click to l' & |
                'eave only Clutter')
        ENTRY(@s64),AT(31,19,182,10),USE(FindTxt),SKIP,FONT('Consolas')
        BUTTON('&Find'),AT(2,18,25,11),USE(?FindNext),SKIP
        BUTTON('Pre&v'),AT(216,18,26,11),USE(?FindPrev),SKIP
        LIST,AT(0,33),FULL,USE(?LIST:PQ),VSCROLL,FONT('Consolas',10),FROM(PQ),FORMAT('27L(3)|FM~Equate~L(1)@s5@80L(2)|FM' & |
                '~Property~@s32@?122L(2)F~Value (Double Click to Edit)~@s255@'),ALRT(DeleteKey), ALRT(EnterKey)
    END
SysMenuCls SysMenuClass
SortCls    CBSortClass 
AtPropEdit LONG,DIM(4),STATIC
    CODE
    PWnd &= SELF.WndRef
    DO LoadPQRtn
    OPEN(Window) ; SysMenuCls.Init(Window) ;SELF.AtSetOrSave(1, AtPropEdit[])
    0{PROP:Text} = 'Edit PROP: for ' & CLIP(FeqTypeName)  &'  Feq: '& FEQ &'  '& FeqName
    MakeOverList(?List:PQ)
    SortCls.Init(PQ,?LIST:PQ,2)
    FindCls.Init(PQ, ?LIST:PQ, ?FindTxt, ?FindNext, ?FindPrev)
    DO SyncLastRtn
    ACCEPT 
        CASE ACCEPTED()
        OF ?UnderBtn ; SysMenuCls_SYSCOMMAND(0{PROP:Handle},SMCmd_MoveUnder)   
        OF ?CopyBtn  ; SELF.PropQCopy2Clip(PQ)
        OF ?HelpBtn  ; GET(PQ,CHOICE(?LIST:PQ)) ; HelpCW(PQ:Name,1)
        OF ?TrashBtn ; PropTrashDelete(PQ)
        OF ?Subset   ; DO LoadPQRtn ; DO SyncLastRtn
        END
        IF FIELD()=?LIST:PQ THEN 
           GET(PQ,CHOICE(?LIST:PQ))
           CASE EVENT()
           OF EVENT:NewSelection OROF EVENT:AlertKey
              Last_EqtLong = PQ:EqtLong
              CASE KEYCODE()
              OF DeleteKey ; DELETE(PQ)
              OF MouseLeft2 OROF EnterKey 
                 PropEditWindow(FEQ, FeqName, PQ, CHOOSE(~PQ:Index,PWnd$Feq{PQ:EqtLong},PWnd$Feq{PQ:EqtLong,PQ:Index}), PQ:Index) 
                 IF PQ:Index THEN DO LoadPQRtn ELSE PQ:Value=PWnd$Feq{PQ:EqtLong} ; PUT(PQ).
              END
           OF EVENT:HeaderPressed ; SortCls.HeaderPressed()              
           END !Case Event
        END !IF List      
    END !Accept
    SELF.AtSetOrSave(2,AtPropEdit[])        
    CLOSE(Window)
    RETURN 
SyncLastRtn ROUTINE
    IF ~Last_EqtLong THEN EXIT.
    PQ:EqtLong = Last_EqtLong
    GET(PQ,PQ:EqtLong) 
    SELECT(?LIST:PQ,CHOOSE(~ERRORCODE(),POINTER(PQ),1))  
LoadPQRtn ROUTINE
    SETTARGET(PWnd) 
    FREE(PQ) ; CLEAR(PQ) ; L=0
    CASE SubSet
    OF 0 OROF 1 ; DO PropHuntRtn
    OF 2 ; LOOP X=1 TO 255 ; PQ:Index=X
                VLg = Feq{PROP:Alrt,X} ; IF ~VLg THEN L+=1 ; IF L>1 AND X<255 THEN CYCLE. ; END
                PQ:Value = CHOOSE(VLg=0,'<9>',ClaKeyCodeExplain(VLg))
                SELF.PropQAdd(PQ,PROP:Alrt,'Alrt,'& FORMAT(X,@n3),PQ:Value)
           END
    OF 3 ; LOOP X=1 TO 16 ; PQ:Index=X  
                SELF.PropQAdd(PQ,PROP:DragID,'DragID,'& FORMAT(X,@n3), Feq{PROP:DragID,X},,1)
                SELF.PropQAdd(PQ,PROP:DropID,'DropID,'& FORMAT(X,@n3), Feq{PROP:DropID,X},,1)
           END 
    OF 9 ; LOOP X=1 TO 255 ; CLEAR(PQ) ; PQ:Index=X
                PQ:Value=Feq{PROP:IconList,X} 
                IF ~PQ:Value THEN L+=1 ; IF L>9 THEN CYCLE. ; END
                SELF.PropQAdd(PQ,PROP:IconList,'IconList,'& FORMAT(X,@n3),PQ:Value,,1)
           END 
    END
    SETTARGET()
    SORT(PQ,PQ:Name) !; SELF.QueueReflection(PQ,'PQ',1)
    EXIT
PropHuntRtn ROUTINE
    DATA        !CPI  ChildIndex Enabled Follows Handle NextTabStop Parent Precedes PrevTabStop RejectCode Type USE UseAddress Visible WNDProc-STD-ScreenText
SkipPROP STRING('7C5Bh7A18h7CA8h7C99h7C96h7A56h7A07h7A7Eh7A57h7A1Bh7C01h7A10h7A52h7CA7h7CB4h7C82h7CBAh') !11.1 7C15 fixed below
                 !-INS-UPR-CAP-IMM-OVR-Password-REQ-ReadOnly       
KeepENTRY STRING('7C57h7C89h7C5Ah7C6Ah7C58h7C7Ah7C7Ch7C7Bh')
EquateH5  STRING(5)  !Equate ####h
P7Q Parse7QType          
TB  STRING('<9>') !Tab shows in list
  CODE
                    COMPILE('!**END 11.1**', _C111_)
  SkipPROP[1:5]='7C15h'    !7C5Bh in C11.0
                             !**END 11.1**  
  PropHuntLoadP7Q(P7Q,FeqTypeNo)
  LOOP X=1 TO RECORDS(P7Q) ; GET(P7Q, X)
    EquateH5=P7Q:EqtHex
    IF INSTRING(EquateH5,SkipPROP) THEN CYCLE.
    PE=P7Q:EqtLong  
    Val=Feq{PE}
    IF ~Val THEN
       CASE PE !TODO Better way to have MAP for Type/PRP to always show
       OF PROP:Checked ; IF FeqTypeNo=CREATE:Check OR FeqTypeNo=CREATE:Radio THEN Val=TB.
       END
       CASE FeqTypeNo
       OF   CREATE:Entry  
       OROF CREATE:Combo
       OROF CREATE:Spin ; IF INSTRING(EquateH5,KeepENTRY) THEN Val=TB. !Pwd is extra
       END
       IF ~Val AND SubSet=0 THEN CYCLE.
       Val=TB  !Tab lets value show
    END 
    SELF.PropDescribe(PE,Val,0)
    SELF.PropQAdd(PQ,PE,P7Q:Name,Val)
  END !Loop
  L = Feq{PROP:Key} ; IF L THEN SELF.PropQAdd(PQ,PROP:Key,'KEY',ClaKeyCodeExplain(L) ).
  EXIT             
!======================================
PropEditWindow PROCEDURE(LONG FEQ, STRING FeqName, PropQType PQ, STRING RawValue, BYTE PropIndex)
Txt  STRING(1024),AUTO
Undo STRING(1024),AUTO
Quoted BYTE
AutoClose BYTE,STATIC
EditWn WINDOW('EDIT Prop'),AT(,,231,56),GRAY,SYSTEM,FONT('Segoe UI',9),RESIZE
        STRING('Value'),AT(2,23,11,18),USE(?Value:Pmt),RIGHT,ANGLE(900)
        TEXT,AT(19,22,,18),FULL,USE(Txt),FONT('Consolas',12),SINGLE
        BUTTON('&Set Value'),AT(18,3,37),USE(?SetBtn),DEFAULT,TIP('Set Control PROP to below value')
        BUTTON('= &1'),AT(60,3,22),USE(?Set1Btn),KEY(AltI),TIP('Alt + Eye')
        BUTTON('= &0'),AT(86,3,22),USE(?Set0Btn),KEY(AltO),TIP('Alt + Oh')
        BUTTON('=" "'),AT(112,3,22),USE(?SpaceBtn),KEY(AltSpace),TIP('Alt + Space')
        BUTTON('&Undo'),AT(142,3,30),USE(?UndoBtn)
        BUTTON('&Close'),AT(180,3,30),USE(?CloseBtn),STD(STD:Close)
        CHECK('Quoted'),AT(18,43),USE(Quoted),SKIP,TIP('Text is Clarion Literal String with << HEX >')
        CHECK('Auto Close'),AT(162,43,47),USE(AutoClose),SKIP,LEFT,TIP('Close on Set Value')
    END
SysMenuCls SysMenuClass 
StSELF &CBWndPreviewClass  
P LONG,DIM(4),STATIC 
X LONG,AUTO
PROP:: LONG,AUTO
    CODE
    PROP::=PQ.EqtLong
    OPEN(EditWn) 
    0{PROP:Text}='Edit PROP '& PQ.EqtHex &' '& CLIP(PQ.Name) &' - FEQ '& FEQ &' '& FeqName
    StSELF.AtSetOrSave(1,P[]) ; SysMenuCls.Init(EditWn) ; 0{PROP:MinHeight}=0{PROP:Height} ; 0{PROP:MaxHeight}=0{PROP:Height} 
    Txt=RawValue
    LOOP X=1 TO SIZE(Txt) !Quote if Low ASCII
        IF VAL(Txt[X])>31 THEN CYCLE.
        Txt=QUOTE(CLIP(RawValue)) ; Quoted=1 ; ?Quoted{PROP:Text}='Quoted <<Hex>'; DISABLE(?Quoted) ; BREAK
    END
    Undo=Txt    
    ACCEPT  
        CASE ACCEPTED()
        OF ?SetBtn  ; DO SetRtn
        OF ?Set0Btn ; Txt='0' ; DO SetRtn 
        OF ?Set1Btn ; Txt='1' ; DO SetRtn 
        OF ?SpaceBtn; Txt=''  ; DO SetRtn 
        OF ?Quoted  ; Txt=QUOTE(CLIP(Txt)) ; Undo=QUOTE(CLIP(RawValue)) ; DISABLE(Quoted) 
        OF ?UndoBtn ; IF ~PropIndex THEN PWnd$FEQ{PROP::}=RawValue ELSE PWnd$FEQ{PROP::,PropIndex}=RawValue.
                      Txt=Undo ; DISPLAY
        END
    END
    StSELF.AtSetOrSave(2,P[])
    CLOSE(EditWn) 
    RETURN 
SetRtn ROUTINE
    IF ~PropIndex THEN 
           PWnd$FEQ{PROP::}=CHOOSE(~Quoted,CLIP(Txt),UNQUOTE(Txt)) 
       Txt=PWnd$FEQ{PROP::}
    ELSE
           PWnd$FEQ{PROP::,PropIndex}=CHOOSE(~Quoted,CLIP(Txt),UNQUOTE(Txt)) 
       Txt=PWnd$FEQ{PROP::,PropIndex}
    END    
    IF Quoted THEN Txt=QUOTE(CLIP(Txt)).
    IF AutoClose THEN POST(EVENT:CloseWindow) ELSE SELECT(?Txt).
    DISPLAY ; EXIT
!=====================================
PropTrashDelete PROCEDURE(PropQType PQ)
X USHORT,AUTO
IsBad USHORT,AUTO
IsCtrlDown BYTE,AUTO
                    OMIT('!**END 11.0**', _C111_)
Bad_Prop STRING('7C96h 7CB4h 7CFAh 7C5Dh 7CA8h 7CFAh 7C12h 7C10h 7C11h 7C13h 7C57h 7C58h 7A1Bh 7CFBh 7CFCh 7A10h 7A52h 7CA7h 7A56h 7A57h Win32 ' & | !Enabled Handle Visible WNDProc 
                '7C5Bh 7A3Ah 7C20h 7C21h 7C22h 7C23h 7C24h 7C25h 7C26h 7C27h 7C28h 7C29h ') !11.0 CPI FontCharSet ListProp Mouse
                          !**END 11.0**'
                    COMPILE('!**END 11.1**', _C111_)  !1st line is same
Bad_Prop STRING('7C96h 7CB4h 7CFAh 7C5Dh 7CA8h 7CFAh 7C12h 7C10h 7C11h 7C13h 7C57h 7C58h 7A1Bh 7CFBh 7CFCh 7A10h 7A52h 7CA7h 7A56h 7A57h Win32 ' & | !Enabled Handle Visible WNDProc 
                '7C15h 7C14h 7ED0h 7ED1h 7ED2h 7ED3h 7ED4h 7ED5h 7ED6h 7ED7h 7ED8h 7ED9h ') !11.1 CPI FontCharSet
                             !**END 11.1**
Bad_Zero STRING('7CE9h 7CE5h 7CE4h 7CE6h ') ! Angle Bevel
    CODE 
    IsCtrlDown=KeyStateSCA(2)     
    LOOP X=RECORDS(PQ) TO 1 BY -1 ; GET(PQ,X)
        IsBad=INSTRING(CLIP(PQ:EqtHex),Bad_Zero,1) !Case
        IF IsBad THEN  !Zero is Bad
           IF PQ:Value<>'0' AND PQ:Value<>'' THEN IsBad=0.  !Keeper
        ELSE
           IsBad=INSTRING(CLIP(PQ:EqtHex),Bad_Prop,1) !Case
        END          
        IF   ~IsCtrlDown AND  IsBad THEN DELETE(PQ) 
        ELSIF IsCtrlDown AND ~IsBad THEN DELETE(PQ).
    END 
    DISPLAY ; RETURN
!======================================
CBWndPreviewClass.PropDescribe PROCEDURE(LONG PE,*STRING Val, BYTE Big2Hex=0)!,BOOL,PROC
ValLng LONG,AUTO
L   LONG,AUTO
    CODE
    CASE PE
    OF PROP:Icon   ; ClaIconEquate(Val)   ; RETURN 1
    OF PROP:Cursor ; ClaCursorEquate(Val) ; RETURN 1
    END    
    L = LenFastClip(Val)
    IF L>16 OR ~NUMERIC(Val[1:16]) THEN RETURN 0.
    L=0+Val[1:16] ; ValLng=L    
    CASE PE  
    OF   PROP:FontColor          !7C12h
    OROF PROP:Fill               !7C61h
    OROF PROP:OldTreeColor       !7CB8h
    OROF PROP:Color              !7CFAh
    OROF PROP:FillColor          !7CFAh
    OROF PROP:SelectedColor      !7CFBh
    OROF PROP:SelectedFillColor  !7CFCh
    OROF PROP:GradientFromColor  !PROP:Fill
    OROF PROP:GradientToColor    !7905h 
    OROF PROPLIST:Grid           !7C29h
    OROF PROPLIST:DefHdrTextColor  !7C2Ah
    OROF PROPLIST:DefHdrBackColor  !7C2Bh
    OROF PROPLIST:HdrSortTextColor !7C2Ch
    OROF PROPLIST:HdrSortBackColor !7C2Dh
    OROF PROPLIST:SortTextColor    !7C2Eh
    OROF PROPLIST:SortBackColor    !7C2Fh        
         IF ValLng=-1 THEN Val='-1 Color:None' ELSE ClaColorEquate(Val).
    OF PROP:Follows OROF PROP:Precedes_C10 orof PROP:Parent OROF PROP:ChoiceFEQ OROF PROP:ToolBar OROF PROP:MenuBar 
                    OROF PROP:NextTabStop OROF PROP:PrevTabStop
       IF ValLng THEN
          Val=CLIP(Val) &' '& ClaControlTypeName(ValLng{PROP:Type}) &'  '& SELF.GetFEQName(ValLng)
       END
    OF PROP:Std ; Val=ValLng &'  '& ClaSTDprop(ValLng)
    OF PROP:Xpos OROF PROP:Ypos OROF PROP:Width OROF PROP:Height ; RETURN 0 !No Hex
    ELSE
       IF ~Big2Hex OR (L>=-256 AND L<=256) THEN RETURN 0.
       Val=CLIP(Val) &' = '& Hex8(L, -4)        
    END  !Case
    RETURN True
!=====================================
CBWndPreviewClass.PropPickList PROCEDURE(*STRING OutHexProp,<Parse7QType InP7Q>)!,BOOL
RetBool BOOL        
P7Q &Parse7QType
LdP7Q Parse7QType
PQ  PropQType        
X   LONG,AUTO
FindCls CBLocateCls
FindTxt STRING(64) 
Window WINDOW('Properties'),AT(,,200,290),GRAY,SYSTEM,FONT('Segoe UI',9),RESIZE
        BUTTON('&Pick'),AT(2,2,19,12),USE(?SelectBtn),SKIP
        ENTRY(@s64),AT(47,3,99,11),USE(FindTxt),SKIP,FONT('Consolas')
        BUTTON('&Find'),AT(25,2,19,12),USE(?FindNext),SKIP
        BUTTON('Pre&v'),AT(149,2,18,12),USE(?FindPrev),SKIP
        BUTTON('?'),AT(169,2,12,12),USE(?HelpBtn),SKIP,FONT(,,,FONT:bold),TIP('Clarion Help - F2'),FLAT,KEY(F2Key)
        BUTTON,AT(184,2,12,12),USE(?Copy),SKIP,ICON(ICON:Copy),FLAT
        LIST,AT(0,18),FULL,USE(?LIST:PQ),VSCROLL,FONT('Consolas',10),FROM(PQ),FORMAT('30L(3)|FM~Equa' & |
                'te~C(0)@s5@80L(2)F~Property~@s32@?'),ALRT(DeleteKey)
    END
SysMenuCls SysMenuClass
SortCls CBSortClass    
  CODE
  OPEN(Window) ; SysMenuCls.Init(Window)
  MakeOverList(?List:PQ) ; SELF.AtSetOrSave(1, AtPickProp[]) ; DISPLAY 
  IF ~OMITTED(InP7Q) THEN P7Q &= InP7Q ELSE P7Q &= LdP7Q ; PropHuntLoadP7Q(P7Q,-1).
  CLEAR(PQ) ; LOOP X=1 TO RECORDS(P7Q) ; GET(P7Q, X) ; PQ:EqtHex=P7Q.EqtHex ; PQ:Name=P7Q.Name ; ADD(PQ,PQ:Name) ; END
  FindCls.Init(PQ,?LIST:PQ, ?FindTxt, ?FindNext, ?FindPrev) ; SortCls.Init(PQ,?LIST:PQ)
  ACCEPT
    IF EVENT()=EVENT:OpenWindow THEN SELECT(?LIST:PQ,1).
    CASE ACCEPTED()
    OF ?SelectBtn ; RetBool=1 ; BREAK
    OF ?HelpBtn ; GET(PQ,CHOICE(?LIST:PQ)) ; HelpCW(PQ:Name,1)
    OF ?Copy ; SetClip2Queue(PQ,1,'Equate<9>Property',,1,2)
    END
    IF FIELD()=?LIST:PQ THEN 
       GET(PQ,CHOICE(?LIST:PQ))
       CASE EVENT()
       OF EVENT:AlertKey ; IF KEYCODE()=DeleteKey THEN DELETE(PQ).
       OF EVENT:NewSelection ; IF KeyCode()=MouseLeft2 THEN RetBool=1 ; BREAK.
       OF EVENT:HeaderPressed ; SortCls.HeaderPressed()    
       END
    END
  END !Accept
  IF RetBool=1 THEN GET(PQ,CHOICE(?LIST:PQ)) ; OutHexProp=PQ:EqtHex.
  SELF.AtSetOrSave(2, AtPickProp[])
  CLOSE(Window)
  DISPLAY
  RETURN RetBool
!----------------------------------------
CBWndPreviewClass.SystemPROPs PROCEDURE()
PQ PropQType        
X  LONG,AUTO
PE LONG,AUTO
L  LONG,AUTO
Val STRING(255),AUTO 
ValLng LONG,AUTO
SysOrPrt BYTE(1) 
AllProp BYTE
FindCls CBLocateCls
FindTxt STRING(64),STATIC 
Window WINDOW('SYSTEM / PRINTER Properties'),AT(,,250,250),GRAY,SYSTEM,FONT('Segoe UI',9),RESIZE
        OPTION,AT(0,0,73,15),USE(SysOrPrt)
            RADIO('&System'),AT(2,4),USE(?SystemRad)
            RADIO('&Printer'),AT(40,4,32),USE(?PrintRad)
        END
        BUTTON,AT(74,2,17,14),USE(?PrinterBtn),SKIP,ICON(ICON:Print1),TIP('Change Printer'),FLAT
        CHECK('ALL'),AT(98,4,24),USE(AllProp),SKIP,TIP('Show All Properties (blank values)')
        BUTTON('Sys &Met'),AT(124,2,31,14),USE(?SysMetBtn),SKIP,TIP('Windows System Metrics')
        BUTTON('&Copy'),AT(221,18,25,11),USE(?CopyBtn),SKIP,TIP('Copy Prop list to Clipboard')
        BUTTON('Fo&nt'),AT(162,2,25,14),USE(?FontBtn),SKIP,TIP('Change System, Tip, or Status Font')
        BUTTON('?'),AT(199,3,12,12),USE(?HelpBtn),KEY(F2Key),SKIP,FONT(,,,FONT:bold),TIP('Clarion He' & |
                'lp - F2')
        BUTTON('Scan'),AT(221,2,25,14),USE(?ScanBtn),SKIP,TIP('Scan for undocumented props 7900h-7DFF?')
        ENTRY(@s64),AT(32,19,155,10),USE(FindTxt),SKIP,FONT('Consolas')
        BUTTON('&Find'),AT(2,18,25,11),USE(?FindNext),SKIP
        BUTTON('Pre&v'),AT(191,18,26,11),USE(?FindPrev),SKIP
        LIST,AT(0,33),FULL,USE(?LIST:PQ),VSCROLL,FONT('Consolas',10),FROM(PQ),FORMAT('27L(3)|FM~Equa' & |
                'te~L(1)@s5@80L(2)|FM~Property~@s32@?20L(2)F~Value~@s255@'),ALRT(DeleteKey)
    END
SysMenuCls SysMenuClass
SortCls CBSortClass    
  CODE
  OPEN(Window) ; SysMenuCls.Init(Window) ; SELF.AtSetOrSave(1, AtCtrlProps[]) 
  MakeOverList(?List:PQ)
  FindCls.Init(PQ, ?LIST:PQ, ?FindTxt, ?FindNext, ?FindPrev)
  SortCls.Init(PQ,?LIST:PQ)
  POST(EVENT:Accepted,?SysOrPrt)
  ACCEPT 
    CASE ACCEPTED() 
    OF ?SysOrPrt  
        DISABLE(?SysOrPrt) ; FREE(PQ) ; DISPLAY
        IF SysOrPrt=1 THEN DO AddSystemRtn ELSE DO AddPrinterRtn . ; SORT(PQ,PQ:Name) ; SETTARGET()
        ENABLE(?SysOrPrt) ; ENABLE(?ScanBtn) ; DISPLAY
        SortCls.SetSortCol(2)
    OF ?AllProp ; POST(EVENT:Accepted,?SysOrPrt)            
    OF ?PrinterBtn ; IF PRINTERDIALOG() THEN SysOrPrt=2 ; POST(EVENT:Accepted,?SysOrPrt).
    OF ?CopyBtn ; SELF.PropQCopy2Clip(PQ)
    OF ?ScanBtn ; DISABLE(?) ; DO ScanRtn 
    OF ?SysMetBtn ; HIDE(0) ; SELF.SystemMetrics() ; UNHIDE(0)
    OF ?FontBtn ; DO FontRtn
    OF ?HelpBtn ; GET(PQ,CHOICE(?LIST:PQ)) ; HelpCW(PQ:Name)
    END
    IF FIELD()=?LIST:PQ THEN 
       GET(PQ,CHOICE(?LIST:PQ))
       CASE EVENT()
       OF EVENT:AlertKey ; IF KEYCODE()=DeleteKey THEN DELETE(PQ).
       OF EVENT:NewSelection
          IF KeyCode()=MouseLeft2 THEN
             PropViewWindow('View SYSTEM '& PQ:Name, PQ, |
                            CHOOSE(PQ:EqtLong <1, PQ:Value, CHOOSE(SysOrPrt=1,SYSTEM{PQ:EqtLong},PRINTER{PQ:EqtLong})) )
          END
       OF EVENT:HeaderPressed ; SortCls.HeaderPressed()    
       END
    END
  END !Accept
  SELF.AtSetOrSave(2, AtCtrlProps[])
  CLOSE(Window)
  RETURN
!------------------------------------   
AddPrinterRtn ROUTINE 
    DATA 
CP STRING('7B00Devmode 7B01Paper 7B02PaperHeight 7B03PaperWidth 7B04Percent 7B05Copies 7B06PaperBin ' &|
  '7B07Resolution 7B08Color 7B09Duplex 7B0AYResolution 7B0BFontmode 7B20PrintToFile ' &|
  '7B21PrintToName 7B22FromPage 7B23ToPage 7B24FromMin 7B25ToMax 7B26Collate 7B27Driver ' &|
  '7B28Device 7B29Port 7B2AContext 7B2BGenerateAll 7B2CExtend 7B2DSupportCopies 7B2ESupportCollate ')
P7Q Parse7QType     
  CODE     
  EquateXStringParse(P7Q,4,CP)
  LOOP X=1 TO RECORDS(P7Q) ; GET(P7Q, X)
    PE=P7Q:EqtLong
    Val=PRINTER{PE} ; IF ~Val AND AllProp THEN Val='<9>'. ; IF ~Val THEN CYCLE.
    LOOP L=1 TO SIZE(Val) ; IF VAL(Val[L])<32 THEN Val[L]='?'. ; END !DevMode
    ValLng=Val ; SELF.PropQAdd(PQ,PE,P7Q:Name,Val)
  END 
!------------------------------------
AddSystemRtn ROUTINE 
    DATA
CP STRING('7CBEAppInstance 7A37AssertHook 7A3CAssertHook2 7A05AutoPaper 7A39CharSet 7A65CloseReportHook ' &|
  '7A63CloseWindowHook 7A30ColorDialogHook 7A70ColorHighByte 7A76Locale 7A77Codepage 7A59DataPath ' &| ! 7D0ECustomColor
  '7A0ADDEmode 7CC1DDETimeOut 7CBDDeferMove 7358DriverTracing 7A38FatalErrorHook ' &| ! 7CC5ExeVersion
  '7A61FileDialogAHook 7A31FileDialogHook 7A32FontDialogHook 7A4BGlobalHelp 7A34HaltHook ' &|
  '7C68Icon 7A4AImageInstance 7A3DInitAStringHook 7A66LastChanceHook 7CBCLazyDisplay ' &|
  '7CA5LFNSupport 7A3FLockThreadHook 7A68LowResourcesHook 7A5FMenuStyle ' &|          ! 7CC4LibVersion
  '7A35MessageHook 7C72Modal 7A7DMsgModeDefault 7CB1NoTips 7A64OpenReportHook 7A62OpenWindowHook 7A67PrinterDialogAHook ' &|
  '7A33PrinterDialogHook 7A1APrintMode 7A58PropVScroll 7A36StopHook 7A3BSystemPropHook ' &|
  '7A45Target 7A21TempImagePath 7A20TempPagePath 7A5EThemeActive 7C95Thread 7A0BThreading ' &|
  '7A60ThreadLockedHook 7CB2TipDelay 7CB3TipDisplay 7356TraceFile 7357TraceKey 7A3EUnlockThreadHook ' &|
  '7C70MDI 7C97Active 7CB8OldTreeColor 7A7ATimeZeroIsLongOne ')
  !FontColor7C12 FontName7C10 FontSize7C11 FontStyle7C13 Font7C10 PROP:FontCharSet
  ! HelpEngine7D40 LibHook7A30 TempPath7A20 
FntCat PSTRING(9)    
FP  LONG
P7Q Parse7QType
  CODE     
  EquateXStringParse(P7Q,4,CP)
  LOOP X=1 TO RECORDS(P7Q) ; GET(P7Q, X)
      PE=P7Q:EqtLong
      Val=SYSTEM{PE} ; IF ~Val AND AllProp THEN Val='<9>'. ; IF ~Val THEN CYCLE.
      SELF.PropQAdd(PQ,PE,P7Q:Name,Val)
  END !Loop

  LOOP L=1 TO 2
    PE=CHOOSE(L,PROP:ExeVersion,PROP:LibVersion)
    LOOP X=1 TO 3
       P7Q:Name=CHOOSE(L,'ExeVersion','LibVersion')&','&X
       Val = SYSTEM{PE,X}  !IF ~Val THEN CYCLE.

       Val=CLIP(Val) &' - '& CHOOSE(X,'Internal Version','Production Version','Build Number') |
                     &' - '& CHOOSE(L,'RTL Linked in Build','RTL DLL Loaded by EXE') 
       SELF.PropQAdd(PQ,PE,P7Q:Name,Val)
    END   
  END 
  LOOP X=1 TO 3
     FntCat=CHOOSE(X,'','Tips ','Status ')
     FP=CHOOSE(X,0, PROP:TipsFont ,PROP:StatusFont )   !' StatusFont0014 (11.1 0016) TipsFont0010'
     PE = FP + PROP:FontName    ; SELF.PropQAdd(PQ, PE, FntCat & 'FontName', SYSTEM{PE})
     PE = FP + PROP:FontSize    ; SELF.PropQAdd(PQ, PE, FntCat & 'FontSize', SYSTEM{PE})
     PE = FP + PROP:FontStyle   ; SELF.PropQAdd(PQ, PE, FntCat & 'FontStyle', SYSTEM{PE})
     PE = FP + PROP:FontCharSet ; SELF.PropQAdd(PQ, PE, FntCat & 'FontCharSet', SYSTEM{PE}) 
     PE = FP + PROP:FontColor   ; Val=SYSTEM{PE} ; ClaColorEquate(Val)    
                                  SELF.PropQAdd(PQ, PE, FntCat & 'FontColor', Val)
  END     
  LOOP X=1 TO 15
     Val = SYSTEM{PROP:WindowsVersion,X}       !TODO show HEX
     IF X>10 AND ~Val THEN CYCLE.
     L=LEN(CLIP(Val)) ; IF L<12 THEN L=12.
     Val=SUB(Val,1,L) &'   '& |         
                      CHOOSE(X,'RTL Description','Win Version Name','Major','Minor','Build',|
                      'Service Pack','64 bit="1"','Server="1"','WINVERSION: Equate','WINEDITION: Equate','') 
     SELF.PropQAdd(PQ,PROP:WindowsVersion,'WindowsVersion,' & FORMAT(X,@n2),Val)
  END 
  PE=PROP:HelpEngine ; LOOP X=1 TO 16 ; L=SYSTEM{PE,X} ; IF L OR X=1 THEN SELF.PropQAdd(PQ,PE,'HelpEngine,' & X, L ). ; END
  PE=PROP:CustomColor ; LOOP X=1 TO 16 ; L=SYSTEM{PE,X} 
                        IF X=1 OR (L<>-1 AND L<>0FFFFFFh) THEN SELF.PropQAdd(PQ,PE,'CustomColor,' & X, ClaColorEquate(L) ). ; END
  EXIT
!--------------    
FontRtn ROUTINE
    DATA
FFace   STRING(32)
FSize   LONG
FStyle  LONG
FCHar   LONG
FColor  LONG
FP      LONG
FntCat  PSTRING(9)
    CODE    
    X=POPUPunder(?FontBtn,'System Font (Messages)|Tips Font|Status Font') ; IF ~X THEN EXIT.
    FP=CHOOSE(X,0, PROP:TipsFont ,PROP:StatusFont)
    FntCat=CHOOSE(X,'System','Tips','Status') 
    FFace =SYSTEM{(FP+PROP:FontName)}   ; FSize =SYSTEM{(FP+PROP:FontSize)}
    FStyle=SYSTEM{(FP+PROP:FontStyle)}  ; FCHar =SYSTEM{(FP+PROP:FontCharSet)}
    FColor=SYSTEM{(FP+PROP:FontColor)}
    IF X=1 THEN Message('Message Font Before ' & CLIP(FFace)&' '& FSize).
    IF ~FONTDIALOGa('Select '& FntCat &' Font',FFace,FSize,FColor,FStyle,FChar) THEN EXIT.
    SYSTEM{(FP+PROP:FontName   )}=FFace  ; SYSTEM{(FP+PROP:FontSize   )}=FSize 
    SYSTEM{(FP+PROP:FontStyle  )}=FStyle ; SYSTEM{(FP+PROP:FontCharSet)}=FCHar 
    SYSTEM{(FP+PROP:FontColor  )}=FColor
    DISPLAY ; IF X=1 THEN Message('Message Font After ' & CLIP(FFace)&' '& FSize).    
!--------------    
ScanRtn     ROUTINE !Scan for undocumented props 7900h-7DFFh
    DATA
BegPE   LONG(7900h)
EndPE   LONG(7E00h - 1)
    CODE 
    IF SysOrPrt = 2 THEN BegPE=07B00h ; EndPE=07BFFh ; END
    X=RECORDS(PQ)
    SORT(PQ, PQ:EqtHex) 
    LOOP PE = BegPE to EndPE
        CASE PE
!        OF   PROP:Ypos OROF PROP:Width OROF PROP:Height  ;  CYCLE 
        END
        Val = CHOOSE(SysOrPrt=2, PRINTER{PE}, SYSTEM{PE})     
        IF ~Val THEN CYCLE.    
        IF Val='0' THEN CYCLE.
        PQ:EqtHex = Hex8(PE,-4) 
        GET(PQ, PQ:EqtHex) ; IF ~ERRORCODE() THEN CYCLE.  !Already Got it
        PQ:Name = '{{' & PQ:EqtHex & '} Scan'
        SELF.PropQAdd(PQ,PE,PQ:Name,Val)
    END 
    SETTARGET()
    DISPLAY ; Message(RECORDS(PQ)-X & ' Added as {{####h} Scan')
    EXIT
!===========================================
CBWndPreviewClass.SystemMetrics  PROCEDURE()       
SMQ   QUEUE,PRE(SMQ) 
sNdx     STRING(5)  !SMQ:sNdx
EqtName  STRING(32) !SMQ:EqtName
Name     STRING(32) !SMQ:Name  3
Value    STRING(32) !SMQ:Value
nNdx     LONG       !SMQ:nNdx    
       END 
X  LONG,AUTO
CB ANY
FindCls CBLocateCls
FindTxt STRING(64),STATIC
Window WINDOW('System Metrics'),AT(,,273,250),GRAY,SYSTEM,FONT('Segoe UI',9),RESIZE
        BUTTON('&Copy'),AT(2,2,29,11),USE(?CopyBtn),SKIP,TIP('Copy Prop list to Clipboard')
        BUTTON('&Close'),AT(34,2,29,11),USE(?CloseBtn),SKIP,STD(STD:Close)
        ENTRY(@s64),AT(96,3,145,10),USE(FindTxt),SKIP,FONT('Consolas')
        BUTTON('&Find'),AT(67,2,25,11),USE(?FindNext),SKIP
        BUTTON('Pre&v'),AT(245,2,26,11),USE(?FindPrev),SKIP
        LIST,AT(0,18),FULL,USE(?LIST:SMQ),VSCROLL,FONT('Consolas',10),FROM(SMQ),FORMAT('22L(3)|FM~Ndx~C(0)@s5@80L(2)|FM~' & |
                'SM_ Equate~@s32@?80L(2)|FM~Description~@s32@?20L(2)F~Value~@s32@'),ALRT(DeleteKey)
    END
SysMenuCls SysMenuClass
SortCls CBSortClass    
  CODE
  OPEN(Window) ; SysMenuCls.Init(Window) ; SELF.AtSetOrSave(1, AtSM[])
  MakeOverList(?List:SMQ)
  SortCls.Init(SMQ,?LIST:SMQ,3)
  FindCls.Init(SMQ, ?LIST:SMQ, ?FindTxt, ?FindNext, ?FindPrev)
  ACCEPT
    CASE EVENT()
    OF EVENT:OpenWindow ; DO LoadSM_Rtn 
    END
    CASE ACCEPTED() 
    OF ?CopyBtn ; SetClip2Queue(SMQ,1,'Ndx<9>SM_ Equate<9>Description<9>Value',,1,4)
    END
    IF FIELD()=?LIST:SMQ THEN
       GET(SMQ,CHOICE(?LIST:SMQ))
       CASE EVENT()
       OF EVENT:AlertKey ; IF KEYCODE()=DeleteKey THEN DELETE(SMQ).
       OF EVENT:HeaderPressed ; SortCls.HeaderPressed() 
       OF EVENT:NewSelection
          IF KeyCode()=MouseLeft2 THEN 
             TextViewWindow('System Metric ' & SMQ:sNdx  &' '& SMQ:EqtName, |
              SMQ:sNdx &'<13,10>'& CLIP(SMQ:EqtName) &'<13,10>'& CLIP(SMQ:Name) &'<13,10,13,10>'& SMQ:Value, SMQ:Value)
          END
       END
    END !IF ?LIST:SMQ       
  END !Accept
  SELF.AtSetOrSave(2, AtSM[]); CLOSE(Window)
  RETURN
!-----------------  
LoadSM_Rtn ROUTINE
    DATA
SM2 STRING('00CXScreen 01CYScreen 02CXVScroll 03CYHScroll 04CYCaption 05CXBorder 06CYBorder 07CXFixedFra' &|
     'me 08CYFixedFrame 09CYVThumb 10CXHThumb 11CXIcon 12CYIcon 13CXCursor 14CYCursor 15CYMenu ' &|
     '16CXFullScreen 17CYFullScreen 18CYKanjiWindow 19Mousepresent 20CYVScroll 21CXHScroll ' &|
     '22Debug 23SwapButton 28CXMin 29CYMin 30CXSize 31CYSize 32CXSizeFrame 33CYSizeFrame 34CXMinTrac' &|
     'k 35CYMinTrack 36CXDoubleClk 37CYDoubleClk 38CXIconSpacing 39CYIconSpacing 40MenuDropAlignment' &|
     ' 41PenWindows 42DBCSEnabled 43CMousebuttons 44Secure 45CXEdge 46CYEdge 47CXMinSpacing ' &|
     '48CYMinSpacing 49CXSmIcon 50CYSmIcon 51CYSmCaption 52CXSmSize 53CYSmSize 54CXMenuSize ' &|
     '55CYMenuSize 56Arrange 57CXMinimized 58CYMinimized 59CXMaxTrack 60CYMaxTrack 61CXMaximized ' &|
     '62CYMaximized 63Network 67CleanBoot 68CXDrag 69CYDrag 70ShowSounds 71CXMenuCheck 72CYMenuCheck' &|
     ' 73SlowMachine 74MideastEnabled 75MouseWheelPresent 76CMetrics 76XVirtualScreen 77YVirtualScre' &|
     'en 78CXVirtualScreen 79CYVirtualScreen 80CMonitors 81SameDisplayFormat 82ImmEnabled 83CXFocusB' &|
     'order 84CYFocusBorder 86TabletPC 87MediaCenter ' ) 
SM4h STRING('1000RemoteSession 2000ShuttingDown 2001RemoteControl 2002CaretBlinkingEnabled 2003ConvertibleSlateMode 2004SystemDocked ')     
P7Q Parse7QType
Nm LIKE(P7Q:Name)
Ln BYTE
ValSM LONG
  CODE     
  EquateXStringParse(P7Q,2,SM2,1) !,1=Decimal
  EquateXStringParse(P7Q,4,SM4h) 
  FREE(SMQ) ; CLEAR(SMQ)
  LOOP X=1 TO RECORDS(P7Q) 
    GET(P7Q, X)
    ValSM = GetSystemMetrics(P7Q:EqtLong) 
    Nm = P7Q:Name
    IF INLIST(Nm[1:3],'CXV','CYH','CYV','CXH')  AND Nm[4] < 'a' THEN      !CXV CYHScroll CYV CXHThumb
       Nm=CLIP(SUB(Nm,4,99)) & CHOOSE(Nm[3]='H',' Horizontal ',' Vertical ') & Nm[1:2]
    ELSIF INLIST(Nm[1:2],'CX','CY') THEN   !CXDrag
       Nm=CLIP(SUB(Nm,3,99))&' '& Nm[1:2]
    ELSIF Nm[1]='C' AND Nm[2] < 'a' THEN   !CMouse
       Nm=CLIP(SUB(Nm,2,99))
    ELSIF (Nm[1]='X' OR Nm[1]='Y') AND Nm[2] < 'a' THEN   !XVirtual
       Nm=CLIP(SUB(Nm,2,99)) &' '& Nm[1]
    END
    IF Nm[1:2]='Sm' AND Nm[3] < 'a' THEN Nm='Small ' & CLIP(SUB(Nm,3,99)). !SmSize from SM_CXSMSIZE
    Ln=LEN(CLIP(Nm))
    CASE SUB(Nm,Ln-1,2)  !SmSize CX --> SmSize Width
    OF 'CX' ; Nm[Ln-1 : SIZE(Nm)]='Height'
    OF 'CY' ; Nm[Ln-1 : SIZE(Nm)]='Width' 
    END
    SMQ:sNdx    = CHOOSE(P7Q:EqtLong>99,P7Q:EqtHex,FORMAT(P7Q:EqtLong,@n02))
    SMQ:EqtName = P7Q:Name  !'SM_' & P7Q:Name 
    SMQ:Name    = Nm
    SMQ:Value   = ValSM
    SMQ:nNdx    = P7Q:EqtLong ; IF SMQ:nNdx < 100 THEN SMQ:sNdx='  '&SMQ:sNdx.
    ADD(SMQ)
  END !Loop    
  SORT(SMQ,SMQ:Name)
!=================
CBWndPreviewClass.PropQCopy2Clip  PROCEDURE(PropQType PQ)   !Copy PQ to Clipboard
  CODE ; SetClip2Queue(PQ,1,'Equate<9>Name<9>Value',,1,3)
!=================  
CBWndPreviewClass.PropQAdd  PROCEDURE(PropQType PrpQ, LONG PQEquate, STRING PQName, STRING PQVal, <STRING FilterVals>, BYTE AddBlank=0)
L   LONG,AUTO 
    CODE
    IF PQVal OR AddBlank THEN
        IF PQVal='<9>' THEN PQVal=''.  !Secret way to add blank values
        IF ~OMITTED(FilterVals) AND INSTRING(CLIP(PQVal),FilterVals,1) THEN RETURN.
        PrpQ.EqtLong = PQEquate
        IF PQEquate=-32 THEN 
           PrpQ.EqtHex = 'Win32'
        ELSE
           PrpQ.EqtHex = Hex8(PQEquate, -4) 
        END   
        PrpQ.Name   = PQName
        PrpQ.Value  = PQVal
        L = LenFastClip(PQVal)  
 !IF PQName='WNDProc' THEN Message(PQName &'|L='& L &'|Numeric=' &  NUMERIC(PQVal) &'|PQVal='& PQVal ).       
        IF L < 16 AND L AND NUMERIC(PQVal[1:L]) THEN
           L = 0 + PQVal
           IF L < -256 OR L > 256 THEN PrpQ.Value = CLIP(PQVal) &' = '& Hex8(L, -4). 
 !IF PQName='WNDProc' THEN Message(PQName &'|L='& L &'|PQVal='& PQVal ).                  
        END
        ADD(PrpQ, PrpQ.EqtHex)
    END
    RETURN   
!----------------
CBWndPreviewClass.GetFeqName PROCEDURE(LONG FEQ)!,STRING
    CODE
    SELF.FeqNmQ.FEQ = FEQ
    GET(SELF.FeqNmQ, SELF.FeqNmQ.FEQ) 
    RETURN CHOOSE(~ERRORCODE(), SELF.FeqNmQ.Name,'')
!----------------
CBWndPreviewClass.FeqCreatedByCB PROCEDURE(LONG F) !Class Create()?
  CODE
  CASE F 
  OF 0 OROF SELF.ReflectionBtn OROF SELF.GGLines.GdFEQ[1] OROF SELF.GGLines.GdFEQ[2]
  ELSE ; SELF.GridQ.LnFEQ=F ; GET(SELF.GridQ,SELF.GridQ.LnFEQ) ; IF ERRORCODE() THEN F=0.
  END 
  RETURN F
!----------------
CBWndPreviewClass.ConfigPut PROCEDURE(*? CfgValue)
WhereN  LONG,AUTO
CfgWho  CSTRING(32),AUTO
    CODE
    WhereN = WHERE(ConfigGrp, CfgValue) ; IF ~WhereN THEN STOP('ConfigPut WhereN=0 ' & ADDRESS(CfgValue) ).
    CfgWho = WHO(ConfigGrp, WhereN)
    IF ~CfgWho THEN STOP('ConfigPut CfgWho=Blank ' & ADDRESS(CfgValue) ) ; RETURN.
    PUTREG(REG_CURRENT_USER, ConfigKey, CfgWho, CfgValue, CHOOSE(~IsString(CfgValue),REG_DWORD,REG_SZ))
    !Message('ConfigPut IsString=' & IsString(CfgValue)  & '|WhereN=' & WhereN & '|CfgWho=' & CfgWho &'|CfgValue=' & CfgValue)
CBWndPreviewClass.ConfigGetAll PROCEDURE()
FldX    LONG,AUTO
CfgWho  CSTRING(32),AUTO
CfgAny  ANY
RegVal  STRING(256),AUTO
    CODE
    LOOP FldX=1 TO 99
         CfgWho=WHO(ConfigGrp,FldX) ; IF ~CfgWho THEN BREAK.
         RegVal=GETREG(REG_CURRENT_USER, ConfigKey, CfgWho)
         CfgAny &= WHAT(ConfigGrp,FldX) ; IF CfgAny &= NULL THEN Message(' CfgAny &= NULL').
         IF RegVal THEN CfgAny=RegVal.
         ! Message('ConfigGet: #' & FldX &' = '& CfgWho &'  |IsString=' & IsSTRING(CfgAny) &'|CfgAny=' & CfgAny &'|RegVal=' & RegVal )         
    END 
    ConfigGrp_DidGet=1
    RETURN
!========================================================================================================
CBWndPreviewClass.ResizeControl  PROCEDURE(LONG FEQ, LONG FeqTypeNo, STRING FeqTypeName, STRING FeqName) 
X       LONG,AUTO
S1Q QUEUE,PRE(S1Q)  !Simple ones
Poz     &LONG       !S1Q:Poz       
Haz     &LONG       !S1Q:Haz       
PROP    LONG        !S1Q:PROP      !{Prop:xx}
Attr    PSTRING(12) !S1Q:Attr      !eg. ANGLE
AParenV BYTE        !S1Q:AParenV   !1=Attr have (Value)
PropTxt PSTRING(24) !S1Q:PropTxt   !if no Attr then PROP
PropTrueRTL BYTE    !S1Q:PropTrueRTL   RTL Prop Default is True so must turn False eg Broken
FeqInp  LONG        !S1Q:FeqInp 
FeqPmt  LONG        !S1Q:FeqPmt
    END    
PosDataType GROUP,TYPE
X           LONG       ! Poz:X       Haz:X       B4Waz:X
Y           LONG       ! Poz:Y       Haz:Y       etc
Wd          LONG       ! Poz:Wd      Haz:Wd
Ht          LONG       ! Poz:Ht      Haz:Ht
No_X        BYTE       ! Poz:No_X    Haz:No_X          _nopos
No_Y        BYTE       ! Poz:No_Y    Haz:No_Y          _nopos
DeftWd      BYTE       ! Poz:DeftWd  Haz:DeftWd        Prop:NoWidth
DeftHt      BYTE       ! Poz:DeftHt  Haz:DeftHt        Prop:NoHeight
FullPr      BYTE       ! Poz:FullPr  Haz:FullPr        Prop:Full
FullWd      BYTE       ! Poz:FullWd  Haz:FullWd
FullHt      BYTE       ! Poz:FullHt  Haz:FullHt

TRN         LONG       ! Poz:TRN     Haz:TRN
Flat        LONG       ! Poz:Flat    Haz:Flat
Items       LONG       ! Poz:Items   Haz:Items
LineHt      LONG       ! Poz:LineHt  Haz:LineHt
Selectd     LONG       ! Poz:Selectd  Haz:Selectd
DropCnt     LONG       ! Poz:DropCnt  Haz:DropCnt
DropWd      LONG       ! Poz:DropWd  Haz:DropWd
Angle10     LONG       ! Poz:Angle10  Haz:Angle10
AngleSpin   DECIMAL(5,1)  ! Poz:AngleSpin
LineWd      LONG       ! Poz:LineWd  Haz:LineWd
Boxed       LONG       ! Poz:Boxed
HScroll     LONG       ! Poz:HScroll PROP:HScroll
VScroll     LONG       ! Poz:VScroll PROP:VScroll
VCR         LONG       ! Poz:VCR
Round       LONG       ! Poz:Round for BOX
BevelOut    LONG       ! Poz:BevelOut  Group Option Panel Region
BevelIn     LONG       ! Poz:BevelIn
BevelSty    LONG       ! Poz:BevelSty
Tiled       LONG       ! Poz:Tiled
Centered    LONG       ! Poz:Centered
Smooth      LONG       ! Poz:Smooth
StepTicks   LONG       ! Poz:StepTicks PROP:STEP for Slider
MarginL     LONG       ! PROP:TextLeftMargin
MarginR     LONG       ! PROP:TextRightMargin

ALCRDn      BYTE       ! Poz:ALCRDn     Align Left Cent Right Dec
AOffset     SHORT      ! Poz:AOffset
AtCODE      STRING(256) ! Poz:AtCODE   Haz:AtCODE   Waz:AtCODE 
AtS1Attr   CSTRING(128) ! Poz:AtS1Attr
AtS1Prop   CSTRING(128) ! Poz:AtS1Prop
ShUp1       BYTE       ! Poz:ShUp1      Haz:ShUp1      !PROP:Up     SHEET
ShDown2     BYTE       ! Poz:ShDown2    Haz:ShDown2    !PROP:Down
ShUpDn      BYTE       ! Poz:ShUpDn     Haz:ShUpDn     !PROP:UpsideDown and UP,DOWN   
ShHScroll   LONG       ! Poz:ShHScroll  Haz:ShHScroll  !PROP:Hscroll
ShJoin      LONG       ! Poz:ShJoin     Haz:ShJoin     !PROP:Join
ShSpread    LONG       ! Poz:ShSpread   Haz:ShSpread   !PROP:Spread
ShNoSheet   LONG       ! Poz:ShNoSheet  Haz:ShNoSheet  !PROP:NoSheet 
ShWizard    LONG       ! Poz:ShWizard   Haz:ShWizard   !PROP:Wizard 
ShBroken    LONG       ! Poz:ShBroken   Haz:ShBroken   !Prop:BrokenTabs 
ShNoTheme   LONG       ! Poz:ShNoTheme  Haz:ShNoTheme  !PROP:NoTheme
ShStyle     LONG       ! Poz:ShStyle    Haz:ShStyle    !PROP:TabSheetStyle 0-4
Picture     STRING(64) ! Poz:Picture    Haz:Picture  For Entry Spin String  

WndResize   BYTE       ! Poz:WndResize Haz:WndResize     WINDOW
WndWide    LONG        ! Poz:WndWide Haz:WndWide 
WndHigh    LONG        ! Poz:WndHigh Haz:WndHigh
         END !PosDataType

Prop_UpDown  LONG,DIM(3)
Try2:Disable BYTE,DIM(2)       ![1]=Was [2]=Now
Try2:Hide    BYTE,DIM(2)       !
HideUnHide   BYTE(1),STATIC   !VScroll needs,a Group move
BeforeWaz GROUP(PosDataType),PRE(B4Waz).    !Original Position Waz
Poz     GROUP(PosDataType),PRE(Poz).    !This Screen Position
Haz     GROUP(PosDataType),PRE(Haz).    !Preview Position Now
IsBEVEL SHORT
IsDROP  SHORT
Dropped BYTE
IsENTRY SHORT !For Picture, also does STRING(@pic)
IsLINE  SHORT
IsLIST  SHORT
IsSHEET SHORT
IsSTRING BYTE !1=Str 2=SStr
IsTEXT  SHORT
EntryV  STRING(255)  !prop:text
AlignQ QUEUE,PRE(AlnQ)
Desc     STRING(8)
ACode    PSTRING(8)
Prop     LONG
PrOffset LONG
Value    BYTE
       END
EntryVisQuoted BYTE !03/20/21 allow <13,10>
BoxItFEQ LONG
Window WINDOW('WYSIWYG Resize'),AT(,,485,207),GRAY,IMM,SYSTEM,FONT('Segoe UI',9),RESIZE
        TOOLBAR,AT(0,0,485,30),USE(?TOOLBAR1)
            BUTTON('&Save'),AT(2,1,25,12),USE(?SaveBtn),SKIP,TIP('Save size changes and Close<13,10>' & |
                    'Tool Tip is updated with Before and After Size.')
            BUTTON('Halt'),AT(63,1,25,12),USE(?HaltBtn)
            BUTTON('Cancel'),AT(31,1,29,12),USE(?CancelBtn),SKIP,TIP('Undo size changes and Close')
            BUTTON('Undo'),AT(2,14,25,12),USE(?UndoBtn),SKIP,TIP('Undo size changes back to Original' & |
                    ', but stay on this window')
            BUTTON('PV'),AT(93,1,14,12),USE(?PropsBtn),SKIP,TIP('PROP View')
            BUTTON('PE'),AT(109,1,14,12),USE(?PropEdBtn),SKIP,TIP('PROP Editor')
            BUTTON('LIST'),AT(125,1,21,12),USE(?LISTBtn),DISABLE,SKIP,TIP('PROPLIST Viewer')
            BUTTON('<50>'),AT(149,14,12,12),USE(?UnderBtn),SKIP,FONT('Webdings'),TIP('Move Under Windows to align under this one')
            LIST,AT(150,2,32,10),USE(Cfg:ResizeSnapTo),FONT(,8),TIP('How to Position Preview Window ' & |
                    'under the Resizer'),DROP(5,44),FROM('Right Snap|#1|Centr Snap|#2|Left Snap|#3|N' & |
                    'one|#9')
            BUTTON('F'),AT(31,14,13,12),USE(?FontBtn),SKIP,FONT(,10,,FONT:bold+FONT:italic),TIP('Font...')
            BUTTON('C'),AT(47,14,13,12),USE(?ColorBtn),SKIP,FONT(,10,COLOR:Red,FONT:bold), |
                    TIP('Pick Color PROPs')
            BUTTON('<176>'),AT(63,14,13,12),USE(?GdLinesBtn),SKIP,FONT('Wingdings',14,Color:GrayText,FONT:bold),tip('Guide and Grid Lines')
            BUTTON('<0a8h>'),AT(63+15,14,13,12),USE(?BoxItBtn),SKIP,FONT('Wingdings 2',14,COLOR:maroon),tip('Box in Red to find it on window')
            BUTTON('Tip'),AT(93,14,17,12),USE(?SeeTipBtn),SKIP,TIP('See Control Tool Tip')
            BUTTON('?'),AT(112,14,11,12),USE(?HelpBtn),KEY(F2Key),SKIP,FONT(,,,FONT:bold),TIP('Clarion Help on Control')
            BUTTON('Test'),AT(125,14,21,12),USE(?TestBtn),SKIP
            BUTTON('G...'),AT(165,14,17,12),USE(?GradBtn),SKIP,TIP('GradientTypes')
        END
        BOX,AT(0,0),FULL,USE(?BOX1),COLOR(COLOR:Red),LINEWIDTH(1)
        PANEL,AT(117,30,1,101),USE(?VertRightPanel),BEVEL(0,0,6000H)
        GROUP,AT(2,30,112,62),USE(?Group:AT)
            PROMPT('Align:'),AT(3,32),USE(?AlignQ:Pmt)
            LIST,AT(26,31,41,10),USE(?List:AlignQ),MSG('Poz:ALCRDn'),DROP(5),FROM(AlignQ), |
                    FORMAT('20L(2)@s8@')
            SPIN(@n-4),AT(74,31,29,10),USE(Poz:AOffset),HSCROLL,RIGHT,TIP('Align Offset<13,10>or Min' & |
                    'imum Tab Width for SHEET ')
            PROMPT('&X'),AT(6,47),USE(?XPoz:Pmt),TRN,FONT(,11)
            SPIN(@n-_6),AT(19,46,40,11),USE(Poz:X),HSCROLL,RIGHT,TIP('Alt Left- or Right+<13,10>Ctrl' & |
                    ' Left/Right also works')
            PROMPT('&Y'),AT(107,47),USE(?YPoz:Pmt),FONT(,11)
            SPIN(@n-_6),AT(63,46,40,11),USE(Poz:Y),HVSCROLL,RIGHT,TIP('Alt Up- or Down+<13,10>Ctrl U' & |
                    'p/Down also works'),STEP(-1)
            BUTTON('W'),AT(3,76,11,12),USE(?ResetWdBtn),SKIP,FONT(,7),TIP('Reset Width to Default/Full')
            BUTTON('H'),AT(102,76,11,12),USE(?ResetHtBtn),SKIP,FONT(,7),TIP('Reset Height to Default/Full')
            CHECK('&Default'),AT(18,73),USE(Poz:DeftWd),SKIP
            CHECK('&Full'),AT(18,82),USE(Poz:FullWd),SKIP
            PROMPT('&W'),AT(6,60),USE(?Wd:Pmt),FONT(,11)
            SPIN(@n_5),AT(19,60,40,11),USE(Poz:Wd),HSCROLL,RIGHT,TIP('Shift Left- or Right+'), |
                    RANGE(0,99999)
            CHECK('D&efault'),AT(63,73),USE(Poz:DeftHt),SKIP
            CHECK('F&ull'),AT(63,82),USE(Poz:FullHt),SKIP
            PROMPT('&H'),AT(106,60),USE(?Ht:Pmt),FONT(,11)
            SPIN(@n_5),AT(63,60,40,11),USE(Poz:Ht),HVSCROLL,RIGHT,TIP('Shift Up- or Down+'),RANGE(0,99999)
        END
        GROUP,AT(2,95,112,13),USE(?Group:Spot1),HIDE
            STRING('Spot1: (2,95 x 112,13)'),AT(3,97),USE(?Spot1:Pmt)
        END
        GROUP,AT(359,30,117,98),USE(?Group:Spotz)
            GROUP,AT(360,32,115,28),USE(?Group:LIST),HIDE
                PROMPT('Line Height:'),AT(363,35),USE(?LineHt:Pmt),TRN
                SPIN(@n2),AT(402,34,20,11),USE(Poz:LineHt),RIGHT,TIP('LIST PROP:LineHeight'), |
                        RANGE(1,99),STEP(1)
                PROMPT('&Items:'),AT(429,35),USE(?Items:Pmt),TRN
                SPIN(@n3),AT(450,34,22,11),USE(Poz:Items),RIGHT,TIP('LIST Items adjusts Height.<13>' & |
                        '<10>Nudge it +1/-1 to get a List Height with no partial Lines showing.<13>' & |
                        '<10>Can set ?PROP:Items=?PROPItems at runtime to fo rnow partial lines.'), |
                        RANGE(1,999),STEP(1)
                BUTTON('<47h>'),AT(363,46,12,11),USE(?DropBtn),DISABLE,SKIP,FONT('Wingdings 3',14),TIP('Drop the List'),FLAT, |
                        TRN
                PROMPT('Drop:'),AT(381,48),USE(?DropCnt:Pmt),DISABLE,TRN
                SPIN(@n2),AT(402,47,20,11),USE(Poz:DropCnt),DISABLE,RIGHT,TIP('LIST PROP:Drop'),RANGE(2,99),STEP(1)
                PROMPT('Width:'),AT(427,48),USE(?DropWd:Pmt),DISABLE,TRN
                SPIN(@n3),AT(450,47,22,11),USE(Poz:DropWd),DISABLE,RIGHT,TIP('LIST PROP:DropWidth'),RANGE(0,999),STEP(5)
                PROMPT('List Select:'),AT(363,61),USE(?Selectd:Pmt),TRN
                SPIN(@n3),AT(402,60,20,11),USE(Poz:Selectd),RIGHT,TIP('LIST Selected'),RANGE(0,999),STEP(1)                        
            END
            GROUP,AT(360,70,115,12),USE(?Group:Line),HIDE
                PROMPT('Line Width:'),AT(363,70),USE(?LineWd:Pmt)
                SPIN(@n3),AT(401,70,32,10),USE(Poz:LineWd),HVSCROLL,RIGHT,RANGE(0,999)
            END
            GROUP,AT(360,81,115,12),USE(?Group:String),HIDE
                PROMPT('Angle:'),AT(363,82),USE(?Angle:Pmt)
                SPIN(@n-6.1),AT(387,82,37,10),USE(Poz:AngleSpin),HVSCROLL,RIGHT,TIP('Angle from X-Ax' & |
                        'is counter-clockwise<13,10>See MSDN Font Escapement'),RANGE(-360,360),STEP(5)
            END
            GROUP,AT(360,111,115,12),USE(?Group:Bevel),HIDE
                PROMPT('Bevel:'),AT(363,113,18),USE(?Bevel:Pmt)
                SPIN(@n-3),AT(386,113,22,11),USE(Poz:BevelOut),RIGHT,TIP('Bevel Outer Edge'), |
                        RANGE(-999,999)
                SPIN(@n-3),AT(411,113,22,11),USE(Poz:BevelIn),RIGHT,TIP('Bevel Inner Edge'), |
                        RANGE(-999,999)
                ENTRY(@n6),AT(439,113,24,11),USE(Poz:BevelSty),RIGHT,TIP('Bevel Style overriding the' & |
                        ' signs of the outer and inner')
            END
            GROUP,AT(360,97,115,12),USE(?Group:Slider),HIDE
                PROMPT('Step between Ticks:'),AT(363,97),USE(?Step:Pmt)
                SPIN(@n4),AT(427,97,36,10),USE(Poz:StepTicks),HVSCROLL,RIGHT,RANGE(0,9999)
            END
        END
        GROUP,AT(360,155,107,12),USE(?Group:Margin),HIDE
            PROMPT('Margin L/R:'),AT(363,157),USE(?Margin:Pmt)
            SPIN(@n-3),AT(404,156,22,11),USE(Poz:MarginL),RIGHT,TIP('TODO - PROP:TextLeftMargin -1=D' & |
                    'efault -2=RTL'),RANGE(-2,999)
            SPIN(@n-3),AT(430,156,22,11),USE(Poz:MarginR),RIGHT,TIP('TODO - PROP:TextRightMargin -1=' & |
                    'Default -2=RTL'),RANGE(-2,999)
        END
        CHECK('Window Resize'),AT(3,151,58),USE(Poz:WndResize),TIP('Make Window Resizable<13,10>With' & |
                ' FULL allows sizing LIST or TEXT')
        SPIN(@n_5),AT(65,151,40,11),USE(Poz:WndWide),SKIP,HVSCROLL,RIGHT,TIP('Window Width'), |
                RANGE(0,99999)
        SPIN(@n_5),AT(111,151,40,11),USE(Poz:WndHigh),SKIP,HVSCROLL,RIGHT,TIP('Window Height'), |
                RANGE(0,99999)
        GROUP,AT(122,30,125,21),USE(?Group:ChecksTop)
            CHECK('Hide'),AT(123,31),USE(Try2:Hide[2]),SKIP
            CHECK('Disable'),AT(153,31,33),USE(Try2:Disable[2]),SKIP
            CHECK('Flat'),AT(123,41),USE(Poz:Flat),SKIP
            CHECK('TRN'),AT(153,41),USE(Poz:TRN),SKIP
            CHECK('Hide/UnHide'),AT(196,38),USE(HideUnHide),SKIP,TIP('Hide, Change, then UnHide. Wor' & |
                    'ks better espcially with groups.<13,10>TODO could have a Hide/Unhide Children o' & |
                    'f Group.')
        END
        GROUP,AT(122,50,42,71),USE(?Group:Checks)
            CHECK('Round'),AT(123,51),USE(Poz:Round),SKIP,DISABLE,TIP('Round Corners')
            CHECK('HScroll'),AT(123,61),USE(Poz:HScroll),SKIP,DISABLE
            CHECK('VScroll'),AT(123,71),USE(Poz:VScroll),SKIP,DISABLE,TIP('HIDE then UnHide to Repaint')
            CHECK('VCR'),AT(123,81),USE(Poz:VCR),SKIP,DISABLE
            CHECK('Centered'),AT(123,91),USE(Poz:Centered),SKIP,DISABLE
            CHECK('Tiled'),AT(123,101),USE(Poz:Tiled),SKIP,DISABLE
            CHECK('Smooth'),AT(123,111),USE(Poz:Smooth),SKIP,DISABLE
        END
        CHECK('Boxed'),AT(123,121),USE(Poz:Boxed),SKIP,HIDE
        GROUP,AT(250,50,90,74),USE(?Group:Sheet)
            CHECK('Up'),AT(251,51,23),USE(Poz:ShUp1),SKIP,TIP('Tab label reads upwards.<13,10>Check ' & |
                    'Down also for Upside Down text. (PROP:UpsideDown)')
            CHECK('Down'),AT(281,51),USE(Poz:ShDown2),SKIP,VALUE('2','0'),TIP('Tab label reads downwards.')
            CHECK('Join'),AT(251,61,23),USE(Poz:ShJoin),SKIP,TIP('One Row of Tabs, Scroll buttons at' & |
                    ' Right or Bottom.')
            CHECK('HScroll '),AT(281,61,33),USE(Poz:ShHScroll),SKIP,TIP('One Row of Tabs, Scroll but' & |
                    'tons on each end.')
            CHECK('Broken'),AT(319,61,33),USE(Poz:ShBroken),SKIP,TIP('Broken / Torn tab visual effec' & |
                    't (PROP:BrokenTabs)<13><10>When scrolling one row with Join or HScroll')
            CHECK('Spread'),AT(251,71),USE(Poz:ShSpread),SKIP
            CHECK('NoSheet'),AT(251,81),USE(Poz:ShNoSheet),SKIP,TIP('No visible 3D Panel.<13,10>Tab ' & |
                    'Ear location and orientation reverses.')
            CHECK('Wizard'),AT(251,91),USE(Poz:ShWizard),SKIP,TIP('No Tab Ears')
            CHECK('No Theme'),AT(251,101),USE(Poz:ShNoTheme),SKIP,TIP('No Visual Styles for SHEET<13,10>Also for LIST, must set before Accept loop')
            LIST,AT(251,112,42,10),USE(Poz:ShStyle),SKIP,TIP('PROP:TabSheetStyle visual style of the' & |
                    ' Tab Ears'),DROP(5),FROM('Defaut|#0|B & W|#1|Colored|#2|Squared|#3|Boxed|#4')
            BUTTON,AT(295,79,14,13),USE(?TabPickBtn),SKIP,ICON(ICON:Pick),TIP('Pick Tab to Show')
        END
        GROUP,AT(170,52,74,75),USE(?Group:Bevel2)
            GROUP('Edges'),AT(170,52,70,70),USE(?Group:BevelEdges),BOXED
                BOX,AT(186,71,38,35),USE(?GroupBtnBox),COLOR(0696969H),LINEWIDTH(1)
                BUTTON('B'),AT(200,108,10,11),USE(?BtnBO1),SKIP
                BUTTON('b'),AT(200,93,10,11),USE(?BtnBI2),SKIP
                BUTTON('R'),AT(226,83,10,11),USE(?BtnRO3),SKIP
                BUTTON('r'),AT(211,83,10,11),USE(?BtnRI4),SKIP
                BUTTON('T'),AT(200,59,10,11),USE(?BtnTO5),SKIP
                BUTTON('t'),AT(200,74,10,11),USE(?BtnTI6),SKIP
                BUTTON('L'),AT(173,83,10,11),USE(?BtnLO7),SKIP
                BUTTON('l'),AT(189,83,10,11),USE(?BtnLI8),SKIP
                BUTTON('A'),AT(173,108,10,11),USE(?BevAllEdgeBtn),SKIP,TIP('Set All Edges')
                BUTTON('<170>'),AT(226,108,10,11),USE(?BevPopBtn),SKIP,TIP('Bevel edge pattern choices'),font('Wingdings 2')
            END
        END
        PROMPT('@Pic:'),AT(3,119),USE(?Picture:Pmt)
        ENTRY(@s64),AT(24,119,88,11),USE(Poz:Picture),DISABLE,TIP('Clear to reset to original picture'),DISABLE
        PROMPT('Value:'),AT(3,136),USE(?EntryV:Pmt)
        ENTRY(@s255),AT(24,135,,11),FULL,USE(EntryV),TIP('Change ENTRY Value to try differen' & |
                't sizes')
        TEXT,AT(2,3,,11),FULL,USE(B4Waz:AtCODE),SKIP,FONT('Consolas'),COLOR(COLOR:BTNFACE), |
                TIP('Before AT()'),SINGLE
        TEXT,AT(2,16,,11),FULL,USE(Poz:AtCODE),SKIP,FONT('Consolas'),COLOR(COLOR:BTNFACE), |
                TIP('After AT()'),SINGLE
        STRING('--Bottom--'),AT(165,162),USE(?YBottom),HIDE
        STRING('TODO: a way to remove Min Max Width'),AT(158,151,138,10),USE(?YBottom:2),HIDE
    END
SysMenuCls SysMenuClass
BevCls BevClass
EVENT:SnapToUnder  EQUATE(EVENT:User+100)
    CODE
!Region BEFORE Open Window
    IF GloT:ResizeControl THEN Message('You have Resize Open for FEQ ' & GloT:ResizeControl ) ; RETURN.
    GloT:ResizeControl=FEQ ; SELF.BoxIt()
    IsENTRY=INLIST(FeqTypeNo,CREATE:Entry,CREATE:Combo,CREATE:Spin,CREATE:SString)
    IsLINE=INLIST(FeqTypeNo,CREATE:Line,CREATE:box,CREATE:ellipse)
    IsLIST=INLIST(FeqTypeNo,CREATE:List,CREATE:Combo,CREATE:DropList,CREATE:DropCombo)
        IF IsLIST=1 THEN IsDROP=PWnd$FEQ{PROP:Drop}. !Combo won't drop
    IsTEXT=INLIST(FeqTypeNo,CREATE:Text,CREATE:singleline) !na CREATE:rtf
    IsSHEET=CHOOSE(FeqTypeNo=CREATE:sheet)
    IsSTRING=INLIST(FeqTypeNo,CREATE:String,CREATE:sString) 
    DO S1QLoadRtn ; DO AlignQLoadRtn
    DO GetPositionOnceRtn ; BeforeWaz=Poz ; Haz=Poz
!EndRegion BEFORE Open Window        - Before Open(Window)
    OPEN(Window) ; SysMenuCls.Init(Window) !OPEN Window--OPEN Window--OPEN Window--OPEN Window--OPEN Window--OPEN Window--OPEN Window
!Region After OPENed Window -- Prepare for ACCEPT - After OPEN(Window)
    IF ~AtReszCont[3] THEN 
      ! 0{PROP:Width}=?Try2:Disable_2{PROP:XPos}+6 
       0{PROP:Width}=?Cfg:ResizeSnapTo{PROP:XPos} +14 + ?Cfg:ResizeSnapTo{PROP:Width}
       0{PROP:Height}=?YBottom{PROP:YPos}+10+ ?TOOLBAR1{PROP:Height}
    END
    SELF.AtSetOrSave(1, AtReszCont[]) ; 0{PROP:MinWidth}=?VertRightPanel{PROP:XPos}+10 ; 0{PROP:MinHeight}=?Poz:FullWd{PROP:YPos}+20+ ?TOOLBAR1{PROP:Height}
    0{PROP:Text} = 'WYSIWYG '& FEQ &' '& CLIP(FeqTypeName) & ' - '& FeqName ! & ' (' & FeqTypeNo   &')  '&
    MakeOverWindow() ; DropListColor(?Cfg:ResizeSnapTo) ; DropListColor(?List:AlignQ) ; DropListColor(?Poz:ShStyle)
    DO S1QWindowOpenRtn ! HIDE/ UNHIDE /DISABLE based on Control Type being resized
    ?List:AlignQ{PROP:Selected}=Poz:ALCRDn
    IF IsLIST THEN ENABLE(?LISTBtn).
    DO SyncRtn
!EndRegion After OPEN Window, Prepare for ACCEPT    
    ACCEPT
        CASE EVENT()
        OF EVENT:OpenWindow   ; SysMenuCls_SYSCOMMAND(0{PROP:Handle},SMCmd_HideUnder) ; POST(EVENT:SnapToUnder)
        OF EVENT:NewSelection ; DO AcceptOrSpinSizesRtn
        OF EVENT:CloseWindow  ; POST(EVENT:Accepted,?CancelBtn) ; CYCLE
        !OF EVENT:Sized        ; GETPOSITION(0,,,W#,H#) ; SC#+=1 ; 0{PROP:Text}='Sized ' & W# &','& H# & '  Cnt=' & SC# &' Min=' & 0{PROP:MinWidth} 
        OF EVENT:Moved        ; POST(EVENT:SnapToUnder)  ! GETPOSITION(0,X#,Y#) ; 0{PROP:Text}='Moved ' & X# &','& Y# 
        OF EVENT:AlertKey     ; DO TakeEVENT:AlertKeyRtn
        OF EVENT:SnapToUnder  ; SELF.SnapToPreview()
        OF Event:Rejected ; BEEP ; SELECT(?) ; CYCLE
        END
        CASE ACCEPTED()
        OF ?HaltBtn ; HALT 
        OF ?Cfg:ResizeSnapTo ; SELF.SnapToPreview() ; SELF.ConfigPut(Cfg:ResizeSnapTo)
        OF ?List:AlignQ ; Poz:ALCRDn=CHOICE(?List:AlignQ) ; GET(AlignQ,Poz:ALCRDn)
        OF ?Poz:ShJoin    ; Poz:ShHScroll=0
        OF ?Poz:ShHScroll ; Poz:ShJoin =0
        OF ?SaveBtn   ; PWnd$Feq{PROP:Tip}=PWnd$Feq{PROP:Tip} &'<13,10>Before Resize: ' & CLIP(B4Waz:AtCODE) &'<13,10>After Resize: ' & CLIP(Poz:AtCODE)  
                        BREAK 
        OF ?CancelBtn ; Poz=BeforeWaz ; DO SizeChangeRtn ; BREAK
        OF ?UndoBtn   ; Poz=BeforeWaz ; DO SizeChangeRtn
        
        OF ?UnderBtn ; SysMenuCls_SYSCOMMAND(0{PROP:Handle},SMCmd_MoveUnder)
        OF ?LISTBtn  ; SELF.ListPROPs(FEQ,FeqTypeNo,FeqTypeName,FeqName)
        OF ?PropsBtn ; SELF.ControlPROPs(FEQ, FeqTypeNo, FeqTypeName,FeqName) 
        OF ?PropEdBtn; HIDE(0) ; SELF.PROPsEditor(FEQ, FeqTypeNo, FeqTypeName,FeqName) ; UNHIDE(0)
        OF ?Poz:Picture ; IF ~Poz:Picture THEN Poz:Picture=B4Waz:Picture. ; Poz:Picture=LEFT(Poz:Picture)
        OF ?EntryV   ; SETTARGET(PWnd)
                       IF IsENTRY OR IsTEXT THEN 
                          CHANGE(FEQ,EntryV) 
                       ELSE 
                          PWnd$FEQ{PROP:Text}=CHOOSE(~EntryVisQuoted,EntryV,UNQUOTE(EntryV))
                       END ; DISPLAY ; SETTARGET() ; CYCLE
        OF ?BtnBO1 TO ?BtnLI8 ; BevCls.BtnClick(?) 
        OF ?BevAllEdgeBtn     ; BevCls.AllBtn()
        OF ?BevPopBtn ; DO BevPopRtn
        OF ?SeeTipBtn ; Message(PWnd$FEQ{PROP:Tip},'Tip: ' & FEQ &' '& CLIP(FeqTypeName) & FeqName)
        OF ?FontBtn     ; DO FontRtn
        OF ?ColorBtn    ; DO ColorRtn
        OF ?GdLinesBtn  ; Self.GuideLines(FEQ, FeqTypeNo, FeqTypeName, FeqName)
        OF ?GradBtn     ; DO GradRtn
        OF ?Try2:Disable_2 ; PWnd$FEQ{PROP:Disable}=Try2:Disable[2] ; CYCLE
        OF ?Try2:Hide_2 ; PWnd$FEQ{PROP:Hide}=Try2:Hide[2] ; CYCLE 
        OF ?TabPickBtn ; DO TabPickBtnRtn
        OF ?BoxItBtn 
            SETTARGET(PWnd)
            IF BoxItFEQ THEN DESTROY(BoxItFEQ) ; BoxItFEQ=0
            ELSE
                X=CREATE(0,Create:Box) ; BoxItFEQ=X; SETPOSITION(X, Poz:X-2,Poz:Y-2,Poz:Wd+4,Poz:Ht+4) !;F{PROP:TRN}=1
                X{PROP:Fill}=COLOR:Yellow ; X{PROP:Color}=COLOR:Red ; UNHIDE(X)
            END
            SETTARGET()
        OF ?DropBtn ; ListDrop(FEQ,Dropped) ; Dropped=1-Dropped ; CYCLE !PostMessage(PWnd$Feq{PROP:Handle}, 14Fh, 1, 0) ! ; PostMessage(PWnd$Feq{PROP:Handle}, 14Fh, 1, 0)
        OF ?HelpBtn ; HelpCW(FeqTypeName)
        OF ?TestBtn ; DO TestBtnRtn
        END
        IF ACCEPTED() THEN DO AcceptOrSpinSizesRtn.        
!TODO add ENTRY value to set
    END !Accept 
    SELF.AtSetOrSave(2, AtReszCont[])
    CLOSE(Window)
    GloT:ResizeControl=0             
    SETTARGET(PWnd)
!05/06/20 fixed this to work in April, it undoes change to Hide/Disable... let it be, can do in the Fields List
!    IF Try2:Disable[2]<>Try2:Disable[1] THEN FEQ{PROP:Disable}=Try2:Disable[1].
!    IF Try2:Hide[2]<>Try2:Hide[1] THEN FEQ{PROP:Hide}=Try2:Hide[1]. 
    IF BoxItFEQ THEN DESTROY(BoxItFEQ). ; IF IsDROP THEN ListDrop(FEQ,1).
    SETTARGET()
    RETURN
!-----------------
TakeEVENT:AlertKeyRtn ROUTINE
    DATA
PM1     SHORT
PmFeq   LONG 
DeftFeq LONG !Default Wd/Hht
PmPoz   &LONG 
    CODE
    CASE KEYCODE()  !Allow Shift+Arrows to Resize like in Window Formatter. Alt+Arrows move XY
    OF ShiftUp    OROF CtrlShiftUp    ; PM1=-1 ; PmFeq=?Poz:Ht  ; PmPoz &= Poz:Ht ; DeftFeq=?Poz:DeftHt
    OF ShiftDown  OROF CtrlShiftDown  ; PM1= 1 ; PmFeq=?Poz:Ht  ; PmPoz &= Poz:Ht ; DeftFeq=?Poz:DeftHt
    OF ShiftLeft  OROF CtrlShiftLeft  ; PM1=-1 ; PmFeq=?Poz:Wd  ; PmPoz &= Poz:Wd ; DeftFeq=?Poz:DeftWd
    OF ShiftRight OROF CtrlShiftRight ; PM1= 1 ; PmFeq=?Poz:Wd  ; PmPoz &= Poz:Wd ; DeftFeq=?Poz:DeftWd   
    OF AltUp      OROF CtrlUp    ; PM1=-1 ; PmFeq=?Poz:Y   ; PmPoz &= Poz:Y
    OF AltDown    OROF CtrlDown  ; PM1= 1 ; PmFeq=?Poz:Y   ; PmPoz &= Poz:Y
    OF AltLeft    OROF CtrlLeft  ; PM1=-1 ; PmFeq=?Poz:X   ; PmPoz &= Poz:X
    OF AltRight   OROF CtrlRight ; PM1= 1 ; PmFeq=?Poz:X   ; PmPoz &= Poz:X
    ELSE ; EXIT
    END
    IF PmFeq AND PM1 THEN
       IF PmFeq{PROP:Disable} OR PmFeq{PROP:Hide} THEN EXIT.
       UPDATE
       IF PmPoz < 1 AND PM1=-1 AND BAND(KEYCODE(),100h) THEN EXIT. !No Negative sizes (Shift Down)
       IF PmFeq{PROP:Disable} AND DeftFeq AND DeftFeq{PROP:Checked} THEN  !Default is checked
          CHANGE(DeftFeq,0) ; DO SyncRtn                                  !uncheck so can change W/H
       END
       PmPoz += PM1 ; POST(EVENT:Accepted,PmFEQ) ; EXIT
    END       
!----------------- 
AcceptOrSpinSizesRtn ROUTINE ! OF ?Poz:X  TO ?Poz:Ht ;  DO AcceptedSizesRtn
    CASE FIELD()
    OF ?Poz:X
    OF ?Poz:Y
    OF ?Poz:Wd   !SPIN(@n6) Poz:Wd      !----Width----
    OF ?Poz:DeftWd ; IF Poz:FullWd THEN Poz:FullWd=0. ; IF Poz:FullHt THEN Poz:FullHt=0 ; Poz:DeftHt=1. !CHECK('&Default') Poz:DeftWd
    OF ?Poz:FullWd ; IF Poz:DeftWd THEN Poz:DeftWd=0. ; IF Poz:DeftHt THEN Poz:DeftHt=0 ; Poz:FullHt=1. !CHECK('&Full')    Poz:FullWd
    OF ?Poz:Ht   !SPIN(@n6) Poz:Ht      !----Height----
    OF ?Poz:DeftHt ; IF Poz:FullHt THEN Poz:FullHt=0. ; IF Poz:FullWd THEN Poz:FullWd=0 ; Poz:DeftWd=1. !CHECK('D&efault') Poz:DeftHt      
    OF ?Poz:FullHt ; IF Poz:DeftHt THEN Poz:DeftHt=0. ; IF Poz:DeftWd THEN Poz:DeftWd=0 ; Poz:FullWd=1. !CHECK('F&ull'),   Poz:FullHt  
    OF ?ResetWdBtn ; Poz:Wd=PWnd$Feq{PROP:Width}  ; Poz:DeftWd=0 ; Poz:FullWd=0
    OF ?ResetHtBtn ; Poz:Ht=PWnd$Feq{PROP:Height} ; Poz:DeftHt=0 ; Poz:FullHt=0
    OF 0 ; EXIT
    !ELSE ; EXIT 
    END
    Poz:FullPr=CHOOSE(Poz:FullWd OR Poz:FullHt)
    DO SizeChangeRtn    
SizeChangeRtn ROUTINE
    DO SyncRtn ; DISPLAY ; DO SetPosition_Poz2PreviewRtn
    EXIT
SyncRtn ROUTINE    
    ?Poz:Wd{PROP:Disable}=CHOOSE(Poz:DeftWd+Poz:FullWd)
    ?Poz:Ht{PROP:Disable}=CHOOSE(Poz:DeftHt+Poz:FullHt)
    IF Poz:No_X THEN DISABLE(?Poz:X).
    IF Poz:No_Y THEN DISABLE(?Poz:Y).
    ?ResetWdBtn{PROP:Disable}=CHOOSE(0=Poz:DeftWd+Poz:FullWd)
    ?ResetHtBtn{PROP:Disable}=CHOOSE(0=Poz:DeftHt+Poz:FullHt)
!--------------------
S1QLoadRtn ROUTINE 
   CLEAR(S1Q);S1Q:Poz&=Poz:Angle10 ;S1Q:Haz&=Haz:Angle10 ; S1Q:PROP=PROP:Angle ; S1Q:Attr='Angle' ; S1Q:AParenV=1 ; S1Q:FeqInp=?Group:String ;ADD(S1Q)
   CLEAR(S1Q);S1Q:Poz&=Poz:BevelOut ;S1Q:Haz&=Haz:BevelOut ;S1Q:PROP=PROP:BevelOuter ;S1Q:FeqInp=?Group:Bevel ;ADD(S1Q)
   CLEAR(S1Q);S1Q:Poz&=Poz:BevelIn  ;S1Q:Haz&=Haz:BevelIn  ;S1Q:PROP=PROP:BevelInner ;ADD(S1Q)
   CLEAR(S1Q);S1Q:Poz&=Poz:BevelSty ;S1Q:Haz&=Haz:BevelSty ;S1Q:PROP=PROP:BevelStyle ;ADD(S1Q)
   CLEAR(S1Q);S1Q:Poz&=Poz:Boxed   ;S1Q:Haz&=Haz:Boxed  ;S1Q:PROP=Prop:Boxed ;S1Q:Attr='Boxed' ;S1Q:FeqInp=?Poz:Boxed ;ADD(S1Q)
   CLEAR(S1Q);S1Q:Poz&=Poz:LineWd  ;S1Q:Haz&=Haz:LineWd ; S1Q:PROP=PROP:LineWidth ; S1Q:Attr='LineWidth' ; S1Q:AParenV=1 ; S1Q:FeqInp=?Group:Line ; ADD(S1Q)  
   CLEAR(S1Q);S1Q:Poz&=Poz:HScroll ;S1Q:Haz&=Haz:HScroll ;S1Q:PROP=Prop:HScroll ;S1Q:Attr='HScroll' ;S1Q:FeqInp=?Poz:HScroll ;ADD(S1Q)
   CLEAR(S1Q);S1Q:Poz&=Poz:VScroll ;S1Q:Haz&=Haz:VScroll ;S1Q:PROP=Prop:VScroll ;S1Q:Attr='VScroll' ;S1Q:FeqInp=?Poz:VScroll ;ADD(S1Q)
   CLEAR(S1Q);S1Q:Poz&=Poz:VCR ;S1Q:Haz&=Haz:VCR ;S1Q:PROP=Prop:VCR ;S1Q:Attr='VCR' ;S1Q:FeqInp=?Poz:VCR ;ADD(S1Q)
   CLEAR(S1Q);S1Q:Poz&=Poz:Round   ;S1Q:Haz&=Haz:Round ;S1Q:PROP=Prop:Round ;S1Q:Attr='Round' ;S1Q:FeqInp=?Poz:Round ;ADD(S1Q)
   CLEAR(S1Q);S1Q:Poz&=Poz:StepTicks  ;S1Q:Haz&=Haz:StepTicks ; S1Q:PROP=PROP:STEP ; S1Q:Attr='Step' ; S1Q:AParenV=1 ; S1Q:FeqInp=?Group:Slider ; ADD(S1Q)  

   CLEAR(S1Q);S1Q:Poz&=Poz:Centered ;S1Q:Haz&=Haz:Centered ;S1Q:PROP=Prop:Centered ;S1Q:Attr='Centered' ;S1Q:FeqInp=?Poz:Centered ;ADD(S1Q)
   CLEAR(S1Q);S1Q:Poz&=Poz:Tiled ;S1Q:Haz&=Haz:Tiled ;S1Q:PROP=Prop:Tiled ;S1Q:Attr='Tiled' ;S1Q:FeqInp=?Poz:Tiled ;ADD(S1Q)
   CLEAR(S1Q);S1Q:Poz&=Poz:Smooth ;S1Q:Haz&=Haz:Smooth ;S1Q:PROP=Prop:Smooth ;S1Q:Attr='Smooth' ;S1Q:FeqInp=?Poz:Smooth ;ADD(S1Q)

  !IF FeqTypeNo=CREATE:Box THEN
  CLEAR(Poz)
  IF FeqTypeNo=CREATE:Box THEN Poz:Round=1.
  IF IsLINE THEN Poz:LineWd=1.
  IF INLIST(FeqTypeNo,CREATE:Group,CREATE:Option,CREATE:Panel,CREATE:Region) THEN 
     IsBEVEL=1 ; Poz:BevelOut=1 ; Poz:BevelIn=1 ; Poz:BevelSty=1
  END
  IF INLIST(FeqTypeNo,CREATE:Group,CREATE:Option,CREATE:Text) THEN Poz:Boxed=1.
  IF FeqTypeNo=CREATE:Image THEN Poz:Centered=1 ; Poz:Tiled=1.
   
  IF IsLIST OR INLIST(FeqTypeNo,CREATE:Spin,CREATE:Text,CREATE:Image) THEN Poz:HScroll=1 ; Poz:VScroll=1.
  IF IsLIST THEN Poz:VCR=1.
  IF FeqTypeNo=CREATE:Progress THEN Poz:Smooth=1.
  IF FeqTypeNo=CREATE:Slider_MIA THEN Poz:StepTicks=1.
  IF IsSTRING THEN Poz:Angle10=1.
  LOOP X=RECORDS(S1Q) TO 1 BY -1
       GET(S1Q,X) ; IF ~S1Q:Poz THEN DELETE(S1Q).
  END

  IF IsSHEET THEN
     CLEAR(S1Q);S1Q:Poz&=Poz:ShHScroll ;S1Q:Haz&=Haz:ShHScroll ;S1Q:PROP=PROP:Hscroll       ;S1Q:Attr='Hscroll'      ;ADD(S1Q)
     CLEAR(S1Q);S1Q:Poz&=Poz:ShJoin    ;S1Q:Haz&=Haz:ShJoin    ;S1Q:PROP=PROP:Join          ;S1Q:Attr='Join'         ;ADD(S1Q)
     CLEAR(S1Q);S1Q:Poz&=Poz:ShSpread  ;S1Q:Haz&=Haz:ShSpread  ;S1Q:PROP=PROP:Spread        ;S1Q:Attr='Spread'       ;ADD(S1Q)
     CLEAR(S1Q);S1Q:Poz&=Poz:ShNoSheet ;S1Q:Haz&=Haz:ShNoSheet ;S1Q:PROP=PROP:NoSheet       ;S1Q:Attr='NoSheet'      ;ADD(S1Q)
     CLEAR(S1Q);S1Q:Poz&=Poz:ShWizard  ;S1Q:Haz&=Haz:ShWizard  ;S1Q:PROP=PROP:Wizard        ;S1Q:Attr='Wizard'       ;ADD(S1Q)
     CLEAR(S1Q);S1Q:Poz&=Poz:ShBroken  ;S1Q:Haz&=Haz:ShBroken  ;S1Q:PROP=Prop:BrokenTabs    ;S1Q:PropTxt='Prop:BrokenTabs' ; S1Q:PropTrueRTL=1 ;ADD(S1Q)
     CLEAR(S1Q);S1Q:Poz&=Poz:ShStyle   ;S1Q:Haz&=Haz:ShStyle   ;S1Q:PROP=PROP:TabSheetStyle ;S1Q:PropTxt='PROP:TabSheetStyle' ;ADD(S1Q)
     CLEAR(S1Q);S1Q:Poz&=Poz:ShNoTheme ;S1Q:Haz&=Haz:ShNoTheme ;S1Q:PROP=PROP:NoTheme       ;S1Q:PropTxt='PROP:NoTheme'       ;ADD(S1Q)
  END

  CLEAR(S1Q);S1Q:Poz&=Poz:Flat ;S1Q:Haz&=Haz:Flat ;S1Q:PROP=PROP:Flat ;S1Q:Attr='Flat' ;S1Q:FeqInp=?Poz:Flat ;ADD(S1Q)
  CLEAR(S1Q);S1Q:Poz&=Poz:TRN  ;S1Q:Haz&=Haz:TRN  ;S1Q:PROP=PROP:TRN ;S1Q:Attr='TRN'; S1Q:FeqInp=?Poz:TRN ;ADD(S1Q)
  CLEAR(Poz)
!-----------------------  
S1QWindowOpenRtn ROUTINE
    LOOP X=1 TO RECORDS(S1Q) ; GET(S1Q,X)
        IF S1Q:FeqInp THEN ENABLE(S1Q:FeqInp) ; UNHIDE(S1Q:FeqInp).
        IF S1Q:FeqPmt THEN ENABLE(S1Q:FeqPmt) ; UNHIDE(S1Q:FeqPmt).
    END
    IF Poz:No_X THEN DISABLE(?Poz:X). ; IF Poz:No_Y THEN DISABLE(?Poz:Y).

    IF    ?Group:Bevel{PROP:Visible}  THEN GroupMoveChildren(?Group:Bevel,?Group:Spot1)
    ELSIF ?Group:String{PROP:Visible} THEN GroupMoveChildren(?Group:String,?Group:Spot1)
    ELSIF ?Group:Line{PROP:Visible}   THEN GroupMoveChildren(?Group:Line,?Group:Spot1)
    ELSIF ?Group:Slider{PROP:Visible} THEN GroupMoveChildren(?Group:Slider,?Group:Spot1)
    ELSIF IsLIST THEN GroupMoveChildren(?Group:LIST,?Group:Spot1) ; UNHIDE(?Group:LIST)
          IF IsENTRY THEN !Combo
             HIDE(?Selectd:Pmt,?Poz:Selectd)
          ELSIF ~IsDROP THEN
             ?Selectd:Pmt{PROP:YPos}=?DropWd:Pmt{PROP:YPos}
             ?Poz:Selectd{PROP:YPos}=?Poz:DropWd{PROP:YPos}
          END    
    ELSIF IsENTRY OR IsText THEN
          GroupMoveChildren(?Group:Margin,?Group:Spot1) ; UNHIDE(?Group:Margin)  !TODO a LIST can have Margins (I think)    
    END
    IF IsDROP THEN ENABLE(?DropBtn,?Poz:DropWd) ELSE HIDE(?DropBtn,?Poz:DropWd).       
    IF IsSheet THEN 
        HIDE(?Group:Checks) ; UNHIDE(?Group:Sheet) ; HIDE(?Group:Bevel2) ; GroupMoveChildren(?Group:Sheet,?Group:Checks,,4)
    ELSIF IsBevel THEN
        HIDE(?Group:Checks) ; HIDE(?Group:Sheet) ; UNHIDE(?Group:Bevel2) ; GroupMoveChildren(?Group:BevelEdges,?Group:Checks,,1)
        BevCls.Init(?BtnBO1, Poz:BevelSty, ?Poz:BevelSty)
    ELSE
        HIDE(?Group:Sheet) ; HIDE(?Group:Bevel2) 
    END
   
    IF NOT(IsENTRY OR IsTEXT) THEN !Combo ISEntry=1
       EntryV=PWnd$FEQ{PROP:Text}
       IF INLIST(FeqTypeNo,CREATE:button,CREATE:prompt,CREATE:check,CREATE:state3,CREATE:radio)  !API CHECK RADIO allow 13,10
          EntryVisQuoted=1
          EntryV=QUOTE(CLIP(EntryV))
          ?EntryV:Pmt{PROP:Text}='"Text"'
          ?EntryV{PROP:Tip}='Quoted (''Literal'')<13,10>Double ''<<{{ and use hex <<13,10>'
       ELSIF EntryV OR ~INLIST(FeqTypeNo,CREATE:region,CREATE:line,CREATE:list,CREATE:box,CREATE:ellipse,CREATE:progress,CREATE:Slider_MIA,CREATE:sheet,CREATE:panel)
          ?EntryV{PROP:Tip}='Change PROP:Text'
          ?EntryV:Pmt{PROP:Text}='Text:'
       ELSE
          HIDE(?EntryV:Pmt,?EntryV)
       END
    END 
    IF IsENTRY THEN 
       IF Poz:Picture THEN ENABLE(?Poz:Picture).
    ELSE 
       HIDE(?Poz:Picture) ; HIDE(?Picture:Pmt)
    END
    ALERT(ShiftUp) ; ALERT(ShiftDown) ; ALERT(ShiftLeft) ; ALERT(ShiftRight)
    ALERT(CtrlShiftUp) ; ALERT(CtrlShiftDown) ; ALERT(CtrlShiftLeft) ; ALERT(CtrlShiftRight)    
    ALERT(AltUp) ; ALERT(AltDown) ; ALERT(AltLeft) ; ALERT(AltRight) 
    ALERT(CtrlUp) ; ALERT(CtrlDown) ; ALERT(CtrlLeft) ; ALERT(CtrlRight)
!-------------------------    
GetPositionOnceRtn ROUTINE !Setup the Initial Poz, Haz and Waz
    SETTARGET(PWnd)
    Poz:WndResize=0{PROP:Resize} ; Poz:WndWide=0{Prop:Width} ; Poz:WndHigh=0{Prop:Height}
    GETPOSITION(Feq,Poz:X,Poz:Y,Poz:Wd,Poz:Ht)
    IF Poz:X=_nopos THEN Poz:X=0 ; Poz:No_X=1 . 
    IF Poz:Y=_nopos THEN Poz:Y=0 ; Poz:No_Y=1 .
    Poz:FullPr=CHOOSE(~Feq{PROP:Full},'','full')
    Poz:DeftWd=Feq{PROP:NoWidth}
    Poz:DeftHt=Feq{PROP:NoHeight}
    Poz:FullPr=Feq{PROP:Full}
    IF Poz:FullPr THEN
       IF Poz:DeftWd THEN Poz:FullWd=1 ; Poz:DeftWd=0.
       IF Poz:DeftHt THEN Poz:FullHt=1 ; Poz:DeftHt=0.
    END
   
    LOOP X=1 TO RECORDS(S1Q)    !Get simple PROPs e.g. was IF IsSTRING THEN Poz:Angle10=Feq{PROP:Angle}  
         GET(S1Q,X) 
         S1Q:Poz=FEQ{S1Q:PROP}  !E.g. S1Q:Poz&=Poz:Angle10 and S1Q:PROP=PROP:Angle, so Poz:Angle10=FEQ{Prop:Angle}
    END    
    Poz:AngleSpin=Poz:Angle10/10  !in S1Q
    
    IF IsENTRY THEN Poz:Picture=FEQ{PROP:Text}.
    IF IsENTRY OR IsTEXT THEN EntryV=CONTENTS(FEQ).  !SString needs Contents
    IF IsLIST THEN Poz:Items=Feq{PROP:Items} ; Poz:LineHt=Feq{PROP:LineHeight} ; Poz:Selectd=PWnd$FEQ{PROP:Selected}.
    IF IsDROP THEN Poz:DropCnt=PWnd$FEQ{PROP:Drop} ; Poz:DropWd =PWnd$FEQ{PROP:DropWidth}.
    LOOP X=1 TO 5     !Check Align e.g. PROP:Left or if NONE the Default 
        GET(AlignQ,X) !Must Leave Q Loaded
        IF X=5 THEN 
           Poz:ALCRDn=5
        ELSIF Feq{AlnQ:Prop} THEN  !If PROP:Left/c/r/dec True
           Poz:ALCRDn=X ; Poz:AOffset=Feq{AlnQ:PrOffset} ; BREAK
        END 
    END
    IF IsSHEET THEN
        Prop_UpDown[1]=PROP:Up ; Prop_UpDown[2]=PROP:Down ; Prop_UpDown[3]=PROP:UpsideDown
        IF FEQ{PROP:UpsideDown} THEN Poz:ShUp1=1 ; Poz:ShDown2=2
        ELSIF FEQ{PROP:Up}      THEN Poz:ShUp1=1
        ELSIF FEQ{PROP:Down}    THEN               Poz:ShDown2=2
        END
        Poz:ShUpDn=Poz:ShUp1+Poz:ShDown2
    END 

    IF FeqTypeNo=CREATE:Progress THEN FEQ{PROP:Progress}=FEQ{PROP:RangeHigh}.
    Try2:Disable[1]=FEQ{PROP:Disable} ; Try2:Disable[2]=Try2:Disable[1]
    Try2:Hide[1]=FEQ{PROP:Hide} ; Try2:Hide[2]=Try2:Hide[1]
    SETTARGET()
    DO Poz:AtCODERtn
    EXIT
Poz:AtCODERtn ROUTINE
    Poz:AtS1Attr='' ; Poz:AtS1Prop=''
    LOOP X=1 TO RECORDS(S1Q) ; GET(S1Q,X) !Get simple PROPs e.g. was IF IsSTRING THEN Poz:Angle10=Feq{PROP:Angle}  
         IF (~S1Q:Poz AND ~S1Q:PropTrueRTL) OR (S1Q:Poz AND S1Q:PropTrueRTL) THEN CYCLE.
         IF S1Q:Attr THEN !Format for AtCODE
            Poz:AtS1Attr=Poz:AtS1Attr&','&S1Q:Attr & CHOOSE(~S1Q:AParenV ,'','('&S1Q:Poz&')')
         END 
         IF S1Q:PropTxt THEN
            Poz:AtS1Prop=Poz:AtS1Prop &' '&S1Q:PropTxt &'='& S1Q:Poz
         END
    END   
  IF IsBEVEL AND (Poz:BevelOut OR Poz:BevelIn OR Poz:BevelSty) THEN
     Poz:AtS1Attr=Poz:AtS1Attr&',Bevel('&Poz:BevelOut &','& Poz:BevelIn & |
        CHOOSE(~Poz:BevelSty,'', ','& HEX8(Poz:BevelSty,4,1)) &')'
  END
  IF IsDROP THEN
     Poz:AtS1Attr=Poz:AtS1Attr&',Drop('&Poz:DropCnt & CHOOSE(~Poz:DropWd,'', ','& Poz:DropWd &')')
  END
  Poz:AtCODE='AT('& CHOOSE(~Poz:No_X,Poz:X&','  ,',') &|
     CHOOSE(~Poz:No_Y,Poz:Y&','  ,',') &|
     CHOOSE(~Poz:DeftWd,Poz:Wd&',' ,',') &|
     CHOOSE(~Poz:DeftHt,Poz:Ht&')' ,')') &|
     CHOOSE(~Poz:FullPr, '', ',FULL') &|
     CHOOSE(~AlnQ:ACode OR AlnQ:Desc[1]='~','',','&AlnQ:ACode &CHOOSE(~Poz:AOffset,'','('&Poz:AOffset&')'))  &|
     Poz:AtS1Attr
    IF IsSHEET THEN
        Poz:AtCODE=CLIP(Poz:AtCODE) & |
        CHOOSE(Poz:ShUpDn,',UP',',DOWN',',UP,DOWN','') & |
        ''
    END
    Poz:AtCODE=CLIP(Poz:AtCODE) & Poz:AtS1Prop
    DISPLAY(?Poz:AtCODE)   
!---------------------
SetPosition_Poz2PreviewRtn ROUTINE
    IF Haz=Poz THEN EXIT.
    SETTARGET(PWnd)
    IF Poz:WndResize<>Haz:WndResize THEN 0{PROP:Resize}=Poz:WndResize.
    IF Poz:WndWide<>Haz:WndWide THEN 0{Prop:Width} =Poz:WndWide.
    IF Poz:WndHigh<>Haz:WndHigh THEN 0{Prop:Height}=Poz:WndHigh.
    IF HideUnHide THEN HIDE(FEQ).   
    IF Poz:ALCRDn<>Haz:ALCRDn THEN        !Did Align change?
       GET(AlignQ,Haz:ALCRDn)             !Turn OFF Old Align First (unless None Val=0)
       IF ~ERRORCODE() AND AlnQ:Value THEN 
           Feq{AlnQ:Prop}=''
       END
       GET(AlignQ,Poz:ALCRDn)
       Feq{AlnQ:Prop}=AlnQ:Value
       IF AlnQ:Value THEN               !If Value=1 AND had an Offset
          Haz:AOffset=-1*Haz:AOffset    !Force a Reset of Offset when Align changes
       END
    END       
    IF Poz:AOffset<>Haz:AOffset AND AlnQ:PrOffset AND AlnQ:Value THEN
       Feq{AlnQ:PrOffset}=Poz:AOffset
    END

    IF Poz:X<>Haz:X AND ~Poz:No_X THEN Feq{PROP:Xpos}=Poz:X.
    IF Poz:Y<>Haz:Y AND ~Poz:No_Y THEN Feq{PROP:Ypos}=Poz:Y.

    IF IsENTRY AND Poz:Picture<>Haz:Picture THEN FEQ{PROP:Text}=CLIP(Poz:Picture).
    
    IF Poz:Items <> Haz:Items THEN
       FEQ{PROP:Items}=Poz:Items ; Poz:Ht=FEQ{PROP:Height}
    END
    IF Poz:LineHt <> Haz:LineHt THEN FEQ{PROP:LineHeight}=Poz:LineHt.

    IF Poz:Selectd<>Haz:Selectd THEN PWnd$FEQ{PROP:Selected}=Poz:Selectd ; X=1 ELSE X=0.
    IF Poz:DropCnt<>Haz:DropCnt OR Poz:DropWd<>Haz:DropWd THEN 
       X=1 ; PWnd$FEQ{PROP:Drop}=Poz:DropCnt ; PWnd$FEQ{PROP:DropWidth}=Poz:DropWd       
    END ; IF X=1 AND IsDROP THEN ListDrop(FEQ) ; Dropped=1.

    IF Haz:DeftWd + Haz:FullWd THEN Haz:Wd=0. !Clear so if uncheck Deft/Full the Width gets set
    IF Haz:DeftHt + Haz:FullHt THEN Haz:Ht=0.
    
    IF Poz:DeftWd AND Poz:DeftWd <> Haz:DeftWd THEN Feq{PROP:NoWidth}=Poz:DeftWd.
    IF Poz:DeftHt AND Poz:DeftHt <> Haz:DeftHt THEN Feq{PROP:NoHeight}=Poz:DeftHt.
    IF Poz:FullWd AND Poz:FullWd <> Haz:FullWd THEN Feq{PROP:NoWidth}=Poz:FullWd.  !Full also uses Default
    IF Poz:FullHt AND Poz:FullHt <> Haz:FullHt THEN Feq{PROP:NoHeight}=Poz:FullHt.
    IF Poz:Wd <> Haz:Wd AND 0=Poz:DeftWd + Poz:FullWd THEN Feq{PROP:Width}=Poz:Wd.
    IF Poz:Ht <> Haz:Ht AND 0=Poz:DeftHt + Poz:FullHt THEN Feq{PROP:Height}=Poz:Ht.
    IF Poz:FullPr AND Poz:FullPr<>Haz:FullPr THEN Feq{PROP:FULL}=Poz:FullPr.

    Poz:Angle10=Poz:AngleSpin*10 
    LOOP X=1 TO RECORDS(S1Q) ; GET(S1Q,X)
         IF S1Q:Poz<>S1Q:Haz THEN FEQ{S1Q:PROP}=S1Q:Poz.
    END    
    IF IsSHEET THEN 
        Poz:ShUpDn=Poz:ShUp1+Poz:ShDown2
        IF Poz:ShUpDn<>Haz:ShUpDn THEN
           IF Haz:ShUpDn THEN FEQ{Prop_UpDown[Haz:ShUpDn]}=False.  !Old Off
           IF Poz:ShUpDn THEN FEQ{Prop_UpDown[Poz:ShUpDn]}=True.   !New ON
        END
    END    
    DISPLAY 
    !All done. Now read back in any values that may have changed
    IF IsLIST THEN Poz:Items=FEQ{PROP:Items}. 
    IF HideUnHide AND ~Try2:Hide[1] THEN UNHIDE(FEQ) ; DISPLAY.
    SETTARGET()
    DO Poz:AtCODERtn
    Haz=Poz
    DISPLAY
!---------------    
FontRtn ROUTINE
    DATA
!WazFont GROUP,PRE(WzF)
!Face     STRING(64)
!Size     LONG
!Color    LONG
!Style    LONG
!CHar     LONG
!        END
!HazFont GROUP(WazFont),PRE(HzF).

FFace   STRING(64)
FSize   LONG
FColor  LONG
FStyle  LONG
FCHar   LONG
    CODE
    SETTARGET(PWnd)
    GETFONT(FEQ,FFace,FSize,FColor,FStyle,FChar)
    SETTARGET()
    !Message(FFace &'||'& FSize &' , '& FColor &' , '& FStyle &' , '& FCHar)
    IF ?FontBtn{PROP:Tip}='' THEN ?FontBtn{PROP:Tip}='Original Font:<13,10>' & FFace &|
                '<13,10>Size: '& FSize &'  Color: '& FColor&'  Style: '& FStyle&'  CharSet: '& FChar .
    IF FONTDIALOGa('Select Control Font',FFace,FSize,FColor,FStyle,FChar) THEN 
        SETTARGET(PWnd)
        SETFONT(FEQ,FFace,FSize,FColor,FStyle,FChar)
        DISPLAY
        SETTARGET()
    END    
!---------------    
ColorRtn ROUTINE
    DATA
MaxClr     EQUATE(14)             
ClrNow     LONG       ,DIM(MaxClr)
Prop:Clr   LONG       ,DIM(MaxClr)
Prop:ClrNm PSTRING(32),DIM(MaxClr)
PropName   STRING(32)
Clr        LONG
C   SHORT
Pop CSTRING(1000)
Clp STRING(1000)
  CODE
  Prop:Clr[1]=PROP:FontColor         ; Prop:Clr[5]=PROP:GradientFromColor  ; Prop:Clr[ 9]=PROPLIST:DefHdrTextColor
  Prop:Clr[2]=Prop:Color             ; Prop:Clr[6]=PROP:GradientToColor    ; Prop:Clr[10]=PROPLIST:DefHdrBackColor
  Prop:Clr[3]=Prop:SelectedColor     ; Prop:Clr[7]=Prop:Fill               ; Prop:Clr[11]=PROPLIST:HdrSortBackColor
  Prop:Clr[4]=Prop:SelectedFillColor ; Prop:Clr[8]=PROPLIST:Grid           ; Prop:Clr[12]=PROPList:HdrSortTextColor
                                                                             Prop:Clr[13]=PROPList:SortBackColor
                                                                             Prop:Clr[14]=PROPList:SortTextColor
!TODO these colors could be in a LIST so Sample could show        
  LOOP C=1 TO MaxClr
     IF C>=8 AND ~IsLIST THEN BREAK. ; IF ~Prop:Clr[C] THEN CYCLE.
     ClrNow[C]=PWnd$FEQ{Prop:Clr[C]}
     PropName=CHOOSE(C,'Prop:FontColor','Prop:Color '& CHOOSE(IsLine or FeqTypeNo=CREATE:Region,'(Border Line)','(Background)'),|
       'Prop:SelectedColor','Prop:SelectedFillColor','-|Prop:GradientFromColor','Prop:GradientToColor','-|Prop:Fill (Box Panel)', |
       '-|PropList:Grid','PropList:DefHdrTextColor','PropList:DefHdrBackColor' , |
       '-|PropList:HdrSortBackColor','PropList:HdrSortTextColor','PropList:SortBackColor','PropList:SortTextColor', |
       '?C='&C) 
     Pop=CHOOSE(C=1,'',Pop&'|') & CLIP(PropName) & |
         CHOOSE(ClrNow[C]=0 or ClrNow[C]=-1,'','<9>'& ClaColorEquate(ClrNow[C])) 
     IF SUB(PropName,1,2)='-|' THEN PropName=SUB(PropName,3,99).
     X=INSTRING('(',PropName) ; IF X THEN PropName=SUB(PropName,1,X-1).
     Prop:ClrNm[C]=CLIP(PropName)
  END
  C=POPUPunder(?ColorBtn,'Colors (Ctrl+Click to Copy)|-|' & Pop)
  IF KeyStateSCA(2) THEN 
     Clp=Pop ; ReplaceInto(Clp,'<9>',' ') ; ReplaceInto(Clp,'|','<13,10>') ; SETCLIPBOARD(Clp) ; EXIT
  END
  IF C<2 THEN EXIT. ; C -= 1
  Clr=ClrNow[C] 
  IF   Clr=-1 THEN Clr=0 
  ELSIF Clr<0 THEN Clr=GetSysColor(BAND(Clr,7Fh)) 
  END
  IF COLORDIALOG('Select Color ' & C,Clr) THEN 
     PWnd$FEQ{Prop:Clr[C]}=Clr 
     Pop='Set Color ' & Prop:ClrNm[C] & ' = '& Clr &' was '& ClrNow[C]
     PWnd$FEQ{Prop:Tip}=Pop &'<13,10>'& PWnd$FEQ{Prop:Tip}
     ?ColorBtn{PROP:Tip}=Pop &'<13,10>'& ?ColorBtn{PROP:Tip}
  END
!TODO make a LIST color tool to set individual column colors {PROPLLIST:TextColor,Col} :Backcolor :TextSelected :BackSelected
!       probably part of a LIST WYSIWYG tool
!---------------
GradRtn ROUTINE
    X=PopupUnder(?GradBtn,'~PROP:GradientType GradientTypes: (0-8)|-|' & |
    'Off|-|Vertical|Horizontal|-|Vertical Cylinder|Horizontal Cylinder|-|Diagonal Top Left|Diagonal Bottom Left|Diagonal Top Right|Diagonal Bottom Right')
    IF X<2 THEN EXIT.
    PWnd$FEQ{PROP:GradientType}=X-2
    ?GradBtn{PROP:Tip}='PROP:GradientType=' & X-2
!---------------     
AlignQLoadRtn ROUTINE
    DATA
N   STRING(5)   
    CODE
    CASE FeqTypeNo
    OF CREATE:Sheet ; DO AlignQSheetRtn ; EXIT     
    OF CREATE:Slider_MIA ; DO AlignQSliderRtn ; EXIT     
    OF CREATE:Group OROF CREATE:Option OROF CREATE:Panel OROF CREATE:Progress OROF CREATE:Tab
                    OROF CREATE:BOX OROF CREATE:Line OROF CREATE:Region OROF CREATE:Ellipse 
        N='~~~~'
    OF CREATE:Button OROF CREATE:Check OROF CREATE:State3 OROF CREATE:Radio ; N=' ~ ~'     !only LR
    OF CREATE:Prompt OROF CREATE:Text ; N[4]='~'     !No Deci
    OF CREATE:Image ; N='~~~~' !TODO CENTERED / TILED

    END
    CLEAR(AlignQ) ; AlnQ:Value=1
    AlnQ:Desc=CLIP(N[1])&'Left'   ; AlnQ:ACode='LEFT'   ; AlnQ:Prop=PROP:Left    ; AlnQ:PrOffset=PROP:LeftOffSet ; ADD(AlignQ) 
    AlnQ:Desc=CLIP(N[2])&'Center' ; AlnQ:ACode='CENTER' ; AlnQ:Prop=PROP:Center  ; AlnQ:PrOffset=AlnQ:Prop+1 ; ADD(AlignQ)
    AlnQ:Desc=CLIP(N[3])&'Right'  ; AlnQ:ACode='RIGHT'  ; AlnQ:Prop=PROP:Right   ; AlnQ:PrOffset=AlnQ:Prop+1 ; ADD(AlignQ)
    AlnQ:Desc=CLIP(N[4])&'Decimal'; AlnQ:ACode='DECIMAL'; AlnQ:Prop=PROP:Decimal ; AlnQ:PrOffset=AlnQ:Prop+1 ; ADD(AlignQ)
    AlnQ:Desc=CLIP(N[5])&'None'   ; AlnQ:ACode=''       ; AlnQ:Prop=PROP:Left    ; AlnQ:PrOffset=AlnQ:Prop+1 ; AlnQ:Value=0 ; ADD(AlignQ)
!---------------    
AlignQSheetRtn ROUTINE
    CLEAR(AlignQ) ; AlnQ:Value=1
    AlnQ:Desc='Above'  ; AlnQ:ACode='ABOVE'  ; AlnQ:Prop=PROP:Above   ; AlnQ:PrOffset=PROP:AboveSize ; ADD(AlignQ)
    AlnQ:Desc='Below'  ; AlnQ:ACode='BELOW'  ; AlnQ:Prop=PROP:Below   ; AlnQ:PrOffset=AlnQ:Prop+1 ; ADD(AlignQ)
    AlnQ:Desc='Left'   ; AlnQ:ACode='LEFT'   ; AlnQ:Prop=PROP:Left    ; AlnQ:PrOffset=PROP:LeftOffSet ; ADD(AlignQ) 
    AlnQ:Desc='Right'  ; AlnQ:ACode='RIGHT'  ; AlnQ:Prop=PROP:Right   ; AlnQ:PrOffset=AlnQ:Prop+1 ; ADD(AlignQ)
    AlnQ:Desc='None'   ; AlnQ:ACode=''       ; AlnQ:Prop=PROP:Above   ; AlnQ:PrOffset=AlnQ:Prop+1 ; AlnQ:Value=0 ; ADD(AlignQ)
AlignQSliderRtn ROUTINE
    CLEAR(AlignQ) ; AlnQ:Value=1
    IF PWnd$FEQ{PROP:Vertical} THEN 
       AlnQ:Desc='Left'   ; AlnQ:ACode='LEFT'   ; AlnQ:Prop=PROP:Left    ; AlnQ:PrOffset=0 ; ADD(AlignQ) 
       AlnQ:Desc='Right'  ; AlnQ:ACode='RIGHT'  ; AlnQ:Prop=PROP:Right   ; AlnQ:PrOffset=0 ; ADD(AlignQ)
    ELSE
       AlnQ:Desc='Above'  ; AlnQ:ACode='ABOVE'  ; AlnQ:Prop=PROP:Above   ; AlnQ:PrOffset=0 ; ADD(AlignQ)
       AlnQ:Desc='Below'  ; AlnQ:ACode='BELOW'  ; AlnQ:Prop=PROP:Below   ; AlnQ:PrOffset=0 ; ADD(AlignQ)
    END
    AlnQ:Desc='Both'   ; AlnQ:ACode='BOTH'    ; AlnQ:Prop=PROP:Both    ; AlnQ:PrOffset=0 ; ADD(AlignQ)
    AlnQ:Desc='NoTicks'; AlnQ:ACode='NoTicks' ; AlnQ:Prop=PROP:NoTicks ; AlnQ:PrOffset=0 ; ADD(AlignQ)

!---------------    
TabPickBtnRtn ROUTINE
    DATA
T   SHORT    
PUP CSTRING(1000)
Hid PSTRING(8)
Chk PSTRING(2)
ChkNo BYTE
TabFEQ LONG
    CODE
    PUP='Tabs (Ctrl+Click to Hide/Unhide)|-'
    SETTARGET(PWnd)
    ChkNo=FEQ{PROP:Selected}
    LOOP T=1 TO FEQ{PROP:NumTabs}
        TabFEQ=FEQ{PROP:Child,T}
        Chk=CHOOSE(T=ChkNo,'+','')
        Hid=CHOOSE(~TabFEQ{PROP:Hide},'','<9>(Hide)')
        PUP=PUP & '|' & Chk & TabFEQ{PROP:Text}  & Hid
!TODO PopupTextFix to Strip POPUP Text Problems like |Pipe| Leading +-~ 
    END 
    SETTARGET() 
    T=PopupBeside(?TabPickBtn,PUP)-1 ; IF T < 1 THEN EXIT.
    SETTARGET(PWnd)
    TabFEQ=FEQ{PROP:Child,T}
    IF ~KeyStateSCA(2) THEN 
        TabFEQ{PROP:Hide}=0
        FEQ{PROP:Selected}=T
    ELSE
        Hid=TabFEQ{PROP:Hide} ; TabFEQ{PROP:Hide}=1-Hid
        IF ~Hid THEN FEQ{PROP:Selected}=T. !Select if Unhiding
    END
    DISPLAY ; SETTARGET()
!---------
BevPopRtn ROUTINE
  DATA
Ib &LONG    
Ob &LONG    
Sb &LONG    
Wb LONG    
Hb LONG    
  CODE 
  Ib&=Poz:BevelIn ; Ob&=Poz:BevelOut ; Sb&=Poz:BevelSty
  EXECUTE POPUP('Rectange{{Sunk|Raised|Ridge|Valley|Entry|Button}|Line{{Vert Grab|Vert Spacer|Vert Sunk|-|Horz Grab|Hort Spacer|Horz Sunk}')
  BEGIN ; Ib=0  ; Ob=-3 ; Sb=0            ; .
  BEGIN ; Ib=0  ; Ob=3  ; Sb=0            ; .
  BEGIN ; Ib=-2 ; Ob=2  ; Sb=0            ; .
  BEGIN ; Ib=2  ; Ob=-2 ; Sb=0            ; .
  BEGIN ; Ib=-1 ; Ob=-1 ; Sb=0            ; .
  BEGIN ; Ib=1  ; Ob=1  ; Sb=0            ; .
  BEGIN ; Ib=0  ; Ob=0  ; Sb=4488h ; Wb=2 ; .
  BEGIN ; Ib=0  ; Ob=0  ; Sb=6000h ; Wb=1 ; .
  BEGIN ; Ib=0  ; Ob=2  ; Sb=6000h ; Wb=1 ; .
  BEGIN ; Ib=0  ; Ob=0  ; Sb=4488h ; Hb=2 ; .
  BEGIN ; Ib=0  ; Ob=0  ; Sb=0600h ; Hb=2 ; .
  BEGIN ; Ib=0  ; Ob=2  ; Sb=0600h ; Hb=2 ; .
  ELSE ; EXIT
  END 
  IF Wb THEN
    Poz:Wd=Wb ; Poz:Ht=B4Waz:Ht   !Rest FULL ?
    If1Clear(Poz:FullWd,?Poz:FullWd) ; If1Clear(Poz:DeftWd,?Poz:DeftWd) 
  END
  IF Hb THEN
     Poz:Ht=Hb ; Poz:Wd=B4Waz:Wd   !Rest FULL ?
     If1Clear(Poz:FullHt,?Poz:FullHt) ; If1Clear(Poz:DeftHt,?Poz:DeftHt)
  END  !; POST(EVENT:Accepted,?Poz:BevelSty)      
!---------    
TestBtnRtn ROUTINE
    CASE POPUPunder(?TestBtn,'Select Random Tab|Tab Pick|Drop List (todo)')
    OF 1 ; X=PWnd$FEQ{PROP:NumTabs} ; IF X<2 THEN EXIT. ; X=RANDOM(1,X) ; PWnd$FEQ{PROP:Selected}=X ; PWnd$0{PROP:Text}='Tab ' & X 
    OF 2 ; DO TabPickBtnRtn
    OF 3 ; SETTARGET(PWnd) ; PressKey(DownKey) ; SETTARGET() !Goes to current window. Maybe API
    END
!========================================================================================================    
CBWndPreviewClass.ResizeWindow  PROCEDURE() 
FEQ         LONG(0) 
FeqTypeNo   LONG(CREATE:window)
FeqTypeName STRING('WINDOW     ') !or  CREATE:application  24 
FeqName     STRING('WINDOW     ') !or  CREATE:application  24 
       
X       LONG,AUTO
S1Q QUEUE,PRE(S1Q)  !Simple ones
Poz     &LONG       !S1Q:Poz       
Haz     &LONG       !S1Q:Haz       
PROP    LONG        !S1Q:PROP      !{Prop:xx}
Attr    PSTRING(12) !S1Q:Attr      !eg. ANGLE
AParenV BYTE        !S1Q:AParenV   !1=Attr have (Value)
PropTxt PSTRING(24) !S1Q:PropTxt   !if no Attr then PROP
PropTrueRTL BYTE    !S1Q:PropTrueRTL   RTL Prop Default is True so must turn False eg Broken
FeqInp  LONG        !S1Q:FeqInp 
FeqPmt  LONG        !S1Q:FeqPmt
    END   
PosDataType GROUP,TYPE
X           LONG       ! Poz:X       Haz:X       Waz:X
Y           LONG       ! Poz:Y       Haz:Y       Waz:Y
Wd          LONG       ! Poz:Wd      Haz:Wd      Waz:Wd
Ht          LONG       ! Poz:Ht      Haz:Ht      Waz:Ht
No_X        BYTE       ! Poz:No_X    Haz:No_X    Waz:No_X     _nopos
No_Y        BYTE       ! Poz:No_Y    Haz:No_Y    Waz:No_Y     _nopos
Center       LONG      ! Poz:Center
MaxBox        LONG     ! Poz:MaxBox    Haz:MaxBox
Gray        LONG       ! Poz:Gray    Haz:Gray
HScroll     LONG       ! Poz:HScroll PROP:HScroll
VScroll     LONG       ! Poz:VScroll PROP:VScroll
System      LONG       ! Poz:System
Tiled       LONG       ! Poz:Tiled
Centered    LONG       ! Poz:Centered
FrmSDRN     BYTE       ! Poz:FrmSDRN     Frame Single (none) Double Resize NoFrame
AtCODE      STRING(256) ! Poz:AtCODE   Haz:AtCODE   Waz:AtCODE 
AtS1Attr   CSTRING(128) ! Poz:AtS1Attr
AtS1Prop   CSTRING(128) ! Poz:AtS1Prop
MnMxWdHt  LONG,DIM(4)      ! Poz:MnMxWdHt [1]=MinWd [2]=MaxWd [3]=MinHt
         END
Prop:MwMh  LONG,DIM(4)
Try2:Hide  BYTE,DIM(2)
HideUnHide BYTE(0),STATIC
BeforeWaz GROUP(PosDataType),PRE(B4Waz). !Original Position Waz
Poz GROUP(PosDataType),PRE(Poz). !This Screen Position
Haz GROUP(PosDataType),PRE(Haz). !Preview Position Now
PropText  STRING(255)
FrameQ QUEUE,PRE(FrmQ) !Frame choices for ?LIST:FrameQ
Desc     STRING(8)   !FrmQ:Desc  
ACode    PSTRING(8)  !FrmQ:ACode  
Prop     LONG        !FrmQ:Prop      eg Prop:Left
PrOffset LONG        !FrmQ:PrOffset  eg Prop:LeftOffset
Value    BYTE        !FrmQ:Value
       END 
Window WINDOW('WYSIWYG Window'),AT(,,249,163),IMM,SYSTEM,FONT('Segoe UI',9),RESIZE
        TOOLBAR,AT(0,0,249,28),USE(?TOOLBAR1)
            BUTTON('Save'),AT(2,1,25,12),USE(?SaveBtn),SKIP,TIP('Save size changes and Close<13,10>Tool Tip is updated w' & |
                    'ith Before and After Size.')
            BUTTON('Halt'),AT(94,1,25,12),USE(?HaltBtn)
            BUTTON('Cancel'),AT(30,1,29,12),USE(?CancelBtn),SKIP,TIP('Undo size changes and Close')
            BUTTON('Undo'),AT(2,14,25,12),USE(?UndoBtn),SKIP,TIP('Undo size changes back to Original, but stay on this window')
            BUTTON('PV'),AT(62,1,14,12),USE(?PropsBtn),SKIP,TIP('PROP View')
            BUTTON('PE'),AT(77,1,14,12),USE(?PropEdBtn),SKIP,TIP('PROP Editor')
            BUTTON('<50>'),AT(122,14,12,12),USE(?UnderBtn),SKIP,FONT('Webdings'),TIP('Move Preview under this Window')
            LIST,AT(123,2,32,10),USE(Cfg:ResizeSnapTo),FONT(,8),TIP('How to Position Preview Window under the Resizer'), |
                    DROP(5,44),FROM('Right Snap|#1|Centr Snap|#2|Left Snap|#3|None|#9')
            BUTTON('F'),AT(30,14,13,12),USE(?FontBtn),MSG('Leave off Tip'),SKIP,FONT(,10,,FONT:bold+FONT:italic)
            BUTTON('C'),AT(46,14,13,12),USE(?ColorBtn),SKIP,FONT(,10,COLOR:Red,FONT:bold)
            BUTTON('<176>'),AT(62,14,13,12),USE(?GdLinesBtn),SKIP,FONT('Wingdings',14,COLOR:GRAYTEXT,FONT:bold), |
                    TIP('Guide and Grid Lines')
            BUTTON('Test'),AT(94,14,25,12),USE(?TestBtn),SKIP
        END
        PANEL,AT(117,32,1,80),USE(?VertRightPanel),BEVEL(0,0,6000H)
        GROUP,AT(2,32,112,44),USE(?Group:AT)
            CHECK('Center'),AT(79,34),USE(Poz:Center),SKIP
            PROMPT('Frame:'),AT(3,34),USE(?FrameQ:Pmt)
            LIST,AT(27,33,44,10),USE(?List:FrameQ),MSG('Poz:FrmSDRN'),DROP(5),FROM(FrameQ),FORMAT('20L(2)@s7@')
            PROMPT('&X'),AT(6,49),USE(?XPoz:Pmt),TRN,FONT(,11)
            SPIN(@n-_6),AT(19,48,40,11),USE(Poz:X),HSCROLL,RIGHT
            PROMPT('&Y'),AT(107,49),USE(?YPoz:Pmt),FONT(,11)
            SPIN(@n-_6),AT(63,48,40,11),USE(Poz:Y),HVSCROLL,RIGHT,STEP(-1)
            PROMPT('&W'),AT(6,62),USE(?Wd:Pmt),FONT(,11)
            SPIN(@n_5),AT(19,62,40,11),USE(Poz:Wd),HSCROLL,RIGHT,RANGE(0,99999)
            PROMPT('&H'),AT(106,62),USE(?Ht:Pmt),FONT(,11)
            SPIN(@n_5),AT(63,62,40,11),USE(Poz:Ht),HVSCROLL,RIGHT,RANGE(0,99999),STEP(-1)
        END
        GROUP,AT(2,79,112,30),USE(?GroupMinMax)
            SPIN(@n_5),AT(18,80,40,11),USE(Poz:MnMxWdHt[1]),SKIP,HVSCROLL,RIGHT,TIP('Window Prop:MinWidth. Can be _NoPos' & |
                    '= 80000000h.'),RANGE(0,65535)
            SPIN(@n_5),AT(18,95,40,11),USE(Poz:MnMxWdHt[2]),SKIP,HVSCROLL,RIGHT,TIP('Prop:MaxWidth'),RANGE(0,65535)
            SPIN(@n_5),AT(62,80,40,11),USE(Poz:MnMxWdHt[3]),SKIP,HVSCROLL,RIGHT,TIP('Prop:MinHeight'),RANGE(0,65535)
            SPIN(@n_5),AT(62,95,40,11),USE(Poz:MnMxWdHt[4]),SKIP,HVSCROLL,RIGHT,TIP('Prop:MaxHeight'),RANGE(0,65535)
            STRING('Max  Min'),AT(4,79,9,27),USE(?MinMaxW),TRN,FONT(,8),ANGLE(900)
            STRING('Min  Max'),AT(106,81,9,27),USE(?MinMaxH),TRN,FONT(,8),ANGLE(2700)
        END
        CHECK('Hide/UnHide'),AT(163,31),USE(HideUnHide),SKIP,TIP('Hide, Change, then UnHide. Works better espcially with' & |
                ' groups.<13,10>TODO could have a Hide/Unhide Children of Group.')
        GROUP,AT(122,30,39,82),USE(?Group:Checks)
            CHECK('Hide'),AT(123,31),USE(Try2:Hide[2]),SKIP
            CHECK('Gray'),AT(123,41),USE(Poz:Gray),SKIP
            CHECK('MAX'),AT(123,61),USE(Poz:MaxBox),SKIP,TIP('Max Button')
            CHECK('HScroll'),AT(123,71),USE(Poz:HScroll),SKIP
            CHECK('VScroll'),AT(123,81),USE(Poz:VScroll),SKIP,TIP('HIDE then UnHide to Repaint')
            CHECK('System'),AT(123,51),USE(Poz:System),SKIP
            CHECK('Centered'),AT(123,91),USE(Poz:Centered),SKIP,DISABLE
            CHECK('Tiled'),AT(123,102),USE(Poz:Tiled),SKIP,DISABLE
        END
        ENTRY(@s255),AT(2,117,,11),FULL,USE(PropText),TIP('Prop Text')
        PROMPT('Before:'),AT(2,3),USE(?AtWaz:Pmt),HIDE
        TEXT,AT(2,3,,11),FULL,USE(B4Waz:AtCODE),SKIP,FONT('Consolas'),COLOR(COLOR:BTNFACE),TIP('Before AT()'),SINGLE
        PROMPT('After:'),AT(2,17),USE(?AtPoz:Pmt),HIDE
        TEXT,AT(2,17,,11),FULL,USE(Poz:AtCODE),SKIP,FONT('Consolas'),COLOR(COLOR:BTNFACE),TIP('After AT()'),SINGLE
        STRING('--Right--'),AT(163,47,35,10),USE(?XRight),HIDE,COLOR(0E3F6FDH)
    END
SysMenuCls SysMenuClass
EVENT:SnapToUnder  EQUATE(EVENT:User+100)
    CODE 
!Region BEFORE Open Window
    IF GloT:ResizeControl THEN Message('You have Resize Open for FEQ ' & GloT:ResizeControl ) ; RETURN.
    GloT:ResizeControl=FEQ
    DO S1QLoadRtn
    DO FrameQLoadRtn
    DO GetPositionOnceRtn ; BeforeWaz=Poz ; Haz=Poz
!EndRegion BEFORE Open Window        - Before Open(Window)
    OPEN(Window) ; SysMenuCls.Init(Window) !OPEN Window--OPEN Window--OPEN Window--OPEN Window--OPEN Window--OPEN Window--OPEN Window
!Region After OPENed Window -- Prepare for ACCEPT - After OPEN(Window)
    IF ~AtReszWind[3] THEN 0{PROP:Width}=?XRight{PROP:XPos}.
    SELF.AtSetOrSave(1, AtReszWind[]) ; 
    0{PROP:MinWidth}=?VertRightPanel{PROP:XPos}+10 ; 0{PROP:MinHeight}=100
    0{PROP:Text} = 'Window Resizer: ' & FEQ &' '& CLIP(FeqTypeName) & ' - '& FeqName ! & ' (' & FeqTypeNo   &')  '& 
    MakeOverWindow() ; DO S1QWindowOpenRtn ! HIDE/ UNHIDE /DISABLE based on Control Type being resized
    ?List:FrameQ{PROP:Selected}=Poz:FrmSDRN
    DO SyncRtn
!EndRegion After OPEN Window, Prepare for ACCEPT    
    ACCEPT
        CASE EVENT()
        OF EVENT:OpenWindow   ; SysMenuCls_SYSCOMMAND(0{PROP:Handle},SMCmd_HideUnder) ; POST(EVENT:SnapToUnder)
        OF EVENT:NewSelection ; DO AcceptOrSpinSizesRtn
        OF EVENT:CloseWindow  ; POST(EVENT:Accepted,?CancelBtn) ; CYCLE
        OF EVENT:Moved        ; POST(EVENT:SnapToUnder)
        OF EVENT:SnapToUnder  ;  SELF.SnapToPreview()
        OF Event:Rejected ; BEEP ; SELECT(?) ; CYCLE
        END
        CASE ACCEPTED()
        OF ?HaltBtn ; HALT 
        OF ?Cfg:ResizeSnapTo ; SELF.SnapToPreview() ; SELF.ConfigPut(Cfg:ResizeSnapTo)
        OF ?List:FrameQ ; Poz:FrmSDRN=CHOICE(?List:FrameQ) ; GET(FrameQ,Poz:FrmSDRN)
        OF ?SaveBtn   ; PWnd$Feq{PROP:Tip}=PWnd$Feq{PROP:Tip} &'<13,10>Before Resize: ' & CLIP(B4Waz:AtCODE) &'<13,10>After Resize: ' & CLIP(Poz:AtCODE)  
                        BREAK 
        OF ?CancelBtn ; Poz=BeforeWaz ; DO SizeChangeRtn ; BREAK
        OF ?UndoBtn   ; Poz=BeforeWaz ; DO SizeChangeRtn
        OF ?UnderBtn  ; SysMenuCls_SYSCOMMAND(0{PROP:Handle},SMCmd_MoveUnder)
        OF ?PropsBtn  ; SELF.ControlPROPs(0, CREATE:Window, 'WINDOW', 'Window') 
        OF ?PropEdBtn ; SELF.PROPsEditor(0, CREATE:Window, 'WINDOW', 'Window')
        OF ?PropText  ; PWnd$FEQ{PROP:Text}=PropText ; CYCLE
        OF ?FontBtn   ; DO FontRtn
        OF ?ColorBtn  ; DO ColorRtn 
        OF ?GdLinesBtn ;  Self.GuideLines(FEQ, FeqTypeNo, FeqTypeName, FeqName)
        OF ?Try2:Hide_2 ; PWnd$FEQ{PROP:Hide}=Try2:Hide[2] ; CYCLE 
        OF ?TestBtn ; DO TestBtnRtn 
        OF ?Poz:Tiled ; Poz:Centered=0
        OF ?Poz:Centered ; Poz:Tiled=0
        END
        IF ACCEPTED() THEN DO AcceptOrSpinSizesRtn.
    END !Accept 
    SELF.AtSetOrSave(2, AtReszWind[])
    CLOSE(Window)
    GloT:ResizeControl=0
    IF Try2:Hide[2]<>Try2:Hide[1] THEN FEQ{PROP:Hide}=Try2:Hide[1].    
    RETURN
!----------------- 
AcceptOrSpinSizesRtn ROUTINE ! OF ?Poz:X  TO ?Poz:Ht ;  DO AcceptedSizesRtn
    IF FIELD() THEN DO SizeChangeRtn.
SizeChangeRtn ROUTINE
    DO SyncRtn ; DISPLAY ; DO SetPosition_Poz2PreviewRtn ; DO Poz:AtCODERtn
    EXIT
SyncRtn ROUTINE    
    IF Poz:No_X THEN DISABLE(?Poz:X).
    IF Poz:No_Y THEN DISABLE(?Poz:Y).
!--------------------
S1QLoadRtn ROUTINE 
!  S1Q:PropTrueRTL=1 
!  CLEAR(S1Q);S1Q:Poz&= ;S1Q:Haz&= ;S1Q:PROP= ;S1Q:Attr='' ;S1Q:AParenV= ;S1Q:PropTxt='' ;S1Q:FeqInp=? ;S1Q:FeqPmt=? ;ADD(S1Q)
!  CLEAR(S1Q);S1Q:Poz&= ;S1Q:Haz&= ;S1Q:PROP= ;S1Q:Attr='' ;S1Q:FeqInp=? ;ADD(S1Q)
  CLEAR(S1Q);S1Q:Poz&=Poz:Center;S1Q:Haz&=Haz:Center ;S1Q:PROP=Prop:Center ;S1Q:Attr='Center' ;S1Q:FeqInp=?Poz:Center ;ADD(S1Q)
  CLEAR(S1Q);S1Q:Poz&=Poz:HScroll ;S1Q:Haz&=Haz:HScroll ;S1Q:PROP=Prop:HScroll ;S1Q:Attr='HScroll' ;S1Q:FeqInp=?Poz:HScroll ;ADD(S1Q)
  CLEAR(S1Q);S1Q:Poz&=Poz:VScroll ;S1Q:Haz&=Haz:VScroll ;S1Q:PROP=Prop:VScroll ;S1Q:Attr='VScroll' ;S1Q:FeqInp=?Poz:VScroll ;ADD(S1Q)
  CLEAR(S1Q);S1Q:Poz&=Poz:System ;S1Q:Haz&=Haz:System ;S1Q:PROP=Prop:System ;S1Q:Attr='System' ;S1Q:FeqInp=?Poz:System ;ADD(S1Q)
  CLEAR(S1Q);S1Q:Poz&=Poz:Centered ;S1Q:Haz&=Haz:Centered ;S1Q:PROP=Prop:Centered ;S1Q:Attr='Centered' ;S1Q:FeqInp=?Poz:Centered ;ADD(S1Q)
  CLEAR(S1Q);S1Q:Poz&=Poz:Tiled ;S1Q:Haz&=Haz:Tiled ;S1Q:PROP=Prop:Tiled ;S1Q:Attr='Tiled' ;S1Q:FeqInp=?Poz:Tiled ;ADD(S1Q)
  CLEAR(Poz)
  IF PWnd{PROP:WallPaper} THEN Poz:Centered=1 ; Poz:Tiled=1.
  Poz:Centered=1 ; Poz:Tiled=1  !turn them on TODO tak this out
  Poz:Center=1 ; Poz:System=1 ; Poz:HScroll=1 ; Poz:VScroll=1
  LOOP X=RECORDS(S1Q) TO 1 BY -1
       GET(S1Q,X) ; IF ~S1Q:Poz THEN DELETE(S1Q).
  END
  CLEAR(S1Q);S1Q:Poz&=Poz:Gray ;S1Q:Haz&=Haz:Gray ;S1Q:PROP=PROP:Gray ;S1Q:Attr='Gray' ;S1Q:FeqInp=?Poz:Gray ;ADD(S1Q)
  CLEAR(S1Q);S1Q:Poz&=Poz:MaxBox ;S1Q:Haz&=Haz:MaxBox ;S1Q:PROP=PROP:MAX  ;S1Q:Attr='MAX'; S1Q:FeqInp=?Poz:MaxBox ;ADD(S1Q)
  CLEAR(Poz)
S1QWindowOpenRtn ROUTINE
    LOOP X=1 TO RECORDS(S1Q) ; GET(S1Q,X)
        IF S1Q:FeqInp THEN ENABLE(S1Q:FeqInp) ; UNHIDE(S1Q:FeqInp).
        IF S1Q:FeqPmt THEN ENABLE(S1Q:FeqPmt) ; UNHIDE(S1Q:FeqPmt).
    END
    IF Poz:No_X THEN DISABLE(?Poz:X). ; IF Poz:No_Y THEN DISABLE(?Poz:Y).
    PropText=PWnd{PROP:Text}    

GetPositionOnceRtn ROUTINE !Setup the Initial Poz, Haz and Waz
    SETTARGET(PWnd)    
    LOOP X=1 TO 4
        Prop:MwMh[x]=CHOOSE(X,Prop:MinWidth,Prop:MaxWidth,Prop:MinHeight,Prop:MaxHeight)
        Poz:MnMxWdHt[x]=0{Prop:MwMh[x]}
        IF Poz:MnMxWdHt[x]=_nopos THEN Poz:MnMxWdHt[x]=0.
    END
    GETPOSITION(Feq,Poz:X,Poz:Y,Poz:Wd,Poz:Ht)
    IF Poz:X=_nopos THEN Poz:X=0 ; Poz:No_X=1 . 
    IF Poz:Y=_nopos THEN Poz:Y=0 ; Poz:No_Y=1 .
   
    LOOP X=1 TO RECORDS(S1Q)    !Get simple PROPs e.g. was IF IsSTRING THEN Poz:Angle10=Feq{PROP:Angle}  
         GET(S1Q,X) 
         S1Q:Poz=FEQ{S1Q:PROP}  !E.g. S1Q:Poz&=Poz:Angle10 and S1Q:PROP=PROP:Angle, so Poz:Angle10=FEQ{Prop:Angle}
    END    
    LOOP X=4 TO 1 BY -1    !Check Frame e.g. PROP:Left or if NONE the Default 
        GET(FrameQ,X) !Must Leave Q Loaded
        IF X=1 THEN 
           Poz:FrmSDRN=1
        ELSIF Feq{FrmQ:Prop} THEN  !If PROP:Double Resize NoFrame
           Poz:FrmSDRN=X ; BREAK
        END 
    END
    IF FeqTypeNo=CREATE:Progress THEN FEQ{PROP:Progress}=FEQ{PROP:RangeHigh}.
    Try2:Hide[1]=FEQ{PROP:Hide} ; Try2:Hide[2]=Try2:Hide[1]
    SETTARGET()
    DO Poz:AtCODERtn
    EXIT
Poz:AtCODERtn ROUTINE
    Poz:AtS1Attr='' ; Poz:AtS1Prop=''
    LOOP X=1 TO RECORDS(S1Q) ; GET(S1Q,X) !Get simple PROPs e.g. was IF IsSTRING THEN Poz:Angle10=Feq{PROP:Angle}  
         IF (~S1Q:Poz AND ~S1Q:PropTrueRTL) OR (S1Q:Poz AND S1Q:PropTrueRTL) THEN CYCLE.
         IF S1Q:Attr THEN !Format for AtCODE
            Poz:AtS1Attr=Poz:AtS1Attr&','&S1Q:Attr & CHOOSE(~S1Q:AParenV ,'','('&S1Q:Poz&')')
         END 
         IF S1Q:PropTxt THEN
            Poz:AtS1Prop=Poz:AtS1Prop &' '&S1Q:PropTxt &'='& S1Q:Poz
         END
    END   
    Poz:AtCODE='AT('& CHOOSE(~Poz:No_X,Poz:X&','  ,',') &|
      CHOOSE(~Poz:No_Y,Poz:Y&','  ,',') & Poz:Wd&','&Poz:Ht&')' &|
      CHOOSE(~FrmQ:ACode,'',','&FrmQ:ACode) & Poz:AtS1Attr
    Poz:AtCODE=CLIP(Poz:AtCODE) & Poz:AtS1Prop
    DISPLAY(?Poz:AtCODE)   
!---------------------
SetPosition_Poz2PreviewRtn ROUTINE
  IF Haz=Poz THEN EXIT.
  SETTARGET(PWnd)
  LOOP X=1 TO 4  !Prop:MinWidth ... 
      IF Poz:MnMxWdHt[x]<>Haz:MnMxWdHt[x] THEN 0{Prop:MwMh[x]}=CHOOSE(Poz:MnMxWdHt[x]=0,_nopos,Poz:MnMxWdHt[x]) .        
  END
  IF HideUnHide THEN HIDE(FEQ).   
  IF Poz:FrmSDRN<>Haz:FrmSDRN THEN       !Did Frame change?
     GET(FrameQ,Haz:FrmSDRN)             !Turn OFF Old Frame First (unless None Val=0)
     IF ~ERRORCODE() AND FrmQ:Value THEN Feq{FrmQ:Prop}=''.
     GET(FrameQ,Poz:FrmSDRN)
     IF FrmQ:Value THEN Feq{FrmQ:Prop}=FrmQ:Value.
  END
  IF Poz:X<>Haz:X AND ~Poz:No_X THEN Feq{PROP:Xpos}=Poz:X.
  IF Poz:Y<>Haz:Y AND ~Poz:No_Y THEN Feq{PROP:Ypos}=Poz:Y.
  IF Poz:Wd <> Haz:Wd THEN Feq{PROP:Width}=Poz:Wd.
  IF Poz:Ht <> Haz:Ht THEN Feq{PROP:Height}=Poz:Ht.
  LOOP X=1 TO RECORDS(S1Q) ; GET(S1Q,X)
       IF S1Q:Poz<>S1Q:Haz THEN FEQ{S1Q:PROP}=S1Q:Poz.
  END       
  DISPLAY !All done. Now read back in any values that may have changed
  IF HideUnHide AND ~Try2:Hide[1] THEN UNHIDE(FEQ) ; DISPLAY.
  SETTARGET()
  Haz=Poz
  DISPLAY 
!---------------    
FontRtn ROUTINE
    DATA
FFace   STRING(64)
FSize   LONG
FColor  LONG
FStyle  LONG
FCHar   LONG
    CODE
    SETTARGET(PWnd)
    GETFONT(FEQ,FFace,FSize,FColor,FStyle,FChar)
    SETTARGET()
    !Message(FFace &'||'& FSize &' , '& FColor &' , '& FStyle &' , '& FCHar)
    IF ?FontBtn{PROP:Tip}='' THEN ?FontBtn{PROP:Tip}='Original Font:<13,10>' & FFace &|
                '<13,10>Size: '& FSize &'  Color: '& FColor&'  Style: '& FStyle&'  CharSet: '& FChar .
    IF FONTDIALOGa('Select Control Font',FFace,FSize,FColor,FStyle,FChar) THEN 
        SETTARGET(PWnd)
        SETFONT(FEQ,FFace,FSize,FColor,FStyle,FChar)
        DISPLAY
        SETTARGET()
    END    
!---------------    
ColorRtn ROUTINE
    DATA
MaxClr     EQUATE(6)             
ClrNow     LONG       ,DIM(MaxClr)
Prop:Clr   LONG       ,DIM(MaxClr)
Prop:ClrNm PSTRING(32),DIM(MaxClr)
PropName   STRING(32)
Clr        LONG
C   SHORT
Pop CSTRING(1000)
Clp STRING(1000)
    CODE
    Prop:Clr[1]=PROP:FontColor         ; Prop:Clr[5]=PROP:GradientFromColor
    Prop:Clr[2]=Prop:Color             ; Prop:Clr[6]=PROP:GradientToColor
    Prop:Clr[3]=Prop:SelectedColor     !; Prop:Clr[7]=Prop:Fill    
    Prop:Clr[4]=Prop:SelectedFillColor
    LOOP C=1 TO MaxClr
         IF ~Prop:Clr[C] THEN CYCLE.
         ClrNow[C]=PWnd$FEQ{Prop:Clr[C]}
         PropName = |
            CHOOSE(C,'Prop:FontColor','Prop:Color (Background)','Prop:SelectedColor','Prop:SelectedFillColor', |
                     '-|Prop:GradientFromColor','Prop:GradientToColor','?C='&C) 
         Pop=CHOOSE(C=1,'',Pop&'|') & CLIP(PropName) & |
             CHOOSE(ClrNow[C]=0 or ClrNow[C]=-1,'','<9>'& ClaColorEquate(ClrNow[C])) 
         IF SUB(PropName,1,2)='-|' THEN PropName=SUB(PropName,3,99).
         X=INSTRING('(',PropName) ; IF X THEN PropName=SUB(PropName,1,X-1).
         Prop:ClrNm[C]=CLIP(PropName)
    END
    C=POPUPunder(?ColorBtn,'Colors (Ctrl+Click to Copy)|-|' & Pop)
    IF KeyStateSCA(2) THEN 
       Clp=Pop ; ReplaceInto(Clp,'<9>',' ') ; ReplaceInto(Clp,'|','<13,10>') ; SETCLIPBOARD(Clp) ; EXIT
    END    
    IF C<2 THEN EXIT. ; C -= 1
    Clr=ClrNow[C] 
    IF   Clr=-1 THEN Clr=0 
    ELSIF Clr<0 THEN Clr=GetSysColor(BAND(Clr,7Fh)) 
    END
    IF COLORDIALOG('Select Color ' & Prop:ClrNm[C],Clr) THEN 
       PWnd$FEQ{Prop:Clr[C]}=Clr ; Message('Set Color ' & Prop:Clr[C] & ' = '& Clr &' was '& ClrNow[C] )
    END 
!---------------    
FrameQLoadRtn ROUTINE
    DATA  
    CODE
    CLEAR(FrameQ) !so FrmQ:Value=0 
    FrmQ:Desc='Single' ; FrmQ:ACode=''       ; FrmQ:Prop=PROP:Double  ; ADD(FrameQ)    
    FrmQ:Value=1
    FrmQ:Desc='Double' ; FrmQ:ACode='DOUBLE' ; FrmQ:Prop=PROP:Double  ; ADD(FrameQ) 
    FrmQ:Desc='Resize' ; FrmQ:ACode='RESIZE' ; FrmQ:Prop=PROP:Resize  ; ADD(FrameQ)
    FrmQ:Desc='NoFrame'; FrmQ:ACode='NoFrame'; FrmQ:Prop=PROP:NoFrame ; ADD(FrameQ)
    FrmQ:Desc='Single' ; FrmQ:ACode=''       ; FrmQ:Prop=PROP:Double  ; FrmQ:Value=0 ; ADD(FrameQ,1)
!---------------    
TestBtnRtn ROUTINE
    IF POPUP('No test') .
!=====================================================================
CBWndPreviewClass.SnapToPreview  PROCEDURE() !was SnapToRtn ROUTINE !TODO make this a single procedure?
Rx LONG
Ry LONG
Rw LONG
Rh LONG
Px LONG
Py LONG
Pw LONG
Ph LONG
    CODE
    IF Cfg:ResizeSnapTo=9 THEN RETURN.
    0{PROP:Pixels}=1 ; GETPOSITION(0,Rx,Ry,Rw,Rh) ; 0{PROP:Pixels}=0
    SETTARGET(PWnd) ; 0{PROP:Pixels}=1 ; GETPOSITION(0,Px,Py,Pw,Ph)
    CASE Cfg:ResizeSnapTo
    OF 1 ; Py=Ry ; Px=Rx+Rw+10     !Right
    OF 2 ; Py=Ry ; Px=Rx+Rw/2-Pw/2 !Center   
    OF 3 ; Py=Ry ; Px=Rx-Pw-10     !Left     
    END
    SETPOSITION(0,Px,Py,Pw,Ph)
    0{PROP:Pixels}=0
    DISPLAY
    SETTARGET()
    RETURN
!=====================================================================
ClaFeqName PROCEDURE(LONG F)!,STRING
c CSTRING(80)
  CODE
  IF ~F then return 'Window'. ; c=ClaFieldNameRTL(F)
  return choose(c<=' ','__Feq_'& F &'__',c)
!------------------------------
HelpCW PROCEDURE(STRING HelpTxt, BYTE IsPROP)
CWHlp CwHelpCls
X LONG,AUTO
  CODE
  IF IsPROP THEN
     IF UPPER(SUB(HelpTxt,1,5))='LIST:' THEN  HelpTxt='Prop' & HelpTxt.
     X=INSTRING(',',HelpTxt) ; IF X THEN HelpTxt=SUB(HelpTxt,1,X-1). !E.g. Child,2
  END ; CWHlp.OpenHelp(LEFT(HelpTxt))
!------------------------------
Hex8 PROCEDURE(LONG _Lng, SHORT Digits=0, BYTE AddH=1,BYTE SpaceAt5=0)!,STRING
LngAdj  LONG,AUTO 
L       BYTE,DIM(4),OVER(LngAdj)
Hex     STRING('0123456789ABCDEF'),STATIC
HX      STRING(9),AUTO 
HxDig   SHORT,AUTO
  CODE
    LngAdj = BAND(BSHIFT(_Lng, -4),0F0F0F0Fh) + 01010101h
    HX[1]=HEX[L[4]]       !about 9% faster, 70 over 9MM
    HX[3]=HEX[L[3]]
    HX[5]=HEX[L[2]]
    HX[7]=HEX[L[1]]
    LngAdj=BAND(_Lng,0F0F0F0Fh)  + 01010101h
    HX[2]=HEX[L[4]]
    HX[4]=HEX[L[3]]
    HX[6]=HEX[L[2]]
    HX[8]=HEX[L[1]]
    HX[9]='h'                      !   ~BAND(_Lng,0f0000000h)
    IF Digits < 0 AND Digits > -8 !!AND HX[8]='0' THEN    !-4=4 minimum, 0 and -1 = 1 Digit Minimum  Negive is minimum
       Digits = -1 * Digits 
!       LOOP HxDig=8-Digits TO 1 BY -1 ; IF HX[HxDig]='0' THEN BREAK. ; END      
       LOOP HxDig=0 TO 7-Digits ; IF HX[HxDig+1]<>'0' THEN BREAK. ; END 
       HxDig=8-HxDig
       
  IF 0 and MESSAGE('Hx=' & Hx &'|Digits=' & Digits & '|HxDig='& HxDig &'<13,10>(8+HxDig)=' & 8-HxDig & |
             '<13,10>BAND(Digits,2)=' & BAND(Digits,2) & |
             '<13,10>BAND(HxDig,1)=' & BAND(HxDig,1) , 'Hex8',,'Continue |Halt',,MSGMODE:CANCOPY+MSGMODE:FIXEDFONT)=2 THEN HALT().

                        !Want Even     but Not Even
       IF HxDig < 8 AND ~BAND(Digits,1) AND BAND(HxDig,1) THEN HxDig+=1. !-2-4-6 return Even digits 
!??? should I just do it for 2 or 4,        
    ELSE    
       HxDig=Digits 
    END
    IF HxDig < 1 OR HxDig > 8 THEN HxDig=8.
    IF SpaceAt5 AND HxDig=8 THEN 
       RETURN(HX[1:4] & CHOOSE(SpaceAt5<=32,' ',CHR(SpaceAt5)) & HX[5 : 8+AddH])
    END
    RETURN HX[ 9 - HxDig : 8+AddH ]
!----------------
Binary8    PROCEDURE(LONG  Num)!,STRING
D       USHORT(33)
BS      STRING(33),AUTO
BB      BYTE,DIM(32),OVER(BS) 
    code 
   ! IF ~Num THEN RETURN '0b'.
    BS[32:33]='0b'
    LOOP WHILE Num 
         D -= 1
         IF BAND(Num,1)
            BB[D]=31h
         ELSE
            BB[D]=30h
         END
         Num = BSHIFT(Num,-1)
    END
    IF D=33 THEN D=32.
    RETURN BS[D : 33]
!----------------
HexEval     Procedure (*string Hx, Byte D)!,LONG 
L   LONG(0)
S   BYTE(0)
X   BYTE,AUTO
B   BYTE,AUTO
    CODE
?   ASSERT(D>=1 AND D<=9 AND D<=SIZE(Hx),'HexEval 8 Max Digits D=' & D) 
    Loop X=D TO 1 BY -1
       B = VAL(Hx[X])
       Case B
       Of 30h To 39h  ; B -= 30h       ! 0-9   B= 0-9     65-70
       Of 41h To 46h  ; B -= (41h-10)  ! A-F   B= 10-15   48-57       
       Of 61h To 66h  ; B -= (61h-10)  ! a-f   B= 10-15  
       Of 48h OrOf 68h OrOf 32 ; CYCLE !H h Space are
       Else 
?       ASSERT(0,'HexEval Non Hex "' & Hx[X] &'" Val='& VAL(Hx[X])  &' in '& Hx & '  B=' & B  ) 
            cycle
       End
       L = BOR(L, BSHIFT(B,S)) 
       S += 4
    End 
    Return L
!---------------- 
ClaColorEquate       PROCEDURE(*STRING Clr, BYTE Verbose=1) !Take String Property and Chnage to Equate
CN      LONG,AUTO
    CODE
    IF NUMERIC(Clr) THEN 
       CN = Clr
       Clr = ClaColorEquate(CN, Verbose) 
    END   
    RETURN
ClaColorEquate       PROCEDURE(LONG CN, BYTE Verbose=1)!,STRING
Clr     STRING(80)
RGB     GROUP,OVER(CN)
R           BYTE
G           BYTE
B           BYTE
            END !BYTE
I       LONG,AUTO
E       LONG,AUTO
ClrHx   STRING(9)          !6 Hex Name  Space   16 Color Equates
ColorEQT  STRING('000000Black 000080Maroon 008000Green 008080Olive 0080FFOrange 800000Navy 800080Purple 808000Teal 808080Gray C0C0C0Silver' &|
                 ' 0000FFRed 00FF00Lime 00FFFFYellow FF0000Blue FF00FFFuchsia FFFF00Aqua FFFFFFWhite' & |
                 ' E16941RoyalBlue B48246SteelBlue EBCE87SkyBlue 00DEEBEFSand 00D8E9ECLightSand 00E0E0E0LightGray ')                 
    CODE
    ClrHx = Hex8(CN, 6)
    IF CN=-1 THEN 
       Clr='-1 Color:None' ; RETURN Clr
    ELSIF CN >= 0 THEN 
!TODO use new Find function    
       I=INSTRING(ClrHx[1:6],ColorEQT,1)    
       IF I THEN 
          E=INSTRING(' ',ColorEQT,1,I+6)
          IF E THEN Clr=CLIP(Clr) &' Color:'& ColorEQT[I+6 : E].          
       END 
       IF Verbose THEN Clr=CLIP(Clr) &'  RGB('& RGB.R &','& RGB.G &',' & RGB.B &')  '& ClrHx.       
    ELSE !Negative color is System Color. Clarion Equate (80000001H) 80h and Index in Red Byte
       Clr=CHOOSE(RGB.R+1,'Scrollbar','Background','ActiveCaption','InactiveCaption','Menu','Window',|
            'WindowFrame','MenuText','WindowText','CaptionText','ActiveBorder','InactiveBorder',|
            'AppWorkspace','Highlight','HighlightText','BtnFace','BtnShadow','GrayText','BtnText',|
            'InactiveCaptionText','BtnHighlight',|
            '3DDkShadow','3DLight','InfoText','InfoBackground','Hotlight','GradientActiveCaption','GradientInactiveCaption',|
            'MenuHighlight','MenuBar','System:' & RGB.R ) 
       Clr='Color:' & Clr 
       IF Verbose THEN Clr=CLIP(Clr) &'  SysColor('& RGB.R &')  '& Hex8(CN).
    END 
    RETURN CLIP(Clr)
 !------------------------------- 
ClaControlTypeName PROCEDURE(LONG CtrlPropType)!,STRING  !Pass FEQ{PROP:Type} to get name e.g. Button
TypeName    STRING(24)
    CODE        
    TypeName=CHOOSE(BAND(CtrlPropType,0FFh)+1,'Window', | 
        'SString','String','Image','Region','Line','Box','Ellipse','Entry','Button','Prompt',|          !1-10
        'Option','Check','Group','List','Combo','Spin','Text','Custom','Menu','Item',|                  !11-20
        'Radio','MenuBar','Create23','Application','Window','Report','Header','Footer','Break','Form',| !21-30
        'Detail','Ole','Droplist','Dropcombo','Progress','Slider','Sheet','Tab','Panel','TextRtf',|     !31-40
        'Text_SingleLine','Check3','Type?' & CtrlPropType)
    CASE CtrlPropType
    OF CREATE:ToolBar   ;  TypeName='ToolBar'  !ToolBar is 128
    OF CREATE:SubList   ;  TypeName='SubList-' & TypeName     !DropList is 270=10Eh CREATE:sublist  EQUATE(CREATE:list + 0100H) 
    OF CREATE:SubList+1 ;  TypeName='ButtonDrop-' & TypeName  !DropButto  is 271=10Fh CREATE:sublist  EQUATE(CREATE:list + 0100H) 
    OF 100h TO 100h+42   !CREATE:sublist  EQUATE(CREATE:list + 0100H)
       TypeName='SubList-' & TypeName    
    END 
!dbg    TypeName=CLIP(TypeName)&' #' & Hex8(CtrlPropType,-1) &' '& CtrlPropType  !Debug
    RETURN CLIP(TypeName)
!------------------------------
ClaKeyCodeExplain   PROCEDURE(LONG K, BYTE Terse=0)!,STRING  !Explain KeyCode() Alt+CtrL+Shift
!R       PSTRING(44),AUTO 
HB      BYTE,AUTO           !Char as Byte
HC      STRING(1),OVER(HB)  !CHAR
KN      PSTRING(32)
HexOfK  STRING(5),AUTO
KeyEQT  STRING(' 01MouseLeft 02MouseRight 03MouseCenter 05MouseLeft2 06MouseRight2 07MouseCenter2 ' &|
         '81MouseLeftUp 82MouseRightUp 83MouseCenterUp 08Backspace 08BS 09Tab 0DEnter 13Pause 14CapsLock ' &|
         '1BEsc 20Space 21PgUp 22PgDn 23End 24Home 25Left 26Up 27Right 28Down 2CPrint 2DInsert ' &|
         '2EDelete 5BWinLeft 5CWinRight 5DApps 6AAst 6BPlus 6DMinus 6EDecimal 6FSlash ' & |
         'BA;VK_OEM_1 BB=VK_OEM_Plus BC,VK_OEM_Comma BD-VK_OEM_Minus BE.VK_OEM_Period BF/VK_OEM_2 C0`VK_OEM_3 DB[VK_OEM_4 DC\VK_OEM_5 DD]VK_OEM_6 DE''VK_OEM_7 E2VK_OEM_102 ' &|
         ' '),STATIC 
!TODO some keys missing like \ 
    CODE                !12345  HHHHh  Alt+Ctrl+Shift+###  Chr:X
    HB    = BAND(K,0FFh) 
    HexOfK = Hex8(K,-4) 
    CASE HB                          !Make a Key Name
    OF 30h TO 39h ; KN = 'Key' & HC              !Key0 - Key9
    OF 41h TO 5Ah ; KN = HC & 'Key'              !AKey - ZKey
    OF 60h TO 69h ; KN = 'KeyPad' & HB-60h       !KeyPad0  - KeyPad9 
    OF 70h TO 7Bh ; KN = 'F' & HB-70H+1 & 'Key'  !F1Key - F12Key 7Bh - F13-F24
!no    OF 7Ch TO 87h ; KN = 'VK_F' & HB-70H+1       !VK_F13 - VK_F24 
    ELSE
        KN=EquateStringFind(' '&HexOfK[3:4],KeyEQT)
        IF ~KN THEN 
            KN=HB&'='&HC
        ELSIF KN[1:3]<>'Mou' THEN
            KN=KN & 'Key'
        END
    END !CASE HB
    IF K > 255 THEN 
        KN = CHOOSE(~BAND(K,400h),'','Alt+')   & |
             CHOOSE(~BAND(K,200h),'','Ctrl+')  & |
             CHOOSE(~BAND(K,100h),'','Shift+') & KN
    END
    IF Terse THEN RETURN CHOOSE(~KN,HexOfK,CLIP(KN)).
    RETURN K &'  '& HexOfK &'  '& CLIP(KN)
!------------------------------------------
ClaAlign PROCEDURE(LONG FEQ, LONG CtrlType)!,STRING   !Pass FEQ,{PROP:Type} to get Left,R,C,D
AX          USHORT,AUTO
PropCLDR    LONG,AUTO
OffsetNo    SHORT,AUTO
CLDR        STRING(1)
Offset      PSTRING(8)
SheetA      PSTRING(8)
    CODE
    !IF ~CtrlType THEN CtrlType=FEQ{PROP:Type}.
    IF CtrlType = CREATE:Sheet THEN DO SheetRtn.
    IF FEQ{PROP:Centered} THEN CLDR='c'.
    LOOP AX = 1 TO 4
         PropCLDR = PROP:Center + (AX-1) * 2 ! PROP:Center Left Dec Right by 2s with Offset
         IF ~FEQ{PropCLDR} THEN CYCLE.
         CLDR=SUB('CLDR',AX,1)
         OffsetNo = ParenWrap(FEQ{PropCLDR+1})      !PROP:CenterOffSet .. RightOffSet
!         IF OffsetNo THEN Offset='('& OffsetNo &')'.
         BREAK 
    END
    RETURN CLDR & Offset
    
SheetRtn ROUTINE  !Align shows Above/Below/Right/Left Up/Down wnshj for various SHEET props
    LOOP AX=1 TO 7 + 5
        PropCLDR=CHOOSE(AX,PROP:Above,PROP:Below,PROP:Left,PROP:Right,  PROP:Up,PROP:Down,PROP:UpsideDown, |
                           PROP:Wizard,PROP:NoSheet,PROP:Spread,PROP:Hscroll,PROP:Join)
        IF FEQ{PropCLDR} THEN
           SheetA=SheetA & CHOOSE(AX,'A','B','L','R',  'U','D','UD',  'w','n','s','h','j','?')
           IF AX <= 2 THEN  !PROP:Above or PROP:Below 
              OffsetNo = FEQ{PropCLDR+1}      
              IF OffsetNo THEN Offset = '('& OffsetNo &')'.
           END 
        END 
    END
    RETURN SheetA & Offset
!--------------------
ClaDataType PROCEDURE(*? UAny, *STRING TypeName, *BYTE HasValue, *LONG USize)!,LONG
UFO &UFOType
UAddr LONG,AUTO 
T LONG,AUTO
S LONG,AUTO
M LONG,AUTO
N PSTRING(24)
L USHORT
  CODE
  UAddr=ADDRESS(UAny) ; UFO&=(UAddr) ; IF UAddr=0 OR UFO &= NULL THEN TypeName='Null?' ; RETURN 0.
  T=UFO._Type(UAddr) ; S=UFO._Size(UAddr) ; M=UFO._Max(UAddr)
  HasValue=CHOOSE(T>=1 AND T<=20 AND T<>15) !ANY Value  BYTE - PSTRING w/o UFO=15 
  USize=S
  CASE T
  OF 29 ; N='PICTURE' ; HasValue=1 !ends up decimal
  OF 31 TO 39 ; N='Reference' !'ANY_&REF' !REFERENCE FILEREF KEYREF QUEUEREF CLASSREF WINDOWREF VIEWREF BLOBREF NAMEREF
  OF 40 ; N='LIKE'           !LibSrc XMLType.inc CLType 
  OF 41 ; N='TYPE'
  OF 42 ; N='BSTRING' 
  OF 43 ; N='ASTRING' ; HasValue=1
  OF 45 ; N='VARIANT' 
  ELSE                     !  1       2       3       4      5      6       7       8       9      10          11
    N=CHOOSE(T+1,'EndGroup','BYTE','SHORT','USHORT','DATE','TIME','LONG','ULONG','SREAL','REAL','DECIMALd','PDECIMALd',|
               'Data12','BFLOAT4','BFLOAT8','UFO','Data16','Data17','STRINGs','CSTRINGs','PSTRINGs','MEMOs',|
               'GROUPs','CLASS','Data23','Data24','QUEUE','BLOB','Data#'&T)  !Data#28
  END
  L=LEN(N)             
  IF N[L]='s' THEN N=N[1:L-1] &'(' & S &')'.
  IF N[L]='d' THEN N=N[1:L-1] &'(' & S * 2 &',)'.   
  TypeName=N & CHOOSE(M=0,'',' Dim[' & M & ']')  ; IF M>1 THEN HasValue=0.
  RETURN T
!-----------------
ClaFont4 PROCEDURE(LONG F, *PSTRING[] Fnt)
CN  LONG,AUTO
CE  PSTRING(24)
    CODE
    Fnt[1]=CLIP(F{PROP:FontName})
    Fnt[2]=CLIP(' '&F{PROP:FontSize})
    Fnt[3]=CLIP(' ' &ClaFontStyle(F{PROP:FontStyle}))  !normally 4
    CN=F{PROP:FontColor} ; IF CN>-1 THEN Fnt[4]=' ' & ClaColorEquate(CN,0) ELSE Fnt[4]=''.
    RETURN
ClaFont4Net PROCEDURE(*PSTRING[] WF, *PSTRING[] CF)!,STRING
X   USHORT,AUTO
    CODE
    LOOP X=1 TO 4 ; IF CF[X]=WF[X] THEN CF[X]=''. ; END
    RETURN LEFT(CF[1] & CF[2] & CF[3] & CF[4])
    
ClaFont PROCEDURE(LONG F)!,STRING
CN  LONG,AUTO
CE  PSTRING(24)
    CODE
    CN=F{PROP:FontColor} ; IF CN>-1 THEN CE=ClaColorEquate(CN).
    RETURN CLIP(F{PROP:FontName} &' '& F{PROP:FontSize} &' '& ClaFontStyle(F{PROP:FontStyle}) &' '& CE)
ClaFontStyle PROCEDURE(LONG FS)!,STRING
N   PSTRING(24)
W   SHORT,AUTO
    CODE
     W=BAND(FS,7FFh)
     CASE W   !could do ranges 
     OF FONT:thin     ; N='Thin'        !100 
     OF FONT:regular !; N='' !'Regular' !400
     OF FONT:bold     ; N='Bold'        !700
     OF FONT:fixed    ; N='Fixed'
     OF 0            !; N=''
     ELSE             ; N='w' & W
     END
     N=N & CHOOSE(~BAND(FS,FONT:italic),'','Italic') |
         & CHOOSE(~BAND(FS,FONT:underline),'','Uline') |
         & CHOOSE(~BAND(FS,FONT:strikeout),'','SkOut') 
     RETURN N
!FW_Thin 100, Extralight 200, Light 300, Normal 400, Medium 500, SemiBold 600, Bold 700, ExtraBold 800, Heavy 900
!------------------------------------------    
ClaIconEquate PROCEDURE(*STRING IcoProp)
EQ STRING('<1,1>Application <1,2>Hand <1,3>Question <1,4>Exclamation <1,5>Asterisk <2,1>Pick <2,2>Save <2,3>Print <2,4>Paste <2,5>Open <2,6>New <2,7>Help '&|
   '<2,8>Cut <2,9>Copy <2,10>Child <2,11>Frame <2,12>Clarion <2,13>NoPrint <2,14>Zoom <2,15>NextPage <2,16>PrevPage <2,17>JumpPage <2,18>Thumbnail <2,19>Tick <2,20>Cross '&|
   '<2,21>Connect <2,22>Print1 <2,23>Ellipsis <2,81H>VCRtop <2,82H>VCRrewind <2,83H>VCRback <2,84H>VCRplay <2,85H>VCRfastforward <2,86H>VCRbottom <2,87H>VCRlocate ') !<0FFH, x,x, 7Fh>
B LONG,AUTO
E LONG,AUTO
    CODE
    IF VAL(IcoProp[1])=255 THEN
       IF VAL(IcoProp[4])=7Fh AND ~IcoProp[5:13] THEN  !<FF,x,x,7F>
          B=INSTRING(IcoProp[2:3],EQ,1)
          IF B THEN
             B+=2 ; E=INSTRING(CHR(32),EQ,1,B)
             IF E>B THEN IcoProp='ICON:' & EQ[B : E-1].
          END
       ELSIF IcoProp[2:13]='<1,0,0>' THEN  IcoProp='ICON:BLANK'  !<0FFH,01H,00H,00H>
       END 
    END 
    RETURN
!------------    
ClaCursorEquate PROCEDURE(*STRING CurProp)
EQ STRING('<1,1>Arrow <1,2>IBeam <1,3>Wait <1,4>Cross <1,5>UpArrow <1,81H>Size <1,82H>Icon <1,83H>SizeNWSE '&|
          '<1,84H>SizeNESW <1,85H>SizeWE <1,86H>SizeNS <1,8AH>Hand <2,1>DragWE <2,2>Drop <2,3>NoDrop <2,4>Zoom ') !<0FFH, x,x, 7Fh>
B LONG,AUTO
E LONG,AUTO
    CODE
    IF VAL(CurProp[1])=255 THEN
       IF VAL(CurProp[4])=7Fh AND ~CurProp[5:13] THEN  !<FF,x,x,7F>
          B=INSTRING(CurProp[2:3],EQ,1)
          IF B THEN
             B+=2 ; E=INSTRING(CHR(32),EQ,1,B)
             IF E>B THEN CurProp='CURSOR:' & EQ[B : E-1].
          END
       ELSIF CurProp[2:13]='<1,0,0>' THEN CurProp='CURSOR:BLANK'
       END 
    END 
    RETURN    
!--------------------------------- 
ClaPicture PROCEDURE(LONG CtrlFEQ, LONG CtrlType)
    CODE
    CASE CtrlType
    OF CREATE:Entry OROF CREATE:Combo OROF CREATE:Spin OROF CREATE:SString
       RETURN CtrlFEQ{PROP:Text}
    END
    RETURN ''
!--------------------------------- 
ClaSTDprop PROCEDURE(LONG PropStd)!,STRING,PRIVATE
    CODE
    RETURN 'STD:' & CHOOSE(PropStd,'WindowList','TileWindow','CascadeWindow','ArrangeIcons','HelpIndex','HelpOnHelp','HelpSearch',|
                           'Help','Cut','Copy','Paste','Clear','Undo','Close','PrintSetup','TileHorz','TileVert','?'&PropStd)
!---------------------------------
GroupMoveChildren  PROCEDURE(LONG FrmGrp, LONG ToGrp, LONG XAdjust=0, LONG YAdjust=0)
!GroupMoveChildren   PROCEDURE(LONG FromGroup, LONG ToControl, LONG XAdjust=0, LONG YAdjust=0)
DX LONG,AUTO  !Destination (To), then Delta
DY LONG,AUTO
FX LONG,AUTO
FY LONG,AUTO
Ndx LONG,AUTO
ChildFEQ LONG,AUTO
  CODE
  GETPOSITION(ToGrp,DX,DY) ; DX+=XAdjust ; DY+=YAdjust
  GETPOSITION(FrmGrp,FX,FY)
  SETPOSITION(FrmGrp,DX,DY) ! Message('To AT( ' & DX &','& DY &'||From  AT( ' & FX &','& FY &'||Delta AT( ' & DX-FX &','& DY-FY) 
  DX -= FX ; DY -= FY
  LOOP Ndx=1 TO 999
     ChildFEQ=FrmGrp{PROP:Child,Ndx} ; IF ~ChildFEQ THEN BREAK.
     GETPOSITION(ChildFEQ,FX,FY)
     SETPOSITION(ChildFEQ,FX+DX,FY+DY) ! IF Ndx=1 THEN  Message('Idx 1 To From  AT( ' & FX &','& FY &'||TO AT( ' & DX+FX &','& DY+FY) .
  END
  RETURN
!----------
If1Clear PROCEDURE(*? TFV, LONG AcceptFEQ)!,BOOL
  CODE
  IF ~TFV THEN RETURN False.
  CLEAR(TFV) ; IF AcceptFEQ THEN POST(EVENT:Accepted,AcceptFEQ). ; RETURN True
!---------
KeyStateSCA PROCEDURE(BYTE S1C2A4)
  CODE !Shft 0100h, Ctrl 0200h, Alt 0400h
  RETURN BSHIFT(BAND(KEYSTATE(),BSHIFT(S1C2A4,8)),-8)
!-----------
LineHt PROCEDURE(LONG F, SHORT L)
  CODE ; F{PROP:LineHeight}=L+F{PROP:LineHeight}
!------------------  
ListColumnPositions PROCEDURE(LONG ListFEQ, *QUEUE PosQueue) 
EipFEQ LONG,AUTO
ListX  LONG,AUTO 
ListY  LONG,AUTO 
CPQ GROUP,PRE(),AUTO 
P LONG,DIM(4)
    END      
C USHORT,AUTO
  CODE 
  SETTARGET(PWnd)
  GETPOSITION(ListFEQ,ListX,ListY)
  EipFEQ=CREATE(0,CREATE:Button)  
  LOOP C=1 TO ListFEQ{PROPList:Exists,0}
     ListFEQ{PROP:Edit,C}=EipFEQ 
     GETPOSITION(EipFEQ,P[1],P[2],P[3],P[4]) ; ListFEQ{PROP:Edit,C}=0
     P[1]+=ListX ; P[2]+=ListY
     PosQueue=CPQ ; ADD(PosQueue)
  END 
  DESTROY(EipFEQ) ; SETTARGET()  
  RETURN 
ListDrop PROCEDURE(LONG F, BYTE U=0)  !0=Dn 1=Up
  CODE  !CB_SHOWDROPDOWN = &H14F
  PostMessage(PWnd$F{PROP:Handle},14Fh,U-1,0)
ListHelpCW PROCEDURE(LONG BtnTip=0)
Nm PSTRING(64),DIM(12)
Hlp PSTRING(64),DIM(12)
N  USHORT
P  USHORT
PM CSTRING(2000)
  CODE 
  IF BtnTip THEN |
     BtnTip{PROP:Tip}='Click for Clarion Help Topics on LIST & FORMAT<13,10>={34}' & |
                      '<13,10>Format Modifiers:<13,10>'&ListHelpMods() ; RETURN.
  N+=1 ; Nm[N]='Help Topics - Click here for FORMAT Modifiers|-' !  ; Hlp[N]='~List__declare_a_window_list_control_.htm'
  N+=1 ; Nm[N]='LIST'   ; Hlp[N]='~List__declare_a_window_list_control_.htm'
  N+=1 ; Nm[N]='COMBO'  ; Hlp[N]='~Combo__declare_an_entry_list_control_.htm'
  N+=1 ; Nm[N]='FROM (set LIST data source)'         ; Hlp[N]='~From__set_listbox_data_source_.htm'
  N+=1 ; Nm[N]='-|FORMAT (set LIST or COMBO layout)' ; Hlp[N]='~format__set_list_or_combo_layout_.htm'
  N+=1 ; Nm[N]='FORMAT() Runtime Properties'         ; Hlp[N]='~Format___other_list_box_properties.htm'  
  N+=1 ; Nm[N]='FORMAT() Other List Box Properties'  ; Hlp[N]='~Format___other_list_box_properties.htm'  
  N+=1 ; Nm[N]='-|List Control Properties  (IDE)'    ; Hlp[N]='~List_control_properties.htm'  
  N+=1 ; Nm[N]='List Box Formatter Dialog  (IDE)'    ; Hlp[N]='~asl7a.htm'  
  N+=1 ; Nm[N]='-|PROPSTYLE - LIST Style Properties' ; Hlp[N]='~Format___style_properties.htm'  
  N+=1 ; Nm[N]='LIST Mouse Click Properties'         ; Hlp[N]='~format___list_box_mouse_click_properties.htm'  
  N+=1 ; Nm[N]='PROPLIST - Properties Index'         ; Hlp[N]='~Proplist_properties_index.htm'
  PM=Nm[1] ; LOOP P=2 TO N ; PM=PM &'|'& Nm[P] ; END
  P=POPUPunder(?, PM) ; IF P>1 THEN HelpCW(Hlp[P]).; IF P=1 THEN MESSAGE(ListHelpMods(),'FORMAT Modifiers',ICON:Help).
ListHelpMods PROCEDURE()
  CODE
  RETURN '<13,10> Pipe = Right Border   M=Resizable <13,10> _ = Underline <13,10> / = LastOnLine <13,10> ? = Locator <13,10> # = FieldNo <13,10,13,10> * = Colors in Q <13,10> B(c)=BarFrame Color ' & |
 '<13,10> E(fbsb)=Color Defaults <13,10>  <13,10> F = Fixed (cannot scroll) <13,10> I = Icon <13,10> J = Icon Transparent <13,10> M = Resizable     |=RightBorder <13,10> P = Tip Cell QText' & |
 '<13,10> Q = Tip "Default" Column <13,10> S(w)=ScrollBar <13,10> T(s)=Tree (Surpress: B=Boxes I=Indent L=Lines R=Root 1=Offset) <13,10> Y = Cell Style No. in Q <13,10> Z(#) = Column Style' &|
 '<13,10,13,10> Queue Order: *Color - Icon index - Tree level - Y style code - P tip text'  
!  RETURN '<13,10>Pipe = Right Border   M=Resizable <13,10>_ = Underline <13,10>/ = LastOnLine <13,10>? = Locator <13,10># = FieldNo <13,10> <13,10>* = Colors in Q <13,10>B(c)=BarFrame Color ' & |
! '<13,10>E(fbsb)=Color Defaults <13,10> <13,10>F = Fixed (cannot scroll) <13,10>I = Icon <13,10>J = Icon Transparent <13,10>M = Resizable     |=RightBorder <13,10>P = Tip Cell QText' & |
! '<13,10>Q = Tip "Default" Column <13,10>S(w)=ScrollBar <13,10>T(s)=Tree (Surpress: B=Boxes I=Indent L=Lines R=Root 1=Offset) <13,10>Y = Cell Style No. in Q <13,10>Z(#) = Column Style' &|
! '<13,10><13,10>Queue Order: *Color - Icon index - Tree level - Y style code - P tip text'
!-----------
LocateInList PROCEDURE(QUEUE QRef, LONG ListFEQ, LONG TextFEQ, LONG NextBtn, LONG PrevBtn) !Field list could not use fancy Locate CLs
Fnd &CBLocateCls
A LONG
    CODE
    CASE EVENT()
    OF EVENT:Accepted ; IF ?<>TextFEQ THEN A=?.  !MUST press Enter or AltF,t Tab would double if AltF
    OF EVENT:AlertKey ; IF ?=TextFEQ AND KEYCODE()=EnterKey THEN A=?.
    END 
    IF ~A THEN RETURN. ; UPDATE
    Fnd &= NEW(CBLocateCls)
    Fnd.Init(QRef,ListFEQ,TextFEQ,NextBtn,PrevBtn,1)
    CASE A
    OF TextFEQ ; Fnd.Locate(1)
    OF NextBtn ; Fnd.Locate(1,1)
    OF PrevBtn ; Fnd.Locate(-1,1)
    END 
    DISPOSE(Fnd)
    RETURN 
!-----------
MakeOverList PROCEDURE(LONG F, BYTE CfgChg=0) !Color Lists to tell from APP
FC LONG(603000h) !804000h !Color:Navy !TODO Let User Config and save
BC LONG(0f1fcfbH)
  CODE
  F{PROP:NoTheme}=1
  IF CfgChg AND ~CFG:MakeOvrList THEN FC=-1 ; BC=-1 ; ELSIF ~CFG:MakeOvrList THEN RETURN .
  F{PROP:Color}=BC ; F{PROP:FontColor}=FC 
  F{PropList:DefHdrBackColor}=BC ; F{PropList:DefHdrTextColor}=FC 

MakeOverWindow PROCEDURE()
F LONG,AUTO
FC EQUATE(0423607H)
BC EQUATE(0E3F6FDH)
  CODE
  IF ~CFG:MakeOvrWnd THEN RETURN.
  0{PROP:Color}=BC ; 0{PROP:FontColor}=FC
  F=0 ; LOOP ; F=0{PROP:NextField,F} ; IF ~F THEN BREAK.
    CASE F{PROP:Type}
    OF CREATE:Text OROF CREATE:singleline OROF Create:ToolBar 
       F{PROP:Color}=BC ; F{PROP:FontColor}=FC    
!    OF Create:Entry
!    OF Create:Spin   ; F{PROP:Fill}=0def7f4h
!    OF Create:Button ; F{PROP:Fill}=BC !0def7f4h
    END
  END 
!----------
MaxNum PROCEDURE(LONG M1, LONG M2, LONG M3=0, LONG M4=0)
  CODE
  IF M1<M2 THEN M1=M2. ; IF M1<M3 THEN M1=M3. ; IF M1<M4 THEN M1=M4. ; RETURN M1
MaxNum PROCEDURE(LONG M1, *STRING S2, LONG M3=0, LONG M4=0)
  CODE
  RETURN MaxNum(M1,LenFastClip(S2),M3,M4)
!---------
ParenWrap PROCEDURE(STRING PropVal)!,STRING  !If not blank wraps in Parens (#)
    CODE
    IF PropVal THEN RETURN '(' & CLIP(PropVal) &')'.
    RETURN ''
!----------
PopClean PROCEDURE(STRING T)
X USHORT,AUTO
N USHORT
    CODE
    LOOP X=1 TO LenFastClip(T)
        CASE T[X]
        OF '{{' OROF '}' OROF '|' ; CYCLE
        OF ' ' OROF '+' OROF '-' OROF '~' ; IF N=0 THEN CYCLE.
        END
        N+=1 ; T[N]=T[X]
    END
    RETURN SUB(T,1,N)
PopItem PROCEDURE(STRING ItemTxt, <LONG Checked>, <LONG Disabled>,BYTE Pipe012=1)!,STRING
Pfx PSTRING(2)
  CODE
  IF ~OMITTED(Checked) AND Checked THEN Pfx=CHOOSE(~Checked,'-','+').
  IF ~OMITTED(Disabled) AND Disabled THEN Pfx='~'. 
  RETURN CHOOSE(Pipe012,'|','|-|','') & Pfx & PopClean(ItemTxt)
PopupBeside PROCEDURE(LONG CtrlFEQ, STRING PopMenu)!,LONG
X LONG,AUTO
Y LONG,AUTO
W LONG,AUTO
    CODE
    GETPOSITION(CtrlFEQ,X,Y,W)
    IF CtrlFEQ{PROP:InToolBar} THEN Y -= (0{PROP:ToolBar}){PROP:Height}.    
    RETURN POPUP(PopMenu,X+W,Y,1)
PopupUnder PROCEDURE(LONG CtrlFEQ, STRING PopMenu)!,LONG
X LONG,AUTO
Y LONG,AUTO
H LONG,AUTO
    CODE
    GETPOSITION(CtrlFEQ,X,Y,,H)
    IF CtrlFEQ{PROP:InToolBar} THEN Y -= (0{PROP:ToolBar}){PROP:Height}.
    RETURN POPUP(PopMenu,X,Y+H+1,1) 
!-----------------------------------------
PropHuntLoadP7Q     PROCEDURE(Parse7QType OutP7Q, LONG FeqTypeNo)
CPGroup  GROUP,PRE()
    !sheet  Above7C0A AboveSize7C0B Below7C06 BelowSize7C07  Decimal7C0A DecimalOffSet7C0B
CP1 STRING('7C00Text 7C01Type 7C08Left 7C09LeftOffSet 7C0CRight 7C0DRightOffSet'&|  !lower 7C06Center 7C07CenterOffSet
 ' 7C10FontName 7C11FontSize 7C12FontColor 7C13FontStyle'&| !7C10Font Array 4   11.1  7C14RangeLow 7C15RangeHigh
 ' 7C16VCR 7C17VCRFeq 7C1AResize 7C40Sum'&| ! 7C1BNoFrame  7C19Double  7C19Both 7C1BNoTicks 
 ' 7C41Ave 7C42Max 7C43Min 7C44CNT 7C45PageNo 7C50Page 7C51Absolute 7C52Alone 7C55First'&|
 ' 7C56Last 7C57INS 7C58OVR 7C59Boxed 7C5ACAP 7C5CColumn 7C5DCursor 7C5EDefault'&|  !11.1  7C5BCPI
 ' 7C5FDisable 7C60Drop 7C61Fill 7C62From 7C63Full 7C64Gray 7C65Hide 7C66HLP 7C67HScroll'&|
 ' 7C68Icon 7C69Iconize 7C6AIMM 7C6CLandscape 7C6DMark 7C6EMask 7C6FMaximize 7C70Mdi'&| !Key7C6B
 ' 7C71Meta 7C72Modal 7C73MSG 7C74NoBar 7C75NoMerge 7C76PageAfter 7C77PageAfterNum 7C78PageBefore'&|
 ' 7C79PageBeforeNum 7C7APassword 7C7BReadOnly 7C7CREQ 7C7DReset 7C7ERound 7C7FScroll'&|
 ' 7C80Separate 7C81Skip 7C82STD 7C83Step 7C84System 7C85Format 7C87Check 7C88TRN 7C89UPR'&|
 ' 7C8BVScroll 7C8EItems 7C90SelEnd 7C91Auto 7C92ToolBox 7C93Palette 7C95Thread 7C96Handle 7C99Follows'&|
 ' 7C9EVScrollPos 7CA0Tip 7CA3Vertical 7CA4Smooth 7CA6Progress 7CA7Visible 7CA8Enabled 7CA9Wizard'&| !dup SliderPos7CA6 
 ' 7CAAChoiceFEQ 7CABClientHandle 7CACLineCount 7CADMinWidth 7CAEMinHeight 7CAFMaxWidth'&|
 ' 7CB0MaxHeight 7CB7Spread 7CBAScreenText 7CBBHScrollPos 7CBFValue 7CC0Value,2 7CC2TabRows'&| !dup Value7CBF Array[2] TrueValue7CBF FalseValue7CC0
 ' 7CC3NumTabs 7CE2Join 7CE3NoSheet 7CE4BevelOuter 7CE5BevelInner 7CE6BevelStyle 7CE7LineWidth'&|
 ' 7CE9Angle 7CFAColor 7CFAFillColor 7CFBSelectedColor 7CFCSelectedFillColor 7CFDLineHeight'&| !dup Background7CFA 
 ' 7CFEDropWidth 7CFFAlwaysDrop 7A00Up 7A01Down 7A02UpsideDown 7A03HeaderHeight 7A04Checked'&|
 ' 7A06Single 7A07Parent 7A08Repeat 7A0FDelay 7A10USE 7A12ListFEQ 7A13ButtonFEQ 7A14Flat'&|
 ' 7A19InToolBar 7A1BRejectCode 7A40NoWidth 7A41NoHeight 7A43XOrigin'&| !11.1 chg 7A3AFontCharSet
 ' 7A44YOrigin 7A47Dock 7A48Docked 7A49BrokenTabs 7A52UseAddress 7A56NextTabStop 7A57PrevTabStop 7A58PropVScroll 7A5ALayout'&| !Makes sense for FOCUS() 7A56NextTabStop 7A57PrevTabStop
 ' 7A5EThemeActive 7A73TextLeftMargin 7A74TextRightMargin 7A75State3Value 7A78NoFont'&|
 ' 7A7EPrecedes 7904NoTheme 7905GradientToColor 7906GradientType 7C86Timer 7903TabSheetStyle'&|
 ' 7CB4WNDProc 7CB5ClientWNDProc 7A18ChildIndex' &|  !Indexed 7D0AChild
 ' 7A55WheelScroll 7C02XPos 7C03YPos 7C04Width 7C05Height 7A80OriginalWidth 7A81OriginalHeight ')  !11.13505
                    OMIT('!**END 11.0**', _C111_)
CP11 STRING(' 7A3AFontCharSet 7C5BCPI 7C14RangeLow 7C15RangeHigh ' &| !11.0   
                 '7C29List:Grid 7C2AList:DefHdrTextColor 7C2BList:DefHdrBackColor 7C2CList:HdrSortTextColor 7C2DList:HdrSortBackColor 7C2EList:SortTextColor 7C2FList:SortBackColor 7C30List:HasSortColumn 7C31List:SortColumn ') 
                          !**END 11.0**'
                    COMPILE('!**END 11.1**', _C111_)  !1st line is same
CP11 STRING(' 7C14FontCharSet 7C15CPI 7CE0RangeLow 7CE1RangeHigh ' &| !11.1
                 '7ED9List:Grid 7EDAList:DefHdrTextColor 7EDBList:DefHdrBackColor 7EDCList:HdrSortTextColor 7EDDList:HdrSortBackColor 7EDEList:SortTextColor 7EDFList:SortBackColor 7EE0List:HasSortColumn 7EE1List:SortColumn ') !11.1
                             !**END 11.1**
CP_Slider_NA STRING(' 7CA6Progress 7C19Double 7C1BNoFrame ')
CP_Slider    STRING(' 7CA6SliderPos 7C19Both 7C1BNoTicks ')
CP_Sheet_NA  STRING(' 7C06Center 7C07CenterOffSet 7C0ADecimal 7C0BDecimalOffSet ') !Dup Sheet
CP_Sheet     STRING(' 7C06Below 7C07BelowSize 7C0AAbove 7C0BAboveSize ')  
                    OMIT('!**END 11.0**', _C111_)
CP_LIST      STRING(' 7A23FromQRef 7C20List:MouseDownRow 7C21List:MouseMoveRow 7C22List:MouseUpRow 7C23List:MouseDownField'&|  !11.0
                    ' 7C24List:MouseMoveField 7C25List:MouseUpField 7C26List:MouseDownZone 7C27List:MouseMoveZone 7C28List:MouseUpZone ')
                          !**END 11.0**'
                    COMPILE('!**END 11.1**', _C111_)  !1st line is same
CP_LIST      STRING(' 7A23FromQRef 7ED0List:MouseDownRow 7ED1List:MouseMoveRow 7ED2List:MouseUpRow 7ED3List:MouseDownField'&|  !11.1
                    ' 7ED4List:MouseMoveField 7ED5List:MouseUpField 7ED6List:MouseDownZone 7ED7List:MouseMoveZone 7ED8List:MouseUpZone ')
                             !**END 11.1**
CP_REPORT    STRING(' 7A46Together 7C8CWithNext 7C8DWithPrior ')
CP_WINDOW    STRING(' 7A71ToolBar 7A72MenuBar ')
CP_OLEOCX    STRING(' 7CEAAutoSize 7CEBClip 7CECStretch 7CEDZoom 7CEECompatibility 7CEFDesign 7CF0Document 7CF1Link 7CF2Align 7CF3Cancel 7CF4TextAlign 7CF5Object 7CF6License 7CF8Language 7CF9Interface' & |
                    ' 7CC6Create 7CC7SaveAs 7CC8Open 7CC9Blob 7CCADoVerb 7CCBSizeMode 7CCCSelectInterface 7CCDAddRef 7CCERelease 7CCFDeactivate 7CD0Update 7CD1Paste 7CD2ReportException 7CD3PasteLink 7CD4Copy' & |
                    ' 7CD5CanPaste 7CD6CanPasteLink 7CD7WindowUI 7CD8DesignMode 7CD9Ctrl 7CDAGrabHandles 7CDBOLE 7CDCIsRadio 7CDDLastEventName 7CDECLSID 7CDFProgID ') !Docs say some are write-only YMMV
CP_Selected  STRING(' 7C8FSelected              ')  !SelStart for ENTRY TEXT
CP_EndSpace  STRING(' ')         ! 7C8FSelStart  <-Add for All
   END 
Add_SelStart              STRING(' 7C8FSelStart')   
CP  STRING(SIZE(CPGroup)),OVER(CPGroup)
    CODE
    CASE FeqTypeNo
    OF CREATE:Sheet      ; CLEAR(CP_Sheet_NA) ; CLEAR(CP_Slider)
    OF CREATE:Slider_MIA ; CLEAR(CP_Sheet)    ; CLEAR(CP_Slider_NA)
    OF CREATE:Entry OROF CREATE:Text OROF CREATE:SingleLine OROF CREATE:Combo ; CP_Selected=Add_SelStart
    OF -1 ; CP_Selected=Add_SelStart & CP_Selected     !ALL for Pick
    ELSE                 ; CLEAR(CP_Sheet)    ; CLEAR(CP_Slider)
    END
    EquateXStringParse(OutP7Q,4,CP) 
    RETURN  
!---------
PropText1 PROCEDURE(LONG FEQ, LONG FType, *STRING AltText, BOOL QuoteIt)
T ANY
  CODE
  CASE FType
  OF CREATE:LIST OROF CREATE:COMBO ; T=CLIP(AltText)
  ELSE ; T=CLIP(PWnd$FEQ{PROP:Text}) ; IF ~T THEN T=CLIP(AltText).
  END 
  RETURN CHOOSE(~QuoteIt,CLIP(T),QUOTE(T))
!--------------------
PropTFName PROCEDURE(LONG FEQ, LONG PropX, STRING TName,<STRING FName>)!,STRING
    CODE
    IF FEQ{PropX} THEN RETURN TName.
    IF ~OMITTED(FName) THEN RETURN FName.
    RETURN ''
!--------------------
ReplaceInto PROCEDURE(*string Into, string Find,string Repl, BYTE ClipInto=0)!,LONG,PROC
X   LONG,AUTO
L   LONG,AUTO
zI  LONG,AUTO   
zF  LONG,AUTO   
zR  LONG,AUTO
FCnt LONG   
  CODE !From ABError, tweaked a LOT. Supports replace the same char e.g. 10 to 13,10
  Find=lower(Find) ; L=1 ; zI=SIZE(Into) ; zF=SIZE(Find) ; zR=SIZE(Repl)  
  IF NOT(Find=lower(Repl) AND zF=zR) THEN 
     LOOP
       IF ClipInto THEN X=INSTRING(Find,CLIP(lower(Into)),1,L) ELSE X=INSTRING(Find,lower(Into),1,L).
       IF ~X THEN BREAK.
       Into=SUB(Into,1,X-1) & Repl & SUB(Into,X+zF,zI) ; L=X+zR ; FCnt+=1
  . .
  RETURN FCnt
ReplaceText PROCEDURE(string S, string Find,string Repl, BYTE Clp=0)!,STRING
S2 STRING(Size(S)*2+256),AUTO
  CODE
  S2=S ; ReplaceInto(S2,Find,Repl,Clp) ; RETURN CLIP(S2)  
!====================
SetClip2Tab2Space PROCEDURE(STRING Tx, BYTE GapClm=1, BYTE HeadDash=0, BYTE FootDash=0, BYTE NoAsk=0)
SD &STRING
S LONG,AUTO
LnT LONG,AUTO
Chr BYTE,AUTO
Lnz LONG,AUTO
CCnt LONG 
CNo LONG 
B LONG,AUTO
X LONG,AUTO
CQ QUEUE,PRE(CQ)
Max LONG !CQ:Max
     END
  CODE
  CASE CHOOSE(NoAsk>0,2,POPUP('Copy in what Format?|-|Space Delimeter|Tab Delimeter (Excel)'))
  OF 0 OROF 1 ; RETURN
  OF 3 ; SETCLIPBOARD(Tx) ; RETURN
  END
  LnT=LenFastClip(Tx) ; IF LnT<3 THEN SETCLIPBOARD(Tx) ; RETURN.
  B=1 ; Lnz=FootDash+HeadDash 
  LOOP X=1 TO LnT+1 
      Chr=CHOOSE(X>LnT,13,VAL(Tx[X]))
      IF Chr<>9 AND Chr<>13 THEN CYCLE.  !Column <9> at X, Began at B
      CNo+=1 ; IF CNo>CCnt THEN CCnt=CNo ; CQ:Max=1 ; ADD(CQ) ELSE GET(CQ,CNo).
      IF X-B>CQ:Max THEN CQ:Max=X-B ; PUT(CQ).   
      IF Chr=13 THEN X+=1 ; CNo=0 ; Lnz+=1. !Skip 10, End Line
      B=X+1
  END  !2=13,10 + Gaps
  S=2+CCnt*GapClm ; LOOP X=1 TO CCnt ; GET(CQ,X) ; S+=CQ:Max ; END  
  SD &= NEW(STRING(S*Lnz+10)) ; B=1 ; S=1 ; CNo=0 ; Lnz=0 
  LOOP X=1 TO LnT+1        
    Chr=CHOOSE(X>LnT,13,VAL(Tx[X]))
    IF Chr=9 OR (Chr=13 AND CNo)     
       CNo+=1 ; IF CNo>1 THEN S+=GapClm.
       IF X-B>0 THEN SD[S : S+X-B-1]=Tx[B : X-1].
       GET(CQ,CNo) ; S+=CQ:Max ; B=X+1
    END 
    IF X>LnT THEN BREAK. ; IF Chr<>13 THEN CYCLE.
    SD[S : S+1]='<13,10>' ; S+=2 ; Lnz+=1
    IF Lnz=HeadDash THEN DO DashRtn.
    X+=1 ; B=X+1 ; CNo=0 !Skip 10
  END           
  IF SD[S]<>CHR(10) THEN SD[S : S+1]='<13,10>' ; S+=2 .
  IF FootDash THEN DO DashRtn.  
  SETCLIPBOARD(SUB(SD,1,S-1)) ; DISPOSE(SD)
  RETURN
DashRtn ROUTINE
 LOOP CNo=1 TO CCnt ; GET(CQ,CNo)
    IF CNo>1 THEN S+=GapClm. ; SD[S : S+CQ:Max-1]=ALL('=',CQ:Max) ; S+=CQ:Max
 END ; SD[S : S+1]='<13,10>' ; S+=2
!---------
SetClip2Queue PROCEDURE(QUEUE Q, BYTE HeadUL=1, <STRING Hd>, <STRING Prefix>, USHORT CMin=1, USHORT CMax=999)
X LONG,AUTO
C LONG,AUTO
CB ANY
Nm STRING(32),AUTO
QV ANY
QS STRING(256),AUTO  !? need to spot ANY &= &STRING that is null
Prx PSTRING(16)
IsHd BYTE
Tbz SHORT
Blank STRING(1)
  CODE
  Tbz=POPUP('Copy in what Format?|-|Space Delimeter|Tab Delimeter (Excel)')-2 ; IF Tbz<0 THEN RETURN.
  IF ~OMITTED(Hd) THEN 
    CB=Hd&'<13,10>' ; IsHd=1
    IF Tbz THEN 
       LOOP X=1 TO SIZE(Hd) ; IF VAL(Hd[X])<>9 THEN Hd[X]='='. ; END ; IF HeadUL THEN CB=CB&Hd&'<13,10>'.
    END
  END 
  IF ~OMITTED(Prefix) THEN Prx=UPPER(Prefix).
  LOOP X=IsHd TO RECORDS(Q) ; GET(Q,X) 
    LOOP C=CMin TO CMax ; Nm=WHO(Q,C) ; IF ~Nm THEN BREAK. ; QV&=WHAT(Q,C) 
      IF X=0 THEN
         IF Prx AND Prx=UPPER(SUB(Nm,1,LEN(Prx))) THEN Nm=SUB(Nm,LEN(Prx)+1,99). ; QV&=Nm 
      END ; QS=QV ; ReplaceInto(QS,CHR(0),' ') ; CB=CB&CHOOSE(C=CMin,'',CHR(9))&CLIP(QS)
    END   ; CB=CB&'<13,10>'
  END 
  IF Tbz THEN SetClipboard(CB) ELSE SetClip2Tab2Space(CB,,HeadUL,,1).
  RETURN   
!----------------
SeeMore PROCEDURE(LONG CaseX, LONG F, LONG CtrlTypeNo)!,STRING,PRIVATE
FQSM STRING(256)
L LONG,AUTO
X LONG,AUTO
    CODE
    CASE CaseX
    OF -1*PROP:Value
       CASE CtrlTypeNo
       OF CREATE:Option ; FQSM='Radios:' ; LOOP X=1 TO 255 ; L=F{PROP:Child,X} ; IF ~L THEN BREAK. ; FQSM=CLIP(FQSM)&' "' & L{PROP:Value} & '"' ; END
       OF CREATE:Radio OROF CREATE:Tab  ; FQSM='Index=' & F{PROP:ChildIndex} &'  Value("'& F{PROP:Value} &'")'
       OF CREATE:Check  ; FQSM='Value("' & F{PROP:Value,1} &'","'& F{PROP:Value,2} &'")' ; IF FQSM='Value("","")' THEN FQSM=''.
       OF Create:State3 ; FQSM='Value("' & F{PROP:Value,1} &'","'& F{PROP:Value,2} &'" , "'& F{PROP:Value,3} &'"'  ; IF FQSM='Value("","","")' THEN FQSM=''.
       OF Create:List OROF CREATE:Combo OROF CREATE:DropList OROF CREATE:DropCombo ; FQSM=F{PROP:From} ; IF FQSM THEN FQSM='From(' & CLIP(FQSM) &')'.
       OF Create:Spin OROF CREATE:Progress ; FQSM=F{PROP:From} ; IF ~FQSM THEN FQSM='Range(' & F{Prop:RangeLow} &' - '& F{Prop:RangeHigh} &' by '& F{PROP:Step} &')'.
       OF CREATE:Button ; X=F{PROP:Std} ; IF X THEN FQSM=ClaSTDprop(X).
       END
    END
    RETURN CLIP(FQSM)
!-----------------------------------------
DropListColor PROCEDURE(LONG ListFEQ)
    CODE
    ListFEQ=ListFEQ{PROP:ListFEQ} !FEQ of Drop List
    IF ListFEQ THEN ListFEQ{PROP:FontColor}=80000017h ; ListFEQ{PROP:Background}=80000018h.
!-----------------------------------------
EquateStringFind PROCEDURE(STRING FindHex, *STRING EqtHexLabel)!,STRING
B   USHORT,AUTO
E   USHORT,AUTO 
L   USHORT,AUTO 
    CODE 
    B=INSTRING(FindHex,EqtHexLabel,1)    
    IF B THEN
       L=SIZE(FindHex)
       E=INSTRING(' ',EqtHexLabel,1,B+L)  
       IF ~E THEN E=SIZE(EqtHexLabel)+1.    !IF E THEN  no end space
       RETURN EqtHexLabel[B+L : E-1]          
    END
    RETURN ''
!-------------------------------
EquateXStringParse  PROCEDURE(Parse7QType P7Q, BYTE HexLen, *STRING CP, BYTE IsDecimal=0)  !Format 7777EquateName 7234Equate                         
B       USHORT,AUTO                        !E+1234
E       USHORT,AUTO
SizCP   USHORT,AUTO
    CODE
    ASSERT(HexLen>=1 AND HexLen<=8)
    CLEAR(P7Q)  
    B = 1 ; SizCP = SIZE(CP)
    LOOP E = 1 TO SizCP +1
         IF B=0 THEN 
            IF E>SizCP THEN BREAK.  !deal with no trailing space
            IF ~CP[E] THEN CYCLE. !Skip over spaces 
            B=E
?   ASSERT(INSTRING(CP[B],'0123456789ABCDEF'), ' E=' & E &' CP[B]=' & CP[B] &' '& SUB(CP,B-10,20))            
            E += HexLen + 1
         END    
         IF E<=SizCP AND CP[E] THEN CYCLE.     !Not Space look for next
         P7Q:Name    = LEFT(CP[B+HexLen : E-1])          
         IF ~IsDecimal THEN 
             P7Q:EqtHex  =  CP[B : B+HexLen-1] & 'h'         
             P7Q:EqtLong = HexEval(P7Q:EqtHex, HexLen) 
?   ASSERT(P7Q:EqtLong=EVALUATE(P7Q:EqtHex), 'EquateXStringParse not = Eval' & P7Q:EqtHex) 
?   ASSERT(P7Q:EqtLong<>0,'Evaluate failed on ' & P7Q:EqtHex &' for ' & P7Q:Name &'<13,10>EquateXStringParse ' & SUB(CP,1,200) ) 
         ELSE
             P7Q:EqtLong = CP[B : B+HexLen-1]
             P7Q:EqtHex  = P7Q:EqtLong   
             !P7Q:EqtHex = Hex8(P7Q:EqtHex,-2)
         END 
         ADD(P7Q)
         B = 0
    END !Loop
    RETURN
!-------------------------------
Equate7StringParse  PROCEDURE(Parse7QType P7Q, *STRING CP)  !Assume CP format  EquateName7234 Equate7777                         
B USHORT,AUTO
E USHORT,AUTO
  CODE
  FREE(P7Q) ;  CLEAR(P7Q)  
  B = 1 
  LOOP E = 2 TO SIZE(CP)
     IF CP[E]<>'7' THEN CYCLE.
     P7Q:Name    = LEFT(CP[B : E-1])
     P7Q:EqtHex  = CP[E : E+3] & 'h'
     P7Q:EqtLong = HexEval(CP[E : E+3],4)
?   ASSERT(P7Q:EqtLong=EVALUATE(P7Q:EqtHex), 'Equate7StringParse not = Eval' & P7Q:EqtHex) 
?   ASSERT(P7Q:EqtLong<>0,'Evaluate failed on ' & P7Q:EqtHex &' for ' & P7Q:Name &'<13,10>Equate7StringParse ' & SUB(CP,1,200) ) 
     E += 4
     B = E + 1     
     ADD(P7Q)
  END !Loop
  RETURN
!----------------------------
FeqNameUpLow  PROCEDURE(STRING Nm)
X LONG,AUTO
  CODE
  X=LenFastClip(Nm) ; IF X<3 THEN RETURN SUB(Nm,1,X). ; IF ~CFG:FEQUpLow OR Nm<>UPPER(Nm) THEN RETURN Nm[1 : X]. ; Nm=UPPER(Nm)
  LOOP X=X TO CHOOSE(Nm[1]='?',3,2) BY -1 ; IF Nm[X-1]>='A' AND Nm[X-1]<='Z' THEN Nm[X]=lower(Nm[X]). ; END 
  RETURN CLIP(Nm)
!-------------------------------
TextViewWindow  PROCEDURE(STRING CapTxt, STRING Txt2See, STRING ValueOnly)
Txt         STRING(4096),AUTO
HexTxt      STRING(4096*5),AUTO
LenVO       LONG    !Also first time Flag
ShowHex     BYTE                 
Window WINDOW('Caption'),AT(,,325,140),GRAY,SYSTEM,FONT('Consolas',10),RESIZE
        TOOLBAR,AT(0,0),USE(?TOOLBAR1)
            CHECK('Show HEX'),AT(0,0),USE(ShowHex),TIP('See Value in Hex')
        END
        TEXT,AT(0,1),FULL,USE(Txt),VSCROLL
        TEXT,AT(0,1),FULL,USE(HexTxt),VSCROLL ,hide
    END
SysMenuCls SysMenuClass
  CODE !TODO Maybe pass in PO and format it in here
  Txt = Txt2See 
  OPEN(Window) ; 0{PROP:Text}=CapTxt
  SysMenuCls.Init(Window)    
  ACCEPT
    CASE ACCEPTED()
    OF ?ShowHex 
        IF ~LenVO THEN 
           LenVO=LEN(CLIP(ValueOnly)) 
           IF ~LenVO THEN 
               LenVO=SIZE(ValueOnly) ; IF LenVO>16 THEN LenVO=16.
           END    
           HexTxt = HexDumpMemory(ADDRESS(ValueOnly),LenVO,0) !SIZE(SrcStr),0)
        END
        ?Txt{PROP:Hide}=CHOOSE(~~ShowHex) ; ?HexTxt{PROP:Hide}=CHOOSE(~ShowHex)
        DISPLAY
    END
  END ; CLOSE(Window) 
  RETURN
!------------------------------- 
!    TextViewWindow('View ' & CLIP(FeqTypeName) & ' FEQ '& FEQ &' '& PQ:Name, |
!                'Equate: ' & PQ:EqtHex &'<13,10><13,10>' & |
!                'Name:   ' & PQ:Name &'<13,10><13,10>' & |
!                'Value:  ' &  CHOOSE(PQ:EqtLong<>PROP:Format, PQ:Value, Wnd$Feq{PROP:Format}) &'<13,10>',|
!                CHOOSE(PQ:EqtLong <1, PQ:Value, Wnd$Feq{PQ:EqtLong}) )
!
!    PropQType  QUEUE,TYPE 
!    EqtHex      STRING(5)      !PQ:EqtHex
!    Name        STRING(32)     !PQ:Name
!    Value       STRING(255)    !PQ:Value        !TODO Have RawValue without Description
!    ValueBIG    &STRING        !PQ:ValueBIG 
!    EqtLong     LONG           !PQ:EqtLong  
PropViewWindow PROCEDURE(STRING CapTxt, PropQType PQ, STRING ValueOnly)
Txt         STRING(4096),AUTO
HexTxt      STRING(4096*5),AUTO
ShowHex     BYTE                 
Window WINDOW('PropView'),AT(,,325,140),GRAY,SYSTEM,FONT('Consolas',10),RESIZE
        !TOOLBAR,AT(0,0),USE(?TOOLBAR1)
            CHECK('Show HEX'),AT(2,1),USE(ShowHex),TIP('See Value in Hex') ,LEFT ,skip
        !END
        TEXT,AT(0,14),FULL,USE(Txt),VSCROLL
        TEXT,AT(0,14),FULL,USE(HexTxt),VSCROLL ,hide
    END
SysMenuCls SysMenuClass    
    CODE
!TODO  CHOOSE(PQ:EqtLong<>PROP:Format, PQ:Value, Wnd$Feq{PROP:Format}) &'<13,10>',|    
    Txt = 'Equate: ' & PQ:EqtHex &'<13,10><13,10>' & |
          'Name:   ' & CLIP(PQ:Name) &'<13,10><13,10>' & |
          'Value:  ' & CLIP(PQ:Value) &'<13,10>'
    HexTxt[1]='`'
    OPEN(Window) ; 0{PROP:Text}=CapTxt
    SysMenuCls.Init(Window)
!TODO    SELF.AtSetOrSave(1, AtPropView[])
    ACCEPT
        CASE ACCEPTED()
        OF ?ShowHex 
            IF HexTxt[1]='`' THEN 
               HexTxt = HexDumpString(ValueOnly,1) !1=clip
            END
            ?Txt{PROP:Hide}=CHOOSE(~~ShowHex)
            ?HexTxt{PROP:Hide}=CHOOSE(~ShowHex)
            DISPLAY
        END
    END
!TOTO    SELF.AtSetOrSave(2, AtPropView[])
    CLOSE(Window) 
    RETURN
!----------------------
HexDumpString PROCEDURE(*STRING SrcStr, BOOL ClipSpaces=0) !,STRING
SrcSize     LONG,AUTO
  CODE
  IF ~ClipSpaces THEN
      SrcSize = SIZE(SrcStr)
  ELSE
      SrcSize=LEN(CLIP(SrcStr))  ;  IF ~SrcSize THEN SrcSize=1.
  END
  RETURN HexDumpMemory(ADDRESS(SrcStr),SrcSize,0)
!-------------------------------
HexDumpMemory PROCEDURE(long SrcAddr, Long SrcSize, Byte ShowAddr=0) !,STRING
Segment16       long,auto
ByteNo          long,auto
Mem_Hex         equate(8+4-4)
Mem_Chr         equate(Mem_Hex+16*3+1)
MemLine         string(Mem_Chr+16),auto         !AAAAAAAA XX x16  16CHRbytes
cMemLine        cstring(size(MemLine)+3),auto
!Byte1           byte,auto
Byte1           &byte
HexD            STRING('0123456789ABCDEF')
Dump            ANY
  CODE 
  !MemLine = SrcAddr &','& SrcSize 
  MemLine = '' !SrcSize 
  MemLine[Mem_Hex : Mem_Hex+16*3]=' 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16'
  MemLine[Mem_Chr : Mem_Chr+15]='0123456789abcdef'
  Dump=clip(MemLine) & '<13,10>' 
  if SrcAddr >= 0 and SrcAddr < 10000h then return (Dump & 'Error Low Address '&SrcAddr).  !memory < 64KB will cause access violation. Could be <0 if /3GB LargeMemory
  if SrcSize <= 0 then return(Dump & 'Error Invalid Size '&SrcSize).                       !Passed LEN(CLIP()) and it was zero
  loop Segment16=SrcAddr to SrcAddr+SrcSize-1 by 16
     MemLine=choose(~ShowAddr,Segment16-SrcAddr,Segment16)  !;MemLine[Mem_Hex+3*8-1]='-'
     Loop ByteNo = 0 to 15
        if Segment16 + ByteNo > SrcAddr+SrcSize-1 then break.
        IF ByteNo=8 THEN MemLine[Mem_Hex+3*8-1]='-'.
        Byte1 &= (Segment16 + ByteNo)
        MemLine[Mem_Hex + ByteNo*3 : Mem_Hex + ByteNo*3+1]=HexD[BSHIFT(Byte1,-4)+1] & HexD[BAND(Byte1,0FH) + 1]
        MemLine[Mem_Chr + ByteNo]=choose(Byte1<32,'.',chr(Byte1))
     end
     Dump = Dump & clip(MemLine) & '<13,10>' 
  end
  RETURN Dump  
!================
SysMenuClass.Init Procedure(WINDOW Wnd2Hook)
hMenu           LONG
MF_SEPARATOR    EQUATE(800h)
MF_STRING       EQUATE(0)
AddOk           BOOL
cTxt            CSTRING(50),AUTO
  CODE
  SELF.WindowRef &= Wnd2Hook
  SELF.hWindow   =  Wnd2Hook{PROP:Handle}
  hMenu = GetSystemMenu(SELF.hWindow, False)
  IF hMenu=0 OR hMenu=-1 THEN RETURN.
  LOOP 1 TIMES 
    IF ~AppendMenu(hMenu, MF_SEPARATOR, 0) THEN BREAK.
    cTxt='CLOSE Windows back to the Preview'        ; IF ~AppendMenu(hMenu, MF_STRING, SMCmd_Close2Prv, cTxt) THEN BREAK.                                                      
    cTxt='RUN ANOTHER Instance of this Preview'     ; IF ~AppendMenu(hMenu, MF_STRING, SMCmd_RunAgain, cTxt) THEN BREAK.                                              
    IF ~AppendMenu(hMenu, MF_SEPARATOR, 0) THEN BREAK.     
    cTxt='MOVE Windows to Position Under this One'  ; IF ~AppendMenu(hMenu, MF_STRING, SMCmd_MoveUnder, cTxt) THEN BREAK.                                              
    cTxt='HIDE Windows UNDER this One'              ; IF ~AppendMenu(hMenu, MF_STRING, SMCmd_HideUnder, cTxt) THEN BREAK.                                              
    cTxt='Hide PREVIEW Window ' & Glo:Built         ; IF ~AppendMenu(hMenu, MF_STRING, SMCmd_HidePWnd , cTxt) THEN BREAK.                                              
    IF ~AppendMenu(hMenu, MF_SEPARATOR, 0) THEN BREAK.
    cTxt='Kill WINDOW Previews with this Caption'   ; IF ~AppendMenu(hMenu, MF_STRING, SMCmd_HaltCapt, cTxt) THEN BREAK.
    cTxt='KILL ALL Preview EXEs'                    ; IF ~AppendMenu(hMenu, MF_STRING, SMCmd_HaltALL , cTxt) THEN BREAK. 
    cTxt='HALT THIS Preview Only'                   ; IF ~AppendMenu(hMenu, MF_STRING, SMCmd_HaltThis, cTxt) THEN BREAK.
    AddOk=1
  END
ASSERT(AddOk,'AppendMenu error ' & GetLastError())
  IF ~AddOk THEN RETURN. 
  SELF.OrigWndProc = Wnd2Hook{PROP:WndProc}
  Wnd2Hook{PROP:WndProc} = ADDRESS(SysMenuCls_SubClassFunc)
  CLEAR(SysMenuClsQ)
  SysMnQ:WinRef  &= Wnd2Hook
  SysMnQ:hWindow     = SELF.hWindow
  SysMnQ:OrigWndProc = SELF.OrigWndProc 
  SysMnQ:ZOrder      = 1 + RECORDS(SysMenuClsQ) 
  ADD(SysMenuClsQ,SysMnQ:hWindow)
  DB('SysMenuClass.Init Added ZOrder=' & SysMnQ:ZOrder &' hWindow=' & SysMnQ:hWindow &' '& SELF.WindowRef{PROP:Text} )
  SELF.bInited = 1    
  RETURN
SysMenuClass.DESTRUCT Procedure()
QX      USHORT,AUTO
  CODE
  LOOP WHILE SELF.bInited 
    SELF.bInited = 0
    SysMnQ:hWindow  = SELF.hWindow
    GET(SysMenuClsQ,SysMnQ:hWindow)
    IF ERRORCODE() THEN
       Message('SysMenuClass.DESTRUCT failed GET(SysMenuClsQ) ' & SELF.hWindow)
       BREAK
    END
    DELETE(SysMenuClsQ)
    CASE STATUS(SELF.WindowRef)
    OF WINDOW:OK OROF WINDOW:ClosePending OROF WINDOW:InDestroy
       SELF.WindowRef{PROP:WndProc} = SELF.OrigWndProc 
    END
    IF SysMnQ:HidePWnd THEN PWnd{PROP:Hide}=0.       
    SORT(SysMenuClsQ,-SysMnQ:ZOrder)  ! -Reverse sort so #1 is Most Recent       
    IF SysMnQ:DoinHide THEN !If Under Hide must Unhide
       DB('DESTRUCT Doin UnHIDE of HIDE Recs=' & RECORDS(SysMenuClsQ) &' for hWindow=' & SysMnQ:hWindow &' '& SysMnQ:WinRef{PROP:Text})
       LOOP QX=1 TO RECORDS(SysMenuClsQ) 
          GET(SysMenuClsQ,QX)
          DB('DESTRUCT HideUnderRtn QX=' & QX &' WasHide=' & SysMnQ:WasHide12 &' hWindow=' & SysMnQ:hWindow &' '& SysMnQ:WinRef{PROP:Text})
          IF SysMnQ:WasHide12<>1 OR QX=1 THEN SysMnQ:WinRef{PROP:Hide}=0.
          IF QX=1 THEN SysMnQ:WasHide12=0 ; PUT(SysMenuClsQ). !Must clear Most Recent and figure it out again
          IF SysMnQ:DoinHide THEN  !this window did HideUnder so it will Unhide parents when it closes
             DB('DESTRUCT HideUnderRtn QX=' & QX &' Was Doin Hide So Stop Unhiding')
             BREAK
          END
       END !Loop Sys Q
    END    !IF DoinHide
    GET(SysMenuClsQ,1) !This should be active window
  END !if bInited 
  RETURN 
!-----------------------------
SysMenuCls_SubClassFunc  FUNCTION(LONG hWnd,LONG wMsg,LONG wParam,LONG lParam)!,LONG,PASCAL
  CODE
  IF SysMnQ:hWindow <> hWnd OR SysMnQ:CloseMe THEN 
     SysMnQ:hWindow  = hWnd
     GET(SysMenuClsQ,SysMnQ:hWindow) 
     IF ERRORCODE() THEN 
        Message('SysMenuCls_SubClassFunc Bug GET(SysMenuClsQ) HWnd=' & HWnd )
        RETURN(1)    
     END
  END   
  CASE wMsg
  OF 011H OROF 016H ; IF GLO:IsPreviewEXE THEN HALT(). !RETURN(True)  !WM_QUERYENDSESSION  WM_ENDSESSION Get the mustard out of here
  OF 0112H          ; SysMenuCls_SYSCOMMAND(hWnd,wParam,lParam)  ; !WM_SYSCOMMAND 
  END
  IF SysMnQ:CloseMe=1 THEN !SMCmd_Close2Prv
     SysMnQ:CloseMe=2
     PUT(SysMenuClsQ)  ; IF ERRORCODE() THEN MESSAGE(SysMnQ:hWindow & ' PUT(SysMenuClsQ) ' & ERROR()).
     PostMessage(hWnd,10h,0,0)  !WM_CLOSE EQUATE(010H)
  END    
  RETURN(CallWindowProc(SysMnQ:OrigWndProc,hWnd,wMsg,wParam,lParam)) 
!--------------------    
SysMenuCls_SYSCOMMAND  FUNCTION(LONG hWnd,LONG SMCmd_wParam,LONG lParam=0)
QX      USHORT,AUTO
X       LONG,AUTO
Y       LONG,AUTO
DoHide  BYTE,AUTO
   CODE
   CASE SMCmd_wParam 
   OF SMCmd_Close2Prv ; DO Close2PreviewRtn
   OF SMCmd_HideUnder ; DO HideUnderRtn     ; IF SysMnQ:DoinHide THEN DO MoveUnderRtn.    !HIDE Windows Under this One
   OF SMCmd_HidePWnd  ; DO HidePWndRtn
   OF SMCmd_MoveUnder ; DO MoveUnderRtn
   OF   SMCmd_HaltThis  
   OROF SMCmd_HaltCapt 
   OROF SMCmd_HaltALL ; DO TaskKillRtn
   OF SMCmd_RunAgain  ; RUN(COMMAND('0'))
   END 
   RETURN
Close2PreviewRtn ROUTINE    
  LOOP QX=1 TO RECORDS(SysMenuClsQ)
       GET(SysMenuClsQ,QX)
       IF SysMnQ:ZOrder=1 THEN CYCLE.
       IF SysMnQ:CloseMe=0 THEN SysMnQ:CloseMe=1.
       PUT(SysMenuClsQ)
  END
  SysMnQ:hWindow = hWnd ; GET(SysMenuClsQ, SysMnQ:hWindow)
  EXIT
HideUnderRtn ROUTINE
  SysMnQ:DoinHide=1-SysMnQ:DoinHide ; DoHide=SysMnQ:DoinHide
  PUT(SysMenuClsQ)
  DB('HideUnderRtn Doin Hide=' & DoHide & ' for hWindow=' & SysMnQ:hWindow)
  LOOP QX=1 TO RECORDS(SysMenuClsQ)
       GET(SysMenuClsQ,QX)
       IF SysMnQ:hWindow = hWnd OR SysMnQ:ZOrder=1 THEN CYCLE.
       IF SysMnQ:WasHide12=0 THEN 
          SysMnQ:WasHide12=CHOOSE(~SysMnQ:WinRef{PROP:Hide},2,1)
          PUT(SysMenuClsQ)
          DB('HideUnderRtn QX=' & QX &' WasHide=' & SysMnQ:WasHide12 & ' for hWindow=' & SysMnQ:hWindow )
       END                                                                                          
       IF SysMnQ:WasHide12=1 THEN CYCLE.
       SysMnQ:WinRef{PROP:Hide}=DoHide 
       DB('Doin HIDE with Clarion =' & DoHide &'  '& ' for hWindow=' & SysMnQ:hWindow )
  END
  SysMnQ:hWindow = hWnd ; GET(SysMenuClsQ, SysMnQ:hWindow)
  EXIT 
HidePWndRtn ROUTINE
  SysMnQ:HidePWnd=1-SysMnQ:HidePWnd ; PWnd{PROP:Hide}=SysMnQ:HidePWnd
  PUT(SysMenuClsQ)
MoveUnderRtn ROUTINE
  SysMnQ:WinRef{PROP:Pixels}=1
  X=SysMnQ:WinRef{PROP:XPos} ; Y=SysMnQ:WinRef{PROP:YPos}
  SysMnQ:WinRef{PROP:Pixels}=0    
  SORT(SysMenuClsQ,-SysMnQ:ZOrder)  ! -Reverse sort so #1 is Most Recent
  LOOP QX=1 TO RECORDS(SysMenuClsQ)
       GET(SysMenuClsQ,QX)
       IF SysMnQ:hWindow = hWnd THEN CYCLE.
       DB('Move QX=' & QX & ' ZOrder=' &  SysMnQ:ZOrder &' ('& X &','& Y &') '& SysMnQ:WinRef{PROP:Text}) 
       SysMnQ:WinRef{PROP:Pixels}=1
       SysMnQ:WinRef{PROP:XPos}=X ; SysMnQ:WinRef{PROP:YPos}=Y - CHOOSE(SysMnQ:ZOrder=1,20,0)
       SysMnQ:WinRef{PROP:Pixels}=0
  END
  SysMnQ:hWindow = hWnd ; GET(SysMenuClsQ, SysMnQ:hWindow)
  EXIT
TaskKillRtn ROUTINE
  DATA    
BatFN   PSTRING(48)
Tm8     STRING(8)
CRLF    STRING('<13,10>') !CRLF STRING('--')  !Tests show max length around 350 chars so below PUTINIs safe from 1024 limit
FI_Filter   PSTRING(256)
Title   STRING(32)
  CODE
  IF ~GLO:IsPreviewEXE THEN
      IF 1=Message(COMMAND('0')&'||This is NOT a Preview EXE.||Do you really want to Halt It?','HALT',ICON:Hand,'Let Run|Halt It') THEN EXIT.
  END 
  IF SMCmd_wParam=SMCmd_HaltThis THEN HALT().
  Tm8 = FORMAT(CLOCK(),@t05) &'00'
  BatFN='.\WinPreviewKill' & Tm8[1:4] &'-'& Tm8[5:8] &'.BAT'
  REMOVE(BatFN)     
  FI_Filter = '/FI "IMAGENAME eq WinPreview*"'
  IF SMCmd_wParam=SMCmd_HaltCapt THEN
     Title=GloT:Caption
     IF Title[1]='"' THEN Title=LEFT(Title[2:32]).
     X=INSTRING('"',Title) ; IF X THEN Title=SUB(Title,1,X-1).
     FI_Filter=CLIP(FI_Filter) & ' /FI "WINDOWTITLE eq ' & CLIP(Title) &'*"' 
  END                
  !Find lines I want, but no blanks:     Findstr /i /b /C:"Image" /C:"PID" /C:"Window"    
  !Find lines I do NOT want, has blanks: Findstr /ivb /C:"Session" /C:"Mem" /C:"Status" /C:"User" /C:"CPU"  
  PUTINI('BAT', 'REM 1 ', |
    CRLF & 'TITLE Halt Window Previews' & CHOOSE(~Title,'',' of "' & CLIP(Title) &'*"' ) & |
    CRLF & 'ECHO OFF' & |   
    CRLF & 'COLOR 1E' & |   
    CRLF & 'CLS' & |   
    CRLF & 'REM /v slowwww: TASKLIST /v /FO LIST ' & FI_Filter & |     !Verbose List above table so can see more details
             ' | Findstr /ivb /C:"Session" /C:"Mem" /C:"Status" /C:"User" /C:"CPU"' & |
    CRLF & 'ECHO+ ' & |               !CRLF &'ECHO ={80}' &| !too slow--> 
    CRLF & 'ECHO+ ' & CRLF , BatFN ) 
  PUTINI('BAT','REM 2 ','Confirm Kill List' & |
    CRLF & 'ECHO TASKLIST ' & FI_Filter & |
    CRLF & 'TASKLIST ' & FI_Filter & |
    CRLF & 'REM ECHO ErrorLevel %ErrorLevel%' & |
    CRLF & 'ECHO+' & |
    CRLF & 'CHOICE /C:YN /N /T 99 /D N /M "Halt the above Window Previews? (Y/N)" ' & |
    CRLF & 'IF %ERRORLEVEL% EQU 2 EXIT /B 666' & |
    CRLF & 'ECHO+ ' & CRLF , BatFN )
  PUTINI('BAT','REM 3 ','Kill Previews ' & |
    CRLF & 'ECHO+' & |   
    CRLF & 'ECHO TASKKILL ' & FI_Filter & |
    CRLF & 'TASKKILL ' & FI_Filter & |
    CRLF & '' & |
    CRLF & 'REM ECHO ErrorLevel %ErrorLevel% & PAUSE' & |
    CRLF & ':EndIt' & |
    CRLF & 'DEL %0 ' & CRLF, BatFN )
  !if 2=message('BatFN=' & BatFN &'||Path=' & LongPath() &'||' & FI_Filter,,,'Run|Close' ) THEN EXIT. 
   RUN(BatFN,1) ; REMOVE(BatFN) ; IF RUNCODE()<>666 THEN HALT().  !User Says NO then BAT does EXIT /B 666
!---------
TypeHasAltKey PROCEDURE(LONG TypeNo, LONG FEQ=0) !1=Yes 2=key  FEQ Requires SETTARGET()
B BOOL(1)
T BYTE
  CODE
  CASE TypeNo
  OF CREATE:button OROF CREATE:check OROF CREATE:state3 OROF CREATE:radio ; T=1
  OF CREATE:option OROF CREATE:group ; IF FEQ AND ~FEQ{PROP:Boxed} THEN B=0. ; T=1
  OF CREATE:prompt OROF CREATE:menu OROF CREATE:item OROF CREATE:tab
  ELSE ; B=0
  END
  IF FEQ AND B AND T THEN 
    B=CHOOSE(FEQ{PROP:Text}<>'')      !skip Button with Icon only
    IF ~B AND FEQ{PROP:Key} THEN B=1. !No Text has KEY()
  END
  RETURN B 
!---------
TypeIsLIST PROCEDURE(LONG PropType)!BYTE 1=List 2=Combo Drop +10h
T   BYTE
  CODE
  CASE PropType
  OF CREATE:List  ; T=1
  OF CREATE:Combo ; T=2
  OF CREATE:DropList ; T=11h
  OF CREATE:DropCombo ; T=12h
  END
  RETURN T
!-------------------------     
DB PROCEDURE(STRING DbTxt)
CStr CSTRING(SIZE(DbTxt)+11)
  CODE
  CStr='WndPrv: ' & CLIP(DbTxt)&'<13,10>' ; OutputDebugString(CStr) ; RETURN
!----------------
QueueViewListVLB PROCEDURE(QUEUE VlbQ, STRING QName)
DeclQ QueDeclareType
  CODE
  QueueDeclareGet(VlbQ,DeclQ)
  QueueViewListVLB(VlbQ, QName,DeclQ)
!---------------  
QueueViewListVLB PROCEDURE(QUEUE VlbQ, STRING QName, QueDeclareType DeclQ)
VlbCls CLASS !From Mark Goldberg, but I hacked it to death
FEQ    LONG
ClmCnt USHORT
Chgs   LONG
Init   PROCEDURE(SIGNED xFEQ, USHORT xClmCnt)
VLBprc PROCEDURE(LONG xRow, USHORT xCol),STRING
Contrt PROCEDURE(USHORT ColWd=24)
Expand PROCEDURE()
      END
VMapQ QUEUE,PRE(VMapQ) !Map for HasValue, no &Ref columns
FieldX USHORT
      END
R LONG
X USHORT
P USHORT
Hdg STRING(80)
Fmt ANY
AddCnt  BYTE(10)
PColumn USHORT
Picture STRING(32)
Window WINDOW('VLB'),AT(,,450,200),GRAY,SYSTEM,MAX,FONT('Segoe UI',9),RESIZE
        BUTTON('&Menu'),AT(2,2,25,12),USE(?MenuBtn),SKIP
        BUTTON('&Add'),AT(49,2,25,12),USE(?AddBtn),SKIP,TIP('Add Duplicates of Selected Row')
        ENTRY(@n2),AT(78,3,14,10),USE(AddCnt),SKIP,TIP('Count to Add')
        STRING('times'),AT(95,3),USE(?AddTms)
        BUTTON('&Delete'),AT(118,2,,12),USE(?DelBtn),KEY(DeleteKey),SKIP,TIP('Delete Selected Row')
        PROMPT('&Picture Column:'),AT(173,3,55),USE(?Pic:Pmt),DISABLE,RIGHT
        COMBO(@s32),AT(233,3,59,10),USE(Picture),DISABLE,VSCROLL,TIP('Change Picture for Column'), |
                DROP(16),FROM('@D1|@D2|@D3|@D17|@N11.2|@S255|@T1|@T3|@T4|@T8')
        LIST,AT(1,17),FULL,USE(?List:VLB),HVSCROLL,COLUMN,VCR,FORMAT('40L(2)|M~Col1~Q''NAME'''),FLAT
    END
  CODE
  LOOP X=1 TO RECORDS(DeclQ)
    GET(DeclQ,X) ; IF ~DeclQ:HasValue THEN CYCLE.
    VMapQ:FieldX = DeclQ:FieldX ; ADD(VMapQ)
    Hdg=DeclQ:Name
    IF INSTRING('::VIEWPOSITION',UPPER(DeclQ:Name),1) OR INSTRING('::POSITION',UPPER(DeclQ:Name),1) THEN CYCLE.
    P=INSTRING('::',Hdg,1)+1 ; IF P<2 THEN P=INSTRING(':',Hdg,1). ; IF P THEN Hdg=SUB(Hdg,P+1,99). !Cutoff Pre:
    Fmt=Fmt&'40L(2)|M~' & DeclQ:FieldX & '. <13,10>'& CLIP(Hdg) &'~Q'' Field: <9>'& DeclQ:FieldX & |
            '<13,10> Name: <9>'& CLIP(DeclQ:Name) &' <13,10> Type: <9>'& CLIP(DeclQ:DType) &' '''
  END
  OPEN(Window)
  ?List:VLB{PROP:Format}=Fmt ; CLEAR(Fmt) ; LineHt(?List:VLB) ; LineHt(?Picture)
  0{PROP:Text}='Queue View: ' & QName &' - '& records(VlbQ) & ' Records - '& records(VMapQ) &' Columns  -  Right-Click for Options'
  VlbCls.Init(?List:VLB, Records(VMapQ))
  ACCEPT
    IF FIELD() THEN R=CHOICE(?List:VLB) ; GET(VlbQ,R).
    IF EVENT()=EVENT:NewSelection AND FIELD()=?List:VLB AND KEYCODE()=MouseRight THEN
       SETKEYCODE(0)
       X=?List:VLB{PROPLIST:MouseDownField} ; GET(VMapQ,X)
       CASE POPUP('Copy Cell to Clipboard|View Cell Text|-|Copy Row|View Row Text|-|Hide Column ' & X &'|Change @ Picture')
       OF 1 ; SETCLIPBOARD(WHAT(VlbQ,VMapQ:FieldX))
       OF 2 ; Fmt&=WHAT(VlbQ,VMapQ:FieldX) ; TextViewWindow(QName & ' Cell '& R &','& X ,Fmt,Fmt) ; CLEAR(Fmt)
       OF 3 ; SetClipboard(VlbQ) 
       OF 4 ; TextViewWindow(QName &' Row '&R,VlbQ,VlbQ)
       OF 5 ; ?List:VLB{PROPLIST:width,X}=0 ; IF X=PColumn THEN DISABLE(?Pic:Pmt,?Picture) .
       OF 6 ; Picture=?List:VLB{PROPLIST:Picture,X} ; ?Pic:Pmt{PROP:Text}='&Picture Col ' & X & ':'
              ENABLE(?Pic:Pmt,?Picture) ; SELECT(?Picture) ; PColumn=X
       END
    END
    CASE ACCEPTED()
    OF ?DelBtn ; DELETE(VlbQ) 
    OF ?AddBtn ; LOOP AddCnt TIMES ; ADD(VlbQ) ; END 
    OF ?MenuBtn
        EXECUTE POPUPunder(?MenuBtn,'Copy Queue to Clipboard|-|Contract Column Widths|Expand Column Widths' & |
                        '|-|Copy VLB Format String')
        SetClip2Queue(VlbQ) 
        VlbCls.Contrt()
        VlbCls.Expand()
        SETCLIPBOARD(?List:VLB{PROP:Format})
        END
    OF ?Picture ; ?List:VLB{PROPLIST:Picture,PColumn}=Picture 
                  ?List:VLB{CHOOSE(~INSTRING(lower(picture[1:2]),'@d@t@n@e'),PROPLIST:Left,PROPLIST:Right) ,PColumn}=1
                  DISPLAY 
    END
  END
  RETURN
VlbCls.Init PROCEDURE(SIGNED xFEQ, USHORT xClmCnt)
  CODE
  SELF.FEQ=xFEQ ; SELF.ClmCnt=xClmCnt
  xFEQ{PROP:VLBval} =ADDRESS(SELF) ; xFEQ{PROP:VLBproc}=ADDRESS(SELF.VLBprc)
  RETURN
VlbCls.VLBprc PROCEDURE(LONG xRow, USHORT xCol)
Chg LONG,AUTO
  CODE
  CASE xRow
  OF -1 ; RETURN RECORDS(VlbQ) !Rows
  OF -2 ; RETURN SELF.ClmCnt   !Columns
  OF -3 ; Chg=SELF.Chgs ; SELF.Chgs=CHANGES(VlbQ) ; RETURN CHOOSE(Chg<>SELF.Chgs)
  END
  GET(VlbQ,xRow) ; GET(VMapQ,xCol) ; RETURN WHAT(VlbQ,VMapQ:FieldX)
VlbCls.Contrt PROCEDURE(USHORT ColWd)
  CODE ; LOOP X=1 TO SELF.ClmCnt ; IF SELF.FEQ{PROPLIST:Width,X}>0 THEN SELF.FEQ{PROPLIST:Width,X}=ColWd. ; END ; DISPLAY
VlbCls.Expand PROCEDURE()
  CODE ; SELF.Contrt(SELF.FEQ{PROP:Width}/SELF.ClmCnt)
  !----------------------
QueueDeclareGet PROCEDURE(QUEUE GetQ, QueDeclareType DeclQ)
QA ANY
DT LONG,AUTO
X LONG,AUTO
    CODE
    LOOP X=1 TO 999 ; QA &= WHAT(GetQ,X) ; IF QA &= NULL THEN BREAK.
      CLEAR(DeclQ)
      DeclQ.FieldX=X
      DeclQ.Name=WHO(GetQ,X)
      DT=ClaDataType(QA ,DeclQ.DType, DeclQ.HasValue, DeclQ.Size) 
      DeclQ.DTypeNo=DT  ! DataType:xxx
      DeclQ.HowMany=HOWMANY(GetQ,X)  !Is array?
      IF DeclQ.HowMany>1 THEN DeclQ.HasValue=0. !Future DIM() also IsGroup
      ADD(DeclQ)
    END
 !----------------------   
ReflectDeclareGet PROCEDURE(*GROUP pGrpRef, QueDeclareType DeclQ, STRING pLevelPrefix)
AnyWHAT     ANY 
DT LONG,AUTO
X LONG,AUTO
DimIdx LONG,AUTO
WhoName PSTRING(128) 
GroupPfx PSTRING(128) 
GroupRef &GROUP 
WhatAsStr STRING(4),AUTO 
WhatAsStrLng LONG,OVER(WhatAsStr)
DTypeNm STRING(16)
GroupCnt LONG
RetCount LONG
   CODE
   LOOP X=1 TO 999 
      AnyWHAT &= WHAT(pGrpRef,X) ; IF AnyWHAT &= NULL THEN BREAK.
      CLEAR(DeclQ) 
      DeclQ.FieldX=X
      WhoName=pLevelPrefix & WHO(pGrpRef,X)
      DeclQ.Name=WhoName
      DT=ClaDataType(AnyWHAT ,DTypeNm, DeclQ.HasValue, DeclQ.Size) 
      DeclQ.DTypeNo=DT  ! DataType:xxx
      DeclQ.DType  =DTypeNm
      DeclQ.HowMany=HOWMANY(pGrpRef,X)  !Is array?
      IF DeclQ.HowMany>1 THEN 
         DeclQ.HasValue=0
            DeclQ.Name = WhoName &' []'
            DeclQ.DValue = 'ARRAY DIM( ' & DeclQ.HowMany &' )' 
            IF ISGROUP(pGrpRef,X) THEN
               DeclQ.DValue = 'GROUP....  ' & DeclQ.DValue
               DO Add1Rtn 
               RetCount += 1
!TODO reflect GROUP,DIM()
               CYCLE
            END
            DO Add1Rtn        !Add Array that's NOT Group
            RetCount += 1
            DeclQ.HasValue=1
            LOOP DimIdx=1 TO DeclQ.HowMany 
                AnyWHAT &= WHAT(pGrpRef,X, DimIdx)
                DeclQ.DValue=AnyWHAT
                IF ~DeclQ.DValue OR DeclQ.DValue='0' THEN CYCLE.
                DeclQ.Name = WhoName &' [ ' & DimIdx &' ]'
                DO Add1Rtn 
            END 
            CYCLE 
            
      ELSIF ISGROUP(pGrpRef,X) THEN
            DeclQ.HasValue=0
            DeclQ.DValue = 'GROUP.{9}' 
            GroupPfx = WhoName &'.'
            DeclQ.Name = WhoName & '.{9}'
            DO Add1Rtn 
            RetCount += 1
            GroupRef &= GETGROUP(pGrpRef,X)
            GroupCnt=ReflectDeclareGet(GroupRef,DeclQ,GroupPfx)
            RetCount += GroupCnt
            X += GroupCnt ; GroupCnt=0  !The Group fields will show again so skip over
      ELSE
            IF DeclQ.HasValue THEN 
               DeclQ.DValue = AnyWHAT
            ELSIF DeclQ.DTypeNo=31 THEN
               WhatAsStr=AnyWHAT
               DeclQ.DValue=WhatAsStrLng 
            END
            DO Add1Rtn
            RetCount += 1
      END
   END !loop x 
   RETURN RetCount
Add1Rtn ROUTINE 
  DeclQ.FieldX=RECORDS(DeclQ)+1 
!!!  IF GroupCnt THEN DeclQ.Name=GroupCnt &'.skip.' &DeclQ.Name ; GroupCnt -= 1 ; END !Debug Group Recursion
  ADD(DeclQ)
!----------------------
ListFormatDejaVu PROCEDURE(LONG ListFEQ, STRING WindowID, BYTE AuRevoir=0)
UPropFmt PSTRING(64)      !use ClaFeqName(ListFEQ), number is good 
FmtNow ANY 
SaveFmt ANY 
    CODE    
    UPropFmt='CbWndPrv:' & ListFEQ & PROP:Format & WindowID
    FmtNow=ListFEQ{PROP:Format}
    IF ~AuRevoir THEN
       SaveFmt=SYSTEM{UPropFmt} !See if have old format
       IF SaveFmt THEN 
          ListFEQ{PROP:Format}=SaveFmt
          FmtNow=SaveFmt
       END
       0{UPropFmt}=FmtNow  !Save Opening Format in Window
    ELSE        
       IF FmtNow<>0{UPropFmt} THEN SYSTEM{UPropFmt}=FmtNow.  !If Format changed save in SYSTEM
    END 
!----------------------
CBWndPreviewClass.GroupReflection PROCEDURE(*GROUP G, STRING N)
    CODE ; ReflectGroupOrQueue(SELF,1,G,N)
CBWndPreviewClass.ClassReflection PROCEDURE(*GROUP C, STRING N)
    CODE ; ReflectGroupOrQueue(SELF,2,C,N)
CBWndPreviewClass.FileReflection  PROCEDURE(*FILE F, <STRING N>)
R &GROUP
    CODE ; R &= F{PROP:Record} 
           ReflectGroupOrQueue(SELF,3,R,CHOOSE(~OMITTED(N),N,'File: '&F{PROP:Name}))
CBWndPreviewClass.QueueReflection PROCEDURE(*QUEUE QueueRef, STRING NameOfQueue, BYTE ViewRecords=0) !View Queue in Window
    CODE ; ReflectGroupOrQueue(SELF,4,QueueRef, NameOfQueue,QueueRef,ViewRecords)
!----------------------  
ReflectGroupOrQueue PROCEDURE(CBWndPreviewClass PrvCls,BYTE GrpClsFilQue,*GROUP ThingRef, STRING ThingName,<*QUEUE FromQ>, BYTE ViewQRecs=0) 
DeclareQ QUEUE(QueDeclareType),PRE(DecQ).
SortCls CBSortClass
FindCls CBLocateCls
FindTxt STRING(64),STATIC
RefWnd WINDOW('Class Reflect'),AT(,,380,220),GRAY,SYSTEM,MAX,FONT('Segoe UI',9),RESIZE
        ENTRY(@s64),AT(32,3,155,11),USE(FindTxt),SKIP,FONT('Consolas')
        BUTTON('&Find'),AT(2,2,25,12),USE(?FindNext),SKIP
        BUTTON('Pre&v'),AT(191,2,26,12),USE(?FindPrev),SKIP
        BUTTON('&Copy'),AT(233,2,30,12),USE(?CopyBtn),SKIP,TIP('Copy Declaration')
        BUTTON('&Gen ='),AT(268,2,30,12),USE(?GenEqBtn),SKIP,TIP('Generate Field= Lines to Clipboard')
        BUTTON('&View Queue'),AT(303,2,50,12),USE(?ViewQBtn),SKIP,TIP('View Queue Records')
        LIST,AT(1,18),FULL,USE(?LIST:DeclareQ),VSCROLL,FONT('Consolas',10),FROM(DeclareQ),FORMAT('18C|FM~Fld<13,10>No.~@' & |
                'n3@115L(2)|FM~Field Name~@s64@?61L(2)|FM~Type~C(0)@s16@20R(6)|FM~Type<13,10>No.~C(0)@n3@30R(2)|FM~Size~' & |
                'C(0)@n10@20R(6)|FM~How<13,10>Many~L(2)@n4@25L(6)|FM~Has<13,10>Value~C(0)@n3@135L(2)F~Data Value~@s255@1' & |
                '35L(2)F~Any Long~@n11@'),ALRT(DeleteKey)
    END
X LONG,AUTO
L BYTE,AUTO
M BYTE,AUTO
A ANY
NAVR PSTRING(2),AUTO
DValLong LONG,AUTO
QueRef &QUEUE 
GrpRef &GROUP
RefStr &STRING
RefCStr &CSTRING
RefPStr &PSTRING
AtRGQ  LONG,DIM(4),STATIC 
    CODE
    OPEN(RefWnd) ; DISPLAY 
    IF OMITTED(FromQ) THEN HIDE(?ViewQBtn).
    ReflectDeclareGet(ThingRef,DeclareQ,'')
    0{PROP:Text}=CLIP(ThingName) & CHOOSE(GrpClsFilQue,' GROUP',' CLASS',' FILE',' QUEUE','') & ' Reflection' |
                                 & ' - ' & Records(DeclareQ) & ' Fields' | 
                                 & CHOOSE(~OMITTED(FromQ),' - '& Records(FromQ) & ' Records','') |
                                 & ' - Address=' & Hex8(Address(ThingRef))  
    FindCls.Init(DeclareQ, ?LIST:DeclareQ, ?FindTxt, ?FindNext, ?FindPrev)
    SortCls.Init(DeclareQ,?LIST:DeclareQ)
    PrvCls.AtSetOrSave(1, AtRGQ[]) 
    ListFormatDejaVu(?LIST:DeclareQ,'Reflect')
    IF ViewQRecs THEN POST(EVENT:Accepted,?ViewQBtn).
    ACCEPT 
        CASE ACCEPTED() 
        OF ?CopyBtn  ; SetClip2Queue(DeclareQ)
        OF ?GenEqBtn ; DO GenEqRtn
        OF ?ViewQBtn ; QueueViewListVLB(FromQ, ThingName, DeclareQ)
        END
        CASE FIELD()
        OF ?LIST:DeclareQ
           GET(DeclareQ,CHOICE(?LIST:DeclareQ)) ; DValLong=DecQ:DValue
           CASE EVENT()
           OF EVENT:AlertKey ; IF KEYCODE()=DeleteKey THEN DELETE(DeclareQ).
           OF EVENT:HeaderPressed ; SortCls.HeaderPressed()
           OF EVENT:NewSelection
              IF KEYCODE()=MouseRight THEN 
                 SETKEYCODE(0)
                 NAVR=CHOOSE(DecQ:DTypeNo=31 AND DValLong<>0,'','~')   
                 CASE POPUP('Copy Field Name|Copy Data Value' & | 
                     '|-|' & NAVR& 'Reference Reflection' & |
                           '{{~[31763(700)]Will GPF if Wrong Reference Type|-|' & | !4796
                             'Queue Reflection|' & |
                             'Class Reflection|' & | 
                             'Group Reflection|' & |
                             '-|Simple Data{{STRING|CSTRING|PSTRING}|' & |
                            '}') 
                   OF 1 ; SETCLIPBOARD(DecQ:Name)         
                   OF 2 ; SETCLIPBOARD(DecQ:DValue)         
                   OF 4 ; QueRef &=(DValLong) ; PrvCls.QueueReflection(QueRef,CLIP(DecQ:Name))
                   OF 5 ; GrpRef &=(DValLong) ; PrvCls.ClassReflection(GrpRef,CLIP(DecQ:Name))
                   OF 6 ; GrpRef &=(DValLong) ; PrvCls.GroupReflection(GrpRef,CLIP(DecQ:Name))
                   OF 7 ; RefStr  &= (DValLong) ; TextViewWindow('STRING',RefPStr,RefStr)
                   OF 8 ; RefCStr &= (DValLong) ; TextViewWindow('CSTRING',RefPStr,RefCStr)
                   OF 9 ; RefPStr &= (DValLong) ; TextViewWindow('PSTRING',RefPStr,RefPStr)
                 END
              END
           END
        END 
    END 
    ListFormatDejaVu(?LIST:DeclareQ,'Reflect',1) 
    PrvCls.AtSetOrSave(2, AtRGQ[])
    CLOSE(RefWnd)
    RETURN
GenEqRtn ROUTINE
  LOOP X=1 TO RECORDS(DeclareQ) ; GET(DeclareQ,X) ; L=LEN(CLIP(DecQ:Name)) ; IF L>M THEN M=L. ; END
  L=PopupUnder(?,'Field = |Field =   !Data Type') ; IF ~L THEN EXIT.
  A='    CLEAR(' & CLIP(ThingName) &')<13,10>'
  LOOP X=1 TO RECORDS(DeclareQ) ; GET(DeclareQ,X)
      A=A&'    '& CHOOSE(GrpClsFilQue=2,'SELF.','') & SUB(DecQ:Name,1,M)&' =' & CHOOSE(L=1,'',' {9}! ' & CLIP(DecQ:DType))&'<13,10>'
  END  
  SETCLIPBOARD(A) ; CLEAR(A)
!----------------------
CBWndPreviewClass.ListPROPs  PROCEDURE(LONG ListFEQ, LONG FeqTypeNo, STRING FeqTypeName, STRING FeqName)
!TODO Tab for FROM if it is Text, as 1 string and split into lines 
!add  BUTTON('FORMAT'),AT(254,2,52,14),USE(?FORMATBtn),SKIP,ICON(ICON:Copy)
FEQ     LONG !Is = ListFEQ so can reuse some of the code
PQ      QUEUE(PropQType),PRE(PQ)
Modifier    STRING(2)        !PQ:Modifier
        END
SQ      QUEUE(PropQType),PRE(SQ) !Styles
StyleNo    LONG
SeqNo      LONG
        END
StyleMin   LONG(1),STATIC        
StyleMax   LONG(255),STATIC        
ListQ QUEUE,PRE(LQ)
ColNo    STRING(5)   !1  LQ:ColNo  
Level    LONG        !-     LQ:Level Tree
FieldNo  USHORT      !2  LQ:FieldNo   PROPLIST:FieldNo  *only* when <>ColNo so #Fld#
GroupNo  USHORT      !3  LQ:GroupNo   PL:GroupNo
Header   STRING(32)  !4  LQ:Header    PL:Header
Picture  STRING(16)  !5  LQ:Picture   PL:Picture
Width    STRING(8)   !6  LQ:Width     PL:width
Align    STRING(5)   !7  LQ:Align     PL:Left (PL:LeftOffset) etc  L(2)
HeadAlign STRING(5)  !8  LQ:HeadAlign PL:HeaderLeft
Mods     STRING(16)  !9  LQ:Mods
More     STRING(32)  !10 LQ:More
Format   STRING(256) !11 LQ:Format    PL:Format
Level2   LONG        !-     LQ:Level2 
ColX     SHORT       !12 LQ:ColX
IsGroup  SHORT       !13 LQ:IsGroup   is grp cnt# ,2,3
FieldX   USHORT      !   LQ:FieldX    PROPLIST:FieldNo  *Always* even if =ColNo
      END
MoreColNo EQUATE(10)     
MoreColWd BYTE(36)
ViewColX  SHORT
AllPropList BYTE,STATIC   !TODO Groups have less PROPs 
Format1Col STRING(512)
FormText CSTRING(10000)
FromQ &QUEUE
FromWho PSTRING(32)
FrmFldQ QUEUE(FrmFldQtype),PRE(FrFQ).
FrmDecQ QUEUE(QueDeclareType),PRE(FrDeQ).
Window WINDOW('Prop'),AT(,,450,300),GRAY,SYSTEM,MAX,FONT('Segoe UI',9),RESIZE
        BUTTON('<50>'),AT(2,2,12,13),USE(?UnderBtn),SKIP,FONT('Webdings'),TIP('Move Preview under this Window'),FLAT
        BUTTON('Cl&ose'),AT(19,2,22,14),USE(?CloseBtn),SKIP,STD(STD:Close),FONT(,8)
        BUTTON('PROP:s'),AT(49,2,30,14),USE(?PROPBtn),SKIP,TIP('View PROP: for LIST')
        BUTTON('Resize'),AT(83,2,27,14),USE(?SizeBtn),SKIP,TIP('Resize or move the LIST control')
        BUTTON('Re-Format'),AT(114,2,40,14),USE(?ReFORMATBtn),SKIP,TIP('LIST Re-FORMAT, Column Resize and WYSIWYG FORMAT' & |
                ' Design')
        BUTTON('M | All'),AT(158,2,28,14),USE(?ColReSizeBtn),SKIP,TIP('Make all Columns Resizable (Mod "M") and strip AL' & |
                'RT(Mouseleft).<13,10><13,10>Close this and resize the columns to desired width<13,10>visually, then ret' & |
                'urn here to see the column widths.<13,10>')
        BUTTON('&Copy'),AT(198,2,27,14),USE(?CopyBtn),SKIP
        BUTTON('Format'),AT(230,2,45,14),USE(?FORMATBtn),SKIP,ICON(ICON:Copy),TIP('Copy PROP:Format to Clipboard'),LEFT
        BUTTON('? CW'),AT(305,2,23,14),USE(?HelpBtn),KEY(F2Key),SKIP
        BUTTON('&Scan'),AT(366,2,25,14),USE(?ScanBtn),SKIP,TIP('Scan for undocumented ListProp 7335h-733Fh')
        BUTTON('Halt'),AT(398,2,25,14),USE(?HaltBtn),SKIP,FONT(,8)
        BUTTON('Tests'),AT(429,2,19,14),USE(?TestsBtn),SKIP,TIP('Carl Tests'),FLAT
        CHECK('All Prop'),AT(407,21,36),USE(AllPropList),SKIP,TIP('Show all LISTPROP for Column')
        SHEET,AT(2,20),FULL,USE(?Sheet1),NOSHEET,BELOW
            TAB(' &List Columns '),USE(?TAB:Cols)
                BUTTON('See More'),AT(293,23,37,12),USE(?SeeMoreBtn),SKIP,TIP('Pick any PROPLIST to add to below and vie' & |
                        'w for all Columns')
                BUTTON('From(Q)'),AT(334,23,33,12),USE(?FromQBtn),SKIP,TIP('Show From(Q) Fields Tab to see Queue feeding List')
                LIST,AT(0,36),FULL,USE(?LIST:ListQ),VSCROLL,FONT('Consolas',10),FROM(ListQ),FORMAT('38L(2)|FMT(B)~Column' & |
                        '~C(0)@s5@18C|FM~Fld~L(1)@n4b@18C|FM~Grp~L(1)@n4b@80L(2)|FM~Header~C(0)@s32@?39L(2)|FM~Picture~L' & |
                        '(1)@s16@25L(2)|FM~Width~L(1)@s8@23L(2)|FM~Align~L(0)@s5@20L(2)|FM~Hdr~@s5@26L(2)|FM~Mods~L(1)@s' & |
                        '16@16L(2)|FM~See More~L(1)@s32@20L(2)FT(B)~Format~L(1)@s255@')
            END
            TAB(' &Column LISTPROP '),USE(?TAB:PROPs1)
                TEXT,AT(0,36,,12),FULL,USE(Format1Col),SKIP,VSCROLL,FONT('Consolas',10)
                BUTTON('Pre&v'),AT(293,23,34,12),USE(?ColPrevBtn),SKIP,ICON(ICON:VCRback),TIP('Move to Prev Column'),LEFT
                BUTTON('&Next'),AT(330,23,34,12),USE(?ColNextBtn),SKIP,ICON(ICON:VCRplay),TIP('Move to Next Column'),RIGHT
                LIST,AT(0,51),FULL,USE(?LIST:PQ),VSCROLL,FONT('Consolas',10),FROM(PQ),FORMAT('27L(3)|FM~Equate~L(1)@s5@8' & |
                        '5L(2)|FM~PROPLIST: Property~@s32@?19C|FM~Mod~@s2@?#6#20L(2)F~Value~@s255@#3#'),ALRT(DeleteKey), |
                         ALRT(CtrlC)
            END
            TAB(' &Styles '),USE(?TAB:Style)
                STRING('Style Search Range:'),AT(3,37),USE(?StyRng:Pmt)
                ENTRY(@n6),AT(66,37,26,11),USE(StyleMin),SKIP
                ENTRY(@n6),AT(97,37,26,11),USE(StyleMax),SKIP
                STRING('Style modifiers are Z(#) for Column, and Y for Cell Style with a LONG in the queue.'),AT(142,37), |
                        USE(?StyInfo:Pmt)
                LIST,AT(0,51),FULL,USE(?LIST:SQ),VSCROLL,FONT('Consolas',10),FROM(SQ),FORMAT('25R(2)|FM~Style~C(0)@N_5b@' & |
                        '#6#28L(3)|FM~Equate~L(1)@s5@#1#85L(2)|FM~PROPSTYLE: Property~@s32@?20L(2)F~Value~@s255@#3#'), |
                        ALRT(DeleteKey), ALRT(CtrlC)
            END
            TAB(' &FORMAT() '),USE(?TAB:Formt)
                TEXT,AT(0,36),FULL,USE(FormText),SKIP,HVSCROLL,FONT('Consolas',10)
            END
            TAB(' F&ROM Q '),USE(?TAB:FromQ),HIDE
                BUTTON('&View From(Q)'),AT(290,23,,12),USE(?FromQViewBtn),SKIP,TIP('View From(Q) records in List')
                LIST,AT(0,36),FULL,USE(?LIST:FrmFldQ),VSCROLL,FONT('Consolas',10),FROM(FrmFldQ),FORMAT('28C|FM~List<13>' & |
                        '<10>Column~@s3@28C|FM~Queue<13,10>Field~@n3@135L(2)|FM~Field Name~@s64@?61L(2)|FM~Type~C(0)@s16' & |
                        '@20L(2)F~Value~@s255@'),ALRT(CtrlC), ALRT(DeleteKey)
            END
            TAB('Declare Q'),USE(?TAB:DeclQ),HIDE,TIP('Debug...View TUFO Declare Q')
                BUTTON('&View Decl(Q)'),AT(290,23,,12),USE(?DeclQViewBtn),SKIP,TIP('View From(Q) in List')
                LIST,AT(0,36),FULL,USE(?LIST:FrmDecQ),VSCROLL,FONT('Consolas',10),FROM(FrmDecQ),FORMAT('28C|FM~Queue<13>' & |
                        '<10>Field~@n3@125L(2)|FM~Field Name~@s64@?61L(2)|FM~Type~C(0)@s16@24R(8)|FM~Type<13,10>No.~C(0)' & |
                        '@n3@38R(2)|FM~Size~C(0)@n10@25R(8)|FM~How<13,10>Many~C(0)@n4@24R(8)|FM~Has<13,10>Value~C(0)@n3@' & |
                        '47L(2)|FM~Data Value~@s255@')
            END
        END
    END
!TestNo  SHORT
SysMenuCls SysMenuClass
X      LONG,AUTO
PE     LONG,AUTO
L      LONG,AUTO
Val    STRING(255),AUTO 
ValLng LONG,AUTO
CBAny ANY  
TB EQUATE('<9>')
ScanInclude BYTE
All7Q QUEUE(Parse7QType)
Sort   STRING(6)
      END
ColumnCount  SHORT
CList   CLASS
LoadListQ    PROCEDURE()
Modifiers    PROCEDURE(CONST *STRING ListFormat,*STRING OutModifiers)
PQLoadColumn PROCEDURE(LONG ColNumber, BOOL IsGroup=0)
SQLoadStyles PROCEDURE()
FromIsText   PROCEDURE(STRING PropFrom),BOOL
WndPrvCls    &CBWndPreviewClass
        END
PX  LONG,AUTO        
PY  LONG,AUTO
EVENT       ITEMIZE(Event:User),PRE
ColumnProps  EQUATE() !               !TODO. WIll need to keep the Number of the LIST Column Showing
            END
SortPQCls CBSortClass
  CODE
  SELF.BoxIt() ; GETPOSITION(0,PX,PY) ; FEQ=ListFEQ
  CList.WndPrvCls &= SELF ; CList.LoadListQ()
  OPEN(Window) ; SysMenuCls.Init(Window) ; ?Sheet1{PROP:TabSheetStyle}=1    
  L=?LIST:ListQ ; L{PROP:SELECTED}=1 ; LineHt(L) ; L{PROPLIST:Width,MoreColNo}=0
  SETPOSITION(0,PX,PY) ; SELF.AtSetOrSave(1, AtListPROPs[])
  0{PROP:Text} = 'PROPLIST & Format() - FEQ '& ListFEQ &'  '& FeqName & ' - Columns: ' & ColumnCount & |
                   ' - Type: ' & FeqTypeNo &' '& FeqTypeName
  MakeOverList(?LIST:ListQ) ; MakeOverList(?LIST:PQ) ; MakeOverList(?LIST:SQ) ; MakeOverList(?FormText) ; MakeOverList(?Format1Col)
  L=?LIST:FrmFldQ ; LineHt(L) ; MakeOverList(L)
  DO WindowOpenRtn ; SortPQCls.Init(PQ,?LIST:PQ,2) ; Do DevTipsRtn ; DO Load1ColumnRtn ; ListHelpCW(?HelpBtn)
  ACCEPT
    CASE ACCEPTED()
    OF ?UnderBtn  ; SysMenuCls_SYSCOMMAND(0{PROP:Handle},SMCmd_MoveUnder) 
    OF ?FORMATBtn ; DO CopyFmtRtn
    OF ?CopyBtn ; DO CopyLBRtn
    OF ?ScanBtn ; ScanInclude=1-ScanInclude ; FREE(All7Q) 
                  MESSAGE('Extra Scan Props: ' & CHOOSE(~ScanInclude,'OFF','ON||PropList now includes undefined 7E34h=7E3Fh|Copy button "All Columns" is good way to see.'),'PropList Scan')
    OF ?HelpBtn ; ListHelpCW()
    OF ?HaltBtn ; halt()
    OF ?PROPBtn ; HIDE(0) ; SELF.ControlPROPs(ListFEQ,FeqTypeNo,FeqTypeName,FeqName) ; UNHIDE(0)
    of ?SizeBtn ; SELF.ResizeControl(ListFEQ,FeqTypeNo,FeqTypeName,FeqName)
    of ?ReFormatBtn ; SELF.ListReFORMAT(ListFEQ,FeqTypeNo,FeqTypeName,FeqName) 
                      CList.LoadListQ() ; DISPLAY ; SELECT(?LIST:ListQ) ; FREE(PQ); FREE(SQ)
    of ?ColReSizeBtn ; ClaListColResizable(PWnd,ListFEQ)
    OF ?ColNextBtn OROF ?ColPrevBtn 
        X=CHOICE(?LIST:ListQ)+CHOOSE(FIELD()=?ColNextBtn,1,-1)
        IF X=0 THEN X=1. !RECORDS(ListQ). 
        IF X > RECORDS(ListQ) THEN X -= 1. !X=1
        ?List:ListQ{PROP:Selected}=X
        DO Show1ColumnRtn
    OF ?SeeMoreBtn ; DO SeeMoreRtn
    OF ?FromQBtn ; DO FromQRtn
    OF ?FromQViewBtn ; QueueViewListVLB(FromQ, FromWho, FrmDecQ)    
    OF ?StyleMin OROF ?StyleMax ; CList.SQLoadStyles() 
    OF ?TestsBtn ; DO TestsRtn
    END    
    CASE FIELD()
    OF ?Sheet1
        CASE EVENT()
        OF EVENT:NewSelection
           CASE ?Sheet1{PROP:ChoiceFEQ}
           OF ?TAB:Style  ; IF ~RECORDS(SQ) THEN CList.SQLoadStyles().
           OF ?TAB:PROPs1 ; IF ~RECORDS(PQ) THEN DO Show1ColumnRtn.
           END
        END
    OF ?LIST:ListQ 
        CASE EVENT()
        OF EVENT:NewSelection ; IF INLIST(KEYCODE(),MouseLeft2,EnterKey) THEN DO Show1ColumnRtn.
        END
    OF ?LIST:PQ 
        GET(PQ,CHOICE(?LIST:PQ))
        CASE EVENT()
        OF EVENT:AlertKey ; 
           CASE KEYCODE()
           OF DeleteKey ; DELETE(PQ)
           OF CtrlC ; SETCLIPBOARD('PROPLIST:' & PQ:Name)
           END
        OF EVENT:HeaderPressed ; SortPQCls.HeaderPressed()
        OF EVENT:NewSelection
           CASE KeyCode()
           OF MouseLeft2 ; PropViewWindow('View ' & CLIP(FeqTypeName) & ' FEQ '& FEQ &' '& PQ:Name, PQ, |
                              CHOOSE(PQ:EqtLong <1, PQ:Value, PWnd$Feq{PQ:EqtLong}) )
           OF MouseRight ; X=POPUP('Copy Property<9>Ctrl+C|Copy Value') ; IF X THEN SETCLIPBOARD(CHOOSE(X,'PROPLIST:' & PQ:Name,PQ:Value)). 
           END    
       END !Case Event
    OF ?LIST:SQ
       GET(SQ,CHOICE(?LIST:SQ)) 
       IF EVENT()=EVENT:AlertKey AND KEYCODE()=DeleteKey THEN DELETE(SQ).
       IF EVENT()=EVENT:AlertKey AND KEYCODE()=CtrlC THEN SETCLIPBOARD('PROPSTYLE:' & SQ:Name).
       IF EVENT()=EVENT:NewSelection AND KEYCODE()=MouseRight THEN
          X=POPUP('Copy Style Property|Copy Value') ; IF X THEN SETCLIPBOARD(CHOOSE(X,'PROPSTYLE:' & SQ:Name,SQ:Value)).
       END
    OF ?LIST:FrmFldQ
       GET(FrmFldQ,CHOICE(?LIST:FrmFldQ))
       IF (EVENT()=EVENT:NewSelection AND KEYCODE()=MouseRight AND POPUP('Copy Field Name')=1) |
       OR (EVENT()=EVENT:AlertKey AND KEYCODE()=CtrlC) THEN SETCLIPBOARD(FrmFldQ:Name). 
       IF EVENT()=EVENT:AlertKey AND KEYCODE()=DeleteKey THEN DELETE(FrmFldQ).
    END  !CASE FIELD()
  END !Accept
  SELF.AtSetOrSave(2, AtListPROPs[])
  CLOSE(Window)
  RETURN
!Region Routines ListPROPS
SeeMoreRtn ROUTINE
  IF ~Self.PropPickList(Val,All7Q) THEN EXIT.
  CLEAR(All7Q) ; All7Q:EqtHex=Val ; GET(All7Q,All7Q:EqtHex) ; PE=All7Q:EqtLong
  Val=CLIP(All7Q:Name)&' '&Val ; DO MoreHeadRtn
  LOOP X=1 TO RECORDS(ListQ)
      GET(ListQ,X) ; LQ:More=PWnd$ListFEQ{PE+CHOOSE(~LQ:IsGroup,0,PROPLIST:Group),LQ:ColNo} ; PUT(ListQ)
  END ; DISPLAY ; EXIT
MoreHeadRtn ROUTINE
  L=?LIST:ListQ ; PY=MoreColNo ; L{PROPLIST:Header,PY}=Val ;L{PROPLIST:DefaultTip,PY}=Val
  IF MoreColWd THEN L{PROPLIST:Width,PY}=MoreColWd. ; MoreColWd=0
FromQRtn ROUTINE
  DATA
FromA LONG
QA ANY
DT LONG
FromP LONG
PROP:FromPtr  EQUATE(7A0EH) !ABLLIST.INT &= IMappedListContents ? 0800xxxxH usually, 7C98h also an II ptr?
PROP:FromQRef EQUATE(7A23H) !Showed up in 13505 = &QUEUE unless no FROM(Q) then ?= Interface for WB see 0800xxxxh
  CODE
  IF CList.FromIsText(PWnd$ListFEQ{PROP:From}) THEN EXIT.
  IF PWnd$ListFEQ{PROP:VLBval}>0 THEN
     MESSAGE('LIST is a VLB. If this is a Preview the Queue was replaced by CB VLB Preview.'&|
     '||For a LIST From(Queue) press Shift when clicking the Preview button, then |hold down Shift until the preview appears.','Virtual List Box')
     EXIT
  END
  Val=PWnd$ListFEQ{'FromWho'} ; IF ~Val THEN Val='From(Q)'. ; FromWho=CLIP(Val)
  DO MoreHeadRtn ; ?LIST:FrmFldQ{PROPLIST:Header,3}=FromWho
  IF FromQ&=NULL THEN 
     FromA=PWnd$ListFEQ{'FromQ'}
     IF FromA=0 THEN
        FromA=PWnd$ListFEQ{PROP:FromQRef} 
        IF FromA THEN 
           FromP=PWnd$ListFEQ{PROP:FromPtr} ; ?FromQBtn{PROP:Tip}='7A23h=' & Hex8(FromA) &'<13,10>7A0Eh=' & Hex8(FromP)
           IF ABS(FromA-FromP)<7FFFh THEN FromA=0. !HACK - Interface pointer, not From(Q), can't use this.
        END
     ELSE ; ?FromQBtn{PROP:Tip}='From Q &Ref =' & Hex8(FromA)
     END
     IF FromA>=0 AND FromA<4097 THEN Message('From() needs ?List{{''FromQ''}=&Q set by .InitList().|'&FromA,'LIST') ; EXIT.
     FromQ&=(FromA) ; IF FromQ&=NULL THEN EXIT.
     QueueDeclareGet(FromQ,FrmDecQ) !Use TUFO to get Q declare
     UNHIDE(?TAB:DeclQ)   !Get rid of tab
     LOOP X=1 TO RECORDS(FrmDecQ)
       GET(FrmDecQ,X)
       CLEAR(FrmFldQ) 
       FrFQ:FieldX=FrDeQ:FieldX   ; FrFQ:Name=FrDeQ:Name
       FrFQ:DTypeNo=FrDeQ:DTypeNo ; FrFQ:DType=FrDeQ:DType ; FrFQ:HasValue=FrDeQ:HasValue     
       IF FrDeQ:HowMany > 1 THEN FrFQ:Value='Array[]' 
       ELSIF FrFQ:DTypeNo=31 THEN FrFQ:Value='ANY or &Ref'       
       ELSIF ~FrFQ:HasValue THEN FrFQ:Value='?'
       ELSE
          QA &= WHAT(FromQ,X) ; FrFQ:Value=QA
       END   
       ADD(FrmFldQ)
     END
  END !IF FromQ Null
  LOOP X=1 TO RECORDS(ListQ)
     GET(ListQ,X) 
     LQ:More=''
     IF ~LQ:IsGroup THEN
        FrFQ:FieldX=LQ:FieldX
        GET(FrmFldQ,FrFQ:FieldX) ; IF ERRORCODE() THEN FrFQ:Name='Err#' & LQ:FieldX.
        FrFQ:Column=LQ:ColNo ; PUT(FrmFldQ)
        LQ:More=FrFQ:Name  !Who(FromQ,LQ:FieldX)
     END 
     PUT(ListQ)
  END
  UNHIDE(?TAB:FromQ) ; SELECT(?TAB:FromQ) ; DISPLAY ; EXIT
!-------------  
Load1ColumnRtn ROUTINE
  GET(ListQ,CHOICE(?LIST:ListQ)) ; IF ERRORCODE() THEN EXIT.
  Format1Col = LQ:Format
  FREE(PQ) ; SETTARGET(PWnd) ; CList.PQLoadColumn(LQ:ColX,LQ:IsGroup) ; SETTARGET()
  SORT(PQ,PQ:Name)
Show1ColumnRtn ROUTINE
  DO Load1ColumnRtn ; SortPQCls.SetSortCol(2) ; SELECT(?TAB:PROPs1) ; DISPLAY
TestsRtn ROUTINE
  CASE POPUP('LIST Tips')
  OF 1 ; CBAny='Col<9>Tips' 
        LOOP X=1 TO PWnd$ListFEQ{PROPLIST:Exists,0} 
            CBAny=CBAny&'<13,10>#' & X &'<9>'&  QUOTE(CLIP(PWnd$ListFEQ{PROPLIST:DefaultTip,X}))
        END ; SetClip2Tab2Space(CBAny)
  END
DevTipsRtn ROUTINE  
  IF PWnd$FEQ{PROP:Msg}<>'*DevToolTips*' THEN EXIT. ; DISPLAY
  CASE MESSAGE('There are Developer Tool Tips on the LIST Columns.' & |
           '|The Tips will be removed so as not to clutter this up.' & |
           '||If there were Column Default Tips they will be restored.' &|
           '|Best to NOT paste this FORMAT into your window.|Best to Run Another fresh Window Preview to edit columns.' &|
           '',0{PROP:Text},,'Restore Tips|Run Another|Cancel LIST|Keep Dev Tips')
  OF 2 ; SysMenuCls_SYSCOMMAND(0,SMCmd_RunAgain) ; POST(EVENT:CloseWindow) ; EXIT
  OF 3 ; POST(EVENT:CloseWindow) ; EXIT
  OF 4 ; EXIT
  END 
  LOOP X=1 TO ColumnCount !OF 1   
    PWnd$FEQ{PROPLIST:DefaultTip,X}=PWnd$FEQ{'WndPrvSave_DefaultTip#'&X} 
  END ; PWnd$FEQ{PROP:Msg}='' ; CList.LoadListQ()
!-----------------
WindowOpenRtn ROUTINE
  X=0  ;             PE=PROPLIST:DefaultTip
  X+=1 ; ?LIST:ListQ{PE,X}='Column Index into  {{PROPLIST: , Index}.' & |
                             '<13,10>Column Count returned by Zero Index {{PROPLIST:Exists, 0}' & |
                             '<13,10>Column Exists checked with {{PROPList:Exists, Index}' ! 1 LQ:ColNo 
  X+=1 ; ?LIST:ListQ{PE,X}='PROPLIST:FieldNo  Queue Field No.' ! 2 LQ:FieldNo   PROPLIST:FieldNo        
  X+=1 ; ?LIST:ListQ{PE,X}='PROPLIST:GroupNo<13,10>Every column is in a Group.<13,10>PROPLIST:Exists+PROPLIST:Group,Group# returns column  count for Group' ! 3 LQ:GroupNo   PROPLIST:GroupNo        
  X+=1 ; ?LIST:ListQ{PE,X}='PROPList:Header' ! 4 LQ:Header    PROPList:Header         
  X+=1 ; ?LIST:ListQ{PE,X}='PROPLIST:Picture' ! 5 LQ:Picture   PROPLIST:Picture        
  X+=1 ; ?LIST:ListQ{PE,X}='PROPLIST:Width' ! 6 LQ:Width     PROPLIST:Width          
  X+=1 ; ?LIST:ListQ{PE,X}='Data Alignment<13,10>PROPLIST:Left  :Center :Right :Decimal & (Offset)' ! 7 LQ:Align
  X+=1 ; ?LIST:ListQ{PE,X}='Header Alignment<13,10>PROPLIST:HeaderLeft :HeaderCenter :HeaderRight :HeaderDecimal & (Offset)' ! 8 LQ:HeadAlign PROPLIST:HeaderLeft     
  X+=1 ; ?LIST:ListQ{PE,X}='Modifier Characters in PROPLIST:Format<13,10><13,10>' & ListHelpMods() ! 9 LQ:Mods 
  X+=1 ! More
  X+=1 ; ?LIST:ListQ{PE,X}='PROPLIST:Format<13,10>' & ListHelpMods() !10 LQ:Format    PROPLIST:Format
  ?LIST:PQ{PE,3} = ListHelpMods()
!EndRegion Routines ListPROPS    
!---------------------
!Region Routines for Copy
CopyFmtRtn ROUTINE
  DATA
Fm ANY
LT ANY
Pfx STRING(' FORMAT(''')
  CODE    
  Fm=QUOTE(CLIP(PWnd$ListFEQ{PROP:Format}))
  L=POPUPunder(?,'Copy Format - 1 Line|Format() Wrapped|Format 1 Field/Line')
  CASE L 
  OF 0 ; EXIT
  OF 1 ; Fm=Pfx&Fm&''')'
  OF 2 ; LT=Fm ; Fm=Pfx ; Pfx='' ; Pfx[Size(Pfx)]=''''
         LOOP ; L=LEN(LT) ; IF L<80 THEN BREAK. 
               Fm=Fm & SUB(LT,1,80) &''' &|<13,10>' & Pfx ; LT=SUB(LT,81,L-80)
         END ; Fm=Fm & LT & ''')'
  OF 3 ; Fm=Pfx ; Pfx='' ; Pfx[Size(Pfx)]=''''
         LOOP L=1 TO ?FormText{PROP:LineCount} ; LT=?FormText{PROP:Line,L} ; IF ~LT THEN CYCLE.
              Fm=Fm& CHOOSE(L=1,'',''' &|<13,10>'& Pfx) & LT
         END ; Fm=Fm & ''')' 
  END     
  SETCLIPBOARD(Fm)
CopyLBRtn ROUTINE
  CLEAR(CBAny)
  EXECUTE PopupUnder(?CopyBtn,'List Columns Tab|-|One Column''s PropLIST|All Columns'' PropLIST|-|Styles' & | 
                    CHOOSE(~RECORDS(FrmFldQ),'|~','|') &'From(Queue) Definition' & |
                      '|-|Debug{{ListQ|PQ Col Props|SQ Styles|All7Q Props}') 
  SetClip2Queue(ListQ,,'ColNo<9>Lvl<9>FldNo<9>GrpNo<9>Header<9>Picture<9>Width<9>Align<9>HdAln<9>Mods<9>Format',|
                  'LQ:',1,11)
  DO CopyLB1ColPropsRtn
  DO CopyLBAllColPropsRtn
  DO CopyLBStylesRtn
  SetClip2Queue(FrmFldQ)
  SetClip2Queue(ListQ)
  SetClip2Queue(PQ) 
  SetClip2Queue(SQ) 
  SetClip2Queue(All7Q) 
  END 
  CLEAR(CBAny)   
CopyLB1ColPropsRtn ROUTINE
  DATA
P2Q QUEUE(PQ).    
  CODE
  IF ?Sheet1{PROP:ChoiceFEQ}=?TAB:Cols THEN DO Load1ColumnRtn.
  LOOP X=1 TO RECORDS(PQ) ; GET(PQ,X) ; P2Q=PQ ; ADD(P2Q) ; END 
  CLEAR(P2Q) ; ADD(P2Q) ; P2Q:Name='*** Unused PROPLIST ***' ; ADD(P2Q)
  LOOP X=1 TO RECORDS(All7Q) ; GET(All7Q,X)
       PQ:EqtLong=All7Q:EqtLong ; GET(PQ,PQ:EqtLong) ; IF ~ERRORCODE() THEN CYCLE.
       CLEAR(P2Q) ; P2Q:EqtHex=All7Q:EqtHex ; P2Q:Name=All7Q:Name ; ADD(P2Q)
  END  
  SELF.PropQCopy2Clip(P2Q)
CopyLBAllColPropsRtn ROUTINE
  DATA
SvColNo LONG    
CNoX LONG    
  CODE
  SvColNo=CHOICE(?LIST:ListQ)
  LOOP CNoX=1 TO RECORDS(ListQ) ; GET(ListQ,CNoX)
    ?LIST:ListQ{PROP:Selected}=CNoX ; DO Load1ColumnRtn 
    IF CNoX=1 THEN     
       CBAny='ColNo<9>#Fld#<9>Grp<9>Picture<9>Width<9>Align<9>HdAln<9>Mods<9>Header'
       LOOP X=1 TO RECORDS(All7Q) ; GET(All7Q,X) 
         IF INLIST(All7Q:EqtLong,PROPLIST:Header,PROPLIST:Format,PROPLIST:DefaultTip,PROPLIST:Picture,PROPLIST:Width,PROPLIST:Exists) THEN CYCLE.
         CBAny=CBAny&'<9>'& CLIP(All7Q:Name)  
       END
       CBAny=CBAny&'<9>Header<9>Format'
    END !if 1
    ReplaceInto(LQ:Header,'0DH,0AH','<182>') !Paragraph 
    CBAny=CBAny&'<13,10>'&|
    CLIP(LQ:ColNo)&tb& LQ:FieldNo&tb& LQ:GroupNo&tb&|
    CLIP(LQ:Picture)&tb& CLIP(LQ:Width)&tb&|
    CLIP(LQ:Align)&tb& CLIP(LQ:HeadAlign)&tb& CLIP(LQ:Mods)&tb&CLIP(LQ:Header)
    LOOP X=1 TO RECORDS(All7Q) ; GET(All7Q,X)
         IF INLIST(All7Q:EqtLong,PROPLIST:Header,PROPLIST:Format,PROPLIST:DefaultTip,PROPLIST:Picture,PROPLIST:Width,PROPLIST:Exists) THEN CYCLE.
         PQ:EqtLong=All7Q:EqtLong ; GET(PQ,PQ:EqtLong) ; IF ERRORCODE() THEN CLEAR(PQ).
         IF PQ:Value='-1 Color:None' THEN PQ:Value='-1'.
         CBAny=CBAny&'<9>'& CLIP(PQ:Value)  
    END !Loop X
    CBAny=CBAny&tb&CLIP(LQ:Header)&tb& CLIP(LQ:Format)              
  END !loop CNoX  
  SetClip2Tab2Space(CBAny,1,1)
  ?LIST:ListQ{PROP:Selected}=SvColNo ; DO Load1ColumnRtn 
CopyLBStylesRtn ROUTINE
    CBAny='Style<9>Equate<9>PROPSTYLE:<9>Value<13,10>'
    LOOP X=1 TO RECORDS(SQ) ; GET(SQ,X)
       CBany=CBAny&SQ:StyleNo &'<9>'& SQ:EqtHex &'<9>'& Clip(SQ:Name) &'<9>'& Clip(SQ:Value) &'<13,10>'
    END ;  SetClip2Tab2Space(CBAny,2,1)
!EndRegion Routines for Copy
!---------------------
!Region CList Class
CList.LoadListQ    PROCEDURE()
ColX   USHORT,AUTO
InX    LONG,AUTO
GrpNo  SHORT
GrpCnt SHORT
LastGrpNo SHORT
ColsInGrp SHORT
Fmt      STRING(1024),AUTO 
FmtOfGrp STRING(1024)
FmtGrpNo SHORT
  CODE
  SETTARGET(PWnd) ; FREE(ListQ) ; CLEAR(FormText)
  ColumnCount=ListFEQ{PROPLIST:Exists,0}
  LOOP ColX=1 TO 1024
     IF ~ListFEQ{PROPList:Exists, ColX} THEN BREAK.
     
     GrpNo =  ListFEQ{PROPLIST:GroupNo, ColX}
     IF GrpNo <> LastGrpNo THEN
        IF FmtGrpNo THEN DO EndFmtGrpRtn.
        LastGrpNo = GrpNo
        ColsInGrp = ListFEQ{PROPLIST:GroupNo + PROPLIST:Group, ColX}
        IF ColsInGrp THEN
           GrpCnt += 1
           CLEAR(ListQ)
           LQ:IsGroup = GrpCnt
           LQ:Level = 1 ;   LQ:Level2 = LQ:Level
           LQ:ColX =ColX
           LQ:ColNo='Grp ' & GrpCnt  !GrpNo !TODO =GrpCnt
           LQ:GroupNo = GrpNo
           LQ:Width  = ListFEQ{PROPLIST:width + PROPLIST:Group, ColX}  !If Blank then fields
           !LQ:Header = QUOTE(CLIP(ListFEQ{PROPList:Header+ PROPLIST:Group, ColX}))
           Fmt=ListFEQ{PROPList:Header+ PROPLIST:Group, ColX} ; ReplaceInto(Fmt,'<13,10>',' ')
           LQ:Header=Fmt 
           LQ:HeadAlign=ClaListColAlign(ListFEQ,ColX,1,1)
           Fmt = ListFEQ{PROPLIST:Format+ PROPLIST:Group, ColX}
           InX=INSTRING(']',Fmt)   !Group Fmt includes all member columns
           Fmt=SUB(Fmt,Inx+1,9999)
           LQ:Format = ' '& QUOTE(CLIP(Fmt)) 
           FormText=CLIP(FormText)&'[<13,10>' ; FmtOfGrp=Fmt ; FmtGrpNo=GrpNo !After ] 
           SELF.Modifiers(Fmt,LQ:Mods)
           LQ:Align=ClaListColAlign(ListFEQ,ColX,1) 
           LQ:Picture='  '& ColsInGrp &' Cols'
           ADD(ListQ)
        END
     END

!Tab for Styles   
     !----
     CLEAR(ListQ) 
     LQ:Level = CHOOSE(ColsInGrp=0,1,2) ; LQ:Level2 = LQ:Level
     LQ:ColX =ColX
     LQ:ColNo=ColX
     LQ:FieldX = ListFEQ{PROPLIST:FieldNo, ColX} ; IF LQ:FieldX<>ColX THEN LQ:FieldNo=LQ:FieldX. !20
     LQ:GroupNo = GrpNo         
     LQ:Picture = ListFEQ{PROPLIST:Picture, ColX}
     LQ:Width   = ListFEQ{PROPLIST:width, ColX}
     Fmt=ListFEQ{PROPList:Header, ColX} ; ReplaceInto(Fmt,'<13,10>',' ')
     LQ:Header = Fmt !QUOTE(CLIP(ListFEQ{PROPList:Header, ColX}))
     LQ:HeadAlign=ClaListColAlign(ListFEQ,ColX,,1)
     Fmt = ListFEQ{PROPLIST:Format, ColX}
     SELF.Modifiers(Fmt,LQ:Mods)
     LQ:Format=' '& QUOTE(CLIP(Fmt))
     FormText=CLIP(FormText)&QUOTE(CLIP(Fmt))&'<13,10>'
     LQ:Align=ClaListColAlign(ListFEQ,ColX)
     ADD(ListQ)
  END
  SETTARGET() ; IF FmtGrpNo THEN DO EndFmtGrpRtn.
  RETURN 
EndFmtGrpRtn ROUTINE
  FmtGrpNo=0 ; FormText=CLIP(FormText) &']'& QUOTE(CLIP(FmtOfGrp))&'<13,10>'      
!-----------------------------------
CList.Modifiers    PROCEDURE(CONST *STRING Fmt,*STRING Modz)
!TODO should I just check the PROPs?`  Do it both ways and see.
!Rather than check each property I thought I would parse the string
!33R(2)|*~Rate~C(0)@n7.3b@ 
!Group: (94)|FM~TRS INFORMATION~L(0)'   Starts with (width) ends with  Align LCRD
LenFmt  USHORT,AUTO
FX      SHORT,AUTO
MX      USHORT
InThing     BYTE
ThingEnd    STRING(1),AUTO 
    CODE 
    LenFmt=LenFastClip(Fmt)
    FX=STRPOS(Fmt,'[^0-9()LRCD]',0) - 1    !Bypass  ####(LRCD)
    IF FX < 0 OR FX > LenFmt THEN RETURN.
    LOOP WHILE FX < LenFmt AND MX < SIZE(Modz)
        FX += 1
        IF InThing THEN 
           IF Fmt[FX]<>ThingEnd THEN CYCLE.
           InThing=0 ; CYCLE
        END 
!FormatDelim1    STRING('''~(@#')       !~Heading~  (Offest#)  @picture@  #FieldNo#  quotes? for tool tip
!FormatDelim2    STRING('''~)@#')       !Note Q '' must be first to spot double
        CASE Fmt[FX]
        OF '~' OROF '''' OROF '@' ; InThing=1 ; ThingEnd=Fmt[FX] 
        OF '('                    ; InThing=1 ; ThingEnd=')'     
        OF '#'                    ; InThing=2 ; ThingEnd=Fmt[FX]  !Show the #
        OF 'L' OROF 'R' OROF 'C' OROF 'D' ; CYCLE  !Align after ~Header~ 
        OF '0' TO '9' ; CYCLE
        END
        IF InThing=1 THEN CYCLE.
        MX += 1
        Modz[Mx] = Fmt[FX]
    END
    
    RETURN
!----------------------------------- 
CList.PQLoadColumn    PROCEDURE(LONG ColX, BOOL IsGroup=0) 
CP GROUP
CP1 STRING('7E00Underline=_ 7E01Resize=M 7E02RightBorder=| 7E03Header=~~ 7E04Width 7E05Picture=@ 7E06Fixed=F 7E07Scroll=S'&|
         ' 7E08LastOnLine=/ 7E09Locator=? 7E0AExists 7E0DGroupNo 7E0EFieldNo=# 7E0FFormat 7E10Left=L 7E11LeftOffset'&|
         ' 7E12Right=R 7E13RightOffset 7E14Center=C 7E15CenterOffset 7E16Decimal=D 7E17DecimalOffset 7E18HeaderLeft=L'&|
         ' 7E19HeaderLeftOffset 7E1AHeaderRight=R 7E1BHeaderRightOffset 7E1CHeaderCenter=C 7E1DHeaderCenterOffset'&|
         ' 7E1EHeaderDecimal=D 7E1FHeaderDecimalOffset 7E20Icon=I 7E21Color=* 7E22Tree=T 7E23TreeLines=(L 7E24TreeBoxes=(B'&|
         ' 7E25TreeIndent=(I 7E26TreeOffset=(1 7E27TreeRoot=(R 7E28textColor=E1 7E29backColor=E2 7E2AtextSelected=E3'&|
         ' 7E2BbackSelected=E4 7E2CCellStyle=Y 7E2DColStyle=Z 7E2EIconTrn=J 7E2FTip=P 7E30DefaultTip=Q 7E31barFrame=B'&|
         ' 7E32hdrTextColor=HT 7E33hdrBackColor=HB 7E0BResvd2 7E0CResvd3 ')
Scan STRING('7E34Scan34 7E35Scan35 7E36Scan36 7E37Scan37 7E38Scan38 7E38Scan38 7E39Scan39 7E3AScan3A 7E3BScan3B 7E3CScan3C 7E3DScan3D 7E3EScan3E 7E3FScan3F ')
    END
E1 USHORT,AUTO
P7Q Parse7QType
IsTree BYTE     
  CODE
  IF ~ScanInclude THEN CLEAR(CP.Scan).
  IF ~RECORDS(All7Q) THEN
     EquateXStringParse(All7Q,4,CP)
     LOOP X=RECORDS(All7Q) TO 1 BY -1  
        GET(All7Q,X) 
        E1=INSTRING('=',All7Q:Name) ; IF E1 THEN All7Q:Name=SUB(All7Q:Name,1,E1-1).
        IF All7Q:Name[1] >= 'a'THEN !Color
           All7Q:Name[1]=UPPER(All7Q:Name[1]) ; All7Q:Sort='c'&All7Q:EqtHex
        END 
        IF All7Q:Name[1:4]='Scan' THEN All7Q:Sort='~'.
        CASE All7Q:EqtLong
       ! OF PROPLIST:Header OROF PROPLIST:Format OROF PROPLIST:DefaultTip OROF PROPLIST:Picture OROF PROPLIST:Width OROF PROPLIST:Exists
       !    DELETE(All7Q) ; CYCLE
        OF PROPLIST:Left TO PROPLIST:DecimalOffset ; All7Q:Sort='j'&All7Q:EqtHex
        OF PROPLIST:HeaderLeft TO PROPLIST:HeaderDecimalOffset ; All7Q:Sort='k'&All7Q:EqtHex
        END  
        PUT(All7Q) 
     END
     SORT(All7Q,All7Q:Sort,All7Q:Name,All7Q:EqtLong)
  END

  CLEAR(PQ)
  IF ~IsGroup
      SELF.WndPrvCls.PropQAdd(PQ,ColX,'  Column ' & ColX,'ListFEQ{{PROPLIST:xxx, ' & ColX & '}') 
  ELSE        
      SELF.WndPrvCls.PropQAdd(PQ,IsGroup,'  Group ' & IsGroup,'Group Index #' & ColX & ' {{PROPLIST: + PROPLIST:Group(40h), ' & ColX & '}' )  
  END
  IsGroup=CHOOSE(~IsGroup,0,PROPLIST:Group)
  EquateXStringParse(P7Q,4,CP)
  IsTree=ListFEQ{PROPLIST:Tree,ColX}
  LOOP X=1 TO RECORDS(P7Q) ; GET(P7Q, X)
    PE=P7Q:EqtLong + IsGroup
    Val=ListFEQ{PE,ColX}
    !IF ~Val AND AllPropList THEN Val='<9>'.
    IF ~Val THEN 
       IF AllPropList OR (IsTree AND UPPER(P7Q:Name[1:4])='TREE') THEN Val='<9>'.  !Tree Props show False="Surpress()"
    END
    IF ~Val THEN CYCLE.  ! ; IF Val='0' THEN    keep all '0' for SYSTEM            
    ValLng = Val
    IF P7Q:Name[1]>='a' THEN
       P7Q:Name[1]=UPPER(P7Q:Name[1])
       ClaColorEquate(Val)
    ELSIF INSTRING(CHR(13),Val) THEN
        Val=QUOTE(CLIP(Val))
    END
    CLEAR(PQ)
    E1=INSTRING('=',P7Q:Name)
    IF E1 THEN
       PQ:Modifier=SUB(P7Q:Name,E1+1,2)
       IF ValLng=1 AND PQ:Modifier[1]='(' AND PQ:Modifier<>'(1' AND UPPER(P7Q:Name[1:4])='TREE' THEN 
          PQ:Modifier=''  !Tree Modifiers are for Surpress and active when False, except (1) Offset 
       END 
       P7Q:Name=SUB(P7Q:Name,1,E1-1)
    END        
    SELF.WndPrvCls.PropQAdd(PQ,PE,P7Q:Name,Val) 
  END !Loop
  RETURN
!-----------------------------------
CList.SQLoadStyles PROCEDURE() !PROPSTYLE:Last=7D3FH
CP STRING('7D10FontName 7D11FontSize 7D13FontStyle 7D14textColor 7D15backColor 7D16textSelected 7D17backSelected 7D18Picture 7D19CharSet 7D1AbarFrame ') ! 7D12FontColor same as Text
P7Q Parse7QType
StyNo  LONG,AUTO
CntSty LONG
LstCnt LONG
  CODE
  SETCURSOR(CURSOR:Wait)
  FREE(SQ) ; DISPLAY
  EquateXStringParse(P7Q,4,CP)
  IF StyleMax < StyleMin THEN StyleMax=StyleMin+10.
  LOOP StyNo=StyleMin TO StyleMax 
    LOOP X=1 TO RECORDS(P7Q) ; GET(P7Q, X)
      PE=P7Q:EqtLong
      Val=PWnd$ListFEQ{PE,StyNo}
!Tested C10-11, if any STYLE is defined then always a FontName. Set to Blank you get System
      IF X=1 AND ~Val THEN BREAK. !If any STYLE always Font (#1) so FASTER to break
      IF ~Val THEN Val='<9>'.  !Show All always
      ValLng = Val
      IF P7Q:Name[1]>='a' THEN
         P7Q:Name[1]=UPPER(P7Q:Name[1])
         ClaColorEquate(Val)
      ELSIF PE=7D13h THEN
          IF ValLng=0 THEN CYCLE. ; Val=ClaFontStyle(ValLng)
     ! ELSIF PE=7D19h AND ValLng=0 THEN CYCLE  !CharSet=0 on all
      END
      CLEAR(SQ)
      SQ:StyleNo=StyNo
      SQ:SeqNo=CntSty
      SELF.WndPrvCls.PropQAdd(SQ,PE,P7Q:Name,Val) ; CntSty+=1 ; IF CntSty=30 THEN DISPLAY.
    END !Loop X
    IF LstCnt<>CntSty THEN
       CntSty+=1 ; LstCnt=CntSty
       CLEAR(SQ) ; SQ:StyleNo=StyNo ; SQ:Name=ALL('-') ; SQ:Value=SQ:Name
       SQ:SeqNo=CntSty ; ADD(SQ)           
    END
  END !Loop StyNo
  IF RECORDS(SQ) THEN
     SORT(SQ,SQ:SeqNo)
  ELSE
     CLEAR(SQ) ; SQ:Name='No Styles Found' ; ADD(SQ) 
     SQ:Name=ALL('-') ; SQ:Value=SQ:Name ; SQ:EqtHex=SQ:Name ; ADD(SQ)
     LOOP X=1 TO RECORDS(P7Q) ; GET(P7Q, X)
       SELF.WndPrvCls.PropQAdd(SQ,P7Q:EqtLong,P7Q:Name,'<9>') 
     END        
  END
  DISPLAY
  SETCURSOR()
  RETURN
!---------------
CList.FromIsText PROCEDURE(STRING PropFrom)
    CODE
    IF ~PropFrom THEN RETURN FALSE. ; ReplaceInto(PropFrom,'|#','<9>#')
    Message('LIST is FROM(''text''):|-{40}|'&PropFrom,'From()') ; RETURN True   
!EndRegion CList Class
!==========================================
CBWndPreviewClass.ListReFORMAT  PROCEDURE(LONG FEQ, LONG FeqTypeNo, STRING FeqTypeName, STRING FeqName) 
ListFEQ LIKE(FEQ)
MsgCaption EQUATE('Re-Format LIST Columns')
X     LONG,AUTO
C     LONG,AUTO
ColX  LONG,AUTO
ColNo LONG(1)
ColNoMax LONG(999)
SaveHasSortColumn LONG
SaveSortColumn LONG
SaveDefHdrBackColor LONG(-1)
SaveDefHdrTextColor LONG(-1)
SaveSortBackColor LONG(-1)
SaveSortTextColor LONG(-1)
S1Q QUEUE,PRE(S1Q)  !Simple 1s
Poz     &LONG       !S1Q:Poz       
Haz     &LONG       !S1Q:Haz       
PROP    LONG        !S1Q:PROP      !{Prop:xx}
FeqInp  LONG        !S1Q:FeqInp 
    END
L1Q QUEUE(S1Q),PRE(L1Q)  !Simple 1s for List 
    END 
PosDataType GROUP,TYPE
ClmWd       LONG       ! Poz:ClmWd      Haz:ClmWd
ClmResize      LONG 	!Poz:ClmResize       PROPLIST:Resize
ClmRightBorder LONG 	!Poz:ClmRightBorder  PROPLIST:RightBorder
ClmUnderline   LONG 	!Poz:ClmUnderline    PROPLIST:Underline
ClmFixed       LONG 	!Poz:ClmFixed        PROPLIST:Fixed
ClmLastOnLine  LONG 	!Poz:ClmLastOnLine   PROPLIST:LastOnLine
ClmScroll      LONG 	!Poz:ClmScroll       PROPLIST:Scroll 
ClmColStyle    LONG 	!Poz:ClmColStyle     PROPLIST:ColStykeScroll 
ClmHeader      STRING(300)  !Poz:ClmHeader       PROPLIST:Header
ClmTree    LONG
ClmTr1Base LONG
ClmTrBoxes LONG
ClmTrLevel LONG
ClmTrLines LONG
ClmTrRoot  LONG
ClmALCRDnD BYTE  ! Poz:ClmALCRDnD     Align Left Cent Right Dec DATA
ClmALCRDnH BYTE  ! Poz:ClmALCRDnH     Align Left Cent Right Dec Header
ClmOffsetD SHORT ! Poz:ClmOffsetD
ClmOffsetH SHORT ! Poz:ClmOffsetH
ClmFORMAT  STRING(1024) ! Poz:ClmFORMAT Haz:ClmFORMAT Waz:ClmFORMAT 
ClmPicture STRING(128)  ! Poz:ClmPicture    Haz:ClmPicture  For Non-Group
TRN     LONG  ! Poz:TRN     Haz:TRN
Flat    LONG  ! Poz:Flat    Haz:Flat
HScroll LONG  ! Poz:HScroll PROP:HScroll
VScroll LONG  ! Poz:VScroll PROP:VScroll
VCR     LONG  ! Poz:VCR
WndWide LONG  ! Poz:WndWide Haz:WndWide 
LstWide LONG  ! Poz:LstWide Haz:LstWide 
         END
Try2:Disable BYTE,DIM(2)       ![1]=Was [2]=Now
Try2:Hide    BYTE,DIM(2)       !
HideUnHide   BYTE(1),STATIC   !VScroll needs,a Group move
BeforeWaz GROUP(PosDataType),PRE(B4Waz).    !Original Position Waz
Poz       GROUP(PosDataType),PRE(Poz).    !This Screen Position
Haz       GROUP(PosDataType),PRE(Haz).    !Preview Position Now

AlignQ  QUEUE,PRE(AlnQ) !Alignment choices for ?LIST:AlignQ
Desc        STRING(8)   !AlnQ:Desc   
PropList    LONG        !AlnQ:PropList  eg PropList:Left
PrOffset    LONG        !AlnQ:PrOffset  eg PropList:LeftOffset
Value       BYTE        !AlnQ:Value
        END
AlignHQ  QUEUE(AlignQ),PRE(AlnHQ) 
         END

ListQ QUEUE,PRE(LQ)
ColNo     STRING(5)    !LQ:ColNo  
Header    STRING(32)   !LQ:Header
HeaderTip STRING(300)  !LQ:HeaderTip also undo
Width     STRING(8)    !LQ:Width    
Align     STRING(5)    !LQ:Align    
HeadAlign STRING(5)    !LQ:HeadAlign
Picture   STRING(16)   !LQ:Picture  
Format255 STRING(255)  !LQ:Format255
Level     LONG         !LQ:Level 
Format    STRING(1024) !LQ:Format
ColX      SHORT        !LQ:ColX  if Group Negative
IsGroup   BYTE         !LQ:IsGroup=PROPLIST:Group=40h
B4WazFormat STRING(1024)
      END
Window WINDOW('ReFormat'),AT(,,483,300),GRAY,IMM,SYSTEM,FONT('Segoe UI',9),RESIZE
        TOOLBAR,AT(0,0,483,30),USE(?TOOLBAR1)
            BUTTON('Save'),AT(2,1,25,12),USE(?SaveBtn),SKIP,TIP('Save size changes and Close<13,10>T' & |
                    'ool Tip is updated with Before and After Size.')
            BUTTON('Halt'),AT(63,1,25,12),USE(?HaltBtn)
            BUTTON('Cancel'),AT(31,1,29,12),USE(?CancelBtn),SKIP,TIP('Undo size changes and Close')
            BUTTON('Undo'),AT(2,14,25,12),USE(?UndoBtn),SKIP,TIP('Undo size changes back to Original' & |
                    ', but stay on this window')
            BUTTON('&PROPs'),AT(93,1,27,12),USE(?PropsBtn),SKIP
            BUTTON('LIST'),AT(125,1,21,12),USE(?LISTBtn),SKIP,TIP('PROPLIST Viewer')
            LIST,AT(150,2,32,10),USE(Cfg:ResizeSnapTo),FONT(,8),TIP('How to Position Preview Window ' & |
                    'under the Resizer'),DROP(5,44),FROM('Right Snap|#1|Centr Snap|#2|Left Snap|#3|N' & |
                    'one|#9')
            BUTTON('F'),AT(31,14,13,12),USE(?FontBtn),SKIP,FONT(,10,,FONT:bold+FONT:italic),TIP('Font...')
            BUTTON('C'),AT(47,14,13,12),USE(?ColorBtn),SKIP,FONT(,10,COLOR:Red,FONT:bold), |
                    TIP('Pick Color PROPs')
            BUTTON('<176>'),AT(63,14,13,12),USE(?GdLinesBtn),SKIP,FONT('Wingdings',14, |
                    COLOR:GRAYTEXT,FONT:bold),TIP('Guide and Grid Lines')
            BUTTON('Tip...'),AT(93,14,27,12),USE(?SeeTipBtn),SKIP,TIP('See Column Tool Tip')
            BUTTON('Test'),AT(125,14,21,12),USE(?TestBtn),SKIP,FLAT
            BUTTON('?'),AT(149,14,12,12),USE(?HelpBtn),SKIP,TIP('CW Help')
            BUTTON('<50>'),AT(170,14,12,12),USE(?UnderBtn),SKIP,FONT('Webdings'),TIP('Move Under Win' & |
                    'dows to align under this one')
        END
        PANEL,AT(117,30,1,89),USE(?VertRightPanel),BEVEL(0,0,6000H)
        GROUP,AT(2,30,112,74),USE(?Group:AT)
            PROMPT('&Width:'),AT(3,32),USE(?Wd:Pmt)
            SPIN(@n_5),AT(25,31,38,11),USE(Poz:ClmWd),HSCROLL,RIGHT,TIP('List Column Width - Shift L' & |
                    'eft- or Right+'),RANGE(0,99999)
            PROMPT('C#'),AT(69,32),USE(?ColNo:Pmt)
            SPIN(@n_4),AT(80,31,33,11),USE(ColNo),SKIP,HVSCROLL,RIGHT,TIP('LIST Column Number to Adj' & |
                    'ust<13,10>Page up and Page Down change the Column'),RANGE(1,9999)
            PROMPT('Align:'),AT(3,46),USE(?AlignQ:Pmt)
            LIST,AT(25,46,40,10),USE(?List:AlignDQ),MSG('Poz:ClmALCRDnD'),DROP(5),FROM(AlignQ), |
                    FORMAT('20L(2)@s8@')
            SPIN(@n-4),AT(70,46,29,10),USE(Poz:ClmOffsetD),HSCROLL,RIGHT,TIP('Data Indent from Align')
            PROMPT('Head:'),AT(3,60),USE(?AlignHQ:Pmt)
            LIST,AT(25,60,40,10),USE(?List:AlignHQ),MSG('Poz:ClmALCRDnH'),DROP(5),FROM(AlignHQ), |
                    FORMAT('20L(2)@s8@')
            SPIN(@n-4),AT(70,60,29,10),USE(Poz:ClmOffsetH),HSCROLL,RIGHT,TIP('Heading Indent from Align')
            BUTTON,AT(101,48,11,12),USE(?HeadCopyBtn),SKIP,ICON(ICON:Copy),TIP('Copy Column Header t' & |
                    'o clipboard for paste into List Box Formatter'),FLAT
            BUTTON('<76>'),AT(101,60,11,12),USE(?HeadUndoBtn),SKIP,FONT('Wingdings 3',11), |
                    TIP('UNDO Heading Text Change'),FLAT
            TEXT,AT(3,74,110,28),USE(Poz:ClmHeader),VSCROLL,TIP('Column Header PROPLIST:Header<13>' & |
                    '<10>Can use to test fit data')
        END
        GROUP,AT(122,28,61,36),USE(?Group:Modifiers)
            CHECK('Resizable M'),AT(123,28),USE(Poz:ClmResize),SKIP,TIP('Resizable Column "M" PROPLI' & |
                    'ST:Resize')
            CHECK('Right Border |'),AT(123,37),USE(Poz:ClmRightBorder),SKIP,TIP('Right Border Line "' & |
                    '|" PROPLIST:RightBorder')
            CHECK('Underline _'),AT(123,46),USE(Poz:ClmUnderline),SKIP,TIP('Underline field "_" PROP' & |
                    'LIST:Underline')
            CHECK('Fixed'),AT(123,55),USE(Poz:ClmFixed),SKIP,TIP('Fixed will NOT Scroll "F" PROPLIST' & |
                    ':Fixed')
            CHECK('Last /'),AT(154,55),USE(Poz:ClmLastOnLine),SKIP,TIP('Next field to appears on a n' & |
                    'ew line<13,10>(only used on a field within a group).<13,10>"/" PROPLIST:Poz:Las' & |
                    'tOnLine')
        END
        GROUP,AT(122,64,61,29),USE(?Group:Tree)
            CHECK('Tree'),AT(123,65),USE(Poz:ClmTree),SKIP,TIP('LIST is a tree control "T(suppress)"' & |
                    ' PROPLIST:Tree'),READONLY
            CHECK('One'),AT(123,74),USE(Poz:ClmTr1Base),SKIP,TIP('Root is Level One "1" PROPLIST:Tre' & |
                    'eOffset')
            CHECK('Box'),AT(123,83),USE(Poz:ClmTrBoxes),SKIP,TIP('Suppress expansion boxes "B" PROPL' & |
                    'IST:TreeBoxes')
            CHECK('Level'),AT(154,65),USE(Poz:ClmTrLevel),SKIP,TIP('Suppress the connecting lines be' & |
                    'tween levels "I" PROPLIST:TreeIndent')
            CHECK('Lines'),AT(154,74),USE(Poz:ClmTrLines),SKIP,TIP('Suppress the connecting lines be' & |
                    'tween levels "L" PROPLIST:TreeLines')
            CHECK('Root'),AT(154,83),USE(Poz:ClmTrRoot),SKIP,TIP('Suppress the connecting lines to t' & |
                    'he root level "R" PROPLIST:TreeRoot')
        END
        PROMPT('Style:'),AT(123,97),USE(?Poz:ClmCS)
        ENTRY(@n4),AT(145,97,19,9),USE(Poz:ClmColStyle),SKIP,TIP('Column Style "Z(#)" PROPLIST:ColStyle')
        PROMPT('Scroll:'),AT(123,110),USE(?Poz:ClmSB)
        ENTRY(@n4),AT(145,109,19,9),USE(Poz:ClmScroll),SKIP,TIP('Scrollable Column (Value) is DLUs. ' & |
                '"S(#)" PROPLIST:Scroll')
        PROMPT('List'),AT(175,97),USE(?ListWH:Pmt:),TRN
        SPIN(@n4),AT(193,97,26,9),USE(Poz:LstWide),SKIP,RIGHT,TIP('LIST Width'),RANGE(0,9999)
        PROMPT('Wnd'),AT(175,110),USE(?WndWH:Pmt:3),TRN
        SPIN(@n4),AT(193,109,26,9),USE(Poz:WndWide),SKIP,RIGHT,TIP('Window Width'),RANGE(0,9999)
        GROUP,AT(192,28,110,34),USE(?Group:ChecksTop)
            CHECK('Hide'),AT(192,28),USE(Try2:Hide[2]),SKIP
            CHECK('Disable'),AT(192,37,33),USE(Try2:Disable[2]),SKIP
            CHECK('Flat'),AT(192,46),USE(Poz:Flat),SKIP
            CHECK('TRN'),AT(192,55),USE(Poz:TRN),SKIP
            CHECK('Hide/UnHide'),AT(248,32),USE(HideUnHide),SKIP,TIP('Hide, Change, then UnHide. Wor' & |
                    'ks better espcially with groups.<13,10>TODO could have a Hide/Unhide Children o' & |
                    'f Group.')
        END
        GROUP,AT(192,64,34,29),USE(?Group:Checks)
            CHECK('HScroll'),AT(192,65),USE(Poz:HScroll),SKIP,TIP('LIST Horizontal Scrollbar')
            CHECK('VScroll'),AT(192,83),USE(Poz:VScroll),SKIP,TIP('LIST Vertical Scrollbar<13,10>HID' & |
                    'E then UnHide to Repaint')
            CHECK('VCR'),AT(192,74),USE(Poz:VCR),SKIP
        END
        PROMPT('@P'),AT(3,107),USE(?Picture:Pmt)
        ENTRY(@s64),AT(17,106,81,11),USE(Poz:ClmPicture),TIP('Clear to reset to original Picture')
        BUTTON('<113>'),AT(101,106,11,12),USE(?ResetFormatBtn),SKIP,FONT('Webdings',11),TIP('Reset t' & |
                'he Format.<13,10>Many List Header changes will not happen,<13,10>e.g.changing the n' & |
                'umber of lines.'),FLAT
        PROMPT('Before:'),AT(2,3),USE(?AtWaz:Pmt),HIDE
        TEXT,AT(2,3,,11),FULL,USE(B4Waz:ClmFORMAT),SKIP,FONT('Consolas'),COLOR(COLOR:BTNFACE), |
                TIP('Before Format'),SINGLE
        PROMPT('After:'),AT(2,17),USE(?AtPoz:Pmt),HIDE
        TEXT,AT(2,16,,11),FULL,USE(Poz:ClmFORMAT),SKIP,FONT('Consolas'),COLOR(COLOR:BTNFACE), |
                TIP('After Format'),SINGLE
        STRING('--RightMax--'),AT(238,81,35,10),USE(?RightMax),HIDE
        LIST,AT(1,127),FULL,USE(?LIST:ListQ),VSCROLL,FROM(ListQ),FORMAT('16L(1)|FM~Col~C(0)@s5@50L(1' & |
                ')|FMP~Header~C(0)@s32@?16L(1)|FM~Wid~@s8@16L(2)|FM~Aln~L(1)@s5@16L(1)|FM~Hdr~@s5@27' & |
                'L(1)|FM~Picture~@s16@20L(2)FPT(B)~Format~L(1)@s255@')
    END
SysMenuCls SysMenuClass
EVENT:SnapToUnder  EQUATE(EVENT:User+100)
  CODE
  ListFEQ=FEQ
  ColNo=1 ; ColNoMax = PWnd$FEQ{PROPLIST:Exists,0} ; IF ColNoMax=0 THEN MESSAGE('This LIST has no columns.', MsgCaption) ; RETURN.
  IF ~PWnd$FEQ{PROP:Format} THEN MESSAGE('This LIST has no Format().', MsgCaption) ; RETURN.
  IF GloT:ReFormatList THEN Message('You have Column Resize Open for LIST FEQ: ' & GloT:ReFormatList, MsgCaption) ; RETURN.
  GloT:ReFormatList=FEQ
  DO S1QLoadRtn ; DO L1QLoadRtn ; DO AlignQLoadRtn
  DO Load_LQ_ColumnsRtn
  DO GetPositionOnceRtn ; BeforeWaz=Poz ; Haz=Poz
  OPEN(Window) ; SysMenuCls.Init(Window)  !OPEN Window--OPEN Window--OPEN Window--OPEN Window--OPEN Window--OPEN Window--OPEN Window
  MakeOverWindow() ; ListHelpCW(?HelpBtn)
  DO GetPositionOnceForColumPROPLISTRtn    !Window must be open since sets fields
  BeforeWaz=Poz ; Haz=Poz
  
!Region After OPENed Window -- Prepare for ACCEPT - After OPEN(Window)
  IF ~AtReFmtList[3] THEN 
     0{PROP:Width}=?Cfg:ResizeSnapTo{PROP:XPos} +6 + ?Cfg:ResizeSnapTo{PROP:Width}
  END
  SELF.AtSetOrSave(1, AtReFmtList[]) 
  0{PROP:MinWidth}=?VertRightPanel{PROP:XPos}+10 
  0{PROP:MaxWidth}=?RightMax{PROP:XPos} + 123
  0{PROP:MinHeight}=?VertRightPanel{PROP:YPos} + ?VertRightPanel{PROP:Height} + 2 + ?TOOLBAR1{PROP:Height}
  0{PROP:Text} = 'Re-Format '& FEQ &' '& FeqName &' - '& CLIP(FeqTypeName)
  MakeOverList(?List:ListQ) ; DropListColor(?Cfg:ResizeSnapTo) ; DropListColor(?List:AlignDQ)
  DO S1QWindowOpenRtn ; DO L1QColumnOpenRtn
  ?List:AlignDQ{PROP:Selected}=Poz:ClmALCRDnD ; ?List:AlignHQ{PROP:Selected}=Poz:ClmALCRDnH
  ?ColNo{PROP:RangeHigh}=ColNoMax ; ?ColNo{PROP:Tip}=?ColNo{PROP:Tip} &'<13,10>Max Columns: ' & ColNoMax
!EndRegion After OPEN Window, Prepare for ACCEPT    
  ACCEPT
    CASE EVENT()
    OF EVENT:OpenWindow   ; SysMenuCls_SYSCOMMAND(0{PROP:Handle},SMCmd_HideUnder) ; POST(EVENT:SnapToUnder)
    OF EVENT:NewSelection ; DO AcceptOrSpinSizesRtn
    OF EVENT:CloseWindow  ; POST(EVENT:Accepted,?CancelBtn) ; CYCLE
    !OF EVENT:Sized        ; GETPOSITION(0,,,W#,H#) ; SC#+=1 ; 0{PROP:Text}='Sized ' & W# &','& H# & '  Cnt=' & SC# &' Min=' & 0{PROP:MinWidth} 
    OF EVENT:Moved        ; POST(EVENT:SnapToUnder)  ! GETPOSITION(0,X#,Y#) ; 0{PROP:Text}='Moved ' & X# &','& Y# 
    OF EVENT:AlertKey     ; DO TakeEVENT:AlertKeyRtn
    OF EVENT:SnapToUnder  ; SELF.SnapToPreview()
    OF Event:Rejected ; BEEP ; SELECT(?) ; CYCLE
    END
    CASE ACCEPTED()
    OF ?HaltBtn ; HALT 
    OF ?Cfg:ResizeSnapTo ; SELF.SnapToPreview() ; SELF.ConfigPut(Cfg:ResizeSnapTo)
    OF ?List:AlignDQ ; Poz:ClmALCRDnD=CHOICE(?List:AlignDQ) ; GET(AlignQ,Poz:ClmALCRDnD)
    OF ?List:AlignHQ ; Poz:ClmALCRDnH=CHOICE(?List:AlignHQ) ; GET(AlignHQ,Poz:ClmALCRDnH)
    OF ?SaveBtn   ; BREAK 
    OF ?CancelBtn ; Poz=BeforeWaz ; DO SizeChangeRtn ; BREAK
    OF ?UndoBtn   ; Poz=BeforeWaz ; DO SizeChangeRtn        
    OF ?UnderBtn ; SysMenuCls_SYSCOMMAND(0{PROP:Handle},SMCmd_MoveUnder)
    OF ?LISTBtn  ; SELF.ListPROPs(FEQ,FeqTypeNo,FeqTypeName,FeqName)
    OF ?PropsBtn ; SELF.ControlPROPs(FEQ, FeqTypeNo, FeqTypeName,FeqName)
    OF ?HeadUndoBtn ; Poz:ClmHeader=LQ:HeaderTip ; DISPLAY
    OF ?HeadCopyBtn ; SETCLIPBOARD(QUOTE(CLIP(Poz:ClmHeader)))
    OF ?Poz:ClmPicture ; IF ~Poz:ClmPicture THEN Poz:ClmPicture=B4Waz:ClmPicture. ; Poz:ClmPicture=LEFT(Poz:ClmPicture)
    OF ?SeeTipBtn ; Message(PWnd$FEQ{PROPLIST:DefaultTip,ColNo},'PROPLIST:DefaultTip Column: ' & ColNo)
    OF ?FontBtn     ; DO FontRtn
    OF ?ColorBtn    ; DO ColorRtn
    OF ?GdLinesBtn  ; Self.GuideLines(FEQ, FeqTypeNo, FeqTypeName, FeqName)
    OF ?Try2:Disable_2 ; PWnd$FEQ{PROP:Disable}=Try2:Disable[2] ; CYCLE
    OF ?Try2:Hide_2 ; PWnd$FEQ{PROP:Hide}=Try2:Hide[2] ; CYCLE 
    OF ?HelpBtn ; ListHelpCW()
    OF ?ResetFormatBtn ; PWnd$ListFEQ{PROP:Left}=1
    OF ?TestBtn ; DO TestBtnRtn
    END
    IF ACCEPTED() THEN DO AcceptOrSpinSizesRtn.        
  END !Accept 
  SELF.AtSetOrSave(2, AtReFmtList[])  ; CLOSE(Window)
  GloT:ReFormatList=0
  PWnd$FEQ{PROPLIST:HasSortColumn}=SaveHasSortColumn ; PWnd$FEQ{PROPLIST:SortColumn}=SaveSortColumn
  PWnd$FEQ{PROPList:SortBackColor}=SaveSortBackColor ; PWnd$FEQ{PROPList:SortTextColor}=SaveSortTextColor   
  PWnd$FEQ{PropList:DefHdrBackColor}=SaveDefHdrBackColor ; PWnd$FEQ{PropList:DefHdrTextColor}=SaveDefHdrTextColor   
  RETURN
!-----------------
TakeEVENT:AlertKeyRtn ROUTINE
    DATA
PM1     SHORT
PmFeq   LONG 
PmPoz   &LONG
MinPz   LONG(-99999) 
MaxPz   LONG(99999) 
    CODE
    X=KEYCODE() ; CASE X !Allow Shift+Arrows to Resize like in Window Formatter. Alt+Arrows move XY
    OF PgUpKey  OROF PgDnKey  ; PM1=CHOOSE(X=PgUpKey,-1,1)  ; PmFeq=?ColNo  ; PmPoz&=ColNo ; MinPz=1 ; MaxPz=ColNoMax
    OF CtrlPgUp OROF CtrlPgDn ; PM1=CHOOSE(X=CtrlPgUp,-1,1) ; PmFeq=?ColNo  ; PmPoz&=ColNo ; MinPz=1 ; MaxPz=ColNoMax 
                              PmPoz=CHOOSE(X=CtrlPgUp,2,MaxPz-1)
    OF ShiftLeft OROF ShiftRight ; PM1=CHOOSE(X=ShiftLeft,-1,1) ; PmFeq=?Poz:ClmWd  ; PmPoz&=Poz:ClmWd ; MinPz=0
    ELSE ; EXIT
    END
    IF PmFeq AND PM1 THEN
       IF PmFeq{PROP:Disable} OR PmFeq{PROP:Hide} THEN EXIT.
       UPDATE
       IF PM1=-1 AND PmPoz <= MinPz THEN EXIT. !Below mini, e.g. Negative sizes (Shift Down)
       IF PM1=1  AND PmPoz >= MaxPz THEN EXIT.
       PmPoz += PM1 ; POST(EVENT:Accepted,PmFEQ) ; EXIT
    END       
!----------------- 
AcceptOrSpinSizesRtn ROUTINE ! OF ?Poz:X  TO ?Poz:Ht ;  DO AcceptedSizesRtn
  CASE FIELD()
  OF ?ColNo ; DO GetPositionOnceForColumPROPLISTRtn ;  DO L1QColumnOpenRtn ; DISPLAY   
  OF ?Poz:ClmWd   !SPIN(@n6) Poz:ClmWd      !----Width---- 
  OF ?Poz:ClmOffsetD
  OF ?Poz:ClmOffsetH
  OF 0 ; EXIT
  OF ?LIST:ListQ ; X=CHOICE(?LIST:ListQ) ; C=X ; GET(ListQ,C) ; IF LQ:IsGroup THEN C=X+1 ; GET(ListQ,C).
                ?LIST:ListQ{PROP:Selected}=C ; ColNo=LQ:ColX ; POST(EVENT:Accepted,?ColNo) ; EXIT
  END
  DO SizeChangeRtn    
SizeChangeRtn ROUTINE
  DISPLAY ; DO SetPosition_Poz2PreviewRtn
!--------------------
S1QLoadRtn ROUTINE !cannot do WINDOW 
  CLEAR(S1Q);S1Q:Poz&=Poz:LstWide ;S1Q:Haz&=Haz:LstWide ;S1Q:PROP=Prop:Width   ;S1Q:FeqInp=?Poz:LstWide ;ADD(S1Q)
  CLEAR(S1Q);S1Q:Poz&=Poz:HScroll ;S1Q:Haz&=Haz:HScroll ;S1Q:PROP=Prop:HScroll ;S1Q:FeqInp=?Poz:HScroll ;ADD(S1Q)
  CLEAR(S1Q);S1Q:Poz&=Poz:VScroll ;S1Q:Haz&=Haz:VScroll ;S1Q:PROP=Prop:VScroll ;S1Q:FeqInp=?Poz:VScroll ;ADD(S1Q)
  CLEAR(S1Q);S1Q:Poz&=Poz:VCR ;S1Q:Haz&=Haz:VCR ;S1Q:PROP=Prop:VCR ;S1Q:FeqInp=?Poz:VCR ;ADD(S1Q)
  CLEAR(S1Q);S1Q:Poz&=Poz:Flat;S1Q:Haz&=Haz:Flat;S1Q:PROP=PROP:Flat;S1Q:FeqInp=?Poz:Flat;ADD(S1Q)
  CLEAR(S1Q);S1Q:Poz&=Poz:TRN ;S1Q:Haz&=Haz:TRN ;S1Q:PROP=PROP:TRN ;S1Q:FeqInp=?Poz:TRN ;ADD(S1Q)
!-----------------------  
S1QWindowOpenRtn ROUTINE
  LOOP X=1 TO RECORDS(S1Q) ; GET(S1Q,X)
      IF S1Q:FeqInp THEN ENABLE(S1Q:FeqInp) ; UNHIDE(S1Q:FeqInp). ! IF S1Q:FeqPmt THEN ENABLE(S1Q:FeqPmt) ; UNHIDE(S1Q:FeqPmt).
  END     
  ALERT(PgUpKey) ; ALERT(PgDnKey) ; ALERT(CtrlPgUp) ; ALERT(CtrlPgDn)
!=========================================
!### List Code -- ### List Code -- ### List Code -- ### List Code 
L1QLoadRtn ROUTINE 
  CLEAR(L1Q);L1Q:Poz&=Poz:ClmWd          ;L1Q:Haz&=Haz:ClmWd          ;L1Q:PROP=PROPLIST:width ;L1Q:FeqInp=?Poz:ClmWd ;ADD(L1Q)
  CLEAR(L1Q);L1Q:Poz&=Poz:ClmResize      ;L1Q:Haz&=Haz:ClmResize      ;L1Q:PROP=PROPLIST:Resize      ;L1Q:FeqInp=?Poz:ClmResize      ;ADD(L1Q)
  CLEAR(L1Q);L1Q:Poz&=Poz:ClmRightBorder ;L1Q:Haz&=Haz:ClmRightBorder ;L1Q:PROP=PROPLIST:RightBorder ;L1Q:FeqInp=?Poz:ClmRightBorder ;ADD(L1Q)
  CLEAR(L1Q);L1Q:Poz&=Poz:ClmUnderline   ;L1Q:Haz&=Haz:ClmUnderline   ;L1Q:PROP=PROPLIST:Underline   ;L1Q:FeqInp=?Poz:ClmUnderline   ;ADD(L1Q)
  CLEAR(L1Q);L1Q:Poz&=Poz:ClmFixed       ;L1Q:Haz&=Haz:ClmFixed       ;L1Q:PROP=PROPLIST:Fixed       ;L1Q:FeqInp=?Poz:ClmFixed       ;ADD(L1Q)
  CLEAR(L1Q);L1Q:Poz&=Poz:ClmLastOnLine  ;L1Q:Haz&=Haz:ClmLastOnLine  ;L1Q:PROP=PROPLIST:LastOnLine  ;L1Q:FeqInp=?Poz:ClmLastOnLine  ;ADD(L1Q)
  CLEAR(L1Q);L1Q:Poz&=Poz:ClmScroll      ;L1Q:Haz&=Haz:ClmScroll      ;L1Q:PROP=PROPLIST:Scroll      ;L1Q:FeqInp=?Poz:ClmScroll      ;ADD(L1Q)
  CLEAR(L1Q);L1Q:Poz&=Poz:ClmColStyle    ;L1Q:Haz&=Haz:ClmColStyle    ;L1Q:PROP=PROPLIST:ColStyle    ;L1Q:FeqInp=?Poz:ClmScroll      ;ADD(L1Q)
  CLEAR(L1Q);L1Q:Poz&=Poz:ClmTree    ;L1Q:Haz&=Haz:ClmTree    ;L1Q:PROP=PROPLIST:Tree       ;L1Q:FeqInp=?Poz:ClmTree    ;ADD(L1Q)
  CLEAR(L1Q);L1Q:Poz&=Poz:ClmTr1Base ;L1Q:Haz&=Haz:ClmTr1Base ;L1Q:PROP=PROPLIST:TreeOffset ;L1Q:FeqInp=?Poz:ClmTr1Base ;ADD(L1Q)
  CLEAR(L1Q);L1Q:Poz&=Poz:ClmTrBoxes ;L1Q:Haz&=Haz:ClmTrBoxes ;L1Q:PROP=PROPLIST:TreeBoxes  ;L1Q:FeqInp=?Poz:ClmTrBoxes ;ADD(L1Q)
  CLEAR(L1Q);L1Q:Poz&=Poz:ClmTrLevel ;L1Q:Haz&=Haz:ClmTrLevel ;L1Q:PROP=PROPLIST:TreeIndent ;L1Q:FeqInp=?Poz:ClmTrLevel ;ADD(L1Q)
  CLEAR(L1Q);L1Q:Poz&=Poz:ClmTrLines ;L1Q:Haz&=Haz:ClmTrLines ;L1Q:PROP=PROPLIST:TreeLines  ;L1Q:FeqInp=?Poz:ClmTrLines ;ADD(L1Q)
  CLEAR(L1Q);L1Q:Poz&=Poz:ClmTrRoot  ;L1Q:Haz&=Haz:ClmTrRoot  ;L1Q:PROP=PROPLIST:TreeRoot   ;L1Q:FeqInp=?Poz:ClmTrRoot  ;ADD(L1Q)
!-------------  
L1QColumnOpenRtn ROUTINE !prepare for new column
!No Need the LQ Columns never HIDE ... but Group is limited far fewer, and Width can be (none) or set
!  LOOP X=1 TO RECORDS(L1Q) ; GET(L1Q,X) 
!    IF L1Q:FeqInp THEN ENABLE(L1Q:FeqInp) ; UNHIDE(L1Q:FeqInp).
!  ! IF L1Q:FeqPmt THEN ENABLE(L1Q:FeqPmt) ; UNHIDE(L1Q:FeqPmt).
! END
  ?Group:Tree{PROP:Disable}=CHOOSE(~Poz:ClmTree)
  LQ:ColNo=ColNo ; GET(ListQ,LQ:ColNo) ; ?List:ListQ{PROP:Selected}=POINTER(ListQ)  !TODO more
  B4Waz:ClmFORMAT=LQ:B4WazFormat ; ?B4Waz:ClmFORMAT{PROP:Tip}=B4Waz:ClmFORMAT  !Grab format before       
!-------------------------    
GetPositionOnceForColumPROPLISTRtn ROUTINE !Setup the Initial Poz, Haz and Waz
  SETTARGET(PWnd)
  LOOP X=1 TO RECORDS(L1Q)
       GET(L1Q,X)
       L1Q:Poz=FEQ{L1Q:PROP,ColNo}  !E.g. !L1Q    Poz:ClmWd=FEQ{PROPLIST:width,ColNo} ; Haz:ClmWd=Poz:ClmWd
       L1Q:Haz=L1Q:Poz
  END   
  Poz:ClmFORMAT=QUOTE(FEQ{PROPLIST:Format,ColNo}) ; Window$?Poz:ClmFORMAT{PROP:Tip}=Poz:ClmFORMAT
  FEQ{PROPLIST:SortColumn}=ColNo  !Highlight column
  Poz:ClmHeader=FEQ{PROPLIST:Header,ColNo} ; Haz:ClmHeader=Poz:ClmHeader
  Poz:ClmPicture=FEQ{PROPLIST:Picture,ColNo} ; Haz:ClmPicture=Poz:ClmPicture ; B4Waz:ClmPicture=Poz:ClmPicture
  LOOP X=1 TO RECORDS(AlignQ)  !Check Align e.g. PROP:Left or if NONE the Default 
      GET(AlignQ,X) !Must Leave Q Loaded
      IF Feq{AlnQ:PropList,ColNo} THEN  !If PROP:Left/c/r/dec True
         Poz:ClmALCRDnD=X ; Poz:ClmOffsetD=Feq{AlnQ:PrOffset,ColNo} ; BREAK
      END 
  END  ; IF ~Poz:ClmALCRDnD THEN Poz:ClmALCRDnD=1.
  LOOP X=1 TO RECORDS(AlignHQ)  !Check Align e.g. PROP:Left or if NONE the Default 
      GET(AlignHQ,X) !Must Leave Q Loaded
      IF Feq{AlnHQ:PropList,ColNo} THEN  !If PROP:Left/c/r/dec True
         Poz:ClmALCRDnH=X ; Poz:ClmOffsetH=Feq{AlnHQ:PrOffset,ColNo} ; BREAK
      END 
  END  ; IF ~Poz:ClmALCRDnH THEN Poz:ClmALCRDnH=1.    
  SETTARGET()
  ?List:AlignDQ{PROP:Selected}=Poz:ClmALCRDnD    
    ?List:AlignHQ{PROP:Selected}=Poz:ClmALCRDnH    
!---------------------
SetPosition_Poz2PreviewRtn_ForColumPROPLISTRtn ROUTINE
!    DO L1QColumnSetPositionRtn   
!L1QColumnSetPositionRtn ROUTINE !write the PROPs 
    LOOP X=1 TO RECORDS(L1Q) ; GET(L1Q,X)
         IF L1Q:Poz<>L1Q:Haz THEN FEQ{L1Q:PROP,ColNo}=L1Q:Poz.
    END      
    IF Poz:ClmHeader <> Haz:ClmHeader THEN Feq{PROPLIST:Header,ColNo}=Poz:ClmHeader.
    IF Poz:ClmPicture <> Haz:ClmPicture THEN Feq{PROPLIST:Picture,ColNo}=Poz:ClmPicture.
    IF Poz:ClmALCRDnD<>Haz:ClmALCRDnD AND Poz:ClmALCRDnD THEN    !Did Align change?
       GET(AlignQ,Poz:ClmALCRDnD)
       Feq{AlnQ:PropList,ColNo}=AlnQ:Value        
    END       
    IF Poz:ClmOffsetD<>Haz:ClmOffsetD AND AlnQ:PrOffset AND AlnQ:Value THEN
       Feq{AlnQ:PrOffset,ColNo}=Poz:ClmOffsetD
    END
    !--- Header Align and offset
    IF Poz:ClmALCRDnH<>Haz:ClmALCRDnH AND Poz:ClmALCRDnH THEN    !Did Align change?
       GET(AlignHQ,Poz:ClmALCRDnH)
       Feq{AlnHQ:PropList,ColNo}=AlnHQ:Value
    END       
    IF Poz:ClmOffsetH<>Haz:ClmOffsetH AND AlnHQ:PrOffset AND AlnHQ:Value THEN
       Feq{AlnHQ:PrOffset,ColNo}=Poz:ClmOffsetH
    END
    Poz:ClmFORMAT=QUOTE(FEQ{PROPLIST:Format,ColNo}) ; Window$?Poz:ClmFORMAT{PROP:Tip}=Poz:ClmFORMAT   
    EXIT  !^^^ Format MUST be last ^^^
!-----------------
GetPositionOnceRtn ROUTINE !Setup the Initial Poz, Haz and Waz
  SETTARGET(PWnd)
  SaveSortBackColor=FEQ{PROPList:SortBackColor} ; SaveSortTextColor=FEQ{PROPList:SortTextColor}
  SaveHasSortColumn=FEQ{PROPLIST:HasSortColumn} ; SaveSortColumn=FEQ{PROPLIST:SortColumn}  !Highlight column we are working
  SaveDefHdrBackColor=FEQ{PropList:DefHdrBackColor} ; SaveDefHdrTextColor=FEQ{PropList:DefHdrTextColor}
  FEQ{PROPLIST:HasSortColumn}=1
  FEQ{PROPLIST:HdrSortBackColor} = COLOR:HIGHLIGHT     ; FEQ{PropList:DefHdrBackColor}=COLOR:BtnFace !COLOR:HIGHLIGHTtext
  FEQ{PROPList:HdrSortTextColor} = COLOR:HIGHLIGHTtext ; FEQ{PropList:DefHdrTextColor}=COLOR:BTNTEXT !COLOR:HIGHLIGHT
  FEQ{PROPList:SortBackColor}    = 80000018h !InfoBackground
  FEQ{PROPList:SortTextColor}    = 80000017h !InfoText
  Poz:WndWide=0{Prop:Width}
  LOOP X=1 TO RECORDS(S1Q) ; GET(S1Q,X) ; S1Q:Poz=FEQ{S1Q:PROP} ; END  !Get simple PROPs
  Try2:Disable[1]=FEQ{PROP:Disable} ; Try2:Disable[2]=Try2:Disable[1]
  Try2:Hide[1]=FEQ{PROP:Hide} ; Try2:Hide[2]=Try2:Hide[1]
  SETTARGET()   
!---------------------
SetPosition_Poz2PreviewRtn ROUTINE
  IF Haz=Poz THEN EXIT.
  SETTARGET(PWnd)
  DO SetPosition_Poz2PreviewRtn_ForColumPROPLISTRtn
  IF Poz:WndWide<>Haz:WndWide THEN 0{Prop:Width} =Poz:WndWide.
  IF HideUnHide THEN HIDE(FEQ).   
  LOOP X=1 TO RECORDS(S1Q) ; GET(S1Q,X) ; IF S1Q:Poz<>S1Q:Haz THEN FEQ{S1Q:PROP}=S1Q:Poz. ; END    
  DISPLAY ; IF HideUnHide AND ~Try2:Hide[1] THEN UNHIDE(FEQ) ; DISPLAY.
  SETTARGET() ; Haz=Poz ; DISPLAY
!--------------    
FontRtn ROUTINE
    DATA
FFace   STRING(64)
FSize   LONG
FColor  LONG
FStyle  LONG
FCHar   LONG
  CODE
  SETTARGET(PWnd)
  GETFONT(FEQ,FFace,FSize,FColor,FStyle,FChar)
  SETTARGET()
  IF ?FontBtn{PROP:Tip}='' THEN ?FontBtn{PROP:Tip}='Original Font:<13,10>' & FFace &|
              '<13,10>Size: '& FSize &'  Color: '& FColor&'  Style: '& FStyle&'  CharSet: '& FChar .
  IF FONTDIALOGa('Select Control Font',FFace,FSize,FColor,FStyle,FChar) THEN 
      SETTARGET(PWnd)
      SETFONT(FEQ,FFace,FSize,FColor,FStyle,FChar)
      DISPLAY
      SETTARGET()
  END    
!---------------    
ColorRtn ROUTINE
    DATA
MaxClr     EQUATE(11)             
ClrNow     LONG       ,DIM(MaxClr)
Prop:Clr   LONG       ,DIM(MaxClr)
Prop:ClrNm PSTRING(32),DIM(MaxClr)
PropName   STRING(32)
Clr        LONG
Pop CSTRING(1000)
Clp STRING(1000)
  CODE
  Prop:Clr[1]=PROP:FontColor         ; Prop:Clr[5]=PROPLIST:Grid            ; Prop:Clr[ 8]=PROPLIST:HdrSortBackColor
  Prop:Clr[2]=Prop:Color             ; Prop:Clr[6]=PROPLIST:DefHdrTextColor ; Prop:Clr[ 9]=PROPList:HdrSortTextColor
  Prop:Clr[3]=Prop:SelectedColor     ; Prop:Clr[7]=PROPLIST:DefHdrBackColor ; Prop:Clr[10]=PROPList:SortBackColor  
  Prop:Clr[4]=Prop:SelectedFillColor ;                                      ; Prop:Clr[11]=PROPList:SortTextColor  
!TODO Put colors in LIST so Sample could show see E:\GitHub\...\DimWindow it has a Color Picker        
  LOOP C=1 TO MaxClr ; IF ~Prop:Clr[C] THEN CYCLE.
     ClrNow[C]=PWnd$FEQ{Prop:Clr[C]}
     PropName=CHOOSE(C,'Prop:FontColor','Prop:Color (Background)','Prop:SelectedColor','Prop:SelectedFillColor', |
         '-|PropList:Grid','-|PropList:DefHdrTextColor','PropList:DefHdrBackColor' , |
         '-|PropList:HdrSortBackColor','PropList:HdrSortTextColor','PropList:SortBackColor','PropList:SortTextColor','?C='&C) 
     Pop=CHOOSE(C=1,'',Pop&'|') & CLIP(PropName) & |
         CHOOSE(ClrNow[C]=0 or ClrNow[C]=-1,'','<9>'& ClaColorEquate(ClrNow[C])) 
     IF SUB(PropName,1,2)='-|' THEN PropName=SUB(PropName,3,99).
     X=INSTRING('(',PropName) ; IF X THEN PropName=SUB(PropName,1,X-1).
     Prop:ClrNm[C]=CLIP(PropName)
  END
  C=POPUP('Colors (Ctrl+Click to Copy)|-|' & Pop)
  IF KeyStateSCA(2) THEN 
     Clp=Pop ; ReplaceInto(Clp,'<9>',' ') ; ReplaceInto(Clp,'|','<13,10>') ; SETCLIPBOARD(Clp) ; EXIT
  END
  IF C<2 THEN EXIT. ; C -= 1
  Clr=ClrNow[C] 
  IF   Clr=-1 THEN Clr=0 
  ELSIF Clr<0 THEN Clr=GetSysColor(BAND(Clr,7Fh)) 
  END
  IF COLORDIALOG('Select Color ' & C,Clr) THEN 
     PWnd$FEQ{Prop:Clr[C]}=Clr 
     Pop='Set Color ' & Prop:ClrNm[C] & ' = '& Clr &' was '& ClrNow[C]
     PWnd$FEQ{Prop:Tip}=Pop &'<13,10>'& PWnd$FEQ{Prop:Tip}
     ?ColorBtn{PROP:Tip}=Pop &'<13,10>'& ?ColorBtn{PROP:Tip}
  END
!TODO make a LIST color tool to set individual column colors {PROPLLIST:TextColor,Col} :Backcolor :TextSelected :BackSelected
!---------------     
AlignQLoadRtn ROUTINE !Note PROPLIST:LeftOffSet=1+PROPLIST:Left, PROPLIST:HeaderLeftOffSet=1+PROPLIST:HeaderLeft
  CLEAR(AlignQ) ; AlnQ:Value=1                                                        ;  X=PROPLIST:HeaderLeft-PROPLIST:Left
  C=PROPLIST:Left   ;AlnQ:Desc='Left'   ;AlnQ:PropList=c ;AlnQ:PrOffset=1+c;ADD(AlignQ); AlignHQ=AlignQ ; AlnHQ:PropList=c+x; AlnHQ:PrOffset=c+x+1; ADD(AlignHQ)
  C=PROPLIST:Center ;AlnQ:Desc='Center' ;AlnQ:PropList=c ;AlnQ:PrOffset=1+c;ADD(AlignQ); AlignHQ=AlignQ ; AlnHQ:PropList=c+x; AlnHQ:PrOffset=c+x+1; ADD(AlignHQ)
  C=PROPLIST:Right  ;AlnQ:Desc='Right'  ;AlnQ:PropList=c ;AlnQ:PrOffset=1+c;ADD(AlignQ); AlignHQ=AlignQ ; AlnHQ:PropList=c+x; AlnHQ:PrOffset=c+x+1; ADD(AlignHQ)
  C=PROPLIST:Decimal;AlnQ:Desc='Decimal';AlnQ:PropList=c ;AlnQ:PrOffset=1+c;ADD(AlignQ); AlignHQ=AlignQ ; AlnHQ:PropList=c+x; AlnHQ:PrOffset=c+x+1; ADD(AlignHQ)
!---------------    
TestBtnRtn ROUTINE
  CASE POPUP('Copy Format Quoted|Copy Format not quoted|Copy ListQ Columns Queue')
  OF 1 ; SETCLIPBOARD(QUOTE(PWnd$FEQ{PROP:Format}))
  OF 2 ; SETCLIPBOARD(PWnd$FEQ{PROP:Format}) 
  OF 3 ; SetClip2Queue(ListQ)!,,,'LQ:')
  END
!---------------
Load_LQ_ColumnsRtn ROUTINE
  DATA
GrpNo  SHORT
GrpCnt SHORT
LastGrpNo SHORT
ColsInGrp SHORT
Fmt STRING(1024)
  CODE
  SETTARGET(PWnd)
  LOOP ColX=1 TO 1024 ; IF ~ListFEQ{PROPList:Exists, ColX} THEN BREAK.   
     GrpNo=ListFEQ{PROPLIST:GroupNo, ColX}
     IF GrpNo<>LastGrpNo THEN
        LastGrpNo=GrpNo
        ColsInGrp=ListFEQ{PROPLIST:GroupNo+PROPLIST:Group, ColX}
        IF ColsInGrp THEN
           GrpCnt += 1
           CLEAR(ListQ)
           LQ:IsGroup=PROPLIST:Group
           LQ:Level=1
           LQ:ColX=ColX * -1
           LQ:ColNo=' G ' & GrpCnt
           LQ:Width=ListFEQ{PROPLIST:width+PROPLIST:Group, ColX}           
           Fmt=ListFEQ{PROPList:Header+PROPLIST:Group, ColX} ; LQ:HeaderTip=Fmt
           ReplaceInto(Fmt,'<13,10>',' ') ; LQ:Header=LEFT(Fmt)
           LQ:HeadAlign=ClaListColAlign(ListFEQ,ColX,1,1) 
           Fmt = ListFEQ{PROPLIST:Format+PROPLIST:Group, ColX}
           X=INSTRING(']',Fmt)   !Group Fmt incl all [member columns]Grp
           Fmt=SUB(Fmt,X+1,9999)
           LQ:Format='[]'&QUOTE(CLIP(Fmt)) ; LQ:B4WazFormat=LQ:Format ; LQ:Format255=LQ:Format
           LQ:Align=ClaListColAlign(ListFEQ,ColX,1)
          !Clutter LQ:Picture='  ' & ColsInGrp & ' Cols'
           ADD(ListQ)
        END
     END
   !---- column
     CLEAR(ListQ) 
     LQ:Level=CHOOSE(ColsInGrp=0,1,2)
     LQ:ColX =ColX
     LQ:ColNo=ColX
     LQ:Picture=ListFEQ{PROPLIST:Picture, ColX}
     LQ:Width=ListFEQ{PROPLIST:width, ColX}
     Fmt=ListFEQ{PROPList:Header, ColX} ;  ; LQ:HeaderTip=Fmt
     ReplaceInto(Fmt,'<13,10>',' ') ; LQ:Header=LEFT(Fmt)
     LQ:HeadAlign=ClaListColAlign(ListFEQ,ColX,,1) 
     Fmt=ListFEQ{PROPLIST:Format, ColX}
     LQ:Format=QUOTE(CLIP(Fmt))  ; LQ:B4WazFormat=LQ:Format ; LQ:Format255=LQ:Format
     LQ:Align=ClaListColAlign(ListFEQ,ColX)
     ADD(ListQ)
  END ; SETTARGET()
!==========================================
CBWndPreviewClass.GuideLines PROCEDURE(LONG FEQ, LONG FeqTypeNo, STRING FeqTypeName, STRING FeqName)
!TODO add this to Window Resize
F LONG,AUTO
X LONG,AUTO
C LONG,AUTO
PosDataType GROUP,TYPE 
GuideXY      LONG,DIM(2,2)  !Poz:GuideXY    !Haz:HoLineY   [1=H 2=V , 1=X 2=Y]
GuideWd      LONG,DIM(2)    !Poz:GuideWd    !Haz:GuideWd
GuideColor   LONG           !Poz:GuideColor !Haz:GuideColor
           END      
Poz GROUP(PosDataType),PRE(Poz).    !This Screen Position
Haz GROUP(PosDataType),PRE(Haz).    !Preview Position Now
GridArea    &USHORT
GridAreaS4  STRING(4) 
GridAppend  BYTE,STATIC
GridHVB  STRING('B'),STATIC
GLWindow WINDOW('Guide and Grid Lines'),AT(,,156,204),GRAY,SYSTEM,FONT('Segoe UI',9),DOUBLE
        GROUP(' Guide Lines '),AT(6,1,145,89),USE(?GRP1),BOXED
            STRING('X / Y'),AT(58,10),USE(?GdHead1)
            STRING('Width'),AT(101,10),USE(?GdHead2)
            PROMPT('&Horizontal:'),AT(13,22),USE(?GdH:Pmt)
            SPIN(@n_5),AT(49,21,40),USE(Poz:GuideXY[1,2]),HVSCROLL,RANGE(0,999),STEP(-1)
            SPIN(@n2),AT(102,21,20),USE(Poz:GuideWd[1]),RANGE(0,99)
            PROMPT('&Vertical:'),AT(13,38),USE(?GdV:Pmt)
            SPIN(@n_5),AT(49,38,40),USE(Poz:GuideXY[2,1]),HSCROLL,RANGE(0,999),STEP(1)
            SPIN(@n2),AT(102,38,20),USE(Poz:GuideWd[2]),RANGE(0,99)
            CHECK('No G&uide Lines'),AT(13,53,,11),USE(?HideGuides),SKIP
            BUTTON,AT(127,30,10,10),USE(?GuideColorBtn),SKIP
            BUTTON('&Snap to PROMPT'),AT(13,68,66,14),USE(?SnapToFEQ)
            BUTTON('Snap to &Window'),AT(82,68,58,14),USE(?SnapToWnd)
        END
        GROUP(' Grid Lines '),AT(6,95,146,89),USE(?GRP2),BOXED
            PROMPT('&Increment:'),AT(13,107),USE(?GrdInc:Pmt)
            SPIN(@n2),AT(53,106,30),USE(Cfg:GridInc),HVSCROLL,RANGE(5,99)
            PROMPT('&Thick Every:'),AT(13,123),USE(?Grd5th:Pmt)
            SPIN(@n2),AT(53,122,30),USE(Cfg:Grid5th),HVSCROLL,RANGE(0,50)
            PROMPT('&Area:'),AT(13,139),USE(?GridArea:Pmt)
            LIST,AT(33,138,50,11),USE(GridAreaS4),VSCROLL,TIP('Grid Lines area painted relative to Guide Lines'),DROP(9), |
                    FROM('All|#1133|North|#1132|East|#2133|South|#1233|West|#1123|Northeast|#2132|Southeast|#2233|Southw' & |
                    'est|#1223|Northwest|#1122')
            BUTTON,AT(89,138,10,10),USE(?GridColorBtn),SKIP
            OPTION,AT(89,105,49,30),USE(GridHVB)
                RADIO('&Both'),AT(89,105,26),USE(?GridHVB:B),VALUE('B')
                RADIO('H&orizontal'),AT(89,115,43),USE(?GridHVB:H),VALUE('H')
                RADIO('V&ertical'),AT(89,126,34),USE(?GridHVB:V),VALUE('V')
            END
            BUTTON('Add &Grid'),AT(13,155,37,11),USE(?GridBtn)
            BUTTON('&Remove'),AT(57,155,37,11),USE(?GridOffBtn)
            BUTTON('|   &List   |<13,10>Columns'),AT(104,155,39,22),USE(?ListGridBtn),DISABLE,TIP('Grid Lines over List Columns')
            CHECK('A&ppend'),AT(13,170),USE(GridAppend),SKIP,TIP('Add Grid to Existing e.g. NE and SW')
        END
        BUTTON('&Close'),AT(117,187,35,13),USE(?CloseBtn),SKIP,STD(STD:Close)
        CHECK('&Lines on Top of Controls'),AT(6,189),USE(?TRNControls),SKIP
    END
SysMenuCls SysMenuClass
Px LONG !PWnd Pos
Py LONG
Pw LONG
Ph LONG   
PRsz SHORT
Fx LONG !Feq Pos
Fy LONG
Fw LONG
Fh LONG
GX         LONG
GdLineFEQ  LONG,DIM(2)  ![1]=H x,w  - [2]=V y,h
GdFEQ      &LONG
UPropTRN STRING(1)
TRNControls &BYTE
HideGuides  &BYTE
GridQ &GridQType
GdX &LONG
GdY &LONG
    CODE
    GdX&=Poz:GuideXY[2,1] ; GdY&=Poz:GuideXY[1,2]  
    GdLineFEQ[1]=SELF.GGLines.GdFEQ[1] 
    GdLineFEQ[2]=SELF.GGLines.GdFEQ[2] 
    GridArea &= SELF.GGLines.GrdArea ; IF ~GridArea THEN GridArea=1133. ;  GridAreaS4=GridArea 
    GridQ &= SELF.GridQ    
    GETPOSITION(0,Px,Py) ; OPEN(GLWindow) ; SETPOSITION(0,Px,Py) ; SysMenuCls.Init(GLWindow)
    MakeOverWindow()
    TRNControls&=SELF.GGLines.TRNControls ; ?TRNControls{PROP:Use}=TRNControls
    HideGuides&=SELF.GGLines.GdHide       ; ?HideGuides{PROP:Use}=HideGuides
    ?GridColorBtn{PROP:Color}=Cfg:GridClr 
    ?SnapToFEQ{PROP:Text}='&Snap to ' & FeqTypeName ; ?SnapToFEQ{PROP:Tip}='Snap Guidelines to control: ' & FeqName &' '& CLIP(FeqTypeName) &' '& CLIP(FeqName) &' '& FEQ
    IF PWnd$FEQ{PROP:Type}=CREATE:List AND PWnd$FEQ{PROP:Drop}=0 THEN ENABLE(?ListGridBtn).
    DO StartRtn ; DO SetPosition_Poz2PreviewRtn
    ACCEPT
      CASE ACCEPTED()
      OF ?GridBtn    ; DO GridRtn ; CYCLE
      OF ?GridOffBtn ; DO GridOffRtn ; CYCLE
      OF ?HideGuides ; PWnd$GdLineFEQ[1]{PROP:Hide}=HideGuides ; PWnd$GdLineFEQ[2]{PROP:Hide}=HideGuides ; CYCLE
      OF ?SnapToFEQ OROF ?SnapToWnd ; DO SnapToFEQRtn ; DISPLAY           
      OF ?GuideColorBtn ; IF ~COLORDIALOG('Select Guide Line Color',Cfg:GuideClr) THEN CYCLE. ; Poz:GuideColor=Cfg:GuideClr ; SELF.ConfigPut(Cfg:GuideClr)
      OF ?GridColorBtn  ; IF ~COLORDIALOG('Select Grid Line Color', Cfg:GridClr) THEN CYCLE.  ; ?GridColorBtn{PROP:Color}=Cfg:GridClr ; SELF.ConfigPut(Cfg:GridClr)
      OF ?GridAreaS4 ; GridArea=GridAreaS4 ;  ?{PROP:Tip}='GridAreaS4=' & GridAreaS4 &'  GridArea=' & GridArea
      OF ?TRNControls ; DO TRNControlsRtn ; CYCLE 
      OF ?ListGridBtn ; DO ListGridRtn ; CYCLE 
      END        
      CASE EVENT()
      OF EVENT:NewSelection ; POST(EVENT:Accepted,?)  !Spin 
      OF EVENT:Accepted     ; DO SetPosition_Poz2PreviewRtn
      END
    END
    CLOSE(GLWindow) ; UNHIDE(0)
    IF HideGuides THEN 
       SETTARGET(PWnd) ; LOOP GX=1 TO 2 ; DESTROY(GdLineFEQ[GX]) ; END ; CLEAR(GdLineFEQ[]) ; SETTARGET() !; HideGuides=0
    END
    SELF.GGLines.GdFEQ[1]=GdLineFEQ[1]
    SELF.GGLines.GdFEQ[2]=GdLineFEQ[2]
    RETURN
!---------------
GridRtn ROUTINE
    DATA 
BX1  LONG
BX2  LONG
BY1  LONG
BY2  LONG
    CODE   
  IF ~GridAppend THEN DO GridOffRtn.
  IF Cfg:GridInc < 5 THEN Cfg:GridInc=5.
  Bx1=CHOOSE(GridAreaS4[1],0,GdX,Pw) ; Bx2=CHOOSE(GridAreaS4[3],0,GdX,Pw)
  By1=CHOOSE(GridAreaS4[2],0,GdY,Ph) ; By2=CHOOSE(GridAreaS4[4],0,GdY,Ph)
  SETTARGET(PWnd)
  IF GridHVB<>'V' THEN  !Hor
     Fx=BX1 ; Fw=BX2-BX1 ; GX=1 ; C=0 ; LOOP Fy=BY1 TO BY2 BY Cfg:GridInc ; DO Grid1Rtn ; END
  END
  IF GridHVB<>'H' THEN  !Ver
     Fy=By1 ; Fh=By2-By1 ; GX=2 ; C=0 ; LOOP Fx=Bx1 TO Bx2 BY Cfg:GridInc ; DO Grid1Rtn ; END  
  END
  SETTARGET()
Grid1Rtn ROUTINE
  GdFEQ &= GridQ.LnFEQ ; GdFEQ=CREATE(0,CREATE:Line,0)   
  EXECUTE GX
   SETPOSITION(GdFEQ,Fx,Fy,Fw,0) 
   SETPOSITION(GdFEQ,Fx,Fy,0 ,Fh) 
  END 
  IF C AND Cfg:Grid5th AND C % Cfg:Grid5th=0 THEN GdFEQ{PROP:LineWidth}=2. ; C+=1 
  GdFEQ{PROP:Color}=Cfg:GridClr ; UNHIDE(GdFEQ)
  ADD(GridQ,GridQ.LnFEQ) 
GridOffRtn ROUTINE
  IF ~RECORDS(GridQ) THEN EXIT.
  SETTARGET(PWnd) ; LOOP GX=1 TO RECORDS(GridQ) ; GET(GridQ,GX) ; DESTROY(GridQ.LnFEQ) ; END ; FREE(GridQ) ; SETTARGET()
!----------
ListGridRtn ROUTINE
  DATA
ColPozQ QUEUE,PRE()
ClmX LONG
ClmY LONG
ClmW LONG
ClmH LONG
     END
  CODE
  IF ~GridAppend THEN DO GridOffRtn.
  ListColumnPositions(FEQ,ColPozQ) ; IF ~RECORDS(ColPozQ) THEN EXIT.
  SETTARGET(PWnd)   ; GX=2 ; Fy=0 ; Fh=Ph
  LOOP X=1 TO RECORDS(ColPozQ)
     GET(ColPozQ,X) ; C=0 ; Fx=ClmX ; DO Grid1Rtn
  END               ; C=0 ; Fx=ClmX+ClmW ; DO Grid1Rtn
  SETTARGET()
!---------------
SnapToFEQRtn ROUTINE
 DATA
FxC LONG 
FyC LONG 
 CODE
  X=POPUPunder(?,'Left{{Top|Center|Bottom}|Center{{Top|Center|Bottom}|Right{{Top|Center|Bottom}')
  IF ~X THEN EXIT. ; IF ?=?SnapToWnd THEN X+=9.
  FxC= Fx+Fw/2
  FyC= Fy+Fh/2
  Poz:GuideXY[2,1]=CHOOSE(X,Fx,Fx, Fx,    FxC,FxC,FxC   ,Fx+Fw,Fx+Fw,Fx+Fw,  10,10,  10   , Pw/2,Pw/2,Pw/2 , Pw-10,Pw-10,Pw-10) !X Left
  Poz:GuideXY[1,2]=CHOOSE(X,Fy,FyC,Fy+Fh, Fy ,FyC,Fy+Fh ,Fy,   FyC,  Fy+Fh,  10,ph/2,Ph-10, 10  ,Ph/2,Ph-10, 10   ,Ph/2 ,Ph-10) !Y Top
  EXIT    !-----         !  LT LC  LBot   CTp  Cen CBo   RT    RC    RBo   W LT LC   LB     CT   C    CB     RT    RC    RB     Window
StartRtn ROUTINE
  SETTARGET(PWnd) 
  GETPOSITION(0,Px,Py,Pw,Ph) ; PRsz=0{PROP:Resize} ; GETPOSITION(FEQ,Fx,Fy,Fw,Fh)
  IF GdLineFEQ[1]=0 THEN
     LOOP GX=1 TO 2
        GdFEQ &= GdLineFEQ[GX] ; GdFEQ  =CREATE(0,CREATE:Line,0)   
        CASE GX
        OF 1 ; SETPOSITION(GdFEQ,0   ,Ph/2,Pw,0)  !Horz
        OF 2 ; SETPOSITION(GdFEQ,Pw/2,0   ,0,Ph)  !Vert
        END
      !  GdFEQ{PROP:Color}=Cfg:GuideColor ; Poz:GuideColor=Cfg:GuideColor
        IF PRsz THEN GdFEQ{PROP:Full}=1. ; GdFEQ{PROP:LineWidth}=2 ; GdFEQ{PROP:TRN}=1 ; IF ~HideGuides THEN UNHIDE(GdFEQ).
     END 
  END  
  LOOP GX=1 TO 2
    GETPOSITION(GdLineFEQ[GX] ,Poz:GuideXY[GX,1],Poz:GuideXY[GX,2]) 
    Poz:GuideWd[GX] = GdLineFEQ[GX]{PROP:LineWidth}
  END    
  Haz=Poz ; Poz:GuideColor=Cfg:GuideClr    
  SETTARGET()
  ?Poz:GuideXY_1_2{PROP:RangeHigh}=Ph
  ?Poz:GuideXY_2_1{PROP:RangeHigh}=Pw 
  EXIT
SetPosition_Poz2PreviewRtn ROUTINE !Poz is here, Haz is real Window 
  IF Haz=Poz THEN EXIT.
  SETTARGET(PWnd)
  LOOP GX=1 TO 2
    GdFEQ &= GdLineFEQ[GX] 
    IF Poz:GuideXY[GX,1]<>Haz:GuideXY[GX,1] OR Poz:GuideXY[GX,2]<>Haz:GuideXY[GX,2] THEN |
       SETPOSITION(GdFEQ,Poz:GuideXY[GX,1],Poz:GuideXY[GX,2]).
    IF Poz:GuideWd[GX]<>Haz:GuideWd[GX] THEN GdFEQ{PROP:LineWidth}=Poz:GuideWd[GX].
    IF Poz:GuideColor<>Haz:GuideColor THEN GdFEQ{PROP:Color}=Poz:GuideColor ; GLWindow$?GuideColorBtn{PROP:Color}=Poz:GuideColor. 
  END    
  Haz=Poz
  SETTARGET()
  EXIT
TRNControlsRtn ROUTINE
  SETTARGET(PWnd) ; F=0 ; LOOP ; F=0{PROP:NextField,F} ; IF ~F THEN BREAK. ; IF SELF.FeqCreatedByCB(F) THEN CYCLE.
    UPropTRN=F{'UPROP_TRN'}
    IF TRNControls THEN 
       IF ~F{PROP:TRN} THEN 
          IF ~UPropTRN THEN F{'UPROP_TRN'}='0'. !Store it was Not TRN
          F{PROP:TRN}='1'
       END
    ELSIF UPropTRN='0'  
       F{PROP:TRN}=''
    END 
  END ; SETTARGET() 
  EXIT  
!=============  Event Logging ============= 06/26/20
CBWndPreviewClass.EvtLogStart PROCEDURE(BYTE Confirm=0)
  CODE           !e.g. WndPrv_EventLog_20-06-26_070345.txt
  SELF.EvtLog.LogName='WndPrv_EventLog_' & format(today(),@d09-) &'_'& format(clock(),@t05) &'.txt'
  IF Confirm AND 2=Message('Log File: ' & SELF.EvtLog.LogName, 'Start Event Log?',,'&Log Events|&Cancel') THEN RETURN.
  SELF.EvtLog.Started=1
  C5LogSetName(SELF.EvtLog.LogName)
  C5LogPrint('WndPreview Class Event Log ' & format(today(),@d3) &' @ '& format(clock(),@t6) &'<13,10>')
  C5LogPrint('Window: ' & SELF.WndRef{PROP:Text} &' {5}EXE: ' & Command('0') &'<13,10>={80}<13,10>' )
  SELF.EvtLogRegEvts() 
CBWndPreviewClass.EvtLogStop PROCEDURE()
CLog CSTRING(13)
  CODE
  IF ~SELF.EvtLog.Started THEN RETURN. ; SELF.EvtLog.Started=0
  SELF.EvtLogRegEvts(1)
  CLog='C' & INT(_CwVer_/100) & 'LOG.TXT' ; C5LogSetName(CLog) !C110LOG.TXT
  RUN('Notepad ' & SELF.EvtLog.LogName)
CBWndPreviewClass.EvtLogRegEvts PROCEDURE(BYTE UnReg=0)
E USHORT
  CODE
  LOOP E=1 TO 400h
    CASE E
    OF 1 TO  1Fh OROF 101h TO 101h OROF 200h TO 250h  OROF 400h-1   !Skip 102h EVENT:Selecting too much
      IF UnReg THEN UNREGISTER(E,ADDRESS(SELF.EvtLogTakeEvt),ADDRESS(SELF),SELF.WndRef) |
               ELSE   REGISTER(E,ADDRESS(SELF.EvtLogTakeEvt),ADDRESS(SELF),SELF.WndRef).
    END
  END
CBWndPreviewClass.EvtLogTakeEvt PROCEDURE()!,BYTE
P USHORT,AUTO
V LONG,AUTO
F LONG,AUTO
T USHORT,AUTO
More CSTRING(200)
  CODE
  F=FIELD() ; T=F{PROP:Type}
  CASE T
  OF CREATE:List OROF CREATE:combo
     More=' Choice:' & CHOICE(F) & CHOOSE(~KEYCODE(),'',' KeyCode:' & ClaKeyCodeExplain(KEYCODE(),1))
     CASE BAND(KEYCODE(),0FFh) !Click mouse on LIST
     OF MouseLeft TO MouseCenter2 OROF MouseLeftUp TO MouseCenterUp
        LOOP P=0 to 8 ; V=F{PROPLIST:MouseDownRow+P} ; IF ~V THEN CYCLE.
             More=More &' '& CHOOSE(P+1,'DnRow','MvRow','UpRow','DnFld','MvFld','UpFld','DnZone','MvZone','UpZone') &':'& V&' '
        END
     END
  OF CREATE:Sheet OROF CREATE:Option ; More=' Choice:' & SELF.EvtLogField(F{PROP:ChoiceFEQ})
  END
  CASE EVENT()
  OF EVENT:Accepted
     IF CONTENTS(F) THEN More=More&' Contents:' & CONTENTS(F).
  OF EVENT:AlertKey OROF EVENT:PreAlertKey
     More=More&' KeyCode:' & ClaKeyCodeExplain(KEYCODE(),1) 
  OF EVENT:Drop     ; More=More &' DragID:' & DragID() &' DropID:' & DropID()
  OF EVENT:Rejected ; More=More &' Reject:' & RejectCode() &' ScrTxt:' & F{PROP:ScreenText}
  END
  SELF.EvtLogWrite(SELF.EvtLogEVENT() &' '& SELF.EvtLogField() &' '& UPPER(ClaControlTypeName(T)) & |
                  CHOOSE(F=FOCUS() OR ~F OR ~FOCUS(),'',' Focus()=' & SELF.EvtLogField(FOCUS())) & More  )
  RETURN 0
CBWndPreviewClass.EvtLogWrite   PROCEDURE(STRING LogLine)
Stamp STRING(12) !hh:mm:ss.ddS
CDb CSTRING(SIZE(Stamp)+SIZE(LogLine)+2)
Tme LONG,AUTO
  CODE
  IF ~SELF.EvtLog.Started THEN RETURN.
  Tme=CLOCK() ; Stamp=FORMAT(Tme,@t04) & '.' & FORMAT( Tme % 100,@n02)
  C5LogPrint(Stamp & LogLine & '<13,10>')
  CDb='EventLog:' & CLIP(LogLine) & '<13,10>' ; OutputDebugString(CDb)
  RETURN  
CBWndPreviewClass.EvtLogEVENT PROCEDURE()
M CSTRING(48)
E LONG,AUTO 
  CODE     
  E=EVENT() ; IF ~E THEN RETURN ''.         
  ClaEventNameRTL5(M,E+0A000h) ; IF ~M THEN ClaEventNameRTL5(M,E).
  E=LEN(M) ; RETURN M & ALL(' ',20-E)
CBWndPreviewClass.EvtLogField PROCEDURE(<LONG F>)
c cstring(64)
  CODE
  IF OMITTED(F) THEN F=FIELD(). ; IF ~F then return ''. !'Window'.
  c=ClaFieldNameRTL(F) ; RETURN CHOOSE(~c,'Feq#'&F,c &' (' & F & ')')  
!==========================================
ClaListColAlign  PROCEDURE(LONG ListFEQ, LONG ColX, BOOL IsGroup=0, BOOL IsHead=0)!,STRING
AX          USHORT,AUTO
PropLRCD    LONG,AUTO
OffsetNo    SHORT,AUTO
LRCD        STRING(1)
Offset      PSTRING(8)
    CODE
    IsGroup=CHOOSE(~IsGroup,0,PROPLIST:Group) 
    !LRCD='C'    !Groups are Default Center and have no Alignment
    LOOP AX = 1 TO 4
         PropLRCD = CHOOSE(~IsHead,PROPLIST:Left,PROPLIST:HeaderLeft) + (AX-1) * 2 ! PROPLIST:Left TO PROPLIST:Decimal BY 2 s
         IF ~ListFEQ{PropLRCD+IsGroup, ColX} THEN CYCLE.
         LRCD=SUB('LRCD',AX,1)
         OffsetNo = ListFEQ{PropLRCD+1+IsGroup, ColX}   !PROPLIST:LeftOffset .. PROPLIST:DecimalOffset
         IF OffsetNo THEN Offset='('& OffsetNo &')'.
         BREAK 
    END
    RETURN LRCD & Offset
!------------------   
ClaListColResizable PROCEDURE(WINDOW WndRef, LONG ListFEQ)
X   USHORT,AUTO
    CODE
    SETTARGET(WndRef)
    LOOP X=1 TO ListFEQ{PROPLIST:Exists,0}-1 ; IF ~ListFEQ{PROPList:Exists, X} THEN BREAK.
         ListFEQ{PROPLIST:Resize + PROPLIST:Group, X}=1 ; ListFEQ{PROPLIST:Resize, X}=1
    END
    LOOP X=1 TO 255 ; IF ListFEQ{PROP:Alrt,X}=MouseLeft THEN ListFEQ{PROP:Alrt,X}=''. ; END
    SETTARGET()
    RETURN
!==========================================================
CwHelpCls.IsInited  PROCEDURE()
CwVer Decimal(3,1),AUTO
    CODE
    IF ~SELF.bInit THEN DO InitRtn.
    RETURN CHOOSE(SELF.bInit=1)
InitRtn ROUTINE
    SELF.bInit=2
    CwVer=SYSTEM{PROP:ExeVersion,2}/1000
    SELF.ChmFile=CLIP(GETREG(REG_LOCAL_MACHINE, 'SOFTWARE\SoftVelocity\Clarion' & CwVer ,'Root')) &'\bin\ClarionHelp.chm'
    IF ~EXISTS(SELF.ChmFile) THEN Message(CwVer & ' CW help not found|'&SELF.ChmFile) ; EXIT.
    IF SELF.HHOcxLoad() THEN SELF.bInit=1 ELSE Message('Cannot load HELP HH OCX').
CwHelpCls.HHOcxLoad  PROCEDURE()
DllName  CSTRING('hhctrl.ocx')
ProcName CSTRING('HtmlHelpA')
hDll LONG,AUTO
    CODE
    IF ~SELF.HHLoaded THEN
        SELF.HHLoaded=2
        hDll=LoadLibrary(DllName) ; IF hDll THEN HtmlHelp_fp=GetProcAddress(hDll, ProcName).
        IF HtmlHelp_fp THEN SELF.HHLoaded=1.
    END
    RETURN CHOOSE(SELF.HHLoaded=1)
CwHelpCls.HHCaller PROCEDURE(LONG HH_Command, LONG dwData=0) !0=Topic 1=TOC 2=Index
CHM CSTRING(300)
    code
    IF SELF.IsInited() THEN HtmlHelp(0,SELF.CHMFile,HH_Command,dwData).
CwHelpCls.OpenHelp PROCEDURE(string sHlp)   !Open a "~Context.htm" or "Keyword"
cData CSTRING(SIZE(sHlp)+1),AUTO
cmd   CSTRING(500),AUTO
    CODE
    IF ~SELF.IsInited() THEN RETURN.
    IF sHlp[1]='~' THEN
       cData=CLIP(sHlp[2 : SIZE(sHlp)])
       !SELF.HHCaller(0,ADDRESS(CData)) ; SELF.HHCaller(1) !Help will close with Preview.EXE
       cmd='HH.EXE ms-its:' & CLIP(SELF.CHMFile) &'::/' & CLIP(cData)
       RUN(Cmd) !Extern HH Viewer will stay open
    ELSE
       cData=CLIP(sHlp) ; SELF.HHCaller(2h, ADDRESS(cData))  !2=HH_DISPLAY_INDEX
    END
!==========================================================
CBSortClass.Init PROCEDURE(QUEUE ListQueue, LONG ListFEQ, SHORT SortColNow=0)
    CODE
    SELF.QRef &= ListQueue
    SELF.FEQ=ListFEQ
    ListFEQ{PROPLIST:HasSortColumn}=1
    SELF.FEQ{PROPLIST:HdrSortBackColor}=8000001Bh  !GradientActiveCaption
    SELF.FEQ{PROPLIST:HdrSortTextColor}=Color:CaptionText
    !Brighter {PROPLIST:HdrSortBackColor}=COLOR:Highlight / {PROPLIST:HdrSortTextColor}=COLOR:HighlightText    
    IF SortColNow THEN SELF.SetSortCol(SortColNow).
    RETURN
!-----------------------------------
CBSortClass.SetSortCol PROCEDURE(SHORT SortColNow)
    CODE
    SELF.ColumnNow=SortColNow
    SELF.FEQ{PROPLIST:Locator,ABS(SortColNow)}=1
    SELF.QFieldLast=SELF.FEQ{PROPLIST:FieldNo,ABS(SortColNow)}
    SELF.Who1st=CHOOSE(SortColNow<0,'-','+') & WHO(SELF.QRef,SELF.QFieldLast) ; SELF.Who2nd=''
    SELF.FEQ{PROPLIST:SortColumn}=ABS(SortColNow)
    RETURN    
!-----------------------------------
CBSortClass.HeaderPressed PROCEDURE(SHORT ForceSortByColumn=0)
QRecord    STRING(SIZE(SELF.QRef)),AUTO
LChoice    LONG,AUTO
X          LONG,AUTO
ColumnNow  &SHORT
ColumnLast &SHORT
QFieldNow  &SHORT
QFieldLast &SHORT
Who1st     &STRING
Who2nd     &STRING
    CODE
    ColumnNow&=SELF.ColumnNow;ColumnLast&=SELF.ColumnLast;QFieldNow&=SELF.QFieldNow;QFieldLast&=SELF.QFieldLast;Who1st&=SELF.Who1st;Who2nd&=SELF.Who2nd
    LChoice = CHOICE(SELF.FEQ)
    IF LChoice THEN GET(SELF.QRef, LChoice) ; QRecord=SELF.QRef.
    ColumnNow=CHOOSE(~ForceSortByColumn, SELF.FEQ{PROPList:MouseDownField}, ForceSortByColumn)
    QFieldNow=SELF.FEQ{PROPLIST:FieldNo,ColumnNow} 
    IF QFieldNow<>ABS(QFieldLast) AND Who1st THEN Who2nd=',' & Who1st.
    Who1st=CHOOSE(QFieldNow=QFieldLast,'-','+') & WHO(SELF.QRef,QFieldNow)
 SELF.FEQ{PROP:Tip}='Sort ColumnNow:'& ColumnNow  &' QNow=' & QFieldNow &' QLast='& QFieldLast &' Who=' & CLIP(Who1st) & Who2nd
    SORT(SELF.QRef,CLIP(Who1st) & Who2nd)
    SELF.FEQ{PROPLIST:Locator,ColumnNow}=1
    ColumnLast=CHOOSE(QFieldNow=ABS(QFieldLast),-1*ColumnLast,ColumnNow) 
    QFieldLast=CHOOSE(QFieldNow=ABS(QFieldLast),-1*QFieldLast,QFieldNow) 
    IF LChoice THEN !Reselect the LChoice that was selected 
       LOOP X=1 TO RECORDS(SELF.QRef) ; GET(SELF.QRef,X)
          IF SELF.QRef=QRecord THEN SELF.FEQ{PROP:Selected}=X ; BREAK.
       END
    END
    DISPLAY
    RETURN
!==========================================================
BevClass.Init PROCEDURE(LONG BOFeq, *LONG BevelStyle, LONG StyleFEQ)
    CODE
    SELF.BOFeq = BOFeq
    SELF.Style &= BevelStyle
    SELF.StyFEQ = StyleFEQ
    SELF.StyleChanged()    
    RETURN
BevClass.StyleChanged PROCEDURE()
E8  BYTE,AUTO
BS  LONG,AUTO
S4  SHORT,AUTO
    CODE
    BS=SELF.Style      
    CLEAR(SELF.Edgz[])
    LOOP E8=1 to 8
        S4=BAND(BS,11b) + 1 ; BS=BSHIFT(BS,-2) ; SELF.BtnConfig(E8,S4)
    END
BevClass.BtnClick PROCEDURE(LONG BtnFEQ)   !OF ?BtnBO1 TO ?BtnLI8 ; BevCls.BtnClick(?)
E8  SHORT,AUTO
S4  SHORT,AUTO
PUP PSTRING(100),AUTO
Chk PSTRING(2),DIM(4)
xB  LONG,AUTO
yB  LONG,AUTO
wB  LONG,AUTO
hB  LONG,AUTO
    CODE
    E8=BtnFEQ-SELF.BOFeq+1
?   ASSERT(E8>=1 AND E8<=8,'E8='&E8)
    IF E8<1 AND E8>8 THEN RETURN.    
    S4=SELF.Edgz[E8]
?   ASSERT(S4>=1 AND S4<=4,'S4='&S4)    
    Chk[S4]='+'
    PUP='~'& BtnFEQ{PROP:Tip} &' |-|'& Chk[1]&'No Edge|'&Chk[2]&'Raised Edge|'&Chk[3]&'Lowered Edge|'&Chk[4]&'Gray Edge'
    GETPOSITION(BtnFEQ,xB,yB,wB,hB) ; IF E8>6 OR E8=4 THEN yB+=hB ELSE xB+=wB.
    S4=POPUP(PUP,xB,yB,1)-1 
    IF S4>=1 AND S4<=4 THEN 
       SELF.BtnConfig(E8,S4) 
       SELF.Edge2Style()
    END       
    RETURN
BevClass.BtnConfig PROCEDURE(BYTE E8, BYTE S4)
    CODE
?   ASSERT(E8>=1 AND E8<=8 AND S4>=1 AND S4<=4,'E8=' & E8 &' S4='&S4)    
    IF SELF.Edgz[E8]<>S4 THEN
       SELF.Edgz[E8]=S4
       (SELF.BOFeq+E8-1){PROP:Text}=SUB('NRLG',S4,1)
       (SELF.BOFeq+E8-1){PROP:Tip}=CHOOSE(INT((E8-1)/2)+1,'Bottom','Right','Top','Left','?') &|
                                   CHOOSE((E8-1)%2+1,' Outer',' Inner','?') &' - '&|
                                   CHOOSE(S4,'No','Raised','Lowered','Gray','?') &' Edge'                        
    END
    RETURN
BevClass.Edge2Style PROCEDURE()
E8  BYTE,AUTO
BS  LONG
    CODE
    LOOP E8=1 to 8       
?       ASSERT(BAND(SELF.Edgz[E8],111b),'SELF.Edgz[E8] = '& SELF.Edgz[E8])
        BS += BSHIFT(SELF.Edgz[E8]-1,(E8-1)*2)
    END
    IF SELF.Style<>BS THEN SELF.Style=BS ; POST(EVENT:Accepted,SELF.StyFEQ) ; DISPLAY.
    RETURN 
BevClass.AllBtn PROCEDURE()
A   BYTE,AUTO
E8  BYTE,AUTO
S4  BYTE,AUTO  ! 1    2      3       4
Ed4 STRING('{{No Edge|Raised|Lowered|Gray}')
    CODE
    A=POPUP('All Outer'& Ed4 &'|All Inner'& Ed4 &'|-|Top && Bottom'& Ed4 &'|Left && Right'& Ed4 &'|-|All Edges'& Ed4) 
    IF ~A THEN RETURN. ; A-=1 ; S4=A%4+1 ; A=A/4+1
    LOOP E8=1 TO 8 
        CASE A
        OF 1 OROF 2 ; IF E8%2<>A%2 THEN CYCLE.
        OF 3 ; IF (E8-1)%4>1 THEN CYCLE. !Top && Bottom
        OF 4 ; IF (E8-1)%4<2 THEN CYCLE. !Left && Right
        END
        SELF.BtnConfig(E8,S4)
    END
    SELF.Edge2Style() ; DISPLAY
    RETURN
!==========================================================
CBLocateCls.Init  PROCEDURE(QUEUE QRef, LONG ListFEQ, LONG TextFEQ, LONG NextBtn, LONG PrevBtn, BYTE Hack=0)
  CODE
  SELF.IsInit=1 
  SELF.QRef   &= QRef    ; SELF.NextBtn = NextBtn
  SELF.ListFEQ = ListFEQ ; SELF.PrevBtn = PrevBtn
  SELF.TextFEQ = TextFEQ ; IF Hack THEN RETURN.
  TextFEQ{PROP:Key}=CtrlF ; TextFEQ{PROP:Alrt,255}=EnterKey
  TextFEQ{PROP:Tip}=CHOOSE(~TextFEQ{PROP:Tip},'Text to Locate, (Ctrl F) to Select.',TextFEQ{PROP:Tip}) & |
                      '<13,10>Prefix with % for Regular Expression'
  REGISTER(EVENT:AlertKey,ADDRESS(SELF.TakeAlertKey),ADDRESS(SELF),,TextFEQ)
  REGISTER(EVENT:Accepted,ADDRESS(SELF.TakeAccepted),ADDRESS(SELF),,TextFEQ)
  REGISTER(EVENT:Accepted,ADDRESS(SELF.TakeAccepted),ADDRESS(SELF),,NextBtn)
  IF ~NextBtn{PROP:Tip} THEN NextBtn{PROP:Tip}='Find Next'.
  IF PrevBtn THEN
     IF ~PrevBtn{PROP:Tip} THEN PrevBtn{PROP:Tip}='Find Previous'.
     REGISTER(EVENT:Accepted,ADDRESS(SELF.TakeAccepted),ADDRESS(SELF),,PrevBtn)
  END   
  RETURN
CBLocateCls.Kill  PROCEDURE()
  CODE
  IF ~SELF.IsInit THEN RETURN.
  IF SELF.TextFEQ THEN 
     UnREGISTER(EVENT:AlertKey,ADDRESS(SELF.TakeAlertKey),ADDRESS(SELF),,SELF.TextFEQ)
     UnREGISTER(EVENT:Accepted,ADDRESS(SELF.TakeAccepted),ADDRESS(SELF),,SELF.TextFEQ)
  END 
  IF SELF.NextBtn THEN UnREGISTER(EVENT:Accepted,ADDRESS(SELF.TakeAccepted),ADDRESS(SELF),,SELF.NextBtn).
  IF SELF.PrevBtn THEN UnREGISTER(EVENT:Accepted,ADDRESS(SELF.TakeAccepted),ADDRESS(SELF),,SELF.PrevBtn).
  SELF.IsInit=0 ; SELF.TextFEQ=0 ; SELF.NextBtn=0 ; SELF.PrevBtn=0
  RETURN
CBLocateCls.TakeAlertKey PROCEDURE()
  CODE
  IF FIELD()=SELF.TextFEQ AND KEYCODE()=EnterKey THEN
     UPDATE(SELF.TextFEQ) ; SELF.Locate(1)
  END
  RETURN 0
CBLocateCls.TakeAccepted PROCEDURE()
  CODE
  UPDATE !SKIP on ENTRY no Update for press Alt+F
  CASE FIELD()
  OF 0     !Not OF SELF.TextFEQ ; SELF.Locate()  else AltF does 2x
  OF SELF.NextBtn ;  SELF.Locate(1,1)
  OF SELF.PrevBtn ;  SELF.Locate(-1,1)
  END
  RETURN 0
CBLocateCls.Locate PROCEDURE(SHORT NextPrev=1, BOOL IsButton=0)
Txt   PSTRING(65),AUTO
Ndx   LONG,AUTO
RegEx BYTE
    CODE
  Txt=CLIP(lower(LEFT(CONTENTS(SELF.TextFEQ)))) 
  IF ~Txt OR ~SELF.IsInit THEN
      IF IsButton THEN SELECT(SELF.TextFEQ). ; RETURN
  END    
  IF Txt[1]='%' AND LEN(Txt)>1 THEN RegEx=1 ; Txt=Txt[2 : LEN(Txt)] .
  Ndx = CHOICE(SELF.ListFEQ) ; IF ~NextPrev THEN NextPrev=1.
  IF -1=NextPrev AND Ndx<2 THEN Ndx=RECORDS(SELF.QRef)+1.
  LOOP
     Ndx += NextPrev
     GET(SELF.QRef, Ndx) ; IF ERRORCODE() THEN BREAK.
     IF (~RegEx AND INSTRING(Txt,lower(SELF.QRef),1,1)) |
     OR (RegEx  AND MATCH(lower(SELF.QRef),Txt,MATCH:Regular)) THEN
        SELECT(SELF.ListFEQ, Ndx)
        BREAK
     END
  END
  RETURN 
CBLocateCls.DisableIfNone PROCEDURE()
D STRING(1)
  CODE
  IF RECORDS(SELF.QRef)<2 THEN D='1'.
  SELF.TextFEQ{PROP:Disable}=D ; SELF.NextBtn{PROP:Disable}=D
  IF SELF.PrevBtn THEN SELF.PrevBtn{PROP:Disable}=D.
  DISPLAY  
  RETURN
!########################    