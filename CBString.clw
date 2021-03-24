! (c) 2016 by Carl Barnes released under MIT License
      Member()
      Include('Equates.CLW'),ONCE
      Map
        module ('win32')
          OutputDebugString(*CString), raw, pascal, name('OutputDebugStringA'),Dll(dll_mode)
        end
      End ! map
      Include('CBString.inc'),ONCE

!--------------------------------------------------------------------------------------------------
!CBStringUtilityClass.Init    PROCEDURE  ()
!  CODE  
!  Message('CodeFlattenCls.Init')   
!  RETURN
!--------------------------------------------------------------------------------------------------
CBStringUtilityClass.FormatN0 FUNCTION (CONST *decimal dNum)!,STRING
  CODE
  RETURN CLIP(LEFT( FORMAT(dNum,@n-31) ))
!--------------------------------------------------------------------------------------------------
CBStringUtilityClass.FormatN2 FUNCTION (CONST *decimal dNum)!,STRING
  CODE
  RETURN CLIP(LEFT( FORMAT(dNum,@n-31.2) ))
!--------------------------------------------------------------------------------------------------
CBStringUtilityClass.InstringReverse FUNCTION(STRING FindThis, STRING SrchText)!,LONG 
Size_FindThis   LONG,AUTO
FindOffset      LONG,AUTO       ! is = Size_FindThis - 1, slice offset is always + 1
FindByte1       BYTE,AUTO
Size_SrchText   LONG,AUTO
Ndx             LONG,AUTO
RetIndx         LONG(0)
!   It does work to do INSTRING(X,InY,-1,SIZE(InY)) ... but has a limit of 255?
  CODE
    Size_FindThis = Size(FindThis)
    Size_SrchText = Size(SrchText)
    IF ~Size_FindThis OR Size_SrchText<Size_FindThis THEN RETURN 0.            !If you want to find space better send ' '

    FindOffset = Size_FindThis - 1
    FindByte1  = VAL(FindThis[1])
    LOOP Ndx=Size_SrchText - FindOffset TO 1 BY -1
         IF VAL(SrchText[Ndx]) = FindByte1 AND |                     !hoping doing this quick BYTE check will save doing many STRING compares
            SrchText[ Ndx : Ndx + FindOffset ] = FindThis
               RetIndx = Ndx
               BREAK
         END
    END

    RETURN RetIndx
!--------------------------------------------------------------------------------------------------
CBStringUtilityClass.ParseTokenStringIntoGroup FUNCTION(String InTxt, *GROUP OutGroup, STRING Token, SHORT Max, BYTE bSkipBlanks)!,LONG,PROC !rtns count
Idx     LONG,AUTO
NextToken   LONG,AUTO
LastToken   LONG,AUTO
GroupIdx    LONG,AUTO
FieldRef    ANY
TokenLen    LONG,AUTO
  CODE
!(*String InString, *GROUP OutGroup, STRING Token, SHORT Max)
    LastToken = 1
    GroupIdx  = 0
    TokenLen  = LEN(CLIP(Token))
?   ASSERT(TokenLen,'ParseTokenStringIntoGroup - No Token')
    LOOP
        IF ~SUB(InTxt, LastToken, 999) THEN BREAK.
        GroupIdx += 1
        FieldRef &= WHAT(OutGroup, GroupIdx)

        IF Max AND GroupIdx = Max                                       !--If this is the Last Field in the Group
           FieldRef = SUB(InTxt, LastToken, 999)                     !  take what's left and leave
           BREAK
        END

        NextToken = INSTRING( Token[1 : TokenLen] , InTxt , 1 , LastToken )
        IF ~NextToken                                                   !--if this is the last token
           FieldRef = SUB(InTxt, LastToken, 999)                     !  take what's left and leave
           BREAK
        END

        FieldRef = LEFT(SUB(InTxt, LastToken, NextToken - LastToken ))          !  take what's left and leave
        LastToken = NextToken + TokenLen
        IF bSkipBlanks AND ~FieldRef THEN  GroupIdx -= 1.                       !Is this a blank line that I am skipping

    END
    RETURN  GroupIdx
!--------------------------------------------------------------------------------------------------
CBStringUtilityClass.ReplaceBetweenTokens FUNCTION(*String Into, String TokenBeg, String TokenEnd, String Replace)!,LONG,PROC !returns start pos, 0=none, -1=no end
RetFound    LONG,AUTO
Token1Pos   LONG,AUTO   !TokenBeg Pos
Token2Pos   LONG,AUTO   !TokenEnd Pos
!for example
! ReplaceBetweenTokens(URL2fix,'&n=','&','newvalue')
! URL=v=18&r=081216&s=12345&vr=K&p=8477051972&n=MyNameHere&c=844&e=11/13/2009
    CODE                                 !   ^
    RetFound = 0
    TokenBeg = UPPER(TokenBeg)
    TokenEnd = UPPER(TokenEnd)
    Token1Pos = INSTRING(TokenBeg,UPPER(into),1) !,StartAt)
    IF Token1Pos THEN
       Token2Pos = INSTRING(TokenEnd,UPPER(into),1,Token1Pos+1)
       IF ~Token2Pos THEN
          RetFound = -1    !Tell caller found Start but not End
       ELSE
          into = SUB(into,1,Token1Pos+size(TokenBeg)-1) |   !...&n=
                 & replace |                                !      new name
                 & SUB(into,Token2Pos,SIZE(into))           !   &.......the rest
          RetFound = Token1Pos
       END
    END
    RETURN RetFound
!--------------------------------------------------------------------------------------------------
CBStringUtilityClass.ParseTokenStringIntoQueue FUNCTION(*string p_String,string p_Token,queue Out_Queue,byte p_BlanksOk=0)!,LONG,PROC
SLen           long,AUTO             !P_string Length
Ndx            long,AUTO
TokenVal       long,auto             !When hunting for chars I like VAL compares, was p_separator
BegPos         long(1)               !BegPos = 1  required to work right
EndPos         long(0)               !EndPos  = 0 required to work right
FoundCount     long(0)               !was p_Number in _SplitParm and counted down
  CODE                                                     ! Begin processed code
    SLen=len(p_String)
    TokenVal=val(p_Token)
    loop Ndx = 1 to SLen + 1       !Go 1 past end of string and assume end is final separator
         if Ndx <= SLen AND VAL(p_String[Ndx]) <> TokenVal THEN CYCLE.   !Not a separator, and < end of p_String cycle back for more

         if Ndx>BegPos or TokenVal<>32     !remove double spaces
            FoundCount  += 1               !Found a separator, so got next Parm

               EndPos = Ndx                 !Save end position of the last separator
               Clear(Out_Queue)              !if Queue has add'l fields that are not string this could be problem
               if EndPos > BegPos            !Endpos only set if parm found, If sequential separators then End-1<Beg and so slice is not valid, so fall thru to return empty
                  Out_Queue = p_String[ BegPos : EndPos - 1 ]
               end
               if p_BlanksOk or Out_Queue
                  ADD(Out_Queue)
                  FoundCount += 1
               end
         end
         BegPos = Ndx + 1         !New data begins after previous p_Separator
    end
    return FoundCount

    !Should I have named this Split2Queue() ?
    !Based on _SplitParm I wrote for NetTalk

!--------------------------------------------------------------------------------------------------
CBStringUtilityClass.ReplacePropText PROCEDURE(long IntoFEQ, String Find, String Replace, SHORT FirstOnly=0)
PText   string(1024),auto   !I am probably doing short prompt so assume that
  CODE
    pText = IntoFEQ{prop:text}
    SELF.ReplaceString(Find, Replace, PText, 1)
    IntoFEQ{prop:text} = pText
    return
!--------------------------------------------------------------------------------------------------
CBStringUtilityClass.ReplaceString PROCEDURE (String Find, String Replace, *String Into, SHORT FirstOnly=0) 
Locate      LONG,AUTO
StartAt     LONG,AUTO
SizFind     LONG,AUTO   ! SIZE(find)
SizInto     LONG,AUTO   ! SIZE(into)
Sizreplace  LONG,AUTO   ! SIZE(replace)
  CODE
! The routine replaces instances of 'find' with 'replace' in 'into'
!Replace       PROCEDURE(string find,string replace,*cstring into, SHORT FirstOnly=0)
!   You MUST have the values of the FIND and REPLACE strings be the exact length you want
!   so you might have to clip them, I'm not sure about trailing blanks
    SizFind     = SIZE(find)
    SizInto     = SIZE(into)
    Sizreplace  = SIZE(replace)

    !seems to work to call (' ','',Txt) to remove spaces, Sizreplace=0
    !stop('SizFind=' & SizFind & ' Sizreplace=' & Sizreplace )

    IF '' = Find THEN GOTO ReplaceSpacesLabel: .
    find = UPPER(find)               !do this once at the start to save on Uppers
    IF Sizreplace >= SizFind AND find = UPPER(replace) THEN RETURN.  !this would hang us
    StartAt = 1
TryAgain
    Locate = INSTRING(find,UPPER(into),1,StartAt)
    IF Locate 
       into = SUB(into,1,Locate-1) & replace & SUB(into,Locate+SizFind,SizInto)
       IF ~FirstOnly THEN
           StartAt = Locate + Sizreplace ! -1 would be wrong FYI
           GOTO TryAgain  !Yes it's a GOTO ... do I need a Scralett "G" for my back?
       END
       !Could make FirstOnly -> ReplaceCount where =0 means ALL
    END
    RETURN
!---------------------- Spaces version, else hangs
ReplaceSpacesLabel:
    IF Sizreplace >= SizFind AND '' = replace THEN RETURN.  !this would hang us
    StartAt = 1
TryAgainSpaces:
    Locate = INSTRING(find,UPPER(CLIP(into)),1,StartAt)     !Must CLIP for spaces
    IF Locate 
       into = SUB(into,1,Locate-1) & replace & SUB(into,Locate+SizFind,SizInto)
       IF ~FirstOnly THEN
           StartAt = Locate + Sizreplace ! -1 would be wrong FYI
           GOTO TryAgainSpaces:
       END
       !Could make FirstOnly -> ReplaceCount where =0 means ALL
    END
    RETURN
!--------------------------------------------------------------------------------------------------
CBStringUtilityClass.SmashAZ09 FUNCTION(string inStr, BOOL NoNumbers=0, BOOL KeepSpaces=0)!,STRING Keep A-Z 0-9, Remove Spaces and Chars
otNdx     LONG,AUTO
inIdx     LONG,AUTO
LenStr    LONG,AUTO
  CODE                                                     ! Begin processed code
    otNdx=0
    LenStr=LEN(CLIP( inStr ))
    LOOP inIdx = 1 TO LenStr
         CASE VAL(inStr[inIdx])
         OF 41h TO 5Ah                              !'A' TO 'Z'
         OF 61h TO 7Ah                              !'a' TO 'z'
         OF 30h TO 39h ; IF NoNumbers THEN CYCLE.   !'0' TO '9'
         OF 32         ; IF ~KeepSpaces THEN CYCLE.
         ELSE
            CYCLE
         END
         otNdx += 1 
         IF otNdx < inIdx THEN 
            inStr[otNdx] = inStr[inIdx]
         END   
    END
    IF ~otNdx THEN RETURN ''.  !let's not crash with a [1:0]
    RETURN inStr[1 : otNdx]
!--------------------------------------------------------------------------------------------------
CBStringUtilityClass.SmashSpaces PROCEDURE(*string Txt, BOOL KeepLowASCII=0)
LenTxt  long,AUTO
InX     long,AUTO
OtX     long,AUTO
  CODE   
    LenTxt = LEN(CLIP(Txt))
    IF ~LenTxt THEN RETURN.
    OtX = 0
    LOOP InX= 1 TO LenTxt
         CASE val(Txt[InX]) 
         OF 0 OROF 9 TO 13  ; CYCLE                         !WhiteSpace,Tab,11,FF,CR,LF
         OF 0 TO 32         ; IF ~KeepLowASCII THEN CYCLE.     !just kill all low ASCII its bad news
         OF   160                                           !HardSpace=160=A0h
         OROF 129           ; CYCLE                         !paste 13,10 can put 81h 129s
         END
         OtX += 1
         IF Otx < InX THEN
            Txt[OtX] = Txt[InX]
         END
    END
    IF OtX < LenTxt THEN Txt[ OtX+1 : LenTxt]=''.   !blank out stuff at end
    RETURN
!--------------------------------------------------------------------------------------------------
CBStringUtilityClass.TrimText1310 PROCEDURE(*STRING Txt)  !Remove 13,10,32,9 Leading or Trailing
Ndx LONG,AUTO
    CODE
    LOOP Ndx=1 TO SIZE(Txt)                     !10/14/17 remove all tabs seems like a good idea
         IF VAL(Txt[Ndx]) = 9 THEN Txt[Ndx]=''. !10/14/17 I think this is done later and NOT needed here
    END
    LOOP Ndx=1 TO SIZE(Txt)             !Remove leading 13,10,9,spaces
         IF VAL(Txt[Ndx]) > 32 THEN     !Found first non-space,9,13,10
            Ndx -= 1                    !back up to last 13,10,9,32
            IF Ndx > 0 THEN
               CLEAR(Txt[1 : Ndx])      !Blank out leading  13,10,9,32
            END
            BREAK
         END
    END
    Txt=LEFT(Txt)                       !Remove leading spaces
    LOOP Ndx=SIZE(Txt) TO 1             !Remove trailing 13,10
         IF VAL(Txt[Ndx]) > 32 THEN
            Ndx += 1
            IF Ndx < SIZE(Txt) THEN
               Txt[Ndx : SIZE(Txt)]='<13,10>'
            END
            BREAK
         END
    END
    RETURN

!--------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------
CBStringUtilityClass.DBhexDump PROCEDURE(long SrcAddr, Long SrcSize, Byte ShowAddr=0)
Segment16       long,auto
ByteNo          long,auto
Mem_Hex         equate(8+4)
Mem_Chr         equate(Mem_Hex+16*3+1)
MemLine         string(Mem_Chr+16),auto         !AAAAAAAA XX x16  16CHRbytes
cMemLine        cstring(size(MemLine)+3),auto
!Byte1           byte,auto
Byte1           &byte
HexD            STRING('0123456789ABCDEF')
  CODE 
!Call as DBHexDump(Address(Thing),Size(Thing)) or DBHexDump(Address(Thing),Size(Thing))
    MemLine='DebugHexDump ' & SrcAddr &','& SrcSize 
    MemLine[Mem_Chr : Mem_Chr+15]='0123456789abcdef'
    cMemLine=clip(MemLine) & '<13,10>' ; outputdebugstring(cMemLine)
    if SrcAddr >= 0 and SrcAddr < 10000h then return.       !memory < 64KB will cause access violation. Could be <0 if /3GB LargeMemory
    if SrcSize <= 0 then return.                            !Passed LEN(CLIP()) and it was zero
!    if IsBadReadPtr(SrcAddr,SrcSize) then return.           !let's not GPF, IsBadReadPtr() has it's own dangers so only use in Debug
    loop Segment16=SrcAddr to SrcAddr+SrcSize-1 by 16
         MemLine=choose(~ShowAddr,Segment16-SrcAddr,Segment16)
         MemLine[Mem_Hex+3*8-1]='-'
         Loop ByteNo = 0 to 15
              if Segment16 + ByteNo > SrcAddr+SrcSize-1 then break.
              !MemCpy(address(Byte1),Segment16 + ByteNo ,1)
              Byte1 &= (Segment16 + ByteNo)
              MemLine[Mem_Hex + ByteNo*3 : Mem_Hex + ByteNo*3+1]=HexD[BSHIFT(Byte1,-4)+1] & HexD[BAND(Byte1,0FH) + 1]
              MemLine[Mem_Chr + ByteNo]=choose(Byte1<32,'.',chr(Byte1))
         end
         cMemLine=clip(MemLine) & '<13,10>' ; outputdebugstring(cMemLine)
    end
    RETURN
!--------------------------------------------------------------------------------------------------    
CBStringUtilityClass.DBhexDumpString PROCEDURE (*STRING SrcStr)
  CODE
    SELF.DBhexDump(ADDRESS(SrcStr),SIZE(SrcStr),0)
    RETURN 
!--------------------------------------------------------------------------------------------------
CBStringUtilityClass.DB   PROCEDURE(STRING xMessage)
sz   CSTRING(SIZE(xMessage)+3),AUTO
  CODE 
  sz=CLIP(xMessage) & '<13,10>'
  OutputDebugString(sz)
  RETURN 
!--------------------------------------------------------------------------------------------------
!====================================================================================
!!--------------------------------------------------------------------------------------------------
!CBStringUtilityClass.
!  CODE
!  RETURN


!--------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------
  
!--------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------

