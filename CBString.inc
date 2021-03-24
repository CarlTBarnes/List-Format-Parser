! (c) 2016 by Carl Barnes released under MIT License            
!--------------------------------------------------------------------------------
CBStringUtilityClass    Class(),Type,Module('CBString.Clw'),DLL(0),Link('CBString.Clw',1)
!Init            PROCEDURE ()
FormatN0            FUNCTION (CONST *decimal dNum),STRING !format number @n-31 so has commas, LEFT just
FormatN2            FUNCTION (CONST *decimal dNum),STRING !format number @n-31.2 so has commas, LEFT just

InstringReverse     FUNCTION(STRING FindThis, STRING SrchText),LONG 
ParseTokenStringIntoGroup FUNCTION(String InTxt, *GROUP OutGroup, STRING Token, SHORT Max, BYTE bSkipBlanks),LONG,PROC !rtns count
ParseTokenStringIntoQueue FUNCTION (*string p_String,string p_Token,queue Out_Queue,byte p_BlanksOk=0),LONG,PROC !Returns Cnt
ReplaceBetweenTokens      FUNCTION(*String Into, String TokenBeg, String TokenEnd, String Replace),LONG,PROC !returns start pos, 0=none, -1=no end
ReplacePropText     PROCEDURE(long IntoFEQ, String Find, String Replace, SHORT FirstOnly=0)
ReplaceString       PROCEDURE(String Find, String Replace, *String Into, SHORT FirstOnly=0)
SmashAZ09           FUNCTION(string inStr, BOOL NoNumbers=0, BOOL KeepSpaces=0),STRING !Keep A-Z 0-9, Remove Spaces + Specials
SmashSpaces         PROCEDURE(*string Txt, BOOL KeepLowASCII=0)      !Remove Whitespace (9,10,13,12,11)
TrimText1310        PROCEDURE(*STRING Txt)  !Remove 13,10,32,9 Leading or Trailing
DB                  PROCEDURE(STRING Info)
DBhexDumpString     PROCEDURE(*STRING SrcStr)
DBhexDump           PROCEDURE(long SrcAddr, Long SrcSize, Byte ShowAddr=0) 
             End

