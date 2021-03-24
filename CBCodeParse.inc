! (c) 2016 by Carl Barnes released under MIT License

CBFlatLineMapQType  QUEUE,TYPE  !This is for some trace/debug
Line1           LONG
Line2           LONG 
Txt             STRING(256)
            END
            
!--------------------------------------------------------------------------------
CBCodeFlattenClass    Class(),Type,Module('CBCodeParse.Clw'),DLL(0),Link('CBCodeParse.Clw',1)
Init            PROCEDURE ()  
TrimText1310    PROCEDURE(*STRING Txt, BOOL AddOne1310toEnd=1)        !Remove 13,10,32,9 Leading or Trailing
Flatten         PROCEDURE(*STRING WinTxt)
Flatten         PROCEDURE(*STRING WinTxt, CBFlatLineMapQType LnMapQ )
             End

CBCodeParseClass    Class(),Type,Module('CBCodeParse.Clw'),DLL(0),Link('CBCodeParse.Clw',1) 
FindAttrParen       PROCEDURE(STRING CodeTxt, STRING AttName, *LONG BegAttPos, *LONG BegParen, *LONG EndParen, *LONG BegQuote, *LONG EndQuote),BOOL
MakeCodeOnlyLine    PROCEDURE(CONST *STRING ALine, *CSTRING COLine) !Mask Text, does leave
ParseCOLineAttrib2Q PROCEDURE(CONST *STRING ALine, CONST *CSTRING COLine, QUEUE LinesQ) !Split at Commas between Attribs
            END
