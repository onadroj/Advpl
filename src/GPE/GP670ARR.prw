#INCLUDE "PROTHEUS.CH"


User Function GP670ARR()
Local aRet := {}

cFornece := ALLTRIM(RETFIELD("SA2",1,XFILIAL("SA2")+RC1->RC1_FORNECE+RC1->RC1_LOJA,"SA2->A2_NREDUZ"))
aadd(aRet,{"E2_HIST",ALLTRIM(RC1->RC1_DESCRI)+" - "+cFornece ,NIL})
aadd(aRet,{"E2_CUSTO",RC1->RC1_CC,NIL})  
aadd(aRet,{"E2_COMPETE",RC1->RC1_XCOMPE,NIL}) 
/*
aadd(aRet,{"E2_RATEIO","S",NIL})

aadd(aRet,{"E2_ARPER",RC1->RC1_ARPER,NIL})
aadd(aRet,{"E2_ARFOR",RC1->RC1_ARFOR,NIL})
aadd(aRet,{"E2_ARFLOJ",RC1->RC1_ARFLOJ,NIL})
aadd(aRet,{"E2_VALOR5",RC1->RC1_OENTID,NIL})
*/

Return aRet
 
