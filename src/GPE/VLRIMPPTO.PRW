#include "rwmake.ch"

User Function VLRIMPPTO()   
Local _Texto := PARAMIXB
Local _RETORNO := .T.
Local _nHoras := 0    
Local _cDemitido   

IF SUBSTR(_Texto,1,1)=="D"
	_cDemitido := RETFIELD("SRA",1,XFILIAL("SRA")+SUBSTR(_Texto,2,6),"RA_SITFOLH")
   IF _cDemitido=="D"
      _RETORNO:=.F.
   ENDIF
ELSEIF SUBSTR(_Texto,1,1)=="F"
   IF SUBSTR(_Texto,8,3)=="413"
      _nHoras:=ROUND(VAL(SUBSTR(_Texto,11,5)),2)
   ELSE
      _nHoras:=VAL(SUBSTR(_Texto,11,3))+ROUND(VAL(SUBSTR(_Texto,14,2))/60,2)
   ENDIF
ELSEIF SUBSTR(_Texto,1,1)=="H"
      _nHoras:=VAL(SUBSTR(_Texto,11,3))+ROUND(VAL(SUBSTR(_Texto,14,2))/60,2)
ELSE
   _RETORNO:=.F.
ENDIF

If SUBSTR(_Texto,1,1) $ ("F/H")
	Return(_nHoras)
Else
	Return(_RETORNO)
Endif

Return()