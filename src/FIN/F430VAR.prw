#include "rwmake.ch"
#include "topconn.ch"
/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณF430VAR   บ Autor ณ EDSON              บ Data ณ  18/07/12   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDescricao ณ PONTO DE ENTRADA PARA TRATAR RETORNO CNAB BRADESCO,CONTENDOบฑฑ
ฑฑบ          ณ BAIXAS DE MAIS DE UMA CONTA NO MESMO ARQUIVO               บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ FINANCEIRO                                                 บฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
/*/

User Function F430VAR
Local _xBuffer := PARAMIXB[1][16]
Local _IdCnab := AllTrim(PARAMIXB[1][1])
Local _aArea := GetArea()
Local _cQuery
Local _cBordero

If cEmpAnt#"03" .OR. Len(_xBuffer)#502 .OR. Substr(_xBuffer,1,1)#"1"
   Return()
Endif

//Localiza tํtulo em SE2 para ler n๚mero do border๔ de envio
_cQuery:="SELECT * FROM "+RETSQLNAME("SE2")+" WHERE "
_cQuery+="E2_IDCNAB = '" + _IdCnab + "' "
_cQuery+="AND D_E_L_E_T_ <> '*'"

TCQUERY _cQuery NEW ALIAS "QRY"

dbselectarea("QRY")
DbGoTop()
If !EOF()
	_cBordero := QRY->E2_NUMBOR
Endif

CLOSE

//Resgata banco, ag๊ncia e conta do border๔ de envio para baixa

_cQuery:="SELECT * FROM "+RETSQLNAME("SEA")+" WHERE "
_cQuery+="EA_NUMBOR = '" + CVALTOCHAR(_cBordero) + "' "
_cQuery+="AND D_E_L_E_T_ <> '*'"

TCQUERY _cQuery NEW ALIAS "QRY"

dbselectarea("QRY")
DbGoTop()
If !EOF()
	cbanco   := QRY->EA_PORTADO
	cagencia := QRY->EA_AGEDEP
	cconta   := QRY->EA_NUMCON
Endif

CLOSE

RestArea(_aArea)
RETURN()