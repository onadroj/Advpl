#INCLUDE "rwmake.ch"
#include "topconn.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณF590INC   บ Autor ณ AP6 IDE            บ Data ณ  10/03/10   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDescricao ณ apos incluir um titulo ao bordero                          บฑฑ
ฑฑบ          ณ                                                            บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ AP6 IDE                                                    บฑฑ
ฑฑศออออออออออฯออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
/*/

User Function F590INC
Local _aDados := {}
Local aArea:=GetArea()
Local nSld:=0
Local cRecPag:=PARAMIXB[1]
Local cNumBor:=PARAMIXB[2]
Local cQuery:=""
if cRecPag=="R"
	cQuery:=" SELECT SUM(E1_SALDO + E1_ACRESC - E1_DECRESC -E1_INSS - E1_ISS - E1_IRRF) AS SLD "
	cQuery+=" FROM "+RetSQLName("SEA") +" SEA, "+RetSQLName("SE1") +" SE1 "
	cQuery+=" WHERE "
	cQuery+=" SEA.D_E_L_E_T_<>'*' AND "
	cQuery+=" SE1.D_E_L_E_T_<>'*' AND "
	cQuery+=" SEA.EA_TIPO    =  SE1.E1_TIPO AND "
	cQuery+=" SEA.EA_PARCELA =  SE1.E1_PARCELA AND "
	cQuery+=" SEA.EA_NUM     =  SE1.E1_NUM AND "
	cQuery+=" SEA.EA_PREFIXO =  SE1.E1_PREFIXO  AND "
	cQuery+=" SEA.EA_FILIAL  =  SE1.E1_FILIAL AND "
	cQuery+=" SEA.EA_CART    = '"+cRecPag+"' AND "
	cQuery+=" SEA.EA_NUMBOR  ='"+cNumBor+"' "
else
	cQuery:=" SELECT SUM(E2_SALDO +E2_ACRESC - E2_DECRESC) AS SLD "
	cQuery+=" FROM "+RetSQLName("SEA") +" SEA, "+RetSQLName("SE2") +" SE2 "
	cQuery+=" WHERE "
	cQuery+=" SEA.D_E_L_E_T_<>'*' AND "
	cQuery+=" SE2.D_E_L_E_T_<>'*' AND "
	cQuery+=" SEA.EA_LOJA    =  SE2.E2_LOJA AND "
	cQuery+=" SEA.EA_FORNECE =  SE2.E2_FORNECE  AND "
	cQuery+=" SEA.EA_TIPO    =  SE2.E2_TIPO AND "
	cQuery+=" SEA.EA_PARCELA =  SE2.E2_PARCELA AND "
	cQuery+=" SEA.EA_NUM     =  SE2.E2_NUM AND "
	cQuery+=" SEA.EA_PREFIXO =  SE2.E2_PREFIXO  AND "
	cQuery+=" SEA.EA_FILIAL  =  SE2.E2_FILIAL AND "
	cQuery+=" SEA.EA_CART    = '"+cRecPag+"' AND "
	cQuery+=" SEA.EA_NUMBOR  ='"+cNumBor+"' "
endif
TCQUERY cQuery ALIAS QRY NEW
dbSelectArea("QRY")
DbGoTop()
if !eof()
	nSld:=QRY->SLD
endif
CLOSE
dbSelectArea("SZJ")
dbsetorder(1)
if dbseek(xfilial("SZJ")+cRecPag+cNumBor)
	RecLock("SZJ",.F.)
	replace ZJ_VALOR with nSld
	replace ZJ_LIBERAD with "I"
	MSUnlock()
ELSE
	RecLock("SZJ",.T.)
	replace ZJ_FILIAL with xFilial("SZJ")
	replace ZJ_BORDERO with cNumBor
	replace ZJ_TIPO with cRecPag
	replace ZJ_LIBERAD with "N"
	replace ZJ_VALOR with nSld
	MSUnlock()
ENDIF
restarea(aArea)

If cEmpAnt=="03" .OR. cEmpAnt=="04" .OR. cEmpAnt=="08" .OR. cEmpAnt=="99"
	aAdd(_aDados,cNumBor)
	aAdd(_aDados,IIf(cRecPag=="P","Pagar","Receber"))
	aAdd(_aDados,AllTrim(Transform(nSld,"@E 999,999,999.99")))
	aAdd(_aDados,cUserName)
	MEnviaMail("_GB",_aDados,,,,.T.)
EndIf

Return
