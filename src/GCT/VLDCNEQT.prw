#INCLUDE "protheus.ch"

User Function VLDCNEQT()
Local _nPosItem := aScan(aHeader,{|aItem| AllTrim(aItem[2])=="CNE_ITEM"})
Local _nPosDet := aScan(aHeader,{|aItem| AllTrim(aItem[2])=="CNE_DETALH"})
Local _aArea := GetArea()

DbSelectArea("CNB")
DbSetOrder(1) //CNB_FILIAL+CNB_CONTRA+CNB_REVISA+CNB_NUMERO+CNB_ITEM

For I:=1 to Len(aCols)
	If DbSeek(xFilial("CNB")+M->CND_CONTRA+M->CND_REVISA+M->CND_NUMERO+aCols[I][_nPosItem])
		aCols[I][_nPosDet] := CNB->CNB_DETALH
	Endif
Next

RestArea(_aArea)

Return(.T.)