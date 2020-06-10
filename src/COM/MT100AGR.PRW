#include "rwmake.ch"        
User Function MT100AGR()        
Local _aArea := GetArea()
PUBLIC _lMT100AGR := .T.

/*	RecLock("SF1",.f.)
	replace SF1->F1_CCUNID with SD1->D1_CCUNID
	MSUnlock()

Chamada da impressão das Ordens de Pagamento.
*/

If INCLUI .AND. (cEmpAnt=="03" .OR. cEmpAnt=="04" .OR. cEmpAnt=="99")
	If !Type("_lImpOP")=="U" 
		If _lImpOP
			DbSelectArea("SE2")
			DbSetOrder(6) //E2_FILIAL+E2_FORNECE+E2_LOJA+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO
			DbSeek(xFilial("SE2")+SF1->F1_FORNECE+SF1->F1_LOJA+SF1->F1_SERIE+SF1->F1_DOC,.T.)
			While !Eof() .AND. E2_FILIAL+E2_FORNECE+E2_LOJA+E2_PREFIXO+E2_NUM==xFilial("SE2")+SF1->F1_FORNECE+SF1->F1_LOJA+SF1->F1_SERIE+SF1->F1_DOC
				If SUBSTR(SE2->E2_TIPO,3,1) <> "-"
					ExecBlock("ORPAGTO",.F.,.F.)
				Endif
				DbSkip()
			End
		Endif
    Endif
Endif

RestArea(_aArea)    
_lMT100AGR:=nil
_lImpOP:=nil


Return

