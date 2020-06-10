#INCLUDE "rwmake.ch"

/*/
‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±…ÕÕÕÕÕÕÕÕÕÕ—ÕÕÕÕÕÕÕÕÕÕÀÕÕÕÕÕÕÕ—ÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÀÕÕÕÕÕÕ—ÕÕÕÕÕÕÕÕÕÕÕÕÕª±±
±±∫Programa  ≥ATUAREF1  ∫ Autor ≥ GATASSE            ∫ Data ≥  29/11/04   ∫±±
±±ÃÕÕÕÕÕÕÕÕÕÕÿÕÕÕÕÕÕÕÕÕÕ ÕÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕ ÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕÕÕÕÕπ±±
±±∫Descricao ≥ GRAVA REFORMAS E ZERA CONTADOR ACUMULADO PARA BENS FILHOS  ∫±±
±±∫          ≥ INCLUSAO DO REGISTRO E ALTERACAO DAS DEMAIS TABELAS        ∫±±
±±ÃÕÕÕÕÕÕÕÕÕÕÿÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕπ±±
±±∫Uso       ≥ AP6 IDE                                                    ∫±±
±±»ÕÕÕÕÕÕÕÕÕÕœÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕÕº±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ
/*/

User Function ATUAREF1
LOCAL AAREA
//_result:=Axinclui("SZQ",0,3,,,,'U_ValSZQ')
_result:=Axinclui("SZQ",0,3)
if _result==1
    AAREA:=GETAREA()
	//   alert("confirmado")
	//ZERA CONTADORES EM ST9
	DBSELECTAREA("ST9")
	RECLOCK("ST9",.F.)
	REPLACE ST9->T9_CONTACU WITH 0
	IF ST9->T9_ESTRUTU<>"S"	 
		REPLACE ST9->T9_POSCONT WITH 0
	ENDIF
	REPLACE T9_DTULTAC WITH SZQ->ZQ_DATA
	MSUNLOCK()
	//ZERA CONTADORES EM STF
	DBSELECTAREA("STF")
	DBSETORDER(1)
	DBSEEK(XFILIAL("STF")+ST9->T9_CODBEM,.T.)
	WHILE !EOF() .AND. XFILIAL("STF")+ST9->T9_CODBEM==STF->TF_FILIAL+STF->TF_CODBEM
		RECLOCK("STF",.F.)
		REPLACE STF->TF_DTULTMA WITH SZQ->ZQ_DATA
		REPLACE STF->TF_CONMANU WITH 0
		MSUNLOCK()
		DBSKIP()
	ENDDO
	RESTAREA(AAREA)	
else
	//   alert("n∆o confirmado")
endif
Return
