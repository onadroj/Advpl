#include "rwmake.ch"
/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡ao    ³MT241EST  ³ Autor ³GATASSE                ³ Data ³05.11.04  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡Æo ³EXCLUIR STL PARA CADA ESTORNO                               ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Modulo    ³ESTOQUE E COMPRAS                                           ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/

User Function MT241EST()
LOCAL AAREA:=GETAREA()    
IF !("OS" $ SD3->D3_OP)
  return
ENDIF

IF substr(SD3->D3_CF,1,1)<>"D"
	cOrdem:=substr(SD3->D3_OP,1,6) 
	dbselectarea("STJ")
	dbsetorder(1)
	dbseek(xfilial("STJ")+cOrdem,.t.) 
	cPlano:=STJ->TJ_PLANO
	dbselectarea("STL")       
	dbsetorder(3)
	cSeq:=0
	dbseek(xfilial("STL")+cOrdem+cPlano,.t.)
	WHILE !EOF() .AND. xfilial("STL")+cOrdem+cPlano==STL->TL_FILIAL+STL->TL_ORDEM+STL->TL_PLANO
//		cSeq:=STL->TL_SEQUENC
		cSeq++
		DBSKIP()
	ENDDO
	cSeq++
	
	RECLOCK("STL",.T.)
	REPLACE TL_FILIAL	WITH XFILIAL("STL")
	REPLACE TL_ORDEM	WITH cOrdem
	REPLACE TL_PLANO	WITH cPlano
	REPLACE TL_SEQUENC	WITH cSeq
	REPLACE TL_SEQRELA	WITH STR(cSeq,3)
	REPLACE TL_TAREFA	WITH "0"
	REPLACE TL_TIPOREG	WITH "P"
	REPLACE TL_CODIGO	WITH SD3->D3_COD
	//REPLACE TL_QUANREC	WITH 	
	REPLACE TL_QUANTID	WITH SD3->D3_QUANT
	REPLACE TL_UNIDADE	WITH SD3->D3_UM
	REPLACE TL_CUSTO	WITH SD3->D3_CUSTO1
	REPLACE TL_DESTINO	WITH "T"
	REPLACE TL_DTINICI	WITH DDATABASE
	REPLACE TL_HOINICI	WITH "08:00"
	REPLACE TL_DTFIM	WITH DDATABASE
	//REPLACE TL_HOFIM	WITH 08:00
	REPLACE TL_REPFIM	WITH "S"
	REPLACE TL_HREXTRA	with "000.00"
	REPLACE TL_NUMSEQ	WITH SD3->D3_NUMSEQ
	REPLACE TL_LOCAL	WITH SD3->D3_LOCAL
	REPLACE TL_GARANTI	with "N"
	REPLACE TL_DTDIGIT  WITH SD3->D3_EMISSAO
	/*
	REPLACE TL_LOTECTL	WITH 
	REPLACE TL_NUMLOTE	WITH 
	REPLACE TL_DTVALID	
	REPLACE TL_LOCALIZ	
	REPLACE TL_NUMSERI	
	REPLACE TL_ETAPA	
	REPLACE TL_LOCAPLI	
	REPLACE TL_NUMSC	
	REPLACE TL_ITEMSC	
	REPLACE TL_NUMOP	
	REPLACE TL_ITEMOP	
	REPLACE TL_SEQUEOP	
	REPLACE TL_OBSERVA	
	REPLACE TL_MOTIVO	
	REPLACE TL_POSCONT	
	*/
	MSUNLOCK()
	DBSELECTAREA("STJ")
	DBSETORDER(1)
	DBSEEK(XFILIAL("STJ")+STL->TL_ORDEM+STL->TL_PLANO,.T.)
	IF !EOF() .AND. XFILIAL("STJ")+STL->TL_ORDEM+STL->TL_PLANO==STJ->TJ_FILIAL+STJ->TJ_ORDEM+STJ->TJ_PLANO
		RECLOCK("STJ",.F.)
		REPLACE STJ->TJ_TIPORET WITH "S"
		MSUNLOCK()
	ENDIF
ELSE
	dbselectarea("STL")
	dbsetorder(7)//TL_FILIAL+TL_NUMSEQ
	IF dbseek(xfilial("STL")+SD3->D3_NUMSEQ)
		cOrdem:=STL->TL_ORDEM
		RECLOCK("STL",.F.)
		DELETE
		MSUNLOCK()
		IF !TEMINSUMO(cOrdem)
			DBSELECTAREA("STJ")
			DBSETORDER(1)
			DBSEEK(XFILIAL("STJ")+cOrdem,.T.)
			IF !EOF() .AND. XFILIAL("STJ")+cOrdem==STJ->TJ_FILIAL+STJ->TJ_ORDEM
				RECLOCK("STJ",.F.)
				REPLACE STJ->TJ_TIPORET WITH " "
				MSUNLOCK()
			ENDIF
		ENDIF
	endif
endif
RESTAREA(AAREA)
Return

STATIC FUNCTION TEMINSUMO(cOrdem)
LOCAL RET:=.F.
dbselectarea("STL")
dbsetorder(1)
dbseek(xfilial("STL")+cOrdem,.t.)
while !EOF() .and. xfilial("STL")+cOrdem==STL->TL_FILIAL+STL->TL_ORDEM
	IF STL->TL_SEQUENC<>0
		RET:=.T.
	ENDIF
	DBSKIP()
ENDDO
RETURN(RET)