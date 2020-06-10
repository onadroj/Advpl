#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³MSD2520   º Autor ³ GATASSE            º Data ³  10/01/05   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ APOS DELETAR A NOTA FISCAL DE SAIDA                        º±±
±±º          ³ ANTES DE APAGAR SD2                                        º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP6 IDE                                                    º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User Function MSD2520

LOCAL AAREA:=GETAREA()
LOCAL AAREASB2
LOCAL AAREASD3
LOCAL _aAux:={}

dbSelectArea("SD3")
AAREASD3:=GETAREA()
dbSetOrder(4)//D3_FILIAL+D3_NUMSEQ+D3_CHAVE+D3_COD
DBSEEK(XFILIAL("SD3")+SD2->D2_NUMSEQ,.T.)
WHILE !EOF() .AND. XFILIAL("SD3")+SD2->D2_NUMSEQ==SD3->D3_FILIAL+SD3->D3_NUMSEQ
	AADD(_aAux,Array(FCount()))
	For nField := 1 To FCount()
		_aAux[len(_aAux)][nField]:=FieldGet(nField)     //SALVA OS VALORES DE CADA CAMPO
	NEXT
	RecLock("SD3",.F.)
	REPLACE D3_ESTORNO	WITH "S"
	MSUNLOCK()
	DBSKIP()
ENDDO
RESTAREA(AAREASD3)

for x:=1 to len(_aAux)
	RecLock("SD3",.T.) //GRAVA NOVO SD3
	FOR nField:=1 TO FCount()
		FieldPut(nField,_aAux[x][nField])
		if fieldname(nField)=="D3_TM"
			REPLACE D3_TM	WITH IIF(_aAux[x][nField]=="999","499","999")
		ENDIF
		if fieldname(nField)=="D3_CF"
			REPLACE D3_CF	WITH IIF(_aAux[x][nField]=="RE4","DE4","RE4")
		ENDIF
		if fieldname(nField)=="D3_CHAVE"
			REPLACE D3_CHAVE	WITH IIF(_aAux[x][nField]=="E0","E9","E0")
		ENDIF
		if fieldname(nField)=="D3_ESTORNO"
			REPLACE D3_ESTORNO	WITH "S"
		ENDIF
		if fieldname(nField)=="D3_DOCTRF"
			REPLACE D3_DOCTRF	WITH SD2->D2_DOC+SD2->D2_SERIE+SD2->D2_ITEM
		ENDIF
	NEXT
	MSUNLOCK()
	
	dbselectarea("SB2")
	AREASB2:=GETAREA()
	DBSETORDER(1)
	IF DBSEEK(XFILIAL("SB2")+SD3->D3_COD+SD3->D3_LOCAL)
		RECLOCK("SB2",.F.)
		
		REPLACE SB2->B2_QATU WITH SB2->B2_QATU+IIF(SD3->D3_CF=="DE4",SD3->D3_QUANT,SD3->D3_QUANT*-1)
		REPLACE SB2->B2_VATU1 WITH SB2->B2_VATU1+IIF(SD3->D3_CF=="DE4",SD3->D3_CUSTO1,SD3->D3_CUSTO1*-1)
		REPLACE SB2->B2_VATU2 WITH SB2->B2_VATU2+IIF(SD3->D3_CF=="DE4",SD3->D3_CUSTO2,SD3->D3_CUSTO2*-1)
		REPLACE SB2->B2_VATU3 WITH SB2->B2_VATU3+IIF(SD3->D3_CF=="DE4",SD3->D3_CUSTO3,SD3->D3_CUSTO3*-1)
		REPLACE SB2->B2_VATU4 WITH SB2->B2_VATU4+IIF(SD3->D3_CF=="DE4",SD3->D3_CUSTO4,SD3->D3_CUSTO4*-1)
		REPLACE SB2->B2_VATU5 WITH SB2->B2_VATU5+IIF(SD3->D3_CF=="DE4",SD3->D3_CUSTO5,SD3->D3_CUSTO5*-1)   
		IF SB2->B2_QATU<>0   
			REPLACE SB2->B2_CM1   WITH SB2->B2_VATU1/SB2->B2_QATU
			REPLACE SB2->B2_CM2   WITH SB2->B2_VATU2/SB2->B2_QATU
			REPLACE SB2->B2_CM3   WITH SB2->B2_VATU3/SB2->B2_QATU
			REPLACE SB2->B2_CM4   WITH SB2->B2_VATU4/SB2->B2_QATU
			REPLACE SB2->B2_CM5   WITH SB2->B2_VATU5/SB2->B2_QATU
		ENDIF
		
		MSUNLOCK()
	ENDIF
	if SB2->B2_QATU<0
		msgstop("A exclusao desta nota levou o item "+alltrim(SD3->D3_COD)+" a saldo negativo no almoxarifado "+sd3->d3_local+".")
	endif
	RESTAREA(AREASB2)
next
RESTAREA(AAREA)
Return