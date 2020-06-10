#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³VALSITOS  º Autor ³ GATASSE            º Data ³  21/02/05   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ VALIDA A SITUACAO DA OS.                                   º±±
±±º          ³ VERIFICA SE JA EXISTE OS ABERTA PARA O MESMO EQUIPAMENTO,  º±±
±±º          ³ SERVICO E FREQUENCIA.                                      º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP6 IDE                                                    º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User Function VALSITOS
LOCAL AAREA:=GETAREA()
LOCAL RET:=.T.               
LOCAL ACHOU:=.F.
LOCAL cOS:=""
dbSelectArea("STJ")
dbSetOrder(2)    
DBSEEK(XFILIAL("STJ")+"B"+STF->TF_CODBEM+STF->TF_SERVICO+STF->TF_SEQRELA,.T.)
WHILE !EOF() .AND. XFILIAL("STJ")+"B"+STF->TF_CODBEM+STF->TF_SERVICO+STF->TF_SEQRELA==STJ->TJ_FILIAL+"B"+STJ->TJ_CODBEM+STJ->TJ_SERVICO+STJ->TJ_SEQRELA
	IF STJ->TJ_TERMINO=="N".AND. STJ->TJ_SITUACA<>"C"
		ACHOU:=.T.      
		cOs:=STJ->TJ_ORDEM		
	ENDIF
	dbskip()
ENDDO
IF ACHOU
	RET:=.F.
	MSGSTOP("Ja existe a OS "+cOs+" em aberto para este servico/equipamento/frequencia!")
ENDIF
RESTAREA(AAREA)
Return(RET)
