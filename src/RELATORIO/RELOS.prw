#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณRELOS     บ Autor ณ GATASSE            บ Data ณ  24/05/04   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDescricao ณ EMISSAO DA OREDM DE SERVICO                                บฑฑ
ฑฑบ          ณ                                                            บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ AP6 IDE                                                    บฑฑ
ฑฑศออออออออออฯออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
/*/

User Function RELOS


//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณ Declaracao de Variaveis                                             ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

Local cDesc1         := "Este programa tem como objetivo imprimir relatorio "
Local cDesc2         := "de acordo com os parametros informados pelo usuario."
Local cDesc3         := "Ordem de Servico"
Local cPict          := ""
Local titulo       := "Ordem de Servico"

Local Cabec1       := ""
Local Cabec2       := ""
Local imprime      := .T.
Private aOrd             := {"Ordem","Bem","Plano+Ordem"}
Private lEnd         := .F.
Private lAbortPrint  := .F.
Private CbTxt        := ""
Private limite           := 80
Private tamanho          := "P"
Private nomeprog         := "RELOS"
Private nTipo            := 18
Private aReturn          := { "Zebrado", 1, "Administracao", 1, 2, 1, "", 1}
Private nLastKey        := 0
Private cPerg       := "URELOS"
Private cbtxt      := Space(10)
Private cbcont     := 00
Private CONTFL     := 01
Private m_pag      := 01
Private wnrel      := "RELOS"
Private nPag:=1
Private cString := "STJ"
Private nLin         := 80

dbSelectArea("STJ")
dbSetOrder(1)


ValidPerg()
pergunte(cPerg,.F.)

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณ Monta a interface padrao com o usuario...                           ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

wnrel := SetPrint(cString,NomeProg,cPerg,@titulo,cDesc1,cDesc2,cDesc3,.F.,aOrd,.F.,Tamanho,,.F.)

If nLastKey == 27
	Return
Endif

SetDefault(aReturn,cString)

If nLastKey == 27
	Return
Endif

nTipo := If(aReturn[4]==1,15,18)
nTipo:=18
//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณ Processamento. RPTSTATUS monta janela com a regua de processamento. ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

RptStatus({|| RunReport(Cabec1,Cabec2,Titulo,nLin) },Titulo)
Return

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบFuno    ณRUNREPORT บ Autor ณ AP6 IDE            บ Data ณ  24/05/04   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDescrio ณ Funcao auxiliar chamada pela RPTSTATUS. A funcao RPTSTATUS บฑฑ
ฑฑบ          ณ monta a janela com a regua de processamento.               บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ Programa principal                                         บฑฑ
ฑฑศออออออออออฯออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
/*/

Static Function RunReport(Cabec1,Cabec2,Titulo,nLin)

Local nOrdem

dbSelectArea(cString)

nOrdem := aReturn[8]
dbSetOrder(nOrdem)
SetRegua(RecCount())
//1-TJ_FILIAL+TJ_ORDEM+TJ_PLANO+TJ_CODBEM+TJ_SERVICO+STR(TJ_SEQUENC,3)
//2-TJ_FILIAL+TJ_CODBEM+TJ_SERVICO+STR(TJ_SEQUENC,3)+TJ_ORDEM+TJ_PLANO
//3-TJ_FILIAL+TJ_PLANO+TJ_ORDEM+TJ_CODBEM+TJ_SERVICO+STR(TJ_SEQUENC,3)
if nOrdem==1
	cSeek:=xfilial("STJ")+MV_PAR10
ELSEIF nOrdem==2
	cSeek:=xfilial("STJ")+"B"+MV_PAR08
ELSE
	cSeek:=xfilial("STJ")+MV_PAR01+MV_PAR10
ENDIF
dbSeek(cSeek,.T.)
While !EOF() .And. xfilial("STJ")==STJ->TJ_FILIAL
	//	iif(nOrdem==1,MV_PAR10==STJ->TJ_ORDEM,iif(nOrdem==2,MV_PAR08==STJ->TJ_CODBEM,MV_PAR01==STJ->TJ_PLANO))
	If DTOS(STJ->TJ_DTORIGI) < DTOS(mv_par02) .OR. DTOS(STJ->TJ_DTORIGI) > DTOS(mv_par03)
		dbSkip()
		Loop
	Endif
	If STJ->TJ_PLANO <> mv_par01
		dbSkip()
		Loop
	Endif
	If STJ->TJ_TERMINO =="S"
		dbSkip()
		Loop
	Endif
	If STJ->TJ_CCUSTO < mv_par04 .OR. STJ->TJ_CCUSTO > mv_par05
		dbSkip()
		Loop
	Endif
	If STJ->TJ_CENTRAB < mv_par06 .OR. STJ->TJ_CENTRAB > mv_par07
		dbSkip()
		Loop
	Endif
	If STJ->TJ_CODBEM < mv_par08 .OR. STJ->TJ_CODBEM > mv_par09
		dbSkip()
		Loop
	Endif
	If STJ->TJ_ORDEM < mv_par10 .OR. STJ->TJ_ORDEM > mv_par11
		dbSkip()
		Loop
	Endif
	
	
	If lAbortPrint
		@nLin,00 PSAY "*** CANCELADO PELO OPERADOR ***"
		Exit
	Endif
	
	If nLin > 50
		ImpCab(@nLin)
	Endif
	
	aarea:=getarea()
	entrou:=.f.
	dbselectarea("STQ")
	DBSETORDER(1)//TQ_FILIAL+TQ_ORDEM+TQ_PLANO+TQ_TAREFA+TQ_ETAPA
	DBSEEK(XFILIAL("STQ")+STJ->TJ_ORDEM+STJ->TJ_PLANO,.T.)
	WHILE !EOF() .AND. XFILIAL("STQ")+STJ->TJ_ORDEM+STJ->TJ_PLANO==;
		STQ->TQ_FILIAL+STQ->TQ_ORDEM+STJ->TJ_PLANO
		If AllTrim(STQ->TQ_TAREFA) == "" .OR. STQ->TQ_TAREFA == NIL
		   dbSkip()
		   Loop
		Endif
		entrou:=.t.
		@nLin,00 PSAY STQ->TQ_TAREFA
//		for t:=1 to mlcount(ST5->T5_DESCAUX,98)
		for t:=1 to mlcount(ST5->T5_DESC,98)
			@nLin,06 PSAY "|"
//			@nLin,07 PSAY memoline(ST5->T5_DESCAUX,98,t)
			@nLin,07 PSAY memoline(ST5->T5_DESC,98,t)
			@nLin,105 PSAY "|            |"
			nLin++
//			If nLin > 50 .and. t<>mlcount(ST5->T5_DESCAUX,98)
			If nLin > 50 .and. t<>mlcount(ST5->T5_DESC,98)
				ImpCab1(@nLin)
			Endif
		next
		@nLin,00 PSAY "------+--------------------------------------------------------------------------------------------------+------------+------------"
		nLin++
		If nLin > 50
			ImpCab1(@nLin)
		Endif
		DBSKIP()
	ENDDO
	entrou:=.f.
	dbselectarea("ST5")
	DBSETORDER(1)
	DBSEEK(XFILIAL("ST5")+STJ->TJ_CODBEM+STJ->TJ_SERVICO+STJ->TJ_SEQRELA,.T.)
	WHILE !EOF() .AND. XFILIAL("ST5")+STJ->TJ_CODBEM+STJ->TJ_SERVICO+STJ->TJ_SEQRELA==;
		ST5->T5_FILIAL+ST5->T5_CODBEM+ST5->T5_SERVICO+ST5->T5_SEQRELA
		entrou:=.t.
		@nLin,00 PSAY ST5->T5_TAREFA
//		for t:=1 to mlcount(ST5->T5_DESCAUX,98)
		for t:=1 to mlcount(ST5->T5_DESC,98)		
			@nLin,06 PSAY "|"
//			@nLin,07 PSAY memoline(ST5->T5_DESCAUX,98,t)
			@nLin,07 PSAY memoline(ST5->T5_DESC,98,t)			
			@nLin,105 PSAY "|            |"
			nLin++
//			If nLin > 50 .and. t<>mlcount(ST5->T5_DESCAUX,98)
			If nLin > 50 .and. t<>mlcount(ST5->T5_DESC,98)			
				ImpCab1(@nLin)
			Endif
		next
		@nLin,00 PSAY "------+--------------------------------------------------------------------------------------------------+------------+------------"
		nLin++
		If nLin > 50
			ImpCab1(@nLin)
		Endif
		DBSKIP()
	ENDDO
	if !entrou
		@nLin,06 PSAY "|"
		@nLin,07 PSAY " "
		@nLin,105 PSAY "|            |"
		nLin++
//		If nLin > 50 .and. t<>mlcount(ST5->T5_DESCAUX,98)
		If nLin > 50 .and. t<>mlcount(ST5->T5_DESC,98)		
			ImpCab1(@nLin)
		Endif
		@nLin,00 PSAY "------+--------------------------------------------------------------------------------------------------+------------+------------"
		nLin++
	endif
	nLin:=nLin+3
	If nLin > 50
		ImpCab1(@nLin)
	endif
	Impcab2(@nLin)
	
	AAREA1:=GETAREA()
	dbselectarea("STG")
	dbsetorder(1)
	dbseek(xfilial("STG")+STJ->TJ_CODBEM+STJ->TJ_SERVICO+STJ->TJ_SEQRELA,.T.)
	while !eof() .and. xfilial("STG")+STJ->TJ_CODBEM+STJ->TJ_SERVICO+STJ->TJ_SEQRELA==;
		STG->TG_FILIAL+STG->TG_CODBEM+STG->TG_SERVICO+STG->TG_SEQRELA
		@nLin,00 PSAY STG->TG_TAREFA
		@nLin,06 PSAY "|"
		@nLin,07 PSAY STG->TG_TIPOREG+"-"+STG->TG_CODIGO
		cDesc:="Erro no registro!"
		IF STG->TG_TIPOREG=="P"//M=Mao de Obra;E=Especialidade;P=Produto;F=Ferramenta;T=Terceiro
			cDesc:=ALLTRIM(retfield("SB1",1,XFILIAL("SB1")+STG->TG_CODIGO,"B1_DESC"))  +" "
			cDesc1:=""
			areastg:=getarea()
			dbselectarea("SZ2")
			DBSETORDER(1)//Z2_Filial + Z2_CodProd + Z2_CodFabr + Z2_CodPeca
			DBSEEK(XFILIAL("SZ2")+STG->TG_CODIGO,.T.)
			WHILE !EOF() .AND. XFILIAL("SZ2")+STG->TG_CODIGO==SZ2->Z2_Filial + SZ2->Z2_CodProd
				IF SZ2->Z2_CodFabr<>"0000"
					cDesc1+=ALLTRIM(RETFIELD("SX5",1,XFILIAL("SX5")+"99"+SZ2->Z2_CodFabr,"X5_DESCRI"))+":"+ALLTRIM(Z2_CodPeca)+ ", "
				ENDIF
				DBSKIP()
			ENDDO
			IF alltrim(cDesc1)<>""
				cDesc1:="=>Equiv.:"+substr(cDesc1,1,len(cDesc1)-1)
				cDesc:=cDesc+cDesc1
			ENDIF
			restarea(areastg)
		ELSEIF STG->TG_TIPOREG=="M"
			cDesc:=retfield("ST1",1,XFILIAL("ST1")+STG->TG_CODIGO,"T1_NOME")
		ELSEIF STG->TG_TIPOREG=="E"
			cDesc:=retfield("ST0",1,XFILIAL("ST0")+STG->TG_CODIGO,"T0_NOME")
		ELSEIF STG->TG_TIPOREG=="F"
			cDesc:=retfield("SH4",1,XFILIAL("SH4")+STG->TG_CODIGO,"H4_DESCRI")
		ELSEIF STG->TG_TIPOREG=="T"
			cDesc:=retfield("SA2",1,XFILIAL("SA2")+STG->TG_CODIGO,"A2_NOME")
		ENDIF
		@nLin,25 PSAY "|"
		@nLin,26 PSAY memoline(cDesc,70,1)
		@nLin,97 PSAY "|"
		@nLin,98 PSAY 		STG->TG_QUANTID PICTURE "9,999,999.99"
		@nLin,110 PSAY "|"
		@nLin,111 PSAY 		STG->TG_UNIDADE
		@nLin,118 PSAY "|"
		@nLin,119 PSAY 		custo(STG->TG_CODIGO) PICTURE "9,999,999.99"
		nLin++
		FOR Z:=2 TO mlcount(cDesc, 70)
			@nLin,06 PSAY "|"
			@nLin,25 PSAY "|"
			@nLin,26 PSAY memoline(cDesc,70,z)
			@nLin,97 PSAY "|"
			@nLin,110 PSAY "|"
			@nLin,118 PSAY "|"
			nLin++
		next
		@nLin,00 PSAY "------+------------------+-----------------------------------------------------------------------+------------+-------+------------"
		nLin++
		If nLin > 50
			ImpCab1(@nLin)
			ImpCab2(@nLin)
		Endif
		DBSKIP()
	ENDDO
	restarea(aarea1)
	for x:=1 to 10
		@nLin,00 PSAY "      |                  |                                                                       |            |       |            "
		nLin++
		@nLin,00 PSAY "------+------------------+-----------------------------------------------------------------------+------------+-------+------------"
		nLin++
		If nLin > 50
			ImpCab1(@nLin)
			ImpCab2(@nLin)
		Endif
	next
	restarea(aarea)
	nLin++
	If nLin > 50
		ImpCab1(@nLin)
	EndIf
	@nLin,00 PSAY "Observacao:      Atencao!!!!! Apos execucao, preencher os campos em branco da ordem de servico e o Check-List,"
	nLin++
	If nLin > 50
		ImpCab1(@nLin)
	EndIf
	@nLin,00 PSAY "                 datar e assinar o impresso de Planejamento. Enviar para o Escritorio Central."
	nLin++
	If nLin > 50
		ImpCab1(@nLin)
	EndIf
	if alltrim(mv_par12)<>""
		@nLin,00 PSAY mv_par12
		nLin++
		If nLin > 50
			ImpCab1(@nLin)
		EndIf
	EndIf
	nLin:=80
	dbSkip() // Avanca o ponteiro do registro no arquivo
EndDo

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณ Finaliza a execucao do relatorio...                                 ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

SET DEVICE TO SCREEN

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณ Se impressao em disco, chama o gerenciador de impressao...          ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

If aReturn[5]==1
	dbCommitAll()
	SET PRINTER TO
	OurSpool(wnrel)
Endif

MS_FLUSH()

Return
Static Function ImpCab(nLin)
nLin:=1
@nLin,00 PSAY "ORDEM DE SERVICO"
@nLin,120 PSAY "Pag.: "+str(nPag,5)
nPag++
nLin++
nLin++
@nLin,00 PSAY "Empresa: "+SM0->M0_NOME
IF STJ->TJ_SITUACA=="C"
	cSt:="Cancelado"
elseif STJ->TJ_SITUACA=="L"
	cSt:="Liberado"
else
	cSt:="Pendente"
endif
@nLin,66 PSAY "Status: "+cSt
nLin++
@nLin,00 PSAY "Numero : "+STJ->TJ_ORDEM
@nLin,66 PSAY "Plano: "+STJ->TJ_PLANO+"-"+RETFIELD("STI",1,XFILIAL("STI")+STJ->TJ_PLANO,'TI_DESCRIC')
nLin++
@nLin,00 PSAY "Data Emissao: "+DTOC(STJ->TJ_DTORIGI)
@nLin,66 PSAY "Tipo do Plano: "+STJ->TJ_TIPO+"-"+RETFIELD("STE",1,XFILIAL("STE")+STJ->TJ_TIPO,'TE_NOME')
nLin++
@nLin,00 PSAY "Servico: "+"-"+RETFIELD("ST4",1,XFILIAL("ST4")+STJ->TJ_SERVICO,'T4_NOME')
cCodArea:=alltrim(RETFIELD("ST4",1,XFILIAL("ST4")+STJ->TJ_SERVICO,'T4_CODAREA'))
cDescArea:=alltrim(RETFIELD("STD",1,XFILIAL("STD")+cCodArea,'TD_NOME'))
cTipoMan:=alltrim(RETFIELD("ST4",1,XFILIAL("ST4")+STJ->TJ_SERVICO,'T4_TIPOMAN'))
@nLin,66 PSAY "Area: "+cCodArea+"-"+cDescArea+"-"+cTipoMan
nLin++
@nLin,00 PSAY "C.Trabalho: "+STJ->TJ_CENTRAB+"-"+SUBSTR(RETFIELD("SHB",1,+XFILIAL("SHB")+STJ->TJ_CODBEM ,"HB_NOME"),1,20)
@nLin,66 PSAY "Frequencia: "+STRZERO(STJ->TJ_SEQUENC,3)+" - "+str(RETFIELD("SZP",1,+XFILIAL("SZP")+STRZERO(STJ->TJ_SEQUENC,3),"ZP_INMANUT"),6)
nLin++
@nLin,00 PSAY "Equipamento: "+ALLTRIM(STJ->TJ_CODBEM)+" - "+RETFIELD("ST9",1,+XFILIAL("ST9")+STJ->TJ_CODBEM,"T9_NOME")
nLin++
nLin++
@nLin,00 PSAY "Dados do Equipamento: "
nLin++
nAcum:=RETFIELD("ST9",1,+XFILIAL("ST9")+STJ->TJ_CODBEM,"T9_CONTACU")
@nLin,00 PSAY "Acumulado  Atual: "
@nLin,18 PSAY  nAcum picture("@E 999,999,999")
@nLin,66 PSAY "Modelo: "+RETFIELD("ST9",1,+XFILIAL("ST9")+STJ->TJ_CODBEM,"T9_MODELO")
nLin++
nPos:=RETFIELD("ST9",1,+XFILIAL("ST9")+STJ->TJ_CODBEM,"T9_POSCONT")
@nLin,00 PSAY "Leitura    Atual: "
@nLin,18 PSAY  nPos picture("@E 999,999,999")
@nLin,66 PSAY "Serie : "+RETFIELD("ST9",1,+XFILIAL("ST9")+STJ->TJ_CODBEM,"T9_SERIE")
nLin++
@nLin,00 PSAY "Data da  Leitura: "+DTOC(RETFIELD("ST9",1,+XFILIAL("ST9")+STJ->TJ_CODBEM,"T9_DTULTAC"))
nLin++
@nLin,00 PSAY "---- Leitura Execucao -----+------ Data Inicio ------+----- Hora Inicio -------+----- Data Termino ------+----- Hora Termino ------"
nLin++
@nLin,00 PSAY "                           |                         |                         |                         |                         "
nLin++
@nLin,00 PSAY "---------------------------+-------------------------+-------------------------+-------------------------+-------------------------"
nLin++
if alltrim(STJ->TJ_OBSERVA)<>""
	For xx:=1 to MlCount(STJ->TJ_OBSERVA,132)
		@nLin,00 PSAY	MemoLine(STJ->TJ_OBSERVA,132,xx)
		nLin++
	next
endif
nLin++
@nLin,00 PSAY "--------------------------------------------------- Tarefas  a serem realizadas ---------------------------------------------------"
nLin++
@nLin,00 PSAY "Tarefa|            Descricao                                                                             |Executor    |Data    "
nLin++
@nLin,00 PSAY "------+--------------------------------------------------------------------------------------------------+------------+------------"
nLin++
return


//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ,ฟ
//ณImprime cabecalho de continuacaoณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ,ู

Static Function ImpCab1(nLin)
nLin:=1
@nLin,00 PSAY "ORDEM DE SERVICO - CONTINUACAO"
@nLin,120 PSAY "Pag.:"+str(nPag,4)
nLin++
nLin++
@nLin,00 PSAY "Empresa: "+SM0->M0_NOME
nLin++
@nLin,00 PSAY "Numero : "+STJ->TJ_ORDEM
nLin++
@nLin,00 PSAY "Equipamento: "+STJ->TJ_CODBEM+" - "+RETFIELD("ST9",1,+XFILIAL("ST9")+STJ->TJ_CODBEM,"T9_NOME")
nLin++
nLin++
return


Static Function ImpCab2(nLin)
@nLin,00 PSAY "--------------------------------------------------- Materiais e pecas aplicados ---------------------------------------------------"
nLin++
@nLin,00 PSAY "Tarefa|TP-Codigo         |Descricao                                                              |Quant.      |UM     |Vlr.Unitario"
nLin++
@nLin,00 PSAY "------+------------------+-----------------------------------------------------------------------+------------+-------+------------"
nLin++
return

Static Function custo(COD)
local ret:=0
local area:=getarea()
dbselectarea("SB2")
DBSETORDER(1)
dbseek(xfilial("SB2")+cod,.t.)
nquant:=0
nval:=0
while !eof() .and. xfilial("SB2")+cod==SB2->B2_FILIAL+SB2->B2_COD
	nquant+=SB2->B2_QATU
	nval+=SB2->B2_VATU1
	DBSKIP()
enddo     
RET:=nval/nquant
restarea(area)
return(ret)

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบFuno    ณVALIDPERG บ Autor ณ AP5 IDE            บ Data ณ  17/12/02   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDescrio ณ Verifica a existencia das perguntas criando-as caso seja   บฑฑ
ฑฑบ          ณ necessario (caso nao existam).                             บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ Programa principal                                         บฑฑ
ฑฑศออออออออออฯออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
/*/

Static Function ValidPerg

Local _sAlias := Alias()
Local aRegs := {}
Local i,j

dbSelectArea("SX1")
dbSetOrder(1)
cPerg := PADR(cPerg,10)

// Grupo/Ordem/Pergunta/Variavel/Tipo/Tamanho/Decimal/Presel/GSC/Valid/Var01/Def01/Cnt01/Var02/Def02/Cnt02/Var03/Def03/Cnt03/Var04/Def04/Cnt04/Var05/Def05/Cnt05
aAdd(aRegs,{cPerg,"01","Do Plano             ?","","","mv_ch1","C",6,0,0,"G","","mv_par01","","","","","","","","","","","","","","","","","","","","","","","","","STI",""})
aAdd(aRegs,{cPerg,"02","Da Data              ?","","","mv_ch2","D",8,0,0,"G","","mv_par02","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"03","Ate a Data           ?","","","mv_ch3","D",8,0,0,"G","","mv_par03","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"04","Do Centro de Custo   ?","","","mv_ch4","C",9,0,0,"G","","mv_par04","","","","","","","","","","","","","","","","","","","","","","","","","CTT",""})
aAdd(aRegs,{cPerg,"05","Ate o Centro de Custo?","","","mv_ch5","C",9,0,0,"G","","mv_par05","","","","","","","","","","","","","","","","","","","","","","","","","CTT",""})
aAdd(aRegs,{cPerg,"06","Do Centro Trabalho   ?","","","mv_ch6","C",6,0,0,"G","","mv_par06","","","","","","","","","","","","","","","","","","","","","","","","","SHB",""})
aAdd(aRegs,{cPerg,"07","Ate Centro Trabalho  ?","","","mv_ch7","C",6,0,0,"G","","mv_par07","","","","","","","","","","","","","","","","","","","","","","","","","SHB",""})
aAdd(aRegs,{cPerg,"08","Do Bem               ?","","","mv_ch8","C",16,0,0,"G","","mv_par08","","","","","","","","","","","","","","","","","","","","","","","","","ST9",""})
aAdd(aRegs,{cPerg,"09","Ate o Bem            ?","","","mv_ch9","C",16,0,0,"G","","mv_par09","","","","","","","","","","","","","","","","","","","","","","","","","ST9",""})
aAdd(aRegs,{cPerg,"10","Da Ordem             ?","","","mv_cha","C",6,0,0,"G","","mv_par10","","","","","","","","","","","","","","","","","","","","","","","","","STJ",""})
aAdd(aRegs,{cPerg,"11","Ate a Ordem          ?","","","mv_chb","C",6,0,0,"G","","mv_par11","","","","","","","","","","","","","","","","","","","","","","","","","STJ",""})
aAdd(aRegs,{cPerg,"12","Observacao           ?","","","mv_chc","C",70,0,0,"G","","mv_par12","","","","","","","","","","","","","","","","","","","","","","","","","",""})
For i:=1 to Len(aRegs)
	If !dbSeek(cPerg+aRegs[i,2])
		RecLock("SX1",.T.)
		For j:=1 to FCount()
			If j <= Len(aRegs[i])
				FieldPut(j,aRegs[i,j])
			Endif
		Next
		MsUnlock()
	Endif
Next

dbSelectArea(_sAlias)

Return
