#INCLUDE "rwmake.ch"
#INCLUDE "topconn.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณRELCONJ   บ Autor ณ EDSON              บ Data ณ  31/07/06   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDescricao ณ LISTAGEM DO CADASTRO DE EQUIPAMENTOS E SUB-CONJUNTOS       บฑฑ
ฑฑบ          ณ                                                            บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ AP6 IDE                                                    บฑฑ
ฑฑศออออออออออฯออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
/*/

User Function RELCONJ()


//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณ Declaracao de Variaveis                                             ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

Local cDesc1         := "Este programa tem como objetivo imprimir relatorio "
Local cDesc2         := "de acordo com os parametros informados pelo usuario."
Local cDesc3         := "Equipamentos e sub-conjuntos"
Local cPict          := ""
Local titulo       := "Equipamentos e Sub-conjuntos"
Local nLin         := 80

//Local Cabec1       := "Bem Pai                                                     Modelo                Compra    Serie                 Fabricante                                 Acumulado"
Local Cabec1       := "Bem Pai                                                                                                                                                                        Ultima Reforma"    
Local Cabec2       := "          Sub-Conjunto                                                Modelo                Serie                 Fabricante                                Acumulado          O.S.      Data           Eqto. Anterior"
Local imprime      := .T.
Local aOrd := {"por equipamento"}
Private lEnd         := .F.
Private lAbortPrint  := .F.
Private CbTxt        := ""
Private limite           := 132
Private tamanho          := "G"
Private nomeprog         := "RELCONJ" // Coloque aqui o nome do programa para impressao no cabecalho
Private nTipo            := 18
Private aReturn          := { "Zebrado", 1, "Administracao", 2, 2, 1, "", 1}
Private nLastKey        := 0
Private cPerg       := "U_RELC"
Private cbtxt      := Space(10)
Private cbcont     := 00
Private CONTFL     := 01
Private m_pag      := 01
Private wnrel      := "RELCONJ" // Coloque aqui o nome do arquivo usado para impressao em disco

Private cString := "STC"

dbSelectArea("STC")
dbSetOrder(1)

VALIDPERG()
pergunte(cPerg,.T.)

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

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณ Processamento. RPTSTATUS monta janela com a regua de processamento. ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
RptStatus({|| RunReport(Cabec1,Cabec2,Titulo,nLin) },Titulo)
Return

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบFuno    ณRUNREPORT บ Autor ณ AP6 IDE            บ Data ณ  03/05/04   บฑฑ
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

Local cQuery

cQuery:="SELECT T9_FILIAL, T9_CODBEM, T9_NOME, T9_CCUSTO, T9_CODFAMI, T9_MODELO, T9_SERIE, T9_CONTACU, T9_DTCOMPR, T9_FABRICA FROM "+RETSQLNAME("ST9")+" " 
cQuery+="WHERE D_E_L_E_T_<>'*' "
cQuery+="AND T9_CCUSTO >= '"+mv_par01+"' AND T9_CCUSTO <= '"+mv_par02+"' "
cQuery+="AND T9_CODFAMI >= '"+mv_par03+"' AND T9_CODFAMI <= '"+mv_par04+"' "
cQuery+="AND T9_CODBEM >= '"+mv_par05+"' AND T9_CODBEM <= '"+mv_par06+"' "
cQuery+="AND T9_TEMCONT='S' AND T9_SITBEM='A' "
cQuery+="ORDER BY T9_CODBEM"

TCQUERY cQuery NEW ALIAS "QRY"

dbSelectArea("QRY")
DbGoTop()
COUNT TO _RECCOUNT
DbGoTop()
SetRegua(_RECCOUNT)

While !EOF() 
	
	If lAbortPrint
		@nLin,00 PSAY "*** CANCELADO PELO OPERADOR ***"
		Exit
	Endif

	If nLin > 50 // Salto de Pแgina. Neste caso o formulario tem 55 linhas...
		Cabec(Titulo,Cabec1,Cabec2,NomeProg,Tamanho,nTipo)
		nLin := 9
	Endif   

	@nLin,00 PSAY ALLTRIM(QRY->T9_CODBEM)+" - "+QRY->T9_NOME
	@nLin,70 PSAY QRY->T9_MODELO
	@nLin,92 PSAY QRY->T9_SERIE
   	@nLin,114 PSAY retfield("ST7",1,XFILIAL("ST7")+QRY->T9_FABRICA,"T7_NOME")
	@nLin,154 PSAY QRY->T9_CONTACU picture("@E 999,999,999")   
	nLin := nLin + 2 // Avanca a linha de impressao do cabecalho

	aAreaX:=GetArea()
	dbSelectArea("STC")
	dbSetOrder(1)
	dbSeek(xFilial("STC")+QRY->T9_CODBEM,.T.)

    While !EOF() .AND. xFilial("STC")==QRY->T9_FILIAL .AND. STC->TC_CODBEM==QRY->T9_CODBEM

		aArea:=GetArea()
		dbSelectArea("ST9")
		dbSetOrder(1)
		dbSeek(xFilial("ST9")+STC->TC_COMPONE)
	
		If ST9->T9_SITBEM <> "A"
			restArea(aArea)
			dbSkip()
			Loop
		Endif
	
		//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
		//ณ Impressao do cabecalho do relatorio. . .                            ณ
		//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

		If nLin > 50 // Salto de Pแgina. Neste caso o formulario tem 55 linhas...
			Cabec(Titulo,Cabec1,Cabec2,NomeProg,Tamanho,nTipo)
			nLin := 9
		Endif
		@nLin,10 PSAY ALLTRIM(ST9->T9_CODBEM)+" - "+ST9->T9_NOME
		@nLin,70 PSAY ST9->T9_MODELO
		@nLin,92 PSAY ST9->T9_SERIE
	   	@nLin,114 PSAY retfield("ST7",1,XFILIAL("ST7")+ST9->T9_FABRICA,"T7_NOME")
		@nLin,154 PSAY ST9->T9_CONTACU picture("@E 999,999,999")   
	   
		//INICIO DO GETAREA OS
		aAreaZ:=GetArea()     
    	DbSelectArea("STJ")
	    DbSetOrder(15)
	    DbSeek(XFILIAL("ST9")+"B"+ST9->T9_CODBEM,.T.)	
	    sDATA:="19000101"
	    sOS:=""
	    While !EOF() .AND. ST9->T9_CODBEM == STJ->TJ_CODBEM
			if DTOS(STJ->TJ_DTORIGI) > sDATA
				sDATA:=DTOS(STJ->TJ_DTORIGI)
				sOS:=STJ->TJ_ORDEM
			endif
			DbSkip()
	    EndDO 
	    if sDATA == "19000101"
	    	sDATA:=""
	    endif    	    
	    @nLin,175 PSAY ALLTRIM(sOS)  
	    if sDATA != ""
		    @nLin,185 PSAY SUBSTR(sDATA,7,2)+"/"+SUBSTR(sDATA,5,2)+"/"+SUBSTR(sDATA,3,2)
	    endif
		RestArea(aAreaZ)

	    aAreaJ:=GetArea()
    	DbSelectArea("STZ")
	    DbSetOrder(1)
	    DbSeek(XFILIAL("STZ")+ST9->T9_CODBEM+"S")
	    sDATABEM := "19000101"
	    sBEM :=""
	    while !EOF() .AND. STZ->TZ_CODBEM == ST9->T9_CODBEM
	    	IF DTOS(STZ->TZ_DATAMOV) > sDATABEM
	    		sDATABEM := DTOS(STZ->TZ_DATAMOV)
	    		sBEM := STZ->TZ_BEMPAI
	    	ENDIF                     
	    	DBSKIP()
	    endDo          
		@nLin,200 PSAY sBEM
		RestArea(aAreaJ)

		nLin := nLin + 1 // Avanca a linha de impressao
		restArea(aArea)
		dbSkip() // Avanca o ponteiro do registro no arquivo
    EndDo
	nLin := nLin + 2 // Avanca a linha de impressao
	restArea(aAreaX)
	dbSkip() //Avanca o ponteiro do registro no arquivo
EndDo
CLOSE
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



Static Function ValidPerg

Local _sAlias := Alias()
Local aRegs := {}
Local i,j

dbSelectArea("SX1")
dbSetOrder(1)
cPerg := PADR(cPerg,10)

// Grupo/Ordem/Pergunta/Variavel/Tipo/Tamanho/Decimal/Presel/GSC/Valid/Var01/Def01/Cnt01/Var02/Def02/Cnt02/Var03/Def03/Cnt03/Var04/Def04/Cnt04/Var05/Def05/Cnt05
aAdd(aRegs,{cPerg,"01","De  Centro de Custo  ?","","","mv_ch1","C",9,0,0,"G","","mv_par01","","","","","","","","","","","","","","","","","","","","","","","","","CTT","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"02","Ate Centro de Custo  ?","","","mv_ch2","C",9,0,0,"G","","mv_par02","","","","","","","","","","","","","","","","","","","","","","","","","CTT","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"03","De  Familia          ?","","","mv_ch3","C",6,0,0,"G","","mv_par03","","","","","","","","","","","","","","","","","","","","","","","","","ST6","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"04","Ate Familia          ?","","","mv_ch4","C",6,0,0,"G","","mv_par04","","","","","","","","","","","","","","","","","","","","","","","","","ST6","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"05","De  Bem              ?","","","mv_ch5","C",16,0,0,"G","","mv_par05","","","","","","","","","","","","","","","","","","","","","","","","","ST9","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"06","Ate Bem              ?","","","mv_ch6","C",16,0,0,"G","","mv_par06","","","","","","","","","","","","","","","","","","","","","","","","","ST9","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
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
