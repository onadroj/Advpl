#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณRELMAPA   บ Autor ณ GATASSE            บ Data ณ  03/05/04   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDescricao ณ RELATORIO DA SITUACAO DE MANUTENCOES POR EQUIPAMENTO       บฑฑ
ฑฑบ          ณ                                                            บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ AP6 IDE                                                    บฑฑ
ฑฑศออออออออออฯออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
/*/

User Function RELMAPA


//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณ Declaracao de Variaveis                                             ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

Local cDesc1         := "Este programa tem como objetivo imprimir relatorio "
Local cDesc2         := "de acordo com os parametros informados pelo usuario."
Local cDesc3         := "Manutencoes do Bem"
Local cPict          := ""
Local titulo       := "SITUACAO DAS MANUTENCOES"
Local nLin         := 80

Local Cabec1       := "EQUIPAMENTO                                                     DATA ULTIMA   ACUMULADO     PROXIMA                       OS EM    "
Local Cabec2       := "     SERVIC  DESCRICAO                                 SEQ.     MANUTENCAO    ULTIMA MANUT  MANUTENCAO  SITUACAO   HORAS  EXECUCAO "
Local imprime      := .T.
Local aOrd := {}
Private lEnd         := .F.
Private lAbortPrint  := .F.
Private CbTxt        := ""
Private limite           := 132
Private tamanho          := "M"
Private nomeprog         := "RELMAPA" // Coloque aqui o nome do programa para impressao no cabecalho
Private nTipo            := 18
Private aReturn          := { "Zebrado", 1, "Administracao", 2, 2, 1, "", 1}
Private nLastKey        := 0
Private cPerg       := "U_RELM"
Private cbtxt      := Space(10)
Private cbcont     := 00
Private CONTFL     := 01
Private m_pag      := 01
Private wnrel      := "RELMAPA" // Coloque aqui o nome do arquivo usado para impressao em disco

Private cString := "STF"

dbSelectArea("STF")
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

Local nOrdem

dbSelectArea("STF")
dbSetOrder(1)
_REGAUX:=""
pvez:=.t.
SetRegua(RecCount())
dbSeek(xFilial()+mv_par01+mv_par03,.T.)
While !EOF() .And. xFilial() == TF_FILIAL .And. TF_CODBEM <= mv_par02 .And. TF_SERVICO <= mv_par04
	
	If lAbortPrint
		@nLin,00 PSAY "*** CANCELADO PELO OPERADOR ***"
		Exit
	Endif
	aarea:=getarea()
	DBSELECTAREA("ST9")
	DBSETORDER(1)
	DBSEEK(XFILIAL("ST9")+STF->TF_CODBEM)
	restarea(aarea)
	
	If ST9->T9_CODFAMI < mv_par05.OR. ST9->T9_CODFAMI > mv_par06
		_REGAUX:=TF_FILIAL+TF_CODBEM
		dbSkip()
		Loop
	Endif
	If ST9->T9_CCUSTO < mv_par07 .OR. ST9->T9_CCUSTO > mv_par08
		_REGAUX:=TF_FILIAL+TF_CODBEM
		dbSkip()
		Loop
	Endif
	If ST9->T9_CENTRAB < mv_par09 .OR. ST9->T9_CENTRAB > mv_par10
		_REGAUX:=TF_FILIAL+TF_CODBEM
		dbSkip()
		Loop
	Endif  
	If ALLTRIM(ST9->T9_MTBAIXA)<>""
    	_REGAUX:=TF_FILIAL+TF_CODBEM
	   dbSkip()
	   Loop
	Endif
    If STF->TF_ATIVO=="N"
	   dbSkip()
	   Loop
    Endif

	P:=STF->TF_CONMANU+STF->TF_INENMAN   //PROXIMA MANUTENCAO=CONTADORATUAL+INCREMENTO
	F:=P-ST9->T9_CONTACU                 //ODOMETRO RESTANTE
	G:=TRANSFORM(F,"@E 999999")
	ACHOU:=""
	IF F>=0
		RET:="A Vencer"
        OSABERTA(@ACHOU)
	ELSE
//		RET:="Atrasada"
		RET:="Vencida"
		OSABERTA(@ACHOU)
//		IF OSABERTA(@ACHOU)
//			RET:="C/Os And"
//		endif
	ENDIF
	if mv_par11<>1
		if mv_par11==2 .and. (RET<>"Vencida" .or. (RET=="Vencida" .and. Alltrim(ACHOU)<>"")) //RET<>"Atrasada"   //atrasadas vencidas sem OS
			_REGAUX:=TF_FILIAL+TF_CODBEM
			dbSkip()
			Loop
		endif
		if mv_par11==3 .and. f<0    //normais 
			_REGAUX:=TF_FILIAL+TF_CODBEM
			dbSkip()
			Loop
		endif
		if mv_par11==3 .and. f>=0    //normais
			if f < mv_par12 .or. f > MV_PAR13
				_REGAUX:=TF_FILIAL+TF_CODBEM
				dbSkip()
				Loop
			endif
		endif
		if mv_par11==4 .and. (RET<>"Vencida" .or. (RET=="Vencida" .and. ALLTRIM(ACHOU)=="")) //com os andamento
			_REGAUX:=TF_FILIAL+TF_CODBEM
			dbSkip()
			Loop
		endif
		
	endif
	
	//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
	//ณ Impressao do cabecalho do relatorio. . .                            ณ
	//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

	If nLin > 50 // Salto de Pแgina. Neste caso o formulario tem 55 linhas...
		Cabec(Titulo,Cabec1,Cabec2,NomeProg,Tamanho,nTipo)
		nLin := 9
		IF _REGAUX==TF_FILIAL+TF_CODBEM
			@nLin,00 PSAY ALLTRIM(TF_CODBEM)+" "+ALLTRIM(ST9->T9_NOME)
			@nLin,49 PSAY ALLTRIM(ST9->T9_TPCONTA)+" ATUAL: "+TRANSFORM(ST9->T9_POSCONT,"@E 999999")
			@nLin,80 PSAY "ACUMULADO: "+TRANSFORM(ST9->T9_CONTACU, "@E 999999")
			if !pvez
				@nLin,119 PSAY "(Continuacao)"
			endif
			pvez:=.f.
			nLin := nLin + 1 // Avanca a linha de impressao
		endif
	Endif
	IF _REGAUX<>TF_FILIAL+TF_CODBEM
		if nLin <> 9
			nLin := nLin + 1 // Avanca a linha de impressao
		endif
		pvez:=.f.
		@nLin,00 PSAY ALLTRIM(TF_CODBEM)+" "+ALLTRIM(ST9->T9_NOME)
		@nLin,49 PSAY ALLTRIM(ST9->T9_TPCONTA)+" ATUAL: "+TRANSFORM(ST9->T9_POSCONT,"@E 999999")
		@nLin,80 PSAY "ACUMULADO: "+TRANSFORM(ST9->T9_CONTACU, "@E 999999")
		nLin := nLin + 1 // Avanca a linha de impressao
	ENDIF
	pvez:=.f.
	@nLin,05 PSAY TF_SERVICO
	@nLin,13 PSAY TF_NOMEMAN
	@nLin,56 PSAY TF_INENMAN PICTURE ("@E 999999")
	@nLin,64 PSAY TF_DTULTMA
	@nLin,78 PSAY TF_CONMANU PICTURE ("@E 999999")
	@nLin,92 PSAY TF_CONMANU+TF_INENMAN PICTURE ("@E 999999")
	@nLin,104 PSAY RET
	@nLin,113 PSAY G
	@nLin,121 PSAY ACHOU
	nLin := nLin + 1 // Avanca a linha de impressao
	achou:=""	
	_REGAUX:=TF_FILIAL+TF_CODBEM
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
aAdd(aRegs,{cPerg,"01","Do Bem               ?","","","mv_ch1","C",16,0,0,"G","","mv_par01","","","","","","","","","","","","","","","","","","","","","","","","","ST9","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"02","Ate o Bem            ?","","","mv_ch2","C",16,0,0,"G","","mv_par02","","","","","","","","","","","","","","","","","","","","","","","","","ST9","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"03","Do Servico           ?","","","mv_ch3","C",6,0,0,"G","","mv_par03","","","","","","","","","","","","","","","","","","","","","","","","","ST4","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"04","Ate Servico          ?","","","mv_ch4","C",6,0,0,"G","","mv_par04","","","","","","","","","","","","","","","","","","","","","","","","","ST4","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"05","Da Familia           ?","","","mv_ch5","C",6,0,0,"G","","mv_par05","","","","","","","","","","","","","","","","","","","","","","","","","ST6","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"06","Ate Familia          ?","","","mv_ch6","C",6,0,0,"G","","mv_par06","","","","","","","","","","","","","","","","","","","","","","","","","ST6","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"07","Do Centro de Custo   ?","","","mv_ch7","C",9,0,0,"G","","mv_par07","","","","","","","","","","","","","","","","","","","","","","","","","CTT","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"08","Ate o Centro de Custo?","","","mv_ch8","C",9,0,0,"G","","mv_par08","","","","","","","","","","","","","","","","","","","","","","","","","CTT","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"09","Do Centro Trabalho   ?","","","mv_ch9","C",6,0,0,"G","","mv_par09","","","","","","","","","","","","","","","","","","","","","","","","","SHB","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"10","Ate Centro Trabalho  ?","","","mv_cha","C",6,0,0,"G","","mv_par10","","","","","","","","","","","","","","","","","","","","","","","","","SHB","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"11","Situacao             ?","","","mv_chb","N",1,0,0,"C","","mv_par11","Todas","t","t","","","Vencidas s/OS","","","","","A vencer","","","","","Vencidas c/OS","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"12","Normais a vencer de  ?","","","mv_chc","N",6,0,0,"G","","mv_par12","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"13","Normais a vencer ate ?","","","mv_chd","N",6,0,0,"G","(MV_PAR13 >= MV_PAR12)","mv_par13","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
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

STATIC FUNCTION OSABERTA(ACHOU)
LOCAL AAREA:=GETAREA()
LOCAL RET:=.F.
DBSELECTAREA("STJ")
DBSETORDER(2)
DBSEEK(XFILIAL("STJ")+"B"+STF->TF_CODBEM+STF->TF_SERVICO+STF->TF_SEQRELA,.T.)
WHILE !EOF() .AND. XFILIAL("STJ")+"B"+STF->TF_CODBEM+STF->TF_SERVICO+STF->TF_SEQRELA==STJ->TJ_FILIAL+"B"+STJ->TJ_CODBEM+STJ->TJ_SERVICO+STJ->TJ_SEQRELA
	IF STJ->TJ_TERMINO<>"S" .and. STJ->TJ_SITUACA=="L"
		ACHOU+=STJ->TJ_ORDEM+" "
		RET:=.T.
	ENDIF
	DBSKIP()
ENDDO
RESTAREA(AAREA)
RETURN(RET)
