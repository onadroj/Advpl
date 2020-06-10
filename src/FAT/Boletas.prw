#INCLUDE "rwmake.ch"
#IFNDEF WINDOWS
	#DEFINE PSAY SAY
#ENDIF

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³BOLETAS   º Autor ³ GATASSE            º Data ³  12/07/01   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ3ÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ EMISSAO DE BOLETAS                                         º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP5 IDE                                                    º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User Function BOLETAS


//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Declaracao de Variaveis                                             ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

Local aOrd := {}
Local cDesc1         := "Este programa tem como objetivo imprimir boleto  "
Local cDesc2         := "Bancario em formulario pre-impresso."
Local cDesc3         := "Emissao de boletas"
Local cPict          := ""
Local titulo       := "BOLETAS"
Local nLin         := 1
Local Cabec1       := ""
Local Cabec2       := ""
Local imprime      := .T.
Private cString
Private CbTxt        := ""
Private lEnd         := .F.
Private lAbortPrint  := .F.
Private limite           := 32
Private tamanho          := "P"
Private nomeprog         := "BOLETAS" // Coloque aqui o nome do programa para impressao no cabecalho
Private nTipo            := 15
Private aReturn          := { "Zebrado", 1, "Administracao", 1, 2, 1, "", 1}
Private nLastKey        := 0
Private cPerg       := "BOLETA"
Private cbtxt      := Space(10)
Private cbcont     := 00
Private CONTFL     := 01
Private m_pag      := 01
Private wnrel      := "BOLETAS"
cString := "SE1"
SetPrvt("TITULO,CDESC,WNREL,AORD,ARETURN,CSTRING")
SetPrvt("CPERG,NTAMANHO,NLASTKEY,NLIN,LEND,AROTOP")
SetPrvt("LIMPRT,LIMITE,CBCONT,NQUANTITEM,CDESCRI,NLIMITE")
SetPrvt("NQUANT,NOMEPROG,NTIPO,CQTD,CBTXT,ESC")
SetPrvt("NULL,PRINTER,HEIGHT,SMALL_BAR,WIDE_BAR,DPL")
SetPrvt("NB,WB,NS,WS,_TPBAR,CHAR25")
SetPrvt("START,_FIM,CHARS,CHAR,_CFIXO1,_CFIXO2")
SetPrvt("_CFIXO3,_VALBOL,_MES1,_ANO1,_QUANT,_NQT")
SetPrvt("_DIA,_PERI1,_PERI2,_QTMESES,_DESC1,_DESC2")
SetPrvt("_BARCOD,NSOMAGER,NI,CCALCDV,_CBLOCO,NSOMA1")
SetPrvt("NSOMA2,NSOMA3,_FIXVAR,_NRES,CSOMA1,CSOMA2")
SetPrvt("CSOMA3,NSOMANN,_VARFIX,CCALCDVNN,_CODE,_CBAR")
SetPrvt("_NX,_NNRO,_CBARX,_NY,I,LETTER")
SetPrvt("_JRS,_SALIAS,AREGS,J,lp,MSG1,MSG2,MSG3,MSGA,MSGB")
//
SetPrvt("ValTitulo,POSICAO,F,j,_cFixo4,_ValBol2")


ValidPerg()

pergunte(cPerg,.t.)

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Monta a interface padrao com o usuario...                           ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
//aReturn   :=  { "Zebrado", 1,"Administracao", 2, 2, 1, "",1 }
//wnrel := SetPrint(cString,NomeProg,cPerg,@titulo,cDesc1,cDesc2,cDesc3,.F.,aOrd,.F.,Tamanho,,.T.)

//wnrel :=SetPrint(cString,NomeProg,cPerg,"",,,,.F.,,,,,,,,.T.,,)
//if nLastKey == 27
//	Return (.F.)
//Endif
//SetDefault(aReturn,cString)
//SetPrc(0,0)
IF mv_par08 == 1 //PREIMPRESSO
	wnrel := SetPrint(cString,NomeProg,cPerg,@titulo,cDesc1,cDesc2,cDesc3,.F.,aOrd,.F.,Tamanho,,.F.)
	//	SetDefault(aReturn,cString)
	Processa({|| RunCont() },"Processando...")
ELSE
	If 	MsgBox("Confirma o processamento conforme os parametros","BOLETAS.PRW","YESNO")
		Processa({|| RunCont() },"Processando...")
		//RptStatus({|| RunReport(Cabec1,Cabec2,Titulo,nLin) },Titulo)
	endif
endif
Return

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºFun‡„o    ³ RUNCONT  º Autor ³ AP5 IDE            º Data ³  01/07/02   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescri‡„o ³ Funcao auxiliar chamada pela PROCESSA.  A funcao PROCESSA  º±±
±±º          ³ monta a janela com a regua de processamento.               º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ Programa principal                                         º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/
//Static Function RunReport(Cabec1,Cabec2,Titulo,nLin)

Static Function RunCont
Local nOrdem
local saux

dbSelectArea("SE1")
dbSetOrder(5)
//SetRegua(RecCount())

ProcRegua(RecCount()) // Numero de registros a processar

//SetPrint(cString,NomeProg,cPerg,"",,,,.F.,,,,,,,,.T.,,)
// SetPrint(Alias(),"","","",,,,.F.,,,,,,,'HPLJ4.DRV',.T.,,"LPT2") //DIRETO NA PORTA
//wnrel:=  SetPrint(Alias(),NomeProg,"",,,,,.F.,,.F.,"G",,,,,.T.,,)
IF mv_par08 == 1 //PREIMPRESSO
	//	wnrel := SetPrint(cString,NomeProg,cPerg,@titulo,cDesc1,cDesc2,cDesc3,.F.,aOrd,.F.,Tamanho,,.F.)
ELSE
	//	wnrel := SetPrint(Alias(),NomeProg,"",,,,,.F.,,.F.,"G",,,,,.T.,.T.,"")
	//	aReturn := { "", 1, "" , 1, 1, "BOLETAS","",1 }      //IMPRIME DIRETO PARA ARQUIVO
    IF mv_par10=3
       cPrt:="USB00"
    ELSE
	   cPrt:="LPT"+alltrim(STR(mv_par10))
	ENDIF
	SetPrint(Alias(),"","","",,,,.F.,,,,,,,'HPLJ4.DRV',.T.,,cPrt)
ENDIF
If nLastKey == 27
	Return
Endif

SetDefault(aReturn,cString)
SetPrc(0,0)


nTipo := If(aReturn[4]==1,15,18)

IF mv_par08 == 1 //PREIMPRESSO
	@0,0 Psay	chr(27)+"@"+chr(27)+"x0"+chr(27)+"0"+chr(27)+"C"+chr(32)+chr(15)
ELSE //LASER
	CbCont	   := ""
	nQuantItem := 0
	cDescri    := ""
	nlimite    := 150 //80
	nQuant	   := 1
	nTipo	   := 18
	cQtd	   := ""
	
	cbtxt	 := SPACE(10)
	cbcont	 := 0
	
	
	// Preparacao Inicio
	
	esc := CHR(27)
	null := ""
	PRINTER   := "L"
	height    := 2.5  && 2
	
	small_bar := 4.2                               && number of points per bar  3
	wide_bar := ROUND(small_bar * 2.25,0)          && 2.25 x small_bar
	dpl := 50                                      && dots per line 300dpi/6lpi = 50dpl
	
	
	nb := esc+"*c"+TRANSFORM(small_bar,'99')+"a"+Alltrim(STR(height*dpl))+"b0P"+esc+"*p+"+TRANSFORM(small_bar,'99')+"X"
	// Barra estreita
	wb := esc+"*c"+TRANSFORM(wide_bar,'99')+"a"+Alltrim(STR(height*dpl))+"b0P"+esc+"*p+"+TRANSFORM(wide_bar,'99')+"X"
	// Barra larga
	ns := esc+"*p+"+TRANSFORM(small_bar,'99')+"X"
	// Espaco estreito
	ws := esc+"*p+"+TRANSFORM(wide_bar,'99')+"X"
	// Espaco largo
	
	_TpBar := "25"
	If _TpBar == "25"
		// Representacao binaria dos numeros 1-Barras/Espacos largas (os)
		// 0-Barras/Espacos estreitas (os)
		char25 := {}
		AADD(char25,"10001")       && "1"
		AADD(char25,"01001")       && "2"
		AADD(char25,"11000")       && "3"
		AADD(char25,"00101")       && "4"
		AADD(char25,"10100")       && "5"
		AADD(char25,"01100")       && "6"
		AADD(char25,"00011")       && "7"
		AADD(char25,"10010")       && "8"
		AADD(char25,"01010")       && "9"
		AADD(char25,"00110")       && "0"
	EndIf
	If _TpBar == "39"
		// O Codigo tipo 39 NAO pode ser usados para boleto - deixo aqui como
		// se faz para referencia futura.
		
		*** adjust cusor position to start at top of line and return to bottom of line
		start := esc+"*p-50Y"
		_Fim := esc+"*p+50Y"
		chars := "1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ-. *$/+%"
		char := {}
		AADD(char,wb+ns+nb+ws+nb+ns+nb+ns+wb)       && "1"
		AADD(char,nb+ns+wb+ws+nb+ns+nb+ns+wb)       && "2"
		AADD(char,wb+ns+wb+ws+nb+ns+nb+ns+nb)       && "3"
		AADD(char,nb+ns+nb+ws+wb+ns+nb+ns+wb)       && "4"
		AADD(char,wb+ns+nb+ws+wb+ns+nb+ns+nb)       && "5"
		AADD(char,nb+ns+wb+ws+wb+ns+nb+ns+nb)       && "6"
		AADD(char,nb+ns+nb+ws+nb+ns+wb+ns+wb)       && "7"
		AADD(char,wb+ns+nb+ws+nb+ns+wb+ns+nb)       && "8"
		AADD(char,nb+ns+wb+ws+nb+ns+wb+ns+nb)       && "9"
		AADD(char,nb+ns+nb+ws+wb+ns+wb+ns+nb)       && "0"
		AADD(char,wb+ns+nb+ns+nb+ws+nb+ns+wb)       && "A"
		AADD(char,nb+ns+wb+ns+nb+ws+nb+ns+wb)       && "B"
		AADD(char,wb+ns+wb+ns+nb+ws+nb+ns+nb)       && "C"
		AADD(char,nb+ns+nb+ns+wb+ws+nb+ns+wb)       && "D"
		AADD(char,wb+ns+nb+ns+wb+ws+nb+ns+nb)       && "E"
		AADD(char,nb+ns+wb+ns+wb+ws+nb+ns+nb)       && "F"
		AADD(char,nb+ns+nb+ns+nb+ws+wb+ns+wb)       && "G"
		AADD(char,wb+ns+nb+ns+nb+ws+wb+ns+nb)       && "H"
		AADD(char,nb+ns+wb+ns+nb+ws+wb+ns+nb)       && "I"
		AADD(char,nb+ns+nb+ns+wb+ws+wb+ns+nb)       && "J"
		AADD(char,wb+ns+nb+ns+nb+ns+nb+ws+wb)       && "K"
		AADD(char,nb+ns+wb+ns+nb+ns+nb+ws+wb)       && "L"
		AADD(char,wb+ns+wb+ns+nb+ns+nb+ws+nb)       && "M"
		AADD(char,nb+ns+nb+ns+wb+ns+nb+ws+wb)       && "N"
		AADD(char,wb+ns+nb+ns+wb+ns+nb+ws+nb)       && "O"
		AADD(char,nb+ns+wb+ns+wb+ns+nb+ws+nb)       && "P"
		AADD(char,nb+ns+nb+ns+nb+ns+wb+ws+wb)       && "Q"
		AADD(char,wb+ns+nb+ns+nb+ns+wb+ws+nb)       && "R"
		AADD(char,nb+ns+wb+ns+nb+ns+wb+ws+nb)       && "S"
		AADD(char,nb+ns+nb+ns+wb+ns+wb+ws+nb)       && "T"
		AADD(char,wb+ws+nb+ns+nb+ns+nb+ns+wb)       && "U"
		AADD(char,nb+ws+wb+ns+nb+ns+nb+ns+wb)       && "V"
		AADD(char,wb+ws+wb+ns+nb+ns+nb+ns+nb)       && "W"
		AADD(char,nb+ws+nb+ns+wb+ns+nb+ns+wb)       && "X"
		AADD(char,wb+ws+nb+ns+wb+ns+nb+ns+nb)       && "Y"
		AADD(char,nb+ws+wb+ns+wb+ns+nb+ns+nb)       && "Z"
		AADD(char,nb+ws+nb+ns+nb+ns+wb+ns+wb)       && "-"
		AADD(char,wb+ws+nb+ns+nb+ns+wb+ns+nb)       && "."
		AADD(char,nb+ws+wb+ns+nb+ns+wb+ns+nb)       && " "
		AADD(char,nb+ws+nb+ns+wb+ns+wb+ns+nb)       && "*"
		AADD(char,nb+ws+nb+ws+nb+ws+nb+ns+nb)       && "$"
		AADD(char,nb+ws+nb+ws+nb+ns+nb+ws+nb)       && "/"
		AADD(char,nb+ws+nb+ns+nb+ws+nb+ws+nb)       && "+"
		AADD(char,nb+ns+nb+ws+nb+ws+nb+ws+nb)       && "%"
	EndIf
	
	_cFixo1   := "4329876543298765432987654329876543298765432"
	_cFixo2   := "21212121212121212121212121212"
	_cFixo3   := "76543298765432"
	_cFixo4   := "432765432765432765432765432765432"
	
	// Preparacao Fim
endif
Set Century On


dbSelectArea("SE1")
dbSetOrder(5) //E1_FILIAL+E1_NUMBOR+E1_NOMCLI+E1_PREFIXO+E1_NUM+E1_PARCELA+E1_TIPO
IncProc()
dbSeek(xFilial("SE1")+mv_par01,.T.)
While !EOF() .And. xFilial("SE1") == SE1->E1_FILIAL .And. SE1->E1_NUMBOR <= mv_par02
	
	
	If ALLTRIM(SE1->E1_TIPO) <> "NF" .AND. MV_PAR09==1   // Se a Serie do Arquivo for Diferente
		DbSkip()                    // do Parametro Informado !!!
		Loop
	Endif
	If ALLTRIM(SE1->E1_TIPO) <>"DP" .AND. MV_PAR09==2   // Se a Serie do Arquivo for Diferente
		DbSkip()                    // do Parametro Informado !!!
		Loop
	Endif
	If ALLTRIM(SE1->E1_TIPO) <>"RC" .AND. MV_PAR09==4   // Se a Serie do Arquivo for Diferente
		DbSkip()                    // do Parametro Informado !!!
		Loop
	Endif
	
	AAREASE1:=GETAREA()
	DBSELECTAREA("SEE")
	DBSETORDER(1)
	DBSEEK(XFILIAL("SEE")+SE1->E1_PORTADO+SE1->E1_AGEDEP+SE1->E1_CONTA,.T.)
	IF XFILIAL("SEE")+SE1->E1_PORTADO+SE1->E1_AGEDEP+SE1->E1_CONTA==SEE->(EE_FILIAL+EE_CODIGO+EE_AGENCIA+EE_CONTA)
		_banco:=RETFIELD("SA6",1,XFILIAL("SA6")+SE1->E1_PORTADO+SE1->E1_AGEDEP+SE1->E1_CONTA,"A6_NREDUZ")
		_titulo:=RETFIELD("SA6",1,XFILIAL("SA6")+SE1->E1_PORTADO+SE1->E1_AGEDEP+SE1->E1_CONTA,"A6_NOME")
		_cdbanco:=SE1->E1_PORTADO
		IF SEE->EE_CODIGO=="237"
			_agencia:=SUBSTR(SEE->EE_AGCAUX,-4)
			_cedente:=SUBSTR(SEE->EE_CONTAUX,-7)
			_agencia2:=SUBSTR(SEE->EE_AGCAUX,-4)+"-"+ExecBlock("DVBRAD",.F.,.F.,"1")
			_cedente2:=SUBSTR(SEE->EE_CONTAUX,-7)+"-"+ExecBlock("DVBRAD",.F.,.F.,"2")
		ELSE
			MSGSTOP("ROTINA NAO PREPARADA PARA O BANCO "+SEE->EE_CODIGO)
		ENDIF
	ELSE
		MSGSTOP("CADASTRO NAO ENCONTRADO NO SEE: PARAMETROS DE BANCO")
		RETURN
	ENDIF
	
	RESTAREA(AAREASE1)
	_VALOR:=EXECBLOCK("_SALDOE1",.F.,.F.,"")
	
	
	If lAbortPrint
		@nLin,00 PSAY "*** CANCELADO PELO OPERADOR ***"
		Exit
	Endif
	
	IF ALLTRIM(SE1->E1_NUMBCO)<>""
		aArea:=GetArea()
		MSG1:=MV_PAR05
		MSG2:=MV_PAR06
		MSG3:=MV_PAR07
		saux:=ALLTRIM(str(val(MV_PAR05)))
		saux:=replicate("0",3-len(saux))+saux
		IF  saux = ALLTRIM(MV_PAR05)
			dbSelectArea("SM4")
			dbSetOrder(1)
			IF dbSeek(xFilial("SM4")+saux)
				MSG1:=&(M4_FORMULA)
			ENDIF
		ENDIF
		saux:=ALLTRIM(str(val(MV_PAR06)))
		saux:=replicate("0",3-len(saux))+saux
		IF saux = ALLTRIM(MV_PAR06)
			dbSelectArea("SM4")
			dbSetOrder(1)
			IF dbSeek(xFilial("SM4")+saux)
				MSG2:=&(M4_FORMULA)
			ENDIF
		ENDIF
		saux:=ALLTRIM(str(val(MV_PAR07)))
		saux:=replicate("0",3-len(saux))+saux
		IF saux = ALLTRIM(MV_PAR07)
			dbSelectArea("SM4")
			dbSetOrder(1)
			IF dbSeek(xFilial("SM4")+saux)
				MSG3:=&(M4_FORMULA)
			ENDIF
		ENDIF
		RestArea(aArea)
		
		IF mv_par08 == 1 //PREIMPRESSA
			nlin:=2
			If mv_par03 == 1
				@nLin,1 PSAY mv_par04
			else
				@nLin,1 PSAY _titulo
			endif
			@nLin,90 PSAY SE1->E1_VENCTO
			nLin := nLin + 4 // Avanca a linha de impressao
			@nLin,1 PSAY SE1->E1_EMISSAO
			@nLin,17 PSAY SE1->E1_NUM + If(!Empty(SE1->E1_PARCELA),"-"+SE1->E1_PARCELA,"")
			
			@nLin,41 PSAY "DM"
			//		    @nLin,53 PSAY ""
			@nLin,62 PSAY DDATABASE
			nLin := nLin + 2 // Avanca a linha de impressao
			@nLin,90 PSAY _VALOR PICTURE "@E 999,999,999.99"
			nLin := nLin + 4 // Avanca a linha de impressao
			@nLin,1 PSAY "NAO LIQUIDAR ESTE BOLETO POR DOC OU DEPOSITO."
			nLin := nLin + 1 // Avanca a linha de impressao
			@nLin,1 PSAY "PROTESTO AUTOMATICO 10 DIAS APOS VENCIMENTO."
			nLin := nLin + 1 // Avanca a linha de impressao
			@nLin,1 PSAY "APOS VENCTO.MULTA DE 2,5% E JUROS DE 1% AO MES."
			nLin := nLin + 1 // Avanca a linha de impressao
			@nLin,1 PSAY msg1
			nLin := nLin + 1 // Avanca a linha de impressao
			@nLin,1 PSAY msg2
			nLin := nLin + 1 // Avanca a linha de impressao
			@nLin,1 PSAY msg3
			nLin := nLin + 3 // Avanca a linha de impressao
			
			aArea:=GetArea()
			DbSelectArea("SA1")
			DbSeek(xFilial("SA1")+SE1->E1_CLIENTE+SE1->E1_LOJA,.F.)
			@nLin,14 PSAY SA1->A1_NOME
			nLin := nLin + 1 // Avanca a linha de impressao
			@nLin,14 PSAY "CPF/CNPJ: " + Transform(Trim(SA1->A1_CGC),If(" "$SA1->A1_CGC,"@R 999.999.999-99","@R 99.999.999/9999-99"))
			nLin := nLin + 1 // Avanca a linha de impressao
			@nLin,14 PSAY SA1->A1_ENDCOB
			nLin := nLin + 1 // Avanca a linha de impressao
			@nLin,14 PSAY Transform(Trim(SA1->A1_CEPC),"@R 99999-999")+"   "+SA1->A1_MUNC+"   "+SA1->A1_ESTC
			nLin := nLin + 1 // Avanca a linha de impressao
			RestArea(aArea)
			for x:=nlin to 32
				@nLin,1 Psay ""
			next
		else //boleta laser
			//Montagem do Codigo de Barras
			// Campo Valor
			
			ValTitulo:=alltrim(STR(_VALOR))
			POSICAO:=AT(".",ValTitulo)
			if POSICAO <> 0  //Tem Casa Decimais
				f := SUBSTR(ValTitulo,1,POSICAO-1)+SUBSTR(ValTitulo,POSICAO+1,(LEN(ValTitulo)-POSICAO))
				if (LEN(ValTitulo)-POSICAO) = 1   //So tem uma casa decimal
					j := f+"0"
					_ValBol := Replicate("0",10-Len(j))+j
				else
					j := f
					_ValBol := Replicate("0",10-Len(j))+j
				endif
			else          // Nao Tem casas decimais
				f := ValTitulo+"00"
				_ValBol := Replicate("0",10-LEN(f))+f
			endif
			_ValBol2 := f         //VALOR DA BOLETA
			_FatorVenc:=STRZERO(SE1->E1_VENCTO-CTOD("07/10/1997"),4,0)
			
			/*			_Mes1 := Month(SE1->E1_VENCTO)
			_Ano1 := Year(SE1->E1_VENCTO)
			_Quant := Val(SE1->E1_PARCELA) - 1
			For _nQt := 1 to _Quant
			_Mes1 := _Mes1 - 1
			If _Mes1 == 0
			_Mes1 := 12
			_Ano1 := _Ano1 - 1
			EndIf
			Next
			*/
			_Desc1 := 0.00
			_Desc2 := 0.00
			
			_BarCod := _cdbanco                     && Banco
			_BarCod := _BarCod + "9"             && Moeda (no banco)
			_BarCod := _BarCod + _FatorVenc      && fator vencimento
			_BarCod := _BarCod + _ValBol        && Valor
			_BarCod := _BarCod + _agencia         && Agencia
			_BarCod := _BarCod + "09" //CARTEIRA
			_BarCod := _BarCod + STRZERO(VAL(SUBSTR(SE1->E1_NUMBCO,1,Len(ALLTRIM(SE1->E1_NUMBCO))-1)),11) && N/Numero
			_BarCod := _BarCod + _cedente      && Nro.Conta Corrente
			
			_BarCod := _BarCod +"0"
			
			//Calculo do DV Geral
			
			nSomaGer := 0
			For nI := 1 to 43
				nSomaGer := nSomaGer + ;
				(Val(Substr(_BarCod,nI,1))*Val(Substr(_cFixo1,nI,1)))
			Next
			If (((11-(nSomaGer%11)) > 9) .or. ((11-(nSomaGer%11))== 0) .or. ((11-(nSomaGer%11))== 1))
				cCalcDv := "1"
			Else
				cCalcDv := ALLTRIM(Str(11-(nSomaGer%11),1))
			Endif
			_BarCod := Left(_BarCod ,4) + cCalcDv + SubStr(_BarCod,5) //_BarCod agora tem 44 posicoes
			
			// Monta sequencia de codigos para o topo do boleto
			//linha digitavel
			
			
			_cBloco := Left(_BarCod,4) + Substr(_BarCod,20)
			
			nSoma1 := 0
			nSoma2 := 0
			nSoma3 := 0
			nSoma4 := 0
			
			// Calcula o DV do primeiro Bloco
			_FixVar := Right(_cFixo2,9)
			For nI := 1 to 9
				//       _nRes := Val(Substr(_cBloco,nI,1))*Val(Substr(_FixVar,nI,1))
				_nRes := Val(Substr(_cBloco,nI,1))*Val(Substr(_cFixo2,nI,1))
				If _nRes > 9
					_nRes := 1 + (_nRes-10)
				Endif
				nSoma1 := nSoma1 + _nRes
			Next
			
			// Calcula o DV do segundo bloco
			_FixVar := Right(_cFixo2,10)
			For nI := 10 to 19
				//       _nRes := Val(Substr(_cBloco,nI,1))*Val(Substr(_FixVar,nI,1))
				_nRes := Val(Substr(_cBloco,nI,1))*Val(Substr(_cFixo2,nI,1))
				If _nRes > 9
					_nRes := 1 + (_nRes-10)
				Endif
				nSoma2 := nSoma2 + _nRes
			Next
			
			// Calcula o DV do terceiro Bloco
			_FixVar := Right(_cFixo2,10)
			For nI := 20 to 29
				//       _nRes := Val(Substr(_cBloco,nI,1))*Val(Substr(_FixVar,nI,1))
				_nRes := Val(Substr(_cBloco,nI,1))*Val(Substr(_cFixo2,nI,1))
				If _nRes > 9
					_nRes := 1 + (_nRes-10)
				Endif
				nSoma3 := nSoma3 + _nRes
			Next
			cSoma1 := Right(StrZero(10-(nSoma1%10),2),1)
			cSoma2 := Right(StrZero(10-(nSoma2%10),2),1)
			cSoma3 := Right(StrZero(10-(nSoma3%10),2),1)
			
			// Uso as funcoes StrZero e Right para pegar o nro correto quando o resto
			// de nSoma/10 for 0
			
			// Monta sequencia de codigos para o topo do boleto com os dvs e o valor
			
			_cBloco := Left(_BarCod,4) + Substr(_BarCod,20,5) + cSoma1 +;
			Substr(_BarCod,25,10) + cSoma2+ Substr(_BarCod,35,10)+ cSoma3 +;
			cCalcDv + _FatorVenc+_ValBol
			
			
			// Calcula o DAC do Nosso N£mero
			
			nSomaNN := 0
			_VarFix := Right(_cFixo4,13)
			For nI := 1 to 13
				nSomaNN := nSomaNN + (Val(Substr(("09"+SE1->E1_NUMBCO),nI,1))*Val(Substr(_VarFix,nI,1)))
			Next
			if (nSomaNN%11)==1
				cCalcDvNN := "P"
			ELSE
				IF (nSomaNN%11)==0
					cCalcDvNN := "0"
				Else
					cCalcDvNN := Alltrim(Str(11-(nSomaNN%11),1))
				Endif
			EndIf
			
			// Monta String do codigo de barras propriamente dito
			_code := ""
			
			If _TpBar == "25"
				// Intercala a referencia binaria dos numeros aos pares, pois nesse tipo
				// os numeros das posicoes impares serao escritos em barras largas e barras
				// estreitas e os numeros das posicoes pares serao escritos com espacos largos
				// e espacos estreitos.
				_cBar := _BarCod
				For _nX := 1 to 43 Step 2 && 44 porque o meu cod.possue 44 numeros
					_nNro := VAl(Substr(_cBar,_nx,1))
					If _nNro == 0
						_nNro := 10
					EndIf
					_cBarx := char25[_nNro]
					_nNro := VAl(Substr(_cBar,_nx+1,1))
					If _nNro == 0
						_nNro := 10
					EndIf
					_cBarx := _cBarx + char25[_nNro]
					
					For _nY := 1 to 5
						If Substr(_cBarx,_nY,1) == "0"
							// Uso Barra estreita
							_code := _code + nb
						Else
							// Uso Barra larga
							_code := _code + wb
						EndIf
						If Substr(_cBarx,_nY+5,1) == "0"
							// Uso Espaco estreito
							_code := _code + ns
						Else
							// Uso Espaco Largo
							_code := _code + ws
						EndIf
					Next
				Next
				_code := nb+ns+nb+ns+_code+wb+ns+nb
				// Guarda de inicio == Barra Estr+Esp.Estr+Barra Estr+Esp.Estr
				// Guarda de Fim    == Barra Larga +Esp.Estr+Barra Estr
				// Estes devem ser colocados antes e depois do codigo montado
			ElseIf _TpBar == "39"
				_code := ""
				_BarCod := "*"+_BarCod+"*"
				FOR I := 1 TO LEN(m->_BarCod)
					letter := SUBSTR(m->_BarCod,I,1)
					_code := _code + IF(AT(letter,chars)=0,letter,char[at(letter,chars)]) + ns
				NEXT
				//   _code := start + _code + _Fim
				
			EndIf
			
			
			aArea:=GetArea()
			DbSelectArea("SA1")
			DbSeek(xFilial("SA1")+SE1->E1_CLIENTE+SE1->E1_LOJA,.F.)
			RestArea(aArea)
			
			@ 00,000 PSAY "E"
			lp:=0
			ImprimeUmaVia()
		endif
	ENDIF
	
	dbSkip() // Avanca o ponteiro do registro no arquivo
EndDo
@0,0 Psay	chr(27)+"@"


//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Finaliza a execucao do relatorio...                                 ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

SET DEVICE TO SCREEN

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Se impressao em disco, chama o gerenciador de impressao...          ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

If aReturn[5]==1
	dbCommitAll()
	SET PRINTER TO
	IF mv_par08 == 1 //PREIMPRESSO
		OurSpool(wnrel) //NAO MOSTRA NO SPOOL
	ENDIF
Endif

MS_FLUSH()
//IF mv_par08 == 2 //LASER
//	WAITRUN ("I:\IMP.PIF")
//ENDIF
Return


/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±ºFun‡„o    ³IMPRIMEUMAVIAº Autor ³ AP5 IDE            º Data ³  12/07/01   º±±
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
/*/

Static Function ImprimeUmaVia()
_aArea:=GetArea()
DbSelectArea("SA1")
DbSetOrder(1)
DbSeek(xFilial("SA1")+SE1->E1_CLIENTE+SE1->E1_LOJA)
RestArea(_aArea)


//DESENHO DA PARTE DE CIMA
@ 0,000 PSAY "&l26A(8U(s1p18v0s0b4148T"
@ 0,000 PSAY "*p566.93x0.00Y*c0.00a82.00b0P"
@ 0,000 PSAY "*p578.74x70.87Y*p803.15x0.00Y*c0.00a82.00b0P"
@ 0,000 PSAY "*p1677.17x0.00Y(s1p09.5v0s3b4148T*p1712.60x70.87YRecibo do Sacado"
@ 0,000 PSAY "(s1p13.5v0s3b"
@ 0,000 PSAY "*p0.00x82.00Y*c3.00a1583.00b0P"
@ 0,000 PSAY "*p2303.15x82.00Y*c3.00a1583.00b0P"
@ 0,000 PSAY "*p0.00x82.00Y*c2303.15a3.00b0P"
@ 0,000 PSAY "*p0.00x179.00Y*c2303.15a3.00b0P"
@ 0,000 PSAY "*p0.00x276.00Y*c2303.15a3.00b0P"
@ 0,000 PSAY "*p0.00x373.00Y*c2303.15a3.00b0P"
@ 0,000 PSAY "*p0.00x470.Y*c2303.15a3.00b0P"
@ 0,000 PSAY "*p0.00x1500.00Y*c2303.15a3.00b0P"
@ 0,000 PSAY "*p0.00x1665.00Y*c2303.15a3.00b0P"
@ 0,000 PSAY "*p413.39x276.002Y*c3.00a194.00b0P"
@ 0,000 PSAY "*p838.58x276.00Y*c3.00a97.06b0P"
@ 0,000 PSAY "*p1039.37x276.00Y*c3.00a97.06b0P"
@ 0,000 PSAY "*p1240.16x276.00Y*c3.00a97.06b0P"
@ 0,000 PSAY "*p1677.17x82.68Y*c3.00a818.74b0P"
@ 0,000 PSAY "*p590.55x373.00Y*c3.00a97.06b0P"
@ 0,000 PSAY "*p909.45x373.00Y*c3.00a97.06b0P"
@ 0,000 PSAY "*p1346.46x373.00Y*c3.00a97.06b0P"
@ 0,000 PSAY "*p1677.17x556.00Y*c625.98a3.00b0P"
@ 0,000 PSAY "*p1677.17x642.00Y*c625.98a3.00b0P"
@ 0,000 PSAY "*p1677.17x728.00Y*c625.98a3.00b0P"
@ 0,000 PSAY "*p1677.17x814.00Y*c625.98a3.00b0P"
@ 0,000 PSAY "*p1677.17x900.00Y*c625.98a3.00b0P"
@ 0,000 PSAY "(8U(s4148T(s1P(s5V(s2B"
//TEXTO DA PARTE DE CIMA
@ 0,000 PSAY "*p11.81x106.00YLocal de Pagamento"
@ 0,000 PSAY "*p1688.98x106.00YVencimento"
@ 0,000 PSAY "*p11.81x203.00YCedente"
@ 0,000 PSAY "*p1688.98x203.00YAgencia/Codigo Cedente"
@ 0,000 PSAY "*p11.81x300.00YData do Documento"
@ 0,000 PSAY "*p425.20x300.00YNo. do Documento"
@ 0,000 PSAY "*p850.39x300.00YEspecie Doc."
@ 0,000 PSAY "*p1051.18x300.00YAceite"
@ 0,000 PSAY "*p1251.97x300.00YData do Processamento"
@ 0,000 PSAY "*p1688.98x300.00YNosso Numero"
@ 0,000 PSAY "*p11.81x397.00YUso do Banco"
@ 0,000 PSAY "*p425.20x397.00YCarteira"
@ 0,000 PSAY "*p602.36x397.00YEspecie da Moeda"
@ 0,000 PSAY "*p921.26x397.00YQuantidade"
@ 0,000 PSAY "*p1358.27x397.00YValor"
@ 0,000 PSAY "*p1688.98x397.00Y1 (=) Valor do Documento"
@ 0,000 PSAY "*p11.81x494.00YInformacoes de Responsabilidade do Cedente"
@ 0,000 PSAY "*p1688.98x494.00Y2 (--) Abatimento"
@ 0,000 PSAY "*p1688.98x580.00Y3 (--) Desconto"
@ 0,000 PSAY "*p1688.98x666.00Y4 (+) Multa/Outros Recebimentos"
@ 0,000 PSAY "*p1688.98x752.00Y5 (+) Juros"
@ 0,000 PSAY "*p1688.98x838.00Y6 (=) Valor Cobrado"
@ 0,000 PSAY "*p11.81x1524.00YSacado"
@ 0,000 PSAY "*p11.81x1655.00YSacador/Avalista"
//@ 0,000 PSAY "*p1688.98x1655.00YCodigo de Baixa:"
@ 0,000 PSAY "*p11.81x1689.00YRecebimento atraves de Cheque num."
@ 0,000 PSAY "*p11.81x1713.00Ydo banco"
@ 0,000 PSAY "*p11.81x1761.00YEsta quitacao so tera validade apos pagamento do cheque pelo"
@ 0,000 PSAY "*p11.81x1785.07Ybanco sacado"
@ 0,000 PSAY "*p1588.98x1689.00YAutenticacao Mecanica"
//PICOTE
@ 0,000 PSAY "*p0.00x1800.36Y*c2303.15a3.00b2g3P"
//DESENHO DA PARTE DE BAIXO
@ 0,000 PSAY "(8U(s1p18v0s0b4148T*p500.93x1968.66Y*c3.00a82.68b0P*p578.74x2279.53Y"
@ 0,000 PSAY "(s1p09.5v0s0b4148T"
@ 0,000 PSAY "*p23.62x2279.53Y"
@ 0,000 PSAY "*p703.15x1968.00Y*c3.00a82.68b0P"
@ 0,000 PSAY "*p0.00x2051.34Y*c2303.15a3.00b0P"
@ 0,000 PSAY "*p0.00x2051.34Y*c3.00a984.09b0P"
@ 0,000 PSAY "*p2303.15x2051.34Y*c3.00a984.09b0P"
@ 0,000 PSAY "*p1677.17x2051.34Y*c3.00a819.09b0P"
@ 0,000 PSAY "*p0.00x2148.83Y*c2303.15a3.00b0P"
@ 0,000 PSAY "*p0.00x2245.88Y*c2303.15a3.00b0P"
@ 0,000 PSAY "*p0.00x2342.94Y*c2303.15a3.00b0P"
@ 0,000 PSAY "*p0.00x2439.99Y*c2303.15a3.00b0P"
@ 0,000 PSAY "*p0.00x2870.08Y*c2303.15a3.00b0P"
@ 0,000 PSAY "*p0.00x3035.43Y*c2303.15a3.00b0P"
@ 0,000 PSAY "*p413.39x2245.88Y*c3.00a194.00b0P"
@ 0,000 PSAY "*p838.58x2245.88Y*c3.00a97.00b0P"
@ 0,000 PSAY "*p1039.37x2245.88Y*c3.00a97.00b0P"
@ 0,000 PSAY "*p1240.16x2245.88Y*c3.00a97.00b0P"
@ 0,000 PSAY "*p590.55x2342.94Y*c3.00a97.00b0P"
@ 0,000 PSAY "*p909.45x2342.94Y*c3.00a97.00b0P"
@ 0,000 PSAY "*p1346.46x2342.94Y*c3.00a97.00b0P"
@ 0,000 PSAY "*p1677.17x2526.01Y*c625.98a3.00b0P"
@ 0,000 PSAY "*p1677.17x2612.03Y*c625.98a3.00b0P"
@ 0,000 PSAY "*p1677.17x2698.04Y*c625.98a3.00b0P"
@ 0,000 PSAY "*p1677.17x2784.06Y*c625.98a3.00b0P"
@ 0,000 PSAY "(8U(s4148T(s1P(s5V(s2B"
//TEXTO DA PARTE DE BAIXO
@ 0,000 PSAY "*p11.81x2075.00YLocal de Pagamento"
@ 0,000 PSAY "*p1688.98x2075.00YVencimento"
@ 0,000 PSAY "*p11.81x2172.00YCedente"
@ 0,000 PSAY "*p1688.98x2172.00YAgencia/Codigo Cedente"
@ 0,000 PSAY "*p11.81x2269.00YData do Documento"
@ 0,000 PSAY "*p425.20x2269.00YNo. do Documento"
@ 0,000 PSAY "*p850.39x2269.00YEspecie Doc."
@ 0,000 PSAY "*p1051.18x2269.00YAceite"
@ 0,000 PSAY "*p1251.97x2269.00YData do Processamento"
@ 0,000 PSAY "*p1688.98x2269.00YNosso Numero"
@ 0,000 PSAY "*p11.81x2366.00YUso do Banco"
@ 0,000 PSAY "*p425.20x2366.00YCarteira"
@ 0,000 PSAY "*p602.36x2366.00YEspecie da Moeda"
@ 0,000 PSAY "*p921.26x2366.00YQuantidade"
@ 0,000 PSAY "*p1358.27x2366.00YValor"
@ 0,000 PSAY "*p1688.98x2366.00Y1 (=) Valor do Documento"
@ 0,000 PSAY "*p11.81x2463.00YInformacoes de Responsabilidade do Cedente"
@ 0,000 PSAY "*p1688.98x2463.00Y2 (--) Abatimento"
@ 0,000 PSAY "*p1688.98x2550.00Y3 (--) Desconto"
@ 0,000 PSAY "*p1688.98x2636.03Y4 (+) Multa/Outros Recebimentos"
@ 0,000 PSAY "*p1688.98x2722.04Y5 (+) Juros"
@ 0,000 PSAY "*p1688.98x2808.06Y6 (=) Valor Cobrado"
@ 0,000 PSAY "*p11.81x2893.70YSacado"
@ 0,000 PSAY "*p11.81x3023.62YSacador/Avalista"
//@ 0,000 PSAY "*p1688.98x3023.62YCodigo de Baixa:"
@ 0,000 PSAY "*p1588.98x3059.06YAutenticacao Mecanica/Ficha de Compensacao"
@ 0,000 PSAY "&f1X"   //MACRO 1
//FIM DO LAY-OUT FIXO E INICIO DOS DADOS VARIAVEIS



@ 0,000 PSAY "(s1p14.0v0s3b4148T*p22x70.87Y"+_Banco
@ 0,000 PSAY "(s1p14.0v0s3b4148T*p22x2037Y"+_Banco
@ 0,000 PSAY "(s1p14.0v0s3b4148T*p520.74x2037Y" +_CdBanco+IIF(_CdBanco="237","-2","  ")
@ 0,000 PSAY "(s1p12.0v0s0b*p730.27x2037Y" + TRANSFORM(_cBloco,"@R 99999.99999 99999.999999 99999.999999 9 99999999999999")

@ 0,000 PSAY "(s0p12h8.5v0s0b3T"
@ 0,000 PSAY "*p49x165YPAGAVEL EM QUALQUER BANCO ATE O VENCIMENTO                            "+space(9)+DTOC(SE1->E1_VENCTO)
@ 0,000 PSAY "*p49x262Y"+upper(SM0->M0_NOMECOM)+space(5)+TRANSFORM(SM0->M0_CGC,"@R 99.999.999/9999-99")+space(07)+upper(_agencia2)+" / "+upper(_cedente2)//cedente
@ 0,000 PSAY "*p49x359Y"+space(4)+DTOC(SE1->E1_EMISSAO)+space(3)+SE1->E1_NUM+If(!Empty(SE1->E1_PARCELA),"-"+SE1->E1_PARCELA,"")+space(24)+DTOC(dDataBase)+space(13)+SUBSTR(SE1->E1_NUMBCO,1,11)+"-"+ SUBSTR(SE1->E1_NUMBCO,12,1)
@ 0,000 PSAY "*p49x456Y"+space(17)+"009"+space(10)+"R$"+space(20)+TRANSFORM(_VALOR,  "@E 9,999,999.99")+space(08)+TRANSFORM(_VALOR,  "@E 9,999,999.99")
@ 0,000 PSAY "*p49x542Y"+"NAO LIQUIDAR ESTE BOLETO POR DOC OU DEPOSITO."//MENSAGEM
IF SM0->M0_CODIGO $ "03/04/08"
  @ 0,000 PSAY "*p49x628Y"+"APOS O VENCIMENTO, MULTA DE 2,5% E JUROS DE 1% AO MES."//MENSAGEM
  @ 0,000 PSAY "*p49x714Y"+"PROTESTO AUTOMATICO 10 DIAS APOS O VENCIMENTO."//MENSAGEM
ELSEIF SM0->M0_CODIGO $ "02/05"
  @ 0,000 PSAY "*p49x628Y"+"ATENCAO: SR. CAIXA: FAVOR NAO RECEBER APOS O VENCIMENTO."//MENSAGEM
ENDIF
@ 0,000 PSAY "*p49x800Y"+msg1
@ 0,000 PSAY "*p49x886Y"+msg2
@ 0,000 PSAY "*p49x972Y"+msg3
@ 0,000 PSAY "*p49x1558Y"+SA1->A1_NOME    +space(12)+   Transform(Trim(SA1->A1_CGC),If(" "$SA1->A1_CGC,"@R 999.999.999-99","@R 99.999.999/9999-99"))
@ 0,000 PSAY "*p49x1595Y"+SA1->A1_ENDCOB + space(5)+SA1->A1_BAIRROC
@ 0,000 PSAY "*p49x1635Y"+transform(SA1->A1_CEPC,"@R 99999-999")+"   "+SA1->A1_MUNC+"   "+SA1->A1_ESTC
//@ 0,000 PSAY "*p49x1328Y"+SM0->M0_NOMECOM+space(5)+TRANSFORM(SM0->M0_CGC,"@R 99.999.999/9999-99")
/*
@ 0,000 PSAY "*p49x1265YPAGAVEL EM QUALQUER BANCO ATE O VENCIMENTO                            "+space(9)+DTOC(SE1->E1_VENCTO)
@ 0,000 PSAY "*p49x1328Y"+upper(SM0->M0_NOMECOM)+space(5)+TRANSFORM(SM0->M0_CGC,"@R 99.999.999/9999-99")+space(13)+upper(_agencia2)+" / "+upper(_cedente2)//cedente
@ 0,000 PSAY "*p49x1385Y"+space(4)+DTOC(SE1->E1_EMISSAO)+space(8)+SE1->E1_NUM+If(!Empty(SE1->E1_PARCELA),"-"+SE1->E1_PARCELA,"")+space(24)+DTOC(dDataBase)+space(17)+SUBSTR(SE1->E1_NUMBCO,1,7)+"-"+ SUBSTR(SE1->E1_NUMBCO,8,1)
@ 0,000 PSAY "*p49x1442Y"+space(17)+"009"+space(10)+"R$"+space(18)+TRANSFORM(_VALOR,  "@E 9,999,999.99")+space(10)+TRANSFORM(_VALOR,  "@E 9,999,999.99")
@ 0,000 PSAY "*p49x1510Y"+"PROTESTO AUTOMATICO 10 DIAS APOS O VENCIMENTO."//MENSAGEM
@ 0,000 PSAY "*p49x1550Y"+"NAO LIQUIDAR ESTE BOLETO POR DOC OU DEPOSITO."//MENSAGEM
@ 0,000 PSAY "*p49x1590Y"+"APOS O VENCIMENTO, MULTA DE 2,5% E JUROS DE 1% AO MES."//MENSAGEM
@ 0,000 PSAY "*p49x1630Y"+msg1
@ 0,000 PSAY "*p49x1670Y"+msg2
@ 0,000 PSAY "*p49x1710Y"+msg3
@ 0,000 PSAY "*p49x1820Y"+SA1->A1_NOME    +space(12)+   Transform(Trim(SA1->A1_CGC),If(" "$SA1->A1_CGC,"@R 999.999.999-99","@R 99.999.999/9999-99"))
@ 0,000 PSAY "*p49x1860Y"+SA1->A1_ENDCOB + space(5)+SA1->A1_BAIRROC
@ 0,000 PSAY "*p49x1895Y"+transform(SA1->A1_CEPC,"@R 99999-999")+"   "+SA1->A1_MUNC+"   "+SA1->A1_ESTC
*/
@ 0,000 PSAY "*p49x2134YPAGAVEL EM QUALQUER BANCO ATE O VENCIMENTO                            "+space(9)+DTOC(SE1->E1_VENCTO)
@ 0,000 PSAY "*p49x2231Y"+upper(SM0->M0_NOMECOM)+space(5)+TRANSFORM(SM0->M0_CGC,"@R 99.999.999/9999-99")+space(07)+upper(_agencia2)+" / "+upper(_cedente2)//cedente
@ 0,000 PSAY "*p49x2328Y"+space(4)+DTOC(SE1->E1_EMISSAO)+space(3)+SE1->E1_NUM+If(!Empty(SE1->E1_PARCELA),"-"+SE1->E1_PARCELA,"")+space(24)+DTOC(dDataBase)+space(13)+SUBSTR(SE1->E1_NUMBCO,1,11)+"-"+ SUBSTR(SE1->E1_NUMBCO,12,1)
@ 0,000 PSAY "*p49x2425Y"+space(17)+"009"+space(10)+"R$"+space(20)+TRANSFORM(_VALOR,  "@E 9,999,999.99")+space(08)+TRANSFORM(_VALOR,  "@E 9,999,999.99")
@ 0,000 PSAY "*p49x2512Y"+"NAO LIQUIDAR ESTE BOLETO POR DOC OU DEPOSITO."//MENSAGEM
IF SM0->M0_CODIGO $ "03/04/08"
  @ 0,000 PSAY "*p49x2580.8Y"+"APOS O VENCIMENTO, MULTA DE 2,5% E JUROS DE 1% AO MES."//MENSAGEM
  @ 0,000 PSAY "*p49x2649.6Y"+"PROTESTO AUTOMATICO 10 DIAS APOS O VENCIMENTO."//MENSAGEM
ELSEIF SM0->M0_CODIGO $ "02/05"
  @ 0,000 PSAY "*p49x628Y"+"ATENCAO: SR. CAIXA: FAVOR NAO RECEBER APOS O VENCIMENTO."//MENSAGEM
ENDIF
@ 0,000 PSAY "*p49x2718.4Y"+msg1
@ 0,000 PSAY "*p49x2787.2Y"+msg2
@ 0,000 PSAY "*p49x2856Y"+msg3
@ 0,000 PSAY "*p49x2923Y"+SA1->A1_NOME    +space(12)+   Transform(Trim(SA1->A1_CGC),If(" "$SA1->A1_CGC,"@R 999.999.999-99","@R 99.999.999/9999-99"))
@ 0,000 PSAY "*p49x2960Y"+SA1->A1_ENDCOB + space(5)+SA1->A1_BAIRROC
@ 0,000 PSAY "*p49x3000Y"+transform(SA1->A1_CEPC,"@R 99999-999")+"   "+SA1->A1_MUNC+"   "+SA1->A1_ESTC

@ 0,000 PSAY "(s1p14.0v0s3b4148T"
@ 0,000 PSAY "*p49x3125Y" + _code
@ 0,000 PSAY "*p49x3175Y" + _code

@ 0,000 psay char(12)
RETURN

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºFun‡„o    ³VALIDPERG º Autor ³ AP5 IDE            º Data ³  12/07/01   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescri‡„o ³ Verifica a existencia das perguntas criando-as caso seja   º±±
±±º          ³ necessario (caso nao existam).                             º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ Programa principal                                         º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

Static Function ValidPerg

Local _sAlias := Alias()
Local aRegs := {}
Local i,j

dbSelectArea("SX1")
dbSetOrder(1)
cPerg := PADR(cPerg,10)
// Grupo/Ordem/Pergunta/Variavel/Tipo/Tamanho/Decimal/Presel/GSC/Valid/Var01/Def01/Cnt01/Var02/Def02/Cnt02/Var03/Def03/Cnt03/Var04/Def04/Cnt04/Var05/Def05/Cnt05
//AADD(aRegs,{cPerg,"01","LIM.FISCAL         ?" ,"LIM.FISCAL         ?" ,"LIM.FISCAL         ?", "mv_ch1","D",08,00,0,"G","","mv_par01","","","","","","","","","","","","","","","","","","","","","","","","","",""})
//aAdd(aRegs,{cPerg,"06","Cons param. abaixo?"  ,"Cons param. abaixo?"  ,"Cons param. abaixo?",  "mv_ch6","N",01,0,2,"C","","mv_par06" ,"Sim","Si","Yes","","","Nao","No","No","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"01","Do Bordero         ?" ,"Do Bordero         ?" ,"Do Bordero         ?" ,"mv_ch1","C",06,00,0,"G","","mv_par01","","","","","","","","","","","","","","","","","","","","","","","","","_EA",""})
aAdd(aRegs,{cPerg,"02","Ate o Bordero      ?" ,"Ate o Bordero      ?" ,"Ate o Bordero      ?" ,"mv_ch2","C",06,00,0,"G","","mv_par02","","ZZZZZZ","","","","","","","","","","","","","","","","","","","","","","","_EA",""})
aAdd(aRegs,{cPerg,"03","Imp.local pagamento?" ,"Imp.local pagamento?" ,"Imp.local pagamento?" ,"mv_ch3","C",01,00,0,"C","","mv_par03","Sim","Si","Yes","","","Nao","No","No","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"04","Local de Pagamento ?" ,"Local de Pagamento ?" ,"Local de Pagamento ?" ,"mv_ch4","C",60,00,0,"G","","mv_par04","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"05","Mensagem/Cd.Formula?" ,"Mensagem/Cd.Formula?" ,"Mensagem/Cd.Formula?" ,"mv_ch5","C",80,00,0,"G","","mv_par05",""    ,"","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"06","Mensagem/Cd.Formula?" ,"Mensagem/Cd.Formula?" ,"Mensagem/Cd.Formula?" ,"mv_ch6","C",80,00,0,"G","","mv_par06",""    ,"","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"07","Mensagem/Cd.Formula?" ,"Mensagem/Cd.Formula?" ,"Mensagem/Cd.Formula?" ,"mv_ch7","C",80,00,0,"G","","mv_par07",""    ,"","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"08","Tipo de Boleto     ?" ,"Tipo de Boleto     ?" ,"Tipo de Boleto     ?" ,"mv_ch8","N",01,00,1,"C","","mv_par08","Pre-impresso","Pre-impresso","Pre-impresso","","","Laser","Laser","Laser","","","","","" ,"","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"09","Tipo do Documento  ?" ,"Tipo de Documento  ?" ,"Tipo de Documento  ?" ,"mv_ch9","N",01,00,1,"C","","mv_par09","NF          ","NF          ","NF          ","","","DP   ","DP   ","DP   ","","","NF e DP","NF e DP","NF e DP" ,"","","RC","RC","RC","","","","","","","",""})
aAdd(aRegs,{cPerg,"10","Porta de Impressao ?" ,"Porta de Impressao ?" ,"Porta de Impressao ?" ,"mv_cha","N",01,00,1,"C","","mv_par10","LPT1        ","LPT1        ","LPT1        ","","","LPT2 ","LPT2 ","LPT2 ","","","USB    ","USB    ","USB    " ,"","","","","","","","","","","","",""})



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
