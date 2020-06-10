#Include "PROTHEUS.Ch"

//Tradução PTG 20080721

// 17/08/2009 -- Filial com mais de 2 caracteres
Static nTamCdoCusto := 20
Static nTamConta    := 18

//--------------------------RELEASE 04------------------------------------------------//
User Function FECtbr285()
Local cPerg			:= "FECTBR2850"
Local aPerg			:= {}
Local aHelp			:= {}
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Variaveis utilizadas para parametros					       ³
//³ mv_par01				// Data Inicial              	       ³
//³ mv_par02				// Data Final                          ³
//³ mv_par03				// C. Custo Direto Inicio  		       ³
//³ mv_par04				// C. Custo Direto Fim				   ³
//³ MV_PAR05				// C. Custo Indireto Inicio            ³
//³ mv_par06				// C. Custo Indireto Fim   		       ³
//³ mv_par07				// C. Custo Despesa Inicio			   ³
//³ MV_PAR08				// C. Custo Despesa Fim                      ³
//³ MV_PAR09				// Conta Inicial                       ³
//³ MV_PAR10				// Conta Final   					   ³
//³ MV_PAR11				// Imprime Contas:Sintet/Analit/Ambas  ³
//³ MV_PAR12				// Set Of Books				    	   ³
//³ MV_PAR13				// Saldos Zerados?			     	   ³
//³ MV_PAR14				// Moeda?          			     	   ³
//³ MV_PAR15				// Pagina Inicial  		     		   ³
//³ MV_PAR16				// Saldos? Reais / Orcados/Gerenciais  ³
//³ MV_PAR17				// Imprimir ate o Segmento?			   ³
//³ MV_PAR18				// Filtra Segmento?					   ³
//³ MV_PAR19				// Conteudo Inicial Segmento?		   ³
//³ MV_PAR20				// Conteudo Final Segmento?		       ³
//³ MV_PAR21				// Conteudo Contido em?				   ³
//³ MV_PAR22				// Pula Pagina                         ³
//³ MV_PAR23				// Imprime Cod. C.Custo? Normal/Red.   ³
//³ MV_PAR24				// Imprime Cod. Conta? Normal/Reduzido ³
//³ MV_PAR25				// Salta linha sintetica?              ³
//³ MV_PAR26 				// Imprime Valor 0.00?                 ³
//³ MV_PAR27 				// Divide por?                         ³
//³ MV_PAR28				// Posicao Ant. L/P? Sim / Nao         ³
//³ MV_PAR29				// Data Lucros/Perdas?                 ³
//³ MV_PAR30				// Totaliza periodo ?                  ³
//³ MV_PAR31				// Se Totalizar ?                  	   ³
//³ MV_PAR32				// Imprime C.C?Sintet/Analit/Ambas 	   ³
//³ MV_PAR33				// Imprime Totalizacao de C.C. Sintet. ³
//³ MV_PAR34				// Tipo de Comparativo?(Movimento/Acumulado)  ³
//³ MV_PAR35				// Quebra por Grupo Contabil?		   ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

AADD(aPerg, {cPerg, "Data Inicial ?                "	, "D", 8	, 0, "G", "", "", "", "", "", "", "",".CTR04001."})
AADD(aPerg, {cPerg, "Data Final ?                  "	, "D", 8	, 0, "G", "", "", "", "", "", "", "",".CTR04002."})
AADD(aPerg, {cPerg, "C. Custo Direto Inicio ?      "	, "C", 08	, 0, "G", "", "CTT", "", "", "", "", "",""})
AADD(aPerg, {cPerg, "C. Custo Direto Fim	 ?       "	, "C", 08	, 0, "G", "", "CTT", "", "", "", "", "",""})
AADD(aPerg, {cPerg, "C. Custo Indireto Inicio ?    "	, "C", 08	, 0, "G", "", "CTT", "", "", "", "", "",""})
AADD(aPerg, {cPerg, "C. Custo Indireto Fim	 ?	  ", "C", 08	, 0, "G", "", "CTT", "", "", "", "", "",""})
AADD(aPerg, {cPerg, "C. Custo Depesa Inicio ?      "	, "C", 08	, 0, "G", "", "CTT", "", "", "", "", "",""})
AADD(aPerg, {cPerg, "C. Custo Despesa Fim	 ?	  	  ", "C", 08	, 0, "G", "", "CTT", "", "", "", "", "",""})
AADD(aPerg, {cPerg, "Da Conta ?                    "	, "C", 20	, 0, "G", "", "CT1", "", "", "", "", "",".CTR04003."})
AADD(aPerg, {cPerg, "Ate a Conta ?                 "	, "C", 20	, 0, "G", "", "CT1", "", "", "", "", "",".CTR04004."})
AADD(aPerg, {cPerg, "Imprime Contas ?              "	, "N", 01	, 0, "C", "", "", "Sinteticas", "Analiticas", "Ambas", "", "",".CTR18007."})
AADD(aPerg, {cPerg, "Cod. Config. Livros ?         "	, "C", 03	, 0, "G", "", "CTN", "", "", "", "", "",".CTR04006."})
AADD(aPerg, {cPerg, "Saldos Zerados ?              "	, "N", 01	, 0, "C", "", "", "Sim", "Nao", "", "", "",".CTR18009."})
AADD(aPerg, {cPerg, "Moeda ?                       "	, "C", 02	, 0, "G", "", "CTO", "", "", "", "", "",".CTR04008."})
AADD(aPerg, {cPerg, "Folha  Inicial ?              "	, "N", 06	, 0, "G", "", "", "", "", "", "", "",".CTR18011."})
AADD(aPerg, {cPerg, "Tipo de Saldo ?               "	, "C", 01	, 0, "G", "VldTpSald( MV_PAR16,.T. )", "SLD", "", "", "", "", "",".CTR04010."})
AADD(aPerg, {cPerg, "Imprimir ate o seg. ?         "	, "C", 02	, 0, "G", "", "", "", "", "", "", "",".CTR18014."})
AADD(aPerg, {cPerg, "Filtra Segmento No. ?         "	, "C", 02	, 0, "G", "", "", "", "", "", "", "",".CTR18015."})
AADD(aPerg, {cPerg, "Conteudo Ini Segmen ?         "	, "C", 20	, 0, "G", "", "", "", "", "", "", "",".CTR18016."})
AADD(aPerg, {cPerg, "Conteudo Fim Segmen ?         "	, "C", 20	, 0, "G", "", "", "", "", "", "", "",".CTR18017."})
AADD(aPerg, {cPerg, "Conteudo Contido em ?         "	, "C", 30	, 0, "G", "", "", "", "", "", "", "",".CTR18018."})
AADD(aPerg, {cPerg, "Pula Pagina ?                 "	, "N", 01	, 0, "C", "", "", "Sim", "Nao","","","",".CTR44020."})
AADD(aPerg, {cPerg, "Imprime Cod. C.Custo ?        "	, "N", 01	, 0, "C", "", "", "Normal", "Reduzido", "", "", "",".CTR18024."})
AADD(aPerg, {cPerg, "Imprime Cod. Conta ?          "	, "N", 01	, 0, "C", "", "", "Normal", "Reduzido", "", "", "",".CTR18026."})
AADD(aPerg, {cPerg, "Salta linha sintetica ?       "	, "N", 01	, 0, "C", "", "", "Sim", "Nao", "", "", "",".CTR18022."})
AADD(aPerg, {cPerg, "Imprime Valor 0.00 ?          "	, "N", 01	, 0, "C", "", "", "Sim", "Nao", "", "", "",".CTR18023."})
AADD(aPerg, {cPerg, "Divide por ?                  "	, "N", 01	, 0, "C", "", "", "Nao se aplica", "Cem", "Mil", "Milhao", "",".CTR18025."})
AADD(aPerg, {cPerg, "Posicao Ant. L/P ?            "	, "N", 01	, 0, "C", "", "", "Sim", "Nao", "", "", "",".CTR18027."})
AADD(aPerg, {cPerg, "Data Lucros/Perdas ?          "	, "D", 08	, 0, "G", "", "", "", "", "", "", "",".CTR18028."})
AADD(aPerg, {cPerg, "Totaliza Periodo ?            "	, "N", 01	, 0, "C", "", "", "Sim", "Nao", "", "", "",".CTR26523."})
AADD(aPerg, {cPerg, "Se totalizar, mostra ?        "	, "N", 01	, 0, "C", "", "", "Conta", "Descricäo", "", "", "",".CTR26524."})
AADD(aPerg, {cPerg, "Imprime C.Custo ?             "	, "N", 01	, 0, "C", "", "", "Sinteticos", "Analiticos", "Ambos", "", "",".CTR18034."})
AADD(aPerg, {cPerg, "Imprime Tot.Sintet. ?         "	, "N", 01	, 0, "C", "", "", "Sim", "Nao", "", "", "",""})
AADD(aPerg, {cPerg, "Comparar ?                    "	, "N", 01	, 0, "C", "", "", "Mov. Periodo   ", "Saldo Acumulado", "", "", "",""})
AADD(aPerg, {cPerg, "Quebra por Grupo Contabil?    "	, "N", 01	, 0, "C", "", "", "Sim", "Nao", "", "", "",".CTR18013."})

TestaSX1(cPerg,aPerg)

If FindFunction("TRepInUse") .And. TRepInUse()
	CTBR285R4()
Else
    Return CTBR285R3() // Executa versão anterior do fonte
Endif

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³Ctbr285R4 ºAutor  ³Paulo Carnelossi    º Data ³  16/08/06   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³Release 4                                                   º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP                                                         º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Static Function Ctbr285R4()
Local aArea := GetArea()
Local cSayCC		:= CtbSayApro("CTT")
LOCAL cString		:= "CTT"
Local cTitulo 		:= "Comparativo por"+Upper(Alltrim(cSayCC))+" / "+ " Conta " 	//"Comparativo de" " Conta "
Local lAtSlComp		:= Iif(GETMV("MV_SLDCOMP") == "S",.T.,.F.)
Local cMensagem		:= ""

If ( !AMIIn(34) )		// Acesso somente pelo SIGACTB
	Return
EndIf

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Mostra tela de aviso - Atualizacao de saldos				 ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
cMensagem := "Caso nao atualize os saldos compostos na"+chr(13)  		//"Caso nao atualize os saldos compostos na"
cMensagem += "emissao dos relatorios(MV_SLDCOMP ='N'),"+chr(13)  		//"emissao dos relatorios(MV_SLDCOMP ='N'),"
cMensagem += "rodar a rotina de atualizacao de saldos "+chr(13)  		//"rodar a rotina de atualizacao de saldos "

IF !lAtSlComp
	IF !MsgYesNo(cMensagem,"ATEN€O")	//"ATEN€O"
		Return
	Endif
EndIf

Pergunte("FECTBR2850",.F.)


//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Interface de impressao                                                  ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
oReport := ReportDef(cSayCC, cString, cTitulo)

If !Empty(oReport:uParam)
	Pergunte(oReport:uParam,.F.)
EndIf	

oReport:PrintDialog()

RestArea(aArea)
	
Return

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³Ctbr285R4 ºAutor  ³Paulo Carnelossi    º Data ³  16/08/06   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³Release 4                                                   º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP                                                         º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Static Function ReportDef(cSayCC, cString, cTitulo)
Local cPerg	 	    := "FECTBR2850"
Local cDesc1 		:= "Este programa ira imprimir o Balancete Comparativo "
Local cDesc2 		:= Upper(Alltrim(cSayCC)) +" / "+ " CONTA "	// " Conta "
Local cDesc3 		:= "de acordo com os parametros solicitados pelo Usuario"

Local oReport
Local oCentroCusto
Local nX
Local aOrdem := {}

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Criacao do componente de impressao                                      ³
//³                                                                        ³
//³TReport():New                                                           ³
//³ExpC1 : Nome do relatorio                                               ³
//³ExpC2 : Titulo                                                          ³
//³ExpC3 : Pergunte                                                        ³
//³ExpB4 : Bloco de codigo que sera executado na confirmacao da impressao  ³
//³ExpC5 : Descricao                                                       ³
//³                                                                        ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

oReport := TReport():New("CTBR285",cTitulo, cPerg, ;
			{|oReport| If(!ct040Valid(MV_PAR12), oReport:CancelPrint(), ReportPrint(oReport,cSayCC, cString, cTitulo))},;
			cDesc1+CRLF+cDesc2+CRLF+cDesc3 )

If TamSx3("CTT_CUSTO")[1]> 9
	nTamCdoCusto := 25
EndIf			

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Criacao da secao utilizada pelo relatorio                               ³
//³                                                                        ³
//³TRSection():New                                                         ³
//³ExpO1 : Objeto TReport que a secao pertence                             ³
//³ExpC2 : Descricao da seçao                                              ³
//³ExpA3 : Array com as tabelas utilizadas pela secao. A primeira tabela   ³
//³        sera considerada como principal para a seção.                   ³
//³ExpA4 : Array com as Ordens do relatório                                ³
//³ExpL5 : Carrega campos do SX3 como celulas                              ³
//³        Default : False                                                 ³
//³ExpL6 : Carrega ordens do Sindex                                        ³
//³        Default : False                                                 ³
//³                                                                        ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
//adiciona ordens do relatorio

oCentroCusto := TRSection():New(oReport, cSayCC, {"CTT", "CT1"}, aOrdem /*{}*/, .F., .F.)

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Criacao da celulas da secao do relatorio                                ³
//³                                                                        ³
//³TRCell():New                                                            ³
//³ExpO1 : Objeto TSection que a secao pertence                            ³
//³ExpC2 : Nome da celula do relatório. O SX3 será consultado              ³
//³ExpC3 : Nome da tabela de referencia da celula                          ³
//³ExpC4 : Titulo da celula                                                ³
//³        Default : X3Titulo()                                            ³
//³ExpC5 : Picture                                                         ³
//³        Default : X3_PICTURE                                            ³
//³ExpC6 : Tamanho                                                         ³
//³        Default : X3_TAMANHO                                            ³
//³ExpL7 : Informe se o tamanho esta em pixel                              ³
//³        Default : False                                                 ³
//³ExpB8 : Bloco de código para impressao.                                 ³
//³        Default : ExpC2                                                 ³
//³                                                                        ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
TRCell():New(oCentroCusto,	"CTT_CUSTO"	,"CTT",/*Titulo*/,/*Picture*/,nTamConta /*Tamanho*/,/*lPixel*/,/*{|| bloco-de-impressao }*/)
TRCell():New(oCentroCusto,	"CTT_DESC01","CTT",/*Titulo*/,/*Picture*/,nTamConta/*Tamanho*/,/*lPixel*/,/*{|| bloco-de-impressao }*/)

For nX := 1 To 12
	TRCell():New(oCentroCusto,	"VALOR_PER"+StrZero(nX,2),"","PERIODO "+StrZero(nX,2)/*Titulo*/,/*Picture*/,15/*Tamanho*/,/*lPixel*/,/*{|| bloco-de-impressao }*/)
Next

TRCell():New(oCentroCusto,	"VALOR_TOTAL","","TOTAL PERIODO"/*Titulo*/,/*Picture*/,15/*Tamanho*/,/*lPixel*/,/*{|| bloco-de-impressao }*/)
oCentroCusto:Cell("VALOR_TOTAL"):Disable()
oCentroCusto:Cell("CTT_CUSTO"):SetLineBreak()
oCentroCusto:Cell("CTT_DESC01"):SetLineBreak()
oCentroCusto:Cell("CTT_DESC01"):disable()
oCentroCusto:SetColSpace(0)
oCentroCusto:SetHeaderPage()
oCentroCusto:SetLineBreak()

TRPosition():New(oCentroCusto,'CT1',1,{ || xFilial('CT1')+cArqDad->CONTA } )

//-----------total do centro custo
oTotCenCusto := TRSection():New(oReport, "Total - "+AllTrim(cSayCC), {"CTT", "CT1"}, aOrdem /*{}*/, .F., .F.)	//	"Total - "

TRCell():New(oTotCenCusto,	"CTT_CUSTO"	,"CTT",/*Titulo*/,/*Picture*/,nTamConta/*Tamanho*/,/*lPixel*/,/*{|| bloco-de-impressao }*/)
TRCell():New(oTotCenCusto,	"CTT_DESC01","CTT",/*Titulo*/,/*Picture*/,nTamConta/*Tamanho*/,/*lPixel*/,/*{|| bloco-de-impressao }*/)

For nX := 1 To 12
	TRCell():New(oTotCenCusto,	"VALOR_PER"+StrZero(nX,2),"","PERIODO "+StrZero(nX,2)/*Titulo*/,/*Picture*/,15/*Tamanho*/,/*lPixel*/,/*{|| bloco-de-impressao }*/)	//	"PERIODO "
Next

TRCell():New(oTotCenCusto,	"VALOR_TOTAL","","TOTAL PERIODO"/*Titulo*/,/*Picture*/,15/*Tamanho*/,/*lPixel*/,/*{|| bloco-de-impressao }*/)	//	"TOTAL PERIODO"
oTotCenCusto:Cell("CTT_CUSTO"):SetLineBreak()
oTotCenCusto:Cell("CTT_DESC01"):SetLineBreak()
oTotCenCusto:Cell("CTT_CUSTO"):Hide()
oTotCenCusto:Cell("CTT_DESC01"):Hide()
oTotCenCusto:Cell("VALOR_TOTAL"):Disable()
oTotCenCusto:SetColSpace(0)
oTotCenCusto:SetLineBreak()
                                 
oTotCenCusto:SetNoFilter({"CTT","CT1"})

//-----------total do centro custo superior
oTotSupCusto := TRSection():New(oReport, "Total - "+AllTrim(cSayCC)+" Superior", {"CTT", "CT1"}, aOrdem /*{}*/, .F., .F.)	//	"Total - " + cSayCC + " Superior"

TRCell():New(oTotSupCusto,	"CTT_CUSTO"	,"CTT",/*Titulo*/,/*Picture*/,nTamConta/*Tamanho*/,/*lPixel*/,/*{|| bloco-de-impressao }*/)
TRCell():New(oTotSupCusto,	"CTT_DESC01","CTT",/*Titulo*/,/*Picture*/,nTamConta/*Tamanho*/,/*lPixel*/,/*{|| bloco-de-impressao }*/)

For nX := 1 To 12
	TRCell():New(oTotSupCusto,	"VALOR_PER"+StrZero(nX,2),"","PERIODO "+StrZero(nX,2)/*Titulo*/,/*Picture*/,15/*Tamanho*/,/*lPixel*/,/*{|| bloco-de-impressao }*/)	//	"PERIODO "
Next

TRCell():New(oTotSupCusto,	"VALOR_TOTAL","","TOTAL PERIODO"/*Titulo*/,/*Picture*/,15/*Tamanho*/,/*lPixel*/,/*{|| bloco-de-impressao }*/)	//	"TOTAL PERIODO"
oTotSupCusto:Cell("CTT_CUSTO"):SetLineBreak()
oTotSupCusto:Cell("CTT_DESC01"):SetLineBreak()
oTotSupCusto:Cell("CTT_CUSTO"):Hide()
oTotSupCusto:Cell("CTT_DESC01"):Hide()
oTotSupCusto:Cell("VALOR_TOTAL"):Disable()
oTotSupCusto:SetColSpace(0)
oTotSupCusto:SetLineBreak()

oTotSupCusto:SetNoFilter({"CTT","CT1"})

//---total geral
oTotGerCusto := TRSection():New(oReport, "Total Geral - "+AllTrim(cSayCC), {"CTT", "CT1"}, aOrdem /*{}*/, .F., .F.)	//	"Total Geral - "+cSayCC

TRCell():New(oTotGerCusto,	"CTT_CUSTO"	,"CTT",/*Titulo*/,/*Picture*/,nTamConta/*Tamanho*/,/*lPixel*/,/*{|| bloco-de-impressao }*/)
TRCell():New(oTotGerCusto,	"CTT_DESC01","CTT",/*Titulo*/,/*Picture*/,nTamConta/*Tamanho*/,/*lPixel*/,/*{|| bloco-de-impressao }*/)

For nX := 1 To 12
	TRCell():New(oTotGerCusto,	"VALOR_PER"+StrZero(nX,2),"","PERIODO "+StrZero(nX,2)/*Titulo*/,/*Picture*/,15/*Tamanho*/,/*lPixel*/,/*{|| bloco-de-impressao }*/)	//	"PERIODO"
Next

TRCell():New(oTotGerCusto,	"VALOR_TOTAL","","TOTAL PERIODO"/*Titulo*/,/*Picture*/,15/*Tamanho*/,/*lPixel*/,/*{|| bloco-de-impressao }*/)	//	"TOTAL PERIODO"
oTotGerCusto:Cell("CTT_CUSTO"):SetLineBreak()
oTotGerCusto:Cell("CTT_DESC01"):SetLineBreak()
oTotGerCusto:Cell("CTT_CUSTO"):Hide()
oTotGerCusto:Cell("CTT_DESC01"):Hide()
oTotGerCusto:Cell("VALOR_TOTAL"):Disable()
oTotGerCusto:SetColSpace(0)
oTotGerCusto:SetLineBreak()
                           
oTotGerCusto:SetNoFilter({"CTT","CT1"})

//-----------total do grupo
oTotGrupo := TRSection():New(oReport, "Total - "+"Grupo", {"CTT", "CT1"}, aOrdem /*{}*/, .F., .F.)

TRCell():New(oTotGrupo,	"CTT_CUSTO"	,"CTT",/*Titulo*/,/*Picture*/,nTamConta/*Tamanho*/,/*lPixel*/,/*{|| bloco-de-impressao }*/)
TRCell():New(oTotGrupo,	"CTT_DESC01","CTT",/*Titulo*/,/*Picture*/,nTamConta/*Tamanho*/,/*lPixel*/,/*{|| bloco-de-impressao }*/)

For nX := 1 To 12
	TRCell():New(oTotGrupo,	"VALOR_PER"+StrZero(nX,2),"","PERIODO "+StrZero(nX,2)/*Titulo*/,/*Picture*/,15/*Tamanho*/,/*lPixel*/,/*{|| bloco-de-impressao }*/)	//	"PERIODO "
Next

TRCell():New(oTotGrupo,	"VALOR_TOTAL","","TOTAL PERIODO"/*Titulo*/,/*Picture*/,15/*Tamanho*/,/*lPixel*/,/*{|| bloco-de-impressao }*/)
oTotGrupo:Cell("CTT_CUSTO"):SetLineBreak()
oTotGrupo:Cell("CTT_DESC01"):SetLineBreak()
oTotGrupo:Cell("CTT_CUSTO"):Hide()
oTotGrupo:Cell("CTT_DESC01"):Hide()
oTotGrupo:Cell("VALOR_TOTAL"):Disable()
oTotGrupo:SetColSpace(0)
oTotGrupo:SetLineBreak()
             
oTotGrupo:SetNoFilter({"CTT","CT1"})

Return(oReport)

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³Ctbr285R4 ºAutor  ³Paulo Carnelossi    º Data ³  16/08/06   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³Release 4                                                   º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP                                                         º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/

Static Function ReportPrint(oReport,cSayCC, cString, cTitulo)

Local oSection1 	:= oReport:Section(1)
                       
Local aSetOfBook
Local aCtbMoeda	:= {}
Local lRet			:= .T.    
Local nDivide		:= 1
Local cPicture
Local cDescMoeda
Local cCodMasc		:= ""
Local cMascara		:= ""
Local cMascCC		:= ""           
Local cSepara1		:= ""
Local cSepara2		:= ""
Local cGrupo		:= ""
Local cGrupoAnt	:= ""
Local lFirstPage	:= .T.
Local nDecimais
Local cCustoAnt		:= ""
Local cCCResAnt		:= ""
Local lImpConta		:= .F.
Local lImpCusto		:= .T.
Local cCtaIni		:= MV_PAR09
Local cCtaFim		:= MV_PAR10
Local nPosAte		:= 0
Local nDigitAte		:= 0
Local cSegAte   	:= MV_PAR17
Local cArqDad   	:= ""
Local cArqTmp		:= ""
Local cArqInd			:= CriaTrab(Nil, .F.)
Local cArqAux			:= "cArqDad"
Local cCCSup		:= ""//Centro de Custo Superior do centro de custo atual
Local cAntCCSup		:= ""//Centro de Custo Superior do centro de custo anterior
Local lPula			:= Iif(MV_PAR25==1,.T.,.F.) 
Local lPrintZero	:= Iif(MV_PAR26==1,.T.,.F.)
Local lImpAntLP		:= Iif(MV_PAR28 == 1,.T.,.F.)
Local lVlrZerado	:= Iif(MV_PAR13==1,.T.,.F.)
Local dDataLP  		:= MV_PAR29
Local aMeses		:= {}          
Local dDataFim 		:= mv_par02
Local lJaPulou		:= .F.
Local nMeses		:= 1
Local aTotCol		:= {0,0,0,0,0,0,0,0,0,0,0,0}
Local aTotCC		:= {0,0,0,0,0,0,0,0,0,0,0,0}
Local aTotCCSup		:= {}
Local aSupCC		:= {}
Local nTotLinha		:= 0
Local nCont			:= 0
Local aTotGrupo		:= {0,0,0,0,0,0,0,0,0,0,0,0}
Local nTotLinGrp	:= 0

Local lImpSint 		:= Iif(MV_PAR11 == 2,.F.,.T.)
Local lImpTotS		:= Iif(MV_PAR33 == 1,.T.,.F.)
Local lImpCCSint	:= .T.
Local lNivel1		:= .F. 

Local nPos 			:= 0
Local nDigitos 		:= 0
Local n				:= 0
Local nVezes		:= 0
Local nPosCC		:= 0 
Local nTamaTotCC	:= 0
Local nAtuTotCC		:= 0 
Local cTpComp		:= If( MV_PAR34 == 1,"M","S" )	//	Comparativo : "M"ovimento ou "S"aldo Acumulado
Local oCentroCusto  := oReport:Section(1)
Local oTotCenCusto  := oReport:Section(2)
Local oTotSupCusto  := oReport:Section(3)
Local oTotGerCusto  := oReport:Section(4)
Local oTotGrupo	 	:= oReport:Section(5)
Local cText_CC
Local cText_CCSup
Local cText_CCGer
Local cText_Grupo
Local Titulo
Local nRegTmp       := 1

PRIVATE nomeProg  	:= "CTBR285"

oReport:SetLandscape()

aSetOfBook := CTBSetOf(MV_PAR12)

If MV_PAR27 == 2			// Divide por cem
	nDivide := 100
ElseIf MV_PAR27 == 3		// Divide por mil
	nDivide := 1000
ElseIf MV_PAR27 == 4		// Divide por milhao
	nDivide := 1000000
EndIf	

If lRet
	aCtbMoeda  	:= CtbMoeda(MV_PAR14,nDivide)
	If Empty(aCtbMoeda[1])                        
      Help(" ",1,"NOMOEDA")
      oReport:CancelPrint()
      Return
   Endif
Endif


If oReport:lXlsTable 
     Alert('Formato de impressão Relatório em Formato de Tabela não suportado neste relatório')  
     oReport:CancelPrint() 
     Return 
Endif


cDescMoeda 	:= aCtbMoeda[2]
nDecimais 	:= DecimalCTB(aSetOfBook,MV_PAR14)
cPicture		:= aSetOfBook[4]

aPeriodos := ctbPeriodos(MV_PAR14, mv_par01, mv_par02, .T., .F.)

For nCont := 1 to len(aPeriodos)       
	//Se a Data do periodo eh maior ou igual a data inicial solicitada no relatorio.
	If aPeriodos[nCont][1] >= mv_par01 .And. aPeriodos[nCont][2] <= mv_par02 
		If nMeses <= 12
			AADD(aMeses,{StrZero(nMeses,2),aPeriodos[nCont][1],aPeriodos[nCont][2]})	
		EndIf
		nMeses += 1           					
	EndIf
Next                                                                   

//Mascara do Centro de Custo
If Empty(aSetOfBook[6])
	cMascCC :=  GetMv("MV_MASCCUS")
Else
	cMascCC := RetMasCtb(aSetOfBook[6],@cSepara1)
EndIf

// Mascara da Conta Contabil
If Empty(aSetOfBook[2])
	cMascara := GetMv("MV_MASCARA")
Else
	cMascara := RetMasCtb(aSetOfBook[2],@cSepara2)
EndIf


//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Carrega titulo do relatorio: Analitico / Sintetico			 ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
IF MV_PAR11 == 1
	Titulo:=	"COMPARATIVO SINTETICO POR "+ Upper(Alltrim(cSayCC)) + " / "+ " CONTA " 		//"COMPARATIVO SINTETICO DE  "
ElseIf MV_PAR11 == 2
	Titulo:=	"COMPARATIVO ANALITICO POR " + Upper(Alltrim(cSayCC)) + " / "+ " CONTA "		//"COMPARATIVO ANALITICO DE  "
ElseIf MV_PAR11 == 3
	Titulo:=	"COMPARATIVO POR " + Upper(Alltrim(cSayCC)) + " / "+ " CONTA "		//"COMPARATIVO DE  "
EndIf

Titulo += 	" DE " + DTOC(mv_par01) + " ATE " + Dtoc(mv_par02) + 	" EM " + cDescMoeda
Titulo += 	OemToAnsi(" CC ") 
If !Empty(MV_PAR03) .or. !Empty(MV_PAR04)
	if Alltrim(MV_PAR03) == Alltrim(MV_PAR04)
		Titulo += " "+Alltrim(MV_PAR03)
	Else
		Titulo += " "+Alltrim(MV_PAR03)+"-"+Alltrim(MV_PAR04)
	Endif	
Endif	
If !Empty(MV_PAR05) .or. !Empty(MV_PAR06)
	if Alltrim(MV_PAR05) == Alltrim(MV_PAR06)
		Titulo += " "+Alltrim(MV_PAR05)
	Else
		Titulo += " "+Alltrim(MV_PAR05)+"-"+Alltrim(MV_PAR06)
	Endif	
Endif	
If !Empty(MV_PAR07) .or. !Empty(MV_PAR08)
	if Alltrim(MV_PAR07) == Alltrim(MV_PAR08)
		Titulo += " "+Alltrim(MV_PAR07)
	Else
		Titulo += " "+Alltrim(MV_PAR07)+"-"+Alltrim(MV_PAR08)
	Endif	
Endif	

If MV_PAR16 > "1"			
	Titulo += " (" + Tabela("SL", MV_PAR16, .F.) + ")"
Endif

If MV_PAR34 = 2
	MV_PAR30 := 2
	Titulo := AllTrim(Titulo) + " - " + "ACUMULADO"
EndIf
oReport:SetTitle(Titulo)
oReport:SetCustomText( {|| CtCGCCabTR(,,,,,dDataFim,titulo,,,,,oReport) } )

If MV_PAR30 = 1		// Com total, nao imprime descricao
	oCentroCusto:Cell("VALOR_TOTAL"):Enable()
	oTotCenCusto:Cell("VALOR_TOTAL"):Enable()
	oTotSupCusto:Cell("VALOR_TOTAL"):Enable()
	oTotGerCusto:Cell("VALOR_TOTAL"):Enable()
	oTotGrupo:Cell("VALOR_TOTAL"):Enable()

	oCentroCusto:Cell("CTT_DESC01"):Disable()
	oTotCenCusto:Cell("CTT_DESC01"):Disable()
	oTotSupCusto:Cell("CTT_DESC01"):Disable()
	oTotGerCusto:Cell("CTT_DESC01"):Disable()
	oTotGrupo:Cell("CTT_DESC01"):Disable()
Else
	oCentroCusto:Cell("VALOR_TOTAL"):Disable()
	oTotCenCusto:Cell("VALOR_TOTAL"):Disable()
	oTotSupCusto:Cell("VALOR_TOTAL"):Disable()
	oTotGerCusto:Cell("VALOR_TOTAL"):Disable()
	oTotGrupo:Cell("VALOR_TOTAL"):Disable()

	oCentroCusto:Cell("CTT_DESC01"):Enable()
	oTotCenCusto:Cell("CTT_DESC01"):Enable()
	oTotSupCusto:Cell("CTT_DESC01"):Enable()
	oTotGerCusto:Cell("CTT_DESC01"):Enable()
	oTotGrupo:Cell("CTT_DESC01"):Enable()
Endif

For nCont := 1 to Len(aMeses)
	cabec2 := SPACE(1)+Strzero(Day(aMeses[nCont][2]),2)+"/"+Strzero(Month(aMeses[nCont][2]),2)+ " - "
	cabec2 += Strzero(Day(aMeses[nCont][3]),2)+"/"+Strzero(Month(aMeses[nCont][3]),2)
	oCentroCusto:Cell("VALOR_PER"+StrZero(nCont,2)):SetTitle("PERIODO "+StrZero(nCont,2)+CRLF+cabec2)
Next

oReport:SetPageNumber(MV_PAR15)


// Verifica Se existe filtragem Ate o Segmento
If !Empty(cSegAte)
	For n := 1 to Val(cSegAte)
		nDigitAte += Val(Subs(cMascara,n,1))	
	Next
EndIf		

If !Empty(MV_PAR18)			//// FILTRA O SEGMENTO Nº
	If Empty(MV_PAR12)		//// VALIDA SE O CÓDIGO DE CONFIGURAÇÃO DE LIVROS ESTÁ CONFIGURADO
		help("",1,"CTN_CODIGO")
	    oReport:CancelPrint()
	    Return
	Else
		If !Empty(aSetOfBook[5])
			MsgInfo("O plano gerencial ainda näo esta disponivel para este relatorio. "+CHR(10)+"Altere a configuracäo de livros...","Config. de Livros...")
		    oReport:CancelPrint()
		    Return
		Endif
	Endif
	dbSelectArea("CTM")
	dbSetOrder(1)
	If MsSeek(xFilial()+aSetOfBook[2])
		While !Eof() .And. CTM->CTM_FILIAL == xFilial() .And. CTM->CTM_CODIGO == aSetOfBook[2]
			nPos += Val(CTM->CTM_DIGITO)
			If CTM->CTM_SEGMEN == STRZERO(val(MV_PAR18),2)
				nPos -= Val(CTM->CTM_DIGITO)
				nPos ++
				nDigitos := Val(CTM->CTM_DIGITO)
				Exit
			EndIf
			dbSkip()
		EndDo
	Else
		help("",1,"CTM_CODIGO")
	    oReport:CancelPrint()
	    Return
	EndIf
EndIf

#IFNDEF TOP
	DbSelectArea("CTT")
	cFilter := oCentroCusto:GetAdvplExp('CTT')
	CTT->( dbSetFilter( { || &cFiltro }, cFiltro ) )
#ELSE
	cFilter := oCentroCusto:GetSQLExp('CTT')
#ENDIF            

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Monta Arquivo Temporario para Impressao						 ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
/*
MsgMeter({|	oMeter, oText, oDlg, lEnd | ;
				CTGerComp(oMeter, oText, oDlg, @lEnd,@cArqTmp,;
				mv_par01,mv_par02,"CT3","",mv_par05,mv_par06,mv_par03,mv_par04,,,,,mv_par10,;
				mv_par12,aSetOfBook,mv_par14,mv_par15,mv_par16,mv_par17,;
				.F.,.F.,,"CTT",lImpAntLP,dDataLP,nDivide,cTpComp,.F.,,.T.,aMeses,lVlrZerado,,,lImpSint,cString,oCentroCusto:GetAdvplExp(), (lImpTotS .And. (lCT1Sint .Or. lCTTSint) );
				,,,,,,,,,,,@cNomeTab )},;		
				STR0013,;  //"Criando Arquivo Tempor rio..."
				STR0003+Upper(Alltrim(cSayCC)) +" / " +  STR0011 )     //"Balancete Verificacao C.CUSTO / CONTA		
*/
MsgMeter({|	oMeter, oText, oDlg, lEnd | ;
				CTGerComp(oMeter, oText, oDlg, @lEnd,@cArqTmp,;
				mv_par01,mv_par02,"CT3","",MV_PAR09,MV_PAR10,mv_par03,mv_par04,,,,,MV_PAR14,;
				MV_PAR16,aSetOfBook,MV_PAR18,MV_PAR19,MV_PAR20,MV_PAR21,;
				.F.,.F.,,"CTT",lImpAntLP,dDataLP,nDivide,cTpComp,.F.,,.T.,aMeses,lVlrZerado,,,lImpSint,cString,oCentroCusto:GetAdvplExp(),lImpTotS)},;
				"Criando Arquivo Temporario... ",;  //"Criando Arquivo Tempor rio..."
				"Comparativo por "+Upper(Alltrim(cSayCC)) +" / " +  " CONTA " )     //"Balancete Verificacao C.CUSTO / CONTA				

If Select("cArqTmp") <> 0
	CriaArqDad("cArqTmp",@cArqDad,@cArqAux,@cArqInd)
	CopDados("cArqTmp","cArqDad")
	("cArqTmp")->(dbCloseArea())
Endif	


//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Monta Arquivo Temporario para Impressao						 ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

MsgMeter({|	oMeter, oText, oDlg, lEnd | ;
				CTGerComp(oMeter, oText, oDlg, @lEnd,@cArqTmp,;
				mv_par01,mv_par02,"CT3","",MV_PAR09,MV_PAR10,mv_par05,mv_par06,,,,,MV_PAR14,;
				MV_PAR16,aSetOfBook,MV_PAR18,MV_PAR19,MV_PAR20,MV_PAR21,;
				.F.,.F.,,"CTT",lImpAntLP,dDataLP,nDivide,cTpComp,.F.,,.T.,aMeses,lVlrZerado,,,lImpSint,cString,oCentroCusto:GetAdvplExp(),lImpTotS)},;
				"Criando Arquivo Temporario... ",;  //"Criando Arquivo Tempor rio..."
				"Comparativo por "+Upper(Alltrim(cSayCC)) +" / " +  " CONTA " )     //"Balancete Verificacao C.CUSTO / CONTA				

If Select("cArqTmp") <> 0
	CriaArqDad("cArqTmp",@cArqDad,@cArqAux,@cArqInd)
	CopDados("cArqTmp","cArqDad")
	("cArqTmp")->(dbCloseArea())
Endif	

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Monta Arquivo Temporario para Impressao						 ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

MsgMeter({|	oMeter, oText, oDlg, lEnd | ;
				CTGerComp(oMeter, oText, oDlg, @lEnd,@cArqTmp,;
				mv_par01,mv_par02,"CT3","",MV_PAR09,MV_PAR10,mv_par07,mv_par08,,,,,MV_PAR14,;
				MV_PAR16,aSetOfBook,MV_PAR18,MV_PAR19,MV_PAR20,MV_PAR21,;
				.F.,.F.,,"CTT",lImpAntLP,dDataLP,nDivide,cTpComp,.F.,,.T.,aMeses,lVlrZerado,,,lImpSint,cString,oCentroCusto:GetAdvplExp(),lImpTotS)},;
				"Criando Arquivo Temporario... ",;  //"Criando Arquivo Tempor rio..."
				"Comparativo por "+Upper(Alltrim(cSayCC)) +" / " +  " CONTA " )     //"Balancete Verificacao C.CUSTO / CONTA				

If Select("cArqTmp") <> 0
	CriaArqDad("cArqTmp",@cArqDad,@cArqAux,@cArqInd)
	CopDados("cArqTmp","cArqDad")
	("cArqTmp")->(dbCloseArea())
Endif	

If Select("cArqDad") == 0
	oReport:CancelPrint()
	Return
EndIf          

If MV_PAR33 == 1	//Se totaliza centro de custo 
	dbSelectArea("cArqDad")
	dbSetOrder(1)
	dbGotop()
	While!Eof()		       
		If !Empty(cArqDad->CCSUP) 
			dbSelectArea("CTT")
			dbSetOrder(1)
			If MsSeek(xFilial()+cArqDad->CCSUP)
				If Empty(CTT->CTT_CCSUP) 
					lNivel1	:= .T.
				Else
					lNivel1	:= .F.
				EndIf
			EndIf
			
			dbSelectArea("cArqDad")  
			If (( MV_PAR32 == 2 .And. TIPOCC == "2" ) .Or. (MV_PAR32 == 1 .And. TIPOCC == "1" ) .Or. (MV_PAR32 == 3 ) .Or. lNivel1) .And.;
				(( MV_PAR11 == 2 .And. TIPOCONTA == "2" ) .Or. (MV_PAR11 <> 2 .And. TIPOCONTA == "1" .And. Empty(CTASUP)))                

		
				nPosCC	:= ASCAN(aTotCCSup,{|x| x[1]==CCSUP})			
				If  nPosCC <= 0 				
	   		        aSupCC := {}
					For nVezes := 1 to Len(aMeses)	
		                aAdd(aSupCC,&("COLUNA"+Alltrim(Str(nVezes,2))))
					Next
					If Len(aMeses) < 12
						For nVezes := Len(aMeses)+1 to 12
		                	aAdd(aSupCC,0)				
						Next
					EndIf	                
					AADD(aTotCCSup,{CCSUP,aSupCC})
				Else     
					For nVezes := 1 to Len(aMeses)				
						aTotCCSup[nPosCC][2][nVezes]	+= 	&("COLUNA"+Alltrim(Str(nVezes,2)))
					Next										
				EndIf
			EndIf                
		EndIf
		dbSkip()
	End
EndIf
					
dbSelectArea("cArqDad")
dbSetOrder(1)
dbGoTop()

//Se tiver parametrizado com Plano Gerencial, exibe a mensagem que o Plano Gerencial 
//nao esta disponivel e sai da rotina.
If RecCount() == 0 .And. !Empty(aSetOfBook[5])                                       
	dbCloseArea()
	FErase(cArqDad+GetDBExtension())
	FErase("cArqInd"+OrdBagExt())
	oReport:CancelPrint()
	Return
Endif

//linha detalhe
//Se totaliza e mostra a descricao
If MV_PAR30 = 1 .And. MV_PAR31 = 2
	oCentroCusto:Cell("CTT_CUSTO"):SetBlock({||Left(cArqDad->DESCCTA,18)})
Else
	If MV_PAR24 == 1 //Codigo Normal
		oCentroCusto:Cell("CTT_CUSTO"):SetBlock({|| EntidadeCTB(Subs(cArqDad->CONTA,1,nTamConta),,,nTamConta,.F.,cMascara,cSepara2,/*cAlias*/,/*nOrder*/,.F./*lGraf*/,/*oPrint*/,.F./*lSay*/) } )	
	Else //Codigo Reduzido
		oCentroCusto:Cell("CTT_CUSTO"):SetBlock({|| EntidadeCTB(cArqDad->CTARES,,,nTamConta,.F.,cMascara,cSepara2,/*cAlias*/,/*nOrder*/,.F./*lGraf*/,/*oPrint*/,.F./*lSay*/) } )
	Endif
Endif	

// Se nao totalizar ou se totalizar e mostrar a descricao da conta
If MV_PAR30 == 2 
	oCentroCusto:Cell("CTT_DESC01"):SetBlock({|| Left(cArqDad->DESCCTA,19) })
Endif           

oCentroCusto:Cell("VALOR_PER01"):SetBlock({|| ValorCTB(cArqDad->COLUNA1,,,12,nDecimais,CtbSinalMov(),cPicture, cArqDad->NORMAL, , , , , ,lPrintZero,.F./*lSay*/) } )
oCentroCusto:Cell("VALOR_PER02"):SetBlock({|| ValorCTB(cArqDad->COLUNA2,,,12,nDecimais,CtbSinalMov(),cPicture, cArqDad->NORMAL, , , , , ,lPrintZero,.F./*lSay*/) } )
oCentroCusto:Cell("VALOR_PER03"):SetBlock({|| ValorCTB(cArqDad->COLUNA3,,,12,nDecimais,CtbSinalMov(),cPicture, cArqDad->NORMAL, , , , , ,lPrintZero,.F./*lSay*/) } )
oCentroCusto:Cell("VALOR_PER04"):SetBlock({|| ValorCTB(cArqDad->COLUNA4,,,12,nDecimais,CtbSinalMov(),cPicture, cArqDad->NORMAL, , , , , ,lPrintZero,.F./*lSay*/) } )
oCentroCusto:Cell("VALOR_PER05"):SetBlock({|| ValorCTB(cArqDad->COLUNA5,,,12,nDecimais,CtbSinalMov(),cPicture, cArqDad->NORMAL, , , , , ,lPrintZero,.F./*lSay*/) } )
oCentroCusto:Cell("VALOR_PER06"):SetBlock({|| ValorCTB(cArqDad->COLUNA6,,,12,nDecimais,CtbSinalMov(),cPicture, cArqDad->NORMAL, , , , , ,lPrintZero,.F./*lSay*/) } )
oCentroCusto:Cell("VALOR_PER07"):SetBlock({|| ValorCTB(cArqDad->COLUNA7,,,12,nDecimais,CtbSinalMov(),cPicture, cArqDad->NORMAL, , , , , ,lPrintZero,.F./*lSay*/) } )
oCentroCusto:Cell("VALOR_PER08"):SetBlock({|| ValorCTB(cArqDad->COLUNA8,,,12,nDecimais,CtbSinalMov(),cPicture, cArqDad->NORMAL, , , , , ,lPrintZero,.F./*lSay*/) } )
oCentroCusto:Cell("VALOR_PER09"):SetBlock({|| ValorCTB(cArqDad->COLUNA9,,,12,nDecimais,CtbSinalMov(),cPicture, cArqDad->NORMAL, , , , , ,lPrintZero,.F./*lSay*/) } )
oCentroCusto:Cell("VALOR_PER10"):SetBlock({|| ValorCTB(cArqDad->COLUNA10,,,12,nDecimais,CtbSinalMov(),cPicture, cArqDad->NORMAL, , , , , ,lPrintZero,.F./*lSay*/) } )
oCentroCusto:Cell("VALOR_PER11"):SetBlock({|| ValorCTB(cArqDad->COLUNA11,,,12,nDecimais,CtbSinalMov(),cPicture, cArqDad->NORMAL, , , , , ,lPrintZero,.F./*lSay*/) } )
oCentroCusto:Cell("VALOR_PER12"):SetBlock({|| ValorCTB(cArqDad->COLUNA12,,,12,nDecimais,CtbSinalMov(),cPicture, cArqDad->NORMAL, , , , , ,lPrintZero,.F./*lSay*/) } )

If MV_PAR30 == 1   
	oCentroCusto:Cell("VALOR_TOTAL"):enable()
	oCentroCusto:Cell("VALOR_TOTAL"):SetBlock({|| ValorCTB(nTotLinha,,,12,nDecimais,CtbSinalMov(),cPicture, cArqDad->NORMAL, , , , , ,lPrintZero,.F./*lSay*/) } )
EndIf
//------------------total do centro de custo
If MV_PAR30 == 2
	If MV_PAR23 == 2	.And. cArqDad->TIPOCC == '2'//Se Imprime cod. reduzido do centro de Custo e eh analitico
		oTotCenCusto:Cell("CTT_CUSTO"):SetBlock({|| "" })
		oTotCenCusto:Cell("CTT_DESC01"):SetBlock({|| "TOTAIS DO "+ Upper(Alltrim(cSayCC))+ " : " +EntidadeCTB(cCCResAnt,,,nTamCdoCusto,.F.,cMascCC,cSepara1,/*cAlias*/,/*nOrder*/,.F./*lGraf*/,/*oPrint*/,.F./*lSay*/) })
	Else //Se Imprime cod. normal do Centro de Custo
		oTotCenCusto:Cell("CTT_CUSTO"):SetBlock({|| "" })
		oTotCenCusto:Cell("CTT_DESC01"):SetBlock({|| "TOTAIS DO "+ Upper(Alltrim(cSayCC))+ " : "+ EntidadeCTB(cCustoAnt,,,nTamCdoCusto,.F.,cMascCC,cSepara1,/*cAlias*/,/*nOrder*/,.F./*lGraf*/,/*oPrint*/,.F./*lSay*/)	 })
	Endif 	   
Else
	oTotCenCusto:Cell("CTT_CUSTO"):SetBlock({|| "" })
	oTotCenCusto:Cell("CTT_DESC01"):SetBlock({||"TOTAIS: "+;
										If(MV_PAR23 == 2 .And. cArqDad->TIPOCC == '2'/*Se Imprime cod. reduzido do centro de Custo e eh analitico*/,;
												Subs(cCCResAnt,1,10),;
												Subs(cCustoAnt,1,10);
											) })  //"TOTAIS: "						
EndIf		
oTotCenCusto:Cell("VALOR_PER01"):SetBlock({|| ValorCTB(aTotCC[ 1],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotCenCusto:Cell("VALOR_PER02"):SetBlock({|| ValorCTB(aTotCC[ 2],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotCenCusto:Cell("VALOR_PER03"):SetBlock({|| ValorCTB(aTotCC[ 3],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotCenCusto:Cell("VALOR_PER04"):SetBlock({|| ValorCTB(aTotCC[ 4],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotCenCusto:Cell("VALOR_PER05"):SetBlock({|| ValorCTB(aTotCC[ 5],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotCenCusto:Cell("VALOR_PER06"):SetBlock({|| ValorCTB(aTotCC[ 6],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotCenCusto:Cell("VALOR_PER07"):SetBlock({|| ValorCTB(aTotCC[ 7],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotCenCusto:Cell("VALOR_PER08"):SetBlock({|| ValorCTB(aTotCC[ 8],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotCenCusto:Cell("VALOR_PER09"):SetBlock({|| ValorCTB(aTotCC[ 9],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotCenCusto:Cell("VALOR_PER10"):SetBlock({|| ValorCTB(aTotCC[10],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotCenCusto:Cell("VALOR_PER11"):SetBlock({|| ValorCTB(aTotCC[11],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotCenCusto:Cell("VALOR_PER12"):SetBlock({|| ValorCTB(aTotCC[12],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )

If MV_PAR30 == 1   
	oTotCenCusto:Cell("VALOR_TOTAL"):enable()
	oTotCenCusto:Cell("VALOR_TOTAL"):SetBlock({|| ValorCTB(nTotLinha,,,12,nDecimais,CtbSinalMov(),cPicture, cArqDad->NORMAL, , , , , ,lPrintZero,.F./*lSay*/) } )
EndIf
//------------------total do centro de custo superior
If MV_PAR30 == 2
	If MV_PAR23 == 2	.And. cArqDad->TIPOCC == '2'//Se Imprime cod. reduzido do centro de Custo e eh analitico
		oTotSupCusto:Cell("CTT_CUSTO"):SetBlock({|| "TOTAIS DO "+ Upper(Alltrim(cSayCC))+ " : "+EntidadeCTB(cCCResAnt,,,nTamCdoCusto,.F.,cMascCC,cSepara1,/*cAlias*/,/*nOrder*/,.F./*lGraf*/,/*oPrint*/,.F./*lSay*/) })
	Else //Se Imprime cod. normal do Centro de Custo
		oTotSupCusto:Cell("CTT_CUSTO"):SetBlock({|| "TOTAIS DO "+ Upper(Alltrim(cSayCC))+ " : "+EntidadeCTB(cAntCCSup,,,nTamCdoCusto,.F.,cMascCC,cSepara1,/*cAlias*/,/*nOrder*/,.F./*lGraf*/,/*oPrint*/,.F./*lSay*/) })
	Endif
Else
	If MV_PAR23 == 2	.And. cArqDad->TIPOCC == '2'//Se Imprime cod. reduzido do centro de Custo e eh analitico
		oTotSupCusto:Cell("CTT_CUSTO"):SetBlock({|| "TOTAIS: " + Subs(cCCResAnt,1,10)})
	Else //Se Imprime cod. normal do Centro de Custo
		oTotSupCusto:Cell("CTT_CUSTO"):SetBlock({|| "TOTAIS: " + Subs(cAntCCSup,1,10)})
	Endif								
EndIf
oTotSupCusto:Cell("VALOR_PER01"):SetBlock({|| ValorCTB(aTotCCSup[nPosCC][2][ 1],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotSupCusto:Cell("VALOR_PER02"):SetBlock({|| ValorCTB(aTotCCSup[nPosCC][2][ 2],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotSupCusto:Cell("VALOR_PER03"):SetBlock({|| ValorCTB(aTotCCSup[nPosCC][2][ 3],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotSupCusto:Cell("VALOR_PER04"):SetBlock({|| ValorCTB(aTotCCSup[nPosCC][2][ 4],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotSupCusto:Cell("VALOR_PER05"):SetBlock({|| ValorCTB(aTotCCSup[nPosCC][2][ 5],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotSupCusto:Cell("VALOR_PER06"):SetBlock({|| ValorCTB(aTotCCSup[nPosCC][2][ 6],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotSupCusto:Cell("VALOR_PER07"):SetBlock({|| ValorCTB(aTotCCSup[nPosCC][2][ 7],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotSupCusto:Cell("VALOR_PER08"):SetBlock({|| ValorCTB(aTotCCSup[nPosCC][2][ 8],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotSupCusto:Cell("VALOR_PER09"):SetBlock({|| ValorCTB(aTotCCSup[nPosCC][2][ 9],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotSupCusto:Cell("VALOR_PER10"):SetBlock({|| ValorCTB(aTotCCSup[nPosCC][2][10],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotSupCusto:Cell("VALOR_PER11"):SetBlock({|| ValorCTB(aTotCCSup[nPosCC][2][11],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotSupCusto:Cell("VALOR_PER12"):SetBlock({|| ValorCTB(aTotCCSup[nPosCC][2][12],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )

If MV_PAR30 == 1   
	oTotSupCusto:Cell("VALOR_TOTAL"):enable()
	oTotSupCusto:Cell("VALOR_TOTAL"):SetBlock({|| ValorCTB(nTotLinha,,,12,nDecimais,CtbSinalMov(),cPicture, cArqDad->NORMAL, , , , , ,lPrintZero,.F./*lSay*/) } )
EndIf

//------------------total Geral
If MV_PAR30 == 2
	oTotGerCusto:Cell("CTT_CUSTO"):SetBlock({|| "T O T A I S  D O  P E R I O D O: " })   //"T O T A I S  D O  P E R I O D O : "
Else //Se Imprime cod. normal do Centro de Custo
	oTotGerCusto:Cell("CTT_CUSTO"):SetBlock({|| "TOTAIS DO PERIODO:" })   //"TOTAIS  DO  PERIODO: "
EndIf
oTotGerCusto:Cell("VALOR_PER01"):SetBlock({|| ValorCTB(aTotCol[ 1],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotGerCusto:Cell("VALOR_PER02"):SetBlock({|| ValorCTB(aTotCol[ 2],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotGerCusto:Cell("VALOR_PER03"):SetBlock({|| ValorCTB(aTotCol[ 3],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotGerCusto:Cell("VALOR_PER04"):SetBlock({|| ValorCTB(aTotCol[ 4],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotGerCusto:Cell("VALOR_PER05"):SetBlock({|| ValorCTB(aTotCol[ 5],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotGerCusto:Cell("VALOR_PER06"):SetBlock({|| ValorCTB(aTotCol[ 6],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotGerCusto:Cell("VALOR_PER07"):SetBlock({|| ValorCTB(aTotCol[ 7],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotGerCusto:Cell("VALOR_PER08"):SetBlock({|| ValorCTB(aTotCol[ 8],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotGerCusto:Cell("VALOR_PER09"):SetBlock({|| ValorCTB(aTotCol[ 9],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotGerCusto:Cell("VALOR_PER10"):SetBlock({|| ValorCTB(aTotCol[10],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotGerCusto:Cell("VALOR_PER11"):SetBlock({|| ValorCTB(aTotCol[11],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotGerCusto:Cell("VALOR_PER12"):SetBlock({|| ValorCTB(aTotCol[12],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )

If MV_PAR30 == 1   
	oTotGerCusto:Cell("VALOR_TOTAL"):enable()
	oTotGerCusto:Cell("VALOR_TOTAL"):SetBlock({|| ValorCTB(nTotGeral,,,12,nDecimais,CtbSinalMov(),cPicture, cArqDad->NORMAL, , , , , ,lPrintZero,.F./*lSay*/) } )
EndIf


//------------------total do grupo
If MV_PAR35 == 1
	oTotGrupo:Cell("CTT_CUSTO"):SetBlock({|| "" })
	oTotGrupo:Cell("CTT_DESC01"):SetBlock({|| "GRUPO (" + Left(cGrupo,10) + ")" })		//"GRUPO ("
EndIf
oTotGrupo:Cell("VALOR_PER01"):SetBlock({|| ValorCTB(aTotGrupo[ 1],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotGrupo:Cell("VALOR_PER02"):SetBlock({|| ValorCTB(aTotGrupo[ 2],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotGrupo:Cell("VALOR_PER03"):SetBlock({|| ValorCTB(aTotGrupo[ 3],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotGrupo:Cell("VALOR_PER04"):SetBlock({|| ValorCTB(aTotGrupo[ 4],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotGrupo:Cell("VALOR_PER05"):SetBlock({|| ValorCTB(aTotGrupo[ 5],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotGrupo:Cell("VALOR_PER06"):SetBlock({|| ValorCTB(aTotGrupo[ 6],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotGrupo:Cell("VALOR_PER07"):SetBlock({|| ValorCTB(aTotGrupo[ 7],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotGrupo:Cell("VALOR_PER08"):SetBlock({|| ValorCTB(aTotGrupo[ 8],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotGrupo:Cell("VALOR_PER09"):SetBlock({|| ValorCTB(aTotGrupo[ 9],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotGrupo:Cell("VALOR_PER10"):SetBlock({|| ValorCTB(aTotGrupo[10],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotGrupo:Cell("VALOR_PER11"):SetBlock({|| ValorCTB(aTotGrupo[11],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
oTotGrupo:Cell("VALOR_PER12"):SetBlock({|| ValorCTB(aTotGrupo[12],,,12,nDecimais,CtbSinalMov(),cPicture, , , , , , ,lPrintZero,.F./*lSay*/) } )
If MV_PAR30 == 1
	oTotGrupo:Cell("VALOR_TOTAL"):enable()
	oTotGrupo:Cell("VALOR_TOTAL"):SetBlock({|| ValorCTB(nTotLinGrp,,,12,nDecimais,CtbSinalMov(),cPicture, cArqDad->NORMAL, , , , , ,lPrintZero,.F./*lSay*/) } )
EndIf

//------------------inicio do relatorio
dbSelectArea("cArqDad")
dbGoTop()
oReport:SetMeter(RecCount())
oCentroCusto:Init()

cGrupo    := cArqDad->GRUPO

While !Eof()

	If oReport:Cancel()
		Exit
	EndIF

	oReport:IncMeter()

	cText_CC 	:= ""
	cText_CCSup := ""
	cText_CCGer	:= ""
	cText_Grupo	:= ""

	******************** "FILTRAGEM" PARA IMPRESSAO *************************
	

	If MV_PAR32 == 1					// So imprime Sinteticas
		If TIPOCC == "2"
			dbSkip()
			Loop
		EndIf
	ElseIf MV_PAR32 == 2				// So imprime Analiticas
		If TIPOCC == "1"
			dbSkip()
			Loop
		EndIf
	EndIf	

	If MV_PAR11 == 1					// So imprime Sinteticas
		If TIPOCONTA == "2"
			dbSkip()
			Loop
		EndIf
	ElseIf MV_PAR11 == 2				// So imprime Analiticas
		If TIPOCONTA == "1"
			dbSkip()
			Loop
		EndIf
	EndIf
	
	//Filtragem ate o Segmento ( antigo nivel do SIGACON)		
	If !Empty(cSegAte)
		If Len(Alltrim(CONTA)) > nDigitAte
			dbSkip()
			Loop
		Endif
	EndIf

	dbSelectArea("cArqDad")
	If lVlrZerado	.And. ;
	(Abs(COLUNA1)+Abs(COLUNA2)+Abs(COLUNA3)+Abs(COLUNA4)+Abs(COLUNA5)+Abs(COLUNA6)+Abs(COLUNA7)+Abs(COLUNA8)+;
		Abs(COLUNA9)+Abs(COLUNA10)+Abs(COLUNA11)+Abs(COLUNA12)) == 0	    
			If CtbExDtFim("CTT")  
				dbSelectArea("CTT")
				dbSetOrder(1)
				If MsSeek(xFilial()+ cArqDad->CUSTO)			
					If !CtbVlDtFim("CTT",mv_par01) 
						dbSelectArea("cArqDad")
						dbSkip()
						Loop
					EndIf			
				EndIf
			EndIf			
			
			If CtbExDtFim("CT1")
				dbSelectArea("CT1")
				dbSetOrder(1)
				If MsSeek(xFilial()+ cArqDad->CONTA)
					If !CtbVlDtFim("CT1",mv_par01) 
						dbSelectArea("cArqDad")
						dbSkip()			
						Loop
					EndIf						
				EndIf
			EndIf
		dbSelectArea("cArqDad")
	EndIf
		
	//Caso faca filtragem por segmento de Conta,verifico se esta dentro 
	//da solicitacao feita pelo usuario. 
	If !Empty(MV_PAR18)
		If Empty(MV_PAR19) .And. Empty(MV_PAR20) .And. !Empty(MV_PAR21)
			If  !(Substr(cArqDad->CONTA,nPos,nDigitos) $ (MV_PAR21) ) 
				dbSkip()
				Loop
			EndIf	
		Else
			If Substr(cArqDad->CONTA,nPos,nDigitos) < Alltrim(MV_PAR19) .Or. Substr(cArqDad->CONTA,nPos,nDigitos) > Alltrim(MV_PAR20)
				dbSkip()
				Loop
			EndIf	
		Endif
	EndIf	                                        
	
	************************* ROTINA DE IMPRESSAO *************************

	If MV_PAR35 == 1														// Quebra por Grupo Contabil

		If !lFirstPage .And.;
		   (cGrupo <> cArqDad->GRUPO) .Or.;										// Grupo Diferente ou
		   ((cCustoAnt <> cArqDad->CUSTO) .And. ! Empty(cCustoAnt))				// Centro de Custo Diferente

			oReport:ThinLine()

			nTotLinGrp	:= 0
			For nVezes := 1 to Len(aTotGrupo)
				nTotLinGrp	+= aTotGrupo[nVezes]
			Next

			oTotGrupo:Init()
			oTotGrupo:lPrintHeader := .F.
			cText_Grupo := "GRUPO (" + Left(cGrupo,10) + ")"		//"GRUPO ("

        	oReport:PrintText(cText_Grupo, oReport:Row(), 5)
			oReport:SkipLine()
	        oReport:ThinLine()
			oTotGrupo:PrintLine()
			oTotGrupo:Finish()

			cGrupo		:= cArqDad->GRUPO
			aTotGrupo	:= {0,0,0,0,0,0,0,0,0,0,0,0}
		EndIf

	Else																					// Nao quebra por Grupo

		If (cCustoAnt <> cArqDad->CUSTO) .And. ! Empty(cCustoAnt)
			oReport:ThinLine()
			
			dbSelectArea("CTT")
			dbSetOrder(1)
			If MsSeek(xFilial()+cArqDad->CUSTO)
				cCCSup	:= CTT->CTT_CCSUP
			Else
				cCCSup	:= ""
			EndIf
			
			dbSelectArea("CTT")
			dbSetOrder(1)
			If MsSeek(xFilial()+cCustoAnt)
				cAntCCSup	:= CTT->CTT_CCSUP
			Else
				cAntCCSup	:= ""
			EndIf
	
			//Total da Linha 
			nTotLinha	:= 0     			// Incluso esta linha para impressao dos totais 
			For nVezes := 1 to Len(aMeses)	// por periodo em 09/06/2004 por Otacilio
				nTotLinha	+= aTotCC[nVezes]
			Next
	        
	        oTotCenCusto:Init()
	        oTotCenCusto:lPrintHeader := .F.
	
			If MV_PAR30 == 2
				cText_CC := "TOTAIS DO "+ Upper(Alltrim(cSayCC))+ " : " +EntidadeCTB( If(MV_PAR23 == 2 .And. cArqDad->TIPOCC == '2' /* Se Imprime cod. reduzido do centro de Custo e eh analitico */,;
				                                                                 cCCResAnt,cCustoAnt),,,nTamCdoCusto,.F.,cMascCC,cSepara1,/*cAlias*/,/*nOrder*/,.F./*lGraf*/,/*oPrint*/,.F./*lSay*/)
			Else
		        cText_CC := "TOTAIS: "+If( MV_PAR23 == 2 .And. cArqDad->TIPOCC == '2'/*Se Imprime cod. reduzido do centro de Custo e eh analitico*/,;
			                            Subs(cCCResAnt,1,10),;
										Subs(cCustoAnt,1,10))
			EndIf		
	
	        oReport:PrintText(cText_CC, oReport:Row(), 5)
	        oReport:SkipLine()
	        oReport:ThinLine()
	        oTotCenCusto:PrintLine()
	        //oReport:SkipLine()
	        oTotCenCusto:Finish()
	
			aTotCC 	:= {0,0,0,0,0,0,0,0,0,0,0,0}
	
			If lImpTotS .And. cCCSup <> cAntCCSup .And. !Empty(cAntCCSup) //Se for centro de custo superior diferente
				oReport:SkipLine()
	
				//Total da Linha
				nTotLinha	:= 0     			// Incluso esta linha para impressao dos totais
	
				nPosCC	:= ASCAN(aTotCCSup,{|x| x[1]== cAntCCSup })
				If  nPosCC > 0
					For nVezes := 1 to Len(aMeses)	// por periodo em 09/06/2004 por Otacilio
						nTotLinha	+= aTotCCSup[nPosCC][2][nVezes]
					Next
			        oTotSupCusto:Init()
					oTotSupCusto:lPrintHeader := .F.
					If MV_PAR30 == 2
						cText_CCSup := "TOTAIS DO "+Upper(Alltrim(cSayCC))+ " : "+EntidadeCTB( If(MV_PAR23 == 2,cCCResAnt,cAntCCSup) ,,,nTamCdoCusto,.F.,cMascCC,cSepara1,/*cAlias*/,/*nOrder*/,.F./*lGraf*/,/*oPrint*/,.F./*lSay*/)
					Else
						cText_CCSup :=  "TOTAIS: " + Subs( IF(MV_PAR23 == 2 .And. cArqDad->TIPOCC == '2' /*Se Imprime cod. reduzido do centro de Custo e eh analitico*/,;
						                                   cCCResAnt,;
						                                   cAntCCSup) ,1,10)
					EndIf
	
			        oReport:PrintText(cText_CCSup, oReport:Row(), 5)
	        		oReport:SkipLine()
	        		oReport:ThinLine()
			        oTotSupCusto:PrintLine()
			        oTotSupCusto:Finish()
	
					dbSelectArea("cArqDad")
					nRegTmp	:= Recno()
					dbSelectArea("CTT")
					lImpCCSint	:= .T.
				EndIf
	
				While lImpCCSint
					dbSelectArea("CTT")
					If MsSeek(xFilial()+cAntCCSup) .And. !Empty(CTT->CTT_CCSUP)
						cAntCCSup	:= CTT->CTT_CCSUP
						dbSelectArea("cArqDad")
						
						//Total da Linha
						nTotLinha	:= 0     			// Incluso esta linha para impressao dos totais
						nPosCC	:= ASCAN(aTotCCSup,{|x| x[1]== cAntCCSup })			
						If  nPosCC > 0 							
							For nVezes := 1 to Len(aMeses)	// por periodo em 09/06/2004 por Otacilio
								nTotLinha	+= aTotCCSup[nPosCC][2][nVezes]
							Next
					        oTotSupCusto:Init()
							oTotSupCusto:lPrintHeader := .F.
					        
							If MV_PAR30 == 2
								cText_CCSup := "TOTAIS DO "+Upper(Alltrim(cSayCC))+ " : "+EntidadeCTB( If(MV_PAR23 == 2,cCCResAnt,cAntCCSup) ,,,nTamCdoCusto,.F.,cMascCC,cSepara1,/*cAlias*/,/*nOrder*/,.F./*lGraf*/,/*oPrint*/,.F./*lSay*/)
							Else
								cText_CCSup :=  "TOTAIS: " + Subs( IF(MV_PAR23 == 2 .And. cArqDad->TIPOCC == '2' /*Se Imprime cod. reduzido do centro de Custo e eh analitico*/,;
								                                   cCCResAnt,;
								                                   cAntCCSup) ,1,10)
							EndIf
	
					        oReport:PrintText(cText_CCSup, oReport:Row(), 5)
					        oReport:SkipLine()
	        				oReport:ThinLine()
					        oTotSupCusto:PrintLine()
	
			        		oTotSupCusto:Finish()
							lImpCCSint	:= .T. 
						EndIF
					Else
						lImpCCSint	:= .F.
					EndIf
				End
				cAntCCSup		:= ""
				cCCSup			:= ""
				dbSelectArea("cArqDad")
				dbGoto(nRegTmp)
			EndIf
		Endif
	    
	EndIf

	If MV_PAR35 == 1									// Quebra por Grupo Contabil
		If cGrupo != cArqDad->GRUPO .And. !lFirstPage		// Grupo Diferente
			oReport:EndPage()
		EndIf
	Else
		If MV_PAR22 == 1 .And. ! Empty(cCustoAnt)
			If cCustoAnt <> cArqDad->CUSTO //Se o CC atual for diferente do CC anterior
				oReport:EndPage()
			EndIf
		Endif
	EndIf


	//Se mudar de centro de custo
	If 	(cArqDad->CUSTO <> cCustoAnt .And. !Empty(cCustoAnt)) .Or. lFirstPage .Or. ;
		(MV_PAR35 == 1 .And. cGrupoAnt <> cArqDad->GRUPO)

		
		//Imprime titulo do centro de custo
		oReport:SkipLine()
		oReport:ThinLine()

		//Imprime titulo do centro de custo
		//oReport:PrintText(Upper(cSayCC), oReport:Row(), oReport:Col())
		//If MV_PAR23 == 2 .And. cArqDad->TIPOCC == '2'//Se Imprime Cod Reduzido do C.Custo e eh analitico
		//	oReport:PrintText(EntidadeCTB(CCRES,,,25,.F.,cMascCC,cSepara1,/*cAlias*/,/*nOrder*/,.F./*lGraf*/,/*oPrint*/,.F./*lSay*/), oReport:Row(), oReport:Col()+200)
		//Else //Se Imprime Cod. Normal do C.Custo
		//	oReport:PrintText(EntidadeCTB(CUSTO,,,25,.F.,cMascCC,cSepara1,/*cAlias*/,/*nOrder*/,.F./*lGraf*/,/*oPrint*/,.F./*lSay*/), oReport:Row(), oReport:Col()+200)
		//Endif
		oReport:PrintText(cArqDad->DESCCC, oReport:Row(), oReport:Col()+500)

		oReport:SkipLine()
		oReport:ThinLine()
		
		lFirstPage := .F.
	Endif


	dbSelectArea("cArqDad")
	//Total da Linha
	nTotLinha	:= COLUNA1+COLUNA2+COLUNA3+COLUNA4+COLUNA5+COLUNA6+COLUNA7+COLUNA8+COLUNA9+COLUNA10+COLUNA11+COLUNA12	
    oCentroCusto:PrintLine()

	lJaPulou := .F.
	If lPula .And. TIPOCONTA == "1"				// Pula linha entre sinteticas
		oReport:SkipLine()
		oReport:SkipLine()
		lJaPulou := .T.
	Else
		oReport:SkipLine()
	EndIf			

	************************* FIM   DA  IMPRESSAO *************************

	If MV_PAR11 != 1					// Imprime Analiticas ou Ambas
		If TIPOCONTA == "2"
			If (MV_PAR32 != 1 .And. TIPOCC == "2")
				For nVezes := 1 to Len(aMeses)
					aTotCol[nVezes]	+=&("COLUNA"+Alltrim(Str(nVezes,2)))					
				Next
			ElseIf (MV_PAR32 == 1 .And. cArqDad->TIPOCC != "2"	)	//Imprime centro de custo sintetico
				If MV_PAR11 == 2 	//Imprime contas analiticas
					For nVezes := 1 to Len(aMeses)            
						If Empty(CCSUP)
							aTotCol[nVezes]	+=&("COLUNA"+Alltrim(Str(nVezes,2)))					
						EndIf
					Next         
				ElseIf MV_PAR11 == 3	//Imprime contas sinteticas e analiticas
					If Empty(CCSUP)      //Somar somente o centro de custo sintetico
						For nVezes := 1 to Len(aMeses)
							aTotCol[nVezes]	+=&("COLUNA"+Alltrim(Str(nVezes,2)))											
						Next         					
					EndIf				
				EndIf
			EndIf	
			For nVezes := 1 to Len(aMeses)
				aTotCC[nVezes] 		+=&("COLUNA"+Alltrim(Str(nVezes,2)))									
				aTotGrupo[nVezes]	+=&("COLUNA"+Alltrim(Str(nVezes,2)))
			Next	
		Endif
	Else
		If (TIPOCONTA == "1" .And. Empty(CTASUP))
			If (MV_PAR32 != 1 .And. cArqDad->TIPOCC == "2")
				For nVezes := 1 to Len(aMeses)
					aTotCol[nVezes] 	+=&("COLUNA"+Alltrim(Str(nVezes,2)))					
				Next
			ElseIf (MV_PAR32 == 1 .And. cArqDad->TIPOCC != "2"	)
				If Empty(CCSUP)
					For nVezes := 1 to Len(aMeses)
						aTotCol[nVezes] 	+=&("COLUNA"+Alltrim(Str(nVezes,2)))			
					Next
				EndIf
			EndIf	
			For nVezes := 1 to Len(aMeses)
				aTotCC[nVezes] 		+=&("COLUNA"+Alltrim(Str(nVezes,2)))
				aTotGrupo[nVezes]	+=&("COLUNA"+Alltrim(Str(nVezes,2)))
			Next
		EndIf		
	Endif

	cCustoAnt := cArqDad->CUSTO
	cCCResAnt := cArqDad->CCRES
	cGrupoAnt := cArqDad->GRUPO
	cGrupo 	  := cArqDad->GRUPO

	dbSelectarea("cArqDad")
	dbSkip()
	
	If lPula .And. TIPOCONTA == "1" 			// Pula linha entre sinteticas
		If !lJaPulou
			oReport:SkipLine()
		EndIf	
	EndIf	
EndDO

oCentroCusto:Finish()


IF MV_PAR35 == 1				// Quebra por Grupo Contabil
    If cGrupo <> cArqDad->GRUPO
		oReport:ThinLine()
	
		nTotLinGrp	:= 0
		For nVezes := 1 to Len(aTotGrupo)
			nTotLinGrp	+= aTotGrupo[nVezes]
		Next
	
		oTotGrupo:Init()
		oTotGrupo:lPrintHeader := .F.
		cText_Grupo := "GRUPO (" + Left(cGrupo,10) + ")"		//"GRUPO ("
	
		oReport:PrintText(cText_Grupo, oReport:Row(), 5)
		oReport:SkipLine()
		oReport:ThinLine()
		oTotGrupo:PrintLine()
		oTotGrupo:Finish()
    EndIf
ELSE

	//Imprime o total do ultimo Conta a ser impresso.
	oReport:ThinLine()
	
	dbSelectArea("CTT")
	dbSetOrder(1)
	If MsSeek(xFilial("CTT")+cArqDad->CUSTO)
		cCCSup	:= CTT->CTT_CCSUP	//Centro de Custo Superior
	Else
		cCCSup	:= ""
	EndIf
	
	If MsSeek(xFilial("CTT")+cCustoAnt)
		cAntCCSup := CTT->CTT_CCSUP	//Centro de Custo Superior do Centro de custo anterior.
		cCCRes	  := CTT->CTT_RES
	Else
		cAntCCSup := ""
	EndIf
	
	dbSelectArea("cArqDad")
	
	//Total da Linha
	nTotLinha	:= 0
	For nVezes := 1 to Len(aMeses)
		nTotLinha	+= aTotCC[nVezes]
	Next
	
	oTotCenCusto:Init()
	oTotCenCusto:lPrintHeader := .F.
	
	If MV_PAR30 == 2
		cText_CC := "TOTAIS DO "+ Upper(Alltrim(cSayCC))+ " : " +EntidadeCTB( If(MV_PAR23 == 2 .And. cArqDad->TIPOCC == '2' /*Se Imprime cod. reduzido do centro de Custo e eh analitico*/,;
		                                                                 cCCResAnt,cCustoAnt),,,nTamCdoCusto,.F.,cMascCC,cSepara1,/*cAlias*/,/*nOrder*/,.F./*lGraf*/,/*oPrint*/,.F./*lSay*/)
	Else
	    cText_CC := "TOTAIS: "+If( MV_PAR23 == 2 .And. cArqDad->TIPOCC == '2'/*Se Imprime cod. reduzido do centro de Custo e eh analitico*/,;
	                            Subs(cCCResAnt,1,10),;
								Subs(cCustoAnt,1,10))
	EndIf		
	oReport:PrintText(cText_CC, oReport:Row(), 5)
	oReport:SkipLine()
	oReport:ThinLine()
	oTotCenCusto:PrintLine()
	
	oTotCenCusto:Finish()
	
	If (cArqDad->TIPOCC == "1" .And. !Empty(cArqDad->CCSUP)) .Or. (cArqDad->TIPOCC == "2")	
		aTotCC 	:= {0,0,0,0,0,0,0,0,0,0,0,0}
	EndIf
	
	If lImpTotS .And. cCCSup <> cAntCCSup .And. !Empty(cAntCCSup) //Se for centro de custo superior diferente
	
		//Total da Linha
		nTotLinha	:= 0     			// Incluso esta linha para impressao dos totais
		nPosCC	:= ASCAN(aTotCCSup,{|x| x[1]== cAntCCSup })			
		If  nPosCC > 0 							
			For nVezes := 1 to Len(aMeses)	// por periodo em 09/06/2004 por Otacilio
				nTotLinha	+= aTotCCSup[nPosCC][2][nVezes]
			Next
	
			oTotSupCusto:Init()
			oTotSupCusto:lPrintHeader := .F.

			If MV_PAR30 == 2
				cText_CCSup := "TOTAIS DO "+Upper(Alltrim(cSayCC))+ " : "+EntidadeCTB( If(MV_PAR23 == 2,cCCResAnt,cAntCCSup) ,,,nTamCdoCusto,.F.,cMascCC,cSepara1,/*cAlias*/,/*nOrder*/,.F./*lGraf*/,/*oPrint*/,.F./*lSay*/)
			Else
				cText_CCSup :=  "TOTAIS: " + Subs( IF(MV_PAR23 == 2 .And. cArqDad->TIPOCC == '2' /* Se Imprime cod. reduzido do centro de Custo e eh analitico*/,;
				                                   cCCResAnt,;
				                                   cAntCCSup) ,1,10)
			EndIf
	
	        oReport:PrintText(cText_CCSup, oReport:Row(), 5)
	        oReport:SkipLine()
			oReport:ThinLine()
			oTotSupCusto:PrintLine()	
			oTotSupCusto:Finish()

			dbSelectArea("CTT")
			lImpCCSint	:= .T.
	    EndIf
		
		While lImpCCSint
			dbSelectArea("CTT")
			If MsSeek(xFilial()+cAntCCSup) .And. !Empty(CTT->CTT_CCSUP)
				cAntCCSup	:= CTT->CTT_CCSUP
				dbSelectArea("cArqDad")
				
				//Total da Linha
				nTotLinha	:= 0     			// Incluso esta linha para impressao dos totais
				nPosCC	:= ASCAN(aTotCCSup,{|x| x[1]== cAntCCSup })			
				If  nPosCC > 0 							
					For nVezes := 1 to Len(aMeses)	// por periodo em 09/06/2004 por Otacilio
						nTotLinha	+= aTotCCSup[nPosCC][2][nVezes]
					Next
					
					oTotSupCusto:Init()
					oTotSupCusto:lPrintHeader := .F.

					If MV_PAR30 == 2
						cText_CCSup := "TOTAIS DO "+Upper(Alltrim(cSayCC))+ " : "+EntidadeCTB( If(MV_PAR23 == 2,cCCResAnt,cAntCCSup) ,,,nTamCdoCusto,.F.,cMascCC,cSepara1,/*cAlias*/,/*nOrder*/,.F./*lGraf*/,/*oPrint*/,.F./*lSay*/)
					Else
						cText_CCSup :=  "TOTAIS: " + Subs( IF(MV_PAR23 == 2 .And. cArqDad->TIPOCC == '2' /*Se Imprime cod. reduzido do centro de Custo e eh analitico*/,;
						                                   cCCResAnt,;
						                                   cAntCCSup) ,1,10)
					EndIf
	
			        oReport:PrintText(cText_CCSup, oReport:Row(), 5)
			        oReport:SkipLine()
	        		oReport:ThinLine()
			        oTotSupCusto:PrintLine()
	        		
					oTotSupCusto:Finish()
					
					lImpCCSint	:= .T.
				EndIf
			Else
				lImpCCSint	:= .F.
			EndIf
		End
		cAntCCSup		:= ""
		cCCSup			:= ""
		dbSelectArea("cArqDad")
	EndIf

ENDIF

IF ! oReport:Cancel()
	oReport:ThinLine()
	
	//TOTAL GERAL
	nTotGeral	:= aTotCol[1]+aTotCol[2]+aTotCol[3]+aTotCol[4]+aTotCol[5]+aTotCol[6]+aTotCol[7]
	nTotGeral 	+= aTotCol[8]+aTotCol[9]+aTotCol[10]+aTotCol[11]+aTotCol[12]

	oTotGerCusto:Init()
	oTotGerCusto:lPrintHeader := .F.

	If MV_PAR30 == 2	//	Se NAO Totaliza Periodo
		cText_CCGer := "T O T A I S  D O  P E R I O D O: "		//"T O T A I S  D O  P E R I O D O : "
	Else
		cText_CCGer := "TOTAIS DO PERIODO:"		//"TOTAIS  DO  PERIODO: "
	EndIf

	oReport:PrintText(cText_CCGer, oReport:Row(), 5)
    oReport:SkipLine()
	oReport:ThinLine()
	oTotGerCusto:PrintLine()

	oTotGerCusto:Finish()

	nTotGeral	:= 0

    oReport:ThinLine()

	Set Filter To

EndIF

dbSelectArea("cArqDad")
Set Filter To
dbCloseArea()
Ferase(cArqDad+GetDBExtension())
Ferase("cArqInd"+OrdBagExt())
dbselectArea("CT2")

Return



//--------------------------RELEASE 03------------------------------------------------//

#DEFINE 	COL_SEPARA1			1
#DEFINE 	COL_CONTA 			2
#DEFINE 	COL_SEPARA2			3
#DEFINE 	COL_DESCRICAO		4
#DEFINE 	COL_SEPARA3			5
#DEFINE 	COL_COLUNA1       	6
#DEFINE 	COL_SEPARA4			7
#DEFINE 	COL_COLUNA2       	8
#DEFINE 	COL_SEPARA5			9 
#DEFINE 	COL_COLUNA3       	10
#DEFINE 	COL_SEPARA6			11
#DEFINE 	COL_COLUNA4   		12
#DEFINE 	COL_SEPARA7			13                                                                                       
#DEFINE 	COL_COLUNA5   		14
#DEFINE 	COL_SEPARA8			15
#DEFINE 	COL_COLUNA6   		16
#DEFINE 	COL_SEPARA9			17
#DEFINE 	COL_COLUNA7			18
#DEFINE 	COL_SEPARA10		19
#DEFINE 	COL_COLUNA8			20
#DEFINE 	COL_SEPARA11		21
#DEFINE 	COL_COLUNA9			22
#DEFINE 	COL_SEPARA12		23
#DEFINE 	COL_COLUNA10		24
#DEFINE 	COL_SEPARA13		25
#DEFINE 	COL_COLUNA11		26
#DEFINE 	COL_SEPARA14		27
#DEFINE 	COL_COLUNA12		28
#DEFINE 	COL_SEPARA15		29

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o	 ³ Ctbr285R3³ Autor ³ Simone Mie Sato   	³ Data ³ 29.04.04 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³ Balancete Comparativo por C.Custo x Cta  s/ 6 meses. 		  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Sintaxe	 ³ Ctbr285R3      											  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Retorno	 ³ Nenhum       							  				  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Uso 		 ³ SIGACTB      							  				  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ Nenhum									  				  ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/
Static Function Ctbr285R3()

Local aSetOfBook
Local aCtbMoeda	:= {}
Local cSayCC		:= CtbSayApro("CTT")
Local cDesc1 		:= "Este programa ira imprimir o Balancete Comparativo "
Local cDesc2 		:= Upper(Alltrim(cSayCC)) +" / "+ " CONTA "	// " Conta "
Local cDesc3 		:= "de acordo com os parametros solicitados pelo Usuario"
Local cNomeArq
LOCAL wnrel
LOCAL cString		:= "CTT"
Local titulo 		:= "Comparativo por"+Upper(Alltrim(cSayCC))+" / "+ " Conta " 	//"Comparativo de" " Conta "
Local lRet			:= .T.    
Local nDivide		:= 1
Local cMensagem		:= ""
Local lAtSlComp		:= Iif(GETMV("MV_SLDCOMP") == "S",.T.,.F.)

PRIVATE nLastKey 	:= 0
PRIVATE cPerg	 	:= "FECTBR2850"
PRIVATE aReturn 	:= { "Zebrado", 1,"Administracao", 2, 2, 1, "",1 }  //"Zebrado"###"Administracao"
PRIVATE aLinha		:= {}
PRIVATE nomeProg  	:= "CTBR285"
PRIVATE Tamanho		:="G"

If TamSx3("CTT_CUSTO")[1]> 9
	nTamCdoCusto := 25
EndIf			
If ( !AMIIn(34) )		// Acesso somente pelo SIGACTB
	Return
EndIf

li 		:= 80
m_pag		:= 1

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Mostra tela de aviso - Atualizacao de saldos				 ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
cMensagem := "Caso nao atualize os saldos compostos na"+chr(13)  		//"Caso nao atualize os saldos compostos na"
cMensagem += "emissao dos relatorios(MV_SLDCOMP ='N'),"+chr(13)  		//"emissao dos relatorios(MV_SLDCOMP ='N'),"
cMensagem += "rodar a rotina de atualizacao de saldos "+chr(13)  		//"rodar a rotina de atualizacao de saldos "

IF !lAtSlComp
	IF !MsgYesNo(cMensagem,"ATEN€O")	//"ATEN€O"
		Return
	Endif
EndIf

Pergunte("FECTBR2850",.F.)

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Variaveis utilizadas para parametros					       ³
//³ mv_par01				// Data Inicial              	       ³
//³ mv_par02				// Data Final                          ³
//³ mv_par03				// C. Custo Direto Inicio  		       ³
//³ mv_par04				// C. Custo Direto Fim				   ³
//³ MV_PAR05				// C. Custo Indireto Inicio            ³
//³ mv_par06				// C. Custo Indireto Fim   		       ³
//³ mv_par07				// C. Custo Despesa Inicio			   ³
//³ MV_PAR08				// C. Custo Despesa Fim                      ³
//³ MV_PAR09				// Conta Inicial                       ³
//³ MV_PAR10				// Conta Final   					   ³
//³ MV_PAR11				// Imprime Contas:Sintet/Analit/Ambas  ³
//³ MV_PAR12				// Set Of Books				    	   ³
//³ MV_PAR13				// Saldos Zerados?			     	   ³
//³ MV_PAR14				// Moeda?          			     	   ³
//³ MV_PAR15				// Pagina Inicial  		     		   ³
//³ MV_PAR16				// Saldos? Reais / Orcados/Gerenciais  ³
//³ MV_PAR17				// Imprimir ate o Segmento?			   ³
//³ MV_PAR18				// Filtra Segmento?					   ³
//³ MV_PAR19				// Conteudo Inicial Segmento?		   ³
//³ MV_PAR20				// Conteudo Final Segmento?		       ³
//³ MV_PAR21				// Conteudo Contido em?				   ³
//³ MV_PAR22				// Pula Pagina                         ³
//³ MV_PAR23				// Imprime Cod. C.Custo? Normal/Red.   ³
//³ MV_PAR24				// Imprime Cod. Conta? Normal/Reduzido ³
//³ MV_PAR25				// Salta linha sintetica?              ³
//³ MV_PAR26 				// Imprime Valor 0.00?                 ³
//³ MV_PAR27 				// Divide por?                         ³
//³ MV_PAR28				// Posicao Ant. L/P? Sim / Nao         ³
//³ MV_PAR29				// Data Lucros/Perdas?                 ³
//³ MV_PAR30				// Totaliza periodo ?                  ³
//³ MV_PAR31				// Se Totalizar ?                  	   ³
//³ MV_PAR32				// Imprime C.C?Sintet/Analit/Ambas 	   ³
//³ MV_PAR33				// Imprime Totalizacao de C.C. Sintet. ³
//³ MV_PAR34				// Tipo de Comparativo?(Movimento/Acumulado)  ³
//³ MV_PAR35				// Quebra por Grupo Contabil?		   ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

wnrel	:= "CTBR285"            //Nome Default do relatorio em Disco
wnrel := SetPrint(cString,wnrel,cPerg,@titulo,cDesc1,cDesc2,,.F.,"",,Tamanho)

If nLastKey == 27
	Set Filter To
	Return
Endif

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Verifica se usa Set Of Books + Plano Gerencial (Se usar Plano³
//³ Gerencial -> montagem especifica para impressao)			 ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If !ct040Valid(MV_PAR12)
	lRet := .F.
Else
   aSetOfBook := CTBSetOf(MV_PAR12)
Endif

If MV_PAR27 == 2			// Divide por cem
	nDivide := 100
ElseIf MV_PAR27 == 3		// Divide por mil
	nDivide := 1000
ElseIf MV_PAR27 == 4		// Divide por milhao
	nDivide := 1000000
EndIf	

If lRet
	aCtbMoeda  	:= CtbMoeda(MV_PAR14,nDivide)
	If Empty(aCtbMoeda[1])                        
      Help(" ",1,"NOMOEDA")
      lRet := .F.
   Endif
Endif

If !lRet
	Set Filter To
	Return
EndIf

SetDefault(aReturn,cString)

If nLastKey == 27
	Set Filter To
	Return
Endif

RptStatus({|lEnd| CTR285Imp(@lEnd,wnRel,cString,aSetOfBook,aCtbMoeda,cSayCC,nDivide)})

Return

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Program   ³CTR285IMP ³ Autor ³ Simone Mie Sato       ³ Data ³ 29.04.04 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³ Imprime relatorio -> Balancete C.Custo/Conta               ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Sintaxe   ³ CTR285Imp(lEnd,wnRel,cString,aSetOfBook,aCtbMoeda,cSayCC)  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Retorno   ³ Nenhum                                                     ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Uso       ³ SIGACTB                                                    ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ lEnd 		= Acao do CodeBlock                           ³±±
±±³			 ³ WnRel 		= Titulo do Relatorio				          ³±±
±±³			 ³ cString		= Mensagem						              ³±±
±±³			 ³ aSetOfBook 	= Registro de Config. Livros   		          ³±±
±±³			 ³ aCtbMoeda	= Registro ref. a moeda escolhida             ³±±
±±³			 ³ cSayCC		= Descric.C.Custo utilizado pelo usuario. 	  ³±±
±±³			 ³ nDivide		= Fator de div.dos valores a serem impressos. ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/
Static Function CTR285Imp(lEnd,WnRel,cString,aSetOfBook,aCtbMoeda,cSayCC,nDivide)

Local aColunas		:= {}
Local CbTxt			:= Space(10)
Local CbCont		:= 0
Local limite		:= 220
Local cabec1  		:= ""
Local cabec2		:= ""
Local cPicture
Local cDescMoeda
Local cCodMasc		:= ""
Local cMascara		:= ""
Local cMascCC		:= ""           
Local cSepara1		:= ""
Local cSepara2		:= ""
Local cGrupo		:= ""
Local cGrupoAnt	:= ""
Local lFirstPage	:= .T.
Local nDecimais
Local cCustoAnt		:= ""
Local cCCResAnt		:= ""
Local l132			:= .T.
Local lImpConta		:= .F.
Local lImpCusto		:= .T.
Local cCtaIni		:= MV_PAR09
Local cCtaFim		:= MV_PAR10
Local nPosAte		:= 0
Local nDigitAte		:= 0
Local cSegAte   	:= MV_PAR17
Local cArqDad   	:= ""
Local cArqTmp		:= ""
Local cArqInd			:= CriaTrab(Nil, .F.)
Local cArqAux			:= "cArqDad"
Local cCCSup		:= ""//Centro de Custo Superior do centro de custo atual
Local cAntCCSup		:= ""//Centro de Custo Superior do centro de custo anterior
Local lPula			:= Iif(MV_PAR25==1,.T.,.F.) 
Local lPrintZero	:= Iif(MV_PAR26==1,.T.,.F.)
Local lImpAntLP		:= Iif(MV_PAR28 == 1,.T.,.F.)
Local lVlrZerado	:= Iif(MV_PAR13==1,.T.,.F.)
Local dDataLP  		:= MV_PAR29
Local aMeses		:= {}          
Local dDataFim 		:= mv_par02
Local lJaPulou		:= .F.
Local nMeses		:= 1
Local aTotCol		:= {0,0,0,0,0,0,0,0,0,0,0,0}
Local aTotCC		:= {0,0,0,0,0,0,0,0,0,0,0,0}
Local aTotCCSup		:= {}
Local aSupCC		:= {}
Local nTotLinha		:= 0
Local nCont			:= 0
Local aTotGrupo		:= {0,0,0,0,0,0,0,0,0,0,0,0}
Local nTotLinGrp	:= 0

Local lImpSint 		:= Iif(MV_PAR11 == 2,.F.,.T.)
Local lImpTotS		:= Iif(MV_PAR33 == 1,.T.,.F.)
Local lImpCCSint	:= .T.
Local lNivel1		:= .F. 

Local nPos 			:= 0
Local nDigitos 		:= 0
Local n				:= 0
Local nVezes		:= 0
Local nPosCC		:= 0 
Local nTamaTotCC	:= 0
Local nAtuTotCC		:= 0 
Local cTpComp		:= If( MV_PAR34 == 1,"M","S" )	//	Comparativo : "M"ovimento ou "S"aldo Acumulado
Local lImpCabecCC 	:= .F.

cDescMoeda 	:= aCtbMoeda[2]
nDecimais 	:= DecimalCTB(aSetOfBook,MV_PAR14)

aPeriodos := ctbPeriodos(MV_PAR14, mv_par01, mv_par02, .T., .F.)

For nCont := 1 to len(aPeriodos)       
	//Se a Data do periodo eh maior ou igual a data inicial solicitada no relatorio.
	If aPeriodos[nCont][1] >= mv_par01 .And. aPeriodos[nCont][2] <= mv_par02 
		If nMeses <= 12
			AADD(aMeses,{StrZero(nMeses,2),aPeriodos[nCont][1],aPeriodos[nCont][2]})	
		EndIf
		nMeses += 1           					
	EndIf
Next                                                                   

//Mascara do Centro de Custo
If Empty(aSetOfBook[6])
	cMascCC :=  GetMv("MV_MASCCUS")
Else
	cMascCC := RetMasCtb(aSetOfBook[6],@cSepara1)
EndIf

// Mascara da Conta Contabil
If Empty(aSetOfBook[2])
	cMascara := GetMv("MV_MASCARA")
Else
	cMascara := RetMasCtb(aSetOfBook[2],@cSepara2)
EndIf

cPicture 		:= aSetOfBook[4]
cabec1 := "|CODIGO            |DESCRICAO          |  PERIODO 01  |  PERIODO 02  |  PERIODO 03  |  PERIODO 04  |  PERIODO 05  |  PERIODO 06  |  PERIODO 07  |  PERIODO 08  |  PERIODO 09  |  PERIODO 10  |  PERIODO 11  |  PERIODO 12  |"
tamanho := "G"
limite	:= 220        
l132	:= .F.

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Carrega titulo do relatorio: Analitico / Sintetico			 ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
IF MV_PAR11 == 1
	Titulo:=	"COMPARATIVO SINTETICO POR "+ Upper(Alltrim(cSayCC)) + " / "+ " CONTA " 		//"COMPARATIVO ANALITICO DE  "
ElseIf MV_PAR11 == 2
	Titulo:=	"COMPARATIVO ANALITICO POR " + Upper(Alltrim(cSayCC)) + " / "+ " CONTA "		//"COMPARATIVO SINTETICO DE  "
ElseIf MV_PAR11 == 3
	Titulo:=	"COMPARATIVO POR " + Upper(Alltrim(cSayCC)) + " / "+ " CONTA "		//"COMPARATIVO DE  "
EndIf

Titulo += 	" DE " + DTOC(mv_par01) + " ATE " + Dtoc(mv_par02) + 	" EM " + cDescMoeda
Titulo += 	OemToAnsi(" CC ") 
If !Empty(MV_PAR03) .or. !Empty(MV_PAR04)
	if Alltrim(MV_PAR03) == Alltrim(MV_PAR04)
		Titulo += " "+Alltrim(MV_PAR03)
	Else
		Titulo += " "+Alltrim(MV_PAR03)+"-"+Alltrim(MV_PAR04)
	Endif	
Endif	
If !Empty(MV_PAR05) .or. !Empty(MV_PAR06)
	if Alltrim(MV_PAR05) == Alltrim(MV_PAR06)
		Titulo += " "+Alltrim(MV_PAR05)
	Else
		Titulo += " "+Alltrim(MV_PAR05)+"-"+Alltrim(MV_PAR06)
	Endif	
Endif	
If !Empty(MV_PAR07) .or. !Empty(MV_PAR08)
	if Alltrim(MV_PAR07) == Alltrim(MV_PAR08)
		Titulo += " "+Alltrim(MV_PAR07)
	Else
		Titulo += " "+Alltrim(MV_PAR07)+"-"+Alltrim(MV_PAR08)
	Endif	
Endif	


If MV_PAR16 > "1"			
	Titulo += " (" + Tabela("SL", MV_PAR16, .F.) + ")"
Endif

If MV_PAR34 = 2
	MV_PAR30 := 2
	Titulo := AllTrim(Titulo) + " - " + "ACUMULADO"
EndIf
aColunas := { 000, 001, 019, 020, 039, 040, 054, 055, 069, 070, 084, 085, 099, 100, 114,  115, 129, 130, 144, 145, 159, 160, 174, 175, 189, 190 , 204, 205, 219} 

cabec1 := "|CODIGO            |DESCRICAO          |  PERIODO 01  |  PERIODO 02  |  PERIODO 03  |  PERIODO 04  |  PERIODO 05  |  PERIODO 06  |  PERIODO 07  |  PERIODO 08  |  PERIODO 09  |  PERIODO 10  |  PERIODO 11  |  PERIODO 12  |"

If MV_PAR30 = 1		// Com total, nao imprime descricao
	If MV_PAR31 = 2
		Cabec1 := Stuff(Cabec1, 2, 10, Subs(Cabec1, 21, 10))
	Endif
	Cabec1 := Stuff(Cabec1, 21, 20, "")
	If MV_PAR31 == 1
		Cabec1 += " "+"TOTAL PERIODO"+"     |"	// TOTAL PERIODO
	Else
		Cabec1 += " "+"TOTAL PERIODO"+"|"	// TOTAL PERIODO
	EndIf	
	

	For nCont := 6 to (Len(aColunas)-1) 
		If MV_PAR31 == 1 			//Se mostrar conta
			aColunas[nCont] -= 20		
		ElseIf MV_PAR31 == 2		// Se mostrar a descricao
			aColunas[nCont] -= 15			
		EndIf
	Next         
	
	If MV_PAR31 = 2
		Cabec1 := Stuff(Cabec1, 19, 0, Space(5))
		cabec2 := "|                       |"
	Else
		cabec2 := "|                  |"
	Endif
Else
	If MV_PAR24 = 2
		Cabec1 := 	Left(Cabec1, 11) + "|" + Subs(Cabec1, 21, 15) + Space(12) + "|" +;
					Subs(Cabec1, 41)
		Cabec2 := 	"|          |                           |"
	Else
		cabec2 := "|                  |                   |" 
	Endif
Endif
For nCont := 1 to Len(aMeses)
	cabec2 += SPACE(1)+Strzero(Day(aMeses[nCont][2]),2)+"/"+Strzero(Month(aMeses[nCont][2]),2)+ " - "
	cabec2 += Strzero(Day(aMeses[nCont][3]),2)+"/"+Strzero(Month(aMeses[nCont][3]),2)+"|"
Next

For nCont:= Len(aMeses) to 12
	If nCont == 12                           
		//Se totaliza a linha e mostra a conta                  
		If MV_PAR30 == 1  .And. MV_PAR31 == 1
			cabec2+=SPACE(19)+"|"			
		Else				
			cabec2+=SPACE(14)+"|"						
		EndIf
	Else
		cabec2+=SPACE(14)+"|"
	EndIf	
Next         

m_pag := MV_PAR15

// Verifica Se existe filtragem Ate o Segmento
If !Empty(cSegAte)
	For n := 1 to Val(cSegAte)
		nDigitAte += Val(Subs(cMascara,n,1))	
	Next
EndIf		

If !Empty(MV_PAR18)			//// FILTRA O SEGMENTO Nº
	If Empty(MV_PAR12)		//// VALIDA SE O CÓDIGO DE CONFIGURAÇÃO DE LIVROS ESTÁ CONFIGURADO
		help("",1,"CTN_CODIGO")
		Return
	Else
		If !Empty(aSetOfBook[5])
			MsgInfo("O plano gerencial ainda näo esta disponivel para este relatorio. "+CHR(10)+"Altere a configuracäo de livros...","Config. de Livros...")
			Return
		Endif
	Endif
	dbSelectArea("CTM")
	dbSetOrder(1)
	If MsSeek(xFilial()+aSetOfBook[2])
		While !Eof() .And. CTM->CTM_FILIAL == xFilial() .And. CTM->CTM_CODIGO == aSetOfBook[2]
			nPos += Val(CTM->CTM_DIGITO)
			If CTM->CTM_SEGMEN == STRZERO(val(MV_PAR18),2)
				nPos -= Val(CTM->CTM_DIGITO)
				nPos ++
				nDigitos := Val(CTM->CTM_DIGITO)
				Exit
			EndIf
			dbSkip()
		EndDo
	Else
		help("",1,"CTM_CODIGO")
		Return
	EndIf
EndIf 

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Monta Arquivo Temporario para Impressao						 ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

/*

MsgMeter({|	oMeter, oText, oDlg, lEnd | ;
				CTGerComp(oMeter, oText, oDlg, @lEnd,@cArqTmp,;
				mv_par01,mv_par02,"CT3","",mv_par05,mv_par06,mv_par03,mv_par04,,,,,mv_par10,;
				mv_par12,aSetOfBook,mv_par14,mv_par15,mv_par16,mv_par17,;
				.F.,.F.,,"CTT",lImpAntLP,dDataLP,nDivide,cTpComp,.F.,,.T.,aMeses,lVlrZerado,,,lImpSint,cString,aReturn[7],(lImpTotS .And. (lCT1Sint .Or. lCTTSint) );
				,,,,,,,,,,,@cNomeTab )},;
				STR0013,;  //"Criando Arquivo Tempor rio..."
				STR0003+Upper(Alltrim(cSayCC)) +" / " +  STR0011 )     //"Balancete Verificacao C.CUSTO / CONTA				



*/
MsgMeter({|	oMeter, oText, oDlg, lEnd | ;
				CTGerComp(oMeter, oText, oDlg, @lEnd,@cArqTmp,;
				mv_par01,mv_par02,"CT3","",MV_PAR09,MV_PAR10,mv_par03,mv_par04,,,,,MV_PAR14,;
				MV_PAR16,aSetOfBook,MV_PAR18,MV_PAR19,MV_PAR20,MV_PAR21,;
				.F.,.F.,,"CTT",lImpAntLP,dDataLP,nDivide,cTpComp,.F.,,.T.,aMeses,lVlrZerado,,,lImpSint,cString,aReturn[7],lImpTotS)},;
				"Criando Arquivo Temporario... ",;  //"Criando Arquivo Tempor rio..."
				"Comparativo por "+Upper(Alltrim(cSayCC)) +" / " +  " CONTA " )     //"Balancete Verificacao C.CUSTO / CONTA				

If Select("cArqTmp") <> 0
	CriaArqDad("cArqTmp",@cArqDad,@cArqAux,@cArqInd)
	CopDados("cArqTmp","cArqDad")
	("cArqTmp")->(dbCloseArea())
Endif	


//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Monta Arquivo Temporario para Impressao						 ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
MsgMeter({|	oMeter, oText, oDlg, lEnd | ;
				CTGerComp(oMeter, oText, oDlg, @lEnd,@cArqTmp,;
				mv_par01,mv_par02,"CT3","",MV_PAR09,MV_PAR10,mv_par05,mv_par06,,,,,MV_PAR14,;
				MV_PAR16,aSetOfBook,MV_PAR18,MV_PAR19,MV_PAR20,MV_PAR21,;
				.F.,.F.,,"CTT",lImpAntLP,dDataLP,nDivide,cTpComp,.F.,,.T.,aMeses,lVlrZerado,,,lImpSint,cString,aReturn[7],lImpTotS)},;
				"Criando Arquivo Temporario... ",;  //"Criando Arquivo Tempor rio..."
				"Comparativo por "+Upper(Alltrim(cSayCC)) +" / " +  " CONTA " )     //"Balancete Verificacao C.CUSTO / CONTA				

If Select("cArqTmp") <> 0
	CriaArqDad("cArqTmp",@cArqDad,@cArqAux,@cArqInd)
	CopDados("cArqTmp","cArqDad")
	("cArqTmp")->(dbCloseArea())
Endif	

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Monta Arquivo Temporario para Impressao						 ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
MsgMeter({|	oMeter, oText, oDlg, lEnd | ;
				CTGerComp(oMeter, oText, oDlg, @lEnd,@cArqTmp,;
				mv_par01,mv_par02,"CT3","",MV_PAR09,MV_PAR10,mv_par07,mv_par08,,,,,MV_PAR14,;
				MV_PAR16,aSetOfBook,MV_PAR18,MV_PAR19,MV_PAR20,MV_PAR21,;
				.F.,.F.,,"CTT",lImpAntLP,dDataLP,nDivide,cTpComp,.F.,,.T.,aMeses,lVlrZerado,,,lImpSint,cString,aReturn[7],lImpTotS)},;
				"Criando Arquivo Temporario... ",;  //"Criando Arquivo Tempor rio..."
				"Comparativo por "+Upper(Alltrim(cSayCC)) +" / " +  " CONTA " )     //"Balancete Verificacao C.CUSTO / CONTA				

If Select("cArqTmp") <> 0
	CriaArqDad("cArqTmp",@cArqDad,@cArqAux,@cArqInd)
	CopDados("cArqTmp","cArqDad")
	("cArqTmp")->(dbCloseArea())
Endif	

If Select("cArqDad") == 0
	Return
EndIf          



If MV_PAR33 == 1	//Se totaliza centro de custo 
	dbSelectArea("cArqDad")
	dbSetOrder(1)
	dbGotop()
	While!Eof()		       
		If !Empty(cArqDad->CCSUP) 
			dbSelectArea("CTT")
			dbSetOrder(1)
			If MsSeek(xFilial()+cArqDad->CCSUP)
				If Empty(CTT->CTT_CCSUP) 
					lNivel1	:= .T.
				Else
					lNivel1	:= .F.
				EndIf
			EndIf
			
			dbSelectArea("cArqDad")  
//			If (( MV_PAR32 == 2 .And. TIPOCC == "2" ) .Or. (MV_PAR32 == 1 .And. TIPOCC == "1" .And. Empty(CCSUP)) .Or. (MV_PAR32 == 3 ) .Or. lNivel1) .And.;
			If (( MV_PAR32 == 2 .And. TIPOCC == "2" ) .Or. (MV_PAR32 == 1 .And. TIPOCC == "1" ) .Or. (MV_PAR32 == 3 ) .Or. lNivel1) .And.;
				(( MV_PAR11 == 2 .And. TIPOCONTA == "2" ) .Or. (MV_PAR11 <> 2 .And. TIPOCONTA == "1" .And. Empty(CTASUP)))                

		
				nPosCC	:= ASCAN(aTotCCSup,{|x| x[1]==CCSUP})			
				If  nPosCC <= 0 				
	   		        aSupCC := {}
					For nVezes := 1 to Len(aMeses)	
		                aAdd(aSupCC,&("COLUNA"+Alltrim(Str(nVezes,2))))
					Next
					If Len(aMeses) < 12
						For nVezes := Len(aMeses)+1 to 12
		                	aAdd(aSupCC,0)				
						Next
					EndIf	                
					AADD(aTotCCSup,{CCSUP,aSupCC})
				Else     
					For nVezes := 1 to Len(aMeses)				
						aTotCCSup[nPosCC][2][nVezes]	+= 	&("COLUNA"+Alltrim(Str(nVezes,2)))
					Next										
				EndIf
			EndIf                
		EndIf
		dbSkip()
	End
EndIf
					
dbSelectArea("cArqDad")
dbSetOrder(1)
dbGoTop()

//Se tiver parametrizado com Plano Gerencial, exibe a mensagem que o Plano Gerencial 
//nao esta disponivel e sai da rotina.
If RecCount() == 0 .And. !Empty(aSetOfBook[5])                                       
	dbCloseArea()
	FErase(cArqDad+GetDBExtension())
	FErase("cArqInd"+OrdBagExt())
	Return
Endif

SetRegua(RecCount())

cGrupo := GRUPO
While !Eof()

	If lEnd
		@Prow()+1,0 PSAY "***** CANCELADO PELO OPERADOR *****"   //"***** CANCELADO PELO OPERADOR *****"
		Exit
	EndIF

	IncRegua()

	******************** "FILTRAGEM" PARA IMPRESSAO *************************
	

	If MV_PAR32 == 1					// So imprime Sinteticas
		If TIPOCC == "2"
			dbSkip()
			Loop
		EndIf
	ElseIf MV_PAR32 == 2				// So imprime Analiticas
		If TIPOCC == "1"
			dbSkip()
			Loop
		EndIf
	EndIf	

	If MV_PAR11 == 1					// So imprime Sinteticas
		If TIPOCONTA == "2"
			dbSkip()
			Loop
		EndIf
	ElseIf MV_PAR11 == 2				// So imprime Analiticas
		If TIPOCONTA == "1"
			dbSkip()
			Loop
		EndIf
	EndIf
	
	//Filtragem ate o Segmento ( antigo nivel do SIGACON)		
	If !Empty(cSegAte)
		If Len(Alltrim(CONTA)) > nDigitAte
			dbSkip()
			Loop
		Endif
	EndIf
		
	If lVlrZerado	.And. ;
	(Abs(COLUNA1)+Abs(COLUNA2)+Abs(COLUNA3)+Abs(COLUNA4)+Abs(COLUNA5)+Abs(COLUNA6)+Abs(COLUNA7)+Abs(COLUNA8)+;
		Abs(COLUNA9)+Abs(COLUNA10)+Abs(COLUNA11)+Abs(COLUNA12)) == 0	    
			If CtbExDtFim("CTT")  
				dbSelectArea("CTT")
				dbSetOrder(1)
				If MsSeek(xFilial()+ cArqDad->CUSTO)			
					If !CtbVlDtFim("CTT",mv_par01) 
						dbSelectArea("cArqDad")
						dbSkip()
						Loop
					EndIf			
				EndIf
			EndIf			
			
			If CtbExDtFim("CT1")
				dbSelectArea("CT1")
				dbSetOrder(1)
				If MsSeek(xFilial()+ cArqDad->CONTA)
					If !CtbVlDtFim("CT1",mv_par01) 
						dbSelectArea("cArqDad")
						dbSkip()			
						Loop
					EndIf						
				EndIf
			EndIf
		dbSelectArea("cArqDad")
	EndIf
		
	//Caso faca filtragem por segmento de Conta,verifico se esta dentro 
	//da solicitacao feita pelo usuario. 
	If !Empty(MV_PAR18)
		If Empty(MV_PAR19) .And. Empty(MV_PAR20) .And. !Empty(MV_PAR21)
			If  !(Substr(cArqDad->CONTA,nPos,nDigitos) $ (MV_PAR21) ) 
				dbSkip()
				Loop
			EndIf	
		Else
			If Substr(cArqDad->CONTA,nPos,nDigitos) < Alltrim(MV_PAR19) .Or. Substr(cArqDad->CONTA,nPos,nDigitos) > Alltrim(MV_PAR20)
				dbSkip()
				Loop
			EndIf	
		Endif
	EndIf	                                        
	
	************************* ROTINA DE IMPRESSAO *************************
	If MV_PAR35 == 1																	// Quebra por Grupo Contabil
		If (cGrupo <> GRUPO) .Or.;													// Grupo Diferente ou
		   ((cCustoAnt <> cArqDad->CUSTO) .And. ! Empty(cCustoAnt))	// Centro de Custo Diferente

			@li,00 PSAY REPLICATE("-",limite)
			li+=2
			@li,00 PSAY REPLICATE("-",limite)
			li++
			@li,aColunas[COL_SEPARA1] PSAY "|"
			@li,01 PSAY "GRUPO (" + Left(cGrupo,10) + ")"  		//"GRUPO ("       
			@li,aColunas[COL_SEPARA2] PSAY "|"

			ValorCTB(aTotGrupo[1],li,aColunas[COL_COLUNA1],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA4]		PSAY "|"
			ValorCTB(aTotGrupo[2],li,aColunas[COL_COLUNA2],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA5]		PSAY "|"
			ValorCTB(aTotGrupo[3],li,aColunas[COL_COLUNA3],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA6]		PSAY "|"
			ValorCTB(aTotGrupo[4],li,aColunas[COL_COLUNA4],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA7] PSAY "|"
			ValorCTB(aTotGrupo[5],li,aColunas[COL_COLUNA5],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA8] PSAY "|"
			ValorCTB(aTotGrupo[6],li,aColunas[COL_COLUNA6],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA9] PSAY "|"
			ValorCTB(aTotGrupo[7],li,aColunas[COL_COLUNA7],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA10] PSAY "|"
			ValorCTB(aTotGrupo[8],li,aColunas[COL_COLUNA8],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA11] PSAY "|"
			ValorCTB(aTotGrupo[9],li,aColunas[COL_COLUNA9],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA12] PSAY "|"
			ValorCTB(aTotGrupo[10],li,aColunas[COL_COLUNA10],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA13] PSAY "|"
			ValorCTB(aTotGrupo[11],li,aColunas[COL_COLUNA11],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA14] PSAY "|"
			ValorCTB(aTotGrupo[12],li,aColunas[COL_COLUNA12],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			If MV_PAR30 = 1		// Imprime Total
				nTotLinGrp	:= 0
				For nVezes := 1 to Len(aTotGrupo)
					nTotLinGrp	+= aTotGrupo[nVezes]
				Next
				If MV_PAR31 == 1//Mostrar a conta
					@ li,aColunas[COL_SEPARA15]-20 PSAY "|"
					ValorCTB(nTotLinGrp,li,aColunas[COL_SEPARA15]-18,12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
				ElseIf MV_PAR31 == 2	//Mostrar a descricao
					@ li,aColunas[COL_SEPARA15]-15 PSAY "|"
					ValorCTB(nTotLinGrp,li,aColunas[COL_SEPARA15]-14,12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
				EndIf
			Endif
			@ li,aColunas[COL_SEPARA15] PSAY "|"

			li			:= 60
			cGrupo		:= GRUPO
			aTotGrupo	:= {0,0,0,0,0,0,0,0,0,0,0,0}
		EndIf		
	Else	
		If (cCustoAnt <> cArqDad->CUSTO) .And. ! Empty(cCustoAnt)
		   	@li,00 PSAY	Replicate("-",limite)
			li++
			@li,aColunas[COL_SEPARA1] PSAY "|"
			
			If MV_PAR30 == 2		
				@li,aColunas[COL_CONTA] PSAY "TOTAIS DO "+ Upper(Alltrim(cSayCC))+ " : " //"T O T A I S  D O  "			
				If MV_PAR23 == 2	.And. cArqDad->TIPOCC == '2'//Se Imprime cod. reduzido do centro de Custo e eh analitico
					EntidadeCTB(cCCResAnt,li,aColunas[COL_CONTA]+23,nTamCdoCusto,.F.,cMascCC,cSepara1)			
				Else //Se Imprime cod. normal do Centro de Custo
					EntidadeCTB(cCustoAnt,li,aColunas[COL_CONTA]+23,nTamCdoCusto,.F.,cMascCC,cSepara1)					
				Endif
				@ li,aColunas[COL_SEPARA15] PSAY "|"
				li++                               
				@li,00 PSAY	Replicate("-",limite)
				li++ 	   
				@ li,aColunas[COL_SEPARA3] PSAY "|"						                                 						
			Else
				@li,aColunas[COL_CONTA] PSAY "TOTAIS: " //"TOTAIS: "			
				If MV_PAR23 == 2	.And. cArqDad->TIPOCC == '2'//Se Imprime cod. reduzido do centro de Custo e eh analitico
					@li,25 PSAY Subs(cCCResAnt,1,25)
				Else //Se Imprime cod. normal do Centro de Custo
					@li,25 PSAY Subs(cCustoAnt,1,25)			
				Endif			

				@ li,aColunas[COL_SEPARA15] PSAY "|"
				li++                               
				@li,00 PSAY	Replicate("-",limite)
				li++ 	   
				@ li,aColunas[COL_SEPARA2] PSAY "|"	
			EndIf		
			
			dbSelectArea("CTT")
			dbSetOrder(1)
			If MsSeek(xFilial()+cArqDad->CUSTO)
				cCCSup	:= CTT->CTT_CCSUP
			Else
				cCCSup	:= ""
			EndIf
			
			dbSelectArea("CTT")
			dbSetOrder(1)
			If MsSeek(xFilial()+cCustoAnt)
				cAntCCSup	:= CTT->CTT_CCSUP
			Else
				cAntCCSup	:= ""
			EndIf
			dbSelectArea("cArqDad")			
	
			//Total da Linha 
			nTotLinha	:= 0     			// Incluso esta linha para impressao dos totais 
			For nVezes := 1 to Len(aMeses)	// por periodo em 09/06/2004 por Otacilio
				nTotLinha	+= aTotCC[nVezes]
			Next
			@li,aColunas[COL_SEPARA1] PSAY "|"
			ValorCTB(aTotCC[1],li,aColunas[COL_COLUNA1],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA4]		PSAY "|"
			ValorCTB(aTotCC[2],li,aColunas[COL_COLUNA2],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA5]		PSAY "|"
			ValorCTB(aTotCC[3],li,aColunas[COL_COLUNA3],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA6]		PSAY "|"
			ValorCTB(aTotCC[4],li,aColunas[COL_COLUNA4],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA7] PSAY "|"	
			ValorCTB(aTotCC[5],li,aColunas[COL_COLUNA5],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA8] PSAY "|"
			ValorCTB(aTotCC[6],li,aColunas[COL_COLUNA6],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA9] PSAY "|"
			ValorCTB(aTotCC[7],li,aColunas[COL_COLUNA7],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA10] PSAY "|"
			ValorCTB(aTotCC[8],li,aColunas[COL_COLUNA8],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA11] PSAY "|"     
			ValorCTB(aTotCC[9],li,aColunas[COL_COLUNA9],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA12] PSAY "|"
			ValorCTB(aTotCC[10],li,aColunas[COL_COLUNA10],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA13] PSAY "|"		
			ValorCTB(aTotCC[11],li,aColunas[COL_COLUNA11],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA14] PSAY "|"
			ValorCTB(aTotCC[12],li,aColunas[COL_COLUNA12],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			If MV_PAR30 = 1		// Imprime Total        
				If MV_PAR31 == 1//Mostrar a conta
					@ li,aColunas[COL_SEPARA15]-20 PSAY "|"				
					ValorCTB(nTotLinha,li,aColunas[COL_SEPARA15]-18,12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
				ElseIf MV_PAR31 == 2	//Mostrar a descricao
					@ li,aColunas[COL_SEPARA15]-15 PSAY "|"				
					ValorCTB(nTotLinha,li,aColunas[COL_SEPARA15]-14,12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)			
				EndIf
			Endif			
			@ li,aColunas[COL_SEPARA15] PSAY "|"		
			aTotCC 	:= {0,0,0,0,0,0,0,0,0,0,0,0}
			
			If lImpTotS .And. cCCSup <> cAntCCSup .And. !Empty(cAntCCSup) //Se for centro de custo superior diferente
				li++
				@li,aColunas[COL_SEPARA1] PSAY "|"
				If MV_PAR30 == 2
					@li,aColunas[COL_CONTA] PSAY "TOTAIS DO "+ Upper(Alltrim(cSayCC))+ " : " //"T O T A I S  D O  "
					If MV_PAR23 == 2	.And. cArqDad->TIPOCC == '2'//Se Imprime cod. reduzido do centro de Custo e eh analitico
						EntidadeCTB(cCCResAnt,li,aColunas[COL_CONTA]+23,nTamCdoCusto,.F.,cMascCC,cSepara1)
					Else //Se Imprime cod. normal do Centro de Custo
						EntidadeCTB(cAntCCSup,li,aColunas[COL_CONTA]+23,nTamCdoCusto,.F.,cMascCC,cSepara1)
					Endif
					@ li,aColunas[COL_SEPARA15] PSAY "|"
					li++                               
					@li,00 PSAY	Replicate("-",limite)
					li++ 	   
					@ li,aColunas[COL_SEPARA3] PSAY "|"	
				Else
					@li,aColunas[COL_CONTA] PSAY "TOTAIS: " //"TOTAIS: "
					If MV_PAR23 == 2	.And. cArqDad->TIPOCC == '2'//Se Imprime cod. reduzido do centro de Custo e eh analitico
						@li,25 PSAY Subs(cCCResAnt,1,25)
					Else //Se Imprime cod. normal do Centro de Custo
						@li,25 PSAY Subs(cAntCCSup,1,25)			
					Endif								
					
					@ li,aColunas[COL_SEPARA15] PSAY "|"
					li++                               
					@li,00 PSAY	Replicate("-",limite)
					li++ 	   
					@ li,aColunas[COL_SEPARA2] PSAY "|"	
				EndIf
				
				//Total da Linha
				nTotLinha	:= 0     			// Incluso esta linha para impressao dos totais
				
				nPosCC	:= ASCAN(aTotCCSup,{|x| x[1]== cAntCCSup })			
				If  nPosCC > 0 							
					For nVezes := 1 to Len(aMeses)	// por periodo em 09/06/2004 por Otacilio
						nTotLinha	+= aTotCCSup[nPosCC][2][nVezes]
					Next
					@li,aColunas[COL_SEPARA1] PSAY "|"				
					ValorCTB(aTotCCSup[nPosCC][2][1],li,aColunas[COL_COLUNA1],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
					@ li,aColunas[COL_SEPARA4]		PSAY "|"
					ValorCTB(aTotCCSup[nPosCC][2][2],li,aColunas[COL_COLUNA2],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
					@ li,aColunas[COL_SEPARA5]		PSAY "|"
					ValorCTB(aTotCCSup[nPosCC][2][3],li,aColunas[COL_COLUNA3],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
					@ li,aColunas[COL_SEPARA6]		PSAY "|"
					ValorCTB(aTotCCSup[nPosCC][2][4],li,aColunas[COL_COLUNA4],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
					@ li,aColunas[COL_SEPARA7] PSAY "|"
					ValorCTB(aTotCCSup[nPosCC][2][5],li,aColunas[COL_COLUNA5],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
					@ li,aColunas[COL_SEPARA8] PSAY "|"
					ValorCTB(aTotCCSup[nPosCC][2][6],li,aColunas[COL_COLUNA6],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
					@ li,aColunas[COL_SEPARA9] PSAY "|"
					ValorCTB(aTotCCSup[nPosCC][2][7],li,aColunas[COL_COLUNA7],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
					@ li,aColunas[COL_SEPARA10] PSAY "|"
					ValorCTB(aTotCCSup[nPosCC][2][8],li,aColunas[COL_COLUNA8],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
					@ li,aColunas[COL_SEPARA11] PSAY "|"
					ValorCTB(aTotCCSup[nPosCC][2][9],li,aColunas[COL_COLUNA9],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
					@ li,aColunas[COL_SEPARA12] PSAY "|"
					ValorCTB(aTotCCSup[nPosCC][2][10],li,aColunas[COL_COLUNA10],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
					@ li,aColunas[COL_SEPARA13] PSAY "|"
					ValorCTB(aTotCCSup[nPosCC][2][11],li,aColunas[COL_COLUNA11],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
					@ li,aColunas[COL_SEPARA14] PSAY "|"
					ValorCTB(aTotCCSup[nPosCC][2][12],li,aColunas[COL_COLUNA12],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
					If MV_PAR30 = 1		// Imprime Total
						If MV_PAR31 == 1//Mostrar a conta
							@ li,aColunas[COL_SEPARA15]-20 PSAY "|"
							ValorCTB(nTotLinha,li,aColunas[COL_SEPARA15]-18,12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
						ElseIf MV_PAR31 == 2	//Mostrar a descricao
							@ li,aColunas[COL_SEPARA15]-15 PSAY "|"
							ValorCTB(nTotLinha,li,aColunas[COL_SEPARA15]-14,12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
						EndIf
					Endif
					@ li,aColunas[COL_SEPARA15] PSAY "|"
					dbSelectArea("cArqDad")
					nRegTmp	:= Recno()
					dbSelectArea("CTT")
					lImpCCSint	:= .T.
				EndIf
				While lImpCCSint
					dbSelectArea("CTT")
					If MsSeek(xFilial()+cAntCCSup) .And. !Empty(CTT->CTT_CCSUP)
						cAntCCSup	:= CTT->CTT_CCSUP
						li++
						@li,aColunas[COL_SEPARA1] PSAY "|"
						If MV_PAR30 == 2
							@li,aColunas[COL_CONTA] PSAY "TOTAIS DO "+ Upper(Alltrim(cSayCC))+ " : " //"T O T A I S  D O  "
							EntidadeCTB(cAntCCSup,li,aColunas[COL_CONTA]+23,nTamCdoCusto,.F.,cMascCC,cSepara1)

							@ li,aColunas[COL_SEPARA15] PSAY "|"
							li++                               
							@li,00 PSAY	Replicate("-",limite)
							li++ 	   
							@ li,aColunas[COL_SEPARA3] PSAY "|"	
						Else
							@li,aColunas[COL_CONTA] PSAY "TOTAIS: " //"TOTAIS: "
							@li,25 PSAY Subs(cAntCCSup,1,25)			
	
							@ li,aColunas[COL_SEPARA15] PSAY "|"
							li++                               
							@li,00 PSAY	Replicate("-",limite)
							li++ 	   
							@ li,aColunas[COL_SEPARA2] PSAY "|"	
						EndIf
						dbSelectArea("cArqDad")
						
						//Total da Linha
						nTotLinha	:= 0     			// Incluso esta linha para impressao dos totais
						nPosCC	:= ASCAN(aTotCCSup,{|x| x[1]== cAntCCSup })			
						If  nPosCC > 0 							
							For nVezes := 1 to Len(aMeses)	// por periodo em 09/06/2004 por Otacilio
								nTotLinha	+= aTotCCSup[nPosCC][2][nVezes]
							Next
							@li,aColunas[COL_SEPARA1] PSAY "|"						
							ValorCTB(aTotCCSup[nPosCC][2][1],li,aColunas[COL_COLUNA1],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
							@ li,aColunas[COL_SEPARA4]		PSAY "|"
							ValorCTB(aTotCCSup[nPosCC][2][2],li,aColunas[COL_COLUNA2],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
							@ li,aColunas[COL_SEPARA5]		PSAY "|"
							ValorCTB(aTotCCSup[nPosCC][2][3],li,aColunas[COL_COLUNA3],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
							@ li,aColunas[COL_SEPARA6]		PSAY "|"
							ValorCTB(aTotCCSup[nPosCC][2][4],li,aColunas[COL_COLUNA4],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
							@ li,aColunas[COL_SEPARA7] PSAY "|"
							ValorCTB(aTotCCSup[nPosCC][2][5],li,aColunas[COL_COLUNA5],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
							@ li,aColunas[COL_SEPARA8] PSAY "|"
							ValorCTB(aTotCCSup[nPosCC][2][6],li,aColunas[COL_COLUNA6],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
							@ li,aColunas[COL_SEPARA9] PSAY "|"
							ValorCTB(aTotCCSup[nPosCC][2][7],li,aColunas[COL_COLUNA7],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
							@ li,aColunas[COL_SEPARA10] PSAY "|"
							ValorCTB(aTotCCSup[nPosCC][2][8],li,aColunas[COL_COLUNA8],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
							@ li,aColunas[COL_SEPARA11] PSAY "|"
							ValorCTB(aTotCCSup[nPosCC][2][9],li,aColunas[COL_COLUNA9],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
							@ li,aColunas[COL_SEPARA12] PSAY "|"
							ValorCTB(aTotCCSup[nPosCC][2][10],li,aColunas[COL_COLUNA10],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
							@ li,aColunas[COL_SEPARA13] PSAY "|"
							ValorCTB(aTotCCSup[nPosCC][2][11],li,aColunas[COL_COLUNA11],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
							@ li,aColunas[COL_SEPARA14] PSAY "|"
							ValorCTB(aTotCCSup[nPosCC][2][12],li,aColunas[COL_COLUNA12],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
							If MV_PAR30 = 1		// Imprime Total
								If MV_PAR31 == 1//Mostrar a conta
									@ li,aColunas[COL_SEPARA15]-20 PSAY "|"
									ValorCTB(nTotLinha,li,aColunas[COL_SEPARA15]-18,12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
								ElseIf MV_PAR31 == 2	//Mostrar a descricao
									@ li,aColunas[COL_SEPARA15]-15 PSAY "|"
									ValorCTB(nTotLinha,li,aColunas[COL_SEPARA15]-14,12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
								EndIf
							Endif
							@ li,aColunas[COL_SEPARA15] PSAY "|"
							li++
							lImpCCSint	:= .T. 
						EndIF
					Else
						lImpCCSint	:= .F.
					EndIf
				End
				cAntCCSup		:= ""
				cCCSup			:= ""
				dbSelectArea("cArqDad")
				dbGoto(nRegTmp)
			EndIf
		
		Endif
	EndIf

	If MV_PAR35 == 1												// Quebra por Grupo Contabil
		If cGrupo != GRUPO									// Grupo Diferente
			li	:= 60
		EndIf
	Else
		If MV_PAR22 == 1 .And. ! Empty(cCustoAnt)
			If cCustoAnt <> cArqDad->CUSTO //Se o CC atual for diferente do CC anterior
				li 	:= 60
			EndIf
		Endif
	EndIf
	
	If li > 58 
		If !lFirstPage 
			@ Prow()+1,00 PSAY	Replicate("-",limite)
		EndIf
		CtCGCCabec(,,,Cabec1,Cabec2,dDataFim,Titulo,,"2",Tamanho)
		If MV_PAR35 == 2 .And. MV_PAR32 == 3 .Or. lFirstPage
			If MV_PAR22 == 1 .or. lFirstPage
				//Imprime titulo do centro de custo		
				li++	
				@li,00 PSAY REPLICATE("-",limite)					
				li++
				@ li,aColunas[COL_SEPARA1] PSAY "|"
				//@ li,aColunas[COL_CONTA]+4 PSAY Upper(cSayCC)
				If MV_PAR23 == 2 .And. cArqDad->TIPOCC == '2'//Se Imprime Cod Reduzido do C.Custo e eh analitico
					EntidadeCTB(CCRES,li,aColunas[COL_CONTA]+20,25,.F.,cMascCC,cSepara1)			
				Else //Se Imprime Cod. Normal do C.Custo
					EntidadeCTB(CUSTO,li,aColunas[COL_CONTA]+20,25,.F.,cMascCC,cSepara1)			
				Endif          
				//@ li,aColunas[COL_CONTA]+ Len(CriaVar("CTT_DESC01")) PSAY SPACE(10)+"  -  " +cArqDad->DESCCC
				@ li,aColunas[COL_CONTA]+4  PSAY cArqDad->DESCCC
				@ li,aColunas[COL_SEPARA15] PSAY "|"				                
				li++                                               
				@li,00 PSAY REPLICATE("-",limite)					
				li++
				lImpCabecCC := .T.
			Endif
		EndIf
		lFirstPage := .F.
	Endif
	
	//Se mudar de centro de custo
	If ! lImpCabecCC
		If	(CUSTO <> cCustoAnt .And. !Empty(cCustoAnt))	.Or.;
			li > 58									    	.Or.;
			(MV_PAR35 == 1 .And. cGrupoAnt <> cArqDad->GRUPO)                    

			//Imprime titulo do centro de custo
			li++	
			@li,00 PSAY REPLICATE("-",limite)
			li++
			@ li,aColunas[COL_SEPARA1] PSAY "|"
			//@ li,aColunas[COL_CONTA]+4 PSAY Upper(cSayCC)
			If MV_PAR23 == 2 .And. cArqDad->TIPOCC == '2'//Se Imprime Cod Reduzido do C.Custo e eh analitico
				EntidadeCTB(CCRES,li,aColunas[COL_CONTA]+20,25,.F.,cMascCC,cSepara1)
			Else //Se Imprime Cod. Normal do C.Custo
				EntidadeCTB(CUSTO,li,aColunas[COL_CONTA]+20,25,.F.,cMascCC,cSepara1)
			Endif
			//@ li,aColunas[COL_CONTA]+ Len(CriaVar("CTT_DESC01")) PSAY SPACE(10)+"  -  " +cArqDad->DESCCC
			@ li,aColunas[COL_CONTA]+ 4 PSAY cArqDad->DESCCC
			@ li,aColunas[COL_SEPARA15] PSAY "|"
			li++
			@li,00 PSAY REPLICATE("-",limite)
			li++
		EndIf
	Else
		lImpCabecCC := .F.
	Endif
	
	//Total da Linha
	nTotLinha	:= COLUNA1+COLUNA2+COLUNA3+COLUNA4+COLUNA5+COLUNA6+COLUNA7+COLUNA8+COLUNA9+COLUNA10+COLUNA11+COLUNA12	
	
	@ li,aColunas[COL_SEPARA1] PSAY "|"
	//Se totaliza e mostra a descricao
	If MV_PAR30 = 1 .And. MV_PAR31 = 2
		@ li,aColunas[COL_CONTA] PSAY Left(DESCCTA,18)
		@ li,aColunas[COL_SEPARA2]+5 PSAY "|"				
	Else
		If MV_PAR24 == 1       //Codigo Normal
			EntidadeCTB(Subs(CONTA,1,nTamConta),li,aColunas[COL_CONTA],nTamConta,.F.,cMascara,cSepara2)	
		Else //Codigo Reduzido
			EntidadeCTB(CTARES,li,aColunas[COL_CONTA],nTamConta,.F.,cMascara,cSepara2)		
		Endif
		@ li,aColunas[COL_SEPARA2] PSAY "|"		
	Endif	

	// Se nao totalizar ou se totalizar e mostrar a descricao da conta
	If MV_PAR30 == 2 
		@ li,aColunas[COL_DESCRICAO] PSAY Left(DESCCTA,19)   
		@ li,aColunas[COL_SEPARA3] PSAY "|"		
	Endif           
	ValorCTB(COLUNA1,li,aColunas[COL_COLUNA1],12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA4]		PSAY "|"
	ValorCTB(COLUNA2,li,aColunas[COL_COLUNA2],12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA5]		PSAY "|"
	ValorCTB(COLUNA3,li,aColunas[COL_COLUNA3],12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA6]		PSAY "|"
	ValorCTB(COLUNA4,li,aColunas[COL_COLUNA4],12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA7] PSAY "|"	
	ValorCTB(COLUNA5,li,aColunas[COL_COLUNA5],12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA8] PSAY "|"
	ValorCTB(COLUNA6,li,aColunas[COL_COLUNA6],12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA9] PSAY "|"     
	ValorCTB(COLUNA7,li,aColunas[COL_COLUNA7],12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA10] PSAY "|"     
	ValorCTB(COLUNA8,li,aColunas[COL_COLUNA8],12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA11] PSAY "|"     
	ValorCTB(COLUNA9,li,aColunas[COL_COLUNA9],12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA12] PSAY "|"     
	ValorCTB(COLUNA10,li,aColunas[COL_COLUNA10],12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA13] PSAY "|"     
	ValorCTB(COLUNA11,li,aColunas[COL_COLUNA11],12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA14] PSAY "|"     
	ValorCTB(COLUNA12,li,aColunas[COL_COLUNA12],12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
	If MV_PAR30 == 1   
		If MV_PAR31 == 1
			@ li,aColunas[COL_SEPARA15]-20 PSAY "|"     	                      
			ValorCTB(nTotLinha,li,aColunas[COL_SEPARA15]-18,12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
		ElseIf MV_PAR31 == 2	//Mostrar a descricao
			@ li,aColunas[COL_SEPARA15]-15 PSAY "|"				
			ValorCTB(nTotLinha,li,aColunas[COL_SEPARA15]-14,12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)			
		EndIf		
	EndIf
	@ li,aColunas[COL_SEPARA15] PSAY "|"     
	lJaPulou := .F.
	If lPula .And. TIPOCONTA == "1"				// Pula linha entre sinteticas
		li++
		@ li,aColunas[COL_SEPARA1] PSAY "|"
		//Se totaliza e mostra a descricao da conta	
		If MV_PAR30 == 1 .And. MV_PAR31 == 2  
			@ li,aColunas[COL_SEPARA2]+5 PSAY "|"			
		Else
			@ li,aColunas[COL_SEPARA2] PSAY "|"
		EndIf
		//Se nao totaliza periodo
		If MV_PAR30 == 2
			@ li,aColunas[COL_SEPARA3] PSAY "|"	
		EndIf
		@ li,aColunas[COL_SEPARA4] PSAY "|"
		@ li,aColunas[COL_SEPARA5] PSAY "|"
		@ li,aColunas[COL_SEPARA6] PSAY "|"
		@ li,aColunas[COL_SEPARA7] PSAY "|"
		@ li,aColunas[COL_SEPARA8] PSAY "|"
		@ li,aColunas[COL_SEPARA9] PSAY "|"
		@ li,aColunas[COL_SEPARA10] PSAY "|"
		@ li,aColunas[COL_SEPARA11] PSAY "|"
		@ li,aColunas[COL_SEPARA12] PSAY "|"
		@ li,aColunas[COL_SEPARA13] PSAY "|"
		@ li,aColunas[COL_SEPARA14] PSAY "|"								
		If MV_PAR30 == 1
			If MV_PAR31 == 1
				@ li,aColunas[COL_SEPARA15]-20 PSAY "|"     	                      		
			Else
				@ li,aColunas[COL_SEPARA15]-15 PSAY "|"     	                      					
			Endif
		EndIf
		@ li,aColunas[COL_SEPARA15] PSAY "|"
		li++
		lJaPulou := .T.
	Else
		li++
	EndIf			

	************************* FIM   DA  IMPRESSAO *************************

	If MV_PAR11 != 1					// Imprime Analiticas ou Ambas
		If TIPOCONTA == "2"
			If (MV_PAR32 != 1 .And. TIPOCC == "2")
				For nVezes := 1 to Len(aMeses)
					aTotCol[nVezes] 	+=&("COLUNA"+Alltrim(Str(nVezes,2)))					
					aTotGrupo[nVezes] 	+=&("COLUNA"+Alltrim(Str(nVezes,2)))
				Next
			ElseIf (MV_PAR32 == 1 .And. cArqDad->TIPOCC != "2"	)	//Imprime centro de custo sintetico
				If MV_PAR11 == 2 	//Imprime contas analiticas
					For nVezes := 1 to Len(aMeses)            
						If Empty(CCSUP)
							aTotCol[nVezes] 	+=&("COLUNA"+Alltrim(Str(nVezes,2)))					
							aTotGrupo[nVezes] 	+=&("COLUNA"+Alltrim(Str(nVezes,2)))
						EndIf
					Next         
				ElseIf MV_PAR11 == 3	//Imprime contas sinteticas e analiticas
					If Empty(CCSUP)      //Somar somente o centro de custo sintetico
						For nVezes := 1 to Len(aMeses)
							aTotCol[nVezes] 	+=&("COLUNA"+Alltrim(Str(nVezes,2)))											
							aTotGrupo[nVezes] 	+=&("COLUNA"+Alltrim(Str(nVezes,2)))
						Next         					
					EndIf				
				EndIf
			EndIf	
			For nVezes := 1 to Len(aMeses)
				aTotCC[nVezes] 		+=&("COLUNA"+Alltrim(Str(nVezes,2)))									
			Next	
		Endif
	Else
		If (TIPOCONTA == "1" .And. Empty(CTASUP))
			If (MV_PAR32 != 1 .And. cArqDad->TIPOCC == "2")
				For nVezes := 1 to Len(aMeses)
					aTotCol[nVezes] 	+=&("COLUNA"+Alltrim(Str(nVezes,2)))					
					aTotGrupo[nVezes] 	+=&("COLUNA"+Alltrim(Str(nVezes,2)))
				Next
			ElseIf (MV_PAR32 == 1 .And. cArqDad->TIPOCC != "2"	)
				If Empty(CCSUP)
					For nVezes := 1 to Len(aMeses)
						aTotCol[nVezes] 	+=&("COLUNA"+Alltrim(Str(nVezes,2)))			
						aTotGrupo[nVezes] 	+=&("COLUNA"+Alltrim(Str(nVezes,2)))
					Next
				EndIf
			EndIf	
			For nVezes := 1 to Len(aMeses)
				aTotCC[nVezes] 		+=&("COLUNA"+Alltrim(Str(nVezes,2)))					
			Next
		EndIf		
	Endif

	cCustoAnt := cArqDad->CUSTO
	cCCResAnt := cArqDad->CCRES
	cGrupoAnt := cArqDad->GRUPO
	

	dbSelectarea("cArqDad")
	dbSkip()
	
	If lPula .And. TIPOCONTA == "1" 			// Pula linha entre sinteticas
		If !lJaPulou
			@ li,aColunas[COL_SEPARA1] PSAY "|"
			//Se totaliza e mostra a descricao da conta	
			If MV_PAR30 == 1 .And. MV_PAR31 == 2  
				@ li,aColunas[COL_SEPARA2]+5 PSAY "|"			
			Else
				@ li,aColunas[COL_SEPARA2] PSAY "|"
			EndIf
			//Se nao totaliza periodo
			If MV_PAR30 == 2
				@ li,aColunas[COL_SEPARA3] PSAY "|"	
			EndIf
			@ li,aColunas[COL_SEPARA4] PSAY "|"
			@ li,aColunas[COL_SEPARA5] PSAY "|"
			@ li,aColunas[COL_SEPARA6] PSAY "|"
			@ li,aColunas[COL_SEPARA7] PSAY "|"
			@ li,aColunas[COL_SEPARA8] PSAY "|"
			@ li,aColunas[COL_SEPARA9] PSAY "|"			
			@ li,aColunas[COL_SEPARA10] PSAY "|"
			@ li,aColunas[COL_SEPARA11] PSAY "|"			
			@ li,aColunas[COL_SEPARA12] PSAY "|"
			@ li,aColunas[COL_SEPARA13] PSAY "|"
			@ li,aColunas[COL_SEPARA14] PSAY "|"
			//Se totaliza linha
			If MV_PAR30 == 1          
				If MV_PAR31 == 1
					@ li,aColunas[COL_SEPARA15]-20 PSAY "|"	
				Else
					@ li,aColunas[COL_SEPARA15]-15 PSAY "|"					
				EndIf
			EndIf
			
			@ li,aColunas[COL_SEPARA15] PSAY "|"			
			li++
		EndIf	
	EndIf	
EndDO

If MV_PAR35 == 1				// Quebra por Grupo Contabil
	If cGrupo <> GRUPO			// Grupo Diferente - Totaliza e Quebra
		@li,00 PSAY REPLICATE("-",limite)
		li+=2
		@li,00 PSAY REPLICATE("-",limite)
		li++
		@li,aColunas[COL_SEPARA1] PSAY "|"
		@li,01 PSAY "GRUPO (" + Left(cGrupo,10) + ")"  		//"GRUPO ("       
		@li,aColunas[COL_SEPARA2] PSAY "|"

		ValorCTB(aTotGrupo[1],li,aColunas[COL_COLUNA1],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
		@ li,aColunas[COL_SEPARA4]		PSAY "|"
		ValorCTB(aTotGrupo[2],li,aColunas[COL_COLUNA2],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
		@ li,aColunas[COL_SEPARA5]		PSAY "|"
		ValorCTB(aTotGrupo[3],li,aColunas[COL_COLUNA3],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
		@ li,aColunas[COL_SEPARA6]		PSAY "|"
		ValorCTB(aTotGrupo[4],li,aColunas[COL_COLUNA4],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
		@ li,aColunas[COL_SEPARA7] PSAY "|"
		ValorCTB(aTotGrupo[5],li,aColunas[COL_COLUNA5],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
		@ li,aColunas[COL_SEPARA8] PSAY "|"
		ValorCTB(aTotGrupo[6],li,aColunas[COL_COLUNA6],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
		@ li,aColunas[COL_SEPARA9] PSAY "|"
		ValorCTB(aTotGrupo[7],li,aColunas[COL_COLUNA7],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
		@ li,aColunas[COL_SEPARA10] PSAY "|"
		ValorCTB(aTotGrupo[8],li,aColunas[COL_COLUNA8],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
		@ li,aColunas[COL_SEPARA11] PSAY "|"
		ValorCTB(aTotGrupo[9],li,aColunas[COL_COLUNA9],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
		@ li,aColunas[COL_SEPARA12] PSAY "|"
		ValorCTB(aTotGrupo[10],li,aColunas[COL_COLUNA10],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
		@ li,aColunas[COL_SEPARA13] PSAY "|"
		ValorCTB(aTotGrupo[11],li,aColunas[COL_COLUNA11],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
		@ li,aColunas[COL_SEPARA14] PSAY "|"
		ValorCTB(aTotGrupo[12],li,aColunas[COL_COLUNA12],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
		If MV_PAR30 = 1		// Imprime Total
			nTotLinGrp	:= 0
			For nVezes := 1 to Len(aTotGrupo)
				nTotLinGrp	+= aTotGrupo[nVezes]
			Next
			If MV_PAR31 == 1//Mostrar a conta
				@ li,aColunas[COL_SEPARA15]-20 PSAY "|"
				ValorCTB(nTotLinGrp,li,aColunas[COL_SEPARA15]-18,12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
			ElseIf MV_PAR31 == 2	//Mostrar a descricao
				@ li,aColunas[COL_SEPARA15]-15 PSAY "|"
				ValorCTB(nTotLinGrp,li,aColunas[COL_SEPARA15]-14,12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
			EndIf
		Endif
		@ li,aColunas[COL_SEPARA15] PSAY "|"
		li++

		cGrupo		:= GRUPO
		aTotGrupo	:= {0,0,0,0,0,0,0,0,0,0,0,0}
	EndIf		
Else	

	If li > 50
		If !lFirstPage 
			@ Prow()+1,00 PSAY	Replicate("-",limite)
		EndIf
		CtCGCCabec(,,,Cabec1,Cabec2,dDataFim,Titulo,,"2",Tamanho)
	Endif                        
	
	//Imprime o total do ultimo Conta a ser impresso.
			@li,00 PSAY	Replicate("-",limite)
			li++
			@li,aColunas[COL_SEPARA1] PSAY "|"
	
	dbSelectArea("CTT")
	dbSetOrder(1)
	If MsSeek(xFilial("CTT")+cArqDad->CUSTO)
		cCCSup	:= CTT->CTT_CCSUP	//Centro de Custo Superior
	Else
		cCCSup	:= ""
	EndIf
	
	If MsSeek(xFilial("CTT")+cCustoAnt)
		cAntCCSup := CTT->CTT_CCSUP	//Centro de Custo Superior do Centro de custo anterior.
		cCCRes	  := CTT->CTT_RES
	Else
		cAntCCSup := ""
	EndIf
	
	dbSelectArea("cArqDad")
	
	If MV_PAR30 == 2
		@li,aColunas[COL_CONTA] PSAY "TOTAIS DO "+ Upper(cSayCC)+ " : " //"T O T A I S  D O  "
		If MV_PAR23 == 2	.And. cArqDad->TIPOCC == '2'//Se Imprime cod. reduzido do centro de Custo e eh analitico
			EntidadeCTB(cCCResAnt,li,aColunas[COL_CONTA]+23,nTamCdoCusto,.F.,cMascCC,cSepara1)
		Else //Se Imprime cod. normal do Centro de Custo
			EntidadeCTB(cCustoAnt,li,aColunas[COL_CONTA]+23,nTamCdoCusto,.F.,cMascCC,cSepara1)
		Endif
		@ li,aColunas[COL_SEPARA15] PSAY "|"
		li++                               
		@li,00 PSAY	Replicate("-",limite)
		li++ 	   
		@ li,aColunas[COL_SEPARA3] PSAY "|"
	Else
		@li,aColunas[COL_CONTA] PSAY "TOTAIS: " //"TOTAIS: "
		If MV_PAR23 == 2	.And. cArqDad->TIPOCC == '2'//Se Imprime cod. reduzido do centro de Custo e eh analitico
			@li,25 PSAY Subs(cCCResAnt,1,25)
		Else //Se Imprime cod. normal do Centro de Custo
			@li,25 PSAY Subs(cCustoAnt,1,25)			
		Endif				

		@ li,aColunas[COL_SEPARA15] PSAY "|"
		li++                               
		@li,00 PSAY	Replicate("-",limite)
		li++ 	   
		@ li,aColunas[COL_SEPARA2] PSAY "|"
	EndIf
	@li,aColunas[COL_SEPARA1] PSAY "|"	
	ValorCTB(aTotCC[1],li,aColunas[COL_COLUNA1],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA4]		PSAY "|"
	ValorCTB(aTotCC[2],li,aColunas[COL_COLUNA2],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA5]		PSAY "|"
	ValorCTB(aTotCC[3],li,aColunas[COL_COLUNA3],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA6]		PSAY "|"
	ValorCTB(aTotCC[4],li,aColunas[COL_COLUNA4],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA7] PSAY "|"
	ValorCTB(aTotCC[5],li,aColunas[COL_COLUNA5],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA8] PSAY "|"
	ValorCTB(aTotCC[6],li,aColunas[COL_COLUNA6],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA9] PSAY "|"
	ValorCTB(aTotCC[7],li,aColunas[COL_COLUNA7],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA10] PSAY "|"
	ValorCTB(aTotCC[8],li,aColunas[COL_COLUNA8],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA11] PSAY "|"
	ValorCTB(aTotCC[9],li,aColunas[COL_COLUNA9],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA12] PSAY "|"
	ValorCTB(aTotCC[10],li,aColunas[COL_COLUNA10],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA13] PSAY "|"
	ValorCTB(aTotCC[11],li,aColunas[COL_COLUNA11],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA14] PSAY "|"
	ValorCTB(aTotCC[12],li,aColunas[COL_COLUNA12],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
	
	//Total da Linha
	nTotLinha	:= 0
	For nVezes := 1 to Len(aMeses)
		nTotLinha	+= aTotCC[nVezes]
	Next
	
	If MV_PAR30 == 1
		If MV_PAR31 == 1	//Mostrar a conta
			@ li,aColunas[COL_SEPARA15]-20 PSAY "|"
			ValorCTB(nTotLinha,li,aColunas[COL_SEPARA15]-18,12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
		ElseIf MV_PAR31 == 2	//Mostrar a descricao
			@ li,aColunas[COL_SEPARA15]-15 PSAY "|"
			ValorCTB(nTotLinha,li,aColunas[COL_SEPARA15]-14,12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
		EndIf
	EndIf
	@ li,aColunas[COL_SEPARA15] PSAY "|"
	li++
	@li,00 PSAY	Replicate("-",limite)
	If (cArqDad->TIPOCC == "1" .And. !Empty(cArqDad->CCSUP)) .Or. (cArqDad->TIPOCC == "2")	
		aTotCC 	:= {0,0,0,0,0,0,0,0,0,0,0,0}
	EndIf
	li++
	@ li,aColunas[COL_SEPARA15] PSAY "|"
	
	If lImpTotS .And. cCCSup <> cAntCCSup .And. !Empty(cAntCCSup) //Se for centro de custo superior diferente
	
		@li,aColunas[COL_SEPARA1] PSAY "|"
		li++
		@li,aColunas[COL_SEPARA1] PSAY "|"
		If MV_PAR30 == 2
			@li,aColunas[COL_CONTA] PSAY "TOTAIS DO "+ Upper(Alltrim(cSayCC))+ " : " //"T O T A I S  D O  "
			EntidadeCTB(cAntCCSup,li,aColunas[COL_CONTA]+23,nTamCdoCusto,.F.,cMascCC,cSepara1)

			@ li,aColunas[COL_SEPARA15] PSAY "|"
			li++                               
			@li,00 PSAY	Replicate("-",limite)
			li++ 	   
			@ li,aColunas[COL_SEPARA3] PSAY "|"	
		Else 
			@li,aColunas[COL_CONTA] PSAY "TOTAIS: " //"TOTAIS: "
			@li,25 PSAY Subs(cAntCCSup,1,25)		
			@ li,aColunas[COL_SEPARA15] PSAY "|"
			li++                               
			@li,00 PSAY	Replicate("-",limite)
			li++ 	   
			@ li,aColunas[COL_SEPARA2] PSAY "|"	
		EndIf
		
		//Total da Linha
		nTotLinha	:= 0     			// Incluso esta linha para impressao dos totais
		nPosCC	:= ASCAN(aTotCCSup,{|x| x[1]== cAntCCSup })			
		If  nPosCC > 0 							
			For nVezes := 1 to Len(aMeses)	// por periodo em 09/06/2004 por Otacilio
				nTotLinha	+= aTotCCSup[nPosCC][2][nVezes]
			Next
			@li,aColunas[COL_SEPARA1] PSAY "|"		
			ValorCTB(aTotCCSup[nPosCC][2][1],li,aColunas[COL_COLUNA1],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA4]		PSAY "|"
			ValorCTB(aTotCCSup[nPosCC][2][2],li,aColunas[COL_COLUNA2],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA5]		PSAY "|"
			ValorCTB(aTotCCSup[nPosCC][2][3],li,aColunas[COL_COLUNA3],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA6]		PSAY "|"
			ValorCTB(aTotCCSup[nPosCC][2][4],li,aColunas[COL_COLUNA4],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA7] PSAY "|"
			ValorCTB(aTotCCSup[nPosCC][2][5],li,aColunas[COL_COLUNA5],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA8] PSAY "|"
			ValorCTB(aTotCCSup[nPosCC][2][6],li,aColunas[COL_COLUNA6],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA9] PSAY "|"
			ValorCTB(aTotCCSup[nPosCC][2][7],li,aColunas[COL_COLUNA7],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA10] PSAY "|"
			ValorCTB(aTotCCSup[nPosCC][2][8],li,aColunas[COL_COLUNA8],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA11] PSAY "|"
			ValorCTB(aTotCCSup[nPosCC][2][9],li,aColunas[COL_COLUNA9],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA12] PSAY "|"
			ValorCTB(aTotCCSup[nPosCC][2][10],li,aColunas[COL_COLUNA10],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA13] PSAY "|"
			ValorCTB(aTotCCSup[nPosCC][2][11],li,aColunas[COL_COLUNA11],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			@ li,aColunas[COL_SEPARA14] PSAY "|"
			ValorCTB(aTotCCSup[nPosCC][2][12],li,aColunas[COL_COLUNA12],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
			If MV_PAR30 = 1		// Imprime Total
				If MV_PAR31 == 1//Mostrar a conta
					@ li,aColunas[COL_SEPARA15]-20 PSAY "|"
					ValorCTB(nTotLinha,li,aColunas[COL_SEPARA15]-18,12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
				ElseIf MV_PAR31 == 2	//Mostrar a descricao
					@ li,aColunas[COL_SEPARA15]-15 PSAY "|"
					ValorCTB(nTotLinha,li,aColunas[COL_SEPARA15]-14,12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
				EndIf
			Endif
			@ li,aColunas[COL_SEPARA15] PSAY "|"
			li++
			dbSelectArea("CTT")
			lImpCCSint	:= .T.
	    EndIf
		
		While lImpCCSint
			dbSelectArea("CTT")
			If MsSeek(xFilial()+cAntCCSup) .And. !Empty(CTT->CTT_CCSUP)
				cAntCCSup	:= CTT->CTT_CCSUP
				@li,aColunas[COL_SEPARA1] PSAY "|"
				@li,aColunas[COL_SEPARA1] PSAY "|"
				If MV_PAR30 == 2
					@li,aColunas[COL_CONTA] PSAY "TOTAIS DO "+ Upper(Alltrim(cSayCC))+ " : " //"T O T A I S  D O  "
					EntidadeCTB(cAntCCSup,li,aColunas[COL_CONTA]+23,nTamCdoCusto,.F.,cMascCC,cSepara1)
					
					@ li,aColunas[COL_SEPARA15] PSAY "|"
					li++                               
					@li,00 PSAY	Replicate("-",limite)
					li++ 	   
					@ li,aColunas[COL_SEPARA3] PSAY "|"	
				Else
					@li,aColunas[COL_CONTA] PSAY "TOTAIS: " //"TOTAIS: "
					@li,25 PSAY Subs(cAntCCSup,1,25)			
											
			   		@ li,aColunas[COL_SEPARA15] PSAY "|"
					li++                               
					@li,00 PSAY	Replicate("-",limite)
					li++ 	   
					@ li,aColunas[COL_SEPARA2] PSAY "|"	
				EndIf
				dbSelectArea("cArqDad")
				
				//Total da Linha
				nTotLinha	:= 0     			// Incluso esta linha para impressao dos totais
				nPosCC	:= ASCAN(aTotCCSup,{|x| x[1]== cAntCCSup })			
				If  nPosCC > 0 							
					For nVezes := 1 to Len(aMeses)	// por periodo em 09/06/2004 por Otacilio
						nTotLinha	+= aTotCCSup[nPosCC][2][nVezes]
					Next
					@li,aColunas[COL_SEPARA1] PSAY "|"					
					ValorCTB(aTotCCSup[nPosCC][2][1],li,aColunas[COL_COLUNA1],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
					@ li,aColunas[COL_SEPARA4]		PSAY "|"
					ValorCTB(aTotCCSup[nPosCC][2][2],li,aColunas[COL_COLUNA2],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
					@ li,aColunas[COL_SEPARA5]		PSAY "|"
					ValorCTB(aTotCCSup[nPosCC][2][3],li,aColunas[COL_COLUNA3],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
					@ li,aColunas[COL_SEPARA6]		PSAY "|"
					ValorCTB(aTotCCSup[nPosCC][2][4],li,aColunas[COL_COLUNA4],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
					@ li,aColunas[COL_SEPARA7] PSAY "|"
					ValorCTB(aTotCCSup[nPosCC][2][5],li,aColunas[COL_COLUNA5],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
					@ li,aColunas[COL_SEPARA8] PSAY "|"
					ValorCTB(aTotCCSup[nPosCC][2][6],li,aColunas[COL_COLUNA6],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
					@ li,aColunas[COL_SEPARA9] PSAY "|"
					ValorCTB(aTotCCSup[nPosCC][2][7],li,aColunas[COL_COLUNA7],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
					@ li,aColunas[COL_SEPARA10] PSAY "|"
					ValorCTB(aTotCCSup[nPosCC][2][8],li,aColunas[COL_COLUNA8],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
					@ li,aColunas[COL_SEPARA11] PSAY "|"
					ValorCTB(aTotCCSup[nPosCC][2][9],li,aColunas[COL_COLUNA9],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
					@ li,aColunas[COL_SEPARA12] PSAY "|"
					ValorCTB(aTotCCSup[nPosCC][2][10],li,aColunas[COL_COLUNA10],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
					@ li,aColunas[COL_SEPARA13] PSAY "|"
					ValorCTB(aTotCCSup[nPosCC][2][11],li,aColunas[COL_COLUNA11],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
					@ li,aColunas[COL_SEPARA14] PSAY "|"
					ValorCTB(aTotCCSup[nPosCC][2][12],li,aColunas[COL_COLUNA12],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
					If MV_PAR30 = 1		// Imprime Total
						If MV_PAR31 == 1//Mostrar a conta
							@ li,aColunas[COL_SEPARA15]-20 PSAY "|"
							ValorCTB(nTotLinha,li,aColunas[COL_SEPARA15]-18,12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
						ElseIf MV_PAR31 == 2	//Mostrar a descricao
							@ li,aColunas[COL_SEPARA15]-15 PSAY "|"
							ValorCTB(nTotLinha,li,aColunas[COL_SEPARA15]-14,12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)
						EndIf
					Endif
					@ li,aColunas[COL_SEPARA15] PSAY "|"
					li++ 
					lImpCCSint	:= .T.
				EndIf
			Else
				lImpCCSint	:= .F.
			EndIf
		End
		cAntCCSup		:= ""
		cCCSup			:= ""
		dbSelectArea("cArqDad")
	EndIf
EndIf

IF li != 80 .And. !lEnd
	@li,00 PSAY REPLICATE("-",limite)
	li++
	@li,00 PSAY REPLICATE("-",limite)	
	li++
	@li,aColunas[COL_SEPARA1] PSAY "|"
	If MV_PAR30 == 2
		@li,aColunas[COL_CONTA]   PSAY "T O T A I S  D O  P E R I O D O: "  		//"T O T A I S  D O  P E R I O D O : "
		@ li,aColunas[COL_SEPARA3]		PSAY "|"
	Else
		@li,aColunas[COL_CONTA]   PSAY "TOTAIS DO PERIODO:"  		//"TOTAIS  DO  PERIODO: "
		If MV_PAR31 == 1		
			@ li,aColunas[COL_SEPARA2]		PSAY "|"	
		Else
			@ li,aColunas[COL_SEPARA2]+4   PSAY "|"			
		EndIf
	EndIf
	ValorCTB(aTotCol[1],li,aColunas[COL_COLUNA1],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA4]		PSAY "|"
	ValorCTB(aTotCol[2],li,aColunas[COL_COLUNA2],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA5]		PSAY "|"
	ValorCTB(aTotCol[3],li,aColunas[COL_COLUNA3],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA6]		PSAY "|"
	ValorCTB(aTotCol[4],li,aColunas[COL_COLUNA4],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA7] PSAY "|"	
	ValorCTB(aTotCol[5],li,aColunas[COL_COLUNA5],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA8] PSAY "|"
	ValorCTB(aTotCol[6],li,aColunas[COL_COLUNA6],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA9] PSAY "|"
	ValorCTB(aTotCol[7],li,aColunas[COL_COLUNA7],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA10] PSAY "|"
	ValorCTB(aTotCol[8],li,aColunas[COL_COLUNA8],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA11] PSAY "|"     
	ValorCTB(aTotCol[9],li,aColunas[COL_COLUNA9],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA12] PSAY "|"
	ValorCTB(aTotCol[10],li,aColunas[COL_COLUNA10],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA13] PSAY "|"		
	ValorCTB(aTotCol[11],li,aColunas[COL_COLUNA11],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
	@ li,aColunas[COL_SEPARA14] PSAY "|"
	ValorCTB(aTotCol[12],li,aColunas[COL_COLUNA12],12,nDecimais,CtbSinalMov(),cPicture,, , , , , ,lPrintZero)
	
	//TOTAL GERAL
	nTotGeral	:= aTotCol[1]+aTotCol[2]+aTotCol[3]+aTotCol[4]+aTotCol[5]+aTotCol[6]+aTotCol[7]
	nTotGeral 	+= aTotCol[8]+aTotCol[9]+aTotCol[10]+aTotCol[11]+aTotCol[12]
	
	If MV_PAR30 = 1		// Imprime Total        
		If MV_PAR31 == 1//Mostrar a conta
			@ li,aColunas[COL_SEPARA15]-20 PSAY "|"				
			ValorCTB(nTotGeral,li,aColunas[COL_SEPARA15]-18,12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)				
		ElseIf MV_PAR31 == 2	//Mostrar a descricao
			@ li,aColunas[COL_SEPARA15]-15 PSAY "|"				
			ValorCTB(nTotGeral,li,aColunas[COL_SEPARA15]-14,12,nDecimais,CtbSinalMov(),cPicture, NORMAL, , , , , ,lPrintZero)			
		EndIf
	Endif			

	nTotGeral	:= 0
	@ li,aColunas[COL_SEPARA15] PSAY "|"

	li++
	@li,00 PSAY REPLICATE("-",limite)	
	li++
	@li,0 PSAY " "	
	roda(cbcont,cbtxt,"M")
	Set Filter To	
EndIF

If aReturn[5] = 1
	Set Printer To
	Commit
	Ourspool(wnrel)
EndIf

dbSelectArea("cArqDad")
Set Filter To
dbCloseArea()
Ferase(cArqDad+GetDBExtension())
Ferase("cArqInd"+OrdBagExt())
dbselectArea("CT2")

MS_FLUSH()

//------------------------------------------------------------------------------------------
/*/{Protheus.doc} TestaSX1
Permite a manutenção de dados armazenados em .

@author    TOTVS | Developer Studio - Gerado pelo Assistente de Código
@version   1.xx
@since     15/05/2015
/*/
//------------------------------------------------------------------------------------------
Static Function TestaSX1(cPergunta, aPerguntas)

	Local nxZ := 1
	Local aSalvArea := GetArea()

	cPergunta  := IIf(cPergunta  == Nil,ParamIXB[1],cPergunta)
	aPerguntas := IIf(aPerguntas == Nil,ParamIXB[2],aPerguntas)

	dbSelectArea("SX1")
	dbSetOrder(1)

	For nxZ := 1 To Len(aPerguntas)
		RecLock("SX1",!dbSeek(cPergunta+StrZero(nxZ,2)))
		Replace  X1_Grupo   With  cPergunta
		Replace  X1_Ordem   With  StrZero(nxZ,2)
		Replace  X1_Pergunt With  aPerguntas[nxZ,2]
		Replace  X1_PerSpa  With  aPerguntas[nxZ,2]
		Replace  X1_PerEng  With  aPerguntas[nxZ,2]
		Replace  X1_Variavl With  "Mv_Ch"+IIf(nxZ <=9,AllTrim(Str(nxZ)),Chr(nxZ + 55))
		Replace  X1_Tipo    With  aPerguntas[nxZ,3]
		Replace  X1_Tamanho With  aPerguntas[nxZ,4]
		Replace  X1_Decimal With  aPerguntas[nxZ,5]
		Replace  X1_GSC     With  aPerguntas[nxZ,6]
		Replace  X1_Var01   With  "Mv_Par"+StrZero(nxZ,2)
		Replace  X1_Valid   With  aPerguntas[nxZ,7]
		Replace  X1_F3      With  aPerguntas[nxZ,8]
		If (aPerguntas[nxZ,6] == "C")
			For nxY := 9 To 13
				If (aPerguntas[nxZ,nxY] == "")
					Exit
				Else
					Do Case
					Case ((nxY - 8) == 1)
						Replace X1_Def01 With aPerguntas[nxZ,nxY]
					Case ((nxY - 8) == 2)
						Replace X1_Def02 With aPerguntas[nxZ,nxY]
					Case ((nxY - 8) == 3)
						Replace X1_Def03 With aPerguntas[nxZ,nxY]
					Case ((nxY - 8) == 4)
						Replace X1_Def04 With aPerguntas[nxZ,nxY]
					Case ((nxY - 8) == 5)
						Replace X1_Def05 With aPerguntas[nxZ,nxY]
					EndCase
				EndIf
			Next
		EndIf
		if len(aPerguntas[nxZ]) > 13
			Replace  X1_HELP    With  aPerguntas[nxZ,14]
		Endif	
		
		MsUnLock()
	Next

	RestArea(aSalvArea)

Return Nil

//------------------------------------------------------------------------------------------
/*/{Protheus.doc} CopDados
Permite a manutenção de dados armazenados em .

@author    TOTVS | Developer Studio - Gerado pelo Assistente de Código
@version   1.xx
@since     15/05/2015
/*/
//------------------------------------------------------------------------------------------
Static Function CopDados(cArqAux,cArqTmp)

If Select(cArqAux) > 0
	(cArqAux)->(dbGoTop())
	Do While !(cArqAux)->(Eof())
		if (cArqTmp)->(DBSeek((cArqAux)->CONTA))
			RecLock(cArqTmp,.f.) 
			Replace (cArqTmp)->COLUNA1 	with (cArqTmp)->COLUNA1+(cArqAux)->COLUNA1
			Replace (cArqTmp)->COLUNA2 	with (cArqTmp)->COLUNA2+(cArqAux)->COLUNA2
			Replace (cArqTmp)->COLUNA3 	with (cArqTmp)->COLUNA3+(cArqAux)->COLUNA3
			Replace (cArqTmp)->COLUNA4 	with (cArqTmp)->COLUNA4+(cArqAux)->COLUNA4
			Replace (cArqTmp)->COLUNA5 	with (cArqTmp)->COLUNA5+(cArqAux)->COLUNA5
			Replace (cArqTmp)->COLUNA6 	with (cArqTmp)->COLUNA6+(cArqAux)->COLUNA6
			Replace (cArqTmp)->COLUNA7 	with (cArqTmp)->COLUNA7+(cArqAux)->COLUNA7
			Replace (cArqTmp)->COLUNA8 	with (cArqTmp)->COLUNA8+(cArqAux)->COLUNA8
			Replace (cArqTmp)->COLUNA9 	with (cArqTmp)->COLUNA9+(cArqAux)->COLUNA9
			Replace (cArqTmp)->COLUNA10	with (cArqTmp)->COLUNA10+(cArqAux)->COLUNA10
			Replace (cArqTmp)->COLUNA11	with (cArqTmp)->COLUNA11+(cArqAux)->COLUNA11
			Replace (cArqTmp)->COLUNA12	with (cArqTmp)->COLUNA12+(cArqAux)->COLUNA12
			(cArqTmp)->(MsUnlock())
		Else
			RecLock(cArqTmp,.t.) 
			Replace (cArqTmp)->CONTA		with (cArqAux)->CONTA
			Replace (cArqTmp)->NORMAL		with (cArqAux)->NORMAL
			Replace (cArqTmp)->CTARES		with (cArqAux)->CTARES
			Replace (cArqTmp)->DESCCTA	with (cArqAux)->DESCCTA
			Replace (cArqTmp)->CUSTO		with "" //(cArqAux)->CUSTO
			Replace (cArqTmp)->CCRES		with (cArqAux)->CCRES
			Replace (cArqTmp)->DESCCC		with (cArqAux)->DESCCC
			Replace (cArqTmp)->ITEM		with (cArqAux)->ITEM
			Replace (cArqTmp)->ITEMRES	with (cArqAux)->ITEMRES
			Replace (cArqTmp)->DESCITEM 	with (cArqAux)->DESCITEM
			Replace (cArqTmp)->CLVL		with (cArqAux)->CLVL
			Replace (cArqTmp)->CLVLRES	with (cArqAux)->CLVLRES
			Replace (cArqTmp)->DESCCLVL 	with (cArqAux)->DESCCLVL			
			Replace (cArqTmp)->COLUNA1 	with (cArqAux)->COLUNA1
			Replace (cArqTmp)->COLUNA2 	with (cArqAux)->COLUNA2
			Replace (cArqTmp)->COLUNA3 	with (cArqAux)->COLUNA3
			Replace (cArqTmp)->COLUNA4 	with (cArqAux)->COLUNA4
			Replace (cArqTmp)->COLUNA5 	with (cArqAux)->COLUNA5
			Replace (cArqTmp)->COLUNA6 	with (cArqAux)->COLUNA6
			Replace (cArqTmp)->COLUNA7 	with (cArqAux)->COLUNA7
			Replace (cArqTmp)->COLUNA8 	with (cArqAux)->COLUNA8
			Replace (cArqTmp)->COLUNA9 	with (cArqAux)->COLUNA9
			Replace (cArqTmp)->COLUNA10	with (cArqAux)->COLUNA10
			Replace (cArqTmp)->COLUNA11	with (cArqAux)->COLUNA11
			Replace (cArqTmp)->COLUNA12	with (cArqAux)->COLUNA12
			Replace (cArqTmp)->TIPOCONTA	with (cArqAux)->TIPOCONTA
			Replace (cArqTmp)->TIPOCC		with (cArqAux)->TIPOCC
			Replace (cArqTmp)->TIPOITEM	with (cArqAux)->TIPOITEM
			Replace (cArqTmp)->TIPOCLVL	with (cArqAux)->TIPOCLVL
			Replace (cArqTmp)->CTASUP	with (cArqAux)->CTASUP
			Replace (cArqTmp)->CCSUP		with (cArqAux)->CCSUP
			Replace (cArqTmp)->ITSUP		with (cArqAux)->ITSUP
			Replace (cArqTmp)->CLSUP		with (cArqAux)->CLSUP
			Replace (cArqTmp)->ORDEM		with (cArqAux)->ORDEM
			Replace (cArqTmp)->GRUPO		with (cArqAux)->GRUPO
			Replace (cArqTmp)->TOTVIS		with (cArqAux)->TOTVIS
			Replace (cArqTmp)->SLDENT		with (cArqAux)->SLDENT			
			Replace (cArqTmp)->FATSLD		with (cArqAux)->FATSLD
			Replace (cArqTmp)->VISENT		with (cArqAux)->VISENT
			Replace (cArqTmp)->IDENTIFI		with (cArqAux)->IDENTIFI
			Replace (cArqTmp)->ESTOUR		with (cArqAux)->ESTOUR
			Replace (cArqTmp)->NIVEL1		with (cArqAux)->NIVEL1
			Replace (cArqTmp)->COLVISAO		with (cArqAux)->COLVISAO
			Replace (cArqTmp)->FILIAL		with (cArqAux)->FILIAL
			(cArqTmp)->(MsUnlock())				
		Endif
		(cArqAux)->(DBSkip())
	EndDo
	
Endif	                                                       

Return

//------------------------------------------------------------------------------------------
/*/{Protheus.doc} CriaArqDad
Permite a manutenção de dados armazenados em .

@author    TOTVS | Developer Studio - Gerado pelo Assistente de Código
@version   1.xx
@since     15/05/2015
/*/
//------------------------------------------------------------------------------------------
Static Function CriaArqDad(cArqTmp,cArqDad,cArqAux,cArqInd)
	
	If Empty(cArqDad)
		
		aStru 		:= (cArqTmp)->(DbStruct())
		cArqDad 	:= CriaTrab(aStru, .T.)
		dbUseArea( .T.,, cArqDad, cArqAux, .F., .F. )
			
		cChave		:= "CONTA"
		IndRegua(cArqAux,cArqInd,cChave,,,OemToAnsi("Selecionando Registros..."))
		dbSelectArea(cArqAux)
		DbClearIndex()
		dbSetIndex(cArqInd+OrdBagExt())			
	Endif		

Return


