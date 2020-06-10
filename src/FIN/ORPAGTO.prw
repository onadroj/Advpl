#INCLUDE "PROTHEUS.CH"
#include "topconn.ch"

// Impressão da Ordem de Pagamento em formato gráfico

User Function ORPAGTO()
Local oReport

oReport:= ReportDef()
oReport:PrintDialog()


Return

Static Function ReportDef()
Local cTitle   := "Emissao de Ordens de Pagamento"
Local oReport
Local oSection1
Local oSection2
Local nTamCdProd:= TamSX3("D1_COD")[1]
Local nTamCdDesc:= TamSX3("B1_DESC")[1]
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Variaveis utilizadas para parametros                         ³
//³ mv_par01               Da Entrada                            ³
//³ mv_par02               Ate a Entrada                         ³
//³ mv_par03               Do Fornecedor                         ³
//³ mv_par04               Ate o Fornecedor                      ³
//³ mv_par05               Do Título                             ³
//³ mv_par06               Até o Título    	                     ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

ValidPerg()
IF type("_lMT100AGR")=="U" .AND. type("_lFA050GRV")=="U"
	Pergunte("ORPAGTO",.T.)
ENDIF


//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Criacao do componente de impressao                                      ³
//³                                                                        ³
//³TReport():New                                                           ³
//³ExpC1 : Nome do relatorio                                               ³
//³ExpC2 : Titulo                                                          ³
//³ExpC3 : Pergunte                                                        ³
//³ExpB4 : Bloco de codigo que sera executado na confirmacao da impressao  ³
//³ExpC5 : Descricao                                                       ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
IF type("_lMT100AGR")=="U" .AND. type("_lFA050GRV")=="U"
	oReport:= TReport():New("ORPAGTO",cTitle,"ORPAGTO", {|oReport| ReportData(oReport)},"Impressão de Ordens de Pagamento em formato gráfico.")
ELSE
	oReport:= TReport():New("ORPAGTO",cTitle,"", {|oReport| ReportData(oReport)},"Impressão de Ordens de Pagamento em formato gráfico.")
ENDIF


oReport:SetPortrait()
oReport:HideParamPage()
oReport:HideHeader()
oReport:HideFooter()
oReport:SetTotalInLine(.F.)
oReport:nFontBody := 8
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
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
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
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
oSection1:= TRSection():New(oReport,"Ordem de Pagamento",{"SE2","SM0","SA2"},/*aOrdem*/)
oSection1:SetLineStyle()
oSection1:SetReadOnly()

TRCell():New(oSection1,"M0_NOMECOM","SM0",/*Titulo*/   ,/*Picture*/,49,/*lPixel*/,/*{|| code-block de impressao }*/)
TRCell():New(oSection1,"M0_CGC"    ,"SM0","CNPJ"       ,/*Picture*/,18,/*lPixel*/,{|| Transform(SM0->M0_CGC,PesqPict("SA2","A2_CGC")) })
TRCell():New(oSection1,"M0_ENDCOB" ,"SM0","ENDERECO"   ,/*Picture*/,48,/*lPixel*/,/*{|| code-block de impressao }*/)
TRCell():New(oSection1,"M0_CEPCOB" ,"SM0","CEP"        ,/*Picture*/,10,/*lPixel*/,{|| Transform(SM0->M0_CEPCOB,PesqPict("SA2","A2_CEP")) })
TRCell():New(oSection1,"M0_CIDCOB" ,"SM0","CIDADE"     ,/*Picture*/,20,/*lPixel*/,/*{|| code-block de impressao }*/)
TRCell():New(oSection1,"M0_ESTCOB" ,"SM0","UF"         ,/*Picture*/,11,/*lPixel*/,/*{|| code-block de impressao }*/)
TRCell():New(oSection1,"A2_NOME"   ,"SA2",/*Titulo*/   ,/*Picture*/,40,/*lPixel*/,/*{|| code-block de impressao }*/)
TRCell():New(oSection1,"A2_COD"    ,"SA2",/*Titulo*/   ,/*Picture*/,20,/*lPixel*/,/*{|| code-block de impressao }*/)
TRCell():New(oSection1,"A2_LOJA"   ,"SA2",/*Titulo*/   ,/*Picture*/,04,/*lPixel*/,/*{|| code-block de impressao }*/)
TRCell():New(oSection1,"A2_END"    ,"SA2",/*Titulo*/   ,/*Picture*/,40,/*lPixel*/,/*{|| code-block de impressao }*/)
TRCell():New(oSection1,"A2_BAIRRO" ,"SA2",/*Titulo*/   ,/*Picture*/,20,/*lPixel*/,/*{|| code-block de impressao }*/)
TRCell():New(oSection1,"A2_MUN"    ,"SA2",/*Titulo*/   ,/*Picture*/,15,/*lPixel*/,/*{|| code-block de impressao }*/)
TRCell():New(oSection1,"A2_EST"    ,"SA2",/*Titulo*/   ,/*Picture*/,02,/*lPixel*/,/*{|| code-block de impressao }*/)
TRCell():New(oSection1,"A2_CEP"    ,"SA2",/*Titulo*/   ,/*Picture*/,08,/*lPixel*/,/*{|| code-block de impressao }*/)
TRCell():New(oSection1,"A2_CGC"    ,"SA2",/*Titulo*/   ,/*Picture*/,/*Tamanho*/,/*lPixel*/,/*{|| code-block de impressao }*/)
TRCell():New(oSection1,"A2_CONTATO","   ","CONTATO"    ,/*Picture*/,25,/*lPixel*/,/*{|| code-block de impressao }*/)
TRCell():New(oSection1,"A2_TEL"    ,"   ","TELEFONE"   ,/*Picture*/,25,/*lPixel*/,{|| "("+Substr(SA2->A2_DDD,1,3)+") "+Substr(SA2->A2_TEL,1,15)})
TRCell():New(oSection1,"A2_INSCR"  ,"SA2",/*Titulo*/   ,/*Picture*/,18,/*lPixel*/,/*{|| code-block de impressao }*/)


oSection1:Cell("A2_BAIRRO"):SetCellBreak()
oSection1:Cell("A2_CGC"   ):SetCellBreak()
oSection1:Cell("A2_INSCR"    ):SetCellBreak()

oSection2:= TRSection():New(oSection1,"Intens da NF",{"SD1","SB1"},/*aOrdem*/)

oSection2:SetCellBorder("ALL",,,.T.)
//oSection2:SetCellBorder("RIGHT")
//oSection2:SetCellBorder("LEFT")

TRCell():New(oSection2,"D1_COD"    ,"SD1","Produto / Serviço",/*Picture*/,/*Tamanho*/,/*lPixel*/,/*{|| code-block de impressao }*/,,,,,,.F.)
TRCell():New(oSection2,"DESCPROD"  ,"   ","Descrição",/*Picture*/,nTamCdDesc,/*lPixel*/, {|| cDescPro},,,,,,.F.)
TRCell():New(oSection2,"NATUREZA"  ,"   ","Natureza" ,/*Picture*/,10,/*lPixel*/,{|| _E2_NATUREZ},,,,,,.F.)
TRCell():New(oSection2,"D1_CONTA"  ,"SD1","Conta Contábil",/*Picture*/,/*Tamanho*/,/*lPixel*/,/*{|| code-block de impressao }*/,,,,,,.F.)
TRCell():New(oSection2,"D1_CC"     ,"SD1","Centro de Custo",/*Picture*/,/*Tamanho*/,/*lPixel*/,/*{|| code-block de impressao }*/,,,,,,.F.)
TRCell():New(oSection2,"D1_TES"    ,"SD1","TES",/*Picture*/,/*Tamanho*/,/*lPixel*/,/*{|| code-block de impressao }*/,,,,,,.F.)
TRCell():New(oSection2,"D1_TOTAL"  ,"SD1","Vlr. Total","@E 99,999,999.99",/*Tamanho*/,/*lPixel*/,/*{|| code-block de impressao }*/,,,,,,.F.)

oSection2:Cell("D1_COD"):SetLineBreak()
oSection2:Cell("DESCPROD"):SetLineBreak()
oSection2:Cell("D1_CC"):SetLineBreak()

Return(oReport)

Static Function ReportData(oReport)
Local _cQuery := ""                
Local _cQueryAux := ""
Local _aArea
Local _nReg
Private _NUsuario := ""
Private _NUsuImp := ""
Private _aArray := {}
Private _E2_SEQ
Private _E2_VENCTO
Private _E2_VENCREA
Private _E2_FORNECE
Private _E2_LOJA
Private _E2_NUM
Private _E2_PARCELA
Private _E2_EMISSAO
Private _E2_CNTRATS
Private _E2_HIST
Private _E2_PREFIXO
Private _E2_VALOR
Private _E2_IRRF
Private _E2_INSS
Private _E2_ISS
Private _E2_PIS
Private _E2_COFINS
Private _E2_CSLL
Private _E2_ACRESC
Private _E2_DECRESC
Private _E2_RETCNTR
Private _E2_MDDESC
Private _E2_MDBONI
Private _E2_MDMULT
Private _E2_NATUREZ
Private _E2_NRORPGT
Private _E2_ORPGTIM          
Private _E2_DATALIB
Private _E2_USUALIB
Private _E2_MEDCOMP
Private _E2_ORIGEM
Private _E2_CCTBL
Private _E2_CUSTO

IF pswseek(__CUSERID)
	_aArray:=pswret()
	_NUsuImp := _aArray[1][4]
Endif


if type("_lMT100AGR")=="U" .AND. type("_lFA050GRV")=="U"
	_cQuery := "SELECT E2_FILIAL, E2_FILORIG, E2_TIPO, E2_VENCTO, E2_VENCREA, E2_FORNECE, E2_LOJA, E2_NUM, E2_PARCELA, E2_EMISSAO, E2_CNTRATS, "
	_cQuery += "E2_HIST, E2_PREFIXO, E2_VALOR, E2_IRRF, E2_INSS, E2_ISS, E2_PIS, E2_COFINS, E2_CSLL, E2_ACRESC, "
	_cQuery += "E2_DECRESC, E2_RETCNTR, E2_MDDESC, E2_MDBONI, E2_MDMULT, E2_NATUREZ, E2_USERLGI, E2_NRORPGT, E2_ORPGTIM, E2_DATALIB, E2_USUALIB, E2_ORIGEM, E2_CCTBL, E2_CUSTO  "
	_cQuery += "FROM " + RETSQLNAME("SE2") + " WHERE "
	_cQuery += "E2_EMIS1 >= '" + DTOS(MV_PAR01) + "' AND E2_EMIS1 <= '" + DTOS(MV_PAR02) + "' "
	_cQuery += "AND E2_FORNECE >= '" + MV_PAR03 +  "' AND E2_FORNECE <= '" + MV_PAR04 + "' "
	_cQuery += "AND E2_NUM >= '" + MV_PAR05 +  "' AND E2_NUM <= '"  + MV_PAR06 + "' "
	_cQuery += "AND E2_NRORPGT >= '" + MV_PAR07 +  "' AND E2_NRORPGT <= '"  + MV_PAR08 + "' "
	_cQuery += "AND SUBSTRING(E2_TIPO,3,1) <> '-' AND D_E_L_E_T_ <> '*' "
	_cQuery += "ORDER BY E2_EMIS1, E2_FORNECE, E2_NUM"
	
	
	TCQUERY _cQuery NEW ALIAS "QRY"
	dbSelectArea("QRY")
	DbGoTop()
	Do While !Eof()
		_E2_VENCTO := DTOC(STOD(QRY->E2_VENCTO))
		_E2_VENCREA := DTOC(STOD(QRY->E2_VENCREA))
		_E2_FORNECE := QRY->E2_FORNECE
		_E2_LOJA := QRY->E2_LOJA
		_E2_NUM := QRY->E2_NUM
		_E2_PARCELA := QRY->E2_PARCELA
		_E2_EMISSAO := DTOC(STOD(QRY->E2_EMISSAO))
		_E2_CNTRATS := QRY->E2_CNTRATS
        If !Empty(QRY->E2_CNTRATS)
			_E2_MEDCOMP := BuscaMed(QRY->E2_FILORIG,QRY->E2_NUM,QRY->E2_PREFIXO,QRY->E2_FORNECE,QRY->E2_LOJA)
		Else
			_E2_MEDCOMP := ""
        Endif
		_E2_HIST := QRY->E2_HIST
		_E2_PREFIXO := QRY->E2_PREFIXO
		_E2_VALOR := QRY->E2_VALOR
		_E2_IRRF := QRY->E2_IRRF
		_E2_INSS := QRY->E2_INSS
		_E2_ISS := QRY->E2_ISS
		_E2_PIS := QRY->E2_PIS
		_E2_COFINS := QRY->E2_COFINS
		_E2_CSLL := QRY->E2_CSLL
		_E2_ACRESC := QRY->E2_ACRESC
		_E2_DECRESC := QRY->E2_DECRESC
		_E2_RETCNTR := QRY->E2_RETCNTR
		_E2_MDDESC := QRY->E2_MDDESC
		_E2_MDBONI := QRY->E2_MDBONI
		_E2_MDMULT := QRY->E2_MDMULT
		_E2_NATUREZ := QRY->E2_NATUREZ
		_E2_NRORPGT := QRY->E2_NRORPGT
		_E2_ORPGTIM := QRY->E2_ORPGTIM
		_E2_DATALIB := QRY->E2_DATALIB
		_E2_USUALIB := QRY->E2_USUALIB
		_E2_ORIGEM := QRY->E2_ORIGEM
		_E2_CCTBL := QRY->E2_CCTBL
		_E2_CUSTO := QRY->E2_CUSTO
//		_NUsuario := FWLeUserlg("QRY->E2_USERLGI",1)
		Posicione("SE2",1,QRY->E2_FILIAL+QRY->E2_PREFIXO+QRY->E2_NUM+QRY->E2_PARCELA+QRY->E2_TIPO+QRY->E2_FORNECE+QRY->E2_LOJA,"E2_USERLGI")
		_NUsuario := FWLeUserlg("SE2->E2_USERLGI",1)
		
		psworder(1)
		_nReg:="000000"
		While _nReg<"000300"
			IF pswseek(_nReg)
				_aArray:=pswret()
				IF Len(_aArray)>0
					IF Alltrim(_aArray[1][2])==Alltrim(_NUsuario)
						_NUsuario := _aArray[1][4]
						Exit
					ENDIF
				ENDIF
			Endif
			_nReg:=strzero(val(_nReg)+1,6)
		EndDo
		
		ReportPrint(oReport)

		If _E2_ORPGTIM<>"S"
			_cQueryAux := "UPDATE " + RETSQLNAME("SE2") + " SET E2_ORPGTIM = 'S'"
			_cQueryAux += " WHERE E2_FORNECE = '" + QRY->E2_FORNECE + "' AND E2_LOJA = '" + QRY->E2_LOJA + "' AND E2_NUM = '" + QRY->E2_NUM + "'"
			_cQueryAux += " AND E2_PARCELA = '" + QRY->E2_PARCELA + "' AND E2_PREFIXO = '" + QRY->E2_PREFIXO + "'"
			_cQueryAux += " AND D_E_L_E_T_ <> '*'"
			nret:=TCSQLExec(_cQueryAux)
		Endif
		
		DbSkip()
	EndDo
	
	CLOSE
else
	_E2_VENCTO := DTOC(SE2->E2_VENCTO)
	_E2_VENCREA := DTOC(SE2->E2_VENCREA)
	_E2_FORNECE := SE2->E2_FORNECE
	_E2_LOJA := SE2->E2_LOJA
	_E2_NUM := SE2->E2_NUM
	_E2_PARCELA := SE2->E2_PARCELA
	_E2_EMISSAO := DTOC(SE2->E2_EMISSAO)
	_E2_CNTRATS := SE2->E2_CNTRATS
	If !Empty(SE2->E2_CNTRATS)
		_E2_MEDCOMP := BuscaMed(SE2->E2_FILORIG,SE2->E2_NUM,SE2->E2_PREFIXO,SE2->E2_FORNECE,SE2->E2_LOJA)
	Else
		_E2_MEDCOMP := ""
	Endif
	_E2_HIST := SE2->E2_HIST
	_E2_PREFIXO := SE2->E2_PREFIXO
	_E2_VALOR := SE2->E2_VALOR
	_E2_IRRF := SE2->E2_IRRF
	_E2_INSS := SE2->E2_INSS
	_E2_ISS := SE2->E2_ISS
	_E2_PIS := SE2->E2_PIS
	_E2_COFINS := SE2->E2_COFINS
	_E2_CSLL := SE2->E2_CSLL
	_E2_ACRESC := SE2->E2_ACRESC
	_E2_DECRESC := SE2->E2_DECRESC
	_E2_RETCNTR := SE2->E2_RETCNTR
	_E2_MDDESC := SE2->E2_MDDESC
	_E2_MDBONI := SE2->E2_MDBONI
	_E2_MDMULT := SE2->E2_MDMULT
	_E2_NATUREZ := SE2->E2_NATUREZ
	_E2_NRORPGT := SE2->E2_NRORPGT
	_E2_ORPGTIM := SE2->E2_ORPGTIM
	_E2_DATALIB := SE2->E2_DATALIB
	_E2_USUALIB := SE2->E2_USUALIB
	_E2_ORIGEM := SE2->E2_ORIGEM
	_E2_CCTBL := SE2->E2_CCTBL
	_E2_CUSTO := SE2->E2_CUSTO
	_NUsuario := FWLeUserlg("SE2->E2_USERLGI",1)
	
	IF pswseek(__CUSERID)
		_aArray:=pswret()
		_NUsuario := _aArray[1][4]
	Endif
	
	_aArea := GetArea()
	ReportPrint(oReport)
	RestArea(_aArea)

	If _E2_ORPGTIM<>"S"
		RecLock("SE2",.F.)
		SE2->E2_ORPGTIM := "S"
		MsUnlock()
	Endif

endif

Return()


Static Function ReportPrint(oReport)
Local oSection1   := oReport:Section(1)
Local oSection2   := oReport:Section(1):Section(1)

Local cPictVTot  := PesqPict("SD1","D1_TOTAL")
Local nX          := 0
Local nY          := 0
Local nVias       := 0
Local nPageWidth  := 2390 // oReport:PageWidth()
Local nPrinted    := 0
Local nLinPC	  := 0
Local nLinObs     := 0
Local nDescProd   := 0
Local nPagina     := 0
Local cUserId     := RetCodUsr()
Local cCont       := Nil
Local lImpri      := .F.
Local _aAreaX     := GetArea()

Private cDescPro  := ""

MakeAdvplExpr(oReport:uParam)

TRPosition():New(oSection2,"SB1",1,{ || xFilial("SB1") + SD1->D1_COD })

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Executa o CodeBlock com o PrintLine da Sessao 1 toda vez que rodar o oSection1:Init()   ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
oReport:onPageBreak( { || CabecOP(oReport,oSection1) })

//oReport:SetMeter(SE2->(LastRec()))
dbSelectArea("SD1")
dbSelectArea("SD1")
dbSetOrder(1)
dbSeek(xFilial("SD1")+_E2_NUM+_E2_PREFIXO+_E2_FORNECE+_E2_LOJA,.T.)

oSection2:Init()

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Dispara a cabec especifica do relatorio.                     ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
oReport:EndPage()


While !oReport:Cancel() .And. !Eof() .And. xFilial("SD1")+SD1->D1_DOC+SD1->D1_SERIE+SD1->D1_FORNECE+SD1->D1_LOJA==xFilial("SD1")+_E2_NUM+_E2_PREFIXO+_E2_FORNECE+_E2_LOJA
	
	If oReport:Cancel()
		Exit
	EndIf
	
	//			oReport:IncMeter()
	
	If oReport:Row() > oReport:LineHeight() * 100
		oReport:Box( oReport:Row(),010,oReport:Row() + oReport:LineHeight() * 3, nPageWidth ) // Box do rodapé "Continua..."
		oReport:SkipLine()
		oReport:PrintText("Continua na Proxima pagina ....",, 050 )
		oReport:EndPage()
	EndIf
	
	cDescPro :=  ""
	cDescPro := Alltrim(RetField("SB1",1,xFilial("SB1")+SD1->D1_COD,"B1_DESC"))
	
	oSection2:PrintLine()
	
	nPrinted ++
	lImpri  := .T.
	
	/*
	If oReport:Row() > oReport:LineHeight() * 68
	
	oReport:Box( oReport:Row(),010,oReport:Row() + oReport:LineHeight() * 3, nPageWidth )
	oReport:SkipLine()
	oReport:PrintText("Continua na Proxima pagina ....",, 050 )
	
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Dispara a cabec especifica do relatorio.                     ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	oReport:EndPage()
	oReport:PrintText(" ",1992 , 010 ) // Necessario para posicionar Row() para a impressao do Rodape
	
	oReport:Box( 280,010,oReport:Row() + oReport:LineHeight() * ( 93 - nPrinted ) , nPageWidth )
	
	Else
	oReport:Box( oReport:Row(),oReport:Col(),oReport:Row() + oReport:LineHeight() * ( 93 - nPrinted ) , nPageWidth )
	EndIf
	*/
	//	oReport:Box( 1990 ,010,oReport:Row() + oReport:LineHeight() * ( 93 - nPrinted ) , nPageWidth )
	
	
	dbSkip()
	
EndDo

If "FIN" $ _E2_ORIGEM
	oReport:PrintText(" ",, 15 )
	oReport:Box( oReport:Row(),oReport:Col(),oReport:Row() + 40 , nPageWidth ) 
	oReport:PrintText("Produto \ Serviço    |Descrição                        |Natureza            |Conta Contábil      |Centro de Custo     |TES|Vlr. Total",, 15 )
	oReport:SkipLine()
	oReport:PrintText("ITEN NÃO ESPECIFICADO – LANÇADO A PARTIR DO FINANCEIRO |"+_E2_NATUREZ+Replicate(" ",20-Len(_E2_NATUREZ))+"|"+_E2_CCTBL+Replicate(" ",20-Len(_E2_CCTBL))+"|"+_E2_CUSTO+Replicate(" ",20-Len(_E2_CUSTO))+"|",, 15 )
EndIf

If oReport:Row() > oReport:LineHeight() * 100
	oReport:Box( oReport:Row(),010,oReport:Row() + oReport:LineHeight() * 3, nPageWidth )  // Box do rodapé "Continua..."
	oReport:SkipLine()
	oReport:PrintText("Continua na Proxima pagina ....",, 050 )
	oReport:EndPage()
EndIf

/******************************** Imprimindo desposicionado - verificar
oReport:Box( oReport:Row(),oReport:Col(),oReport:Row() + 570 , nPageWidth ) // Box do final da página
*/
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("Valor a Pagar: " + Transform(_E2_VALOR,"@E 99,999,999.99"),nLinPC,15)
oReport:SkipLine(2)
nLinPC := oReport:Row()
oReport:PrintText("VALORES:",nLinPC,15)
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("IR:          " + Transform(_E2_IRRF,"@E 99,999,999.99"),nLinPC,15)
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("INSS:        " + Transform(_E2_INSS,"@E 99,999,999.99"),nLinPC,15)
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("ISS:         " + Transform(_E2_ISS,"@E 99,999,999.99"),nLinPC,15)
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("PIS:         " + Transform(_E2_PIS,"@E 99,999,999.99"),nLinPC,15)
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("COFINS:      " + Transform(_E2_COFINS,"@E 99,999,999.99"),nLinPC,15)
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("CSL:         " + Transform(_E2_CSLL,"@E 99,999,999.99"),nLinPC,15)
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("Acrescimo:   " + Transform(_E2_ACRESC,"@E 99,999,999.99"),nLinPC,15)
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("Decrescimo:  " + Transform(_E2_DECRESC,"@E 99,999,999.99"),nLinPC,15)
oReport:SkipLine(2)
nLinPC := oReport:Row()
oReport:PrintText("CONTRATO:",nLinPC,15)
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("Retenção:    " + Transform(_E2_RETCNTR,"@E 99,999,999.99"),nLinPC,15)
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("Desconto:    " + Transform(_E2_MDDESC,"@E 99,999,999.99"),nLinPC,15)
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("Bonificação: " + Transform(_E2_MDBONI,"@E 99,999,999.99"),nLinPC,15)
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("Multa:       " + Transform(_E2_MDMULT,"@E 99,999,999.99"),nLinPC,15)
oReport:SkipLine(2)
oReport:Box( oReport:Row(),oReport:Col(),oReport:Row() + 120 , nPageWidth )
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("PAGUE-SE A QUANTIA LIQUIDA DE R$ " + Transform(_E2_VALOR,"@E 99,999,999.99")+"  ( " + Extenso(_E2_VALOR) + " )",nLinPC,15)
oReport:SkipLine(3)
oReport:Box( oReport:Row(),oReport:Col(),oReport:Row() + 270 , nPageWidth )
nLinPC := oReport:Row()
oReport:PrintText("Assinaturas:",nLinPC,15)
oReport:SkipLine(5)
nLinPC := oReport:Row()
oReport:PrintText("Gerente Financeiro",nLinPC,15)
oReport:PrintText("Diretor Técnico Comercial",nLinPC,615)
oReport:PrintText("Coord. Planejamento",nLinPC,1315)
oReport:PrintText("Resp. pela Liberação",nLinPC,1915)
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("Carimbo / Assinatura",nLinPC,1915)
oReport:SkipLine(3)
nLinPC := oReport:Row()
oReport:PrintText("Responsável pela inclusão: "+_NUsuario,nLinPC,15)
oReport:SkipLine()
nLinPC := oReport:Row()

oReport:PrintText("Impresso por: "+_NUsuImp,nLinPC,15)
If _E2_ORPGTIM=="S"
	oReport:SkipLine()
	nLinPC := oReport:Row()
	oReport:PrintText("** Reimpressão **",nLinPC,15)
Endif
If AllTrim(_E2_DATALIB)<>""
	oReport:SkipLine()
	nLinPC := oReport:Row()
	oReport:PrintText("** Pagamento já liberado por: " + AllTrim(_E2_USUALIB) + " em " + DTOC(STOD(_E2_DATALIB)) + ". **",nLinPC,15)
Endif

oSection2:Finish()

RestArea(_aAreaX)

Return

Static Function CabecOP(oReport,oSection1)

Local nLinPC := 0
Local nPageWidth:= 2390
//TRPosition():New(oSection1,"SA2",1,{ || xFilial("SA2") + SE2->E2_FORNECE + SE2->E2_LOJA })

oSection1:Init()

oReport:Box( 010 , 010,  260 , nPageWidth )
oReport:Box( 260 , 010,  320 , nPageWidth )
oReport:Box( 320 , 010,  715 , nPageWidth )

//oReport:PrintText( " ",,oSection1:Cell("M0_NOMECOM"):ColPos())

oReport:SkipLine(2)
nLinPC := oReport:Row()
oReport:PrintText(SM0->M0_NOMECOM,nLinPC,15)
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("CNPJ: " + Transform(SM0->M0_CGC,PesqPict("SA2","A2_CGC")) ,nLinPC,15)
oReport:PrintText("IE: " + SM0->M0_INSC,nLinPC,515)
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("END: " + SM0->M0_ENDCOB,nLinPC,15)
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("CEP: " + Trans(SM0->M0_CEPCOB,PesqPict("SA2","A2_CEP"))+Space(2)+"CIDADE: " + SM0->M0_CIDCOB + "UF: " + SM0->M0_ESTCOB ,nLinPC,15)
oReport:SkipLine(4)
nLinPC := oReport:Row()
oReport:PrintText("O  R  D  E  M   D  E   P  A  G  A  M  E  N  T  O    -    No. " + _E2_NRORPGT,nLinPC,755)
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("Data de Impressão: " + DTOC(MSDATE()) + " " + TIME(),nLinPC,15)
oReport:PrintText("Data de Vencimento: " + _E2_VENCTO,nLinPC,815)
oReport:PrintText("Data de Pagamento: " + _E2_VENCREA,nLinPC,1415)
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("Credor: " + _E2_FORNECE + "-" + _E2_LOJA + " " + RETFIELD("SA2",1,XFILIAL("SA2")+_E2_FORNECE+_E2_LOJA,"A2_NOME"),nLinPC,15)
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("CNPJ: " + Transform(RETFIELD("SA2",1,XFILIAL("SA2")+_E2_FORNECE+_E2_LOJA,"A2_CGC"),PesqPict("SA2","A2_CGC")),nLinPC,15)
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("Contato: " + RETFIELD("SA2",1,XFILIAL("SA2")+_E2_FORNECE+_E2_LOJA,"A2_CONTATO"),nLinPC,15)
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("Telefone: " + RETFIELD("SA2",1,XFILIAL("SA2")+_E2_FORNECE+_E2_LOJA,"A2_TEL"),nLinPC,15)
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("E-mail: " + RETFIELD("SA2",1,XFILIAL("SA2")+_E2_FORNECE+_E2_LOJA,"A2_EMAIL"),nLinPC,15)
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("Endereco: " + Alltrim(RETFIELD("SA2",1,XFILIAL("SA2")+_E2_FORNECE+_E2_LOJA,"A2_LOGRAD")) + " " + Alltrim(RETFIELD("SA2",1,XFILIAL("SA2")+_E2_FORNECE+_E2_LOJA,"A2_NR_END")) + " " ;
+ Alltrim(RETFIELD("SA2",1,XFILIAL("SA2")+_E2_FORNECE+_E2_LOJA,"A2_COMPLEM")) + " "  + Alltrim(RETFIELD("SA2",1,XFILIAL("SA2")+_E2_FORNECE+_E2_LOJA,"A2_BAIRRO")) + " " ;
+ Alltrim(RETFIELD("SA2",1,XFILIAL("SA2")+_E2_FORNECE+_E2_LOJA,"A2_MUN")) + "-" + Alltrim(RETFIELD("SA2",1,XFILIAL("SA2")+_E2_FORNECE+_E2_LOJA,"A2_EST")),nLinPC,15)
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("NF No.: " + _E2_NUM + " / Parcela: " + _E2_PARCELA,nLinPC,15)
oReport:PrintText("Data de Emissao: " + _E2_EMISSAO,nLinPC,615)
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("Contrato(s) No.: " + _E2_CNTRATS,nLinPC,15)
oReport:SkipLine()
nLinPC := oReport:Row()
If Len(_E2_MEDCOMP) > 116
	oReport:PrintText("Medição-Competência: " + Substr(_E2_MEDCOMP,1,116),nLinPC,15)
Else
	oReport:PrintText("Medição-Competência: " + _E2_MEDCOMP,nLinPC,15)
Endif
oReport:SkipLine()
nLinPC := oReport:Row()
If Len(_E2_MEDCOMP) > 116
	oReport:PrintText(Substr(_E2_MEDCOMP,120,Len(_E2_MEDCOMP)-120),nLinPC,15)
Endif
oReport:SkipLine()
nLinPC := oReport:Row()
oReport:PrintText("Historico: " + _E2_HIST,nLinPC,15)
oReport:SkipLine(2)

oSection1:Finish()

Return

Static Function ValidPerg()
cPerg := PADR("ORPAGTO",10)
//PutSx1(cGrupo,cOrdem,cPergunt,cPerSpa,cPerEng,cVar,cTipo,nTamanho,nDecimal,nPresel,cGSC,cValid,cF3,cGrpSxg,cPyme,cVar01,cDef01,cDefSpa1,cDefEng1,cCnt01,cDef02,cDefSpa2,cDefEng2,cDef03,cDefSpa3,cDefEng3,cDef04,cDefSpa4,cDefEng4,cDef05,cDefSpa5,cDefEng5,aHelpPor,aHelpEng,aHelpSpa,cHelp)
PutSx1(cPerg,"01","Dt.Entrada De: ","Dt.Entrada De: ","Dt.Entrada De: ","mv_ch1","D",8,0,0,"G","","","","","mv_par01","","","","","","","","","","","","","","","","")
PutSx1(cPerg,"02","Dt.Entrada Ate: ","Dt.Entrada Ate: ","Dt.Entrada Ate: ","mv_ch2","D",8,0,0,"G","","","","","mv_par02","","","","","","","","","","","","","","","","")
PutSx1(cPerg,"03","De Fornecedor: ","De Fornecedor: ","De Fornecedor: ","mv_ch3","C",6,0,0,"G","","SA2","","","mv_par03","","","","","","","","","","","","","","","","")
PutSx1(cPerg,"04","Ate Fornecedor: ","Ate Fornecedor: ","Ate Fornecedor: ","mv_ch4","C",6,0,0,"G","","SA2","","","mv_par04","","","","","","","","","","","","","","","","")
PutSx1(cPerg,"05","De Titulo: ","De Titulo: ","De Titulo: ","mv_ch5","C",12,0,0,"G","","","","","mv_par05","","","","","","","","","","","","","","","","")
PutSx1(cPerg,"06","Ate Titulo: ","Ate Titulo: ","Ate Titulo: ","mv_ch6","C",12,0,0,"G","","","","","mv_par06","","","","","","","","","","","","","","","","")
PutSx1(cPerg,"07","De Ord.Pagto.: ","De Ord.Pagto.: ","De Ord.Pagto.: ","mv_ch7","C",9,0,0,"G","","","","","mv_par07","","","","","","","","","","","","","","","","")
PutSx1(cPerg,"08","Ate Ord.Pagto.: ","Ate Ord.Pagto.: ","Ate Ord.Pagto.: ","mv_ch8","C",9,0,0,"G","","","","","mv_par08","","","","","","","","","","","","","","","","")
RETURN()

Static Function BuscaMed(_cFilorig,_cNum,_cPrefixo,_cFornece,_cLoja)
Local _aAreaX := GetArea()
Local _cMed := ""
Local _cQuery := ""


_cQuery := "SELECT CND_NUMMED, CND_COMPET FROM "+RetSqlName("CND")+" CND INNER JOIN "
_cQuery += "(SELECT D1_PEDIDO FROM "+RetSqlName("SD1")+" SD1 INNER JOIN "+RetSqlName("SE2")+" SE2 "
_cQuery += "ON (D1_FILIAL='"+_cFilorig+"' AND D1_DOC='"+_cNum+"' AND D1_SERIE='"+_cPrefixo+"' AND D1_FORNECE='"+_cFornece+"' AND D1_LOJA='"+_cLoja+"') "
_cQuery += "WHERE SD1.D_E_L_E_T_<>'*' AND SE2.D_E_L_E_T_<>'*') ORIG "
_cQuery += "ON (CND_PEDIDO=ORIG.D1_PEDIDO) "
_cQuery += "WHERE CND.D_E_L_E_T_ <>'*' "
_cQuery += "GROUP BY CND_NUMMED, CND_COMPET "
_cQuery += "ORDER BY CND_NUMMED"

TCQUERY _cQuery NEW ALIAS "QRYMED"
dbSelectArea("QRYMED")
DbGoTop()
Do While !Eof()
	If Empty(_cMed)
		_cMed := QRYMED->CND_NUMMED+"-"+QRYMED->CND_COMPET
	Else
		_cMed += " / "+QRYMED->CND_NUMMED+"-"+QRYMED->CND_COMPET
	Endif
	DbSkip()
Enddo
CLOSE

RestArea(_aAreaX)
Return(_cMed)