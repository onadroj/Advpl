#include 'fivewin.ch'
#include 'topconn.ch'
#include "TOTVS.CH"
#INCLUDE "RPTDEF.CH"
#INCLUDE "FWPrintSetup.ch"
#INCLUDE "protheus.ch"
/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหออออออัอออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณ RLPEDCG  บAutor ณ Felipe Batista      บ Data ณ  06/08/08   บฑฑ
ฑฑฬออออออออออุออออออออออสออออออฯอออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDesc.     ณ PEDIDO DE COMPRAS (Emissao em formato Grafico)             บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ Compras                                                    บฑฑ
ฑฑฬออออออออออุออออออออออัอออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบDATA      ณ ANALISTA ณ MOTIVO                                          บฑฑ
ฑฑฬออออออออออุออออออออออุอออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบ          ณ          ณ                                                 บฑฑ
ฑฑศออออออออออฯออออออออออฯอออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
*/

User Function RLPEDCG()
Private	lEnd		:= .f.,;
aAreaSC7	:= SC7->(GetArea()),;
aAreaSA2	:= SA2->(GetArea()),;
aAreaSA5	:= SA5->(GetArea()),;
aAreaSF4	:= SF4->(GetArea()),;
cPais 		:= "",;
cPerg		:= 'RLPEDC    '

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณAjusta os parametros.ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
//		AjustaSX1(cPerg)
If Funname() <> "MATA160"
	If	( ! Pergunte(cPerg,.T.) )
		Return
	Else
		Private	cNumPed  	:= mv_par01			// Numero do Pedido de Compras
		Private	lImpPrc		:= (mv_par02==2)	// Imprime os Precos ?
		Private	nTitulo 	:= mv_par03			// Titulo do Relatorio ?
		Private cComplPr 	:= ""
		Private	cObserv1	:= mv_par04			// 1a Linha de Observacoes
		Private	cObserv2	:= mv_par05			// 2a Linha de Observacoes
		Private	cObserv3	:= mv_par06			// 3a Linha de Observacoes
		//			Private	cObserv4	:= mv_par07			// 4a Linha de Observacoes
		Private	lPrintCodFor:= (mv_par08==1)	// Imprime o Codigo do produto no fornecedor ?
	EndIf
Else
	Private	cNumPed  	:= SC7->C7_NUM		// Numero do Pedido de Compras
	Private	lImpPrc		:= .T.				// Imprime os Precos ?
	Private	cObserv1	:= ""				// 1a Linha de Observacoes
	Private	cObserv2	:= ""				// 2a Linha de Observacoes
	Private	cObserv3	:= ""				// 3a Linha de Observacoes
	Private	lPrintCodFor:= .T.				// Imprime o Codigo do produto no fornecedor ?
EndIf
DbSelectArea('SC7')
SC7->(DbSetOrder(1))
If	( ! SC7->(DbSeek(xFilial('SC7') + cNumPed)) )
	Help('',1,'REGNOIS',,OemToAnsi('Pedido nใo encontrado.'),1)
	Return .f.
EndIf

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณExecuta a rotina de impressao ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
Processa({ |lEnd| xPrintRel(),OemToAnsi('Gerando o relat๓rio.')}, OemToAnsi('Aguarde...'))

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณRestaura a area anterior ao processamento. !ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
RestArea(aAreaSC7)
RestArea(aAreaSA2)
RestArea(aAreaSA5)
RestArea(aAreaSF4)



Static  Function xPrintRel()
Private nMoe   := 0
Private nTxmoe := 0
Private cFret  := ""
Private cCond  := ""
Private cDescP := ""
Private lDesc2 := .F.
Private cObse  := ""
Private cPartnum := ""
Private cImpFabr := ""
Private	oPrint := TMSPrinter():New(OemToAnsi('Pedido de Compras')),;
oBrush		:= TBrush():New(,4),;
oPen		:= TPen():New(0,5,CLR_BLACK),;
nPage		:= SetPage ( 10 ),;
cFileLogotp	:= GetSrvProfString('Startpath','') + 'lgrl01' + '.BMP',;
oFont07		:= TFont():New('Courier New',07,07,,.F.,,,,.T.,.F.),;
oFont08		:= TFont():New('Courier New',08,08,,.F.,,,,.T.,.F.),;
oFont09		:= TFont():New('Tahoma',09,09,,.F.,,,,.T.,.F.),;
oFont10		:= TFont():New('Tahoma',10,10,,.T.,,,,.T.,.F.),;
oFont10t	:= TFont():New('Tahoma',10,10,,.F.,,,,.T.,.F.),;
oFont10n	:= TFont():New('Courier New',10,10,,.T.,,,,.T.,.F.),;
oFont11		:= TFont():New('Tahoma',11,11,,.F.,,,,.T.,.F.),;
oFont12		:= TFont():New('Tahoma',12,12,,.T.,,,,.T.,.F.),;
oFont12n	:= TFont():New('Tahoma',12,12,,.F.,,,,.T.,.F.),;
oFont13		:= TFont():New('Tahoma',13,13,,.T.,,,,.T.,.F.),;
oFont14		:= TFont():New('Tahoma',14,14,,.T.,,,,.T.,.F.),;
oFont15		:= TFont():New('Courier New',15,15,,.T.,,,,.T.,.F.),;
oFont18		:= TFont():New('Arial',18,18,,.T.,,,,.T.,.T.),;
oFont16		:= TFont():New('Arial',16,16,,.T.,,,,.T.,.F.),;
oFont20		:= TFont():New('Arial',20,20,,.F.,,,,.T.,.F.),;
oFont22		:= TFont():New('Arial',22,22,,.T.,,,,.T.,.F.);

Private	lFlag		:= .t.,;	// Controla a impressao do fornecedor
nLinha		:= 3000,;	// Controla a linha por extenso
nLinFim		:= 0,;		// Linha final para montar a caixa dos itens
lPrintDesTab:= .f.,;	// Imprime a Descricao da tabela (a cada nova pagina)
cRepres		:= Space(80)

Private	_nQtdReg	:= 0,;		// Numero de registros para intruir a regua
_nValMerc 	:= 0,;		// Valor das mercadorias
_nValIPI	:= 0,;		// Valor do I.P.I.
_nValDesc	:= 0,;		// Valor de Desconto
_nTotAcr	:= 0,;		// Valor total de acrescimo
_nTotSeg	:= 0,;		// Valor de Seguro
_nTotFre	:= 0,;		// Valor de Frete
_nTotIcmsRet:= 0,;
_nTotVcv	:= 0		// Valor do ICMS Retido



//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณPosiciona nos arquivos necessarios. !ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
DbSelectArea('SA2')
SA2->(DbSetOrder(1))
If	! SA2->(DbSeek(xFilial('SA2')+SC7->(C7_FORNECE+C7_LOJA)))
	Help('',1,'REGNOIS')
	Return .f.
EndIf

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณDefine que a impressao deve ser RETRATOณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
oPrint:SetPortrait()


//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณMonta query !ณ    //SC7.C7_CODPRF,
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤู
cSELECT :=	'SC7.C7_FILIAL, SC7.C7_NUM, SC7.C7_EMISSAO, SC7.C7_FORNECE, SC7.C7_LOJA,SC7.C7_TPFRETE,SC7.C7_NUMCOT, '+;
'SC7.C7_ITEM, SC7.C7_PRODUTO, SC7.C7_DESCRI, SC7.C7_QUANT, SC7.C7_CC,SC7.C7_TRANSP,SC7.C7_TXMOEDA,SC7.C7_VLCONV, '+;
'SC7.C7_OBS, SC7.C7_PRECO, SC7.C7_IPI, SC7.C7_TOTAL, SC7.C7_VLDESC, SC7.C7_DESPESA,SC7.C7_MOEDA,SC7.C7_COND,SC7.C7_UM, '+;
'SC7.C7_SEGURO, SC7.C7_VALFRE, SC7.C7_TES, SC7.C7_ICMSRET, SC7.C7_ITEMCTA, SC7.C7_NUMSC,SC7.C7_UM, SB1.B1_FABRIC, '+;
'SB1.B1_FABRIC2, SB1.B1_FABRIC3, SB1.B1_FABRIC4, SB1.B1_FABRIC5, SB1.B1_FABRIC6, SB1.B1_FABRIC7, SB1.B1_FABRIC8 '

cFROM   :=	RetSqlName('SC7') + ' SC7 '

cINNERJ := RetSqlName('SB1') + ' SB1 '

cON := 'SC7.C7_FILIAL = SB1.B1_FILIAL AND SC7.C7_PRODUTO = SB1.B1_COD '

cWHERE  :=	'SC7.D_E_L_E_T_ <> '+CHR(39) + '*'            +CHR(39) + ' AND '+;
'SC7.C7_FILIAL  =    '+CHR(39) + xFilial('SC7') +CHR(39) + ' AND '+;
'SC7.C7_NUM     =    '+CHR(39) + cNumPed        +CHR(39) + 'AND '+;
'SB1.D_E_L_E_T_ <> '+CHR(39) + '*' +CHR(39) + ' AND '+;
'SC7.D_E_L_E_T_ <> '+CHR(39) + '*' +CHR(39)

cORDER  :=	'SC7.C7_FILIAL, SC7.C7_ITEM '

cQuery  :=	' SELECT '   + cSELECT + ;
' FROM '     + cFROM   + ;
' INNER JOIN ' + cINNERJ + ;
' ON ' + cON + ;
' WHERE '    + cWHERE  + ;
' ORDER BY ' + cORDER

TCQUERY cQuery NEW ALIAS 'TRA'

If	! USED()
	MsgBox(cQuery+'. Query errada','RLPEDCG','STOP')
EndIf

DbSelectArea('TRA')
Count to _nQtdReg
ProcRegua(_nQtdReg)
TRA->(DbGoTop())
cNumCot := TRA->C7_NUMCOT
nMoe := TRA->C7_MOEDA
nTxmoe := TRA->C7_TXMOEDA
cFret := TRA->C7_TPFRETE
cCond := RETFIELD("SE4",1,xFilial("SE4")+TRA->C7_COND,"E4_DESCRI")
cPais := retfield("SA2",1,xFilial("SA2")+TRA->C7_FORNECE+TRA->C7_LOJA,"A2_CODPAIS")
While 	TRA->( ! Eof() )
	
	xVerPag()
	
	If	( lFlag )
		//ฺฤฤฤฤฤฤฤฤฤฤฟ
		//ณFornecedorณ
		//ภฤฤฤฤฤฤฤฤฤฤู
		
		If cPais == "01058"
			//ฺฤฤฤฤฤฤฤฤฤฤฟ
			//ณPortugues ณ
			//ภฤฤฤฤฤฤฤฤฤฤู
// editado por edicarlos dia 20/03/2017				
			//oPrint:Say(0400,2000,OemToAnsi('N๚mero'),oFont12)
//			oPrint:Say(0450,2000,SC7->C7_NUM,oFont12)
			oPrint:Say(0500,2000,Dtoc(SC7->C7_EMISSAO),oFont12)
			oPrint:Say(0530,0100,OemToAnsi('Fornecedor:'),oFont10)
			oPrint:Say(0530,0430,AllTrim(SA2->A2_NOME) + '  ('+AllTrim(SA2->A2_COD)+'/'+AllTrim(SA2->A2_LOJA)+')',oFont10t)
			oPrint:Say(0580,0100,OemToAnsi('Endere็o:'),oFont10)
			oPrint:Say(0580,0430,SA2->A2_END,oFont10t)
			oPrint:Say(0580,1470,OemToAnsi('CNPJ:'),oFont10)
			oPrint:Say(0580,1610,SA2->A2_CGC,oFont10t)
			oPrint:Say(0630,0100,OemToAnsi('Municํpio/U.F.:'),oFont10)
			oPrint:Say(0630,0430,AllTrim(SA2->A2_MUN)+'/'+AllTrim(SA2->A2_EST),oFont10t)
			oPrint:Say(0630,1470,OemToAnsi('Cep:'),oFont10)
			oPrint:Say(0630,1610,TransForm(SA2->A2_CEP,'@R 99.999-999'),oFont10t)
			oPrint:Say(0680,0100,OemToAnsi('Telefone:'),oFont10)
			oPrint:Say(0680,0430,substr(SA2->A2_DDD,0,2)+'-'+SA2->A2_TEL,oFont10t)
			oPrint:Say(0680,1470,OemToAnsi('Fax:'),oFont10)
			oPrint:Say(0680,1610,SA2->A2_FAX,oFont10t)
			oPrint:Say(0730,0100,OemToAnsi('Contato:'),oFont10)
			oPrint:Say(0730,0430,SA2->A2_CONTATO,oFont10t)
			oPrint:Say(0730,1470,OemToAnsi('E-mail: '),oFont10)
			oPrint:Say(0730,1610,ALLTRIM(SA2->A2_EMAIL),oFont10t)
		Else
			//ฺฤฤฤฤฤฤฤฤฤฤฟ
			//ณIngles    ณ
			//ภฤฤฤฤฤฤฤฤฤฤู
// editado por edicarlos dia 20/03/2017			
		//	oPrint:Say(0400,2000,OemToAnsi('Number'),oFont12)
		//	oPrint:Say(0450,2000,SC7->C7_NUM,oFont12)
		 	oPrint:Say(0500,2000,Dtoc(SC7->C7_EMISSAO),oFont12)
			oPrint:Say(0530,0100,AllTrim(SA2->A2_NOME) + '  ('+AllTrim(SA2->A2_COD)+'/'+AllTrim(SA2->A2_LOJA)+')',oFont10t)
			oPrint:Say(0580,0100,SA2->A2_END,oFont10t)
			oPrint:Say(0580,1470,OemToAnsi('CNPJ: ')+SA2->A2_CGC,oFont10t)
			//oPrint:Say(0580,1590,SA2->A2_CGC,oFont10t)
			oPrint:Say(0630,0100,OemToAnsi('City: ')+AllTrim(SA2->A2_MUN),oFont10t)
			oPrint:Say(0630,1470,OemToAnsi('Zipcode: ')+TransForm(SA2->A2_CEP,'@R 99.999-999'),oFont10t)
			//oPrint:Say(0630,1590,TransForm(SA2->A2_CEP,'@R 99.999-999'),oFont10t)
			oPrint:Say(0680,0100,OemToAnsi('Phone: ')+substr(SA2->A2_DDD,0,2)+'-'+SA2->A2_TEL,oFont10t)
			//oPrint:Say(0680,0430,substr(SA2->A2_DDD,0,2)+'-'+SA2->A2_TEL,oFont10t)
			oPrint:Say(0680,1470,OemToAnsi('Fax: ')+SA2->A2_FAX,oFont10t)
			//oPrint:Say(0680,1590,SA2->A2_FAX,oFont10t)
			oPrint:Say(0730,0100,OemToAnsi('Contato: ')+SA2->A2_CONTATO,oFont10t)
			//oPrint:Say(0730,0430,SA2->A2_CONTATO,oFont10t)
			oPrint:Say(0730,1470,OemToAnsi('E-mail: ')+ALLTRIM(SA2->A2_EMAIL),oFont10t)
			
		EndIf
		//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
		//ณNumero/Emissaoณ
		//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
		//					oPrint:Box(0530,1900,0730,2300)
		//					oPrint:FillRect({0530,1900,0730,2300},oBrush)
		lFlag := .f.
	EndIf
	
	If	( lPrintDesTab )
		If cPais == "01058"
			//ฺฤฤฤฤฤฤฤฤฤฤฟ
			//ณPortugues ณ
			//ภฤฤฤฤฤฤฤฤฤฤู
			
			oPrint:Line(nLinha,100,nLinha,2300)
			oPrint:Line(nLinha,100,nLinha+70,100)
			oPrint:Line(nLinha,215,nLinha+70,215)
			oPrint:Line(nLinha,370,nLinha+70,370)
			oPrint:Line(nLinha,1065,nLinha+70,1065)
			oPrint:Line(nLinha,1250,nLinha+70,1250)
			oPrint:Line(nLinha,1390,nLinha+70,1390)
			oPrint:Line(nLinha,1470,nLinha+70,1470)
			oPrint:Line(nLinha,1620,nLinha+70,1620)
			oPrint:Line(nLinha,1890,nLinha+70,1890)
			oPrint:Line(nLinha,2030,nLinha+70,2030)
			oPrint:Line(nLinha,2300,nLinha+70,2300)
			
			oPrint:Say(nLinha,0110,OemToAnsi('Item'),oFont10)
			oPrint:Say(nLinha,0230,OemToAnsi('C๓digo'),oFont10)
			oPrint:Say(nLinha,0390,OemToAnsi('Descri็ใo'),oFont10)
			oPrint:Say(nLinha,1085,OemToAnsi('C. Custo'),oFont10)
			oPrint:Say(nLinha,1255,OemToAnsi('N.Solic'),oFont10)
			oPrint:Say(nLinha,1402,OemToAnsi('UM'),oFont10)
			oPrint:Say(nLinha,1505,OemToAnsi('Qtde'),oFont10)
			oPrint:Say(nLinha,1680,OemToAnsi('Vlr.Unit.'),oFont10)
			oPrint:Say(nLinha,1900,OemToAnsi('% IPI'),oFont10)
			oPrint:Say(nLinha,2050,OemToAnsi('Valor Total'),oFont10)
		Else
			//ฺฤฤฤฤฤฤฤฤฤฤฟ
			//ณIngles    ณ
			//ภฤฤฤฤฤฤฤฤฤฤู
			
			oPrint:Line(nLinha,100,nLinha,2300)
			oPrint:Line(nLinha,100,nLinha+70,100)
			oPrint:Line(nLinha,215,nLinha+70,215)
			oPrint:Line(nLinha,370,nLinha+70,370)
			
			oPrint:Line(nLinha,1150,nLinha+70,1150)
			oPrint:Line(nLinha,1335,nLinha+70,1335)
			oPrint:Line(nLinha,1505,nLinha+70,1505)
			oPrint:Line(nLinha,1585,nLinha+70,1585)
			oPrint:Line(nLinha,1760,nLinha+70,1760)
			//oPrint:Line(nLinha,1890,nLinha+70,1890)
			oPrint:Line(nLinha,2030,nLinha+70,2030)
			oPrint:Line(nLinha,2300,nLinha+70,2300)
			
			oPrint:Say(nLinha,0110,OemToAnsi('Item'),oFont10)
			oPrint:Say(nLinha,0230,OemToAnsi('Code'),oFont10)
			oPrint:Say(nLinha,0390,OemToAnsi('Product Description'),oFont10)
			oPrint:Say(nLinha,1170,OemToAnsi('Cost C. '),oFont10)
			oPrint:Say(nLinha,1345,OemToAnsi('Request'),oFont10)
			oPrint:Say(nLinha,1515,OemToAnsi('UM'),oFont10)
			oPrint:Say(nLinha,1595,OemToAnsi('Quantity'),oFont10)
			oPrint:Say(nLinha,1850,OemToAnsi('Price'),oFont10)
			//oPrint:Say(nLinha,1900,OemToAnsi('% IPI'),oFont10)
			oPrint:Say(nLinha,2080,OemToAnsi('Total'),oFont10)
			
		EndIf
		lPrintDesTab := .f.
		nLinha += 70
		oPrint:Line(nLinha,100,nLinha,2300)
	EndIf
	If cPais == "01058"
		//ฺฤฤฤฤฤฤฤฤฤฤฟ
		//ณPortugues ณ
		//ภฤฤฤฤฤฤฤฤฤฤู
		
		oPrint:Line(nLinha,100,nLinha+60,100)
		oPrint:Line(nLinha,215,nLinha+60,215)
		oPrint:Line(nLinha,370,nLinha+60,370)
		oPrint:Line(nLinha,1065,nLinha+60,1065)
		oPrint:Line(nLinha,1250,nLinha+60,1250)
		oPrint:Line(nLinha,1390,nLinha+60,1390)
		oPrint:Line(nLinha,1470,nLinha+60,1470)
		oPrint:Line(nLinha,1620,nLinha+60,1620)
		oPrint:Line(nLinha,1890,nLinha+60,1890)
		oPrint:Line(nLinha,2030,nLinha+60,2030)
		oPrint:Line(nLinha,2300,nLinha+60,2300)
		
		oPrint:Say(nLinha,0115,TRA->C7_ITEM,oFont09)
		oPrint:Say(nLinha,0230,TRA->C7_PRODUTO,oFont09)
		cDescP := alltrim(retfield("SB1",1,xFilial("SB1")+TRA->C7_PRODUTO,"B1_DESC"))
		cComplPr := alltrim(retfield("SB1",1,xFilial("SB1")+TRA->C7_PRODUTO,"B1_OBSERV"))+" NCM: "+alltrim(retfield("SB1",1,xFilial("SB1")+TRA->C7_PRODUTO,"B1_POSIPI"))
		oPrint:Say(nLinha,0390,SubStr(retfield("SB1",1,xFilial("SB1")+TRA->C7_PRODUTO,"B1_DESC"),1,35),oFont09)
		oPrint:Say(nLinha,1075,TRA->C7_CC,oFont09)
		oPrint:Say(nLinha,1265,TRA->C7_NUMSC,oFont09)
		oPrint:Say(nLinha,1410,TRA->C7_UM,oFont09)
		oPrint:Say(nLinha,1600,AllTrim(TransForm(TRA->C7_QUANT,'@E 999,999')),oFont09,,,,1)
		If	( lImpPrc )
			oPrint:Say(nLinha,1880,AllTrim(TransForm(TRA->C7_PRECO,'@E 9,999,999.9999')),oFont09,,,,1)
			oPrint:Say(nLinha,2010,AllTrim(TransForm(TRA->C7_IPI,'@E 999.99')),oFont09,,,,1)
			oPrint:Say(nLinha,2280,AllTrim(TransForm(TRA->C7_TOTAL,'@E 99,999,999.99')),oFont09,,,,1)
		EndIf
		If Len(cDescP) > 35
			nLinha += 40
			oPrint:Line(nLinha,100,nLinha+40,100)
			oPrint:Line(nLinha,215,nLinha+40,215)
			oPrint:Line(nLinha,370,nLinha+40,370)
			oPrint:Line(nLinha,1065,nLinha+40,1065)
			oPrint:Line(nLinha,1250,nLinha+40,1250)
			oPrint:Line(nLinha,1390,nLinha+40,1390)
			oPrint:Line(nLinha,1470,nLinha+40,1470)
			oPrint:Line(nLinha,1620,nLinha+40,1620)
			oPrint:Line(nLinha,1890,nLinha+40,1890)
			oPrint:Line(nLinha,2030,nLinha+40,2030)
			oPrint:Line(nLinha,2300,nLinha+40,2300)
			oPrint:Say(nLinha,0390,SubStr(retfield("SB1",1,xFilial("SB1")+TRA->C7_PRODUTO,"B1_DESC"),36,35),oFont09)
			lDesc2 := .T.
		EndIf
		If !Empty(cComplPr)
			cImpFabr := alltrim(cComplPr)+" "
		EndIf
		cPartnum += ALLTRIM(TRA->B1_FABRIC)+" "
		cPartnum += ALLTRIM(TRA->B1_FABRIC2)+" "
		cPartnum += ALLTRIM(TRA->B1_FABRIC3)+" "
		cPartnum += ALLTRIM(TRA->B1_FABRIC4)+" "
		cPartnum += ALLTRIM(TRA->B1_FABRIC5)+" "
		cPartnum += ALLTRIM(TRA->B1_FABRIC6)+" "
		cPartnum += ALLTRIM(TRA->B1_FABRIC7)+" "
		cPartnum += ALLTRIM(TRA->B1_FABRIC8)        
		If !Empty(cPartnum)
			cImpFabr += "-Fabricantes: "+alltrim(cPartnum)+" "
		EndIf
		If !Empty(TRA->C7_OBS)
			cImpFabr += "-OBS: "+alltrim(TRA->C7_OBS)
		EndIf		
		If !Empty(cImpFabr)
//			cImpFabr += "-Fabricantes: "+alltrim(cPartnum)+" -Obs: "+ALLTRIM(TRA->C7_OBS)
			for x := 1 to mlcount(alltrim(cImpFabr),35)
				nLinha += 40
				oPrint:Line(nLinha,100,nLinha+40,100)
				oPrint:Line(nLinha,215,nLinha+40,215)
				oPrint:Line(nLinha,370,nLinha+40,370)
				oPrint:Line(nLinha,1065,nLinha+40,1065)
				oPrint:Line(nLinha,1250,nLinha+40,1250)
				oPrint:Line(nLinha,1390,nLinha+40,1390)
				oPrint:Line(nLinha,1470,nLinha+40,1470)
				oPrint:Line(nLinha,1620,nLinha+40,1620)
				oPrint:Line(nLinha,1890,nLinha+40,1890)
				oPrint:Line(nLinha,2030,nLinha+40,2030)
				oPrint:Line(nLinha,2300,nLinha+40,2300)
				oPrint:Say(nLinha,0390,memoline(cImpFabr,35,x),oFont09)
			next
			lDesc2 := .T.
		EndIf
		
		If lDesc2
			nLinha += 40
			lDesc2 := .F.
		Else
			nLinha += 60
		EndIf
		
		oPrint:Line(nLinha,100,nLinha,2300)
	Else
		//ฺฤฤฤฤฤฤฤฤฤฤฟ
		//ณIngles    ณ
		//ภฤฤฤฤฤฤฤฤฤฤู
		
		oPrint:Line(nLinha,100,nLinha+60,100)
		oPrint:Line(nLinha,215,nLinha+60,215)
		oPrint:Line(nLinha,370,nLinha+60,370)
		
		oPrint:Line(nLinha,1150,nLinha+60,1150)
		oPrint:Line(nLinha,1335,nLinha+60,1335)
		oPrint:Line(nLinha,1505,nLinha+60,1505)
		oPrint:Line(nLinha,1585,nLinha+60,1585)
		oPrint:Line(nLinha,1760,nLinha+60,1760)
		//oPrint:Line(nLinha,1890,nLinha+60,1890)
		oPrint:Line(nLinha,2030,nLinha+60,2030)
		oPrint:Line(nLinha,2300,nLinha+60,2300)
		
		oPrint:Say(nLinha,0115,TRA->C7_ITEM,oFont09)
		oPrint:Say(nLinha,0230,TRA->C7_PRODUTO,oFont09)
		cDescP := alltrim(retfield("SB1",1,xFilial("SB1")+TRA->C7_PRODUTO,"B1_DESC"))
		cComplPr := alltrim(retfield("SB1",1,xFilial("SB1")+TRA->C7_PRODUTO,"B1_OBSERV"))+" NCM: "+alltrim(retfield("SB1",1,xFilial("SB1")+TRA->C7_PRODUTO,"B1_POSIPI"))
		oPrint:Say(nLinha,0390,SubStr(retfield("SB1",1,xFilial("SB1")+TRA->C7_PRODUTO,"B1_DESC"),1,41),oFont09)
		oPrint:Say(nLinha,1160,TRA->C7_CC,oFont09)
		oPrint:Say(nLinha,1365,TRA->C7_NUMSC,oFont09)
		oPrint:Say(nLinha,1520,TRA->C7_UM,oFont09)
		oPrint:Say(nLinha,1745,AllTrim(TransForm(TRA->C7_QUANT,'@E 999,999')),oFont09,,,,1)
		If	( lImpPrc )
			oPrint:Say(nLinha,2010,AllTrim(TransForm(TRA->C7_PRECO,'@E 9,999,999.9999')),oFont09,,,,1)
			//oPrint:Say(nLinha,2010,AllTrim(TransForm(TRA->C7_IPI,'@E 999.99')),oFont09,,,,1)
			oPrint:Say(nLinha,2280,AllTrim(TransForm(TRA->C7_TOTAL,'@E 99,999,999.99')),oFont09,,,,1)
		EndIf
		
		If Len(cDescP) > 41
			nLinha += 40
			oPrint:Line(nLinha,100,nLinha+50,100)
			oPrint:Line(nLinha,215,nLinha+40,215)
			oPrint:Line(nLinha,370,nLinha+40,370)
			
			oPrint:Line(nLinha,1150,nLinha+40,1150)
			oPrint:Line(nLinha,1335,nLinha+40,1335)
			oPrint:Line(nLinha,1505,nLinha+40,1505)
			oPrint:Line(nLinha,1585,nLinha+40,1585)
			oPrint:Line(nLinha,1760,nLinha+40,1760)
			//oPrint:Line(nLinha,1890,nLinha+60,1890)
			oPrint:Line(nLinha,2030,nLinha+40,2030)
			oPrint:Line(nLinha,2300,nLinha+40,2300)
			oPrint:Say(nLinha,0390,SubStr(retfield("SB1",1,xFilial("SB1")+TRA->C7_PRODUTO,"B1_DESC"),41,41),oFont09)
			lDesc2 := .T.
		EndIf
		If !Empty(cComplPr)
			cImpFabr := alltrim(cComplPr)+" "
		EndIf
		cPartnum += ALLTRIM(TRA->B1_FABRIC)+" "
		cPartnum += ALLTRIM(TRA->B1_FABRIC2)+" "
		cPartnum += ALLTRIM(TRA->B1_FABRIC3)+" "
		cPartnum += ALLTRIM(TRA->B1_FABRIC4)+" "
		cPartnum += ALLTRIM(TRA->B1_FABRIC5)+" "
		cPartnum += ALLTRIM(TRA->B1_FABRIC6)+" "
		cPartnum += ALLTRIM(TRA->B1_FABRIC7)+" "
		cPartnum += ALLTRIM(TRA->B1_FABRIC8)
		If !Empty(cPartnum)
			cImpFabr += "-Fabricantes: "+alltrim(cPartnum)+" "
		EndIf
		If !Empty(TRA->C7_OBS)
			cImpFabr += "-OBS: "+alltrim(TRA->C7_OBS)
		EndIf		
		If !Empty(cImpFabr)
//		If !Empty(cPartnum) .or. !Empty(cComplPr) .or. !Empty(TRA->C7_OBS)
//			cImpFabr += "-Manufacturer: "+cPartnum+" -Obs: "+ALLTRIM(TRA->C7_OBS)
			for x := 1 to mlcount(alltrim(cImpFabr),41)
				nLinha += 40
				oPrint:Line(nLinha,100,nLinha+50,100)
				oPrint:Line(nLinha,215,nLinha+40,215)
				oPrint:Line(nLinha,370,nLinha+40,370)
			
				oPrint:Line(nLinha,1150,nLinha+40,1150)
				oPrint:Line(nLinha,1335,nLinha+40,1335)
				oPrint:Line(nLinha,1505,nLinha+40,1505)
				oPrint:Line(nLinha,1585,nLinha+40,1585)
				oPrint:Line(nLinha,1760,nLinha+40,1760)
				//oPrint:Line(nLinha,1890,nLinha+60,1890)
				oPrint:Line(nLinha,2030,nLinha+40,2030)
				oPrint:Line(nLinha,2300,nLinha+40,2300)
				oPrint:Say(nLinha,0390,memoline(cImpFabr,41,x),oFont09)
			next
			lDesc2 := .T.
		EndIf

		If lDesc2
			nLinha += 40
			lDesc2 := .F.
		Else
			nLinha += 60
		EndIf
		oPrint:Line(nLinha,100,nLinha,2300)
		
	EndIf
	_nValMerc 		+= TRA->C7_TOTAL
	_nValIPI		+= (TRA->C7_TOTAL * TRA->C7_IPI) / 100
	_nValDesc		+= TRA->C7_VLDESC
	_nTotAcr		+= TRA->C7_DESPESA
	_nTotSeg		+= TRA->C7_SEGURO
	_nTotFre		+= TRA->C7_VALFRE
	_nTotVcv		+= TRA->C7_VLCONV
	
	If	( Empty(TRA->C7_TES) )
		_nTotIcmsRet	+= TRA->C7_ICMSRET
	Else
		DbSelectArea('SF4')
		SF4->(DbSetOrder(1))
		If	SF4->(DbSeek(xFilial('SF4') + TRA->C7_TES))
			If	( AllTrim(SF4->F4_INCSOL) == 'S' )
				_nTotIcmsRet	+= TRA->C7_ICMSRET
			EndIf
		EndIf
	EndIf
	cImpFabr := ""
	cPartnum := ""
	IncProc()
	TRA->(DbSkip())
End

xVerPag()

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณImprime TOTAL DE MERCADORIASณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
If	( lImpPrc )
	If cPais == "01058"
		oPrint:Line(nLinha,1390,nLinha+80,1390)
		oPrint:Line(nLinha,1890,nLinha+80,1890)
		oPrint:Line(nLinha,2300,nLinha+80,2300)
		If nMoe == 1
			oPrint:Say(nLinha+10,1405,'Valor de Mercadorias R$',oFont10)
			oPrint:Say(nLinha+10,2030,TransForm(_nValMerc,'@E 99,999,999.99'),oFont10)
		Else
			oPrint:Say(nLinha+10,1405,'Valor Mercadorias US$',oFont10)
			oPrint:Say(nLinha+10,2030,TransForm(_nValMerc,'@E 99,999,999.99'),oFont10)
		EndIf
		nLinha += 80
		oPrint:Line(nLinha,1390,nLinha,2300)
		
		//	Else
		//		oPrint:Line(nLinha,1335,nLinha+80,1335)
		//		oPrint:Line(nLinha,1760,nLinha+80,1760)
		//		oPrint:Line(nLinha,2300,nLinha+80,2300)
		//		oPrint:Say(nLinha+10,1350,'Total Price US$',oFont10)
		//		oPrint:Say(nLinha+10,2030,TransForm(_nValMerc,'@E 99,999,999.99'),oFont10)
		//		nLinha += 80
		//		oPrint:Line(nLinha,1335,nLinha,2300)
		
	EndIf
EndIf

xVerPag()

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณImprime TOTAL DE I.P.I. ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
If	( lImpPrc ) .and. ( _nValIpi > 0 )
	If cPais == "01058"
		oPrint:Line(nLinha,1390,nLinha+80,1390)
		oPrint:Line(nLinha,1890,nLinha+80,1890)
		oPrint:Line(nLinha,2300,nLinha+80,2300)
		oPrint:Say(nLinha+10,1405,'Valor de IPI (+)',oFont10)
		oPrint:Say(nLinha+10,2051,TransForm(_nValIpi,'@E 9,999,999.99'),oFont10)
		nLinha += 80
		oPrint:Line(nLinha,1390,nLinha,2300)
	Else
		oPrint:Line(nLinha,1335,nLinha+80,1335)
		oPrint:Line(nLinha,1760,nLinha+80,1760)
		oPrint:Line(nLinha,2300,nLinha+80,2300)
		oPrint:Say(nLinha+10,1350,'IPI ',oFont10)
		oPrint:Say(nLinha+10,2051,TransForm(_nValIpi,'@E 9,999,999.99'),oFont10)
		nLinha += 80
		oPrint:Line(nLinha,1335,nLinha,2300)
	EndIf
EndIf

xVerPag()

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณImprime TOTAL DE DESCONTOณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
If	( lImpPrc ) .and. ( _nValDesc > 0 )
	If cPais == "01058"
		oPrint:Line(nLinha,1390,nLinha+80,1390)
		oPrint:Line(nLinha,1890,nLinha+80,1890)
		oPrint:Line(nLinha,2300,nLinha+80,2300)
		If nMoe == 1
			oPrint:Say(nLinha+10,1405,'Valor de Desconto (-) R$',oFont10)
			oPrint:Say(nLinha+10,2051,TransForm(_nValDesc,'@E 9,999,999.99'),oFont10)
		Else
			oPrint:Say(nLinha+10,1405,'Valor de Desconto (-) US$',oFont10)
			oPrint:Say(nLinha+10,2051,TransForm(_nValDesc,'@E 9,999,999.99'),oFont10)
		Endif
		nLinha += 80
		oPrint:Line(nLinha,1390,nLinha,2300)
	Else
		oPrint:Line(nLinha,1335,nLinha+80,1335)
		oPrint:Line(nLinha,1760,nLinha+80,1760)
		oPrint:Line(nLinha,2300,nLinha+80,2300)
		oPrint:Say(nLinha+10,1350,'Discount US$',oFont10)
		oPrint:Say(nLinha+10,2051,TransForm(_nValDesc,'@E 9,999,999.99'),oFont10)
		nLinha += 80
		oPrint:Line(nLinha,1335,nLinha,2300)
	EndIf
EndIf

xVerPag()

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณImprime TOTAL DE ACRESCIMO ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
If	( lImpPrc ) .and. ( _nTotAcr > 0 )
	If cPais == "01058"
		oPrint:Line(nLinha,1390,nLinha+80,1390)
		oPrint:Line(nLinha,1890,nLinha+80,1890)
		oPrint:Line(nLinha,2300,nLinha+80,2300)
		If nMoe == 1
			oPrint:Say(nLinha+10,1405,'Valor de Acresc. (+) R$',oFont10)
			oPrint:Say(nLinha+10,2051,TransForm(_nTotAcr,'@E 9,999,999.99'),oFont10)
		Else
			oPrint:Say(nLinha+10,1405,'Valor de Acresc. (+) US$',oFont10)
			oPrint:Say(nLinha+10,2051,TransForm(_nTotAcr,'@E 9,999,999.99'),oFont10)
		Endif
		nLinha += 80
		oPrint:Line(nLinha,1390,nLinha,2300)
	Else
		oPrint:Line(nLinha,1335,nLinha+80,1335)
		oPrint:Line(nLinha,1760,nLinha+80,1760)
		oPrint:Line(nLinha,2300,nLinha+80,2300)
		oPrint:Say(nLinha+10,1350,'Additional Value US$',oFont10)
		oPrint:Say(nLinha+10,2051,TransForm(_nTotAcr,'@E 9,999,999.99'),oFont10)
		nLinha += 80
		oPrint:Line(nLinha,1335,nLinha,2300)
	EndIf
EndIf

xVerPag()

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณImprime TOTAL DE SEGURO ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
If	( lImpPrc ) .and. ( _nTotSeg > 0 )
	If cPais == "01058"
		oPrint:Line(nLinha,1390,nLinha+80,1390)
		oPrint:Line(nLinha,1890,nLinha+80,1890)
		oPrint:Line(nLinha,2300,nLinha+80,2300)
		If nMoe == 1
			oPrint:Say(nLinha+10,1405,'Valor de Seguro (+) R$',oFont10)
			oPrint:Say(nLinha+10,2051,TransForm(_nTotSeg,'@E 9,999,999.99'),oFont10)
		Else
			oPrint:Say(nLinha+10,1405,'Valor de Seguro (+) US$',oFont10)
			oPrint:Say(nLinha+10,2051,TransForm(_nTotSeg,'@E 9,999,999.99'),oFont10)
		Endif
		nLinha += 80
		oPrint:Line(nLinha,1390,nLinha,2300)
	Else
		oPrint:Line(nLinha,1335,nLinha+80,1335)
		oPrint:Line(nLinha,1760,nLinha+80,1760)
		oPrint:Line(nLinha,2300,nLinha+80,2300)
		oPrint:Say(nLinha+10,1350,'Safe Value US$',oFont10)
		oPrint:Say(nLinha+10,2051,TransForm(_nTotSeg,'@E 9,999,999.99'),oFont10)
		nLinha += 80
		oPrint:Line(nLinha,1335,nLinha,2300)
	EndIf
EndIf

xVerPag()

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณImprime TOTAL DE FRETE ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
If	( lImpPrc ) .and. ( _nTotFre > 0 )
	If cPais == "01058"
		oPrint:Line(nLinha,1390,nLinha+80,1390)
		oPrint:Line(nLinha,1890,nLinha+80,1890)
		oPrint:Line(nLinha,2300,nLinha+80,2300)
		If nMoe ==1
			oPrint:Say(nLinha+10,1405,'Valor de Frete (+) R$',oFont10)
			oPrint:Say(nLinha+10,2051,TransForm(_nTotFre,'@E 9,999,999.99'),oFont10)
		Else
			oPrint:Say(nLinha+10,1405,'Valor de Frete (+) US$',oFont10)
			oPrint:Say(nLinha+10,2051,TransForm(_nTotFre,'@E 9,999,999.99'),oFont10)
		Endif
		nLinha += 80
		oPrint:Line(nLinha,1390,nLinha,2300)
	Else
		oPrint:Line(nLinha,1335,nLinha+80,1335)
		oPrint:Line(nLinha,1760,nLinha+80,1760)
		oPrint:Line(nLinha,2300,nLinha+80,2300)
		oPrint:Say(nLinha+10,1350,'Freight US$',oFont10)
		oPrint:Say(nLinha+10,2051,TransForm(_nTotFre,'@E 9,999,999.99'),oFont10)
		nLinha += 80
		oPrint:Line(nLinha,1335,nLinha,2300)
	EndIf
EndIf

xVerPag()

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณImprime ICMS RETIDO    ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
If	( lImpPrc ) .and. ( _nTotIcmsRet > 0 )
	oPrint:Line(nLinha,1390,nLinha+80,1390)
	oPrint:Line(nLinha,1890,nLinha+80,1890)
	oPrint:Line(nLinha,2300,nLinha+80,2300)
	oPrint:Say(nLinha+10,1405,'Valor de ICMS Retido R$',oFont10)
	oPrint:Say(nLinha+10,2051,TransForm(_nTotIcmsRet,'@E 9,999,999.99'),oFont10)
	nLinha += 80
	oPrint:Line(nLinha,1390,nLinha,2300)
EndIf

xVerPag()

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณImprime o VALOR TOTAL !ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
//		oPrint:FillRect({nLinha,1390,nLinha+80,2300},oBrush)
If cPais == "01058"
	oPrint:Line(nLinha,1390,nLinha+80,1390)
	oPrint:Line(nLinha,1890,nLinha+80,1890)
	oPrint:Line(nLinha,2300,nLinha+80,2300)
	If nMoe == 1
		oPrint:Say(nLinha+10,1405,'VALOR TOTAL R$',oFont10)
		If	( lImpPrc )
			oPrint:Say(nLinha+10,2030,TransForm(_nValMerc + _nValIPI - _nValDesc + _nTotAcr	+ _nTotSeg + _nTotFre + _nTotIcmsRet,'@E 99,999,999.99'),oFont10)
		EndIf
	Else
		oPrint:Say(nLinha+10,1405,'VALOR TOTAL US$',oFont10)
		If	( lImpPrc )
			oPrint:Say(nLinha+10,2030,TransForm(_nValMerc + _nValIPI - _nValDesc + _nTotAcr	+ _nTotSeg + _nTotFre + _nTotIcmsRet,'@E 99,999,999.99'),oFont10)
		EndIf
	Endif
	nLinha += 80
	xVerPag()
	oPrint:Line(nLinha,1390,nLinha,2300)
	
Else
	oPrint:Line(nLinha,1335,nLinha+80,1335)
	oPrint:Line(nLinha,1760,nLinha+80,1760)
	oPrint:Line(nLinha,2300,nLinha+80,2300)
	oPrint:Say(nLinha+10,1350,'TOTAL US$',oFont10)
	If	( lImpPrc )
		oPrint:Say(nLinha+10,2030,TransForm(_nValMerc + _nValIPI - _nValDesc + _nTotAcr	+ _nTotSeg + _nTotFre + _nTotIcmsRet,'@E 99,999,999.99'),oFont10)
	EndIf
	nLinha += 80
	xVerPag()
	oPrint:Line(nLinha,1335,nLinha,2300)
	
EndIf
nLinha += 70
xVerPag()

oPrint:Line(nLinha,0100,nLinha,2300)
nLinha += 10
xVerPag()
//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณCONDIวรO PAGAMENTO                !ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
nPraz := retfield("SC8",1,xFilial("SC8")+alltrim(cNumCot),"C8_PRAZO")
If cPais == "01058"
	oPrint:Say(nLinha,0100,OemToAnsi('Prazo entrega: ')+TransForm(nPraz,'@E 9999')+" Dias",oFont11)
	oPrint:Say(nLinha,1700,OemToAnsi('Condi็ใo de Pagto: ')+cCond,oFont11)
	nLinha += 60
	xVerPag()
Else
	oPrint:Say(nLinha,0100,OemToAnsi('Delivery Date: ')+TransForm(nPraz,'@E 9999')+" Days",oFont11)
	oPrint:Say(nLinha,1700,OemToAnsi('Payment Condition: ')+cCond,oFont11)
	nLinha += 60
	xVerPag()
EndIf
//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณImprime a linha de transportadora !ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
If cPais == "01058"
	oPrint:Say(nLinha,0100,OemToAnsi('Transportadora:'),oFont11)
	oPrint:Say(nLinha,0500,retfield("SA4",1,xFilial("SC7")+TRA->C7_TRANSP,"A4_NOME"),oFont11)
	IF cFret == "C"
		cFret := "CIF"
	Else
		cFret := "FOB"
	Endif
	oPrint:Say(nLinha,1700,OemToAnsi('Frete: ')+cFret,oFont11)
	nLinha += 60
	xVerPag()
EndIf
//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณENTREGA, COBRANCA E FATURAMENTO   !ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
If cPais == "01058"
	oPrint:Say(nLinha,0100,OemToAnsi('Local de Entrega:'),oFont11)
	oPrint:Say(nLinha,0470,AllTrim(SM0->M0_ENDENT)+" - "+AllTrim(SM0->M0_BAIRENT)+" - "+AllTrim(SM0->M0_CIDENT)+'/'+AllTrim(SM0->M0_ESTENT)+' - '+AllTrim(TransForm(SM0->M0_CEPENT,'@R 99.999-999')),oFont11)
	nLinha += 60
	xVerPag()
Else
	oPrint:Say(nLinha,0100,OemToAnsi('Shipment Address:'),oFont11)
	oPrint:Say(nLinha,0470,AllTrim(SM0->M0_ENDENT)+" - "+AllTrim(SM0->M0_CIDENT)+'/'+AllTrim(SM0->M0_ESTENT)+' - '+AllTrim(TransForm(SM0->M0_CEPENT,'@R 99.999-999')),oFont11)
	nLinha += 60
	xVerPag()
Endif

If cPais == "01058"
	oPrint:Say(nLinha,0100,OemToAnsi('Loc. Faturamento:'),oFont11)
	oPrint:Say(nLinha,0470,AllTrim(SM0->M0_ENDENT)+" - "+AllTrim(SM0->M0_BAIRENT)+" - "+AllTrim(SM0->M0_CIDENT)+'/'+AllTrim(SM0->M0_ESTENT)+' - '+AllTrim(TransForm(SM0->M0_CEPENT,'@R 99.999-999')),oFont11)
	nLinha += 60
	xVerPag()
	oPrint:Say(nLinha,0100,OemToAnsi('Local de Cobran็a:'),oFont11)
	oPrint:Say(nLinha,0470,"AV. RAJA GABAGLIA, 3800 - BELO HORIZONTE/MG - CEP: 30.494-310",oFont11)
	nLinha += 70
	xVerPag()
Endif

oPrint:Line(nLinha,0100,nLinha,2300)
nLinha += 10
xVerPag()

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณImprime as observacoes dos parametros. !ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
/*
If cPais == "01058"
	oPrint:Say(nLinha,0100,OemToAnsi('Observa็๕es:'),oFont10)
	oPrint:Say(nLinha,0470,SubStr(SC7->C7_OBS,1,77),oFont10)
	cObse := alltrim(SubStr(SC7->C7_OBS,78,77))
	If cObse <> ""
		nLinha += 60
		xVerPag()
		oPrint:Say(nLinha,0470,SubStr(SC7->C7_OBS,78,77),oFont10)
		cObse := alltrim(SubStr(SC7->C7_OBS,156,77))
	Endif
	If cObse <> ""
		nLinha += 60
		xVerPag()
		oPrint:Say(nLinha,0470,SubStr(SC7->C7_OBS,156,77),oFont10)
		cObse := alltrim(SubStr(SC7->C7_OBS,234,66))
	Endif
	If cObse <> ""
		nLinha += 60
		xVerPag()
		oPrint:Say(nLinha,0470,SubStr(SC7->C7_OBS,234,66),oFont10)
	Endif
Else
	oPrint:Say(nLinha,0100,OemToAnsi('Observation:'),oFont10)
	oPrint:Say(nLinha,0470,SubStr(SC7->C7_OBS,1,77),oFont10)
	cObse := alltrim(SubStr(SC7->C7_OBS,78,77))
	If cObse <> ""
		nLinha += 60
		xVerPag()
		oPrint:Say(nLinha,0470,SubStr(SC7->C7_OBS,78,77),oFont10)
		cObse := alltrim(SubStr(SC7->C7_OBS,156,77))
	Endif
	If cObse <> ""
		nLinha += 60
		xVerPag()
		oPrint:Say(nLinha,0470,SubStr(SC7->C7_OBS,156,77),oFont10)
		cObse := alltrim(SubStr(SC7->C7_OBS,234,66))
	Endif
	If cObse <> ""
		nLinha += 60
		xVerPag()
		oPrint:Say(nLinha,0470,SubStr(SC7->C7_OBS,233,67),oFont10)
	Endif
EndIf

nLinha += 60
xVerPag()
*/
If	( ! Empty(cObserv1) )
	If cPais == "01058"
	oPrint:Say(nLinha,0100,OemToAnsi('Observa็๕es:'),oFont10)
	Else
	oPrint:Say(nLinha,0100,OemToAnsi('Observation:'),oFont10)
	EndIf
	oPrint:Say(nLinha,0470,cObserv1,oFont12n)
	nLinha += 60
	xVerPag()
EndIf
If	( ! Empty(cObserv2) )
	oPrint:Say(nLinha,0470,cObserv2,oFont12n)
	xVerPag()
	nLinha += 60
EndIf
If	( ! Empty(cObserv3) )
	oPrint:Say(nLinha,0470,cObserv3,oFont12n)
	xVerPag()
	nLinha += 60
	xVerPag()
EndIf

nLinha += 20
xVerPag()

nLinha += 20
xVerPag()

oPrint:Line(nLinha,0100,nLinha,2300)
nLinha += 10
xVerPag()

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณDados adicionais                          !ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
If cPais == "01058"
	oPrint:Say(nLinha,0100,OemToAnsi('Dados Adicionais: '),oFont12)
	If nMoe == 2
		oPrint:Say(nLinha,0490,'Material importado - Custo importa็ใo nใo incluso no valor acima.',oFont12n)
		nLinha += 60
		xVerPag()
	EndIf
	oPrint:Say(nLinha,0490,'A mercadoria somente serแ recebida com o NR do Pedido destacado na Nota Fiscal.',oFont12n)
	
	nLinha += 60
	xVerPag()
	
	oPrint:Say(nLinha,0490,'Nossos pagamentos sใo realizados nos dias 10, 20 e 30 de cada m๊s, portanto o',oFont12n)
	nLinha += 60
	xVerPag()
	oPrint:Say(nLinha,0490,'vencimento de sua nota fiscal dverแ obedecer a uma destas datas informadas.',oFont12n)
	
	nLinha += 60
	xVerPag()
	
	oPrint:Line(nLinha,0100,nLinha,2300)
	nLinha += 20
	xVerPag()
Else
	oPrint:Say(nLinha,0100,OemToAnsi('Notes: '),oFont12)
	//oPrint:Say(nLinha,0400,'Material importado - Custo importa็ใo nใo incluso no valor acima. Valor Estimado',oFont12n)
    oPrint:Say(nLinha,0400,'Material importado - Custo importa็ใo nใo incluso no valor acima.',oFont12n)
	nLinha += 60
	xVerPag()
	/*
	oPrint:Say(nLinha,0400,'de R$'+TransForm(_nTotVcv,'@E 99,999,999.99'),oFont12n)
	nLinha += 60
	xVerPag()
	
	oPrint:Say(nLinha,0400,'Please send us your proforma invoice for the above items with delivery time,',oFont12n)
	nLinha += 60
	xVerPag()
	oPrint:Say(nLinha,0400,'payment and bank details. Please inform our order in the proforma.',oFont12n)
	nLinha += 60
	xVerPag()
	*/
	oPrint:Line(nLinha,0100,nLinha,2300)
	nLinha += 20
	xVerPag()
	
EndIf
oPrint:Say(3150,0100,OemToAnsi('|COM                   |SOL                    |DIR                    |GAF                    |SUP                    |DAF'),oFont11)

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณImprime o Contato.ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
//		If	( ! Empty(SA2->A2_CONTATO) )
//			oPrint:Say(nLinha,0100,OemToAnsi('Contato: '),oFont12)
//			oPrint:Say(nLinha,0500,SA2->A2_CONTATO,oFont12n)
//			nLinha += 60
//			xVerPag()
//		EndIf

//		oPrint:Line(nLinha,0100,nLinha,2300)
//		nLinha += 10
//		xVerPag()

TRA->(DbCloseArea())

xRodape()

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณImprime em Video, e finaliza a impressao. !ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
oPrint:Preview()

Return

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหออออออัอออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณ xCabec() บAutor ณLuis Henrique Robustoบ Data ณ  25/10/04   บฑฑ
ฑฑฬออออออออออุออออออออออสออออออฯอออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDesc.     ณ Imprime o Cabecalho do relatorio...                        บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ Funcao Principal                                           บฑฑ
ฑฑฬออออออออออุออออออออออัอออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบDATA      ณ ANALISTA ณ  MOTIVO                                         บฑฑ
ฑฑฬออออออออออุออออออออออุอออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบ          ณ          ณ                                                 บฑฑ
ฑฑศออออออออออฯออออออออออฯอออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
*/

Static Function xCabec()

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณImprime o cabecalho da empresa. !ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
oPrint:SayBitmap(050,100,cFileLogotp,550,200)
oPrint:Say(050,950,AllTrim(Upper(SM0->M0_NOMECOM)),oFont12)
oPrint:Say(135,950,AllTrim(SM0->M0_ENDENT)+" - "+AllTrim(SM0->M0_BAIRENT),oFont11)
//cPais := retfield("SA2",1,xFilial("SA2")+TRA->C7_FORNECE+TRA->C7_LOJA,"A2_CODPAIS")
If cPais == "01058"
	oPrint:Say(180,950,Capital(AllTrim(SM0->M0_CIDENT))+'/'+AllTrim(SM0->M0_ESTENT)+ ' - CEP: ' + AllTrim(TransForm(SM0->M0_CEPENT,'@R 99.999-999')) + ' - Tel: ' + substr(SM0->M0_TEL,1,2)+'-'+substr(SM0->M0_TEL,3,8),oFont11)
Else
	oPrint:Say(180,950,Capital('City: '+AllTrim(SM0->M0_CIDENT))+'/'+AllTrim(SM0->M0_ESTENT)+ ' - Zipcode: ' + AllTrim(TransForm(SM0->M0_CEPENT,'@R 99.999-999')) + ' - Phone: 55' + substr(SM0->M0_TEL,1,2)+'-'+substr(SM0->M0_TEL,3,8),oFont11)
EndIf
oPrint:Say(230,950,'www.tacom.com.br',oFont11)
oPrint:Say(285,950,'CNPJ: '+TransForm(SM0->M0_CGC,'@R 99.999.999/9999-99'),oFont11)
oPrint:Say(285,1500,'IE: '+SM0->M0_INSC,oFont11)
oPrint:Line(350,950,350,2270)

If cPais == "01058"
	oPrint:Say(0400,0800,OemToAnsi('PEDIDO DE COMPRA'),oFont14)
	//oPrint:Say(0400,1900,'Numero Pedido ',oFont12)
	//oPrint:Say(0450,1900,SC7->C7_NUM,oFont12)
    oPrint:Say(0400,2000,OemToAnsi('N๚mero'),oFont13)
	oPrint:Say(0450,2000,SC7->C7_NUM,oFont13)
	oPrint:Say(0500,2000,nPage,oFont13)
	
Else
	oPrint:Say(0400,0800,OemToAnsi('PURCHASE ORDER'),oFont14)
	//oPrint:Say(0400,1900,'Numero Pedido ',oFont12)
	//oPrint:Say(0450,1900,SC7->C7_NUM,oFont12)
	oPrint:Say(0400,2000,OemToAnsi('Number'),oFont13)
	oPrint:Say(0450,2000,SC7->C7_NUM,oFont13)
	
EndIf
Return

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหออออออัอออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณ xRodape()บAutor ณLuis Henrique Robustoบ Data ณ  25/10/04   บฑฑ
ฑฑฬออออออออออุออออออออออสออออออฯอออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDesc.     ณ Imprime o Rodape do Relatorio....                          บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ Funcao Principal                                           บฑฑ
ฑฑฬออออออออออุออออออออออัอออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบDATA      ณ ANALISTA ณ  MOTIVO                                         บฑฑ
ฑฑฬออออออออออุออออออออออุอออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบ          ณ          ณ                                                 บฑฑ
ฑฑศออออออออออฯออออออออออฯอออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
*/

Static Function xRodape()
Local cTelc  := retfield("SY1",3,xFilial("SY1")+__cUserID,"Y1_TEL")
Local cNomec := retfield("SY1",3,xFilial("SY1")+__cUserID,"Y1_NOME")

oPrint:Line(3060,0100,3060,2300)
If cPais == "01058"
	oPrint:Say(3250,0100,'Contato มrea de Compras ',oFont12)
	oPrint:Say(3250,1900,'Numero Pedido ',oFont12)
	oPrint:Say(3300,1900,SC7->C7_NUM,oFont12)	
	oPrint:Say(3300,0100,'E-mail: '+AllTrim(usrretmail(__cUserID)),oFont12)
	//oPrint:Say(3300,1900,'Tel: '+cTelc,oFont12)
Else
	oPrint:Say(3250,0100,'Contact ',oFont12)
	oPrint:Say(3250,1900,OemToAnsi('Number'),oFont13)
	oPrint:Say(3000,1900,SC7->C7_NUM,oFont13)
	oPrint:Say(3300,0100,'E-mail: '+AllTrim(usrretmail(__cUserID)),oFont12)
//	oPrint:Say(3300,1790,'Phone: 55 '+cTelc,oFont12)
Endif

//	oPrint:Line(3200,0100,3200,2300)

Return

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหออออออัอออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณ xVerPag()บAutor ณLuis Henrique Robustoบ Data ณ  25/10/04   บฑฑ
ฑฑฬออออออออออุออออออออออสออออออฯอออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDesc.     ณ Verifica se deve ou nao saltar pagina...                   บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ Funcao Principal                                           บฑฑ
ฑฑฬออออออออออุออออออออออัอออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบDATA      ณ ANALISTA ณ  MOTIVO                                         บฑฑ
ฑฑฬออออออออออุออออออออออุอออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบ          ณ          ณ                                                 บฑฑ
ฑฑศออออออออออฯออออออออออฯอออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
*/
SET DEVICE TO SCREEN

Static Function xVerPag()

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณInicia a montagem da impressao.ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
If	( nLinha >= 3000 )
	
	If	( ! lFlag )
		xRodape()
		oPrint:EndPage()
		nLinha:= 600
	Else
		nLinha:= 800
	EndIf
	
	oPrint:StartPage()
	xCabec()
	
	lPrintDesTab := .t.
EndIf

Return

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหออออออัอออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณ AjustaSX1บAutor ณLuis Henrique Robustoบ Data ณ  25/10/04   บฑฑ
ฑฑฬออออออออออุออออออออออสออออออฯอออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDesc.     ณ Ajusta o SX1 - Arquivo de Perguntas..                      บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ Funcao Principal                                           บฑฑ
ฑฑฬออออออออออุออออออออออัอออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบDATA      ณ ANALISTA ณ MOTIVO                                          บฑฑ
ฑฑฬออออออออออุออออออออออุอออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบ          ณ          ณ                                                 บฑฑ
ฑฑศออออออออออฯออออออออออฯอออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
*/

Static Function AjustaSX1(cPerg)
Local	aRegs   := {},;
_sAlias := Alias(),;
nX

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณCampos a serem grav. no SX1ณ
//ณaRegs[nx][01] - X1_GRUPO   ณ
//ณaRegs[nx][02] - X1_ORDEM   ณ
//ณaRegs[nx][03] - X1_PERGUNTEณ
//ณaRegs[nx][04] - X1_PERSPA  ณ
//ณaRegs[nx][05] - X1_PERENG  ณ
//ณaRegs[nx][06] - X1_VARIAVL ณ
//ณaRegs[nx][07] - X1_TIPO    ณ
//ณaRegs[nx][08] - X1_TAMANHO ณ
//ณaRegs[nx][09] - X1_DECIMAL ณ
//ณaRegs[nx][10] - X1_PRESEL  ณ
//ณaRegs[nx][11] - X1_GSC     ณ
//ณaRegs[nx][12] - X1_VALID   ณ
//ณaRegs[nx][13] - X1_VAR01   ณ
//ณaRegs[nx][14] - X1_DEF01   ณ
//ณaRegs[nx][15] - X1_DEF02   ณ
//ณaRegs[nx][16] - X1_DEF03   ณ
//ณaRegs[nx][17] - X1_F3      ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณCria uma array, contendo todos os valores...ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
aAdd(aRegs,{cPerg,'01','Numero do Pedido   ?','Numero do Pedido   ?','Numero do Pedido   ?','mv_ch1','C', 6,0,0,'G','','mv_par01','','','',''})
aAdd(aRegs,{cPerg,'02','Imprime precos     ?','Imprime precos     ?','Imprime precos     ?','mv_ch2','N', 1,0,1,'C','','mv_par02',OemToAnsi('Nใo'),'Sim','',''})
aAdd(aRegs,{cPerg,'03','Titulo do Relatorio?','Titulo do Relatorio?','Titulo do Relatorio?','mv_ch3','N', 1,0,1,'C','','mv_par03',OemToAnsi('Cota็ใo'),'Pedido','',''})
aAdd(aRegs,{cPerg,'04',OemToAnsi('Observa็๕es'),'Observa็๕es         ','Observa็๕es         ','mv_ch4','C',70,0,1,'G','','mv_par04','','','',''})
aAdd(aRegs,{cPerg,'05','                    ','                    ','                    ','mv_ch5','C',70,0,1,'G','','mv_par05','','','',''})
aAdd(aRegs,{cPerg,'06','                    ','                    ','                    ','mv_ch6','C',70,0,0,'G','','mv_par06','','','',''})
aAdd(aRegs,{cPerg,'07','                    ','                    ','                    ','mv_ch7','C',70,0,0,'G','','mv_par07','','','',''})
aAdd(aRegs,{cPerg,'08','Imp. Cod. Prod. For?','Imp. Cod. Prod. For?','Imp. Cod. Prod. For?','mv_ch8','N', 1,0,1,'C','','mv_par08',OemToAnsi('Sim'),OemToAnsi('Nใo'),'',''})

DbSelectArea('SX1')
SX1->(DbSetOrder(1))

For nX:=1 to Len(aRegs)
	If	RecLock('SX1',Iif(!SX1->(DbSeek(aRegs[nx][01]+aRegs[nx][02])),.t.,.f.))
		Replace SX1->X1_GRUPO		With aRegs[nx][01]
		Replace SX1->X1_ORDEM   	With aRegs[nx][02]
		Replace SX1->X1_PERGUNTE	With aRegs[nx][03]
		Replace SX1->X1_PERSPA		With aRegs[nx][04]
		Replace SX1->X1_PERENG		With aRegs[nx][05]
		Replace SX1->X1_VARIAVL		With aRegs[nx][06]
		Replace SX1->X1_TIPO		With aRegs[nx][07]
		Replace SX1->X1_TAMANHO		With aRegs[nx][08]
		Replace SX1->X1_DECIMAL		With aRegs[nx][09]
		Replace SX1->X1_PRESEL		With aRegs[nx][10]
		Replace SX1->X1_GSC			With aRegs[nx][11]
		Replace SX1->X1_VALID		With aRegs[nx][12]
		Replace SX1->X1_VAR01		With aRegs[nx][13]
		Replace SX1->X1_DEF01		With aRegs[nx][14]
		Replace SX1->X1_DEF02		With aRegs[nx][15]
		Replace SX1->X1_DEF03		With aRegs[nx][16]
		Replace SX1->X1_F3   		With aRegs[nx][17]
		MsUnlock('SX1')
	Else
		Help('',1,'REGNOIS')
	Endif
Next nX
Return

Return


/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหออออออัอออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณ xPrintRelบAutor ณLuis Henrique Robustoบ Data ณ  10/09/04   บฑฑ
ฑฑฬออออออออออุออออออออออสออออออฯอออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDesc.     ณ Imprime a Duplicata...                                     บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ Funcao Principal                                           บฑฑ
ฑฑฬออออออออออุออออออออออัอออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบDATA      ณ ANALISTA ณ MOTIVO                                          บฑฑ
ฑฑฬออออออออออุออออออออออุอออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบ          ณ          ณ                                                 บฑฑ
ฑฑศออออออออออฯออออออออออฯอออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
*/

User Function fPrintPDF() 
	Local lAdjustToLegacy := .F.
	Local lDisableSetup  := .T.
	Local oPrinter
	Local cLocal          := "c:\relato\"
	Local cCodINt25 := "34190184239878442204400130920002152710000053475"
	Local cCodEAN :=      "123456789012"   
	Local cFilePrint := ""
	oPrinter := FWMSPrinter():New('orcamento_000000.PD_', IMP_PDF, lAdjustToLegacy,cLocal, lDisableSetup, , , , , , .F., )
	oPrinter:FWMSBAR("INT25" /*cTypeBar*/,1/*nRow*/ ,1/*nCol*/, cCodINt25/*cCode*/,oPrinter/*oPrint*/,.T./*lCheck*/,/*Color*/,.T./*lHorz*/,0.02/*nWidth*/,0.8/*nHeigth*/,.T./*lBanner*/,"Arial"/*cFont*/,NIL/*cMode*/,.F./*lPrint*/,2/*nPFWidth*/,2/*nPFHeigth*/,.F./*lCmtr2Pix*/)
	oPrinter:FWMSBAR("EAN13" /*cTypeBar*/,5/*nRow*/ ,1/*nCol*/ ,cCodEAN  /*cCode*/,oPrinter/*oPrint*/,/*lCheck*/,/*Color*/,/*lHorz*/, /*nWidth*/,/*nHeigth*/,/*lBanner*/,/*cFont*/,/*cMode*/,.F./*lPrint*/,/*nPFWidth*/,/*nPFHeigth*/,/*lCmtr2Pix*/)
	oPrinter:Box( 130, 10, 500, 700, "-4")
	oPrinter:Say(210,10,"Teste para Code128C")
	cFilePrint := cLocal+"orcamento_000000.PD_"
	File2Printer( cFilePrint, "PDF" )
        oPrinter:cPathPDF:= cLocal 
	oPrinter:Preview()
Return



 
// Este exemplo tem como objetivo principal documentar a classe TMailManager, com
// foco nas fun็oes que sใo usadas apenas por conexใo IMAP.
//-----------------------------------------------------------------------------------
// Este exemplo irแ fazer basicamente manipula็ใo dos folders de uma conta de email,
// atrav้s de uma conexใo com o servidor IMAP. Ex: imap.microsiga.com.br
//-----------------------------------------------------------------------------------

User Function tstIMAP()
  Local aStPastas := {}
  Local sFolder := "TSTIMAP"
  Local sErro := ""
  Local lRet
  Private oMailManager
  oMailManager := TMailManager():New()
   
  //uso a fun็ใo init para setar os dados.
  nErro := oMailManager:Init( "imap.gmail.com", "" , "edimagalhaess@gmail.com", "ed287407" )
  If nErro != 0
    sErro := oMailManager:GetErrorString( nErro )
    Conout( sErro )
    Return .F.
  EndIf
   
  //realizo uma CONEXAO IMAP
  // Necessario configurar no arquivo .ini do server
  // [MAIL]
  // Protocol=IMAP
  nErro := oMailManager:IMAPConnect()
  If nErro != 0
    sErro := oMailManager:GetErrorString( nErro )
    Conout( sErro )
    Return .F.
  EndIf
   
  //informo o server que iremos trabalhar com ID real da mensagem
  oMailManager:SetUseRealID( .T. )
   
  //tento ir para o folder TSTIMAP
  If !oMailManager:ChangeFolder( sFolder )
    //entra aqui pq o folder nao existe, entao crio ele
    //tento criar o folder no server IMAP
    If !oMailManager:CreateFolder( sFolder )
      Conout( "Erro na cria็ใo do folder" )
      lRet := .F.
    else
      //set o folder como assinado, para aparecer
      If !oMailManager:SetFolderSubscribe( sFolder, .T. )
        Conout( "Erro na assinatura do folder" )
      EndIf
    EndIf
  EndIf
   
  //pego os folders(pastas) existentes no servidor, incluido o TSTIMAP
  GetFolderList( @aStPastas )
  varinfo( "PASTAS", aStPastas )
   
  //Verificamos o folder corrente em uso
  sFolder := oMailManager:GetFolder()
  conout( "Folder Corrente" + sFolder )
   
  If !oMailManager:DeleteFolder( sFolder )
    conout( "nao foi possivel deletar a pasta" + sFolder )
  EndIf
   
  oMailManager:IMAPDisconnect()
return .T.
Static function GetFolderList( aStPastas )
  Local nI := 0, nTam := 0
  Local aTemp := {}
  Local aFolder
  aTemp := oMailManager:GetFolderList()
  nTam := Len( aTemp )
  For nI := 1 To nTam
    //crio um array temp {nome, nTotalMensagens, nMensagensNaoLidas, lAssinada}
    aFolder := {}
    aAdd( aFolder, aTemp[ nI ][1] )
    aAdd( aFolder, aTemp[ nI ][3] )
    aAdd( aFolder, aTemp[ nI ][5] )
    aAdd( aFolder, .T. )
 
    //adiciono no array de referencia do parametro
    aAdd( aStPastas, aFolder )
    aFolder := NIL
  Next
Return .T.




