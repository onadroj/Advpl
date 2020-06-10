#INCLUDE "PROTHEUS.CH"        
#include "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณConcRazon บ Autor ณ AP6 IDE            บ Data ณ  07/12/09   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDescricao ณ Concilia o razonete com os valores contแbeis               บฑฑ
ฑฑบ          ณ                                                            บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑศออออออออออฯออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
/*/

User Function ConcRazon()
Local aSays 	:= {}
Local aButtons	:= {}
Local nOpca 	:= 0

Private cCadastro := "Concilia็ใo de Clientes e Fornecedores"
Private cFile:=space(80)
Private dDatRef:=CTOD("  /  /  ")
DEFINE FONT oFont NAME "Arial" SIZE 0, -11
DEFINE MSDIALOG oDlg FROM 0, 0 TO 380, 450 TITLE  "Concilia็ใo Clientes/Fornecedores" PIXEL
//@ 03,05 TO 74,135 lable "Parametros"
@ 12,5 Say "Gere o arquivo na versใo R3, em disco. Sempre gere do inํcio at้ a data de corte." size 210,7  of oDlg PIXEL
@ 24,5 Say "Filtre o tipo corretamente. Lembre-se que a conta contแbil vem do cadastro do cliente" size 210,7  of oDlg PIXEL
@ 36,5 Say "e do fornecedor. Tire pela op็ใo sint้tica. Lembre-se, ning๚em deve lan็ar nada no" size 210,7  of oDlg PIXEL
@ 48,5 Say "perํodo e a data de refer๊ncia deve ser a mesma do razonete. " size 210,7  of oDlg PIXEL
@ 72,5 Say "Arquivo a conciliar" size 180,7  of oDlg PIXEL
@ 84,5 MSGet cFile size 170,7 of oDlg PIXEL 
@ 96,5 Say "Data Ref." size 180,7 of oDlg PIXEL
@ 108,5 MsGet dDatRef size 050,7 of oDlg PIXEL 

DEFINE SBUTTON FROM 084,200  TYPE 14 ACTION (RelPath()) ENABLE OF oDlg
DEFINE SBUTTON FROM 160,170  TYPE 1 ACTION (nOpca := 1,oDlg:End()) ENABLE OF oDlg
DEFINE SBUTTON FROM 160,200  TYPE 2 ACTION (nOpca := 0,oDlg:End()) ENABLE OF oDlg
ACTIVATE MSDIALOG oDlg CENTERED

IF nOpca == 1     .AND.!EMPTY(cFile)    .and. !EMPTY(dDatRef)
	//	FinR550R3()
	Processa({|lEnd| Proc01()})
Endif
Return

Static Function Proc01
Local cDesc1         := "Este programa tem como objetivo imprimir relat๓rio "
Local cDesc2         := "de concilia็ใo de clientes/fornecedores."
Local cDesc3         := ""
Local cPict          := ""
Local titulo       := "Concilia็ใo Clientes/Fornecedores"
Local nLin         := 80

Local Cabec1       := "ITEM CONTมBIL  VLR FINANCEIRO VALOR CONTมBIL      DIFERENวA"

Local Cabec2       := ""
Local imprime      := .T.
Local aOrd := {}
Private lEnd         := .F.
Private lAbortPrint  := .F.
Private CbTxt        := ""
Private limite           := 80
Private tamanho          := "P"
Private nomeprog         := "CONCRAZON"
Private nTipo            := 18
Private aReturn          := { "Zebrado", 1, "Administracao", 2, 2, 1, "", 1}
Private nLastKey        := 0
Private cbtxt      := Space(10)
Private cbcont     := 00
Private CONTFL     := 01
Private m_pag      := 01
Private wnrel      := "CONCRAZON"

Private cString := "SA1"

dbSelectArea("SA1")
dbSetOrder(1)

wnrel := SetPrint(cString,NomeProg,"",@titulo,cDesc1,cDesc2,cDesc3,.F.,aOrd,.F.,Tamanho,,.F.)

If nLastKey == 27
	Return
Endif

SetDefault(aReturn,cString)

If nLastKey == 27
	Return
Endif

nTipo := If(aReturn[4]==1,15,18)

RptStatus({|| RunReport(Cabec1,Cabec2,Titulo,nLin) },Titulo)
Return


/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบFuno    ณRUNREPORT บ Autor ณ AP6 IDE            บ Data ณ  09/12/09   บฑฑ
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
Local Soma1:=0
Local Soma2:=0
Local Soma3:=0
LOCAL nRegTxt  := 0
LOCAL i		   := 1
Local cTexto   := ""
Local lfaz     := .t.
Local ctipo    := ""
Local lContinua := .T.

//pergunte("FIN550",.F.)
//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณ abre o txt
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
FT_FUSE(cFile)
nLinhas := FT_flastrec()
FT_FGOTOP()
dDataIni:=ctod("  /  /  ")
dDatafim:=dDatRef
/*
//conto o numero de registros
while !FT_FEOF()
cTexto:= alltrim( FT_FREADLN() )
If at("Pergunta 02", cTexto) > 0 	 //achou pergunta da data fim
IF ASC(substr(ctexto,47,1))==27
dDatafim:=CTOD(substr(ctexto,39,8))
ELSE
dDatafim:=CTOD(substr(ctexto,39,10))
ENDIF
endif
FT_FSKIP()
nRegTxt++
enddo
*/
//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณ inicia de fato
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
SetRegua(nLinhas)

FT_FGOTOP()
While lfaz .and. !FT_FEOF()
	cTexto:= alltrim( FT_FREADLN() )
	If at("Pergunta 01", cTexto) > 0 	 //achou pergunta da data inicio
		IF ASC(substr(ctexto,46,1))==27
			dDataIni:=CTOD(substr(ctexto,31,8))
		ELSE
			dDataIni:=CTOD(substr(ctexto,31,10))
		ENDIF
	endif
	If at("Pergunta 02", cTexto) > 0 	 //achou pergunta da data fim
		IF ASC(substr(ctexto,45,1))==27
			dDatafim:=CTOD(substr(ctexto,30,8))
		ELSE
			dDatafim:=CTOD(substr(ctexto,30,10))
		ENDIF
	endif
	If !FT_FEOF()
		If at("FINR550", cTexto) > 0 //achou o titulo
			If at("CLIENTE", cTexto ) > 0
				ctipo := "C"
			Else
				ctipo := "F"
			End If
			lfaz := .F.
		End If
	else
		lfaz := .f.
	End If
	FT_FSKIP()
enddo
If ctipo == ""
	MsgBox ("Arquivo nใo ้ Razonete!","CONCRAZON")
	lContinua := .F.
End If
lentrou:=.f.

while !FT_FEOF() .and. lContinua
	cTexto:= alltrim( FT_FREADLN() )
	If at("Pergunta 01", cTexto) > 0 	 //achou pergunta da data inicio
		IF ASC(substr(ctexto,46,1))==27
			dDataIni:=CTOD(substr(ctexto,31,8))
		ELSE
			dDataIni:=CTOD(substr(ctexto,31,10))
		ENDIF
	endif
	If at("Pergunta 02", cTexto) > 0 	 //achou pergunta da data fim
		IF ASC(substr(ctexto,45,1))==27
			dDatafim:=CTOD(substr(ctexto,30,8))
		ELSE
			dDatafim:=CTOD(substr(ctexto,30,10))
		ENDIF
	endif
	If lAbortPrint
		@nLin,00 PSAY "*** CANCELADO PELO OPERADOR ***"
		Exit
	Endif
	
	If nLin > 55
		Cabec(Titulo,Cabec1,Cabec2,NomeProg,Tamanho,nTipo)
		nLin := 8
	Endif
	If SUBSTR(cTexto, 7, 1) == "-" .And. SUBSTR(cTexto, 10, 1) == " " .And. ALLTRIM(SUBSTR(cTexto, 99, 1)) == ","  //linha de dados
		if !empty(dDataIni)
			lContinua:=.f.
			@nLin,00 PSAY "Data inํcio do relat๓rio nใo estแ em branco!"
			nLin++
		endif
		if dDataFim#dDatRef
			lContinua:=.f.
			@nLin,00 PSAY "Parโmetro da data de refer๊ncia informado nใo coincide com o do razonete!"
			nLin++
		endif
		IF lContinua
			lentrou:=.t.
			cConta:=ALLTRIM(SUBSTR(cTexto, 45, 20))
			cCodigo := SubStr(cTexto, 1, 6)
			cloja := SubStr(cTexto, 8, 2)
			cvalor := SubStr(cTexto, 117, 15)
			cDC := SubStr(cTexto, 132, 1)
			cItem:=ctipo + cCodigo + cloja
			nValor := CNV(cvalor)
			If cDC == "D"
				nValor := nValor * -1
			End If
		// uSldCT4(dDataIni,dDataFim,cContaIni,cContaFim,cItemIni,cItemFim,lVlrZerado,lImpAntLp,dDataLP)
		
			cCtbVlr := USLDCT4(ctod("  /  /  "),dDatRef,cConta,cConta,cItem,cItem,.T.,.F.,"") *-1
			IF (nValor - cCtbVlr)#0
				@nLin,00 PSAY ctipo + cCodigo + cloja
				@nLin,15 PSAY Transform(nValor,"@E 999,999,999.99")
				@nLin,30 PSAY Transform(cCtbVlr,"@E 999,999,999.99")
				@nLin,45 PSAY Transform(nValor - cCtbVlr,"@E 999,999,999.99")
				Soma1+=nValor
				Soma2+=cCtbVlr
				Soma3+=nValor - cCtbVlr
				nlin++
			ENDIF
		End If
	Endif
	IncRegua()
	
	i+=1
	
	FT_FSKIP()
enddo
if lentrou
	@nLin,15 PSAY Transform(Soma1,"@E 999,999,999.99")
	@nLin,30 PSAY Transform(Soma2,"@E 999,999,999.99")
	@nLin,45 PSAY Transform(Soma3,"@E 999,999,999.99")
endif


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
//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณ fecha o txt aberto
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
FT_FUSE()

Return
//003138

Static Function RelPath()
Local cMask:= "Arquivos Relatorio (*.##r) |*.##r|""
Local nDefaultMask := 0
Local cDefaultDir  := __reldir
Local nOptions:= GETF_OVERWRITEPROMPT+GETF_LOCALHARD+GETF_NETWORKDRIVE
cRelArq	:= cGetFile(cMask, "Selecione arquivo ",nDefaultMask,cDefaultDir,,nOptions)
cfile := cRelArq
Return !Empty(cRelArq)

User Function RelArq()
Return(cRelArq)

Static Function CNV(cValor)
Local cRet:=""
local nret:=0
cValor:=alltrim(cValor)
for x:=1 to len(cValor)
	if SUBSTR(cValor,x,1)#"."
		if SUBSTR(cValor,x,1)==","
			cRet+="."
		else
			cRet+=SUBSTR(cValor,x,1)
		endif
	endif
next
nret:=val(cRet)
return(nRet)


Static Function uSldCT4(dDataIni,dDataFim,cContaIni,cContaFim,cItemIni,cItemFim,lVlrZerado,lImpAntLp,dDataLP)

Local nVlrCt4:=0
Local cQuery		:= ""
Local aAreaQry		:= GetArea()
Local aTamVlr		:= TAMSX3("CT4_DEBITO")
Local cCampUSU		:= ""
Local aStrSTRU		:= {}
Local nStruLen		:= 0
Local nStr			:= 1
if type("lImpAntLP")=="U"
	lImpAntLP	:= .F.
endif
if type("dDataLP")=="U"
	dDataLP 	:= CTOD("  /  /  ")
endif

cQuery := " SELECT CTD_ITEM ITEM,CT1_CONTA CONTA, "
cQuery += " 		(SELECT SUM(CT4_DEBITO) "
cQuery += "			 	FROM "+RetSqlName("CT4")+" CT4 "
cQuery += " 			WHERE CT4_FILIAL = '"+xFilial("CT4")+"'  "
cQuery += " 			AND CT4_DATA BETWEEN '" + DTOS(dDataIni) + "' AND '"+ DTOS(dDataFim) + "' "
cQuery += " 			AND ARQ2.CTD_ITEM	= CT4_ITEM "
cQuery += " 			AND CT4_MOEDA = '01' "
cQuery += " 			AND CT4_TPSALD = '1' "
cQuery += " 			AND ARQ.CT1_CONTA	= CT4_CONTA "
cQuery += " 			AND CT4.D_E_L_E_T_ = '') SALDODEB, "

If lImpAntLP
	cQuery += " 		(SELECT SUM(CT4_DEBITO) "
	cQuery += "			 	FROM "+RetSqlName("CT4")+" CT4 "
	cQuery += " 			WHERE CT4_FILIAL = '"+xFilial("CT4")+"'  "
	cQuery += " 			AND CT4_DATA BETWEEN '" + DTOS(dDataIni) + "' AND '"+ DTOS(dDataFim) + "' "
	cQuery += " 			AND ARQ2.CTD_ITEM	= CT4_ITEM "
	cQuery += " 			AND CT4_MOEDA = '01' "
	cQuery += " 			AND CT4_TPSALD = '1' "
	cQuery += " 			AND ARQ.CT1_CONTA	= CT4_CONTA "
	cQuery += " 			AND CT4_DATA BETWEEN '" + DTOS(dDataIni) + "' AND '"+ DTOS(dDataFim) + "' "
	cQuery += "				AND CT4_LP = 'Z' AND ((CT4_DTLP <> ' ' AND CT4_DTLP >= '"+DTOS(dDataLP)+"') OR (CT4_DTLP = '' AND CT4_DATA >= '"+DTOS(dDataLP)+"'))"
	cQuery += " 			AND CT4.D_E_L_E_T_ = '') "
	cQuery += "  MOVLPDEB, "
EndIf

cQuery += " 		(SELECT SUM(CT4_CREDIT) "
cQuery += " 			FROM "+RetSqlName("CT4")+" CT4 "
cQuery += " 			WHERE CT4_FILIAL	= '"+xFilial("CT4")+"' "
cQuery += " 			AND CT4_DATA BETWEEN '" + DTOS(dDataIni) + "' AND '"+ DTOS(dDataFim) + "' "
cQuery += " 			AND ARQ2.CTD_ITEM	= CT4_ITEM "
cQuery += " 			AND CT4_MOEDA = '01' "
cQuery += " 			AND CT4_TPSALD = '1' "
cQuery += " 			AND ARQ.CT1_CONTA	= CT4_CONTA "
cQuery += " 			AND CT4.D_E_L_E_T_ = '') SALDOCRD "

If lImpAntLP
	cQuery += ", 		(SELECT SUM(CT4_CREDIT) "
	cQuery += "			 	FROM "+RetSqlName("CT4")+" CT4 "
	cQuery += " 			WHERE CT4_FILIAL = '"+xFilial("CT4")+"'  "
	cQuery += " 			AND CT4_DATA BETWEEN '" + DTOS(dDataIni) + "' AND '"+ DTOS(dDataFim) + "' "
	cQuery += " 			AND ARQ2.CTD_ITEM	= CT4_ITEM "
	cQuery += " 			AND CT4_MOEDA = '01' "
	cQuery += " 			AND CT4_TPSALD = '1' "
	cQuery += " 			AND ARQ.CT1_CONTA	= CT4_CONTA "
	cQuery += " 			AND CT4_DATA BETWEEN '" + DTOS(dDataIni) + "' AND '"+ DTOS(dDataFim) + "' "
	cQuery += "				AND CT4_LP = 'Z' AND ((CT4_DTLP <> ' ' AND CT4_DTLP >= '"+DTOS(dDataLP)+"') OR (CT4_DTLP = '' AND CT4_DATA >= '"+DTOS(dDataLP)+"'))"
	cQuery += " 			AND CT4.D_E_L_E_T_ = '') "
	cQuery += "  MOVLPCRD "
EndIf

cQuery += " 	FROM "+RetSqlName("CT1")+" ARQ, "+RetSqlName("CTD")+" ARQ2 "
cQuery += " 	WHERE ARQ.CT1_FILIAL = '"+xFilial("CT1")+"' "
cQuery += " 	AND ARQ.CT1_CONTA BETWEEN '"+cContaIni+"' AND '"+cContaFim+"' "
cQuery += " 	AND ARQ.CT1_CLASSE = '2' "

cQuery += " 	AND  ARQ2.CTD_FILIAL = '"+xFilial("CTD")+"' "
cQuery += " 	AND ARQ2.CTD_ITEM BETWEEN '"+cItemIni+"' AND '"+cItemFim+"' "
cQuery += " 	AND ARQ2.CTD_CLASSE = '2' "


cQuery += " 	AND ARQ.D_E_L_E_T_ = '' "
cQuery += " 	AND ARQ2.D_E_L_E_T_ = '' "

If !lVlrZerado .and. !lImpAntLp
	cQuery += " 	AND ((SELECT SUM(CT4_DEBITO) "
	cQuery += "			 	FROM "+RetSqlName("CT4")+" CT4 "
	cQuery += " 			WHERE CT4_FILIAL = '"+xFilial("CT4")+"'  "
	cQuery += " 			AND CT4_DATA <  '"+DTOS(dDataIni)+"' "
	cQuery += " 			AND ARQ2.CTD_ITEM	= CT4_ITEM "
	cQuery += " 			AND CT4_MOEDA = '01' "
	cQuery += " 			AND CT4_TPSALD = '1' "
	cQuery += " 			AND ARQ.CT1_CONTA	= CT4_CONTA "
	cQuery += " 			AND CT4.D_E_L_E_T_ = '') <> 0 "
	cQuery += " 	OR "
	cQuery += " 		(SELECT SUM(CT4_CREDIT) "
	cQuery += " 			FROM "+RetSqlName("CT4")+" CT4 "
	cQuery += " 			WHERE CT4_FILIAL	= '"+xFilial("CT4")+"' "
	cQuery += " 			AND CT4_DATA <  '"+DTOS(dDataIni)+"' "
	cQuery += " 			AND ARQ2.CTD_ITEM	= CT4_ITEM "
	cQuery += " 			AND CT4_MOEDA = '01' "
	cQuery += " 			AND CT4_TPSALD = '1' "
	cQuery += " 			AND ARQ.CT1_CONTA	= CT4_CONTA "
	cQuery += " 			AND CT4.D_E_L_E_T_ = '') <> 0 "
	cQuery += " 	OR "
	cQuery += " 		(SELECT SUM(CT4_DEBITO) "
	cQuery += "			 	FROM "+RetSqlName("CT4")+" CT4 "
	cQuery += " 			WHERE CT4_FILIAL = '"+xFilial("CT4")+"'  "
	cQuery += " 			AND CT4_DATA BETWEEN '" + DTOS(dDataIni) + "' AND '"+ DTOS(dDataFim) + "' "
	cQuery += " 			AND ARQ2.CTD_ITEM	= CT4_ITEM "
	cQuery += " 			AND CT4_MOEDA = '01' "
	cQuery += " 			AND CT4_TPSALD = '1' "
	cQuery += " 			AND ARQ.CT1_CONTA	= CT4_CONTA "
	cQuery += " 			AND CT4.D_E_L_E_T_ = '')<> 0 "
	cQuery += " 	OR "
	cQuery += " 		(SELECT SUM(CT4_CREDIT) "
	cQuery += " 			FROM "+RetSqlName("CT4")+" CT4 "
	cQuery += " 			WHERE CT4_FILIAL	= '"+xFilial("CT4")+"' "
	cQuery += " 			AND CT4_DATA BETWEEN '" + DTOS(dDataIni) + "' AND '"+ DTOS(dDataFim) + "' "
	cQuery += " 			AND ARQ2.CTD_ITEM	= CT4_ITEM "
	cQuery += " 			AND CT4_MOEDA = '01' "
	cQuery += " 			AND CT4_TPSALD = '1' "
	cQuery += " 			AND ARQ.CT1_CONTA	= CT4_CONTA "
	cQuery += " 			AND CT4.D_E_L_E_T_ = '') <> 0 "
Endif

cQuery := ChangeQuery(cQuery)

If Select("TRBTMP") > 0
	dbSelectArea("TRBTMP")
	dbCloseArea()
Endif

dbUseArea(.T.,"TOPCONN",TcGenQry(,,cQuery),"TRBTMP",.T.,.F.)

TcSetField("TRBTMP","SALDODEB","N",aTamVlr[1],aTamVlr[2])
TcSetField("TRBTMP","SALDOCRD","N",aTamVlr[1],aTamVlr[2])
If lImpAntLP
	TcSetField("TRBTMP","MOVLPDEB","N",aTamVlr[1],aTamVlr[2])
	TcSetField("TRBTMP","MOVLPCRD","N",aTamVlr[1],aTamVlr[2])
EndIf
DBGOTOP()
IF !eof()
	nVlrCt4 := TRBTMP->SALDODEB - TRBTMP->SALDOCRD
	//	nVlrCt4 := nVlrCt4 * -1
ENDIF
dbselectarea("TRBTMP")
close
RestArea(aAreaQry)
return (nVlrCt4)
