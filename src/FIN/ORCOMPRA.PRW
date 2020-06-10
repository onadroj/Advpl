#include "rwmake.ch"        // incluido pelo assistente de conversao do AP5 IDE em 29/05/02
User Function ORCOMPRA()        // incluido pelo assistente de conversao do AP5 IDE em 29/05/02

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Declaracao de variaveis utilizadas no programa atraves da funcao    ³
//³ SetPrvt, que criara somente as variaveis definidas pelo usuario,    ³
//³ identificando as variaveis publicas do sistema utilizadas no codigo ³
//³ Incluido pelo assistente de conversao do AP5 IDE                    ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

SetPrvt("CBTXT,CBCONT,NORDEM,ALFA,Z,M")
SetPrvt("TAMANHO,LIMITE,TITULO,CDESC1,CDESC2,CDESC3")
SetPrvt("ARETURN,NOMEPROG,CPERG,NLASTKEY,LCONTINUA,NLIN")
SetPrvt("NPAG,WNREL,NTAMOC,CSTRING,NIMP,NLININI")
SetPrvt("CPEDATU")
SetPrvt("NOPC,CCOR,CONTINUA,_cond,_tpfrete")
SetPrvt("MOBS,XTOT_FAT,XTOT_IPI,XDESC1,XDESC2,XDESC3,XEMISSAO,XREAJUST")
SetPrvt("J,_QTDIMP,")

#IFNDEF WINDOWS
	// Movido para o inicio do arquivo pelo assistente de conversao do AP5 IDE em 29/05/02 ==> #DEFINE PSAY SAY
#ENDIF

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Variaveis utilizadas para parametros                         ³
//³ mv_par01             // Da ordem de compra                   ³
//³ mv_par02             // Ate a ordem de compra                ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
Private nTipo            := 15
CbTxt:=""
CbCont:=0
nOrdem :=0
Alfa := 0
Z:=0
M:=0
tamanho:="M"
limite:=132
titulo :=PADC("Pedido de Compra",74)
cDesc1 :=PADC("Este programa ira emitir o Pedido de Compra",74)
cDesc2 :=""
cDesc3 :=""
aReturn := { "Especial", 1,"Administracao", 1, 2, 1,"",1 }


nomeprog:="ORCOMPRA"
cPerg:="MTA235"
nLastKey:=0
lContinua := .T.
nLin:=1
npag:=1
wnrel    := "ORCOMPRA"

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Verifica as perguntas selecionadas, busca o padrao da ordcomp           ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
Pergunte(cPerg,.t.)               // Pergunta no SX1
cString:="SC7"


//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Envia controle para a funcao SETPRINT                        ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
lServer  := .t.
aImp     := RetImpWin(lServer ) // .T. Quando for no SERVER e .F. no CLIENT (Retorna o nome das impressoras instaladas)
cDrive   := "Epson.drv"
cNomeImp := "LPT2"

//wnrel:=SetPrint(cString,wnrel,cPerg,Titulo,cDesc1,cDesc2,cDesc3,.T.)
//         SetPrint(cAlias, cNomeRel, cPerg, cDesc1, cDesc2, cDesc3, cDesc4, lDic, aOrdem, lComp, cClass)
//wnrel:= SetPrint(cString,wnrel,cPerg,Titulo,cDesc1,cDesc2,cDesc3,.T.,"",.t.,Tamanho,nil    ,.T.    ,nil    ,cDrive,.T.)
//IMPRIMIR DIRETO NA IMPRESSORA wnrel:= SetPrint(cString,wnrel,cPerg,Titulo,cDesc1,cDesc2,cDesc3,.f.,"",.t.,Tamanho)//,nil    ,.T.    ,nil    ,cDrive,.T.)
//  wnrel:=SetPrint(cString,wnrel,cPerg,Titulo,cDesc1,cDesc2,cDesc3,.f.,"",.T.,"G","",.t.)
wnrel := SetPrint(cString,NomeProg,cPerg,@titulo,cDesc1,cDesc2,cDesc3,.T.,"",.T.,Tamanho,,.T.)

If nLastKey == 27
	Return
Endif
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Verifica Posicao do Formulario na Impressora                 ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
SetDefault(aReturn,cString)
If nLastKey == 27
	Return
Endif
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³                                                              ³
//³ Inicio do Processamento da Ordem de Compra                   ³
//³                                                              ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
nTipo := If(aReturn[4]==1,15,18)
RptStatus({|| RptDetail()})// Substituido pelo assistente de conversao do AP5 IDE em 29/05/02 ==> 	RptStatus({|| Execute(RptDetail)})
Return
Static Function RptDetail()

dbSelectArea("SC7")                //
//IF ALLTRIM(DTOS(mv_par02)) = ""
   DbSetOrder(1)                      // C7_FILIAL+C7_NUM+C7_ITEM+C7_SEQUEN
   cString:="SC7"
   dbSeek(xFilial()+mv_par04,.t.)
//ELSE
//   DbSetOrder(7)                      // C7_FILIAL+DTOS(C7_EMISSAO)+C7_NUM+C7_ITEM+C7_SEQUEN
//   cString:="SC7"
//   dbSeek(xFilial()+DTOS(mv_par02)+mv_par04,.t.)
//ENDIF
SetRegua(val(mv_par05)-val(mv_par04))
nIMP := 0
do while !eof() .and. SC7->C7_NUM <= mv_par05 .and. lcontinua
	nIMP := nIMP + 1
	
	IF lAbortPrint
		@ 00,01 PSAY "** CANCELADO PELO OPERADOR **"
		lContinua := .F.
	Endif
	nLinIni:=nLin                         // Linha Inicial da Impressao
	
	dbSelectArea("SC7")                //
	DbSetOrder(1)
	dbSeek(xFilial()+C7_Num+C7_item,.t.)
	cPedAtu := SC7->C7_Num
	dbSelectArea("SA2")                // * Cadastro de Fornecedores
	dbSetOrder(1)
	dbSeek(xFilial()+SC7->C7_FORNECE+sc7->c7_LOJA)

	Imprime()
	
	IncRegua()                    // Termometro de Impressao
	
	nLin:=0         
	dbSelectArea("SC7")
	dbseek(xFilial()+SC7->C7_NUM)
enddo

dbSelectArea("SC7")
Retindex("SC7")
Set Device To Screen

If aReturn[5] == 1
	Set Printer TO
	dbcommitAll()
	ourspool(wnrel)
Endif

MS_FLUSH()

return

Static Function Imprime()
continua := .f.
npag := 1
_cond:=SC7->C7_COND
_tpfrete:=SC7->C7_TPFRETE
CABEC(@nlin,@_cond,@_tpfrete)
nLin := 11
dbSelectArea("SC7")
DbSetOrder(1)
dbSeek(xFilial()+C7_Num+C7_item,.t.)
cPedAtu := SC7->C7_Num
XDESC1 := SC7->C7_DESC1
XDESC2 := SC7->C7_DESC2
XDESC3 := SC7->C7_DESC3
XEMISSAO := SC7->C7_EMISSAO
XREAJUST := SC7->C7_REAJUST
mOBS1 := ""
mOBS2 := ""
mOBS3 := ""
mOBS4 := ""
xTOT_FAT := 0
xTOT_IPI := 0
xTOT_DESC := 0
xTOT_FRETE := 0
do while cPedAtu == SC7->C7_Num 
	dbSelectArea("SC7")
	@ nlin, 000  PSAY "|"
	@ nlin, 001  PSAY SC7->C7_ITEM
	@ nlin, 004  PSAY substr(SC7->C7_PRODUTO,1,8)
	@ nlin, 013  PSAY substr(SC7->C7_DESCRI,1,49)  	
	@ nlin, 064  PSAY substr(SC7->C7_OP,1,6)  	
	@ nlin, 071  PSAY 	SC7->C7_UM
	@ nlin, 073  PSAY 	transform(SC7->C7_QUANT,"@E 99999.9999")
	@ nlin, 084  PSAY 	transform(SC7->C7_PRECO,"@E 9999999.9999")
	@ nlin, 097  PSAY 	transform(SC7->C7_IPI,"@E 99.99")
	@ nlin, 103  PSAY 	transform(SC7->C7_TOTAL,"@E 9,999,999.99")
	@ nlin, 116  PSAY  dtoc(SC7->C7_DATPRF)
	@ nlin, 125  PSAY 	SC7->C7_NUMSC
	@ nlin, 131  PSAY "|"
	nLin :=nLin+1
	@ nlin, 000  PSAY "|"
	IF GETMV("MV_MCONTAB")="CON"
		I1DESC:=ALLTRIM(POSICIONE("SI1",1,XFILIAL("SI1")+SC7->C7_CONTA,"I1_DESC"))
		I3DESC:=ALLTRIM(POSICIONE("SI3",1,XFILIAL("SI3")+SC7->C7_CC,"I3_DESC"))
	ELSE
		I1DESC:=ALLTRIM(POSICIONE("CT1",1,XFILIAL("CT1")+SC7->C7_CONTA,"CT1_DESC01"))
		I3DESC:=ALLTRIM(POSICIONE("CTT",1,XFILIAL("CTT")+SC7->C7_CC,"CTT_DESC01"))
	ENDIF
	@ nLin, 001 PSAY "Clas.: C.C.:"+ALLTRIM(SC7->C7_CC) +"-"+ I3DESC+" Conta:"+ALLTRIM(SC7->C7_CONTA)+"-"+ I1DESC+" TP.Ent.:"+SC7->C7_TPENT 
	@ nlin, 131  PSAY "|"
	nLin :=nLin+1
	xTOT_FAT := xTOT_FAT + SC7->C7_TOTAL
	xTOT_IPI := xTOT_IPI + SC7->C7_VALIPI
	xTOT_FRETE := xTOT_FRETE + SC7->C7_VALFRE
	xTOT_DESC := XTOT_DESC + SC7->C7_VLDESC
	if mOBS1 = "" 
	  mOBS1 := SC7->C7_OBS
	elseif mOBS2 = ""
	       mOBS2 := SC7->C7_OBS
	    elseif mOBS3 = ""
	            mOBS3 := SC7->C7_OBS
	         elseif mOBS4 = ""
	                 mOBS4 := SC7->C7_OBS
	endif
	//Registra mais uma impressao
    RecLock("SC7",.F.)
    replace C7_QTDREEM with _QTDIMP
    MSUnlock()

	DBskip()
	if eof () 
		exit
	endif
	if nLin > 41
		continua := .t.
		if nLin > 51
			RODAPE1 (@nlin)
			eject
			CABEC(@nlin,@_cond,@_tpfrete)
			continua := .f.
			nLin := 25
		endif
	endif
enddo
@ nLin, 000 PSAY "|----------------------------------------------------------------------------------------------------------------------------------|"
nLin :=nLin+1
J := nLin+(40-nLin)
do while nLin <= J
	@ nLin, 000  PSAY "|"
	@ nLin, nLin-5 PSAY "*      *      *      *      *      *      *      *      *      *      *"
	@ nLin, 131  PSAY "|"
	nLin :=nLin+1
enddo
if continua
	RODAPE1 (@nlin)
	eject
	CABEC(@nlin,@_cond,@_tpfrete)
	continua := .f.
	nLin := 25
endif
RODAPE2 (@nlin)

Return .t.



Static Function CABEC(nlin,_cond,_tpfrete)

// Localiza condicao de pagamento
dbSelectArea("SE4")
DbSetOrder(1)
dbSeek(xFilial()+_COND,.t.)

if continua
	npag := npag + 1
endif
_QTDIMP := SC7->C7_QTDREEM + 1
@ 01, 000 PSAY Chr(027)+chr(ntipo)+"|----------------------------------------------------------------------------------------------------------------------------------|"
@ 02, 000 PSAY "|                                           | O R D E M    D E  C O M P R A S                  |    "+cPedAtu+"/"+ALLTRIM(STR(NPAG))+"   "+ALLTRIM(STR(_QTDIMP))+"a.Emissao  1a.VIA "
@ 02, 131 PSAY "|"
@ 03, 000 PSAY "|"+alltrim(SM0->M0_NOMECOM)
@ 03, 044 PSAY "|--------------------------------------------------------------------------------------|"
@ 04, 000 PSAY "|"+Alltrim(SM0->M0_ENDCOB)
@ 04, 044 PSAY "| "+SA2->A2_NOME+" - "+ SA2->A2_COD+"/"+SA2->A2_LOJA 
@ 04, 098 PSAY " I.E.: "+SA2->A2_INSCR
@ 04, 131 PSAY "|"
@ 05, 000 PSAY "|CEP :"+alltrim(SM0->M0_CEPCOB) +" - "+alltrim(SM0->M0_CIDCOB)+" - "+Alltrim(SM0->M0_ESTCOB)
@ 05, 044 PSAY "| "+SA2->A2_END +"-"+SA2->A2_BAIRRO
@ 05, 131 PSAY "|" 
@ 06, 000 PSAY "|TEL: "+SM0->M0_TEL+" FAX: "+SM0->M0_FAX
@ 06, 044 PSAY "| "+SA2->A2_Mun+" - "+SA2->A2_EST+"CEP : "+SA2->A2_CEP+"  CNPJ:"+SA2->A2_CGC
@ 06, 131 PSAY "|" 
@ 07, 000 PSAY "|CNPJ: "+SM0->M0_CGC+" IE: "+SM0->M0_INSC
@ 07, 044 PSAY "| "+SC7->C7_CONTATO
@ 07, 068 PSAY "FONE: "+SA2->A2_TEL+"  FAX: "+SA2->A2_FAX
@ 07, 131 PSAY "|" 
@ 08, 000 PSAY "|----------------------------------------------------------------------------------------------------------------------------------|"
@ 09, 000 PSAY "|It| Codigo |Descricao do Material                             | O.S. |UM|  Quant. |Vlr Unitario| IPI | Vlr Total  |Entrega | S.C. |"
@ 10, 000 PSAY "|----------------------------------------------------------------------------------------------------------------------------------|"

Return

Static Function RODAPE1(nlin)

@ nLin,    000 PSAY "|----------------------------------------------------------------------------------------------------------------------------------|"
@ nLin+01, 000 PSAY "|                                                                                                                                  |"
@ nLin+02, 000 PSAY "|                                                 Continua...                                                                      |"
@ nLin+03, 000 PSAY "|                                                                                                                                  |"
@ nLin+04, 000 PSAY "|----------------------------------------------------------------------------------------------------------------------------------|"
@ nLin+05, 000 PSAY "                                    "
SetPrc(0,0)                              // (Zera o Formulario)
nLin := 0

Return

// Substituido pelo assistente de conversao do AP5 IDE em 29/05/02 ==> Function RODAPE2
Static Function RODAPE2(nlin)
XTOT_MERC:=XTOT_FAT
xTOT_FAT := xTOT_FAT + xTOT_IPI + xTOT_FRETE- xTOT_DESC


@ nLin+00, 000 PSAY "|----------------------------------------------------------------------------------------------------------------------------------|"
@ nLin+01, 000 PSAY "|             D E S C O N T O S --> "+transform(XDESC1,"@E 99.99")
@ nLin+01, 046 PSAY transform(XDESC2,"@E 99.99")
@ nLin+01, 055 PSAY transform(XDESC3,"@E 99.99")
@ nLin+01, 076 PSAY transform(XDESC1+XDESC2+XDESC3,"@E 99.99")
@ nLin+01, 131 PSAY "|"
@ nLin+02, 000 PSAY "|----------------------------------------------------------------------------------------------------------------------------------|"
@ nLin+03, 000 PSAY "| Local de Entrega  : "+ALLTRIM(SM0->M0_ENDENT)+ " - " +ALLTRIM(SM0->M0_CIDENT)+" - "+ALLTRIM(SM0->M0_ESTENT)+" - CEP : "+ALLTRIM(SM0->M0_CEPENT) 
@ nLin+03, 131 PSAY "|"
@ nLin+04, 000 PSAY "| Local de Cobranca : "+ALLTRIM(SM0->M0_ENDCOB)+ " - " +ALLTRIM(SM0->M0_CIDCOB)+" - "+ALLTRIM(SM0->M0_ESTCOB)+" - CEP : "+ALLTRIM(SM0->M0_CEPCOB) 
@ nLin+04, 131 PSAY "|"                   
@ nLin+05, 000 PSAY "|----------------------------------------------------------------------------------------------------------------------------------|"
@ nLin+06, 000 PSAY "| Condicao de Pagto "+_COND
@ nLin+06, 037 PSAY "|Data de Emissao| Total das Mercadorias :    "+transform(xTOT_MERC,"@E@Z 999,999,999.99")
@ nLin+06, 131 PSAY "|"
@ nLin+07, 000 PSAY "| "+SE4->E4_DESCRI
@ nLin+07, 037 PSAY "|    "+dtoc(XEMISSAO)+"   |                                                                             |"
@ nLin+08, 000 PSAY "|----------------------------------------------------|-----------------------------------------------------------------------------|"
@ nLin+09, 000 PSAY "| Reajuste : "+XREAJUST                                        
@ nLin+09, 053 PSAY "| IPI   :          "+transform(xTOT_IPI,"@E@Z 999,999,999.99")
@ nLin+09, 131 PSAY "|"
@ nLin+10, 000 PSAY "|----------------------------------------------------| Frete :          "+transform(xTOT_FRETE,"@E@Z 999,999,999.99")
@ nLin+10, 131 PSAY "|"
@ nLin+11, 000 PSAY "| Observacoes                                        | Grupo :          "+SC7->C7_GRUPCOM
@ nLin+11, 131 PSAY "|"
@ nLin+12, 000 PSAY "| "+MOBS1
@ nLin+12, 053 PSAY "|-----------------------------------------------------------------------------|"
@ nLin+13, 000 PSAY "| "+MOBS2
@ nLin+13, 053 PSAY "| Total Geral :                      "+transform(xTOT_FAT,"@E@Z 999,999,999.99")
@ nLin+13, 131 PSAY "|"
@ nLin+14, 000 PSAY "| "+MOBS3
@ nLin+14, 053 PSAY "|-----------------------------------------------------------------------------|"
@ nLin+15, 000 PSAY "| "+MOBS4
@ nLin+15, 053 PSAY "|      |           Liberacao do Pedido          | Obs. do Frete: "+IIF(_tpfrete=="F","FOB","CIF")
@ nLin+15, 131 PSAY "|"
@ nLin+16, 000 PSAY "|-----------------------------------------------------------|                                        |                             |"
@ nLin+17, 000 PSAY "|     Comprador     |      Gerencia     |    Diretoria      |     ------------------------------     |                             |"
@ nLin+18, 000 PSAY "|                   |                   |                   |                                        |                             |"
@ nLin+19, 000 PSAY "|----------------------------------------------------------------------------------------------------------------------------------|"
@ nLin+20, 000 PSAY "|   NOTA: So aceitaremos a mercadoria se na sua Nota Fiscal constar o numero do nosso Pedido de Compras.                           |"
@ nLin+21, 000 PSAY "|  POR FORCA DA LEGISLACAO A EMPRESA ESTA OBRIGADA A ENVIAR O XML PARA O EMAIL NFE@GRUPOSANTANNA.COM.BR                            |"
@ nLin+22, 000 PSAY "|----------------------------------------------------------------------------------------------------------------------------------|"
@ 00, 000 PSAY Chr(027)+chr(15)
//      SetPrc(0,0)                              // (Zera o Formulario)
nLin := 0

Return
