#INCLUDE "rwmake.ch"

// Ponto de entrada na rotina de pedidos de venda MATA410()
// Ocorre na alteração, inclusão, exclusão e devolução de compras, após as gravações nos arquivos.

User Function M410STTS
Local aDados:={}
Local _cTipo := ""
Local _cCntObr := ""
Local _aArea := GetArea()
Local _nPosProd := aScan(aHeader,{ |aItem| AllTrim(aItem[2])=="C6_PRODUTO" })
Local _nLin := 0
Local _lGerTit := .F.

IF INCLUI .AND. (cEmpAnt=="03" .OR. cEmpAnt=="04" .OR. cEmpAnt=="08" .OR. cEmpAnt=="06" .OR. cEmpAnt=="99")
	AADD(aDados,"Inclusão")
	AADD(aDados,M->C5_NUM)
	Do Case
		Case M->C5_TIPO=="N"
			_cTipo := "N - Normal"
		Case M->C5_TIPO=="C"
			_cTipo := "C - Compl.Precos"
		Case M->C5_TIPO=="I"
			_cTipo := "I - Compl.ICMS"
		Case M->C5_TIPO=="P"
			_cTipo := "P - Compl.IPI"
		Case M->C5_TIPO=="D"
			_cTipo := "D - Dev.Compras"
		Case M->C5_TIPO=="B"
			_cTipo := "B - Utiliza Fornecedor"
	End
	AADD(aDados,_cTipo)
    If M->C5_TIPO $ "DB"
		AADD(aDados,M->C5_CLIENTE+"/"+M->C5_LOJACLI+" - "+AllTrim(RetField("SA2",1,xFilial("SA2")+M->C5_CLIENTE+M->C5_LOJACLI,"A2_NOME")))
 	Else
		AADD(aDados,M->C5_CLIENTE+"/"+M->C5_LOJACLI+" - "+AllTrim(RetField("SA1",1,xFilial("SA1")+M->C5_CLIENTE+M->C5_LOJACLI,"A1_NOME")))
	Endif
	AADD(aDados,cUserName)
	AADD(aDados,M->C5_CUSTO+" - "+AllTrim(RetField("CTT",1,xFilial("CTT")+M->C5_CUSTO,"CTT_DESC01")))
	_lGerTit := _GerTit(M->C5_NUM)
	If _lGerTit
		AADD(aDados,"Gerou títulos")
	Else
		AADD(aDados,"Não gerou títulos")
	Endif
	MEnviaMail("_PV",aDados,,,,.T.)


	//Dispara MMessenger de contrato desejável no cliente ou produto
	If !(M->C5_TIPO $ "DB") .AND. _lGerTit
		_cCntObr := RetField("SA1",1,xFilial("SA1")+M->C5_CLIENTE+M->C5_LOJACLI,"A1_CNTOBR")
		aDados := {}
		If _cCntObr=="3"
			aAdd(aDados,M->C5_CLIENTE)
			aAdd(aDados,M->C5_LOJACLI)
			aAdd(aDados,RetField("SA1",1,xFilial("SA1")+M->C5_CLIENTE+M->C5_LOJACLI,"A1_NOME"))
			aAdd(aDados,M->C5_NUM)
			aAdd(aDados,cUserName)
			aAdd(aDados,"Pedido de Venda")
			aAdd(aDados,"C")
			MEnviaMail("_CO",aDados,,,,.T.)
		Else
			For _nLin := 1 To Len(aCols)
				If !aCols[_nLin][Len(aCols[_nLin])]
					_cCntObr := RetField("SB1",1,xFilial("SB1")+aCols[_nLin][_nPosProd],"B1_CNTOBR")
					If _cCntObr=="3"
						aAdd(aDados,M->C5_CLIENTE)
						aAdd(aDados,M->C5_LOJACLI)
						aAdd(aDados,RetField("SA1",1,xFilial("SA1")+M->C5_CLIENTE+M->C5_LOJACLI,"A1_NOME"))
						aAdd(aDados,M->C5_NUM)
						aAdd(aDados,cUserName)
						aAdd(aDados,"Pedido de Venda")
						aAdd(aDados,"C")
						MEnviaMail("_CO",aDados,,,,.T.)
						Exit
					Endif					
				Endif
			Next
		Endif
	Endif
ENDIF

RestArea(_aArea)
Return()

Static Function _GerTit(_cPedido)
Local _lReturn := .F.
Local _aArea := GetArea()
Local _cDup := ""

DbSelectArea("SC6")
DbSetOrder(1) //C6_FILIAL+C6_NUM+C6_ITEM+C6_PRODUTO
DbSeek(xFilial("SC6")+_cPedido,.T.)
While !Eof() .AND. xFilial("SC6")+SC6->C6_NUM==xFilial("SC6")+_cPedido
    _cDup := RetField("SF4",1,xFilial("SF4")+SC6->C6_TES,"F4_DUPLIC")
    If _cDup=="S"
		_lReturn := .T.    	
		Exit
    Endif
	DbSkip()
EndDo

RestArea(_aArea)
Return(_lReturn)