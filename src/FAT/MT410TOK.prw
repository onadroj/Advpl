#INCLUDE "protheus.ch"

User Function MT410TOK()
Local _nTipo := PARAMIXB[1] //2-Visualização, 3-Inclusão, 4-Alteração
Local _lRet := .T.
Local _nLin := 0
Local _nPosProd := aScan(aHeader, { |aItem| AllTrim(aItem[2])=="C6_PRODUTO" })
Local _cCntObr := ""

If FUNNAME()=="MATA410" .AND. _nTipo == 3 .AND. !(M->C5_TIPO$"DB") .AND. _TesTit()
	_cCntObr := RetField("SA1",1,xFilial("SA1")+M->C5_CLIENTE+M->C5_LOJACLI,"A1_CNTOBR")
	If _cCntObr=="1"
		Aviso("Contrato","Para este cliente a utilização de contrato obrigatória. O pedido de venda deve ser originado em medição de contrato.",{"OK"},3)
		_lRet := .F.
	Else
		For _nLin := 1 To Len(aCols)
			If !aCols[_nLin][Len(aCols[_nLin])]	
				_cCntObr := RetField("SB1",1,xFilial("SB1")+aCols[_nLin][_nPosProd],"B1_CNTOBR")
				If _cCntObr=="1"
					Aviso("Contrato","Há produto(s) com a utilização de contrato obrigatória. O pedido de venda deve ser originado em medição de contrato.",{"OK"},3)
					_lRet := .F.
					Exit
				Endif
			Endif
		Next
	Endif
Endif

Return(_lRet)

Static Function _TesTit()
Local _lRet := .F.
Local _X := 1
Local _nPosTes := aScan(aHeader,{ |_aItem| AllTrim(_aItem[2])=="C6_TES" } )
Local _cDuplic := ""

For _X := 1 to Len(aCols)
	If !aCols[_X][Len(aCols[_X])]
		_cDuplic := RetField("SF4",1,xFilial("SF4")+aCols[_X][_nPosTes],"F4_DUPLIC")
		If _cDuplic=="S"
			_lRet := .T.
			Exit
		Endif
	Endif
Next
Return(_lRet)