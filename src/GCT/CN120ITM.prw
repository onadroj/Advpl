#include "rwmake.ch"
#include "topconn.ch"

/*
Ponto de entrada na gravação de cada item, no encerramento da medição, para geração do pedido de compra/venda
Neste ponto a tabela CNB (itens da planilha) está posicionada à medida que os itens são adicionados no array para geração do pedido
Tabela CNE=Itens da Mediao
Tabela CNB=Itens da planilha
Condição de relacionamento: 
CNE_CONTRA=CNB_CONTRA .AND CNE_NUMERO = CNB_NUMERO .AND. CNE_REVISA=CNB_REVISA .AND. CNE_ITEM=CNB_ITEM .AND. CNE_PRODUT=CNB_PRODUT
*/

User Function CN120ITM()
Local _nPos
Local _lVenda := !Empty(RetField("CND",4,XFILIAL("CND")+CNE->CNE_NUMMED,"CND_CLIENT"))
Private _aExp1
Private _aExp2
Private _aExp3

_aExp1 := ParamIXB[1]
_aExp2 := ParamIXB[2]
_aExp3 := ParamIXB[3]

If len(_aExp2)==0 				// Executado desde que o array dos itens do pedido contenha dados
	Return({_aExp1,_aExp2})										
Endif

If _lVenda
/*	_nPos := aScan( _aExp1, { |_aItem| _aItem[1] == "C5_EMISSAO"} )
    If aScan( _aExp1, { |_aItem| _aItem[1] == "C5_DTSAID"} ) == 0
		aAdd(_aExp1,{"C5_DTSAID",_aExp1[_nPos,2],nil})
	Endif    
*/
    If aScan( _aExp1, { |_aItem| _aItem[1] == "C5_CUSTO"} ) == 0
		aAdd(_aExp1,{"C5_CUSTO",CNB->CNB_CC,nil})
	Endif    

    If aScan( _aExp1, { |_aItem| _aItem[1] == "C5_CCUNID"} ) == 0
		aAdd(_aExp1,{"C5_CCUNID",CNB->CNB_CCUNID,nil})
	Endif    

    If aScan( _aExp1, { |_aItem| _aItem[1] == "C5_DISCSRV"} ) == 0
		aAdd(_aExp1,{"C5_DISCSRV",CND->CND_DISCSR,nil})
	Endif    

    If aScan( _aExp1, { |_aItem| _aItem[1] == "C5_ESTPRES"} ) == 0
		aAdd(_aExp1,{"C5_ESTPRES",CND->CND_ESTPRE,nil})
	Endif    

    If aScan( _aExp1, { |_aItem| _aItem[1] == "C5_MUNPRES"} ) == 0
		aAdd(_aExp1,{"C5_MUNPRES",CND->CND_MUNPRE,nil})
	Endif    

    If aScan( _aExp1, { |_aItem| _aItem[1] == "C5_DESCMUN"} ) == 0
		aAdd(_aExp1,{"C5_DESCMUN",CND->CND_DESCMU,nil})
	Endif    

    If aScan( _aExp1, { |_aItem| _aItem[1] == "C5_COMPETE"} ) == 0
		aAdd(_aExp1,{"C5_COMPETE",Substr(CND->CND_COMPET,1,2)+Substr(CND->CND_COMPET,4,4),nil})
	Endif    

	_nPos := aScan( _aExp2[len(_aExp2)], { |_aItem| _aItem[1] == "C6_TES"} )
		
	If _nPos > 0
		_aExp2[len(_aExp2)][_nPos,2] := CNB->CNB_TS
	Endif
	aAdd(_aExp2[len(_aExp2)],{"C6_CUSTO",CNB->CNB_CC,nil})
	aAdd(_aExp2[len(_aExp2)],{"C6_CCUNID",CNB->CNB_CCUNID,nil})
	aAdd(_aExp2[len(_aExp2)],{"CNB_PRODFT",CNB->CNB_PRODFT,nil})
	aAdd(_aExp2[len(_aExp2)],{"CNB_DESCFT",CNB->CNB_DESCFT,nil})

	//_AglItens()

Else
	_nPos := aScan( _aExp2[len(_aExp2)], { |_aItem| _aItem[1] == "C7_CONTA"} )
	If _nPos > 0
		_aExp2[len(_aExp2)][_nPos,2] := CNB->CNB_CONTA
	Endif

	_nPos := aScan( _aExp2[len(_aExp2)], { |_aItem| _aItem[1] == "C7_TES"} )
	If _nPos > 0
		_aExp2[len(_aExp2)][_nPos,2] := CNB->CNB_TE
	Endif
	
	_nPos := aScan( _aExp2[len(_aExp2)], { |_aItem| _aItem[1] == "C7_CC"} )
	If _nPos > 0
		_aExp2[len(_aExp2)][_nPos,2] := CNB->CNB_CC
	Endif
	
	_nPos := aScan( _aExp2[len(_aExp2)], { |_aItem| _aItem[1] == "C7_ITEMCTA"})
	If _nPos > 0
		_aExp2[len(_aExp2)][_nPos,2] := CNB->CNB_ITEMCT
	Endif

	aAdd(_aExp2[len(_aExp2)],{"C7_CCUNID",CNB->CNB_CCUNID,nil})
	aAdd(_aExp2[len(_aExp2)],{"C7_TPENT",CNB->CNB_TPENT,nil})  
	
Endif

Return({_aExp1,_aExp2})

Static Function _AglItens
Local _cMed := ""
Local _cQuery := ""
Local _aArea := GetArea()
Local _nItens := 0
Local _aItFat := {}
Local _nI := 0
Local _nX := 0
Local _nPosItem := aScan( _aExp2[1], { |_aItem| _aItem[1] == "C6_ITEM" } )
Local _nPosProd := aScan( _aExp2[1], { |_aItem| _aItem[1] == "C6_PRODUTO" } )
Local _nPosDesc := aScan( _aExp2[1], { |_aItem| _aItem[1] == "C6_DESCRI" } )
Local _nPosQtd  := aScan( _aExp2[1], { |_aItem| _aItem[1] == "C6_QTDVEN" } )
Local _nPosPUni := aScan( _aExp2[1], { |_aItem| _aItem[1] == "C6_PRUNIT" } )
Local _nPosPVen := aScan( _aExp2[1], { |_aItem| _aItem[1] == "C6_PRCVEN" } )
Local _nPosVlr  := aScan( _aExp2[1], { |_aItem| _aItem[1] == "C6_VALOR" } )
Local _nPosUm   := aScan( _aExp2[1], { |_aItem| _aItem[1] == "C6_UM" } )
Local _nPosLoc  := aScan( _aExp2[1], { |_aItem| _aItem[1] == "C6_LOCAL" } )
Local _nPosDesc1:= aScan( _aExp2[1], { |_aItem| _aItem[1] == "C6_DESCRI" }, _nPosDesc+1 ) 
Local _nPosItMed:= aScan( _aExp2[1], { |_aItem| _aItem[1] == "C6_ITEMED" } )
Local _nPosVlDsc:= aScan( _aExp2[1], { |_aItem| _aItem[1] == "C6_VALDESC" } )
Local _nPosProFt:= aScan( _aExp2[1], { |_aItem| _aItem[1] == "CNB_PRODFT" } )
Local _nPosDscFt:= aScan( _aExp2[1], { |_aItem| _aItem[1] == "CNB_DESCFT" } )
Local _nPosIt := 0
Local _nItemPed := 1

_cMed := _aExp1[ aScan( _aExp1, { |_aItem| _aItem[1] == "C5_MDNUMED" } ), 2 ] 

_cQuery := "SELECT COUNT(CNE_ITEM) NITENS FROM " + RetSqlName("CNE") + " WHERE CNE_NUMMED='"+_cMed+"' AND D_E_L_E_T_ <> '*'"
TCQUERY _cQuery NEW ALIAS "QRY"
DbSelectArea("QRY")
DbGoTop()
If !Eof()
	_nItens := QRY->NITENS
Endif
CLOSE

If len(_aExp2)==_nItens
	For _nI := 1 To Len(_aExp2)
		//Item que tem preenchido o produto para aglutinação
		If !Empty(_aExp2[_nI][_nPosProFt][2])
			_nPosIt := aScan(_aItFat, { |_aItem| _aItem[_nPosProd][2] == _aExp2[_nI][_nPosProFt][2] } )
			If _nPosIt==0
				aAdd(_aItFat,_aExp2[_nI])
	
				_aItFat[len(_aItFat)][_nPosItem][2] := StrZero(_nItemPed,2)
				_aItFat[len(_aItFat)][_nPosProd][2] := _aItFat[len(_aItFat)][_nPosProFt][2]
				_aItFat[len(_aItFat)][_nPosDesc][2] := _aItFat[len(_aItFat)][_nPosDscFt][2]
				_aItFat[len(_aItFat)][_nPosQtd][2] := 1
				_aItFat[len(_aItFat)][_nPosUm][2] := Alltrim(RetField("SB1",1,xFilial("SB1")+_aItFat[len(_aItFat)][_nPosProFt][2],"B1_UM"))
				_aItFat[len(_aItFat)][_nPosLoc][2] := Alltrim(RetField("SB1",1,xFilial("SB1")+_aItFat[len(_aItFat)][_nPosProFt][2],"B1_LOCPAD"))
/*
				alert("len(_aItFat) :"+CVALTOCHAR(len(_aItFat)))
				alert("_nPosDesc1 :"+CVALTOCHAR(_nPosDesc1))
				alert("_nPosDesc :"+CVALTOCHAR(_nPosDesc))  

				_aItFat[len(_aItFat)][_nPosDesc1][2] := _aItFat[len(_aItFat)][_nPosDesc][2]
*/
				_aItFat[len(_aItFat)][_nPosItMed][2] := "001"
				_aItFat[len(_aItFat)][_nPosVlDsc][2] := 0
				_aItFat[len(_aItFat)][_nPosPUni][2] := _aItFat[len(_aItFat)][_nPosVlr][2]
				_aItFat[len(_aItFat)][_nPosPVen][2] := _aItFat[len(_aItFat)][_nPosVlr][2]
	
				_nItemPed++
			Else
				_aItFat[_nPosIt][_nPosVlr][2]  += _aExp2[_nI][_nPosVlr][2]
				_aItFat[_nPosIt][_nPosPUni][2] := _aItFat[_nPosIt][_nPosVlr][2]
				_aItFat[_nPosIt][_nPosPVen][2] := _aItFat[_nPosIt][_nPosVlr][2]
			Endif
		//Item que não tem preenchido o produto para aglutinação
		Else
			aAdd(_aItFat,_aExp2[_nI])
		Endif
	Next

	_aExp2 := {}
	For _nI := 1 To Len(_aItFat)
		aAdd(_aExp2,{})
		For _nX := 1 to len(_aItFat[_nI])-2
			aAdd(_aExp2[len(_aExp2)],_aItFat[_nI][_nX])
		Next
	Next

Endif

RestArea(_aArea)
Return()