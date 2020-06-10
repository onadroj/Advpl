#INCLUDE "PROTHEUS.CH"
#INCLUDE "TOPCONN.CH"
/*
Busca dados das notas fiscais relacionadas ao Pedido de Compra/Venda gerado pela medição
Para utilização com campo virtual a ser exibido no browse de medições
*/

User Function NFMED()
Local _cNFs := ""
Local _cQuery := ""
Local _aArea := GetArea()

If (cEmpAnt<>"03" .AND. cEmpAnt<>"99") .OR. Empty(CND->CND_PEDIDO)
//If (cEmpAnt == "05" .OR. cEmpAnt == "07") .OR. Empty(CND->CND_PEDIDO)
	Return(" ")
Endif


If !Empty(CND->CND_FORNEC)
	_cQuery := "SELECT D1_DOC, D1_SERIE, D1_EMISSAO FROM "+RetSqlName("SD1")+" WHERE "
	_cQuery += "D1_FORNECE = '"+CND->CND_FORNEC+"' AND D1_LOJA = '"+CND->CND_LJFORN+"' AND D1_PEDIDO = '"+CND->CND_PEDIDO+"' "
	_cQuery += "AND D_E_L_E_T_ <> '*' "
	_cQuery += "GROUP BY D1_DOC, D1_SERIE, D1_EMISSAO "
	_cQuery += "ORDER BY D1_DOC, D1_SERIE, D1_EMISSAO"
	
	TCQUERY _cQuery NEW ALIAS "QRY"
	DbSelectArea("QRY")
	DbGoTop()
	While !Eof()
		If Empty(_cNFs)
			_cNFs := QRY->D1_DOC+"/"+QRY->D1_SERIE+" "+DTOC(STOD(QRY->D1_EMISSAO))
		Else
			_cNFs += " - "+QRY->D1_DOC+"/"+QRY->D1_SERIE+" "+DTOC(STOD(QRY->D1_EMISSAO))
		Endif
		DbSkip()
	End
	CLOSE
Else
	_cQuery := "SELECT D2_DOC, D2_SERIE, D2_EMISSAO FROM "+RetSqlName("SD2")+" WHERE "
	_cQuery += "D2_CLIENTE = '"+CND->CND_CLIENT+"' AND D2_LOJA = '"+CND->CND_LOJACL+"' AND D2_PEDIDO = '"+CND->CND_PEDIDO+"' "
	_cQuery += "AND D_E_L_E_T_ <> '*' "
	_cQuery += "GROUP BY D2_DOC, D2_SERIE, D2_EMISSAO "
	_cQuery += "ORDER BY D2_DOC, D2_SERIE, D2_EMISSAO"
	
	TCQUERY _cQuery NEW ALIAS "QRY"
	DbSelectArea("QRY")
	DbGoTop()
	While !Eof()
		If Empty(_cNFs)
			_cNFs := QRY->D2_DOC+"/"+QRY->D2_SERIE+" "+DTOC(STOD(QRY->D2_EMISSAO))
		Else
			_cNFs += " - "+QRY->D2_DOC+"/"+QRY->D2_SERIE+" "+DTOC(STOD(QRY->D2_EMISSAO))
		Endif
		DbSkip()
	End
	CLOSE
Endif

RestArea(_aArea)
Return(_cNFs)