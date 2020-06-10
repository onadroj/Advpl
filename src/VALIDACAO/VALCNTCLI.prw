#INCLUDE "rwmake.ch"
#INCLUDE "Topconn.ch"

/*
Verifica e alerta usuário quanto a existência ou obrigatoriedade de contrato para o cliente/produto
*/

User Function VALCNTCLI()
Local _cCnt:=""
Local _aArea:=GetArea()
Local _cCli:=""
Local _cMsg:=""
Local _cTipo:=""
Local _lRet:=.T.
Local _cCntObr:=""    
Local _cLoja:=""
Local _cProd:=""

If cEmpAnt <> "03" .AND. cEmpAnt <> "99" // Executado apenas para a Construtora Sant'Anna ou empresa Teste 
	Return(.T.)										
Endif

If FUNNAME()=="MATA410" .AND. (__READVAR=="M->C5_CLIENTE" .OR. __READVAR=="M->C5_LOJACLI") 
	_cCli:=M->C5_CLIENTE	
	_cLoja:=M->C5_LOJACLI
	_cTipo:=M->C5_TIPO
ElseIf FUNNAME()=="MATA410" .AND. __READVAR=="M->C6_PRODUTO"
	_cCli:=M->C5_CLIENTE	
	_cLoja:=M->C5_LOJACLI
	_cTipo:=M->C5_TIPO
	_cProd:=M->C6_PRODUTO
ElseIf FUNNAME()=="FINA040" .AND. (__READVAR=="M->E1_CLIENTE" .OR. __READVAR=="M->E1_LOJA")
	_cCli:=M->E1_CLIENTE
	_cLoja:=M->E1_LOJA	
EndIf       

If !Empty(_cCli) .AND. !Empty(_cLoja) .AND. !(_cTipo$"DB") .AND. (__READVAR=="M->C5_CLIENTE" .OR. __READVAR=="M->C5_LOJACLI" .OR. __READVAR=="M->E1_CLIENTE" .OR. __READVAR=="M->E1_LOJA") 
    _cCntObr := RetField("SA1",1,xFilial("SA1")+_cCli+_cLoja,"A1_CNTOBR")
	If _cCntObr == "1" .AND. (__READVAR=="M->E1_CLIENTE" .OR. __READVAR=="M->E1_LOJA") 
		_cMsg := "Para este cliente é exigida utilização de contrato, com pedido de venda gerado por medição." +chr(13)+chr(10) ;
		        +"A geração de contas a receber deve ser feita a partir de documentos de saída no módulo Faturamento."
		_lRet := .F.
	Elseif _cCntObr == "1" .AND. (__READVAR=="M->C5_CLIENTE" .OR. __READVAR=="M->C5_LOJACLI")
		_cMsg := "Para este cliente é exigida utilização de contrato."+chr(13)+chr(10) ;
		        +"O pedido de venda somente poderá ser gravado se não gerar financeiro,"+chr(13)+chr(10) ;
		        +"caso contrário deverá ser gerado a partir de medição."
//		_lRet := .F. 
	EndIf
Endif
If _lRet .AND. !Empty(_cCli) .AND. !(_cTipo$"DB")
	_cCnt:=BSCCNT(_cCli)
	If !Empty(_cCnt)
		If FUNNAME()=="FINA040" .AND. (__READVAR=="M->E1_CLIENTE" .OR. __READVAR=="M->E1_LOJA") 
			_cMsg := "Este cliente possui registro(s) no módulo de contratos: "+_cCnt+"."+chr(10)+chr(13) ;
					+"Verifique a existência/necessidade de medições!"+chr(10)+chr(13) ; 
					+"Caso sim, deve ser feita a geração de Documento de Saída, no módulo Faturamento."
		ElseIf FUNNAME()=="MATA410" .AND. (__READVAR=="M->C5_CLIENTE" .OR. __READVAR=="M->C5_LOJACLI")
			_cMsg := "Este cliente possui registro(s) no módulo de contratos: "+_cCnt+"."+chr(10)+chr(13)  ;
					+"Verifique a existência/necessidade de medições!"
		Endif
    Endif
EndIf                 

If INCLUI .AND. _lRet .AND. !Empty(_cProd) .AND. !(_cTipo$"DB")
    _cCntObr := RetField("SB1",1,xFilial("SB1")+_cProd,"B1_CNTOBR")
	If FUNNAME()=="MATA410" .AND. _cCntObr == "1"
		_cMsg := "Para este produto é exigida utilização de contrato."+chr(13)+chr(10) ;
		        +"O pedido de venda somente poderá ser gravado se não gerar financeiro,"+chr(13)+chr(10) ;
		        +"caso contrário deverá ser gerado a partir de medição."
//		_lRet := .F.
	Endif
Endif

If !Empty(_cMsg)
//	MsgStop(_cMsg)
	Aviso("Contrato",_cMsg,{"OK"},3)
Endif
restarea(_aArea)
Return(_lRet)


Static Function BSCCNT(_cCliente)
Local _cRet:=""
LOCAL _AAREA:=GETAREA()
Local _cQuery :=""

_cQuery :="SELECT CNA_CONTRA "
_cQuery += " FROM "+RETSQLNAME("CNA")+" CNA "
_cQuery += " WHERE "
_cQuery += "   CNA_CLIENT='"+_cCliente+"' "
_cQuery += "   AND CNA.D_E_L_E_T_ <>'*' "
_cQuery += "   GROUP BY CNA_CONTRA"
_cQuery += "   ORDER BY CNA_CONTRA"

TCQUERY _cQuery NEW ALIAS "QRY"   
dbSelectArea("QRY")
DBGOTOP()
While !EOF()
	_cRet+=QRY->CNA_CONTRA+" / "
	DbSkip()
End
CLOSE 

If !Empty(_cRet)
	_cRet:=Substr(_cRet,1,Len(_cRet)-3)
Endif
RETURN(_cRet)