#INCLUDE "rwmake.ch"
#INCLUDE "Topconn.ch"

/*
Verifica e alerta usuário quanto a existência de contrato para o fornecedor
Para utilização na validação do campo do fornecedor e loja, em SF1 e SE2
*/

User Function VALCNTFORN()

Local _cCnt:=""
Local _aArea:=GetArea()
Local _cFrn:=""
Local _cMsg:=""
Local _cTipo:=""
Local _lRet:=.T.
Local _cCntObr:=""    
Local _cLoja:=""
Local _cProd:=""

If cEmpAnt <> "03" .AND. cEmpAnt <> "99" // Executado apenas para a Construtora Sant'Anna ou empresa Teste 
//If cEmpAnt == "05" .OR. cEmpAnt == "07"
	Return(.T.)										
Endif


If FUNNAME()=="MATA103" .AND. (__READVAR=="cA100For" .OR. __READVAR=="cLoja") 
//	LNFMEDIC := .F.
	_cFrn:=cA100For	
	_cLoja:=cLoja
	_cTipo:=cTipo
ElseIf FUNNAME()=="MATA103" .AND. __READVAR=="M->D1_COD"
//	LNFMEDIC := .F.
	_cFrn:=cA100For	
	_cLoja:=cLoja
	_cTipo:=cTipo
	_cProd:=M->D1_COD
ElseIf FUNNAME()=="FINA050" .AND. (__READVAR=="M->E2_FORNECE" .OR. __READVAR=="M->E2_LOJA")
	_cFrn:=M->E2_FORNECE
	_cLoja:=M->E2_LOJA	
EndIf       

If !Empty(_cFrn) .AND. !Empty(_cLoja) .AND. !(_cTipo$"DB") .AND. __READVAR<>"cA100For"
    _cCntObr := RetField("SA2",1,xFilial("SA2")+_cFrn+_cLoja,"A2_CNTOBR")
	If _cCntObr == "1" .AND. /*(__READVAR=="M->E2_FORNECE" .OR.*/ __READVAR=="M->E2_LOJA"/*)*/
		_cMsg := "Para este fornecedor é exigida utilização de contrato, com pedido de compra gerado por medição." +chr(13)+chr(10) ;
		        +"A entrada de documentos deve ser feita pela rotina Documento de Entrada no módulo Compras."
		_lRet := .F.
	Elseif _cCntObr == "1" .AND. /*(__READVAR=="cA100For" .OR.*/ __READVAR=="cLoja"/*)*/
		_cMsg := "Para este fornecedor é exigida utilização de contrato."+chr(13)+chr(10) ;
		        +"O campo Filtra Medição deve permanecer marcado e os itens deste documento " +chr(13)+chr(10) ;
		        +"devem ser associados a pedidos de compra gerados por medições."
		LNFMEDIC := .T.
	EndIf
Endif
If _lRet .AND. !Empty(_cFrn) .AND. !(_cTipo$"DB")
	_cCnt:=BSCCNT(_cFrn)
	If !Empty(_cCnt)
		If FUNNAME()=="FINA050" .AND. /*(*/__READVAR=="M->E2_FORNECE" /*.OR. __READVAR=="M->E2_LOJA")*/
			_cMsg := "Este fornecedor possui registro(s) no módulo de contratos: "+_cCnt+"."+chr(10)+chr(13) ;
					+"Verifique a existência/necessidade de medições!"+chr(10)+chr(13) ; 
					+"Caso sim, deve ser utilizada a rotina Documento de Entrada, no módulo Compras."
		ElseIf FUNNAME()=="MATA103" .AND. /*(*/__READVAR=="cA100For" /*.OR. __READVAR=="cLoja")*/
			_cMsg := "Este fornecedor possui registro(s) no módulo de contratos: "+_cCnt+"."+chr(10)+chr(13)  ;
					+"Verifique a existência/necessidade de medições!"+chr(10)+chr(13)  ;
					+"Caso o documento não seja referente a contrato, desmarque a caixa Filtra Medição."
			LNFMEDIC := .T.
		Endif
    Endif
EndIf                 

If _lRet .AND. !Empty(_cProd) .AND. !(_cTipo$"DB")
    _cCntObr := RetField("SB1",1,xFilial("SB1")+_cProd,"B1_CNTOBR")
	If FUNNAME()=="MATA103" .AND. _cCntObr == "1" .AND. !LNFMEDIC
		_cMsg := "Para este produto é exigida utilização de contrato."+chr(13)+chr(10) ;
		        +"O campo Filtra Medição deve permanecer marcado e os itens deste documento " +chr(13)+chr(10) ;
		        +"devem ser associados a pedidos de compra gerados por medições."
		LNFMEDIC := .T.
	Endif
Endif

If !Empty(_cMsg)
//	MsgStop(_cMsg)
	Aviso("Contrato",_cMsg,{"OK"},3)
Endif
restarea(_aArea)
Return(_lRet)


Static Function BSCCNT(_cFornece)
Local _cRet:=""
LOCAL _AAREA:=GETAREA()
Local _cQuery:=""

_cQuery :="SELECT CNA_CONTRA "
_cQuery += " FROM "+RETSQLNAME("CNA")+" CNA "
_cQuery += " WHERE "
_cQuery += "   CNA_FORNEC='"+_cFornece+"' "
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