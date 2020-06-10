#INCLUDE "PROTHEUS.CH"
#INCLUDE "TOPCONN.CH"

//Ponto de entrada após a gravação da alteração da situação do contrato.
//Utilizado para geração do evento M-Messenger de alteração de situação (_SC).

/*
Situações:
'01' Cancelado
'02' Em Elaboração
'03' Emitido
'04' Em Aprovação
'05' Vigente
'06' Paralisado
'07' Sol Fina.
'08' Finalizado
'09' Revisão  
'10' Revisado
*/

User Function CN100SIT()
Local _cSitAtual := PARAMIXB[1]
Local _cSitNova  := PARAMIXB[2]
Local _aArea := GetArea()
Local _cFornec := ""
Local _cTpCont := RetField("CN1",1,xFilial("CN1")+CN9->CN9_TPCTO,"CN1_ESPCTR") //1-Compra;2-Venda
Local _aSit := {}
Local _aDados := {}
Local _cJustif := ""


If cEmpAnt<>"03" .AND. cEmpAnt<>"99" //Executado apenas para Construtora e empresa Teste
	Return(Nil)
Endif


aAdd(_aSit,"Cancelado")
aAdd(_aSit,"Em Elaboração")
aAdd(_aSit,"Emitido")
aAdd(_aSit,"Em Aprovação")
aAdd(_aSit,"Vigente")
aAdd(_aSit,"Paralisado")
aAdd(_aSit,"Sol Fina.")
aAdd(_aSit,"Finalizado")
aAdd(_aSit,"Revisão")
aAdd(_aSit,"Revisado")

If _cTpCont=="1"
	DbSelectArea("CNC")
	DbSetOrder(1) //CNC_FILIAL+CNC_NUMERO+CNC_REVISA+CNC_CODIGO+CNC_LOJA
	DbSeek(xFilial("CNC")+CN9->CN9_NUMERO+CN9->CN9_REVISA)
	Do While !Eof() .AND. xFilial("CNC")+CNC->CNC_NUMERO+CNC->CNC_REVISA==xFilial("CNC")+CN9->CN9_NUMERO+CN9->CN9_REVISA
	    If Empty(_cFornec)
	    	_cFornec := CNC->CNC_CODIGO+"/"+CNC->CNC_LOJA+" "+Alltrim(RetField("SA2",1,xFilial("SA2")+CNC->CNC_CODIGO+CNC->CNC_LOJA,"A2_NOME"))
	    Else
	    	_cFornec += " - "+CNC->CNC_CODIGO+"/"+CNC->CNC_LOJA+" "+Alltrim(RetField("SA2",1,xFilial("SA2")+CNC->CNC_CODIGO+CNC->CNC_LOJA,"A2_NOME"))
	    Endif
		DbSkip()
	EndDo
Endif

aAdd(_aDados,CN9->CN9_NUMERO)
aAdd(_aDados,CN9->CN9_REVISA)
aAdd(_aDados,Alltrim(RetField("CN1",1,xFilial("CN1")+CN9->CN9_TPCTO,"CN1_DESCRI")))
aAdd(_aDados,_cTpCont)
aAdd(_aDados,AllTrim(MSMM(CN9->CN9_CODOBJ)))
aAdd(_aDados,IIf(_cTpCont=="1",_cFornec,CN9->CN9_CLIENT+"/"+CN9->CN9_LOJACL+" "+Alltrim(RetField("SA1",1,xFilial("SA1")+CN9->CN9_CLIENT+CN9->CN9_LOJACL,"A1_NOME"))))
aAdd(_aDados,_aSit[val(_cSitAtual)])
aAdd(_aDados,_aSit[val(_cSitNova)])
aAdd(_aDados,cUserName)
aAdd(_aDados,Resps(CN9->CN9_NUMERO))
If val(_cSitNova) < val(_cSitAtual)
	_cJustif := Justifica()
Endif
aAdd(_aDados,_cJustif)
MEnviaMail("_SC",_aDados,,,,.T.)


RestArea(_aArea)

Return(Nil)

Static Function Resps(_cContrato)
Local _cResps := ""
Local _aArea := GetArea()
Local _cQuery := ""
Local _aArray := {}

_cQuery := "SELECT CNN_USRCOD FROM "+RetSqlName("CNN")+" WHERE CNN_FILIAL='"+xFilial("CNN")+"' "
_cQuery += "AND CNN_CONTRA='"+_cContrato+"' AND D_E_L_E_T_<>'*' "
_cQuery += "GROUP BY CNN_USRCOD"

TCQUERY _cQuery NEW ALIAS "QRY"
DbSelectArea("QRY")
DbGoTop()
Do While !Eof() 
	PswOrder(2)
	If PswSeek(QRY->CNN_USRCOD,.T.)  
		_aArray := PswRet()
		If Empty(_cResps)
			_cResps := Alltrim(_aArray[1][4])
		Else
			_cResps += " / "+Alltrim(_aArray[1][4])
		Endif
	EndIf
	DbSkip()
EndDo
CLOSE

RestArea(_aArea)
Return(_cResps)

Static Function Justifica()
Local _aPergs := {}
Local _aRet := {}
Local _lOk := .F.

aAdd(_aPergs,{11,"Justificativa para retorno de situação","","","",.T.})
   
Do While !_lOk
	If ParamBox(_aPergs ,"Parametros ",_aRet,,,.T.,,,,,.F.,.F.)
		_lOk := .T.
	EndIf
EndDo
Return(_aRet[1])