#include "rwmake.ch"
#include "topconn.ch"

/*
- Rotina para tratamento de títulos provisórios (listagem e exclusão).
- Para utilização via validação de usuário do campo E2_FORNECE (EXECBLOCK("BUSCAPROV",.F.,.F.))
- Na inclusão manual de Contas a Pagar, após digitar o código do fornecedor, busca títulos provisórios e, caso encontre, lista os registros encontrados, 
independente da loja do fornecedor
- Permite a seleção e exclusão de títulos encontrados, alertando o usuário para utilização da rotina de substituição do sistema caso utilize a contabilização 
de títulos provisórios.
*/

User Function BUSCAPROV()
Local _cQuery := ""
Local _aArea:=GetArea()
//Local _lOk
Local _aDbf
Local _aTrb

Private aRotina := {{"Excluir    " ,"ExcPR", 0, 1}}
Private aAC     := {"Abandona","Confirma"}
Private _cArqTrab
Private _oDlg4
Private _nOpca
Private _cMarca

If cEmpAnt=="03" .OR. cEmpAnt=="04" .OR. cEmpAnt=="06" .OR. cEmpAnt=="08"  .OR. cEmpAnt=="99"
		_cQuery := "SELECT E2_PREFIXO, E2_NUM, E2_PARCELA, E2_EMISSAO, E2_FORNECE, E2_LOJA, E2_VENCREA, E2_VALOR, E2_SALDO, E2_MDCONTR, E2_MDREVIS "
		_cQuery += "FROM " + RetSqlName("SE2") + " WHERE "
		_cQuery += "E2_FILIAL = '" + XFILIAL("SE2") + "' AND E2_FORNECE = '" + M->E2_FORNECE + "' AND E2_TIPO = 'PR ' "
		_cQuery += "AND E2_SALDO > 0 AND D_E_L_E_T_ <> '*' "
		_cQuery += "ORDER BY E2_VENCREA, E2_NUM, E2_PARCELA"
		
		TCQUERY _cQuery NEW ALIAS QRY

		DbSelectArea("QRY")
		If Eof()
			Close
			RestArea(_aArea)
			Return(.T.)	
		Endif

		MsgBox("Este fornecedor possui títulos provisórios, neste momento será possível efetuar a exclusão." +chr(13) + chr(10) ; 
		     + "ATENÇÃO: Caso os títulos provisórios sejam contabilizados, utilize a rotina de substituição.","BUSCAPROV")

		_cMarca := GetMark()
		
		_aDbf := {}
		AADD(_aDbf,{ "E2_OK"      , "C" , 2, 0 })
		AADD(_aDbf,{ "E2_PREFIXO" , "C" , TAMSX3("E2_PREFIXO")[1], TAMSX3("E2_PREFIXO")[2] })
		AADD(_aDbf,{ "E2_NUM"     , "C" , TAMSX3("E2_NUM")[1], TAMSX3("E2_NUM")[2] })
		AADD(_aDbf,{ "E2_PARCELA" , "C" , TAMSX3("E2_PARCELA")[1], TAMSX3("E2_PARCELA")[2] })
		AADD(_aDbf,{ "E2_EMISSAO" , "D" , TAMSX3("E2_EMISSAO")[1], TAMSX3("E2_EMISSAO")[2] })
		AADD(_aDbf,{ "E2_FORNECE" , "C" , TAMSX3("E2_FORNECE")[1], TAMSX3("E2_FORNECE")[2] })
		AADD(_aDbf,{ "E2_LOJA"    , "C" , TAMSX3("E2_LOJA")[1], TAMSX3("E2_LOJA")[2] })
		AADD(_aDbf,{ "E2_VENCREA" , "D" , TAMSX3("E2_VENCREA")[1], TAMSX3("E2_VENCREA")[2] })
		AADD(_aDbf,{ "E2_VALOR" , "N" , TAMSX3("E2_VALOR")[1], TAMSX3("E2_VALOR")[2] ,})
		AADD(_aDbf,{ "E2_SALDO" , "N" , TAMSX3("E2_SALDO")[1], TAMSX3("E2_SALDO")[2] })
		AADD(_aDbf,{ "E2_MDCONTR" , "C" , TAMSX3("E2_MDCONTR")[1], TAMSX3("E2_MDCONTR")[2] ,})
		AADD(_aDbf,{ "E2_MDREVIS" , "C" , TAMSX3("E2_MDREVIS")[1], TAMSX3("E2_MDREVIS")[2] })
		
		_aTrb := {}
		
		AADD(_aTrb,{"E2_OK"      ,NIL," "    ,})
		AADD(_aTrb,{"E2_PREFIXO" ,NIL,"Prefixo",})
		AADD(_aTrb,{"E2_NUM"     ,NIL,"Documento",})
		AADD(_aTrb,{"E2_PARCELA" ,NIL,"Parcela",})
		AADD(_aTrb,{"E2_EMISSAO" , NIL,"Emissao",})
		AADD(_aTrb,{"E2_FORNECE" , NIL,"Fornecedor",})
		AADD(_aTrb,{"E2_LOJA"    , NIL,"Loja"})
		AADD(_aTrb,{"E2_VENCREA" , NIL,"Vencto. Real",})
		AADD(_aTrb,{"E2_VALOR"    , NIL,"Valor","@E 999,999,999.99"})
		AADD(_aTrb,{"E2_SALDO"    , NIL,"Saldo","@E 999,999,999.99"})
		AADD(_aTrb,{"E2_MDCONTR"    , NIL,"Contrato",})
		AADD(_aTrb,{"E2_MDREVIS"    , NIL,"Revisao",})
		
		oTempTable := FWTemporaryTable():New(cAlias)
		oTempTable:SetFields(_aDbf)
		oTempTable:Create()
				
		//_cArqTrab := CriaTrab(_aDbf)
		//Use (_cArqTrab) NEW SHARED Alias TRB25
		
		DbSelectArea("QRY")
		While !Eof()
			dbSelectArea(oTempTable)
			oTempTable->(DbAppend())
			oTempTable->E2_OK      := " "
			oTempTable->E2_PREFIXO  := QRY->E2_PREFIXO
			oTempTable->E2_NUM  := QRY->E2_NUM
			oTempTable->E2_PARCELA  := QRY->E2_PARCELA
			oTempTable->E2_EMISSAO  := STOD(QRY->E2_EMISSAO)
			oTempTable->E2_FORNECE  := QRY->E2_FORNECE
			oTempTable->E2_LOJA  := QRY->E2_LOJA
			oTempTable->E2_VENCREA  := STOD(QRY->E2_VENCREA)
			oTempTable->E2_VALOR  := QRY->E2_VALOR
			oTempTable->E2_SALDO  := QRY->E2_SALDO
			oTempTable->E2_MDCONTR  := QRY->E2_MDCONTR
			oTempTable->E2_MDREVIS  := QRY->E2_MDREVIS
			DbSelectArea("QRY")
			DbSkip()
		EndDo
		Close
		dbSelectArea(oTempTable)
		DbGoTop()
		
		_nOpca   := 1
		
		DEFINE MSDIALOG _oDlg4 TITLE "Exclusão de Títulos Provisórios" From 6.5,0 To 35,72 OF oMainWnd
		
		@ 20,30 Say OemToAnsi ("Selecione os títulos a excluir") SIZE 150,20
		oMark := MsSelect():New("oTempTable","E2_OK",,_aTrb,,@_cMarca,{35,1,200,280})
		oMark:oBrowse:lhasMark = .F.
		oMark:oBrowse:lCanAllmark := .t.
		
		ACTIVATE MSDIALOG _oDlg4 ON INIT EnchoiceBar(_oDlg4,{||_nOpca:=2,_oDlg4:End()},{||_nOpca:=1,_oDlg4:End()}) CENTERED
		
		If _nOpca == 2      //OK
			Processa({|lEnd| EXCLUEPR()}, "Excluindo Provisórios")
			Close
			fErase(_cArqTrab)
			MsgBox("Títulos excluídos.","BUSCAPROV")
		ENDIF
		
		If _nOpca == 1  //CANCELAR
			Close
			fErase(_cArqTrab)
		EndIf
		RestArea(_aArea)
ENDIF
Return(.T.)

Static Function EXCLUEPR()
DbSelectArea("TRB25")
DbGoTop()
While !EOF()
	IF TRB25->E2_OK==_cMarca
		DBSELECTAREA("SE2")
		DBSETORDER(1)   //E2_FILIAL+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA
		IF DBSEEK(XFILIAL("SE2")+TRB25->E2_PREFIXO+TRB25->E2_NUM+TRB25->E2_PARCELA+"PR "+TRB25->E2_FORNECE+TRB25->E2_LOJA)
			RecLock("SE2",.F.)
			dbDelete()
			MsUnlock()
		ENDIF
	ENDIF
	dbSelectArea("TRB25")
	dbSkip()
EndDo
Return()
