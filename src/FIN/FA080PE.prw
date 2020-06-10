#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³FA080PE   º Autor ³ GATASSE            º Data ³  02/04/03   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ PROCURA PROVISORIOS APOS A BAIXA                           º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP6 IDE                                                    º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User Function FA080PE


//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Declaracao de Variaveis                                             ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
_FORNECE:=SE2->E2_FORNECE
_LOJA:=SE2->E2_LOJA
IF MSGBOX("Deseja verificar provisorios deste fornecedor?","Confirmacao","YESNO")
	AREAE2:=GETAREA()
	DATADE:=CTOD("  /  /  ")
	DATAATE:=CTOD("  /  /  ")
	@ 96,42 TO 300,405 DIALOG oDlg TITLE "Solicitacao de Dados"
	@ 05,05 TO 74,135 TITLE "Parametros"
	@ 15,14 Say "Da Data  " size 100,50
	@ 15,50 Get Datade SIZE 40,2 Picture "@D" Valid Valdte()
	@ 25,14 Say "Ate a Data  " size 100,50
	@ 25,50 Get Dataate SIZE 40,2 Picture "@D" Valid Valdts()
	@ 10,150 BMPBUTTON TYPE 01 ACTION (lOK:=.T.,CLOSE(ODLG))
	@ 40,150 BMPBUTTON TYPE 02 ACTION (lOK:=.F.,Close(oDlg))
	ACTIVATE DIALOG oDlg CENTERED
	if !lok
		Return
	endif
	
	Private aRotina := {{"Excluir    " ,"ExcPR", 0, 1}}
	PRIVATE aAC     := {"Abandona","Confirma"}
	
	lInverte:= .f.
	cMARCA  := GetMark()
	lQuery := .t.
	
	aDBF := {}
	AADD(aDBF,{ "E2_OK"      , "C" , 2, 0 })
	AADD(aDBF,{ "E2_PREFIXO" , "C" , 3, 0 })
	AADD(aDBF,{ "E2_NUM"     , "C" , 6, 0 })
	AADD(aDBF,{ "E2_PARCELA" , "C" , 1, 0 })
	AADD(aDBF,{ "E2_EMISSAO" , "D" ,8, 0 })
	AADD(aDBF,{ "E2_FORNECE" , "C" , 6, 0 })
	AADD(aDBF,{ "E2_LOJA"    , "C" , 2, 0 })
	AADD(aDBF,{ "E2_VENCREA" , "D" ,8, 0 })
	AADD(aDBF,{ "E2_VALOR" , "N" , 12, 2 ,})
	AADD(aDBF,{ "E2_SALDO" , "N" , 12, 2 })
	
	aTRB := {}
	
	AADD(aTRB,{"E2_OK"      ,NIL," "    ,})
	AADD(aTRB,{"E2_PREFIXO" ,NIL,"Prefixo",})
	AADD(aTRB,{"E2_NUM"     ,NIL,"Documento",})
	AADD(aTRB,{"E2_PARCELA" ,NIL,"Parcela",})
	AADD(aTRB,{"E2_EMISSAO" , NIL,"Emissao",})
	AADD(aTRB,{"E2_FORNECE" , NIL,"Cliente",})
	AADD(aTRB,{"E2_LOJA"    , NIL,"Loja"})
	AADD(aTRB,{"E2_VENCREA" , NIL,"Vencimento",})
	AADD(aTRB,{"E2_VALOR"    , NIL,"Valor","@E 999,999,999.99"})
	AADD(aTRB,{"E2_SALDO"    , NIL,"Saldo","@E 999,999,999.99"})
	
	cArqTrab := CriaTrab(aDBF)
	Use (cArqTrab) NEW SHARED Alias TRB25
	DbSelectArea("SE2")
	DbOrderNickName("SE21") //FORNECE+TIPO
	DBGOTOP()
	//	_cCond   := "SE2->E2_FORNECE = '"+_FORNECE+"' .AND. SE2->E2_LOJA = '"+_LOJA+"' .AND. SE2->E2_TIPO='PR' .AND. SE2->E2_SALDO>0"
	//	SET FILTER TO &_cCond
	DbSeek(xFilial("SE2")+_FORNECE+"PR ")
	While !Eof() .and. xFilial("SE2") == SE2->E2_filial .AND.;
		SE2->E2_FORNECE==_FORNECE .AND. SE2->E2_TIPO=="PR "
		If SE2->E2_VALOR == 0
			dbSkip()
			Loop
		Endif
		If SE2->E2_LOJA <> _LOJA
			dbSkip()
			Loop
		Endif
		dbSelectArea("TRB25")
		TRB25->(DbAppend())
		TRB25->E2_OK      := " "
		TRB25->E2_PREFIXO  := SE2->E2_PREFIXO
		TRB25->E2_NUM  := SE2->E2_NUM
		TRB25->E2_PARCELA  := SE2->E2_PARCELA
		TRB25->E2_EMISSAO  := SE2->E2_EMISSAO
		TRB25->E2_FORNECE  := SE2->E2_FORNECE
		TRB25->E2_LOJA  := SE2->E2_LOJA
		TRB25->E2_VENCREA  := SE2->E2_VENCREA
		TRB25->E2_VALOR  := SE2->E2_VALOR
		TRB25->E2_SALDO  := SE2->E2_SALDO
		DbSelectArea("SE2")
		DbSkip()
	EndDo
	dbSelectArea("TRB25")
	DbGoTop()
	
	nOpca   := 1
	Private oDlg4
	
	DEFINE MSDIALOG ODLG4 TITLE "Exclusao dos Provisorios" From 6.5,0 To 35,72 OF oMainWnd
	
	@ 20,30 Say OemToAnsi ("Selecione os Titulos irao ser excluidos") SIZE 150,20
	oMark := MsSelect():New("TRB25","E2_OK",,aTRB,,@cMarca,{35,1,200,280})
	oMark:oBrowse:lhasMark = .F.
	oMark:oBrowse:lCanAllmark := .t.
	
	ACTIVATE MSDIALOG ODLG4 ON INIT EnchoiceBar(ODLG4,{||nOpca:=2,ODLG4:End()},{||nOpca:=1,ODLG4:End()}) CENTERED
	
	If nOpca == 2      //OK
		Processa({|lEnd| EXCLUEPR()}, "Excluindo Notas")
	ENDIF
	
	If nopca == 1  //CANCELAR
		close
		ferase(cArqTrab)
	EndIf
	RESTAREA(AREAE2)
endif

Return
Static Function EXCLUEPR()
DbSelectArea("TRB25")
DbGoTop()
While !EOF()
	IF TRB25->E2_OK==CMARCA
		DBSELECTAREA("SE2")
		DBSETORDER(1)   //E2_FILIAL+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA
		IF DBSEEK(XFILIAL("SE2")+TRB25->E2_PREFIXO+TRB25->E2_NUM+TRB25->E2_PARCELA+"PR "+_FORNECE+_LOJA)
			RecLock("SE2",.F.)
			dbDelete()
			MsUnlock()
		ENDIF
	ENDIF
	dbSelectArea("TRB25")
	dbSkip()
EndDo
close
ferase(cArqTrab)
Return

Static Function VALdts()
***********************************************************************
/****************************************************
VALIDACAO DA DATA ATE
*****************************************************/

_cret:= !empty(ALLTRIM(DTOS(DATAATE)) )
if _cret
	if !empty(ALLTRIM(DTOS(DATADE)) )
		if DATADE > DATAATE
			_cret:=.f.
		endif
	endif
endif
return(_cret)

***********************************************************************
Static Function VALdte()
***********************************************************************
/****************************************************
VALIDACAO DA DATA DE
*****************************************************/

_cret:= !empty(ALLTRIM(DTOS(DATADE)))
if _cret
	if !empty(ALLTRIM(DTOS(DATAATE)) )
		if DATAATE < DATADE
			_cret:=.f.
		endif
	endif
endif
return(_cret)
