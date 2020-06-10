#INCLUDE "protheus.ch"
#INCLUDE "topconn.ch"

// Ponto de entrada no final da rotina, após o destravamento de todas as tabelas envolvidas na gravação do documento de entrada.

User Function MT103FIM()
Local _nOpcao := PARAMIXB[1]   // Opção Escolhida pelo usuario no aRotina 3=Inclusão;5=Exclusão
Local _nConfirma := PARAMIXB[2]   // Se o usuario confirmou a operação de gravação da NFE     1=Sim
Local _cDoc := SF1->F1_DOC
Local _cSerie := SF1->F1_SERIE
Local _cFornece := SF1->F1_FORNECE
Local _cLoja := SF1->F1_LOJA
Local _aArea:=GetArea()
Local _aAreaSD1
Local _aDados := {}
Local _cContrato := ""
Local _cRevisao := ""
Local _cDocsObr := ""
Local _nI := 0
Private _aContratos := {}
Private _aDocs := {}

If _nOpcao<>3 .OR. _nConfirma<>1 .OR. (cEmpAnt<>"03" .AND. cEmpAnt<>"99")
	Return()
Endif

dbSelectArea("SD1")
_aAreaSD1 := GetArea()
dbSetOrder(1)
dbSeek(xFilial("SD1")+_cDoc+_cSerie+_cFornece+_cLoja,.T.)

While !EOF() .AND. xFilial("SD1")+SD1->D1_DOC+SD1->D1_SERIE+SD1->D1_FORNECE+SD1->D1_LOJA==xFilial("SD1")+_cDoc+_cSerie+_cFornece+_cLoja
	//Caso o documento tenha pedido(s), busca no(s) pedido(s) os dados do(s) contrato(s) (Gestão de Contratos)
	If !Empty(SD1->D1_PEDIDO)
	   LeContrato(SD1->D1_PEDIDO)
	Endif
	dbSelectArea("SD1")
	dbSkip()
EndDo
RestArea(_aAreaSD1)

For _nI:=1 to len(_aContratos)
	LeDocs(_aContratos[_nI][1],_aContratos[_nI][2])
Next

If Len(_aDocs) > 0
	aAdd(_aDados,SF1->F1_DOC + "/" + SF1->F1_SERIE)
	aAdd(_aDados,SF1->F1_FORNECE + "/" + SF1->F1_LOJA + "-" + RetField("SA2",1,xFilial("SA2")+SF1->F1_FORNECE+SF1->F1_LOJA,"A2_NOME"))
	aAdd(_aDados,cUserName)
	
	_cContrato := _aDocs[1][1]
	_cRevisao := _aDocs[1][2]
	_cDocsObr := "<br><br>Contrato: "+_aDocs[1][1]+" Revisão: "+_aDocs[1][2]
	For _I:=1 to len(_aDocs)
		If _aDocs[_I][1]==_cContrato .AND. _aDocs[_I][2]==_cRevisao
			_cDocsObr += "<br>"+_aDocs[_I][3]
		Else
			_cContrato := _aDocs[1][1]
			_cRevisao := _aDocs[1][2]
			_cDocsObr += "<br><br><br>Contrato: "+_aDocs[1][1]+" Revisão: "+_aDocs[1][2]
			_cDocsObr += "<br>"+_aDocs[_I][3]
		Endif
	Next

	aAdd(_aDados,_cDocsObr)
	
	MEnviaMail("_DO",_aDados,,,,.T.) //Messenger de Obrigatoriedade de Documento
Endif

RestArea(_aArea)

Return()

//----------------------------------------------------------------------------------------------------
Static Function LeContrato(_NumPed)
Local _aAreaX := GetArea()
Local _nPos := 0

DbSelectArea("SC7")
DbSetOrder(1) //C7_FILIAL+C7_NUM+C7_ITEM+C7_SEQUEN
If DbSeek(xFilial("SC7")+_NumPed,.T.)
	If !Empty(SC7->C7_CONTRA)
		_nPos := aScan(_aContratos,{ |_Item| _Item[1] == SC7->C7_CONTRA .AND. _Item[2] == SC7->C7_CONTREV })
		If _nPos == 0 
			aAdd(_aContratos,{SC7->C7_CONTRA,SC7->C7_CONTREV})
		EndIf
	Endif
Endif

RestArea(_aAreaX)

Return

//----------------------------------------------------------------------------------------------------
Static Function LeDocs(_cContrato,_cRevisao)
Local _cTpcto := ""
Local _cQuery := ""
Local _aArea := GetArea() 

_cTpcto := RetField("CN9",1,xFilial("CN9")+_cContrato+_cRevisao,"CN9_TPCTO")

_cQuery := "SELECT ZZ_DOC FROM "+RetSqlName("SZZ")+" WHERE "
_cQuery += "ZZ_TPCONT='"+_cTpcto+"' AND D_E_L_E_T_ <> '*'"
TCQUERY _cQuery NEW ALIAS "QRYDOCS"

DbSelectArea("QRYDOCS")
DbGoTop()
While !Eof()
	aAdd(_aDocs,{_cContrato,_cRevisao,RetField("CN5",1,XFILIAL("CN5")+QRYDOCS->ZZ_DOC,"CN5_DESCRI")})
	DbSkip()
EndDo
CLOSE
RestArea(_aArea)

Return()        
