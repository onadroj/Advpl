#INCLUDE "protheus.ch"
#INCLUDE "topconn.ch"

User Function CONSCTR()
PRIVATE cCadastro := "Consulta Contratos"
PRIVATE oGetDad
PRIVATE oDlg
PRIVATE nUsado    := 0

PRIVATE aObjects  := {}
PRIVATE aPosObj   := {}
PRIVATE aSizeAut  := MsAdvSize()
PRIVATE aHEADER := {}
PRIVATE aCOLS   := {}
PRIVATE aGETS   := {}
PRIVATE aTELA   := {}
PRIVATE _cTpcont:=space(3)
PRIVATE _cDescTp:=space(30)
PRIVATE oTip
PRIVATE _cTip := "1"
PRIVATE aTip := {"1=Compra","2=Venda"} 
PRIVATE _cCons := ""
PRIVATE oCliFor
PRIVATE _cCliFor := CriaVar("A2_COD",.F.)
PRIVATE oLjCliFor
PRIVATE _cLjCliFor := CriaVar("A2_LOJA",.F.)
PRIVATE _cDesCliFor := CriaVar("A2_NOME",.F.)
PRIVATE oTitCliFor
PRIVATE _cTitCliFor := "Fornecedor"
PRIVATE _lFaz := .T.

if monta()
	aObjects := {}
	AAdd( aObjects, { 50,  40, .T., .F. } )
	AAdd( aObjects, {300, 300, .T., .T. } )
	aInfo := { aSizeAut[ 1 ], aSizeAut[ 2 ], aSizeAut[ 3 ], aSizeAut[ 4 ],3, 3 }
	aPosObj := MsObjSize( aInfo, aObjects, .T. )
	aPosCol:={10,25,85,120,180,195,230,410,500}
	DEFINE MSDIALOG oDlg TITLE cCadastro From aSizeAut[7],00 To aSizeAut[6],aSizeAut[5] OF oMainWnd PIXEL
	@ 038,aPoscol[1] Say "Tipo"		   	 SIZE 060, 7  OF oDlg PIXEL
	@ 035,aPoscol[2] MSCOMBOBOX oTip VAR _cTip ITEMS aTip SIZE 045,08 OF oDlg PIXEL valid U_CN9FIELDOK() When _lFaz ON CHANGE _CNGTIPO()
	@ 038,aPoscol[3] Say oTitCliFor VAR _cTitCliFor	 SIZE 060, 7  OF oDlg PIXEL
	@ 035,aPoscol[4] MSGet oCliFor VAR _cClifor	SIZE 050, 11 OF oDlg PIXEL valid U_CN9FIELDOK() picture "@!" F3 CpoRetF3("F1_FORNECE") When _lFaz
	@ 038,aPoscol[5] Say "Loja"			 SIZE 060, 7  OF oDlg PIXEL
	@ 035,aPoscol[6] MSGet oLjCliFor VAR _cLjCliFor	 SIZE 020, 11 OF oDlg PIXEL valid U_CN9FIELDOK() picture "@!" F3 CpoRetF3("F1_LOJA") When _lFaz
	@ 035,aPoscol[7] MSGet _cDesCliFor	 SIZE 160, 11 OF oDlg PIXEL when .F. picture "@!"
	@ 035,aPoscol[8] BUTTON "Consulta" SIZE 30, 11 FONT oDlg:oFont ACTION _CargaDados(_cClifor,_cLjClifor,_cTip) OF oDlg PIXEL
	@ 035,aPoscol[9] BUTTON "Limpa" SIZE 30, 11 FONT oDlg:oFont ACTION Eval({|| aCols:={}, _LimpBrow(), _lFaz:=!_lFaz}) OF oDlg PIXEL
	oGetDad := MSGetDados():New(aPosObj[2,1],aPosObj[2,2],aPosObj[2,3],aPosObj[2,4],2,,,,.T.,,,.T.,999,"U_CN9FIELDOK()",,,)
	ACTIVATE MSDIALOG oDlg ON INIT EnchoiceBar(oDlg,{||oDlg:End()},{||oDlg:End()},,/*aButtons*/)
EndIf

Return()

//***********************************************
User Function CN9FIELDOK()
Local nPos1
local lret:=.t.
Local cConteudo:=""
Local nConteudo:=0
Local aArea:=getarea()
nPos1 := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "ZZ_DOC"})
nPos2 := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "ZZ_DESCDOC"})

If "_cCliFor" $ __READVAR .OR. "_cLjCliFor" $ __READVAR 
	If _cTip=="1"
		_cDesCliFor := RetField("SA2",1,xFilial("SA2")+_cCliFor+_cLjCliFor,"A2_NOME")
	Else
		_cDesCliFor := RetField("SA1",1,xFilial("SA1")+_cCliFor+_cLjCliFor,"A1_NOME")
	Endif
Endif

restarea(aArea)
return(lret)


//***********************************************
Static Function Monta()
Local lRetorno := .T.
Local nX       := 0
Local nPos1    := 0
nUsado    := 0

aHeader := {}

dbSelectArea("SX3")
dbSetOrder(1)
MsSeek("CN9")

While ( !EOF() .And. (SX3->X3_ARQUIVO == "CN9") )
	If ( X3Uso(SX3->X3_USADO) .And. cNivel >= SX3->X3_NIVEL .And. Trim(SX3->X3_CAMPO) $ "CN9_NUMERO/CN9_REVISA/CN9_DTINIC/CN9_SALDO/CN9_DESCPG")
		nUsado++
		AADD(aHeader,{Iif(SX3->X3_CAMPO == "CN9_DESCPG", "Situacao", TRIM(X3Titulo())),;
		SX3->X3_CAMPO,;
		SX3->X3_PICTURE,;
		SX3->X3_TAMANHO,;
		SX3->X3_DECIMAL,;
		SX3->X3_VALID,;
		SX3->X3_USADO,;
		SX3->X3_TIPO,;
		SX3->X3_ARQUIVO,;
		SX3->X3_CONTEXT })
	EndIf
	dbSelectArea("SX3")
	dbSkip()
EndDo

_LimpBrow()

Return(lRetorno)


//***************************************************
Static Function _CargaDados(_cClifor,_cLjClifor,_cTipo)
Local _cQuery := ""

Local _nPosNum := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "CN9_NUMERO"})
Local _nPosRev := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "CN9_REVISA"})
Local _nPosIni := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "CN9_DTINIC"})
Local _nPosSld := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "CN9_SALDO"})
Local _nPosSit := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "CN9_DESCPG"})

Local _cSit := ""
/**

	Local oModel  	:= FWModelActive()
	Local oModelCND	:= oModel:GetModel("CNDDETAIL")
	Local _nPosNum	:= oModelCND:GetValue("CN9_NUMERO")
	Local _nPosRev	:= oModelCND:GetValue("CN9_REVISA")
	Local _nPosIni	:= oModelCND:GetValue("CN9_DTINIC")
	Local _nPosSld	:= oModelCND:GetValue("CN9_SALDO")
	Local _nPosSit	:= oModelCND:GetValue("CN9_SITUAC")


*/
aCols := {}
If _cTipo=="1" //Compra
	_cQuery := "SELECT CNC_CODIGO, CNC_LOJA, CNC_NUMERO, CNC_REVISA FROM "
	_cQuery += RetSqlName("CNC") + " WHERE "
	_cQuery += "CNC_CODIGO = '"+_cClifor+"' AND CNC_LOJA = '"+_cLjClifor+"' AND D_E_L_E_T_ <> '*' "
	_cQuery += "ORDER BY CNC_NUMERO, CNC_REVISA "

	TCQUERY _cQuery NEW ALIAS "QRY"

	DbSelectArea("QRY")
	DbGoTop()
	While !Eof() 
		aadd(aCols,Array(nUsado+1))
		
		_cSit := RetField("CN9",1,xFilial("CN9")+QRY->CNC_NUMERO+QRY->CNC_REVISA,"CN9_SITUAC")
        Do Case
			Case _cSit=="01"
				_cSit := "Cancelado"
			Case _cSit=="02"
				_cSit := "Elaboracao"
			Case _cSit=="03"
				_cSit := "Emitido"
			Case _cSit=="04"
				_cSit := "Aprovacao"
			Case _cSit=="05"
				_cSit := "Vigente"
			Case _cSit=="06"
				_cSit := "Paralisado"
			Case _cSit=="07"
				_cSit := "Sol. Finalizacao"
			Case _cSit=="08"
				_cSit := "Finalizado"
			Case _cSit=="09"
				_cSit := "Revisao"
			Case _cSit=="10"
				_cSit := "Revisado"
		End
		
		acols[Len(aCols)][_nPosNum]:=QRY->CNC_NUMERO
		acols[Len(aCols)][_nPosRev]:=QRY->CNC_REVISA
		acols[Len(aCols)][_nPosIni]:=RetField("CN9",1,xFilial("CN9")+QRY->CNC_NUMERO+QRY->CNC_REVISA,"CN9_DTINIC")
		acols[Len(aCols)][_nPosSld]:=RetField("CN9",1,xFilial("CN9")+QRY->CNC_NUMERO+QRY->CNC_REVISA,"CN9_SALDO")
		acols[Len(aCols)][_nPosSit]:=_cSit
		
		aCols[Len(aCols)][nUsado+1] := .F.

		dbSelectArea("QRY")
		dbSkip()
	EndDo
	CLOSE
Else
	_cQuery := "SELECT CN9_CLIENT, CN9_LOJACL, CN9_NUMERO, CN9_REVISA, CN9_SITUAC, CN9_DTINIC, CN9_SALDO FROM "
	_cQuery += RetSqlName("CN9") + " WHERE "
	_cQuery += "CN9_CLIENT = '"+_cClifor+"' AND CN9_LOJACL = '"+_cLjClifor+"' AND D_E_L_E_T_ <> '*' "
	_cQuery += "ORDER BY CN9_NUMERO, CN9_REVISA "

	TCQUERY _cQuery NEW ALIAS "QRY"

	DbSelectArea("QRY")
	DbGoTop()
	While !Eof() 
		aadd(aCols,Array(nUsado+1))
		
        Do Case
			Case QRY->CN9_SITUAC=="01"
				_cSit := "Cancelado"
			Case QRY->CN9_SITUAC=="02"
				_cSit := "Elaboracao"
			Case QRY->CN9_SITUAC=="03"
				_cSit := "Emitido"
			Case QRY->CN9_SITUAC=="04"
				_cSit := "Aprovacao"
			Case QRY->CN9_SITUAC=="05"
				_cSit := "Vigente"
			Case QRY->CN9_SITUAC=="06"
				_cSit := "Paralisado"
			Case QRY->CN9_SITUAC=="07"
				_cSit := "Sol. Finalizacao"
			Case QRY->CN9_SITUAC=="08"
				_cSit := "Finalizado"
			Case QRY->CN9_SITUAC=="09"
				_cSit := "Revisao"
			Case QRY->CN9_SITUAC=="10"
				_cSit := "Revisado"
		End
		
		acols[Len(aCols)][_nPosNum]:=QRY->CN9_NUMERO
		acols[Len(aCols)][_nPosRev]:=QRY->CN9_REVISA
		acols[Len(aCols)][_nPosIni]:=QRY->CN9_DTINIC
		acols[Len(aCols)][_nPosSld]:=QRY->CN9_SALDO
		acols[Len(aCols)][_nPosSit]:=_cSit
		
		aCols[Len(aCols)][nUsado+1] := .F.

		dbSelectArea("QRY")
		dbSkip()
	EndDo
	CLOSE
Endif

if len(acols)==0
	_LimpBrow()
EndIf

GetdRefresh()

_lFaz := .F.

Return() 


//***************************************************
Static Function _CNGTIPO()
Local cTitulo
If _cTip=="2"
	If oCliFor<>Nil
		oCliFor:cF3 := CpoRetF3("F2_CLIENTE")
	EndIf
	If oTitCliFor<>Nil
		cTitulo := oTitCliFor:cCaption
		oTitCliFor:SetText(RetTitle("F2_CLIENTE"))
		If oTitCliFor:cCaption<>cTitulo
			_cCliFor := CriaVar("F2_CLIENTE",.F.)
			_cLjCliFor := CriaVar("F2_LOJA",.F.)		
			_cDesCliFor := CriaVar("A1_NOME",.F.)		
		Endif
	EndIf
Else
	If oCliFor<>Nil
		oCliFor:cF3 := CpoRetF3("F1_FORNECE")
	EndIf
	If oTitCliFor<>Nil
		cTitulo := oTitCliFor:cCaption
		oTitCliFor:SetText(RetTitle("F1_FORNECE"))
		If oTitCliFor:cCaption<>cTitulo
			_cCliFor := CriaVar("F1_FORNECE",.F.)
			_cLjCliFor := CriaVar("F1_LOJA",.F.)		
			_cDesCliFor := CriaVar("A2_NOME",.F.)		
		Endif
	EndIf
EndIf
If oTitCliFor<>Nil
	oTitCliFor:Refresh()
EndIf
If oCliFor<>Nil
	oCliFor:Refresh()
EndIf
If oLjCliFor<>Nil
	oLjCliFor:Refresh()
EndIf
Return()

//***************************************************
Static Function _LimpBrow() 
Local nX := 0

aadd(aCols,Array(nUsado+1))
aCols[1][nUsado+1] := .F.
For nX := 1 To nUsado
	If ( aHeader[nX][10] <> "V" .And. aHeader[nX][08] <> "M")
		aCols[1][nX] := CriaVar(aHeader[nX][2],.F.)
	Endif
Next nX

Return()