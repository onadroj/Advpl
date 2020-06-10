#INCLUDE "Topconn.ch"
#INCLUDE "rwmake.ch"
#INCLUDE "PROTHEUS.ch"

User Function TPCTXDOC()

Private cCadastro := "Tp. Contratos X Docs. Exigidos"
Private aRotina := { {"Pesquisar","AxPesqui",0,1} ,;
{"Visualizar","U_ATUSZZ(2)"	,0,2} ,;
{"Incluir","U_ATUSZZ(3)"    ,0,3} ,;
{"Alterar","U_ATUSZZ(4)"    ,0,4} ,;
{"Excluir","U_ATUSZZ(5)"    ,0,5} }

Private cDelFunc := 'execblock("SZZDEL")'

dbSelectArea("SZZ")
dbSetOrder(2) //
DBGOTOP()
mBrowse( 6,1,22,75,"SZZ",,,,,,)

Return

User Function ATUSZZ(nOpc)
Local cDoc             
PRIVATE aArea     := GetArea()
PRIVATE cCadastro := "Tp. Contratos X Docs. Exigidos"
PRIVATE oGetDad
PRIVATE oDlg
PRIVATE nUsado    := 0
PRIVATE nCntFor   := 0
PRIVATE nOpcA     := 0
PRIVATE lContinua := .T.

PRIVATE bWhile    := {|| .T. }   

PRIVATE aObjects  := {}
PRIVATE aPosObj   := {}
PRIVATE aSizeAut  := MsAdvSize()
PRIVATE aHEADER := {}
PRIVATE aCOLS   := {}
PRIVATE aRegistro := {}
PRIVATE aGETS   := {}
PRIVATE aTELA   := {}
PRIVATE cAlias :="SZZ"
PRIVATE aAlter := .T.
PRIVATE nReg      := SZZ->(RECNO())
PRIVATE _cTpcont:=space(3)
PRIVATE _cDescTp:=space(30)
PRIVATE INCLUI:=.F.
PRIVATE ALTERA:=.F.

dbselectarea("SZZ")
dbsetorder(1) //ZZ_FILIAL+ZZ_TPCONT

If nOPc==3
	_cTpcont:=CRIAVAR("ZZ_TPCONT")
	_cDesctp:=CRIAVAR("ZZ_DESCTP")
	INCLUI:=.T.
else
	_cTpcont:=SZZ->ZZ_TPCONT
	_cDescTp:=RetField("CN1",1,xFilial("CN1")+SZZ->ZZ_TPCONT,"CN1_DESCRI")
EndIf
if nOpc==4
	ALTERA:=.T.
endif

if lcontinua .and. monta(nOpc)
	aObjects := {}
	AAdd( aObjects, { 50,  40, .T., .F. } )
	AAdd( aObjects, {300, 300, .T., .T. } )
	aInfo := { aSizeAut[ 1 ], aSizeAut[ 2 ], aSizeAut[ 3 ], aSizeAut[ 4 ],3, 3 }
	aPosObj := MsObjSize( aInfo, aObjects, .T. )
	nOpca	:= 0
	aPosCol:={10,30,55,150,180,235,260,285}
	DEFINE MSDIALOG oDlg TITLE cCadastro From aSizeAut[7],00 To aSizeAut[6],aSizeAut[5] OF oMainWnd PIXEL
	@ 018,aPoscol[1] Say "Tipo Contrato" SIZE 060, 7  OF oDlg PIXEL
	@ 015,aPoscol[3] MSGet _cTpcont		 SIZE 030, 11 OF oDlg PIXEL valid U_SZZFIELDOK() when INCLUI picture "@!" f3 "CN1"
	@ 018,aPoscol[4] Say "Descrição" 	 SIZE 060, 7  OF oDlg PIXEL
	@ 015,aPoscol[5] MSGet _cDesctp		 SIZE 100, 11 OF oDlg PIXEL when .F. picture "@!"
	oGetDad := MSGetDados():New(aPosObj[2,1],aPosObj[2,2],aPosObj[2,3],aPosObj[2,4],nOpc,"U_SZZLOK()","U_SZZTOK()",,.T.,,,.T.,999,"U_SZZFIELDOK()",,,"U_SZZDEL()")
	ACTIVATE MSDIALOG oDlg ON INIT EnchoiceBar(oDlg,{||nOpca:=If(U_SZZTOK(),1,0),If(nOpcA==1,oDlg:End(),Nil)},{||nOpcA:=2,oDlg:End()},,/*aButtons*/)
	If ( nOpcA == 1 )
		Begin Transaction
		GRAVA(nOpc)
		EvalTrigger()
		End Transaction
	EndIf
EndIf
dbselectarea("SZZ")
Return(.T.)

//***********************************************
User Function SZZFIELDOK()
Local nPos1
local lret:=.t.
Local cConteudo:=""
Local nConteudo:=0
Local aArea:=getarea()
nPos1 := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "ZZ_DOC"})
nPos2 := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "ZZ_DESCDOC"})

IF "_cTpcont" $ __READVAR
	cConteudo:=&(__READVAR)
	dbselectarea("CN1")
	dbsetorder(1)
	if !dbseek(xfilial("CN1")+cConteudo)
		MsgStop("Tipo de contrato não cadastrado!")
		lRet:=.f.
    else
	   _cDesctp := CN1->CN1_DESCRI
	endif

	dbselectarea("SZZ")
	dbsetorder(1)
	if dbseek(xfilial("SZZ")+cConteudo)
		MsgStop("Tipo de contrato já existe nesta tabela. Utilize a opção alterar para incluir/alterar/excluir documentos.")
		lRet:=.f.
	endif

ENDIF

IF "ZZ_DOC" $ __READVAR
	cConteudo:=&(__READVAR)
	dbselectarea("CN5")
	dbsetorder(1)
	if !dbseek(xfilial("CN5")+cConteudo)
		MsgStop("Documento não cadastrado!")
		lRet:=.f.
	else
		for x:=1 to len(acols)
			if acols[x][len(acols[1])]==.F.
				IF acols[x][nPos1] == cConteudo .AND. N#X
					lRet:=.f.
					msgstop("Documento já digitado na linha "+alltrim(str(X)))
					exit
				ENDIF
			endif
		next
	endif
    If lRet
		acols[N][nPos2]:=Retfield("CN5",1,XFILIAL("CN5")+cConteudo,"CN5_DESCRI")
	Endif

ENDIF

restarea(aArea)
return(lret)


//***********************************************
User Function SZZLOK
Local nPos1
local lret:=.t.
Local avet:={}
Local nDef:=0
nPos1 := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "ZZ_DOC"})

IF Empty(acols[N][nPos1])
	lret:=.f.
	msgstop("Código do documento não pode ficar em branco!")
ENDIF

return(lret)


//***********************************************
User Function SZZTOK
Local lret:=.t.

If Empty(_cTpcont)
	MsgStop("Tipo de contrato precisa ser informado!")
	lret:=.f.
ElseIf Len(aCols)==1 .AND. Empty(aCols[1,1])
	MsgStop("Nenhum documento foi digitado na tabela!")
	lret:=.f.
Endif

return(lRet)

//***********************************************
Static Function Monta(nOpc,bBlock)
Local aArea    := GetArea()
Local lRetorno := .T.
Local nX       := 0
Local nPos1    := 0
nUsado    := 0
DEFAULT bBlock := {|| .T.}

aHeader := {}
aCols := {}
aRegistro:={}
dbSelectArea("SX3")
dbSetOrder(1)
MsSeek("SZZ")

While ( !EOF() .And. (SX3->X3_ARQUIVO == "SZZ") )
	If ( X3Uso(SX3->X3_USADO) .And. cNivel >= SX3->X3_NIVEL .And. ;
		Trim(SX3->X3_CAMPO) $ "ZZ_DOC/ZZ_DESCDOC")
		nUsado++
		AADD(aHeader,{ TRIM(X3Titulo()),;
		SX3->X3_CAMPO,;
		SX3->X3_PICTURE,;
		SX3->X3_TAMANHO,;
		SX3->X3_DECIMAL,;
		SX3->X3_VALID,;
		SX3->X3_USADO,;
		SX3->X3_TIPO,;
		SX3->X3_ARQUIVO,;
		SX3->X3_CONTEXT } )
	EndIf
	dbSelectArea("SX3")
	dbSkip()
EndDo

dbSelectArea("SZZ")
dbSetOrder(2)

MsSeek(xFilial("SZZ")+_cTpcont,.T.)
While ( !Eof() .And. SZZ->ZZ_FILIAL==xFilial("SZZ") .And. _cTpcont==SZZ->ZZ_TPCONT ) .AND. nOpc # 3
	If ( Eval(bBlock,"SZZ") .And. If(nOpc<>2,SoftLock("SZZ"),.T.) )
		aadd(aCols,Array(nUsado+1))
		AADD(aRegistro,SZZ->(RecNo()))
		
		For nX := 1 to Len(aHeader)
			
			If ( aHeader[nX][10] <> "V" .And. aHeader[nX][08] <> "M")
				aCols[Len(aCols)][nX] := FieldGet(FieldPos(aHeader[nX][2]))
			Else
				If ( aHeader[nX][08] <> "M" )
					aCols[Len(aCols)][nX] := CriaVar(aHeader[nX][2],.T.)
				Else
					aCols[Len(aCols)][nX] := FieldGet(FieldPos(aHeader[nX][2]))
				EndIf
			EndIf
			aCols[Len(aCols)][nUsado+1] := .F.
		Next nX
	Else
		lRetorno := .F.
		Exit
	EndIf
	nPos1 := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "ZZ_DESCDOC"})
	acols[Len(aCols)][nPos1]:=Retfield("CN5",1,XFILIAL("CN5")+SZZ->ZZ_DOC,"CN5_DESCRI")
	dbSelectArea("SZZ")
	dbSkip()
EndDo
if len(acols)==0
	aadd(aCols,Array(nUsado+1))
	aCols[1][nUsado+1] := .F.
	For nX := 1 To nUsado
		If ( aHeader[nX][10] <> "V" .And. aHeader[nX][08] <> "M")
			aCols[1][nX] := CriaVar(aHeader[nX][2],.T.)
		Endif
	Next nX
	nOpc:=3
EndIf
RestArea(aArea)
//GETDREFRESH()
Return(lRetorno)


//***********************************************
STATIC FUNCTION GRAVA(nOpc)
Local aarea
Local lDeleted  := .F.
Local cDoc
IF !nOpc==3
	aarea:=GetArea()
endif
If ( nOpc == 5 ) //EXCLUIR
	lDeleted := .T.
EndIf

dbselectarea("SZZ")
dbSetOrder(1)
For nX := 1 To Len(aCols)
	If ValType(aCols[nX,Len(aCols[nX])]) == "L"
		lDeleted := aCols[nX,Len(aCols[nX])]
	EndIf
	If  nOpc == 5
		lDeleted := .T.
	EndIf

	If ( Len(aRegistro) >= nx ) 
		dbSelectArea("SZZ")
		dbGoto( aRegistro[nx] )
		RecLock("SZZ",.F.)
	Else                                
		If ( !lDeleted )
			RecLock("SZZ",.T.)
		EndIf
	EndIf
	If ( !lDeleted )
		SZZ->ZZ_FILIAL := xFilial("SZZ")
		SZZ->ZZ_TPCONT := _cTpcont
		
		For nY := 1 to Len(aHeader)
			If ( aHeader[nY][10] <> "V" )
				SZZ->(FieldPut(FieldPos(Trim(aHeader[nY][2])),aCols[nX][nY]))
			EndIf
		Next nY
	Else
		If ( Len(aRegistro) >= nx )
			SZZ->(dbDelete())
		EndIf
	EndIf
	SZZ->(MsUnLock())
Next nX

if !nOpc==3
	restarea(aarea)
endif
return

//***********************************************
User Function SZZDEL()
Local lRet:=.t.
Local cDoc:=""
Local lDeleted:=.f.
If ValType(aCols[n,Len(aCols[n])]) == "L"
	lDeleted := aCols[n,Len(aCols[n])]
EndIf
If ( Len(aRegistro) >= n ) .and. lDeleted 
endif
return	(lRet)
