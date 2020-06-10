#INCLUDE "Topconn.ch"
#INCLUDE "rwmake.ch"
#INCLUDE "PROTHEUS.ch"

User Function USRXSIT()

Private cCadastro := "Usuário X Situações de Contratos"
Private aRotina := { {"Pesquisar","AxPesqui",0,1} ,;
{"Visualizar","U_ATUZZ0(2)"	,0,2} ,;
{"Incluir","U_ATUZZ0(3)"    ,0,3} ,;
{"Alterar","U_ATUZZ0(4)"    ,0,4} ,;
{"Excluir","U_ATUZZ0(5)"    ,0,5} }

Private cDelFunc := 'execblock("ZZ0DEL")'

dbSelectArea("ZZ0")
dbSetOrder(1) 
DBGOTOP()
mBrowse( 6,1,22,75,"ZZ0",,,,,,)

Return

User Function ATUZZ0(nOpc)
Local cDoc             
PRIVATE aArea     := GetArea()
PRIVATE cCadastro := "Usuário X Situações de Contratos"
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
PRIVATE cAlias :="ZZ0"
PRIVATE aAlter := .T.
PRIVATE nReg      := ZZ0->(RECNO())
PRIVATE _cUser:=CriaVar("ZZ0_CODUSR",.F.)
PRIVATE _cDescUser:=space(40)
PRIVATE INCLUI:=.F.
PRIVATE ALTERA:=.F.

dbselectarea("ZZ0")
dbsetorder(1) //ZZ0_FILIAL+ZZ0_CODUSR

If nOPc==3
	_cUser:=CRIAVAR("ZZ0_CODUSR")
	_cDescUser:=space(40)
	INCLUI:=.T.
else
	_cUser:=ZZ0->ZZ0_CODUSR
	_cDescUser:=UsrRetName(ZZ0->ZZ0_CODUSR) 
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
	@ 018,aPoscol[1] Say "Usuário" SIZE 060, 7  OF oDlg PIXEL
	@ 015,aPoscol[3] MSGet _cUser	 SIZE 030, 11 OF oDlg PIXEL valid U_ZZ0FIELDOK() when INCLUI picture "@!" F3 "USR"
	@ 018,aPoscol[4] Say "Nome" 	 SIZE 060, 7  OF oDlg PIXEL
	@ 015,aPoscol[5] MSGet _cDescUser	SIZE 100, 11 OF oDlg PIXEL when .F. picture "@!"
	oGetDad := MSGetDados():New(aPosObj[2,1],aPosObj[2,2],aPosObj[2,3],aPosObj[2,4],nOpc,"U_ZZ0LOK()","U_ZZ0TOK()",,.T.,,,.T.,999,"U_ZZ0FIELDOK()",,,"U_ZZ0DEL()")
	ACTIVATE MSDIALOG oDlg ON INIT EnchoiceBar(oDlg,{||nOpca:=If(U_ZZ0TOK(),1,0),If(nOpcA==1,oDlg:End(),Nil)},{||nOpcA:=2,oDlg:End()},,/*aButtons*/)
	If ( nOpcA == 1 )
		Begin Transaction
		GRAVA(nOpc)
		EvalTrigger()
		End Transaction
	EndIf
EndIf
dbselectarea("ZZ0")
Return(.T.)

//***********************************************
User Function ZZ0FIELDOK()
Local nPos1
local lret:=.t.
Local cConteudo:=""
Local nConteudo:=0
Local aArea:=getarea()
nPos1 := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "ZZ0_SITUAC"})

IF "_cUser" $ __READVAR
	cConteudo:=&(__READVAR)
	lret := UsrExist(cConteudo)
	if !lret
		MsgStop("Usuário não cadastrado!")
    else
	   _cDescUser := UsrRetName(cConteudo) 
	endif

	dbselectarea("ZZ0")
	dbsetorder(1)
	if dbseek(xfilial("ZZ0")+cConteudo)
		MsgStop("Usuário já existe nesta tabela. Utilize a opção alterar para incluir/alterar/excluir situações.")
		lRet:=.f.
	endif

ENDIF

IF "ZZ0_SITUAC" $ __READVAR
	cConteudo:=&(__READVAR)
	if !(cConteudo $ "01/02/03/04/05/06/07/08/09/10")
		MsgStop("Situação Inválida!")
		lRet:=.f.
	else
		for x:=1 to len(acols)
			if acols[x][len(acols[1])]==.F.
				IF acols[x][nPos1] == cConteudo .AND. N#X
					lRet:=.f.
					msgstop("Situação já digitada na linha "+alltrim(str(X)))
					exit
				ENDIF
			endif
		next
	endif

ENDIF

restarea(aArea)
return(lret)


//***********************************************
User Function ZZ0LOK
Local nPos1
local lret:=.t.
Local avet:={}
Local nDef:=0
nPos1 := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "ZZ0_SITUAC"})

IF Empty(acols[N][nPos1])
	lret:=.f.
	msgstop("Situação não pode ficar em branco!")
ENDIF

return(lret)


//***********************************************
User Function ZZ0TOK
Local lret:=.t.
Local nPos1 := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "ZZ0_SITUAC"})

If Empty(_cUser)
	MsgStop("Código do usuário precisa ser informado!")
	lret:=.f.
ElseIf Len(aCols)==1 .AND. Empty(aCols[1,1])
	MsgStop("Nenhuma situação foi digitada na tabela!")
	lret:=.f.
ElseIf Empty(acols[N][nPos1])
	lret:=.f.
	msgstop("Situação não pode ficar em branco!")
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
MsSeek("ZZ0")

While ( !EOF() .And. (SX3->X3_ARQUIVO == "ZZ0") )
	If ( X3Uso(SX3->X3_USADO) .And. cNivel >= SX3->X3_NIVEL .And. ;
		Trim(SX3->X3_CAMPO) $ "ZZ0_SITUAC")
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

dbSelectArea("ZZ0")
dbSetOrder(1)

MsSeek(xFilial("ZZ0")+_cUser)
While ( !Eof() .And. ZZ0->ZZ0_FILIAL==xFilial("ZZ0") .And. _cUser==ZZ0->ZZ0_CODUSR ) .AND. nOpc # 3
	If ( Eval(bBlock,"ZZ0") .And. If(nOpc<>2,SoftLock("ZZ0"),.T.) )
		aadd(aCols,Array(nUsado+1))
		AADD(aRegistro,ZZ0->(RecNo()))
		
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
	dbSelectArea("ZZ0")
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

dbselectarea("ZZ0")
dbSetOrder(1)
For nX := 1 To Len(aCols)
	If ValType(aCols[nX,Len(aCols[nX])]) == "L"
		lDeleted := aCols[nX,Len(aCols[nX])]
	EndIf
	If  nOpc == 5
		lDeleted := .T.
	EndIf

	If ( Len(aRegistro) >= nx ) 
		dbSelectArea("ZZ0")
		dbGoto( aRegistro[nx] )
		RecLock("ZZ0",.F.)
	Else                                
		If ( !lDeleted )
			RecLock("ZZ0",.T.)
		EndIf
	EndIf
	If ( !lDeleted )
		ZZ0->ZZ0_FILIAL := xFilial("ZZ0")
		ZZ0->ZZ0_CODUSR := _cUser
		
		For nY := 1 to Len(aHeader)
			If ( aHeader[nY][10] <> "V" )
				ZZ0->(FieldPut(FieldPos(Trim(aHeader[nY][2])),aCols[nX][nY]))
			EndIf
		Next nY
	Else
		If ( Len(aRegistro) >= nx )
			ZZ0->(dbDelete())
		EndIf
	EndIf
	ZZ0->(MsUnLock())
Next nX

if !nOpc==3
	restarea(aarea)
endif
return

//***********************************************
User Function ZZ0DEL()
Local lRet:=.t.
Local cDoc:=""
Local lDeleted:=.f.
If ValType(aCols[n,Len(aCols[n])]) == "L"
	lDeleted := aCols[n,Len(aCols[n])]
EndIf
If ( Len(aRegistro) >= n ) .and. lDeleted 
endif
return	(lRet)
