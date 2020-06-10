#INCLUDE "Topconn.ch"
#INCLUDE "FONT.CH"
#Include "Colors.ch"
#INCLUDE "PROTHEUS.ch"
#INCLUDE "RWMAKE.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³RDE811    º Autor ³ GATASSE            º Data ³  03/03/04   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ CADASTRO DE RDE.                                           º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP6 IDE                                                    º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User Function RDE811


//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Declaracao de Variaveis                                             ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

Private cCadastro := "RDE"
Private aRotina := { {"Pesquisar","AxPesqui",0,1} ,;
{ "Visualizar" ,'U_ATUALSZN',0,2},;
{ "Incluir"    ,'U_ATUALSZN',0,3},;
{ "Alterar"    ,'U_ATUALSZN',0,4}}
Private cDelFunc := ".T." // Validacao para a exclusao. Pode-se utilizar ExecBlock
Private cString := "SZN"
dbSelectArea("SZN")
dbSetOrder(1)
dbgotop()
dbSelectArea(cString)
mBrowse( 6,1,22,75,cString)
Return


/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³ATUALSZN   ºAutor  ³Microsiga           º Data ³  12/17/07   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³                                                            º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP                                                        º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/

User Function ATUALSZN(cAlias,nReg,nOpc)
LOCAL DOC,SERIE,CLIENTE,LOJA
Local aAltera		:= {}
Local aButton 	:= {}

Local cArq1
Local cArq2
Local cSeqChave 	:= ""
Local cEmpOri		:= cEmpAnt
Local cFilOri		:= cFilAnt

Local lDel 	  		:= .F.
Local lRet			:= .T.
Local nOpcao		:= nOpc	//Variavel para carregar a GetDB
Local aSizeAut		:= MsAdvSize(,.F.)
Local oDlg
Local oInf
Local oFnt
Local aObjects		:= {}
Local nOpcA     := 0
//campos que podem ser alterado no enchoice	
Local aCpos1    := {;
"ZN_NUM",;
"ZN_DATA",;             		
"ZN_CODBEM",;
"ZN_CODFUNC",;
"ZN_CONTADO",;
"ZN_DTINIC",;
"ZN_CONTINI",;
"ZN_OBS",;
"ZN_LOJA",;
"ZN_CLIENTE",;
"ZN_CCUSTO",;
"ZN_CENTRAB"}
//"ZN_NOMEBEM",;
//"ZN_NOMEFUN",;
//ZN_NOMCLI",;
Local oSAY1
Local oSAY2
Local oSAY3
Local oSAY4
Private INCLUI:=IIF(nOpc==3,.t.,.f.)
Private ALTERA:=IIF(nOpc==4,.t.,.f.)
Private aTotais:={0,0,0}
Private N:=1
Private aCols	:= {}
Private aHeader	:= {}
Private nUsado		:= 0
Private aRegistro:={}
Private cHoras, cHorast, cHorasm        

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Monta Getdados '                                             ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
if monta(nOpc)
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Inicializa desta forma para criar uma nova instancia de variaveis private ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	RegToMemory( "SZN", INCLUI, .F. )
	AAdd( aObjects, { 0,    100, .T., .F. }) //aObjects, { Tam.Colunas, Tam. Linhas, Hab. Col, Hab.Lin } do Cabeçalho
	AAdd( aObjects, { 100, 100, .T., .T. }) // aObjects, { Tam.Colunas, Tam. Linhas, Hab. Col, Hab.Lin } do Corpo
	AAdd( aObjects, { 010, 005, .F., .F. }) // aObjects, { Tam.Colunas, Tam. Linhas, Hab. Col, Hab.Lin } do Rodae
	aInfo := { aSizeAut[ 1 ], aSizeAut[ 2 ], aSizeAut[ 3 ], aSizeAut[ 4 ], 2, 2 }
	aPosObj := MsObjSize( aInfo, aObjects )

	aPosGet := MsObjGetPos(aSizeAut[3]-aSizeAut[1],305,;
	{{10,45,105,145,225,265,210,255},;
	{10,45,105,145,225,265,210,255},;
	{10,35,100,135,205,255},;
	{10,35,100,135,205,255},;
	{10,35,100,135,205,255},;
	{10,35,100,135,205,255}})
	DEFINE MSDIALOG oDlg TITLE OemToAnsi("Cadastro RDE") From aSizeAut[7],0 TO aSizeAut[6],aSizeAut[5] OF oMainWnd PIXEL
	EnChoice( cAlias, nReg, nOpc, , , , , aPosObj[1], aCPos1, 3 )
/*
	@ 14,aPosGet[1,1] 	SAY OemToAnsi("Data") of oDlg PIXEL SIZE 30,09
	@ 13,aPosGet[1,2] 	MSGET M->ZN_DATA 	Picture "@D" When INCLUI of oDlg PIXEL SIZE 20,10 VALID U_VALIDSZN()
	@ 14,aPosGet[1,3] 	SAY OemToAnsi("Numero") of oDlg PIXEL SIZE 20,09
	@ 13,aPosGet[1,4] 	MSGET oNum var M->ZN_NUM 	Picture "999999" When INCLUI of oDlg PIXEL SIZE 20,10 VALID U_VALIDSZN()
//proxima linha +14	
	*/
	oGet := MSGetDados():New(aPosObj[2,1],aPosObj[2,2],aPosObj[2,3],aPosObj[2,4],nOpc,"ExecBlock('VALIDSZN',.F.,.F.,'L')","ExecBlock('VALIDSZN',.F.,.F.,'T')","+ZN_ITEM",IIF(nOpc==4,.t.,.f.),,,,300)
//inicio rodape
	nGetLin := aPosObj[3,1]
	@ nGetLin,aPosGet[3,1]  SAY OemToAnsi("H.Prog.")						SIZE 020,09 OF oDlg PIXEL
	@ nGetLin,aPosGet[3,2]  SAY oSAY1 VAR 0 PICTURE "99:99"	SIZE 050,09 OF oDlg	PIXEL
	@ nGetLin,aPosGet[3,3]  SAY OemToAnsi("H.Trab.")						SIZE 020,09 OF oDlg PIXEL
	@ nGetLin,aPosGet[3,4]  SAY oSAY2 VAR 0 PICTURE "99:99"	SIZE 050,09 OF oDlg	PIXEL
	@ nGetLin,aPosGet[3,5]  SAY OemToAnsi("H.Manut.")						SIZE 020,09 OF oDlg PIXEL
	@ nGetLin,aPosGet[3,6]  SAY oSAY3 VAR 0 PICTURE "99:99"	SIZE 050,09 OF oDlg	PIXEL
	//função para rodapé, se mais de um campo, colocar vários osay....
	oDlg:Cargo	:= {|n1,n2,n3| oSay1:SetText(n1),oSay2:SetText(n2),oSay3:SetText(n3)}
	U_ZNRodape(oGet)
	ACTIVATE MSDIALOG oDlg ON INIT EnchoiceBar(oDlg,{||nOpcA:=1,IIf(oGet:TudoOk(),oDlg:End(),nOpcA := 0)},{||oDlg:End()},, )
	If nOpc == 2
		nOpca	:= 0
	EndIf
	If nOpca == 1
		Grava(nOpc)
	EndIf
endif
Return .T.

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³MONTA    ºAutor  ³Microsiga           º Data ³  12/17/07   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³                                                            º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP                                                        º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/

Static Function Monta(nOpc,bBlock)
Local aArea    := GetArea()
Local lRetorno := .T.
Local nX       := 0
//M->ZN_DATA,M->ZN_NUM,M->ZN_CODBEM,M->ZN_NOMEBEM,M->ZN_CCUSTO,M->ZN_CENTRAB,M->ZN_CODFUNC,M->ZN_NOMEFUN,M->ZN_CLIENTE,M->ZN_LOJA
//M->ZN_NOMCLI,M->ZN_CONTINI,M->ZN_DTINIC,M->ZN_CONTADO 
DEFAULT bBlock := {|| .T.}

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Faz a montagem do aHeader a partir dos campos SX3.           ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
aHeader := {}
aCols := {}
aRegistro:={}
dbSelectArea("SX3")
dbSetOrder(1)
MsSeek("SZN")
While ( !EOF() .And. (SX3->X3_ARQUIVO == "SZN") )
//f ( X3Uso(SX3->X3_USADO) .And. cNivel >= SX3->X3_NIVEL .And. ;
	If (cNivel >= SX3->X3_NIVEL .And. ;
		AllTrim(SX3->X3_Campo) $ "ZN_ITEM/ZN_HORAI/ZN_HORAF/ZN_THORA/ZN_CODOCOR/ZN_NOMOCOR/ZN_TIPO/ZN_OBS")
		nUsado++
		AADD(aHeader,{ TRIM(X3Titulo()),;
		SX3->X3_CAMPO,;
		SX3->X3_PICTURE,;
		SX3->X3_TAMANHO,;
		SX3->X3_DECIMAL,;
		"ExecBlock('VALIDSZN',.F.,.F.,'F')",;
		SX3->X3_USADO,;
		SX3->X3_TIPO,;
		SX3->X3_ARQUIVO,;
		SX3->X3_CONTEXT } )
	EndIf
	dbSelectArea("SX3")
	dbSkip()
EndDo
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Faz a montagem do aCols baseado no aHeader.                  ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
if !INCLUI
	N:=1
	dbSelectArea("SZN")
	dbSetOrder(1)//
	cChave:=SZN->ZN_CODBEM+DTOS(SZN->ZN_DATA)+SZN->ZN_CODFUNC
	aCols:={}
	dbSelectArea("SZN")
	dbSetOrder(1)//ZN_FILIAL+ZN_CODBEM+DTOS(ZN_DATA)+ZN_CODFUNC+ZN_NUM+ZN_ITEM
	dbSeek(xFilial("SZN")+cChave,.T.)
	While (! Eof())                           .And. ;
		(SZN->ZN_Filial  == xFilial("SZN")) .And. ;
		(SZN->ZN_CODBEM+DTOS(SZN->ZN_DATA)+SZN->ZN_CODFUNC == cChave)
		If ( Eval(bBlock,"SZN") .And. If(nOpc<>2,SoftLock("SZN"),.T.) )
			aadd(aCols,Array(nUsado+1))
			AADD(aRegistro,SZN->(RecNo()))
			
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
				IF ALLTRIM(aHeader[nX,2])=="ZN_HORAS"
					aCols[Len(aCols),nX] := VLDHORAS(FieldGet(FieldPos("ZN_HORASI")))-VLDHORAS(FieldGet(FieldPos("ZN_HORASF")))
				ENDIF
				IF ALLTRIM(aHeader[nX,2])=="ZN_NOMOCOR"
					aCols[Len(aCols),nX] := RETFIELD("SZO",1,XFILIAL("SZO")+FieldGet(FieldPos("ZN_CODOCOR")),"ZO_NOME")
				ENDIF
				IF ALLTRIM(aHeader[nX,2])=="ZN_TIPO"
					aCols[Len(aCols),nX] := RETFIELD("SZO",1,XFILIAL("SZO")+FieldGet(FieldPos("ZN_CODOCOR")),"ZO_TIPO")
				ENDIF
			Next nX
		Else
			lRetorno := .F.
			Exit
		EndIf
		dbSelectArea("SZN")
		dbSkip()
	EndDo
	if len(acols)==0
		aadd(aCols,Array(nUsado+1))
		aCols[1][nUsado+1] := .F.
		For nX := 1 To nUsado
			If ( Trim(aHeader[nX][2]) == "ZN_ITEM" )
				aCols[1][nX] := StrZero(1,Len(ZN_ITEM))
			ELSEIf ( Trim(aHeader[nX][2]) $ "ZN_HORAI/ZN_HORAF" )
				aCols[1][nX] := U_SZNRELA1(N)
			ELSE
				aCols[1][nX] := CriaVar(alltrim(aHeader[nX][2]),.T.)             	
			EndIf
		Next nX
		nOpc:=3
	EndIf
Endif
RestArea(aArea)
Return(lRetorno)
/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³GRAVA     ºAutor  ³                    º Data ³  27/03/07   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³GRAVA  º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP                                                        º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
STATIC FUNCTION GRAVA(nOpc)
Local aarea
Local lDeleted  := .F.
IF !nOpc==3
	aarea:=GetArea()
endif
dbselectarea("SZN")
dbSetOrder(1)
For nX := 1 To Len(aCols)
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³Verifico se a linha foi deletada.                                     ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	If ValType(aCols[nX,Len(aCols[nX])]) == "L"
		lDeleted := aCols[nX,Len(aCols[nX])]
	EndIf
	If ( nOpc == 5 ) //EXCLUIR
		lDeleted := .T.
	EndIf
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³Posiciono no registro para alteracao/exclusao                         ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	If ( Len(aRegistro) >= nx ) //antigo
		dbSelectArea("SZN")
		dbGoto( aRegistro[nx] )
		RecLock("SZN",.F.)
	Else                                //novo
		If ( !lDeleted )
			RecLock("SZN",.T.)
		EndIf
	EndIf
	If ( !lDeleted )
		Replace  ZN_Filial  With  xFilial("SZN")
		Replace  ZN_NUM with M->ZN_NUM
		Replace  ZN_DATA  with M->ZN_DATA
		Replace  ZN_CLIENTE with M->ZN_CLIENTE
		Replace  ZN_LOJA with M->ZN_LOJA
		Replace  ZN_CODBEM with M->ZN_CODBEM
		Replace  ZN_NOMEBEM with M->ZN_NOMEBEM
		Replace  ZN_CODFUNC with M->ZN_CODFUNC
		Replace  ZN_NOMEFUN with M->ZN_NOMEFUN
		Replace  ZN_CONTADO with M->ZN_CONTADO
		Replace  ZN_CCUSTO with M->ZN_CCUSTO
		Replace  ZN_CENTRAB with M->ZN_CENTRAB
		For nY := 1 to Len(aHeader)
			If ( aHeader[nY][10] <> "V" )
				SZN->(FieldPut(FieldPos(Trim(aHeader[nY][2])),aCols[nX][nY]))
			EndIf
		Next nY
	Else
		If ( Len(aRegistro) >= nx )
			SZN->(dbDelete())
		EndIf
	EndIf
	SZN->(MsUnLock())
Next nX
if !nOpc==3
	restarea(aarea)
endif
return


STATIC FUNCTION HORAST(cHoras, cHorasm, cHorasT)
LOCAL T:=0
LOCAL T1:=0
LOCAL T2:=0
LOCAL nCol3
nCol3:= (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "ZN_THORA"}))
nCol5:= (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "ZN_TIPO"}))

FOR X:=1 TO LEN(ACOLS)
	T+=VALHORA(ACOLS[X,nCol3])
	if acols[x,nCol5]=="T"
		T1+=VALHORA(ACOLS[X,nCol3])
	endif
	if acols[x,nCol5]=="M"
		T2+=VALHORA(ACOLS[X,nCol3])
	endif
NEXT
cHoras:=STRHORA(T)
cHorast:=STRHORA(T1)
cHorasm:=STRHORA(T2)            

U_ZNRODAPE(oGet)
RETURN
/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³HORAST   ºAutor  ³Microsiga           º Data ³  12/17/07   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³                                                            º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP                                                        º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/

STATIC function VALHORA(nVal)
LOCAL RET
nHora:=val(substr(nVal,1,2))
nMin:=val(substr(nVal,4,2))
ret:=nHora*60+nMin
RETURN(RET)
/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³HORAST   ºAutor  ³Microsiga           º Data ³  12/17/07   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³                                                            º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP                                                        º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/

STATIC function STRHORA(N)
LOCAL RET
cHora:=strzero((int(n/60)),2)
cMin:=strzero((n%60),2)
ret:=chora+":"+cMin
RETURN(RET)
/*
STATIC FUNCTION FRegCont()
LOCAL AAREA
AAREA:=GETAREA()
dbSelectArea("ST9")
dbsetorder(1)//T9_FILIAL+T9_CODBEM
dbseek(XFILIAL("ST9")+M->ZN_CODBEM)
nPOSCONT:=ST9->T9_POSCONT

nACUMST9 := CHKCONTACU(M->ZN_CONTADO, M->ZN_CODBEM, "C")
//STI-PLANO MANUTENCAO
dbSelectArea("STI")
dbsetorder(1)
dbseek(XFILIAL("STI")+"000000")
dbSelectArea("ST9")
nVARDIA := NGVARDIA(ALLTRIM(M->ZN_CODBEM),M->ZN_DATA,nACUMST9)

IF nVardia > 999999
nVardia := 999999
Endif

RECLOCK("ST9",.F.)
REPLACE ST9->T9_POSCONT WITH M->ZN_CONTADO
REPLACE ST9->T9_DTULTAC WITH M->ZN_DATA
REPLACE ST9->T9_CONTACU WITH ST9->T9_CONTACU+(M->ZN_CONTADO-nPOSCONT)
REPLACE ST9->T9_VARDIA WITH nVARDIA
MSUNLOCK()

dbSelectArea("STP")

cORDEM := GETSXENUM("STP", "TP_ORDEM") //NEXTOSMAN()
ConfirmSX8()
cPLANO := "000000"


RECLOCK("STP",.T.)

STP->TP_FILIAL	:=XFILIAL("STP")
STP->TP_ORDEM	:=	cOrdem
STP->TP_PLANO	:=	CPlano
STP->TP_CODBEM	:=M->ZN_CODBEM
//STP->TP_SERVICO	:=
//STP->TP_SEQUENC
STP->TP_DTORIGI	:=DDATABASE
STP->TP_POSCONT	:=M->ZN_CONTADO
STP->TP_DTREAL	:=DDATABASE
STP->TP_DTLEITU	:=M->ZN_DATA
STP->TP_SITUACA	:="L"
STP->TP_TERMINO	:="S"
//STP->TP_USUCANC
STP->TP_USULEI	:=CUSERNAME
STP->TP_DTULTAC	:=M->ZN_DATA
//STP->TP_COULTAC
STP->TP_CCUSTO	:=ST9->T9_CCUSTO
STP->TP_CENTRAB	:=ST9->T9_CENTRAB
STP->TP_VARDIA	:=ST9->T9_VARDIA
STP->TP_TEMCONT	:=ST9->T9_TEMCONT
STP->TP_ACUMCON	:=ST9->T9_CONTACU
//STP->TP_VIRACON
STP->TP_HORA:="23:00"
STP->TP_TIPOLAN:="C"
MSUNLOCK()
RESTAREA(AAREA)
RETURN
*/
USER Function ZNRodape(oGetDad)
Local oDlg
Local nX     	:= 0

Local nUsado    := Len(aHeader)
Local lTestaDel := nUsado <> Len(aCols[1])

LOCAL T:=0
LOCAL T1:=0
LOCAL T2:=0
LOCAL nCol3 := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "ZN_THORA"}))
LOCAL nCol5 := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "ZN_TIPO"}))

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Caso nao seja passado o objeto da getdados deve-se pegar a janela default³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If ( oGetDad == Nil )
	oDlg		:= GetWndDefault()
	If ( ValType(oDlg:Cargo)<>"B" )
		oDlg := oDlg:oWnd
	EndIf
Else
	oDlg := oGetDad:oWnd
EndIf
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Soma as variaveis do Rodape                                    ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³Soma as variaveis do aCols                                     ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	For nX := 1 To Len(aCols)
		If ( (lTestaDel .And. !aCols[nX][nUsado+1]) .Or. !lTestaDel )
			T+=VALHORA(ACOLS[nX,nCol3])
			if acols[nX,nCol5] $ "TP"
				T1+=VALHORA(ACOLS[nX,nCol3])
			endif
			if acols[nX,nCol5]=="M"
				T2+=VALHORA(ACOLS[nX,nCol3])
			endif
		EndIf
	Next nX
    cHoras:=STRHORA(T)
    cHorast:=STRHORA(T1)
    cHorasm:=STRHORA(T2)

If ValType(oDlg) == "O"
	If ( ValType(oDlg:Cargo)=="B" )
		Eval(oDlg:Cargo,cHoras,cHorast,cHorasm)
	EndIf
Endif
Return(.T.)


/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³VALIDSZN    º Autor ³ GATASSE            º Data ³  04/03/04   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ VALIDA CADASTRO DE RDE                                     º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP6 IDE                                                    º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User Function VALIDSZN
LOCAL RET:=.T., AAREA
PRIVATE nCol1,nCol2,nCol3,nCol4,nCol5
AAREA:=GETAREA()
nCol1 := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "ZN_HORAI"}))
nCol2 := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "ZN_HORAF"}))
nCol3 := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "ZN_THORA"}))
nCol4 := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "ZN_NOMOCOR"}))
nCol5 := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "ZN_TIPO"}))
nCol6 := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "ZN_CODOCOR"}))
IF PARAMIXB=="L"
	IF DTOC(M->ZN_DATA)=="  /  /  " .OR. M->ZN_CONTADO==0
		RET:=.F.
		MSGSTOP("Campos obrigatorios no cabecalho nao foram preenchidos!")
	ENDIF
	if ret
		IF (alltrim(ACOLS[N,nCol1])=="" .or. alltrim(ACOLS[N,nCol2])=="" .or. alltrim(ACOLS[N,nCol6])=="").and. acols[n,len(acols[n])]==.f.
			RET:=.F.
			MSGSTOP("Campos obrigatorios na grade nao foram preenchidos!")
		ENDIF
	ENDIF
ENDIF
if inclui
	if VALDUP()
		RET:=.F.
		MSGSTOP("Ja existe RDE lancado para este Bem , Data e Operador!")
	ENDIF
endif

IF PARAMIXB=="T"
	IF DTOC(M->ZN_DATA)=="  :  " .OR. M->ZN_CONTADO==0
		RET:=.F.
		MSGSTOP("Campos obrigatorios no cabecalho nao foram preenchidos!")
	ENDIF
	if ret
		for x:=1 to len(acols)
			if acols[x,len(acols[1])]<>.t.
				if alltrim(acols[x,nCol1])="  :  " .or. alltrim(acols[x,nCol2])="  :  " .or.;
					alltrim(acols[x,nCol6])=""
					RET:=.F.
					MSGSTOP("Campos obrigatorios na grade nao foram preenchidos!")
				endif
			endif
		next
	endif
ENDIF

if __readvar=="M->ZN_HORAI" .or. __readvar=="M->ZN_HORAF"
	if len(alltrim(&__readvar))<>5
		msgstop("Hora invalida!")
		ret:=.f.
	elseif alltrim(&__readvar)<>""
		nHora:=val(substr(&__readvar,1,2))
		nMin:=val(substr(&__readvar,4,2))
		if nHora<0 .or. nHora >23
			msgstop("Hora invalida!")
			ret:=.f.
		endif
		if nMin<0 .or. nMin >59
			msgstop("Hora invalida!")
			ret:=.f.
		endif
		if __readvar=="M->ZN_HORAI" .and. &__readvar<>"  :  "
			nV1:=iif(__readvar=="M->ZN_HORAI",VALHORA(&__readvar),VALHORA(acols[N,nCol1]))
			IF n>1 .and. IIF(type("acols")="U",.F.,IIF(LEN(ACOLS)>1,.T.,.F.))
				Y:=0
				FOR X:=N-1 TO 1 STEP -1
					IF ACOLS[X,LEN(ACOLS[1])]<>.T.
						y:=x
						exit
					ENDIF
				NEXT
				IF Y<>0 .AND.	&__readvar<>Acols[y,nCol2]
					MsgStop("Hora Inicial precisa ser identica `a Hora Final no ultimo lancamento valido!")
					ret:=.f.
				ENDIF
			ENDIF
		endif
		if ret
			if (__readvar=="M->ZN_HORAI" .and. &__readvar<>"  :  " .and. alltrim(acols[N,nCol2])<>"") .or.;
				(__readvar=="M->ZN_HORAF" .and. &__readvar<>"  :  " .and. alltrim(acols[N,nCol1])<>"" )
				nV1:=iif(__readvar=="M->ZN_HORAI",VALHORA(&__readvar),VALHORA(acols[N,nCol1]))
				nV2:=iif(__readvar=="M->ZN_HORAF",VALHORA(&__readvar),VALHORA(acols[N,nCol2]))
				if nV2<nV1
					nTot:=1440-nV1+nV2
				else
					nTot:=nV2-nV1
				endif
				acols[N,nCol3]:=STRHORA(nTot)
				HORAST()
                U_ZNRODAPE(oGet)
			endif
		endif
	endif
endif
if __readvar=="M->ZN_CODOCOR"
	AAREA:=GETAREA()
	DBSELECTAREA("SZO")
	DBSETORDER(1)
	DBSEEK(XFILIAL("SZO")+&__READVAR)
	acols[N,nCol4]:=SZO->ZO_NOME
	acols[N,nCol5]:=SZO->ZO_TIPO
	RESTAREA(AAREA)
	HORAST()
	if valhora(cHoras)>1440
		msgstop("Periodo informado acima de 24 horas!")
		ret:=.f.
	endif
ENDIF
IF __readvar=="M->ZN_CODFUNC"
	IF !EXISTCPO("ST1",&__readvar)
		ret:=.f.
	else
		M->ZN_NOMEFUN:=retfield("ST1",1,XFILIAL("ST1")+&__READVAR,"T1_NOME")
	endif
endif
IF __readvar=="M->ZN_CLIENTE" .OR. __readvar=="M->ZN_LOJA"
	IF __readvar=="M->ZN_CLIENTE"
		cCli:=&__readvar
		cLj:=M->ZN_LOJA
	else
		cCli:=M->ZN_CLIENTE
		cLj:=&__readvar
	endif
	DBSELECTAREA("SA1")
	DBSETORDER(1)
	IF DBSEEK(XFILIAL("SA1")+cCli+cLj)
		M->ZN_NOMCLI:=SA1->A1_NREDUZ
	else
		ret:=.f.
	endif
endif
IF __readvar=="M->ZN_CODBEM"
	DBSELECTAREA("ST9")     //VERIFICA SE BEM EXISTE E RECUPERA DADOS
	DBSETORDER(1)
	IF DBSEEK(XFILIAL("ST9")+&__READVAR)
		M->ZN_CENTRAB:=ST9->T9_CENTRAB
		M->ZN_CCUSTO:=ST9->T9_CCUSTO
		M->ZN_CONTINI	:=ST9->T9_POSCONT
		M->ZN_DTINIC:=ST9->T9_DTULTAC
		M->ZN_NOMEBEM:=ST9->T9_NOME
	ELSE
		ret:=.f.
	endif
endif
RESTAREA(AAREA)
Return(ret)
/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³VALDUP   ºAutor  ³Microsiga           º Data ³  12/17/07   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³                                                            º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP                                                        º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/

STATIC FUNCTION VALDUP()
LOCAL RET,AAREA
RET:=.F.
AAREA:=GETAREA()
dbSelectArea("SZN")
dbSetOrder(1)//ZN_FILIAL+ZN_CODBEM+DTOS(ZN_DATA)+ZN_CODFUNC+ZN_NUM+ZN_ITEM
dbSeek(xFilial("SZN")+M->ZN_CODBEM+DTOS(M->ZN_DATA)+M->ZN_CODFUNC,.T.)
IF xFilial("SZN")+M->ZN_CODBEM+DTOS(M->ZN_DATA)+M->ZN_CODFUNC==SZN->ZN_FILIAL+SZN->ZN_CODBEM+DTOS(SZN->ZN_DATA)+SZN->ZN_CODFUNC
	RET:=.T.
ENDIF
RESTAREA(AAREA)
RETURN(RET)
/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³SZNRELA1   º Autor ³ GATASSE            º Data ³  22/03/04   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ INICIALIZADOR PADRAO PARA X3_RELACAO DE ZN_HORAI           º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP6 IDE                                                    º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User Function SZNRELA1(N)
LOCAL RET
Local y,x
RET:="  :  "
IF n>1 .and. IIF(type("acols")="U",.F.,IIF(LEN(ACOLS)>1,.T.,.F.))
	nCol1 := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "ZN_HORAI"}))
	nCol2 := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "ZN_HORAF"}))
	Y:=0
	FOR X:=N-1 TO 1 STEP -1
		IF ACOLS[X,LEN(ACOLS[1])]<>.T.
			y:=x
			exit
		ENDIF
	NEXT
	IF Y<>0
		RET:=Acols[y,nCol2]
	ENDIF
ENDIF

Return(RET)
