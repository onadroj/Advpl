//#include "fivewin.ch"
#include "rwmake.ch"

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³BROWSB1   ºAutor  ³GATASSE             º Data ³  02/15/06   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³ ROTINA PARA CADASTRO DE PRODUTOS VINCULANDO COM FABRICANTE º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP                                                        º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/

USER Function BROWSB1()
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Define Variaveis                                     ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PRIVATE aPos   := {  15,  1, 70, 315 }
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Define Array contendo as Rotinas a executar do programa      ³
//³ ----------- Elementos contidos por dimensao ------------     ³
//³ 1. Nome a aparecer no cabecalho                              ³
//³ 2. Nome da Rotina associada                                  ³
//³ 3. Usado pela rotina                                         ³
//³ 4. Tipo de Transa‡„o a ser efetuada                          ³
//³    1 - Pesquisa e Posiciona em um Banco de Dados             ³
//³    2 - Simplesmente Mostra os Campos                         ³
//³    3 - Inclui registros no Bancos de Dados                   ³
//³    4 - Altera o registro corrente                            ³
//³    5 - Remove o registro corrente do Banco de Dados          ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
PRIVATE cCadastro := OemToAnsi("Produtos x Fabricantes")
PRIVATE aRotina := { { "Pesquisar","AxPesqui"  ,0,1},;
{ "Visualizar","u_BSB1Visual",0,2},;
{ "Incluir","u_BSB1Inclui",0,3},;
{ "Alterar","u_BSB1Altera",0,4,20},;
{ "Excluir","u_BSB1Deleta",0,5,21}}
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Endereca a funcao de BROWSE                          ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
dbSelectArea("SB1")
dbSetOrder(1)
dbSeek(xFilial())
mBrowse( 6, 1,22,75,"SB1")
Return


User Function BSB1Visual(cAlias,nReg,nOpc)
Local nOpcA     := 0
Local nUsado    := 0
Local nCntFor   := 0
Local naCols    := 0
Local oDlg
Local lContinua := .T.
Private aTela[0][0]
Private aGets[0]
Private aHeader := {}
Private aCols   := {}
Private bCampo:= { |nField| FieldName(nField) }
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Cria Variaveis de Memoria da Enchoice                ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
dbSelectArea("SB1")
For nCntFor:= 1 To FCount()
	M->&(EVAL(bCampo,nCntFor)) := FieldGet(nCntFor)
Next nCntFor
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Monta o aHeader                                       ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
aHeader	:= {}
dbSelectArea("SX3")
dbSetOrder(1)
dbSeek("SZ2",.T.)

nUsado := 0
While ( !Eof() .And. SX3->X3_ARQUIVO == "SZ2" )
	If ( X3USO(SX3->X3_USADO) .And. cNivel >= SX3->X3_NIVEL )
		
		If !(AllTrim(SX3->X3_Campo) $ "Z2_CODPROD/Z2_DESPROD/Z2_LOCAL/Z2_MSBLQL")
			AADD(aHeader,{ AllTrim(X3Titulo()),;
			SX3->X3_CAMPO,;
			IIF(AllTrim(SX3->X3_Campo)=="Z2_CODFABR","@!",SX3->X3_PICTURE),;
			SX3->X3_TAMANHO,;
			SX3->X3_DECIMAL,;
			SX3->X3_VALID,;
			SX3->X3_USADO,;
			SX3->X3_TIPO,;
			SX3->X3_ARQUIVO,;
			SX3->X3_CONTEXT } )
			nUsado++
		Endif
	endif
	dbSkip()
EndDo

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Monta o aCols                                         ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
aCols	:= {}
nAcols:= 0
dbSelectArea("SZ2")
dbSetOrder(1)
dbSeek(xFilial()+SB1->B1_COD)

While ( 	!Eof() .And. xFilial() == SZ2->Z2_FILIAL .And. ;
	SZ2->Z2_CODPROD == SB1->B1_COD )
	aadd(aCols,Array(nUsado+1))
	nAcols ++
	For nCntFor := 1 To nUsado
		If ( aHeader[nCntFor][10] != "V")
			aCols[nAcols][nCntFor] := FieldGet(FieldPos(aHeader[nCntFor][2]))
		Else
			aCols[nAcols][nCntFor] := CriaVar(aHeader[nCntFor][2],.T.)
		EndIf
	Next nCntFor
	aCols[nAcols][nUsado+1] := .F.
	dbSelectArea("SZ2")
	dbSkip()
EndDo
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Envia para processamento dos Gets          ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
nOpcA:=0
dbSelectArea("SB1")
N:=LEN(aCols)
aSize := MsAdvSize()
aObjects := {}
AAdd( aObjects, { 100,100,.T.,.T. } )
AAdd( aObjects, { 100,100,.T.,.T. } )
aInfo := { aSize[1],aSize[2],aSize[3],aSize[4],3,3}
aPosObj := MsObjSize( aInfo, aObjects )
DEFINE MSDIALOG oDlg TITLE OemToAnsi( "Produto x Codigo de Fabricante" ) From ;
aSize[7],0 To aSize[6],aSize[5] OF oMainWnd PIXEL

EnChoice( cAlias, SB1->(RECNO()), nOpc, , , , , aPosObj[1], , 3 )

oGet := MSGetDados():New( aPosObj[2,1],aPosObj[2,2],aPosObj[2,3],aPosObj[2,4], nOpc,"u_BSB1LinOk","u_BSB1TudOk", , .t. )

ACTIVATE MSDIALOG oDlg ON INIT EnchoiceBar( oDlg,{ || nOpca := 1,if(u_BSB1TudOk(),oDlg:End(),.t.) }, {|| nOpca := 0, oDlg:End()})



DbSelectArea("SB1")


Return(.T.)

User Function BSB1Altera(cAlias,nReg,nOpc)
Local nOpcA     := 0
Local nUsado    := 0
Local nCntFor   := 0
Local naCols    := 0
Local oDlg
Local lContinua := .T.
Local aAltera   := {}
Private aTela[0][0]
Private aGets[0]
Private aHeader := {}
Private aCols   := {}
Private bCampo:= { |nField| FieldName(nField) }
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Cria Variaveis de Memoria da Enchoice                ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
dbSelectArea("SB1")
lContinua := SoftLock("SB1")
For nCntFor:= 1 To FCount()
	M->&(EVAL(bCampo,nCntFor)) := FieldGet(nCntFor)
Next nCntFor
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Monta o aHeader                                       ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
aHeader	:= {}
dbSelectArea("SX3")
dbSetOrder(1)
dbSeek("SZ2",.T.)
nUsado := 0
While ( !Eof() .And. SX3->X3_ARQUIVO == "SZ2" )
	If ( X3USO(SX3->X3_USADO) .And. cNivel >= SX3->X3_NIVEL )
		If !(AllTrim(SX3->X3_Campo) $ "Z2_CODPROD/Z2_DESPROD/Z2_LOCAL/Z2_MSBLQL")
			AADD(aHeader,{ AllTrim(X3Titulo()),;
			SX3->X3_CAMPO,;
			IIF(AllTrim(SX3->X3_Campo)=="Z2_CODFABR","@!",SX3->X3_PICTURE),;
			SX3->X3_TAMANHO,;
			SX3->X3_DECIMAL,;
			SX3->X3_VALID,;
			SX3->X3_USADO,;
			SX3->X3_TIPO,;
			SX3->X3_ARQUIVO,;
			SX3->X3_CONTEXT } )
			nUsado++
		Endif
	Endif
	dbSkip()
EndDo
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Monta o aCols                                         ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
nAcols	:= 0
aCols		:= {}
dbSelectArea("SZ2")
dbSetOrder(1)
IF !dbSeek(xFilial()+SB1->B1_COD)
	dbSelectArea("SZ2")
	dbSetOrder(1)
	RecLock("SZ2",.T.)
	SZ2->Z2_FILIAL  	:= xFilial("SZ2")
	SZ2->Z2_CODPROD 	:= M->B1_COD
	SZ2->Z2_DESPROD     := M->B1_DESC
	SZ2->Z2_CODFABR     := "INTERN"
	SZ2->Z2_MSBLQL      := M->B1_MSBLQL
	SZ2->Z2_CODPECA     := M->B1_COD
	SZ2->Z2_LOCAL       := M->B1_LOCPAD
	MSUNLOCK()
EndIf
dbSeek(xFilial()+SB1->B1_COD)
While ( 	!Eof() .And. xFilial() == SZ2->Z2_FILIAL .And. ;
	SZ2->Z2_CODPROD == SB1->B1_COD )
	aadd(aCols,Array(nUsado+1))
	nAcols ++
	For nCntFor := 1 To nUsado
		If ( aHeader[nCntFor][10] != "V")
			aCols[nAcols][nCntFor] := FieldGet(FieldPos(aHeader[nCntFor][2]))
		Else
			aCols[nAcols][nCntFor] := CriaVar(aHeader[nCntFor][2],.T.)
		EndIf
	Next nCntFor
	aCols[nAcols][nUsado+1] := .F.
	dbSelectArea("SZ2")
	lContinua := SoftLock("SZ2")
	aadd(aAltera,Recno())
	dbSkip()
EndDo
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Envia para processamento dos Gets          ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
nOpcA:=0
dbSelectArea("SB1")
If ( lContinua )
	aSize := MsAdvSize()
	aObjects := {}
	AAdd( aObjects, { 100,100,.T.,.T. } )
	AAdd( aObjects, { 100,100,.T.,.T. } )
	aInfo := { aSize[1],aSize[2],aSize[3],aSize[4],3,3}
	aPosObj := MsObjSize( aInfo, aObjects )
	DEFINE MSDIALOG oDlg TITLE OemToAnsi( "Produto x Codigo de Fabricante" ) From ;
	aSize[7],0 To aSize[6],aSize[5] OF oMainWnd PIXEL
	
	EnChoice( cAlias, SB1->(RECNO()), nOpc, , , , , aPosObj[1], , 3 )
	
	oGet := MSGetDados():New( aPosObj[2,1],aPosObj[2,2],aPosObj[2,3],aPosObj[2,4], nOpc,"u_BSB1LinOk()","u_BSB1TudOk()", , .t. )
	
	ACTIVATE MSDIALOG oDlg ON INIT EnchoiceBar( oDlg,{ || nOpca := 1,if(u_BSB1TudOk(),oDlg:End(),.t.) }, {|| nOpca := 0, oDlg:End()})
	
	
	DbSelectArea("SB1")
	dbGoto(nReg)
	If ( nOpcA == 1 )
		Begin Transaction
		BSB1Grava(2,aAltera) 
		End Transaction
	EndIf
EndIf
MsUnLockAll()
Return(.T.)

User Function BSB1Inclui(cAlias,nReg,nOpc)
Local nOpcA     := 0
Local nUsado    := 0
Local nCntFor   := 0
Local oDlg
Local lContinua := .T.
Private aTela[0][0]
Private aGets[0]
Private aHeader := {}
Private aCols   := {}
Private bCampo  := { |nField| FieldName(nField) }
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Cria Variaveis de Memoria da Enchoice                ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
dbSelectArea("SB1")
For nCntFor:= 1 To FCount()
	M->&(EVAL(bCampo,nCntFor)) := CriaVar(FieldName(nCntFor),.T.)
Next nCntFor
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Monta o aHeader                                       ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
aHeader := {}
dbSelectArea("SX3")
dbSetOrder(1)
dbSeek("SZ2",.T.)

nUsado := 0
While ( !Eof() .And. SX3->X3_ARQUIVO == "SZ2" )
	If ( X3USO(SX3->X3_USADO) .And. cNivel >= SX3->X3_NIVEL )
		If !(AllTrim(SX3->X3_Campo) $ "Z2_CODPROD/Z2_DESPROD/Z2_LOCAL/Z2_MSBLQL")
			AADD(aHeader,{ AllTrim(X3Titulo()),;
			SX3->X3_CAMPO,;
			IIF(AllTrim(SX3->X3_Campo)=="Z2_CODFABR","@!",SX3->X3_PICTURE),;
			SX3->X3_TAMANHO,;
			SX3->X3_DECIMAL,;
			SX3->X3_VALID,;
			SX3->X3_USADO,;
			SX3->X3_TIPO,;
			SX3->X3_ARQUIVO,;
			SX3->X3_CONTEXT } )
			nUsado++
		ENDIF
	Endif
	dbSkip()
EndDo
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Monta o aCols                                         ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
aCols := {}
aadd(aCols,Array(nUsado+1))
dbSelectArea("SX3")
dbSeek("SZ2")
nUsado := 0
While ( !Eof() .And. SX3->X3_ARQUIVO == "SZ2" )
	If ( X3USO(SX3->X3_USADO) .And. cNivel >= SX3->X3_NIVEL )
		If !(AllTrim(SX3->X3_Campo) $ "Z2_CODPROD/Z2_DESPROD/Z2_LOCAL/Z2_MSBLQL")
			
			nUsado++
			aCols[1][nUsado] := CriaVar(allTrim(SX3->X3_CAMPO),.T.)
		Endif
	Endif
	dbSkip()
End
aCols[1][nUsado+1] := .F.
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Envia para processamento dos Gets          ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
nOpcA:=0
dbSelectArea("SB1")
aSize := MsAdvSize()
aObjects := {}
AAdd( aObjects, { 100,100,.T.,.T. } )
AAdd( aObjects, { 100,100,.T.,.T. } )
aInfo := { aSize[1],aSize[2],aSize[3],aSize[4],3,3}
aPosObj := MsObjSize( aInfo, aObjects )
DEFINE MSDIALOG oDlg TITLE OemToAnsi( "Produto x Código de Fabricante" ) From ;
aSize[7],0 To aSize[6],aSize[5] OF oMainWnd PIXEL

EnChoice( cAlias, SB1->(RECNO()), nOpc, , , , , aPosObj[1], , 3 )

oGet := MSGetDados():New( aPosObj[2,1],aPosObj[2,2],aPosObj[2,3],aPosObj[2,4], nOpc,"u_BSB1LinOk","u_BSB1TudOk", , .t. )

ACTIVATE MSDIALOG oDlg ON INIT EnchoiceBar( oDlg,{ || nOpca := 1,if(u_BSB1TudOk(),oDlg:End(),.t.) }, {|| nOpca := 0, oDlg:End()})



DbSelectArea("SB1")
dbGoto(nReg)
If ( nOpcA == 1 )
	Begin Transaction
	If ( BSB1Grava(1) )
		dbSelectArea("SZ5")
		RECLOCK("SZ5",.F.)
		SZ5->Z5_DESCRI := alltrim(substr(M->B1_COD,5))
		MSUNLOCK()
		dbSelectArea("SB1")
	EndIf
	End Transaction
Else
	dbSelectArea("SZ5")
	MSUNLOCK()
	dbSelectArea("SB1")
EndIf
Return(nOpca)

User Function BSB1Deleta(cAlias,nReg,nOpc)
Local nOpcA     := 0
Local nUsado    := 0
Local nCntFor   := 0
Local naCols    := 0
Local oDlg
Local lContinua := .T.
Private aTela[0][0]
Private aGets[0]
Private aHeader := {}
Private aCols   := {}
Private bCampo  := { |nField| FieldName(nField) }
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Cria Variaveis de Memoria da Enchoice                ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
dbSelectArea("SB1")
lContinua := SoftLock("SB1")
For nCntFor:= 1 To FCount()
	M->&(EVAL(bCampo,nCntFor)) := FieldGet(nCntFor)
Next nCntFor
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Monta o aHeader                                       ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
dbSelectArea("SX3")
dbSetOrder(1)
dbSeek("SZ2",.T.)
nUsado := 0
While ( !Eof() .And. SX3->X3_ARQUIVO == "SZ2" )
	If ( X3USO(SX3->X3_USADO) .And. cNivel >= SX3->X3_NIVEL )
		If !(AllTrim(SX3->X3_Campo) $ "Z2_CODPROD/Z2_DESPROD/Z2_LOCAL/Z2_MSBLQL")
			AADD(aHeader,{ AllTrim(X3Titulo()),;
			SX3->X3_CAMPO,;
			IIF(AllTrim(SX3->X3_Campo)=="Z2_CODFABR","@!",SX3->X3_PICTURE),;
			SX3->X3_TAMANHO,;
			SX3->X3_DECIMAL,;
			SX3->X3_VALID,;
			SX3->X3_USADO,;
			SX3->X3_TIPO,;
			SX3->X3_ARQUIVO,;
			SX3->X3_CONTEXT } )
			nUsado++
		ENDIF
	Endif
	dbSkip()
EndDo
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Monta o aCols                                         ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
nAcols := 0
aCols  := {}
dbSelectArea("SZ2")
dbSetOrder(1)
dbSeek(xFilial()+SB1->B1_COD)
While ( 	!Eof() .And. xFilial() == SZ2->Z2_FILIAL .And. ;
	SZ2->Z2_CODPROD == SB1->B1_COD )
	aadd(aCols,Array(nUsado+1))
	nAcols ++
	For nCntFor := 1 To nUsado
		If ( aHeader[nCntFor][10] != "V")
			aCols[nAcols][nCntFor] := FieldGet(FieldPos(aHeader[nCntFor][2]))
		Else
			aCols[nAcols][nCntFor] := CriaVar(aHeader[nCntFor][2],.T.)
		EndIf
	Next nCntFor
	aCols[nAcols][nUsado+1] := .F.
	dbSelectArea("SZ2")
	lContinua := SoftLock("SZ2")
	dbSkip()
EndDo
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Envia para processamento dos Gets          ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
nOpcA:=0
dbSelectArea("SB1")
If ( lContinua )
	aSize := MsAdvSize()
	aObjects := {}
	AAdd( aObjects, { 100,100,.T.,.T. } )
	AAdd( aObjects, { 100,100,.T.,.T. } )
	aInfo := { aSize[1],aSize[2],aSize[3],aSize[4],3,3}
	aPosObj := MsObjSize( aInfo, aObjects )
	DEFINE MSDIALOG oDlg TITLE OemToAnsi( "Produto x Codigo de Fabricante" ) From ;
	aSize[7],0 To aSize[6],aSize[5] OF oMainWnd PIXEL
	
	EnChoice( cAlias, SB1->(RECNO()), nOpc, , , , , aPosObj[1], , 3 )
	
	oGet := MSGetDados():New( aPosObj[2,1],aPosObj[2,2],aPosObj[2,3],aPosObj[2,4], nOpc,"u_BSB1LinOk","u_BSB1TudOk", , .t. )
	
	ACTIVATE MSDIALOG oDlg ON INIT EnchoiceBar( oDlg,{ || nOpca := 1,if(u_BSB1TudOk(),oDlg:End(),.t.) }, {|| nOpca := 0, oDlg:End()})
	
	DbSelectArea("SB1")
	dbGoto(nReg)
	If ( nOpcA == 1 )
		Begin Transaction
		If ( BSB1Grava(3) )
		EndIf
		End Transaction
	EndIf
EndIf
MsUnLockAll()
Return(nOpca)

User Function BSB1TudOk()
Local lRetorno := .T.
lRetorno:=u_BSB1LinOk()
Return(lRetorno)

User Function BSB1LinOk()
Local lRetorno := .T.

nUsado  := Len(aHeader)+1
nPosField1 := aScan(aHeader,{|x| Trim(x[2])=="Z2_CODFABR" })
nPosField2 := aScan(aHeader,{|x| Trim(x[2])=="Z2_CODPECA" })
If ( !aCols[n][nusado]  )
	
	For nCntFor := 1 To Len(aCols)
		If ( !aCols[nCntFor][nusado] .And. !Empty(aCols[nCntFor,nPosField1]) )
			If N<>nCntFor
				IF  ( aCols[nCntFor][nPosField1]+aCols[nCntFor][nPosField2]==aCols[n][nPosField1]+aCols[n][nPosField2] )
					lRetorno := .F.
					MsgStop("Registro ja existente!")
				EndIf
			EndIf
		EndIf
	Next nCntFor
ENDIF
Return(lRetorno)

Static Function BSB1Grava(nOpc,aAltera)
Local aAreax
Local lGravou   := .F.
Local nCntFor 	:= 0
Local nCntFor2  := 0
Local nUsado    := 0
Local nSeq      := 0
Local nPosField := 0
Local nPosPrd   := 0
Private bCampo  := { |nField| FieldName(nField) }
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ nOpc : 1 - Inclusao de Registros                     ³
//³ nOpc : 2 - Alteracao de Registros                    ³
//³ nOpc : 3 - Exclusao de Registros                     ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
nUsado := Len(aHeader) + 1
If ( nOpc == 1 )
	aareax:=getarea()
	dbselectarea("ST7")  //GRAVOU FABRICANTE PADRAO
	dbSetOrder(1)
	if !dbseek(xfilial("ST7")+"INTERN")
		reclock("ST7",.T.)
		ST7->T7_FILIAL := XFILIAL("ST7")
		ST7->T7_FABRICA :="INTERN"
		ST7->T7_NOME := "PROPRIA EMPRESA"
		MsUnLock()
	ENDIF
	Restarea(aareax)
	dbSelectArea("SB1")
	RecLock("SB1",.T.)
	For nCntFor := 1 To FCount()
		If ("FILIAL" $ FieldName(nCntFor) )
			FieldPut(nCntFor,xFilial())
		Else
			FieldPut(nCntFor,M->&(EVAL(bCampo,nCntFor)))
		EndIf
	Next nCntFor
	
	aSort(aCols,,,{|x,y| x[1] < y[2] })
	nSeq := 1
	
	dbSelectArea("SZ2")
	dbSetOrder(1)
	RecLock("SZ2",.T.)
	SZ2->Z2_FILIAL  	:= xFilial("SZ2")
	SZ2->Z2_CODPROD 	:= M->B1_COD
	SZ2->Z2_DESPROD     := M->B1_DESC
	SZ2->Z2_CODFABR     := "INTERN"
	SZ2->Z2_CODPECA     := M->B1_COD
	SZ2->Z2_MSBLQL      := M->B1_MSBLQL
	SZ2->Z2_LOCAL       := M->B1_LOCPAD
	MSUNLOCK()
	
	For nCntFor := 1 To Len(aCols)
		nPosField := aScan(aHeader,{|x| Trim(x[2])=="Z2_CODFABR" })
		If ( !aCols[nCntFor][nUsado] .And. !Empty(aCols[nCntFor,nPosField]) )
			RecLock("SZ2",.T.)
			For nCntFor2 := 1 To Len(aHeader)
				nPosField := FieldPos(Trim(aHeader[nCntFor2,2]))
				If nPosField <> 0
					FieldPut(nPosField,aCols[nCntFor,nCntFor2])
				EndIf
			Next nCntFor2
			SZ2->Z2_FILIAL  	:= xFilial("SZ2")
			SZ2->Z2_CODPROD 	:= M->B1_COD
			SZ2->Z2_DESPROD     := M->B1_DESC
			SZ2->Z2_MSBLQL      := M->B1_MSBLQL
			SZ2->Z2_LOCAL       := M->B1_LOCPAD
			nSeq ++
		EndIf
	Next nCntFor
	lGravou:=.t.
EndIf
If ( nOpc == 2 ) //ALTERACAO
	nSeq := 1
	nPosPrd := aScan(aHeader,{|x| Trim(x[2])=="Z2_CODFABR" })
	For nCntFor := 1 To Len(aCols)
		dbSelectArea("SZ2")
		dbSetOrder(1)
		//dbSeek(xFilial("SZ2")+M->B1_COD+aCols[nCntFor,nPosPrd]+StrZero(nCntFor,2),.F.)
		If ( nCntFor <= Len(aAltera) )
			dbGoto(aAltera[nCntFor])
			RecLock("SZ2",.F.)
			If ( aCols[nCntFor][nUsado] .Or. Empty(aCols[nCntFor,nPosPrd]) )
				dbDelete()
			EndIf
		Else
			If ( !aCols[nCntFor][nUsado] .And. !Empty(aCols[nCntFor,nPosPrd]) )
				RecLock("SZ2",.T.)
			EndIf
		EndIf
		If ( !aCols[nCntFor][nUsado] .And. !Empty(aCols[nCntFor,nPosPrd]) )
			For nCntFor2 := 1 To Len(aHeader)
				nPosField := FieldPos(Trim(aHeader[nCntFor2,2]))
				If nPosField <> 0
					FieldPut(nPosField,aCols[nCntFor,nCntFor2])
				EndIf
			Next nCntFor2
			SZ2->Z2_CODPROD 	:= M->B1_COD
			SZ2->Z2_DESPROD     := M->B1_DESC
			SZ2->Z2_MSBLQL      := M->B1_MSBLQL
			SZ2->Z2_LOCAL       := M->B1_LOCPAD
			nSeq ++
		EndIf
	Next nCntFor
	dbSelectArea("SB1")
	RecLock("SB1",.F.)
	For nCntFor := 1 To FCount()
		If ("FILIAL" $ FieldName(nCntFor) )
			FieldPut(nCntFor,xFilial())
		Else
			FieldPut(nCntFor,M->&(EVAL(bCampo,nCntFor)))
		EndIf
	Next
	lGravou:=.t.
EndIf
If ( nOpc == 3 )
	nSeq := 1
	dbSelectArea("SZ2")
	dbSetOrder(1)
	dbSeek(xFilial("SZ2")+M->B1_COD,.T.)
	While ( 	!Eof() .And. xFilial("SZ2") == SZ2->Z2_FILIAL .And.;
		SZ2->Z2_CODPROD == M->B1_COD )
		RecLock("SZ2")
		dbDelete()
		dbSkip()
	EndDo
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Deleta o Cabecario da Sugestao de Orcamento          ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	dbSelectArea("SB1")
	RecLock("SB1")
	dbDelete()
	lGravou := .T.
EndIf
MsUnLockAll()
Return(lGravou)

User Function BSB1Prod()
Local aArea	   := GetArea()
Local lRetorno := .T.
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Posiciona Registros                                  ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
dbSelectArea("SB1")
dbSetOrder(1)
dbSeek(xFilial("SB1")+M->B1_COD,.T.)
dbSelectArea("SG1")
dbSetOrder(1)
dbSeek(xFilial()+M->B1_COD,.T.)
If ( xFilial("SG1") == SG1->G1_FILIAL .And. M->B1_COD == SG1->G1_COD )
	Help(" ",1,"BSB1PROD01")
	lRetorno := .F.
Else
	M->BG_DESCRI := SB1->B1_DESC
EndIf
RestArea(aArea)
Return(lRetorno)

User Function BSB1Comp()

Local aArea	   := { Alias(),IndexOrd(),Recno() }
Local lRetorno := .T.
Local nPosDesc := 0
Local nPosCmp  := 0
Local nTamanho := 0

nPosDesc := aScan(aHeader,{|x| AllTrim(x[2])=="Z2_DESCRI" })
nPosCmp  := aScan(aHeader,{|x| AllTrim(x[2])=="Z2_CODCOMP" })
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Posiciona Registros                                  ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
dbSelectArea("SB1")
dbSetOrder(1)
dbSeek(xFilial("SB1")+M->Z2_CODCOMP,.T.)
If ( lRetorno .And. M->Z2_CODCOMP == M->B1_COD )
	Help(" ",1,"BSB1COMP01")
	lRetorno := .F.
EndIf
dbSelectArea("SB1")
dbSetOrder(1)
dbSeek(xFilial()+M->Z2_CODCOMP)
If ( lRetorno .And. Found() )
	Help(" ",1,"BSB1COMP02")
	lRetorno := .F.
EndIf
If ( lRetorno .And. ALTERA .And. M->Z2_CODCOMP != aCols[n,nPosCmp] .And.;
	!Empty(aCols[n,nPosCmp]) )
	Help(" ",1,"BSB1COMP03")
	lRetorno := .F.
EndIf
If ( nPosDesc != 0 .And. lRetorno)
	nTamanho := Len(aCols[n][nPosDesc])
	aCols[n][nPosDesc] := PadL(SB1->B1_DESC,nTamanho)
EndIf
oGet:Refresh()
RestArea(aArea)
Return(lRetorno)

User Function BSB1Descri(cCampo)
Local aArea		:= GetArea()
Local cRetorno  := ""

cCampo := AllTrim(cCampo)
If ( !Eof() )
	dbSelectArea("SB1")
	dbSetOrder(1)
	If ( cCampo == "BG_DESCRI" .And. !INCLUI )
		dbSeek(xFilial()+SB1->B1_COD,.T.)
		If ( Found() )
			cRetorno := SB1->B1_DESC
		Endif
	EndIf
	If ( cCampo == "Z2_DESCRI" .And. !INCLUI )
		dbSeek(xFilial()+GdFieldGet("Z2_CODCOMP",Len(aCols)),.T.)
		If Found()
			cRetorno := SB1->B1_DESC
		EndIf
	EndIf
EndIf
cRetorno := PadL(cRetorno,TamSX3(cCampo)[1])
RestArea(aArea)
Return(cRetorno)

User Function BSB1Novo(cAlias,nReg,nOpc)
Local areax:=getarea()
Local nOpcA     := 0
Local nUsado    := 0
Local nCntFor   := 0
Local oDlg
Local lContinua := .T.
Local aColsbkp		:= aclone(acols)
Local aHeaderbkp	:= aclone(aHeader)
Local cAliasBkp		:= cAlias
Local nNbkp			:= N
N:=1
PRIVATE aPos   := {  15,  1, 70, 315 }
Private aTela[0][0]
Private aGets[0]
Private aHeader := {}
Private aCols   := {}
Private bCampo  := { |nField| FieldName(nField) }
cAlias:="SB1"
nReg:=0
nOpc:=3
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Cria Variaveis de Memoria da Enchoice                ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
dbSelectArea("SB1")
For nCntFor:= 1 To FCount()
	M->&(EVAL(bCampo,nCntFor)) := CriaVar(FieldName(nCntFor),.T.)
Next nCntFor

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Monta o aHeader                                       ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
aHeader := {}
dbSelectArea("SX3")
dbSetOrder(1)
dbSeek("SZ2",.T.)

nUsado := 0
While ( !Eof() .And. SX3->X3_ARQUIVO == "SZ2" )
	If ( X3USO(SX3->X3_USADO) .And. cNivel >= SX3->X3_NIVEL )
		If !(AllTrim(SX3->X3_Campo) $ "Z2_CODPROD/Z2_DESPROD/Z2_LOCAL/Z2_MSBLQL")
			AADD(aHeader,{ AllTrim(X3Titulo()),;
			SX3->X3_CAMPO,;
			IIF(AllTrim(SX3->X3_Campo)=="Z2_CODFABR","@!",SX3->X3_PICTURE),;
			SX3->X3_TAMANHO,;
			SX3->X3_DECIMAL,;
			SX3->X3_VALID,;
			SX3->X3_USADO,;
			SX3->X3_TIPO,;
			SX3->X3_ARQUIVO,;
			SX3->X3_CONTEXT } )
			nUsado++
		ENDIF
	Endif
	dbSkip()
EndDo
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Monta o aCols                                         ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
aCols := {}
aadd(aCols,Array(nUsado+1))
dbSelectArea("SX3")
dbSeek("SZ2")
nUsado := 0
While ( !Eof() .And. SX3->X3_ARQUIVO == "SZ2" )
	If ( X3USO(SX3->X3_USADO) .And. cNivel >= SX3->X3_NIVEL )
		If !(AllTrim(SX3->X3_Campo) $ "Z2_CODPROD/Z2_DESPROD/Z2_LOCAL/Z2_MSBLQL")
			nUsado++
			aCols[1][nUsado] := CriaVar(allTrim(SX3->X3_CAMPO),.T.)
		Endif
	Endif
	dbSkip()
End
aCols[1][nUsado+1] := .F.
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Envia para processamento dos Gets          ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
nOpcA:=0
dbSelectArea("SB1")
aSize := MsAdvSize()
aObjects := {}
AAdd( aObjects, { 100,100,.T.,.T. } )
AAdd( aObjects, { 100,100,.T.,.T. } )
aInfo := { aSize[1],aSize[2],aSize[3],aSize[4],3,3}
aPosObj := MsObjSize( aInfo, aObjects )
DEFINE MSDIALOG oDlg TITLE OemToAnsi( "Produto x Codigo de Fabricante" ) From ;
aSize[7],0 To aSize[6],aSize[5] OF oMainWnd PIXEL

EnChoice( cAlias, SB1->(RECNO()), nOpc, , , , , aPosObj[1], , 3 )

oGet := MSGetDados():New( aPosObj[2,1],aPosObj[2,2],aPosObj[2,3],aPosObj[2,4], nOpc,"u_BSB1LinOk","u_BSB1TudOk", , .t. )

ACTIVATE MSDIALOG oDlg ON INIT EnchoiceBar( oDlg,{ || nOpca := 1,if(u_BSB1TudOk(),oDlg:End(),.t.) }, {|| nOpca := 0, oDlg:End()})



DbSelectArea("SB1")
dbGoto(nReg)
If ( nOpcA == 1 )
	Begin Transaction
	If ( BSB1Grava(1) )
		dbSelectArea("SZ5")
		SZ5->Z5_DESCRI := alltrim(substr(M->B1_COD,5))
		MSUNLOCK()
		dbSelectArea("SB1")
	EndIf
	End Transaction
Else
	dbSelectArea("SZ5")
	MSUNLOCK()
	dbSelectArea("SB1")
EndIf
acols			:= aColsbkp
aHeader 		:= aHeaderbkp
cAlias			:= cAliasbkp
N 				:= nNbkp
return(	SZ2->Z2_CODPROD,SZ2->Z2_DESPROD,SZ2->Z2_CODFABR,SZ2->Z2_CODPECA,SZ2->Z2_LOCAL )

User Function BSB1Vis
local aarea:=getarea()
PRIVATE cCadastro := OemToAnsi("Produtos x Fabricantes")
PRIVATE aRotina := { { "Pesquisar","AxPesqui"  ,0,1},;
{ "Visualizar","u_BSB1Visual",0,2},;
{ "Incluir","u_BSB1Inclui",0,3},;
{ "Alterar","u_BSB1Altera",0,4,20},;
{ "Excluir","u_BSB1Deleta",0,5,21}}

dbSelectArea("SB1")
dbsetorder(1)
dbseek(XFILIAL("SB1")+SZ2->Z2_CODPROD)
u_BSB1Visual("SB1",SB1->(RECNO()),2)
restarea(aarea)
RETURN(.F.)           //SE RETORNO = .T., PASSA DIRETO , SENAO VOLTA PARA TELA DO F3

User Function NextB1Cod
Local aArea:=GetArea()
Local lRet:=.t.
Local cRet
IF INCLUI
	dbSelectArea("SZ5")
	dbSetOrder(1)
	If dbSeek(xFilial("SZ5")+M->B1_GRUANAL)
		If RecLock("SZ5",Eof())
			cRet    :=M->B1_GRUANAL
			cDescri :=SZ5->Z5_DESCRI // Jose Antonio 03/12/2012
			dbSelectArea("SB1")
			while dbSeek(xFilial("SB1")+cRet+cDescri)
				dbSelectArea("SZ5")
				dbSetOrder(1)
				If dbSeek(xFilial("SZ5")+cRet+cDescri)
				   cDescri:=SOMA1(cDescri) // Jose Antonio 03/12/2012
				Endif   
				dbSelectArea("SB1")
			enddo  
			M->B1_COD:=M->B1_GRUANAL+cDescri
		//	if vAntigo # cRet
		//		MsgBox("Código do Produto foi alterado para "+cRet+".","INFORMAÇÃO")
		//	endif    
		else
			MsgBox("Grupo em uso por outro usuário. Tente mais tarde!","BROWSB1")
			lRet:=.f.
		endif
	else
		MsgBox("Grupo Analítico não encontrado!","BROWSB1")
		lRet:=.f.
	endif
ENDIF
restarea(aArea)
Return(lRet)
