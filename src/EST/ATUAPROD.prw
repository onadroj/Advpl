#include "rwmake.ch"
#INCLUDE "Topconn.ch"

User Function ATUAPROD()

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡ao    ³ATUAPROD  ³ Autor ³GATASSE                ³ Data ³05.05.03  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡„o ³VISUALIZA POSICAO DO ESTOQUE DE TODAS AS EMPRESAS           ³±±
±±³          ³                                                            ³±±
±±³          ³                                                            ³±±
±±³          ³                                                            ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Uso       ³VISUALIZACAO                                                ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Tabelas   ³SB2                                                         ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Modulo    ³                                                            ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
if ParamIXB == "E"
	Processa({|| RunCont() },"Criando consulta SQL. Aguarde...")
ELSE
	Equivale()
ENDIF
Return

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºFun‡„o    ³ RUNCONT  º Autor ³ AP5 IDE            º Data ³  20/03/03   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescri‡„o ³ FUNCAO MOSTRAR ESTOQUE DE PRODUTOS                         º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³                                                            º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

Static Function RunCont

nOpcE:=2
nOpcG:=2

AREAB1:=GETAREA()
DBSELECTAREA("SM0")
AREASM0:=GETAREA()
DBSETORDER(1)
DBGOTOP()
_VET:={}
WHILE !EOF() //CARREGA EMPRESAS
	IF ASCAN(_VET,SM0->M0_CODIGO)==0 .AND. SM0->M0_CODIGO $ "02/03/04/05/06/07/08"
		AADD(_VET,SM0->M0_CODIGO)
	Endif
	DBSKIP()
ENDDO
RESTAREA(AREASM0)
DBSELECTAREA("SB1")
DBSETORDER(1)
DBSEEK(XFILIAL("SB1")+SZ2->Z2_CODPROD)
SZ2->Z2_CODPROD
Inclui := .T.
cQuery := ""
FOR X:=1 TO LEN(_VET)
	IF X<>1
		cQuery += " UNION "
	ENDIF
	cQuery += " SELECT '"+_VET[X]+"' AS EMPRESA,B2_FILIAL, B2_LOCAL, B2_QATU, B2_CM1, B2_VATU1 FROM SB2"+_VET[X]+"0 "
	cQuery += " WHERE D_E_L_E_T_ <> '*' AND B2_COD='"+ALLTRIM(SZ2->Z2_CODPROD)+"' "
	
NEXT

TCQUERY cQuery NEW ALIAS "QRY"

/* Opcao de acesso para o Modelo 2
- 3,4 Permitem alterar getdados e incluir linhas
- 6 So permite alterar getdados e nao incluir linhas
- Qualquer outro numero so visualiza
*/


/* Montando aHeader e aCols*/

dbSelectArea("SX3")
dbSetOrder(1)
dbSeek("SB2")

aHeader   := {}
cChvValid := ""

nUsado    := 1
Aadd(aHeader,{"Empresa", "EMPRESA"  , "@!" ,;
2, 0, cChvValid       ,;
""  , "C", "QRY" ,""})

While (! Eof())
	IF    ((AllTrim(SX3->X3_Campo) == "B2_FILIAL") .Or. ;
		(AllTrim(SX3->X3_Campo) == "B2_LOCAL") .Or. ;
		(AllTrim(SX3->X3_Campo) == "B2_QATU") .Or. ;
		(AllTrim(SX3->X3_Campo) == "B2_CM1") .Or. ;
		(AllTrim(SX3->X3_Campo) == "B2_VATU1"))
		nUsado    := nUsado + 1
		Aadd(aHeader,{TRIM(SX3->X3_titulo), SX3->X3_campo  , SX3->X3_picture ,;
		SX3->X3_tamanho, SX3->X3_decimal, cChvValid       ,;
		SX3->X3_usado  , SX3->X3_tipo   , "QRY" , SX3->X3_context})
	EndIf
	dbSkip()
End

/* Montando aCols */

aCols     := {}
dbSelectArea("QRY")
COUNT TO _RECCOUNT
IF _RECCOUNT==0
	MSGSTOP("Não há itens em estoque para o produto "+ALLTRIM(SZ2->Z2_CODPROD)+".")
	Close
	RESTAREA(AREAB1)
	RETURN
ENDIF
dbGoTop()
While ! Eof()
	Aadd(aCols,Array(nUsado+1))
	For _ni:=1 To nUsado
		aCols[Len(aCols),_ni] := QRY->(FieldGet(QRY->(FieldPos(aHeader[_ni,2]))))
	Next
	aCols[Len(aCols),nUsado+1] := .f.
	dbSkip()
EndDo

dbSelectArea("SX3")
dbSeek("SB1")

aCpoEnchoice:={}

While (! Eof()) .And. (SX3->X3_Arquivo == "SB1")
	
	If X3USO(SX3->X3_Usado) .And. (cNivel >= SX3->X3_Nivel)
		Aadd(aCpoEnchoice,SX3->X3_Campo)
	Endif
	
	dbSelectArea("SX3")
	dbSkip()
EndDo

If (Len(aCols) > 0)
	
	cTitulo        := "CONSULTAR ESTOQUE INTEGRADO"
	cAliasEnchoice := "SB1"
	cAliasGetD     := "QRY"
	cLinOk         := "AllwaysTrue()"
	cTudOk         := "AllwaysTrue()"
	cFieldOk       := "AllwaysTrue()"
	
	_lRet:=Modelo3(cTitulo,cAliasEnchoice,cAliasGetD,aCpoEnchoice,cLinOk,cTudOk,nOpcE,nOpcG,cFieldOk)
Endif
DbSelectArea("QRY")
Close
RESTAREA(AREAB1)
Return


/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºFun‡„o    ³ EQUIVALE º Autor ³ GATASSE            º Data ³  20/03/03   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescri‡„o ³ MOSTRA EQUIVALENTES                                        º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³                                                            º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

Static Function Equivale
/* Montando aHeader e aCols para multiline*/
nOpcE:=2
nOpcG:=2
areax:=getarea()
DBSELECTAREA("SB1")
DBSETORDER(1)
DBSEEK(XFILIAL("SB1")+SZ2->Z2_CODPROD)

dbSelectArea("SX3")
dbSetOrder(1)
dbSeek("SZ2")
aHeader   := {}
cChvValid := ".F."
While (! Eof()) .and. SX3->X3_ARQUIVO=="SZ2"
	IF    (	(AllTrim(SX3->X3_Campo) == "Z2_CODFABR") .Or. ;
		(AllTrim(SX3->X3_Campo) == "Z2_CODPECA") )
		Aadd(aHeader,{TRIM(SX3->X3_titulo), SX3->X3_campo  , SX3->X3_picture ,;
		SX3->X3_tamanho, SX3->X3_decimal, cChvValid       ,;
		SX3->X3_usado  , SX3->X3_tipo   , "SZ2" , SX3->X3_context})
	EndIf
	dbSkip()
End


nUsado:=LEN(aHeader)
aCols:={}

dbSelectArea("SZ2")
dbSetOrder(1)
dbSeek(xFilial("SZ2")+SB1->B1_Cod)

While (! Eof())                           .And. ;
	(SZ2->Z2_Filial  == xFilial("SZ2")) .And. ;
	(SZ2->Z2_CodProd == SB1->B1_Cod)

	Aadd(aCols,Array(nUsado+1))
	For _ni:=1 To nUsado
		aCols[Len(aCols),_ni] := FieldGet(FieldPos(aHeader[_ni,2]))
	Next
	aCols[Len(aCols),nUsado+1]:=.F.
	dbSkip()
End

dbSelectArea("SX3")
dbSeek("SB1")

aCpoEnchoice:={}

While (! Eof()) .And. (SX3->X3_Arquivo == "SB1")
	If X3USO(SX3->X3_Usado) .And. (cNivel >= SX3->X3_Nivel)
		Aadd(aCpoEnchoice,SX3->X3_Campo)
	Endif
	dbSelectArea("SX3")
	dbSkip()
EndDo


If (Len(aCols) > 0)
	cTitulo        := "CONSULTAR EQUIVALENTES"
	cAliasEnchoice := "SB1"
	cAliasGetD     := "SZ2"
	cLinOk         := "AllwaysTrue()"
	cTudOk         := "AllwaysTrue()"
	cFieldOk       := "AllwaysTrue()"
	
	_lRet:=Modelo3(cTitulo,cAliasEnchoice,cAliasGetD,aCpoEnchoice,cLinOk,cTudOk,nOpcE,nOpcG,cFieldOk)
Endif
restarea(areax)
return
//28040152
