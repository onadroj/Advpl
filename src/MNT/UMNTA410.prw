#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³UMNTA410  º Autor ³ GATASSE            º Data ³  05/11/04   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ SUBSTITUI A ROTINA ORIGINAL DO SIGA MNTA410                º±±
±±º          ³ GRAVA INSUMOS DE MAO DE OBRA EM STL                       º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ MODULO DE MANUTENCAO                                       º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User Function UMNTA410  


//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Declaracao de Variaveis                                             ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
local avet:={}
//salva ultimo registro de stj e stl

DBSELECTAREA("STJ")
DBGOBOTTOM()
cOrdemSTJ:=STJ->TJ_ORDEM 
DBSELECTAREA("STL")
DBGOBOTTOM()
cOrdem:=STL->TL_ORDEM

MNTA410()           

//cobre campo TJ_SEQUENC com TJ_SEQRELA para novas OS

cQuery:="UPDATE "+RETSQLNAME("STJ")+" SET TJ_SEQUENC = CONVERT(INT, TJ_SEQRELA) "
cQuery+="WHERE TJ_ORDEM > '"+cOrdemSTJ+"' AND D_E_L_E_T_<>'*' "
nret:=TCSQLExec(cQuery)
IF nret<> 0
	msgstop("Ocorreu um erro com codigo "+str(nret)+" na atualizacao de STJ. ")
endif

DBSELECTAREA("STL")
dbsetorder(1)
dbseek(xfilial("STL")+cOrdem,.t.)
dbskip()
npp:=0
While !eof() .AND. STL->TL_ORDEM >= cOrdem
    If STL->TL_PLANO=="000000"
       dbskip()
       loop
    Endif
	AADD(Avet,Array(FCount()))
	npp++
	FOR nFieldx:=1 TO FCount()
		cMacrMemo := "STL->" + FieldName(nFieldx)
		aVet[Npp,nFieldx]:=&cMacrMemo
		IF cMacrMemo=="STL->TL_SEQUENC"
			aVet[Npp,nFieldx]:=1
		endif
		IF cMacrMemo=="STL->TL_REPFIM"
			aVet[Npp,nFieldx]:="S"
		endif
		IF cMacrMemo=="STL->TL_DTDIGIT"
			aVet[Npp,nFieldx]:=DDATABASE
		endif
	NEXT
	dbskip()
Enddo
FOR X:=1 TO LEN(aVet)
	IF aVet[x][6]=="M"
		reclock("STL",.T.)
		For nFieldx := 1 To FCount()
			FieldPut(nFieldx,aVet[X,nFieldx])
			IF  FieldName(nFieldx)="TL_NUMSEQ"
				FieldPut(nFieldx,PROXNUM())				
			ENDIF
		Next
		MSUNLOCK()
		gravaSD3()
	ENDIF
NEXT
Return

STATIC FUNCTION GRAVASD3
LOCAL AAREA:=GETAREA()
LOCAL AAREAsd3
cProduto:="MOD"+RETFIELD("ST1",1,XFILIAL("ST1")+STL->TL_CODIGO,"T1_CCUSTO")
DBSELECTAREA("SD3")
AAREAsd3:=GETAREA()
RECLOCK("SD3",.T.)
REPLACE SD3->D3_FILIAL 	WITH XFILIAL("SD3")
REPLACE SD3->D3_TM		WITH "999"
REPLACE SD3->D3_COD		WITH cProduto
REPLACE SD3->D3_UM		WITH STL->TL_UNIDADE
REPLACE SD3->D3_QUANT	WITH STL->TL_QUANTID
REPLACE SD3->D3_CF		WITH "RE1"
REPLACE SD3->D3_OP		WITH STL->TL_ORDEM+"OS001"
REPLACE SD3->D3_LOCAL	WITH STL->TL_LOCAL
REPLACE SD3->D3_DOC		WITH STL->TL_ORDEM
REPLACE SD3->D3_EMISSAO	WITH STL->TL_DTINICI
REPLACE SD3->D3_GRUPO	WITH RETFIELD("SB1",1,XFILIAL("SB1")+cProduto,"B1_GRUPO")
REPLACE SD3->D3_CUSTO1	WITH STL->TL_CUSTO
REPLACE SD3->D3_CC		WITH RETFIELD("STJ",1,XFILIAL("STJ")+STL->TL_ORDEM,"TJ_CCUSTO")
REPLACE SD3->D3_TIPO	WITH RETFIELD("SB1",1,XFILIAL("SB1")+cProduto,"B1_TIPO")
REPLACE SD3->D3_USUARIO	WITH cUserName
REPLACE SD3->D3_CHAVE	WITH "E0"
REPLACE SD3->D3_ORDEM	WITH STL->TL_ORDEM
REPLACE SD3->D3_NUMSEQ	WITH STL->TL_NUMSEQ
/*
REPLACE D3_CONTA
REPLACE SD3->D3_CUSTO2
REPLACE SD3->D3_CUSTO3
REPLACE SD3->D3_CUSTO4
REPLACE SD3->D3_CUSTO5
REPLACE SD3->D3_PARCTOT
REPLACE SD3->D3_ESTORNO
REPLACE SD3->D3_SEGUM
REPLACE SD3->D3_QTSEGUM
REPLACE SD3->D3_PERDA
REPLACE SD3->D3_DTLANC
REPLACE SD3->D3_TRT
REPLACE SD3->D3_NIVEL
REPLACE SD3->D3_IDENT
REPLACE SD3->D3_SEQCALC
REPLACE SD3->D3_RATEIO
REPLACE SD3->D3_LOTECTL
REPLACE SD3->D3_NUMLOTE
REPLACE SD3->D3_DTVALID
REPLACE SD3->D3_LOCALIZ
REPLACE SD3->D3_NUMSERI
REPLACE SD3->D3_CUSFF1
REPLACE SD3->D3_CUSFF2
REPLACE SD3->D3_CUSFF3
REPLACE SD3->D3_CUSFF4
REPLACE SD3->D3_CUSFF5
REPLACE SD3->D3_ITEM
REPLACE SD3->D3_OK
REPLACE SD3->D3_ITEMCTA
REPLACE SD3->D3_CLVL
REPLACE SD3->D3_PROJPMS
REPLACE SD3->D3_TASKPMS
REPLACE SD3->D3_SERVIC
REPLACE SD3->D3_STSERV
*/

MSUNLOCK()
RESTAREA(AAREAsd3)
RESTAREA(AAREA)
RETURN
