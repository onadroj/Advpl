#INCLUDE "rwmake.ch"
#INCLUDE "topconn.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³MT100GE2  º Autor ³ GATASSE            º Data ³  22/11/04   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ PONTO DE ENTRADA APOS GRAVAR SE2 NA NOTA DE ENTRADA        º±±
±±º          ³ ALTERA TITULOS PARA FLUXO NAO CASO A CONDICAO TENHA SIDO CXº±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP6 IDE                                                    º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User Function MT100GE2

LOCAL cDoc := SD1->D1_DOC
LOCAL cSerie := SD1->D1_SERIE
LOCAL cFornece := SD1->D1_FORNECE
LOCAL cLoja := SD1->D1_LOJA
LOCAL cOSOPObr
LOCAL _NRORPGT
LOCAL _cContratos
Private _aPedidos := {}

aArea:=GetArea()
dbSelectArea("SD1")
dbSetOrder(1)
dbGoTop()
dbSeek(xFilial("SD1")+cDoc+cSerie+cFornece+cLoja,.T.)

do while !EOF() .AND. SD1->D1_DOC+SD1->D1_SERIE+SD1->D1_FORNECE+SD1->D1_LOJA==cDoc+cSerie+cFornece+cLoja

	//Caso o documento tenha pedido(s), busca no(s) pedido(s) os dados do(s) contrato(s) (Gestão de Contratos) para gravação no título em SE2
//	If (cEmpAnt=="03" .OR. cEmpAnt=="04" .OR. cEmpAnt=="08" .OR. cEmpAnt=="99") .AND. !Empty(SD1->D1_PEDIDO)
	If (cEmpAnt<>"02" .AND. cEmpAnt<>"05" .AND. cEmpAnt<>"07" ) .AND. !Empty(SD1->D1_PEDIDO)
		_nPos := aScan(_aPedidos,{ |_Item| _Item[1] == SD1->D1_PEDIDO})
		If _nPos == 0 
		   BuscaPedido(SD1->D1_PEDIDO)
		EndIf
	Endif

  cOSOPObr := RETFIELD("SF4",1,XFILIAL("SF4")+SD1->D1_TES,"F4_OSOPOBR")
  if cOSOPObr<>"1"
     dbSkip()
     loop
  endif
  dbSelectArea("SD3")
  dbSetOrder(8)
  dbSeek(xFilial("SD3")+SD1->D1_DOC+SD1->D1_NUMSEQ)
  if !EOF()
    RecLock("SD3",.F.)
    Replace SD3->D3_TPENT with SD1->D1_TPENT
    MSUnlock()
  endif
  dbSelectArea("SD1")
  dbSkip()
enddo
dbSkip(-1)

dbSelectArea("SE2")
RECLOCK("SE2",.F.)
REPLACE SE2->E2_FLUXO WITH "S"
replace E2_BCOPREF with SA2->A2_BANCO
IF "CX" $ SF1->F1_COND
	REPLACE SE2->E2_FLUXO WITH "N"
	REPLACE SE2->E2_BCOPREF WITH "CX "
	replace E2_CTABAIX with cbco+cag+cConta
    IF GETMV("MV_CTLIPAG") .AND. (cEmpAnt=="03" .OR. cEmpAnt=="99") // Caso utilize a liberação de títulos para baixa, grava títulos já pagos pelos CAIXAS como liberados
	   REPLACE SE2->E2_DATALIB WITH dDataBase
	ENDIF
ELSEIF ISALPHA(SF1->F1_COND)
	REPLACE SE2->E2_BCOPREF WITH SF1->F1_COND
ENDIF     
IF ALLTRIM(SE2->E2_TIPO)=="NF" .AND. SE2->E2_IRRF <>0 
	REPLACE SE2->E2_DIRF WITH "1" 
	REPLACE SE2->E2_CODRET WITH SA2->A2_CODRET
ENDIF
MSUNLOCK()


	//Caso não seja um título de retenção de imposto,  incrementa e grava o número da Ordem de Pagamento para o título 
	//Grava, caso existam, os dados dos contratos (Gestão de Contratos) relacionados ao título
If (cEmpAnt=="03" .OR. cEmpAnt=="04" .OR. cEmpAnt=="08" .OR. cEmpAnt=="06" .OR. cEmpAnt=="99")
	_cContratos := ""
	For i:=1 to len(_aPedidos)
	   _cContratos += _aPedidos[i][2] + "/" + _aPedidos[i][3] + ","
       GravaDocs(_aPedidos[i][2],_aPedidos[i][3])
	Next
 	_cContratos := Substr(_cContratos,1,Len(_cContratos)-1)
	RecLock("SE2",.F.)
	SE2->E2_CNTRATS := _cContratos
	MsUnlock()

	If !Type("_lImpOP")=="U" .AND. SUBSTR(SE2->E2_TIPO,3,1) <> "-"
		_NRORPGT := GetSXENum("SE2","E2_NRORPGT")
		ConfirmSX8()
		RecLock("SE2",.F.)
		SE2->E2_NRORPGT := _NRORPGT
		MsUnlock()
    Endif
Endif


RestArea(aArea)    
cbco:=nil
cag:=nil
cConta:=nil
Return

Static Function BuscaPedido(_NumPed)
Local _aAreaX := GetArea()

DbSelectArea("SC7")
DbSetOrder(1) //C7_FILIAL+C7_NUM+C7_ITEM+C7_SEQUEN
If DbSeek(xFilial("SC7")+_NumPed,.T.)
	If !Empty(SC7->C7_CONTRA)
		aAdd(_aPedidos,{_NumPed,SC7->C7_CONTRA,SC7->C7_CONTREV})
	Endif
Endif

RestArea(_aAreaX)

Return

Static Function GravaDocs(_cContrato,_cRevisao)
Local _cTpcto := ""
Local _cQuery := ""
Local _aArea := GetArea() 

_cTpcto := RetField("CN9",1,xFilial("CN9")+_cContrato+_cRevisao,"CN9_TPCTO")

_cQuery := "SELECT ZZ_DOC FROM "+RetSqlName("SZZ")+" WHERE "
_cQuery += "ZZ_TPCONT='"+_cTpcto+"' AND D_E_L_E_T_ <> '*'"

TCQUERY _cQuery NEW ALIAS "QRYDOCS"
DbSelectArea("QRYDOCS")
DbGoTop()
If !Eof()
	RecLock("SE2",.F.)
	SE2->E2_TEMDOCS := "1"
	MsUnlock()
Endif

Do While !Eof()
	DbSelectArea("FRD")
	DbSetOrder(1) //FRD_FILIAL+FRD_PREFIX+FRD_NUM+FRD_PARCEL+FRD_TIPO+FRD_FORNEC+FRD_LOJA+FRD_DOCUM
	If !DbSeek(xFilial("FRD")+SE2->E2_PREFIXO+SE2->E2_NUM+SE2->E2_PARCELA+SE2->E2_TIPO+SE2->E2_FORNECE+SE2->E2_LOJA+QRYDOCS->ZZ_DOC)
		RecLock("FRD",.T.)
		FRD->FRD_FILIAL:=xFilial("FRD")
		FRD->FRD_PREFIX:=SE2->E2_PREFIXO
		FRD->FRD_NUM:=SE2->E2_NUM
		FRD->FRD_TIPO:=SE2->E2_TIPO
		FRD->FRD_PARCEL:=SE2->E2_PARCELA
		FRD->FRD_FORNEC:=SE2->E2_FORNECE
		FRD->FRD_LOJA:=SE2->E2_LOJA
		FRD->FRD_DOCUM:=QRYDOCS->ZZ_DOC
		FRD->FRD_RECEB:="2"
		MsUnlock()
	Endif
	DbSelectArea("QRYDOCS")
	DbSkip()
EndDo
CLOSE
RestArea(_aArea)

Return()        

