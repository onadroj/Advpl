#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³MT100LOK  º Autor ³ GATASSE            º Data ³  05/11/04   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ VALIDA LINHA NA NFE                                        º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP6 IDE                                                    º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User Function MT100LOK

LOCAL lRet  := .T.
LOCAL cERRO := 0
LOCAL nPos1 := (AScan(aHeader,{|aItem| UPPER(AllTrim(aItem[2]))=="D1_OP"}))
LOCAL nPos2 := (AScan(aHeader,{|aItem| UPPER(AllTrim(aItem[2]))=="D1_TES"}))
LOCAL nPos3 := (AScan(aHeader,{|aItem| UPPER(AllTrim(aItem[2]))=="D1_TPENT"}))
LOCAL nPos4 := (AScan(aHeader,{|aItem| UPPER(AllTrim(aItem[2]))=="D1_CC"}))
LOCAL nPos5 := (AScan(aHeader,{|aItem| UPPER(AllTrim(aItem[2]))=="D1_ITEMCTA"}))
LOCAL nPos6 := (AScan(aHeader,{|aItem| UPPER(AllTrim(aItem[2]))=="D1_CLVL"}))
LOCAL cTipo:=""
LOCAL cCC:=""
LOCAL aArea
Local cCusto := SubStr(GdFieldGet("D1_CC"),1,1)
//Local cRateio := SubStr(GdFieldGet("D1_RATEIO"),1,1)
Local _cCntObr:=""
Local _nPosProd:=aScan( aHeader, { |aItem| Alltrim(aItem[2])=="D1_COD" })
Local _nPosPed:=aScan( aHeader, { |aItem| Alltrim(aItem[2])=="D1_PEDIDO" })
cTipo:=Retfield("SF4",1,XFILIAL("SF4")+ACOLS[N][nPos2],"F4_OSOPOBR")

aArea:=GetArea()
DbSelectArea("STJ")
DbSetOrder(1)
DbSeek(XFILIAL("STJ")+SUBSTR(ACOLS[N][nPos1],1,6),.T.)
cCC:=STJ->TJ_CCUSTO
RestArea(aArea)
if ACOLS[N][len(acols[1])]==.f.  //NAO DELETADO
	IF ALLTRIM(ACOLS[N][nPos1])=="" .AND. cTipo=="1"
		cERRO:=1
	endif
	IF ALLTRIM(ACOLS[N][nPos1])<>"" .AND. cTipo<>"1"
		cERRO:=1
	endif
	IF !(ALLTRIM(ACOLS[N][nPos3]) $ "CDY") .AND. cTipo=="1"
		cERRO:=1
	endif
	IF ALLTRIM(ACOLS[N][nPos3])<>"" .AND. cTipo<>"1"
		cERRO:=1
	endif
	IF ALLTRIM(ACOLS[N][nPos4])<>ALLTRIM(cCC) .AND. cTipo=="1"
		cERRO:=1
	endif                    
//	If cEmpAnt=="03" .OR. cEmpAnt=="04" .OR. cEmpAnt=="08" .OR. cEmpAnt=="06" .OR. cEmpAnt=="99"
	If cEmpAnt=="03" .OR. cEmpAnt=="99"
		If !(cTipo$"DB") .AND. (!LNFMEDIC .OR. Empty(aCols[N,_nPosPed]))
			_cCntObr:=RetField("SA2",1,xFilial("SA2")+cA100For+cLoja,"A2_CNTOBR")   
			If _cCntObr=="1"  
				LNFMEDIC:=.T.
				cErro:=2
			Endif
			_cCntObr:=RetField("SB1",1,xFilial("SB1")+aCols[N,_nPosProd],"B1_CNTOBR")   
			If _cCntObr=="1"
				LNFMEDIC:=.T.
				cErro:=3
			Endif
		Endif
	Endif
endif

IF cERRO>0
	if cErro==1
		msgstop("Incompatibilidade do Tipo de Movimentação com a Numeração da OP, ou"+chr(13)+chr(10)+ ;
	        "do Tipo de Entrada com a utilização de OP, ou"+chr(13)+chr(10)+ ;
	        "do Centro de Custo utilizado com o Centro de Custo da OP."+chr(13)+chr(10)+ ;
	        "Verifique a linha "+ALLTRIM(STR(N))+"!" )
    ElseIf cErro==2
		MsgStop ("Para este fornecedor é exigida utilização de contrato."+chr(13)+chr(10) ;
				+"O campo Filtra Medição deve estar marcado e todos os itens " +chr(13)+chr(10) ;
			 	+"devem ser associados a pedidos de compra gerados por medições.")
    ElseIf cErro==3
		MsgStop ("Para este produto é exigida utilização de contrato."+chr(13)+chr(10) ;
				+"O campo Filtra Medição deve estar marcado e todos os itens " +chr(13)+chr(10) ;
			 	+"devem ser associados a pedidos de compra gerados por medições.")
	Endif
	lRet:=.f.
ENDIF                 

//INCLUIDO POR JORDANO EM 11/07/14 PARA VALIDAR A CLASSE DE VALOR

If cCusto == "3" // Se a primeira posição do centro de custo for igual a 3
	If Empty(GdFieldGet("D1_CLVL")) // Se estiver vazia a classe de valor, não deixa passar...
		Msgbox("Para Centros de Custo de Equipamentos deve-se informar a classe de valor correspondente.","MT100LOK","ALERT")
		lRet := .F.
	End If
End If
//If cRateio == "2" // Quando não há rateio deve-se informar o centro de custo
//	If Empty(GdFieldGet("D1_CC")) // Se estiver vazio o centro de custo, não deixa passar...
//		Msgbox("Centro de Custo não informado.","MT100LOK","ALERT")
//		lRet := .F.
//	End If
//End If
//FIM DA INCLUSAO POR JORDANO EM 11/07/14 PARA VALIDAR A CLASSE DE VALOR
Return(lRet)
