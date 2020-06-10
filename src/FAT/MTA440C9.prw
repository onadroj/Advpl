#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³MTA440C9  º Autor ³ GATASSE            º Data ³  03/11/03   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ APOS LIBERACAO DE PEDIDOS NO FATURAMENTO                   º±±
±±º          ³ GRAVA VALOR BASE ISS COM REDUCAO DE MATERIAIS EM SC5.      º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP5 IDE                                                    º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/


User Function MTA440C9()
LOCAL _BASEISS
aarea:=getarea()
DBSELECTAREA("SC9")
aareasc9:=getarea()
_BASEISS:=0
DBSETORDER(1)
DBSEEK(XFILIAL("SC9")+SC5->C5_NUM,.T.)
WHILE !EOF() .AND. XFILIAL("SC9")==SC9->C9_FILIAL .AND. SC9->C9_PEDIDO==SC5->C5_NUM
	_tes:=RETFIELD("SC6",1,XFILIAL("SC6")+SC9->C9_PEDIDO+SC9->C9_ITEM,"C6_TES")
	AREA2:=GETAREA()
	DBSELECTAREA("SF4")
	DBSEEK(XFILIAL("SF4")+_TES)
	IF  SF4->F4_ISS=="S"
		_BASEISS+=SC9->C9_QTDLIB*SC9->C9_PRCVEN   
	ENDIF
	RESTAREA(AREA2)
	dbskip()
enddo
DBSELECTAREA("SC5")
RECLOCK("SC5",.F.)
REPLACE SC5->C5_BASEISS WITH _BASEISS
MSUnlock()
RESTAREA(aareasc9)
RESTAREA(aarea)
RETURN
