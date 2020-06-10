#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณM410LIOK  บ Autor ณ GATASSE            บ Data ณ  10/01/05   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDescricao ณ VALIDA LINHA NO PEDIDO DE VENDA                            บฑฑ
ฑฑบ          ณ                                                            บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ AP6 IDE                                                    บฑฑ
ฑฑศออออออออออฯออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
/*/

User Function M410LIOK

LOCAL RET:=.T.
LOCAL cTipo
//VALIDA SE DIGITOU ALMOXARIFADO DE DESTINO PARA TES DE TRANSFERENCIA
POS1 := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "C6_TES"}))
POS2 := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "C6_LOCDEST"}))
POS3 := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "C6_LOCAL"}))
cTipo:=RETFIELD("SF4",1,XFILIAL("SF4")+aCols[N,POS1],"F4_TIPOCTA")
IF cTipo=="T" .AND. ALLTRIM(ACOLS[n,POS2])==""
	RET:=.F.
	MSGSTOP("Para TES de transferencia o almoxarifado de destino eดobrigatorio!")
	IF RET
		IF ALLTRIM(ACOLS[n,POS2])==ALLTRIM(ACOLS[n,POS3])
			RET:=.F.
			MSGSTOP("O almoxarifado de destino nao pode ser igual ao local da saida!")
		ENDIF
		
	ENDIF
ENDIF
Return(RET)
