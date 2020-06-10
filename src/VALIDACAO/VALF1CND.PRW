#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณVALF1CND  บ Autor ณ GATASSE            บ Data ณ  22/03/04   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDescricao ณ VALIDA CONDICAO DE PAGAMENTO NA NOTA DE ENTRADA            บฑฑ
ฑฑบ          ณ E PREPARA PARA O CASO DE CAIXAS-JA PAGAS                   บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ AP6 IDE                                                    บฑฑ
ฑฑศออออออออออฯออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
/*/

User Function VALF1CND
LOCAL RET:=.T.
LOCAL AAREA:=GETAREA()
PUBLIC cBCO:=space(3)
PUBLIC cAG:=space(5)
PUBLIC cCONTA:=space(10)
IF &__READVAR=="CX "
	OK:=.T.
	WHILE OK
		@ 96,42 TO 300,405 DIALOG oDlg TITLE "Solicitacao de Dados"
		@ 15,14 Say "A condicao CX ้ utilizada pra titulos ja pagos pelo caixa." size 180,50
		@ 25,14 Say "Informe o caixa no qual o titulo foi baixado." size 120,50
		@ 35,14 Say "Banco:" size 120,50
		@ 35,90 Get cBCO SIZE 40,2 Picture "@!" Valid "CX"$cBCO F3 "SA6"
		@ 45,14 Say "Agencia:" size 120,50
		@ 45,90 Get cAG SIZE 40,2 Picture "@!" when .f.
		@ 55,14 Say "Conta:" size 120,50
		@ 55,90 Get cCONTA SIZE 40,2 Picture "@!" when .f.
		@ 80,30 BMPBUTTON TYPE 01 ACTION (RET:=.t.,CLOSE(ODLG))
//		@ 80,100 BMPBUTTON TYPE 02 ACTION (RET:=.f.,Close(oDlg))
		ACTIVATE DIALOG oDlg CENTERED
		OK:=.F.
		IF RET
			DBSELECTAREA("SA6")
			DBSETORDER(1)
			IF !DBSEEK(XFILIAL("SA6")+cBCO+cAG+cCONTA)
			    ret:=.f.
			    OK:=.T.
			    msgstop("Banco/Agencia/Conta nao encontrado. Utilize a tecla F3 para selecionar o caixa!")
			ENDIF		
		ENDIF
		RESTAREA(AAREA)
	ENDDO
ENDIF
Return (RET)
