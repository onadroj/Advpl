#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณVALPROD   บ Autor ณ GATASSE            บ Data ณ  12/01/04   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDescricao ณ VALIDA PRODUTO.                                            บฑฑ
ฑฑบ          ณ                                                            บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ AP5 IDE                                                    บฑฑ
ฑฑศออออออออออฯออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
/*/

User Function VALPROD


//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณ Declaracao de Variaveis                                             ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
LOCAL AREA,CCOD,CCAMPO,_RET
//Local cCtbDes, cCtbCus, cCtbInd
Local _aDados := {}
_RET:=.T.
AREA:=GETAREA()
CCAMPO:=__READVAR

/*	MSGSTOP("C๓digo do Produto "+&CCAMPO+" - "+cCtbDes+" - "+cCtbCus+" - "+cCtbInd)
IF POSICIONE("SB1",1,XFILIAL("SB1")+&CCAMPO,"B1_CTBADES") == "" .OR. POSICIONE("SB1",1,XFILIAL("SB1")+&CCAMPO,"B1_CTBACUS") == ""  .OR. POSICIONE("SB1",1,XFILIAL("SB1")+&CCAMPO,"B1_CTBACI") == ""
	cCtbDes := RETFIELD("SB1",1,XFILIAL("SB1")+&CCAMPO,"B1_CTBADES")
	cCtbCus := RETFIELD("SB1",1,XFILIAL("SB1")+&CCAMPO,"B1_CTBACUS")
	cCtbInd := RETFIELD("SB1",1,XFILIAL("SB1")+&CCAMPO,"B1_CTBACI")
	aAdd(_aDados,cCtbDes)
	aAdd(_aDados,"Pagamento")
	aAdd(_aDados,cCtbCus)
	aAdd(_aDados,cCtbInd)
	MEnviaMail("_CC",_aDados,,,,.T.)	 
	MSGSTOP("C๓digo do Produto "+&CCAMPO+" - "+cCtbDes+" - "+cCtbCus+" - "+cCtbInd)

B1_CTBADES
B1_CTBACUS
B1_CTBACI

ENDIF    
*/
IF ALLTRIM(&CCAMPO)=""
	MSGSTOP("C๓digo do Produto ้ Obrigat๓rio!")
	//_RET:=.F.
ELSE
	IF POSICIONE("SB1",1,XFILIAL("SB1")+&CCAMPO,"B1_SITPROD") == "OB"
//		IF MSGBOX(ALLTRIM(&CCAMPO)+"-"+POSICIONE("SB1",1,XFILIAL("SB1")+&CCAMPO,"B1_DESC")+CHR(10)+CHR(13)+"Produto se encontra em situacao de OBSOLETO!. Deseja usar assim mesmo?","Confirmacao","YESNO")
//			_RET:=.T.
//		ELSE
        	MSGSTOP("Produto nใo habilitado para uso!")
			_RET:=.F.
//		ENDIF
	ENDIF
ENDIF
RESTAREA(AREA)
Return(_RET)
