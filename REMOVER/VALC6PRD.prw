#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณVALC6PRD  บ Autor ณ GATASSE            บ Data ณ  04/07/03   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDescricao ณ VALIDA PRODUTO E PREENCHE CAMPOS EM C5                     บฑฑ
ฑฑบ          ณ                                                            บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ AP6 IDE                                                    บฑฑ
ฑฑศออออออออออฯออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
/*/

User Function VALC6PRD

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณ Declaracao de Variaveis                                             ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
LOCAL RET,X,NCOL,OKSV,OKLE,HABILIT
RET:=.T.
OKSV:=.F.
OKLE:=.F.
if ALLTRIM(C6_PRODUTO)<>""
  IF POSICIONE("SB1",1,XFILIAL("SB1")+C6_PRODUTO,"B1_SITPROD") == "OB"
     MSGSTOP("Produto nao habilitado para uso!")
     RET:=.F.
  ELSE
	nCol := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "C6_PRODUTO"}))
	FOR X:=1 TO LEN(ACOLS)
		IF ACOLS[X,LEN(ACOLS[1])]<>.t.
			IF X==N
				TP:=RETFIELD("SB1",1,XFILIAL("SB1")+C6_PRODUTO,"B1_TIPO")
			ELSE
				TP:=RETFIELD("SB1",1,XFILIAL("SB1")+ACOLS[X,nCol],"B1_TIPO")
			ENDIF
			IF TP$"LE"
				OKLE:=.T.
			ENDIF
			IF TP$"SV"
				OKSV:=.T.
			ENDIF
		ENDIF
	NEXT
	IF OKSV .OR. OKLE
		IF ALLTRIM(M->C5_LOCMED)=="" .OR. ALLTRIM(M->C5_MEDICAO)==""
			RET:=.F.
			MSGSTOP("MEDICAO E LOCAL DA MEDICAO SAO OBRIGATORIOS!"    )
		ELSE
			IF OKSV .AND. OKLE
				M->C5_MENNOTA:=" SERVICO(S) E LOCACAO DE EQUIPAMENTO ABAIXO, CONFORME MEDICAO "+ALLTRIM(M->C5_MEDICAO)+" "+ALLTRIM(M->C5_LOCMED)
			ELSEIF OKLE
				M->C5_MENNOTA:="LOCACAO DE EQUIPAMENTO(S) ABAIXO, CONFORME MEDICAO "+ALLTRIM(M->C5_MEDICAO)+" "+ALLTRIM(M->C5_LOCMED)
			ELSEIF OKSV
				M->C5_MENNOTA:="SERVICO(S) REFERENTE(S) A MEDICAO "+ALLTRIM(M->C5_MEDICAO)+" "+ALLTRIM(M->C5_LOCMED)
			ENDIF
	   //		C:=FieldPos("C5_MENNOTA")
//			ATELA[C][2]:=M->C5_MENNOTA
			
		ENDIF
	ENDIF
  ENDIF
ENDIF
GETDREFRESH()
//SYSREFRESH()
IF PARAMIXB=="G"
	RET:=M->C5_MENNOTA
ENDIF
Return(RET)
