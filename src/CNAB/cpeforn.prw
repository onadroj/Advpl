#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³CPEFORN  º Autor ³ GATASSE             º Data ³  06/10/03   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ DEVOLVE DADOS DO FORNECEDOR PARA CNAB A PAGAR BRADESCO     º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP6 IDE                                                    º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User Function CPEFORN
LOCAL RET
IF ALLTRIM(SA2->A2_REPRCGC)<>""
	RET:=IIF(LEN(ALLTRIM(SA2->A2_REPRCGC))==14,"2","1")
	RET += iif(LEN(ALLTRIM(SA2->A2_REPRCGC))==14,"0" + SA2->A2_REPRCGC,;
          substr(SA2->A2_REPRCGC,1,9) + "0000" + substr(SA2->A2_REPRCGC,10,2))
	RET+=SUBSTR(SA2->A2_REPRES,1,30)                                
	RET+=alltrim(SA2->A2_REPR_EN)+"-"+alltrim(SA2->A2_REPRMUN)+"-"+alltrim(SA2->A2_REPREST)
	RET+=IIF(SA2->A2_REPRCEP="     ","00000",SUBSTR(SA2->A2_REPRCEP,1,5))    
	RET+=IIF(SA2->A2_REPRCEP="     ","00000",SUBSTR(SA2->A2_REPRCEP,6,3))    
ELSE
	RET:=IIF(SA2->A2_TIPO="J","2",IIF(SA2->A2_TIPO="F","1","3"))   
	RET += iif(SA2->A2_TIPO="J","0" + SA2->A2_CGC,;
          iif(SA2->A2_TIPO="F",substr(SA2->A2_CGC,1,9) + "0000" + substr(SA2->A2_CGC,10,2),STRZERO(VAL(SA2->A2_CGC))))
	RET+=SUBSTR(SA2->A2_NOME,1,30)                                
	RET+=alltrim(SA2->A2_END)+"-"+alltrim(SA2->A2_MUN)+"-"+alltrim(SA2->A2_EST)
	RET+=IIF(SA2->A2_CEP="     ","00000",SUBSTR(SA2->A2_CEP,1,5))    
	RET+=IIF(SA2->A2_CEP="     ","00000",SUBSTR(SA2->A2_CEP,6,3))    
ENDIF
Return(RET)
