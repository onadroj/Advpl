#include "rwmake.ch"
/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³DVBRAD    º Autor ³ EDSON PEREIRA      º Data ³  13/08/03   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ CALC. DV AGENCIA E CONTA PARA MODALIDADE 31 -PAGTO. BOLETO º±±
±±º          ³ BRADESCO DE TERCEIROS VIA CNAB                             º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP6 IDE                                                    º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User Function DVBRAD

Local cFixo,cFonte,nSoma,cDv,nTam

if ParamIXB = "A"
  cFixo  := "5432"
  cFonte := Substr(SE2->E2_CODBAR,20,4) //Agencia
  nTam   := 4
elseIF ParamIXB = "C"
  cFixo  := "765432"
  cFonte := Substr(SE2->E2_CODBAR,38,6) //Conta Corrente
  nTam   := 6
elseIF ParamIXB = "2"
  cFixo  := "765432"
  cFonte := Substr(SEE->EE_CONTAUX,-6) //Conta Corrente SEE
  nTam   := 6
elseIF ParamIXB = "1"
  cFixo  := "5432"
  cFonte := Substr(SEE->EE_AGCAUX,-4) //Agencia SEE
  nTam   := 4
endif

// Calcula o DV
nSoma := 0
For nI := 1 to nTam             
	nSoma := nSoma + ;
	(Val(Substr(cFonte,nI,1))*Val(Substr(cFixo,nI,1)))
Next
If (11-(nSoma%11)) > 9
	cDv := "0"
Else
	cDv := Alltrim(Str(11-(nSoma%11),1))
Endif
RETURN(cDv)
