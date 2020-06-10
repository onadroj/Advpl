#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³F241IMP   º Autor ³ AP6 IDE            º Data ³  04/12/09   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ Codigo gerado pelo AP6 IDE.                                º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP6 IDE                                                    º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User Function F241IMP


//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Declaracao de Variaveis                                             ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ


Local cFornece:=SE2->E2_FORNECE
Local cLoja:=SE2->E2_LOJA
Local cPrefixo:=SE2->E2_PREFIXO
Local cNum:=SE2->E2_NUM
Local cLj
Local aArea:=GetArea()
//GUARDA DADOS PARA RECUPERAR TITULOS DE IMPOSTOS
Local cUniao:=GETMV("MV_UNIAO")
Local cE2CCUNID := SE2->E2_CCUNID
Local cE2CUSTO  := SE2->E2_CUSTO
Local cE2HIST := SE2->E2_HIST
Local cE2COMPETE := SE2->E2_COMPETE
Local cE2TITPAI := SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA)    
return
IF Len(cUniao)>=8
	cLj:=SUBSTR(cUniao,7,2)
	cUniao:=SUBSTR(cUniao,1,6)
ELSEIF Len(cUniao)<7
	cLj:="00"
	cUniao:=SUBSTR(cUniao+"        ",1,6)
ENDIF
dbSelectArea("SE2")
aArease2:=GetArea()
dbSetOrder(6)
dbSeek(XFilial("SE2")+cUniao+cLj+cPrefixo+cNum,.T.)
WHILE ( (! EOF())                 .AND. ;
	(SE2->E2_FILIAL  == xFilial("SE2"))  .AND. ;
	(SE2->E2_FORNECE == cUniao) .AND. ;
	(SE2->E2_LOJA    == cLj)    .AND. ;
	(SE2->E2_PREFIXO == cPrefixo) .AND. ;
	(SE2->E2_NUM     == cNum))
	IF SE2->E2_TIPO == "TX "  .AND. cUniao+cLj==SE2->E2_FORNECE+SE2->E2_LOJA .AND. ALLTRIM(cE2TITPAI) == ALLTRIM(SE2->E2_TITPAI)
		RecLock("SE2",.F.)
		SE2->E2_CCUNID := cE2CCUNID
		SE2->E2_CUSTO  := cE2CUSTO
		SE2->E2_COMPETE  := cE2COMPETE
		SE2->E2_HIST   := cE2HIST
		SE2->E2_FLUXO  := "S"
		MSUnlock()
	ENDIF
	dbskip()
ENDDO
restarea(aArease2)
restarea(aArea)
Return
//NESTE PONTO DE ENTRADA JÁ GEROU OS IMPOSTOS