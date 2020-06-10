#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³VALD1OP   º Autor ³ GATASSE            º Data ³  29/10/10   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ VALIDA O CAMPO D1_OP                                       º±±
±±º          ³ USADO NA VALIDACAO DE D1_OP PARA PREENCHER D1_ITEMCTA      º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP6 IDE                                                    º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß

Se a empresa é a 08, o item é o equipamento pai e a classe o componente, para o caso da OS 
ser de um componente.Se a OS já é do pai (tem contador), não tem classe e o item é o pai.


Para a empresa 03, o item é o bem da OS e a classe o centro de custo da OS
/*/

User Function VALD1OP
Local aArea:=GetArea()
Local cOS:=&(__READVAR)
LOCAL nPosConta:=(AScan(aHeader,{|aItem| UPPER(AllTrim(aItem[2]))=="D1_CONTA"}))
LOCAL nPosItem:=(AScan(aHeader,{|aItem| UPPER(AllTrim(aItem[2]))=="D1_ITEMCTA"}))
LOCAL nPosCLVL := (AScan(aHeader,{|aItem| UPPER(AllTrim(aItem[2]))=="D1_CLVL"}))
Local lRet:=.t.
if cEmpAnt $ "08" .AND. ACOLS[N][nPosConta]>"3"
	if "OS" $ cOS//É UMA OS E NÃO UMA OP
		cOS:=substr(cOS,1,6)
		dbSelectArea("STJ")
		dbSetOrder(1)
		if dbSeek(xFilial("STJ")+cOS,.T.)
			acols[n][nPosItem]:="E"+STJ->TJ_CODBEM
			acols[n][nPosCLVL]:=SPACE(9)
			dbSelectArea("ST9")
			dbSetOrder(1)
			if dbSeek(xFilial("ST9")+STJ->TJ_CODBEM)
				IF  ! ST9->T9_TEMCONT=="S"
					DBSELECTAREA("STC")
					DBSETORDER(3)//TC_FILIAL+TC_COMPONE+TC_CODBEM
					if dbSeek(xFilial("STC")+STJ->TJ_CODBEM)
						acols[n][nPosItem]:="E"+ALLTRIM(STC->TC_CODBEM) //Preenche Item cta com bem pai
						acols[n][nPosCLVL]:="E"+ALLTRIM(STJ->TJ_CODBEM)  //Preenche Classe Valor com bem filho
					endif
				endif
			endif
		endif
	endif
endif
if cEmpAnt $ "03" .AND. ACOLS[N][nPosConta]>"3"
	if "OS" $ cOS//É UMA OS E NÃO UMA OP
		cOS:=substr(cOS,1,6)
		dbSelectArea("STJ")
		dbSetOrder(1)
		if dbSeek(xFilial("STJ")+cOS,.T.)
			acols[n][nPosItem]:="E"+STJ->TJ_CODBEM
			acols[n][nPosCLVL]:=ALLTRIM(STJ->TJ_CCUSTO)  //Preenche Classe Valor com o centro de custo
			dbSelectArea("ST9")
			dbSetOrder(1)
			if dbSeek(xFilial("ST9")+STJ->TJ_CODBEM)
				IF  ! ST9->T9_TEMCONT=="S"
					DBSELECTAREA("STC")
					DBSETORDER(3)//TC_FILIAL+TC_COMPONE+TC_CODBEM
					if dbSeek(xFilial("STC")+STJ->TJ_CODBEM)
						acols[n][nPosItem]:="E"+ALLTRIM(STC->TC_CODBEM) //Preenche Item cta com bem pai
					endif
				endif
			endif
		endif
	endif
endif
restarea(aArea)
Return(lRet)

