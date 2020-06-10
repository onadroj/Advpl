#INCLUDE "protheus.ch"

//Validação de dados do cadastro de fornecedores

User Function MA020ALT()
Local crotina 	:= FunName()
Local aDados 	:= {}
Local cAtual 	:= "" 
Local cAntigo 	:= ""
Local aArea		:= GetArea()
Local cUser 	:= cUsername

IF ALTERA
	dbSelectArea("SX3")
    dbSetOrder(1)
	dbSeek("SA2")
	While !EOF() .and. SX3->X3_ARQUIVO == "SA2"
		If X3Uso(SX3->X3_Usado) .And. (cNivel >= SX3->X3_Nivel) .And. ("V" <> ALLTRIM(SX3->X3_CONTEXTO))
			cAntigo := &("SA2->"+ALLTRIM(SX3->X3_CAMPO))
			cAtual  := &("M->"+ALLTRIM(SX3->X3_CAMPO))
			cCampo	:= ALLTRIM(SX3->X3_TITULO)
			If SX3->X3_TIPO == "N"
				cAntigo :=  Alltrim(STR(cAntigo))
				cAtual 	:= Alltrim(STR(cAtual))
			EndIf
			If  cAntigo # cAtual
				AADD(aDados,{"ALTERA",cAntigo,cAtual,cCampo,cUser})
			EndIf
		ENDIF
		dbSkip()
	EndDo
ENDIF

If Len(aDados) > 0
	lRet := U_GRVHIST(crotina,aDados)
EndIf

restarea(aArea)

Return(.T.)