#include "rwmake.ch"
/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³TPPGTO2   º Autor ³                    º Data ³  27/11/00   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ INFORMACOES COMPLEMENTARES PARA CNAB A PAGAR - BRADESCO    º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³                                                            º±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/                      

User Function TPPGTO2

Local TipoA,TipoB

// TipoA := iif(SEA->EA_MODELO="03","C"+ "000000" + SEA->EA_TIPOPAG + SPACE(31),;
//          iif(SEA->EA_MODELO="31",SUBSTR(SE2->E2_CODBAR,20,25) + SUBSTR(SE2->E2_CODBAR,5,1) + SUBSTR(SE2->E2_CODBAR,4,1) + SPACE(13),SPACE(40)))       


// RETURN(TipoA)
TipoA:="C"
IF SEA->EA_TIPOPAG=="01"
	TipoA:="D"
endif
Do Case
	Case SEA->EA_MODELO == "31" 
		Return(SUBSTR(SE2->E2_CODBAR,20,25) + SUBSTR(SE2->E2_CODBAR,5,1) + SUBSTR(SE2->E2_CODBAR,4,1)+Space(13))
	Case SEA->EA_MODELO == "03" .AND. SEA->EA_TIPOPAG == "10"
		Return(TipoA+Replicate("0",6)+"04"+Space(31))
	Case SEA->EA_MODELO == "03" .AND. SEA->EA_TIPOPAG == "20"
		Return(TipoA+Replicate("0",6)+"07"+Space(31))
	Case SEA->EA_MODELO == "03" .AND. SEA->EA_TIPOPAG == "30"
		Return(TipoA+Replicate("0",6)+"06"+Space(31))
	Case SEA->EA_MODELO == "03" .AND. SEA->EA_TIPOPAG == "40"
		Return(TipoA+Replicate("0",6)+"08"+Space(31))
	Case SEA->EA_MODELO == "03" .AND. !(SEA->EA_TIPOPAG $ "10/20/30/40")
		Return(TipoA+Replicate("0",6)+"01"+Space(31))
	Case SEA->EA_MODELO == "07" 
		Return(TipoA+Replicate("0",6)+"01"+Space(31))
	Case SEA->EA_MODELO == "08" //TED
		Return(TipoA+Replicate("0",6)+"01"+Space(31))
	OtherWise
	       Return(Space(40))
EndCase 





