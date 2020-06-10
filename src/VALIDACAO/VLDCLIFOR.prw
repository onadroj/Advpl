#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³VLDCLIFOR º Autor ³ GATASSE            º Data ³  03/12/10   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ Valida se campos do cadastro de cliente/fornecedor estao   º±±
±±º          ³ preenchidos                                                º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP6 IDE                                                    º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User Function VLDCLIFOR
Local cEmite:=""
Local cCod:=""
Local cLj:=""
Local lRet:=.t.
Local aArea:=Getarea()
if FUNNAME() == "MATA410" //SAIDA
	IF M->C5_TIPO $ "DB" //fornecedor
		cEmite:="F"
	else //cliente
		cEmite:="C"
	endif
	if __READVAR=="M->C5_CLIENTE"
		cCod:=&(__READVAR)
		cLj:=M->C5_LOJAENT
	else
		cCod:=M->C5_CLIENTE
		cLj:=&(__READVAR)
	endif
elseif Substr(FUNNAME(),1,3) <> "CNT"  //ENTRADA
	IF CTIPO $ "DB" //cliente
		cEmite:="C"
	else //fornecedor
		cEmite:="F"
	endif
	if __READVAR=="cA100For"
		cCod:=&(__READVAR)
		cLj:=cLoja
	else
	   	cCod:=cA100For
		cLj:=&(__READVAR)
	endif                                                                                     		
endif
if !empty(cCod) .and. !empty(cLj)
	IF cEmite="F"
		dbselectarea("SA2")
		dbsetorder(1)
		if dbseek(xfilial("SA2")+cCod+cLj)
			IF empty(SA2->A2_LOGRAD).or. empty(SA2->A2_NR_END)
				MsgBox("Dados do fornecedor estão incompletos! Corrija antes de continuar.","VLDCLIFOR")
				lRet:=.f.
			endif 			
		else
			lRet:=.f.
		endif
	else
		dbselectarea("SA1")
		dbsetorder(1)
		if dbseek(xfilial("SA1")+cCod+cLj)
			IF empty(SA1->A1_LOGRAD) .or. empty(SA1->A1_NR_END)
				MsgBox("Dados do cliente estão incompletos! Corrija antes de continuar.","VLDCLIFOR")
				lRet:=.f.
			endif 			
		else
			lRet:=.f.
		endif
	endif
endif
restarea(aArea)
Return(lRet)
