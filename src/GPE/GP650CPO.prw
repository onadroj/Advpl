#INCLUDE 'PROTHEUS.CH'
#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³GP650CPO   º Autor ³ AP6 IDE           º Data ³  30/06/14   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ 															  º±±
±±ºEssa implementação permite atualizar informações dos campos da tabela  º±±
±±ºRC1 - Movimentações de Títulos.										  º±±
±±ºNa rotina GPEM650 (Gera Movimentação de Títulos no arquivo RC1).		  º±±
º±±                                                                       º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP6 IDE                                                    º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User Function GP650CPO()
	Local cCompet      := mv_par12
	Local cFornecFunc :=""
	Local cLojaFunc :=""
	Local aArea  := GetArea()
	
	RC1->RC1_XCOMPE  := cCompet
	if mv_par14 == 1                                              
		
		cCpf := fAcha("SRA",XFILIAL("SRA")+RC1->RC1_MAT,1,"RA_CIC")
		cFornecFunc := POSICIONE("SA2", 3, xFilial("RC1") + cCpf, "A2_COD")
		cLojaFunc := POSICIONE("SA2", 3, xFilial("RC1") + cCpf, "A2_LOJA")
		IF EMPTY(cFornecFunc)
			ALERT("Cadastrar o Funcionario "+ fAcha("SRA",XFILIAL("SRA")+RC1->RC1_MAT,1,"RA_NOME") +" Como Fornecedor!")		
		ENDIF
		RC1->RC1_FORNEC := cFornecFunc
		RC1->RC1_LOJA := cLojaFunc
	endif
	
	RestArea(aArea)
Return()
