/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³MT010ALT  ºAutor  ³Microsiga           º Data ³  11/13/09   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³ Ponto de entrada na alteracao de produto                   º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP                                                        º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß

Ponto de Entrada para complementar a alteração no cadastro do Produto.
LOCALIZAÇÃO : Function A010Altera - Função de Alteração do Produto, após sua alteração.
EM QUE PONTO: Após alterar o Produto, este Ponto de Entrada nem confirma nem cancela a 
operação, deve ser utilizado para gravar arquivos/campos do usuário, complementando a alteração.


*/


User Function MT010ALT()
Local aArea:=GetArea()
//Bloqueia em Z2 os produtos bloqueados em B1
dbselectarea("SZ2")
dbSetorder(1)//Z2_Filial + Z2_CodProd + Z2_CodFabr + Z2_CodPeca
dbseek(xfilial("SZ2")+SB1->B1_COD,.T.)
While !eof() .and. SZ2->(Z2_Filial + Z2_CodProd ) == SB1->(B1_FILIAL+SB1->B1_COD)
	RecLock("SZ2",.F.)
	SZ2->Z2_MSBLQL:=SB1->B1_MSBLQL
	MsUnlock()
	DBSKIP()
ENDDO             
restarea(aArea)
cAmbiente := GetEnvServer()
If upper(ALLTRIM(cAmbiente)) == "TESTE_NEW" .OR. upper(ALLTRIM(cAmbiente))=="TESTE_LOCAL"
   U_ATPRPROD()
	restarea(aArea)
Endif
Return            
