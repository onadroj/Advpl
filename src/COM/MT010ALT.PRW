/*
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
�������������������������������������������������������������������������ͻ��
���Programa  �MT010ALT  �Autor  �Microsiga           � Data �  11/13/09   ���
�������������������������������������������������������������������������͹��
���Desc.     � Ponto de entrada na alteracao de produto                   ���
���          �                                                            ���
�������������������������������������������������������������������������͹��
���Uso       � AP                                                        ���
�������������������������������������������������������������������������ͼ��
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������

Ponto de Entrada para complementar a altera��o no cadastro do Produto.
LOCALIZA��O : Function A010Altera - Fun��o de Altera��o do Produto, ap�s sua altera��o.
EM QUE PONTO: Ap�s alterar o Produto, este Ponto de Entrada nem confirma nem cancela a 
opera��o, deve ser utilizado para gravar arquivos/campos do usu�rio, complementando a altera��o.


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
