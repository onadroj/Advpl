#include "rwmake.ch"
/*/
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
�������������������������������������������������������������������������ͻ��
���Programa  �NNROFOR   � Autor �                    � Data �  16/11/00   ���
�������������������������������������������������������������������������͹��
���Descricao � INFOR. O NOSSO NUMERO DO FORNECEDOR PARA O TIPO DE PGTO 31 ���
���          � PARA O CNAB PAGAR DO BANCO BRADESCO                        ���
�������������������������������������������������������������������������͹��
���Uso       �                                                            ���
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
/*/                      

User Function NNROFOR

Local NNro

NNro := iif(SEA->EA_MODELO="31",iif(substr(SE2->E2_CODBAR,1,3)="237",;
        "0" + substr(SE2->E2_CODBAR,26,11),replicate("0",12)),replicate("0",12))

RETURN(NNro)
