#include "rwmake.ch"
/*/
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
�������������������������������������������������������������������������ͻ��
���Programa  �DTLIDESC  � Autor �                    � Data �  21/02/01   ���
�������������������������������������������������������������������������͹��
���Descricao � RETORNAR A DATA LIMITE PARA DESCONTO CASO HAJA ABATIMENTO. ���
���          �                                                            ���
�������������������������������������������������������������������������͹��
���Uso       �                                                            ���
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
/*/

User Function DTLIDESC  

Local dDtLiDesc  

dDtLiDesc := Iif(SOMAABAT(SE2->E2_PREFIXO,SE2->E2_NUM,SE2->E2_PARCELA,"P",1,SE2->E2_VENCREA,SE2->E2_FORNECE,SE2->E2_LOJA)=0,;
             Replicate("0",8),GravaData(SE2->E2_VENCREA,.F.,8))

RETURN(dDtLiDesc)