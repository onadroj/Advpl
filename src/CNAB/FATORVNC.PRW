#include "rwmake.ch"
/*/
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
�������������������������������������������������������������������������ͻ��
���Programa  �FATORVNC  � Autor � GATASSE            � Data �  08/09/03   ���
�������������������������������������������������������������������������͹��
���Descricao � RETORNAR O VALOR A DO  ABATIMENTO PARA DE CADA TITULO.     ���
���          �                                                            ���
�������������������������������������������������������������������������͹��
���Uso       �                                                            ���
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
/*/

User Function FATORVNC

Local nFATORVNC

/*
if (SEA->EA_MODELO=="31")
	nFATORVNC := SUBSTR(SE2->E2_CODBAR,6,4)
else
	nFATORVNC := REPLICATE("0",4)
ENDIF
*/
IF ALLTRIM(SUBSTR(SE2->E2_CODBAR,6,4))==""
	nFATORVNC := REPLICATE("0",4)
ELSEIF SUBSTR(SE2->E2_CODBAR,6,4)<>"0000"
	nFATORVNC := SUBSTR(SE2->E2_CODBAR,6,4)
else
	nFATORVNC := REPLICATE("0",4)
ENDIF
	
RETURN(nFATORVNC)
