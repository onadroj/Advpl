#INCLUDE "rwmake.ch"

/*/
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
�������������������������������������������������������������������������ͻ��
���Programa  �MT241TOK  � Autor � BRUNO            � Data �  05/11/04   ���
ALTERACAO JORDANO
�������������������������������������������������������������������������͹��
���Descricao � VALIDA MOVIMENTACAO INTERNA                                ���
���          �                                                            ���
�������������������������������������������������������������������������͹��
���Uso       � AP6 IDE                                                    ���
�������������������������������������������������������������������������ͼ��
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
/*/

User Function MT240TOK

LOCAL lRet:=.T.
LOCAL aArea := GetArea()
IF SUBSTR(M->D3_CC,1,1) == "3" .AND. EMPTY(M->D3_CLVL)
		Msgbox("Para Centros de Custo de Equipamentos deve-se informar a CLASSE DE VALOR correspondente.","MT240LOK","ALERT")
		lRet := .F.
ENDIF
RESTAREA(aArea)
Return(lRet)