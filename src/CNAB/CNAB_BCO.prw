#INCLUDE "rwmake.ch"

/*/
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
�������������������������������������������������������������������������ͻ��
���Programa  �CNAB_BCO  � Autor � GATASSE            � Data �  14/07/03   ���
�������������������������������������������������������������������������͹��
���Descricao � RETORNO CODIGO DA AGENCIA+CONTA+DIGITO PARA CNAB           ���
���          �                                                            ���
�������������������������������������������������������������������������͹��
���Uso       � AP6 IDE                                                    ���
�������������������������������������������������������������������������ͼ��
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
/*/

User Function CNAB_BCO


//���������������������������������������������������������������������Ŀ
//� Declaracao de Variaveis                                             �
//�����������������������������������������������������������������������

LOCAL RET
RET:='0009'+SEE->EE_AGCAUX+RIGHT(SEE->EE_CONTAUX,7)+RIGHT(ALLTRIM(SEE->EE_DGCTA),1)

Return(RET)
