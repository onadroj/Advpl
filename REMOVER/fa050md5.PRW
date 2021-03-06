#include "rwmake.ch"       

User Function fa050md5()   

/*/
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
�������������������������������������������������������������������������Ŀ��
���Fun�ao    �FA050MD5  � Autor �MAURICIO/GATASSE       � Data �21.08.00  ���
�������������������������������������������������������������������������Ĵ��
���Descri��o �Verifica se o titulo lancado em SE2 e do tipo PA para serem ���
���          �gravados o mesmo sequencial e origem em SE5                 ���
���          �                                                            ���
���          �                                                            ���
�������������������������������������������������������������������������Ĵ��
���Uso       �DR                                                           ���
�������������������������������������������������������������������������Ĵ��
���Tabelas   �SE2 E SE5                                                   ���
�������������������������������������������������������������������������Ĵ��
���Modulo    �FINANCEIRO                                                  ���
��������������������������������������������������������������������������ٱ�
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
*/

cAliaAnte := Alias()
nAreaAnte := IndexOrd()
nRegiAnte := RecNo()


if ALLTRIM(SE2->E2_TIPO) == "PA" .AND. ALLTRIM(SE2->E2_NUMBCO) <> ""
	DbSelectArea("SE5")
	
	RecLock("SE5",.F.)
    replace SE5->E5_SEQC with SE2->E2_SEQ
    replace SE5->E5_ORIG with SE2->E2_ORIG
    replace SE5->E5_CCUNID with SE2->E2_CCUNID

	MSUnlock()
ENDIF


dbSelectArea(cAliaAnte)
dbSetOrder(nAreaAnte)
dbGoTo(nRegiAnte)

return(.t.)