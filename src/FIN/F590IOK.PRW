#INCLUDE "rwmake.ch"
#include "topconn.ch"

/*/
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
�������������������������������������������������������������������������ͻ��
���Programa  �F590IOK    � Autor � EDSON             � Data �  31/05/07   ���
�������������������������������������������������������������������������͹��
���Descricao � PERMITE, OU NAO, A EXCLUS�O DE T�TULOS DE UM BORDER�       ���
���          �                                                            ���
�������������������������������������������������������������������������͹��
���Uso       � FINANCEIRO                                                 ���
�������������������������������������������������������������������������ͼ��
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
/*/

User Function F590IOK

LOCAL _return
/*

_return:=.t.
cQuery:="SELECT ZJ_BORDERO, ZJ_LIBERAD "
cQuery:=cQuery+" FROM "+RetSQLName("SZJ") 
cQuery:=cQuery+" WHERE ZJ_BORDERO>='"+MV_PAR01+"' And ZJ_BORDERO<='"+MV_PAR02+"' AND D_E_L_E_T_<>'*' AND ZJ_LIBERAD='N' AND ZJ_TIPO='P' "
cQuery:=ChangeQuery(cQuery)
TCQUERY cQuery ALIAS QRY NEW   

DBSELECTAREA("QRY")
WHILE !EOF()
	_B:=_B+QRY->ZJ_BORDERO+ " "  
	DBSKIP()
ENDDO
DBCLOSEAREA("QRY")            

if ALLTRIM(_B)<>""
	_return :=.F.
	msgstop("O(s) Bordero(s) "+_b+" ainda n�o est�o liberados. Nenhum arquivo ser� gerado.!!!")
endif

*/
//Msgstop("O ponto de entrada na inclus�o de t�tulos no border� est� funcionando.!!!")

Return(_return)