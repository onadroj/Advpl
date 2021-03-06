#include "rwmake.ch"        

User Function VerifSeq()


SetPrvt("CPARAM,CALIAS,CCHAV,SEQ,NumSC5,NumSXE,NumSXF,NumSug")

/*
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
�������������������������������������������������������������������������Ŀ��
���Fun�ao    �VerifSeq  � Autor �MAURICIO/GATASSE       � Data �04.07.00  ���
�������������������������������������������������������������������������Ĵ��
���Descri��o �Associa Sequencial ao numero do pedido subtituindo          ���
���          �inizializador padrao da SIGA GETSX8NUM()                    ���
���          �                                                            ���
���          �                                                            ���
�������������������������������������������������������������������������Ĵ��
���Uso       �VALIDACAO DO USUARIO EM C5_NUM                              ���
�������������������������������������������������������������������������Ĵ��
���Tabelas   �SC5, SXE e SXF                                              ���
�������������������������������������������������������������������������Ĵ��
���Modulo    �FINANCEIRO                                                  ���
��������������������������������������������������������������������������ٱ�
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
*/


cparam:=paramIXB

cAlias := Alias()
nOrd := IndexOrd()
cRec := Recno()

    
If !INCLUI
   return
Endif


dbSelectArea("SX5")
dbSetOrder(1)
dbSeek(xFilial("SX5")+"91"+"SEQ")

Seq:=AllTrim(Str(val(SX5->X5_DESCRI) + 1))
Seq:=subst("000000",1,6-len(Seq))+Seq

RecLock("SX5",Eof())
replace X5_DESCRI with Seq
MSUnlock()


dbSelectArea(cAlias)
dbSetOrder(nOrd)
dbGoTo(cRec)



Return(Seq)

