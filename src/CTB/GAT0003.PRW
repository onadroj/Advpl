#include "rwmake.ch"

/*/
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
�������������������������������������������������������������������������Ŀ��
���Fun�ao    �GAT0003   � Autor �EDSON                  � Data �12.04.05  ���
�������������������������������������������������������������������������Ĵ��
���Descri��o �RESGATA A OS E O TIPO DE ENTRADA DO PEDIDO DE COMPRA        ���
���          �PARA TES COM OS OBRIGATORIA                                 ���
���          �PARAMIXB="O" retorna a OS e PARAMIXB="T" retorna TPENT      ���
���          �                                                            ���
�������������������������������������������������������������������������Ĵ��
���Uso       �GATILHO                                                     ���
�������������������������������������������������������������������������Ĵ��
���Tabelas   �SC7,SD1                                                     ���
�������������������������������������������������������������������������Ĵ��
���Modulo    �COMPRAS                                                     ���
��������������������������������������������������������������������������ٱ�
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
*/

User Function Gat0003()

LOCAL cParam:=paramIXB
LOCAL cValor:=""
LOCAL nPCCol
LOCAL nITPCCol
LOCAL cOSOPObr
LOCAL aArea:=GETAREA()
lOCAL nTESCol := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "D1_TES"}))

cOSOPObr:=RETFIELD("SF4",1,XFILIAL("SF4")+ACOLS[N][nTESCol],"F4_OSOPOBR")

if cOSOPObr<>"1"
   Return(cValor)
endif

nPCCol := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "D1_PEDIDO"}))
nITPCCol := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "D1_ITEMPC"}))
if cParam=="O"
   cValor :=RETFIELD("SC7",1,XFILIAL("SC7")+ACOLS[N,nPCCol]+ACOLS[N,nITPCCol],"C7_OP")
else
   cValor :=RETFIELD("SC7",1,XFILIAL("SC7")+ACOLS[N,nPCCol]+ACOLS[N,nITPCCol],"C7_TPENT")
endif


Return(cValor)
