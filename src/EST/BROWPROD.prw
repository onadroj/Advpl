#INCLUDE "rwmake.ch"

/*/
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
�������������������������������������������������������������������������ͻ��
���Programa  �BROWPROD  � GATASSE                    � Data �  05/05/03   ���
�������������������������������������������������������������������������͹��
���Descricao � Consulta produtos englobando saldo em todas as empresas.   ���
���          �                                                            ���
�������������������������������������������������������������������������͹��
���Uso       � AP6 IDE                                                    ���
�������������������������������������������������������������������������ͼ��
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
/*/

User Function BROWPROD


//���������������������������������������������������������������������Ŀ
//� Declaracao de Variaveis                                             �
//�����������������������������������������������������������������������

Private cCadastro := "Consulta Produtos"


//���������������������������������������������������������������������Ŀ
//� Monta um aRotina proprio                                            �
//�����������������������������������������������������������������������

Private aRotina := {{"Pesquisar","AxPesqui",0,1} ,;
					{"Estoque",'ExecBlock("AtuaProd",.F.,.F.,"E")',0,2},;
					{"Equivalentes",'ExecBlock("AtuaProd",.F.,.F.,"V")',0,2}}
Private cDelFunc := ".T." // Validacao para a exclusao. Pode-se utilizar ExecBlock

Private cString := "SZ2"

dbSelectArea("SZ2")
dbSetOrder(1)


dbSelectArea(cString)
mBrowse( 6,1,22,75,cString)

Return
