#INCLUDE "rwmake.ch"
#INCLUDE "TOPCONN.CH"
/*/
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
��������������������������������������������������������������������������"��
���Programa  �_SALDOE1     � Autor � GATASSE         � Data �  05/07/02   ���
�������������������������������������������������������������������������͹��
���Descricao � RECUPERA VALOR LIQUIDO DO TITULO INCLUINDO AB-             ���
���          �                                                            ���
�������������������������������������������������������������������������͹��
���Uso       � RECEBER COMO PARAMETRO FILIAL,PREFIXO,NUM,PARCELA          ���
�������������������������������������������������������������������������ͼ��
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
/*/
User Function _SALDOE1
                                              
//���������������������������������������������������������������������Ŀ
//� Declaracao de Variaveis                                             �
//�����������������������������������������������������������������������
Local aArea:=GetArea()
Local _VALOR,_STRCNAB
Local cTit:=xFilial("SE1") +SE1->(E1_CLIENTE+E1_LOJA+E1_PREFIXO+E1_NUM)
_VALOR:=0
cQuery := " SELECT * "
cQuery += " FROM "+RETSQLNAME("SE1")+" SE1 "
cQuery += " WHERE "
cQuery += " SE1.E1_FILIAL='"+XFILIAL("SE1")+"' AND "
cQuery += " SE1.D_E_L_E_T_ <> '*' AND "
cQuery += " SE1.E1_PREFIXO='"+SE1->E1_PREFIXO+"' AND "
cQuery += " SE1.E1_NUM='"+SE1->E1_NUM+"'  AND "
cQuery += " SE1.E1_CLIENTE='"+SE1->E1_CLIENTE+"'  AND "
cQuery += " SE1.E1_LOJA='"+SE1->E1_LOJA+"'  "
TCQUERY cQuery NEW ALIAS "QRY"
dbSelectArea("QRY")
dbgotop()
While !EOF() 
 If ALLTRIM(QRY->E1_TIPO) == "NF" .OR. ALLTRIM(QRY->E1_TIPO) == "DP" 
  _VALOR := _VALOR + QRY->E1_SALDO + QRY->E1_ACRESC - QRY->E1_DECRESC
    ElseIf ALLTRIM(QRY->E1_TIPO) == "RC"
  _VALOR:=_VALOR + QRY->E1_SALDO - QRY->E1_TXADM + QRY->E1_ACATR + ;
   QRY->E1_IPTU - QRY->E1_DESCALU - QRY->E1_ANUNCIO + QRY->E1_TXCOND - ;
   QRY->E1_CUSTAS + QRY->E1_ACRESC - QRY->E1_DECRESC
 ENDIF
 If SUBSTR(ALLTRIM(QRY->E1_TIPO),3,1) == "-"
  _VALOR:=_VALOR - QRY->E1_SALDO
 Endif
 DbSkip()
ENDDO
_STRCNAB:=STRZERO(INT(_VALOR*100),13)
CLOSE
Restarea(aArea)
If paramIXB=="CNAB"
  Return(_STRCNAB)
else
  Return(_VALOR)
ENDIF

