#INCLUDE "rwmake.ch"

/*/
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
�������������������������������������������������������������������������ͻ��
���Programa  �MT103NAT     � Autor � AP6 IDE            � Data �  20/08/10   ���
�������������������������������������������������������������������������͹��
���Descricao � Codigo gerado pelo AP6 IDE.                                ���
���          �                                                            ���
�������������������������������������������������������������������������͹��
���Uso       � AP6 IDE                                                    ���
�������������������������������������������������������������������������ͼ��
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
/*/

User Function MT103NAT
Local lRet:=.t.
local nPosAliqISS := AScan(aHeader,{|aItem| UPPER(AllTrim(aItem[2]))=="D1_ALIQISS"})
Local nAlq
lOCAL cNat:=PARAMIXB
IF cTipo=="N" .and. !empty(cNat)
	cCalcISS := retfield("SED",1,xFilial("SED")+cNat,"ED_CALCISS")
	if cCalcISS=="S"
		nAlq:=retfield("SA2",1,XFILIAL("SA2")+cA100For,"A2_ALIQISS")
		IF nAlq#0
			ACOLS[N][nPosAliqISS]:=nAlq
			MaFisAlt("IT_ALIQISS",nAlq,n)
		endif
	endif
endif
Return(lRet)
