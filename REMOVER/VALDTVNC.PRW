#INCLUDE "rwmake.ch"

/*/
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
�������������������������������������������������������������������������ͻ��
���Programa  �VALDTVNC     � Autor � GATASSE            � Data �  11/09/02���
�������������������������������������������������������������������������͹��
���Descricao � VALIDA SE DATA DE VENCIMENTO A PAGAR E SUPERIOR A UM       ���
���          � NUMERO DE DIAS EM MV_VENCIN                                ���
�������������������������������������������������������������������������͹��
���Uso       � COMPRAS E FINANCEIRO                                       ���
�������������������������������������������������������������������������ͼ��
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
/*/

User Function VALDTVNC


//���������������������������������������������������������������������Ŀ
//� Declaracao de Variaveis                                             �
//�����������������������������������������������������������������������

Private _RET

_ret:=.t.
Dias:= GetMV("MV_VENCMIN")
dDtIni := MSDATE()
dDtFim := M->E2_VENCREA
if alltrim(dtos(dDtFim))<>""
	nDUtil :=0
	While dDtIni<dDtFim
		If DataValida(dDtIni) == dDtIni
			nDUtil := nDUtil + 1
		Endif
		dDtIni := dDtIni + 1
	EndDo
	if nDUtil<Dias .OR. dDtFim<dDtIni
		_ret:=.f.
		MsgStop("Vencimento real menor que o prazo m�nimo de dias �teis estabelecido em MV_VENCMIN!")
	endif
endif
Return(_RET)
