#INCLUDE "protheus.ch"
/**
O ponto de entrada FA580LIB foi desenvolvido para bloquear ou não a liberação manual. 
Necessita de um retorno .T. ou .F.

**/
User Function FA580LIB()
Local _lRet := .T.
Local _aArea := GetArea()
Local _cErro := ""

DbSelectArea("FRD")
DbSetOrder(1) // FRD_FILIAL+FRD_PREFIX+FRD_NUM+FRD_PARCEL+FRD_TIPO+FRD_FORNEC+FRD_LOJA+FRD_DOCUM
DbSeek(xFilial("FRD")+SE2->E2_PREFIXO+SE2->E2_NUM+SE2->E2_PARCELA+SE2->E2_TIPO+SE2->E2_FORNECE+SE2->E2_LOJA,.T.)

While !Eof() .AND. xFilial("FRD")+FRD->FRD_PREFIX+FRD->FRD_NUM+FRD->FRD_PARCEL+FRD->FRD_TIPO+FRD->FRD_FORNEC+FRD->FRD_LOJA==                ;
                   xFilial("FRD")+SE2->E2_PREFIXO+SE2->E2_NUM+SE2->E2_PARCELA+SE2->E2_TIPO+SE2->E2_FORNECE+SE2->E2_LOJA
	If FRD->FRD_RECEB<>"1"
		If Empty(_cErro)
	   		_cErro := Alltrim(RetField("CN5",1,xFilial("CN5")+FRD->FRD_DOCUM,"CN5_DESCRI"))
	 	Else
	   		_cErro += ", "+Alltrim(RetField("CN5",1,xFilial("CN5")+FRD->FRD_DOCUM,"CN5_DESCRI"))
	 	Endif
	Endif
    DbSkip()
EndDo

If !Empty(_cErro)
	_lRet := .F.
²	MsgStop("Este título não pode ser liberado pela falta dos seguintes documentos: " + _cErro)
Endif

RestArea(_aArea)

Return(_lRet)   