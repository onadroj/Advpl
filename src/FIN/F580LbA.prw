#INCLUDE "protheus.ch"
/***
Ponto de Entrada F580LbA que efetua ou não o bloqueio da liberação automática dos títulos a pagar. O retorno deve ser  .T. ou .F.
http://tdn.totvs.com/pages/releaseview.action?pageId=6071244

***/
User Function F580LbA()
Local _lRet := .T.
Local _cErro := ""
Local _aArea := GetArea()

If Empty(QRYSE2->E2_OK)
	Return(_lRet)
Endif

DbSelectArea("FRD")
DbSetOrder(1) // FRD_FILIAL+FRD_PREFIX+FRD_NUM+FRD_PARCEL+FRD_TIPO+FRD_FORNEC+FRD_LOJA+FRD_DOCUM
DbSeek(xFilial("FRD")+QRYSE2->E2_PREFIXO+QRYSE2->E2_NUM+QRYSE2->E2_PARCELA+QRYSE2->E2_TIPO+QRYSE2->E2_FORNECE+QRYSE2->E2_LOJA,.T.)

While !Eof() .AND. xFilial("FRD")+FRD->FRD_PREFIX+FRD->FRD_NUM+FRD->FRD_PARCEL+FRD->FRD_TIPO+FRD->FRD_FORNEC+FRD->FRD_LOJA==                ;
                   xFilial("FRD")+QRYSE2->E2_PREFIXO+QRYSE2->E2_NUM+QRYSE2->E2_PARCELA+QRYSE2->E2_TIPO+QRYSE2->E2_FORNECE+QRYSE2->E2_LOJA
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
	MsgStop("O título "+QRYSE2->E2_NUM+"/"+QRYSE2->E2_PARCELA+" do fornecedor "+QRYSE2->E2_FORNECE+"/"+QRYSE2->E2_LOJA+", não pode ser liberado pela falta dos seguintes documentos: " + _cErro)
Endif

RestArea(_aArea)

Return(_lRet)