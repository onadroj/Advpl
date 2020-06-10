#INCLUDE "protheus.ch"
/**
O ponto de entrada FA60FIL será executado no filtro de registros que serão 
processados para a elaboração do borderô(tipo Indregua).

**/
User Function M103DSE2()
Local _aArea := GetArea()

DbSelectArea("FRD")
DbSetOrder(1) // FRD_FILIAL+FRD_PREFIX+FRD_NUM+FRD_PARCEL+FRD_TIPO+FRD_FORNEC+FRD_LOJA+FRD_DOCUM
DbSeek(xFilial("FRD")+SE2->E2_PREFIXO+SE2->E2_NUM+SE2->E2_PARCELA+SE2->E2_TIPO+SE2->E2_FORNECE+SE2->E2_LOJA,.T.)

While !Eof() .AND. xFilial("FRD")+FRD->FRD_PREFIX+FRD->FRD_NUM+FRD->FRD_PARCEL+FRD->FRD_TIPO+FRD->FRD_FORNEC+FRD->FRD_LOJA==                ;
                   xFilial("FRD")+SE2->E2_PREFIXO+SE2->E2_NUM+SE2->E2_PARCELA+SE2->E2_TIPO+SE2->E2_FORNECE+SE2->E2_LOJA
	RecLock("FRD",.F.)
    DbDelete()   
    MsUnlock()
    DbSkip()
EndDo

RestArea(_aArea)

Return()