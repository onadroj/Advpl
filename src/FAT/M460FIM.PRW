#include "rwmake.ch" 
/**
Este P.E. e' chamado apos a Gravacao da NF de Saida, e fora da transação.
**/       
User Function M460FIM()        
	RecLock("SF2",.f.)
	replace SF2->F2_CCUNID with SC5->C5_CCUNID
	MSUnlock()

	aArea:=GetArea()
	dbSelectArea("SC6")
	dbSetOrder(1)  //C6_FILIAL+C6_NUM+C6_ITEM+C6_PRODUTO
	dbSeek(xFilial("SC6")+SC5->C5_NUM+"  ",.T.)
	While ((! Eof())                         .And.;
 		  	SC5->C5_Filial   == xFilial("SC6")   .And.;
			SC5->C5_NUM  == SC6->C6_NUM)
		aArea:=GetArea()
		dbSelectArea("SD2")
		dbSetOrder(8)  //D2_FILIAL+D2_PEDIDO+D2_ITEMPV
		IF dbSeek(xFilial("SD2")+SC6->C6_NUM+SC6->C6_ITEM)
			RecLock("SD2",.F.)
			REPLACE SD2->D2_CUSTO      WITH SC6->C6_CUSTO
			REPLACE SD2->D2_CCUNID     WITH SC6->C6_CCUNID
			MSUnlock()
		ENDIF
		RestArea(aArea)
	    dbSkip()
	EndDo
	RestArea(aArea)
	
Return

