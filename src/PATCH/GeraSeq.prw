#include "rwmake.ch"

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³GERASEQ   ºAutor  ³GATASSE             º Data ³  03/20/03   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³GERA NUMERO DE DOCUMENTO SEQUENCIAL AUTOMATICAMENTE         º±±
±±º          ³PARA ALGUNS TIPOS DE DOCUMENTOS DO FINANCEIRO               º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP6                                                        º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/

User Function GeraSeq()

cChav := "GRN"
cCampo:=__READVAR

if cCampo=="M->E1_TIPO"
	cTipo:=M->E1_TIPO
else
	cTipo:=M->E2_TIPO
endif
if alltrim(cTipo) $ "GR/GRN/RC/FER/RES/FOL/PR/RPA/ADI/TX/DEV/FV/AMC/TXC/PRC/"
	Seq:=AllTrim(Posicione("SX5",1,xFilial("SX5")+"96"+cChav,"X5_DESCRI"))
	Seq:=AllTrim(Str(val(Seq) + 1))
	Seq:=subst("000000000",1,9-len(Seq))+Seq
	FwPutSX5(/*cFlavour*/, "96", cChav, Seq, Seq, Seq, /*cTextoAlt*/)
	/*
	_area:=getarea()
	dbSelectArea("SX5")
	dbSetOrder(1)
	IF dbSeek(xFilial("SX5")+"96"+cChav)
		Seq:=AllTrim(Str(val(SX5->X5_DESCRI) + 1))
//		Seq:=subst("000000",1,6-len(Seq))+Seq
		Seq:=subst("000000000",1,12-len(Seq))+Seq
		RecLock("SX5",.F.)
	ELSE
		RecLock("SX5",.T.)
		replace X5_TABELA WITH "96"
		replace X5_CHAVE WITH "GRN"
//		Seq:="000001"
		Seq:="000000000001"
	ENDIF
	replace X5_DESCRI with Seq
	MSUnlock()
	restarea(_area)
	*/
ENDIF
Return(Seq)
