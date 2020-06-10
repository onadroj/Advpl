#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณVLT9CODBEMบ Autor ณ GATASSE            บ Data ณ  29/10/10   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDescricao ณ Valida codigo do BEM de ST9                                บฑฑ
ฑฑบ          ณ USADO EM X3_VLDUSER DE T9_CODBEM                           บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ AP6 IDE                                                    บฑฑ
ฑฑศออออออออออฯออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
/*/

User Function VLT9CODBEM
Local lRet:=.t.
Local cConteudo:=ALLTRIM(&(__READVAR))      
Local xx
//MsgBox("Erro no formato do campo. (XXX-999)"+cConteudo)

//valida formato @R XXX-999
if len(cConteudo) <> 7
	lRet:=.f.
endif
for xx:=1 to 7
	if xx<4  .and. (substr(cConteudo,xx,1)<"A" .or. substr(cConteudo,xx,1)>"Z")
		lRet:=.f.
	endif
	if xx=4 .and. substr(cConteudo,xx,1)<>"-"
		lRet:=.f.
	endif
	if xx>4 .and. (substr(cConteudo,xx,1)<"0" .or. substr(cConteudo,xx,1)>"9")
		lRet:=.f.
	endif
next
if !lRet
	MsgBox("Erro no formato do campo. (XXX-999)","VLT9CODBEM")
endif
Return(lRet)
