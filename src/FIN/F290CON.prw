#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³F290CON   º Autor ³ GATASSE            º Data ³  30/05/03   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ SUBSTITUI TELA DE CONDICAO DE PAGAMENTO E SOLICITA MAIS    º±±
±±º          ³ DADOS                                                      º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP6 IDE                                                    º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User Function F290CON (VET)
PUBLIC _cCodPagto,_CUSTO ,_CCUNID,_COMPETE ,_HIST 
_cCodPagto :="   "
_CUSTO  :=SPACE(9)
//_CCUNID :=SPACE(9)
_COMPETE :=SPACE(6)
_HIST :=SPACE(45)
_nDescCusto:=""
_nDescCond:=""

_NOK:=.T.
Do while _NOK
	@ 001, 001 TO 180,440 DIALOG oDlg3 TITLE "Dados adicionais"
	@ 004, 002 SAY "Condicao"
	@ 004, 036 GET _cCodPagto SIZE 10,2 Picture "999" Valid ValidaE4() F3 "SE4"
	@ 004, 080 SAY _nDescCond
	@ 020, 001 TO 80,220 TITLE "Informacoes para os titulos"
	@ 034, 002 SAY "Centro"
	@ 034, 036 GET _custo SIZE 10,2 Picture "@9.99.9.999.9" Valid ValidaI3() F3 "CTT"
	@ 034, 080 SAY _nDescCusto
//	@ 044, 002 SAY "CC Unid."
//	@ 050, 036 SAY _CCUNID
	@ 054, 002 SAY "Competencia"
	@ 054, 036 GET _COMPETE SIZE 10,2 Picture "@R 99/9999" Valid ValidaCmp()
	@ 064, 002 SAY "Historico"
	@ 064, 036 GET _HIST SIZE 160,2 Picture "@!" 
	@ 078, 180 BMPBUTTON TYPE 01 ACTION (_NOK:=.F.,close(oDlg3))
	ACTIVATE DIALOG oDlg3 CENTER  
	IF ALLTRIM(_cCodPagto)="" .or.  ALLTRIM(_CUSTO)="" .or.  ALLTRIM(_COMPETE)="".or.  ALLTRIM(_HIST)=""
		_NOK:=.T.
		MSGSTOP("Existem campos vazios de preenchimento obrigatorio!")	
	ENDIF
	IF !_NOK
		aParcelas := Condicao(nvalor,_cCodPagto,,ddatabase)  //FUNCAO SIGA PARA MONTAR VETOR COM PARCELAMENTO
		_t:=0
		for _x:=1 to len(aParcelas)
			_t:=_t+aParcelas[_x,2]
		next
		aParcelas[1,2]:=aParcelas[1,2]+nValor-_t  //AJUSTE NA PRIMEIRA PARCELA
	ENDIF
EndDo
Return   (aParcelas)

Static Function ValidaE4()
aArea:=GetArea()
_cret:= !empty(ALLTRIM(_cCodPagto)) .and. ExistCpo("SE4",_cCodPagto)
if _cret
	dbSelectArea("SE4")
	dbSetOrder(1)
	dbSeek(xFilial("SE4")+_cCodPagto)
	_nDescCond:=SE4->E4_DESCRI
	@ 004, 080 SAY _nDescCond
endif
RestArea(aArea)
dlgRefresh(oDlg3)
ObjectMethod(oDlg3,"Refresh()")
return(_cret)

Static Function ValidaI3()
aArea:=GetArea()
_cret:= !empty(ALLTRIM(_custo)) .and. ExistCpo("CTT",_custo)
if _cret
	dbSelectArea("CTT")
	dbSetOrder(1)
	dbSeek(xFilial("CTT")+_custo)
	_nDescCusto:=CTT->CTT_DESC01
//	_CCUNID:=CTT->CTT_CCUNID
	@ 034, 080 SAY _nDescCusto
//	@ 044, 036 SAY _CCUNID
endif
RestArea(aArea)
dlgRefresh(oDlg3)
ObjectMethod(oDlg3,"Refresh()")
return(_cret)


Static Function ValidaCmp
mes:=substr(_COMPETE,1,2)
ANO:=substr(_COMPETE,3,4)
_cret:=.t.
if mes < "01" .or. mes >"12"
	_cret:= .f.
endif
if ano <"1989" .or. ano >"2020"
	_cret:= .f.
endif
Return( _cret )
