#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณMT241LOK  บ Autor ณ GATASSE            บ Data ณ  29/10/10   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDescricao ณ VALIDA LINHA DA MOVIMENTACAO INTERNA                       บฑฑ
ฑฑบ          ณ                                                            บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ AP6 IDE                                                    บฑฑ
ฑฑศออออออออออฯออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
/*/

User Function MT241LOK


//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณ Declaracao de Variaveis                                             ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
LOCAL lRet:=.T.
LOCAL ERRO:=.F.
LOCAL nPosOS:=(AScan(aHeader,{|aItem| UPPER(AllTrim(aItem[2]))=="D3_OP"}))
LOCAL nPosCod:=(AScan(aHeader,{|aItem| UPPER(AllTrim(aItem[2]))=="D3_COD"}))
LOCAL nPosUnd:=(AScan(aHeader,{|aItem| UPPER(AllTrim(aItem[2]))=="D3_UM"}))
LOCAL nPosQtd:=(AScan(aHeader,{|aItem| UPPER(AllTrim(aItem[2]))=="D3_QUANT"}))
LOCAL nPosItem:=(AScan(aHeader,{|aItem| UPPER(AllTrim(aItem[2]))=="D3_ITEMCTA"}))
LOCAL nPosCLVL:=(AScan(aHeader,{|aItem| UPPER(AllTrim(aItem[2]))=="D3_CLVL"}))
LOCAL cVet:=""
LOCAL cVetQtd:=""
LOCAL cVetIt:=""
LOCAL lSldOk:=.T.
LOCAL cTipo:=retfield("SF5",1,XFILIAL("SF5")+CTM,"F5_MANUTEN")
LOCAL cTipoMov:=RetField("SF5",1,xFilial("SF5")+cTM,"F5_TIPO")
//LOCAL cTipoCC:=retfield("CTT",1,XFILIAL("CTT")+CCC,"CTT_CCUNID")
Local lErroItem:=.f.
if ACOLS[N][len(acols[1])]==.f.
	IF (ALLTRIM(ACOLS[N][nPosOS])=="" .AND. cTipo=="1") .or. (ALLTRIM(ACOLS[N][nPosOS])<>"" .AND. cTipo<>"1")
		msgstop("Incompatibilidade do Tipo de Movimenta็ใo com a Numera็ใo da OP!" )
		lRet:=.f.
	endif 
	IF  substr(CCC, 1,1) == "3" .AND. Empty(ACOLS[N][nPosCLVL])
		msgstop("Para este Centro de Custo e obrigatorio informar classe de valor.","MT241LOK" )
	lRet:=.f.
	endif

	cOrdem:=substr(aCols[N][nPosOS],1,6)
	IF cTipoMov=="D" .AND. "OS" $ aCols[N][nPosOS]
		aArea:=GetArea()
		dbSelectArea("STL")
		dbSetOrder(1)
		dbSeek(xFilial("STL")+cOrdem,.T.)
		nQuant:=0
		while !EOF() .AND. xFilial("STL")+cOrdem==STL->TL_FILIAL+STL->TL_ORDEM
			if STL->TL_CODIGO==aCols[N,nPosCod]
				if AllTrim(STL->TL_UNIDADE)==AllTrim(aCols[N,nPosUnd])
					nQuant+=STL->TL_QUANTID
					if nQuant>=aCols[N,nPosQtd]
						Exit
					endif
				endif
			endif
			DbSkip()
		enddo
		if nQuant<aCols[N,nPosQtd]
			MsgStop("Quantidade insuficiente na OS, para devolu็ใo para estoque!")
			lRet:=.F.
		endif
		RestArea(aArea)
	endif
endif

Return(lRet)
