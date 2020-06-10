//Alteracao: 03/06/03 - Voltou a permitir rateios entre unidades (Edson)
#include "rwmake.ch"
//#include "protheus.ch"
User Function MT100TOK()
LOCAL lRet:=.T.
LOCAL _ACHOU:=.F.
Local _cCntFor:=""
Local _cCntPrd:=""
Local _nItem:=0
Local _nPosProd:=aScan( aHeader, { |aItem| Alltrim(aItem[2])=="D1_COD" })
Local _nPosPed:=aScan( aHeader, { |aItem| Alltrim(aItem[2])=="D1_PEDIDO" })
Local _aDados:={}
PUBLIC cF1_HIST        
PUBLIC _lImpOP := .F.

If cEmpAnt == "03" .OR. cEmpAnt == "04" .OR. cEmpAnt == "08" .OR. cEmpAnt == "06" .OR. cEmpAnt == "99"  
	_cCntFor:=RetField("SA2",1,xFilial("SA2")+cA100For+cLoja,"A2_CNTOBR")   
	If _cCntFor=="1" .AND. !LNFMEDIC .AND. !(cTipo$"DB")
		MsgStop ("Para este fornecedor é exigida utilização de contrato."+chr(13)+chr(10) ;
				+"O campo Filtra Medição deve permanecer marcado e os itens deste documento " +chr(13)+chr(10) ;
			 	+"devem ser associados a pedidos de compra gerados por medições.")
		lRet := .F.
    ElseIf _cCntFor=="3" .AND. !LNFMEDIC .AND. !(cTipo$"DB")
    	aAdd(_aDados,cA100For)
    	aAdd(_aDados,cLoja)
		aAdd(_aDados,RetField("SA2",1,xFilial("SA2")+cA100For+cLoja,"A2_NOME"))
    	aAdd(_aDados,cNFiscal + " - Serie: "+cSerie)
		aAdd(_aDados,cUserName)
		aAdd(_aDados,"Doc. Entrada")
	Endif
	
	If lRet 
	   For _nItem:= 1 to Len(aCols)
	      If !aCols[_nItem,Len(aCols[_nItem])]
			 _cCntPrd:=RetField("SB1",1,xFilial("SB1")+aCols[_nItem,_nPosProd],"B1_CNTOBR")   
	         If _cCntFor=="1" .AND. !(cTipo$"DB") .AND. Empty(aCols[_nItem,_nPosPed])
				MsgStop ("Para este fornecedor é exigida utilização de contrato."+chr(13)+chr(10) ;
						+"Todos itens deste documento devem ser associados a pedidos de compra gerados por medições.")
				lRet := .F.         
				Exit
			 ElseIf _cCntPrd=="1" .AND. !(cTipo$"DB") .AND. (!LNFMEDIC .OR. (LNFMEDIC .AND. Empty(aCols[_nItem,_nPosPed])))
				MsgStop ("Para o produto da linha "+str(_nItem)+" é exigida utilização de contrato."+chr(13)+chr(10) ;
						+"Deve ser marcado o campo Filtra Medição e todos itens deste documento devem ser " ;
						+"associados a pedidos de compra gerados por medições.")
				lRet := .F.         
				Exit
			 ElseIf (_cCntFor=="3" .OR. _cCntPrd=="3") .AND. !(cTipo$"DB") .AND. (!LNFMEDIC .OR. (LNFMEDIC .AND. Empty(aCols[_nItem,_nPosPed])))
		    	If Len(_aDados)==0
		    		aAdd(_aDados,cA100For)
		    		aAdd(_aDados,cLoja)
		    		aAdd(_aDados,RetField("SA2",1,xFilial("SA2")+cA100For+cLoja,"A2_NOME"))
		    		aAdd(_aDados,cNFiscal + " - Serie: "+cSerie)
					aAdd(_aDados,cUserName)
                    aAdd(_aDados,"Doc. Entrada")
		    	Endif
			 Endif
	      Endif
	   Next
	Endif
Endif


If lRet .AND. MaFisRet(,"NF_BASEDUP") > 0
	if type("cbco")=="U"
   		lRet:=.f.
  		mSgStop("Condição de pagamento é CX. Falta definir o caixa no qual o titulo foi baixado!")
	endif
endif
if lRet .AND. !(CTIPO $ "BD")
	if MaFisRet(,"NF_VALIRR")#0 .AND. EMPTY(SA2->A2_CODRET)
		lRet:=MSGBOX('Documento possui IRRF e código de retenção não existe no cadastro do fornecedor. Proceda a manutenção. '+chr(10)+chr(13)+'O código de retenção e o campo "Gera DIRF" estão preenchidos?',"MT100TOK","YESNO")
	endif
endif        
IF lRet .AND. CTIPO=="N" .AND. MaFisRet(,"NF_VALISS") > 0  //NOTA COM ISS VALIDA ESPECIES
	IF !(ALLTRIM(cEspecie)$"NFPS/NFSE/NFF")
   		lRet:=.f.
  		MsgStop("Espécie inválida por se tratar de uma Nota fiscal de Serviço. Válidas: NFPS,NFF,NFSE.")
	ENDIF
ENDIF
/*
if( LEN(alltrim(cSerie))<3)
	MsgStop("A série da NF deve ter 3 dígitos!")
	lRet:=.f.
endif
*/
if lRet
	IF INCLUI
		cF1_HIST:=CRIAVAR("F1_HIST")
	ELSE
		cF1_HIST:=SF1->F1_HIST
	ENDIF
	while len(alltrim(cF1_HIST))==0
		@ 96,42 TO 250,500	 DIALOG oDlg TITLE "Solicitação de Dados"
		@ 05,05 TO 74,170 TITLE "Parâmetros"
		@ 15,14 Say "Entre com o Histórico" size 100,9
		@ 25,14 Get cF1_HIST picture "@!" size 150,9 VALID len(alltrim(cF1_HIST))<=45
		@ 10,200 BMPBUTTON TYPE 01 ACTION (Close(oDlg))
		ACTIVATE DIALOG oDlg   CENTERED
	enddo

	if (cEmpAnt=="03" .OR. cEmpAnt=="04" .OR. cEmpAnt=="99") 
		If CTIPO=="N" .AND. MaFisRet(,"NF_BASEDUP") > 0
		   If ApMsgYesNo("Deseja imprimir as Ordens de Pagamento após a gravação"+chr(13)+chr(10)+ ;
	                         "dos títulos a pagar?")
		      _lImpOP := .T.
		   Endif
		Endif
		If Len(_aDados)>0
			MEnviaMail("_CO",_aDados,,,,.T.)
		Endif
	Endif          
endif

RETURN(lRet)

/*
_TESCol := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "D1_TES"}))
FOR X:=1 TO LEN(ACOLS)
	if aCols[x,len(aCols[1])] <> .T. //nao deletado
		IF 	RETFIELD("SF4",1,XFILIAL("SF4")+aCols[x,_TESCol],"F4_DUPLIC")=="S"
			_ACHOU:=.T.
			EXIT
		endif
	endif
NEXT

if CTIPO<>"D" .AND. alltrim(ccondicao)=="" .AND. _ACHOU
	lRet:=.f.
endif
IF CTIPO<>"D" .AND. LEN(ADUPL)>0 .AND. !("CX" $ ccondicao)
	Dias:= GetMV("MV_VENCMIN")
	dDtIni := MSDATE()
	dDtFim := CTOD(SUBSTR(ADUPL[1],16,08))
	nDUtil :=0
	While dDtIni<dDtFim
		If DataValida(dDtIni) == dDtIni
			nDUtil := nDUtil + 1
		Endif
		dDtIni := dDtIni + 1
	EndDo
	if nDUtil<Dias .OR. dDtFim<dDtIni
		MsgStop("Vencimento real menor que o prazo minimo de dias uteis estabelecido em MV_VENCMIN!")
		lRet:=.f.
	endif
ENDIF

//TESTA CENTROS DE CUSTOS
//	_ccCol := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "D1_CCUNID"}))
//	_ccaux:=aCols[1,_ccCol]
//	if len(acols) > 1
//		FOR X:=2 TO LEN(ACOLS)
//		    if aCols[x,len(aCols[1])] <> .T. //nao deletado
//		  		IF 	aCols[x,_ccCol]<>_ccaux
//					lRet:=.F.
//					msgstop("Centros de custos dos itens devem pertencer a uma unica unidade.")
//				endif
//		    endif
//		NEXT
//	endif
Return(lRet)
*/