#INCLUDE "PROTHEUS.CH"

User Function MEDEXCEL(_cMed,_cItem)
Local _aDados := {}
Local _cGrupo := "" 
Local _cDescProd := ""
Local _nSaldo := 0
Local _cRet := ""
Local _cPessoa :=""
Local _cDesc := ""

If _cItem == "ITENS"
	DbSelectArea("CNE")
	DbSetOrder(4) //CNE_FILIAL+CNE_NUMMED
	If DbSeek(xFilial("CNE")+_cMed)	
		While !Eof() .AND. xFilial("CNE")+CNE->CNE_NUMMED==xFilial("CNE")+_cMed
			_cGrupo := RetField("SB1",1,xFilial("SB1")+CNE->CNE_PRODUT,"B1_GRUPO")
			_cDescProd := Alltrim(RetField("SB1",1,xFilial("SB1")+CNE->CNE_PRODUT,"B1_DESC"))
			DbSelectArea("CNB")
			DbSetOrder(1) //CNB_FILIAL+CNB_CONTRA+CNB_REVISA+CNB_NUMERO+CNB_ITEM
			If DbSeek(xFilial("CNB")+CNE->CNE_CONTRA+CNE->CNE_REVISA+CNE->CNE_NUMERO+CNE->CNE_ITEM)	
				_nSaldo := CNB->CNB_VLTOT-((CNE->CNE_QTDSOL-CNE->CNE_QTAMED+CNE->CNE_QUANT)*CNE->CNE_VLUNIT)
				aAdd(_aDados, {_cGrupo,CNE->CNE_PRODUT,_cDescProd,CNE->CNE_CONTA,CNB->CNB_UM,CNB->CNB_QUANT,CNE->CNE_QUANT,CNE->CNE_QTDSOL-CNE->CNE_QTAMED+CNE->CNE_QUANT,CNE->CNE_VLUNIT,   ;
				               CNB->CNB_VLTOT,CNE->CNE_VLTOT,(CNE->CNE_QTDSOL-CNE->CNE_QTAMED+CNE->CNE_QUANT)*CNE->CNE_VLUNIT,_nSaldo,Round((_nSaldo/CNB->CNB_VLTOT)*100,2)})
		    Else
		    	aAdd(_aDados,"Planilha não localizada!")
		    Endif
			
			DbSelectArea("CNE")
			dbSkip()
		End
    Else
    	aAdd(_aDados,"Medição não localizada!")
	EndIf
	
	Return(_aDados)
EndIf

If _cItem == "CONT"
	_cRet := Alltrim(RetField("CND",4,xFilial("CND")+_cMed,"CND_CONTRA"))
	Return(_cRet)
Endif

If _cItem == "REV"
	_cRet := RetField("CND",4,xFilial("CND")+_cMed,"CND_REVISA")
	Return(_cRet)
Endif

If _cItem == "PLAN"
	_cRet := RetField("CND",4,xFilial("CND")+_cMed,"CND_NUMERO")
	Return(_cRet)
Endif

If _cItem == "COMP"
	_cRet := RetField("CND",4,xFilial("CND")+_cMed,"CND_COMPET")
	Return(_cRet)
Endif

If _cItem == "DATA"
	_cRet := DTOC(RetField("CND",4,xFilial("CND")+_cMed,"CND_DTFIM"))
	Return(_cRet)
Endif

If _cItem == "VENC"
	_cRet := DTOC(RetField("CND",4,xFilial("CND")+_cMed,"CND_DTVENC"))
	Return(_cRet)
Endif

If _cItem == "CLIFOR"
	DbSelectArea("CND")
	DbSetOrder(4) //CND_FILIAL+CND_NUMMED
	If DbSeek(xFilial("CND")+_cMed)	
		If !Empty(CND->CND_FORNEC)
			_cRet := Alltrim(RetField("SA2",1,xFilial("SA2")+CND->CND_FORNEC+CND->CND_LJFORN,"A2_NOME"))
		Else
			_cRet := Alltrim(RetField("SA1",1,xFilial("SA1")+CND->CND_CLIENT+CND->CND_LOJACL,"A1_NOME"))
		Endif
	Endif		
	Return(_cRet)
Endif

If _cItem == "END"
	DbSelectArea("CND")
	DbSetOrder(4) //CND_FILIAL+CND_NUMMED
	If DbSeek(xFilial("CND")+_cMed)	
		If !Empty(CND->CND_FORNEC)
			DbSelectArea("SA2")
			DbSetOrder(1)
			DbSeek(xFilial("SA2")+CND->CND_FORNEC+CND->CND_LJFORN)
			_cRet := Alltrim(SA2->A2_END)+"-"+Alltrim(SA2->A2_BAIRRO)+"-"+Alltrim(SA2->A2_MUN)+"/"+Alltrim(SA2->A2_EST)
		Else
			DbSelectArea("SA1")
			DbSetOrder(1)
			DbSeek(xFilial("SA1")+CND->CND_CLIENT+CND->CND_LOJACL)
			_cRet := Alltrim(SA1->A1_END)+"-"+Alltrim(SA1->A1_BAIRRO)+"-"+Alltrim(SA1->A1_MUN)+"/"+Alltrim(SA1->A1_EST)
		Endif
	Endif		
	Return(_cRet)
Endif

If _cItem == "EMAIL"
	DbSelectArea("CND")
	DbSetOrder(4) //CND_FILIAL+CND_NUMMED
	If DbSeek(xFilial("CND")+_cMed)	
		If !Empty(CND->CND_FORNEC)
			_cRet := Alltrim(RetField("SA2",1,xFilial("SA2")+CND->CND_FORNEC+CND->CND_LJFORN,"A2_EMAIL"))
		Else
			_cRet := Alltrim(RetField("SA1",1,xFilial("SA1")+CND->CND_CLIENT+CND->CND_LOJACL,"A1_EMAIL"))
		Endif
	Endif		
	Return(_cRet)
Endif

If _cItem == "CONTATO"
	DbSelectArea("CND")
	DbSetOrder(4) //CND_FILIAL+CND_NUMMED
	If DbSeek(xFilial("CND")+_cMed)	
		If !Empty(CND->CND_FORNEC)
			_cRet := Alltrim(RetField("SA2",1,xFilial("SA2")+CND->CND_FORNEC+CND->CND_LJFORN,"A2_CONTATO"))
		Else
			_cRet := Alltrim(RetField("SA1",1,xFilial("SA1")+CND->CND_CLIENT+CND->CND_LOJACL,"A1_CONTATO"))
		Endif
	Endif		
	Return(_cRet)
Endif

If _cItem == "TEL"
	DbSelectArea("CND")
	DbSetOrder(4) //CND_FILIAL+CND_NUMMED
	If DbSeek(xFilial("CND")+_cMed)	
		If !Empty(CND->CND_FORNEC)
			_cRet := Alltrim(RetField("SA2",1,xFilial("SA2")+CND->CND_FORNEC+CND->CND_LJFORN,"A2_TEL"))
		Else
			_cRet := Alltrim(RetField("SA1",1,xFilial("SA1")+CND->CND_CLIENT+CND->CND_LOJACL,"A1_TEL"))
		Endif
	Endif		
	Return(_cRet)
Endif

If _cItem == "CNPJ"
	DbSelectArea("CND")
	DbSetOrder(4) //CND_FILIAL+CND_NUMMED
	If DbSeek(xFilial("CND")+_cMed)	
		If !Empty(CND->CND_FORNEC)
			_cPessoa := RetField("SA2",1,xFilial("SA2")+CND->CND_FORNEC+CND->CND_LJFORN,"A2_TIPO")
			_cRet := Transform(RetField("SA2",1,xFilial("SA2")+CND->CND_FORNEC+CND->CND_LJFORN,"A2_CGC"),IIF(_cPessoa=="J","@R 99.999.999/9999-99","@R 999.999.999-99"))
		Else
			_cPessoa := RetField("SA2",1,xFilial("SA2")+CND->CND_FORNEC+CND->CND_LJFORN,"A1_PESSOA")
			_cRet := Transform(RetField("SA1",1,xFilial("SA1")+CND->CND_CLIENT+CND->CND_LOJACL,"A1_CGC"),IIF(_cPessoa=="J","@R 99.999.999/9999-99","@R 999.999.999-99"))
		Endif
	Endif		
	Return(_cRet)
Endif

If _cItem == "DESCS"
	DbSelectArea("CNQ")
	DbSetOrder(1) //CNQ_FILIAL+CNQ_NUMMED+CNQ_TPDESC
	If DbSeek(xFilial("CNQ")+_cMed,.T.)	
		While !Eof() .AND. xFilial("CNQ")+CNQ->CNQ_NUMMED==xFilial("CNQ")+_cMed
				_cDesc := AllTrim(RetField("CNP",1,xFilial("CNP")+CNQ->CNQ_TPDESC,"CNP_DESCRI"))
				aAdd(_aDados, {_cDesc,CNQ->CNQ_VALOR})
			dbSkip()
		End
	Else
		aAdd(_aDados,{" ",0})
	EndIf
	
	Return(_aDados)
EndIf

If _cItem == "ACRES"
	DbSelectArea("CNR")
	DbSetOrder(1) //CNR_FILIAL+CNR_NUMMED
	If DbSeek(xFilial("CNR")+_cMed)	
		While !Eof() .AND. xFilial("CNR")+CNR->CNR_NUMMED==xFilial("CNR")+_cMed
				aAdd(_aDados, {CNR->CNR_DESCRI,CNR->CNR_VALOR})
			dbSkip()
		End
	Else
		aAdd(_aDados,{" ",0})
	EndIf
	
	Return(_aDados)
EndIf


Return(Nil)