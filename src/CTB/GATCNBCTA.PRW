#include "rwmake.ch"

User Function GATCNBCTA()

	/*
	Utilizado como gatilho, para buscar a conta contábil no cadastro do produto de acordo com o tipo de movimento informado
	*/
	
	Local _nTipo
	Local _nProd
	Local _Tipo 
	Local _Prod
	Local _nValor 	:= ""
	Local _aAreaX	:= GetArea()
	Local oModel  	:= FWModelActive()
	Local oModelCNB	:= oModel:GetModel("CNBDETAIL")
	Local _cProd	:= oModelCNB:GetValue("CNB_PRODUT")
	Local _cTipo	:= oModelCNB:GetValue("CNB_TPENT")

	//_nTipo := (aScan(aHeader, {|aItem| AllTrim(aItem[2]) == "CNB_TPENT"}))
	//_nProd := (aScan(aHeader, {|aItem| AllTrim(aItem[2]) == "CNB_PRODUT"}))	
	//_cTipo := AllTrim(aCols[N, _nTipo])
	//_cProd := AllTrim(aCols[N, _nProd])
	
	dbSelectArea("SB1")
	dbSetOrder(1)
	dbSeek(xFilial("SB1") + _cProd)
	
	If _cTipo <> "" .AND. _cProd <> ""
	
		If _cTipo == "I"
			_nValor := "2204010103"   // Conta Transitoria de Ativo Fixo - Mesmo conceito do GAT0002.PRX
		EndIf
		
		If _cTipo == "E"
			_nValor := SB1->B1_CONTA
		EndIf
		
		If _cTipo == "D"
			_nValor := SB1->B1_CTBADES
		EndIf
		
		If _cTipo == "C"
			_nValor := SB1->B1_CTBACUS
		EndIf
		
		If _cTipo == "Y"
			_nValor := SB1->B1_CTBACI
		EndIf
		
		If _nValor == ""
			_nValor := "DIGITE"
		EndIf
	EndIf
	
	RestArea(_aAreaX)

Return(_nValor)

