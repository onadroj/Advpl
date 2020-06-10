#INCLUDE "protheus.ch"

//Ponto de entrada executado após a aprovação da revisão de contrato

User Function CN150APR()
Local _cContrato := CN9->CN9_NUMERO //Contrato
Local _cRevnova := CN9->CN9_REVISA //Revisão
Local _cClausula := AllTrim(MSMM(CN9->CN9_CODCLA)) //Cláusula 
Local _cJustif := AllTrim(MSMM(CN9->CN9_CODJUS)) //Justificativa   
Local _aDados := {}

If cEmpAnt<>"03" .AND. cEmpAnt<>"04" .AND. cEmpAnt<>"08" .AND. cEmpAnt<>"06"  .AND. cEmpAnt<>"99" //Executado apenas para Construtora e empresa Teste
	Return(Nil)
Endif

aAdd(_aDados,"3")
aAdd(_aDados,_cContrato)
aAdd(_aDados,_cRevnova)
aAdd(_aDados,_cClausula)
aAdd(_aDados,_cJustif)
aAdd(_aDados,cUserName)
MEnviaMail("_RC",_aDados,,,,.T.)

Return(Nil)