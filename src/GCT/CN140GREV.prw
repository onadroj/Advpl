#INCLUDE "protheus.ch"

//Ponto de entrada executado após a gravação da revisão de contrato

User Function CN140GREV()
Local _cContrato := PARAMIXB[1]   //Código do Contrato
Local _cRevbase  := PARAMIXB[2]   //Código da revisão base
Local _cRevnova  := PARAMIXB[3]   //Código da revisão gerada
Local _cTpRev    := PARAMIXB[4]   //Código do tipo de revisão selecionada
Local _cJustif   := PARAMIXB[5]   //Array of Record   Conteúdo da Justificativa           
Local _cClausula := PARAMIXB[6]   //Array of Record   Conteúdo da Clausula  
Local _aDados := {}

If cEmpAnt<>"03" .AND. cEmpAnt<>"04" .AND. cEmpAnt<>"08" .AND. cEmpAnt<>"06" .AND. cEmpAnt<>"99" //Executado apenas para Construtora e empresa Teste
	Return(Nil)
Endif

aAdd(_aDados,"1")
aAdd(_aDados,_cContrato)
aAdd(_aDados,_cRevbase)
aAdd(_aDados,_cRevnova)
aAdd(_aDados,Alltrim(RetField("CN0",1,xFilial("CN0")+_cTpRev,"CN0_DESCRI")))
aAdd(_aDados,_cJustif)
aAdd(_aDados,_cClausula)
aAdd(_aDados,cUserName)
MEnviaMail("_RC",_aDados,,,,.T.)

Return(Nil)