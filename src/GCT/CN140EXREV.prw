#INCLUDE "protheus.ch"

//Ponto de entrada executado após a exlcusão da revisão de contrato

User Function CN140EXREV()
Local _cContrato	:= PARAMIXB[1]    //código do contrato
Local _cRevbase	 	:= PARAMIXB[2]	  //Código da revisão base
Local _cRevnova		:= PARAMIXB[3]	  //Código da revisão gerada
Local _aDados := {}

If cEmpAnt<>"03" .AND. cEmpAnt<>"04" .AND. cEmpAnt<>"08" .AND. cEmpAnt<>"06"  .AND. cEmpAnt<>"99" //Executado apenas para Construtora e empresa Teste
	Return(Nil)
Endif

aAdd(_aDados,"2")
aAdd(_aDados,_cContrato)
aAdd(_aDados,_cRevbase)
aAdd(_aDados,_cRevnova)
aAdd(_aDados,cUserName)
MEnviaMail("_RC",_aDados,,,,.T.)

Return(nil)
