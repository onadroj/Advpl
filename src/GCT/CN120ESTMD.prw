#INCLUDE "protheus.ch"

//Ponto de entrada na gravação do estorno de medições de contratos
User Function CN120ESTMD()
Local _aDados := {}

aAdd(_aDados,"Estorno")
aAdd(_aDados,CND->CND_NUMMED)
aAdd(_aDados,CND->CND_CONTRA)
aAdd(_aDados,CND->CND_REVISA)
aAdd(_aDados,cUserName)
If !Empty(CND->CND_FORNEC)
	aAdd(_aDados,CND->CND_FORNEC+"/"+CND->CND_LJFORN)
	aAdd(_aDados,RetField("SA2",1,xFilial("SA2")+CND->CND_FORNEC+CND->CND_LJFORN,"A2_NOME"))
Else
	aAdd(_aDados,CND->CND_CLIENT+"/"+CND->CND_LOJACL)
	aAdd(_aDados,RetField("SA1",1,xFilial("SA1")+CND->CND_CLIENT+CND->CND_LOJACL,"A1_NOME"))
Endif

MEnviaMail("_MD",_aDados,,,,.T.) 

Return()