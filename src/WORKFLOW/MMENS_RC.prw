#INCLUDE "protheus.ch"
/*
MESSENGER PARA O EVENTO DE REVISÃO DE CONTRATO NO MÓDULO GESTÃO DE CONTRATOS(_RC)              
*/
User Function MMENS_RC
Local _aDados:=paramixb[1]
Local _cMensagem:=""

If _aDados[1]=="1" //Inclusão
	_cMensagem:="INCLUSÃO DE REVISÃO DE CONTRATO"
ElseIf _aDados[1]=="2" //Exclusão
	_cMensagem:="EXCLUSÃO DE REVISÃO DE CONTRATO"
Else //Aprovação
	_cMensagem:="APROVAÇÃO DE REVISÃO DE CONTRATO"
Endif

If _aDados[1]=="1" //Inclusão
	_cMensagem+="<br>Contrato: "+_aDados[2]
	_cMensagem+="<br>Revisão base: "+_aDados[3]
	_cMensagem+="<br>Revisão gerada: "+_aDados[4]
	_cMensagem+="<br>Tipo: "+_aDados[5]
	_cMensagem+="<br>Justificativa: "+_aDados[6]
	_cMensagem+="<br>Clausula: "+_aDados[7]
	_cMensagem+="<br>Responsável pela inclusão: "+_aDados[8]
ElseIf _aDados[1]=="2" //Exclusão
	_cMensagem+="<br>Contrato: "+_aDados[2]
	_cMensagem+="<br>Revisão base: "+_aDados[3]
	_cMensagem+="<br>Revisão gerada: "+_aDados[4]
	_cMensagem+="<br>Responsável pela exclusão: "+_aDados[5]
Else //Aprovação
	_cMensagem+="<br>Contrato: "+_aDados[2]
	_cMensagem+="<br>Revisão gerada: "+_aDados[3]
	_cMensagem+="<br>Clausula: "+_aDados[4]
	_cMensagem+="<br>Justificativa: "+_aDados[5]
	_cMensagem+="<br>Responsável pela aprovação: "+_aDados[6]
Endif

Return(_cMensagem)