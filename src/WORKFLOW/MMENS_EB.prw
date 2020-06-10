#INCLUDE "protheus.ch"
/*
MESSENGER PARA O EVENTO DE EXCLUSÃO DE BORDERÔS (_EB)              
*/
User Function MMENS_EB
Local _aDados:=paramixb[1]
Local _cMensagem:=""

_cMensagem:="EXCLUSÃO DE/EM BORDERÔ: "
_cMensagem+="<br>Borderô: "+_aDados[1]
_cMensagem+="<br>Usuário: "+_aDados[2]

Return(_cMensagem)