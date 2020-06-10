#INCLUDE "protheus.ch"
/*
MESSENGER PARA O EVENTO DE GERAÇÃO DE BORDERÔS (_GB)              
*/
User Function MMENS_GB
Local _aDados:=paramixb[1]
Local _cMensagem:=""

_cMensagem:="INCLUSÃO DE/EM BORDERÔ: "
_cMensagem+="<br>Borderô: "+_aDados[1]
_cMensagem+="<br>Tipo: "+_aDados[2]
_cMensagem+="<br>Valor total: "+_aDados[3]
_cMensagem+="<br>Usuário: "+_aDados[4]

Return(_cMensagem)