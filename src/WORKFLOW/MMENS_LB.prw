#INCLUDE "protheus.ch"
/*
MESSENGER PARA O EVENTO DE LIBERAÇÃO DE BORDERÔS (_LB)              
*/
User Function MMENS_LB
Local _aDados:=paramixb[1]
Local _cMensagem:=""

_cMensagem:="BORDERÔ: "
_cMensagem+="<br>Operação: "+_aDados[5]
_cMensagem+="<br>Borderô: "+_aDados[1]
_cMensagem+="<br>Tipo: "+_aDados[2]
_cMensagem+="<br>Valor: "+_aDados[3]
_cMensagem+="<br>Responsável: "+_aDados[4]

Return(_cMensagem)