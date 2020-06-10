#INCLUDE "protheus.ch"
/*
MESSENGER PARA EVENTOS DE MEDIÇÕES (_MD) - Encerramento e Estorno
*/
User Function MMENS_MD()
Local _aDados:=paramixb[1]
Local _cMensagem:=""

_cMensagem:="Medição de Contrato"
_cMensagem+="<br>Operação: "+_aDados[1]
_cMensagem+="<br>Medição No.: "+_aDados[2]
_cMensagem+="<br>Contrato: "+_aDados[3]
_cMensagem+="<br>Revisão: "+_aDados[4]
_cMensagem+="<br>"+_aDados[6]+" - "+_aDados[7]
_cMensagem+="<br>Responsável: "+_aDados[5]

Return(_cMensagem)