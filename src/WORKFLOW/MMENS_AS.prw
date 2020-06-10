#INCLUDE "protheus.ch"
/*
MESSENGER PARA O EVENTO DE ALTERAÇÃO SALARIAL (_AS)              
*/
User Function MMENS_AS
Local _aDados:=paramixb[1]
Local _cMensagem:=""

_cMensagem:="ALTERAÇÃO SALARIAL EFETUADA: "
_cMensagem+="<br>Funcionário: "+_aDados[1]
_cMensagem+="<br>Função: "+_aDados[7]
_cMensagem+="<br>Situação: "+_aDados[8]
_cMensagem+="<br>C.Custo: "+_aDados[9]
_cMensagem+="<br>Salário anterior: R$ "+_aDados[2]
_cMensagem+="<br>Novo salário: R$ "+_aDados[3]
_cMensagem+="<br>Motivo: "+_aDados[4]
_cMensagem+="<br>Data: "+_aDados[5]
_cMensagem+="<br>Usuário: "+_aDados[6]

Return(_cMensagem)