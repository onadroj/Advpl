#INCLUDE "protheus.ch"
/*
MESSENGER PARA O EVENTO DE INCLUSÃO DE DOCUMENTO SEM CONTRATO PARA FORNECEDOR OU PRODUTO COM CONTRATO=DESEJAVEL (CTAS. A PAGAR OU DOC. ENTRADA) (_CO)              
*/
User Function MMENS_CO
Local _aDados:=paramixb[1]
Local _cMensagem:=""

_cMensagem:="INCLUSÃO DE REGISTRO PARA FORNECEDOR/PRODUTO COM CONTRATO DESEJÁVEL: "
_cMensagem+="<br>Fornecedor: "+_aDados[1]
_cMensagem+="<br>Loja: "+_aDados[2]
_cMensagem+="<br>Nome: "+_aDados[3]
_cMensagem+="<br>Documento: "+_aDados[4]
_cMensagem+="<br>Rotina: "+_aDados[6]
_cMensagem+="<br>Responsável: "+_aDados[5]

Return(_cMensagem)