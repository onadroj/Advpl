#INCLUDE "protheus.ch"
/*
MESSENGER PARA O EVENTO DE INCLUSÃO DE DOCUMENTO ENTRADA GERANDO TÍTULOS COM DOCUMENTAÇÃO OBRIGATÓRIA PARA LIBERAÇÃO PARA BAIXA NO FINANCEIRO (_DO)              
*/
User Function MMENS_DO
Local _aDados:=paramixb[1]
Local _cMensagem:=""

_cMensagem:="O documento de entrada abaixo foi incluído no sistema, relacionado a medição(ões) de contrato(s)."
_cMensagem+="Para o(s) tipo(s) do(s) contrato(s) relacionado(s), serão exigidos os documentos listados, para liberação para baixa dos títulos a pagar."
_cMensagem+="<br><br>Documento/Série: "+_aDados[1]
_cMensagem+="<br>Fornecedor/Loja: "+_aDados[2]
_cMensagem+="<br>Responsável pela inclusão: "+_aDados[3]
_cMensagem+="<br><br>Contrato(s) e documento(s) exigido(s): "+_aDados[4]
_cMensagem+="<br><br>"

Return(_cMensagem)