#INCLUDE "protheus.ch"
/*
MESSENGER PARA EVENTOS DE PEDIDOS DE VENDAS (_PV)
*/
User Function MMENS_PV()
Local _aDados:=paramixb[1]
Local _cMensagem:=""

_cMensagem:="Pedido de Venda"
_cMensagem+="<br>Operação: "+_aDados[1]
_cMensagem+="<br>Número: "+_aDados[2]
_cMensagem+="<br>Tipo: "+_aDados[3]
_cMensagem+="<br>C. Custo: "+_aDados[6]
_cMensagem+="<br>Financeiro: "+_aDados[7]

If Substr(_aDados[3],1,1) $ "DB"
	_cMensagem+="<br>Fornecedor: "+_aDados[4]
Else
	_cMensagem+="<br>Cliente: "+_aDados[4]
Endif

_cMensagem+="<br>Responsável: "+_aDados[5]

Return(_cMensagem)