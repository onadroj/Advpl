#include "protheus.ch"
/*
O ponto de entrada FA050INC - será executado na validação da Tudo Ok na inclusão do contas a pagar
*/
User Function FA050INC()
Local _lRet := .T.
Local _cCntObr := ""  
Local _aDados := {}


If cEmpAnt <> "03"  .OR. cEmpAnt <> "04"  .OR. cEmpAnt <> "08"  .OR. cEmpAnt <> "06"   .OR. cEmpAnt <> "99"  // Executado apenas para a Construtora Sant'Anna ou empresa Teste 
	Return(.T.)										
Endif


_cCntObr := RetField("SA2",1,xFilial("SA2")+M->E2_FORNECE+M->E2_LOJA,"A2_CNTOBR")
If _cCntObr=="1"
	MsgStop("Para este fornecedor é exigida utilização de contrato, com pedido de compra gerado por medição." +chr(13)+chr(10) ;
		   +"A entrada de documentos deve ser feita pela rotina Documento de Entrada no módulo Compras.")
	_lRet := .F.
ElseIf _cCntObr=="3"
	aAdd(_aDados,M->E2_FORNECE)
	aAdd(_aDados,M->E2_LOJA)
	aAdd(_aDados,RetField("SA2",1,xFilial("SA2")+M->E2_FORNECE+M->E2_LOJA,"A2_NOME"))
	aAdd(_aDados,M->E2_NUM + " - Prefixo: "+M->E2_PREFIXO)
	aAdd(_aDados,cUserName)
	aAdd(_aDados,"Contas a Pagar")
	MEnviaMail("_CO",_aDados,,,,.T.)
Endif

Return(_lRet)