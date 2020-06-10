#INCLUDE 'PROTHEUS.CH'

/*
Ponto de entrada executado nas rotinas que efetuam alteração salarial. 
Dispara evento M-Messenger para envio de e-mail.
Rotinas:
GPEA010 (Cadastro de Funcionários)
GPEM690 (Dissídio Retroativo)
CSAM080 (Atualização do Aumento Programado)
GPER200 (Aumento Salarial)
GPEA250 (Histórico de Salário)
*/

User Function GPALTSAL()
Local _aDados := {}

If cEmpAnt<>"03" .AND. cEmpAnt<>"04" .AND. cEmpAnt<>"99"
	Return()
EndIf

aAdd(_aDados,SRA->RA_MAT + "-" + SRA->RA_NOME)
aAdd(_aDados,AllTrim(Transform(SRA->RA_SALARIO,"@E 999,999,999.99")))
aAdd(_aDados,AllTrim(Transform(SR3->R3_VALOR,"@E 999,999,999.99")))
aAdd(_aDados,RetField("SX5",1,xFilial("SX5")+"41"+SR3->R3_TIPO,"X5_DESCRI"))
aAdd(_aDados,DTOC(SR3->R3_DATA))
aAdd(_aDados,cUserName)
aAdd(_aDados,SRA->RA_CODFUNC+"-"+RetField("SRJ",1,xFilial("SRJ")+SRA->RA_CODFUNC,"RJ_DESC"))
aAdd(_aDados,RetField("SX5",1,xFilial("SX5")+"31"+SRA->RA_SITFOLH,"X5_DESCRI"))
aAdd(_aDados,SRA->RA_CC+"-"+RetField("CTT",1,xFilial("CTT")+SRA->RA_CC,"CTT_DESC01"))
MEnviaMail("_AS",_aDados,,,,.T.)


Return()