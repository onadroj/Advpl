#INCLUDE 'PROTHEUS.CH'

/*
Ponto de entrada executado na gravação do cadastro de funcionários. 
Dispara evento M-Messenger para envio de e-mail caso o campo RA_EXAMEDI tenha sido alterado.
*/

User Function Gp010ValPE()
Local _aDados := {}

If (cEmpAnt<>"03" .AND. cEmpAnt<>"04" .AND. cEmpAnt<>"99") .OR. INCLUI .OR. (!INCLUI .AND. M->RA_EXAMEDI==SRA->RA_EXAMEDI)
	Return(.T.)
EndIf

aAdd(_aDados,SRA->RA_MAT + "-" + SRA->RA_NOME)
aAdd(_aDados,DTOC(SRA->RA_EXAMEDI))
aAdd(_aDados,DTOC(M->RA_EXAMEDI))
aAdd(_aDados,cUserName)
MEnviaMail("_EM",_aDados,,,,.T.)

Return(.T.)