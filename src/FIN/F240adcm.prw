#include "rwmake.ch"

User Function F240ADCM()

/* 

Autor: Edson
Data: 03/08/09
Função: Retorna campos customizados do SE2 a incluir no arquivo temporário da rotina de geração de borderôs de 
pagamentos (utilização no f240fil, antes do browse para seleção de títulos para inclusão no borderô)

*/

Return({"E2_BCOPREF","E2_CTABAIX"})