#Include 'Protheus.ch'
/**
Filtra o browser
http://tdn.totvs.com/display/public/PROT/MNTA8304+-+Ponto+de+Entrada+para+adicionar+filtro+no+Browser
**/
User Function MNTA8304()
Local _cMens
_cMens := "Deseja filtrar somente as manutencoes ativas?"
	If MsgYesNo(_cMens,"ATENÇÃO - MNTA8304","YESNO")
		cCondTmp := " .And. ST9->T9_SITMAN = 'A' .And. ST9->T9_SITBEM = 'A' "
	Else
		cCondTmp := ""
	EndIf
	
Return cCondTmp

