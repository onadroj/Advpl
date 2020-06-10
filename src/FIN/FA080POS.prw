#INCLUDE "rwmake.ch"
#INCLUDE "TOPCONN.CH"
/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³NOVO23    º Autor ³ AP6 IDE            º Data ³  16/07/14   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ Codigo gerado pelo AP6 IDE.                                º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP6 IDE                                                    º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
O ponto de entrada FA080POS é chamado apos carregar os dados do titulo
a ser baixado, antes da visualização de informações na tela. Deste modo 
será possível alterar variavéis de memória com os dados do título. 
Variáveis disponibilizadas : cBanco , cAgencia, cConta, cCheque

/*/

User Function FA080POS


//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Declaracao de Variaveis                                             ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

Local aArea := GetArea()
Local lRet := .T.
Local lMNat      
Local cQuery
Local nTotReg
lMNat := RETFIELD("SE2",1,XFILIAL("SE2")+SE2->E2_PREFIXO+SE2->E2_NUM,"SE2->E2_MULTNAT")  
       
if lMNat == "1"
	cQuery := "SELECT * FROM "+RETSQLNAME("SEZ")+"  WHERE EZ_NUM = '"+SE2->E2_NUM+"' AND"
	cQuery +=" EZ_PREFIXO = '"+SE2->E2_PREFIXO+"' AND"
	cQuery +=" EZ_PARCELA = '"+SE2->E2_PARCELA+"' AND"
	cQuery +=" EZ_TIPO ='"+SE2->E2_TIPO+"' AND "
	cQuery +=" EZ_CLIFOR = '"+SE2->E2_FORNECE+"' AND EZ_LOJA = '"+SE2->E2_LOJA+"' AND D_E_L_E_T_ <> '*' "  
	
	TCQUERY cQuery NEW ALIAS "QRY"
	dbSelectArea("QRY")
	nTotReg := Contar("QRY","!Eof()")
	QRY->(DbGoTop())
	if nTotReg > 0
		MsgBox("O titulo deve ser baixado com multiplas naturezas!", "FA080POS", "Alert")
		lRet := .F.     
	else
		MsgBox("Titulo com multiplas naturezas mas nao houve rateio na inclusao do titulo!", "FA080POS", "Alert")
		lRet := .F.     
	endif         
	QRY->(DbCloseArea())
	RestArea(aArea)
endif	
Return lRet

/*                          
lMNat := RETFIELD("SE2",1,XFILIAL("SE2")+SE2->E2_PREFIXO+SE2->E2_NUM,"SE2->E2_MULTNAT")  
 A VARIAVEL LMULTNAT É DE ESCOPO LOCAL DENTRO DO PROGRAMA PRINCIPAL PORTANTO NAO ENCONTREI FORMA DE VALIDAR SEU VALOR 
cQuery := " SELECT * FROM "+RETSQLNAME("SEZ")+"  WHERE EZ_NUM = '"+SE2->E2_NUM+"'"
TCQUERY cQuery NEW ALIAS "QRY"
dbSelectArea("QRY")
dbgotop()
ALERT("valor: "+QRY->QTD)
if lMNat == "1" .AND. QRY->QTD > 0
	alert(QRY->QTD)
	MsgBox("O titulo deve ser baixado com multinaturezas!", "FA080POS", "Alert")
	lRet := .F.
endif
*/