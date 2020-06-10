#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³FA070TIT  º Autor ³ GATASSE            º Data ³  11/10/02   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ PEDE DADOS COMPLEMENTARES PARA TITULO DE ALUGUEL.          º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP5 IDE                                                    º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User Function FA070TIT


//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Declaracao de Variaveis                                             ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
IF SE1->E1_ALUGUEL <>"S"  //SAI SE NAO FOR ALUGUEL
	RETURN(.T.)
ENDIF
nRetorno:=.t.    
IF NDESCONT>0    //Verifica se foi digitada a taxa de administracao como desconto na tela de baixa
   TXADM:=NDESCONT
ELSE
   TXADM:=SE1->E1_TXADM
ENDIF
ACATR:=SE1->E1_ACATR
IPTU:=SE1->E1_IPTU
DESCALU:=SE1->E1_DESCALU
ANUNCIO:=SE1->E1_ANUNCIO
TXCOND:=SE1->E1_TXCOND
CUSTAS:=SE1->E1_CUSTAS

NOK:=.T.
Do while NOK
	@ 004, 033 TO 260,280 DIALOG oDlg TITLE "ALUGUEL: DADOS COMPLEMENTARES"
	@ 010, 005 SAY "Tx.Administ  "
	@ 010, 055 GET TXADM Size 60,8 PICTURE "@E 999,999,999,999.99" Valid positivo()
	@ 022, 005 SAY "Acrs.Atraso  "
	@ 022, 055 GET ACATR Size 60,8 PICTURE "@E 999,999,999,999.99" Valid positivo()
	@ 034, 005 SAY "IPTU         "
	@ 034, 055  GET IPTU Size 60,8 PICTURE "@E 999,999,999,999.99" Valid positivo()
	@ 046, 005 SAY "Dsc.Aluguel   "
	@ 046, 055 GET DESCALU Size 60,8 PICTURE "@E 999,999,999,999.99" Valid positivo()
	@ 058, 005 SAY "Desp.Anuncio "
	@ 058, 055  GET ANUNCIO Size 60,8 PICTURE "@E 999,999,999,999.99" Valid positivo()
	@ 070, 005 SAY "Tx.Condominio"
	@ 070, 055  GET TXCOND Size 60,8 PICTURE "@E 999,999,999,999.99" Valid positivo()
	@ 082, 005 SAY "Custas Proc. "
	@ 082, 055 GET CUSTAS Size 60,8 PICTURE "@E 999,999,999,999.99" Valid positivo()
	@ 100, 080 BMPBUTTON TYPE 01 ACTION Confirma()
	@ 100, 040 BMPBUTTON TYPE 02 ACTION (nRetorno:=.F.,NOK:=.F.,Close(oDlg))
	ACTIVATE DIALOG oDlg CENTER
EndDo

Return(nRetorno)

Static Function Confirma()
NOK:=.F.
Close(oDlg)
RECLOCK("SE1",.F.)
REPLACE SE1->E1_TXADM   WITH TXADM
REPLACE SE1->E1_ACATR   WITH ACATR
REPLACE SE1->E1_IPTU    WITH IPTU
REPLACE SE1->E1_DESCALU WITH DESCALU
REPLACE SE1->E1_ANUNCIO WITH ANUNCIO
REPLACE SE1->E1_TXCOND  WITH TXCOND
REPLACE SE1->E1_CUSTAS  WITH CUSTAS                             
MSUNLOCK()
Return
