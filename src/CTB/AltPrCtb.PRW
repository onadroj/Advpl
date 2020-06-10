#INCLUDE "rwmake.ch"
#INCLUDE "PROTHEUS.CH"
#Include "FiveWin.ch"
#Include "Colors.ch" 

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³ALTPRCON  ºAutor  ³Microsiga           º Data ³  11/12/09   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³                                                            º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP                                                        º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/

User Function AltPrCtb()
	if ! select("SM0") > 0
		RpcSetType( 3 )
		RpcSetEnv( "03", "01" )
	endif
	FTrocaPar()
Return()

static function FTrocaPar()
Local bOk  		        := {|| Iif(GravaSx6(),oDlg:End(),AllWaysTrue())}
Local oFont   			:= TFont():New( "Arial",,14,,.T.,,,,.T.,.F. )
Local oDlg        
Private dDataFis		:=Getmv("MV_DATAFIS") 
Private dDataDE	    	:=Getmv("MV_DATADE") 
Private dDataATE		:=Getmv("MV_DATAATE") 
Private dDataINI		:=Getmv("MV_DATAINI") 
Private dDataFIM 		:=Getmv("MV_DATAFIM") 
Private dDataEST 		:=Getmv("MV_ULMES") 
Private dDataFIN 		:=Getmv("MV_DATAFIN") 
Private dDataBLQ 		:=Getmv("MV_DBLQMOV") 

DEFINE MSDIALOG oDlg FROM 000,000 To 270,500 PIXEL TITLE OemToAnsi('Altera Data Fiscal')

oGroup1 := TGroup():New(035,005,130,240,"",oDlg,,,.T.,.T.)
                           
///oSay01  := tSay():New(028,030,{|| OemToAnsi('Data Fiscal: ') },oDlg,,oFont,,,,.T.,,,280,050)
@ 043,010 Say OemToAnsi("Data Fiscal - MV_DATAFIS") PIXEL SIZE 100,09 font oFont of oDlg 
@ 042,150 MSGet oGet  Var dDataFis Size 040,08 of oDlg Pixel 
@ 055,010 Say OemToAnsi("Dt.Ctb Calendário Inicial - MV_DATADE") PIXEL SIZE 110,09 font oFont of oDlg 
@ 054,150 MSGet oGet  Var dDataDE Size 040,08 of oDlg Pixel 
@ 067,010 Say OemToAnsi("Dt.Ctb Calendário Final - MV_DATAATE") PIXEL SIZE 110,09 font oFont of oDlg 
@ 066,150 MSGet oGet  Var dDataATE Size 040,08 of oDlg Pixel 
@ 079,010 Say OemToAnsi("Dt.Ctb Outros Inicial - MV_DATAINI") PIXEL SIZE 110,09 font oFont of oDlg 
@ 078,150 MSGet oGet  Var dDataINI Size 040,08 of oDlg Pixel 
@ 091,010 Say OemToAnsi("Dt.Ctb Outro Final - MV_DATAFIM") PIXEL SIZE 110,09 font oFont of oDlg 
@ 090,150 MSGet oGet  Var dDataFIM Size 040,08 of oDlg Pixel 
@ 103,010 Say OemToAnsi("Dt.Fechamento Estoque - MV_ULMES") PIXEL SIZE 110,09 font oFont of oDlg 
@ 102,150 MSGet oGet  Var dDataEST Size 040,08 of oDlg Pixel 
@ 115,010 Say OemToAnsi("Dt.Fechamento Financeiro - MV_DATAFIN") PIXEL SIZE 110,09 font oFont of oDlg 
@ 114,150 MSGet oGet  Var dDataFIN Size 040,08 of oDlg Pixel 
@ 127,010 Say OemToAnsi("Dt. Bloq. Estoque - MV_DBLQMOV") PIXEL SIZE 110,09 font oFont of oDlg 
@ 128,150 MSGet oGet  Var dDataBLQ Size 040,08 of oDlg Pixel 

Activate MsDialog oDlg Centered on init EnchoiceBar(oDlg,{|| Eval(bOk)},{|| oDlg:End()})
Return(.T.)


Static Function GravaSx6()
**************************************************************************************
*
*
*******                  

dbSelectArea("SX6")
if dbSeek(xfilial("SX6")+"MV_DATAFIS")
	If reclock("SX6",.F.)
		SX6->X6_CONTEUD := dtoc(dDataFis)
		SX6->(MsUnlock())
	Endif
Endif
dbSelectArea("SX6")
if dbSeek(xfilial("SX6")+"MV_DATADE")
	If reclock("SX6",.F.)
		SX6->X6_CONTEUD := dtoc(dDataDE)
		SX6->(MsUnlock())
	Endif
Endif
dbSelectArea("SX6")
if dbSeek(xfilial("SX6")+"MV_DATAATE")
	If reclock("SX6",.F.)
		SX6->X6_CONTEUD := dtoc(dDataATE)
		SX6->(MsUnlock())
	Endif
Endif
dbSelectArea("SX6")
if dbSeek(xfilial("SX6")+"MV_DATAINI")
	If reclock("SX6",.F.)
		SX6->X6_CONTEUD := dtoc(dDataINI)
		SX6->(MsUnlock())
	Endif
Endif
dbSelectArea("SX6")
if dbSeek(xfilial("SX6")+"MV_DATAFIM")
	If reclock("SX6",.F.)
		SX6->X6_CONTEUD := dtoc(dDataFIM)
		SX6->(MsUnlock())
	Endif
Endif

dbSelectArea("SX6")
if dbSeek(xfilial("SX6")+"MV_ULMES")
	If reclock("SX6",.F.)
		SX6->X6_CONTEUD := dtoc(dDataEST)
		SX6->(MsUnlock())
	Endif
Endif

dbSelectArea("SX6")
if dbSeek(xfilial("SX6")+"MV_DATAFIN")
	If reclock("SX6",.F.)
		SX6->X6_CONTEUD := dtoc(dDataFIN)
		SX6->(MsUnlock())
	Endif
Endif


dbSelectArea("SX6")
if dbSeek(xfilial("SX6")+"MV_DBLQMOV")
	If reclock("SX6",.F.)
		SX6->X6_CONTEUD := dtoc(dDataBLQ)
		SX6->(MsUnlock())
	Endif
Endif

Return(.T.)