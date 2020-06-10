#include "rwmake.ch"
/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡ao    ³          ³ Autor ³GATASSE       ³                          ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡Æo ³ Altera filtro de contas a PAGAR  dependendo do filtro     ³±±
±±³          ³                                                            ³±±
±±³          ³                                                            ³±±
±±³          ³                                                            ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Uso       ³ contas a pagar / bordero                                   ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Tabelas   ³                                                            ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Modulo    ³                                                            ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/

User Function F240FIL()     

	Local nAchou := .F.
	Local aAreaX:=GetArea()
	Local aAreaSE2
	Private cRet,nRadioX,aRadioX
	
	DbSelectArea("SE2")      
	aAreaSE2:=GetArea()
	DbSetOrder(3)  //E2_FILIAL+DTOS(E2_VENCREA)+E2_NOMFOR+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO
	
	dbSeek(xFilial("SE2")+DTOS(DVENINI240),.T.)
	
	While !EOF() .And. !nAchou .And. xFilial("SE2") == SE2->E2_FILIAL .And. DTOS(SE2->E2_VENCREA) <= DTOS(DVENFIM240)
		If SE2->E2_SALDO > 0 .And. Alltrim(SE2->E2_TIPO)<>"PR" .And. Alltrim(SE2->E2_NUMBOR)=="" .And. Alltrim(SE2->E2_NUMBCO)=="" .And. Alltrim(SE2->E2_CODBAR)=="" .AND. Alltrim(SE2->E2_BCOPREF)==""
			nAchou:=.T.
			MSGSTOP("HA TITULOS SEM COD. BARRAS OU BCO. PREF. NESTE INTERVALO DE DATAS!")
		Else
			dbSkip()
		Endif
	EndDo 
	
	RESTAREA(aAreaSE2)
	
	cRet:= ""
	//cTipo:=space(1)
	aRadioX := {"1-Mesmo banco do Fornecedor", "2-Com Boleto","3-DOC ou TED","4-Caixa","5-Todos"}
	//aRadioX := {"1-Com Cnab","2-Todos"}
	nRadioX := 1
	//cFil:=space(4)
	
	NOK:=.T.
	
	Do while NOK                       
		@ 0, 0 TO 360,360 DIALOG oDlg TITLE "FILTRO"
		@ 12,12 TO 170,140 TITLE "Filtro"
		@ 23,20 RADIO aRadioX VAR nRadioX
		@ 020, 145 BMPBUTTON TYPE 01 ACTION ConfirmaX()
		@ 045, 145 BMPBUTTON TYPE 02 ACTION (cRet:="",NOK:=.F.,Close(oDlg))
		ACTIVATE DIALOG oDlg CENTER
	EndDo
	
	//Filtrar Só CSC, Outros ou Todos na Meganova
	/*************
	If cEmpAnt=="06"
	aRadioX := {"1-Meganova", "2-CSC","3-Todos"}
	nRadioX := 1
	NOK:=.T.
	Do while NOK                       
	@ 0, 0 TO 360,360 DIALOG oDlg TITLE "Filtro Meganova"
	@ 12,12 TO 170,140 TITLE "Centros de Custos"
	@ 23,20 RADIO aRadioX VAR nRadioX
	@ 020, 145 BMPBUTTON TYPE 01 ACTION ConfirmaZ()
	@ 045, 145 BMPBUTTON TYPE 02 ACTION (NOK:=.F.,Close(oDlg))
	ACTIVATE DIALOG oDlg CENTER
	EndDo
	Endif
	**************/
	RestArea(aAreaX)
	           
Return(cRet)



Static Function ConfirmaX()
	***********************************************************************
	* Cria a tela para pergunta
	***********************************************************************
	
	NOK:=.F.                                     
	
	if nRadioX==1
		cRet:= ' E2_BCOPREF = "'+CPORT240+'" .and. ALLTRIM(E2_CODBAR)=="" .and. E2_FLUXO =="S" '
	endif
	
	if nRadioX==2
		cRet:= " (!EMPTY(E2_CODBAR) .AND. E2_FLUXO ='S') " 
	endif
	
	if nRadioX==3
		cRet:= ' E2_BCOPREF <> "'+CPORT240+'" .and. ALLTRIM(E2_BCOPREF)<>"" .and. ALLTRIM(E2_CODBAR)="" .and. !ISALPHA(E2_BCOPREF) .and. E2_FLUXO ="S" '
	endif
	
	if nRadioX==4
		cRet:= ' E2_BCOPREF = "'+CPORT240+'" .and.  ALLTRIM(E2_BCOPREF)<>"" .and. E2_FLUXO ="N" .AND. E2_CTABAIX="'+CPORT240+CAGEN240+CCONTA240+'" '
	endif
	
	if nRadioX==5 
		cRet:= ''
	endif
	
	Close(oDlg)     

Return        

//018957 05 D 26/11/09   
/*
Static Function ConfirmaZ()
NOK:=.F.                                     
if nRadioX==1                   
If AllTrim(cRet)#""
cRet+= '.and. SUBSTR(E2_CUSTO,1,4) # "1067" '
Else
cRet:= ' SUBSTR(E2_CUSTO,1,4) # "1067" '
Endif
endif
if nRadioX==2
If AllTrim(cRet)#""
cRet+= '.and. SUBSTR(E2_CUSTO,1,4) == "1067" ' 
Else
cRet:= ' SUBSTR(E2_CUSTO,1,4) == "1067" ' 
Endif  
endif
Close(oDlg)     
Return        
*/