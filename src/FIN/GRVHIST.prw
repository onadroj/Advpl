#INCLUDE "PROTHEUS.CH"

//Gravação do log de alterações em cadastros

User Function GRVHIST(cRotina,aDados)
LOCAL AAREA:=GETAREA()
LOCAL nopc:=0
Local ret:=.f.
Local cOrig, cAtual
Local aCampos := aDados
Local cRef:=""
Local aMsg :={}

Do Case
	Case AllTrim(cRotina)=="MATA020"   // Alteração do cadastro de Fornecedor
		cRef:="SA2"
	Case AllTrim(cRotina)=="MATA030"   // Alteração do cadastro de Clientes
		cRef:="SA1"
EndCase
if cRef # ""
	cData:=dtoc(MSDATE())+"-"+time()
	cUser:=__CUSERID+"-"+alltrim(CUSERNAME)
	cMotivo := CRIAVAR("ZZ1_MOTIVO")         
	
	dbSelectArea("ZZ1")                                           
	dbSetOrder(1)
	DEFINE MSDIALOG oDlg TITLE "Históricos de Alterações" FROM 33,25 TO 320,349 PIXEL
	@ 015,005 Say "Referência" SIZE 55, 7 OF oDlg PIXEL
	@ 015,040 MSGet  cRef  SIZE 60, 11 OF oDlg PIXEL when .f.
	@ 025,005 Say "Data/Hora" SIZE 55, 7 OF oDlg PIXEL
	@ 025,040 MSGet cData   SIZE 60, 11 OF oDlg PIXEL when .f.
	@ 035,005 Say "Rotina"    SIZE 55, 7 OF oDlg PIXEL
	@ 035,040 MSGet cRotina SIZE 60, 11 OF oDlg PIXEL  picture "@!" when .f.
	@ 045,005 Say "Usuário"   SIZE 55, 7 OF oDlg PIXEL
	@ 045,040 MsGet cUser SIZE 100, 11 OF oDlg PIXEL  picture "@!"    when .f.
	@ 055,005 Say "Motivo"   SIZE 55, 7 OF oDlg PIXEL
	@ 055,040 Get oMEMO  VAR cMotivo SIZE 100, 60 OF oDlg PIXEL  MEMO valid !empty(cMotivo)
	
	DEFINE SBUTTON FROM 130 ,80  TYPE 1 ACTION (iif(!empty(cMotivo),(oDlg:End(),nOpc:=1),nOpc:=0)) ENABLE OF oDlg
	ACTIVATE MSDIALOG oDlg CENTERED
	
	RESTAREA(AAREA)
	IF nOpc==1
		aAdd(aMsg, cUserName)
		aAdd(aMsg, cMotivo)
		Do Case
			Case cRef=="SA1"   // Cadastro de Clientes
				cMotivo:= "Justificativa para alteração do Cadastro do Cliente "+SA1->A1_COD+"/"+SA1->A1_LOJA+": "+ cMotivo + " Campos Alterados: "+Chr(13)+Chr(10)
				aAdd(aMsg, SA1->A1_COD+"/"+SA1->A1_LOJA+" - "+Alltrim(RetField("SA1",1,xFilial("SA1")+SA1->A1_COD+SA1->A1_LOJA,"A1_NOME")))
			Case cRef=="SA2"   // Cadastro de Fornecedores
				cMotivo:= "Justificativa para alteração do Cadastro do Fornecedor "+SA2->A2_COD+"/"+SA2->A2_LOJA+": "+ cMotivo + " Campos Alterados: "+Chr(13)+Chr(10)
				aAdd(aMsg, SA2->A2_COD+"/"+SA2->A2_LOJA+" - "+Alltrim(RetField("SA2",1,xFilial("SA2")+SA2->A2_COD+SA2->A2_LOJA,"A2_NOME")))
		EndCase
		For L := 1 To Len(aCampos)
			cMotivo += Alltrim(aCampos[L,4])+ " - Vlr Original ["+Alltrim(aCampos[L,2])+"] - Modificado ["+ Alltrim(aCampos[L,3])+"]"+Chr(13)+Chr(10)
			aAdd(aMsg, Alltrim(aCampos[L,4])+ " - Vlr Original ["+Alltrim(aCampos[L,2])+"] - Modificado ["+ Alltrim(aCampos[L,3])+"]")
		Next L

		reclock("ZZ1",.T.)
		REPLACE	ZZ1_FILIAL 	WITH XFILIAL("ZZ1")
		REPLACE	ZZ1_REF 	WITH cRef
		REPLACE	ZZ1_DATA   	WITH dDatabase
		REPLACE	ZZ1_HORA   	WITH time()
		REPLACE	ZZ1_ROTINA 	WITH cRotina
		REPLACE	ZZ1_USER 	WITH __CUSERID
		REPLACE	ZZ1_NAME 	WITH cUserName
		REPLACE	ZZ1_MOTIVO 	WITH cMotivo
		msunlock()

		Do Case
			Case cRef=="SA1"   // Cadastro de Clientes
				MEnviaMail("_AC",aMsg,,,,.T.)
			Case cRef=="SA2"   // Cadastro de Fornecedores
				MEnviaMail("_AF",aMsg,,,,.T.)
		EndCase

		ret:=.t.
	Endif
else
	Ret := .T.
Endif
Return(ret)
