#INCLUDE "PROTHEUS.CH" 
#INCLUDE "TOPCONN.CH" 
#INCLUDE "FWBROWSE.CH" 
#INCLUDE "RWMAKE.CH" 
#INCLUDE "FWMVCDEF.CH" 


User Function LIBBOR_MVC()
	
	Local aCoors := FWGetDialogSize( oMainWnd )
	Local oPanelUp, oFWLayer, oPanelLeft, oPanelRight, oBrowseUp, oBrowseDown,oMark, oRelacZA4, oRelacZA5
	
	Private oDlgPrinc
	
	Define MsDialog oDlgPrinc Title 'Liberação de Borderos' From aCoors[1], aCoors[2] To aCoors[3], aCoors[4] Pixel
	
	//
	// Cria o conteiner onde serão colocados os browses
	
	//
	oFWLayer := FWLayer():New()
	oFWLayer:Init( oDlgPrinc, .F., .T. )
	
	//
	// Define Painel Superior
	//
	oFWLayer:AddLine( 'UP', 50, .F. )
	// Cria uma "linha" com 50% da tela
	oFWLayer:AddCollumn( 'ALL', 100, .T., 'UP' )
	// Na "linha" criada eu crio uma coluna com 100% da tamanho dela
	oPanelUp := oFWLayer:GetColPanel( 'ALL', 'UP' )
	// Pego o objeto desse pedaço do container
	//
	// Painel Inferior
	//
	oFWLayer:AddLine( 'DOWN', 50, .F. )
	oFWLayer:AddCollumn( 'LEFT' , 100, .T., 'DOWN' )
	oPanelLeft := oFWLayer:GetColPanel( 'LEFT' , 'DOWN' ) // Pego o objeto do pedaço esquerdo
	
	//
	// FWMarkBrowse Superior
	//
	
	oMark := FWMarkBrowse():New()
	oMark:SetOwner(oPanelUp)
	oMark:SetAlias('SZJ')
	oMark:SetDescription('Liberação de Borderos')
	oMark:SetFieldMark('ZJ_OK')
	oMark:AddLegend("ZJ_LIBERAD=='S'","GREEN","Liberado")
	oMark:AddLegend("ZJ_LIBERAD=='N'","RED","Não Liberado")
	oMark:AddLegend("ZJ_LIBERAD=='I'","YELLOW","Não Lib. - Incluido Titulo")
	oMark:AddLegend("ZJ_LIBERAD=='X'","BLUE","Não Lib. - Removido Titulo")
//	oMark:SetFilterDefault("ZJ_LIBERAD<>'S'")
	
	oMark:Activate()
	
	
	//
	// FWmBrowse Inferior
	//
	oBrowseDown:= FWMBrowse():New()
	oBrowseDown:SetOwner( oPanelLeft )
	oBrowseDown:SetDescription( 'Títulos a Pagar' )
	oBrowseDown:SetAlias( 'SE2' )
	oBrowseDown:SetMenuDef( '' )
	oBrowseDown:SetProfileID( '2' )
	oBrowseDown:ForceQuitButton()
	oBrowseDown:SetFilterDefault("SE2->E2_NUMBOR<>''")
	oBrowseDown:DisableDetails()
	oBrowseDown:Activate()
	
	
	// Relacionamento entre os Paineis
	oRelacZA4:= FWBrwRelation():New()
	oRelacZA4:AddRelation( oMark , oBrowseDown , { { 'E2_FILIAL', 'xFilial( "SE2" )' }, {'E2_NUMBOR','ZJ_BORDERO'} })
	oRelacZA4:Activate()
	
	Activate MsDialog oDlgPrinc Center
	
Return NIL

Static Function MenuDef()
	Local aRotina :={}
	ADD OPTION aRotina TITLE 'Liberar' ACTION 'U_COMP025PROC()' OPERATION 4 ACCESS 0
	ADD OPTION aRotina TITLE 'Sair' ACTION 'oDlgPrinc:End()' OPERATION 2 ACCESS 0
return aRotina

//-------------------------------------------------------------------
/*/{Protheus.doc} ViewDef
Definição do interface

@author JORDANO

@since 06/08/2014
@version 1.0
/*/
//-------------------------------------------------------------------

Static Function ViewDef()
	Local oView
	Local oModel := ModelDef()	
	oView := FWFormView():New()	
	oView:SetModel(oModel)
	
Return oView

//-------------------------------------------------------------------
/*/{Protheus.doc} ModelDef
Definição do modelo de Dados

@author JORDANO

@since 06/08/2014
@version 1.0
/*/
//-------------------------------------------------------------------

Static Function ModelDef()
Local oModel

oModel := MPFormModel():New('ModelName')
oModel:SetDescription('Liberação de Borderos')


Return oModel

Static Function FPedeSenha()
// Desenha a tela do programa
	Local _Ok:= .F.
	Local NOPC
	Local __Senha := Space(10)
	
	@ 064, 033 TO 190,375 DIALOG oDlg TITLE "Senha de Liberacao"
	@ 020, 015 SAY "Senha : "
	@ 020, 060 GET __Senha  Valid .T. PASSWORD

	@ 050, 098 BMPBUTTON TYPE 1 ACTION (NOPC:=1,Close(oDlg))
	@ 050, 126 BMPBUTTON TYPE 2 ACTION Close(oDlg)

	ACTIVATE DIALOG oDlg CENTER

	IF NOPC = 1
		PswOrder(1)	//	1 - ID do usuário/grupo 2 - Nome do usuário/grupo; 3 - Senha do usuário 4 - E-mail do usuário
		If PswSeek( __cUserId, .T. )
			_Ok := PswName( __Senha )
		EndIf
		if !_Ok
			MsgAlert("Senha Incorreta!")
		endif
	END IF	
//	_ok:=.T.
return(_ok)  

User Function COMP025PROC()
	Local aArea := GetArea()
	Local cMarca := oMark:Mark()
	Local nCt := 0
	Local bCondic := "ZJ_LIBERAD<>'S'"
	Local _aDados := {}
	SZJ->( dbGoTop() )
	SZJ->(dbSetFilter( {||ZJ_LIBERAD<>'S'}, "ZJ_LIBERAD<>'S'"))
	if FPedeSenha()
		While !SZJ->( EOF() )
			If oMark:IsMark(cMarca)
				nCt++
				RecLock("SZJ",.F.)
				REPLACE SZJ->ZJ_LIBERAD WITH 'S'
				REPLACE SZJ->ZJ_LIBPOR WITH LogUserName()
				REPLACE SZJ->ZJ_DATA WITH dDataBase
				MsUnLock()
			EndIf
			If cEmpAnt=="03" .OR. cEmpAnt=="04" .OR. cEmpAnt=="08" .OR. cEmpAnt=="99"
				aAdd(_aDados,SZJ->ZJ_BORDERO)
				aAdd(_aDados,IIF(SZJ->ZJ_TIPO=="P","Pagar","Receber"))
				aAdd(_aDados,AllTrim(Transform(SZJ->ZJ_VALOR,"@E 999,999,999.99")))
				aAdd(_aDados,SZJ->ZJ_LIBPOR)
				aAdd(_aDados,"Liberação")
				MEnviaMail("_LB",_aDados,,,,.T.)
			EndIf
			SZJ->( dbSkip() )
		End
	endif
	if nCt == 0
		ApMsgInfo('Nenhum Bordero Liberado!')
	else
		ApMsgInfo( 'Total de Borderos Liberados: ' + AllTrim( Str( nCt ) ) )
	endif
	RestArea( aArea )
	oMark:GoTo(1,.T.)
Return NIL

