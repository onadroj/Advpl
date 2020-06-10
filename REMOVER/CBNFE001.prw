/*
*****************************************************************************************************************************************************************
 IMPORTA NFe Entrada Direto da SEFAZ SEM CERTIFICADO DIGITAL !!!!
  
 BOSSWARE | JOSÉ TEIXEIRA - jose.teixeira@bossware.com.br | teixeira.totvs@gmail.com
 SET/2014


PREMISSAS :

1 - Quando efetuar Pré-Nota, sem uso da rotina de importaçãp do XML, o registro será bloqueado até que o COMPRAS librere o registro em questão.
1.1 - OBS : Documento de Entrada da mesma forma. ( F1_MSBLQL ) + F1_ISPC == 2

2 - Quando efetuar a rotina de importação da NFe, o irá Gerar Pré-Nota com campo F1_ISPC ( CONTÉM PEDIDO DE COMPRAS COM STATUS 1 =, 0 SEM PEDIDO DE COMPRAS )

3 - Não será permitido o uso da classificação da NFe caso esteja bloqueado E/OU campo F1_ISPC 1= Ped.Com, 0 = Sem PC, 2 = Pré Nota / Doc.Entreda sem PC
		( F1_MSBLQL ) + F1_ISPC == 1


[ O MENU  ]                                          

+---------------------------------------------------+
|SIGAEST -> ATUALIZAÇÕES -> MISCELANEA -> DANFE NFe |
+---------------------------------------------------+

<Menu Status="Enable">
		<Title lang="pt">&Miscelanea</Title>
		<Title lang="es">&Miscelanea</Title>
		<Title lang="en">&Miscellaneous</Title>
		<Menu Status="Enable">
			<Title lang="pt">DANFE NFe</Title>
			<Title lang="es">DANFE NFe</Title>
			<Title lang="en">DANFE NFe</Title>
			<MenuItem Status="Enable">
				<Title lang="pt">Recepção</Title>
				<Title lang="es">Recepção</Title>
				<Title lang="en">Recepção</Title>
				<Function>CBNFE001</Function>
				<Type>3</Type>
				<Tables>SC2</Tables>
				<Access>xxxxxxxxxx</Access>
				<Module>04</Module>
				<Owner>4</Owner>
			</MenuItem>
</MenuItem>			
*****************************************************************************************************************************************************************
*/



#include "TOTVS.CH"  

#DEFINE  __ENTER__ CHR( 13 ) + CHR( 10 )                
#DEFINE __CONNECT 'TOPCONN'  
#DEFINE __TRUE__   .T.
#DEFINE __FALSE__  .F.
#DEFINE __OFF NIL  
#DEFINE MDITOP 13
#DEFINE CLR_BACKGROUND RGB(123,161,202)
#DEFINE CLR_BACKLOGIX  RGB(232,239,247)
#DEFINE CLR_LINE       RGB(192,192,192)
#DEFINE CLR_LIGHTBLUE  RGB(217,235,255)
#DEFINE CLR_DARKBLUE   RGB(21,92,158)
#DEFINE CLR_TTVWHTBLUE RGB(235,238,247)
#DEFINE CLR_TTVBLUEWCL RGB(160,183,237)  
#DEFINE C1 
#DEFINE C2
#DEFINE C3 
#define GRID_MOVEUP       0
#define GRID_MOVEDOWN     1
#define GRID_MOVEHOME     2
#define GRID_MOVEEND      3
#define GRID_MOVEPAGEUP   4
#define GRID_MOVEPAGEDOWN 5           

#define BTN_BLUE  " QPushButton { background-color: qlineargradient(spread:reflect, x1:0.500318, y1:1, x2:1, y2:0.1, stop:0.0795455 rgba(83, 126, 170, 255), stop:1 rgba(234, 254, 255, 227)); border: 1px solid rgb(0,196,225) } QPushButton:hover {  background-color: qlineargradient(spread:reflect, x1:0.500318, y1:1, x2:1, y2:0.1, stop:0.0795455 rgba(0, 225, 239, 255), stop:1 rgba(234, 254, 255, 227)); border: 1px solid rgb(0,225,239) } QPushButton:pressed {  background-color: qlineargradient(spread:reflect, x1:0.500318, y1:1, x2:1, y2:0.1, stop:0.0795455 rgba(0, 206, 230, 255), stop:1 rgba(234, 254, 255, 227)); border: 0px solid rgb(0,206,230) } "
#define TXT_MEMO  " Q3TextEdit {background: white}Q3TextEdit {border-style: solid; border-width: 1px; border-color: #7f9db9}Q3TextEdit:hover {border-color: #ffbd69} "
#define TXT_LINE  " QLineEdit {background-color: white; border-style: transparent ; color: #b6b6b6; font: 'Segoe UI';border-style: solid; border-width: 1px; border-style: transparent ; border-color: #7f9db9} QLineEdit:hover {border-color: #ffbd69}"
#define CSS_TBO   " QTab{background-image:url(rpo:DarkBorderWhite.png);}QTabBarWidget{background-image:url(rpo:DarkBorderWhite.png);}QTabBar{background-color:#cccccc;}QWidget{ background-color: qlineargradient(spread:top, x1:0.500318, y1:1, x2:1, y2:0.1, stop:0.0795455 rgba(83, 126, 170, 255), stop:1 rgba(234, 254, 255, 227)); border: 0px solid rgb(0,196,225);} "
#define BTN_CYAN  ' QPushButton { background-color: qlineargradient(spread:reflect, x1:0.500318, y1:1, x2:1, y2:0.30, stop:0.0795455  rgba(0, 126, 170, 255), stop:1 rgba(100, 254, 255, 227));color: #000000; font: "Segoe UI";font-size:09px; } QPushButton:hover {  background-color: qlineargradient(spread:reflect, x1:0.500318, y1:1, x2:1, y2:0.1, stop:0.0795455 rgba(0, 225, 239, 255), stop:1 rgba(234, 254, 255, 227)); color: #FFFFFF; font: "Segoe UI";font-size:09px;   } QPushButton:pressed {  background-color: qlineargradient(spread:reflect, x1:0.500318, y1:1, x2:1, y2:0.1, stop:0.0795455 rgba(0, 206, 230, 255), stop:1 rgba(234, 254, 255, 227)) } '
                                        

User Function CBNFE001   
private aCabec 		:= {}
private aItens 		:= {}
private aLinha 		:= {}

private lUsaPedCom := .F.
private lExistPC := .F.
private lSelectPC := .F.
private cPerg := "CBNFE001"
private vCHAVE_NFE := ""
private cCadastro := "CBandNFe| Nota Fiscal Entrada"
private oInfo
private cInfo := Space(600)
private oDlg
private qBTN_BLUE := ""
private __vNFeAmb := ""
private __vNFeRet := ""
private __vNFePro := ""
 
private nPIPIIT := ""
private nVIPIIT	:= ""
 
private lMsErroAuto := .F.
private lContinue := .F.
private aList := {}
private __aLST := {}
private oOK   := LoadBitmap(GetResources(),'br_verde')    
private oNO   := LoadBitmap(GetResources(),'br_vermelho') 
private lProcess := .F. 

qBTN_BLUE += " QPushButton {"
qBTN_BLUE += "    font: 1em;             "
qBTN_BLUE += "    margin: 0 1px 0 1px;"
qBTN_BLUE += "    color: white;"
qBTN_BLUE += "    background-color: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1, "
qBTN_BLUE += "                                      stop: 0 #2198c0, stop: 1 #0d5ca6);"
qBTN_BLUE += "    border-style: outset;"
qBTN_BLUE += "    border-radius: 3px;"
qBTN_BLUE += "    border-width: 1px;"
qBTN_BLUE += "    border-color: #0c457e;"
qBTN_BLUE += "}"
qBTN_BLUE += " "
qBTN_BLUE += "QPushButton:pressed {                      "
qBTN_BLUE += "    background-color: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,"
qBTN_BLUE += "                                      stop: 0 #0d5ca6, stop: 1 #2198c0);"
qBTN_BLUE += "}" "

private __OPC
private __aField := {}
private aCores := {}
private aRotina := { {"Pesquisar","AxPesqui",0,1  } ,;
    		         {"Visualizar","U_GETNFEIT()",0,2 } ,;
	        	     {"NFeSeFaz","U_TONFE",0,3    } ,;
        		     {"Excluir","U_TONFE",0,4    } ,;
           		     {"Pre-Nota","U_TONFENT()",0,5    },; 
           		     {"Produto|Fornecedor","U_BNFE001()",0,6    },;            		     
           		     {"Legenda","U_LEGZF1()",0,7    }}
                                          

private cDelFunc := ".T."  

Private cAlias := "ZF1"
       
private vAG
private vCC     
private vCH
private cComp          
private vCodCli
private vCli                                                  
private vNF          
private vSerie := ""
private vPrefix
private vParc
private vDtVenc
private vValor  
private vVlrCH := 0.00
private oVlrCH 
dbSelectArea("ZF1")
dbSetOrder(1)




Aadd(aCores, { " ZF1_STATUS  == ' ' .OR. EMPTY(ZF1_STATUS) "  ,"BR_VIOLETA" })     
Aadd(aCores, { " ZF1_STATUS  == 'E' " ,"BR_VERDE" })     
Aadd(aCores, { " ZF1_STATUS  == 'X' " ,"BR_PRETO" })     
 
 
dbSelectArea(cAlias)
mBrowse( 6, 1, 22, 75, "ZF1",,,,,, aCores )


 

Return



User Function TONFE( cAlias,nReg,nOpc )  
local cStatement
private oNumero
private cNumero := Space(44) 
private __PROCESS := .F.
private __aCHEQUE := {}            
private __VALOR_NF := 0

// CHEQUE

// <39915064<0062778135>115060959507:
// <00400484<0080000495>100002873305:
// <39915064<0062778135>115060959507:
// <10414339<0070002275>200300047503:
private oFont := TFont():New('Arial',,14,,.F.,,,,.F.,.F.)
private oFontch := TFont():New('Arial',,50,,.T.,,,,.F.,.F.)
private oFontBC := TFont():New('Arial',,20,,.T.,,,,.F.,.F.)
private oFontBold := TFont():New('Arial',,14,,.T.,,,,.F.,.F.)
        
__OPC := nOpc

IF __OPC == 4   
                   

	IF MsgYesNo(" Deseja excluir o movimento ? ", "TOTVS")
	   
		cStatement := " UPDATE " + RETSQLNAME("ZF1") + " SET  D_E_L_E_T_ = '*'  WHERE R_E_C_N_O_ = " + cValToChar( ZF1->(Recno()) )
		 
	
		IF (TCSQLExec(cStatement) < 0)
		    Return MsgStop("TCSQLError() " + TCSQLError())
		EndIf
	    TCRefresh(RETSQLNAME("ZF1"))
		Return	
	EndIF	
EndIF

IF __OPC == 3
   
	oDlg := MSDialog():New(180,180,360,1200,'DANFE|NFe',,,,,CLR_BLACK,CLR_WHITE,,,.T.)  
	 
	oSay:= TSay():New( 10 ,10 ,{||"CHAVE NFe"}, oDlg ,,oFontBold,,,,.T.,CLR_BLACK,CLR_WHITE,100,20) 
	
	oNumero := TGet():New( 20,;
	              10 ,{|u|if(PCount()>0, cNumero := U, cNumero )},oDlg,500,40,;
	              "",,0,,oFontch,.F.,,.T.,,.F.,,.F.,.F.,,.F.,.F.,,'cNumero',,,, )
	
	oNumero:lReadOnly := __FALSE__
	//oNumero:bWhen := {|| bValid() }
	vCHAVE_NFE := cNumero
	oNumero:bValid := {|| __PNFe( cNumero, @oDlg ) }
	//oNumero:bChange := {|| bValid() }
	 
	
	 
	oBtnCan := TButton():New( 70 , 10,;
								"Cancelar",oDlg,{|| __CLOSE( @oDlg ) },80,15,,,,.T.)
	oBtnCan:SetCSS( qBTN_BLUE )
	
	//oBtnOk := TButton():New( 70 , 95,;
	//							"Confirmar",oDlg,{|| IIF( __PROCESS , __PNFe( cNumero ), __OFF ) },80,15,,,,.T.)
	//oBtnOk:SetCSS( qBTN_BLUE )
	
	//oDlg:Activate(,,,.T.,{||msgstop('validou!'),.T.},,{||msgstop('iniciando…')} )
	
	//oDlg:Owner():lEscClose:= .F. // desabilita o fechamento por ESC da tela
	//oDlg:LMAXIMIZED := .T.
	
	oDlg:LCENTERED := .T.
	
	oDlg:Activate(,,,.T.,{||  },,{||} )

EndIF
 
Return .T.
//<39915064<0062778135>115060959507:
//<00105852<0078500165>801001406196:
/*
   BOSSWARE
*/
Static Function __CLOSE(__OBJ)
	__OBJ:End()
Return

Static Function  __PNFe(  cNumero , oDlg )
local lConsulta

local cFileSrv := "\bossware\NFeSefaz\Filial\"+cFilAnt+"\"+cNumero+"-nfe.xml"

IF Len( AllTrim(cNumero) ) < 44 
   MsgAlert("Chave informada menor 44 posições","BOSSWARE") 
   Return
EndIF

IF !File( cFileSrv )

	MsgRun("Conectando a SEFAZ...","BOSSWARE",{|| lConsulta := lWSNFe(cNumero) }) 
	
	IF !lConsulta
		
		// 	msgTimer( NIL, "Protocolo :" + __vNFePro + " . Gerando Capctha...", 3000, "BOSSWARE" ) 
		 MsgRun("Aguarde..." ,"BOSSWARE",{||  doDelay(2000) })
	
	     MsgRun("Gerando CAPTCHA para Download NFe...","BOSSWARE",{|| getNFe( cNumero , oDlg ) })
	
		//MsAguarde( { || lConsulta := getNFe( cNumero ) },"Aguarde...","Download NFe|SeFaz ... ")      
		//getNFe( cNumero , oDlg )                   	
		
	EndIF
Else
		 MsgRun("Aguarde..." ,"BOSSWARE",{||  doDelay(2000) })
		 MsgRun("Download NFe...","BOSSWARE",{|| getNFe( cNumero , oDlg ) })		 	
EndIF
Return

Static Function doSave( oDlg )      
local aStruct := ZP1->(DBSTRUCT()) 
local nX := 0          
local __lock :=  ( __OPC == 3 )
local bEnd    := {||oDlg:End() }
//  AEVAL( aStruct, {|aField| QOUT(aField[DBS_NAME])} )
/*
ZP1_FILIAL ZP1_CODIGO ZP1_AGENCI ZP1_CONTA       ZP1_CHEQUE           ZP1_CLIENT ZP1_CLINOM                                                   ZP1_NF               ZP1_SERIE ZP1_PREFIX ZP1_PARCEL ZP1_DATLAN ZP1_DATVEN ZP1_UNAME                      ZP1_VALOR              D_E_L_E_T_ R_E_C_N_O_
---------- ---------- ---------- --------------- -------------------- ---------- ------------------------------------------------------------ -------------------- --------- ---------- ---------- ---------- ---------- ------------------------------ ---------------------- ---------- -----------
aStruct[1][1] - campo
aStruct[1][2] - tipo
aStruct[1][3] - tamanho
*/
IF __OPC == 3 
	IF 	!lContinue() //lExist() 
		vVlrCH:= 0.00
		cInfo:= Space(600)
	    oDlg:Refresh()
		oInfo:Refresh()
		SysRefresh()
		oDlg:End()
		Return
	EndIF
EndIF	

 
   
    RecLock(cAlias , __lock )
 	     ZP1->ZP1_FILIAL := xFilial("ZP1")
		 ZP1->ZP1_CODIGO := GETSX8NUM('ZP1','ZP1_CODIGO')
		 ZP1->ZP1_AGENCI := vAG
		 ZP1->ZP1_CONTA  := vCC     
		 ZP1->ZP1_CHEQUE := vCH          
		 ZP1->ZP1_CLIENT := vCodCli
		 ZP1->ZP1_CLINOM := vCli                                                  
		 ZP1->ZP1_NF     := vNF          
		 ZP1->ZP1_SERIE  := vSerie
		 ZP1->ZP1_PREFIX := vPrefix
		 ZP1->ZP1_PARCEL := vParc
		 ZP1->ZP1_DATLAN := DDATABASE
		 ZP1->ZP1_DATVEN := vDtVenc
		 ZP1->ZP1_UNAME  := CUSERNAME                    
		 ZP1->ZP1_VALOR  := vValor  
       
	(cAlias)->(MsUnlock())
    MsUnLock()
         
	cInfo:= Space(600)
    oDlg:Refresh()
	oInfo:Refresh()
	SysRefresh()
	oDlg:End()

    //E1_FILIAL+E1_PREFIXO+E1_NUM+E1_PARCELA+E1_TIPO                                                                                                                  
    /*
    DBSelectArea("SE1")
    IF SE1->(DBSeek(xFilial("SE1")+vPrefix+vNF+vParc) )
	    RecLock("SE1", .F. )
	    
		    SE1->E1_CHNUM := vCH
			    
		SE1->(MsUnlock())
	    MsUnLock()
    EndIF
    */

Return


                         

Static Function bChange( )
    
	IF __PROCESS
		oNumero:lReadOnly := .T.
		oNumero:Refresh()	 
	EndIF

Return


Static Function bWhen( )
    
	IF __PROCESS
		oNumero:lReadOnly := .T.
		oNumero:Refresh()	 
	EndIF

Return

Static Function bValid()
local nX := 0
local nSize := Len(ALLTRIM(cNumero))
local lNext := .F.
 

For nX := 1 To nSize
     IF ( SubStr( ALLTRIM(cNumero),nX,1) $ "?"  )
        lNext := .T.       
     EndIF
Next nX

IF lNext                    

 
   cNumero := Space(90)     
 //  oNumero:lReadOnly := .F.
   oNumero:Refresh()
   oNumero:SetFocus()   
Else  
//	__PROCESS := .T.
  //	oNumero:lReadOnly := .T.
	oNumero:Refresh()
EndIF    

Return




Static Function bVALCMC7()
local nX := 0
local nSize := Len(ALLTRIM(cNumero))
local lNext := .F.
local __MAGNETIC :=  StrTran( ALLTRIM(cNumero), "<","")

__MAGNETIC :=  StrTran( ALLTRIM(__MAGNETIC), ">","")
__MAGNETIC :=  StrTran( ALLTRIM(__MAGNETIC), ":","")               

__MAGNETIC := ALLTRIM( __MAGNETIC )

For nX := 1 To nSize
     IF ( SubStr( ALLTRIM(cNumero),nX,1) $ "?"  )
        lNext := .T.       
     EndIF
Next nX

IF lNext                    

 
   cNumero := Space(90)     
 //  oNumero:lReadOnly := .F.
   oNumero:Refresh()
   oNumero:SetFocus()   
Else  
//	__PROCESS := .T.
  //	oNumero:lReadOnly := .T.
	oNumero:Refresh()
EndIF    

IF  !(__CMC7(__MAGNETIC) )
	__PROCESS := .F.
	msgTimer( __OFF,"Número do Cheque Não Confere. Tente novamente !",3000,"BOSSWARE") 
Else
	__PROCESS:= .T.
EndIF              

oDlg:Refresh()
Return .T.

  

Static Function lExist()
local cQuery := ""      
local __cNameAlias := GETNEXTALIAS()
local lExist := .F.

cQuery := " SELECT * FROM "  + RETSQLNAME("ZP1") 
cQuery += " WHERE ZP1_NF = '" + vNF + "' AND ZP1_PREFIX = '" + vPrefix + "'   AND ZP1_CLIENT = '" + vCodCli+ "' "
cQuery += " AND ZP1_FILIAL ='" + xFilial("ZP1") + "' AND ZP1_PARCEL ='" + vParc + "'        AND D_E_L_E_T_ = ' ' " 
      
 
		 
dbUseArea(.T.,__CONNECT,TcGenQry(,,cQuery),__cNameAlias,.T.,.T.)

IF (__cNameAlias)->(!Eof())
	lExist  := .T.
EndIF
      
 (__cNameAlias)->(DBCloseArea())
 
Return lExist


Static Function lContinue()
local cQuery := ""      
local __cNameAlias := GETNEXTALIAS()
local lContinue := .T.
local __nTOTAL := 0
 

cQuery := " SELECT SUM(ZP1_VALOR) AS VALOR FROM "  + RETSQLNAME("ZP1") 
cQuery += " WHERE ZP1_NF = '" + vNF + "' AND ZP1_PREFIX = '" + SE1->E1_PREFIXO + "'   AND ZP1_CLIENT = '" + SE1->E1_CLIENTE+ "' "
cQuery += " AND ZP1_FILIAL ='" + xFilial("ZP1") + "' AND ZP1_PARCEL ='" + SE1->E1_PARCELA + "'        AND D_E_L_E_T_ = ' ' " 
 
 
		 
dbUseArea(.T.,__CONNECT,TcGenQry(,,cQuery),__cNameAlias,.T.,.T.)

__nTOTAL  := (__cNameAlias)->VALOR + vVlrCH
//IF (__cNameAlias)->(!Eof())
	   IF __nTOTAL > SE1->E1_VALOR   
	   		lContinue := .F.
			msgTimer( __OFF," VALOR MAIOR QUE A PARCELA. NÃO CONFERE !!! ",4000,"BOSSWARE") 		   
		EndIF	
//EndIF
      
 (__cNameAlias)->(DBCloseArea())
 
Return lContinue


/*

999 9999 9999 9999 9999 9999 9999 9999 9999 9999 9999 , compondo 44 posições  como segue:


Código da UF  ( 02 )

Ano e Mês de emissão da NF-e  ( 04)

CNPJ do Emitente  ( 14)

Modelo  ( 02)

Série  ( 03)

Número da NF-e  ( 09)

forma de emissão da NF-e  ( 01)

Código Numérico  ( 08)

DV -digito verificador ( 01)
*/
Static Function lWSNFe(cChaveNFe,cIdEnt,lWeb)

Local cURL := PadR(GetNewPar("MV_SPEDURL","http://"),250)
Local cMensagem:= ""
Local oWS      
Local cIdEnt := GetIdEnt()
Local lErro := .F.


If ValType(lWeb) == 'U'
	lWeb := .F.
EndIf

oWs:= WsNFeSBra():New()
oWs:cUserToken := "TOTVS"
oWs:cID_ENT := cIdEnt
ows:cCHVNFE	:= cChaveNFe
oWs:_URL := AllTrim(cURL)+"/NFeSBRA.apw"

If oWs:ConsultaChaveNFE()
	cMensagem := ""
	If !Empty(oWs:oWSCONSULTACHAVENFERESULT:cVERSAO)
		cMensagem += "Versão da Mensagem"+": "+oWs:oWSCONSULTACHAVENFERESULT:cVERSAO+CRLF
	EndIf

 

	
	cMensagem += "Ambiente"+": "+IIf(oWs:oWSCONSULTACHAVENFERESULT:nAMBIENTE==1,"Produção","Homologação")+CRLF //"Produção"###"Homologação"
	 
	__vNFeAmb := IIF(oWs:oWSCONSULTACHAVENFERESULT:nAMBIENTE==1,"Produção","Homologação")
	
	cMensagem += "Cod.Ret.NFe"+": "+oWs:oWSCONSULTACHAVENFERESULT:cCODRETNFE+CRLF
	
	cMensagem += "Msg.Ret.NFe"+": "+oWs:oWSCONSULTACHAVENFERESULT:cMSGRETNFE+CRLF

IF !Empty(oWs:oWSCONSULTACHAVENFERESULT:cPROTOCOLO)
	cMensagem += "Protocolo"+": "+oWs:oWSCONSULTACHAVENFERESULT:cPROTOCOLO+CRLF
	__vNFePro := oWs:oWSCONSULTACHAVENFERESULT:cPROTOCOLO 
EndIf

//QUANDO NAO ESTIVER OK NAO IMPORTA, CODIGO DIFERENTE DE 100
If oWs:oWSCONSULTACHAVENFERESULT:cCODRETNFE # "100"
	lErro := .T.
EndIf
 
/*
IF !lWeb
	Aviso("Consulta NF",cMensagem,{"Ok"},3)
Else
	Return({lErro,cMensagem})
EndIf
*/

Else
	MsgAlert( IIf(Empty(GetWscError(3)),GetWscError(1),GetWscError(3)),"BOSSWARE" )
EndIF            

Return(lErro)


Static Function GetIdEnt()

Local aArea  := GetArea()
Local cIdEnt := ""
Local cURL   := PadR(GetNewPar("MV_SPEDURL","http://"),250)
Local oWs
Local lUsaGesEmp := IIF(FindFunction("FWFilialName") .And. FindFunction("FWSizeFilial") .And. FWSizeFilial() > 2,.T.,.F.)
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Obtem o codigo da entidade                                              ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
oWS := WsSPEDAdm():New()
oWS:cUSERTOKEN := "TOTVS"
	
oWS:oWSEMPRESA:cCNPJ       := IIF(SM0->M0_TPINSC==2 .Or. Empty(SM0->M0_TPINSC),SM0->M0_CGC,"")	
oWS:oWSEMPRESA:cCPF        := IIF(SM0->M0_TPINSC==3,SM0->M0_CGC,"")
oWS:oWSEMPRESA:cIE         := SM0->M0_INSC
oWS:oWSEMPRESA:cIM         := SM0->M0_INSCM		
oWS:oWSEMPRESA:cNOME       := SM0->M0_NOMECOM
oWS:oWSEMPRESA:cFANTASIA   := IIF(lUsaGesEmp,FWFilialName(),Alltrim(SM0->M0_NOME))
oWS:oWSEMPRESA:cENDERECO   := FisGetEnd(SM0->M0_ENDENT)[1]
oWS:oWSEMPRESA:cNUM        := FisGetEnd(SM0->M0_ENDENT)[3]
oWS:oWSEMPRESA:cCOMPL      := FisGetEnd(SM0->M0_ENDENT)[4]
oWS:oWSEMPRESA:cUF         := SM0->M0_ESTENT
oWS:oWSEMPRESA:cCEP        := SM0->M0_CEPENT
oWS:oWSEMPRESA:cCOD_MUN    := SM0->M0_CODMUN
oWS:oWSEMPRESA:cCOD_PAIS   := "1058"
oWS:oWSEMPRESA:cBAIRRO     := SM0->M0_BAIRENT
oWS:oWSEMPRESA:cMUN        := SM0->M0_CIDENT
oWS:oWSEMPRESA:cCEP_CP     := Nil
oWS:oWSEMPRESA:cCP         := Nil
oWS:oWSEMPRESA:cDDD        := Str(FisGetTel(SM0->M0_TEL)[2],3)
oWS:oWSEMPRESA:cFONE       := AllTrim(Str(FisGetTel(SM0->M0_TEL)[3],15))
oWS:oWSEMPRESA:cFAX        := AllTrim(Str(FisGetTel(SM0->M0_FAX)[3],15))
oWS:oWSEMPRESA:cEMAIL      := UsrRetMail(RetCodUsr())
oWS:oWSEMPRESA:cNIRE       := SM0->M0_NIRE
oWS:oWSEMPRESA:dDTRE       := SM0->M0_DTRE
oWS:oWSEMPRESA:cNIT        := IIF(SM0->M0_TPINSC==1,SM0->M0_CGC,"")
oWS:oWSEMPRESA:cINDSITESP  := ""
oWS:oWSEMPRESA:cID_MATRIZ  := ""
oWS:oWSOUTRASINSCRICOES:oWSInscricao := SPEDADM_ARRAYOFSPED_GENERICSTRUCT():New()
oWS:_URL := AllTrim(cURL)+"/SPEDADM.apw"
If oWs:ADMEMPRESAS()
	cIdEnt  := oWs:cADMEMPRESASRESULT
Else
	Aviso("SPED",IIf(Empty(GetWscError(3)),GetWscError(1),GetWscError(3)),{"OK"},3)
EndIf

RestArea(aArea)
Return(cIdEnt)





  

Static Function getNFe(  vKey , oDlg )
local __HDL := NIL          
local vIniRmt := GetRemoteIniName()
local cIniName:= GetRemoteIniName()
local lUnix:= IsSrvUnix()
local nPos:= Rat( IIF(lUnix,"/","\"),cIniName )
local cPathRmt                       

local cAmb:= GetEnvServer() 
local cStartPath :=GetSrvProfString( 'StartPath', '' )   
local cRootPath := GetSrvProfString( 'RootPath', '' )  
local cVersao := GetVersao(.T.)  
local cUser := __cUserId + ' ' +  cUserName  
local cComp := GetComputerName()  
local vPath := cRootPath+"\bossware\NFeSefaz\Filial\" 
local cFile := "" // cRootPath+"\bossware\NFeSefaz\"+vKey+".xml"
local cFLocal := "" 
local __HARDLOCK := "2100008290"
local lCompacta := .T.
local lSucess
local cFileSrv := ""
local cParam :=  ""  

IF nPos!=0
    cPathRmt:= Substr( cIniName,1,nPos-1 )
Else
    cPathRmt:=""
EndIF
      
cFLocal :=  cPathRmt
/*
HINSTANCE ShellExecute(
  _In_opt_  HWND hwnd,
  _In_opt_  LPCTSTR lpOperation,
  _In_      LPCTSTR lpFile,
  _In_opt_  LPCTSTR lpParameters,
  _In_opt_  LPCTSTR lpDirectory,
  _In_      INT nShowCmd
);
         

Parameters

hwnd [in, optional]
Type: HWND
A handle to the parent window used for displaying a UI or error messages. This value can be NULL if the operation is not associated with a window.
lpOperation [in, optional]
Type: LPCTSTR
A pointer to a null-terminated string, referred to in this case as a verb, that specifies the action to be performed. The set of available verbs depends on the particular file or folder. Generally, the actions available from an object's shortcut menu are available verbs. The following verbs are commonly used:
edit
Launches an editor and opens the document for editing. If lpFile is not a document file, the function will fail.
explore
Explores a folder specified by lpFile.
find
Initiates a search beginning in the directory specified by lpDirectory.
open
Opens the item specified by the lpFile parameter. The item can be a file or folder.
print
Prints the file specified by lpFile. If lpFile is not a document file, the function fails.
NULL
The default verb is used, if available. If not, the "open" verb is used. If neither verb is available, the system uses the first verb listed in the registry.
lpFile [in]
Type: LPCTSTR
A pointer to a null-terminated string that specifies the file or object on which to execute the specified verb. To specify a Shell namespace object, pass the fully qualified parse name. Note that not all verbs are supported on all objects. For example, not all document types support the "print" verb. If a relative path is used for the lpDirectory parameter do not use a relative path for lpFile.
lpParameters [in, optional]
Type: LPCTSTR
If lpFile specifies an executable file, this parameter is a pointer to a null-terminated string that specifies the parameters to be passed to the application. The format of this string is determined by the verb that is to be invoked. If lpFile specifies a document file, lpParameters should be NULL.
lpDirectory [in, optional]
Type: LPCTSTR
A pointer to a null-terminated string that specifies the default (working) directory for the action. If this value is NULL, the current working directory is used. If a relative path is provided at lpFile, do not use a relative path for lpDirectory.
nShowCmd [in]
Type: INT
The flags that specify how an application is to be displayed when it is opened. If lpFile specifies a document file, the flag is simply passed to the associated application. It is up to the application to decide how to handle it. These values are defined in Winuser.h.
SW_HIDE (0)
Hides the window and activates another window.
SW_MAXIMIZE (3)
Maximizes the specified window.
SW_MINIMIZE (6)
Minimizes the specified window and activates the next top-level window in the z-order.
SW_RESTORE (9)
Activates and displays the window. If the window is minimized or maximized, Windows restores it to its original size and position. An application should specify this flag when restoring a minimized window.
SW_SHOW (5)
Activates the window and displays it in its current size and position.
SW_SHOWDEFAULT (10)
Sets the show state based on the SW_ flag specified in the STARTUPINFO structure passed to the CreateProcess function by the program that started the application. An application should call ShowWindow with this flag to set the initial show state of its main window.
SW_SHOWMAXIMIZED (3)
Activates the window and displays it as a maximized window.
SW_SHOWMINIMIZED (2)
Activates the window and displays it as a minimized window.
SW_SHOWMINNOACTIVE (7)
Displays the window as a minimized window. The active window remains active.
SW_SHOWNA (8)
Displays the window in its current state. The active window remains active.
SW_SHOWNOACTIVATE (4)
Displays a window in its most recent size and position. The active window remains active.
SW_SHOWNORMAL (1)
Activates and displays a window. If the window is minimized or maximized, Windows restores it to its original size and position. An application should specify this flag when displaying the window for the first time.
Return value

Type: HINSTANCE
If the function succeeds, it returns a value greater than 32. If the function fails, it returns an error value that indicates the cause of the failure. The return value is cast as an HINSTANCE for backward compatibility with 16-bit Windows applications. It is not a true HINSTANCE, however. It can be cast only to an int and compared to either 32 or the following error codes below.
Return code	Description
0
The operating system is out of memory or resources.
ERROR_FILE_NOT_FOUND
The specified file was not found.
ERROR_PATH_NOT_FOUND
The specified path was not found.
ERROR_BAD_FORMAT
The .exe file is invalid (non-Win32 .exe or error in .exe image).
SE_ERR_ACCESSDENIED
The operating system denied access to the specified file.
SE_ERR_ASSOCINCOMPLETE
The file name association is incomplete or invalid.
SE_ERR_DDEBUSY
The DDE transaction could not be completed because other DDE transactions were being processed.
SE_ERR_DDEFAIL
The DDE transaction failed.
SE_ERR_DDETIMEOUT
The DDE transaction could not be completed because the request timed out.
SE_ERR_DLLNOTFOUND
The specified DLL was not found.
SE_ERR_FNF
The specified file was not found.
SE_ERR_NOASSOC
There is no application associated with the given file name extension. This error will also be returned if you attempt to print a file that is not printable.
SE_ERR_OOM
There was not enough memory to complete the operation.
SE_ERR_PNF
The specified path was not found.
SE_ERR_SHARE
A sharing violation occurred.
FT_FUse(cPathWrap+cFileDes) 		
	FT_FGoTop()
	While !FT_FEof()
		cSqlRet	+= FT_FReadLn() + chr(10)
		FT_FSkip()
	EndDo
	cSqlRet	+= chr(10)
    FT_Fuse()
*/
//vUF ,vKey, vHLock, vPath
                                             
IF isProgress( vKey , .F. )
   MsgAlert(" Processo NFe|SeFaz por outro usuário","BOSSWARE")
   Return .T.
EndIF
                        

IF !File(cPathRmt+"\BOSSWARENFE.exe")
   MsgAlert("BOSSWARE.EXE não encontrado no SmartClient.","BOSSWARE")
   Return .T.
EndIF


cFileSrv := "\bossware\NFeSefaz\Filial\"+cFilAnt+"\"+vKey+"-nfe.xml"

IF File( cFileSrv )
     
	IF MsgYesNo("Deseja realizar a importação?","BOSSWARE")
	
		oDlg:End()
        
	    MsgRun("Aguarde, processando Nota Fiscal...","BOSSWARE",{|| XMLNfe( cFileSrv ) })
        

	EndIF

Else

// taskkill /IM "process_name" /T /F                                    
// 26150724073694000155550000008544151008544153
cParam :=  cValToChar(__HARDLOCK) + SPACE(5) + vKey + SPACE(5) + '"' + cFLocal + ' " '

WaitRun ( cPathRmt+"\BOSSWARENFE.exe  " + cParam, 1 )
 
//__HDL := ShellExecute( "open" ,cPathRmt+"\BWNFe.exe",cParam,Left(cPathRmt,3), 1)
//taskkill /IM notepad.exe
cFile := cPathRmt+"\"+vKey+"-nfe.xml" // Arquivo XML Download...
                                                                          

IF !File( cFile )
   MsgAlert("Erro de acesso. Contacte Administrador !", "BOSSWARE")
   Return .T.
EndIF	         
                                      
IF !File( "\bossware\NFeSefaz\Filial\"+cFilAnt )     // Pasta Existe ?
	IF MakeDir( "\bossware\NFeSefaz\Filial\"+cFilAnt ) != 0
	   MsgAlert("Contacte Administrador de sistemas! " + cValToChar( FError() ),"BOSSWARE")
	   Return
	EndIF   
EndIF        
cFileSrv := "\bossware\NFeSefaz\Filial\"+cFilAnt+"\"+vKey+"-nfe.xml"
lSucess := CpyT2S(cFile,"\bossware\NFeSefaz\Filial\"+cFilAnt+"\",lCompacta)

// Trata retorno da cópia
IF lSucess
	FErase( cFile )
  	//msgTimer( NIL, "Copiando arquivo para o servidor..." , 2000, "BOSSWARE" ) 
    MsgRun("Aguarde, validando XML do Fornecedor...","BOSSWARE",{|| doDelay(1000) })
  	
Else
    MsgAlert("Erro ao copiar arquivo! entre contato com o administrador TI", "BOSSWARE")
EndIF

IF lSucess
	
 //	isProgress( vKey , .T. ) // desabilitar controle Lock...
	 	
	IF MsgYesNo("Download NFeSefaz executado com sucesso. Deseja realizar a importação?","BOSSWARE")
	
		oDlg:End()
        
	    MsgRun("Aguarde, processando Nota Fiscal...","BOSSWARE",{|| XMLNfe( cFileSrv ) })
 
	EndIF

 EndIF
 
EndIF

Return .T.



Static Function GetHardLock()

Static __HardLock 

DEFAULT __HardLock:= LS_GetID()

Return(__HardLock)


Static Function isProgress( __vLck , lBlock )
local __lLock := .F.
local nHdl

IF File("\SEMAFORO\" + "Lock_" + __vLck + ".LCK" )
 __vLck := .T.
	 Else
	nHdl := MSFCREATE( "\SEMAFORO\" + "Lock_" + __vLck + ".LCK" )	
	FClose(nHdl)  
EndIF

IF lBlock
	FErase( "\SEMAFORO\" + "Lock_" + __vLck + ".LCK" )
	__lLock := .F.
EndIF

Return __lLock



Static Function XMLNfe(cFile)
local oErr := ErrorBlock( { |e| __TO_ERR(E) } )
Local cError      	:= ""
Local cWarning      := ""
Local oXml          := NIL          
Local lContinua		:= .T.
local vA2_COD := ""   
local vA2_NREDUZ := ""
local vA2_END := ""
local vA2_MUN := ""
local vA2_BAIRRO := ""
local vA2_EST := ""
local vA2_COD_MUN := ""
local vA2_CEP := ""
local vA2_TEL := ""
local vA2_INSCR := ""
local vA2_INSCRM := ""
local vA2_NOME := ""
local vA2_CNAE := ""
local vF1_SERIE := ""
local vF1_DOC := ""
Local cDtEmissao
Local nNumItens
Local cCodProd
Local cQuant
Local nVlUnit
Local nVlTotal                
Local cFornece	
Local cLoja     	
Local cNumNF		    
Local cCondPgto
Local cAdicao
Local cSeqAdic
Local cDocImp
Local cDtImp
Local cLocDesem
Local cUFDesem
Local cDtDesem

Local lMsHelpAuto   := .T.     
Local nBCICM

local cQuery := ""      
local __cNameAlias  
local __nCont := 0
local vA2_CGC := ""
local vA2_COND  
local vZNF_TES := ""
local vZNF_CFOP := ""
local cDesProd := ""        
local oDet      
local aImp := {}
oXml := XmlParserFile(  cFile, "_", @cError, @cWarning )

IF ValType(oXml) != "O"
     MsgAlert(cFile+" - "+cError, "BOSSWARE" )
     Return NIL
EndIF       
//XMLNfe
// 26140809722463000131550060002368021050515970  24 ITENS
// 35140845070190000585550010000916761561220920  1 ITEM
// 41140979327649000171550020003109261988348695  DANFE
IF ValType(oXml:_NFEPROC:_NFE:_INFNFE:_DET) = "O"
     XmlNode2Arr(oXml:_NFEPROC:_NFE:_INFNFE:_DET, "_DET")
EndIF 



BEGIN SEQUENCE
                
vA2_CGC     := oXml:_NFE:_INFNFE:_EMIT:_CNPJ:TEXT  // oXml:_NFEPROC:_NFE:_INFNFE:_EMIT:_CNPJ:TEXT


vA2_NREDUZ :=  oXml:_NFEPROC:_NFE:_INFNFE:_EMIT:_XNOME:TEXT
vA2_END := oXml:_NFE:_INFNFE:_EMIT:_ENDEREMIT:_XLGR:TEXT + ", " + oXml:_NFE:_INFNFE:_EMIT:_ENDEREMIT:_NRO:TEXT 
vA2_MUN :=  oXml:_NFE:_INFNFE:_EMIT:_ENDEREMIT:_XMUN:TEXT
vA2_BAIRRO :=  oXml:_NFE:_INFNFE:_EMIT:_ENDEREMIT:_XBAIRRO:TEXT
vA2_EST :=  oXml:_NFE:_INFNFE:_EMIT:_ENDEREMIT:_UF:TEXT
vA2_COD_MUN :=  oXml:_NFE:_INFNFE:_EMIT:_ENDEREMIT:_CMUN:TEXT
vA2_CEP :=  oXml:_NFE:_INFNFE:_EMIT:_ENDEREMIT:_CEP:TEXT
vA2_TEL :=  oXml:_NFE:_INFNFE:_EMIT:_ENDEREMIT:_FONE:TEXT
vA2_INSCR := oXml:_NFE:_INFNFE:_EMIT:_IE:TEXT
vA2_INSCRM := oXml:_NFE:_INFNFE:_EMIT:_IM:TEXT
vA2_NOME := oXml:_NFE:_INFNFE:_EMIT:_XNOME:TEXT
vA2_CNAE := oXml:_NFE:_INFNFE:_EMIT:_CNAE:TEXT


nNumItens  	:= LEN ( oXml:_NFE:_INFNFE:_DET )
vF1_DOC		:= StrZero(Val(oXml:_NFE:_INFNFE:_IDE:_NNF:TEXT),9)
vF1_SERIE	:= oXml:_NFE:_INFNFE:_IDE:_SERIE:TEXT
cDtEmissao 	:=  oXml:_NFE:_INFNFE:_IDE:_DHEMI:TEXT
cDtEmissao 	:= Substr(cDtEmissao,9,2)+"/"+Substr(cDtEmissao,6,2)+"/"+Substr(cDtEmissao,1,4)
cFornece	:= oXml:_NFE:_INFNFE:_EMIT:_XNOME:TEXT
         

//IF Empty(vF1_SERIE) .OR. AllTrim(vF1_SERIE)=="000" .OR.  AllTrim(vF1_SERIE)=="00" .OR. AllTrim(vF1_SERIE)=="0"
//  vF1_SERIE := "1"
//EndIF

  
DbSelectArea("SA2")
DbSetOrder(3)  

IF DbSeek(xFilial("SA2") + vA2_CGC )
	vA2_COD 	:= SA2->A2_COD
	cLoja		:= SA2->A2_LOJA
	vA2_COND	:= SA2->A2_COND
	vA2_IE      := SA2->A2_INSCR
/*	
	SA2->(RecLock("SA2", .F. ))
	                 
		SA2->A2_NREDUZ := IIF( vA2_NREDUZ != NIL, vA2_NREDUZ , SA2->A2_NREDUZ )
		SA2->A2_END := IIF( vA2_END != NIL , vA2_END,  SA2->A2_END )
		SA2->A2_MUN := IIF( vA2_MUN  != NIL, vA2_MUN, SA2->A2_MUN )
		SA2->A2_BAIRRO := IIF( vA2_BAIRRO != NIL, vA2_BAIRRO , SA2->A2_BAIRRO )
		SA2->A2_EST := IIF( vA2_EST != NIL, vA2_EST,  SA2->A2_EST )
		SA2->A2_COD_MUN := IIF( vA2_COD_MUN != NIL, vA2_COD_MUN, SA2->A2_COD_MUN )
		SA2->A2_CEP := IIF( vA2_CEP != NIL, vA2_CEP , SA2->A2_CEP )
		SA2->A2_TEL := IIF( vA2_TEL  != NIL, vA2_TEL , SA2->A2_TEL )
		SA2->A2_INSCR := IIF( vA2_INSCR != NIL, vA2_INSCR , SA2->A2_INSCR  )
		SA2->A2_INSCRM := IIF( vA2_INSCRM != NIL, vA2_INSCRM ,  SA2->A2_INSCRM )
		SA2->A2_NOME := IIF( vA2_NOME != NIL, vA2_NOME , SA2->A2_NOME )
		SA2->A2_CNAE := IIF( vA2_CNAE != NIL, vA2_CNAE , SA2->A2_CNAE  )
			
	SA2->(MsUnLock())
*/	
Else
	MsgAlert( "Fornecedor :" + cFornece + " não cadastrado na base de dados", "BOSSWARE" )
    Return Nil
EndIf                                                          

// 41140979327649000171550020003109261988348695
DbSelectArea("ZF1")
DbSetOrder(2) 
IF DbSeek(xFilial("ZF1") + PADR(vF1_DOC,15) + PADR(vF1_SERIE,3) + vA2_CGC + cLoja) // ZF1_FILIAL+ZF1_DOC+ZF1_SERIE+ZF1_CNPJ+ZF1_LOJA

	MsgInfo("Nota Fiscal :" + vF1_DOC + ". Já foi processada !","BOSSWARE")
	Return Nil

EndIF

 
      
	 DBSelectArea("ZNF")     
	 DBSetOrder(3)
	 DBSeek( xFilial("ZNF")+vA2_CGC )
	 vZNF_TES := ZNF->ZNF_TES
	 vZNF_CFOP := ZNF->ZNF_CFOP
     
     
     IF nNumItens = 0 .OR. nNumItens = NIL .OR. Empty(nNumItens)
     
     	nQuant		:= oXml:_NFE:_INFNFE:_DET:_PROD:_QCOM:TEXT 
	 	nVlUnit  	:= oXml:_NFE:_INFNFE:_DET:_PROD:_VUNTRIB:TEXT
	 	nVlTotal  	:= ROUND( VAL(nQuant) * VAL( nVlUnit) , 2 )
 	    cCodProd	:= oXml:_NFE:_INFNFE:_DET:_PROD:_CPROD:TEXT
 	    cDesProd	:= oXml:_NFE:_INFNFE:_DET:_PROD:_XPROD:TEXT

		oDet := oXml:_NFE:_INFNFE:_DET:_IMPOSTO  // DETALHE IMPOSTOS PRODUTO...
	
		aImp := getImposto( @oDet )
		nVICMIT := aImp[1][1]
		nPICMIT := aImp[1][2]
		nBCICM  := aImp[1][3]
		


        ZD1->(RecLock("ZD1", .T. ))
			ZD1->ZD1_FILIAL := xFilial("ZD1")
			ZD1->ZD1_DOC := vF1_DOC
			ZD1->ZD1_SERIE := vF1_SERIE
			ZD1->ZD1_CNPJ  :=  vA2_CGC
			ZD1->ZD1_LOJA := cLoja
			ZD1->ZD1_FORMUL := 'N'
			ZD1->ZD1_EMISSA := CTOD(cDtEmissao)
			ZD1->ZD1_COD  := cCodProd
			ZD1->ZD1_PRODES := toChars( cDesProd )
			ZD1->ZD1_TES :=  vZNF_TES
			ZD1->ZD1_QUANT := Val(nQuant)
			ZD1->ZD1_VUNIT := Val(nVlUnit)
			ZD1->ZD1_TOTAL := nVlTotal
			ZD1->ZD1_LOCAL := "01"
			ZD1->ZD1_BASEIC := ROUND(VAL(nBCICM),2)
			ZD1->ZD1_VALICM  := ROUND(VAL(nVICMIT),2)
			ZD1->ZD1_PICM := ROUND(VAL(nPICMIT),2)
			ZD1->ZD1_VALIPI := ROUND(VAL(nVIPIIT),2)
			ZD1->ZD1_IPI := ROUND(VAL(nPIPIIT),2)
			
        ZD1->(MsUnLock())			
     
     EndIF
     
	 For nx := 1 to nNumItens 

		nQuant		:= oXml:_NFE:_INFNFE:_DET[nx]:_PROD:_QCOM:TEXT 
	 	nVlUnit  	:= oXml:_NFE:_INFNFE:_DET[nx]:_PROD:_VUNTRIB:TEXT
	 	nVlTotal  	:= ROUND( VAL(nQuant) * VAL( nVlUnit) , 2 )
 	    cCodProd	:= oXml:_NFE:_INFNFE:_DET[nx]:_PROD:_CPROD:TEXT
 	    cDesProd	:= oXml:_NFE:_INFNFE:_DET[nx]:_PROD:_XPROD:TEXT

		oDet := oXml:_NFE:_INFNFE:_DET[nx]:_IMPOSTO  // DETALHE IMPOSTOS PRODUTO...
	
		aImp := getImposto( @oDet )
		nVICMIT := aImp[1][1]
		nPICMIT := aImp[1][2]
		nBCICM  := aImp[1][3]
		


        ZD1->(RecLock("ZD1", .T. ))
			ZD1->ZD1_FILIAL := xFilial("ZD1")
			ZD1->ZD1_DOC := vF1_DOC
			ZD1->ZD1_SERIE := vF1_SERIE
			ZD1->ZD1_CNPJ  :=  vA2_CGC
			ZD1->ZD1_LOJA := cLoja
			ZD1->ZD1_FORMUL := 'N'
			ZD1->ZD1_EMISSA := CTOD(cDtEmissao)
			ZD1->ZD1_COD  := cCodProd
			ZD1->ZD1_PRODES := toChars( cDesProd )
			ZD1->ZD1_TES :=  vZNF_TES
			ZD1->ZD1_QUANT := Val(nQuant)
			ZD1->ZD1_VUNIT := Val(nVlUnit)
			ZD1->ZD1_TOTAL := nVlTotal
			ZD1->ZD1_LOCAL := "01"
			ZD1->ZD1_BASEIC := ROUND(VAL(nBCICM),2)
			ZD1->ZD1_VALICM  := ROUND(VAL(nVICMIT),2)
			ZD1->ZD1_PICM := ROUND(VAL(nPICMIT),2)
			ZD1->ZD1_VALIPI := ROUND(VAL(nVIPIIT),2)
			ZD1->ZD1_IPI := ROUND(VAL(nPIPIIT),2)
			
        ZD1->(MsUnLock())			
				
	Next nx                                                      


 	ZF1->( RecLock("ZF1", .T. ) )
		ZF1->ZF1_FILIAL := xFilial("ZF1") 	
		ZF1->ZF1_TIPO := "N"
		ZF1->ZF1_FORMUL := "N"
		ZF1->ZF1_DOC := vF1_DOC
		ZF1->ZF1_SERIE := vF1_SERIE
		ZF1->ZF1_EMISSA := CTOD(cDtEmissao)
		ZF1->ZF1_FORNEC := vA2_COD
		ZF1->ZF1_FNOME := cFornece
		ZF1->ZF1_CNPJ := vA2_CGC
		ZF1->ZF1_IE := vA2_IE
		ZF1->ZF1_LOJA := cLoja
		ZF1->ZF1_COND := vA2_COND
		ZF1->ZF1_ESPECI := "SPED"     
		ZF1->ZF1_STATUS := " "
		
	ZF1->(MsUnLock())


	IF MsgYesNo("A Nota Fiscal : " + vF1_DOC + " foi importada com sucesso.Deseja visualizar itens dessa Nota ?","BOSSWARE" )
		U_GETNFEIT()	
	Else
		Return
 	EndIF	

RECOVER
    ErrorBlock(oErr)
END SEQUENCE
 	
Return Nil

Static Function __TO_ERR( E )
 	//MsgAlert( "ERRO -> " + CHR(10) + E:DESCRIPTION )
 	ConOut( "ERRO -> " + CHR(10) + E:DESCRIPTION )
Return

Static Function msgTimer(obj,msg,segundos,titulo) 
local cStyle := "Q3Frame{ background-color:qlineargradient(spread:pad, x1:0, y1:0, x2:0, y2:0, stop: 0 #2589c9, stop: 1 #0f79bc) }"
local oPanel
local oDlg
local bEnd    := {||oDlg:End() }

   
local oFont := TFont():New('Arial',,14,,.T.,,,,.F.,.F.)
DEFINE DIALOG oDlg FROM  80,102 TO 100,510 TITLE oemtoansi(titulo) STYLE nOR( WS_VISIBLE, WS_POPUP ) PIXEL FONT oFont 
oPanel := TPanelCss():New(00,00,"",oDlg,,.F.,.F.,,,190,74,.T.,.F.)
oPanel:SetCSS( cStyle )

oPanel:setCSS( cStyle )
DEFINE TIMER oTimer INTERVAL (segundos) ACTION ( Eval( bEnd )) OF oDlg

@ 10, 29 SAY oemtoansi(msg) SIZE 200, 10 PIXEL COLOR CLR_WHITE FONT oFont OF oPanel

                                                          
ACTIVATE MSDIALOG oDlg ON INIT ( oTimer:Activate()) CENTERED
			                               
Return




 

User Function BNFE001() 

private cCadastro:='Amarração NFe|Fornecedor x Produto'
private cAlsZNF:= "ZNF"
private cAlsZNI:= "ZNI"
private aRotina:= {}
private __NF := "" 
Private aCores      := {}
 
aAdd( aRotina, {"Pesquisar  " , "AxPesqui    " , 0, 1 })
aAdd( aRotina, {"Visualizar " , "u_LMod3Manut " , 0, 2 })
aAdd( aRotina, {"Incluir    " , "u_LMod3Manut " , 0, 3 })
aAdd( aRotina, {"Alterar    " , "u_LMod3Manut " , 0, 4 })
aAdd( aRotina, {"Excluir    " , "u_LMod3Manut " , 0, 5 })


IF Empty(Posicione("SX3",1,cAlsZNF,"X3_ARQUIVO"))
   Help("",1,"","NOX3X2IX","NÃO É POSSÍVEL EXECUTAR",1,0)
   RETURN
EndIF

IF Empty(Posicione("SX3",1,cAlsZNI,"X3_ARQUIVO"))
   Help("",1,"","NOX3X2IX","NÃO É POSSÍVEL EXECUTAR",1,0)
   RETURN
EndIF
 
 
DbSelectArea(cAlsZNF)
DbSetOrder(1)
DbGoTop()


 
 


mBrowse( 6, 1, 22, 75, cAlsZNF )

Return
 
 
 
User Function LMod3Manut(cAlias,nRecno,nOpc)
Local i:=0
Local cLinok := "allwaystrue"
Local cTudook := "u_LMD3TudOk"
Local nOpce := nopc

Local nOpcg := nopc
Local cFieldok := "allwaystrue"
Local lVirtual := .T.
Local nLinhas := 500
Local nFreeze := 0
Local lRet := .T.
 
Private aCols := {}
Private aHeader := {}
Private aCpoEnchoice := {}
Private aAltEnchoice := {}
Private aAlt := {}
 
 
 
Regtomemory(cAlsZNF,(nOpc==3))
Regtomemory(cAlsZNI,(nOpc==3))
 
 
 
CriaHeader()
 
CriaCols(nOpc)
 
 
lRet:=Modelo3(cCadastro,cAlsZNF,cAlsZNI,aCpoEnchoice,cLinok,cTudook,nOpce,;
	nOpcg,cFieldok,lVirtual,nLinhas,aAltenchoice,nFreeze)
 
 
IF lRet
 
 
IF nOpc == 3
 
	IF MsgYesNo("Confirma gravação dos dados ?",cCadastro)
 
	Processa({||Grvdados()},cCadastro,"Gravando os dados, aguarde...")
 
EndIF
 
 
//Se opção for alteração
 
 
ElseIF nOpc == 4
 
	IF MsgYesNo("Confirma alteração dos dados ?", cCadastro)
 
	Processa({||Altdados()},cCadastro,"Alterando os dados, aguarde...")
 
EndIF
 
//Se opção for exclusão
 
 
ElseIF nOpc == 5
 
	IF MsgYesNo("Confirma exclusão dos dados ?", cCadastro)
 
	Processa({||Excluidados()},cCadastro,"Excluindo os dados, aguarde...")
 
EndIF
 
EndIF
 
Else
 
	RollbackSx8()
 
EndIF
 
return
 


// Criando a função CriaHeader
 
Static Function CriaHeader()
 
aHeader:= {}
 
aCpoEnchoice := {}
 
aAltEnchoice :={}
 
dbselectarea("SX3")
 
dbsetorder(1)
 
dbseek(cAlsZNI)
 
While ! eof() .and. x3_arquivo == cAlsZNI
 
		IF x3uso(x3_usado) .and. cnivel >= x3_nivel
		 
		aAdd(aHeader,{trim(x3_titulo),;
		 		x3_campo,;
				x3_picture,;
				x3_tamanho,;
				x3_decimal,;
				x3_valid,;
				x3_usado,;
				x3_tipo,;
				x3_arquivo,;
				x3_context})
		 
		EndIF
		 
dbskip()
 
Enddo
 
DbSeek(cAlsZNF)
 
While ! eof() .and. x3_arquivo == cAlsZNF
 
	IF X3Uso(x3_usado) .and. cnivel >= x3_nivel
 
		aAdd(aCpoEnchoice,x3_campo)
 		aAdd(aAltEnchoice,x3_campo)
 
	EndIF
 
dbskip()
 
EndDo
 
return
 
 
 
Static function CriaCols(nOpc)
 
Local nQtdcpo := 0
Local i:= 0
Local nCols := 0

nQtdcpo := len(aHeader)
aCols:= {}
aAlt := {}
 
IF nOpc == 3
 
	aAdd(aCols,array(nQtdcpo+1))
 
	For i := 1 To nQtdcpo
	 
		aCols[1,i] := Criavar(aHeader[i,2])
	 
	Next i
	 
		aCols[1,nQtdcpo+1] := .F.

Else
 
DbSelectArea(cAlsZNI)
DbSetOrder(2)
DbSeek(xFilial("ZNI")+(cAlsZNF)->ZNF_CNPJ)
                                                                                  
While .Not. Eof()  

		IF AllTrim((cAlsZNI)->ZNI_CNPJ) == (cAlsZNF)->ZNF_CNPJ
			 
					aAdd(aCols,array(nQtdcpo+1))
			
					nCols++
				 
				For i:= 1 To nQtdcpo
				 
					IF aHeader[i,10] <> "V"
				 
					aCols[nCols,i] := Fieldget(Fieldpos(aHeader[i,2]))
				 
					Else
				 
					aCols[nCols,i] := Criavar(aHeader[i,2],.T.)
				 
					EndIF
				 
				Next i
				 
					aCols[nCols,nQtdcpo+1] := .F.
				 	aAdd(aAlt,Recno())
	
		EndIF
				 
					DbSelectArea(cAlsZNI)
		 
		DbSkip()
				 
	EndDo
	
EndIF
 

Return
 
 
Static Function GrvDados()
 
Local bcampo := {|nfield| field(nfield) }
Local i:= 0
Local y:= 0
Local nItem :=0     

DBSelectArea("ZNF")     

    
procregua(len(aCols)+fCount())
 
dbselectarea(cAlsZNF)
 
Reclock(cAlsZNF,.T.)

	 
For i:= 1 To fcount()
 
IncProc()


	IF "FILIAL" $ FieldName(i)
	 
		Fieldput(i,xfilial(cAlsZNF))
		
	Else
	
		Fieldput(i, M->&(EVAL(BCAMPO,i)))
	 
	EndIF
 
 
Next


 
Msunlock()
DbSelectArea(cAlsZNI)
 
DbSetOrder(1)
 
For i:=1 To len(aCols)
 
incproc()
 
IF .Not. aCols[i,len(aHeader)+1]
 
	Reclock(cAlsZNI,.T.)
 
For y:= 1 To len(aHeader)
 
	Fieldput(Fieldpos(trim(aHeader[y,2])),aCols[i,y])
 
Next
 
nItem++
 
 
(cAlsZNI)->ZNI_CNPJ := (cAlsZNF)->ZNF_CNPJ

 

 
Msunlock()
 
endif
 
Next
 
return
 
 
Static Function Altdados()
 
Local bcampo := { |nfield| field(nfield) }
Local i:= 0
Local y:= 0
Local nitem := 0
 
procregua(len(aCols)+fCount())
 
dbselectarea(cAlsZNF)
 
Reclock(cAlsZNF,.F.)

	 

 
For i:= 1 To fcount()
 
incproc()
 
IF "FILIAL" $ fieldname(i)
 
	Fieldput(i,xfilial(cAlsZNF))
 
Else

	Fieldput(i,M->&(EVAL(BCAMPO,i)))
 
EndIF
 
Next i
 
Msunlock()
 
dbselectarea(cAlsZNI)
 
dbsetorder(1)
 
nItem := len(aAlt)+1
 
For i:=1 To len(aCols)
 
	IF i<=len(aAlt)
		 
			dbgoto(aAlt[i])
			 
			Reclock(cAlsZNI,.F.)
			 
			IF aCols[i,len(aHeader)+1]
			 
				DbDelete()
			 
			Else
			 
				For y:= 1 To len(aHeader)
				 
					Fieldput(Fieldpos(trim(aHeader[y,2])),aCols[i,y])
				 
				Next y
			 
			EndIF
		 
			Msunlock()
	 
	Else
	 
		IF ! aCols[i,len(aHeader)+1]
	 
			Reclock(cAlsZNI,.T.)
	 
				For y:= 1 To len(aHeader)
	 
					Fieldput(Fieldpos(trim(aHeader[y,2])),aCols[i,y])
	 
				Next y
	 
			(cAlsZNI)->ZNI_CNPJ := (cAlsZNF)->ZNF_CNPJ
	 
		Msunlock()
	 
		nItem++
	 
	EndIF
	 
	EndIF
	 
	Next i
	 
Return
 
 
 
//Criando a função ExcluiDados
 
Static Function Excluidados()
 
procregua(len(aCols)+1)
 
DbSelectArea(cAlsZNI)
DbSetOrder(2)
 
DbSeek(xFilial(cAlsZNI)+(cAlsZNF)->ZNF_CNPJ )
 
Do while .not. eof() 

IF (cAlsZNI)->ZNI_CNPJ==(cAlsZNF)->ZNF_CNPJ
 
	incproc()
	 
	Reclock(cAlsZNI,.F.)
	 
		DbDelete()
	 
		Msunlock()
	
	EndIF 

DBSkip()
 
EndDo
 
dbselectarea(cAlsZNF)
 
dbsetorder(1)
 
incproc()
 
Reclock(cAlsZNF,.F.)
 
DbDelete()
 
Msunlock()
 
Return
 
 
User function LMD3TudOk()
Local lRet:= .T.
Local i:=0
Local nDel :=0
 
For i:=1 To len(aCols)
 
	IF aCols[i,len(aHeader)+1]
	 
		nDel++
	 
	EndIF
 
Next
 
IF nDel == len(aCols)
 
	Msginfo("Para excluir todos os itens, utilize a opção EXCLUIR",cCadastro)
 
	lRet := .F.
 
EndIF

 
Return(lRet)


 

 
 


Static Function DateDiff(dDataIni, dDataFim )
local nDias := CTod(Stod(dDataFim))-CTod(Stod(dDataIni))
Return nDias

 


Static Function Stod( dData )

local cData := ""
IF ValType(dData) == "D"
	cData := AllTrim( Dtos( dData ) )
Else
    cData := dData	
EndIF
	
cData := SubString( cData, 7,2 )+'/'+SubString( cData, 5,2 )+'/'+SubString( cData, 1,4 )

Return cData




User Function SEF04Cols()

Local nCntFor := 0

Aadd(aCols,Array(Len(aHeader)+1))
For nCntFor := 1 To Len(aHeader)
	aCols[Len(aCols),nCntFor] := CriaVar(aHeader[nCntFor,2])
Next
aCols[Len(aCols),Len(aHeader)+1] := .F.

Return

 
Static Function doDelay( nDelay )
	Sleep( nDelay )
Return .T.


User Function TONFENT

local __cNameAlias  
local cQuery := ""
local aCabec := {}
local cErro := ""
local __nCount := 0	 
local __cAlias 
local vZNF_TES := ""
local vZNF_CFOP := ""
local aProc := {}   
local aItens  := {}
local cCodProd := ""
local cProds := ""  
local nItem := 0      
local cUM := ""
local nPos := 0 
local cItPC := ""
local cProduto := ""
local __cKey   := ""
local __cPedCom := ""
local __cItem := ""

// BOSS001   -> Pedido de Compras



//doParamter()        

//Pergunte("CBNFE001")
lSelectPC := .F.
  
getPedCom(  ZF1->ZF1_FORNEC, ZF1->ZF1_LOJA  )

//IF lExistPC  
//    MsgAlert("Existe(m) pedido(s) de compra(s) para esse CNPJ. Seu usuário será gravado para auditoria !","BOSSWARE")
//EndIF
   

 
IF isExist ( ZF1->ZF1_DOC, ZF1->ZF1_FORNEC, ZF1->ZF1_LOJA )
    MsgAlert("Nota Fiscal: " + ALLTRIM( ZF1->ZF1_DOC ) + " foi processada anteriormente !", "BOSSWARE")
    Return
EndIF
                    
__cNameAlias := GETNEXTALIAS()

	aCabec := {{"F1_FILIAL"	    ,xFilial("SF1")							,NIL},;
	           {"F1_TIPO"	    ,'N'									,NIL},;
			   {"F1_FORMUL"	    ,"N"									,NIL},;
			   {"F1_DOC"		,ZF1->ZF1_DOC							,NIL},;
			   {"F1_SERIE"		,ZF1->ZF1_SERIE							,NIL},;
			   {"F1_EMISSAO"	,ZF1->ZF1_EMISSA						,NIL},;									
			   {"F1_FORNECE"	,ZF1->ZF1_FORNEC    					,NIL},;		
			   {"F1_LOJA"	    ,ZF1->ZF1_LOJA        					,NIL},;		       
			   {"F1_COND" 	    ,ZF1->ZF1_COND							,NIL},;		
			   {"F1_CHVNFE"     ,vCHAVE_NFE                             ,NIL},; 
			   {"F1_ESPECIE"	,"SPED"    								,NIL}}
		  //	   {"F1_ISPC"       ,"1"                                    ,NIL}} // PEDIDO DE COMPRAS
      


DbSelectArea("SF1")
DbSetOrder(1) 
IF DbSeek(xFilial("SF1") + ZF1->ZF1_DOC + ZF1->ZF1_SERIE + ZF1->ZF1_FORNEC + ZF1->ZF1_LOJA )

	MsgInfo("Nota Fiscal " + ZF1->ZF1_DOC + " já foi processada !","BOSSWARE")                      
	Return Nil

EndIF

DBSelectArea("ZNF")     
DBSetOrder(1)
DBSeek( xFilial("ZNF") + ZF1->ZF1_FORNEC )
vZNF_TES := ZNF->ZNF_TES
vZNF_CFOP := ZNF->ZNF_CFOP
	         
//IF Empty( vZNF_TES ) .OR. Empty( vZNF_CFOP )
//   MsgAlert("Fornecedor não está associado a TES/CFOP","BOSSWARE")
//   Return NIL
//EndIF        

cQuery := " SELECT * FROM "  + RETSQLNAME("ZD1") 
cQuery += " WHERE ZD1_CNPJ = '" + ZF1->ZF1_CNPJ  + "' AND ZD1_DOC = '" + ZF1->ZF1_DOC + "' AND ZD1_SERIE ='" + ZF1->ZF1_SERIE + "' "
cQuery += " AND ZD1_LOJA = '" + ZF1->ZF1_LOJA + "' AND D_E_L_E_T_ = ' ' " 
		 
dbUseArea(.T.,__CONNECT,TcGenQry(,,cQuery),__cNameAlias,.T.,.T.)


IF (__cNameAlias)->(!Eof())

                                     
     nItem := 0
	
	//nPOS :=  Ascan(__aCAR, {|aVal|aVal[1] = (__cNameAlias)->DAI_COD  })
	
//    IF !Empty ( MV_PAR01 )	.AND. !Empty ( MV_PAR02 )
//       getPedCom( SC7->C7_NUM, SC7->C7_FORNECE, SC7->C7_LOJA  )
//    EndIF
	  
 	

	While (__cNameAlias)->(!Eof())
	  
	                                
	     nItem += 1
	     
		__cAlias := GETNEXTALIAS()
		cQuery := " SELECT * FROM "  + RETSQLNAME("ZNI") 
		cQuery += " WHERE ZNI_CNPJ = '" + ZF1->ZF1_CNPJ + "' AND ZNI_PRDFOR = '" +  (__cNameAlias)->ZD1_COD + "' AND D_E_L_E_T_ = ' ' " 
		 
		dbUseArea(.T.,__CONNECT,TcGenQry(,,cQuery), __cAlias,.T.,.T.)

		IF (__cAlias)->(!Eof())
			cCodProd := (__cAlias)->ZNI_CODPRD
			cProds += (__cAlias)->ZNI_CODPRD + ","
			dbSelectArea("SB1")
			dbSetOrder(1)
			
			IF !MsSeek(xFilial("SB1")+cCodProd)
			    AADD( aProc,  "PRODUTO : " +  cCodProd+"/"+cProds+ " NÃO CADASTRADO NA BASE"    )
			Else
			   cUM := SB1->B1_UM    
			EndIF   
		Else

		    AADD( aProc,  "PRODUTO : " +  cCodProd+"/"+cProds + " SEM AMARRAÇÃO !"    )
			__nCount += 1
		EndIF
	  
	    cProduto := (__cNameAlias)->ZD1_COD+"/"+(__cNameAlias)->ZD1_PRODES 
	  
	    SB1->(DBCloseArea())
	    
		aLinha:={}             
 
		IF (__cAlias)->(!Eof())       
		                                      
				AADD(aLinha,{"D1_FILIAL"		,xFilial("SD1")		,NIL})		
				
				AADD(aLinha,{"D1_ITEM"	    , StrZero(nItem,4)      ,NIL})										
				                                      	
				AADD(aLinha,{"D1_COD"		,cCodProd	 		    ,NIL})
				AADD(aLinha,{"D1_UM"		,cUM 	 			    ,NIL})
				AADD(aLinha,{"D1_FORMUL"	,'N' 		,NIL})		
                
			IF !Empty( aList )
					nPos := Ascan( aList , {|  aVal| ALLTRIM( aVal[4] ) == ALLTRIM(cCodProd)  })
					IF nPos !=0
						IF aList[nPOS][1] // Se foi marcado o produto...
						    __cPedCom := aList[nPOS][2]
							__cItem := aList[nPOS][3]
						EndIF	                   
					//Else     
					//    IF Ascan( aList , {|  aVal|  aVal[1]  == .T.  }) != 0
					//    	MsgAlert("Produto : " + cProduto + " da NF: " + ZF1->ZF1_DOC + " SERIE/"+ ZF1->ZF1_SERIE + " não foi encontrado pedido de compras. A rotina será encerrada !","BOSSWARE")
					//	    Return
					//	 EndIF   
					EndIF	
				EndIF	

				AADD(aLinha,{"D1_QUANT"		,(__cNameAlias)->ZD1_QUANT			,NIL})
				AADD(aLinha,{"D1_VUNIT"		,(__cNameAlias)->ZD1_VUNIT			,NIL})
				AADD(aLinha,{"D1_TOTAL"		,(__cNameAlias)->ZD1_TOTAL			,NIL})


                IF !Empty ( __cPedCom )
					AADD(aLinha,{"D1_PEDIDO"	,__cPedCom      	         ,NIL})
					AADD(aLinha,{"D1_ITEMPC"	,__cItem 			         ,NIL})					
				EndIF

				AADD(aLinha,{"D1_FORNECE"	,ZF1->ZF1_FORNEC    				,NIL})							
				AADD(aLinha,{"D1_LOJA"	    ,ZF1->ZF1_LOJA    					,NIL})				
				AADD(aLinha,{"D1_LOCAL"		,"01"							    ,NIL})
				AADD(aLinha,{"D1_BASEICM"	,(__cNameAlias)->ZD1_BASEIC			,NIL})		
				AADD(aLinha,{"D1_VALICM"	,(__cNameAlias)->ZD1_VALICM			,NIL})
				AADD(aLinha,{"D1_PICM" 		,(__cNameAlias)->ZD1_PICM			,NIL})
				AADD(aLinha,{"D1_VALIPI" 	,(__cNameAlias)->ZD1_VALIPI			,NIL})
				AADD(aLinha,{"D1_IPI" 		,(__cNameAlias)->ZD1_IPI			,NIL})
				
				
				AADD(aItens,aLinha)                                   
	    EndIF
 		(__cAlias)->(DBCloseArea())
	       		
	(__cNameAlias)->(DBSKIP())
	End                       
	
	
EndIF	
 
(__cNameAlias)->(DBCloseArea())

cProds := SubStr( ALLTRIM( cProds ), 1, LEN(ALLTRIM( cProds )) -1 )

//Pergunte("BOSS001", .T. )
       

IF __nCount != 0 

    IF MsgYesNo ( "Foram encontrados erros na Nota Fiscal. Deseja visualizar ?","BOSSWARE")
		doFinish( @aProc )    
    EndIF
    
	Return NIL

EndIF
	  

//		 putPreNota(aCabec,aItens)
  
//		U_ESTP01TES() <-- teste

         
   MsgRun("Aguarde gerando Pré-Nota de Entrada...",,{|| MSExecAuto ( {|x,y,z| MATA140(x,y,z) }, aCabec, aItens, 3)})
 	
	IF lMsErroAuto
		MostraErro()                                              	
		RollBackSX8()

	Else                              
		ConfirmSX8()   
		
		IF ( MsgYesNo ("Nota Fiscal de Entrada foi processada com sucesso. Deseja visualizar pré-nota ? ", "BOSSWARE") )
		   
		   MATA140( )

/*				dbSelectArea("SF1")
				SET FILTER TO AllTrim(F1_DOC) = AllTrim(ZF1->ZF1_DOC) .AND. AllTrim(F1_SERIE) ==ZF1->ZF1_SERIE
				MATA140()
				dbSelectArea("SF1")
				SET FILTER TO */


		    
		EndIF
			
	EndIF

 
		DbSelectArea("ZF1")
		DbSetOrder(2) 
		
		IF DbSeek(xFilial("SF1") + ZF1->ZF1_DOC + ZF1->ZF1_SERIE + ZF1->ZF1_CNPJ + ZF1->ZF1_LOJA ) // ZF1_FILIAL+ZF1_DOC+ZF1_SERIE+ZF1_CNPJ+ZF1_LOJA
		
		 ZF1->(RecLock("ZF1", .F. ) )
				ZF1->ZF1_STATUS := IIF( !lMsErroAuto , "E", "X" )
		 ZF1->(MsUnLock())
	
		EndIF
 
Return



Static Function doFinish( aProc )
local cMask     := "Arquivos Texto (*.TXT) |*.txt|"
local cTexto := ''
local cFileLog  
local nX := 0


For nX := 1 To Len( aProc )
     cTexto += aProc[nX] + __ENTER__
Next nX

cFileLog := MemoWrite( CriaTrab( , .F. ) + '.log', cTexto )

Define Font oFont Name 'Mono AS' Size 5, 12


			Define MsDialog oDlg Title 'LOG PROCESSO' From 3, 0 to 340, 417 Pixel

			@ 5, 5 Get oMemo Var cTexto Memo Size 200, 145 Of oDlg Pixel
			oMemo:bRClicked := { || AllwaysTrue() }
			oMemo:oFont     := oFont

			Define SButton From 153, 175 Type  1 Action oDlg:End() Enable Of oDlg Pixel // Apaga
			Define SButton From 153, 145 Type 13 Action ( cFile := cGetFile( cMask, '' ), If( cFile == '', .T., ;
			MemoWrite( cFile, cTexto ) ) ) Enable Of oDlg Pixel // Salva e Apaga //'Salvar Como...'

			Activate MsDialog oDlg Center
Return


User Function LEGZF1()
local aLegenda := {}          
/*   
   aLegenda := { { "BR_LARANJA"  ,   "Atrasado"  },;
                 { "BR_AZUL"     ,   "Coletado"  },;
                 { "BR_VERMELHO" ,   "Em Rota"    },;
                 { "BR_VIOLETA"  ,   "Enviado"   },;
                 { "BR_VERDE"    ,   "Entregue"   },;
                 { "BR_PRETO"   ,   "Merc.Apreendida"   }}

*/                 

   aLegenda := { { "BR_VIOLETA"  ,   "NFe não processada" },;
                 { "BR_VERDE"    ,   "NFe Processada"     },;
                 { "BR_PRETO"   ,    "NFe com erro"       }}
                 
   BRWLEGENDA( "NFe|Sefaz", "NFe|Sefaz", aLegenda )
   
Return .t.

Static Function getImposto( oDet , oXml )
local oError := ErrorBlock({|e| CONOUT( +chr(10)+ e:Description)} )               
local nVICMIT, nPICMIT, nBCICM := 0
local aImp := {}


Begin Sequence

//nPIPIIT	:= oXml:_NFE:_INFNFE:_DET[nx]:_IMPOSTO:_IPI:_IPITRIB:_PIPI:TEXT
//nVIPIIT	:= oXml:_NFE:_INFNFE:_DET[nx]:_IMPOSTO:_IPI:_IPITRIB:_VIPI:TEXT						

nPIPIIT	:= oDet:_IPI:_IPITRIB:_PIPI:TEXT
nVIPIIT	:= oDet:_IPI:_IPITRIB:_VIPI:TEXT

IF VALTYPE( oDet:_ICMS:_ICMS00  ) == "O"
				nVICMIT := oDet:_ICMS:_ICMS00:_VICMS:TEXT
				nPICMIT	:= oDet:_ICMS:_ICMS00:_PICMS:TEXT				
				nBCICM	:= oDet:_ICMS:_ICMS00:_VBC:TEXT				
EndIF				

IF VALTYPE( oDet:_ICMS:_ICMS10  ) == "O"
				nVICMIT := oDet:_ICMS:_ICMS10:_VICMS:TEXT
				nPICMIT	:= oDet:_ICMS:_ICMS10:_PICMS:TEXT				
				nBCICM	:= oDet:_ICMS:_ICMS10:_VBC:TEXT								
EndIF

IF VALTYPE( oDet:_ICMS:_ICMS20  ) == "O"
				nVICMIT := oDet:_ICMS:_ICMS20:_VICMS:TEXT
				nPICMIT	:= oDet:_ICMS:_ICMS20:_PICMS:TEXT								
				nBCICM	:= oDet:_ICMS:_ICMS20:_VBC:TEXT								
EndIF

IF VALTYPE( oDet:_ICMS:_ICMS30  ) == "O"
				nVICMIT := oDet:_ICMS:_ICMS30:_VICMS:TEXT
				nPICMIT	:= oDet:_ICMS:_ICMS30:_PICMS:TEXT								
				nBCICM	:= oDet:_ICMS:_ICMS30:_VBC:TEXT								
EndIF               

IF VALTYPE( oDet:_ICMS:_ICMS40  ) == "O"
				nVICMIT := oDet:_ICMS:_ICMS40:_VICMS:TEXT
				nPICMIT	:= oDet:_ICMS:_ICMS40:_PICMS:TEXT								
				nBCICM	:= oDet:_ICMS:_ICMS40:_VBC:TEXT								
EndIF

IF VALTYPE( oDet:_ICMS:_ICMS50  ) == "O"
				nVICMIT := oDet:_ICMS:_ICMS50:_VICMS:TEXT
				nPICMIT	:= oDet:_ICMS:_ICMS50:_PICMS:TEXT								
				nBCICM	:= oDet:_ICMS:_ICMS50:_VBC:TEXT								
EndIF               

IF VALTYPE( oDet:_ICMS:_ICMS60  ) == "O"
				nVICMIT := oDet:_ICMS:_ICMS60:_VICMS:TEXT
				nPICMIT	:= oDet:_ICMS:_ICMS60:_PICMS:TEXT								
				nBCICM	:= oDet:_ICMS:_ICMS60:_VBC:TEXT								
EndIF

IF VALTYPE( oDet:_ICMS:_ICMS70  ) == "O"
				nVICMIT := oDet:_ICMS:_ICMS70:_VICMS:TEXT
				nPICMIT	:= oDet:_ICMS:_ICMS70:_PICMS:TEXT								
				nBCICM	:= oDet:_ICMS:_ICMS70:_VBC:TEXT								
EndIF 

IF VALTYPE( oDet:_ICMS:_ICMS90  ) == "O"
				nVICMIT := oDet:_ICMS:_ICMS90:_VICMS:TEXT
				nPICMIT	:= oDet:_ICMS:_ICMS90:_PICMS:TEXT								
				nBCICM	:= oDet:_ICMS:_ICMS90:_VBC:TEXT								
EndIF 


End Sequence
 
ErrorBlock(oError)

AADD( aImp, { nVICMIT,  nPICMIT, nBCICM } )

IF Empty( nPIPIIT ) .OR. nPIPIIT == NIL
	nPIPIIT := '0.00'
EndIF

IF Empty( nVIPIIT ) .OR. nVIPIIT == NIL
	nVIPIIT := '0.00'
EndIF

Return aImp                                   



User Function getNFeView()

 
Return


// MyGrid ( Classe para encapsular acesso ao componente TGrid )
//------------------------------------------------------------------------------           
CLASS TGRIDCss
	DATA oGrid	
	DATA oFrame
	DATA oButtonsFrame
	DATA oButtonHome
	DATA oButtonPgUp
	DATA oButtonUp
	DATA oButtonDown
	DATA oButtonPgDown
	DATA oButtonEnd
	DATA aData
	DATA nLenData
	DATA nRecNo
	DATA nCursorPos	 
	DATA nVisibleRows    
	          
	METHOD New(oDlg) CONSTRUCTOR
	METHOD onMove( o,nMvType,nCurPos,nOffSet,nVisRows )
	METHOD isBof()
	METHOD isEof()
	METHOD ShowData( nFirstRec, nCount ) 
	METHOD ClearRows() 
	METHOD DoUpdate()   
	METHOD SelectRow(n)       
	
	METHOD GoHome()                       
	METHOD GoEnd() 
	METHOD GoPgUp() 
	METHOD GoPgDown()      
	METHOD GoUp(nOffSet)
	METHOD GoDown(nOffSet)     
	METHOD lEdtGet(aCampos,oBrowse,cPict,nCol,cF3,lReadOnly)	
	METHOD SetCSS(cCSS)    
	METHOD GetRect()

END CLASS
                    
METHOD GetRect() CLASS TGRIDCss
Local oRet
Return oRect

METHOD New( oDlg, aData ) CLASS TGRIDCss     
local nPos
	::oFrame:= tPanel():New(4,4,,oDlg,,,,,,580,230 )
	::nRecNo:= 1
	::nCursorPos:= 0     
	::nVisibleRows:= 14 // Forçado para 1o ::GoEnd()
	
	::aData:= aData
	::nLenData:= Len(aData)
	
	::oGrid:= TGrid():New( ::oFrame )
	::oGrid:Align:= CONTROL_ALIGN_ALLCLIENT                       
	
	::oButtonsFrame:= TPanel():New(0,0,, ::oFrame,,,,,, 10,230,.F.,.T. )
	::oButtonsFrame:Align:= CONTROL_ALIGN_RIGHT
    
	::oButtonHome:= TBtnBmp():NewBar( "VCTOP.BMP",,,,, {||::GoHome()},,::oButtonsFrame )
	::oButtonHome:Align:= CONTROL_ALIGN_TOP

	::oButtonPgUp:= TBtnBmp():NewBar( "VCPGUP.BMP",,,,, {||::GoPgUp()},,::oButtonsFrame )
	::oButtonPgUp:Align:= CONTROL_ALIGN_TOP 
	
	::oButtonUp:= TBtnBmp():NewBar( "VCUP.BMP",,,,,{||::GoUp(1)},,::oButtonsFrame )
	::oButtonUp:Align:= CONTROL_ALIGN_TOP 

	::oButtonEnd:= TBtnBmp():NewBar( "VCBOTTOM.BMP",,,,, {||::GoEnd()},,::oButtonsFrame )
	::oButtonEnd:Align:= CONTROL_ALIGN_BOTTOM

	::oButtonPgDown:= TBtnBmp():NewBar( "VCPGDOWN.BMP",,,,, {||::GoPgDown()},,::oButtonsFrame )
	::oButtonPgDown:Align:= CONTROL_ALIGN_BOTTOM

	::oButtonDown:= TBtnBmp():NewBar( "VCDOWN.BMP",,,,, {||::GoDown(1)},,::oButtonsFrame )
	::oButtonDown:Align:= CONTROL_ALIGN_BOTTOM 

  // ITEM | PROD.ORIGEM | PROD.DESTINO| DESCRIÇÃO | QUANTIDADE | VALOR UNIT | VALOR TOTAL | % IPI | VALOR IPI | % ICMS | VALOR ICMS 	
	::oGrid:addColumn( 1,  "ITEM"         , 60, CONTROL_ALIGN_LEFT )
	::oGrid:addColumn( 2,  "PROD.ORIGEM"  , 80, 0 )
	::oGrid:addColumn( 3,  "PROD.DESTINO" , 80, CONTROL_ALIGN_LEFT ) 
	::oGrid:addColumn( 4,  "DESCRIÇÃO"    , 500, CONTROL_ALIGN_LEFT ) 	
	::oGrid:addColumn( 5,  "QUANTIDADE"   , 80, CONTROL_ALIGN_LEFT ) 
	::oGrid:addColumn( 6,  "VALOR UNIT"   , 80, CONTROL_ALIGN_LEFT ) 		
	::oGrid:addColumn( 7,  "VALOR TOTAL"  , 80, CONTROL_ALIGN_LEFT ) 			
	::oGrid:addColumn( 8,  "% IPI"        , 80, CONTROL_ALIGN_LEFT ) 			
	::oGrid:addColumn( 9,  "VALOR IPI"    , 80, CONTROL_ALIGN_LEFT ) 				
	::oGrid:addColumn( 10, "% ICMS"       , 80, CONTROL_ALIGN_LEFT ) 					
	::oGrid:addColumn( 11, "VALOR ICMS"   , 80, CONTROL_ALIGN_LEFT ) 						
	::oGrid:bCursorMove:= {|o,nMvType,nCurPos,nOffSet,nVisRows| ::onMove(o,nMvType,nCurPos,nOffSet,nVisRows) } 
    ::ShowData(1)    
	                             
	::oGrid:SetSelectionMode(0)
    ::SelectRow( ::nCursorPos )  
                                                        

	// configura acionamento do duplo clique  
    ::oGrid:bLDblClick:= {|| ::lEdtGet(aData,::oGrid,"",1,"", .F., nCurPos , ::oFrame )  } 
    //  oBrw:bLDblClick := { || BrwEdtCol( __aEDITAL,oBrw,"@!", 2, "", .F., 2, @aItems ) }

RETURN

METHOD isBof() CLASS TGRIDCss
RETURN ( ::nRecno==1 )

METHOD isEof() CLASS TGRIDCss
RETURN ( ::nRecno==::nLenData )

METHOD GoHome() CLASS TGRIDCss

	if ::isBof()
		return
	endif		                 
	
	::nRecno = 1
	::oGrid:ClearRows()
	::ShowData( 1, ::nVisibleRows )    	
	::nCursorPos:= 0
	::SelectRow( ::nCursorPos )	

RETURN
                
METHOD GoEnd() CLASS TGRIDCss
  if ::isEof()
  	return
  endif                                     
  ::nRecno:= ::nLenData
	::oGrid:ClearRows()
	::ShowData( ::nRecno - ::nVisibleRows + 1, ::nVisibleRows )  
	::nCursorPos:= ::nVisibleRows-1
	::SelectRow( ::nCursorPos )
RETURN

METHOD GoPgUp() CLASS TGRIDCss
	if ::isBof()
		return
	endif                                
	// força antes ir para a 1a linha da grid           
	if ::nCursorPos != 0    
		::nRecno -= ::nCursorPos
		if ::nRecno<=0 
			::nRecno:=1
		endif			     
		::nCursorPos:= 0
		::oGrid:setRowData( ::nCursorPos, {|o| { ::aData[::nRecno,1], ::aData[::nRecno,2], ::aData[::nRecno,3] , ::aData[::nRecno,4], ::aData[::nRecno,5] ,;
		, ::aData[::nRecno,6], ::aData[::nRecno,7], ::aData[::nRecno,8], ::aData[::nRecno,9], ::aData[::nRecno,10], ::aData[::nRecno,11]  } } ) 		
	else
		::nRecno -= ::nVisibleRows
		if ::nRecno<=0 
			::nRecno:=1		
		endif
		::oGrid:ClearRows()
		::ShowData( ::nRecno, ::nVisibleRows )
		::nCursorPos:= 0
	endif				

	::SelectRow( ::nCursorPos )
RETURN 

METHOD GoPgDown() CLASS TGRIDCss

Local nLastVisRow

if ::isEof()
		return
endif                                         
// força antes ir para a última linha da grid	
nLastVisRow:= ::nVisibleRows-1 

if ::nCursorPos!=nLastVisRow    
	if ::nRecno+nLastVisRow > ::nLenData
		nLastVisRow:= ( ::nRecno+nLastVisRow ) - ::nLenData
		::nRecno:= ::nLenData
	else		
		::nRecNo += nLastVisRow
	endif

	::nCursorPos:= nLastVisRow
	::oGrid:setRowData( :::nCursorPos, {|o| { ::aData[::nRecno,1], ::aData[::nRecno,2], ::aData[::nRecno,3] , ::aData[::nRecno,4], ::aData[::nRecno,5] ,;
		, ::aData[::nRecno,6], ::aData[::nRecno,7], ::aData[::nRecno,8], ::aData[::nRecno,9], ::aData[::nRecno,10], ::aData[::nRecno,11]  } }  )
else  
	::oGrid:ClearRows()				
	::nRecno += ::nVisibleRows
	if ::nRecno > ::nLenData
		::nVisibleRows = ::nRecno-::nLenData
		::nRecno:= ::nLenData
	endif 
	::ShowData( ::nRecNo - ::nVisibleRows + 1, ::nVisibleRows )
	::nCursorPos:= ::nVisibleRows-1
endif   

::SelectRow( ::nCursorPos )				

RETURN

METHOD GoUp(nOffSet) CLASS TGRIDCss

Local lAdjustCursor:= .F.

if ::isBof()
	RETURN
endif		       

if ::nCursorPos==0
	::oGrid:scrollLine(-1)
	lAdjustCursor:= .T.
else            		
	::nCursorPos -= nOffSet		
endif		   
      
::nRecno -= nOffSet
  
  // atualiza linha corrente  
::oGrid:setRowData( ::nCursorPos, {|o| { ::aData[::nRecno,1], ::aData[::nRecno,2], ::aData[::nRecno,3] , ::aData[::nRecno,4], ::aData[::nRecno,5] ,;
		, ::aData[::nRecno,6], ::aData[::nRecno,7], ::aData[::nRecno,8], ::aData[::nRecno,9], ::aData[::nRecno,10], ::aData[::nRecno,11]  } }  ) 

if lAdjustCursor  
	::nCursorPos:= 0
endif	              

::SelectRow( ::nCursorPos )

RETURN

METHOD GoDown(nOffSet) CLASS TGRIDCss
    
Local lAdjustCursor:= .F.  
  
if ::isEof()
	RETURN
endif      

if ::nCursorPos==::nVisibleRows-1
	::oGrid:scrollLine(1)		 
	lAdjustCursor:= .T.
else
	::nCursorPos += nOffSet						
endif
                 
::nRecno += nOffSet

// atualiza linha corrente  
::oGrid:setRowData( ::nCursorPos, {|o| { ::aData[::nRecno,1], ::aData[::nRecno,2], ::aData[::nRecno,3] , ::aData[::nRecno,4], ::aData[::nRecno,5] ,;
		, ::aData[::nRecno,6], ::aData[::nRecno,7], ::aData[::nRecno,8], ::aData[::nRecno,9], ::aData[::nRecno,10], ::aData[::nRecno,11]  } }  ) 
				
if lAdjustCursor 
	::nCursorPos:= ::nVisibleRows-1
endif		  	                   
             
::SelectRow( ::nCursorPos )       

RETURN

METHOD onMove( oGrid,nMvType,nCurPos,nOffSet,nVisRows ) CLASS TGRIDCss
                          
::nCursorPos:= nCurPos
::nVisibleRows:= nVisRows

if nMvType == GRID_MOVEUP  
	::GoUp(nOffSet)
elseif nMvType == GRID_MOVEDOWN       
	::GoDown(nOffSet)
elseif nMvType == GRID_MOVEHOME           
	::GoHome()
elseif nMvType == GRID_MOVEEND
	::GoEnd()  
elseif nMvType == GRID_MOVEPAGEUP
	::GoPgUp()
elseif nMvType == GRID_MOVEPAGEDOWN 
	::GoPgDown()
endif

RETURN
             
METHOD ShowData( nFirstRec, nCount ) CLASS TGRIDCss

local i, nRec, ci
DEFAULT nCount:=30

for i=0 to nCount-1 

	nRec:= nFirstRec+i

	if nRec > ::nLenData
		RETURN
	endif			

	ci:= Str( nRec )             
	cb:= "{|o| { Self:aData["+ci+",1], Self:aData["+ci+",2], Self:aData["+ci+",3] , Self:aData["+ci+",4] , Self:aData["+ci+",5] , Self:aData["+ci+",6] , Self:aData["+ci+",7] , Self:aData["+ci+",8] , Self:aData["+ci+",9] , Self:aData["+ci+",10] , Self:aData["+ci+",11]} }"
	::oGrid:setRowData( i, &cb )

next i     

RETURN

METHOD ClearRows() CLASS TGRIDCss
	::oGrid:ClearRows()
	::nRecNo:=1
RETURN                                                         

METHOD DoUpdate() CLASS TGRIDCss     
	::nRecNo:=1
	::Showdata(1)
	::SelectRow(0)
RETURN

METHOD SelectRow(n) CLASS TGRIDCss
	::oGrid:setSelectedRow(n)
RETURN           

METHOD SetCSS(cCSS) CLASS TGRIDCss
	::oGrid:setCSS(cCSS)
RETURN   



METHOD lEdtGet(aCampos,oBrowse,cPict,nCol,cF3,lReadOnly, nCurPos, oFrame ) CLASS TGRIDCss
local oDlg
local oRect
local oGet1
local oBtn
local cMacro := ''
local nRow   := 1
local oOwner := oBrowse:oWnd 
local aRect := {}              
local nColPos := oBrowse:ColPos()  
//local aRect := ::GetRect()

//local nPos := aScan( aRotina , { |x| x[4] == nOpc } )
DEFAULT cPict := ''
DEFAULT nCol  := 1
DEFAULT lReadOnly := .F.

  oRect := TRect():New(0,0,0,0)            // obtem as coordenadas da celula (lugar onde
//  oRect := oBrowse:GetRect()
  //aRect := oBrowse:GetCellRect( aRect )   // a janela de edicao deve ficar)
// aRect := GetCoors( ::hWnd )
  //aDim  := {oRect:nTop,oRect:nLeft,oRect:nBottom,oRect:nRight}
   
   aDim := { nCurPos,nColPos,15,40  }
  DEFINE MSDIALOG oDlg OF oOwner  FROM 0, 0 TO 0, 0 STYLE nOR( WS_VISIBLE, WS_POPUP ) PIXEL

  cMacro := "M->CELL"+StrZero(nRow,6)
  &cMacro:= aCampos[nRow,nCol]
  cPict  := cPict

  oGet1 := TGet():New(0,0,bSetGet(&(cMacro)),oDlg,0,0,cPict,,,,oOwner:oFont,,,.T.,,,,,,,lReadOnly,,cF3,,,,,,.T.)
  
  oGet1:Move(-2,-2, (aDim[ 4 ] - aDim[ 2 ]) + 4, aDim[ 3 ] - aDim[ 1 ] + 4 )

  @ 0, 0 BUTTON oBtn PROMPT "ze" SIZE 0,0 OF oDlg
  oBtn:bGotFocus := {|| oDlg:nLastKey := VK_RETURN, oDlg:End(0)}

  oGet1:cReadVar  := cMacro

  ACTIVATE MSDIALOG oDlg ON INIT oDlg:Move(aDim[1],aDim[2],aDim[4]-aDim[2], aDim[3]-aDim[1])

  aCampos[nRow,nCol] := &cMacro
  //oBrowse:aArray[nRow,nCol] := &cMacro
 // oBrowse:nAt := nRow
 //oBrowse:ColOrder ( aArray )
  SetFocus(oBrowse:hWnd)
  oBrowse:Refresh()

Return Nil

      
// U_TSTGRID ( Executa Grid )
//------------------------------------------------------------------         
Static Function getNFeItens( aDATA )
                                   
Local oDlg, i, oGrid
Local oEdit, nEdit:= 0
Local oBtnAdd, oBtnClr, oBtnLoa
Local cCSSQTbV := ""
Local cCSS := ""
LOCAL aRect
LOCAL ORECT 
local oFontBold := TFont():New('Arial',,14,,.T.,,,,.F.,.F.)                             
local oBtnCan
local qBTN_BLUE := ""
// configura pintura da TGrid
//Local cCSS:= "QTableView{ alternate-background-color: red; background: yellow; selection-background-color: #669966 }"                              
// qlineargradient(x1: 0, y1: 0, x2: 0.5, y2: 0.5, stop: 0 #FF92BB, stop: 1 white,color:#000000)
// background-color: qlineargradient(spread:reflect, x1:0.500318, y1:1, x2:1, y2:0.30, stop:0.0795455  rgba(0, 126, 170, 255), stop:1 rgba(100, 254, 255, 227))
//Local cCSS:= "QTableView{   selection-background-color: background-color: qlineargradient(spread:reflect, x1:0.500318, y1:1, x2:1, y2:0.30, stop:0.0795455  rgba(0, 126, 170, 255), stop:1 rgba(100, 254, 255, 227)); color: #000000; font: 'Segoe UI';font-size:09px;font-weight: bold; } "

// configura pintura do Header da TGrid	
//cCSS+= "QHeaderView::section { background-color: qlineargradient(x1:0, y1:0, x2:0, y2:1, stop:0 #616161, stop: 0.5 #505050, stop: 0.6 #434343, stop:1 #656565); color: white; padding-left: 4px; border: 1px solid #6c6c6c; }"
  
qBTN_BLUE += " QPushButton {"
qBTN_BLUE += "    font: 1em;             "
qBTN_BLUE += "    margin: 0 1px 0 1px;"
qBTN_BLUE += "    color: white;"
qBTN_BLUE += "    background-color: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1, "
qBTN_BLUE += "                                      stop: 0 #2198c0, stop: 1 #0d5ca6);"
qBTN_BLUE += "    border-style: outset;"
qBTN_BLUE += "    border-radius: 3px;"
qBTN_BLUE += "    border-width: 1px;"
qBTN_BLUE += "    border-color: #0c457e;"
qBTN_BLUE += "}"
qBTN_BLUE += " "
qBTN_BLUE += "QPushButton:pressed {                      "
qBTN_BLUE += "    background-color: qlineargradient(x1: 0, y1: 0, x2: 0, y2: 1,"
qBTN_BLUE += "                                      stop: 0 #0d5ca6, stop: 1 #2198c0);"
qBTN_BLUE += "}" "


cCSSQTbV += " QTableView{                "
cCSSQTbV += "  border: 1px solid #6c6c6c; "
cCSSQTbV += "  border-radius: 20px;       "
cCSSQTbV += "  background-color:transparent; "
cCSSQTbV += "  gridline-color: gray; "
cCSSQTbV += " } "
cCSSQTbV += "  "
cCSSQTbV += " QTableView QHeaderView{ "
cCSSQTbV += "   border-top-right-radius: 20px; "
cCSSQTbV += "   border-bottom-left-radius: 20px; "
cCSSQTbV += "   background-color:transparent;  "
cCSSQTbV += " } "
cCSSQTbV += " "
cCSSQTbV += " QTableCornerButton::section{ "
cCSSQTbV += "  border-top-left-radius: 20px; "
cCSSQTbV += " } "    
cCSSQTbV += " QTableView::item:focus{ "
cCSSQTbV += " border: none; "
//cCSSQTbV += " background-color :rgb(249, 249, 238); "
cCSSQTbV += " selection-background-color:qlineargradient(spread:reflect, x1:0.500318, y1:1, x2:1, y2:0.30, stop:0.0795455  rgba(0, 126, 170, 255), stop:1 rgba(100, 254, 255, 227)) "
cCSSQTbV += " selection-color : black; "
cCSSQTbV += "} "

cCSS += " "
cCSS += "QTableView { "
cCSS += "     selection-background-color: qlineargradient(x1: 0, y1: 0, x2: 0.5, y2: 0.5, "
cCSS += "                                 stop: 0 #FF92BB, stop: 1 white); "
cCSS += " } "

// NOVO 2
cCSS := "  " 
cCSS += "QTableView { "
//cCSS += "     selection-background-color: qlineargradient(x1:0, y1:0, x2:0, y2:1, "
//cCSS += "                                       stop:0 #616161, stop: 0.5 #505050, "
//cCSS += "                                       stop: 0.6 #434343, stop:1 #656565); "
cCSS += "     selection-background-color: qlineargradient(x1:0, y1:0, x2:0, y2:1, "
cCSS += "                                       stop:0 #616161, stop: 0.5 rgba(0, 126, 170, 255), "
cCSS += "                                       stop: 0.6 #434343, stop:1 rgba(100, 254, 255, 227) ); "

cCSS += "     color: black; "
cCSS += "     padding-left: 4px; "
cCSS += "     border: 1px solid #6c6c6c; "
cCSS += "	 height:25px; "      
cCSS += " } "      


// NOVO 3

cCSS := ""
cCSS += " QHeaderView { "
cCSS += " "
cCSS += "        background-color: qlineargradient(x1:0, y1:0, x2:0, y2:1, "
cCSS += " "
cCSS += "            stop:0 #ffffff, stop:0.5 #e0e0e0, stop:1 #ffffff); "
cCSS += " "
cCSS += "    } "
cCSS += " "
cCSS += "    QHeaderView::section { "
cCSS += " "
cCSS += "        background-color: qlineargradient(x1:0, y1:0, x2:0, y2:1, "
cCSS += " "
cCSS += "            stop:0 #ffffff, stop:0.5 #e0e0e0, stop:1 #ffffff); "
cCSS += " "
cCSS += "        border: 1px solid #a0a0a0; "
cCSS += " "
cCSS += "        border-top: 0px; "
cCSS += " "
cCSS += "        border-left: 0px; "
cCSS += " "
cCSS += "        padding-left: 4px; "
cCSS += " "
cCSS += "        padding-right: 4px; "
cCSS += " "
cCSS += "    } "
cCSS += " "
cCSS += "    QHeaderView::section:checked { "
cCSS += " "
cCSS += "        background-color: qlineargradient(x1:0, y1:0, x2:0, y2:1, "
cCSS += " "
cCSS += "            stop:0 #bfd5ea, stop:0.5 #5497d9, stop:1 #bfd5ea); "
cCSS += ""
cCSS += "    } "
cCSS += " "
cCSS += "    QHeaderView::section:pressed { "
cCSS += " "
cCSS += "        background-color: qlineargradient(x1:0, y1:0, x2:0, y2:1, "
cCSS += " "
cCSS += "            stop:0 #b0c6db, stop:0.5 #4588ca, stop:1 #b0c6db); "
cCSS += " "
cCSS += "    }"

// NOVO 4
cCSS := " "
cCSS += " QTableView { "
cCSS += "     selection-background-color: qlineargradient(x1: 0, y1: 0, x2: 0.5, y2: 0.5, "
cCSS += "                                 stop: 0 #00cee6, stop: 1 #91ebf6);  color: #000000; font: 'Segoe UI';font-size:09px;font-weight: bold; Foreground-color:#000000; "
cCSS += " } "
cCSS += " QTableView QTableCornerButton::section { "
cCSS += "     background: red; "
cCSS += "     border: 2px outset red; "
cCSS += " } "
cCSS += " QHeaderView::down-arrow { "
cCSS += "     background-color: qlineargradient(x1:0, y1:0, x2:0, y2:1, "
cCSS += "                                       stop:0 #293e6b, stop: 1 #F4F4F4); "
cCSS += " } "





// Dados                      
/*
for i:=1 to 10000                          
	cCodProd:= StrZero(i,6)     
	if i<3           
		// inserindo imagem nas 2 primeiras linhas
		cProd:= "RPO_IMAGE=OK.BMP" 
	else	
		cProd:= 'Produto '+cCodProd
	endif		
	cVal = Transform( 10.50, "@E 99999999.99" )
	AADD( aData, { cCodProd, cProd, cVal, "LIN"+ALLTRIM(Str(i,10)),'','','','','','','' } )
	
next
 */
DEFINE DIALOG oDlg FROM 0,0 TO 600,1200 PIXEL

oGrid:= TGRIDCss():New(oDlg,@aData)

//aRect := GETCLIENTRECT( oGrid:hWnd )
//aRect := GetClientRect( oGrid:hWnd )   // GetWndRect( oGrid:hWnd  )
 
//ORECT := oGrid:ownd:GETCLIRECT()

// Aplica configuração de pintura via CSS
oGrid:SetCSS( cCSS )                              


 
oBtnCan := TButton():New( 250 , 5,;
							"Fechar",oDlg,{|| oDlg:End() },80,15,,,,.T.)
oBtnCan:SetCSS( qBTN_BLUE )

 ACTIVATE DIALOG oDlg CENTERED

RETURN

User Function GETNFEIT(  )
local __cNameAlias := GETNEXTALIAS()
local cQuery := ""
local aData := {}    
local nX := 0
local cProd := ""
                
cQuery := " SELECT * FROM "  + RETSQLNAME("ZD1") 
cQuery += " WHERE ZD1_CNPJ = '" + ZF1->ZF1_CNPJ + "' AND ZD1_DOC = '" + ZF1->ZF1_DOC + "'   AND ZD1_SERIE = '" + ZF1->ZF1_SERIE + "' AND ZD1_LOJA ='"  + ZF1->ZF1_LOJA + "' "
cQuery += " AND ZD1_FILIAL ='" + xFilial("ZD1") + "'  AND D_E_L_E_T_ = ' ' " 

dbUseArea(.T.,__CONNECT,TcGenQry(,,cQuery),__cNameAlias,.T.,.T.)
 
 
  // ITEM | PROD.ORIGEM | PROD.DESTINO| DESCRIÇÃO | QUANTIDADE | VALOR UNIT | VALOR TOTAL | % IPI | VALOR IPI | % ICMS | VALOR ICMS 	
 While (__cNameAlias)->(!Eof())  

		cAlias := GETNEXTALIAS()
		
		cQuery := " SELECT * FROM "  + RETSQLNAME("ZNI") + " WHERE ZNI_CNPJ = '" +  ZF1->ZF1_CNPJ  + "' AND ZNI_PRDFOR = '" + (__cNameAlias)->ZD1_COD + "' "  
		
		dbUseArea(.T.,__CONNECT,TcGenQry(,,cQuery),cAlias,.T.,.T.)
		IF (cAlias)->(!Eof())  
        	cProd :=(cAlias)->ZNI_CODPRD
		Else        	
        	cProd := " ??? "
        EndIF	

		(cAlias)->(DBCloseArea())
 
  		nx += 1                                                               
  
  AADD( aDATA, { StrZero(nx,3) , (__cNameAlias)->ZD1_COD , cProd , (__cNameAlias)->ZD1_PRODES, ;
           TransForm(  (__cNameAlias)->ZD1_QUANT, "@E 9999999999999.99" ) ,;
           Transform((__cNameAlias)->ZD1_VUNIT,"@E 9999999999999.99" )	  ,;
           TransForm(  (__cNameAlias)->ZD1_TOTAL, "@E 9999999999999.99")  ,; 
           TransForm( (__cNameAlias)->ZD1_IPI,"@E 9999999999999.99")      ,;
           TransForm( (__cNameAlias)->ZD1_VALIPI, "@E 9999999999999.99" ) ,;
           TransForm( (__cNameAlias)->ZD1_PICM,"@E 9999999999999.99" ) ,;
           TransForm( (__cNameAlias)->ZD1_VALICM, "@E 9999999999999.99" ) }  )
 
           
 (__cNameAlias)->(DBSkip())
 End	

(__cNameAlias)->(DBCloseArea())

IF( aDATA != NIL .OR. Empty( aDATA ) )
	getNFeItens( @aDATA )
Else
	MsgAlert("Não há dados referente a NFe","BOSSWARE")	
EndIF
Return                                                      


Static Function toChars(cString)
Local cChar  := ""
Local nX     := 0 
Local nY     := 0
Local cVogal := "aeiouAEIOU"
Local cAgudo := "áéíóú"+"ÁÉÍÓÚ"
Local cCircu := "âêîôû"+"ÂÊÎÔÛ"
Local cTrema := "äëïöü"+"ÄËÏÖÜ"
Local cCrase := "àèìòù"+"ÀÈÌÒÙ" 
Local cTio   := "ãõÃÕ"
Local cCecid := "çÇ"
Local cMaior := "&lt;"
Local cMenor := "&gt;"

For nX:= 1 To Len(cString)
	cChar:=SubStr(cString, nX, 1)
	IF cChar$cAgudo+cCircu+cTrema+cCecid+cTio+cCrase
		nY:= At(cChar,cAgudo)
		If nY > 0
			cString := StrTran(cString,cChar,SubStr(cVogal,nY,1))
		EndIf
		nY:= At(cChar,cCircu)
		If nY > 0
			cString := StrTran(cString,cChar,SubStr(cVogal,nY,1))
		EndIf
		nY:= At(cChar,cTrema)
		If nY > 0
			cString := StrTran(cString,cChar,SubStr(cVogal,nY,1))
		EndIf
		nY:= At(cChar,cCrase)
		If nY > 0
			cString := StrTran(cString,cChar,SubStr(cVogal,nY,1))
		EndIf		
		nY:= At(cChar,cTio)
		If nY > 0          
			cString := StrTran(cString,cChar,SubStr("aoAO",nY,1))
		EndIf		
		nY:= At(cChar,cCecid)
		If nY > 0
			cString := StrTran(cString,cChar,SubStr("cC",nY,1))
		EndIf
	Endif
Next

If cMaior$ cString 
	cString := strTran( cString, cMaior, "" ) 
EndIf
If cMenor$ cString 
	cString := strTran( cString, cMenor, "" )
EndIf

cString := StrTran( cString, CRLF, " " )

For nX:=1 To Len(cString)
	cChar:=SubStr(cString, nX, 1)
	If (Asc(cChar) < 32 .Or. Asc(cChar) > 123) .and. !cChar $ '|' 
		cString:=StrTran(cString,cChar,".")
	Endif
Next nX
Return cString
                    


User Function SF1140I
               
IF Empty(SF1->F1_ISPC)  // PRÉ-NOTA SEM USO NFe Consulta / XML...
   RecLock("SF1", .F. )
             SF1->F1_MSBLQL := "1"
   MsUnLock()
EndIF                             

Return


Static Function doParamter()
Local aArea := GetArea()
Local aHelpPor := {} 
Local aHelpEng := {}
Local AHelpSpa := {} 
                     

dbSelectArea("SX1")
dbSetOrder(1)
IF( dbSeek(cPerg) )
	
	Do While !Eof() .And. ALLTRIM(SX1->X1_GRUPO)  == cPerg
	    RecLock("SX1", .f.)
	    	dbDelete()
	    MsUnLock()
	   
	    dbSkip()	
	EndDo 

EndIF

   Aadd( aHelpPor, 'Informe Pedido de Compras')
   PutSx1(cPerg   ,"01"  ,"Ped.Compras De"     ,"Ped.Compras De"   ,"Ped.Compras De"   ,"mv_ch1","C"  ,9       ,0       ,0      ,"G" ,""    ,"SC7" ,""     ,"","MV_PAR01",""       ,""       ,""       ,""    ,"","","","","","","","","","","","",aHelpPor,aHelpEng,aHelpSpa)

   aHelpPor := {}
   aHelpEng := {}
   aHelpSpa := {}

   Aadd( aHelpPor, 'Informe Pedido de Compras')
   PutSx1(cPerg   ,"02"  ,"Ped.Compras Ate"    ,"Ped.Compras Ate"   ,"Ped.Compras Ate"   ,"mv_ch2","C"  ,9       ,0       ,0      ,"G" ,""    ,"SC7" ,""     ,"","MV_PAR02",""       ,""       ,""       ,""    ,"","","","","","","","","","","","",aHelpPor,aHelpEng,aHelpSpa)

   aHelpPor := {}
   aHelpEng := {}
   aHelpSpa := {}
   
                                             
/*
   Aadd( aHelpPor, 'Informe Data  Final')
   PutSx1(cPerg   ,"02"  ,"Data  Ate"     ,"Data  Ate"   ,"Data  Ate"   ,"mv_ch2","D"  ,8       ,0       ,0      ,"G" ,""    ,"" ,""     ,"","MV_PAR02",""       ,""       ,""       ,""    ,"","","","","","","","","","","","",aHelpPor,aHelpEng,aHelpSpa)



   aHelpPor := {}
   aHelpEng := {}
   aHelpSpa := {}

   Aadd( aHelpPor, 'Selecione a Filial')
   PutSx1(cPerg   ,"03"  ,"Filial De"     ,"Filial De"   ,"Filial De"   ,"mv_ch3","C"  ,2       ,0       ,0      ,"G" ,""    ,"SM0" ,""     ,"","MV_PAR03",""       ,""       ,""       ,"M0_CODFIL"    ,"","","","","","","","","","","","",aHelpPor,aHelpEng,aHelpSpa)
   
   aHelpPor := {}
   aHelpEng := {}
   aHelpSpa := {}

   Aadd( aHelpPor, 'Selecione a Filial')
   PutSx1(cPerg   ,"04"  ,"Filial Ate"     ,"Filial Ate"   ,"Filial Ate"   ,"mv_ch4","C"  ,2       ,0       ,0      ,"G" ,""    ,"SM0" ,""     ,"","MV_PAR04",""       ,""       ,""       ,"M0_CODFIL"    ,"","","","","","","","","","","","",aHelpPor,aHelpEng,aHelpSpa)

 */
   RestArea(aArea)
   
 Return

/* 

  PARA USO PEDIDO DE COMPRAS

	If (ExistBlock("SF1140I"))
		ExecBlock("SF1140I",.F.,.F.)
	EndIf


dbSelectArea("SB1")
dbSetOrder(1)
MsSeek(xFilial("SB1")+SD1->D1_COD)
				
DBSelectArea("SC7")
SC7->(DbSetOrder(1))
SC7->(MsSeek(xFilial("SC7")+SD1->D1_PEDIDO+SD1->D1_ITEMPC))

SD1->D1_GRUPO	:= SB1->B1_GRUPO
SD1->D1_RATEIO	:= IIF(SC7->(FieldPos("C7_RATEIO"))>0,SC7->C7_RATEIO,"")



lMsHelpAuto          :=.F.
lMsErroAuto          :=.F.

Aadd(_aCabSF1,{"F1_FILIAL"    ,_cFSFILDEST          ,Nil})
Aadd(_aCabSF1,{"F1_DOC"       ,TSC7->C7_XNFFAS     ,Nil})
Aadd(_aCabSF1,{"F1_SERIE"     ,TSC7->C7_XSERFAS     ,Nil})
Aadd(_aCabSF1,{"F1_FORNECE"    ,TSC7->C7_FORNECE     ,Nil})
Aadd(_aCabSF1,{"F1_LOJA"       ,TSC7->C7_LOJA          ,Nil})
Aadd(_aCabSF1,{"F1_COND"       ,TSC7->C7_COND          ,Nil})
Aadd(_aCabSF1,{"F1_EMISSAO"    ,TSC7->C7_EMISSAO     ,Nil})
Aadd(_aCabSF1,{"F1_FORMUL"     ,"N"               ,Nil})
Aadd(_aCabSF1,{"F1_ESPECIE"    ,"NFE"               ,Nil})
Aadd(_aCabSF1,{"F1_TIPO, 
Aadd(_aCabSF1,{"F1_DTDIGIT, 
Aadd(_aCabSF1,{"F1_EST      
AADD(aCab, {"F1_CHVNFE"        ,_cChavNFE })
aadd( aCab, { "E2_NATUREZ"   ,"2.3.14" })      
aadd(aCab,{"E2_VENCTO",_dDtVenc})   &#61671;== não está pegando esta data..


Aadd(_aCabSF1,{"D1_CF, 

                    
          
_aLinha:={}
               
Aadd(_aLinha,{"D1_FILIAL"     ,_cFSFILDEST          ,Nil})
Aadd(_aLinha,{"D1_ITEM"          ,TSC7->C7_ITEM          ,Nil})
Aadd(_aLinha,{"D1_COD"          ,TSC7->C7_PRODUTO     ,Nil})
Aadd(_aLinha,{"D1_UM"          ,TSC7->C7_UM          ,Nil})
Aadd(_aLinha,{"D1_QUANT"     ,TSC7->C7_QUANT          ,Nil})
Aadd(_aLinha,{"D1_VUNIT"     ,TSC7->C7_PRECO          ,Nil})
Aadd(_aLinha,{"D1_TOTAL"     ,TSC7->C7_TOTAL          ,Nil})
Aadd(_aLinha,{"D1_OPER"          ,_cFSOPERENT          ,Nil})
Aadd(_aLinha,{"D1_TES"          ,TSC7->C7_TES          ,Nil})
Aadd(_aLinha,{"D1_PEDIDO"     ,TSC7->C7_NUM          ,Nil})
Aadd(_aLinha,{"D1_ITEMPC"     ,TSC7->C7_ITEM          ,Nil})
Aadd(_aLinha,{"D1_FORNECE"     ,TSC7->C7_FORNECE     ,Nil})
Aadd(_aLinha,{"D1_LOJA"          ,TSC7->C7_LOJA          ,Nil})
Aadd(_aLinha,{"D1_LOCAL"     ,TSC7->C7_LOCAL          ,Nil})
Aadd(_aLinha,{"D1_DOC"          ,TSC7->C7_XNFFAS     ,Nil})
Aadd(_aLinha,{"D1_EMISSAO"     ,TSC7->C7_EMISSAO     ,Nil})
Aadd(_aLinha,{"D1_TIPO"          ,"N"               ,Nil})
Aadd(_aLinha,{"D1_SERIE"     ,TSC7->C7_XSERFAS     ,Nil})
Aadd(_aLinha,{"D1_FORMUL"     ,"N"               ,Nil})
Aadd(_aLinha,{"AUTDELETA"      ,"N"               ,Nil})                
Aadd(_aLinha,{"D1_RATEIO, 
Aadd(_aLinha,{"D1_DTDIGIT, 
Aadd(_aLinha,{"D1_TP
Aadd(_aItensSD1,_aLinha)

MSExecAuto({|x,y,z| MATA103(x,y,z)},_aCabSF1,{_aItensSD1},3)     
If lMsErroAuto
     MostraErro()
Endif          



User Function MT100AGR()
Local cNota	:=	SD1->D1_DOC
Local cSerie	:=	SD1->D1_SERIE
Local cFornece	:=	SD1->D1_FORNECE
Local cLoja	:=	SD1->D1_LOJA
Local aAreaSD1	:=	SD1->(GetArea())
Local aAreaSB8	:=	SB8->(GetArea())
Local aAreaSB1	:=	SB1->(GetArea())
nRecSd1 := SD1->(Recno())

SD1->(DBSETORDER(1))


*/


Static Function getPedCom(  cFornece, cLoja )
Local oFont := TFont():New("Arial",,-11,.T.)
Local oSearch
Local cSearch := Space(40)
Local oBtn
aList := {}       

lProcess := .T.
                                   
aList := PCQuery( cFornece, cLoja )

IF Empty ( aList )
    MsgAlert("Pedido de compras não encontrado. Será utilzado Pré-Nota/Normal","BOSSWARE")
    lUsaPedCom := .F.   
    lProcess := .T.
    Return
Else
	lUsaPedCom := .T.   
	lProcess := .T.
EndIF

DEFINE DIALOG oDlg TITLE "Pedido de Compras" FROM 180,180 TO 650,1100 PIXEL     

//        aBrowse   := {  {.F.,'CLIENTE 001','RUA CLIENTE 001',111.11} }
                                                               
 
	    oSay:= TSay():New(015,005,{||"Filtrar"},oDlg,,oFont,,,,.T.,CLR_BLUE,CLR_WHITE,100,20) 
	    oSearch:= TGet():New(012,40, {|U|      ;
	                 IF(PCount()>0,cSearch:= U,cSearch)},;
	                 oDlg,140,010,"@!",{|O|cSearch!=""},,,,,,.T.,,,,,,,,,"","cSearch")        
   
        oBrowse := TCBrowse():New( 40 , 01, 500, 156,,;
                                    {'','Pedido','Item','Produto','Descrição','Tipo','Quant'},{20,50,50,100,50,100},;
                                    oDlg,,,,,{||},,,,,,,.F.,,.T.,,.F.,,, )
 
        oBrowse:SetArray(aList)
  
        oBrowse:bLine := {||{ IF(aList[oBrowse:nAt,01],oOK,oNO),;
                                    aList[oBrowse:nAt,02],;
                                    aList[oBrowse:nAt,03],;
                                    aList[oBrowse:nAt,04],;                                    
                                    aList[oBrowse:nAt,05],;                                    
                                    aList[oBrowse:nAt,06],;                                                                        
                                    Transform(aList[oBrowse:nAT,07],'@E 9999999.99') } }
                                           
		oBrowse:Refresh()
 
	 //	oSearch:bValid := {|| ALERT( 'bValid')  }
     //	oSearch:bChange := {|| ALERT( 'bChange')  }
				                        
        oBtn := TButton():New(012,190,"Buscar",oDlg,{|| search(cSearch,0)},50,10,,,,.T.)                                   
     				
        oBrowse:bLDblClick  := {|| toArray(oBrowse:nAt,oBrowse,@aList)  }
        
        
        TButton():New( 210, 002, "Confirma"    , oDlg ,{||  getList( @aList , oDlg) },40,010,,,.F.,.T.,.F.,,.F.,,,.F. )
        TButton():New( 210, 052, "Cancelar"    , oDlg ,{||  oDlg:End() },40,010,,,.F.,.T.,.F.,,.F.,,,.F. )
        
    ACTIVATE DIALOG oDlg CENTERED 
RETURN                                                  


Static Function search(cSearch,nOper)  
Local _nI  := 0
Local _nY  := 0
Local _nX  := 0
Local nPOS := 0 
Local aPESQ := {} 
Local aARRAY := {}
Local aRESULT := {}                  
Local lRet := isLetterOrNum(ALLTRIM(cSearch))


aList := aCLONE ( __aLST  )
aPESQ := aCLONE ( aList )

// __aLST

IF(!Empty(cSearch))
	For _nI := 1 To Len(aPESQ)
	    IF( lRet )
		     IF( ALLTRIM(cSearch) $ ALLTRIM(aPESQ[_nI,2])  )
	            aList[_nI,9] := .T. // found...
	         EndIF   
        EndIF

	Next
	

	For _nY := 1 To Len(aList)
	    IF( aList[_nY,9] )
	     	AADD(aARRAY, { aList[_nY,1],aList[_nY,2],aList[_nY,3],aList[_nY,4] ,aList[_nY,5] , aList[_nY,6], aList[_nY,7], aList[_nY,8], aList[_nY,9], aList[_nY,10] } )
	   EndIF
	Next                           
	
                        
	aList := Nil
    aList := {}
    
		      
	For _nY := 1 To Len(aARRAY)
	     	AADD(aList, { aARRAY[_nY,1],aARRAY[_nY,2],aARRAY[_nY,3],aARRAY[_nY,4] ,aARRAY[_nY,5] , aARRAY[_nY,6], aARRAY[_nY,7], aARRAY[_nY,8], .F., aARRAY[_nY,10] } )
	Next 
                                                                
                       
//		 ASort(aList,,,{|x,y| x[1] == .T. })
 


        oBrowse:SetArray(aList)
  
        oBrowse:bLine := {||{ IF(aList[oBrowse:nAt,01],oOK,oNO),;
                                    aList[oBrowse:nAt,02],;
                                    aList[oBrowse:nAt,03],;
                                    aList[oBrowse:nAt,04],;                                    
                                    aList[oBrowse:nAt,05],;                                    
                                    aList[oBrowse:nAt,06],;                                                                        
                                    Transform(aList[oBrowse:nAT,07],'@E 9999999.99') } }
                                           
		oBrowse:Refresh()
				                        
        oBrowse:bLDblClick  := {|| toArray(oBrowse:nAt,oBrowse,@aList)  }

	                            
Else
     
 
        oBrowse:SetArray(aList)
  
        oBrowse:bLine := {||{ IF(aList[oBrowse:nAt,01],oOK,oNO),;
                                    aList[oBrowse:nAt,02],;
                                    aList[oBrowse:nAt,03],;
                                    aList[oBrowse:nAt,04],;                                    
                                    aList[oBrowse:nAt,05],;                                    
                                    aList[oBrowse:nAt,06],;                                                                        
                                    Transform(aList[oBrowse:nAT,07],'@E 9999999.99') } }
                                           
		oBrowse:Refresh()
				                        
        oBrowse:bLDblClick  := {|| toArray(oBrowse:nAt,oBrowse,@aList)  }
	
EndIF	
	
	
	
return

Static Function toArray(nPos,oBrowse,aList) 
Local nRecno := 0
Local xPos := 0

lSelectPC := .F.

	IF (aList[nPos,1])                           
		  aList[nPos,1]:= .F.
		  lSelectPC := .F.
		Else                   
		  aList[nPos,1]:= .T.
		  lSelectPC := .T.
	EndIF

	 
	 
     xPos := Ascan( __aLST , {|  aVal| aVal[10]  == aList[nPos,10]  })
     IF xPos != 0
		IF (aList[nPos,1])                           
			   __aLST[xPos,1]:= .T.
			    lSelectPC := .T.
			Else                   
			   __aLST[xPos,1]:= .F.
			    lSelectPC := .F.
		EndIF
     EndIF
					
   oBrowse:Refresh()
return
                

Static Function isLetterOrNum(cParam)  
Local lRet := .f.
Local _nI := 0
Local _nY := 0
Local aNUM := {'1','2','3','4','5','6','7','8','9','0'}
Local nCount := 0

	
    For _nI := 1 To Len(cParam)
		For _nY := 1 To Len(aNUM)
		   IF( ALLTRIM(SubStr(cParam,_nI,1) )  == ALLTRIM( aNUM[_nY])  )
              nCount++		                                      
           EndIF
		Next	
	Next
    

return IIF(nCount!=0,.t.,lRet)


Static Function getList( aList , oDlg )
lProcess := .T.
oDlg:End()
Return 

Static Function PCQuery( cFornece, cLoja )
local __cNameAlias 
local __cKey := ""

cQuery := " SELECT A.C7_NUM, A.C7_ITEM, A.C7_PRODUTO, A.C7_QUANT, B.B1_DESC, B.B1_UM, A.R_E_C_N_O_ FROM "  + RETSQLNAME("SC7") + " A "
cQuery += " INNER JOIN "  + RETSQLNAME("SB1") + " B "
cQuery += " ON "
cQuery += " A.C7_PRODUTO = B.B1_COD "
cQuery += " WHERE A.C7_FORNECE = '" + cFornece + "' AND A.C7_LOJA = '" + cLoja + "' AND C7_FILIAL = '" + xFilial("SC7") + "' "
cQuery += " AND A.C7_QUANT > A.C7_QUJE "
cQuery += " AND A.C7_RESIDUO = ''"
cQuery += " AND A.C7_CONAPRO <> 'B' "     
cQuery += " AND A.C7_ENCER = '' "

//cQuery += " WHERE A.C7_NUM BETWEEN '" + MV_PAR01 + "' AND '" +  MV_PAR02  + "' AND A.C7_FORNECE = '" + cFornece + "' AND A.C7_LOJA = '" + cLoja + "' AND C7_FILIAL = '" + xFilial("SC7") + "' "
cQuery += " AND A.D_E_L_E_T_ = ' ' " 
cQuery += " AND B.D_E_L_E_T_ = ' ' " 
cQuery += " ORDER BY C7_NUM, C7_ITEM  " 

__cNameAlias := GETNEXTALIAS()
		 
dbUseArea(.T.,__CONNECT,TcGenQry(,,cQuery),__cNameAlias,.T.,.T.)

__aLST := {}

lExistPC := .F.

IF (__cNameAlias)->(!Eof())

	lExistPC := .T.
	While (__cNameAlias)->(!Eof())
	    __cKey := ALLTRIM( (__cNameAlias)->C7_NUM ) +  ALLTRIM( (__cNameAlias)->C7_ITEM )
 		AADD( aList, { .F., (__cNameAlias)->C7_NUM, (__cNameAlias)->C7_ITEM,  (__cNameAlias)->C7_PRODUTO, (__cNameAlias)->B1_DESC, (__cNameAlias)->B1_UM , (__cNameAlias)->C7_QUANT, __cKey, .F., (__cNameAlias)->R_E_C_N_O_  } )

    (__cNameAlias)->(DBSKIP())
	End 

EndIF

(__cNameAlias)->(DBCLOSEAREA())

__aLST := ACLONE( aList )
    
Return aList


Static Function isExist( cNF, cFornece, cLoja )
local cQuery := ""
local __cNameAlias
local lExist := .F.

cQuery := " SELECT * FROM " +   RETSQLNAME("SF1") + " WHERE F1_DOC = '" + cNF + "' AND F1_FORNECE = '" + cFornece + "' AND F1_FILIAL = '" + xFilial("SF1") + "' " 
cQuery += " AND F1_LOJA = '" + cLoja + "' AND D_E_L_E_T_ = '' "

__cNameAlias := GETNEXTALIAS()
		 
dbUseArea(.T.,__CONNECT,TcGenQry(,,cQuery),__cNameAlias,.T.,.T.)


IF (__cNameAlias)->(!Eof())
      lExist := .T.
EndIF      
             
(__cNameAlias)->(DBCLOSEAREA())

Return lExist


Static Function putPreNota(aCabec,aItens)
//Local aArea := GetArea()
local nItem := 0

//    MsgRun("Aguarde gerando Pré-Nota de Entrada...",,{|| MSExecAuto ( {|x,y,z| MATA140(x,y,z) }, aCabec, aItens, 3)})

//F1_TIPO, F1_DTDIGIT, F1_EST
//D1_CF, D1_RATEIO, D1_DTDIGIT, D1_TP

 MsgRun("Aguarde gerando Pré-Nota de Entrada...",,{|| MSExecAuto ( {|x,y,z| MATA140(x,y,z) }, aCabec, aItens, 3)})
 	
	IF lMsErroAuto
		MostraErro()                                              	
		RollBackSX8()

	Else                              
		ConfirmSX8()   
		
		IF ( MsgYesNo ("Nota Fiscal de Entrada foi processada com sucesso. Deseja visualizar pré-nota ? ", "BOSSWARE") )
		   
		   MATA140( )

/*				dbSelectArea("SF1")
				SET FILTER TO AllTrim(F1_DOC) = AllTrim(ZF1->ZF1_DOC) .AND. AllTrim(F1_SERIE) ==ZF1->ZF1_SERIE
				MATA140()
				dbSelectArea("SF1")
				SET FILTER TO */


		    
		EndIF
			
	EndIF
		


//RestArea(aArea)

Return


User Function ESTP01TES()

Local _aCabec := {}  
Local _aItens := {}
Local _aItemAux := {}

SetPrvt("x,y,z,")

DbSelectArea("SF1")
DbSetOrder( 1 )
	                      
	_aCabec := {{"F1_FILIAL"	,xFilial("SF1")							,NIL},;
	           {"F1_TIPO"	    ,'N'									,NIL},;
			   {"F1_FORMUL"	    ,"N"									,NIL},;
			   {"F1_DOC"		,ZF1->ZF1_DOC							,NIL},;
			   {"F1_SERIE"		,ZF1->ZF1_SERIE							,NIL},;
			   {"F1_EMISSAO"	,ZF1->ZF1_EMISSA						,NIL},;									
			   {"F1_FORNECE"	,ZF1->ZF1_FORNEC    					,NIL},;		
			   {"F1_LOJA"	    ,ZF1->ZF1_LOJA        					,NIL},;		       
			   {"F1_COND" 	    ,ZF1->ZF1_COND							,NIL},;		
			   {"F1_CHVNFE"     ,vCHAVE_NFE                             ,NIL},; 
			   {"F1_ESPECIE"	,"SPED"    								,NIL}}
			   
Aadd( _aItemAux, { "D1_ITEM"   , "0001"    , NIL } )
Aadd( _aItemAux, { "D1_COD"    , "00019580", NIL } )
Aadd( _aItemAux, { "D1_UM"     , "CX"      , NIL } )
Aadd( _aItemAux, { "D1_QUANT"  , 1         , NIL } )
Aadd( _aItemAux, { "D1_VUNIT"  , 219.67    , NIL } )
Aadd( _aItemAux, { "D1_TOTAL"  , 219.67    , NIL } )
Aadd( _aItemAux, { "D1_LOCAL"  , "01"      , NIL } )
Aadd( _aItemAux, { "D1_CC"     , ""        , NIL } )
Aadd( _aItemAux, { "D1_VALDESC", 0         , ""  } )
Aadd( _aItemAux, { "D1_VALFRE" , 0         , ""  } )
Aadd( _aItemAux, { "D1_DESPESA", 0         , ""  } )
Aadd( _aItemAux, { "D1_FORMUL" , "N"       , NIL } )

aAdd( _aItens, Aclone( _aItemAux ) )

lMsErroAuto := .F.
MsExecAuto( {|x,y,z| mata140( x,y,z ) }, _aCabec, _aItens, 3 )

if lMsErroAuto
   MostraErro()
endif
	