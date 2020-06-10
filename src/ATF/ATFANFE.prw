#INCLUDE "PROTHEUS.CH"
#INCLUDE "TOPCONN.CH"

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³ATFANFE   ºAutor  ³GATASSE             º Data ³  27/08/2010 º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³ CARREGA NUMERO DO ANFE                                     º±±
±±º          ³ USAR NA VALIDACAO DE N1_CBASE E N1_GRUPO                   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP                                                         º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/

User Function ATFANFE
/*
Local cLabel	:= "Opção"
Local lOpcOk	:= .F.
Local nOpca	:= 0
Local oRadio
Local oDlg
Local oGroup
Local oFont   
Local oGet
Local aArea:=Getarea()
Local CQUERY := ""
Local cRet:=""
local lRet:=.t.
Public TipoBem_usr := 1

//IF INCLUI    .OR. (FUNNAME()="ATFA240" .AND. LCLASSIFICA)
IF INCLUI    .OR. FUNNAME()="ATFA240"
	if tembem(&(__READVAR))
		DEFINE FONT oFont NAME "Arial" SIZE 0,-11 BOLD
		DEFINE MSDIALOG oDlg FROM  094,001 TO 250,350 TITLE cCadastro PIXEL
		
		@ 015,005	GROUP oGroup TO 075,170 LABEL cLabel OF oDlg PIXEL
		oGroup:oFont:=oFont
		
		@ 023,010	RADIO oRadio VAR TipoBem_usr ITEMS 	"Novo bem para o grupo",;
		"Novo item para o bem" ;
		SIZE 115,010 OF oDlg PIXEL
		
		oDlg:lEscClose := .F. //Nao permite sair ao se pressionar a tecla ESC.
		DEFINE SBUTTON FROM 63, 100 TYPE 1  ACTION (nOpca:=1,oDlg:End())  ENABLE OF oDlg
		ACTIVATE MSDIALOG oDlg CENTERED
	ELSE
		TipoBem_usr:=3
	ENDIF
ENDIF
IF TipoBem_usr==1
	M->N1_CBASE := Proxbem(&(__READVAR))
	M->N1_ANFE  := NovoANFE()
	M->N1_ITEM  := 	"0001"
elseIF TipoBem_usr==2
   	_cBase:=criavar("N1_CBASE")
 	IF  INCLUI    .OR. FUNNAME()="ATFA240" //.AND. LCLASSIFICA)
		DEFINE FONT oFont NAME "Arial" SIZE 0,-11 BOLD
		DEFINE MSDIALOG oDlg FROM  094,001 TO 250,350 TITLE "Novo item para um bem existente" PIXEL
		@ 025,010 Say "Código Base do Bem" PIXEL SIZE 80,09 font oFont of oDlg
		@ 024,120 MSGet oGet  Var _cBase Size 040,08  of oDlg Pixel VALID ValidCB(_cBase)
		oDlg:lEscClose := .F. //Nao permite sair ao se pressionar a tecla ESC.
		DEFINE SBUTTON FROM 63, 100 TYPE 1  ACTION (nOpca:=1,oDlg:End())  ENABLE OF oDlg
		ACTIVATE MSDIALOG oDlg CENTERED
		if empty(_cBase)
			lRet:=.f.
		endif
	else
		_cBase:=SN1->N1_CBASE
	ENDIF  
	M->N1_CBASE := _cBase
	M->N1_ANFE  := ProxAnfe(_cBase)
	M->N1_ITEM  := proxitem(_cBase)
	M->N1_PREFIXO := prefixo(_cBase)
else
	M->N1_CBASE := alltrim(&(__READVAR))+"0001"
	M->N1_ANFE  := NovoANFE()
	M->N1_ITEM  := 	"0001"
endif
M->N1_SEQ:=BuscaSeq()
restarea(aArea)
if lRet
	nPos0 := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "N3_TIPO"})
	nPos1 := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "N3_CCONTAB"})
	nPos2 := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "N3_CDEPREC"})
	nPos3 := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "N3_CCDEPR"})
	nPos4 := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "N3_CDESP"})
	nPos5 := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "N3_CCORREC"})
	nPos6 := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "N3_TXDEPR1"})
	nPos7 := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "N3_TXDEPR2"})
	nPos8 := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "N3_TXDEPR3"})
	nPos9 := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "N3_TXDEPR4"})
	nPosa := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "N3_TXDEPR5"})
	nPosB := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "N3_HISTOR"})
	nPosC := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "N3_VORIG1"})
	nPosD := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "N3_VORIG4"})
	nPosE := aScan(aHeader,{|aAux|alltrim(aAux[2]) == "N3_DINDEPR"})
	aarea1:=getarea()
	
	dbSelectArea("SNG")
	dbSetOrder(1)
	IF dbSeek(XFILIAL("SNG")+(&(__READVAR)))
		if alltrim(SNG->NG_CCONTAB)==""
			msgstop("Conta do ativo no cadastro do grupo não foi informada.")
			lRet:=.f.
		else
			acols[n,nPos1]:= SNG->NG_CCONTAB
			acols[n,nPos2]:= SNG->NG_CDEPREC
			acols[n,nPos3]:= SNG->NG_CCDEPR
			acols[n,nPos5]:= SNG->NG_CDESP
			acols[n,nPos4]:= SNG->NG_CCORREC
			acols[n,nPos6]:= SNG->NG_TXDEPR1
			acols[n,nPos7]:= SNG->NG_TXDEPR2
			acols[n,nPos8]:= SNG->NG_TXDEPR3
			acols[n,nPos9]:= SNG->NG_TXDEPR4
			acols[n,nPosa]:= SNG->NG_TXDEPR5
			acols[n,nPosB]:= IIF(acols[n,nPos0]=="01","AQUISICAO",IIF(acols[n,nPos0]=="03","ADIANT",SPACE(40)))
			acols[n,nPosD]:= acols[n,nPosC]/SM2->M2_MOEDA4
		endif
	ELSE
		msgstop("Problema no cadastro do grupo.")
		lRet:=.f.
	ENDIF
	restarea(aarea1)
	If ExistTrigger("N1_QUANTD") 
		RunTrigger(1,,,,"N1_QUANTD ")  
	Endif
	GETDREFRESH()
endif
return(lRet)

STATIC FUNCTION proxitem(_cbase)
//procura ultimo item do _cbase e soma1
Local aArea:=GetArea()
Local CQUERY := ""
Local cRet:=""
CQUERY := " SELECT  MAX(N1_ITEM) AS MAXR "
CQUERY += " FROM "+RETSQLNAME("SN1")+" SN1 "
CQUERY += " WHERE "
CQUERY += " D_E_L_E_T_<>'*' AND "
CQUERY += " N1_FILIAL ='"+XFILIAL("SN1")+"' AND "
CQUERY += " N1_CBASE = '"+ALLTRIM(_cbase)+"' "
TCQUERY CQUERY NEW ALIAS "QRY"
DBSELECTAREA("QRY")
DBGOTOP()
IF !QRY->(EOF())
	cRet:=SOMA1(alltrim(QRY->MAXR))
ENDIF
close
restarea(aArea)
return(cRet)

STATIC FUNCTION ProxBem(cGrupo)
//procura ultimo bem para o grupo e devolve o proximo
Local aArea:=GetArea()
Local CQUERY := ""
Local cRet:=""
CQUERY := " SELECT  MAX(N1_CBASE) AS MAXR  "
CQUERY += " FROM "+RETSQLNAME("SN1")+" SN1 "
CQUERY += " WHERE "
CQUERY += " D_E_L_E_T_<>'*' AND "
CQUERY += " N1_FILIAL ='"+XFILIAL("SN1")+"' AND "
CQUERY += " N1_GRUPO = '"+ALLTRIM(cGrupo)+"' AND "
CQUERY += " SUBSTRING(N1_CBASE,1,3) = '"+ALLTRIM(cGrupo)+"' AND LEN(RTRIM(N1_CBASE)) = 7 "
TCQUERY CQUERY NEW ALIAS "QRY"
DBSELECTAREA("QRY")
DBGOTOP()
IF !QRY->(EOF())
	cRet:=SOMA1(alltrim(QRY->MAXR))
ENDIF
close
restarea(aArea)
IF EMPTY(cRet)
	cRet:=ALLTRIM(cGrupo)+"0001"
endif
return(cRet)

STATIC FUNCTION TemBem(cGrupo)
//procura ultimo bem para o grupo e devolve o proximo
Local aArea:=GetArea()
Local CQUERY := ""
Local lRet:=.t.
CQUERY := " SELECT  N1_GRUPO  "
CQUERY += " FROM "+RETSQLNAME("SN1")+" SN1 "
CQUERY += " WHERE "
CQUERY += " D_E_L_E_T_<>'*' AND "
CQUERY += " N1_FILIAL ='"+XFILIAL("SN1")+"' AND "
CQUERY += " N1_GRUPO = '"+ALLTRIM(cGrupo)+"' "
TCQUERY CQUERY NEW ALIAS "QRY"
DBSELECTAREA("QRY")
DBGOTOP()
IF QRY->(EOF())
	lRet:=.f.
ENDIF
close
restarea(aArea)
return(lRet)

STATIC FUNCTION NovoANFE()
//se novo bem, proximo anfe 
Local aArea:=GetArea()
Local CQUERY := ""
Local cRet:=""
CQUERY := " SELECT  MAX(N1_ANFE) AS MAXR "
CQUERY += " FROM "+RETSQLNAME("SN1")+" SN1 "
CQUERY += " WHERE "
CQUERY += " D_E_L_E_T_<>'*' AND "
CQUERY += " N1_FILIAL ='"+XFILIAL("SN1")+"' AND "
//CQUERY += " N1_GRUPO = '"+ALLTRIM(cGrupo)+"' "
CQUERY += " LEN(RTRIM(N1_ANFE)) = 6 "
TCQUERY CQUERY NEW ALIAS "QRY"
DBSELECTAREA("QRY")
DBGOTOP()
IF !QRY->(EOF())
	cRet:=SOMA1(alltrim(QRY->MAXR))
ENDIF
close
restarea(aArea)
return(cRet)


STATIC FUNCTION ProxANFE(_cbase)
//se é proximo item de um bem existente pega mesmo anfe
Local aArea:=GetArea()
Local CQUERY := ""
Local cRet:=""
CQUERY := " SELECT  N1_ANFE  "
CQUERY += " FROM "+RETSQLNAME("SN1")+" SN1 "
CQUERY += " WHERE "
CQUERY += " D_E_L_E_T_<>'*' AND "
CQUERY += " N1_FILIAL ='"+XFILIAL("SN1")+"' AND "
CQUERY += " N1_CBASE = '"+ALLTRIM(_cBase)+"' "
TCQUERY CQUERY NEW ALIAS "QRY"
DBSELECTAREA("QRY")
DBGOTOP()
IF !QRY->(EOF())
	cRet:=alltrim(QRY->N1_ANFE)
ENDIF
close
restarea(aArea)
return(cRet)

STATIC FUNCTION BuscaSeq()
//busca sequencial da nota de entrada
Local aArea:=GetArea()
Local CQUERY := ""
Local cRet:=SPACE(8)
if !empty(M->N1_FORNEC) .AND. !empty(M->N1_NFISCAL)
	CQUERY := " SELECT  F1_SEQ  "
	CQUERY += " FROM "+RETSQLNAME("SF1")+" SF1 "
	CQUERY += " WHERE "
	CQUERY += " D_E_L_E_T_<>'*' AND "
	CQUERY += " F1_FILIAL ='"+XFILIAL("SF1")+"' AND "
	CQUERY += " F1_DOC = '"+M->N1_NFISCAL+"' AND "
	CQUERY += " F1_SERIE = '"+M->N1_NSERIE+"' AND "
	CQUERY += " F1_FORNECE = '"+M->N1_FORNEC+"' AND "
	CQUERY += " F1_LOJA = '"+M->N1_LOJA+"' "
	TCQUERY CQUERY NEW ALIAS "QRY"
	DBSELECTAREA("QRY")
	DBGOTOP()
	IF !QRY->(EOF())
		cRet:=alltrim(F1_SEQ)
	ENDIF
	close
ENDIF
restarea(aArea)
return(cRet)

Static Function ValidCB(_cBase)
Local lRet:=.t.
Local cRet:=""
if	!(SUBSTR(_cBase,1,3)==ALLTRIM(M->N1_GRUPO))
	lRet:=.f.
	msgstop("Bem não pertence ao grupo!")
else
	cRet:=proxitem(_cbase)
	if empty(cRet)
		lRet:=.f.
		msgstop("Bem informado não foi encontrado!")
	endif
endif
RETURN(lRet)

STATIC FUNCTION prefixo(_cbase)
//retorna o prefixo do _cbase informado
Local aArea:=GetArea()
Local CQUERY := ""
Local cRet:=""
CQUERY := " SELECT TOP 1 N1_PREFIXO "
CQUERY += " FROM "+RETSQLNAME("SN1")+" SN1 "
CQUERY += " WHERE "
CQUERY += " D_E_L_E_T_<>'*' AND "
CQUERY += " N1_FILIAL ='"+XFILIAL("SN1")+"' AND "
CQUERY += " N1_CBASE = '"+ALLTRIM(_cbase)+"' "
CQUERY += " ORDER BY N1_ITEM "
TCQUERY CQUERY NEW ALIAS "QRY"
DBSELECTAREA("QRY")
DBGOTOP()
IF !QRY->(EOF())
	cRet:=QRY->N1_PREFIXO
ENDIF
close
restarea(aArea)
return(cRet)
*/