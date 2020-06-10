#INCLUDE "rwmake.ch"
#INCLUDE "TOPCONN.CH"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³AF010TOK  º Autor ³ GATASSE            º Data ³  03/09/10   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ VALIDA CADASTRO DO ATIVO                                   º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP6 IDE                                                    º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User Function AF010TOK

//A variavel TipoBem_usr é publica e vem do rdmake ATFANFE
Local cBase
Local cItem
Local cNewBase
Local cNewItem
Local lRet:=.t.

//IF INCLUI    .OR. (FUNNAME()="ATFA240" .AND. LCLASSIFICA)
IF INCLUI .OR. FUNNAME()="ATFA240"
	if TipoBem_usr==1 //novo bem 
		cBase:=alltrim(M->N1_CBASE)
		cNewBase:=ProxBem(M->N1_GRUPO)
		if cNewBase#cBase
			M->N1_CBASE:=cNewBase
			M->N1_ANFE := NovoANFE()
			Msgstop("O código do bem foi alterado para : "+cNewBase+"." + chr(13) + ;
			        "A ANFE foi alterada para: "+M->N1_ANFE+".")
		endif
	elseif TipoBem_usr==2 //novo item
		cItem:=M->N1_ITEM
	    cNewItem:=ProxItem(M->N1_CBASE)
		if cNewItem#cItem
			M->N1_ITEM:=cNewItem
			Msgstop("O código do item foi alterado para : "+cNewItem+".")
		endif
	else
		cNewBase:=ProxBem(M->N1_GRUPO)
		if !empty(cNewBase)
			M->N1_CBASE:=cNewBase
			M->N1_ANFE := NovoANFE()
			Msgstop("O código do bem foi alterado para : "+cNewBase+"." + chr(13) + ;
			        "A ANFE foi alterada para: "+M->N1_ANFE+".")
		endif
	endif
ENDIF
TipoBem_usr := nil
GETDREFRESH()
Return(lRet)

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
CQUERY += " SUBSTRING(N1_CBASE,1,3) = '"+ALLTRIM(cGrupo)+"' AND LEN(RTRIM(N1_CBASE)) = 7"
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

STATIC FUNCTION ProxANFE(cbase)
//procura ultimo bem para o grupo e devolve o proximo
Local aArea:=GetArea()
Local CQUERY := ""
Local cRet:=""
CQUERY := " SELECT  N1_ANFE  "
CQUERY += " FROM "+RETSQLNAME("SN1")+" SN1 "
CQUERY += " WHERE "
CQUERY += " D_E_L_E_T_<>'*' AND "
CQUERY += " N1_FILIAL ='"+XFILIAL("SN1")+"' AND "
CQUERY += " N1_CBASE = '"+ALLTRIM(cBase)+"' "
TCQUERY CQUERY NEW ALIAS "QRY"
DBSELECTAREA("QRY")
DBGOTOP()
IF !QRY->(EOF())
	cRet:=alltrim(QRY->N1_ANFE)
ENDIF
close
restarea(aArea)
return(cRet)


STATIC FUNCTION proxitem(cbase)
//procura ultimo item do cbase e soma1
Local aArea:=GetArea()
Local CQUERY := ""
Local cRet:=""
CQUERY := " SELECT  MAX(N1_ITEM) AS MAXR "
CQUERY += " FROM "+RETSQLNAME("SN1")+" SN1 "
CQUERY += " WHERE "
CQUERY += " D_E_L_E_T_<>'*' AND "
CQUERY += " N1_FILIAL ='"+XFILIAL("SN1")+"' AND "
CQUERY += " N1_CBASE = '"+ALLTRIM(cbase)+"' "
TCQUERY CQUERY NEW ALIAS "QRY"
DBSELECTAREA("QRY")
DBGOTOP()
IF !QRY->(EOF())
	cRet:=SOMA1(alltrim(QRY->MAXR))
ENDIF
close
restarea(aArea)
return(cRet)

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
