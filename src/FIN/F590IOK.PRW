#INCLUDE "rwmake.ch"
#include "topconn.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³F590IOK    º Autor ³ EDSON             º Data ³  31/05/07   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ PERMITE, OU NAO, A EXCLUSÃO DE TÍTULOS DE UM BORDERÔ       º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ FINANCEIRO                                                 º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User Function F590IOK

LOCAL _return
/*

_return:=.t.
cQuery:="SELECT ZJ_BORDERO, ZJ_LIBERAD "
cQuery:=cQuery+" FROM "+RetSQLName("SZJ") 
cQuery:=cQuery+" WHERE ZJ_BORDERO>='"+MV_PAR01+"' And ZJ_BORDERO<='"+MV_PAR02+"' AND D_E_L_E_T_<>'*' AND ZJ_LIBERAD='N' AND ZJ_TIPO='P' "
cQuery:=ChangeQuery(cQuery)
TCQUERY cQuery ALIAS QRY NEW   

DBSELECTAREA("QRY")
WHILE !EOF()
	_B:=_B+QRY->ZJ_BORDERO+ " "  
	DBSKIP()
ENDDO
DBCLOSEAREA("QRY")            

if ALLTRIM(_B)<>""
	_return :=.F.
	msgstop("O(s) Bordero(s) "+_b+" ainda não estão liberados. Nenhum arquivo será gerado.!!!")
endif

*/
//Msgstop("O ponto de entrada na inclusão de títulos no borderô está funcionando.!!!")

Return(_return)