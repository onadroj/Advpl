#INCLUDE "rwmake.ch"
#include "topconn.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³FA420CRI    º Autor ³ GATASSE          º Data ³  11/03/02   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ LIBERACAO DE PAGAMENTO ELETRONICO                          º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP5 IDE                                                    º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User Function FA420CRI


//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Declaracao de Variaveis                                             ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
LOCAL _return
LOCAL _aBorderos := {}
LOCAL _nItem := 0
LOCAL _nPos := 0
LOCAL _cQuery:=""

_B:=""
_return:=.t.

For _nItem:=val(MV_PAR01) to val(MV_PAR02)
  aAdd(_aBorderos,{StrZero(_nItem,6),"N"})
Next
 
_cQuery:="SELECT ZJ_BORDERO, ZJ_LIBERAD "
_cQuery+=" FROM "+RetSQLName("SZJ") 
_cQuery+=" WHERE ZJ_BORDERO>='"+MV_PAR01+"' And ZJ_BORDERO<='"+MV_PAR02+"' AND D_E_L_E_T_<>'*' AND ZJ_TIPO='P' "
//_cQuery+=ChangeQuery(cQuery)
TCQUERY _cQuery ALIAS QRY NEW   

DBSELECTAREA("QRY")
WHILE !EOF()
	_nPos:= aScan(_aBorderos,{|aItem| Alltrim(aItem[1])==QRY->ZJ_BORDERO})
	_aBorderos[_nPos][2]:=QRY->ZJ_LIBERAD 
	DBSKIP()
ENDDO
DBCLOSEAREA("QRY")            

For _nItem:=1 to Len(_aBorderos)
	If _aBorderos[_nItem][2]=="N"
		_B:=_B+_aBorderos[_nItem][1]+ " "     
	Endif
Next

if ALLTRIM(_B)<>""
	_return :=.F.
	msgstop("O(s) Bordero(s) "+_b+" ainda não estão liberados. Nenhum arquivo será gerado.!!!")
endif
Return(_return)