#INCLUDE "rwmake.ch"
#include "topconn.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณATUSZJ2   บ Autor ณ AP6 IDE            บ Data ณ  10/05/04   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDescricao ณ GRAVA LIBERACOES DE BORDERO NO SZJ                         บฑฑ
ฑฑบ          ณ                                                            บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ AP6 IDE                                                    บฑฑ
ฑฑศออออออออออฯออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
/*/

User Function ATUSZJ2
Processa({|| RunCont() },"Processando arquivo de libera็๕es...")
Return

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบFuno    ณ RUNCONT  บ Autor ณ AP5 IDE            บ Data ณ  10/05/04   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDescrio ณ Funcao auxiliar chamada pela PROCESSA.  A funcao PROCESSA  บฑฑ
ฑฑบ          ณ monta a janela com a regua de processamento.               บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ Programa principal                                         บฑฑ
ฑฑศออออออออออฯออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
/*/

Static Function RunCont

aAreaX:=GetArea()
//*****   APAGA LIBERACOES PARA BORDEROS CANCELADOS  *****
cQuery:="DELETE "
cQuery+="FROM "+RetSQLName("SZJ") +" "
cQuery+="WHERE ZJ_LIBERAD='N' OR D_E_L_E_T_='*' "
nret:=TCSQLExec(cQuery)
IF nret<> 0
	msgstop("Ocorreu um erro com codigo "+str(nret)+" na exclusao de registros de SZJ.")
	erro:=.t.
endif      
//*****   CRIA BORDEROS AINDA NAO CADASTRADOS   *****

cQuery:="SELECT EA_NUMBOR, EA_CART "
cQuery:=cQuery+" FROM "+RetSQLName("SEA") +" SEA LEFT JOIN "+RetSQLName("SZJ") +" SZJ ON (EA_CART = ZJ_TIPO AND EA_NUMBOR = ZJ_BORDERO) "
cQuery:=cQuery+" WHERE (((SEA.D_E_L_E_T_)<>'*') AND ((SZJ.D_E_L_E_T_)='*' Or (SZJ.D_E_L_E_T_) Is Null)) "
cQuery:=cQuery+" GROUP BY EA_NUMBOR, EA_CART   "                  
TCQUERY cQuery ALIAS QRY NEW   
dbSelectArea("QRY")
DbGoTop()            
COUNT TO _RECCOUNT
DbGoTop()            
ProcRegua(_RECCOUNT)
WHILE !(eof())                           
    IncProc()
	IF QRY->EA_CART="P"
		DbSelectArea("SE2")
//		DBOrderNickName("SE25")//E2_FILIAL+E2_NUMBOR+DTOS(E2_EMISSAO)    ALTERADO POR JORDANO 31-01-2012
		dbSetOrder(12)   // Jose Antonio
		DBSEEK(xfilial("SE2")+QRY->EA_NUMBOR,.T.)
		_T:=0
		while !eof() .and. QRY->EA_NUMBOR == SE2->E2_NUMBOR
			_T:=_T+IIF(SUBSTR(SE2->E2_TIPO,1,3)="-",SE2->E2_SALDO*-1,SE2->E2_SALDO) + SE2->E2_ACRESC - SE2->E2_DECRESC  
			DBSKIP()	    		
    	enddo                   
    	DBSELECTAREA("QRY")
	ELSE    
		DbSelectArea("SE1")
		DBSETORDER(12)//E1_FILIAL+E1_NUMBOR+DTOS(E1_EMISSAO)
		DBSEEK(XFILIAL("SE1")+QRY->EA_NUMBOR,.T.)
		_T:=0
		while !eof() .and. QRY->EA_NUMBOR == SE1->E1_NUMBOR
			_T:=_T+IIF(SUBSTR(SE1->E1_TIPO,1,3)="-",SE1->E1_SALDO*-1,SE1->E1_SALDO) + SE1->E1_ACRESC - SE1->E1_DECRESC
			DBSKIP()	    		
    	enddo                   
    	DBSELECTAREA("QRY")
	ENDIF
	dbSelectArea("SZJ")
	RecLock("SZJ",.T.)
	replace ZJ_FILIAL with xFilial("SZJ")
	replace ZJ_BORDERO with QRY->EA_NUMBOR
	replace ZJ_TIPO with QRY->EA_CART
	replace ZJ_LIBERAD with "N"
	replace ZJ_VALOR with _T
	MSUnlock()
	dbSelectArea("QRY")
	dbskip()
enddo
DBSELECTAREA("QRY")
DBCLOSEAREA("QRY")
RestArea(aAreaX)
Return
