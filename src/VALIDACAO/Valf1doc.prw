#INCLUDE "rwmake.ch"
#INCLUDE "Topconn.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณValf1doc  บGATASSEณ AP6 IDE            บ Data ณ  11/01/10   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDescricao ณ Valida็ใo se a nota jแ havia sido entrada com 6 dํgitos.   บฑฑ
ฑฑบ          ณ                                                            บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ AP6 IDE                                                    บฑฑ
ฑฑศออออออออออฯออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
/*/

User Function Valf1doc


//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณ Declaracao de Variaveis                                             ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
Local lRet:=.t.
Local cNum                          
Local aArea:=GetArea()
Local cMsg:=""        
Local cDoc:=""
Local cFrn:=""
Local cLj:=""

if __READVAR=="cNFiscal"
	cDoc:=&(__READVAR)	
else      
	cDoc:=cNFiscal
EndIf
if __READVAR=="cA100For"
	cFrn:=&(__READVAR)	
else      
	cFrn:=cA100For
EndIf
if __READVAR=="cLoja"
	cLj:=&(__READVAR)	
else      
	cLj:=cLoja
EndIf
if !empty(cDoc) .and. !empty(cFrn) .and. !empty(cLj) .and. !(cTipo$"DB")
	cNum:=strzero(val(cNFiscal),9)+"   "
	cMsg:=BscNota(cNum,cFrn,cLj)
	if !empty(cMsg)
		if MsgYesNo(cMsg+chr(10)+chr(13)+"Verifique ! "+chr(10)+chr(13)+"Deseja continuar ?")
			lRet:=.t.
		else
			lRet:=.f.
		EndIf                                                                        	
	EndIf
EndIf                 
restarea(aArea)
Return(lRet)

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณBscNota   บAutor  ณMicrosiga           บ Data ณ  01/11/10   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDesc.     ณ Verifica se a nota passada como parโmetro jแ existe na     บฑฑ
ฑฑบ          ณ base de dados e retorna a mensagem. Nใo testa a s้rie.     บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ AP                                                        บฑฑ
ฑฑศออออออออออฯออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
s*/

Static Function BscNota(cDoc,cFornece,cLoja)
Local cRet:=""
LOCAL AAREA:=GETAREA()
Local cQuery :="SELECT F1_DOC, F1_EMISSAO, F1_VALBRUT, F1_SERIE "
cQuery += " FROM "+RETSQLNAME("SF1")+" SF1 "
cQuery += " WHERE "
cQuery += "   F1_FILIAL='"+XFILIAL("SF1")+"' "
cQuery += "   AND SF1.D_E_L_E_T_ <>'*' "
cQuery += "   AND F1_DOC='"+cDoc+"' "	
cQuery += "   AND F1_FORNECE='"+cFornece+"' "	
cQuery += "   AND F1_LOJA='"+cLoja+"' "	
TCQUERY cQuery NEW ALIAS "QRY"   
TCSETFIELD("QRY","F1_EMISSAO","D",8,0)
dbSelectArea("QRY")
DBGOTOP()
if !EOF()
	cRet:="Nota com mesmo n๚mero, por้m com 9 dํgitos, lan็ada com a S้rie '"+QRY->F1_SERIE+"', emissใo em "+dtoc(QRY->F1_EMISSAO)+" e valor bruto de R$"+transform (QRY->F1_VALBRUT,"@E 999,999,999.99") +"!"
EndIf
CLOSE
RETURN(cRet)

