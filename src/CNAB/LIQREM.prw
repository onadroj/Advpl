#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³LIQREM    º Autor ³ GATASSE            º Data ³  28/07/03   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ PREPARA DADOS PARA REMESSA DE LIQUIDO DE FOLHA DO BRADESCO º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP6 IDE                                                    º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User Function LIQREM
LOCAL RET
aConta:={"","00722308","00652369","00633259","00666661","00750182","00750689","00766402"}  //conta + digito de cada empresa
if SUBSTR(PARAMIXB,2,4)=="0513"
	aCodigo:={"","","44059","44058","","87282","87283","52011"} //codigo da empresa no banco agencia 0513
else
	if SUBSTR(PARAMIXB,2,4)=="3484"
		aCodigo:={"","","10134","10132","10136","","10135","10138"} //codigo da empresa no banco agencia 3484
	endif	
endif	
aNome:={"","MEGA","CONSTRUTORA SANT'ANNA","FRESAR TEC DE PAVIMENTOS ","TRANSPOR","MEGANOVA","VAGANOVA","ALMAQ SANT'ANNA"}//nome da empresa
IF SUBSTR(PARAMIXB,1,1)=="C" //CONTA+AGENCIA
	ret:=aConta[VAL(SM0->M0_CODIGO)]
ENDIF
IF SUBSTR(PARAMIXB,1,1)=="E" //CODIGO DA EMPRESA
	ret:=aCodigo[VAL(SM0->M0_CODIGO)]
ENDIF
IF SUBSTR(PARAMIXB,1,1)=="N" //NOME DA EMPRESA
	ret:=aNome[VAL(SM0->M0_CODIGO)]
ENDIF
Return(RET)
