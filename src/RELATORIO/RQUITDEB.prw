#INCLUDE "TOPCONN.CH"
#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³RQUITDEB  º Autor ³ EDSON              º Data ³  15/04/11   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ Relatório para geração da carta de quitação de débitos     º±±
±±º          ³ conforme a lei 12.007/09                                   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ Financeiro                                                 º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User Function RQUITDEB


//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Declaracao de Variaveis                                             ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

Local cDesc1  := "Emissão das cartas de quitação anual de débitos "
Local cDesc2  := "de acordo com a lei 12.007/09."
Local cDesc3  := "Cartas de quitação de débitos"
Local cPict   := ""
Local titulo  := "Relatório de Quitação de Débitos"
Local nLin    := 80


Local Cabec1  := "Cabec1"
Local Cabec2  := "Cabec2"
Local imprime := .T.
Local aOrd  := {}
Private lEnd  := .F.
Private lAbortPrint  := .F.
Private CbTxt      := ""
Private limite     := 132
Private tamanho    := "M"
Private nomeprog   := "RQUITDEB"
Private nTipo      := 15
Private aReturn    := {"Zebrado",1,"Administracao",1,2,1,"",1}
Private nLastKey   := 0
Private cPerg      := "RQUITDEB"
Private cbtxt      := Space(10)
Private cbcont     := 00
Private CONTFL     := 01
Private m_pag      := 01
Private wnrel      := "RQUITDEB"

Private cString := "SE1"

dbSelectArea("SE1")
dbSetOrder(1)

ValidPerg()
pergunte(cPerg,.T.)

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Monta a interface padrao com o usuario...                           ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

wnrel := SetPrint(cString,NomeProg,cPerg,@titulo,cDesc1,cDesc2,cDesc3,.F.,aOrd,.F.,Tamanho,,.F.)

If nLastKey == 27
	Return
Endif

SetDefault(aReturn,cString)

If nLastKey == 27
	Return
Endif

nTipo := If(aReturn[4]==1,15,18)
RptStatus({|| RunReport(Cabec1,Cabec2,Titulo,nLin) },Titulo)
Return


Static Function RunReport(Cabec1,Cabec2,Titulo,nLin)
Local cQuery

/*
cQuery:="SELECT E1_CLIENTE, E1_LOJA, A1_NOME, Sum(E1_SALDO) AS SomaDeE1_SALDO, A1_END, A1_COMPLEM, A1_BAIRRO, "
cQuery+="A1_EST, A1_CEP, A1_CGC, A1_MUN " 
cQuery+="FROM "+RetSqlname("SE1")+" SE1 INNER JOIN "+RetSqlname("SA1")+" SA1 "
cQuery+="ON (SE1.E1_CLIENTE = SA1.A1_COD) AND (SE1.E1_LOJA = SA1.A1_LOJA) "
cQuery+="WHERE SE1.D_E_L_E_T_<>'*' AND E1_VENCTO <= '"+AllTrim(Str(MV_PAR01))+"1231' AND E1_CLIENTE BETWEEN '"+MV_PAR02+"' AND '"+MV_PAR03+"' "
cQuery+="AND SA1.D_E_L_E_T_<>'*' AND A1_NOME BETWEEN '"+MV_PAR04+"' AND '"+MV_PAR05+"' "
cQuery+="GROUP BY E1_CLIENTE, E1_LOJA, A1_NOME, A1_END, A1_COMPLEM, A1_BAIRRO, A1_EST, "
cQuery+="A1_COD_MUN, A1_CEP, A1_CGC, A1_MUN "
cQuery+="HAVING Sum(E1_SALDO)=0 "
cQuery+="ORDER BY A1_NOME"
*/

cQuery:="SELECT SE1.E1_CLIENTE, SE1.E1_LOJA, SA1.A1_NOME, Sum(SE1.E1_SALDO) AS SomaDeE1_SALDO, SA1.A1_END, SA1.A1_COMPLEM, "
cQuery+="SA1.A1_BAIRRO, SA1.A1_EST, SA1.A1_CEP, SA1.A1_CGC, SA1.A1_MUN "
cQuery+="FROM ("+RetSqlname("SE1")+" SE1 INNER JOIN "+RetSqlname("SA1")+" SA1 "
cQuery+="ON (SE1.E1_CLIENTE = SA1.A1_COD) AND (SE1.E1_LOJA = SA1.A1_LOJA)) "
cQuery+="LEFT JOIN "
cQuery+="( "
cQuery+="SELECT SE1A.E1_CLIENTE, SE1A.E1_LOJA "
cQuery+="FROM "+RetSqlname("SE1")+" SE1A INNER JOIN "+RetSqlname("SA1")+" SA1A "
cQuery+="ON (SE1A.E1_CLIENTE = SA1A.A1_COD) AND (SE1A.E1_LOJA = SA1A.A1_LOJA) "
cQuery+="WHERE SE1A.D_E_L_E_T_<>'*' AND SE1A.E1_VENCTO < '"+AllTrim(Str(MV_PAR01))+"0101' AND SE1A.E1_CLIENTE BETWEEN '"+MV_PAR02+"' AND '"+MV_PAR03+"' "
cQuery+="AND SA1A.D_E_L_E_T_<>'*' AND SA1A.A1_NOME BETWEEN '"+MV_PAR04+"' AND '"+MV_PAR05+"' "
cQuery+="GROUP BY SE1A.E1_CLIENTE, SE1A.E1_LOJA "
cQuery+="HAVING Sum(SE1A.E1_SALDO)=0 "
cQuery+=") RESANT "
cQuery+="ON (SE1.E1_CLIENTE = RESANT.E1_CLIENTE) AND (SE1.E1_LOJA = RESANT.E1_LOJA) "
cQuery+="WHERE SE1.D_E_L_E_T_<>'*' AND SE1.E1_VENCTO like '"+AllTrim(Str(MV_PAR01))+"%' AND SE1.E1_CLIENTE BETWEEN '"+MV_PAR02+"' AND '"+MV_PAR03+"' "
cQuery+="AND SA1.D_E_L_E_T_<>'*' AND SA1.A1_NOME BETWEEN '"+MV_PAR04+"' AND '"+MV_PAR05+"' "
cQuery+="GROUP BY SE1.E1_CLIENTE, SE1.E1_LOJA, SA1.A1_NOME, SA1.A1_END, SA1.A1_COMPLEM, SA1.A1_BAIRRO, SA1.A1_EST, "
cQuery+="SA1.A1_COD_MUN, SA1.A1_CEP, SA1.A1_CGC, SA1.A1_MUN "
cQuery+="HAVING Sum(SE1.E1_SALDO)=0 "
cQuery+="ORDER BY SA1.A1_NOME "

TCQUERY cQuery NEW ALIAS "QRY"

dbselectarea("QRY")
DbGoTop()
COUNT TO _RECCOUNT
DbGoTop()
SetRegua(_RECCOUNT)

While !EOF()
	If lAbortPrint
		@nLin,00 PSAY "*** CANCELADO PELO OPERADOR ***"
		Exit
	Endif
    IncRegua()
//   	Cabec(Titulo,Cabac1,Cabec2,NomeProg,Tamanho,nTipo,"",.F.,"")
   	Cabec("","","","",Tamanho,nTipo,"",.F.,"")

	nLin := 8
	@nLin,010 PSAY "A"
	nLin:=nLin+2
	@nLin,010 PSAY QRY->A1_NOME
	nLin++
	@nLin,010 PSAY Alltrim(QRY->A1_END)+" "+QRY->A1_COMPLEM
	nLin++
	@nLin,010 PSAY QRY->A1_BAIRRO
	nLin:=nLin++
	nLin++
	@nLin,010 PSAY Alltrim(QRY->A1_MUN)+" - "+QRY->A1_EST
	nLin++
	@nLin,010 PSAY "CEP: "+Substr(QRY->A1_CEP,1,5)+"-"+Substr(QRY->A1_CEP,6,3)
	nLin:=nLin+13

	@nLin,000 PSAY Replicate("-",80)
	nLin:=nLin+7
	@nLin,010 PSAY "Remetente: "+SM0->M0_NOMECOM
	nLin++
	@nLin,010 PSAY SM0->M0_ENDENT
	nLin++
	@nLin,010 PSAY Alltrim(SM0->M0_CIDENT)+" - "+SM0->M0_ESTENT
	nLin++
	@nLin,010 PSAY "CEP: "+Substr(SM0->M0_CEPENT,1,5)+"-"+Substr(SM0->M0_CEPENT,6,3)
	nLin:=nLin+20

	@nLin,000 PSAY Replicate("-",80)
	nLin:=nLin+7
	@nLin,010 PSAY "Declaração de Quitação Anual de Débitos"
	nLin:=nLin+4
	@nLin,010 PSAY  "Prezado(a). " + QRY->A1_NOME
	nLin:=nLin+2
	@nLin,010 PSAY  "A "+Alltrim(SM0->M0_NOMECOM)+" declara,  para fins de atendimento às disposições da Lei 12.007,  de 29/07/2009,"
	nLin++
	@nLin,010 PSAY  "que o cliente portador do CPF/CNPJ "+Alltrim(QRY->A1_CGC)+" está quite quanto as parcelas com vencimento no ano de "+AllTrim(str(MV_PAR01))+"," 
	nLin++
	@nLin,010 PSAY  "referentes a locação de espaços."
	nLin++
	@nLin,010 PSAY  "Esta declaração substitui, para comprovação do cumprimento das obrigações do cliente, as quitações dos débitos" 
	nLin++
	@nLin,010 PSAY  "relativos aos meses de janeiro a dezembro do ano de "+AllTrim(str(MV_PAR01))+", bem como dos anos anteriores."
	nLin++
	@nLin,010 PSAY  "Caso qualquer das parcelas no período tenha sido paga por meio de cheque, sua quitação somente ocorrerá após a"
	nLin++
	@nLin,010 PSAY  "compensação do referido cheque. Em não havendo a efetiva compensação, ficará sem efeito a quitação mencionada"
	nLin++
	@nLin,010 PSAY  "no presente termo quanto àquelas parcelas, podendo a "+Alltrim(SM0->M0_NOMECOM)+" adotar as medidas cabíveis para"
	nLin++
	@nLin,010 PSAY  "cobrança do débito."
	nLin:=nLin+2
	@nLin,010 PSAY  "Belo Horizonte, "+Alltrim(Str(Day(dDatabase)))+" de "+MesExtenso(Month(dDatabase))+" de "+Alltrim(Str(Year(dDatabase)))+"."
	nLin:=nLin+2
	@nLin,010 PSAY  Alltrim(SM0->M0_NOMECOM)
	nLin:=nLin+2

	Dbskip()
EndDo
CLOSE

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Finaliza a execucao do relatorio...                                 ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

SET DEVICE TO SCREEN

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Se impressao em disco, chama o gerenciador de impressao...          ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

If aReturn[5]==1
	dbCommitAll()
	SET PRINTER TO
	OurSpool(wnrel)
Endif

MS_FLUSH()

Return

Static Function ValidPerg
cPerg := PADR(cPerg,10)   //valida somente com 10 caracteres o cperg
PutSx1(cPerg,"01","Ano ?           ","+Ano?          ","Year ?           ","mv_ch1","N", 4,0,0,"G","(mv_par01>=2009)","","","","mv_par01","","","","","","","","","","","","","","","","",{"Ano para geração das cartas","                                       ","                    "},{"                                       ","                                      "},{"                                       ","                                      "})
PutSx1(cPerg,"02","Do cliente ?","Do cliente ?","Do cliente ?","mv_ch2","C", 6,0,0,"G","","SA1","","","mv_par02","","","","","","","","","","","","","","","","",{"Informe o cliente inicial  ","                                      ","                    "},{"                                       ","                                      "},{"                                       ","                                      "})
PutSx1(cPerg,"03","Até o cliente ?","Até o cliente ?","Até o cliente ?","mv_ch3","C", 6,0,0,"G","","SA1","","","mv_par03","","","","","","","","","","","","","","","","",{"Informe o cliente final  ","                                      ","                    "},{"                                       ","                                      "},{"                                       ","                                      "})
PutSx1(cPerg,"04","Do nome ?","Do nome ?","Do nome ?","mv_ch4","C", 60,0,0,"G","","","","","mv_par04","","","","","","","","","","","","","","","","",{"Nome do cliente inicial  ","                                      ","                    "},{"                                       ","                                      "},{"                                       ","                                      "})
PutSx1(cPerg,"05","Até o nome ?","Até o nome ?","Até o nome ?","mv_ch5","C", 60,0,0,"G","","","","","mv_par05","","","","","","","","","","","","","","","","",{"Nome do cliente final  ","                                      ","                    "},{"                                       ","                                      "},{"                                       ","                                      "})

//PutSx1(cGrupo, cOrdem, cPergunt, cPerSpa, cPerEng, cVar, cTipo,nTamanho, nDecimal, nPresel, cGSC, cValid, cF3, cGrpSxg ,cPyme,cVar01, cDef01, cDefSpa1 , cDefEng1, cCnt01, cDef02, cDefSpa2,cDefEng2, cDef03, cDefSpa3, cDefEng3, cDef04, cDefSpa4, cDefEng4,cDef05, cDefSpa5, cDefEng5, aHelpPor, aHelpEng, aHelpSpa, cHelp)

Return