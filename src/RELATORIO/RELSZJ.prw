#INCLUDE "TOPCONN.CH"
#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³RELSZJ    º Autor ³ EDSON              º Data ³  22/10/11   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ Relatório de títulos liberados por borderô                 º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ Financeiro                                                 º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User Function RELSZJ


//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Declaracao de Variaveis                                             ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

Local cDesc1  := "Emissão da relação de títulos por"
Local cDesc2  := "borderôs de pagamentos liberados."
Local cDesc3  := "Títulos por borderôs liberados."
Local cPict   := ""
Local titulo  := "Títulos por borderôs liberados."
Local nLin    := 80


Local Cabec1  := "BORDERO BCO AG.   CONTA      DT.LIB.    VALOR             LIBERADOR"
Local Cabec2  := "          TIT.      PARC. FORN./LOJA  NOME                 EMISSAO  VENCTO   VALOR             HISTORICO                                     BENEF.                         BCO AG      CONTA"
Local imprime := .T.
Local aOrd  := {}
Private lEnd  := .F.
Private lAbortPrint  := .F.
Private CbTxt      := ""
Private limite     := 132
Private tamanho    := "G"
Private nomeprog   := "RELSZJ"
Private nTipo      := 18
Private aReturn    := {"Zebrado",1,"Administracao",2,2,1,"",1}
Private nLastKey   := 0
Private cPerg      := "RELSZJ"
Private cbtxt      := Space(10)
Private cbcont     := 00
Private CONTFL     := 01
Private m_pag      := 01
Private wnrel      := "RELSZJ"

Private cString := "SZJ"

dbSelectArea("SZJ")
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
Local _cBordAtu := ""
Local cQuery

cQuery:="SELECT ZJ_DATA, ZJ_BORDERO, ZJ_VALOR, ZJ_LIBPOR, EA_PORTADO, EA_AGEDEP, EA_NUMCON, EA_BENCNAB , EA_BCOCNAB, EA_AGECNAB, EA_CTACNAB, EA_MODELO, "
cQuery+="E2_NUM, E2_PARCELA, E2_FORNECE, E2_LOJA, E2_NOMFOR, E2_EMISSAO, E2_VENCTO, "
cQuery+="'VALOR'=(CASE WHEN E2_SALDO>0 Then E2_SALDO+E2_ACRESC-E2_DECRESC Else E2_VALLIQ End), E2_HIST "
cQuery+="FROM ("+RetSqlname("SZJ")+" SZJ INNER JOIN "+RetSqlname("SEA")+" SEA ON ZJ_BORDERO = EA_NUMBOR) "
cQuery+="INNER JOIN "+RetSqlname("SE2")+" SE2 ON (EA_LOJA = E2_LOJA AND EA_FORNECE = E2_FORNECE AND EA_TIPO = E2_TIPO "
cQuery+="AND EA_PARCELA = E2_PARCELA AND EA_NUM = E2_NUM AND EA_PREFIXO = E2_PREFIXO AND EA_NUMBOR = E2_NUMBOR)"
cQuery+="WHERE ZJ_DATA Between '"+Dtos(MV_PAR01)+"' And '"+Dtos(MV_PAR02)+"' AND SZJ.D_E_L_E_T_<>'*' AND ZJ_TIPO='P' AND ZJ_LIBERAD='S' "
cQuery+="AND ZJ_BORDERO Between '"+MV_PAR03+"' And '"+MV_PAR04+"' "
cQuery+="AND EA_FORNECE Between '"+MV_PAR05+"' And '"+MV_PAR06+"' AND SEA.D_E_L_E_T_<>'*' "
cQuery+="AND SE2.D_E_L_E_T_<>'*' "
cQuery+="ORDER BY ZJ_DATA, ZJ_BORDERO, E2_NOMFOR, E2_NUM"

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

	If nLin > 60
		Cabec(Titulo,Cabec1,Cabec2,NomeProg,Tamanho,nTipo)
		nLin := 9
	Endif

	If QRY->ZJ_BORDERO <> _cBordAtu
		@nLin,000 PSAY Replicate("-",80)
		nLin++
		@nLin,000 PSAY QRY->ZJ_BORDERO
		@nLin,008 PSAY QRY->EA_PORTADO
		@nLin,012 PSAY QRY->EA_AGEDEP
		@nLin,018 PSAY QRY->EA_NUMCON
		@nLin,029 PSAY Dtoc(Stod(QRY->ZJ_DATA))
		@nLin,040 PSAY QRY->ZJ_VALOR Picture "99,999,999,999.99"
		@nLin,058 PSAY QRY->ZJ_LIBPOR
        @nLin,089 PSAY "Modelo: "+QRY->EA_MODELO+"-"+RetField("SX5",1,xFilial("SX5")+"58"+QRY->EA_MODELO,"X5_DESCRI")
		_cBordAtu := QRY->ZJ_BORDERO
		nLin++
		If nLin > 60
			Cabec(Titulo,Cabec1,Cabec2,NomeProg,Tamanho,nTipo)
			nLin := 9
		Endif
    Endif
	@nLin,010 PSAY QRY->E2_NUM
	@nLin,020 PSAY QRY->E2_PARCELA
	@nLin,026 PSAY QRY->E2_FORNECE+" / "+QRY->E2_LOJA
	@nLin,038 PSAY QRY->E2_NOMFOR
	@nLin,059 PSAY Dtoc(Stod(QRY->E2_EMISSAO))
	@nLin,068 PSAY Dtoc(Stod(QRY->E2_VENCTO))
	@nLin,077 PSAY QRY->VALOR  Picture "99,999,999,999.99"
	@nLin,095 PSAY QRY->E2_HIST
	@nLin,141 PSAY QRY->EA_BENCNAB
	@nLin,172 PSAY QRY->EA_BCOCNAB
	@nLin,176 PSAY QRY->EA_AGECNAB
	@nLin,183 PSAY QRY->EA_CTACNAB
	nLin++

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

PutSx1(cPerg,"01","Da Data            ?","Da Data            ?","Da Data            ?","mv_ch1","D",8,0,0,"G","","","","","mv_par01","","","","","","","","","","","","","","","","",{},{},{})
PutSx1(cPerg,"02","Até a Data         ?","Até a Data         ?","Até a Data         ?","mv_ch2","D",8,0,0,"G","","","","","mv_par02","","","","","","","","","","","","","","","","",{},{},{})
PutSx1(cPerg,"03","Do bordero         ?","Do bordero         ?","Do bordero         ?","mv_ch3","C",6,0,0,"G","","","","","mv_par03","","","","","","","","","","","","","","","","",{},{},{})
PutSx1(cPerg,"04","Até bordero        ?","Até bordero        ?","Até bordero        ?","mv_ch4","C",6,0,0,"G","","","","","mv_par04","","","","","","","","","","","","","","","","",{},{},{})
PutSx1(cPerg,"05","Do fornecedor      ?","Do fornecedor      ?","Do fornecedor      ?","mv_ch5","C",6,0,0,"G","","SA2","","","mv_par05","","","","","","","","","","","","","","","","",{"Informe o codigo do      ","fornecedor inicial. Tecle F3 para     ","consulta ao cadastro"},{"                                       ","                                      "},{"                                       ","                                      "})
PutSx1(cPerg,"06","Até fornecedor     ?","Até fornecedor     ?","Até fornecedor     ?","mv_ch6","C",6,0,0,"G","","SA2","","","mv_par06","","","","","","","","","","","","","","","","",{"Informe o codigo do      ","fornecedor final. Tecle F3 para       ","consulta ao cadastro"},{"                                       ","                                      "},{"                                       ","                                      "})

Return