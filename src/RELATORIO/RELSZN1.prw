#INCLUDE "rwmake.ch"
#include "topconn.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณRELSZN1   บ Autor ณ GATASSE            บ Data ณ  18/03/04   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDescricao ณ RELATORIO ANALISE DE DESEMPENHO                            บฑฑ
ฑฑบ          ณ                                                            บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ AP6 IDE                                                    บฑฑ
ฑฑศออออออออออฯออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
/*/

User Function RELSZN1


//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณ Declaracao de Variaveis                                             ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

Local cDesc1         := "Este programa tem como objetivo imprimir relatorio "
Local cDesc2         := "de acordo com os parametros informados pelo usuario."
Local cDesc3         := "Relatorio de Analise de Desempenho"
Local cPict          := ""
Local titulo       := "Relatorio de Analise de Desempenho"
Local nLin         := 80

Local Cabec1       := "                                                                                               D I S P O N I B I L I D A D E   B E M"
Local Cabec2       := "Codigo Bem          Nome Bem                                   HP     HT     HM   HPAR    HAUX  DISP.APAR.DISP.FIS. IND.UTIL.IND.RND "
Local imprime      := .T.
Private aOrd       := {"Centro de Custo+Bem","Centro de Trabalho+Bem","Bem"}
Private lEnd         := .F.
Private lAbortPrint  := .F.
Private CbTxt        := ""
Private limite           := 132
Private tamanho          := "M"
Private nomeprog         := "RELSZN1" // Coloque aqui o nome do programa para impressao no cabecalho
Private nTipo            := 15
Private aReturn          := { "Zebrado", 1, "Administracao", 2, 2, 1, "", 1}
Private nLastKey        := 0
Private cPerg       := "RELSZN"
Private cbtxt      := Space(10)
Private cbcont     := 00
Private CONTFL     := 01
Private m_pag      := 01
Private wnrel      := "NOME" // Coloque aqui o nome do arquivo usado para impressao em disco

Private cString := "SZN"

dbSelectArea("SZN")
dbSetOrder(1)


ValidPerg()
pergunte(cPerg,.F.)

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณ Monta a interface padrao com o usuario...                           ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

wnrel := SetPrint(cString,NomeProg,cPerg,@titulo,cDesc1,cDesc2,cDesc3,.F.,aOrd,.F.,Tamanho,,.T.)

If nLastKey == 27
	Return
Endif

SetDefault(aReturn,cString)

If nLastKey == 27
	Return
Endif

nTipo := If(aReturn[4]==1,15,18)

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณ Processamento. RPTSTATUS monta janela com a regua de processamento. ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

RptStatus({|| RunReport(Cabec1,Cabec2,Titulo,nLin) },Titulo)
Return

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบFuno    ณRUNREPORT บ Autor ณ AP6 IDE            บ Data ณ  18/03/04   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDescrio ณ Funcao auxiliar chamada pela RPTSTATUS. A funcao RPTSTATUS บฑฑ
ฑฑบ          ณ monta a janela com a regua de processamento.               บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ Programa principal                                         บฑฑ
ฑฑศออออออออออฯออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
/*/

Static Function RunReport(Cabec1,Cabec2,Titulo,nLin)

Local nOrdem, cSeek
cFilterUser:=aReturn[7]
dbselectarea("SZN")
nOrdem := aReturn[8]

cQuery:="SELECT T9_CODFAMI, ZN_ITEM, ZN_HORAI, ZN_HORAF, ZN_THORA, ZN_CODOCOR, ZN_TIPO, ZN_NUM, ZN_DATA, ZN_CODBEM, ZN_NOMEBEM, ZN_CODFUNC, ZN_NOMEFUN, ZN_CONTADO, ZN_OBS, ZN_LOJA, ZN_CLIENTE, ZN_CCUSTO, ZN_NOMCLI, ZN_CENTRAB "
cQuery+="FROM "+RetSQLName("ST9") +" ST9 INNER JOIN "+RetSQLName("SZN") +" SZN ON ST9.T9_CODBEM = SZN.ZN_CODBEM "
cQuery+="WHERE ST9.D_E_L_E_T_<>'*' AND SZN.D_E_L_E_T_<>'*' AND ST9.T9_MTBAIXA ='' AND "
cQuery+="ST9.T9_CODFAMI >='"+ALLTRIM(mv_par09)+"'       And ST9.T9_CODFAMI <='"+ALLTRIM(mv_par10)+"' AND "
cQuery+="SZN.ZN_FILIAL   ='"+XFILIAL("SZN")+"' AND "
cQuery+="SZN.ZN_DATA    >='"+ALLTRIM(DTOS(mv_par01))+"' AND SZN.ZN_DATA    <='"+ALLTRIM(DTOS(mv_par02))+"' AND "
cQuery+="SZN.ZN_CODBEM  >='"+ALLTRIM(mv_par07)+"'       And SZN.ZN_CODBEM  <='"+ALLTRIM(mv_par08)+"' AND "
cQuery+="SZN.ZN_CCUSTO  >='"+ALLTRIM(mv_par03)+"'       And SZN.ZN_CCUSTO  <='"+ALLTRIM(mv_par04)+"' AND "
cQuery+="SZN.ZN_CENTRAB >='"+ALLTRIM(mv_par05)+"'       And SZN.ZN_CENTRAB <='"+ALLTRIM(mv_par06) +"' "

IF nOrdem==1
	cQuery+=" Order BY ZN_CCUSTO+ZN_CODBEM+ZN_DATA"
elseIF nOrdem==2
	cQuery+=" Order BY ZN_CENTRAB+ZN_CODBEM+ZN_DATA "
else
	cQuery+=" Order BY ZN_CODBEM"
endif
TCQUERY cQuery ALIAS QRY NEW
dbSelectArea("QRY")
DbGoTop()
COUNT TO _RECCOUNT
DbGoTop()
SetRegua(_RECCOUNT)


pVez:=.t.
cCampo:=iif (nOrdem==1,QRY->ZN_CCUSTO,iif (nOrdem==2,QRY->ZN_CENTRAB,QRY->ZN_CODBEM))
T1:=0
T2:=0
T3:=0
T4:=0
T5:=0
TT1:=0
if nOrdem==1
	cCampoAux:=QRY->ZN_CCUSTO+QRY->ZN_CODBEM
elseif nOrdem==2
	cCampoAux:=QRY->ZN_CENTRAB+QRY->ZN_CODBEM
else
	cCampoAux:=QRY->ZN_CODBEM
endif
cNomeAux:=QRY->ZN_NOMEBEM
//titulo=titulo+" por "+iif(nOrdem==1,"Centro de Custo","Centro de Trabalho")
titulo=titulo+"De "+dtoc(mv_par01)+" a "+dtoc(mv_par02)
While !EOF()
	cCampo2:=iif (nOrdem==1,"QRY->ZN_CCUSTO",iif (nOrdem==2,"QRY->ZN_CENTRAB","QRY->ZN_CODBEM"))
	If lAbortPrint
		@nLin,00 PSAY "*** CANCELADO PELO OPERADOR ***"
		Exit
	Endif
    IncRegua()
	
	
	//	IF	IIF(Empty(cFilterUser),.T.,&cFilterUser) .AND.;
	If nLin > 60
		Cabec(Titulo,Cabec1,Cabec2,NomeProg,Tamanho,nTipo)
		nLin := 9
		if nOrdem==1
			@nLin,00 PSAY "Centro de Custo: "+QRY->ZN_CCUSTO
			nLin ++
		elseif nOrdem==2
			@nLin,00 PSAY "Centro de Trabalho: "+QRY->ZN_CENTRAB
			nLin ++
//		else
//			@nLin,00 PSAY "Equipamento: "+QRY->ZN_CODBEM
		endif
	Endif
	
	if nOrdem==1
		cAux:=QRY->ZN_CCUSTO+QRY->ZN_CODBEM
	elseif nOrdem==2
		caux:=QRY->ZN_CENTRAB+QRY->ZN_CODBEM
	else
		cAux:=QRY->ZN_CODBEM
	endif
	
	if 	cCampoAux<>cAux
		@nLin,000 PSAY IIF( nOrdem=1,substr(cCampoAux,10,16),IIF( nOrdem=2,substr(cCampoAux,7,16),cCampoAux))
		@nLin,017 PSAY cNomeAux
		
		@nLin,059 PSAY strhora(t1)
		@nLin,066 PSAY strhora(t2)
		@nLin,073 PSAY strhora(t3)
		@nLin,080 PSAY strhora(t4-T3)
		@nLin,087 PSAY strhora(t5)
		@nLin,095 PSAY (T2/(T2+T3))*100 PICTURE "@E 9999.99"
		@nLin,102 PSAY "%"
		@nLin,105 PSAY ((T1-T3)/T1)*100 PICTURE "@E 9999.99"
		@nLin,112 PSAY "%"
		@nLin,115 PSAY (T2/(T1-T3))*100 PICTURE "@E 9999.99"
		@nLin,122 PSAY "%"
		@nLin,124 PSAY (T2/T1)*100 PICTURE "@E 9999.99"
		@nLin,131 PSAY "%"
		nLin := nLin + 1 // Avanca a linha de impressao
		T1:=0
		T2:=0
		T3:=0
		T4:=0
		T5:=0
	endif
	If cCampo<>&cCampo2 
		if nOrdem<>3
			ctexto:=iif (nOrdem==1,"Centro de Custo: "+QRY->ZN_CCUSTO,"Centro de Trabalho: "+QRY->ZN_CENTRAB)
			nLin ++
			@nLin,00 PSAY cTexto
			nLin ++
		endif
	Endif
	pVez:=.f.
	T1+=VALHORA(QRY->ZN_THORA)
	TT1+=VALHORA(QRY->ZN_THORA)
	IF QRY->ZN_TIPO $ "TP"
		T2+=VALHORA(QRY->ZN_THORA)
	ELSE
		T4+=VALHORA(QRY->ZN_THORA)
		IF QRY->ZN_TIPO=="M"
			T3+=VALHORA(QRY->ZN_THORA)
		ENDIF
	ENDIF
	IF ALLTRIM(MV_PAR11)<>""
		IF QRY->ZN_CODOCOR $ MV_PAR11
			T5+=VALHORA(SZN->ZN_THORA)
		ENDIF
	ENDIF
	cCampo:=iif (nOrdem==1,QRY->ZN_CCUSTO,iif (nOrdem==2,QRY->ZN_CENTRAB,QRY->ZN_CODBEM))
	if nOrdem==1
		cCampoAux:=QRY->ZN_CCUSTO+QRY->ZN_CODBEM
	elseif nOrdem==2
		cCampoAux:=QRY->ZN_CENTRAB+QRY->ZN_CODBEM
	else
		cCampoAux:=QRY->ZN_CODBEM
	endif
	cNomeAux:=QRY->ZN_NOMEBEM
	dbSkip() // Avanca o ponteiro do registro no arquivo
EndDo
@nLin,000 PSAY IIF( nOrdem=1,substr(cCampoAux,10,16),IIF( nOrdem=2,substr(cCampoAux,7,16),cCampoAux))
@nLin,017 PSAY cNomeAux

@nLin,059 PSAY strhora(t1)
@nLin,066 PSAY strhora(t2)
@nLin,073 PSAY strhora(t3)
@nLin,080 PSAY strhora(t4-T3)
@nLin,087 PSAY strhora(t5)
@nLin,095 PSAY (T2/(T2+T3))*100 PICTURE "@E 9999.99"
@nLin,102 PSAY "%"
@nLin,105 PSAY ((T1-T3)/T1)*100 PICTURE "@E 9999.99"
@nLin,112 PSAY "%"
@nLin,115 PSAY (T2/(T1-T3))*100 PICTURE "@E 9999.99"
@nLin,123 PSAY "%"
@nLin,124 PSAY (T2/T1)*100 PICTURE "@E 9999.99"
@nLin,131 PSAY "%"
nLin := nLin + 1 // Avanca a linha de impressao
//@nLin,059 PSAY strhora(tT1)
//nLin := nLin + 1 // Avanca a linha de impressao
IF nlin>64
	Cabec(Titulo,Cabec1,Cabec2,NomeProg,Tamanho,nTipo)
endif


nLin := 64
@nLin,0   PSAY "HORAS:HP=Programadas HPAR=Paradas HAUX=Oc.Col.Extra   INDICES:Disp.Mec.Aparente=(HT/(HT+HM))*100  Ind.Utilizacao = (HT/(HP-HM))*100"
@nLin+1,0 PSAY "      HT=Trabalhadas HM=Manutencao                            Disp.Fisica      =((HP-HM)/HP)*100  Ind.Rendimento = (HT/HP)*100"
//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณ Finaliza a execucao do relatorio...                                 ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
CLOSE
SET DEVICE TO SCREEN

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณ Se impressao em disco, chama o gerenciador de impressao...          ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

If aReturn[5]==1
	dbCommitAll()
	SET PRINTER TO
	OurSpool(wnrel)
Endif

MS_FLUSH()

Return

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบFuno    ณVALIDPERG บ Autor ณ AP5 IDE            บ Data ณ  17/12/02   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDescrio ณ Verifica a existencia das perguntas criando-as caso seja   บฑฑ
ฑฑบ          ณ necessario (caso nao existam).                             บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ Programa principal                                         บฑฑ
ฑฑศออออออออออฯออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
/*/

Static Function ValidPerg

Local _sAlias := Alias()
Local aRegs := {}
Local i,j

dbSelectArea("SX1")
dbSetOrder(1)
cPerg := PADR(cPerg,10)

// Grupo/Ordem/Pergunta/Variavel/Tipo/Tamanho/Decimal/Presel/GSC/Valid/Var01/Def01/Cnt01/Var02/Def02/Cnt02/Var03/Def03/Cnt03/Var04/Def04/Cnt04/Var05/Def05/Cnt05
aAdd(aRegs,{cPerg,"01","Da Data              ?","","","mv_ch1","D",8,0,0,"G","","mv_par01","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"02","Ate a Data           ?","","","mv_ch2","D",8,0,0,"G","","mv_par02","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"03","Do Centro de Custo   ?","","","mv_ch3","C",9,0,0,"G","","mv_par03","","","","","","","","","","","","","","","","","","","","","","","","","CTT","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"04","Ate o Centro de Custo?","","","mv_ch4","C",9,0,0,"G","","mv_par04","","","","","","","","","","","","","","","","","","","","","","","","","CTT","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"05","Do Centro Trabalho   ?","","","mv_ch5","C",6,0,0,"G","","mv_par05","","","","","","","","","","","","","","","","","","","","","","","","","SHB","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"06","Ate Centro Trabalho  ?","","","mv_ch6","C",6,0,0,"G","","mv_par06","","","","","","","","","","","","","","","","","","","","","","","","","SHB","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"07","Do Bem               ?","","","mv_ch7","C",16,0,0,"G","","mv_par07","","","","","","","","","","","","","","","","","","","","","","","","","ST9","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"08","Ate o Bem            ?","","","mv_ch8","C",16,0,0,"G","","mv_par08","","","","","","","","","","","","","","","","","","","","","","","","","ST9","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"09","Da Familia           ?","","","mv_ch9","C",6,0,0,"G","","mv_par09","","","","","","","","","","","","","","","","","","","","","","","","","ST6","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"10","Ate Familia          ?","","","mv_cha","C",6,0,0,"G","","mv_par10","","","","","","","","","","","","","","","","","","","","","","","","","ST6","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"11","Ocor.P/Coluna Extra  ?","","","mv_chb","C",80,0,0,"G","","mv_par11","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
For i:=1 to Len(aRegs)
	If !dbSeek(cPerg+aRegs[i,2])
		RecLock("SX1",.T.)
		For j:=1 to FCount()
			If j <= Len(aRegs[i])
				FieldPut(j,aRegs[i,j])
			Endif
		Next
		MsUnlock()
	Endif
Next
dbSelectArea(_sAlias)
Return

STATIC function VALHORA(N)
LOCAL RET
nHora:=val(substr(N,1,2))
nMin:=val(substr(N,4,2))
ret:=nHora*60+nMin
RETURN(RET)

STATIC function STRHORA(N)
LOCAL RET
cHora:=TRANSFORM(int(n/60),"@E 9999")
cMin:=strzero((n%60),2)
ret:=chora+":"+cMin
RETURN(RET)
