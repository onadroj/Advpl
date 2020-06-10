#INCLUDE "TOPCONN.CH"
#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณUMTR001   บ Autor ณ GATASSE            บ Data ณ  08/11/04   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDescricao ณ Relatorio de Consumo de Combustiveis/Lubrificantes         บฑฑ
ฑฑบ          ณ                                                            บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ AP6 IDE                                                    บฑฑ
ฑฑศออออออออออฯออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
/*/

User Function UMTR001


//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณ Declaracao de Variaveis                                             ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

Local cDesc1         := "Este programa tem como objetivo imprimir relatorio "
Local cDesc2         := "de acordo com os parametros informados pelo usuario."
Local cDesc3         := "Relatorio de Consumo de Combustiveis/Lubrificantes"
Local cPict          := ""
Local titulo       := "Rel.de Consumo Combustiveis/Lubrificantes"
Local nLin         := 80

Local Cabec1       := "BEM"
Local Cabec2       := "          DESCRICAO                                                           QUANTIDADE UM    HRS TRAB.    MEDIA       CUSTO"
Local imprime      := .T.
Local uRde         := ""
Private uData      := ""
Private aOrd             := {"por equipamento","por famํlia","por modelo"}
Private lEnd         := .F.
Private lAbortPrint  := .F.
Private CbTxt        := ""
Private limite           := 132
Private tamanho          := "M"
Private nomeprog         := "UMTR001"
Private nTipo            := 18
Private aReturn          := { "Zebrado", 1, "Administracao", 2, 2, 1, "", 1}
Private nLastKey        := 0
Private cPerg       := "UMT001"
Private cbtxt      := Space(10)
Private cbcont     := 00
Private CONTFL     := 01
Private m_pag      := 01
Private wnrel      := "UMTR001"
Private cCodigo
Private cDesc
Private nQuant
Private cUM
Private nHoras
Private nMedia
Private nCusto
Private H := 0
Private cString := "SZN"

dbSelectArea("SZN")
dbSetOrder(1)


ValidPerg()
pergunte(cPerg,.F.)

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณ Monta a interface padrao com o usuario...                           ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

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

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบFuno    ณRUNREPORT บ Autor ณ AP6 IDE            บ Data ณ  08/11/04   บฑฑ
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
Local nOrdem := aReturn[8]
Local cTroca :="AND    TL_DESTINO <> 'T'  "

//Query para totaliza็ใo de horas trabalhadas por famํlia ou modelo
if nOrdem==2
	cQuery:="SELECT TJ_CODBEM, T9_CODFAMI, TJ_CCUSTO, TJ_CENTRAB, TL_CODIGO "
	cQuery+="FROM ("+RETSQLNAME("STJ")+" STJ INNER JOIN "+RETSQLNAME("STL")+" STL ON (TJ_FILIAL = TL_FILIAL) AND (TJ_PLANO = TL_PLANO) AND (TJ_ORDEM = TL_ORDEM)) INNER JOIN "+RETSQLNAME("ST9")+" ST9 ON TJ_CODBEM = T9_CODBEM "
	cQuery+="WHERE  TJ_DTORIGI >= '"+DTOS(MV_PAR01)+"' AND TJ_DTORIGI <= '"+DTOS(MV_PAR02)+"' AND T9_FILIAL ='"+XFILIAL("ST9")+"' AND TJ_SITUACA<>'C' "
    cQuery+="AND    ST9.D_E_L_E_T_<>'*' AND STJ.D_E_L_E_T_<>'*' AND STL.D_E_L_E_T_<>'*' AND (ST9.T9_MTBAIXA ='' OR ST9.T9_MTBAIXA ='0003') "
	cQuery+="AND    TJ_CCUSTO  >= '"+MV_PAR03+"' AND TJ_CCUSTO <= '"+MV_PAR04+"' "
	cQuery+="AND    TJ_CENTRAB >= '"+MV_PAR05+"' AND TJ_CENTRAB <= '"+MV_PAR06+"' "
	cQuery+="AND    TJ_CODBEM  >= '"+MV_PAR07+"' AND TJ_CODBEM <= '"+MV_PAR08+"' "
	cQuery+="AND    T9_CODFAMI >= '"+MV_PAR09+"' AND T9_CODFAMI <= '"+MV_PAR10+"' "
	cQuery+="AND    TL_DTDIGIT >= '"+DTOS(MV_PAR13)+"' AND TL_DTDIGIT <= '"+DTOS(MV_PAR14)+"'  "
    if MV_PAR15==2
      cQuery+=cTroca
    endif
	cQuery+="GROUP BY T9_CODFAMI, TJ_CODBEM,TJ_CCUSTO, TJ_CENTRAB, TL_CODIGO "
	cQuery+="ORDER BY T9_CODFAMI+TJ_CODBEM"
    TCQUERY cQuery NEW ALIAS "QRY1"
elseif nOrdem==3
	cQuery:="SELECT TJ_CODBEM, T9_MODELO, TJ_CCUSTO, TJ_CENTRAB, TL_CODIGO "
	cQuery+="FROM ("+RETSQLNAME("STJ")+" STJ INNER JOIN "+RETSQLNAME("STL")+" STL ON (TJ_FILIAL = TL_FILIAL) AND (TJ_PLANO = TL_PLANO) AND (TJ_ORDEM = TL_ORDEM)) INNER JOIN "+RETSQLNAME("ST9")+" ST9 ON TJ_CODBEM = T9_CODBEM "
	cQuery+="WHERE  TJ_DTORIGI >= '"+DTOS(MV_PAR01)+"' AND TJ_DTORIGI <= '"+DTOS(MV_PAR02)+"' AND T9_FILIAL ='"+XFILIAL("ST9")+"' AND TJ_SITUACA<>'C' "
    cQuery+="AND    ST9.D_E_L_E_T_<>'*' AND STJ.D_E_L_E_T_<>'*' AND STL.D_E_L_E_T_<>'*' AND (ST9.T9_MTBAIXA ='' OR ST9.T9_MTBAIXA ='0003') "
	cQuery+="AND    TJ_CCUSTO  >= '"+MV_PAR03+"' AND TJ_CCUSTO <= '"+MV_PAR04+"' "
	cQuery+="AND    TJ_CENTRAB >= '"+MV_PAR05+"' AND TJ_CENTRAB <= '"+MV_PAR06+"' "
	cQuery+="AND    TJ_CODBEM  >= '"+MV_PAR07+"' AND TJ_CODBEM <= '"+MV_PAR08+"' "
	cQuery+="AND    T9_CODFAMI >= '"+MV_PAR09+"' AND T9_CODFAMI <= '"+MV_PAR10+"' "
	cQuery+="AND    TL_DTDIGIT >= '"+DTOS(MV_PAR13)+"' AND TL_DTDIGIT <= '"+DTOS(MV_PAR14)+"'  "
    if MV_PAR15==2
      cQuery+=cTroca
    endif
	cQuery+="GROUP BY T9_MODELO, TJ_CODBEM,TJ_CCUSTO, TJ_CENTRAB, TL_CODIGO "
	cQuery+="ORDER BY T9_MODELO+TJ_CODBEM"
    TCQUERY cQuery NEW ALIAS "QRY1"
endif	


//Gera็ใo da query para trabalho

cQuery:="SELECT TJ_CODBEM, T9_CODFAMI, TJ_CCUSTO, TJ_CENTRAB, TL_CODIGO, B1_GRUPO, Sum(TL_QUANTID) AS QUANTID, B1_UM, Sum(TL_CUSTO) AS CUSTO, B1_DESC, T9_NOME "
cQuery+="FROM (("+RETSQLNAME("STJ")+" STJ INNER JOIN "+RETSQLNAME("STL")+" STL ON (TJ_FILIAL = TL_FILIAL) AND (TJ_PLANO = TL_PLANO) AND (TJ_ORDEM = TL_ORDEM)) INNER JOIN "+RETSQLNAME("SB1")+" SB1 ON TL_CODIGO = B1_COD) INNER JOIN "+RETSQLNAME("ST9")+" ST9 ON TJ_CODBEM = T9_CODBEM "
cQuery+="WHERE  TJ_DTORIGI >= '"+DTOS(MV_PAR01)+"' AND TJ_DTORIGI <= '"+DTOS(MV_PAR02)+"' AND T9_FILIAL ='"+XFILIAL("ST9")+"' AND TJ_SITUACA<>'C' "
cQuery+="AND    STJ.D_E_L_E_T_<>'*' AND ST9.D_E_L_E_T_<>'*' AND STL.D_E_L_E_T_<>'*' AND SB1.D_E_L_E_T_<>'*' AND (ST9.T9_MTBAIXA ='' OR ST9.T9_MTBAIXA ='0003') "
cQuery+="AND    TJ_CCUSTO  >= '"+MV_PAR03+"' AND TJ_CCUSTO <= '"+MV_PAR04+"' "
cQuery+="AND    TJ_CENTRAB >= '"+MV_PAR05+"' AND TJ_CENTRAB <= '"+MV_PAR06+"' "
cQuery+="AND    TJ_CODBEM  >= '"+MV_PAR07+"' AND TJ_CODBEM <= '"+MV_PAR08+"' "
cQuery+="AND    T9_CODFAMI >= '"+MV_PAR09+"' AND T9_CODFAMI <= '"+MV_PAR10+"' "
cQuery+="AND    TL_DTDIGIT >= '"+DTOS(MV_PAR13)+"' AND TL_DTDIGIT <= '"+DTOS(MV_PAR14)+"'  "
if MV_PAR15==2
   cQuery+=cTroca
endif
If nOrdem ==1
	cQuery+="GROUP BY TJ_CODBEM, T9_CODFAMI, TJ_CCUSTO, TJ_CENTRAB, TL_CODIGO, B1_GRUPO, B1_UM, B1_DESC,T9_NOME "
	cQuery+="ORDER BY TJ_CODBEM+TL_CODIGO"
	Cabec1       := "BEM"
ELSEIF nOrdem==2
	cQuery+="GROUP BY T9_CODFAMI, TJ_CODBEM,TJ_CCUSTO, TJ_CENTRAB, TL_CODIGO, B1_GRUPO, B1_UM, B1_DESC,T9_NOME "
	cQuery+="ORDER BY T9_CODFAMI+TL_CODIGO+TJ_CODBEM"
	Cabec1       := ""
else
	cQuery:="SELECT TJ_CODBEM, T9_MODELO, TJ_CCUSTO, TJ_CENTRAB, TL_CODIGO, B1_GRUPO, Sum(TL_QUANTID) AS QUANTID, B1_UM, Sum(TL_CUSTO) AS CUSTO, B1_DESC, T9_NOME "
	cQuery+="FROM (("+RETSQLNAME("STJ")+" STJ INNER JOIN "+RETSQLNAME("STL")+" STL ON (TJ_FILIAL = TL_FILIAL) AND (TJ_PLANO = TL_PLANO) AND (TJ_ORDEM = TL_ORDEM)) INNER JOIN "+RETSQLNAME("SB1")+" SB1 ON TL_CODIGO = B1_COD) INNER JOIN "+RETSQLNAME("ST9")+" ST9 ON TJ_CODBEM = T9_CODBEM "
	cQuery+="WHERE  TJ_DTORIGI >= '"+DTOS(MV_PAR01)+"' AND TJ_DTORIGI <= '"+DTOS(MV_PAR02)+"' AND T9_FILIAL ='"+XFILIAL("ST9")+"' AND TJ_SITUACA<>'C' "
    cQuery+="AND    STJ.D_E_L_E_T_<>'*' AND ST9.D_E_L_E_T_<>'*' AND STL.D_E_L_E_T_<>'*' AND SB1.D_E_L_E_T_<>'*' AND (ST9.T9_MTBAIXA ='' OR ST9.T9_MTBAIXA ='0003') "
	cQuery+="AND    TJ_CCUSTO  >= '"+MV_PAR03+"' AND TJ_CCUSTO <= '"+MV_PAR04+"' "
	cQuery+="AND    TJ_CENTRAB >= '"+MV_PAR05+"' AND TJ_CENTRAB <= '"+MV_PAR06+"' "
	cQuery+="AND    TJ_CODBEM  >= '"+MV_PAR07+"' AND TJ_CODBEM <= '"+MV_PAR08+"' "
	cQuery+="AND    T9_CODFAMI >= '"+MV_PAR09+"' AND T9_CODFAMI <= '"+MV_PAR10+"' "
	cQuery+="AND    TL_DTDIGIT >= '"+DTOS(MV_PAR13)+"' AND TL_DTDIGIT <= '"+DTOS(MV_PAR14)+"'  "
    if MV_PAR15==2
      cQuery+=cTroca
    endif
	cQuery+="GROUP BY T9_MODELO, TJ_CODBEM,TJ_CCUSTO, TJ_CENTRAB, TL_CODIGO, B1_GRUPO, B1_UM, B1_DESC,T9_NOME "
	cQuery+="ORDER BY T9_MODELO+TL_CODIGO+TJ_CODBEM"
	Cabec1       := "MODELO"
ENDIF

TCQUERY cQuery NEW ALIAS "QRY"

dbselectarea("QRY")
DbGoTop()
COUNT TO _RECCOUNT
DbGoTop()
SetRegua(_RECCOUNT)

CAUX:=""
caux1:=""
IF nOrdem==2
	caux2:=QRY->T9_CODFAMI+QRY->TL_CODIGO
elseIF nOrdem==3
	caux2:=QRY->T9_MODELO+QRY->TL_CODIGO
endif
nTot1:=0
nTot2:=0
PVEZ:=.T.
aDET:={0,0,0,0,0,0}
aBem:={}
ENTROU:=.F.
While !EOF()
	ENTROU:=.T.
	If lAbortPrint
		@nLin,00 PSAY "*** CANCELADO PELO OPERADOR ***"
		Exit
	Endif
    IncRegua()
	
	//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
	//ณ Considera filtro do usuario                                  ณ
	//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
	valido:=.f.
	If (!Empty(MV_PAR11).and. (QRY->B1_GRUPO$MV_PAR11))
		valido:=.t.
	endif
	If  (!Empty(MV_PAR12).and. (QRY->B1_GRUPO$MV_PAR12))
		valido:=.t.
	endif
	IF !valido
		dbSkip()
		Loop
	Endif
	
	If nLin > 60
		Cabec(Titulo,Cabec1,Cabec2,NomeProg,Tamanho,nTipo)
		nLin := 9
	Endif
	if nOrdem==1
		H:=HTRAB(QRY->TJ_CODBEM,@uData)
	elseif nOrdem==2
		H:=HTRAB1(QRY->T9_CODFAMI)
	ELSE
		H:=HTRAB2(QRY->T9_MODELO)
	endif
	IF nOrdem=1
		IF CAUX<>QRY->TJ_CODBEM
			IF !PVEZ
				//imprimecusto(@nLin,ntot1,ntot2)
     			nTot1:=0
	    		nTot2:=0
				//nLin++
				nLin:=nLin + 2
			ENDIF
			PVEZ:=.F.   
			uRde := LERDE(QRY->TJ_CODBEM)
			
			@nLin,000 PSAY  ALLTRIM(QRY->TJ_CODBEM)  +" "+QRY->T9_NOME
			//@nLin,060 PSAY  QRY->T9_CODFAMI+" "+RETFIELD("ST6",1,XFILIAL("ST6")+QRY->T9_CODFAMI,"T6_NOME")
			nLin++
			@nLin,007 PSAY  "ULTIMO CONTADOR INFORMADO EM: "+substr(uData,7,2)+"/"+substr(uData,5,2)+"/"+substr(uData,3,2)
			nLin++
			@nLin,007 PSAY  "ULTIMO RDE INFORMADO EM:      "+substr(uRde,7,2)+"/"+substr(uRde,5,2)+"/"+substr(uRde,3,2)
			nLin++
		endif
		cCodigo := QRY->TL_CODIGO
		cDesc   := QRY->B1_DESC
		nQuant  := QRY->QUANTID
		cUM		:= QRY->B1_UM
		nHoras  := H
		nMedia  := QRY->QUANTID/H
		nCusto  := QRY->CUSTO
		impDet(@nLin)
		
	else
		OK:=.F.
		IF	nOrdem==2
			IF caux2<>QRY->T9_CODFAMI+QRY->TL_CODIGO
				OK:=.T.
			ENDIF
		ENDIF
		IF nOrdem==3
			IF caux2<>QRY->T9_MODELO+QRY->TL_CODIGO
				OK:=.T.
			ENDIF
		ENDIF
		IF OK .AND. !PVEZ
			cCodigo := aDet[1]
			cDesc   := aDet[2]
			nQuant  := aDet[3]
			cUM		:= aDet[4]
			nHoras  := aDet[5]
			nMedia  := aDet[3]/aDet[5]
			nCusto  := aDet[6]
			impDet(@nLin)
			aDET:={0,0,0,0,0,0}

		endif
		OK:=.F.
		IF nOrdem==2
			IF CAUX1<>QRY->T9_CODFAMI
				OK:=.T.
			ENDIF
		ENDIF
		IF nOrdem==3
			IF CAUX1<>QRY->T9_MODELO
				OK:=.T.
			ENDIF
		ENDIF
		IF OK
			IF !PVEZ
				//imprimecusto(@nLin,ntot1,ntot2)
    			nTot1:=0
	    		nTot2:=0
				//nLin++        
				nLin:= nLin + 2
				
			ENDIF
			PVEZ:=.F.
			IF nOrdem==2
				@nLin,000 PSAY  QRY->T9_CODFAMI+" "+RETFIELD("ST6",1,XFILIAL("ST6")+QRY->T9_CODFAMI,"T6_NOME")
			else
				@nLin,000 PSAY  QRY->T9_MODELO
			endif
		   	nLin++

		ENDIF
		aDet[1]:=QRY->TL_CODIGO
		aDet[2]:=QRY->B1_DESC
		aDet[3]+=QRY->QUANTID
		aDet[4]:=QRY->B1_UM
		aDet[5]:=H
		aDet[6]+=QRY->CUSTO
	ENDIF
	if QRY->B1_GRUPO$MV_PAR11
		ntot1+=QRY->CUSTO
	endif
	if QRY->B1_GRUPO$MV_PAR12
		ntot2+=QRY->CUSTO
	endif
	//	QRY->CUSTO
	CAUX:=QRY->TJ_CODBEM
	IF nOrdem<>3
		caux1:=QRY->T9_CODFAMI
		caux2:=QRY->T9_CODFAMI+QRY->TL_CODIGO
	else
		caux1:=QRY->T9_MODELO
		caux2:=QRY->T9_MODELO+QRY->TL_CODIGO
	endif
	DBSKIP()
EndDo
IF ENTROU
	IF nOrdem==2 .OR. nOrdem==3
		cCodigo := aDet[1]
		cDesc   := aDet[2]
		nQuant  := aDet[3]
		cUM		:= aDet[4]
		nHoras  := aDet[5]
		nMedia  := aDet[3]/aDet[5]
		nCusto  := aDet[6]
		impDet(@nLin)
	ENDIF
	//imprimecusto(@nLin,ntot1,ntot2)
	nTot1:=0
	nTot2:=0
//	nLin++
	nLin:=nLin + 2
ENDIF
CLOSE
if nOrdem==2 .OR. nOrdem==3
	DbSelectArea("QRY1")
	CLOSE 
endif

//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณ Finaliza a execucao do relatorio...                                 ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

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
aAdd(aRegs,{cPerg,"01","Da Data Abert.       ?","","","mv_ch1","D",8,0,0,"G","","mv_par01","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"02","Ate a Data Abert.    ?","","","mv_ch2","D",8,0,0,"G","","mv_par02","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"03","Do Centro de Custo   ?","","","mv_ch3","C",9,0,0,"G","","mv_par03","","","","","","","","","","","","","","","","","","","","","","","","","CTT","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"04","Ate o Centro de Custo?","","","mv_ch4","C",9,0,0,"G","","mv_par04","","","","","","","","","","","","","","","","","","","","","","","","","CTT","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"05","Do Centro Trabalho   ?","","","mv_ch5","C",6,0,0,"G","","mv_par05","","","","","","","","","","","","","","","","","","","","","","","","","SHB","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"06","Ate Centro Trabalho  ?","","","mv_ch6","C",6,0,0,"G","","mv_par06","","","","","","","","","","","","","","","","","","","","","","","","","SHB","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"07","Do Bem               ?","","","mv_ch7","C",16,0,0,"G","","mv_par07","","","","","","","","","","","","","","","","","","","","","","","","","ST9","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"08","Ate o Bem            ?","","","mv_ch8","C",16,0,0,"G","","mv_par08","","","","","","","","","","","","","","","","","","","","","","","","","ST9","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"09","Da Familia           ?","","","mv_ch9","C",6,0,0,"G","","mv_par09","","","","","","","","","","","","","","","","","","","","","","","","","ST6","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"10","Ate Familia          ?","","","mv_cha","C",6,0,0,"G","","mv_par10","","","","","","","","","","","","","","","","","","","","","","","","","ST6","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"11","Grupos Combustiveis  ?","","","mv_chb","C",40,0,0,"G","","mv_par11","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"12","Grupos Lubrificantes ?","","","mv_chc","C",40,0,0,"G","","mv_par12","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"13","Da Data Requis.      ?","","","mv_chd","D",8,0,0,"G","","mv_par13","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"14","Ate a Data Requis.   ?","","","mv_che","D",8,0,0,"G","","mv_par14","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"15","Considera trocas     ?","","","mv_chf","N",1,0,0,"C","","mv_par15","Sim","","","","","Nao","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})

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

STATIC FUNCTION HTRAB(CODBEM,UDATA)
LOCAL nTotal:=0       
LOCAL nContIni:=0
LOCAL nContAtu:=0
LOCAL aArea:=GetArea()
LOCAL cHr:="00:00"
dbSelectArea("STP")
dbSetOrder(5)//TP_FILIAL+TP_CODBEM+DTOS(TP_DTLEITU)+TP_HORA
dbSeek(xFilial("STP")+CODBEM+Dtos(MV_PAR13),.T.)

while !Eof() .AND. STP->TP_FILIAL+STP->TP_CODBEM+Dtos(STP->TP_DTLEITU)<=xFilial("STP")+CODBEM+Dtos(MV_PAR14)
    if nContIni==0
       nContIni:= STP->TP_POSCONT
       nContAtu:= STP->TP_POSCONT
    endif
 	if Dtos(STP->TP_DTLEITU)==uData .AND. STP->TP_HORA <= cHr
 	   DbSkip()
 	   Loop
 	endif
    if STP->TP_TIPOLAN=="Q"
       nTotal:=nContAtu - nContIni
       nContIni:=STP->TP_POSCONT
    endif
    nContAtu:=STP->TP_POSCONT
    uData:=Dtos(STP->TP_DTLEITU)
    cHr:=STP->TP_HORA
	DbSkip()
enddo

nTotal+=nContAtu-nContIni
RestArea(aArea)           
Return(nTotal)

//STATIC FUNCTION	imprimecusto(nLin,ntot1,ntot2)
//@nLin,007 PSAY  "Total Gr.Comb.:"
//@nLin,021 PSAY  ntot1	PICTURE "@E 9999,999.99"
//@nLin,043 PSAY  "Total Gr.Lubr.:"
//@nLin,058 PSAY  ntot2	PICTURE "@E 9999,999.99"
//@nLin,080 PSAY  "Gr.Lubr./Gr.Comb.(%):"
//@nLin,101 PSAY  ntot2/ntot1*100	PICTURE "@E 9999,999.99" 

RETURN

STATIC FUNCTION	impDet(nLin)
//@nLin,006 PSAY cCodigo
@nLin,007 PSAY cDesc
@nLin,074 PSAY nQuant PICTURE "@E 9,999,999.99"
@nLin,089 PSAY cUM
@nLin,092 PSAY nHoras PICTURE "@E 9999,999.99"
@nLin,104 PSAY nMedia PICTURE "@E 9999,999.99"
@nLin,116 PSAY nCusto PICTURE "@E 9999,999.99"
nLin := nLin + 1 // Avanca a linha de impressao
RETURN

STATIC FUNCTION	HTRAB1(CODFAMI)
LOCAL nTotal:=0
LOCAL cBem:=""
dbselectarea("QRY1")
DbGoTop() 
While !EOF() 
	if QRY1->T9_CODFAMI==CODFAMI .AND. QRY1->TJ_CODBEM<>cBem
       nTotal+=HTRAB(QRY1->TJ_CODBEM,"")
	   cBem:=QRY1->TJ_CODBEM
    endif
	DBSKIP()
ENDDO
dbselectarea("QRY")
Return(nTotal)


STATIC FUNCTION	HTRAB2(MODELO)
LOCAL nTotal:=0
LOCAL cBem:=""
dbselectarea("QRY1")
DbGoTop()
While !EOF()
	if QRY1->T9_MODELO==MODELO .AND. QRY1->TJ_CODBEM<>cBem
       nTotal+=HTRAB(QRY1->TJ_CODBEM,"")
	   cBem:=QRY1->TJ_CODBEM
    endif
	DBSKIP()
ENDDO
dbselectarea("QRY")
Return(nTotal)

STATIC FUNCTION LERDE(CODBEM)
LOCAL aArea:=GetArea()
LOCAL cData:=""
dbSelectArea("SZN")
dbSetOrder(1)//ZN_FILIAL+ZN_CODBEM+DTOS(ZN_DATA)+ZN_CODFUNC+ZN_NUM+ZN_ITEM
dbSeek(xFilial("SZN")+CODBEM+Dtos(MV_PAR13),.T.)
while !Eof() .AND. SZN->ZN_FILIAL+SZN->ZN_CODBEM+Dtos(SZN->ZN_DATA)<=xFilial("SZN")+CODBEM+Dtos(MV_PAR14)
    cData:=Dtos(SZN->ZN_DATA)
	DbSkip()
enddo

RestArea(aArea)           
Return(cData)

