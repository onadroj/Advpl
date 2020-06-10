#INCLUDE "TOPCONN.CH"
#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณUMTR002   บ Autor ณ GATASSE            บ Data ณ  29/11/04   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDescricao ณ Relatorio de Consumo de Combustiveis/Lubrificantes         บฑฑ
ฑฑบ          ณ No Ano                                                     บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ AP6 IDE                                                    บฑฑ
ฑฑศออออออออออฯออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
/*/

User Function UMTR002


//ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
//ณ Declaracao de Variaveis                                             ณ
//ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

Local cDesc1         := "Este programa tem como objetivo imprimir relatorio "
Local cDesc2         := "de acordo com os parametros informados pelo usuario."
Local cDesc3         := "Relatorio Anual de Consumo de Combustiveis/Lubrificantes"
Local cPict          := ""
Local titulo       := "Rel. Anual de Consumo"
Local nLin         := 80

Local Cabec1       := "DESCRICAO                            JAN            FEV            MAR            ABR            MAI            JUN            JUL            AGO            SET            OUT            NOV            DEZ         MEDIA"
Local Cabec2       := ""
Local imprime      := .T.
Private aOrd             := {"por equipamento","por famํlia","por modelo"}
Private lEnd         := .F.
Private lAbortPrint  := .F.
Private CbTxt        := ""
Private limite           := 220
Private tamanho          := "G"
Private nomeprog         := "UMTR002"
Private nTipo            := 18
Private aReturn          := { "Zebrado", 1, "Administracao", 2, 2, 1, "", 1}
Private nLastKey        := 0
Private cPerg       := "UMT002"
Private cbtxt      := Space(10)
Private cbcont     := 00
Private CONTFL     := 01
Private m_pag      := 01
Private wnrel      := "UMTR002"
Private cCodigo
Private cDesc
Private nQuant
Private cUM
Private nHoras
Private nMedia
Private nCusto

Private cString := "SZN"

dbSelectArea("SZN")
dbSetOrder(1)


ValidPerg()
pergunte(cPerg,.F.)                         
titulo:=titulo+" "+mv_par01

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

nOpca   := 1
Private oDlg4
lInverte:= .f.
cMARCA  := GetMark()

DEFINE MSDIALOG ODLG4 TITLE "Selecao de Categoria" From 6.5,0 To 35,72 OF oMainWnd
@ 20,30 Say OemToAnsi ("Selecione as categorias para impresssao") SIZE 150,20
oMark := MsSelect():New("SZR","ZR_OK",,,,@cMarca,{35,1,200,280})
oMark:oBrowse:lhasMark = .F.
oMark:oBrowse:lCanAllmark := .t.

ACTIVATE MSDIALOG ODLG4 ON INIT EnchoiceBar(ODLG4,{||nOpca:=2,ODLG4:End()},{||nOpca:=1,ODLG4:End()}) CENTERED
If nOpca == 2      //OK
	RptStatus({|| RunReport(Cabec1,Cabec2,Titulo,nLin) },Titulo)
ENDIF

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
Local nOrdem
LOCAL AAREA,AAREA2
nOrdem := aReturn[8]
DbSelectArea("SZR")
DbGoTop()
IF EOF()
	MSGSTOP("Categorias de Estoque nao foram cadastradas!")
	RETURN
ENDIF

if nOrdem==1
   cCampo:="TJ_CODBEM"
elseif nOrdem==2
   cCampo:="T9_CODFAMI"
else
   cCampo:="T9_MODELO"
endif

//Prepara arquivo auxiliar
if nOrdem==2 .OR. nOrdem==3
	cQuery:="SELECT "+cCampo+", TJ_CODBEM "
	cQuery+="FROM ((("+RETSQLNAME("STJ")+" STJ INNER JOIN "+RETSQLNAME("STL")+" STL ON (TJ_FILIAL=TL_FILIAL) AND (TJ_PLANO=TL_PLANO) AND (TJ_ORDEM=TL_ORDEM)) INNER JOIN "+RETSQLNAME("SB1")+" SB1 ON TL_CODIGO=B1_COD) INNER JOIN ("+RETSQLNAME("SZR")+" SZR INNER JOIN "+RETSQLNAME("SZS")+" SZS ON (ZR_FILIAL=ZS_FILIAL) AND (ZR_CATEGO=ZS_CATEGO)) ON B1_GRUPO=ZS_GRUPO) INNER JOIN "+RETSQLNAME("ST9")+" ST9 ON TJ_CODBEM=T9_CODBEM "
	cQuery+="WHERE STJ.D_E_L_E_T_<>'*' AND STL.D_E_L_E_T_<>'*'  AND SB1.D_E_L_E_T_<>'*' AND SZR.D_E_L_E_T_<>'*' AND SZS.D_E_L_E_T_<>'*' AND ST9.D_E_L_E_T_<>'*' AND (ST9.T9_MTBAIXA ='' OR ST9.T9_MTBAIXA ='0003') "
	cQuery+="AND    TJ_SITUACA<>'C' "
	cQuery+="AND    TJ_CCUSTO  >= '"+MV_PAR02+"' AND TJ_CCUSTO <= '"+MV_PAR03+"' "
	cQuery+="AND    TJ_CENTRAB >= '"+MV_PAR04+"' AND TJ_CENTRAB <= '"+MV_PAR05+"' "
	cQuery+="AND    TJ_CODBEM  >= '"+MV_PAR06+"' AND TJ_CODBEM <= '"+MV_PAR07+"' " 
	cQuery+="AND    "+IIF(MV_PAR10==1,"TJ_DTORIGI","TL_DTDIGIT")+" LIKE'"+MV_PAR01+"%' "
	cQuery+="AND    ZR_OK='"+cMARCA+"' "
	cQuery+="AND    T9_CODFAMI >= '"+MV_PAR08+"' AND T9_CODFAMI <= '"+MV_PAR09+"' "
	cQuery+="AND    T9_TEMCONT='S' "
	cQuery+="GROUP BY "+cCampo+", TJ_CODBEM "
	cQuery+="ORDER BY "+cCampo+", TJ_CODBEM "
	TCQUERY cQuery NEW ALIAS "QRY1"
endif	

//Prepara arquivo de trabalho
	cQuery:="SELECT "+cCampo+", ZR_CATEGO, ZR_DESC, SUBSTRING("+IIF(MV_PAR10==1,"TJ_DTORIGI","TL_DTDIGIT")+",5,2) AS MES, SUM(TL_CUSTO) AS CUSTO, ZR_TIPO "
	cQuery+="FROM ((("+RETSQLNAME("STJ")+" STJ INNER JOIN "+RETSQLNAME("STL")+" STL ON (TJ_FILIAL=TL_FILIAL) AND (TJ_PLANO=TL_PLANO) AND (TJ_ORDEM=TL_ORDEM)) INNER JOIN "+RETSQLNAME("SB1")+" SB1 ON TL_CODIGO=B1_COD) INNER JOIN ("+RETSQLNAME("SZR")+" SZR INNER JOIN "+RETSQLNAME("SZS")+" SZS ON (ZR_FILIAL=ZS_FILIAL) AND (ZR_CATEGO=ZS_CATEGO)) ON B1_GRUPO=ZS_GRUPO) INNER JOIN "+RETSQLNAME("ST9")+" ST9 ON TJ_CODBEM=T9_CODBEM "
	cQuery+="WHERE STJ.D_E_L_E_T_<>'*' AND STL.D_E_L_E_T_<>'*'  AND SB1.D_E_L_E_T_<>'*' AND SZR.D_E_L_E_T_<>'*' AND SZS.D_E_L_E_T_<>'*' AND ST9.D_E_L_E_T_<>'*' AND (ST9.T9_MTBAIXA ='' OR ST9.T9_MTBAIXA ='0003') "
	cQuery+="AND    TJ_SITUACA<>'C' "
	cQuery+="AND    TJ_CCUSTO  >= '"+MV_PAR02+"' AND TJ_CCUSTO <= '"+MV_PAR03+"' "
	cQuery+="AND    TJ_CENTRAB >= '"+MV_PAR04+"' AND TJ_CENTRAB <= '"+MV_PAR05+"' "
	cQuery+="AND    TJ_CODBEM  >= '"+MV_PAR06+"' AND TJ_CODBEM <= '"+MV_PAR07+"' " 
	cQuery+="AND    "+IIF(MV_PAR10==1,"TJ_DTORIGI","TL_DTDIGIT")+" LIKE'"+MV_PAR01+"%' "
	cQuery+="AND    ZR_OK='"+cMARCA+"' "
	cQuery+="AND    T9_CODFAMI >= '"+MV_PAR08+"' AND T9_CODFAMI <= '"+MV_PAR09+"' "
	cQuery+="AND    T9_TEMCONT='S' "
	cQuery+="GROUP BY "+cCampo+", ZR_CATEGO, ZR_DESC, SUBSTRING("+IIF(MV_PAR10==1,"TJ_DTORIGI","TL_DTDIGIT")+",5,2), ZR_TIPO "
	cQuery+="ORDER BY "+cCampo+", ZR_CATEGO, ZR_DESC, SUBSTRING("+IIF(MV_PAR10==1,"TJ_DTORIGI","TL_DTDIGIT")+",5,2), ZR_TIPO "

TCQUERY cQuery NEW ALIAS "QRY"

dbselectarea("QRY")
DbGoTop()
IF EOF()
    CLOSE
    if nOrdem==2 .OR. nOrdem==3
       dbSelectArea("QRY1")
       CLOSE
    endif
	MSGSTOP("Nao existem dados para geracao do relatorio. Verifique os parametros!")
	RETURN
ENDIF
COUNT TO _RECCOUNT
DbGoTop()
SetRegua(_RECCOUNT)

if nOrdem==1
   cAux:=TJ_CODBEM
elseif nOrdem==2
   cAux:=T9_CODFAMI
else
   cAux:=T9_MODELO
endif
cCat:= ZR_CATEGO
lPvez:=.T.            
lUltimo:=.F.
lFim:=.F.
cDesc:= ZR_DESC
While !lFim 
	If lAbortPrint
		@nLin,00 PSAY "*** CANCELADO PELO OPERADOR ***"
		Exit
	Endif
	IncRegua()

	If nLin > 55
		Cabec(Titulo,Cabec1,Cabec2,NomeProg,Tamanho,nTipo)
		nLin := 8
	Endif


    if &cCampo<>cAux .OR. lUltimo
    	if mv_par11==1 .or. mv_par11==3
			NLIN++
			@nLin,005 PSAY cDESC
			med:=0
			FOR X:=1 TO 12
				@nlin,11+x*15 PSAY AVALORES[X] PICTURE "@E 999,999,999.99"
				med+=AVALORES[X]
			NEXT
			@nlin,209 PSAY med/ntoth PICTURE "99999"
		endif
		if mv_par11==2 .or. mv_par11==3
			NLIN++
			@nLin,005 PSAY cDESC+"/HT"
			med:=0
			FOR X:=1 TO 12
				@nlin,11+x*15 PSAY AVALORES[X]/AHT[X] PICTURE "@E 999,999,999.99"
//				med+=AVALORES[x]
			NEXT
//			@nlin,209 PSAY med/ntoth PICTURE "99999"
		endif
	    NLIN++
		NLIN++
	    @nLin,005 PSAY "TOTAIS POR TIPO"
        NLIN++
   	    med:=0
	    @nLin,005 PSAY "COMBUSTIVEIS"
	    FOR X:= 1 TO 12
	    	@nlin,11+x*15 PSAY aComb[x] PICTURE "@E 999,999,999.99"
//	    	med+=aComb[x]*aht[x]
	    	med+=aComb[x]
	    NEXT
	    @nlin,209 PSAY med/ntoth PICTURE "99999"
	    NLIN++
		med:=0
	    @nLin,005 PSAY "LUBRIFICANTES"
	    FOR X:= 1 TO 12
	    	@nlin,11+x*15 PSAY aLub[x] PICTURE "@E 999,999,999.99"
//	    	med+=aLub[x]*aht[x]
	    	med+=aLub[x]
	    NEXT
	    @nlin,209 PSAY med/ntoth PICTURE "99999"
	    NLIN++
		med:=0
	    @nLin,005 PSAY "OUTROS"
	    FOR X:= 1 TO 12
	    	@nlin,11+x*15 PSAY aOut[x] PICTURE "@E 999,999,999.99"
//	    	med+=aOut[x]*aht[x]
	    	med+=aOut[x]
	    NEXT
	    @nlin,209 PSAY med/ntoth PICTURE "99999"
	    NLIN++
		if mv_par11==1 .or. mv_par11==3
			NLIN++
			@nLin,005 PSAY  "TOTAL DOS CUSTOS"
			med:=0
			FOR X:=1 TO 12
				@nlin,11+x*15 PSAY ATOT[X] PICTURE "@E 999,999,999.99"
//				med+=ATOT[X]*aht[x]
				med+=ATOT[X]
			NEXT
			@nlin,209 PSAY IIF(ntoth=0,0,med/ntoth) PICTURE "99999"
		endif
		if mv_par11==2 .or. mv_par11==3
			NLIN++
			@nLin,005 PSAY  "TOTAL CUSTOS/HT"
			med:=0
			FOR X:=1 TO 12
				@nlin,11+x*15 PSAY IIF(AHT[X]=0,0,ATOT[X]/AHT[X]) PICTURE "@E 999,999,999.99"
//				med+=ATOT[X]
			NEXT
//			@nlin,209 PSAY IIF(ntoth =0,0,med/ntoth) PICTURE "99999"
		endif
		if mv_par11==1 .or. mv_par11==3
			NLIN++
			@nLin,005 PSAY  "CUSTOS ACUMULADOS"
			med:=0
			FOR X:=1 TO 12
				MED+=ATOT[X]
				@nlin,11+x*15 PSAY MED PICTURE "@E 999,999,999.99"
			NEXT
		endif
		if mv_par11==2 .or. mv_par11==3
			NLIN++
			@nLin,005 PSAY  "CUSTOS ACUM./HT"
			med:=0
			hta:=0
			FOR X:=1 TO 12
				MED+=ATOT[X]
				hta+=Aht[X]
				@nlin,11+x*15 PSAY IIF(hta=0,0,MED/hta) PICTURE "@E 999,999,999.99"
			NEXT
		endif
	    NLIN++
		@nLin,005 PSAY  "IND.LUB/COMB x 100"
		med:=0
		FOR X:=1 TO 12
			@nlin,11+x*15 PSAY iif(ACOMB[X]<>0,ALUB[X]/ACOMB[X]*100,0) PICTURE "@E 999,999,999.99"
//			med+=iif(ACOMB[X]<>0,ALUB[X]/ACOMB[X]*100*aht[x],0)
//			med+=iif(ACOMB[X]<>0,ALUB[X]/ACOMB[X]*100,0)
		NEXT
//		@nlin,209 PSAY med/ntoth PICTURE "99999"
		NLIN++
		NLIN++
        if nOrdem==1
           cAux:=TJ_CODBEM
        elseif nOrdem==2
           cAux:=T9_CODFAMI
        else
           cAux:=T9_MODELO
        endif
        cCat:= ZR_CATEGO
        cDesc:=ZR_DESC
        lPvez:=.T.
        if lUltimo
           lFim:=.T.
        endif
	endif

    if &cCampo==cAux .AND. !lFim
       if lPvez
			if nOrdem==1
				@nLin,000 PSAY  TJ_CODBEM           
				@nLin,020 PSAY  RETFIELD("ST9",1,xFilial("ST9")+TJ_CODBEM,"T9_NOME")
			elseif nOrdem==2
				@nLin,000 PSAY  T9_CODFAMI
				@nLin,010 PSAY  RETFIELD("ST6",1,XFILIAL("ST6")+T9_CODFAMI,"T6_NOME")
			else
				@nLin,000 PSAY  T9_MODELO
			ENDIF
			NLIN++
			AHT:={0,0,0,0,0,0,0,0,0,0,0,0}
			@nLin,005 PSAY  "HORAS TRABALHADAS"
			ntoth:=0
			FOR X:=1 TO 12
				if nOrdem==1
					h:=HTRAB(TJ_CODBEM,strzero(x,2))
				elseif nOrdem==2
					H:=HTRAB1(T9_CODFAMI,strzero(x,2))
				else
				    h:=HTRAB2(T9_MODELO,strzero(x,2))
				endif
				@nlin,20+x*15 PSAY h PICTURE "99999"
				ntoth+=h
				AHT[X]:=h
			next
			//media
			@nlin,209 PSAY ntoth/IIF(val(mv_par01)==year(ddatabase),month(ddatabase),12) PICTURE "99999"
			AVALORES:={0,0,0,0,0,0,0,0,0,0,0,0}
    		ATOT:={0,0,0,0,0,0,0,0,0,0,0,0} //total dos custos
			ACOMB:={0,0,0,0,0,0,0,0,0,0,0,0}
			ALUB:={0,0,0,0,0,0,0,0,0,0,0,0}
			AOUT:={0,0,0,0,0,0,0,0,0,0,0,0}
			lPvez:=.F.
       endif  
       if ZR_CATEGO==cCat
          X:=val(MES)
          AVALORES[X]:=CUSTO
          ATOT[X]+=CUSTO
    	  IF ZR_TIPO=="C"
		     ACOMB[X]+=CUSTO
          ENDIF
          IF ZR_TIPO=="L"
			 ALUB[X]+=CUSTO
          ENDIF
          IF ZR_TIPO=="O"
			 AOUT[X]+=CUSTO
          ENDIF
	   else
    		if mv_par11==1 .or. mv_par11==3
				NLIN++
				@nLin,005 PSAY cDESC
				med:=0
				FOR X:=1 TO 12
					@nlin,11+x*15 PSAY AVALORES[X] PICTURE "@E 999,999,999.99"
					med+=AVALORES[X]
				NEXT
				@nlin,209 PSAY med/ntoth PICTURE "99999"
			endif
			if mv_par11==2 .or. mv_par11==3
				NLIN++
				@nLin,005 PSAY cDESC+"/HT"
				med:=0
				FOR X:=1 TO 12
					@nlin,11+x*15 PSAY AVALORES[X]/AHT[X] PICTURE "@E 999,999,999.99"
//					med+=AVALORES[x]
				NEXT
//				@nlin,209 PSAY med/ntoth PICTURE "99999"
			endif
            cCat:=ZR_CATEGO     
            cDesc:=ZR_DESC
   		    AVALORES:={0,0,0,0,0,0,0,0,0,0,0,0}
            X:=val(MES)
            AVALORES[X]:=CUSTO
            ATOT[X]+=CUSTO
    	    IF ZR_TIPO=="C"
		       ACOMB[X]+=CUSTO
            ENDIF
            IF ZR_TIPO=="L"
			   ALUB[X]+=CUSTO
            ENDIF
            IF ZR_TIPO=="O"
			   AOUT[X]+=CUSTO
            ENDIF
       endif
    endif
	if !lFim
	  DBSKIP()
	  if EOF() 
	     lUltimo:=.T.
	  endif
	endif
ENDDO
CLOSE
if nOrdem==2 .OR. nOrdem==3
  dbSelectArea("QRY1")
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
//X1_GRUPO	X1_ORDEM	X1_PERGUNT	X1_PERSPA	X1_PERENG	X1_VARIAVL	X1_TIPO	X1_TAMANHO	X1_DECIMAL	X1_PRESEL	X1_GSC	X1_VALID	X1_VAR01	X1_DEF01	X1_DEFSPA1	X1_DEFENG1	X1_CNT01	X1_VAR02	X1_DEF02	X1_DEFSPA2	X1_DEFENG2	X1_CNT02	X1_VAR03	X1_DEF03	X1_DEFSPA3	X1_DEFENG3	X1_CNT03	X1_VAR04	X1_DEF04	X1_DEFSPA4	X1_DEFENG4	X1_CNT04	X1_VAR05	X1_DEF05	X1_DEFSPA5	X1_DEFENG5	X1_CNT05	X1_F3	X1_PYME	X1_GRPSXG	X1_HELP	X1_PICTURE	X1_IDFIL
// Grupo/Ordem/Pergunta/Variavel/Tipo/Tamanho/Decimal/Presel/GSC/Valid/Var01/Def01/Cnt01/Var02/Def02/Cnt02/Var03/Def03/Cnt03/Var04/Def04/Cnt04/Var05/Def05/Cnt05
aAdd(aRegs,{cPerg,"01","Ano                  ?","","","mv_ch1","C",4,0,0,"G","(mv_par01>='2000' .AND. mv_par01<='2040')","mv_par01","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"02","Do Centro de Custo   ?","","","mv_ch2","C",9,0,0,"G","","mv_par02","","","","","","","","","","","","","","","","","","","","","","","","","CTT","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"03","Ate o Centro de Custo?","","","mv_ch3","C",9,0,0,"G","","mv_par03","","","","","","","","","","","","","","","","","","","","","","","","","CTT","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"04","Do Centro Trabalho   ?","","","mv_ch4","C",6,0,0,"G","","mv_par04","","","","","","","","","","","","","","","","","","","","","","","","","SHB","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"05","Ate Centro Trabalho  ?","","","mv_ch5","C",6,0,0,"G","","mv_par05","","","","","","","","","","","","","","","","","","","","","","","","","SHB","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"06","Do Bem               ?","","","mv_ch6","C",16,0,0,"G","","mv_par06","","","","","","","","","","","","","","","","","","","","","","","","","ST9","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"07","Ate o Bem            ?","","","mv_ch7","C",16,0,0,"G","","mv_par07","","","","","","","","","","","","","","","","","","","","","","","","","ST9","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"08","Da Familia           ?","","","mv_ch8","C",6,0,0,"G","","mv_par08","","","","","","","","","","","","","","","","","","","","","","","","","ST6","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"09","Ate Familia          ?","","","mv_ch9","C",6,0,0,"G","","mv_par09","","","","","","","","","","","","","","","","","","","","","","","","","ST6","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"10","Quanto a Data        ?","","","mv_cha","N",1,0,0,"C","","mv_par10","Da Ordem","","","","","Da Requisicao","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"11","Imprime              ?","","","mv_chb","N",1,0,0,"C","","mv_par11","Custo","","","","","Custo Hora","","","","","Ambos","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})

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

STATIC FUNCTION HTRAB(CODBEM,cMES)
LOCAL nTotal:=0       
LOCAL nContIni:=0
LOCAL nContAtu:=0
LOCAL aArea:=GetArea()
LOCAL cHr:="00:00"    
LOCAL cData
dbSelectArea("STP")
dbSetOrder(5)//TP_FILIAL+TP_CODBEM+DTOS(TP_DTLEITU)+TP_HORA
dbSeek(xFilial("STP")+CODBEM+MV_PAR01+cMES+"01",.T.)

while !Eof() .AND. STP->TP_FILIAL+STP->TP_CODBEM+Dtos(STP->TP_DTLEITU)<=xFilial("STP")+CODBEM+MV_PAR01+cMES+"31"
    if nContIni==0
       nContIni:= STP->TP_POSCONT
       nContAtu:= STP->TP_POSCONT
    endif
 	if Dtos(STP->TP_DTLEITU)==cData .AND. STP->TP_HORA <= cHr
 	   DbSkip()
 	   Loop
 	endif
    if STP->TP_TIPOLAN=="Q"
       nTotal:=nContAtu - nContIni
       nContIni:=STP->TP_POSCONT
    endif
    nContAtu:=STP->TP_POSCONT
    cData:=Dtos(STP->TP_DTLEITU)
    cHr:=STP->TP_HORA
	DbSkip()
enddo

nTotal+=nContAtu-nContIni
RestArea(aArea)           
Return(nTotal)

STATIC FUNCTION	HTRAB1(CODFAMI,cMES)
LOCAL aArea:=GetArea()
LOCAL nTotal:=0
LOCAL cBem:=""
dbselectarea("QRY1")
DbGoTop() 
While !EOF() 
	if QRY1->T9_CODFAMI==CODFAMI .AND. QRY1->TJ_CODBEM<>cBem
       nTotal+=HTRAB(QRY1->TJ_CODBEM,cMes)
	   cBem:=QRY1->TJ_CODBEM
    endif
	DBSKIP()
ENDDO
RestArea(aArea)
Return(nTotal)

STATIC FUNCTION	HTRAB2(MODELO,cMES)
LOCAL aArea:=GetArea()
LOCAL nTotal:=0
LOCAL cBem:=""
dbselectarea("QRY1")
DbGoTop()
While !EOF()
	if QRY1->T9_MODELO==MODELO .AND. QRY1->TJ_CODBEM<>cBem
       nTotal+=HTRAB(QRY1->TJ_CODBEM,cMes)
	   cBem:=QRY1->TJ_CODBEM
    endif
	DBSKIP()
ENDDO
RestArea(aArea)
Return(nTotal)
