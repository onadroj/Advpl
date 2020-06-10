#INCLUDE "rwmake.ch"

/*/
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
�������������������������������������������������������������������������ͻ��
���Programa  �RELOSGR   � Autor � GATASSE            � Data �  02/02/2005 ���
�������������������������������������������������������������������������͹��
���Descricao � EMISSAO DA ORDEM DE SERVICO GRAFICA                        ���
���          �                                                            ���
�������������������������������������������������������������������������͹��
���Uso       � AP6 IDE                                                    ���
�������������������������������������������������������������������������ͼ��
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
/*/

User Function RELOSGR

Private cPerg       := "URELOS"
Private nLin         := 100
Private nPag:=1
Private cArq  
Private xDescaux
Private ntamlin := 80
cArq := CriaTrab(,.F.)            
cArq=carq+".bmp"
CBitMap		:= "lgrl"+SM0->M0_CODIGO+".bmp"
U_FILECOPY(CBitMap,cArq)
dbSelectArea("STJ")
dbSetOrder(1)

ValidPerg()
pergunte(cPerg,.T.)
Private oFont, cCode

oFont1 := TFont():New( "Times New Roman",,08,,.t.,,,,,.f. )
oFont2 := TFont():New( "Times New Roman",,10,,.t.,,,,,.f. )
oFont3 := TFont():New( "Times New Roman",,12,,.t.,,,,,.f. )
oFont4 := TFont():New( "Times New Roman",,14,,.t.,,,,,.f. )
oFont5 := TFont():New( "Times New Roman",,16,,.t.,,,,,.f. )

oFont6 := TFont():New( "HAETTENSCHWEILLER",,10,,.t.,,,,,.f. )

oFont8 := TFont():New( "Free 3 of 9" ,,44,,.t.,,,,,.f. )
oFont10:= TFont():New( "Free 3 of 9" ,,38,,.t.,,,,,.f. )

oFont11:= TFont():New( "Courier New" ,,10,,.t.,,,,,.f. )
oFont12:= TFont():New( "Courier New" ,,08,,.t.,,,,,.f. )
oFont13:= TFont():New( "Arial"       ,,06,,.f.,,,,,.f. )
oFont14:= TFont():New( "Courier New"   ,,09,,.T.,,,,,.f. )
oFont15:= TFont():New( "Arial"         ,,10,,.t.,,,,,.f. )
oFont16:= TFont():New( "Arial"         ,,10,,.f.,,,,,.f. )
oFont17:= TFont():New( "Arial"         ,,11,,.f.,,,,,.f. )
oFont18:= TFont():New( "Arial"         ,,09,,.T.,,,,,.f. )
oFont19:= TFont():New( "Arial"         ,,22,,.t.,,,,,.f. )
oFont20:= TFont():New( "Arial Black"   ,,12,,.f.,,,,,.f. )
oFont21:= TFont():New( "Arial"         ,,14,,.f.,,,,,.f. )
oFont22:= TFont():New( "Arial"         ,,11,,.t.,,,,,.f. )
oFont23:= TFont():New( "Arial Black"   ,,8,,.t.,,,,,.f. )

oPrn := TMSPrinter():New()
oPrn:Setup()
//oPrn:EndPage()
oPrn:StartPage()

nOrdem := 1
dbSetOrder(nOrdem)
//SetRegua(RecCount())
//1-TJ_FILIAL+TJ_ORDEM+TJ_PLANO+TJ_CODBEM+TJ_SERVICO+STR(TJ_SEQUENC,3)
//2-TJ_FILIAL+TJ_CODBEM+TJ_SERVICO+STR(TJ_SEQUENC,3)+TJ_ORDEM+TJ_PLANO
//3-TJ_FILIAL+TJ_PLANO+TJ_ORDEM+TJ_CODBEM+TJ_SERVICO+STR(TJ_SEQUENC,3)
if nOrdem==1
	cSeek:=xfilial("STJ")+MV_PAR10
ELSEIF nOrdem==2
	cSeek:=xfilial("STJ")+"B"+MV_PAR08
ELSE
	cSeek:=xfilial("STJ")+MV_PAR01+MV_PAR10
ENDIF
dbSeek(cSeek,.T.)

While !EOF() .And. xfilial("STJ")==STJ->TJ_FILIAL
	//	iif(nOrdem==1,MV_PAR10==STJ->TJ_ORDEM,iif(nOrdem==2,MV_PAR08==STJ->TJ_CODBEM,MV_PAR01==STJ->TJ_PLANO))
	If DTOS(STJ->TJ_DTORIGI) < DTOS(mv_par02) .OR. DTOS(STJ->TJ_DTORIGI) > DTOS(mv_par03)
		dbSkip()
		Loop
	Endif
	If STJ->TJ_PLANO <> mv_par01
		dbSkip()
		Loop
	Endif
	If STJ->TJ_TERMINO =="S" .OR. STJ->TJ_SITUACA=="C"
		dbSkip()
		Loop
	Endif
	If STJ->TJ_CCUSTO < mv_par04 .OR. STJ->TJ_CCUSTO > mv_par05
		dbSkip()
		Loop
	Endif
	If STJ->TJ_CENTRAB < mv_par06 .OR. STJ->TJ_CENTRAB > mv_par07
		dbSkip()
		Loop
	Endif
	If STJ->TJ_CODBEM < mv_par08 .OR. STJ->TJ_CODBEM > mv_par09
		dbSkip()
		Loop
	Endif
	If STJ->TJ_ORDEM < mv_par10 .OR. STJ->TJ_ORDEM > mv_par11
		dbSkip()
		Loop
	Endif
	
	If nLin > ntamlin
		ImpCab(@nLin,carq)
	Endif
	
	aarea:=getarea()
	entrou:=.f.
//TAREFAS	
	dbselectarea("STQ")
	DBSETORDER(1)//TQ_FILIAL+TQ_ORDEM+TQ_PLANO+TQ_TAREFA+TQ_ETAPA+TQ_OPCAO
	DBSEEK(XFILIAL("STQ")+STJ->TJ_ORDEM+STJ->TJ_PLANO,.T.)
	WHILE !EOF() .AND. XFILIAL("STQ")+STJ->TJ_ORDEM+STJ->TJ_PLANO==;
		STQ->TQ_FILIAL+STQ->TQ_ORDEM+STJ->TJ_PLANO
		If AllTrim(STQ->TQ_TAREFA) == "" .OR. STQ->TQ_TAREFA == NIL
		   dbSkip()
		   Loop
		Endif
		entrou:=.t.
		VERT(nlin,nlin,0)
		VERT(nlin,nlin,195)
		VERT(nlin,nlin,1415)
		VERT(nlin,nlin,1895)
		VERT(nlin,nlin,2245)
		xSay ( nlin, 3, STQ->TQ_TAREFA,1)
//		xDescaux:=alltrim(ST5->T5_DESCAUX)
		xDescaux:=alltrim(ST5->T5_DESC)		
		for t:=1 to mlcount(xDescaux,60)
			xSay (nlin,450 ,memoline(xDescaux,60,t),1,1)
			nLin++
			If nLin > ntamlin .and. t<>mlcount(xDescaux,60)
			    oPrn:EndPage()
  				ImpCab1(@nLin)
			Endif
		next
		nLin++
		If nLin > ntamlin
		    oPrn:EndPage()
			ImpCab1(@nLin)
		Endif
		DBSKIP()
	ENDDO

	entrou:=.f.
	dbselectarea("ST5")
	DBSETORDER(1)
	DBSEEK(XFILIAL("ST5")+STJ->TJ_CODBEM+STJ->TJ_SERVICO+STJ->TJ_SEQRELA,.T.)
	WHILE !EOF() .AND. XFILIAL("ST5")+STJ->TJ_CODBEM+STJ->TJ_SERVICO+STJ->TJ_SEQRELA==;
		ST5->T5_FILIAL+ST5->T5_CODBEM+ST5->T5_SERVICO+ST5->T5_SEQRELA
		entrou:=.t.
		VERT(nlin,nlin,0)
		VERT(nlin,nlin,2245)                
		xSAY (nlin,03,ST5->T5_TAREFA,1)
//		xDescaux:=alltrim(ST5->T5_DESCAUX)
		xDescaux:=alltrim(ST5->T5_DESC)		
		for t:=1 to mlcount(xDescaux,60)
			VERT(nlin,nlin,0)
			VERT(nlin,nlin,195)
			VERT(nlin,nlin,1415)
			VERT(nlin,nlin,1895)
			VERT(nlin,nlin,2245)
			xSAY(nlin,200, memoline(xDescaux,60,t),1)
			nLin++
			If nLin > ntamlin .and. t<>mlcount(xDescaux,60)
			    oPrn:EndPage()
				ImpCab1(@nLin)
			Endif
		next
		nLin++
		If nLin > ntamlin
		    oPrn:EndPage()
			ImpCab1(@nLin)
		Endif
		DBSKIP()
	ENDDO
	if !entrou
		nLin++
		If nLin > ntamlin .and. t<>mlcount(alltrim(ST5->T5_DESC),60)		
		    oPrn:EndPage()
			ImpCab1(@nLin)
		Endif
		nLin++
	endif
//	nLin:=nLin+3
	If nLin > ntamlin
	    oPrn:EndPage()
		ImpCab1(@nLin)
	endif
	Impcab2(@nLin)
	
	AAREA1:=GETAREA()
	dbselectarea("STG")
	dbsetorder(1)	
	dbseek(xfilial("STG")+STJ->TJ_CODBEM+STJ->TJ_SERVICO+STJ->TJ_SEQRELA,.T.)
	while !eof() .and. xfilial("STG")+STJ->TJ_CODBEM+STJ->TJ_SERVICO+STJ->TJ_SEQRELA==;
		STG->TG_FILIAL+STG->TG_CODBEM+STG->TG_SERVICO+STG->TG_SEQRELA
		VERT(nlin,nlin,0)
		VERT(nlin,nlin,195)
		VERT(nlin,nlin,445)

		XSAY(nlin,03, STG->TG_TAREFA,1)
		XSAY(nlin,200,STG->TG_TIPOREG+"-"+STG->TG_CODIGO,1)
		cDesc:="Erro no registro!"
		IF STG->TG_TIPOREG=="P"//M=Mao de Obra;E=Especialidade;P=Produto;F=Ferramenta;T=Terceiro
			cDesc:=ALLTRIM(retfield("SB1",1,XFILIAL("SB1")+STG->TG_CODIGO,"B1_DESC"))  +" "
			cDesc1:=""
			areastg:=getarea()
			dbselectarea("SZ2")
			DBSETORDER(1)//Z2_Filial + Z2_CodProd + Z2_CodFabr + Z2_CodPeca
			DBSEEK(XFILIAL("SZ2")+STG->TG_CODIGO,.T.)
			WHILE !EOF() .AND. XFILIAL("SZ2")+STG->TG_CODIGO==SZ2->Z2_Filial + SZ2->Z2_CodProd
				IF SZ2->Z2_CodFabr<>"0000"
					cDesc1+=ALLTRIM(RETFIELD("SX5",1,XFILIAL("SX5")+"99"+SZ2->Z2_CodFabr,"X5_DESCRI"))+":"+ALLTRIM(Z2_CodPeca)+ ", "
				ENDIF
				DBSKIP()
			ENDDO
			IF alltrim(cDesc1)<>""
				cDesc1:="=>Equiv.:"+substr(cDesc1,1,len(cDesc1)-1)
				cDesc:=cDesc+cDesc1
			ENDIF
			restarea(areastg)
		ELSEIF STG->TG_TIPOREG=="M"
			cDesc:=retfield("ST1",1,XFILIAL("ST1")+STG->TG_CODIGO,"T1_NOME")
		ELSEIF STG->TG_TIPOREG=="E"
			cDesc:=retfield("ST0",1,XFILIAL("ST0")+STG->TG_CODIGO,"T0_NOME")
		ELSEIF STG->TG_TIPOREG=="F"
			cDesc:=retfield("SH4",1,XFILIAL("SH4")+STG->TG_CODIGO,"H4_DESCRI")
		ELSEIF STG->TG_TIPOREG=="T"
			cDesc:=retfield("SA2",1,XFILIAL("SA2")+STG->TG_CODIGO,"A2_NOME")
		ENDIF
		xSAY (nlin,450,memoline(cDesc,70,1),1)
		VERT(nlin,nlin,1595)
		VERT(nlin,nlin,1745)
		VERT(nlin,nlin,1895)
		VERT(nlin,nlin,2245)
		xSAY (nlin,1600,transform(STG->TG_QUANTID, "9,999,999.99"),1)
		xSAY (nlin,1750,STG->TG_UNIDADE,1)
//		xSAY (nlin,trsnform(custo(STG->TG_CODIGO), "9,999,999.99"),1)
		nLin++
		FOR Z:=2 TO mlcount(cDesc, 70)
			VERT(nlin,nlin,0)
			VERT(nlin,nlin,195)
			VERT(nlin,nlin,445)
			VERT(nlin,nlin,1595)
			VERT(nlin,nlin,1745)
			VERT(nlin,nlin,1895)
			VERT(nlin,nlin,2245)
			xSAY(nlin,450, memoline(cDesc,70,z),1)
			nLin++
		next
		nLin++
		If nLin > ntamlin
		    oPrn:EndPage()
			ImpCab1(@nLin)
			ImpCab2(@nLin)
		Endif
		DBSKIP()
	ENDDO
	restarea(aarea1)
	for x:=nlin to 48
		VERT(x,x,0)
		VERT(x,x,195)
		VERT(x,x,445)
		VERT(x,x,1595)
		VERT(x,x,1745)
		VERT(x,x,1895)
		VERT(x,x,2245)
	next
	restarea(aarea)

	oPrn:line( 80*40, 77, 80*40, 2322)
	VERT(80,82,0)
	VERT(80,82,2245)
	oPrn:line( 83*40-1, 77, 83*40-1, 2322)

 //	xSAY(80,03, "Observa��o:      Aten��o!!!!! Para ordem de servi�o de manuten��o � obrigat�rio o preenchimento do Relat�rio de Servi�o Mec�nico.",1)
	if alltrim(mv_par12)<>""
		xSAY (82,03,mv_par12,1)
	EndIf 
	nLin:=100
	oPrn:EndPage()
	dbSkip() // Avanca o ponteiro do registro no arquivo
EndDo
oPrn:EndPage()
oPrn:Preview()
SetPgEject(.F.)
MS_Flush()
IF FILE(carq)
	FErase(cArq) 
endif

Return                     

Static Function ImpCab(nLin,carq)              
oPrn:SayBitmap( 45, 85,carq,263,68 )
oPrn:line( 40, 77, 40, 2322)

oPrn:line( 80, 377, 80, 2322)
VERT(1,2,300)
for x:=3 to 80
	oPrn:line( x*40, 77, x*40, 2322)
next
//for x:=0 to 239
//	oPrn:line( 0, 77+X*100, 40, 77+X*100)
//next
//for x:=1 to 82
//	oPrn:Say ( X*40, 0, STR(X),oFont2,100)
//next         
VERT(1,2,0)
VERT(1,1,1595)
VERT(1,1,1895)
VERT(1,1,2095)
VERT(1,2,2245)
VERT(4,7,0)
VERT(4,7,2245)
VERT(9,12,0)
VERT(9,12,2245)
VERT(13,16,0)
VERT(13,16,2245)
xSay(1, 500, "ORDEM DE SERVICO ",2)
XSay ( 1, 1600, "N."+STJ->TJ_ORDEM,2)
xSay ( 1, 1900, "Emissao",3)
xSay ( 1,2100,DTOC(STJ->TJ_DTORIGI),1)
xSay ( 2,303,SM0->M0_NOMECOM,3)
VERT(2,2,1895)
VERT(2,2,2095)
xSay ( 2,1900,"Status",3)

nPag++
IF STJ->TJ_SITUACA=="C"
	cSt:="Cancelado"
elseif STJ->TJ_SITUACA=="L"
	cSt:="Liberado"
else
	cSt:="Pendente"
endif
xSay ( 2,2100,cSt,1)
xSay(4,03,STJ->TJ_PLANO+"-"+RETFIELD("STI",1,XFILIAL("STI")+STJ->TJ_PLANO,'TI_DESCRIC'),1)
xSay ( 4,1100,STJ->TJ_TIPO+"-"+RETFIELD("STE",1,XFILIAL("STE")+STJ->TJ_TIPO,'TE_NOME'),1)
xSay ( 5,03,"Servico",3)
xSay ( 5,300,RETFIELD("ST4",1,XFILIAL("ST4")+STJ->TJ_SERVICO,'T4_NOME'),1)
VERT(5,5,1895)
VERT(5,5,2095)
xSay ( 5, 1900, "FREQ",3)
// xSay ( 5, 2100, str(RETFIELD("SZP",1,+XFILIAL("SZP")+STRZERO(STJ->TJ_SEQUENC,3),"ZP_INMANUT"),6),1)
xSay ( 5, 2100, str(RETFIELD("STF",1,+XFILIAL("STF")+STJ->TJ_CODBEM+TJ_SERVICO+TJ_SEQRELA,"TF_INENMAN"),6),1)
xSay ( 6,03, "C.Trabalho",3)
xSay ( 6,300,STJ->TJ_CENTRAB+"-"+SUBSTR(RETFIELD("SHB",1,+XFILIAL("SHB")+STJ->TJ_CODBEM ,"HB_NOME"),1,20),1)
xSay ( 7,03, "Area",3)
cCodArea:=alltrim(RETFIELD("ST4",1,XFILIAL("ST4")+STJ->TJ_SERVICO,'T4_CODAREA'))
cDescArea:=alltrim(RETFIELD("STD",1,XFILIAL("STD")+cCodArea,'TD_NOME'))
cTipoMan:=alltrim(RETFIELD("ST4",1,XFILIAL("ST4")+STJ->TJ_SERVICO,'T4_TIPOMAN'))
xSay ( 7,300,cCodArea+"-"+cDescArea+"-"+cTipoMan,1)
xSay ( 9,750,"DADOS DO EQUIPAMENTO",2)
xSay ( 10,03,ALLTRIM(STJ->TJ_CODBEM)+"  ---  "+RETFIELD("ST9",1,+XFILIAL("ST9")+STJ->TJ_CODBEM,"T9_NOME"),3)
xSay ( 11,03,"Modelo",3)
VERT(11,11,445)
VERT(11,11,915)
VERT(11,11,1415)
VERT(11,11,1895)
xSay ( 11,450,"Serie",3)
xSay ( 11,920,"Cont.Acumulado",3)
xSay ( 11,1420,"Contador Atual",3)
xSay ( 11,1900,"Data da Leitura",3)
VERT(12,12,445)
VERT(12,12,915)
VERT(12,12,1415)
VERT(12,12,1895)
xSay ( 12,03,RETFIELD("ST9",1,+XFILIAL("ST9")+STJ->TJ_CODBEM,"T9_MODELO"),1)
xSay ( 12,450,RETFIELD("ST9",1,+XFILIAL("ST9")+STJ->TJ_CODBEM,"T9_SERIE"),1)
nAcum:=RETFIELD("ST9",1,+XFILIAL("ST9")+STJ->TJ_CODBEM,"T9_CONTACU")
nPos:=RETFIELD("ST9",1,+XFILIAL("ST9")+STJ->TJ_CODBEM,"T9_POSCONT")
xSay ( 12,920,TRANSFORM(nAcum ,"@E 999,999,999"),1)
xSay ( 12,1420,TRANSFORM(nPos, "@E 999,999,999"),1)
xSay ( 12,1900,DTOC(RETFIELD("ST9",1,+XFILIAL("ST9")+STJ->TJ_CODBEM,"T9_DTULTAC")),1)
nPosCon:=RETFIELD("STF",1,+XFILIAL("STF")+STJ->TJ_CODBEM+STJ->TJ_SERVICO+STJ->TJ_SEQRELA,"TF_CONMANU")
IncMan :=RETFIELD("STF",1,+XFILIAL("STF")+STJ->TJ_CODBEM+STJ->TJ_SERVICO+STJ->TJ_SEQRELA,"TF_INENMAN")
VERT(13,13,445)
VERT(13,13,915)
VERT(13,13,1415)
//VERT(13,13,1895)
//xSay ( 13,03,RETFIELD("STF",1,+XFILIAL("STF")+STJ->TJ_CODBEM,"TF_CONMANU"),1)
xSay ( 13,03,"Cont. Acum. Ult. Manut:",3)
xSay ( 13,450,TRANSFORM(nPosCon, "@E 999,999,999"),1)
xSay ( 13,920,"Cont. Acum. Prox. Manut:",3)
xSay ( 13,1420,TRANSFORM(nPosCon+IncMan, "@E 999,999,999"),1)
xSay ( 14,750,"REGISTRO DA OS",2)
VERT(15,15,445)
VERT(15,15,915)
VERT(15,15,1415)
VERT(15,15,1895)
VERT(16,16,445)
VERT(16,16,915)
VERT(16,16,1415)
VERT(16,16,1895)
xSay ( 15,03,"Leitura Execucao",3)
xSay ( 15,450,"Data Inicio",3)
xSay ( 15,920,"Hora Inicio",3)
xSay ( 15,1420,"Data Termino",3)
xSay ( 15,1900,"Hora Termino",3)
nLin:=18             
if alltrim(STJ->TJ_OBSERVA)<>""
	VERT(18,18,0)
	VERT(18,18,2245)
	xSay ( nLin,750,"OBSERVACOES",2)
	nLin:=19             
	xobs:=alltrim(STJ->TJ_OBSERVA)
	For xx:=1 to MlCount(xobs,132)
		VERT(nlin,nlin,0)
		VERT(nlin,nlin,2245)
		xSAY(nLin,0,MemoLine(xobs,132,xx),1)
		nLin++
	next
endif
nLin++
VERT(nlin,nlin,0)
VERT(nlin,nlin,2245)
xSay ( nlin,750,"TAREFAS A SEREM REALIZADAS",2)    
nLin++
VERT(nlin,nlin,0)
xSay ( nlin,03,"Tarefa",3)
VERT(nlin,nlin,195)
xSay ( nlin,200,"Descricao",3)
VERT(nlin,nlin,1415)
xSay ( nlin,1420,"Executor",3)
VERT(nlin,nlin,1895)
xSay ( nlin,1900,"Data",3)
VERT(nlin,nlin,2245)
nLin++

return


	//���������������������������������,�
//�Imprime cabecalho de continuacao�
//���������������������������������,�

Static Function ImpCab1(nLin)
nLin:=1
xSAY(nLin,03, "ORDEM DE SERVICO "+STJ->TJ_ORDEM+" - CONTINUACAO",1)
nLin++
//for x:=3 to 50
for x:=3 to ntamlin
	oPrn:line( x*40, 77, x*40, 2322)
next
return


Static Function ImpCab2(nLin)
VERT(nlin,nlin,0)
VERT(nlin,nlin,2245)
xSay ( nlin,750,"MATERIAIS E PECAS APLICADOS",2)
nLin++
VERT(nlin,nlin,0)
VERT(nlin,nlin,195)
VERT(nlin,nlin,445)
VERT(nlin,nlin,1595)
VERT(nlin,nlin,1745)
VERT(nlin,nlin,1895)
VERT(nlin,nlin,2245)

xSAY(nlin,03, "Tarefa",3)
xSAY(nlin,200,"TP-Codigo",3)
xSAY(nlin,450,"Descricao",3)
xSAY(nlin,1600,"Quant.",3)
xSAY(nlin,1750,"UM",3)
xSAY(nlin,1900,"Vlr.Unitario",3)
nLin++
return

Static Function custo(COD)
local ret:=0
local area:=getarea()
dbselectarea("SB2")
DBSETORDER(1)
dbseek(xfilial("SB2")+cod,.t.)
nquant:=0
nval:=0
while !eof() .and. xfilial("SB2")+cod==SB2->B2_FILIAL+SB2->B2_COD
	nquant+=SB2->B2_QATU
	nval+=SB2->B2_VATU1
	DBSKIP()
enddo     
RET:=nval/nquant
restarea(area)
return(ret)

/*/
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
�������������������������������������������������������������������������ͻ��
���Fun��o    �VALIDPERG � Autor � AP5 IDE            � Data �  17/12/02   ���
�������������������������������������������������������������������������͹��
���Descri��o � Verifica a existencia das perguntas criando-as caso seja   ���
���          � necessario (caso nao existam).                             ���
�������������������������������������������������������������������������͹��
���Uso       � Programa principal                                         ���
�������������������������������������������������������������������������ͼ��
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
/*/

Static Function ValidPerg

Local _sAlias := Alias()
Local aRegs := {}
Local i,j

dbSelectArea("SX1")
dbSetOrder(1)
cPerg := PADR(cPerg,10)

// Grupo/Ordem/Pergunta/Variavel/Tipo/Tamanho/Decimal/Presel/GSC/Valid/Var01/Def01/Cnt01/Var02/Def02/Cnt02/Var03/Def03/Cnt03/Var04/Def04/Cnt04/Var05/Def05/Cnt05
aAdd(aRegs,{cPerg,"01","Do Plano             ?","","","mv_ch1","C",6,0,0,"G","","mv_par01","","","","","","","","","","","","","","","","","","","","","","","","","STI",""})
aAdd(aRegs,{cPerg,"02","Da Data              ?","","","mv_ch2","D",8,0,0,"G","","mv_par02","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"03","Ate a Data           ?","","","mv_ch3","D",8,0,0,"G","","mv_par03","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"04","Do Centro de Custo   ?","","","mv_ch4","C",9,0,0,"G","","mv_par04","","","","","","","","","","","","","","","","","","","","","","","","","CTT",""})
aAdd(aRegs,{cPerg,"05","Ate o Centro de Custo?","","","mv_ch5","C",9,0,0,"G","","mv_par05","","","","","","","","","","","","","","","","","","","","","","","","","CTT",""})
aAdd(aRegs,{cPerg,"06","Do Centro Trabalho   ?","","","mv_ch6","C",6,0,0,"G","","mv_par06","","","","","","","","","","","","","","","","","","","","","","","","","SHB",""})
aAdd(aRegs,{cPerg,"07","Ate Centro Trabalho  ?","","","mv_ch7","C",6,0,0,"G","","mv_par07","","","","","","","","","","","","","","","","","","","","","","","","","SHB",""})
aAdd(aRegs,{cPerg,"08","Do Bem               ?","","","mv_ch8","C",16,0,0,"G","","mv_par08","","","","","","","","","","","","","","","","","","","","","","","","","ST9",""})
aAdd(aRegs,{cPerg,"09","Ate o Bem            ?","","","mv_ch9","C",16,0,0,"G","","mv_par09","","","","","","","","","","","","","","","","","","","","","","","","","ST9",""})
aAdd(aRegs,{cPerg,"10","Da Ordem             ?","","","mv_cha","C",6,0,0,"G","","mv_par10","","","","","","","","","","","","","","","","","","","","","","","","","STJ",""})
aAdd(aRegs,{cPerg,"11","Ate a Ordem          ?","","","mv_chb","C",6,0,0,"G","","mv_par11","","","","","","","","","","","","","","","","","","","","","","","","","STJ",""})
aAdd(aRegs,{cPerg,"12","Observacao           ?","","","mv_chc","C",70,0,0,"G","","mv_par12","","","","","","","","","","","","","","","","","","","","","","","","","",""})
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
STATIC FUNCTION XSAY(L,C,T,F)
	oPrn:Say (L*40-IIF(F=2,7,0), 77+C,T,iif(F=1,oFont2,IIF(F=2,oFont20,oFont23)),100)
RETURN
STATIC FUNCTION VERT(L1,L2,C)
	oPrn:line( 40*L1, 77+C, 40*L2+40, 77+C)
RETURN