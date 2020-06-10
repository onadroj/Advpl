#include "rwmake.ch"        // incluido pelo assistente de conversao do AP5 IDE em 27/03/00

User Function Lctofi()        // incluido pelo assistente de conversao do AP5 IDE em 27/03/00

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Declaracao de variaveis utilizadas no programa atraves da funcao    ³
//³ SetPrvt, que criara somente as variaveis definidas pelo usuario,    ³
//³ identificando as variaveis publicas do sistema utilizadas no codigo ³
//³ Incluido pelo assistente de conversao do AP5 IDE                    ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

SetPrvt("CSAVCUR1,CSAVROW1,CSAVCOL1,CSAVCOR1,CSAVSCR1,CSAVSCR2")
SetPrvt("ODLG,NOPCA,NLASTKEY,ACRA,ACA,CSAVCORM")
SetPrvt("NOMEPROG,NTOTREGS,LGERATIT,CARQUIVO,CLOTE,NTOTAL")
SetPrvt("LHEAD,LRODA,LCTOCONT,ESC,VECTO,NUMTIT")
SetPrvt("SEMANA,FILIALDE,FILIALATE,CCDE,CCATE,MATDE")
SetPrvt("MATATE,NOMDE,NOMATE,CSITUACAO,CCATEGORIA,LINHA1")
SetPrvt("LAGLUTINA,CCODPAD1,CCODPAD2,CCODPAD3,CNATUREZA,DDIASFERDE")
SetPrvt("DDIASFERAT,NRESCISOES,DDTIR,DDTINSS,DDTFGTS,CCODIGOS")
SetPrvt("CPREFFOL,CFORNFOL,CTIPOFOL,CNATUFOL,CFLUXO,CPREFINS")
SetPrvt("CFORNINS,CTIPOINS,CNATUINS,CPREFIR,CFORNIR,CTIPOIR")
SetPrvt("CNATUIR,CPREFFGT,CFORNFGT,CTIPOFGT,CNATUFGT,CPREFPEN")
SetPrvt("CFORNPEN,CTIPOPEN,CNATUPEN,CPARCELA,CVERBA,NFOR")
SetPrvt("CINICIO,CFIM,NVALOR,NVLFGTS,NVLIRFOL,NVLINSS")
SetPrvt("NVALPENR,NVALPENF,NVALPENSAO,CHAVSRC,CHAVSRA,CTIPO")
SetPrvt("CFORNEC,CHAVSRI,CPREFIXO,NVLIR,NVLISS,CAREA")
SetPrvt("NREC,CIND,CNREDUZ,CNREDUZPEN,LCALCIR,LCALCISS")
SetPrvt("NSAVREC,NHDLPRV,")

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡ao    ³LCTOFI    ³ Autor ³MAURICIO/GATASSE       ³ Data ³25.02.00  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡Æo ³Lacto de Enc.Folha, Ferias, Pensao, 13§Sal no Financeiro    ³±±
±±³          ³                                                            ³±±
±±³          ³                                                            ³±±
±±³          ³                                                            ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Uso       ³ROTINA                                                      ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Tabelas   ³SX1,SX5,SA2,SE2,SRA,SRC,SRI,SRR                             ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Modulo    ³FOLHA DE PAGAMETO / FINANCEIRO                              ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Programa  ³ LCTOFIN  ³ Autor ³ Marcos Antonio A Matos³ Data ³ 02.06.98 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡„o ³ Lacto de Enc.Folha, Ferias, Pensao, 13§Sal no Financeiro   ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Sintaxe   ³ Chamada padr„o para programas em RDMake.                   ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Uso      ³ RdMake                                                     ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³         ATUALIZACOES SOFRIDAS DESDE A CONSTRU€AO INICIAL.             ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Programador ³ Data   ³ BOPS ³  Motivo da Alteracao                     ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³M.machado   ³27/07/99³      ³ Adequacao para a Paraibuna               ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß*/

IF !(SX1->(dbSeek("GPELCA24",.F.)))
	GravaPerg()
EndIF

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Define Variaveis Locais do Programa                          ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
#IFNDEF WINDOWS
	cSavCur1:=""
	cSavRow1:=""
	cSavCol1:=""
	cSavCor1:=""
	cSavScr1:=""
	cSavScr2:=""
#ELSE
	oDlg:=""
#ENDIF

nOpca := 0
nLastKey  := 0

#IFDEF WINDOWS
	
	Pergunte("GPELCA",.T.)
	
	#IFNDEF WINDOWS
		Inkey()
		ALERT("CANCELADO PELO OPERADOR")
		If Lastkey() == 286
			Return
		EndIf
		If Lastkey() == 27
			Return
		EndIf
	#ENDIF
	
	LCTOProcessa()
	
#ELSE
	
	aCRA:= { "Confirma","Redigita","Abandona" }
	aCA := { "Continua","Abandona"}
	cSavCorM        := SetColor()
	
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Salva a Integridade dos dados de Entrada                                     ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	cSavScr1 := SaveScreen(3,0,24,79)
	cSavCur1 := SetCursor(0)
	cSavRow1 := ROW()
	cSavCol1 := COL()
	cSavCor1 := SetColor("bg+/b,,,")
	
	DispBegin()
	ScreenDraw("SMT250", 3, 0, 0, 0)
	@ 03,01 Say "Gera Lancto no Contas a Pagar" Color "b/w"
	SetColor("n/w,,,")
	@ 17,05 SAY Space(71)
	DispEnd()
	
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Descricao generica do programa                                                               ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	SetColor("b/bg")
	@ 10,05 Say "Gera Lancto no Contas a Pagar"
	Inkey(0)
	
	While .T.
		//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
		//³ Carrega as Perguntas selecionadas                                                    ³
		//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		PERGUNTE("GPELCA",.T.)
		
		DispBegin()
		ScreenDraw("SMT250", 3, 0, 0, 0)
		@ 03,01 Say "Gera Lancto no Contas a Pagar" Color "b/w"
		SetColor("n/w,,,")
		@ 17,05 SAY Space(71)
		DispEnd()
		
		//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
		//³ Descricao generica do programa                                                               ³
		//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
		SetColor("b/bg")
		@ 10,05 Say "Gera Lancto no Contas a Pagar "
		nOpcA:=menuh(aCRA,17,6,"b/w,w+/n,r/w","CRA","",1)
		If nOpcA == 2
			Loop
		Endif
		Exit
	EndDo
	If nOpca == 1
		LCTOProcessa()
	Endif
	RestScreen(3,0,24,79,cSavScr1)
	SetCursor(cSavCur1)
	DevPos(cSavRow1,cSavCol1)
	SetColor(cSavCor1)
#ENDIF



// Substituido pelo assistente de conversao do AP5 IDE em 27/03/00 ==> Function LCTOProcessa
Static Function LCTOProcessa()

nomeprog  := 'LCTOFIN'

If nLastKey == 27
	ALERT("CANCELADO PELO OPERADOR")
	Return Nil
Endif

nTotregs := 0 //-- Regua.
lGeraTit := .F.
cArquivo :=""
cLote    :=Space(4)
nTotal   :=0
lHead           := .T.
lRoda           := .F.

dbSelectArea( "SX5" )
dbSeek( xFilial("SX5") + "09" + "GPE" )
If Eof( )
	Help( " ", 1, "NOLOTCONT" )
	cLote := Space( 4)
Else
	cLote := Substr( SX5 -> X5_DESCRI,1,4)
EndIf

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Carregando variaveis mv_par?? para Variaveis do Sistema.     ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Variaveis utilizadas para parametros                             ³
//³ mv_par01   a    //  Faz Lacto Contabil                           ³
//³ mv_par02   b    //  Fazer Lactos (Enc.Folha/1¦/2¦/Pensao/Ferias) ³
//³ mv_par03   c    //  Data Vecto                                   ³
//³ mv_par04   d    //  1o.Num.Titulo                                ³
//³ mv_par05   e    //  Numero da Semana                             ³
//³ mv_par06   f    //  Filial De                                    ³
//³ mv_par07   g    //  Filial Ate                                   ³
//³ mv_par08   h    //  Centro de Custo De                           ³
//³ mv_par09   i    //  Centro de Custo Ate                          ³
//³ mv_par10   j    //  Matricula De                                 ³
//³ mv_par11   k    //  Matricula Ate                                ³
//³ mv_par12   l    //  Nome De                                      ³
//³ mv_par13   m    //  Nome Ate                                     ³
//³ mv_par14   n    //  Situacoes                                    ³
//³ mv_par15   o    //  Categorias                                   ³
//³ mv_par16   p    //  Linha 1 -- Codigos da folha                  ³
//³ mv_par17   q    //  Aglutina Lactos                              ³
//³ mv_par18   r    //  Cod.Lcto P.Tit  '                            ³
//³ mv_par19   s    //  Cod.Lcto P.IR                                ³
//³ mv_par20   t    //  Cod.Lcto P.ISS                               ³
//³ mv_par21   u    //  Natureza                                     ³
//³ mv_par22   v    //  Pagto Ferias De                              ³
//³ mv_par23   w    //  Pagto Ferias Ate                             ³
//³ mv_par24   x    //  Rescisoes                                    ³
//³ mv_par25   x    //  Data Pagto IR                                ³
//³ mv_par26   x    //  Data Pagto INSS                              ³
//³ mv_par27   x    //  Data Pagto FGTS                              ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

LctoCont  := mv_par01
Esc       := mv_par02
Vecto     := mv_par03
NumTit    := mv_par04
Semana    := mv_par05
FilialDe  := mv_par06
FilialAte := mv_par07
CcDe      := mv_par08
CcAte     := mv_par09
MatDe     := mv_par10
MatAte    := mv_par11
NomDe     := mv_par12
NomAte    := mv_par13
cSituacao := mv_par14
cCategoria:= mv_par15
Linha1   := AllTrim(mv_par16)
lAglutina := IF(mv_par17==1,.T.,.F.)
cCodPad1  := mv_par18
cCodPad2  := mv_par19
cCodPad3  := mv_par20
cNatureza := mv_par21
dDiasFerDe:= mv_par22
dDiasFerAt:= mv_par23
nRescisoes:= mv_par24
dDtIR     := mv_par25
dDtINSS   := mv_par26
dDtFGTS   := mv_par27
cCodigos := AllTrim(Linha1)

cPrefFol := ""
cFornFol := ""
cTipoFol := ""
cNatuFol := ""
cFluxo   := ""

cPrefIns := ""
cFornIns := ""
cTipoIns := ""
cNatuIns := ""
cFluxo   := ""

cPrefIr  := ""
cFornIr  := ""
cTipoIr  := ""
cNatuIr  := ""
cFluxo   := ""

cPrefFgt := ""
cFornFgt := ""
cTipoFgt := ""
cNatuFgt := ""
cFluxo   := ""

cPrefPen := ""
cFornPen := ""
cTipoPen := ""
cNatuPen := ""
cFluxo   := ""

cParcela := ""
// Separa os Codigos das verbas solicitadas a listar
cVerba := ""
For nFor := 1 To Len(cCodigos) Step 3
	cVerba := cVerba + Subs(cCodigos,nFor,3)
	If Len(cCodigos) > ( nFor+3 )
		cVerba := cVerba + "/"
	Endif
Next

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Selecionando a Ordem de impressao escolhida no parametro.    ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
dbSelectArea('SRA')
dbSetOrder(1)
dbSeek(FilialDe + MatDe,.T.)
cInicio := 'SRA->RA_FILIAL + SRA->RA_MAT'
cFim    := FilialAte + MatAte

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Carrega Regua Processamento                                  ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
SetRegua(RecCount()) //-- Total de elementos da regua.

dbSelectArea('SRA')
Do While !EOF() .And. &cInicio <= cFim
	
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Movimenta Regua Processamento                                ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	IncRegua()  //-- Move a regua.
	
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Cancela ImpresÆo ao se pressionar <ALT> + <A>                ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	
	#IFNDEF WINDOWS
		Inkey()
		If Lastkey() == 286
			Return
		EndIf
		If Lastkey() == 27
			Return
		EndIf
	#ENDIF
	
	SetRegua(RecCount())
	
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Consiste Parametrizacao do Intervalo de Impressao            ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	If (SRA->RA_NOME < NomDe)    .Or. (SRA->RA_NOME > NomAte)    .Or. ;
		(SRA->RA_MAT < MatDe)     .Or. (SRA->RA_MAT > MatAte)     .Or. ;
		(SRA->RA_CC < CcDe)       .Or. (SRA->RA_CC > CcAte)
		SRA->(dbSkip(1))
		Loop
	EndIf
	
	//-- Despreza registros conforme situacao e categoria dos funcionarios.
	If !( SRA->RA_SITFOLH $ cSituacao ) .Or.  !( SRA->RA_CATFUNC $ cCategoria )
		dbSkip()
		Loop
	Endif
	
	If SRA->RA_SITFOLH $'D' .And. SUBSTR(DTOS(SRA->RA_DEMISSA),1,6) #SUBSTR(DTOS(dDataBase),1,6)
		dbSkip()
		Loop
	Endif
	
	nValor    :=0
	nVlFgts   :=0
	nVlIrFol  :=0
	nVlInss   :=0
	nValPenR  :=0
	nValPenF  :=0
	nValPensao:=0
	If (Esc == 1 .Or. Esc == 2 .or. Esc == 4) .and. !(nRescisoes == 1 )
		//  Fazer Lactos (1-Folha /2-1¦Parcela /3-2¦Parcela/ 4-Pensao/ 5-Ferias)
		dbSelectArea('SRC')
		If dbSeek(SRA->RA_FILIAL + SRA->RA_MAT)
			
			If Esc == 1 .or. Esc == 2
				ChavSRC := "        "
				ChavSRA := "        "
			ELSE
				ChavSRC := SRC->RC_FILIAL+SRC->RC_MAT
				ChavSRA := SRA->RA_FILIAL+SRA->RA_MAT
			ENDIF
			
			*           Do While !Eof()  // .And. SRC->RC_FILIAL+SRC->RC_MAT == SRA->RA_FILIAL+SRA->RA_MAT
			Do While !Eof() .And. ChavSRC == ChavSRA
				
				Incregua()
				
				If SRC->RC_SEMANA #Semana
					dbSkip()
					Loop
				Endif
				
				If Esc == 1
					
					If SRC->RC_PD == "401" //aCodFol[064,1]
						nVlInss:=(nVlInss+SRC->RC_Valor)
						cFornIns := "003981"
						cPrefIns := "FOL"
						cTipoIns := "INSS"
						cNatuIns := "20401"
						cFluxo   := ""
						cTipo    := cTipoIns
						cFornec  := cFornIns
					Endif
					If SRC->RC_PD == "405" //aCodFol[066,1]
						nVlIrFol:=(nVlIrFol+SRC->RC_Valor)
						cFornIr  := "010507"
						cPrefIr  := "FOL"
						cTipoIr  := "TX"
						cNatuIr  := "20605"
						cFluxo   := ""
						cTipo    := cTipoIr
						cFornec  := cFornIr
					Endif
					If SRC->RC_PD == "732" //aCodFol[018,1]
						nVlFgts:=(nVlFgts+SRC->RC_Valor)
						cFornFgt := "000752"
						cPrefFgt := "FOL"
						cTipoFgt := "FGTS"
						cNatuFgt := "20403"
						cFluxo   := ""
						cTipo    := cTipoFgt
						cFornec  := cFornFgt
					Endif
					If SRC->RC_PD == "799" //aCodFol[047,1]
						nValor :=(nValor +SRC->RC_Valor)
						cFornFol := "010000"
						cPrefFol := "FOL"
						cTipoFol := "FOL "
						cNatuFol := "20202"
						cFluxo   := ""
						cTipo    := cTipoFol
						cFornec  := cFornFol
					Endif
					
				ElseIf Esc == 2
					
					If SRC->RC_PD == "137" //aCodFol[022,1]
						nValor   :=(nValor+SRC->RC_Valor)
						cFornFol := "010506"
						cPrefFol := "FOL"
						cTipoFol := "131"
						cNatuFol := "20206"
						cFluxo   := ""
						cTipo    := cTipoFol
						cFornec  := cFornFol
					Endif
					
					If SRC->RC_PD == "739" //aCodFol[109,1]
						nVlFgts  :=(nVlFgts+SRC->RC_Valor)
						cFornFgt := "000752"
						cPrefFgt := "13S"
						cTipoFgt := "FGTS"
						cNatuFgt := "20403"
						cFluxo   := ""
						cTipo    := cTipoFgt
						cFornec  := cFornFgt
					Endif
					
				Else
					If SRC->RC_PD $ "448*450"       //aCodFol[128,1]  aCodFol[056,1]
						nValPensao:=( nValPensao + SRC->RC_Valor )
						cFornFol  := "010046"
						cPrefFol  := "FOL"
						cTipoFol  := "PEN"
						cNatuFol  := "20309"
						cFluxo    := "S"
						cTipo     := cTipoFol
						cFornec   := cFornFol
					Endif
					
				EndIf
				dbSkip()
			Enddo
		Endif
	Elseif Esc == 3 .and. !( nRescisoes == 1 )
		dbSelectArea('SRI')
		dbSetOrder(2)
		If dbSeek(SRA->RA_FILIAL+SRA->RA_CC+SRA->RA_MAT)
			
			If Esc == 3
				ChavSRI := "                 "
				ChavSRA := "                 "
			ELSE
				ChavSRI := SRI->RI_FILIAL+SRI->RI_CC+SRI->RI_MAT
				ChavSRA := SRA->RA_FILIAL+SRA->RA_CC+SRA->RA_MAT
			ENDIF
			
			IncRegua()
			
			*           Do While !Eof() .And. SRA->RA_FILIAL+SRA->RA_CC+SRA->RA_MAT == SRI->RI_FILIAL+SRI->RI_CC+SRI->RI_MAT
			Do While !Eof() .And. ChavSRA == ChavSRI
				If SRI->RI_PD == "709" //aCodFol[021,0]
					nValor := (nValor+SRI->RI_Valor)
					cFornFol := "010506"
					cPrefFol := "13S"
					cTipoFol := "132"
					cNatuFol := "20206"
					cFluxo   := ""
					cTipo    := cTipoFol
					cFornec  := cFornFol
				Endif
				If SRI->RI_PD == "739" //aCodFol[109,0]
					nVlFgts := (nVlFgts+SRI->RI_Valor)
					cFornFgt := "000752"
					cPrefFgt := "13S"
					cTipoFgt := "FGTS"
					cNatuFgt := "20403"
					cFluxo   := ""
					cTipo    := cTipoFgt
					cFornec  := cFornFgt
				Endif
				If SRI->RI_PD == "403" //aCodFol[071,0]
					nVlIrFol := (nVlIrFol+SRI->RI_Valor)
					cFornIr  := "010507"
					cPrefIr  := "13S"
					cTipoIr  := "TX"
					cNatuIr  := "20605"
					cFluxo   := ""
					cTipo    := cTipoIr
					cFornec  := cFornIr
				Endif
				If SRI->RI_PD == "402" //aCodFol[070,0]
					nVlInss  := (nVlInss+SRI->RI_Valor)
					cFornIns := "003981"
					cPrefIns := "13S"
					cTipoIns := "INSS"
					cNatuIns := "20401"
					cFluxo   := ""
					cTipo    :=  cTipoIns
					cFornec  := cFornIns
				Endif
				dbSkip()
			Enddo
		EndIf
	Elseif Esc == 5 .and. !( nRescisoes == 1 )
		set softseek on
		dbSelectArea('SRR')
		dbSetOrder(1)
		If dbSeek(SRA->RA_FILIAL+SRA->RA_MAT+"F"+DTOS( dDiasFerDe )+"404")
			set softseek off
			Do While !Eof() .And. SRA->RA_FILIAL+SRA->RA_MAT == SRR->RR_FILIAL+SRR->RR_MAT;
				.And. SRR->RR_DATA >= dDiasFerDe .And. SRR->RR_DATA <= dDiasFerAte
				If SRR->RR_PD == "404" //aCodFol[102,1]
					nValor   := SRR->RR_Valor
					cFornFol := "010500"
					cPrefFol := "FER"
					cTipoFol := "FER"
					cNatuFol := "20205"
					cFluxo   := "S"
					cTipo    := cTipoFol
					cFornec  := cFornFol
				Endif
				If SRR->RR_PD == "495" //aCodFol[067,1]
					nVlIrFol := SRR->RR_Valor
					cFornIr  := "010507"
					cPrefIr  := "FER"
					cTipoIr  := "TX"
					cNatuIr  := "20605"
					cFluxo   := ""
					cTipo    := cTipoIr
					cFornec  := cFornIr
				Endif
				If SRR->RR_PD == "449" //aCodFol[170,1]
					nValPenF := SRR->RR_Valor
					cFornPen := "010046"
					cPrefPen := "FER"
					cTipoPen := "PEN"
					cNatuPen := "20309"
					cFluxo    := "S"
					cTipo    := cTipoPen
					cFornec  := cFornPen
				Endif
				dbSkip()
			Enddo
		EndIf
	Elseif nRescisoes ==  1
		dbSelectArea('SRR')
		dbSetOrder(1)
		If dbSeek(SRA->RA_FILIAL+SRA->RA_MAT+"R")
			Do While !Eof() .And. SRA->RA_FILIAL+SRA->RA_MAT == SRR->RR_FILIAL+SRR->RR_MAT;
				.And. SRR->RR_DATA >= dDiasFerDe .And. SRR->RR_DATA <= dDiasFerAte
				If SRR->RR_PD == "496" //aCodFol[126,1]
					nValor   := (nValor + SRR->RR_Valor)
					cFornFol := "010508"
					cPrefFol := "RES"
					cTipoFol := "RES"
					cNatuFol := "20201"
					cFluxo   := ""
					cTipo    := cTipoFol
					cFornec  := cFornFol
				Endif
				If SRR->RR_PD $ "403*405*495" //aCodFol[071,1]  aCodFol[066,1]  aCodFol[067,1]
					
					nVlIrFol := (nVlIrFol + SRR->RR_Valor)
					cFornIr  := "010507"
					cPrefIr  := "RES"
					cTipoIr  := "TX"
					cNatuIr  := "20605"
					cFluxo   := ""
					cTipo    := cTipoIr
					cFornec  := cFornIr
					
				Endif
				If SRR->RR_PD $ "745*771*772" //aCodFol[117,1]  aCodFol[120,1]  aCodFol[119,1]
					
					nVlFgts:=(nVlFgts + SRR->RR_Valor)
					cFornFgt := "000752"
					cPrefFgt := "RES"
					cTipoFgt := "FGTS"
					cNatuFgt := "20403"
					cFluxo   := ""
					cTipo    := cTipoFgt
					cFornec  := cFornFgt
					
				Endif
				If SRR->RR_PD $ "448*449*450" //aCodFol[128,1]  aCodFol[170,1]  aCodFol[056,1]
					
					nValPenR :=(nValPenR + SRR->RR_Valor)
					cFornPen := "010046"
					cPrefPen := "RES"
					cTipoPen := "PEN"
					cNatuPen := "20309"
					cFluxo   := "S"
					cTipo    := cTipoPen
					cFornec  := cFornPen
					
				Endif
				dbSkip()
			Enddo
		Endif
	Endif
	
	dbSelectArea('SRA')
	
	If (nValor+nVlInss+nVlFgts+nVlIrFol+nValPensao+nValPenR+nValPenF) > 0
		
		cPrefixo:="FOL"
		
		nVlIR:=0
		nVlISS:=0
		
		cArea:=Alias()
		nRec:=Recno()
		cInd:=IndexOrd()
		dbSelectArea("SX5")
		dbSetorder(1)
		If dbSeek(xFilial("SX5")+"96BEN")
			NumTit     := Substr(SX5->X5_DESCRI,Len(SX5->X5_DESCRI)-5,6)
			lGeraTit   := .T.
		else
			#IFDEF WINDOWS
				MsgStop("Nao achou o cadastro de natureza, lancamento no financeiro abortado.")
			#ELSE
				Alert("Nao achou o cadastro de natureza, lancamento no financeiro abortado.")
			#ENDIF
			dbSelectArea(cArea)
			dbSetOrder(nRec)
			dbGoto(cInd)
			Return(.F.)
		Endif
		
		If lGeraTit
			*                       NumTit:=StrZero(Val(NumTit)+1,6)
			NumTit:=StrZero(Val(NumTit),6)
		Endif
		
		dbSelectArea("SE2")
		dbSetOrder(1)
		
		If dbSeek( xFilial("SE2") + cPrefixo + NumTit + " " + cTipo + cFornec + "01")
			HELP( ' ',1,'LCTOJAEXIT' )
			Return Nil
			*                       dbSelectArea("SRA")
			*                       dbSkip()
			*                       Loop
		Else
			cNReduz:=Space(20)
			dbSelectArea("SA2")
			Do Case
				Case cPrefFol == "FOL"
					cFornec := "010000"
				Case cPrefFol == "FER"
					cFornec := "010500"
				Case cPrefFol == "RES"
					cFornec := "010508"
				Case cPrefFol == "13S"
					cFornec := "010506"
			EndCase
			dbsetorder(1)			
			dbSeek(xFilial("SA2") + cFornec,.t.)
			if cFornec == SA2->A2_COD
				cNReduz:=SA2->A2_NREDUZ
			Endif
			
			If ( nValPensao+nValPenR+nValPenF ) > 0.00
				
				dbSeek(xFilial("SA2") + "010046" )
				IF cFornec == "010046"
					cNReduzPen:=SA2->A2_NREDUZ
				Endif
				
			EndIf
			
			If nValor > 0.00
				
				dbSelectArea( "SE2" )
				Reclock("SE2",.T.)
				
				SE2->E2_FILIAL      := XFilial("SE2")
				SE2->E2_PREFIXO     := cPrefFol
				SE2->E2_NUM         := NumTit
				SE2->E2_TIPO        := cTipoFol
				SE2->E2_PARCELA     := cParcela
				SE2->E2_NATUREZ     := cNatuFol
				SE2->E2_FORNECE     := cFornFol
				SE2->E2_LOJA        := SA2->A2_LOJA
				SE2->E2_NOMFOR      := cNReduz
				SE2->E2_EMISSAO     := dDataBase
				SE2->E2_VENCTO      := Vecto
				SE2->E2_VENCREA     := DataValida(Vecto,.T.)
				SE2->E2_VALOR       := nValor
				SE2->E2_VLCRUZ      := nValor
				SE2->E2_MOEDA       := 1
				SE2->E2_SALDO       := nValor
				SE2->E2_VENCORI     := Vecto
				SE2->E2_FLUXO       := cFluxo
				
			EndIf
			
			If nValPensao  > 0.00
				
				dbSelectArea( "SE2" )
				Reclock("SE2",.T.)
				
				SE2->E2_FILIAL      := XFilial("SE2")
				SE2->E2_PREFIXO     := cPrefFol
				SE2->E2_NUM         := NumTit
				SE2->E2_TIPO        := cTipoFol
				SE2->E2_PARCELA     := cParcela
				SE2->E2_NATUREZ     := cNatuFol
				SE2->E2_FORNECE     := cFornFol
				SE2->E2_LOJA        := SA2->A2_LOJA
				SE2->E2_NOMFOR      := cNReduzPen
				SE2->E2_EMISSAO     := dDataBase
				SE2->E2_VENCTO      := Vecto
				SE2->E2_VENCREA     := DataValida(Vecto,.T.)
				SE2->E2_VALOR       := nValPensao
				SE2->E2_VLCRUZ      := nValPensao
				SE2->E2_MOEDA       := 1
				SE2->E2_SALDO       := nValPensao
				SE2->E2_VENCORI     := Vecto
				SE2->E2_FLUXO       := cFluxo
				
			EndIf
			
			If nVlFgts > 0.00
				
				dbSelectArea( "SE2" )
				Reclock("SE2",.T.)
				
				SE2->E2_FILIAL      := XFilial("SE2")
				SE2->E2_PREFIXO     := cPrefFgt
				*                SE2->E2_NUM         := StrZero(Val(NumTit)+1,6)
				SE2->E2_NUM         := StrZero(Val(NumTit),6)
				SE2->E2_TIPO        := cTipoFgt
				SE2->E2_PARCELA     := cParcela
				SE2->E2_NATUREZ     := cNatuFgt
				SE2->E2_FORNECE     := cFornFgt
				SE2->E2_LOJA        := SA2->A2_LOJA
				SE2->E2_NOMFOR      := cNReduz
				SE2->E2_EMISSAO     := dDataBase
				SE2->E2_VENCTO      := dDtFGTS
				SE2->E2_VENCREA     := DataValida(dDtFGTS,.T.)
				SE2->E2_VALOR       := nVlFgts
				SE2->E2_VLCRUZ      := nVlFgts
				SE2->E2_MOEDA       := 1
				SE2->E2_SALDO       := nVlFgts
				SE2->E2_VENCORI     := dDtFGTS
				SE2->E2_FLUXO       := cFluxo
				
			EndIf
			If nVlIrFol > 0.00
				
				dbSelectArea( "SE2" )
				Reclock("SE2",.T.)
				
				SE2->E2_FILIAL      := XFilial("SE2")
				SE2->E2_PREFIXO     := cPrefIr
				*                SE2->E2_NUM         := StrZero(Val(NumTit)+1,6)
				SE2->E2_NUM         := StrZero(Val(NumTit),6)
				SE2->E2_TIPO        := cTipoIr
				SE2->E2_PARCELA     := cParcela
				SE2->E2_NATUREZ     := cNatuIr
				SE2->E2_FORNECE     := cFornIr
				SE2->E2_LOJA        := SA2->A2_LOJA
				SE2->E2_NOMFOR      := cNReduz
				SE2->E2_EMISSAO     := dDataBase
				SE2->E2_VENCTO      := dDtIR
				SE2->E2_VENCREA     := DataValida(dDtIR,.T.)
				SE2->E2_VALOR       := nVlIrFol
				SE2->E2_VLCRUZ      := nVlIrFol
				SE2->E2_MOEDA       := 1
				SE2->E2_SALDO       := nVlIrFol
				SE2->E2_VENCORI     := dDtIR
				SE2->E2_FLUXO       := cFluxo
				
			EndIf
			If nVlInss > 0.00
				
				dbSelectArea( "SE2" )
				Reclock("SE2",.T.)
				
				SE2->E2_FILIAL      := XFilial("SE2")
				SE2->E2_PREFIXO     := cPrefIns
				*                SE2->E2_NUM         := StrZero(Val(NumTit)+1,6)
				SE2->E2_NUM         := StrZero(Val(NumTit),6)
				SE2->E2_TIPO        := cTipoIns
				SE2->E2_PARCELA     := cParcela
				SE2->E2_NATUREZ     := cNatuIns
				SE2->E2_FORNECE     := cFornIns
				SE2->E2_LOJA        := SA2->A2_LOJA
				SE2->E2_NOMFOR      := cNReduz
				SE2->E2_EMISSAO     := dDataBase
				SE2->E2_VENCTO      := dDtINSS
				SE2->E2_VENCREA     := DataValida(dDtINSS,.T.)
				SE2->E2_VALOR       := nVlInss
				SE2->E2_VLCRUZ      := nVlInss
				SE2->E2_MOEDA       := 1
				SE2->E2_SALDO       := nVlInss
				SE2->E2_VENCORI     := dDtINSS
				SE2->E2_FLUXO       := cFluxo
				
			EndIf
			If nValPenR > 0.00
				
				dbSelectArea( "SE2" )
				Reclock("SE2",.T.)
				
				SE2->E2_FILIAL      := XFilial("SE2")
				SE2->E2_PREFIXO     := cPrefPen
				*                SE2->E2_NUM         := StrZero(Val(NumTit)+1,6)
				SE2->E2_NUM         := StrZero(Val(NumTit),6)
				SE2->E2_TIPO        := cTipoPen
				SE2->E2_PARCELA     := cParcela
				SE2->E2_NATUREZ     := cNatuPen
				SE2->E2_FORNECE     := cFornPen
				SE2->E2_LOJA        := SA2->A2_LOJA
				SE2->E2_NOMFOR      := cNReduzPen
				SE2->E2_EMISSAO     := dDataBase
				SE2->E2_VENCTO      := Vecto
				SE2->E2_VENCREA     := DataValida(Vecto,.T.)
				SE2->E2_VALOR       := nValPenR
				SE2->E2_VLCRUZ      := nValPenR
				SE2->E2_MOEDA       := 1
				SE2->E2_SALDO       := nValPenR
				SE2->E2_VENCORI     := Vecto
				SE2->E2_FLUXO       := cFluxo
				
			EndIf
			If nValPenF > 0.00
				
				dbSelectArea( "SE2" )
				Reclock("SE2",.T.)
				
				SE2->E2_FILIAL      := XFilial("SE2")
				SE2->E2_PREFIXO     := cPrefPen
				*                SE2->E2_NUM         := StrZero(Val(NumTit)+1,6)
				SE2->E2_NUM         := StrZero(Val(NumTit),6)
				SE2->E2_TIPO        := cTipoPen
				SE2->E2_PARCELA     := cParcela
				SE2->E2_NATUREZ     := cNatuPen
				SE2->E2_FORNECE     := cFornPen
				SE2->E2_LOJA        := SA2->A2_LOJA
				SE2->E2_NOMFOR      := cNReduzPen
				SE2->E2_EMISSAO     := dDataBase
				SE2->E2_VENCTO      := Vecto
				SE2->E2_VENCREA     := DataValida(Vecto,.T.)
				SE2->E2_VALOR       := nValPenF
				SE2->E2_VLCRUZ      := nValPenF
				SE2->E2_MOEDA       := 1
				SE2->E2_SALDO       := nValPenF
				SE2->E2_VENCORI     := Vecto
				SE2->E2_FLUXO       := cFluxo
				
			EndIf
			lCalcIR := .F.
			lCalcISS:= .F.
			
			dbSelectArea("SED")
			dbSeek(XFilial("SED")+SE2->E2_naturez)
			If !Eof()
				lCalcIR :=IF(SED->ED_CALCIRF == "S",.T.,.F.)
				lCalcISS:=IF(SED->ED_CALCISS == "S",.T.,.F.)
			Endif
			
			//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
			//³ Calcula IR se a natureza mandar                     ³
			//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
			If lCalcIR
				IF SA2->A2_TIPO == "F"
					nVlIR:=fa050tabir(nValor)
				Else
					nVlIR:= Round(((nValor*Iif(AllTrim(Str(SE2->E2_MOEDA,2))$"01",1,RecMoeda(SE2->E2_EMISSAO,SE2->E2_MOEDA))) * IIF(SED->ED_PERCIRF>0,SED->ED_PERCIRF,GetMV("MV_ALIQIRF"))/100),2)
				EndIF
			Endif
			
			//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
			//³ Calcula ISS se a natureza mandar                                              ³
			//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
			If lCalcISS
				If SED->ED_CALCISS == "S" .And. SA2->A2_RECISS != "S"
					nVlISS:= Round(nValor*(GetMV("MV_ALIQISS")/100),2)
				EndIf
			Endif
			
			If lCalcIR .or. lCalcISS
				dbSelectArea( "SE2" )
				Reclock("SE2")
				SE2->E2_VALOR   := nValor-nVlIR-nVlISS
				SE2->E2_VLCRUZ  := nValor-nVlIR-nVlISS
				SE2->E2_IRRF    := nVlIR
				SE2->E2_ISS     := nVlISS
				SE2->E2_PARCIR  := "1"
				SE2->E2_PARCISS := "1"
			Endif
			
			nSavRec := RecNo()
			//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
			//³ Rotina de complemento de grava‡„o de t¡tulo a pagar ³
			//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
			a050DupPag( "LCTOFIN" )
			
			//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
			//³ Rotina de contabiliza‡„o do titulo                  ³
			//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
			If LctoCont == 1   //-- Sim
				
				dbSelectArea( "SI5" )
				dbSeek ( XFilial("SI5")+cCodPad1 )
				
				dbSelectArea( "SE2" )
				dbGoto( nSavRec )
				If lHead
					nHdlPrv:=HeadProva(cLote,"LCTOFIN",Substr(cUsuario,7,6),@cArquivo)
					lHead := .F.
					lRoda := .T.
				Endif
				nTotal:=nTotal+DetProva(nHdlPrv,cCodPad1,"LCTOFIN",cLote)
				dbSelectArea("SE2")
				//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
				//³ Atualiza flag de Lan‡amento Cont bil       ³
				//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
				Reclock("SE2")
				Replace E2_LA With "S"
				Replace E2_EMISSAO With dDataBase
				
				IF nVlIR > 0
					dbSelectArea( "SI5" )
					dbSeek ( XFilial("SI5")+cCodPad2 )
					
					dbSelectArea("SE2")
					dbSkip()
					If lHead
						nHdlPrv:=HeadProva(cLote,"LCTOFIN",Substr(cUsuario,7,6),@cArquivo)
						lHead := .F.
						lRoda := .T.
					Endif
					nTotal:=nTotal+DetProva(nHdlPrv,cCodPad2,"LCTOFIN",cLote)
					dbSelectArea("SE2")
					//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
					//³ Atualiza flag de Lan‡amento Cont bil       ³
					//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
					Reclock("SE2")
					Replace E2_LA With "S"
					Replace E2_EMISSAO With dDataBase
					
				Endif
				
				IF nVlISS > 0
					dbSelectArea( "SI5" )
					dbSeek ( XFilial("SI5")+cCodPad3 )
					
					dbSelectArea("SE2")
					dbSkip()
					If lHead
						nHdlPrv:=HeadProva(cLote,"LCTOFIN",Substr(cUsuario,7,6),@cArquivo)
						lHead := .F.
						lRoda := .T.
					Endif
					nTotal:=nTotal+DetProva(nHdlPrv,cCodPad3,"LCTOFIN",cLote)
					dbSelectArea("SE2")
					//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
					//³ Atualiza flag de Lan‡amento Cont bil       ³
					//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
					Reclock("SE2")
					Replace E2_LA With "S"
					Replace E2_EMISSAO With dDataBase
				Endif
			Endif
		Endif
		lGeraTit := .T.
	Endif
	
	nValPensao := 0
	nValPenF   := 0
	nValPenR   := 0
	
	
	dbSelectArea('SRA')
	dbSkip()
Enddo

dbSelectArea("SX5")
dbSetorder(1)
dbSeek(xFilial("SX5")+"96BEN")
If RecLock("SX5",.F.)
	NumTit     := Substr(SX5->X5_DESCRI,1,Len(SX5->X5_DESCRI)-6)+NumTit
	SX5->X5_DESCRI := NumTit
	MsUnlock()
else
	#IFDEF WINDOWS
		MsgStop("Nao conseguiu gravar o No. de sequencia dos lancamentos.")
	#ELSE
		Alert("Nao conseguiu gravar o No. de sequencia dos lancamentos.")
	#ENDIF
	
Endif

If lRoda
	RodaProva(nHdlPrv,nTotal)
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Envia para Lan‡amento Cont bil                                                        ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	cA100Incl(cArquivo,nHdlPrv,3,cLote,.F.,lAglutina)
Endif

dbSelectArea("SX1")
dbSetOrder(1)
If dbSeek('GPELCA' + '04',.F.)
	RecLock('SX1',.F.)
	SX1->X1_CNT01:=NumTit
	msUnlock()
EndIf

/*
ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
³Grava as Perguntas utilizadas no Programa no SX1                             ³
ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ*/
// Substituido pelo assistente de conversao do AP5 IDE em 27/03/00 ==> Function GravaPerg
Static Function GravaPerg()

dbSelectArea( "SX1" )
dbSetOrder( 1 )


SX1->( dbAppend() )
SX1->( rLock() )

SX1->X1_GRUPO   := "GPELCA"
SX1->X1_ORDEM   := "24"
SX1->X1_PERGUNT := "Gerar rescisao"
SX1->X1_VARIAVL := "mv_chp"
SX1->X1_TIPO    := "N"
SX1->X1_TAMANHO := 1
SX1->X1_DECIMAL := 0
SX1->X1_PRESEL  := 1
SX1->X1_GSC     := "C"
SX1->X1_VALID   := ""
SX1->X1_VAR01   := "mv_par24"
SX1->X1_DEF01   := "Sim"
SX1->X1_CNT01   := ""
SX1->X1_VAR02   := ""
SX1->X1_DEF02   := "Nao"
SX1->X1_CNT02   := ""
SX1->X1_VAR03   := ""
SX1->X1_DEF03   := ""
SX1->X1_CNT03   := ""
SX1->X1_VAR04   := ""
SX1->X1_DEF04   := ""
SX1->X1_CNT04   := ""
SX1->X1_VAR05   := ""
SX1->X1_DEF05   := ""
SX1->X1_CNT05   := ""
SX1->X1_F3      := ""
SX1->(  MSUnlock() )
// Substituido pelo assistente de conversao do AP5 IDE em 27/03/00 ==> __Return( .T. )
Return( .T. )        // incluido pelo assistente de conversao do AP5 IDE em 27/03/00
