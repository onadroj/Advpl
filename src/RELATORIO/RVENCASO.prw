#Include "PROTHEUS.ch"

User Function RVENCASO()     
PRIVATE titulo		:= "" 
PRIVATE nomeprog	:= "TESTE"
PRIVATE aSelFil		:= {} 
PRIVATE cPerg		:= "_VENCASO" 

ValidPerg()

RVENCASOR4()

Return


Static Function RVENCASOR4()         
Local aArea := GetArea()

Pergunte(cPerg,.T.)

oReport := ReportDef()      

IF ValType( oReport ) == "O"
	oReport :PrintDialog()      
ENDIF
        
RestArea(aArea) 

Return                                


Static Function ReportDef() 
Local oReport
local aArea	   		:= GetArea()   
Local cReport		:= "Relação de ASOs a vencer"
Local cTitulo		:= "ASOs a vencer no mês seguinte"				   			 
Local cDesc			:= "Este programa ira imprimir a relação de ASOs a vencer no mês seguinte"	  		 

PRIVATE cPerg		:= "_VENCASO"		   


//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Criacao do componente de impressao                                      ³
//³                                                                        ³
//³TReport():New                                                           ³
//³ExpC1 : Nome do relatorio                                               ³
//³ExpC2 : Titulo                                                          ³
//³ExpC3 : Pergunte                                                        ³
//³ExpB4 : Bloco de codigo que sera executado na confirmacao da impressao  ³
//³ExpC5 : Descricao                                                       ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

oReport	:= TReport():New( cReport,cTitulo,cPerg, { |oReport| ReportPrint( oReport ) }, cDesc ) 
oReport:SetPortrait(.T.)
oReport:ParamReadOnly()

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Criacao da secao utilizada pelo relatorio                               ³
//³                                                                        ³
//³TRSection():New                                                         ³
//³ExpO1 : Objeto TReport que a secao pertence                             ³
//³ExpC2 : Descricao da seçao                                              ³
//³ExpA3 : Array com as tabelas utilizadas pela secao. A primeira tabela   ³
//³        sera considerada como principal para a seção.                   ³
//³ExpA4 : Array com as Ordens do relatório                                ³
//³ExpL5 : Carrega campos do SX3 como celulas                              ³
//³        Default : False                                                 ³
//³ExpL6 : Carrega ordens do Sindex                                        ³
//³        Default : False                                                 ³
//³                                                                        ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
oSection0  := TRSection():New( oReport, "Funcionários", {"SRA"},, .F., .F. )	  

TRCell():New( oSection0, "RA_CC"     , "SRA","C.CUSTO"/*Titulo*/,/*Picture*/,/*Tamanho*/,/*lPixel*/,/*CodeBlock*/)  
TRCell():New( oSection0, "RA_MAT"    , "SRA","MATRICULA"/*Titulo*/,/*Picture*/,/*Tamanho*/,/*lPixel*/,/*CodeBlock*/)  
TRCell():New( oSection0, "RA_NOME"   , "SRA","NOME"/*Titulo*/,/*Picture*/,/*Tamanho*/,/*lPixel*/,/*CodeBlock*/)  
TRCell():New( oSection0, "DSFUNC"    ,,"FUNÇÃO"/*Titulo*/,/*Picture*/,20/*Tamanho*/,/*lPixel*/,/*CodeBlock*/,"LEFT",,"LEFT") //RJ_DESC 
TRCell():New( oSection0, "RA_EXAMEDI", "SRA","VENC.EXAME"/*Titulo*/,/*Picture*/,/*Tamanho*/,/*lPixel*/,/*CodeBlock*/) 
TRCell():New( oSection0, "DSSIT"     ,,"SITUAÇÃO"/*Titulo*/,/*Picture*/,55/*Tamanho*/,/*lPixel*/,/*CodeBlock*/,"LEFT",,"LEFT") //X5_DESCRI

oreport:DisableOrientation()

Return(oReport)

Static Function ReportPrint( oReport )
Local oSection0 	:= oReport:Section(1)
Local cQuery		:= "SRA"

cFil := xFilial("SRA")

_Ano := Year(MsDate())
_Mes := Month(MsDate())
_Mes += 1
If _Mes == 13
	_Mes := 1
	_Ano += 1
Endif        

Titulo = "RELAÇÃO DE ASOs A VENCER EM "+Upper(MesExtenso(_Mes))+" / "+Alltrim(Str(_Ano))

oReport:SetCustomText({|| CtCGCCabTR(,,,,,dDataBase,titulo,,,,,oReport)} )

cFilter := "SRA->RA_FILIAL=='"+cFil+"' .and. "
cFilter += "SRA->RA_SITFOLH<>'D' .and. "
cFilter += "SRA->RA_CC >= '"+mv_par01+"' .and. SRA->RA_CC <= '"+mv_par02+"' .and. "
cFilter += "Substr(DTOS(SRA->RA_EXAMEDI),1,6)== '"+Alltrim(Str(_Ano))+Strzero(_Mes,2)+"' "

SRA->(DbSetOrder(2)) //RA_FILIAL+RA_CC+RA_MAT
SRA->(DbGotop())

oSection0:SetFilter( cFilter )

/*
oBreak2 := TRBreak():New( oSection0, {|| SRA->RA_CC } )
oBreak2:SetPageBreak(.T.)
*/

oSection0:Cell("DSFUNC"):SetBlock( { || RetField("SRJ",1,xFilial("SRJ")+SRA->RA_CODFUNC,"RJ_DESC") } )
oSection0:Cell("DSSIT"):SetBlock( { || RetField("SX5",1,xFilial("SX5")+"31"+SRA->RA_SITFOLH,"X5_DESCRI") } )

oSection0:Print()

oReport:IncMeter()

Return
                     
Static Function ValidPerg
cPerg := PADR(cPerg,10)

//PutSx1(cGrupo,cOrdem,cPergunt,cPerSpa,cPerEng,cVar,cTipo,nTamanho,nDecimal,nPresel,cGSC,cValid,cF3,cGrpSxg,cPyme,cVar01,cDef01,cDefSpa1,cDefEng1,cCnt01,cDef02,cDefSpa2,cDefEng2,cDef03,cDefSpa3,cDefEng3,cDef04,cDefSpa4,cDefEng4,cDef05,cDefSpa5,cDefEng5,aHelpPor,aHelpEng,aHelpSpa,cHelp)
PutSx1(cPerg,"01","Do Centro de Custo   ?","Do Centro de Custo   ?","Do Centro de Custo   ?","mv_ch1","C",9,0,0,"G","","_CT","","","mv_par01","","","","","","","","","","","","","","","","")
PutSx1(cPerg,"02","Ate Centro de Custo  ?","Ate Centro de Custo  ?","Ate Centro de Custo  ?","mv_ch2","C",9,0,0,"G","","_CT","","","mv_par02","","","","","","","","","","","","","","","","")

Return

Static Function Scheddef()
Local _aParam
Local aOrd := {}

_aParam := {"R",;        //Tipo R para relatorio P para processo   
			"_VENCASO",;		// Pergunte do relatorio, caso nao use passar ParamDef            
			"SRA",;  	// Alias            
			aOrd,;   	//Array de ordens   
			"ASOs a vencer."}    

Return(_aParam)