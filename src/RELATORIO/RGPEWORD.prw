#INCLUDE "PROTHEUS.CH"
#INCLUDE "RWMAKE.CH" 
#INCLUDE "MSOLE.CH"


User Function RGPEWORD()          

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ 26/07/05  -   Edson                                                 ³
//³ Gera campos para integracao com Word - para relatórios de admissão e³
//³ demissão                                                            ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

Private CCADASTRO
Private ASAYS
Private ABUTTONS
Private NOPCA
Private CTYPE
Private CARQUIVO
Private OWORD
Private cPerg := "UGPWRD"

ValidPerg()
pergunte(cPerg,.F.)

cCadastro := "Integração com MS-Word"
aSays	  :={}
aButtons  :={}

AADD(aSays,"Esta rotina irá gerar relatórios de admissão/demissão de funcionários ")

AADD(aButtons, { 5,.T.,{|| Pergunte(cPerg,.T. )}})
AADD(aButtons, { 1,.T.,{|o| nOpca := 1,FechaBatch()}})
AADD(aButtons, { 2,.T.,{|o| FechaBatch() }} )

FormBatch( cCadastro, aSays, aButtons )

If nOpca == 1
	Processa({|| WORDIMP()})  
EndIf
	
Return

Static FUNCTION WORDIMP()
Local aCampos := {}
Local aInfo := {}

// Seleciona Arquivo Modelo 
cType := "*.*  | *.DOT"
cArquivo := cGetFile(cType, OemToAnsi("Selecione arquivo "+Subs(cType,1,6)))

// Inicializa o Ole com o MS-Word 97 ( 8.0 )	
oWord := OLE_CreateLink('')		

//OLE_SetProperty( oWord, oleWdVisible,   .f. )
//OLE_SetProperty( oWord, oleWdPrintBack, .t. )
	


dbSelectArea("SRA")   
dbSetOrder(1)
dbSeek(XFILIAL("SRA")+mv_par01)
While !Eof()        
	
	if xFilial("SRA")+SRA->RA_MAT < xFilial("SRA")+mv_par01
	   dbSkip()
	   Loop
	endif
	if xFilial("SRA")+SRA->RA_MAT > xFilial("SRA")+mv_par02
	   Exit
	endif

	if (mv_par05==1 .AND. (SRA->RA_ADMISSA < mv_par03 .OR. SRA->RA_ADMISSA > mv_par04)) .OR. ;
	   (mv_par05==2 .AND. (SRA->RA_DEMISSA < mv_par03 .OR. SRA->RA_DEMISSA > mv_par04))
	   dbSkip()
	   Loop 
	endif

    OLE_NewFile(oWord,cArquivo)
    
    fInfo(@aInfo,SRA->RA_FILIAL)

	cTipInsc  := If (aInfo[15] == 1 ,"2","1" )
	cCgc      := If (cTipInsc == "2",aInfo[8],Transform(aInfo[8],"@R ##.###.###/####-##")) // CGC
	
	AADD(aCampos,{"Empresa",AllTrim(aInfo[3])})
	AADD(aCampos,{"EnderecoEmp",AllTrim(aInfo[4])})
	AADD(aCampos,{"Compemp",AllTrim(aInfo[14])})
	AADD(aCampos,{"BairroEmp",AllTrim(aInfo[13])})
	AADD(aCampos,{"CidadeEmp",AllTrim(aInfo[5])})
	AADD(aCampos,{"EstadoEmp",AllTrim(aInfo[6])})
	AADD(aCampos,{"CGC",cCgc})
	AADD(aCampos,{"Nome",IIF(AllTrim(SRA->RA_NOMECMP)=="",AllTrim(SRA->RA_NOME),AllTrim(SRA->RA_NOMECMP))})
	AADD(aCampos,{"Apelido",ALLTRIM(SRA->RA_APELIDO)})
	AADD(aCampos,{"Cargo",AllTrim(DescFun(SRA->RA_CODFUNC,SRA->RA_FILIAL))})
	AADD(aCampos,{"CBO",RETFIELD("SRJ",1,XFILIAL("SRJ")+SRA->RA_CODFUNC,"RJ_CODCBO")})
	AADD(aCampos,{"Filial",SRA->RA_FILIAL})
	AADD(aCampos,{"Matricula",AllTrim(SRA->RA_MAT)})
	AADD(aCampos,{"DataAdmissao",SUBSTR(DTOC(SRA->RA_ADMISSA),1,2)+" de "+MesExtenso(MONTH(SRA->RA_ADMISSA))+" de "+STR(YEAR(SRA->RA_ADMISSA),4)})
	AADD(aCampos,{"Periodo",ALLTRIM(STR(int(SRA->RA_PEREXP)))})
	AADD(aCampos,{"VenctoExp",SUBSTR(DTOC(SRA->RA_VCTOEXP),1,2)+" de "+MesExtenso(MONTH(SRA->RA_VCTOEXP))+" de "+STR(YEAR(SRA->RA_VCTOEXP),4)})
	AADD(aCampos,{"Registro",SRA->RA_REGISTR})
	AADD(aCampos,{"FlsFicha",SRA->RA_FICHA})
	AADD(aCampos,{"Remuneracao",ALLTRIM(TRANSFORM(SRA->RA_SALARIO,"@E 999,999,999.99"))})
	aAreaX := GETAREA()  //Funcao extenso desposiciona o Alias
	AADD(aCampos,{"Extenso","("+Extenso(SRA->RA_SALARIO,.F.,1)+")"})
	RESTAREA(aAreaX)
	AADD(aCampos,{"TipoPag",If(SRA->RA_CATFUNC$"M*C"," POR MES",If(SRA->RA_CATFUNC="H"," POR HRS"," POR DIA"))})
	AADD(aCampos,{"EndFunc",ALLTRIM(SRA->RA_ENDEREC)})
	AADD(aCampos,{"NumEndFunc",ALLTRIM(SRA->RA_NUMENDE)}) //INSERIDO EM 01/03/16 - JORDANO
	AADD(aCampos,{"CompFunc",ALLTRIM(SRA->RA_COMPLEM)})
	AADD(aCampos,{"BairroFunc",ALLTRIM(SRA->RA_BAIRRO)})
	AADD(aCampos,{"MunicFunc",ALLTRIM(SRA->RA_MUNICIP)})
	AADD(aCampos,{"UfFunc",ALLTRIM(SRA->RA_ESTADO)})
	AADD(aCampos,{"CepFunc",ALLTRIM(SRA->RA_CEP)})
	AADD(aCampos,{"TelFunc",ALLTRIM(SRA->RA_TELEFON)})
	AADD(aCampos,{"EstCivil",AllTrim(RETFIELD("SX5",1,xFilial("SX5")+"33"+SRA->RA_ESTCIVI,"X5_DESCRI"))})
	AADD(aCampos,{"Nacional",AllTrim(RETFIELD("SX5",1,xFilial("SX5")+"34"+SRA->RA_NACIONA,"X5_DESCRI"))})
	AADD(aCampos,{"Natural",AllTrim(RETFIELD("SX5",1,xFilial("SX5")+"12"+SRA->RA_NATURAL,"X5_DESCRI"))})
	AADD(aCampos,{"NaturCid",AllTrim(SRA->RA_MUNNASC)})
	AADD(aCampos,{"Categoria",Alltrim(RETFIELD("SX5",1,xFilial("SX5")+"28"+SRA->RA_CATFUNC,"X5_DESCRI"))})
	AADD(aCampos,{"CTPS",ALLTRIM(SRA->RA_NUMCP)})
	AADD(aCampos,{"Serie",ALLTRIM(SRA->RA_SERCP)})
	AADD(aCampos,{"UFCTPS",ALLTRIM(SRA->RA_UFCP)})
	AADD(aCampos,{"DtEmisCTPS",ALLTRIM(SRA->RA_DTCPEXP)})
	AADD(aCampos,{"HorasSemanais",ALLTRIM(STR(INT(SRA->RA_HRSEMAN)))})
	aAreaX := GETAREA()  //Funcao extenso desposiciona o Alias
	AADD(aCampos,{"HSExt",Extenso(SRA->RA_HRSEMAN,.T.,1)})
	RESTAREA(aAreaX)
	AADD(aCampos,{"MunTrabalho",ALLTRIM(SRA->RA_MNTRB)})
	AADD(aCampos,{"Turno",ALLTRIM(SRA->RA_TNOTRAB)})
	AADD(aCampos,{"HTurno",Alltrim(RETFIELD("SR6",1,xFilial("SR6")+SRA->RA_TNOTRAB,"R6_DESC"))})
//	AADD(aCampos,{"HTurno",IIF(SRA->RA_TNOTRAB=="001","08 às 18 horas, de Segunda à Quinta Feira e na Sexta Feira será de 08 às 17 horas", ;
//	                                            "07 às 17 horas, de Segunda à Quinta Feira e na Sexta Feira será de 07 às 16 horas")})
	AADD(aCampos,{"Habilitacao",ALLTRIM(SRA->RA_HABILIT)})
	AADD(aCampos,{"Identidade",ALLTRIM(SRA->RA_RG)})
	AADD(aCampos,{"OrgEmissorRG",ALLTRIM(SRA->RA_RGORG)})
	AADD(aCampos,{"UFEmissorRG",ALLTRIM(SRA->RA_RGUFEXP)})
	AADD(aCampos,{"TituloEleitor",ALLTRIM(SRA->RA_TITULOE)})
	AADD(aCampos,{"CidTitulo",ALLTRIM(SRA->RA_CIDTITU)})
	AADD(aCampos,{"UFTitulo",ALLTRIM(SRA->RA_UFTITU)})
	AADD(aCampos,{"ZonaSecTitEleitor",ALLTRIM(SRA->RA_ZONASEC)})
	AADD(aCampos,{"FatorRh",ALLTRIM(SRA->RA_FATORRH)})
	AADD(aCampos,{"Sexo",IIF(SRA->RA_SEXO=="M","Masculino","Feminino")})
	AADD(aCampos,{"DataDemissao",SUBSTR(DTOC(SRA->RA_DEMISSA),1,2)+" de "+MesExtenso(MONTH(SRA->RA_DEMISSA))+" de "+STR(YEAR(SRA->RA_DEMISSA),4)})
	AADD(aCampos,{"DataRelatorio",SUBSTR(DTOC(MSDate()),1,2)+" de "+MesExtenso(MONTH(MSDate()))+" de "+STR(YEAR(MSDate()),4)})
	AADD(aCampos,{"Sindicato",SUBSTR(RETFIELD("RCE",1,xFilial("RCE")+SRA->RA_SINDICA,"RCE_DESCRI"),1,40)})
	AADD(aCampos,{"Banco",TRANSFORM(SRA->RA_BCDEPSA,PesqPict("SRA","RA_BCDEPSA"))})
	AADD(aCampos,{"Conta",SRA->RA_CTDEPSA})   
	AADD(aCampos,{"Idade",STR(Calc_Idade(dDatabase, SRA->RA_NASC))})   
	AADD(aCampos,{"TipoConta",IIF(SRA->RA_TIPCONT=="C","Conta Corrente",IIF(SRA->RA_TIPCONT=="P","Poupanca",IIF(SRA->RA_TIPCONT=="E","Especie","Cheque")))})
	AADD(aCampos,{"Cta033",IIF(SUBSTR(SRA->RA_BCDEPSA,1,3)=="033",SRA->RA_CTDEPSA,"")})   
	AADD(aCampos,{"Ag033",IIF(SUBSTR(SRA->RA_BCDEPSA,1,3)=="033",SUBSTR(SRA->RA_BCDEPSA,4,8),"")})   
	AADD(aCampos,{"Bco033",IIF(SUBSTR(SRA->RA_BCDEPSA,1,3)=="033","033 - Banco Santander (Brasil) S.A.","")})   


    aAreaX := GETAREA()

    dbSelectArea("SRB")
    dbSetOrder(1)           //RB_FILIAL+RM_MATRICULA
    dbSeek(xFilial("SRB")+SRA->RA_MAT)
    Conjuge:=""
    While !Eof() .and. SRB->RB_MAT==SRA->RA_MAT
       if SRB->RB_GRAUPAR=="C"
          Conjuge:=ALLTRIM(SRB->RB_NOME)
          Exit
       endif
       DBSKIP()
    ENDDO

    RESTAREA(aAreaX)

	AADD(aCampos,{"Conjuge",Conjuge})
	AADD(aCampos,{"ContExp",SUBSTR(DTOC(SRA->RA_VCTOEXP),1,2)+" de "+MesExtenso(MONTH(SRA->RA_VCTOEXP))+" de "+STR(YEAR(SRA->RA_VCTOEXP),4)})
	AADD(aCampos,{"PIS",AllTrim(SRA->RA_PIS)})
	AADD(aCampos,{"CartReserv",AllTrim(SRA->RA_RESERVI)})
	AADD(aCampos,{"CategReserv",AllTrim(SRA->RA_CATRESE)})
	AADD(aCampos,{"RegProfissional",AllTrim(SRA->RA_REGPROF)})
	AADD(aCampos,{"ConsProfissional",AllTrim(SRA->RA_CONPROF)})
	AADD(aCampos,{"CPF",AllTrim(SRA->RA_CIC)})
	AADD(aCampos,{"DT_Nasc",(DTOC(SRA->RA_NASC))})
	AADD(aCampos,{"PAI",ALLTRIM(SRA->RA_PAI)})
	AADD(aCampos,{"MAE",ALLTRIM(SRA->RA_MAE)})
	AADD(aCampos,{"GrauInst",AllTrim(RETFIELD("SX5",1,xFilial("SX5")+"26"+SRA->RA_GRINRAI,"X5_DESCRI"))})
	AADD(aCampos,{"TurnoTrab",AllTrim(RETFIELD("SR6",1,xFilial("SR6")+SRA->RA_TNOTRAB,"R6_DESC"))})
	AADD(aCampos,{"DtFinalAviso",SUBSTR(DTOC(mv_par06),1,2)+" de "+MesExtenso(MONTH(mv_par06))+" de "+STR(YEAR(mv_par06),4)})
	AADD(aCampos,{"DtIniAus",SUBSTR(DTOC(mv_par07),1,2)+" de "+MesExtenso(MONTH(mv_par07))+" de "+STR(YEAR(mv_par07),4)})
	AADD(aCampos,{"DtFimAus",SUBSTR(DTOC(mv_par08),1,2)+" de "+MesExtenso(MONTH(mv_par08))+" de "+STR(YEAR(mv_par08),4)})
	AADD(aCampos,{"DtHomologacao",SUBSTR(DTOC(mv_par09),1,2)+" de "+MesExtenso(MONTH(mv_par09))+" de "+STR(YEAR(mv_par09),4)})
	AADD(aCampos,{"DtCienciAviso",SUBSTR(DTOC(mv_par10),1,2)+" DE "+UPPER(MesExtenso(MONTH(mv_par10)))+" DE "+STR(YEAR(mv_par10),4)})
	
    for I:=1 to len(aCampos) 
        if aCampos[I,2]==""
           aCampos[I,2]:=" "
        endif
	    OLE_SetDocumentVar(oWord,aCampos[I,1],aCampos[I,2])
    next
				
	//--Atualiza Variaveis
	OLE_UpDateFields(oWord)

	//-- Imprime as variaveis				
   //OLE_SetProperty( oWord, '208', .F. ) 
   //OLE_PrintFile( oWord, "ALL",,, 1 ) 
	
	dbSkip()
Enddo	         

//cArqCria := "C:\ModeloEIT\ReciboRPA"+Alltrim(mv_par04)+"-"+Alltrim(mv_par05)+".DOC"
//OLE_SaveAsFile( oWord, cArqCria )

MsgStop("Ao clicar em OK o arquivo será fechado no Word!")

OLE_CloseLink( oWord ) 			// Fecha o Documento

Return
                                        

Static Function ValidPerg

Local _sAlias := Alias()
Local aRegs := {}
Local i,j

dbSelectArea("SX1")
dbSetOrder(1)
cPerg := PADR(cPerg,10)

// Grupo/Ordem/Pergunta/Variavel/Tipo/Tamanho/Decimal/Presel/GSC/Valid/Var01/Def01/Cnt01/Var02/Def02/Cnt02/Var03/Def03/Cnt03/Var04/Def04/Cnt04/Var05/Def05/Cnt05
aAdd(aRegs,{cPerg,"01","Da Matricula         ?","","","mv_ch1","C",6,0,0,"G","","mv_par01","","","","","","","","","","","","","","","","","","","","","","","","","SRA","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"02","Ate a Matricula      ?","","","mv_ch2","C",6,0,0,"G","","mv_par02","","","","","","","","","","","","","","","","","","","","","","","","","SRA","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"03","Da Data              ?","","","mv_ch3","D",8,0,0,"G","","mv_par03","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"04","Ate a Data           ?","","","mv_ch4","D",8,0,0,"G","","mv_par04","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"05","Quanto a Data        ?","","","mv_ch5","N",1,0,0,"C","","mv_par05","Da Admissao","","","","","Da Demissao","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"06","Dt. Final do Aviso   ?","","","mv_ch6","D",8,0,0,"G","","mv_par06","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"07","Dt. Ini. Aus. Aviso  ?","","","mv_ch7","D",8,0,0,"G","","mv_par07","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"08","Dt. Fim Aus. Aviso   ?","","","mv_ch8","D",8,0,0,"G","","mv_par08","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"09","Dt. Homologacao      ?","","","mv_ch9","D",8,0,0,"G","","mv_par09","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"10","Dt. Ciencia Aviso    ?","","","mv_cha","D",8,0,0,"G","","mv_par10","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})

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