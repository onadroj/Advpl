#include "PROTHEUS.CH"
#include "TOPCONN.CH"

// Gera dados de contratos para documentos em Word

User Function RGCTWORD()
Local _aParametros := {}
Local _aRet := {}
Private _cTipo
Private _cArquivo
Private _oWord
Private _cContrato
Private _cRevisao
Private _cMunicipio 
Private _cRespEmp   
Private _cCRespEmp 
Private _cRespCliFor 
Private _cCRespCliFor


aAdd(_aParametros,{1,"Contrato                 ",CriaVar("CN9_NUMERO",.F.),"@!",,"CN9",nil,70,.T.})
aAdd(_aParametros,{1,"Revisão                  ",CriaVar("CN9_REVISA",.F.),"@!",,,nil,30,.F.})
aAdd(_aParametros,{1,"Município                ","                                        ","@!",,,nil,100,.F.})
aAdd(_aParametros,{1,"Responsável Empresa      ","                                        ","@!",,,nil,100,.F.})
aAdd(_aParametros,{1,"Cargo Resp. Empresa      ","                                        ","@!",,,nil,100,.F.})
aAdd(_aParametros,{1,"Responsável Forn./Cliente","                                        ","@!",,,nil,100,.F.})
aAdd(_aParametros,{1,"Cargo Resp. Forn./Cliente","                                        ","@!",,,nil,100,.F.})

If Parambox(_aParametros,"Seleção do Contrato",_aRet,,,.T.,,,,,.F.,.F.)
	_cContrato 	      := _aRet[1]
	_cRevisao  	      := _aRet[2]
	_cMunicipio       := AllTrim(_aRet[3])
	_cRespEmp         := AllTrim(_aRet[4])
	_cCRespEmp		:= AllTrim(_aRet[5])
	_cRespCliFor      := AllTrim(_aRet[6])
	_cCRespCliFor := AllTrim(_aRet[7])
	_cArquivo         := cGetFile( '*.dotx|*.dot' , 'Modelos Word', 1, 'C:\', .F., nOR( GETF_LOCALHARD, GETF_NETWORKDRIVE ),.T., .T. )

	GeraWord()
Else
	Return()
Endif

Static Function GeraWord()
Local _aCampos := {}
Local _aInfo := {}
Local _cPessoa := ""
Local _aFornec := {}
Local _cFornec := ""
Local _cLoja := ""
Local _cQuery := ""
Local _aArea := {}

dbSelectArea("CN9")
dbSetOrder(1) //CN9_FILIAL+CN9_NUMERO+CN9_REVISA
dbSeek(xFilial("CN9")+_cContrato+_cRevisao)
While !Eof() .AND. xFilial("CN9")+CN9->CN9_NUMERO+CN9->CN9_REVISA==xFilial("CN9")+_cContrato+_cRevisao
	AADD(_aCampos,{"RespEmpresa",_cRespEmp})
	AADD(_aCampos,{"CargoRespEmp",_cCRespEmp})
	AADD(_aCampos,{"RespCliFor",_cRespCliFor})
	AADD(_aCampos,{"CargoRespCliFor",_cCRespCliFor})
	
	fInfo(@_aInfo,CN9->CN9_FILIAL)
	
	cTipInsc  := If (_aInfo[15] == 1 ,"2","1" )
	cCnpj     := If (cTipInsc == "2",_aInfo[8],Transform(_aInfo[8],"@R ##.###.###/####-##")) // CGC
	
	AADD(_aCampos,{"Empresa",AllTrim(_aInfo[3])})
	AADD(_aCampos,{"EnderecoEmp",AllTrim(_aInfo[4])})
	AADD(_aCampos,{"Compemp",AllTrim(_aInfo[14])})
	AADD(_aCampos,{"BairroEmp",AllTrim(_aInfo[13])})
	AADD(_aCampos,{"CidadeEmp",AllTrim(_aInfo[5])})
	AADD(_aCampos,{"EstadoEmp",AllTrim(_aInfo[6])})
	AADD(_aCampos,{"CepEmp",Transform(AllTrim(_aInfo[7]),"@R 99999-999")})
	AADD(_aCampos,{"Cnpj",cCnpj})

	AADD(_aCampos,{"Contrato",CN9->CN9_NUMERO})
	AADD(_aCampos,{"Revisao",CN9->CN9_REVISA})
	
	AADD(_aCampos,{"Municipio",IIF(!Empty(_cMunicipio),_cMunicipio,AllTrim(_aInfo[5]))})
	AADD(_aCampos,{"DataInicio",DTOC(CN9->CN9_DTINIC)})
	AADD(_aCampos,{"Vigencia",Alltrim(STR(CN9->CN9_VIGE))+" "+IIf(CN9->CN9_UNVIGE=="1","dias",IIf(CN9->CN9_UNVIGE=="2","meses",IIf(CN9->CN9_UNVIGE=="3","anos","Indeterminada")))})
	AADD(_aCampos,{"CondPgto",RetField("SE4",1,xFilial("SE4")+CN9->CN9_CONDPG,"E4_DESCRI")})
	AADD(_aCampos,{"IndiceReaj",AllTrim(RetField("CN6",1,xFilial("CN6")+CN9->CN9_INDICE,"CN6_DESCRI"))})
	AADD(_aCampos,{"Objeto",AllTrim(MSMM(CN9->CN9_CODOBJ))})
	AADD(_aCampos,{"Justific",AllTrim(MSMM(CN9->CN9_CODJUS))})
	AADD(_aCampos,{"DataRev",DTOC(CN9->CN9_DTREV)})

	AADD(_aCampos,{"DataAssinatura",SUBSTR(DTOC(CN9->CN9_DTASSI),1,2)+" de "+MesExtenso(MONTH(CN9->CN9_DTASSI))+" de "+STR(YEAR(CN9->CN9_DTASSI),4)})

	
    _aArea:=GetArea()
	_cQuery := "SELECT SUM(CNB_VLUNIT) AS TOTAL FROM "+RetSqlName("CNB")+" "
	_cQuery += "WHERE CNB_CONTRA='"+_cContrato+"' AND CNB_REVISA='"+_cRevisao+"' AND D_E_L_E_T_<>'*' "
	TCQUERY _cQuery NEW ALIAS "QRY"
	dbSelectArea("QRY")
	DbGoTop()
	AADD(_aCampos,{"VlrParc",IIf(!EOF(),Alltrim(Transform(QRY->TOTAL,"@E 999,999,999.99")),"0,00")})
	CLOSE
	RestArea(_aArea)	
	
	
	If !Empty(CN9->CN9_CLIENT)
		_cPessoa := RetField("SA1",1,xFilial("SA1")+CN9->CN9_CLIENT+CN9->CN9_LOJACL,"A1_PESSOA")
		AADD(_aCampos,{"Cliente",RetField("SA1",1,xFilial("SA1")+CN9->CN9_CLIENT+CN9->CN9_LOJACL,"A1_NOME")})
		AADD(_aCampos,{"EndCli",RetField("SA1",1,xFilial("SA1")+CN9->CN9_CLIENT+CN9->CN9_LOJACL,"A1_LOGRAD")})
		AADD(_aCampos,{"NumCli",RetField("SA1",1,xFilial("SA1")+CN9->CN9_CLIENT+CN9->CN9_LOJACL,"A1_NR_END")})
		AADD(_aCampos,{"ComplCli",RetField("SA1",1,xFilial("SA1")+CN9->CN9_CLIENT+CN9->CN9_LOJACL,"A1_COMPLEM")})
		AADD(_aCampos,{"BairroCli",RetField("SA1",1,xFilial("SA1")+CN9->CN9_CLIENT+CN9->CN9_LOJACL,"A1_BAIRRO")})
		AADD(_aCampos,{"UFCli",RetField("SA1",1,xFilial("SA1")+CN9->CN9_CLIENT+CN9->CN9_LOJACL,"A1_EST")})
		AADD(_aCampos,{"MunCli",RetField("SA1",1,xFilial("SA1")+CN9->CN9_CLIENT+CN9->CN9_LOJACL,"A1_MUN")})
		AADD(_aCampos,{"CnpjCli",Transform(RetField("SA1",1,xFilial("SA1")+CN9->CN9_CLIENT+CN9->CN9_LOJACL,"A1_CGC"),IIF(_cPessoa=="J","@R 99.999.999/9999-99","@R 999.999.999-99"))})
		AADD(_aCampos,{"CepCli",Transform(RetField("SA1",1,xFilial("SA1")+CN9->CN9_CLIENT+CN9->CN9_LOJACL,"A1_CEP"),"@R 99999-999")})
		/*************************************************************/
        AADD(_aCampos,{"DddCli",RetField("SA1",1,xFilial("SA1")+CN9->CN9_CLIENT+CN9->CN9_LOJACL,"A1_DDD")}) //DDD do cliente - incluso por Pierre
        AADD(_aCampos,{"TelCli",RetField("SA1",1,xFilial("SA1")+CN9->CN9_CLIENT+CN9->CN9_LOJACL,"A1_TEL")}) //Telefone do cliente - incluso por Pierre    
        AADD(_aCampos,{"FaxCli",RetField("SA1",1,xFilial("SA1")+CN9->CN9_CLIENT+CN9->CN9_LOJACL,"A1_FAX")}) //Fax do cliente - incluso por Pierre              
        AADD(_aCampos,{"MailCli",RetField("SA1",1,xFilial("SA1")+CN9->CN9_CLIENT+CN9->CN9_LOJACL,"A1_EMAIL")}) //Email do cliente - incluso por Pierre      
        AADD(_aCampos,{"RgClipf",RetField("SA1",1,xFilial("SA1")+CN9->CN9_CLIENT+CN9->CN9_LOJACL,"A1_PFISICA")}) //RG do cliente  PF - incluso por Pierre
        AADD(_aCampos,{"CodCli",RetField("SA1",1,xFilial("SA1")+CN9->CN9_CLIENT+CN9->CN9_LOJACL,"A1_COD")}) //Código do cliente  PF - incluso por Pierre
	
	
	Else                                                                                                                               
	
		_aArea := GetArea()
		DbSelectArea("CNC")
		DbSetOrder(1) //CNC_FILIAL+CNC_NUMERO+CNC_REVISA+CNC_CODIGO+CNC_LOJA
		DbSeek(xFilial("CNC")+CN9->CN9_NUMERO+CN9->CN9_REVISA,.T.)
		While !Eof() .AND. xFilial("CNC")+CNC->CNC_NUMERO+CNC->CNC_REVISA==xFilial("CNC")+CN9->CN9_NUMERO+CN9->CN9_REVISA
			aAdd(_aFornec,CNC->CNC_CODIGO+"/"+CNC->CNC_LOJA+" - "+RetField("SA2",1,xFilial("SA2")+CNC->CNC_CODIGO+CNC->CNC_LOJA,"A2_NOME"))
			DbSkip()
		End
		RestArea(_aArea)
		If len(_aFornec) > 1
			aAdd(_aParametros,{2,"Fornecedor a utilizar",0,_aFornec,100,,.T.})
			
			If Parambox(_aParametros,"Selecione o Fornecedor",_aRet,,,.T.,,,,,.F.,.F.)
				_X := _aRet[1]
				If Type("_X") == "N"
					MsgStop("Não foi selecionado fornecedor para geração do documento."+chr(13)+chr(10)   ;
					+ "Operação cancelada!")
					Return()
				Else
					_cFornec := Substr(_aRet[1],1,6)
					_cLoja := Substr(_aRet[1],8,2)
				EndIF
			Else
				Return()
			Endif
		Else
			_cFornec := Substr(_aFornec[1],1,6)
			_cLoja := Substr(_aFornec[1],8,2)
		Endif
		_cPessoa := RetField("SA2",1,xFilial("SA2")+_cFornec+_cLoja,"A2_TIPO")
		AADD(_aCampos,{"Fornecedor",RetField("SA2",1,xFilial("SA2")+_cFornec+_cLoja,"A2_NOME")})
		AADD(_aCampos,{"EndFor",RetField("SA2",1,xFilial("SA2")+_cFornec+_cLoja,"A2_LOGRAD")})
		AADD(_aCampos,{"NumFor",RetField("SA2",1,xFilial("SA2")+_cFornec+_cLoja,"A2_NR_END")})
		AADD(_aCampos,{"ComplFor",RetField("SA2",1,xFilial("SA2")+_cFornec+_cLoja,"A2_COMPLEM")})
		AADD(_aCampos,{"BairroFor",RetField("SA2",1,xFilial("SA2")+_cFornec+_cLoja,"A2_BAIRRO")})
		AADD(_aCampos,{"UFFor",RetField("SA2",1,xFilial("SA2")+_cFornec+_cLoja,"A2_EST")})
		AADD(_aCampos,{"MunFor",RetField("SA2",1,xFilial("SA2")+_cFornec+_cLoja,"A2_MUN")})
		AADD(_aCampos,{"CnpjFor",Transform(RetField("SA2",1,xFilial("SA2")+_cFornec+_cLoja,"A2_CGC"),IIF(_cPessoa=="J","@R 99.999.999/9999-99","@R 999.999.999-99"))})
		AADD(_aCampos,{"CepFor",Transform(RetField("SA2",1,xFilial("SA2")+_cFornec+_cLoja,"A2_CEP"),"@R 99999-999")})
		/*************************************************************/
        AADD(_aCampos,{"CodFor",RetField("SA2",1,xFilial("SA2")+_cFornec+_cLoja,"A2_COD")}) // Código do fornecedor - Incluso por Pierre
        AADD(_aCampos,{"TelFor",RetField("SA2",1,xFilial("SA2")+_cFornec+_cLoja,"A2_TEL")}) // Telefone do fornecedor - Incluso por Pierre
        AADD(_aCampos,{"Faxfor",RetField("SA2",1,xFilial("SA2")+_cFornec+_cLoja,"A2_FAX")}) // Fax do fornecedor - Incluso por Pierre
        AADD(_aCampos,{"MailFor",RetField("SA2",1,xFilial("SA2")+_cFornec+_cLoja,"A2_EMAIL")}) // EMAIL do fornecedor - Incluso por Pierre
		
	Endif
	
	
	dbSkip()
Enddo

// Inicializa o Ole com o Word
_oWord := OLE_CreateLink('TMsOleWord97')

OLE_NewFile(_oWord,_cArquivo)

for I:=1 to len(_aCampos)
	if _aCampos[I,2]==""
		_aCampos[I,2]:=" "
	endif
	OLE_SetDocumentVar(_oWord,_aCampos[I,1],_aCampos[I,2])
next

//OLE_SetProperty( _oWord, oleWdVisible,   .f. )
//OLE_SetProperty( _oWord, oleWdPrintBack, .t. )

//--Atualiza os campos no Word
OLE_UpDateFields(_oWord)

MsgStop("Ao clicar em OK o arquivo será fechado no Word!")

OLE_CloseLink( _oWord )

Return()
