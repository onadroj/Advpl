#Include "Protheus.ch"
#Include "TopConn.ch"

User Function FCTB001() 
	
	If !Pergunte("FIMP001",.T.)
	   Return
	EndIf 
	
	MsgInfo("Este programa faz a leitura de um arquivo CSV contendo o novo plano de contas da Fresar e realiza o de/para das contas") 
	
	If MsgYesNo("Foi gerado backup do banco de dados antes da execução desta rotina? S/N") 
	   Processa({||FGeraDados()},"Aguarde...","Processando...")
	EndIf 
	
Return 

Static Function FGeraDados()

	Local nPos 
	Local cLinha 
	Local cCodAnt
	Local cDesAnt 
	Local cCodNov
	Local cDesNov 
	
	If ( !File(MV_Par01) )
		Return
	EndIf
	
	// Abrindo o arquivo
	FT_FUse(MV_Par01)
	FT_FGoTop()
	
	ProcRegua(RecCount())
	
	// Capturando as linhas do arquivo
	Do While !FT_FEof()
	    IncProc() 
		cLinha := FT_FREADLN()
		nPos    := At(";",cLinha)
		cCodAnt := AllTrim(SubStr(cLinha,1,nPos-1))  //Antigo 
	
		cLinha  := SubStr(cLinha,nPos+1,Len(cLinha)-nPos)
		nPos    := At(";",cLinha)
		cDesAnt := AllTrim(SubStr(cLinha,1,nPos-1)) //
		cLinha  := SubStr(cLinha,nPos+1,Len(cLinha)-nPos)
		
		nPos    := At(";",cLinha)
		cCodNov := AllTrim(SubStr(cLinha,1,nPos-1)) //
		cLinha  := SubStr(cLinha,nPos+1,Len(cLinha)-nPos)
		
		nPos    := At(";",cLinha)
		cDesNov := AllTrim(SubStr(cLinha,1,nPos-1)) //Novo
		cLinha  := SubStr(cLinha,nPos+1,Len(cLinha)-nPos)
	
	    FAtuaCT1(cCodAnt,cCodNov) 
		
		FT_FSkip()
	enddo  
	FT_FUSE()
	MsgInfo("Arquivo processado!!!") 
Return     


Static Function FAtuaCT1(cCodAnt,cCodNov) 
	Local cQuery := ""              
	//Se conta for igual não houve atualização. 
	If AllTrim(cCodAnt) == AllTrim(cCodNov) 
	   Return 
	EndIf 
	
	//Atualiza plano de contas. 
	cQuery := "UPDATE " + RetSqlName("CT1") + " SET CT1_CONTA = '" + cCodNov + "' WHERE CT1_CONTA = '" + cCodAnt + "'" 
	
	TcSQLExec(cQuery) 
	
	//Atualiza fornecedores. 
	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SA2") + " SET A2_CONTA = '" + cCodNov + "' WHERE A2_CONTA = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 
	
	
	//Atualiza produtos. 
	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SB1") + " SET B1_CONTA = '" + cCodNov + "' WHERE B1_CONTA = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 
	
	//Atualiza Bancos. 
	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SA6") + " SET A6_CONTA = '" + cCodNov + "' WHERE A6_CONTA = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 
	
	
	//Atualiza naturezas. 
	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SED") + " SET ED_CONTA = '" + cCodNov + "' WHERE ED_CONTA = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 
	
	//Atualiza clientes. 
	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SA1") + " SET A1_CONTA = '" + cCodNov + "' WHERE A1_CONTA = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 
	
	
	//Atualiza itens do ativo. 
	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SN3") + " SET N3_CCONTAB = '" + cCodNov + "' WHERE N3_CCONTAB = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 
	
	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SN3") + " SET N3_CDEPREC = '" + cCodNov + "' WHERE N3_CDEPREC = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 
	
	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SN3") + " SET N3_CCDEPR = '" + cCodNov + "' WHERE N3_CCDEPR = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 
	
	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SN3") + " SET N3_CDESP  = '" + cCodNov + "' WHERE N3_CDESP  = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 
	
	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SN3") + " SET N3_CCORREC  = '" + cCodNov + "' WHERE N3_CCORREC  = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 
	
	
	//Atualiza grupos do ativo. 
	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SNG") + " SET NG_CCONTAB = '" + cCodNov + "' WHERE NG_CCONTAB = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 
	
	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SNG") + " SET NG_CDEPREC = '" + cCodNov + "' WHERE NG_CDEPREC = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 
	
	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SNG") + " SET NG_CCDEPR = '" + cCodNov + "' WHERE NG_CCDEPR = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 
	
	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SNG") + " SET NG_CDESP  = '" + cCodNov + "' WHERE NG_CDESP  = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 
	
	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SNG") + " SET NG_CCORREC  = '" + cCodNov + "' WHERE NG_CCORREC  = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 

/*****
Incluido por Jordano em 27/12/13
*****/

	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SRV") + " SET RV_CTADEB3  = '" + cCodNov + "' WHERE RV_CTADEB3  = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 

	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SRV") + " SET RV_CTADEB2  = '" + cCodNov + "' WHERE RV_CTADEB2  = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 

	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SRV") + " SET RV_CTADEB  = '" + cCodNov + "' WHERE RV_CTADEB  = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 

	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SRV") + " SET RV_CTACRED  = '" + cCodNov + "' WHERE RV_CTACRED  = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 

	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SRV") + " SET RV_CTCRED2  = '" + cCodNov + "' WHERE RV_CTCRED2  = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 

	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SRV") + " SET RV_CTCRED3  = '" + cCodNov + "' WHERE RV_CTCRED3  = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 
    
/*****
Incluido por Jordano em 07/01/14
*****/
	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SB1") + " SET B1_CTBACUS  = '" + cCodNov + "' WHERE B1_CTBACUS  = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 
    
	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SB1") + " SET B1_CTBACI  = '" + cCodNov + "' WHERE B1_CTBACI  = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 
    
	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SB1") + " SET B1_CTBADES  = '" + cCodNov + "' WHERE B1_CTBADES  = '" + cCodAnt + "'" 
	TcSQLExec(cQuery)  

    
/*****
Incluido por Jordano em 28/01/14       

*****/
	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SBM") + " SET BM_CONTA  = '" + cCodNov + "' WHERE BM_CONTA  = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 
    
	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SBM") + " SET BM_CTBADES  = '" + cCodNov + "' WHERE BM_CTBADES  = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 
    
	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SBM") + " SET BM_CTBACUS  = '" + cCodNov + "' WHERE BM_CTBACUS  = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 
    
	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("SBM") + " SET BM_CTBACI  = '" + cCodNov + "' WHERE BM_CTBACI  = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 

/*****
Incluido por Jordano em 06/03/14       

*****/ 
   
	cQuery := "" 
	cQuery := "UPDATE " + RetSqlName("CNB") + " SET CNB_CONTA  = '" + cCodNov + "' WHERE CNB_CONTA  = '" + cCodAnt + "'" 
	TcSQLExec(cQuery) 

Return 
