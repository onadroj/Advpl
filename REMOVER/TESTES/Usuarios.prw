#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³USUARIOS   º Autor ³ EDSON              º Data ³  30/11/11  º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ Gera arquivo texto com a relação de usuarios do sistema    º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP6 IDE                                                    º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User function Usuarios()
Private lEnd  := .F.
Private lAbortPrint  := .F.
Private CbTxt      := ""
Private limite     := 132
Private tamanho    := "G"
Private nomeprog   := "RELALCADAS"
Private nTipo      := 18
Private aReturn    := {"Zebrado",1,"Administracao",2,2,1,"",1}
Private nLastKey   := 0
Private cPerg      := ""
Private cbtxt      := Space(10)
Private cbcont     := 00
Private CONTFL     := 01
Private m_pag      := 01
Private wnrel      := "RELALCADAS"
Private aModulos := {}
Private aGrupos := {"CONSTRUTORA","FRESAR","ALMAQ","MEGA LOCAÇÃO","MOVING","MEGANOVA","CSC","TRANSPOR","LOGÍSTICA"}
Private nLin := 80

Private titulo  := "Permissões de acesso dos usuários."

Private Cabec1  := "USUÁRIO                  MÓDULO                                  ROTINA"
Private Cabec2  := ""

For x:= 1 to 70
	aAdd(aModulos,"OUTRO") 
Next
aModulos[1] := "ATF - ATIVO IMOBILIZADO"
aModulos[2] := "COM - COMPRAS" 
aModulos[3] := "CON - CONTABILIDADE"
aModulos[4] := "EST - ETOQUE/CUSTOS"
aModulos[5] := "FAT - FATURAMENTO"
aModulos[6] := "FIN - FINANCEIRO"
aModulos[7] := "GPE - GESTÃO DE PESSOAL"
aModulos[9] := "FIS - LIVROS FISCAIS"
aModulos[13] := "TMK - CALL CENTER"
aModulos[15] := "RPM - REPORT"
aModulos[16] := "PON - PONTO ELETRÔNICO"
aModulos[19] := "MNT - MANUTENÇÃO DE ATIVOS"
aModulos[20] := "MDT - MED. DO TRABALHO"
aModulos[26] := "TRM - TREINAMENTO"
aModulos[34] := "CTB - CONTABILIDADE GERENCIAL"
aModulos[35] := "MDT - MEDICINA E SEGURANÇA DO TRABALHO"
aModulos[69] := "GCT - GESTÃO DE CONTRATOS"


//MsgStop("Modulo: "+str(nModulo)+" - "+cModulo)
//Return()

//ArqModulos()

RelAlcadas()
Return()

Static Function RelAlcadas()
Local cDesc1  := "Emissão da relação de usuários do"
Local cDesc2  := "sistema e permissões de acesso"
Local cDesc3  := "por módulo."
Local cPict   := ""

Local imprime := .T.
Local aOrd  := {}
Local cString := "SA1"

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Declaracao de Variaveis                                             ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ



dbSelectArea("SA1")
dbSetOrder(1)

//ValidPerg()
//pergunte(cPerg,.T.)

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
RptStatus({|| RunReport(Cabec1,Cabec2,Titulo) },Titulo)
Return


Static Function RunReport(Cabec1,Cabec2,Titulo)
Local _cLinha := ""
Local _cTexto := ""
Local _aNivel := {}
Local _aEnable := {}
Local _nNivel := 0
Local _lListar := .F.   
Local _lItem := .F.
Local _cTxt := ""     
Local _nPos := 0
Local _N := 0
Private aarray := {}
psworder(1)       
SetRegua(200)
nreg:="000001"
While nreg<"000200"
	If lAbortPrint
		@nLin,00 PSAY "*** CANCELADO PELO OPERADOR ***"
		Exit
	Endif
	If nLin > 60
		Cabec(Titulo,Cabec1,Cabec2,NomeProg,Tamanho,nTipo)
		nLin := 9
	Endif    
	IncRegua()
	 IF pswseek(nreg)
//		_N:=0
		aarray:=pswret()
        If Len(aarray)>0
	//      aarray[1][2] - usuario
	//      aarray[3][x],3,1 <> "X" - indica que o modulo e usado
	//      aarray[1][10][x] - codigo dos grupos que o usuario participa
			nLin++
		    @nLin,000 PSAY Upper(Alltrim(aarray[1][2]))+" - "+IIF(len(aarray[1][10])>0, IIF(aarray[1][10][1]=="000000","ADMINISTRADORES",aGrupos[val(aarray[1][10][1])]),"")
	//		@nLin,00 PSAY "Modulos:"
	//		nLin++
			for _nPos:=1 to len(aarray[3])  
				IF SUBSTR(aarray[3][_nPos],3,1)<>"X"
	//				@nLin,00 PSAY aarray[1][2]+SPACE(10)+	aarray[3][x]
					nLin++
					@nLin,025 PSAY IIF(val(substr(aarray[3][_nPos],1,2))>70,"OUTRO",aModulos[val(substr(aarray[3][_nPos],1,2))])
	//				nLin++             
					If aarray[1][1]<>"000000"
					   ListaMenu(SUBSTR(aarray[3][_nPos],4,Len(aarray[3][_nPos])-3))
					Else
					   @nLin,65 PSAY "Acesso completo"
					   nLin++
					Endif   
				ENDIF
				If nLin > 60
					Cabec(Titulo,Cabec1,Cabec2,NomeProg,Tamanho,nTipo)
					nLin := 9
				Endif
			next                            
		Endif
		//@nLin,026 PSAY QRY->E2_FORNECE+" / "+QRY->E2_LOJA
		@nLin,000 PSAY Replicate("-",30)
		nLin++
	ELSE
//		_N++
//		IF _N==10
//			nreg:="999998"
//		ENDIF		
	ENDIF
	nreg:=strzero(val(nreg)+1,6)
EndDo

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


Static Function ListaMenu(_ArqMenu)
Local _cLinha := ""
Local _cTexto := ""
Local _aNivel := {}
Local _aEnable := {}
Local _nNivel := 0
Local _lListar := .F.   
Local _lItem := .F.
Local _cTxt := ""  
Local _nPos := 0 
Local _cErro := 0

For _nPos:= 1 To 10      
  aAdd(_aNivel,"")
  aAdd(_aEnable,.F.)
Next

//ARQUIVO TEXTO
//nH := fcreate("\tmp_xnu.txt")
//If nH == -1
//	MsgStop('Erro de abertura : FERROR '+str(ferror(),4))
//Else

_cErro := FT_FUse(_ArqMenu) // Abre o arquivo de menu
If _cErro==-1
	MsgStop("Erro ao abrir o menu "+_ArqMenu+" Usuário: "+aarray[1][2])
Else
	FT_FGOTOP()
	While !FT_FEof()    
		_cLinha:=AllTrim(FT_FReadln())
	    _cTexto := LimpaLinha(_cLinha)
//        If _cTexto == '<Menu Status="Enable">' .OR. _cTexto == '<MenuItem Status="Enable">'
        If Substr(_cTexto,1,13) == '<Menu Status=' .OR. Substr(_cTexto,1,17) == '<MenuItem Status='
           If _cTexto == '<Menu Status="Enable">'
            If !_lItem
            	_nNivel++
            Else
           		_lItem:= .F.
           	Endif
           	_aEnable[_nNivel]:= .T.
           ElseIf _cTexto == '<MenuItem Status="Enable">'
             If !_lItem
            	_nNivel++
            	_lItem:= .T.
			 Endif
            _aEnable[_nNivel]:= .T.
		   ElseIf Substr(_cTexto,1,13) == '<Menu Status='	
            If !_lItem
            	_nNivel++
            Else
	           	_lItem:= .F.
	    	Endif
            _aEnable[_nNivel]:= .F.
		   ElseIf Substr(_cTexto,1,17) == '<MenuItem Status='
             If !_lItem
               _nNivel++
	           _lItem:= .T.
	    	 Endif
            _aEnable[_nNivel]:= .F.
           Endif
        ElseIf Substr(_cTexto,1,11) == "</MenuItem>"  
            _lListar:=.T.
            For _nPos:=1 to _nNivel
               If !_aEnable[_nPos]
                  _lListar:=.F.
                  Exit
               Endif
            Next
	 	    If _lListar   
	 	        _cTxt:= ""
	 	    	For _nPos:=1 to _nNivel
	 	    	  _cTxt+="->"+_aNivel[_nPos]
	 	    	Next
//				fwrite(nH,_cTxt) 
				@nLin,65 PSAY _cTxt
				nLin++
				If nLin > 60
					Cabec(Titulo,Cabec1,Cabec2,NomeProg,Tamanho,nTipo)
					nLin := 9
				Endif
			Endif
        ElseIf Substr(_cTexto,1,7) == "</Menu>"
            If _lItem
              _aEnable[_nNivel]:= .F.
              _nNivel:=_nNivel-1
              _lItem:= .F.
            Endif
            _aEnable[_nNivel]:= .F.
            _nNivel:=_nNivel-1
	 	Else 
		 	If Substr(_cTexto,1,17) == '<Title lang="pt">' 
		       _aNivel[_nNivel]:=LeTitulo(_cTexto)
		    ElseIf Substr(_cTexto,1,10) == '<Function>'
		       _aNivel[_nNivel]:=_aNivel[_nNivel]+" (Função: "+LeFuncao(_cTexto)+")"
		    ElseIf Substr(_cTexto,1,8) == '<Access>'
		       _aNivel[_nNivel]:=_aNivel[_nNivel]+" Permissões: "+LeAcesso(_cTexto)
			Endif
        Endif
		FT_FSkip()
	Enddo
	FT_FUse() // Fecha o arquivo de menu
//	fclose(nH) // Fecha o arquivo texto
//Endif
Endif
Return()

Static Function LimpaLinha(TxtLinha)
Local _Texto := "" 
Local _nPos:=0
For _nPos:=1 to len(TxtLinha)
  If Substr(TxtLinha,_nPos,1)<>"&" .AND. Substr(TxtLinha,_nPos,1)<>chr(9) 
     _Texto+=Substr(TxtLinha,_nPos,1)
  Endif
Next
Return(_Texto)       

Static Function LeTitulo(TxtLinha)
Local _Texto := ""                
Local _nPos:=0

For _nPos:=18 to len(TxtLinha)
     If Substr(TxtLinha,_nPos,1)=="<"
       Exit
     Else
       _Texto+=Substr(TxtLinha,_nPos,1)
     Endif
Next
Return(_Texto)  

Static Function LeFuncao(TxtLinha)
Local _Texto := ""  
Local _nPos:=0

For _nPos:=11 to len(TxtLinha)
     If Substr(TxtLinha,_nPos,1)=="<"
       Exit
     Else
       _Texto+=Substr(TxtLinha,_nPos,1)
     Endif
Next
Return(_Texto)

Static Function LeAcesso(TxtLinha)
Local _Texto := ""
For X:=9 to len(TxtLinha)
     If Substr(TxtLinha,X,1)=="<"
       Exit
     Else
       _Texto+=Substr(TxtLinha,X,1)
     Endif
Next
Return(_Texto)

Static Function ArqModulos()
//ARQUIVO TEXTO
nH := fcreate("c:\usuarios.txt")
If nH == -1
	MsgStop('Erro de abertura : FERROR '+str(ferror(),4))
Endif

psworder(1)
nreg:="000000"
_N:=0
While nreg<"999999"
 //	If nLin > 55 // Salto de Página. Neste caso o formulario tem 55 linhas...
 //	Cabec(Titulo,Cabec1,Cabec2,NomeProg,Tamanho,nTipo)
 //		nLin := 8 
 //	Endif
	 IF pswseek(nreg)
		_N:=0
		aarray:=pswret()
        If Len(aarray)>0
	//      aarray[1][2] - usuario
	//      aarray[3][x],3,1 <> "X" - indica que o modulo e usado
	//      aarray[1][10][x] - codigo dos grupos que o usuario participa
	//		@nLin,00 PSAY "Usuario: " + aarray[1][2] + " - Grupo: " + IIF(len(aarray[1][10])>0, IIF(aarray[1][10][1]=="000000","Administradores",aGrupos[val(aarray[1][10][1])]),"")
			cLinha := aarray[1][2]+";"+IIF(len(aarray[1][10])>0, IIF(aarray[1][10][1]=="000000","Administradores",aGrupos[val(aarray[1][10][1])]),"")+";"
	//		nLin++
	//		@nLin,00 PSAY "Modulos:"
	//		nLin++
			for x:=1 to len(aarray[3])  
				IF SUBSTR(aarray[3][x],3,1)<>"X"
	//				@nLin,00 PSAY aarray[1][2]+SPACE(10)+	aarray[3][x]
	//				@nLin,00 PSAY IIF(val(substr(aarray[3][x],1,2))>40,"Outros",aModulos[val(substr(aarray[3][x],1,2))])
					cLinha += IIF(val(substr(aarray[3][x],1,2))>40,"Outros",aModulos[val(substr(aarray[3][x],1,2))])+";"
	//				nLin++
				ENDIF
			next                            
		Endif
	ELSE
		_N++
		IF _N==10
			nreg:="999998"
		ENDIF		
	ENDIF
	cLinha += chr(10)+chr(13)
	fwrite(nH,cLinha)
	nreg:=strzero(val(nreg)+1,6)
EndDo
fclose(nH)
MsgStop("Arquivo gerado com êxito!")
Return           
