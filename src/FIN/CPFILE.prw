#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³CPFILE    º Autor ³ GATASSE            º Data ³  23/09/04   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ COPIA ARQUIVOS DE E PARA A MAQUINA DO USUARIO              º±±
±±º          ³ LOCAL ORIGEM SIGAADV                                       º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP6 IDE                                                    º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User Function CPFILE
//_cVersao:=getversao()
//if "TESTE" $ UPPER(_cVersao)
//	PRIVATE cCaminho1 := "\sigateste\"
//else
PRIVATE cCaminho1 := "\system\"
//endif
PRIVATE aImporta  := Directory(cCaminho1+"*.REM")
PRIVATE cCaminho2 := "c:\cnab\"+space(40)
PRIVATE nList     := 1
PRIVATE aItems    := {}
PRIVATE aRadio := {"Do Siga => Para Micro Local","Do Micro Local => Para o Siga"}
PRIVATE nRadio := 1
@ 0,0 TO 400,400 DIALOG oDlg TITLE "Copia de Arquivos CNAB"
@ 10,5 SAY "Sentido"
@ 10,30 RADIO aRadio VAR nRadio
@ 30,5  SAY "Path dos Arquivos no Micro Local"
@ 40,5 get cCaminho2 size 80,6 valid fvalida()
@ 60,5 SAY "Selecione arquivo a copiar "
@ 70,5 LISTBOX nList ITEMS aItems SIZE 180,100
@ 180,80 BMPBUTTON TYPE 01 ACTION fProcessa()
@ 180,120 BMPBUTTON TYPE 02 ACTION Close(oDlg)
ACTIVATE DIALOG oDlg CENTER
Return


Static Function fProcessa()
aImporta  :={}
cCaminho2:=alltrim(cCaminho2)
if substr(cCaminho2,len(cCaminho2),1)<>"\"
	cCaminho2+="\"
endif
Close(oDlg)
IF nRadio==1
	aImporta  := Directory(cCaminho1+"*.REM")
else
	aImporta  := Directory(cCaminho2+"*.RET")
endif
nList     := 1
aItems    := {}
For nCont := 1 to Len(aImporta)
	Aadd(aItems,aImporta[nCont,1]+" | "+Str(aImporta[nCont,2],20)+"  |  "+	Dtoc(aImporta[nCont,3])+"  |  "+aImporta[nCont,4])
Next
dlgrefresh(oDlg)
cArqName:=space(30)
@ 0,0 TO 400,400 DIALOG oDlg TITLE "Copia de Arquivos"
@ 10,5 SAY "Sentido "+aRadio[nRadio]
@ 20,5 SAY "Path dos Arquivos no Micro Local"
@ 30,5 say cCaminho2 size 80,6
//if nRadio==2
//	@ 40,5 SAY "Nome do arquivo no destino sem extensao"
//	@ 50,5 get cArqName size 80,6 valid fvalida()
//endif
@ 60,5 SAY "Selecione arquivo a copiar "
@ 70,5 LISTBOX nList ITEMS aItems SIZE 180,100 sorted
@ 180,80 BMPBUTTON TYPE 01 ACTION fProcessa1()
@ 180,120 BMPBUTTON TYPE 02 ACTION Close(oDlg)
ACTIVATE DIALOG oDlg CENTER
return

Static Function fProcessa1()
Close(oDlg)
// INICIO criado para não dar erro se não tiver arquivos no diretório especificado.
if type("aitems[nlist]") == "U"
	MsgStop("Não há arquivo para ser copiado!")
else
	if alltrim(AITEMS[NLIST])<>"" //Aparace o erro: array out of bounds, quando não tem nenhum arquivo.
		cName:=substr(AITEMS[NLIST],1,at("|",AITEMS[NLIST])-1)
		if nRadio==1
			cSource:=cCaminho1+cname
			cTarget:=cCaminho2+cName
		else
			cSource:=cCaminho2+cname
			if alltrim(cArqName)<>""
				cTarget:=cCaminho1+alltrim(cArqName)
			else
				cTarget:=cCaminho1+cName
			endif
		endif
		faz:=.t.
		if file(cTarget)
			If 	MsgBox("Arquivo "+cTarget+" existe. Sobrescrever?","CPFILE","YESNO")
				faz:=.t.
			else
				faz:=.f.
			endif
		endif
		if faz
			U_filecopy(cSource,cTarget)
		endif
	endif
endif

return


Static Function fvalida()
local ret:=.t.
/*
if at(".",cArqName)>0
msgstop("Nome do arquivo nao pode ter extensao (.)")
ret:=.f.
endif
if at(":",cArqName)>0
msgstop("Nome do arquivo nao pode conter :")
ret:=.f.
endif
if at("\",cArqName)>0
msgstop("Nome do arquivo nao pode conter \")
ret:=.f.
endif
*/
return(ret)
