#include "rwmake.ch"

User function ValidaEnd()

Local cAtual 
Local cLog
Local cNr
Local cCompl 
Local cEnd
cAtual := __READVAR

IF "A1_" $ __READVAR
	IF "M->A1_LOGRAD" == cAtual
		cLog := &(cAtual)
		cNr := M->A1_NR_END
		cCompl := M->A1_COMPLEM
	ENDIF
	IF "M->A1_NR_END" == cAtual
		cLog := M->A1_LOGRAD
		cNr := &(cAtual)
		cCompl := M->A1_COMPLEM
	ENDIF
	IF "M->A1_COMPLEM" == cAtual
		cLog := M->A1_LOGRAD
		cNr := M->A1_NR_END
		cCompl := &(__READVAR)
	ENDIF               
	cEnd := ALLTRIM(cLog) + ", " + ALLTRIM(cNr) + " "+ ALLTRIM(cCompl)
	aTam := TamSX3("A1_END")	
	M->A1_END := substr(cEnd,1,aTam[1])
	M->A1_ENDSR := substr(cEnd,1,aTam[1])
	IF LEN(M->A1_END) > aTam[1]
		ALERT("OS VALORES DIGITADOS EM LOGRADOURO, NUMERO E COMPLEMENTO EXCEDERAM O TAMANHO DO CAMPO ENDERECO, REAJUSTE O CAMPO ENDERECO!")
	ENDIF
ENDIF

IF "A2_" $ __READVAR
	IF "M->A2_LOGRAD" == cAtual
		cLog := &(cAtual)
		cNr := M->A2_NR_END
		cCompl := M->A2_COMPLEM
	ENDIF
	IF "M->A2_NR_END" == cAtual
		cLog := M->A2_LOGRAD
		cNr := &(cAtual)
		cCompl := M->A2_COMPLEM
	ENDIF
	IF "M->A2_COMPLEM" == cAtual
		cLog := M->A2_LOGRAD
		cNr := M->A2_NR_END
		cCompl := &(__READVAR)
	ENDIF      
	cEnd := ALLTRIM(cLog) + ", " + ALLTRIM(cNr) + " "+ ALLTRIM(cCompl)
	aTam := TamSX3("A2_END")	
	M->A2_END := substr(cEnd,1,aTam[1])  
	M->A2_ENDSR := substr(cEnd,1,aTam[1])	
	IF LEN(M->A2_END) > aTam[1]
		ALERT("OS VALORES DIGITADOS EM LOGRADOURO, NUMERO E COMPLEMENTO EXCEDERAM O TAMANHO DO CAMPO ENDERECO, REAJUSTE O CAMPO ENDERECO!")
	ENDIF
ENDIF

IF "A4_" $ __READVAR
	IF "M->A4_LOGRAD" == cAtual
		cLog := &(cAtual)
		cNr := M->A4_NR_END
		cCompl := M->A4_COMPLEM
	ENDIF
	IF "M->A4_NR_END" == cAtual
		cLog := M->A4_LOGRAD
		cNr := &(cAtual)
		cCompl := M->A4_COMPLEM
	ENDIF
	IF "M->A4_COMPLEM" == cAtual
		cLog := M->A4_LOGRAD
		cNr := M->A4_NR_END
		cCompl := &(__READVAR)
	ENDIF
	cEnd := ALLTRIM(cLog) + ", " + ALLTRIM(cNr) + " "+ ALLTRIM(cCompl)
	aTam := TamSX3("A4_END")	
	M->A4_END := substr(cEnd,1,aTam[1])
	IF LEN(cEnd) > aTam[1]
		ALERT("OS VALORES DIGITADOS EM LOGRADOURO, NUMERO E COMPLEMENTO EXCEDERAM O TAMANHO DO CAMPO ENDERECO, REAJUSTE O CAMPO ENDERECO!")
	ENDIF
ENDIF

Return (.T.)