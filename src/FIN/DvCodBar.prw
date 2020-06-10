#include "protheus.ch"
#include "tbiconn.ch"
#include "rwmake.ch"

User Function DVCODBAR()
/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³DVCODBAR  ºAutor  ³GATASSE             º Data ³  01/09/03   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³ VALIDA CODIGO DE BARRA DE E2_CODBAR                        º±±
±±º          ³                                                            º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP                                                         º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/

local	_cFixo1   := "4329876543298765432987654329876543298765432"
local ret:=.t.
local _BarCod,nSomaGer,nI,OK
Local cCGC
Local cRep

DbSelectArea("SA2")
cRep := GetAdvFVal("SA2","A2_REPRCGC",xFilial("SA2")+E2_FORNECE+E2_LOJA,1,"Erro")       
cCGC := GetAdvFVal("SA2","A2_CGC",xFilial("SA2")+E2_FORNECE+E2_LOJA,1,"Erro")       
MsgAlert("Confira o CNPJ do Cedente no Boleto" + Chr(13) + Chr(10) + Chr(13) + Chr(10) + "CNPJ: " + cCGC + Chr(13) + Chr(10) + "Representante:"+cRep)

if alltrim(M->E2_CODBAR)<>""
	_BarCod:=M->E2_CODBAR
	_DG:=SubStr(_BarCod,5,1)
	_BarCod:=Left(_BarCod ,4)+ SubStr(_BarCod,6)
	nSomaGer := 0
	For nI := 1 to 43
		nSomaGer := nSomaGer + ;
		(Val(Substr(_BarCod,nI,1))*Val(Substr(_cFixo1,nI,1)))
	Next
	If (((11-(nSomaGer%11)) > 9) .or. ((11-(nSomaGer%11))== 0) .or. ((11-(nSomaGer%11))== 1))
		cCalcDv := "1"
	Else
		cCalcDv := ALLTRIM(Str(11-(nSomaGer%11),1))
	Endif
	IF ((cCalcDv<>_DG) .OR. (LEN(alltrim(M->E2_CODBAR))<>44))
		msgstop("Codigo Invalido!")
		ret:=.f.
	endif
else
	If MsgYesNo("Deseja entrar com a linha digitavel?")
		part1:=space(5)
		part2:=space(5)
		part3:=space(5)
		part4:=space(6)
		part5:=space(5)
		part6:=space(6)
		part7:=space(1)
		part8:=space(14)
		OK:=.F.
		REPETE:=.T.
		//99999.99999 99999.999999 99999.999999 9 99999999999999
		WHILE REPETE
			DEFINE MSDIALOG oDlgX TITLE "Linha Digitavel" FROM 0,0 TO 10,75
			@ 15,14 Say "Entre com a Linha Digitavel"
			@ 25,03  Get part1 picture "99999" VALID (LEN(ALLTRIM(part1))=5)
			@ 25,34  Get part2 picture "99999" VALID (LEN(ALLTRIM(part2))=5)
			@ 25,70  Get part3 picture "99999" VALID (LEN(ALLTRIM(part3))=5)
			@ 25,101 Get part4 picture "999999" VALID (LEN(ALLTRIM(part4))=6)
			@ 25,132 Get part5 picture "99999" VALID (LEN(ALLTRIM(part5))=5)
			@ 25,163 Get part6 picture "999999" VALID (LEN(ALLTRIM(part6))=6)
			@ 25,190 Get part7 picture "9" VALID (LEN(ALLTRIM(part7))=1)
			@ 25,203 Get part8 picture "99999999999999"  SIZE 80,10 VALID (LEN(ALLTRIM(part8))=14)
			@ 40,10 BUTTON "Ok" SIZE 40,10 ACTION (OK:=.T.,Close(oDlgX))
			@ 40,60 BUTTON "Cancelar" SIZE 40,10 ACTION (OK:=.F.,Close(oDlgX))
			ACTIVATE MSDIALOG oDlgX   CENTERED
			IF !OK
				REPETE:=.F.
			ELSEIF OK
				_cBloco   :=part1+substr(part2,1,4)+part3+substr(part4,1,5)+part5+substr(part6,1,5)
				_ld       :=part1+part2+part3+part4+part5+part6+part7+part8
				_cFixo2   := "21212121212121212121212121212"
				if len(alltrim(part8))<>14
					part8:=STRZERO(VAL(part8),14)					
				endif				
				nSoma1 := 0
				nSoma2 := 0
				nSoma3 := 0
				nSoma4 := 0
				
				// Calcula o DV do primeiro Bloco
				_FixVar := Right(_cFixo2,9)
				For nI := 1 to 9
					//       _nRes := Val(Substr(_cBloco,nI,1))*Val(Substr(_FixVar,nI,1))
					_nRes := Val(Substr(_cBloco,nI,1))*Val(Substr(_cFixo2,nI,1))
					If _nRes > 9
						_nRes := 1 + (_nRes-10)
					Endif
					nSoma1 := nSoma1 + _nRes
				Next
				
				// Calcula o DV do segundo bloco
				_FixVar := Right(_cFixo2,10)
				For nI := 10 to 19
					//       _nRes := Val(Substr(_cBloco,nI,1))*Val(Substr(_FixVar,nI,1))
					_nRes := Val(Substr(_cBloco,nI,1))*Val(Substr(_cFixo2,nI,1))
					If _nRes > 9
						_nRes := 1 + (_nRes-10)
					Endif
					nSoma2 := nSoma2 + _nRes
				Next
				
				// Calcula o DV do terceiro Bloco
				_FixVar := Right(_cFixo2,10)
				For nI := 20 to 29
					//       _nRes := Val(Substr(_cBloco,nI,1))*Val(Substr(_FixVar,nI,1))
					_nRes := Val(Substr(_cBloco,nI,1))*Val(Substr(_cFixo2,nI,1))
					If _nRes > 9
						_nRes := 1 + (_nRes-10)
					Endif
					nSoma3 := nSoma3 + _nRes
				Next
				cSoma1 := Right(StrZero(10-(nSoma1%10),2),1)
				cSoma2 := Right(StrZero(10-(nSoma2%10),2),1)
				cSoma3 := Right(StrZero(10-(nSoma3%10),2),1)
				
				// Uso as funcoes StrZero e Right para pegar o nro correto quando o resto
				// de nSoma/10 for 0
				
				// Monta sequencia de codigos para o topo do boleto com os dvs e o valor
						
				ld := Left(_cBloco,9) +  cSoma1 +;
					Substr(_cBloco,10,10) + cSoma2+ Substr(_cBloco,20,10)+ cSoma3 +;
					part7+part8
				if ld<>	_ld
					msgstop("Linha digitada contem erros!")
					ret:=.f.
				ELSE
					REPETE:=.F.
					_COD:=SUBSTR(_ld,1,4)+SUBSTR(_ld,33,1)+SUBSTR(_ld,34,4)+SUBSTR(_ld,38,10)+SUBSTR(_ld,5,4)+SUBSTR(_ld,9,1)+SUBSTR(_ld,11,1)+SUBSTR(_ld,12,9)+SUBSTR(_ld,22,2)+SUBSTR(_ld,24,8)
					M->E2_CODBAR:=_COD
					GETDREFRESH()
				endif
			ENDIF
		ENDDO
	endif
endif
RETURN(RET)
