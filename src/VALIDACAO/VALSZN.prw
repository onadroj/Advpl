#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณVALSZN    บ Autor ณ GATASSE            บ Data ณ  04/03/04   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDescricao ณ VALIDA CADASTRO DE RDE                                     บฑฑ
ฑฑบ          ณ                                                            บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ AP6 IDE                                                    บฑฑ
ฑฑศออออออออออฯออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
/*/

User Function VALSZN
LOCAL RET:=.T., AAREA
PRIVATE nCol1,nCol2,nCol3,nCol4,nCol5
AAREA:=GETAREA()
nCol1 := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "ZN_HORAI"}))
nCol2 := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "ZN_HORAF"}))
nCol3 := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "ZN_THORA"}))
nCol4 := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "ZN_NOMOCOR"}))
nCol5 := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "ZN_TIPO"}))
nCol6 := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "ZN_CODOCOR"}))
IF PARAMIXB=="L"
	IF DTOC(_ZN_DATA)=="  /  /  " .OR. _ZN_CONTADO==0
		RET:=.F.
		MSGSTOP("Campos obrigatorios no cabecalho nao foram preenchidos!")
	ENDIF
	if ret
		IF (alltrim(ACOLS[N,nCol1])=="" .or. alltrim(ACOLS[N,nCol2])=="" .or. alltrim(ACOLS[N,nCol6])=="").and. acols[n,len(acols[n])]==.f.
			RET:=.F.
			MSGSTOP("Campos obrigatorios na grade nao foram preenchidos!")
		ENDIF
	ENDIF
ENDIF
if inclui
	if VALDUP()
		RET:=.F.
		MSGSTOP("Ja existe RDE lancado para este Bem , Data e Operador!")
	ENDIF
endif

IF PARAMIXB=="T"
	IF DTOC(_ZN_DATA)=="  :  " .OR. _ZN_CONTADO==0
		RET:=.F.
		MSGSTOP("Campos obrigatorios no cabecalho nao foram preenchidos!")
	ENDIF
	if ret
		for x:=1 to len(acols)
			if acols[x,len(acols[1])]<>.t.
				if alltrim(acols[x,nCol1])="  :  " .or. alltrim(acols[x,nCol2])="  :  " .or.;
					alltrim(acols[x,nCol6])=""
					RET:=.F.
					MSGSTOP("Campos obrigatorios na grade nao foram preenchidos!")
				endif
			endif
		next
	endif
ENDIF

if __readvar=="M->ZN_HORAI" .or. __readvar=="M->ZN_HORAF"
	if len(alltrim(&__readvar))<>5
		msgstop("Hora invalida!")
		ret:=.f.
	elseif alltrim(&__readvar)<>""
		nHora:=val(substr(&__readvar,1,2))
		nMin:=val(substr(&__readvar,4,2))
		if nHora<0 .or. nHora >23
			msgstop("Hora invalida!")
			ret:=.f.
		endif
		if nMin<0 .or. nMin >59
			msgstop("Hora invalida!")
			ret:=.f.
		endif
		if __readvar=="M->ZN_HORAI" .and. &__readvar<>"  :  "
			nV1:=iif(__readvar=="M->ZN_HORAI",VALHORA(&__readvar),VALHORA(acols[N,nCol1]))
			IF n>1 .and. IIF(type("acols")="U",.F.,IIF(LEN(ACOLS)>1,.T.,.F.))
				Y:=0
				FOR X:=N-1 TO 1 STEP -1
					IF ACOLS[X,LEN(ACOLS[1])]<>.T.
						y:=x
						exit
					ENDIF
				NEXT
				IF Y<>0 .AND.	&__readvar<>Acols[y,nCol2]
					MsgStop("Hora Inicial precisa ser identica `a Hora Final no ultimo lancamento valido!")
					ret:=.f.
				ENDIF
			ENDIF
		endif
		if ret
			if (__readvar=="M->ZN_HORAI" .and. &__readvar<>"  :  " .and. alltrim(acols[N,nCol2])<>"") .or.;
				(__readvar=="M->ZN_HORAF" .and. &__readvar<>"  :  " .and. alltrim(acols[N,nCol1])<>"" )
				nV1:=iif(__readvar=="M->ZN_HORAI",VALHORA(&__readvar),VALHORA(acols[N,nCol1]))
				nV2:=iif(__readvar=="M->ZN_HORAF",VALHORA(&__readvar),VALHORA(acols[N,nCol2]))
				if nV2<nV1
					nTot:=1440-nV1+nV2
				else
					nTot:=nV2-nV1
				endif
				acols[N,nCol3]:=STRHORA(nTot)
				HORAST()
			endif
		endif
	endif
endif
if __readvar=="M->ZN_CODOCOR"
	AAREA:=GETAREA()
	DBSELECTAREA("SZO")
	DBSETORDER(1)
	DBSEEK(XFILIAL("SZO")+&__READVAR)
	acols[N,nCol4]:=SZO->ZO_NOME
	acols[N,nCol5]:=SZO->ZO_TIPO
	RESTAREA(AAREA)
	HORAST()
	if valhora(cHoras)>1440
		msgstop("Periodo informado acima de 24 horas!")
		ret:=.f.
	endif
ENDIF
IF __readvar=="_ZN_CODFUNC"
	IF !EXISTCPO("ST1",&__readvar)
		ret:=.f.
	else
		_ZN_NOMEFUN:=retfield("ST1",1,XFILIAL("ST1")+&__READVAR,"T1_NOME")
	endif
endif
IF __readvar=="_ZN_CLIENTE" .OR. __readvar=="_ZN_LOJA"
	IF __readvar=="_ZN_CLIENTE"
		cCli:=&__readvar
		cLj:=_ZN_LOJA
	else
		cCli:=_ZN_CLIENTE
		cLj:=&__readvar
	endif
	DBSELECTAREA("SA1")
	DBSETORDER(1)
	IF DBSEEK(XFILIAL("SA1")+cCli+cLj)
		_ZN_NOMCLI:=SA1->A1_NREDUZ
	else
		ret:=.f.
	endif
endif
IF __readvar=="_ZN_CODBEM"
	DBSELECTAREA("ST9")     //VERIFICA SE BEM EXISTE E RECUPERA DADOS
	DBSETORDER(1)
	IF DBSEEK(XFILIAL("ST9")+&__READVAR)
		_ZN_CENTRAB:=ST9->T9_CENTRAB
		_ZN_CCUSTO:=ST9->T9_CCUSTO
		_ZN_CONTINI	:=ST9->T9_POSCONT
		_ZN_DTINIC:=ST9->T9_DTULTAC
		_ZN_NOMEBEM:=ST9->T9_NOME
	ELSE
		ret:=.f.
	endif
endif
RESTAREA(AAREA)
Return(ret)

STATIC FUNCTION VALDUP()
LOCAL RET,AAREA
RET:=.F.
AAREA:=GETAREA()
dbSelectArea("SZN")
dbSetOrder(1)//ZN_FILIAL+ZN_CODBEM+DTOS(ZN_DATA)+ZN_CODFUNC+ZN_NUM+ZN_ITEM
dbSeek(xFilial("SZN")+_ZN_CODBEM+DTOS(_ZN_DATA)+_ZN_CODFUNC,.T.)
IF xFilial("SZN")+_ZN_CODBEM+DTOS(_ZN_DATA)+_ZN_CODFUNC==SZN->ZN_FILIAL+SZN->ZN_CODBEM+DTOS(SZN->ZN_DATA)+SZN->ZN_CODFUNC
	RET:=.T.
ENDIF
RESTAREA(AAREA)
RETURN(RET)

STATIC function VALHORA(N)
LOCAL RET
nHora:=val(substr(N,1,2))
nMin:=val(substr(N,4,2))
ret:=nHora*60+nMin
RETURN(RET)

STATIC function STRHORA(N)
LOCAL RET
cHora:=strzero((int(n/60)),2)
cMin:=strzero((n%60),2)
ret:=chora+":"+cMin
RETURN(RET)

STATIC FUNCTION HORAST()
LOCAL T:=0
LOCAL T1:=0
LOCAL T2:=0

FOR X:=1 TO LEN(ACOLS)
	T+=VALHORA(ACOLS[X,nCol3])
	if acols[x,nCol5]=="T"
		T1+=VALHORA(ACOLS[X,nCol3])
	endif
	if acols[x,nCol5]=="M"
		T2+=VALHORA(ACOLS[X,nCol3])
	endif
NEXT
cHoras:=STRHORA(T)
cHorast:=STRHORA(T1)
cHorasm:=STRHORA(T2)
RETURN
