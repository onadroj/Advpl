#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบPrograma  ณCHEKCONT  บ Autor ณ EDSON              บ Data ณ  19/08/05   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDescricao ณ Efetua checagem de contadores acumulados de bens no m๓dulo บฑฑ
ฑฑบ          ณ Manuten็ใo de Ativos                                       บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ AP6 IDE                                                    บฑฑ
ฑฑศออออออออออฯออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
/*/

User Function CHEKCONT
Private cPerg := "CHKCNT"

IF MSGBOX("Para a execu็ใo desta rotina ้ necessแrio que o m๓dulo de Manuten็ใo"+chr(13)+ ;
          "nใo esteja sendo utilizado por outros usuแrios!!!"+chr(13)+  ;
          "Deseja realmente efetuar os ajustes nos contadores acumulados?","CHEKCONT","YESNO")
  ValidPerg(cPerg)
  Pergunte(cPerg,.T.)

  Processa({|| FVerifica() },"Processando...")
ENDIF

Return


Static Function FVerifica()
Private cString := "ST9"
Private nContador := 0
Private nAcumulado := 0 
Private nAcum := 0
Private lAchou

dbSelectArea("ST9")
dbSetOrder(1)

DBGoTop()
COUNT to _RECCOUNT
DBGoTop()
ProcRegua(_RECCOUNT)
DBSeek(xFilial("ST9")+mv_par02,.T.)

WHILE !EOF()
    IncProc("Ajustando contadores do bem : "+AllTrim(ST9->T9_CODBEM))

    If ST9->T9_CODBEM < mv_par02 .OR. ST9->T9_CODBEM > mv_par03
       DbSkip()
       Loop
	Endif       

    If ST9->T9_CCUSTO < mv_par04 .OR. ST9->T9_CCUSTO > mv_par05
       DbSkip()
       Loop
	Endif       

    If ST9->T9_TEMCONT<>"S" .OR. ST9->T9_SITBEM<>"A"
       DbSkip()
       Loop
    Endif

    AjustaSTP(ST9->T9_CODBEM)
    
   RecLock("ST9",.F.)
   Replace ST9->T9_VARDIA With 1
   MsUnlock()
    if lAchou 
//	    MsgBox("O contador acumulado do bem "+alltrim(ST9->T9_CODBEM)+" serแ alterado para "+alltrim(str(nAcumulado)))
        RecLock("ST9",.F.)
        Replace ST9->T9_CONTACU With nAcumulado
        MsUnlock()
        aAreaX:=GetArea()
        DbSelectArea("STC")
        DbSetOrder(1)
        DbSeek(xFilial("STC")+ST9->T9_CODBEM,.T.)
        While !EOF() .AND. xFilial("STC")+STC->TC_CODBEM==xFilial("ST9")+ST9->T9_CODBEM
          AjustaSTP(STC->TC_COMPONE)
          aAreaY:=GetArea()
          DbSelectArea("ST9")
          DbSetOrder(1)
          if DbSeek(xFilial("ST9")+STC->TC_COMPONE)
             RecLock("ST9",.F.)
             Replace ST9->T9_VARDIA With 1
             MsUnlock()
             if lAchou 
//      	       MsgBox("O contador acumulado do sub-conjunto "+alltrim(ST9->T9_CODBEM)+" serแ alterado para "+alltrim(str(nAcumulado)))
               RecLock("ST9",.F.)
               Replace ST9->T9_CONTACU With nAcumulado
               MsUnlock()
             endif
          endif
          RestArea(aAreaX)
          RestArea(aAreaY)
          DbSkip()
        Enddo
        RestArea(aAreaX)
	endif

	DBSKIP()
ENDDO

MsgBox("Processamento terminado. Verifique se os contadores ajustados estใo coerentes"+chr(13)+   ;
       "atrav้s do relat๓rio de situa็ใo das manuten็๕es e da rotina de altera็ใo de contador!","CHEKCONT")

Return

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
ฑฑษออออออออออัออออออออออหอออออออัออออออออออออออออออออหออออออัอออออออออออออปฑฑ
ฑฑบFuno    ณVALIDPERG บ Autor ณ AP5 IDE            บ Data ณ  17/12/02   บฑฑ
ฑฑฬออออออออออุออออออออออสอออออออฯออออออออออออออออออออสออออออฯอออออออออออออนฑฑ
ฑฑบDescrio ณ Verifica a existencia das perguntas criando-as caso seja   บฑฑ
ฑฑบ          ณ necessario (caso nao existam).                             บฑฑ
ฑฑฬออออออออออุออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออนฑฑ
ฑฑบUso       ณ Programa principal                                         บฑฑ
ฑฑศออออออออออฯออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผฑฑ
ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ
฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿฿
/*/

Static Function ValidPerg

Local _sAlias := Alias()
Local aRegs := {}
Local i,j

dbSelectArea("SX1")
dbSetOrder(1)
cPerg := PADR(cPerg,10)

// Grupo/Ordem/Pergunta/Variavel/Tipo/Tamanho/Decimal/Presel/GSC/Valid/Var01/Def01/Cnt01/Var02/Def02/Cnt02/Var03/Def03/Cnt03/Var04/Def04/Cnt04/Var05/Def05/Cnt05
aAdd(aRegs,{cPerg,"01","A Partir Da Data     ?","","","mv_ch1","D",8,0,0,"G","","mv_par01","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"02","Do Bem               ?","","","mv_ch2","C",16,0,0,"G","","mv_par02","","","","","","","","","","","","","","","","","","","","","","","","","ST9","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"03","Ate o Bem            ?","","","mv_ch3","C",16,0,0,"G","","mv_par03","","","","","","","","","","","","","","","","","","","","","","","","","ST9","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"04","Do Centro de Custo   ?","","","mv_ch4","C",9,0,0,"G","","mv_par04","","","","","","","","","","","","","","","","","","","","","","","","","CTT","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
aAdd(aRegs,{cPerg,"05","Ate o Centro de Custo?","","","mv_ch5","C",9,0,0,"G","","mv_par05","","","","","","","","","","","","","","","","","","","","","","","","","CTT","","","","","","","","","","","","","","","","","","","","","","","","","","",""})

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


Static Function AjustaSTP(CODBEM)
    aArea:=GetArea()
    DbSelectArea("STP")
    DbSetOrder(5)
    DbSeek(xFilial("STP")+CODBEM+DTOS(mv_par01),.T.)
    nContador := STP->TP_POSCONT
    nAcumulado := STP->TP_ACUMCON
    lAchou:=.F.
    lConv:=.F.
    While !EOF() .AND. STP->TP_CODBEM==CODBEM
       RecLock("STP",.F.)
       Replace STP->TP_VARDIA With 1
       MsUnlock()
       nAcum:=nAcumulado + STP->TP_POSCONT - nContador
//       if STP->TP_POSCONT<nContador .AND. STP->TP_ACUMCON>=nAcumulado .AND. STP->TP_TIPOLAN<>"Q"
       if STP->TP_POSCONT<nContador .AND. STP->TP_TIPOLAN<>"Q"
//          if MsgBox("O contador do bem "+alltrim(CODBEM)+" sofreu redu็ใo no dia "+dtoc(STP->TP_DTLEITU)+  ;
//                 "-"+STP->TP_HORA+" e precisa ser ajustado. Deseja ajustแ-lo pelo registro anterior?","Confirma็ใo","YESNO")
//             MsgBox("Os contadores do bem "+alltrim(CODBEM)+" serao alterados no dia "+dtoc(STP->TP_DTLEITU)+  ;
//                 "-"+STP->TP_HORA+" para "+alltrim(str(nContador))+" e "+alltrim(str(nAcumulado)))
             RecLock("STP",.F.)
             Replace STP->TP_POSCONT With nContador
             Replace STP->TP_ACUMCON With nAcumulado
             MsUnlock()
             lAchou:=.T.
//          else
//             lAchou:=.F.
//             Exit                                         
//          endif
       elseif STP->TP_ACUMCON<>nAcum .AND. STP->TP_TIPOLAN<>"Q" .AND. STP->TP_POSCONT>=nContador
//          MsgBox("O contador acumulado do bem "+alltrim(CODBEM)+" serแ alterado no dia "+dtoc(STP->TP_DTLEITU)+  ;
//                 "-"+STP->TP_HORA+" para "+alltrim(str(nAcum)))
          RecLock("STP",.F.)
          Replace STP->TP_ACUMCON With nAcum
          MsUnlock()
          nAcumulado := nAcum
          lAchou:=.T.
       elseif STP->TP_ACUMCON<>nAcumulado .AND. STP->TP_TIPOLAN=="Q"
//          MsgBox("O contador acumulado do bem "+alltrim(CODBEM)+" serแ alterado no dia "+dtoc(STP->TP_DTLEITU)+  ;
//                 "-"+STP->TP_HORA+" para "+alltrim(str(nAcumulado)))
          RecLock("STP",.F.)
          Replace STP->TP_ACUMCON With nAcumulado
          MsUnlock()
          lAchou:=.T.
       else
          nAcumulado := STP->TP_ACUMCON
       endif
       nContador := STP->TP_POSCONT
       DbSkip()
    Enddo
    RestArea(aArea)
Return