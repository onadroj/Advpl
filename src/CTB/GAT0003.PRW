#include "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡ao    ³GAT0003   ³ Autor ³EDSON                  ³ Data ³12.04.05  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡Æo ³RESGATA A OS E O TIPO DE ENTRADA DO PEDIDO DE COMPRA        ³±±
±±³          ³PARA TES COM OS OBRIGATORIA                                 ³±±
±±³          ³PARAMIXB="O" retorna a OS e PARAMIXB="T" retorna TPENT      ³±±
±±³          ³                                                            ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Uso       ³GATILHO                                                     ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Tabelas   ³SC7,SD1                                                     ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Modulo    ³COMPRAS                                                     ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/

User Function Gat0003()

LOCAL cParam:=paramIXB
LOCAL cValor:=""
LOCAL nPCCol
LOCAL nITPCCol
LOCAL cOSOPObr
LOCAL aArea:=GETAREA()
lOCAL nTESCol := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "D1_TES"}))

cOSOPObr:=RETFIELD("SF4",1,XFILIAL("SF4")+ACOLS[N][nTESCol],"F4_OSOPOBR")

if cOSOPObr<>"1"
   Return(cValor)
endif

nPCCol := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "D1_PEDIDO"}))
nITPCCol := (AScan(aHeader,{|aItem| AllTrim(aItem[2]) == "D1_ITEMPC"}))
if cParam=="O"
   cValor :=RETFIELD("SC7",1,XFILIAL("SC7")+ACOLS[N,nPCCol]+ACOLS[N,nITPCCol],"C7_OP")
else
   cValor :=RETFIELD("SC7",1,XFILIAL("SC7")+ACOLS[N,nPCCol]+ACOLS[N,nITPCCol],"C7_TPENT")
endif


Return(cValor)
