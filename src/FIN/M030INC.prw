/*
+----------+----------+-------+---------------------+-----+-----------------+
|Programa  |M030INC   |Autor  |Ricardo Diniz        |Data |  06/03/06       |
+----------+----------+-------+---------------------+-----+-----------------+
|Desc.     |Ponto de Entrada na inclusão do cliente para gerar automatica-  |
|          |mente o item contabil a partir do código e loja do cliente.     |
+----------+----------------------------------------------------------------+
|            ALTERACOES REALIZADAS DESDE A CRIACAO                          |
+----------+-------------+--------------------------------------------------+
| DATA     | PROGRAMADOR | DESCRICAO                                        |
+----------+-------------+--------------------------------------------------+
|          |             |                                                  |
+----------+-------------+--------------------------------------------------+
*/

#include "rwmake.ch"

User Function M030INC()


dbSelectArea("CTD")
dbSetOrder(1)
dbseek(xFilial("CTD")+"C"+SA1->A1_COD+SA1->A1_LOJA)
 
If Eof()
   cItemcont:="C"+SA1->A1_COD+SA1->A1_LOJA
   dbSelectArea("CTD")
   Reclock("CTD",.T.)
   Replace CTD_FILIAL With xFilial("CTD") , ;
           CTD_ITEM   With cItemcont      , ; 
           CTD_DESC01 With SA1->A1_NOME   , ;
           CTD_CLASSE With "2"            , ; 
           CTD_DTEXIS With CTOD("01/01/1980") , ;
           CTD_BLOQ   With '2'
   MsUnlock("CTD")
EndIf
 
Return