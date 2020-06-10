/*
+----------+----------+-------+---------------------+-----+-----------------+
|Programa  |M020INC   |Autor  |              |Data |                        |
+----------+----------+-------+---------------------+-----+-----------------+
|Desc.     |Ponto de Entrada na inclusão do fornecedor para gerar automati- |
|          |camente o item contabil a partir do código e loja do fornecedor.|
+----------+----------------------------------------------------------------+
|            ALTERACOES REALIZADAS DESDE A CRIACAO                          |
+----------+-------------+--------------------------------------------------+
| DATA     | PROGRAMADOR | DESCRICAO                                        |
+----------+-------------+--------------------------------------------------+
|          |             |                                                  |
+----------+-------------+--------------------------------------------------+
*/

#include "rwmake.ch"

User Function M020INC()


dbSelectArea("CTD")
dbSetOrder(1)
dbseek(xFilial("CTD")+"F"+SA2->A2_COD+SA2->A2_LOJA)
 
If Eof()
   cItemcont:="F"+SA2->A2_COD+SA2->A2_LOJA
   dbSelectArea("CTD")
   Reclock("CTD",.T.)
   Replace CTD_FILIAL With xFilial("CTD") , ;
           CTD_ITEM   With cItemcont      , ; 
           CTD_DESC01 With SA2->A2_NOME   , ;
           CTD_CLASSE With "2"            , ; 
           CTD_DTEXIS With CTOD("01/01/1980") , ;
           CTD_BLOQ   With '2'
   MsUnlock("CTD")
EndIf
 
Return