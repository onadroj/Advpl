#include "rwmake.ch"

User Function VALZ3(nTipo)

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡ao    ³ VALZ3  ³ Autor ³ EDSON                   ³ Data ³ 20.07.09 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡Æo ³ Valida inclusão, alteração e exclusão do cadastro de       ³±±
±±³          ³ Liberadores Borderôs de Pagamentos                         ³±±
±±³          ³                                                            ³±±
±±³          ³                                                            ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Uso       ³ FINANCEIRO                                                 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Tabelas   ³ SZ3                                                        ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Modulo    ³ FINANCEIRO                                                 ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/

Local lRetorno := .T.
Local cCod, cCateg, cAtivo, cCodsis 

aAreaX := GetArea()

If nTipo == 1
   MsgStop("Este cadastro não permite exclusão de registros." + chr(13) ;
           + "Utilize o campo Ativo, para desabilitar o usuário!")
   lRetorno := .F.
Else
   cCod := M->Z3_CODIGO
   cCateg := M->Z3_CATEG  
   cAtivo := M->Z3_ATIVO
   cCodsis := M->Z3_CODSIS
   
   DbSelectArea("SZ3")
   DbSetOrder(1)
   DbGoTop()
   While ! EOF()
      If SZ3->Z3_CODIGO <> cCod .AND. SZ3->Z3_CODSIS == cCodsis
         MsgStop("Usuário já cadastrado!") 
         lRetorno := .F.
         Exit
      ElseIf cCateg <> "A" .AND. cAtivo == "S"
         If SZ3->Z3_CODIGO <> cCod .AND. SZ3->Z3_CATEG == cCateg .AND. SZ3->Z3_ATIVO == "S"
            MsgStop("Existe outro usuário ativo na mesma categoria." + chr(13) ;
                    + "Só é permitido um usuário ativo nesta categoria!") 
            lRetorno := .F.
            Exit 
         End If             
      End If
      DbSkip()
   End
End If

RestArea(aAreaX)
Return(lRetorno)
