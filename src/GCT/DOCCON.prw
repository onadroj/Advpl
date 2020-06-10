#INCLUDE "protheus.ch"

//Busca no banco de conhecimento se há documentos vinculados ao contrato e retorna a quantidade

User Function DOCCON()
Local _cDocs := " "
Local _aArea := GetArea()
Local _nDocs := 0        

DbSelectArea("AC9")
DbSetOrder(2) //AC9_FILIAL+AC9_ENTIDA+AC9_FILENT+AC9_CODENT+AC9_CODOBJ
DbSeek(xFilial("AC9")+"CN9"+CN9->CN9_FILIAL+CN9->CN9_NUMERO,.T.)
While !Eof() .AND. xFilial("AC9")+"CN9"+AC9->AC9_FILENT+Alltrim(AC9->AC9_CODENT)==xFilial("AC9")+"CN9"+CN9->CN9_FILIAL+Alltrim(CN9->CN9_NUMERO)
	_nDocs ++
	DbSkip()
EndDo

If _nDocs > 0
	_cDocs := Str(_nDocs)
Endif

RestArea(_aArea)
Return(_cDocs)