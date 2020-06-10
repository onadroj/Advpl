#INCLUDE "rwmake.ch"

/*/
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
�������������������������������������������������������������������������ͻ��
���Programa  �NOVO51    � Autor � AP6 IDE            � Data �  03/07/14   ���
�������������������������������������������������������������������������͹��
���Descricao � Codigo gerado pelo AP6 IDE.                                ���
���          �                                                            ���
�������������������������������������������������������������������������͹��
���Uso       � AP6 IDE                                                    ���
�������������������������������������������������������������������������ͼ��
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
/*/

User Function RELBALCC


//���������������������������������������������������������������������Ŀ
//� Declaracao de Variaveis                                             �
//�����������������������������������������������������������������������

Local cDesc1         := "Este programa tem como objetivo imprimir relatorio "
Local cDesc2         := "de acordo com os parametros informados pelo usuario."
Local cDesc3         := "Balancete Por Centro de Custos"
Local cPict          := ""
Local titulo       := "Balancete Por Centro de Custos"
Local nLin         := 80

Local Cabec1       := "Balancete por Centro de Custo"
Local Cabec2       := "Codigo        D E S C R I C A O                            SALDO ANTERIOR      DEBITO              CREDITO             MOV. NO PERIODO      SALDO ATUAL"
Local imprime      := .T.
Local aOrd := {}
Local nMovMes, nMovDev, nMovCred, nSaldoAnt, nSaldoAtu, nTotCred, nTotDeb

Private lEnd         := .F.
Private lAbortPrint  := .F.
Private CbTxt        := ""
Private limite           := 132
Private tamanho          := "G"
Private nomeprog         := "RELBALCC" // Coloque aqui o nome do programa para impressao no cabecalho
Private nTipo            := 18
Private aReturn          := { "Zebrado", 1, "Administracao", 2, 2, 1, "", 1}
Private nLastKey        := 0
Private cPerg       := "U_RELBAL"
Private cbtxt      := Space(10)
Private cbcont     := 00
Private CONTFL     := 01
Private m_pag      := 01
Private wnrel      := "RELBALCC" // Coloque aqui o nome do arquivo usado para impressao em disco

Private cString := "CT1"

dbSelectArea("CT1")
dbSetOrder(1)


pergunte(cPerg,.F.)

//���������������������������������������������������������������������Ŀ
//� Monta a interface padrao com o usuario...                           �
//�����������������������������������������������������������������������

wnrel := SetPrint(cString,NomeProg,cPerg,@titulo,cDesc1,cDesc2,cDesc3,.T.,aOrd,.T.,Tamanho,,.T.)

If nLastKey == 27
	Return
Endif

SetDefault(aReturn,cString)

If nLastKey == 27
   Return
Endif

nTipo := If(aReturn[4]==1,15,18)

//���������������������������������������������������������������������Ŀ
//� Processamento. RPTSTATUS monta janela com a regua de processamento. �
//�����������������������������������������������������������������������

RptStatus({|| RunReport(Cabec1,Cabec2,Titulo,nLin) },Titulo)
Return

/*/
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
�������������������������������������������������������������������������ͻ��
���Fun��o    �RUNREPORT � Autor � AP6 IDE            � Data �  03/07/14   ���
�������������������������������������������������������������������������͹��
���Descri��o � Funcao auxiliar chamada pela RPTSTATUS. A funcao RPTSTATUS ���
���          � monta a janela com a regua de processamento.               ���
�������������������������������������������������������������������������͹��
���Uso       � Programa principal                                         ���
�������������������������������������������������������������������������ͼ��
�����������������������������������������������������������������������������
�����������������������������������������������������������������������������
/*/

Static Function RunReport(Cabec1,Cabec2,Titulo,nLin)

Local nOrdem

dbSelectArea(cString)
dbSetOrder(1)

//���������������������������������������������������������������������Ŀ
//� SETREGUA -> Indica quantos registros serao processados para a regua �
//�����������������������������������������������������������������������

SetRegua(RecCount())

//���������������������������������������������������������������������Ŀ
//� Posicionamento do primeiro registro e loop principal. Pode-se criar �
//� a logica da seguinte maneira: Posiciona-se na filial corrente e pro �
//� cessa enquanto a filial do registro for a filial corrente. Por exem �
//� plo, substitua o dbGoTop() e o While !EOF() abaixo pela sintaxe:    �
//�                                                                     �
//� dbSeek(xFilial())                                                   �
//� While !EOF() .And. xFilial() == A1_FILIAL                           �
//�����������������������������������������������������������������������

//���������������������������������������������������������������������Ŀ
//� O tratamento dos parametros deve ser feito dentro da logica do seu  �
//� relatorio. Geralmente a chave principal e a filial (isto vale prin- �
//� cipalmente se o arquivo for um arquivo padrao). Posiciona-se o pri- �
//� meiro registro pela filial + pela chave secundaria (codigo por exem �
//� plo), e processa enquanto estes valores estiverem dentro dos parame �
//� tros definidos. Suponha por exemplo o uso de dois parametros:       �
//� mv_par01 -> Indica o codigo inicial a processar                     �
//� mv_par02 -> Indica o codigo final a processar                       �
//�                                                                     �
//� dbSeek(xFilial()+mv_par01,.T.) // Posiciona no 1o.reg. satisfatorio �
//� While !EOF() .And. xFilial() == A1_FILIAL .And. A1_COD <= mv_par02  �
//�                                                                     �
//� Assim o processamento ocorrera enquanto o codigo do registro posicio�
//� nado for menor ou igual ao parametro mv_par02, que indica o codigo  �
//� limite para o processamento. Caso existam outros parametros a serem �
//� checados, isto deve ser feito dentro da estrutura de la�o (WHILE):  �
//�                                                                     �
//� mv_par01 -> Indica o codigo inicial a processar                     �
//� mv_par02 -> Indica o codigo final a processar                       �
//� mv_par03 -> Considera qual estado?                                  �
//�                                                                     �
//� dbSeek(xFilial()+mv_par01,.T.) // Posiciona no 1o.reg. satisfatorio �
//� While !EOF() .And. xFilial() == A1_FILIAL .And. A1_COD <= mv_par02  �
//�                                                                     �
//�     If A1_EST <> mv_par03                                           �
//�         dbSkip()                                                    �
//�         Loop                                                        �
//�     Endif                                                           �
//�����������������������������������������������������������������������

dbGoTop()
While !EOF()

   //���������������������������������������������������������������������Ŀ
   //� Verifica o cancelamento pelo usuario...                             �
   //�����������������������������������������������������������������������

   If lAbortPrint
      @nLin,00 PSAY "*** CANCELADO PELO OPERADOR ***"
      Exit
   Endif

   //���������������������������������������������������������������������Ŀ
   //� Impressao do cabecalho do relatorio. . .                            �
   //�����������������������������������������������������������������������

   If nLin > 55 // Salto de P�gina. Neste caso o formulario tem 55 linhas...
      Cabec(Titulo,Cabec1,Cabec2,NomeProg,Tamanho,nTipo)
      nLin := 8
   Endif

   // Coloque aqui a logica da impressao do seu programa...
   // Utilize PSAY para saida na impressora. Por exemplo:   
   // Efeito: Retorna o saldo atual [1] do centro de custo "2101" na database do sistema para o moeda "01", // saldo "1"
   nSaldoAtu := SaldoCCus(CT1->CT1_CONTA,mv_par05,mv_par02,"01","1",1) + SaldoCCus(CT1->CT1_CONTA,mv_par06,mv_par02,"01","1",1)
   nSaldoAnt := SaldoCCus(CT1->CT1_CONTA,mv_par05,mv_par01-1,"01","1",1) + SaldoCCus(CT1->CT1_CONTA,mv_par06,mv_par01-1,"01","1",1)

   ///:= MovCusto(CT1->CT1_CONTA,mv_par05,mv_par01,mv_par02,"01","1",4)   
   if nSaldoAtu != 0
       nMovMes  := MovCusto(CT1->CT1_CONTA,mv_par05,mv_par01,mv_par02,"01","1",3) +  MovCusto(CT1->CT1_CONTA,mv_par06,mv_par01,mv_par02,"01","1",3)   
       nMovDev  := MovCusto(CT1->CT1_CONTA,mv_par05,mv_par01,mv_par02,"01","1",1) +  MovCusto(CT1->CT1_CONTA,mv_par06,mv_par01,mv_par02,"01","1",1)   
       nMovCred := MovCusto(CT1->CT1_CONTA,mv_par05,mv_par01,mv_par02,"01","1",2) +  MovCusto(CT1->CT1_CONTA,mv_par06,mv_par01,mv_par02,"01","1",2)   

	   @nLin,00 PSAY CT1->CT1_CONTA PICTURE "@R 9.9.99.99.9999"
	   @nLin,15 PSAY CT1->CT1_DESC01
	   @nLin,60 PSAY nSaldoAnt      PICTURE "@) 999,999,999.99"                            
	   @nLin,80 PSAY nMovDev		PICTURE "@E 999,999,999.99"
	   @nLin,100 PSAY nMovCred		PICTURE "@E 999,999,999.99"
	   @nLin,120 PSAY nMovMes		PICTURE "@E 999,999,999.99"
	   @nLin,140 PSAY nSaldoAtu		PICTURE "@E 999,999,999.99"
	   //nTotDeb := nTotDeb + nMovDev
	   //nTotCred:= nTotCred + nMovCred
	   	   
	   nLin := nLin + 1 // Avanca a linha de impressao
   endif

   dbSkip() // Avanca o ponteiro do registro no arquivo
EndDo                           
@nLin,00 PSAY REPLICATE("_",limite)
//nLin := nLin + 1 // Avanca a linha de impressao
//@nLin,00 PSAY "TOTAIS DO PERIODO                                                              "+nTotDeb
//@nLin,00 PSAY REPLICATE(" ",100)+nTotCred

//���������������������������������������������������������������������Ŀ
//� Finaliza a execucao do relatorio...                                 �
//�����������������������������������������������������������������������

SET DEVICE TO SCREEN

//���������������������������������������������������������������������Ŀ
//� Se impressao em disco, chama o gerenciador de impressao...          �
//�����������������������������������������������������������������������

If aReturn[5]==1
   dbCommitAll()
   SET PRINTER TO
   OurSpool(wnrel)
Endif

MS_FLUSH()

Return
