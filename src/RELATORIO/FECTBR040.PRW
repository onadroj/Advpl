#Include "PROTHEUS.Ch"       

#DEFINE 	COL_SEPARA1			1
#DEFINE 	COL_CONTA 				2
#DEFINE 	COL_SEPARA2			3
#DEFINE 	COL_DESCRICAO			4
#DEFINE 	COL_SEPARA3			5
#DEFINE 	COL_SALDO_ANT    		6
#DEFINE 	COL_SEPARA4			7
#DEFINE 	COL_VLR_DEBITO   		8
#DEFINE 	COL_SEPARA5			9 
#DEFINE 	COL_VLR_CREDITO  		10
#DEFINE 	COL_SEPARA6			11
#DEFINE 	COL_MOVIMENTO 		12
#DEFINE 	COL_SEPARA7			13                                                                                       
#DEFINE 	COL_SALDO_ATU 		14
#DEFINE 	COL_SEPARA8			15   
#DEFINE 	TAM_VALOR				20   

Static cTpValor  := "D"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o	 ³ Ctbr040	³ Autor ³ Pilar S Albaladejo	³ Data ³ 12.09.00 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³ Balancete Analitico Sintetico Modelo 1			 		  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Sintaxe   ³ Ctbr040()                               			 		  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Retorno	 ³ Nenhum       											  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Uso    	 ³ Generico     											  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ Nenhum													  ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/
User Function FECtbr040()
Local lImpBalR4		:= FindFunction( "TRepInUse" ) .And. TRepInUse()
Local cPerg			:= "FECTBR0400"
Local aPerg			:= {}
Local aHelp			:= {}
Private titulo		:= ""
Private nomeprog	:= "FECTBR040" 

Public aSldRecDes := {0,0,0,0,0,0,0,0}

cTpValor := Alltrim(GetMV("MV_TPVALOR"))    

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Variaveis utilizadas para parametros								  ³
//³ MV_PAR01				// Data Inicial                  	  		  ³
//³ MV_PAR02				// Data Final                        		  ³
//³ MV_PAR03				// Conta Inicial                         	  ³
//³ MV_PAR04				// Conta Final  							     ³
//³ MV_PAR05				// C. Custo Direto Inicio				     ³
//³ MV_PAR06				// C. Custo Direto Fim					     ³
//³ MV_PAR07				// C. Custo Indireto Inicio 			     ³
//³ MV_PAR08				// C. Custo Indireto Fim 			         ³
//³ MV_PAR09				// C. Custo Despesa Inicio 			         ³
//³ MV_PAR10				// C. Custo Despesa Fim 			  			  ³
//³ MV_PAR11				// Imprime Contas: Sintet/Analit/Ambas   	  ³
//³ MV_PAR12				// Set Of Books				    		      ³
//³ MV_PAR13				// Saldos Zerados?			     		      ³
//³ MV_PAR14				// Moeda?          			     		      ³
//³ MV_PAR15				// Pagina Inicial  		     		    	  ³
//³ MV_PAR16				// Saldos? Reais / Orcados	/Gerenciais   	  ³
//³ MV_PAR17				// Quebra por Grupo Contabil?		    	  ³
//³ MV_PAR18				// Filtra Segmento?					    	  ³
//³ MV_PAR19				// Conteudo Inicial Segmento?		   		  ³
//³ MV_PAR20				// Conteudo Final Segmento?		    		  ³
//³ MV_PAR21				// Conteudo Contido em?				    	  ³
//³ MV_PAR22				// Imprime Coluna Mov ?				    	  ³
//³ MV_PAR23				// Salta linha sintetica ?			    	  ³
//³ MV_PAR24				// Imprime valor 0.00    ?			    	  ³
//³ MV_PAR25				// Imprimir Codigo? Normal / Reduzido  		  ³
//³ MV_PAR26				// Divide por ?                   			  ³
//³ MV_PAR27				// Imprimir Ate o segmento?			   		  ³
//³ MV_PAR28				// Posicao Ant. L/P? Sim / Nao         		  ³
//³ MV_PAR29				// Data Lucros/Perdas?                 		  ³
//³ MV_PAR30				// Imprime Quadros Contábeis?				  ³		
//³ MV_PAR31				// Rec./Desp. Anterior Zeradas?				  ³		
//³ MV_PAR32				// Grupo Receitas/Despesas?      			  ³		
//³ MV_PAR33				// Data de Zeramento Receita/Despesas?		  ³		
//³ MV_PAR34            // Num.linhas p/ o Balancete Modelo 1		  ³ 
//³ MV_PAR35				// Descricao na moeda?						  ³		
//³ MV_PAR36			   // Seleciona Filiais            			  ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ


aHelp := {}
AADD(aPerg, {cPerg, "Data Inicial ?            "	, "D", 8	, 0, "G", "", "", "", "", "", "", "" })
aadd(aHelp,{})
aadd(aHelp[1],"Informe a Data Inicial a ")
aadd(aHelp[1],"partir da qual se deseja ")
aadd(aHelp[1],"o relatório.")

AADD(aPerg, {cPerg, "Data Final ?              "	, "D", 8	, 0, "G", "", "", "", "", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[2],"Informe Data Final até ")
aadd(aHelp[2],"a qual se deseja o relatório.")

AADD(aPerg, {cPerg, "Conta Inicial ?           "	, "C", 20	, 0, "G", "", "CT1", "", "", "", "", "" })
aadd(aHelp,{})
aadd(aHelp[3],"Informe a Conta inicial ")
aadd(aHelp[3],"a partir da qual")
aadd(aHelp[3],"se deseja imprimir o relatório.")
aadd(aHelp[3],"Caso queira imprimir todas as ")
aadd(aHelp[3],"contas, deixe esse campo em branco.")
aadd(aHelp[3],"Utilize <F3> para escolher. ")
aadd(aHelp[3],"Obs: Se na pergunta 'Cod Conf Livros?'")
aadd(aHelp[3],"for escolhida uma configuração que ")
aadd(aHelp[3],"possua uma Visão Gerencial associada,")
aadd(aHelp[3],"esta pergunta será desprezada. ")


AADD(aPerg, {cPerg, "Conta Final ?             "	, "C", 20	, 0, "G", "", "CT1", "", "", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[4],"Informe a Conta final até a ")
aadd(aHelp[4],"qual se desejá imprimir o ")
aadd(aHelp[4],"relatório. Caso queira ")
aadd(aHelp[4],"imprimir todas as Contas ")
aadd(aHelp[4],"preencha este  campo com ")
aadd(aHelp[4],"'ZZZZZZZZZZZZZZZZZZZZ'. ")
aadd(aHelp[4],"Utilize <F3> para escolher. ")
aadd(aHelp[4],"Obs: Se na pergunta 'Cod Conf 	Livros?'")
aadd(aHelp[4],"for escolhida uma configuração que ")
aadd(aHelp[4],"possua uma Visão Gerencial associada,")
aadd(aHelp[4],"esta pergunta será desprezada. ")

AADD(aPerg, {cPerg, "C. Custo Direto Inicio?   "	, "C", 08	, 0, "G", "", "CTT", "", "", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[5],"Informe o C.Custo Direito ")
aadd(aHelp[5],"Inicial do qual se deseja ")
aadd(aHelp[5],"o relatório.")

AADD(aPerg, {cPerg, "C. Custo Direto Fim?      "	, "C", 08	, 0, "G", "", "CTT", "", "", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[6],"Informe o C.Custo Direito ")
aadd(aHelp[6],"Final do qual se deseja ")
aadd(aHelp[6],"o relatório.")

AADD(aPerg, {cPerg, "C. Custo Indireto Inicio? "	, "C", 08	, 0, "G", "", "CTT", "", "", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[7],"Informe o C.Custo Indireto ")
aadd(aHelp[7],"Inicial do qual se deseja ")
aadd(aHelp[7],"o relatório.")

AADD(aPerg, {cPerg, "C. Custo Indireto Fim?	  ", "C", 08	, 0, "G", "", "CTT", "", "", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[8],"Informe o C.Custo Indireto ")
aadd(aHelp[8],"Final do qual se deseja ")
aadd(aHelp[8],"o relatório.")

AADD(aPerg, {cPerg, "C. Custo Depesa Inicio?   "	, "C", 08	, 0, "G", "", "CTT", "", "", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[9],"Informe o C.Custo Despesa ")
aadd(aHelp[9],"Inicial do qual se deseja ")
aadd(aHelp[9],"o relatório.")


AADD(aPerg, {cPerg, "C. Custo Despesa Fim?	 ", "C", 08	, 0, "G", "", "CTT", "", "", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[10],"Informe o C.Custo Despesa ")
aadd(aHelp[10],"Final do qual se deseja ")
aadd(aHelp[10],"o relatório.")


AADD(aPerg, {cPerg, "Imprime Contas ?          "	, "N", 01	, 0, "C", "", "", "Sinteticas", "Analiticas", "Ambas", "", ""})
aadd(aHelp,{})
aadd(aHelp[11],"Informe se deseja imprimir as Contas: ") 
aadd(aHelp[11],"- Sinteticas ")
aadd(aHelp[11],"- Analiticas ")
aadd(aHelp[11],"- Ambas ")
aadd(aHelp[11],"Obs: Se na pergunta 'Cod Conf Livros?'")
aadd(aHelp[11],"for escolhida uma configuração que ")
aadd(aHelp[11],"possua uma Visão Gerencial associada,")
aadd(aHelp[11],"esta pergunta será desprezada. ")

AADD(aPerg, {cPerg, "Cod. Config. Livros ?     "	, "C", 03	, 0, "G", "", "CTN", "", "", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[12],"Determine qual a Configuração de ")
aadd(aHelp[12],"Livro será utilizada neste relatório.")
aadd(aHelp[12],"Se não desejar nenhuma configuração ")
aadd(aHelp[12],"específica deixe este campo em branco. ")
aadd(aHelp[12],"Utilize <F3> para escolher. Obs: Se ")
aadd(aHelp[12],"o Livro escolhido tiver uma ")
aadd(aHelp[12],"Visão Gerencial associada, ")
aadd(aHelp[12],"a Visão é queserá impressa. ")


AADD(aPerg, {cPerg, "Saldos Zerados ?          "	, "N", 01	, 0, "C", "", "", "Sim", "Nao", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[13],"Informe se deseja que as contas")
aadd(aHelp[13],"com saldos zerados sejam ")
aadd(aHelp[13],"impressas ou não. ")

AADD(aPerg, {cPerg, "Moeda ?                   "	, "C", 02	, 0, "G", "", "CTO", "", "", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[14],"Informe o código da moeda ")
aadd(aHelp[14],"na qual deseja imprimir o ")
aadd(aHelp[14],"relatório. Utilize <F3> para escolher. ")

AADD(aPerg, {cPerg, "Folha  Inicial ?          "	, "N", 06	, 0, "G", "", "", "", "", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[15],"Informe o número com o qual ")
aadd(aHelp[15],"deseja iniciar a numeração ")
aadd(aHelp[15],"de página do relatório. ")

AADD(aPerg, {cPerg, "Tipo de Saldo ?           "	, "C", 01	, 0, "G", "VldTpSald( MV_PAR16,.T. )", "SLD", "", "", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[16],"Informe qual o tipo de saldo ")
aadd(aHelp[16],"que devera ser impresso. Utilize")
aadd(aHelp[16],"<F3> para escolher. ")

AADD(aPerg, {cPerg, "Quebra ?                  "	, "N", 01	, 0, "C", "", "", "por Grupo", "por Codigo", "Nao Quebra", "", ""})
aadd(aHelp,{})
aadd(aHelp[17],"Informe se deseja que o relatório ")
aadd(aHelp[17],"execute a quebra de página por ")
aadd(aHelp[17],"grupo, por código ou não quebre ")
aadd(aHelp[17],"página. Caso escolha por grupo, ")
aadd(aHelp[17],"os grupos deverão ser cadastrados ")
aadd(aHelp[17],"no item 'Configurações Contábeis - ' ")
aadd(aHelp[17],"'Grupos Contábeis 'e posteriormente ")
aadd(aHelp[17],"associados as Contas Contábeis no ")
aadd(aHelp[17],"Plano de Contas. ")


AADD(aPerg, {cPerg, "Filtra Segmento No. ?     "	, "C", 02	, 0, "G", "", "", "", "", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[18],"Informe qual o segmento da ")
aadd(aHelp[18],"máscara contábil que se deseja ")
aadd(aHelp[18],"filtrar. Se não for informado ")
aadd(aHelp[18],"nenhum Cod Conf Livros'")
aadd(aHelp[18],", não será possível filtrar o ")
aadd(aHelp[18],"segmento da máscara. ")
aadd(aHelp[18],"As próximas 3 perguntas são dependentes")
aadd(aHelp[18],"desta. Se na pergunta 'Cod Conf Livros'")
aadd(aHelp[18],"for escolhida uma Configuração que ")
aadd(aHelp[18],"possua uma Visão Gerencial associada,")
aadd(aHelp[18],"esta pergunta será desprezada. ")


AADD(aPerg, {cPerg, "Conteudo Ini Segmen ?     "	, "C", 20	, 0, "G", "", "", "", "", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[19],"Informe o conteúdo inicial do ")
aadd(aHelp[19],"segmento de máscara contábil a ")
aadd(aHelp[19],"ser filtrado na emissão do relatório. ")
aadd(aHelp[19],"Esta pergunta depende da resposta ")
aadd(aHelp[19],"da pergunta 'Filtra Segmento No.?' ")


AADD(aPerg, {cPerg, "Conteudo Fim Segmen ?     "	, "C", 20	, 0, "G", "", "", "", "", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[20],"Informe o conteúdo final do ")
aadd(aHelp[20],"segmento de máscara contábil a ")
aadd(aHelp[20],"ser filtrado na emissão do ")
aadd(aHelp[20],"relatório.")
aadd(aHelp[20],"Esta pergunta depende da resposta ")
aadd(aHelp[20],"da pergunta 'Filtra Segmento No.?' ")


AADD(aPerg, {cPerg, "Conteudo Contido em ?     "	, "C", 30	, 0, "G", "", "", "", "", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[21],"Informe quais os conteúdos a")
aadd(aHelp[21],"serem filtrados. Esta opção é")
aadd(aHelp[21],"válida para casos onde os ")
aadd(aHelp[21],"conteúdos a serem ")
aadd(aHelp[21],"filtrados não são sequenciais.")
aadd(aHelp[21],"Separe-ospor '/'. Ex: 001/004/010 ")
aadd(aHelp[21],"Se for preenchida esta pergunta as ")
aadd(aHelp[21],"perguntas 'Conteúdo Ini Segmen'")
aadd(aHelp[21]," e 'Conteúdo Fim Segmen' serão ")
aadd(aHelp[21],"desprezadas. Esta pergunta ")
aadd(aHelp[21],"depende da resposta de ")
aadd(aHelp[21],"'Filtra Segmento No.?' ")

AADD(aPerg, {cPerg, "Imprime Coluna Mov. ?     "	, "N", 01	, 0, "C", "", "", "Sim", "Nao", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[22],"Informe se deseja que a coluna")
aadd(aHelp[22],"de Movimento do período seja ")
aadd(aHelp[22],"impressa. A impressão desta ")
aadd(aHelp[22],"coluna altera a orientação do ")
aadd(aHelp[22],"relatório de Retrato para Paisagem.")

AADD(aPerg, {cPerg, "Salta Linha Sintet. ?     "	, "N", 01	, 0, "C", "", "", "Sim", "Nao", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[23],"Informe se deseja que uma ")
aadd(aHelp[23],"linha seja pulada entre as ")
aadd(aHelp[23],"contas sintéticas. Ex: ")
aadd(aHelp[23],"1 ")
aadd(aHelp[23],"1.1 ")
aadd(aHelp[23],"1.1.01")
aadd(aHelp[23],"1.1.02")
aadd(aHelp[23],"1.1.03 ")

AADD(aPerg, {cPerg, "Imprime Valor 0.00 ?      "	, "N", 01	, 0, "C", "", "", "Sim", "Nao", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[24],"Informe se deseja que o ")
aadd(aHelp[24],"número '0,00' sejá impresso ")
aadd(aHelp[24],"ou não no relatório. Em caso ")
aadd(aHelp[24],"negativo,será deixado em branco o ")
aadd(aHelp[24],"espaço onde seria impresso ")
aadd(aHelp[24],"o número '0,00'. ")

AADD(aPerg, {cPerg, "Imprime Codigo ?          "	, "N", 01	, 0, "C", "", "", "Normal", "Reduzido", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[25],"Informe se deseja que o ")
aadd(aHelp[25],"código da entidade seja impresso: ")
aadd(aHelp[25],"Normal ")
aadd(aHelp[25],"Reduzido ")

AADD(aPerg, {cPerg, "Divide por ?              "	, "N", 01	, 0, "C", "", "", "Nao se aplica", "Cem", "Mil", "Milhao", ""})
aadd(aHelp,{})
aadd(aHelp[26],"Informe se deseja que os ")
aadd(aHelp[26],"valores dos relatórios sejam ")
aadd(aHelp[26],"divididos por: ")
aadd(aHelp[26],"Cem - Mil - Milhão ")
aadd(aHelp[26],"Não se aplica (não divide). ")

AADD(aPerg, {cPerg, "Imprimir ate o seg. ?     "	, "C", 01	, 0, "G", "", "", "", "", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[27],"Indique até qual segmento ")
aadd(aHelp[27],"da máscara da entidade deseja ")
aadd(aHelp[27],"que o relatório seja ")
aadd(aHelp[27],"impresso. Esta pergunta só ")
aadd(aHelp[27],"será considerada se a pergunta ")
aadd(aHelp[27],"'Cod Conf Livros?' for preenchida. ")

AADD(aPerg, {cPerg, "Posicao Ant. L/P ?        "	, "N", 01	, 0, "C", "", "", "Sim", "Nao", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[28],"Indique se deseja que os ")
aadd(aHelp[28],"valores a seremimpressos irão ")
aadd(aHelp[28],"considerar ou não os lançamentos ")
aadd(aHelp[28],"da Apuração de Resultados. ")

AADD(aPerg, {cPerg, "Data Lucros/Perdas ?      "	, "D", 08	, 0, "G", "", "CTZ", "", "", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[29],"Informe a data de Apuração ")
aadd(aHelp[29],"de Resultado que deseja ")
aadd(aHelp[29],"desconsiderar. ")
aadd(aHelp[29],"Esta pergunta depende da ")
aadd(aHelp[29],"resposta à pergunta ")
aadd(aHelp[29],"'Posição Ant L/P?'. ")

AADD(aPerg, {cPerg, "Imp Quadros Contabeis ?   "	, "N", 01	, 0, "C", "CTBOpQuad(MV_PAR30==1)", "", "Sim", "Nao", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[30],"Informe se deseja que sejam")
aadd(aHelp[30],"impressos osQuadros Contábeis")
aadd(aHelp[30],"ao final do relatório.Ao ")
aadd(aHelp[30],"responder com 'Sim' esta ")
aadd(aHelp[30],"pergunta, será aberta uma ")
aadd(aHelp[30],"outra tela onde deverão ser ")
aadd(aHelp[30],"informadas as Contas que serão")
aadd(aHelp[30],"impressas no Quadro Contábil. ")

AADD(aPerg, {cPerg, "Ignora Sl Ant.Rec/Des ?   "	, "N", 01	, 0, "C", "", "", "Sim", "Nao", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[31],"Informe se deseja ignorar o")
aadd(aHelp[31],"saldo anterior das contas ")
aadd(aHelp[31],"de receitas/despesasde acordo ")
aadd(aHelp[31],"com o grupo e a data escolhida")
aadd(aHelp[31],"duas perguntas abaixo. ")

AADD(aPerg, {cPerg, "Grupo Receitas/Desp. ?    "	, "C", 05	, 0, "G", "", "", "", "", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[32],"Informe quais os grupos de ")
aadd(aHelp[32],"receitas/despesas que deverao")
aadd(aHelp[32],"ter seus saldos anteriores ")
aadd(aHelp[32],"ignorados.")
aadd(aHelp[32],"Essa pergunta sera considerada ")
aadd(aHelp[32],"somente se a ")
aadd(aHelp[32],"pergunta 'Ignora Sl Ant.Rec/Des'")
aadd(aHelp[32],"estiver preenchida com 'Sim'. ")

AADD(aPerg, {cPerg, "Data Sld Ant. Receitas/Desp.?"	, "D", 08	, 0, "G", "", "", "", "", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[33],"Informe qual a data que as")
aadd(aHelp[33],"contas de receitas/despesas ")
aadd(aHelp[33],"terao seus saldos anteriores")
aadd(aHelp[33],"ignorados. Essa pergunta ")
aadd(aHelp[33],"sera considerada somente se")
aadd(aHelp[33],"a pergunta 'Ignora Sl Ant.Rec")
aadd(aHelp[33],"/Des' estiver preenchida com 'Sim'. ")

AADD(aPerg, {cPerg, "Num.linhas p/ o Balancete ?  "	, "N", 02	, 0, "G", "", "", "", "", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[34],"Informe a quantidade de linhas")
aadd(aHelp[34],"que serao impressas em cada pag.")
aadd(aHelp[34],"do Balancete Aux. de Verificacao. ")

AADD(aPerg, {cPerg, "Descricao na Moeda ?      "	, "C", 02	, 0, "G", "", "CTO", "", "", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[35],"Informe em qual moeda deseja ")
aadd(aHelp[35],"imprimir a descrição das contas.")

AADD(aPerg, {cPerg, "Seleciona Filiais ?       "	, "N", 01	, 0, "C", "", "", "Sim", "Nao", "", "", ""})
aadd(aHelp,{})
aadd(aHelp[36],"Escolha Sim se deseja ")
aadd(aHelp[36],"selecionar as filiais ")


TestaSX1(cPerg,aPerg,aHelp)

If lImpBalR4
	CTBR040R4()
Else
	CTBR040R3()
EndIf

aSldRecDes := Nil

Return

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o	 ³ CTBR040R4 ³ Autor³ Daniel Sakavicius		³ Data ³ 01/08/06 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³ Balancete Analitico Sintetico Modelo 1 - R4                ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Sintaxe	 ³ CTBR040R4												  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Uso		 ³ SIGACTB                                    				  ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/
Static Function CTBR040R4()
Local nQuadro
Local cPerg	:= "FECTBR0400"
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Interface de impressao                                                  ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

Private aQuadro := { "","","","","","","",""}              
Private aSelFil		:= {} 
Private n_pagini 	 

For nQuadro :=1 To Len(aQuadro)
	aQuadro[nQuadro] := Space(Len(CriaVar("CT1_CONTA")))
Next	

CtbCarTxt()

Pergunte(cPerg,.F.)

oReport := ReportDef()

If Valtype( oReport ) == 'O'
	If ! Empty( oReport:uParam )
		Pergunte( oReport:uParam, .F. )
	EndIf	
	
	oReport:PrintDialog()      
Endif
	
oReport := Nil

Return                                

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Programa  ³ReportDef ³ Autor ³ Daniel Sakavicius		³ Data ³ 28/07/06 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³Esta funcao tem como objetivo definir as secoes, celulas,   ³±±
±±³          ³totalizadores do relatorio que poderao ser configurados     ³±±
±±³          ³pelo relatorio.                                             ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Uso		 ³ SIGACTB                                    				  ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/
Static Function ReportDef()
local aArea	   		:= GetArea()   
Local CREPORT			:= "CTBR040"
Local CTITULO			:= "Emissao do Relat. Conf. Dig. "
Local CDESC			:= "Este programa ira imprimir o Relatorio para Conferencia"
Local cPerg	   		:= "FECTBR0400" 
Local CCOLBAR			:= "|"                   
Local aTamConta		:= TAMSX3("CT1_CONTA")    
Local aTamVal			:= TAMSX3("CT2_VALOR")
Local aTamDesc		:= {40}  
Local cPictVal 		:= PesqPict("CT2","CT2_VALOR")
Local nDecimais
Local cMascara		:= ""
Local cSeparador		:= ""
Local nTamConta		:= 20
Local aSetOfBook
Local nMaskFator 		:= 1 


// Efetua a pergunta antes de montar a configuração do relatorio, afim de poder definir o layout a ser impresso
Pergunte( "FECTBR0400" , .T. )

If MV_PAR36 == 1 .And. Len( aSelFil ) <= 0  .And. !IsBlind()
	aSelFil := AdmGetFil()
	If Len( aSelFil ) <= 0
		Return
	EndIf 
EndIf
	

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Verifica se usa Set Of Books + Plano Gerencial (Se usar Plano³
//³ Gerencial -> montagem especifica para impressao)	    	  ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If ! ct040Valid( MV_PAR12 )
	Return .F.
Else
   aSetOfBook := CTBSetOf( MV_PAR12 )
Endif

cMascara := RetMasCtb( aSetOfBook[2], @cSeparador )

If ! Empty( cMascara )
	nTamConta := aTamConta[1] + ( Len( Alltrim( cMascara ) ) / 2 )
Else
	nTamConta := aTamConta[1]
EndIf

cPicture := aSetOfBook[4]

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Criacao do componente de impressao                                      ³
//³                                                                        ³
//³TReport():New                                                           ³
//³ExpC1 : Nome do relatorio                                               ³
//³ExpC2 : Titulo                                                          ³
//³ExpC3 : Pergunte                                                        ³
//³ExpB4 : Bloco de codigo que sera executado na confirmacao da impressao  ³
//³ExpC5 : Descricao                                                       ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

//"Este programa tem o objetivo de emitir o Cadastro de Itens Classe de Valor "
//"Sera impresso de acordo com os parametros solicitados pelo"
//"usuario"
oReport	:= TReport():New( cReport,Capital(CTITULO),cPerg, { |oReport| Pergunte(cPerg , .F. ), If(! ReportPrint( oReport ), oReport:CancelPrint(), .T. ) }, CDESC ) 
oReport:ParamReadOnly()

//Habilitado o parametro de personalização porém,
// não será permitido a alteração das sections
IF GETNEWPAR("MV_CTBPOFF",.T.)
	oReport:SetEdit(.F.)
ENDIF	

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Criacao da secao utilizada pelo relatorio                               ³
//³                                                                        ³
//³TRSection():New                                                         ³
//³ExpO1 : Objeto TReport que a secao pertence                             ³
//³ExpC2 : Descricao da seçao                                              ³
//³ExpA3 : Array com as tabelas utilizadas pela secao. A primeira tabela   ³
//³        sera considerada como principal para a seção.                   ³
//³ExpA4 : Array com as Ordens do relatório                                ³
//³ExpL5 : Carrega campos do SX3 como celulas                              ³
//³        Default : False                                                 ³
//³ExpL6 : Carrega ordens do Sindex                                        ³
//³        Default : False                                                 ³
//³                                                                        ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
oSection1  := TRSection():New( oReport, "Plano de contas", {"cArqDad","CT1"},, .F., .F.,,,,,,,,,0 ) //

TRCell():New( oSection1, "CONTA"	,, "Conta"/*Titulo*/	,/*Picture*/, nTamConta + 10 /*Tamanho*/, /*lPixel*/, /*CodeBlock*/, /*"LEFT"*/,,/*"LEFT"*/,,,.F.)
TRCell():New( oSection1, "DESCCTA"  ,,"Descricao"/*Titulo*/	,/*Picture*/, aTamDesc[1]/*Tamanho*/, /*lPixel*/,/*CodeBlock*/,  /*"LEFT"*/,.T.,/*"LEFT"*/,,,.F.)

TRCell():New( oSection1, "SALDOANT" ,,"Saldo Anterior"/*Titulo*/	,/*Picture*/, TAM_VALOR+2 /*Tamanho*/, /*lPixel*/, /*CodeBlock*/, /*"RIGHT"*/,,"RIGHT",,,.F.)
TRCell():New( oSection1, "SALDODEB" ,,"Debito"/*Titulo*/	,/*Picture*/, TAM_VALOR+2 /*Tamanho*/, /*lPixel*/, /*CodeBlock*/, /*"RIGHT"*/,,"RIGHT",,,.F.)
TRCell():New( oSection1, "SALDOCRD" ,,"Credito"/*Titulo*/	,/*Picture*/, TAM_VALOR+2 /*Tamanho*/, /*lPixel*/, /*CodeBlock*/, /*"RIGHT"*/,,"RIGHT",,,.F.)
TRCell():New( oSection1, "MOVIMENTO",,"Movimento"/*Titulo*/	,/*Picture*/, TAM_VALOR+2 /*Tamanho*/, /*lPixel*/, /*CodeBlock*/, /*"RIGHT"*/,,"RIGHT",,,.F.)
TRCell():New( oSection1, "SALDOATU" ,,"Saldo Atual"/*Titulo*/	,/*Picture*/, TAM_VALOR+2 /*Tamanho*/, /*lPixel*/, /*CodeBlock*/, /*"RIGHT"*/,,"RIGHT",,,.F.)

TRPosition():New( oSection1, "CT1", 1, {|| xFilial( "CT1" ) + cArqDad->CONTA })

oSection1:Cell("CONTA"):lHeaderSize		:= .F.
oSection1:Cell("DESCCTA"):lHeaderSize	:= .F. 
oSection1:Cell("SALDOANT"):lHeaderSize	:= .F.
oSection1:Cell("SALDODEB"):lHeaderSize	:= .F.
oSection1:Cell("SALDOCRD"):lHeaderSize	:= .F.
oSection1:Cell("MOVIMENTO"):lHeaderSize	:= .F.
oSection1:Cell("SALDOATU"):lHeaderSize	:= .F.  

oSection1:SetTotalInLine(.F.)          
oSection1:SetTotalText('') 
oSection1:SetEdit(.F.)

Return( oReport )

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Programa  ³ReportPrint³ Autor ³ Daniel Sakavicius	³ Data ³ 28/07/06 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³Imprime o relatorio definido pelo usuario de acordo com as  ³±±
±±³          ³secoes/celulas criadas na funcao ReportDef definida acima.  ³±±
±±³          ³Nesta funcao deve ser criada a query das secoes se SQL ou   ³±±
±±³          ³definido o relacionamento e filtros das tabelas em CodeBase.³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Sintaxe   ³ ReportPrint(oReport)                                       ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Retorno   ³EXPO1: Objeto do relatório                                  ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Static Function ReportPrint( oReport )  

Local oSection1 	:= oReport:Section(1) 
Local lExterno		:= .F.   
Local aSetOfBook
Local dDataFim 		:= mv_par02
Local lFirstPage	:= .T.
Local lJaPulou		:= .F.
Local lRet			:= .T.
Local lPrintZero	:= (MV_PAR24==1)
Local lPula			:= (MV_PAR23==1) 
Local lNormal		:= (MV_PAR25==1)
Local lVlrZerado	:= (MV_PAR13==1)
Local lQbGrupo		:= (MV_PAR17==1) 
Local lQbConta		:= (MV_PAR17==2)
Local l132			:= .T.
Local nDecimais
Local nDivide		:= 1
Local nTotDeb		:= 0
Local nTotCrd		:= 0
Local nTotMov		:= 0
Local nGrpDeb		:= 0
Local nGrpCrd		:= 0                     
Local cSegAte   	:= MV_PAR27
Local nDigitAte		:= 0
Local lImpAntLP		:= (MV_PAR28 == 1)
Local dDataLP		:= MV_PAR29
Local lImpSint		:= Iif(MV_PAR11=1 .Or. MV_PAR11 ==3,.T.,.F.)
Local lRecDesp0		:= (MV_PAR31 == 1)
Local lImpMov		:= (MV_PAR22 == 1)
Local cRecDesp		:= MV_PAR32
Local dDtZeraRD		:= MV_PAR33
Local n
Local oMeter
Local oText
Local oDlg
Local oBreak
Local lImpPaisgm	:= .F.	
Local nMaxLin   	:= MV_PAR34
Local cMoedaDsc		:= MV_PAR35
Local aCtbMoeda		:= {}
Local aCtbMoedadsc	:= {}
Local CCOLBAR		:= "|"                   
Local cTipoAnt		:= ""
Local cGrupoAnt		:= ""
Local cArqTmp		:= ""
Local cArqDad		:= ""
Local Tamanho		:= "M"
Local cSeparador	:= ""
Local aTamVal		:= TAMSX3("CT2_VALOR")
Local oTotGerDeb		
Local oTotGerCrd 
Local oTotGerMov
Local oTotGrpMov
Local cPicture
Local cContaSint
Local cBreak			:= "2"
Local cGrupo			:= ""
Local nTotGerDeb		:= 0
Local nTotGerCrd		:= 0
Local nTotGerMov		:= 0
Local nCont			:= 0
Local cFilUser		:= ""
Local nMasc			:= 0
Local cMasc			:= ""  
Local lEnd
Local aStru 			:= {}
Local cArqInd			:= CriaTrab(Nil, .F.)
Local cArqAux			:= "cArqDad"

Private nLinReport    := 9     
n_pagini 	  := MV_PAR15

If oReport:nDevice == 5 .OR. oReport:nDevice == 3 
	oSection1:Cell("SALDOANT"):SetAlign("RIGHT")  
	oSection1:Cell("SALDODEB"):SetAlign("RIGHT")
	oSection1:Cell("SALDOCRD"):SetAlign("RIGHT")
	oSection1:Cell("MOVIMENTO"):SetAlign("RIGHT")  
	oSection1:Cell("SALDOATU"):SetAlign("RIGHT")     
Endif 


//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Verifica se usa Set Of Books + Plano Gerencial (Se usar Plano³
//³ Gerencial -> montagem especifica para impressao)	    	  ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If ! ct040Valid( MV_PAR12 )
	Return .F.
Else
   aSetOfBook := CTBSetOf(MV_PAR12)
Endif

If MV_PAR26 == 2			// Divide por cem
	nDivide := 100
ElseIf MV_PAR26 == 3		// Divide por mil
	nDivide := 1000
ElseIf MV_PAR26 == 4		// Divide por milhao
	nDivide := 1000000
EndIf	     

aSetOfBook[9] := nDivide

If lRet
	aCtbMoeda := CtbMoeda( MV_PAR14 , nDivide )

	If Empty(aCtbMoeda[1])                       
		Help(" ",1,"NOMOEDA")
		lRet := .F.
		Return lRet
	Endif

    // validação da descrição da moeda
	if lRet .And. ! Empty( MV_PAR35 ) .and. MV_PAR35 <> nil
		aCtbMoedadsc := CtbMoeda( MV_PAR35 , nDivide )

		If Empty( aCtbMoedadsc[1] )                       
    		Help( " " , 1 , "NOMOEDA")
	        lRet := .F.
    	    Return lRet
	    Endif
	Endif
Endif

If lRet
	If (MV_PAR31 == 1) .and. ( Empty(MV_PAR32) .or. Empty(MV_PAR33) )
		cMensagem	:= "Favor preencher os parametros Grupos Receitas/Despesas e "	//"Favor preencher os parametros Grupos Receitas/Despesas e "
		cMensagem	+= "Data Sld Ant. Receitas/Desp. "	//"Data Sld Ant. Receitas/Desp. "
		MsgAlert(cMensagem,"Ignora Sl Ant.Rec/Des")	 //"Ignora Sl Ant.Rec/Des"
		lRet    	:= .F.	
	    Return lRet
    EndIf
EndIf

aCtbMoeda  	:= CtbMoeda(MV_PAR14,nDivide)                

cDescMoeda 	:= Alltrim(aCtbMoeda[2])
nDecimais 	:= DecimalCTB(aSetOfBook,MV_PAR14)

If Empty(aSetOfBook[2])
	cMascara := GetMv("MV_MASCARA")
Else
	cMascara 	:= RetMasCtb(aSetOfBook[2],@cSeparador)
EndIf
cPicture 		:= aSetOfBook[4]

lPrintZero	:= Iif(MV_PAR24==1,.T.,.F.)

If oReport:Title() == oReport:cRealTitle
	IF MV_PAR11 == 1
		Titulo:=	OemToAnsi("BALANCETE DE VERIFICACAO POR CENTRO DE CUSTO SINTETICO DE ")	
	ElseIf MV_PAR11 == 2
		Titulo:=	OemToAnsi("BALANCETE DE VERIFICACAO POR CENTRO DE CUSTO ANALITICO DE ")	
	ElseIf MV_PAR11 == 3
		Titulo:=	OemToAnsi("BALANCETE DE VERIFICACAO POR CENTRO DE CUSTO DE ")	
	EndIf
	Titulo += 	DTOC(mv_par01) + OemToAnsi(" ATE ") + Dtoc(mv_par02) + ;
			OemToAnsi(" MOEDA ") + cDescMoeda + CtbTitSaldo(MV_PAR16) + ;
			OemToAnsi(" CC ") 
			
	If !Empty(MV_PAR05) .or. !Empty(MV_PAR06)
		if Alltrim(MV_PAR05) == Alltrim(MV_PAR06)
			Titulo += " "+Alltrim(MV_PAR05)
		Else
			Titulo += " "+Alltrim(MV_PAR05)+"-"+Alltrim(MV_PAR06)
		Endif	
	Endif	
	If !Empty(MV_PAR07) .or. !Empty(MV_PAR08)
		if Alltrim(MV_PAR07) == Alltrim(MV_PAR08)
			Titulo += " "+Alltrim(MV_PAR07)
		Else
			Titulo += " "+Alltrim(MV_PAR07)+"-"+Alltrim(MV_PAR08)
		Endif	
	Endif	
	If !Empty(MV_PAR09) .or. !Empty(MV_PAR10)
		if Alltrim(MV_PAR09) == Alltrim(MV_PAR10)
			Titulo += " "+Alltrim(MV_PAR09)
		Else
			Titulo += " "+Alltrim(MV_PAR09)+"-"+Alltrim(MV_PAR10)
		Endif	
	Endif	
Else
	Titulo := oReport:Title()
Endif

oReport:SetPageNumber( MV_PAR15 )
oReport:SetCustomText( {|| nCtCGCCabTR(dDataFim,titulo,oReport)})  


cFilUser := oSection1:GetAdvplExpr("CT1")    
If Empty(cFilUser)
	cFilUser := ".T."
EndIf	

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Monta Arquivo Temporario para Impressao			  		     ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

MsgMeter({|	oMeter, oText, oDlg, lEnd | ;
				CTGerPlan(oMeter, oText, oDlg, @lEnd,@cArqTmp,;
				mv_par01,mv_par02,"CT3","",mv_par03,mv_par04,MV_PAR05,MV_PAR06,,,,,MV_PAR14,;
				MV_PAR16,aSetOfBook,MV_PAR18,MV_PAR19,MV_PAR20,MV_PAR21,;
				.F.,.T.,MV_PAR17,,lImpAntLP,dDataLP,nDivide,lVlrZerado,,,,,,,,,,,,,,lImpSint,cFilUser,lRecDesp0,;
				cRecDesp,dDtZeraRD,,,,,,,cMoedaDsc,,aSelFil)},;
				OemToAnsi(OemToAnsi("Criando Arquivo Tempor rio...")),;  
				OemToAnsi("Balancete Verificacao"))
				
If Select("cArqTmp") <> 0
	CriaArqDad("cArqTmp",@cArqDad,@cArqAux,@cArqInd)
	CopDados("cArqTmp","cArqDad")
	("cArqTmp")->(dbCloseArea())
Endif	

MsgMeter({|	oMeter, oText, oDlg, lEnd | ;
				(oMeter, oText, oDlg, @lEnd,@cArqTmp,;
				mv_par01,mv_par02,"CT3","",mv_par03,mv_par04,MV_PAR07,MV_PAR08,,,,,MV_PAR14,;
				MV_PAR16,aSetOfBook,MV_PAR18,MV_PAR19,MV_PAR20,MV_PAR21,;
				.F.,.F.,MV_PAR17,,lImpAntLP,dDataLP,nDivide,lVlrZerado,,,,,,,,,,,,,,lImpSint,cFilUser,lRecDesp0,;
				cRecDesp,dDtZeraRD,,,,,,,cMoedaDsc,,aSelFil)},;
				OemToAnsi(OemToAnsi("Criando Arquivo Tempor rio...")),;  
				OemToAnsi("Balancete Verificacao"))

If Select("cArqTmp") <> 0
	CriaArqDad("cArqTmp",@cArqDad,@cArqAux,@cArqInd)
	CopDados("cArqTmp","cArqDad")
	("cArqTmp")->(dbCloseArea())
Endif	

MsgMeter({|	oMeter, oText, oDlg, lEnd | ;
				CTGerPlan(oMeter, oText, oDlg, @lEnd,@cArqTmp,;
				mv_par01,mv_par02,"CT3","",mv_par03,mv_par04,MV_PAR09,MV_PAR10,,,,,MV_PAR14,;
				MV_PAR16,aSetOfBook,MV_PAR18,MV_PAR19,MV_PAR20,MV_PAR21,;
				.F.,.T.,MV_PAR17,,lImpAntLP,dDataLP,nDivide,lVlrZerado,,,,,,,,,,,,,,lImpSint,cFilUser,lRecDesp0,;
				cRecDesp,dDtZeraRD,,,,,,,cMoedaDsc,,aSelFil)},;
				OemToAnsi(OemToAnsi("Criando Arquivo Tempor rio...")),;  
				OemToAnsi("Balancete Verificacao"))				

If Select("cArqTmp") <> 0
	CriaArqDad("cArqTmp",@cArqDad,@cArqAux,@cArqInd)
	CopDados("cArqTmp","cArqDad")
	("cArqTmp")->(dbCloseArea())	
Endif	

If Select("cArqDad") == 0
	oReport:CancelPrint()
	Return
EndIf     

nCount := cArqDad->(RecCount())

oReport:SetMeter(nCont)

lRet := !(nCount == 0 .And. !Empty(aSetOfBook[5]))

If lRet
	      
	// Verifica Se existe filtragem Ate o Segmento
  	If ! Empty( cSegAte )
		
		//Efetua tratamento da mascara para consegui efetuar o controle do segmento 
		For nMasc := 1 to Len( cMascara )
			
			cMasc += SubStr( cMascara,nMasc,1 )
			
		Next nMasc
	
	
		nDigitAte := CtbRelDig( cSegAte, cMasc ) 	

		oSection1:SetFilter( 'Len(Alltrim(cArqDad->CONTA)) <= ' + alltrim( Str( nDigitAte )) )  
	EndIf	 

	cArqDad->(dbGoTop())
	
	If lNormal
		oSection1:Cell("CONTA"):SetBlock( {|| EntidadeCTB(cArqDad->CONTA,000,000,030,.F.,cMascara,cSeparador,,,.F.,,.F.)} )
	Else
		oSection1:Cell("CONTA"):SetBlock( {|| cArqDad->CTARES } )
	EndIf	
	
	oSection1:Cell("DESCCTA"):SetBlock( { || cArqDad->DESCCTA } )
	If cTpValor != "P"	
	  	oSection1:Cell("SALDOANT"):SetBlock( { || FEValCtb(cArqDad->SALDOANT,,,TAM_VALOR-2,nDecimais,.T.,cPicture,cArqDad->NORMAL,,,,,,lPrintZero,.F.) } )
	  	oSection1:Cell("SALDODEB"):SetBlock( { || FEValCtb(cArqDad->SALDODEB,,,TAM_VALOR,nDecimais,.F.,cPicture,"1",,,,,,lPrintZero,.F.) } )
	  	oSection1:Cell("SALDOCRD"):SetBlock( { || FEValCtb(cArqDad->SALDOCRD,,,TAM_VALOR,nDecimais,.F.,cPicture,"2",,,,,,lPrintZero,.F.) } )
	    oSection1:Cell("SALDOATU"):SetBlock( { || FEValCtb(cArqDad->SALDOATU,,,TAM_VALOR-2,nDecimais,.T.,cPicture,cArqDad->NORMAL,,,,,,lPrintZero,.F.) } )	    
	Else	
	  	oSection1:Cell("SALDOANT"):SetBlock( { || PadL(FEValCtb(cArqDad->SALDOANT,,,TAM_VALOR-2,nDecimais,.T.,cPicture,cArqDad->NORMAL,,,,,,lPrintZero,.F.),TAM_VALOR) } )
	  	oSection1:Cell("SALDODEB"):SetBlock( { || PadL(FEValCtb(cArqDad->SALDODEB,,,TAM_VALOR,nDecimais,.F.,cPicture,"1",,,,,,lPrintZero,.F.),TAM_VALOR) } )
	  	oSection1:Cell("SALDOCRD"):SetBlock( { || PadL(FEValCtb(cArqDad->SALDOCRD,,,TAM_VALOR,nDecimais,.F.,cPicture,"2",,,,,,lPrintZero,.F.),TAM_VALOR) } )
	    oSection1:Cell("SALDOATU"):SetBlock( { || PadL(FEValCtb(cArqDad->SALDOATU,,,TAM_VALOR-2,nDecimais,.T.,cPicture,cArqDad->NORMAL,,,,,,lPrintZero,.F.),TAM_VALOR) } )
	EndIf
	
	//	 Imprime Movimento
  	If !lImpMov   
  		oSection1:Cell("MOVIMENTO"):SetSize(0)
 		oSection1:Cell("MOVIMENTO"):Disable()
 	ElseIf cTpValor != "P"
 		oSection1:Cell("MOVIMENTO"):SetBlock( { || FEValCtb(cArqDad->MOVIMENTO,,,TAM_VALOR-2,nDecimais,.T.,cPicture,cArqDad->NORMAL,,,,,, lPrintZero,.F.) } )
 	Else
 		oSection1:Cell("MOVIMENTO"):SetBlock( { || PadL(FEValCtb(cArqDad->MOVIMENTO,,,TAM_VALOR-2,nDecimais,.T.,cPicture,cArqDad->NORMAL,,,,,, lPrintZero,.F.),TAM_VALOR) } ) 	
 	EndIf

	If lQbGrupo

		//*********************************
		// Total por Grupo do relatorio   *
		//*********************************
	
		oBrkGrupo := TRBreak():New(oSection1, { || cArqDad->GRUPO },{|| " T O T A I S "+" ( "+ RTrim( Upper(AllTrim(cGrupo) )) + " )" },,,.T.)	//	
	  	oBrkGrupo:OnBreak( { |x| cGrupo := x, If(cArqDad->(Eof()),oBrkGrupo:lPageBreak := .F.,.T.)} )
	
		oTotGrpDeb := TRFunction():New(oSection1:Cell("SALDODEB"),,"SUM",oBrkGrupo/*oBreak*/,/*Titulo*/,/*cPicture*/,;
			{ || Iif(cArqDad->TIPOCONTA="1",0,cArqDad->SALDODEB) },.F.,.F.,.F.,oSection1)
		oTotGrpDeb:Disable()		
				
		oTotGrpCrd := TRFunction():New(oSection1:Cell("SALDOCRD"),,"SUM",oBrkGrupo/*oBreak*/,/*Titulo*/,/*cPicture*/,;
			{ || Iif(cArqDad->TIPOCONTA="1",0,cArqDad->SALDOCRD) },.F.,.F.,.F.,oSection1)
		oTotGrpCrd:Disable() 
		
		If cTpValor != "P"	
			TRFunction():New(oSection1:Cell("SALDODEB"),,"ONPRINT",oBrkGrupo/*oBreak*/,/*Titulo*/,/*cPicture*/,;
				{ || FEValCtb(oTotGrpDeb:GetValue(),,,TAM_VALOR,nDecimais,.F.,cPicture,"1",,,,,,lPrintZero,.F.) },.F.,.F.,.F.,oSection1 )
	
			TRFunction():New(oSection1:Cell("SALDOCRD"),,"ONPRINT",oBrkGrupo/*oBreak*/,/*Titulo*/,/*cPicture*/,;
				{ || FEValCtb(oTotGrpCrd:GetValue(),,,TAM_VALOR,nDecimais,.F.,cPicture,"2",,,,,,lPrintZero,.F.) },.F.,.F.,.F.,oSection1 )
		Else
			TRFunction():New(oSection1:Cell("SALDODEB"),,"ONPRINT",oBrkGrupo/*oBreak*/,/*Titulo*/,/*cPicture*/,;
				{ || PadL(FEValCtb(oTotGrpDeb:GetValue(),,,TAM_VALOR,nDecimais,.F.,cPicture,"1",,,,,,lPrintZero,.F.),TAM_VALOR) },.F.,.F.,.F.,oSection1 )
	
			TRFunction():New(oSection1:Cell("SALDOCRD"),,"ONPRINT",oBrkGrupo/*oBreak*/,/*Titulo*/,/*cPicture*/,;
				{ || PadL(FEValCtb(oTotGrpCrd:GetValue(),,,TAM_VALOR,nDecimais,.F.,cPicture,"2",,,,,,lPrintZero,.F.),TAM_VALOR) },.F.,.F.,.F.,oSection1 )		
		EndIf
                                                             
	 	If !lImpMov   	 	
	 	 	oTotGrpMov := TRFunction():New(oSection1:Cell("MOVIMENTO"),,"SUM", oBrkGrupo,/*Titulo*/,/*cPicture*/,;
			{ || Iif(cArqDad->TIPOCONTA="1",0,(cArqDad->SALDOCRD - cArqDad->SALDODEB)) },.F.,.F.,.F.,oSection1)
		   	oTotGrpMov:Disable()  
   	  
			If cTpValor != "P"	
	 			TRFunction():New(oSection1:Cell("MOVIMENTO"),,"ONPRINT",oBrkGrupo/*oBreak*/,/*Titulo*/,/*cPicture*/,;
	 			{ || FEValCtb(oTotGrpMov:GetValue(),,,TAM_VALOR-2,nDecimais,.T.,cPicture,If(Round(NoRound(oTotGrpMov:GetValue(),3),2) < 0, "1","2"),,,,,,lPrintZero,.F.)},.F.,.F.,.F.,oSection1) 
	 		Else
	 			TRFunction():New(oSection1:Cell("MOVIMENTO"),,"ONPRINT",oBrkGrupo/*oBreak*/,/*Titulo*/,/*cPicture*/,;
	 			{ || PadL(FEValCtb(oTotGrpMov:GetValue(),,,TAM_VALOR-2,nDecimais,.T.,cPicture,If(Round(NoRound(oTotGrpMov:GetValue(),3),2) < 0, "1","2"),,,,,,lPrintZero,.F.),TAM_VALOR)},.F.,.F.,.F.,oSection1) 	 		 			
	 		EndIf
		Endif		
	EndIf

	//******************************
	// Total Geral do relatorio    *
	//******************************
	oBrkGeral := TRBreak():New(oSection1, { || cArqDad->(!Eof()) },{|| " T O T A I S " },,,.F.)		

	// Totaliza
	oTotGerDeb := TRFunction():New(oSection1:Cell("SALDODEB"),,"SUM",/*oBreak*/,/*Titulo*/,/*cPicture*/,;
		{ || Iif(cArqDad->TIPOCONTA="1",0,cArqDad->SALDODEB) },.F.,.F.,.F.,oSection1)
   	oTotGerDeb:Disable()

	oTotGerCrd := TRFunction():New(oSection1:Cell("SALDOCRD"),,"SUM",/*oBreak*/,/*Titulo*/,/*cPicture*/,;
		{ || Iif(cArqDad->TIPOCONTA="1",0,cArqDad->SALDOCRD) },.F.,.F.,.F.,oSection1)
   	oTotGerCrd:Disable()     

                    
	If cTpValor != "P"	
	    TRFunction():New(oSection1:Cell("SALDODEB"),,"ONPRINT",oBrkGeral/*oBreak*/,/*Titulo*/,/*cPicture*/,;
	  		{ || FEValCtb(oTotGerDeb:GetValue(),,,TAM_VALOR,nDecimais,.F.,cPicture,"1",,,,,,lPrintZero,.F.) },.F.,.F.,.F.,oSection1)
		                    		
	 	TRFunction():New(oSection1:Cell("SALDOCRD"),,"ONPRINT",oBrkGeral/*oBreak*/,/*Titulo*/,/*cPicture*/,;
	 		{ || FEValCtb(oTotGerCrd:GetValue(),,,TAM_VALOR,nDecimais,.F.,cPicture,"2",,,,,,lPrintZero,.F.)},.F.,.F.,.F.,oSection1) 
	Else	 		
	    TRFunction():New(oSection1:Cell("SALDODEB"),,"ONPRINT",oBrkGeral/*oBreak*/,/*Titulo*/,/*cPicture*/,;
	  		{ || PadL(FEValCtb(oTotGerDeb:GetValue(),,,TAM_VALOR,nDecimais,.F.,cPicture,"1",,,,,,lPrintZero,.F.),TAM_VALOR) },.F.,.F.,.F.,oSection1)
		                    		
	 	TRFunction():New(oSection1:Cell("SALDOCRD"),,"ONPRINT",oBrkGeral/*oBreak*/,/*Titulo*/,/*cPicture*/,;
	 		{ || PadL(FEValCtb(oTotGerCrd:GetValue(),,,TAM_VALOR,nDecimais,.F.,cPicture,"2",,,,,,lPrintZero,.F.),TAM_VALOR)},.F.,.F.,.F.,oSection1) 
	EndIf

 	If lImpMov  
 	 	oTotGerMov := TRFunction():New(oSection1:Cell("MOVIMENTO"),,"SUM",/*oBreak*/,/*Titulo*/,/*cPicture*/,;
			{ || Iif(cArqDad->TIPOCONTA="1",0,(cArqDad->SALDOCRD - cArqDad->SALDODEB)) },.F.,.F.,.F.,oSection1)
	   	oTotGerMov:Disable()  
   	  
		If cTpValor != "P"	
 			TRFunction():New(oSection1:Cell("MOVIMENTO"),,"ONPRINT",oBrkGeral/*oBreak*/,/*Titulo*/,/*cPicture*/,;
 				{ || FEValCtb(oTotGerMov:GetValue(),,,TAM_VALOR-2,nDecimais,.T.,cPicture,If(Round(NoRound(oTotGerMov:GetValue(),3),2) < 0, "1","2"),,,,,,lPrintZero,.F.)},.F.,.F.,.F.,oSection1) 
 		Else
 			TRFunction():New(oSection1:Cell("MOVIMENTO"),,"ONPRINT",oBrkGeral/*oBreak*/,/*Titulo*/,/*cPicture*/,;
 				{ || Padl(FEValCtb(oTotGerMov:GetValue(),,,TAM_VALOR-2,nDecimais,.T.,cPicture,If(Round(NoRound(oTotGerMov:GetValue(),3),2) < 0, "1","2"),,,,,,lPrintZero,.F.),TAM_VALOR)},.F.,.F.,.F.,oSection1)  		
 		EndIf
    Endif

	oSection1:OnPrintLine( {|| 	FECT40OnPrint( lPula, lQbConta, nMaxLin, @cTipoAnt, @nLinReport, @cGrupoAnt ) } )
           
	oSection1:Print()

	If MV_PAR30 == 1     
	    oReport:Section(1):SetHeaderSection(.F.)                                                       
		FEImpQuad(Tamanho,X3USO("CT2_DCD"),dDataFim,MV_PAR14,aQuadro,cDescMoeda,oReport:ClassName(),(If (lImpAntLP,dDataLP,cTod(""))),cPicture,nDecimais,lPrintZero,MV_PAR16,oReport)
	EndIf	

EndIf     

dbSelectArea("cArqDad")
Set Filter To
dbCloseArea()
If Select(cArqDad) == 0
	FErase(cArqDad+GetDBExtension())
	FErase(cArqDad+OrdBagExt())
EndIF	

Return .T.

  
/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³FECT40OnPrint ºAutor ³ Gustavo Henrique º Data ³ 07/02/07   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ Executa acoes especificadas nos parametros do relatorio,   º±±
±±º          ³ antes de imprimir cada linha.                              º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºParametros³ EXPL1 - Indicar se deve saltar linha entre conta sintetica º±±
±±º          ³ EXPL2 - Indicar se deve quebrar pagina por conta           º±±
±±º          ³ EXPN3 - Informar o total de linhas por pagina do balancete º±±
±±º          ³ EXPC4 - Guardar o tipo da conta impressa (sint./analitica) º±±
±±º          ³ EXPN5 - Guardar linha atual do relatorio para validacao    º±±
±±º          ³         com o valor do parametro EXPN3.                    º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºRetorno   ³ EXPL1 - Indicar se deve imprimir a linha (.T.)             º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ Contabilidade Gerencial                                    º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Static Function FECT40OnPrint( lPula, lQbConta, nMaxLin, cTipoAnt, nLinReport )
                                                                        
Local lRet := .T.           

// Verifica salto de linha para conta sintetica (MV_PAR23)
If lPula .And. (cTipoAnt == "1" .Or. (cArqDad->TIPOCONTA == "1" .And. cTipoAnt == "2"))
	oReport:SkipLine()
EndIf	

// Verifica quebra de pagina por conta (MV_PAR17)
/*
If lQbConta .And. cArqDad->NIVEL1
	oReport:EndPage()
	nLinReport := 9
	Return
EndIf	
*/

// Verifica numero maximo de linhas por pagina (MV_PAR34)
If ! Empty(nMaxLin)
	CTR040MaxL(nMaxLin,@nLinReport)
EndIf	

cTipoAnt := cArqDad->TIPOCONTA

If MV_PAR11 == 1		// Apenas sinteticas
	lRet := (cArqDad->TIPOCONTA == "1")
ElseIf MV_PAR11 == 2	// Apenas analiticas
	lRet := (cArqDad->TIPOCONTA == "2")
EndIf

Return lRet


/*
------------------------------------------------------------------------- RELEASE 3 -------------------------------------------------------------------------------
*/



/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Fun‡…o	 ³ Ctbr040R3³ Autor ³ Pilar S Albaladejo	³ Data ³ 12.09.00 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³ Balancete Analitico Sintetico Modelo 1			 		  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Sintaxe   ³ Ctbr040()                               			 		  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Retorno	 ³ Nenhum       											  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Uso    	 ³ Generico     											  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ Nenhum													  ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/
Static Function CtbR040R3(wnRel)

Local aSetOfBook
Local aCtbMoeda	:= {}
LOCAL cDesc1 		:= OemToAnsi("Este programa ira imprimir o Balancete de Verificacao Modelo 1 (80 Colunas), a")	
LOCAL cDesc2 		:= OemToansi("conta eh impressa limitando-se a 20 caracteres e sua descricao 30 caracteres,")   
LOCAL cDesc3		:= OemToansi("os valores impressao sao saldo anterior, debito, credito e saldo atual do periodo.")   
LOCAL cString		:= "CT1"
Local cTitOrig		:= ""
Local lRet			:= .T.
Local nDivide		:= 1
Local lExterno 	:= .F.
Local nQuadro
Local lPerg			:= .T.
PRIVATE nLastKey 		:= 0
PRIVATE cPerg			:= "FECTBR0400"
PRIVATE aLinha		:= {}
PRIVATE nomeProg  	:= "FECTBR040"
PRIVATE titulo 		:= OemToAnsi("Balancete de Verificacao por Centro de Custo") 	
Private aSelFil		:= {} 

Default wnRel := ""

lExterno := !Empty(wnRel)

If ! lExterno
	PRIVATE Tamanho		:= "M"
	PRIVATE aReturn 	:= { OemToAnsi("Zebrado"), 1,OemToAnsi("Administracao"), 2, 2, 1, "",1 }  
EndIf

cTitOrig	:= titulo

If ( !AMIIn(34) )		// Acesso somente pelo SIGACTB
	Return
EndIf

li	:= 60 //80 



Private aQuadro := { "","","","","","","",""}              

For nQuadro :=1 To Len(aQuadro)
	aQuadro[nQuadro] := Space(Len(CriaVar("CT1_CONTA")))
Next	

CtbCarTxt()

Pergunte("FECTBR0400",.F.)





If ! lExterno  	
	Pergunte("FECTBR0400",.T.)  
    
	If MV_PAR36 == 1 .And. Len( aSelFil ) <= 0 .And. !IsBlind()
		aSelFil := AdmGetFil()
		If Len( aSelFil ) <= 0
			Return
		EndIf 
	EndIf

	wnrel	:= "FECTBR040"            //Nome Default do relatorio em Disco
	wnrel := SetPrint(cString,wnrel,,@titulo,cDesc1,cDesc2,cDesc3,.F.,"",,Tamanho)  

Endif 

If wnRel == "CTBR110"
	If MV_PAR36 == 1 .And. Len( aSelFil ) <= 0 .And. !IsBlind()
		aSelFil := AdmGetFil()
		If Len( aSelFil ) <= 0
			Return
		EndIf 
	EndIf
Endif

If nLastKey == 27
	Set Filter To
	Return
Endif

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Verifica se usa Set Of Books + Plano Gerencial (Se usar Plano³
//³ Gerencial -> montagem especifica para impressao)		     ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If !ct040Valid(MV_PAR12)
	lRet := .F.
Else
   aSetOfBook := CTBSetOf(MV_PAR12)
Endif

If MV_PAR26 == 2			// Divide por cem
	nDivide := 100
ElseIf MV_PAR26 == 3		// Divide por mil
	nDivide := 1000
ElseIf MV_PAR26 == 4		// Divide por milhao
	nDivide := 1000000
EndIf	

If lRet
	aCtbMoeda  	:= CtbMoeda(MV_PAR14,nDivide)
	If Empty(aCtbMoeda[1])                       
      Help(" ",1,"NOMOEDA")
      lRet := .F.
   Endif
Endif

If lRet
	If (MV_PAR31 == 1) .and. ( Empty(MV_PAR32) .or. Empty(MV_PAR33) )
		cMensagem	:= "Favor preencher os parametros Grupos Receitas/Despesas e "	
		cMensagem	+= "Data Sld Ant. Receitas/Desp. "	
		MsgAlert(cMensagem,"Ignora Sl Ant.Rec/Des")	
		lRet    	:= .F.	
    EndIf
EndIf

If !lRet
	Set Filter To
	Return
EndIf


If !lExterno .And. ( MV_PAR22 == 1 .Or. ( MV_PAR22 == 2 .And.	aReturn[4] == 2 ))	//Se nao imprime coluna mov. e eh paisagem
	tamanho := "G"
EndIf	

If nLastKey == 27
	Set Filter To
	Return
Endif

RptStatus({|lEnd| CTR040Imp(@lEnd,wnRel,cString,aSetOfBook,aCtbMoeda,nDivide,lExterno,cTitorig)})

Return

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Program   ³CTR040IMP ³ Autor ³ Pilar S. Albaladejo   ³ Data ³ 24.07.00 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³ Imprime relatorio -> Balancete Verificacao Modelo 1        ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Sintaxe   ³CTR040Imp(lEnd,WnRel,cString,aSetOfBook,aCtbMoeda)          ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Retorno   ³ Nenhum                                                     ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Uso       ³ Generico                                                   ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ lEnd       - A‡ao do Codeblock                             ³±±
±±³          ³ WnRel      - T¡tulo do relat¢rio                           ³±±
±±³          ³ cString    - Mensagem                                      ³±±
±±³          ³ aSetOfBook - Matriz ref. Config. Relatorio                 ³±±
±±³          ³ aCtbMoeda  - Matriz ref. a moeda                           ³±±
±±³          ³ nDivide    - Valor para divisao de valores                 ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/
Static Function CTR040Imp(lEnd,WnRel,cString,aSetOfBook,aCtbMoeda,nDivide,lExterno,cTitOrig)

Local aColunas		:= {}
LOCAL CbTxt			:= Space(10)
Local CbCont		:= 0
LOCAL limite		:= 132
Local cabec1   	:= ""
Local cabec2   	:= ""
Local cSeparador	:= ""
Local cPicture
Local cDescMoeda
Local cCodMasc
Local cMascara
Local cGrupo		:= ""
Local cArqDad		:= ""
Local cArqTmp		:= ""
Local dDataFim 	:= mv_par02
Local lFirstPage	:= .T.
Local lJaPulou		:= .F.
Local lPrintZero	:= Iif(MV_PAR24==1,.T.,.F.)
Local lPula			:= Iif(MV_PAR23==1,.T.,.F.) 
Local lNormal		:= Iif(MV_PAR25==1,.T.,.F.)
Local lVlrZerado	:= Iif(MV_PAR13==1,.T.,.F.)
Local l132			:= .T.
Local nDecimais
Local nTotDeb		:= 0
Local nTotCrd		:= 0
Local nTotMov		:= 0
Local nGrpDeb		:= 0
Local nGrpCrd		:= 0                     
Local cSegAte   	:= MV_PAR27
Local nDigitAte	:= 0
Local lImpAntLP	:= Iif(MV_PAR28 == 1,.T.,.F.)
Local dDataLP		:= MV_PAR29
Local lImpSint		:= Iif(MV_PAR11=1 .Or. MV_PAR11 ==3,.T.,.F.)
Local lRecDesp0		:= Iif(MV_PAR31==1,.T.,.F.)
Local cRecDesp		:= MV_PAR32
Local dDtZeraRD		:= MV_PAR33
Local n
Local oMeter
Local oText
Local oDlg
Local lImpPaisgm	:= .F.	
Local nMaxLin   	:= iif( MV_PAR34 > 58 , 58 , MV_PAR34 )
Local cMoedaDsc		:= MV_PAR35
Local nMasc			:= 0 
Local cMasc			:= ""
Local dDataOld 		:= dDataBase
Local aStru 			:= {}
Local cArqInd			:= CriaTrab(Nil, .F.)
Local cArqAux			:= "cArqDad"


cDescMoeda 	:= Alltrim(aCtbMoeda[2])
nDecimais 	:= DecimalCTB(aSetOfBook,MV_PAR14)

if !lExterno // Chamado THZFJJ - Se não for chado por outro relatorio trocar a data 
	dDataBase := dDataFim
endif
	
If Empty(aSetOfBook[2])
	cMascara := GetMv("MV_MASCARA")
Else
	cMascara 	:= RetMasCtb(aSetOfBook[2],@cSeparador)
EndIf
cPicture 		:= aSetOfBook[4]

If MV_PAR22 == 2 .And. !lExterno .And. 	aReturn[4] == 2	//Se nao imprime coluna mov. e eh paisagem
	lImpPaisgm	:= .T.
	limite		:= 220
EndIf

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Carrega titulo do relatorio: Analitico / Sintetico			  ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If Alltrim(Titulo) == Alltrim(cTitorig) // Se o titulo do relatorio nao foi alterado pelo usuario
	IF MV_PAR11 == 1
		Titulo:=	OemToAnsi("BALANCETE DE VERIFICACAO POR CENTRO DE CUSTO SINTETICO DE ")	
	ElseIf MV_PAR11 == 2
		Titulo:=	OemToAnsi("BALANCETE DE VERIFICACAO POR CENTRO DE CUSTO ANALITICO DE ")	
	ElseIf MV_PAR11 == 3
		Titulo:=	OemToAnsi("BALANCETE DE VERIFICACAO POR CENTRO DE CUSTO DE ")	
	EndIf
EndIf	
Titulo += 	DTOC(mv_par01) + OemToAnsi(" de ") + Dtoc(mv_par02) + ;
			OemToAnsi(" Moeda ") + cDescMoeda + CtbTitSaldo(MV_PAR16)+ ;
			OemToAnsi(" CC ") 
	If !Empty(MV_PAR05) .or. !Empty(MV_PAR06)
		if Alltrim(MV_PAR05) == Alltrim(MV_PAR06)
			Titulo += " "+Alltrim(MV_PAR05)
		Else
			Titulo += " "+Alltrim(MV_PAR05)+"-"+Alltrim(MV_PAR06)
		Endif	
	Endif	
	If !Empty(MV_PAR07) .or. !Empty(MV_PAR08)
		if Alltrim(MV_PAR07) == Alltrim(MV_PAR08)
			Titulo += " "+Alltrim(MV_PAR07)
		Else
			Titulo += " "+Alltrim(MV_PAR07)+"-"+Alltrim(MV_PAR08)
		Endif	
	Endif	
	If !Empty(MV_PAR09) .or. !Empty(MV_PAR10)
		if Alltrim(MV_PAR09) == Alltrim(MV_PAR10)
			Titulo += " "+Alltrim(MV_PAR09)
		Else
			Titulo += " "+Alltrim(MV_PAR09)+"-"+Alltrim(MV_PAR10)
		Endif	
	Endif				
			
If nDivide > 1			
	Titulo += " (" + OemToAnsi("") + Alltrim(Str(nDivide)) + ")"
EndIf	

If MV_PAR22 == 1 .And. ! lExterno		// Se imprime saldo movimento do periodo
	cabec1 := OemToAnsi("|  CODIGO                     |   D  E  S  C  R  I  C  A  O                     |   SALDO ANTERIOR              |    DEBITO        |    CREDITO        | MOVIMENTO DO PERIODO          |   SALDO ATUAL                     |")  
	tamanho := "G"
	limite	:= 220        
	l132	:= .F.
Else	  
	If lImpPaisgm		//Se imprime em formato paisagem
		cabec1 := "|  CODIGO                     |      D E S C R I C A O                          |        SALDO ANTERIOR             |           DEBITO             |            CREDITO                |         SALDO ATUAL               |"  
	Else	
		cabec1 := OemToAnsi("|  CODIGO               |   D  E  S  C  R  I  C  A  O    |   SALDO ANTERIOR  |      DEBITO    |      CREDITO   |   SALDO ATUAL     |")  
	EndIf
Endif

If ! lExterno
	SetDefault(aReturn,cString,,,Tamanho,If(Tamanho="G",2,1))
Endif

If l132
	If lImpPaisgm
		aColunas := { 000,001, 030, 032, 080,086, 116, 118, 147, 151, 183, , ,187,219}
	Else	
		aColunas := { 000,001, 024, 025, 057,058, 077, 078, 094, 095, 111, , , 112, 131 }
	EndIf
Else                   
	aColunas := { 000,001, 030, 032, 080,082, 112, 114, 131, 133, 151, 153, 183,185,219}
Endif

If ! lExterno
	m_pag := MV_PAR15
Endif

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Monta Arquivo Temporario para Impressao			  		     ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

MsgMeter({|	oMeter, oText, oDlg, lEnd | ;
				CTGerPlan(oMeter, oText, oDlg, @lEnd,@cArqTmp,;
				mv_par01,mv_par02,"CT3","",mv_par03,mv_par04,MV_PAR05,MV_PAR06,,,,,MV_PAR14,;
				MV_PAR16,aSetOfBook,MV_PAR18,MV_PAR19,MV_PAR20,MV_PAR21,;
				.F.,.T.,MV_PAR17,,lImpAntLP,dDataLP,nDivide,lVlrZerado,,,,,,,,,,,,,,lImpSint,aReturn[7],lRecDesp0,;
				cRecDesp,dDtZeraRD,,,,,,,cMoedaDsc,,aSelFil)},;
				OemToAnsi(OemToAnsi("Criando Arquivo Tempor rio...")),;  
				OemToAnsi("Balancete Verificacao"))
				
If Select("cArqTmp") <> 0
	CriaArqDad("cArqTmp",@cArqDad,@cArqAux,@cArqInd)
	CopDados("cArqTmp","cArqDad")
	("cArqTmp")->(dbCloseArea())
Endif	

MsgMeter({|	oMeter, oText, oDlg, lEnd | ;
				CTGerPlan(oMeter, oText, oDlg, @lEnd,@cArqTmp,;
				mv_par01,mv_par02,"CT3","",mv_par03,mv_par04,MV_PAR07,MV_PAR08,,,,,MV_PAR14,;
				MV_PAR16,aSetOfBook,MV_PAR18,MV_PAR19,MV_PAR20,MV_PAR21,;
				.F.,.T.,MV_PAR17,,lImpAntLP,dDataLP,nDivide,lVlrZerado,,,,,,,,,,,,,,lImpSint,aReturn[7],lRecDesp0,;
				cRecDesp,dDtZeraRD,,,,,,,cMoedaDsc,,aSelFil)},;
				OemToAnsi(OemToAnsi("Criando Arquivo Tempor rio...")),;  
				OemToAnsi("Balancete Verificacao"))

If Select("cArqTmp") <> 0
	CriaArqDad("cArqTmp",@cArqDad,@cArqAux,@cArqInd)
	CopDados("cArqTmp","cArqDad")
	("cArqTmp")->(dbCloseArea())
Endif	

MsgMeter({|	oMeter, oText, oDlg, lEnd | ;
				CTGerPlan(oMeter, oText, oDlg, @lEnd,@cArqTmp,;
				mv_par01,mv_par02,"CT3","",mv_par03,mv_par04,MV_PAR09,MV_PAR10,,,,,MV_PAR14,;
				MV_PAR16,aSetOfBook,MV_PAR18,MV_PAR19,MV_PAR20,MV_PAR21,;
				.F.,.T.,MV_PAR17,,lImpAntLP,dDataLP,nDivide,lVlrZerado,,,,,,,,,,,,,,lImpSint,aReturn[7],lRecDesp0,;
				cRecDesp,dDtZeraRD,,,,,,,cMoedaDsc,,aSelFil)},;
				OemToAnsi(OemToAnsi("Criando Arquivo Tempor rio...")),;  
				OemToAnsi("Balancete Verificacao"))				

If Select("cArqTmp") <> 0
	CriaArqDad("cArqTmp",@cArqDad,@cArqAux,@cArqInd)
	CopDados("cArqTmp","cArqDad")
	("cArqTmp")->(dbCloseArea())	
Endif	

If Select("cArqDad") == 0
	Return .t.
EndIf     

// Verifica Se existe filtragem Ate o Segmento
If !Empty(cSegAte)
	
	//Efetua tratamento da mascara para consegui efetuar o controle do segmento 
	For nMasc := 1 to Len( cMascara )
			
		cMasc += SubStr( cMascara,nMasc,1 )
			
	Next nMasc


	nDigitAte := CtbRelDig(cSegAte,cMasc) 	
	
EndIf				

dbSelectArea("cArqDad")
dbGoTop()

SetRegua(RecCount())

cGrupo := GRUPO

While !Eof()

	If lEnd
		@Prow()+1,0 PSAY OemToAnsi("***** CANCELADO PELO OPERADOR *****")   
		Exit
	EndIF

	IncRegua()

	******************** "FILTRAGEM" PARA IMPRESSAO *************************

	If MV_PAR11 == 1					// So imprime Sinteticas
		If TIPOCONTA == "2"
			dbSkip()
			Loop
		EndIf
	ElseIf MV_PAR11 == 2				// So imprime Analiticas
		If TIPOCONTA == "1"
			dbSkip()
			Loop
		EndIf
	EndIf

	//Filtragem ate o Segmento ( antigo nivel do SIGACON)		
	If !Empty(cSegAte)
		If Len(Alltrim(CONTA)) > nDigitAte
			dbSkip()
			Loop
		Endif
	EndIf
	

	************************* ROTINA DE IMPRESSAO *************************

	If MV_PAR17 == 1 							// Grupo Diferente - Totaliza e Quebra
		If cGrupo != GRUPO
			@li,00 PSAY REPLICATE("-",limite)
			li+=2
			@li,00 PSAY REPLICATE("-",limite)
			li++
			@li,aColunas[COL_SEPARA1] PSAY "|"
			@li,39 PSAY OemToAnsi("T O T A I S  D O  G R U P O: ") + cGrupo + ") : "  		       
			@li,aColunas[COL_SEPARA4] PSAY "|"
			FEValCtb(nGrpDeb,li,aColunas[COL_VLR_DEBITO],16,nDecimais,.F.,cPicture,"1", , , , , ,lPrintZero)
			@li,aColunas[COL_SEPARA5] PSAY "|"
			FEValCtb(nGrpCrd,li,aColunas[COL_VLR_CREDITO],16,nDecimais,.F.,cPicture,"2", , , , , ,lPrintZero)
			@li,aColunas[COL_SEPARA6] PSAY "|"
			@li,aColunas[COL_SEPARA8] PSAY "|"
			li++      
			li		:= 60
			cGrupo	:= GRUPO
			nGrpDeb	:= 0
			nGrpCrd	:= 0		
		EndIf		

	ElseIf  MV_PAR17 == 2
		If NIVEL1				// Sintetica de 1o. grupo
			//li := 60
		EndIf
	EndIf

	IF li > nMaxLin
		If !lFirstPage
			@Prow()+1,00 PSAY	Replicate("-",limite)
		EndIf
		CtCGCCabec(,,,Cabec1,Cabec2,dDataFim,Titulo,,"2",Tamanho,,dDataOld)
		lFirstPage := .F.
	EndIf

	@ li,aColunas[COL_SEPARA1] 		PSAY "|"
	If lNormal
		If TIPOCONTA == "2" 		// Analitica -> Desloca 2 posicoes
			If l132
				EntidadeCTB(CONTA,li,aColunas[COL_CONTA]+2,21,.F.,cMascara,cSeparador)			
			Else
				EntidadeCTB(CONTA,li,aColunas[COL_CONTA]+2,27,.F.,cMascara,cSeparador)
			EndIf
		Else	                                              
			If l132
				EntidadeCTB(CONTA,li,aColunas[COL_CONTA],23,.F.,cMascara,cSeparador)
			Else                                                                     
				EntidadeCTB(CONTA,li,aColunas[COL_CONTA],29,.F.,cMascara,cSeparador)
			EndIf			
		EndIf	
	Else
		If TIPOCONTA == "2"		// Analitica -> Desloca 2 posicoes
			@li,aColunas[COL_CONTA] PSAY Alltrim(CTARES)
		Else
			@li,aColunas[COL_CONTA] PSAY Alltrim(CONTA)
		EndIf						
	EndIf	

	@ li,aColunas[COL_SEPARA2] 		PSAY "|"

	If !l132
		@ li,aColunas[COL_DESCRICAO] 	PSAY Substr(DESCCTA,1,48)
	Else		
		@ li,aColunas[COL_DESCRICAO] 	PSAY Substr(DESCCTA,1,30)
	Endif	

	@ li,aColunas[COL_SEPARA3]		PSAY "|"
	FEValCtb(SALDOANT,li,aColunas[COL_SALDO_ANT],17,nDecimais,.T.,cPicture,NORMAL, , , , , ,lPrintZero)

	@ li,aColunas[COL_SEPARA4]		PSAY "|"
	FEValCtb(SALDODEB,li,aColunas[COL_VLR_DEBITO],16,nDecimais,.F.,cPicture,NORMAL, , , , , ,lPrintZero)

	@ li,aColunas[COL_SEPARA5]		PSAY "|"
	FEValCtb(SALDOCRD,li,aColunas[COL_VLR_CREDITO],16,nDecimais,.F.,cPicture,NORMAL, , , , , ,lPrintZero)
	
	@ li,aColunas[COL_SEPARA6]		PSAY "|"

	If !l132
		FEValCtb(MOVIMENTO,li,aColunas[COL_MOVIMENTO],17,nDecimais,.T.,cPicture,NORMAL, , , , , ,lPrintZero)
		@ li,aColunas[COL_SEPARA7] PSAY "|"	
	Endif
	FEValCtb(SALDOATU,li,aColunas[COL_SALDO_ATU],17,nDecimais,.T.,cPicture,NORMAL, , , , , ,lPrintZero)

	@ li,aColunas[COL_SEPARA8] PSAY "|"
	
	lJaPulou := .F.
	
	If lPula .And. TIPOCONTA == "1"				// Pula linha entre sinteticas
		li++
		@ li,aColunas[COL_SEPARA1] PSAY "|"
		@ li,aColunas[COL_SEPARA2] PSAY "|"
		@ li,aColunas[COL_SEPARA3] PSAY "|"	
		@ li,aColunas[COL_SEPARA4] PSAY "|"
		@ li,aColunas[COL_SEPARA5] PSAY "|"
		@ li,aColunas[COL_SEPARA6] PSAY "|"
		If !l132  
			@ li,aColunas[COL_SEPARA7] PSAY "|"
			@ li,aColunas[COL_SEPARA8] PSAY "|"
		Else
			@ li,aColunas[COL_SEPARA8] PSAY "|"
		EndIf	
		li++
		lJaPulou := .T.
	Else
		li++
	EndIf			

	************************* FIM   DA  IMPRESSAO *************************

	If MV_PAR11 == 1					// So imprime Sinteticas - Soma Sinteticas
		If TIPOCONTA == "1"
			If NIVEL1
				nTotDeb += SALDODEB
				nTotCrd += SALDOCRD
				nGrpDeb += SALDODEB
				nGrpCrd += SALDOCRD
			EndIf
		EndIf
	Else									// Soma Analiticas
		If Empty(cSegAte)				//Se nao tiver filtragem ate o nivel
			If TIPOCONTA == "2"
				nTotDeb += SALDODEB
				nTotCrd += SALDOCRD
				nGrpDeb += SALDODEB
				nGrpCrd += SALDOCRD
			EndIf
		Else							//Se tiver filtragem, somo somente as sinteticas
			If TIPOCONTA == "1"
				If NIVEL1
					nTotDeb += SALDODEB
					nTotCrd += SALDOCRD
					nGrpDeb += SALDODEB
					nGrpCrd += SALDOCRD
				EndIf
			EndIf	
    	Endif			
	EndIf

	dbSkip()       
	If lPula .And. TIPOCONTA == "1" 			// Pula linha entre sinteticas
		If !lJaPulou
			@ li,aColunas[COL_SEPARA1] PSAY "|"
			@ li,aColunas[COL_SEPARA2] PSAY "|"
			@ li,aColunas[COL_SEPARA3] PSAY "|"	
			@ li,aColunas[COL_SEPARA4] PSAY "|"
			@ li,aColunas[COL_SEPARA5] PSAY "|"
			@ li,aColunas[COL_SEPARA6] PSAY "|"
			If !l132  
				@ li,aColunas[COL_SEPARA7] PSAY "|"
				@ li,aColunas[COL_SEPARA8] PSAY "|"
			Else
				@ li,aColunas[COL_SEPARA8] PSAY "|"
			EndIf	
			li++
		EndIf	
	EndIf		
EndDO

//IF li != 80 .And. !lEnd
IF li <= 58 .OR. li >= 58 .And. !lEnd
	IF li > nMaxLin
		@Prow()+1,00 PSAY	Replicate("-",limite)
		CtCGCCabec(,,,Cabec1,Cabec2,dDataFim,Titulo,,"2",Tamanho,,dDataOld)
		li++
	End
	If MV_PAR17 == 1							// Grupo Diferente - Totaliza e Quebra
		If cGrupo != GRUPO .Or. Eof()
			@li,00 PSAY REPLICATE("-",limite)
			li++
			@li,aColunas[COL_SEPARA1] PSAY "|"
			@li,39 PSAY OemToAnsi("T O T A I S  D O  G R U P O: ") + cGrupo + ") : "  		
			@li,aColunas[COL_SEPARA4] PSAY "|"
			FEValCtb(nGrpDeb,li,aColunas[COL_VLR_DEBITO],16,nDecimais,.F.,cPicture,"1", , , , , ,lPrintZero)
			@li,aColunas[COL_SEPARA5] PSAY "|"
			FEValCtb(nGrpCrd,li,aColunas[COL_VLR_CREDITO],16,nDecimais,.F.,cPicture,"2", , , , , ,lPrintZero)
			@li,aColunas[COL_SEPARA6] PSAY "|"
			If !l132
				nTotMov := nTotMov + (nGrpCrd - nGrpDeb)
				If Round(NoRound(nTotMov,3),2) < 0
					FEValCtb(nTotMov,li,aColunas[COL_MOVIMENTO],17,nDecimais,.T.,cPicture,"1", , , , , ,lPrintZero)
				ElseIf Round(NoRound(nTotMov,3),2) > 0
					FEValCtb(nTotMov,li,aColunas[COL_MOVIMENTO],17,nDecimais,.T.,cPicture,"2", , , , , ,lPrintZero)
                EndIf
				@ li,aColunas[COL_SEPARA7] PSAY "|"	
			Endif
			@li,aColunas[COL_SEPARA8] PSAY "|"
			li++
			@li,00 PSAY REPLICATE("-",limite)
			li+=2
		EndIf		
	EndIf

	@li,00 PSAY REPLICATE("-",limite)
	li++
	@li,aColunas[COL_SEPARA1] PSAY "|"
	@li,39 PSAY OemToAnsi("T O T A I S  D O  P E R I O D O: ")  		
	@li,aColunas[COL_SEPARA4] PSAY "|"
	FEValCtb(nTotDeb,li,aColunas[COL_VLR_DEBITO],16,nDecimais,.F.,cPicture,"1", , , , , ,lPrintZero)
	@li,aColunas[COL_SEPARA5] PSAY "|"
	FEValCtb(nTotCrd,li,aColunas[COL_VLR_CREDITO],16,nDecimais,.F.,cPicture,"2", , , , , ,lPrintZero)
	@li,aColunas[COL_SEPARA6] PSAY "|"
 	If !l132	
		nTotMov := nTotMov + (nTotCrd - nTotDeb)
		If Round(NoRound(nTotMov,3),2) < 0
			FEValCtb(nTotMov,li,aColunas[COL_MOVIMENTO],17,nDecimais,.T.,cPicture,"1", , , , , ,lPrintZero)
		ElseIf Round(NoRound(nTotMov,3),2) > 0
			FEValCtb(nTotMov,li,aColunas[COL_MOVIMENTO],17,nDecimais,.T.,cPicture,"2", , , , , ,lPrintZero)
		EndIf
		@li,aColunas[COL_SEPARA7] PSAY "|"	
	EndIf		                                
	@li,aColunas[COL_SEPARA8] PSAY "|"
	li++
	@li,00 PSAY REPLICATE("-",limite)
	li++
	@li,0 PSAY " "

	IF lExterno 
		If (li + 3) < 60 
			@57,00 PSAY __PrtfatLine()
  		  	@58,01 Psay "Microsiga Software S/A"     
 		   	If Tamanho == "M"
   		 		@58,100 Psay "Hora Termino: " + " " + Time()      
   	 		ElseIf Tamanho == "G"
	   		 	@58,190 Psay "Hora Termino: " + " "+ Time()  
    		Else
	    		@58,050 Psay "Hora Termino: " + " "+ Time()	   
			EndIf               
			@59,00 PSAY __PrtfatLine()
		EndIf	
	Endif
	Set Filter To
EndIF

If MV_PAR30 ==1
	FEImpQuad(Tamanho,X3USO("CT2_DCD"),dDataFim,MV_PAR14,aQuadro,cDescMoeda,nomeprog,(If (lImpAntLP,dDataLP,cTod(""))),cPicture,nDecimais,lPrintZero,MV_PAR16)
EndIf	
	
If aReturn[5] = 1 .And. ! lExterno
	Set Printer To
	Commit
	Ourspool(wnrel)
EndIf

dbSelectArea("cArqDad")
Set Filter To
dbCloseArea() 
If Select("cArqDad") == 0
	FErase(cArqDad+GetDBExtension())
	FErase(cArqDad+OrdBagExt())
EndIF	
dbselectArea("CT2")

If ! lExterno
	MS_FLUSH()
Endif

dDataBase := dDataOld 

Return .T.

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³Programa  ³CT040Valid³ Autor ³ Pilar S. Albaladejo   ³ Data ³ 24.07.00 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³Descri‡…o ³ Valida Perguntas                                           ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Sintaxe   ³Ct040Valid(cSetOfBook)                                      ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Retorno   ³ .T./.F.                                                    ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Uso       ³ Generico                                                   ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³Parametros³ ExpC1 = Codigo da Config. Relatorio                        ³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/
Static Function Ct040Valid(cSetOfBook)

Local aSaveArea:= GetArea()
Local lRet		:= .T.	

If !Empty(cSetOfBook)
	dbSelectArea("CTN")
	dbSetOrder(1)
	If !dbSeek(xfilial()+cSetOfBook)
		aSetOfBook := ("","",0,"","")
		Help(" ",1,"NOSETOF")
		lRet := .F.
	EndIf
EndIf
	
RestArea(aSaveArea)

Return lRet


/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³ CTR040MAXL ºAutor ³ Eduardo Nunes Cirqueira º Data ³  31/01/07 º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³ Baseado no parametro MV_PAR34 ("Num.linhas p/ o Balancete      º±±
±±º          ³ Modelo 1"), cujo conteudo esta na variavel "nMaxLin", controla º±±
±±º          ³ a quebra de pagina no TReport                                  º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP                                                             º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Static Function CTR040MaxL(nMaxLin,nLinReport)

nLinReport++

If nLinReport > nMaxLin
	oReport:EndPage()
	nLinReport := 10
EndIf

Return Nil
                                                                          

/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³ nCtCGCCabTR  º Autor ³ Fabio Jadao Caires      º Data ³ 31/01/07º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³ Chama a funcao padrao CtCGCCabTR reiniciando o contador de      º±±
±±º          ³ linhas para o controle do relatorio.                            º±±
±±º          ³                                                                 º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP                                                              º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
STATIC FUNCTION nCtCGCCabTR(dDataFim,titulo,oReport)

nLinReport := 10                                  
oReport:SetPageNumber(n_pagini)
oReport:OnPageBreak({|| n_pagini += 1})                                                    
nLinReport := 10

RETURN CtCGCCabTR(,,,,,dDataFim,titulo,,,,,oReport)



//------------------------------------------------------------------------------------------
/*/{Protheus.doc} TestaSX1
Permite a manutenção de dados armazenados em .

@author    TOTVS | Developer Studio - Gerado pelo Assistente de Código
@version   1.xx
@since     15/05/2015
/*/
//------------------------------------------------------------------------------------------
Static Function TestaSX1(cPergunta, aPerguntas,aHelp)

	Local nxZ := 1
	Local aSalvArea := GetArea()

	cPergunta  := IIf(cPergunta  == Nil,ParamIXB[1],cPergunta)
	aPerguntas := IIf(aPerguntas == Nil,ParamIXB[2],aPerguntas)

	dbSelectArea("SX1")
	dbSetOrder(1)

	For nxZ := 1 To Len(aPerguntas)
		RecLock("SX1",!dbSeek(cPergunta+StrZero(nxZ,2)))
		Replace  X1_Grupo   With  cPergunta
		Replace  X1_Ordem   With  StrZero(nxZ,2)
		Replace  X1_Pergunt With  aPerguntas[nxZ,2]
		Replace  X1_PerSpa  With  aPerguntas[nxZ,2]
		Replace  X1_PerEng  With  aPerguntas[nxZ,2]
		Replace  X1_Variavl With  "Mv_Ch"+IIf(nxZ <=9,AllTrim(Str(nxZ)),Chr(nxZ + 55))
		Replace  X1_Tipo    With  aPerguntas[nxZ,3]
		Replace  X1_Tamanho With  aPerguntas[nxZ,4]
		Replace  X1_Decimal With  aPerguntas[nxZ,5]
		Replace  X1_GSC     With  aPerguntas[nxZ,6]
		Replace  X1_Var01   With  "Mv_Par"+StrZero(nxZ,2)
		Replace  X1_Valid   With  aPerguntas[nxZ,7]
		Replace  X1_F3      With  aPerguntas[nxZ,8]
		If (aPerguntas[nxZ,6] == "C")
			For nxY := 9 To 13
				If (aPerguntas[nxZ,nxY] == "")
					Exit
				Else
					Do Case
					Case ((nxY - 8) == 1)
						Replace X1_Def01 With aPerguntas[nxZ,nxY]
					Case ((nxY - 8) == 2)
						Replace X1_Def02 With aPerguntas[nxZ,nxY]
					Case ((nxY - 8) == 3)
						Replace X1_Def03 With aPerguntas[nxZ,nxY]
					Case ((nxY - 8) == 4)
						Replace X1_Def04 With aPerguntas[nxZ,nxY]
					Case ((nxY - 8) == 5)
						Replace X1_Def05 With aPerguntas[nxZ,nxY]
					EndCase
				EndIf
			Next
		EndIf
		MsUnLock()
		
		cKey  := "P." + AllTrim( cPergunta ) + AllTrim( StrZero(nxZ,2) ) + "."		
		PutSX1Help(cKey,aHelp[nxZ],aHelp[nxZ],aHelp[nxZ])
		
		
	Next
	
	

	RestArea(aSalvArea)

Return Nil


Static Function FEValCtb(	nSaldo,nLin,nCol,nTamanho,nDecimais,lSinal,cPicture,;
					cTipo,cConta,lGraf,oPrint,cTipoSinal, cIdentifi,lPrintZero,lSay)

Local aSaveArea	:= GetArea()
Local cImpSaldo := ""
Local lDifZero	:= .T.
Local lInformada:= .T.                      
Local cCharSinal:= ""

Default nSaldo := 0

lPrintZero := Iif(lPrintZero==Nil,.T.,lPrintZero)

// Nao imprime o valor 0,00
If !lPrintZero 
	If (Int(nSaldo*100)/100) == 0
		lDifZero := .F.			// O saldo nao eh diferente de zero
	EndIf
EndIf		

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Tipo D -> Default (D/C)												  ³
//³ Tipo S -> Imprime saldo com sinal									  ³
//³ Tipo P -> Imprime saldo entre parenteses (qdo. negativo)	  ³
//³ Tipo C -> So imprime "C" (o "D" nao e impresso)              ³
//³ Tipo N -> Imprime saldo com sinal (-) se o saldo for credor³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
DEFAULT cTipoSinal := GetMV("MV_TPVALOR")       // Assume valor default

DEFAULT lSay := .T.

cTipo 		:= Iif(cTipo == Nil, Space(1), cTipo)
nDecimais	:= Iif(nDecimais==Nil,GetMv("MV_CENT"),nDecimais)

dbSelectArea("CT1")
dbSetOrder(1)

If !Empty(cConta) .And. Empty(cTipo)
	If MsSeek(cFilial+cConta)
		cTipo := CT1->CT1_NORMAL
	Endif
EndIf

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Retorna a picture. Caso nao exista espaco, retira os pontos  ³
//³ separadores de dezenas, centenas e milhares 					  ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
If Empty(cPicture)        
	If cTipoSinal $ "D/C"
		cPicture := TmContab(Abs(nSaldo),nTamanho,nDecimais)
	Else
		cPicture := TmContab(nSaldo,nTamanho,nDecimais)
	EndIf	
	lInformada  := .F.
EndIf

//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³* Alguns valores, apesar de  terem sinal devem ser impressos  ³
//³ sem sinal (lSinal). Ex: valores de colunas Debito e Credito  ³
//³* Se estiver com a opcao de lingua estrangeira (lEstrang) a   ³
//³ picture sera invertida para exibir valores: 999,999,999.99   ³
//³* O tipo de sinal "D" - default nao leva em consideracao a    ³
//³ a natureza da conta. Dessa forma valores negativos serao	  ³
//³ impressos sem sinal, e ao seu lado "D" (Devedor) e valores   ³
//³ positivos terao um "C" (Credito) impresso ao seu lado.       ³
//³* O tipo de Sinal "P" - Parenteses, imprimira valores de saldo³
//³  invertidos da condicao normal da conta entre parenteses.	  ³
//³* O tipo de Sinal "S" - Sinal, imprimira valores de saldo in- ³
//³  vertidos da condicao normal da conta com sinal - 			  ³
//³EXEMPLOS  -  EXEMPLOS  -  EXEMPLOS	-	EXEMPLOS  - EXEMPLOS   ³
//³Cond Normal 	Saldo 	Default      Sinal   Parenteses		  ³
//³	D			   -1000	   1000 D 		 1000		 1000			  	  ³
//³	D				 1000 	1000 C		-1000 	(1000)			  ³
//³	C				-1000 	1000 D		-1000 	(1000)			  ³
//³	C				 1000 	1000 C		 1000 	 1000 			  ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

// So imprime valor se for diferente de zero!
If lDifZero
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Neste caso (Default), nao importa a natureza da conta! Saldos³
	//³ devedores serao impressos com "D" e credores com "C".        ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
	// Neste caso, nao importa a natureza da conta!!
	If cTipoSinal == "D" .Or. cTipoSinal == "C" .Or. cTipoSinal == "N"			// D(Default) ou C(so Credito)
		If !lInformada
			cPicture := "@E " + cPicture
		Endif	         
		If lSinal
			If nSaldo < 0
				If lGraf                                     
					If cTipoSinal == "D"				
						cCharSinal := Iif(cPaisLoc<>"MEX","D","C")
					EndIf
				Else	 
					// No Tipo C -> so sao impressos os "C´s"
					If cTipoSinal == "D"
						cCharSinal := Iif(cPaisLoc<>"MEX","D","C")
					EndIf	
				Endif
			ElseIf nSaldo > 0
				If lGraf                                                                
					If cIdentifi # Nil .And. cIdentifi $ "34"                                                           
						If cTipoSinal == "D"
							cCharSinal := Iif(cPaisLoc<>"MEX","C","A")
						EndIf
					Else
						cCharSinal := Iif(cPaisLoc<>"MEX","C","A")
					Endif
				Else
					cCharSinal := Iif(cPaisLoc<>"MEX","C","A")
				Endif
			EndIf
			cCharSinal := " "+cCharSinal			
		EndIf
		                           
		//Se o parametro MV_TPVALOR == "N" => nao considera a condicao normal da conta. 
		//So imprime sinal (-) se o saldo for credor. 
		If cTipoSinal == "N"
			If lSinal 
				cImpSaldo := Transform(nSaldo*(-1),cPicture)
			Else
				cImpSaldo := Transform(ABS(nSaldo),cPicture)			                                                 
			EndIf
		Else
			cImpSaldo := Transform(Abs(nSaldo),cPicture)+cCharSinal
		EndIf
		
		If lGraf                                                
			If cIdentifi # Nil .And. cIdentifi $ "34"
				If cIdentifi == "3" .And. Type("oCouNew08N") <> "U"
					oPrint:Say(nLin,nCol,cImpSaldo,oCouNew08N)
				Else
					oPrint:Say(nLin,nCol,cImpSaldo,oCouNew08S)
				EndIf
			Else
				oPrint:Say(nLin,nCol,cImpSaldo,oFont08)				
			Endif
		ElseIf lSay
			@ nLin, nCol pSay cImpSaldo 
		Endif
		
	Else
		//Utiliza conceito de conta estourada e a conta eh redutora.
		If Select(cArqDad) > 0 .And. cArqDad->(FieldPos("ESTOUR")) <> 0 .And.  cArqDad->ESTOUR == "1"
			If cTipo == "1" 								// Conta Devedora
				If cTipoSinal == "S"              			// Sinal
					If !lSinal
						nSaldo := Abs(nSaldo)
					EndIf
					If !lInformada
						cPicture := "@E " + cPicture
					EndIf 
					If lGraf
						cImpSaldo := Transform(nSaldo,cPicture)			
						If cIdentifi # Nil .And. cIdentifi $ "34"						
							If cIdentifi == "3" .And. Type("oCouNew08N") <> "U"
								oPrint:Say(nLin,nCol,cImpSaldo,oCouNew08N)
							Else						
								oPrint:Say(nLin,nCol,cImpSaldo,oCouNew08S)
							EndIf
						Else
							oPrint:Say(nLin,nCol,cImpSaldo,oFont08)
						Endif
					ElseIf lSay
						@ nLin, nCol PSAY nSaldo Picture cPicture
					Else
						cImpSaldo := Transform(nSaldo,cPicture)				
					Endif
				ElseIf (cTipoSinal) == "P"              	// Parenteses
					If !lSinal 
						nSaldo := Abs(nSaldo)
					EndIf

					If !lInformada         				
						cPicture := "@E( " + cPicture
					EndIf
					If lGraf
						cImpSaldo := Transform(nSaldo,cPicture)			
						If cIdentifi # Nil .And. cIdentifi $ "34"					
							If cIdentifi == "3" .And. Type("oCouNew08N") <> "U"
								oPrint:Say(nLin,nCol,cImpSaldo,oCouNew08N)
							Else
								oPrint:Say(nLin,nCol,cImpSaldo,oCouNew08S)
							EndIf
						Else
							oPrint:Say(nLin,nCol,cImpSaldo,oFont08)
						Endif
					ElseIf lSay
						@ nLin, nCol pSay nSaldo Picture cPicture
					Else
						cImpSaldo := Transform(nSaldo,cPicture)				
					Endif
				EndIf
			Else
				If (cTipoSinal) == "S"                  	// Sinal
					If lSinal 
						nSaldo := nSaldo * (-1)
//					If !lSinal .And. cTipo == "2" 			// Conta Credora
					Else
						nSaldo := Abs(nSaldo)
					EndIf
					If !lInformada
						cPicture := "@E " + cPicture
					EndIf 
					If lGraf
						cImpSaldo := Transform(nSaldo,cPicture)			
						If cIdentifi # Nil .And. cIdentifi $ "34"
							If cIdentifi == "3" .And. Type("oCouNew08N") <> "U"
								oPrint:Say(nLin,nCol,cImpSaldo,oCouNew08N)
							Else
								oPrint:Say(nLin,nCol,cImpSaldo,oCouNew08S)
							EndIf
						Else
							oPrint:Say(nLin,nCol,cImpSaldo,oFont08)
						Endif
					ElseIf lSay
						@ nLin, nCol PSAY nSaldo Picture cPicture
					Else
						cImpSaldo := Transform(nSaldo,cPicture)				
					Endif
				ElseIf (cTipoSinal) == "P"              // Parenteses
					If lSinal                  
						nSaldo := nSaldo * (-1)					
//					If !lSinal .And. cTipo == "2" 			// Conta Credora
					Else
						nSaldo := Abs(nSaldo)
					EndIf
					If !lInformada
						cPicture := "@E( " + cPicture
					EndIf    
					If lGraf
						cImpSaldo := Transform(nSaldo,cPicture)			// Debito
						If cIdentifi # Nil .And. cIdentifi $ "34"
							If cIdentifi == "3" .And. Type("oCouNew08N") <> "U"
								oPrint:Say(nLin,nCol,cImpSaldo,oCouNew08N)
							Else
								oPrint:Say(nLin,nCol,cImpSaldo,oCouNew08S)
							EndIf
						Else
							oPrint:Say(nLin,nCol,cImpSaldo,oFont08)
						Endif					
					ElseIf lSay
						@ nLin, nCol pSay nSaldo Picture cPicture
					Else
						cImpSaldo := Transform(nSaldo,cPicture)				
					Endif
				EndIf		
			EndIf		
		Else	//Se nao utiliza conceito de conta estourada
			If cTipo == "1" 								// Conta Devedora
				If cTipoSinal == "S"              			// Sinal
					If lSinal
						nSaldo := nSaldo * (-1)
					Else
						nSaldo := Abs(nSaldo)
					EndIf
					If !lInformada
						cPicture := "@E " + cPicture
					EndIf 
					If lGraf
						cImpSaldo := Transform(nSaldo,cPicture)			
						If cIdentifi # Nil .And. cIdentifi $ "34"
							If cIdentifi == "3" .And. Type("oCouNew08N") <> "U"
								oPrint:Say(nLin,nCol,cImpSaldo,oCouNew08N)
							Else
								oPrint:Say(nLin,nCol,cImpSaldo,oCouNew08S)
							EndIf
						Else
							oPrint:Say(nLin,nCol,cImpSaldo,oFont08)
						Endif
					ElseIf lSay
						@ nLin, nCol PSAY nSaldo Picture cPicture
					Else
						cImpSaldo := Transform(nSaldo,cPicture)			
					Endif
				ElseIf (cTipoSinal) == "P"              	// Parenteses
					If lSinal 
						nSaldo := nSaldo * (-1) 		  		// a Picture so exibe parenteses para numeros negativos
					Else
						nSaldo := Abs(nSaldo)
					EndIf
        	
					If !lInformada         				
						cPicture := "@E( " + cPicture
					EndIf
					If lGraf
						cImpSaldo := Transform(nSaldo,cPicture)			
						If cIdentifi # Nil .And. cIdentifi $ "34"
							If cIdentifi == "3" .And. Type("oCouNew08N") <> "U"
								oPrint:Say(nLin,nCol,cImpSaldo,oCouNew08N)
							Else
								oPrint:Say(nLin,nCol,cImpSaldo,oCouNew08S)
							EndIf
						Else
							oPrint:Say(nLin,nCol,cImpSaldo,oFont08)
						Endif
					ElseIf lSay
						@ nLin, nCol pSay nSaldo Picture cPicture
					Else
						cImpSaldo := AllTrim(Transform(nSaldo,cPicture))
					Endif
				EndIf
			Else
				If (cTipoSinal) == "S"                  	// Sinal
					If !lSinal .And. cTipo == "2" 			// Conta Credora
						nSaldo := Abs(nSaldo)
					EndIf
					If !lInformada
						cPicture := "@E " + cPicture
					EndIf 
					If lGraf
						cImpSaldo := Transform(nSaldo,cPicture)			
						If cIdentifi # Nil .And. cIdentifi $ "34"
							If cIdentifi == "3" .And. Type("oCouNew08N") <> "U"
								oPrint:Say(nLin,nCol,cImpSaldo,oCouNew08N)
							Else
								oPrint:Say(nLin,nCol,cImpSaldo,oCouNew08S)
							EndIf
						Else
							oPrint:Say(nLin,nCol,cImpSaldo,oFont08)
						Endif
					ElseIf lSay
						@ nLin, nCol PSAY nSaldo Picture cPicture
					Else
						cImpSaldo := Transform(nSaldo,cPicture)				
					Endif
				ElseIf (cTipoSinal) == "P"              // Parenteses
					If !lSinal .And. cTipo == "2" 			// Conta Credora
						nSaldo := Abs(nSaldo)
					EndIf
					If !lInformada
						cPicture := "@E( " + cPicture
					EndIf    
					If lGraf
						cImpSaldo := Transform(nSaldo,cPicture)			// Debito
						If cIdentifi # Nil .And. cIdentifi $ "34"
							If cIdentifi == "3" .And. Type("oCouNew08N") <> "U"
								oPrint:Say(nLin,nCol,cImpSaldo,oCouNew08N)
							Else
								oPrint:Say(nLin,nCol,cImpSaldo,oCouNew08S)
							EndIf
						Else
							oPrint:Say(nLin,nCol,cImpSaldo,oFont08)
						Endif					
					ElseIf lSay
						@ nLin, nCol pSay nSaldo Picture cPicture
					Else
						cImpSaldo := Transform(nSaldo,cPicture)				
					Endif
				EndIf
			EndIf
		EndIf
	EndIf
EndIf
RestArea(aSaveArea)
         
If lSay
	Return
Else
	If Empty( cImpSaldo )
		If lPrintZero
			cImpSaldo := Transform(nSaldo,cPicture)   
		EndIf
	EndIf
	Return cImpSaldo
EndIf


Static Function CopDados(cArqAux,cArqTmp)

If Select(cArqAux) > 0
	(cArqAux)->(dbGoTop())
	Do While !(cArqAux)->(Eof())
		if (cArqTmp)->(DBSeek((cArqAux)->CONTA))
			RecLock(cArqTmp,.f.) 
			Replace (cArqTmp)->SALDOANT 	with (cArqTmp)->SALDOANT + (cArqAux)->SALDOANT  
			Replace (cArqTmp)->SALDOANTDB 	with (cArqTmp)->SALDOANTDB + (cArqAux)->SALDOANTDB
			Replace (cArqTmp)->SALDOANTCR 	with (cArqTmp)->SALDOANTCR + (cArqAux)->SALDOANTCR
			Replace (cArqTmp)->SALDODEB		with (cArqTmp)->SALDODEB + (cArqAux)->SALDODEB
			Replace (cArqTmp)->SALDOCRD		with (cArqTmp)->SALDOCRD + (cArqAux)->SALDOCRD
			Replace (cArqTmp)->SALDOATU		with (cArqTmp)->SALDOATU + (cArqAux)->SALDOATU
			Replace (cArqTmp)->SALDOATUDB	with (cArqTmp)->SALDOATUDB + (cArqAux)->SALDOATUDB
			Replace (cArqTmp)->SALDOATUCR	with (cArqTmp)->SALDOATUCR + (cArqAux)->SALDOATUCR
			Replace (cArqTmp)->MOVIMENTO	with (cArqTmp)->MOVIMENTO + (cArqAux)->MOVIMENTO
			(cArqTmp)->(MsUnlock())
		Else
			RecLock(cArqTmp,.t.) 
			Replace (cArqTmp)->CONTA		with (cArqAux)->CONTA
			Replace (cArqTmp)->SUPERIOR 	with (cArqAux)->SUPERIOR
			Replace (cArqTmp)->NORMAL		with (cArqAux)->NORMAL
			Replace (cArqTmp)->CTARES		with (cArqAux)->CTARES
			Replace (cArqTmp)->DESCCTA	with (cArqAux)->DESCCTA
			Replace (cArqTmp)->CUSTO		with (cArqAux)->CUSTO
			Replace (cArqTmp)->CCRES		with (cArqAux)->CCRES
			Replace (cArqTmp)->DESCCC		with (cArqAux)->DESCCC
			Replace (cArqTmp)->ITEM		with (cArqAux)->ITEM
			Replace (cArqTmp)->ITEMRES	with (cArqAux)->ITEMRES
			Replace (cArqTmp)->DESCITEM 	with (cArqAux)->DESCITEM
			Replace (cArqTmp)->CLVL		with (cArqAux)->CLVL
			Replace (cArqTmp)->CLVLRES	with (cArqAux)->CLVLRES
			Replace (cArqTmp)->DESCCLVL 	with (cArqAux)->DESCCLVL			
			Replace (cArqTmp)->SALDOANT 	with (cArqAux)->SALDOANT  
			Replace (cArqTmp)->SALDOANTDB with (cArqAux)->SALDOANTDB
			Replace (cArqTmp)->SALDOANTCR with (cArqAux)->SALDOANTCR
			Replace (cArqTmp)->SALDODEB	with (cArqAux)->SALDODEB
			Replace (cArqTmp)->SALDOCRD	with (cArqAux)->SALDOCRD
			Replace (cArqTmp)->SALDOATU	with (cArqAux)->SALDOATU
			Replace (cArqTmp)->SALDOATUDB	with (cArqAux)->SALDOATUDB
			Replace (cArqTmp)->SALDOATUCR	with (cArqAux)->SALDOATUCR
			Replace (cArqTmp)->MOVIMENTO	with (cArqAux)->MOVIMENTO
			Replace (cArqTmp)->TIPOCONTA	with (cArqAux)->TIPOCONTA
			Replace (cArqTmp)->TIPOCC		with (cArqAux)->TIPOCC
			Replace (cArqTmp)->TIPOITEM	with (cArqAux)->TIPOITEM
			Replace (cArqTmp)->TIPOCLVL	with (cArqAux)->TIPOCLVL
			Replace (cArqTmp)->CCSUP		with (cArqAux)->CCSUP
			Replace (cArqTmp)->ITSUP		with (cArqAux)->ITSUP
			Replace (cArqTmp)->CLSUP		with (cArqAux)->CLSUP
			Replace (cArqTmp)->ORDEM		with (cArqAux)->ORDEM
			Replace (cArqTmp)->GRUPO		with (cArqAux)->GRUPO
			Replace (cArqTmp)->IDENTIFI	with (cArqAux)->IDENTIFI
			Replace (cArqTmp)->TOTVIS		with (cArqAux)->TOTVIS
			Replace (cArqTmp)->SLDENT		with (cArqAux)->SLDENT			
			Replace (cArqTmp)->FATSLD		with (cArqAux)->FATSLD
			Replace (cArqTmp)->VISENT		with (cArqAux)->VISENT
			Replace (cArqTmp)->ESTOUR		with (cArqAux)->ESTOUR
			Replace (cArqTmp)->NIVEL1		with (cArqAux)->NIVEL1
			Replace (cArqTmp)->NATCTA		with (cArqAux)->NATCTA
			Replace (cArqTmp)->FILIAL		with (cArqAux)->FILIAL
			Replace (cArqTmp)->COLUNA		with (cArqAux)->COLUNA
			(cArqTmp)->(MsUnlock())				
		Endif
		(cArqAux)->(DBSkip())
	EndDo
	
Endif	                                                       

Return

Static Function CriaArqDad(cArqTmp,cArqDad,cArqAux,cArqInd)
	
	If Empty(cArqDad)
		
		aStru 		:= (cArqTmp)->(DbStruct())
		cArqDad 	:= CriaTrab(aStru, .T.)
		dbUseArea( .T.,, cArqDad, cArqAux, .F., .F. )
			
		cChave		:= "CONTA"
		IndRegua(cArqAux,cArqInd,cChave,,,OemToAnsi("Selecionando Registros..."))
		dbSelectArea(cArqAux)
		DbClearIndex()
		dbSetIndex(cArqInd+OrdBagExt())			
	Endif		

Return


/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÚÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿±±
±±³ Fun‡…o	  ³ FEImpQuad³ Autor ³ Lucimara Soares	     ³ Data ³ 29/11/02 ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Descri‡…o ³ Impressao dos Quadros finais do balancete                  ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Sintaxe   ³ FEImpQuad()                                                ³±±
±±ÃÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´±±
±±³ Uso	      ³ SIGACTB 																															³±±
±±ÀÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/
Static Function FEImpQuad(Tamanho,lDigVer,dData,cMoeda,aQuadro,cDescMoeda,nomeprog,dDataLP,cPicture,nDecimais,lPrintZero,cTpSald,oReport)

Local cData 	:=	StrZero(Month(dData),2) +"/" + Str(Year(dData),4)
Local nTotMov	:=	0
Local nResult	:=	0
Local nPtmCorr := 0
Local	nPtmLiq  := 0
Local nReceita := 0
Local nDespesa := 0
Local nVal1		:= 0
Local nVal2		:= 0
Local lReport  := IIF(oReport == NIL,.F.,.T.)

#DEFINE RCOL_01		30
#DEFINE RCOL_02		800
#DEFINE RCOL_03		1600
#DEFINE RCOL_TIT		300
#DEFINE RCOL_SBTIT		100

If !lReport 

	Cabec1 := OemToAnsi("QUADROS DEMONSTRATIVOS") // "QUADROS DEMONSTRATIVOS"
	Cabec2 := " "
	
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Quadro 1: Totais do Ativo / Passivo.                         ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ		
//	Cabec( Titulo, Cabec1, Cabec2, NomeProg, Tamanho, 15 )
	CtCGCCabec(,,,Cabec1,Cabec2,dData,Titulo,,"2",Tamanho)
	
	li++           
	
	@li,00 PSAY "+" + Repl( "-", 100 ) + "+"
	li++
	@li,00 PSAY "|" + Repl( " ", 100 ) + "|"
	li++
	@li,00 PSAY "|" + PadC( OemToAnsi("TOTAIS DO ATIVO / PASSIVO") , 100, " " ) + "|" // 
	li++
	@li,00 PSAY "|" + Repl( " ", 100 ) + "|"
	li++   
	@li,00 PSAY "+" + Repl( "-", 100 ) + "|"
	li++
	@li,00 PSAY "|" + Repl( " ", 100 ) + "|"
	li++
	
	DbSelectArea("CT1")
	dbSeek( cFilial + aQuadro[1] ) // Totalizadora do Ativo - mv _par08
	@li,00 PSAY "|"
	If lDigVer 
		@li,02 PSAY Mascara(Alltrim(aQuadro[1])+CT1->CT1_DC)
	Else
		@li,02 PSAY Mascara(CT1->CT1_CONTA)
	EndIf	
	
	@li,035 PSAY OemToAnsi("Total do Ativo") // 
	
	dbSelectArea("cArqDad")
	dbSetOrder(1)
	If dbSeek(aQuadro[1])
		nTotMov	:= SALDOATU
		ValorCTB(nTotMov,li,80,17,nDecimais,.T.,cPicture,"1", , , , , ,lPrintZero)
	EndIf
	@li,101 PSAY "|"
	
	li++
	@li,00 PSAY "|" + Repl( " ", 100 ) + "|"
	li++
	
	DbSelectArea("CT1")
	dbSeek( cFilial + aQuadro[2]) // Totalizadora do Passivo - mv_par09
	@li,00 PSAY "|"
	If lDigVer 
		@li,02 PSAY Mascara(Alltrim(CT1->CT1_CONTA)+CT1->CT1_DC)
	Else
		@li,02 PSAY Mascara(CT1->CT1_CONTA)
	EndIf
	
	@li,35 PSAY OemToAnsi("Total do Passivo") // 
	dbSelectArea("cArqDad")
	dbSetOrder(1)
	If dbSeek(aQuadro[2])
		nTotMov	:= SALDOATU
		ValorCTB(nTotMov,li,80,17,nDecimais,.T.,cPicture,"2", , , , , ,lPrintZero)
	EndIf	
	@li,101 PSAY "|"
	
	li++
	@li,00 PSAY "|" + Repl( " ", 100 ) + "|"
	li++
	@li,00 PSAY "+" + Repl( "-", 100 ) + "+"
	
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Quadro 2: Demonstracao de resultados                         ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ		
	li += 4
	
	@li,00 PSAY "+" + Repl( "-", 100 ) + "+"
	li++
	@li,00 PSAY "|" + Repl( " ", 100 ) + "|"
	li++
	@li,00 PSAY "|" + PadC( OemToAnsi("DEMONSTRATIVO DE RESULTADOS - MENSAL") , 100, " " ) + "|" // 
	li++
	@li,00 PSAY "|" + Repl( " ", 100 ) + "|"
	li++ 
	@li,00 PSAY "+" + Repl( "-", 100 ) + "|"
	li++
	@li,00 PSAY "|" + Repl( " ", 100 ) + "|"
	li++
	
	DbSelectArea("CT1")
	dbSeek( cFilial + aQuadro[3] ) // Resultado a debito
	
	@li,00 PSAY "|"
	If lDigVer 
		@li,02 PSAY Mascara(Alltrim(CT1->CT1_CONTA)+CT1->CT1_DC)
	Else
		@li,02 PSAY Mascara(CT1->CT1_CONTA)
	EndIf	
	@li,35 PSAY OemToAnsi("CONTAS DE RESULTADO ") // 
	DbSelectArea("cArqDad")
	DbSetOrder(1)
	If DbSeek(aQuadro[3])
		nTotMov	:= MOVIMENTO
		nVal1 	:= SALDOATU
		ValorCTB(nTotMov,li,80,17,nDecimais,.T.,cPicture,"1", , , , , ,lPrintZero)
	Endif	
	nResult := nTotMov
	
	@li,101 PSAY "|"
	
	li++
	@li,00 PSAY "|" + Repl( " ", 100 ) + "|"
	li++
	
	DbSelectArea("CT1")
	dbSeek( cFilial + aQuadro[4] ) // Resultado a credito
	
	@li,00 PSAY "|"
	If lDigVer 
		@li,02 PSAY Mascara(Alltrim(CT1->CT1_CONTA)+CT1->CT1_DC)
	Else
		@li,02 PSAY Mascara(CT1->CT1_CONTA)
	EndIf	
	@li,35 PSAY OemToAnsi("CONTAS DE RESULTADO ") // 
	DbSelectArea("cArqDad")
	DbSetOrder(1)
	If DbSeek(aQuadro[4])
		nTotMov	:= MOVIMENTO
		nVal2	:= SALDOATU
		ValorCTB(nTotMov,li,80,17,nDecimais,.T.,cPicture,"2", , , , , ,lPrintZero)
	Else
		nTotMov	:= 0 
	Endif
	nResult	+= nTotMov
	
	DbSelectArea("CT1")
	@li,101 PSAY "|"
	
	li++
	@li,00 PSAY "|" + Repl( " ", 100 ) + "|"
	li++
	@li,00 PSAY "|" + Repl( " ", 100 ) + "|"
	li++
	
	@li,00 PSAY "|"
	@li,02 PSAY OemToAnsi("RESULTADO DO MES ") + cData // 
	
	ValorCTB(nResult,li,80,17,nDecimais,.T.,cPicture,"1", , , , , ,lPrintZero)
	@li,101 PSAY "|"
	
	li++
	@li,00 PSAY "|" + Repl( " ", 100 ) + "|"
	li++
	
	@li,00 PSAY "|"
	@li,02 PSAY OemToAnsi("RESULTADO ACUMULADO ATE ") + cData  // 
	nTotMov	:= nVal1+nVal2
	ValorCTB(nTotMov,li,80,17,nDecimais,.T.,cPicture,"1", , , , , ,lPrintZero)
	@li,101 PSAY "|"
	
	li++
	@li,00 PSAY "|" + Repl( " ", 100 ) + "|"
	li++
	@li,00 PSAY "+" + Repl( "-", 100 ) + "+"
	
	
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Quadro 3: Demonstracao de Patrimonio Liquido destacado       ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ		
//	Cabec( Titulo, Cabec1, Cabec2, NomeProg, Tamanho, 15 )
	CtCGCCabec(,,,Cabec1,Cabec2,dData,Titulo,,"2",Tamanho)
	
	@li,00 PSAY "+" + Repl( "-", 100 ) + "+"
	li++
	@li,00 PSAY "|" + Repl( " ", 100 ) + "|"
	li++
	@li,00 PSAY "|" + ;
			 PadC( OemToAnsi("DEMONSTRACAO DO PATRIMONIO LIQUIDO DESTACADO"), 100, " " ) + "|" // 
	
	li++		 
	@li,00 PSAY "|" + PadC( OemToAnsi("PARA O MES ") + cData, 100, " " ) + "|" // 
	
	li++
	@li,00 PSAY "|" + Repl( " ", 100 ) + "|"
	li++
	@li,00 PSAY "+" + Repl( "-", 100 ) + "|"
	li++
	@li,00 PSAY "|" + Repl( " ", 100 ) + "|"
	li++
	
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Procura a conta de patrimonio Liquido.                       ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ		
	dbSeek( cFilial + aQuadro[5] )
	
	@ li,00 PSAY OemToAnsi("| PATRIMONIO LIQUIDO") // 
	DbSelectArea("cArqDad")
	DbSetOrder(1)
	If DbSeek(aQuadro[5])
		nPtmLiq := SALDOATU
		ValorCTB(nPtmLiq,li,80,17,nDecimais,.T.,cPicture,"2", , , , , ,lPrintZero)
	Endif	               
	DbSelectArea("CT1")
	@ li,101 PSAY "|"
	
	li++
	@li,00 PSAY "|" + Repl( " ", 100 ) + "|"
	li++
	
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Procura a conta de receita.                                  ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ		
	dbSeek( cFilial + aQuadro[6] )
	
	@li,00 PSAY OemToAnsi("| TOTAL DA RECEITA") // 
	DbSelectArea("cArqDad")
	DbSetOrder(1)
	If DbSeek(aQuadro[6])
	   nReceita := SALDOATU
		ValorCTB(nReceita,li,80,17,nDecimais,.T.,cPicture,"2", , , , , ,lPrintZero)
	Endif
	DbSelectArea("CT1")	
	@li,101 PSAY "|"
	
	li++
	@li,00 PSAY "|" + Repl( " ", 100 ) + "|"
	li++
	
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Procura a conta de despesa.                                  ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ		
	dbSeek( cFilial + aQuadro[7] )
	
	@li,00 PSAY OemToAnsi("| TOTAL DA DESPESA") // 
	DbselectArea("cArqDad")
	DbSetOrder(1)
	If DbSeek(aQuadro[7])
		nDespesa := SALDOATU
		ValorCTB(nDespesa,li,80,17,nDecimais,.T.,cPicture,"1", , , , , ,lPrintZero)
	Endif
	DbSelectArea("CT1")	
	@li,101 PSAY "|"
	
	li++
	@li,00 PSAY "|" + Repl( " ", 100 ) + "|"
	li++
	
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Calcula o patrimonio liquido corrigido.                      ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ		
	nPtmCorr := nPtmLiq + nReceita + nDespesa
	@li,00 PSAY OemToAnsi("| PATRIMONIO LIQUIDO CORRIGIDO ")+cData //  + cData
	ValorCTB(nPtmCorr,li,80,17,nDecimais,.T.,cPicture,"2", , , , , ,lPrintZero)
	@li,101 PSAY "|"
	
	li++
	@li,00 PSAY "|" + Repl( " ", 100 ) + "|"
	li++
	
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Calcula Lancamento de Correcao		                          ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ		
	nLctoCorr := nReceita + nDespesa
	@li,00 PSAY OemToAnsi("| LANCAMENTO DE CORRECAO") // 
	ValorCTB(nLctoCorr,li,80,17,nDecimais,.T.,cPicture,"2", , , , , ,lPrintZero)
	@li,101 PSAY "|"
	
	li++
	@li,00 PSAY "|" + Repl( " ", 100 ) + "|"
	li++
	@li,00 PSAY "|" + Repl( "-", 100 ) + "|"
	li++

Else
//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³Inicio da impressao dos quadros para relatorios relese 4.                                           ³
//³                                                                                                    ³
//³Deve-se passar o objeto oReport para utilizar esta função.                                          ³
//³                                                                                                    ³
//³Não esquecer de desabilitar a impressão do Header da Section principal antes de imprimir os quadros.³
//³                                                                                                    ³
//³Exemplo da chamada da função que esta no fonte CTBR060():                                           ³
//³                                                                                                    ³
//³If mv_par25 ==1                                                                                     ³
//³     oReport:Section(1):SetHeaderSection(.F.)                                                       ³
//³     FEImpQuad(,,,,,,oReport:ClassName(),,,,,,oReport)                                              ³
//³EndIf	                                                                                              ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

	oReport:EndPage(.T.) // Quebra direto

	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Quadro 1: Totais do Ativo / Passivo.                         ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ		
	oReport:SkipLine() 

	oReport:PrintText(OemToAnsi("QUADROS DEMONSTRATIVOS"),,RCOL_TIT) 	// 

	oReport:SkipLine() 
	oReport:ThinLine()    
	oReport:SkipLine() 

	oReport:PrintText(OemToAnsi("TOTAIS DO ATIVO / PASSIVO"),,RCOL_SBTIT)// 
	
	oReport:SkipLine() 
	oReport:ThinLine()    
	oReport:SkipLine() 

	DbSelectArea("CT1")
	dbSeek( cFilial + aQuadro[1] ) // Totalizadora do Ativo - mv _par08
	
	oReport:PrintText(IIF(lDigVer,Mascara(Alltrim(aQuadro[1])+CT1->CT1_DC),Mascara(CT1->CT1_CONTA)),oReport:Row(),RCOL_01)

	oReport:PrintText(OemToAnsi("Total do Ativo"),oReport:Row(),RCOL_02) 	// 
	
	dbSelectArea("cArqDad")
	dbSetOrder(1)
	If dbSeek(aQuadro[1])
		nTotMov	:= cArqDad->SALDOATU
		oReport:PrintText(ValorCTB(nTotMov,,,17,2,.T.,cPicture,"1",,,,,,Iif(mv_par25==1,.T.,.F.),.F.),oReport:Row(),RCOL_03)
	EndIf

	oReport:SkipLine() 
	oReport:SkipLine() 
	
	DbSelectArea("CT1")
	dbSeek( cFilial + aQuadro[2]) // Totalizadora do Passivo - mv_par09
	oReport:PrintText(IIF (lDigVer,Mascara(Alltrim(CT1->CT1_CONTA)+CT1->CT1_DC),Mascara(CT1->CT1_CONTA)),oReport:Row(),RCOL_01)

	oReport:PrintText(OemToAnsi("Total do Passivo"),oReport:Row(),RCOL_02 )// 

	dbSelectArea("cArqDad")
	dbSetOrder(1)
	If dbSeek(aQuadro[2])
		nTotMov	:= cArqDad->SALDOATU
		oReport:PrintText(ValorCTB(nTotMov,,,17,2,.T.,cPicture,"2",,,,,,Iif(mv_par25==1,.T.,.F.),.F.),oReport:Row(),RCOL_03)
	EndIf	

	oReport:SkipLine() 
	oReport:SkipLine() 

	oReport:ThinLine()    

	oReport:SkipLine() 
	oReport:SkipLine() 
	oReport:SkipLine() 
	oReport:SkipLine() 

	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Quadro 2: Demonstracao de resultados                         ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ		

	oReport:SkipLine() 
	oReport:ThinLine()    
	oReport:SkipLine() 

	oReport:PrintText(OemToAnsi("DEMONSTRATIVO DE RESULTADOS - MENSAL"),,RCOL_SBTIT)// 

	oReport:SkipLine() 
	oReport:ThinLine()    
	oReport:SkipLine() 
	
	DbSelectArea("CT1")
	dbSeek( cFilial + aQuadro[3] ) // Resultado a debito

	oReport:PrintText(IIF(lDigVer,Mascara(Alltrim(CT1->CT1_CONTA)+CT1->CT1_DC),Mascara(CT1->CT1_CONTA)),oReport:Row(),RCOL_01 )
	
	oReport:PrintText(OemToAnsi("CONTAS DE RESULTADO "),oReport:Row(),RCOL_02  ) // 

	DbSelectArea("cArqDad")
	DbSetOrder(1)
	If DbSeek(aQuadro[3])
		nTotMov	:= cArqDad->MOVIMENTO
		nVal1 	:= cArqDad->SALDOATU
		oReport:PrintText(ValorCTB(nTotMov,,,17,2,.T.,cPicture,"1",,,,,,Iif(mv_par25==1,.T.,.F.),.F.),oReport:Row(),RCOL_03)
	Endif	
	nResult := nTotMov

	oReport:SkipLine() 
	oReport:SkipLine() 

	DbSelectArea("CT1")
	dbSeek( cFilial + aQuadro[4] ) // Resultado a credito

	oReport:PrintText(IIF(lDigVer,Mascara(Alltrim(CT1->CT1_CONTA)+CT1->CT1_DC),Mascara(CT1->CT1_CONTA)),oReport:Row(), 30 )
	
	oReport:PrintText(OemToAnsi("CONTAS DE RESULTADO"),oReport:Row(),RCOL_02  ) // "CONTAS DE RESULTADO - "

	DbSelectArea("cArqDad")
	DbSetOrder(1)
	If DbSeek(aQuadro[4])
		nTotMov	:= cArqDad->MOVIMENTO
		nVal2	:= cArqDad->SALDOATU
		oReport:PrintText(ValorCTB(nTotMov,,,17,2,.T.,cPicture,"2",,,,,,Iif(mv_par25==1,.T.,.F.),.F.),oReport:Row(),RCOL_03)
	Else
		nTotMov	:= 0 
	Endif
	nResult	+= nTotMov

	oReport:SkipLine() 
	oReport:SkipLine() 

	DbSelectArea("CT1")

	oReport:PrintText(OemToAnsi("RESULTADO DO MES ") + cData,oReport:Row(),RCOL_01) // 
	
	oReport:PrintText(ValorCTB(nResult,,,17,2,.T.,cPicture,"1",,,,,,Iif(mv_par25==1,.T.,.F.),.F.),oReport:Row(),RCOL_03)

	oReport:SkipLine() 
	oReport:SkipLine() 

	oReport:PrintText(OemToAnsi("RESULTADO ACUMULADO ATE ") + cData,oReport:Row(),RCOL_01) // 

	nTotMov	:= nVal1+nVal2
	oReport:PrintText(ValorCTB(nTotMov,,,17,2,.T.,cPicture,"1",,,,,,Iif(mv_par25==1,.T.,.F.),.F.),oReport:Row(),RCOL_03)

	oReport:SkipLine() 
	oReport:SkipLine() 

	oReport:ThinLine()    

	oReport:SkipLine() 
	oReport:SkipLine() 
	oReport:SkipLine() 
	oReport:SkipLine() 

	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Quadro 3: Demonstracao de Patrimonio Liquido destacado       ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ		

	oReport:SkipLine() 
	oReport:ThinLine()    
	oReport:SkipLine() 
	
	oReport:PrintText(OemToAnsi("DEMONSTRACAO DO PATRIMONIO LIQUIDO DESTACADO"),,RCOL_SBTIT)// 
	oReport:PrintText(OemToAnsi("PARA O MES ") + cData,,RCOL_SBTIT)// 
	
	oReport:SkipLine() 
	oReport:ThinLine()    
	oReport:SkipLine() 

	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Procura a conta de patrimonio Liquido.                       ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ		
	dbSeek( cFilial + aQuadro[5] )

	oReport:PrintText(OemToAnsi(SUBS("| PATRIMONIO LIQUIDO",3)),oReport:Row(),RCOL_01)// 

	DbSelectArea("cArqDad")
	DbSetOrder(1)
	If DbSeek(aQuadro[5])
		nPtmLiq := cArqDad->SALDOATU
		oReport:PrintText(ValorCTB(nPtmLiq,,,17,2,.T.,cPicture,"2",,,,,,Iif(mv_par25==1,.T.,.F.),.F.),oReport:Row(),RCOL_03)
	Endif	               
	DbSelectArea("CT1")

	oReport:SkipLine() 
	oReport:SkipLine() 

	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Procura a conta de receita.                                  ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ		
	dbSeek( cFilial + aQuadro[6] )
	
	oReport:PrintText(OemToAnsi(SUBS("| TOTAL DA RECEITA",3)),oReport:Row(),RCOL_01)// "| TOTAL DA RECEITA"

	DbSelectArea("cArqDad")
	DbSetOrder(1)
	If DbSeek(aQuadro[6])
		nReceita := cArqDad->SALDOATU
		oReport:PrintText(ValorCTB(nReceita,,,17,2,.T.,cPicture,"2",,,,,,Iif(mv_par25==1,.T.,.F.),.F.),oReport:Row(),RCOL_03)
	Endif
	DbSelectArea("CT1")	

	oReport:SkipLine() 
	oReport:SkipLine() 
	
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Procura a conta de despesa.                                  ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ		
	dbSeek( cFilial + aQuadro[7] )
	
	oReport:PrintText(OemToAnsi(SUBS("| TOTAL DA DESPESA",3)),oReport:Row(),RCOL_01)// "| TOTAL DA DESPESA"

	DbselectArea("cArqDad")
	DbSetOrder(1)
	If DbSeek(aQuadro[7])
		nDespesa := cArqDad->SALDOATU
		oReport:PrintText(ValorCTB(nDespesa,,,17,2,.T.,cPicture,"1",,,,,,Iif(mv_par25==1,.T.,.F.),.F.),oReport:Row(),RCOL_03)
	Endif
	DbSelectArea("CT1")	

	oReport:SkipLine() 
	oReport:SkipLine() 

	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Calcula o patrimonio liquido corrigido.                      ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ		
	nPtmCorr := nPtmLiq + nReceita + nDespesa

	oReport:PrintText(OemToAnsi(SUBS("| PATRIMONIO LIQUIDO CORRIGIDO ",3))+cData,oReport:Row(),RCOL_01)// "| PATRIMONIO LIQUIDO CORRIGIDO " + cData

	oReport:PrintText(ValorCTB(nPtmCorr,,,17,2,.T.,cPicture,"2",,,,,,Iif(mv_par25==1,.T.,.F.),.F.),oReport:Row(),RCOL_03)

	oReport:SkipLine() 
	oReport:SkipLine() 
	
	//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
	//³ Calcula Lancamento de Correcao		                          ³
	//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ		
	nLctoCorr := nReceita + nDespesa

	oReport:PrintText(OemToAnsi(SUBS("| LANCAMENTO DE CORRECAO",3)),oReport:Row(),RCOL_01)// 

	oReport:PrintText(ValorCTB(nLctoCorr,,,17,2,.T.,cPicture,"2",,,,,,Iif(mv_par25==1,.T.,.F.),.F.),oReport:Row(),RCOL_03)

	oReport:SkipLine() 
	oReport:ThinLine()    
	oReport:SkipLine() 

EndIf
                        
Return
