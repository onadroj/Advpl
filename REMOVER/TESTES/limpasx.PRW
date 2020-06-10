#INCLUDE "rwmake.ch"

/*/
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³NOVO51    º Autor ³ AP6 IDE            º Data ³  13/06/14   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDescricao ³ Codigo gerado pelo AP6 IDE.                                º±±
±±º          ³                                                            º±±                       

±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP6 IDE                                                    º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
/*/

User Function limpasx


//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
//³ Declaracao de Variaveis                                             ³
//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ

Local aCampo 
Local aOrdem
Local i
/*
aCampo := {"B1_CC","C1_CC","C2_CC","C7_CC","D1_CC","E1_CUSTO","E2_CUSTO",;
"E5_CUSTO","E5_CCD","E5_CCC","H1_CCUSTO","H9_CCUSTO","HB_CC","I2_CCD",   ;
"I2_CCC","I3_CCUNID","I9_CCD","I9_CCC","IA_CC","IB_CCC","IB_CCD",        ;
"IC_CCD","IC_CCC","N3_CUSTBEM","N3_CCUSTO","O3_CATIV","OC_CC","P0_CC",   ;
"P2_CC","PB_CC","R1_CC","RC_CC","RE_CCD","RE_CCP","RI_CC","RK_CC",       ;
"RZ_CC","Z8_CUSTO","Z9_CUSTO","ZD_CCD","ZD_CCC","CNB_CC"}
*/
aCampo := {"RA_FILIAL","RA_BITMAP","RA_MAT","RA_NOME","RA_NOMECMP","RA_MAE","RA_PAI",												;												
"RA_SEXO","RA_RACACOR","RA_NASC","RA_ALTNASC","RA_ESTCIVI","RA_CPAISOR","RA_PAISORI",												;
"RA_NACIONA","RA_NACIONC","RA_NACIONN","RA_BRNASEX","RA_NATURAL","RA_CODMUNN","RA_MUNNASC",											;
"RA_APELIDO","RA_GRINRAI","RA_DESCGIN","RA_EMAIL","RA_EMAIL2","RA_RECMAIL","RA_TPMAIL","RA_DEFIFIS",								;
"RA_BRPDH","RA_TPDEFFI","RA_PORTDEF","RA_OBSDEFI","RA_CC","RA_DESCCC","RA_CLVL","RA_ITEM",											;
"RA_ADMISSA","RA_ALTADM","RA_TIPOADM","RA_DEPIR","RA_DEPSF","RA_DEMISSA","RA_OPCAO","RA_ALTOPC",									;
"RA_BCDPFGT","RA_CTDPFGT","RA_CHAPA","RA_TNOTRAB","RA_LOCBNF","RA_PERFGTS","RA_DESCTUR","RA_BCDEPSA",								;
"RA_TPCTSAL","RA_CTDEPSA","RA_TPPREVI","RA_SITFOLH","RA_HRSMES","RA_HRSEMAN","RA_HRSDIA",											;
"RA_CODFUNC","RA_DESCFUN","RA_SALARIO","RA_ANTEAUM","RA_PGCTSIN","RA_ADCPERI","RA_TIPOALT",											;
"RA_DATAALT","RA_CESTAB","RA_TPCONTR","RA_DTFIMCT","RA_VALEREF","RA_PROCES","RA_HOPARC",											;
"RA_VALEALI","RA_SEGUROV","RA_CLAURES","RA_PERCADT","RA_PENSALI","RA_SINDICA","RA_DESCSIN","RA_CBO",								;
"RA_CODCBO","RA_ALTCBO","RA_TIPOPGT","RA_CATFUNC","RA_VIEMRAI","RA_CATEG","RA_CATEFD","RA_PERICUL",									;
"RA_VCTOEXP","RA_VCTEXP2","RA_INSMIN","RA_INSMED","RA_EXAMEDI","RA_DTVTEST","RA_ADCINS","RA_AFASFGT",								;
"RA_ASSIST","RA_CONFED","RA_MENSIND","RA_RESCRAI","RA_MESESAN","RA_MESTRAB","RA_FTINSAL","RA_CLASSEC",								;
"RA_OCORREN","RA_PERCSAT","RA_CARGO","RA_DCARGO","RA_CODTIT","RA_POSTO","RA_ALTNOME","RA_DEPTO",									;
"RA_CODRET","RA_DDEPTO","RA_CRACHA","RA_FECREI","RA_REGRA","RA_DEMIANT","RA_MOLEST","RA_COMPSAB",									;
"RA_EAPOSEN","RA_NJUD14","RA_SEQTURN","RA_TPREINT","RA_SENHA","RA_NRPROC","RA_NRLEIAN","RA_DTEFRET",								;
"RA_DTEFRTN","RA_NIVEL","RA_CIC","RA_PIS","RA_TPRCBT","RA_ALTPIS","RA_TCFMSG","RA_RG","RA_DTRGEXP","RA_INSSSC",						;
"RA_RGUF","RA_RGORG","RA_RGEXP","RA_DISTSN","RA_ORGEMRG","RA_COMPLRG","RA_NUMCP","RA_BHFOL","RA_SERCP","RA_UFCP",					;
"RA_ACUMBH","RA_OKTRANS","RA_DTCPEXP","RA_TABELA","RA_ALTCP","RA_HABILIT","RA_TABNIVE","RA_TABFAIX",								;
"RA_CNHORG","RA_DTEMCNH","RA_RECPFNC","RA_DTVCCNH","RA_RESERVI","RA_TIPENDE","RA_TITULOE","RA_RESEXT","RA_ZONASEC","RA_PAISEXT",	;
"RA_SECAO","RA_PAISEXN","RA_MEMOTCF","RA_REGISTR","RA_LOGRTP","RA_LOGRTPD","RA_FICHA","RA_SERVENT",									;
"RA_LOGRDSC","RA_LOGRNUM","RA_CODACER","RA_ENDEREC","RA_REGCIVI","RA_NUMENDE","RA_TPLIVRO","RA_MSBLQL",								;
"RA_COMPLEM","RA_TIPCERT","RA_BAIRRO","RA_EMICERT","RA_ESTADO","RA_MATCERT","RA_LIVCERT","RA_CODMUN",								;
"RA_CODMUNE","RA_FOLCERT","RA_MUNICIP","RA_CARCERT","RA_CEP","RA_UFCERT","RA_CDMUCER","RA_CPOSTAL",									;
"RA_CEPCXPO","RA_MUNCERT","RA_ALTEND","RA_NUMEPAS","RA_TIPAMED","RA_EMISPAS","RA_DDDFONE","RA_TELEFON",								;
"RA_ASMEDIC","RA_UFPAS","RA_DPASSME","RA_DEMIPAS","RA_DDDCELU","RA_TPASODO","RA_DVALPAS","RA_NUMCELU",								;
"RA_ASODONT","RA_CHIDENT","RA_CODPAIS","RA_PAISPAS","RA_NUMRIC","RA_EMISRIC","RA_UFRIC","RA_CDMURIC",								;
"RA_NUMINSC","RA_SERVICO","RA_MUNIRIC","RA_DEXPRIC","RA_CODIGO","RA_OCEMIS","RA_OCDTEXP","RA_OCDTVAL",								;
"RA_CODUNIC","RA_PRCFCH","RA_PERFCH","RA_ROTFCH","RA_NUPFCH","RA_RNE","RA_RNEORG","RA_RNEDEXP",										;
"RA_DATCHEG","RA_ANOCHEG","RA_NUMNATU","RA_DATNATU","RA_CASADBR","RA_FILHOBR","RA_INSSAUT","RA_REGIME",								;
"RA_FWIDM","RA_INSMAX","RA_ADCCONF","RA_ADCTRF","RA_RHEXP","RA_ADTPOSE","RA_TPJORNA","RA_DTCAGED",									;
"RA_MATMIG","RA_DESEPS","RA_HOJORVA","RA_NSOCIAL","RA_CLASEST","RA_CATCNH","RA_UFCNH","RA_PLSAUDE",									;
"RA_FITIPEN","RA_MATIPEN","RA_TIPOPEN","RA_APOSENT","RA_SUBCARR","RA_DTNOMEA","RA_DEFETIV","RA_CODCON",								;
"RA_ADICEDI","RA_JORNRED","RA_TPSBCOM","RA_DTHREST","RA_TPCUEST","RA_ANOSEME","RA_PERESTU","RA_HABILMT",							;
"RA_DTINCON","RA_CTRLEMA","RA_CTPCD","RA_USRADM","RA_AUTMEI","RA_PLAPRE","RA_TIPCTA"}

aOrdem := {"01","02","03","04","05","06","07","08","09",							;
"10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25",	;
"26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41",	;
"42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57",	;
"58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73",	;
"74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89",	;
"90","91","92","93","94","95","96","97","98","99","A0","A1","A2","A3","A4","A5",	;
"A6","A7","A8","A9","B0","B1","B2","B3","B4","B5","B6","B7","B8","B9","C0","C1",	;
"C2","C3","C4","C5","C6","C7","C8","C9","D0","D1","D2","D3","D4","D5","D6","D7",	;
"D8","D9","E0","E1","E2","E3","E4","E5","E6","E7","E8","E9","F0","F1","F2","F3",	;
"F4","F5","F6","F7","F8","F9","G0","G1","G2","G3","G4","G5","G6","G7","G8","G9",	;
"H0","H1","H2","H3","H4","H5","H6","H7","H8","H9","I0","I1","I2","I3","I4","I5",	;
"I6","I7","I8","I9","J0","J1","J2","J3","J4","J5","J6","J7","J8","J9","K0","K1",	;
"K2","K3","K4","K5","K6","K7","K8","K9","L0","L1","L2","L3","L4","L5","L6","L7",	;
"L8","L9","M0","M1","M2","M3","M4","M5","M6","M7","M8","M9","N0","N1","N2","N3",	;
"N4","N5","N6","N7","N8","N9","O0","O1","O2","O3","O4","O5","O6","O7","O8","O9",	;
"P0","P1","P2","P3","P4","P5","P6","P7","P8","P9","Q0","Q1","Q2","Q3","Q4","Q5",	;
"Q6","Q7","Q8","Q9","R0","R1","R2","R3","R4","R5","R6","R7","R8","R9","S0","S1",	;
"S2","S3","S4","S5","S6","S7"}
nTam := len(aCampo)
dbSelectArea("SX3")
dbSetOrder(2)    

for i:= 1 to nTam
	if dbSeek(xfilial("SX3")+aCampo[i])     
		reclock("SX3",.F.)
		SX3->X3_ORDEM := aOrdem[i]
		SX3->(MsUnlock())
		alert("aCampo[i] = "+aCampo[i] + " aOrdem[i] =" + aOrdem[i])
		
	Endif	
next

Return
