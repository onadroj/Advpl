#INCLUDE "PROTHEUS.CH"

#DEFINE SIMPLES Char( 39 )
#DEFINE DUPLAS  Char( 34 )

#DEFINE CSSBOTAO	"QPushButton { color: #024670; "+;
"    border-image: url(rpo:fwstd_btn_nml.png) 3 3 3 3 stretch; "+;
"    border-top-width: 3px; "+;
"    border-left-width: 3px; "+;
"    border-right-width: 3px; "+;
"    border-bottom-width: 3px }"+;
"QPushButton:pressed {	color: #FFFFFF; "+;
"    border-image: url(rpo:fwstd_btn_prd.png) 3 3 3 3 stretch; "+;
"    border-top-width: 3px; "+;
"    border-left-width: 3px; "+;
"    border-right-width: 3px; "+;
"    border-bottom-width: 3px }"

//--------------------------------------------------------------------
/*/{Protheus.doc} UPDEXP
Função de update de dicionários para compatibilização

@author TOTVS Protheus
@since  28/06/2018
@obs    Gerado por EXPORDIC - V.5.4.1.3 EFS / Upd. V.4.21.17 EFS
@version 1.0
/*/
//--------------------------------------------------------------------
User Function UPDEXP( cEmpAmb, cFilAmb )

Local   aSay      := {}
Local   aButton   := {}
Local   aMarcadas := {}
Local   cTitulo   := "ATUALIZAÇÃO DE DICIONÁRIOS E TABELAS"
Local   cDesc1    := "Esta rotina tem como função fazer  a atualização  dos dicionários do Sistema ( SX?/SIX )"
Local   cDesc2    := "Este processo deve ser executado em modo EXCLUSIVO, ou seja não podem haver outros"
Local   cDesc3    := "usuários  ou  jobs utilizando  o sistema.  É EXTREMAMENTE recomendavél  que  se  faça um"
Local   cDesc4    := "BACKUP  dos DICIONÁRIOS  e da  BASE DE DADOS antes desta atualização, para que caso "
Local   cDesc5    := "ocorram eventuais falhas, esse backup possa ser restaurado."
Local   cDesc6    := ""
Local   cDesc7    := ""
Local   lOk       := .F.
Local   lAuto     := ( cEmpAmb <> NIL .or. cFilAmb <> NIL )

Private oMainWnd  := NIL
Private oProcess  := NIL

#IFDEF TOP
    TCInternal( 5, "*OFF" ) // Desliga Refresh no Lock do Top
#ENDIF

__cInterNet := NIL
__lPYME     := .F.

Set Dele On

// Mensagens de Tela Inicial
aAdd( aSay, cDesc1 )
aAdd( aSay, cDesc2 )
aAdd( aSay, cDesc3 )
aAdd( aSay, cDesc4 )
aAdd( aSay, cDesc5 )
//aAdd( aSay, cDesc6 )
//aAdd( aSay, cDesc7 )

// Botoes Tela Inicial
aAdd(  aButton, {  1, .T., { || lOk := .T., FechaBatch() } } )
aAdd(  aButton, {  2, .T., { || lOk := .F., FechaBatch() } } )

If lAuto
	lOk := .T.
Else
	FormBatch(  cTitulo,  aSay,  aButton )
EndIf

If lOk
	If lAuto
		aMarcadas :={{ cEmpAmb, cFilAmb, "" }}
	Else

		If !FWAuthAdmin()
			Final( "Atualização não Realizada." )
		EndIf

		aMarcadas := EscEmpresa()
	EndIf

	If !Empty( aMarcadas )
		If lAuto .OR. MsgNoYes( "Confirma a atualização dos dicionários ?", cTitulo )
			oProcess := MsNewProcess():New( { | lEnd | lOk := FSTProc( @lEnd, aMarcadas, lAuto ) }, "Atualizando", "Aguarde, atualizando ...", .F. )
			oProcess:Activate()

			If lAuto
				If lOk
					MsgStop( "Atualização Realizada.", "UPDEXP" )
				Else
					MsgStop( "Atualização não Realizada.", "UPDEXP" )
				EndIf
				dbCloseAll()
			Else
				If lOk
					Final( "Atualização Realizada." )
				Else
					Final( "Atualização não Realizada." )
				EndIf
			EndIf

		Else
			Final( "Atualização não Realizada." )

		EndIf

	Else
		Final( "Atualização não Realizada." )

	EndIf

EndIf

Return NIL


//--------------------------------------------------------------------
/*/{Protheus.doc} FSTProc
Função de processamento da gravação dos arquivos

@author TOTVS Protheus
@since  28/06/2018
@obs    Gerado por EXPORDIC - V.5.4.1.3 EFS / Upd. V.4.21.17 EFS
@version 1.0
/*/
//--------------------------------------------------------------------
Static Function FSTProc( lEnd, aMarcadas, lAuto )
Local   aInfo     := {}
Local   aRecnoSM0 := {}
Local   cAux      := ""
Local   cFile     := ""
Local   cFileLog  := ""
Local   cMask     := "Arquivos Texto" + "(*.TXT)|*.txt|"
Local   cTCBuild  := "TCGetBuild"
Local   cTexto    := ""
Local   cTopBuild := ""
Local   lOpen     := .F.
Local   lRet      := .T.
Local   nI        := 0
Local   nPos      := 0
Local   nRecno    := 0
Local   nX        := 0
Local   oDlg      := NIL
Local   oFont     := NIL
Local   oMemo     := NIL

Private aArqUpd   := {}

If ( lOpen := MyOpenSm0(.T.) )

	dbSelectArea( "SM0" )
	dbGoTop()

	While !SM0->( EOF() )
		// Só adiciona no aRecnoSM0 se a empresa for diferente
		If aScan( aRecnoSM0, { |x| x[2] == SM0->M0_CODIGO } ) == 0 ;
		   .AND. aScan( aMarcadas, { |x| x[1] == SM0->M0_CODIGO } ) > 0
			aAdd( aRecnoSM0, { Recno(), SM0->M0_CODIGO } )
		EndIf
		SM0->( dbSkip() )
	End

	SM0->( dbCloseArea() )

	If lOpen

		For nI := 1 To Len( aRecnoSM0 )

			If !( lOpen := MyOpenSm0(.F.) )
				MsgStop( "Atualização da empresa " + aRecnoSM0[nI][2] + " não efetuada." )
				Exit
			EndIf

			SM0->( dbGoTo( aRecnoSM0[nI][1] ) )

			RpcSetType( 3 )
			RpcSetEnv( SM0->M0_CODIGO, SM0->M0_CODFIL )

			lMsFinalAuto := .F.
			lMsHelpAuto  := .F.

			AutoGrLog( Replicate( "-", 128 ) )
			AutoGrLog( Replicate( " ", 128 ) )
			AutoGrLog( "LOG DA ATUALIZAÇÃO DOS DICIONÁRIOS" )
			AutoGrLog( Replicate( " ", 128 ) )
			AutoGrLog( Replicate( "-", 128 ) )
			AutoGrLog( " " )
			AutoGrLog( " Dados Ambiente" )
			AutoGrLog( " --------------------" )
			AutoGrLog( " Empresa / Filial...: " + cEmpAnt + "/" + cFilAnt )
			AutoGrLog( " Nome Empresa.......: " + Capital( AllTrim( GetAdvFVal( "SM0", "M0_NOMECOM", cEmpAnt + cFilAnt, 1, "" ) ) ) )
			AutoGrLog( " Nome Filial........: " + Capital( AllTrim( GetAdvFVal( "SM0", "M0_FILIAL" , cEmpAnt + cFilAnt, 1, "" ) ) ) )
			AutoGrLog( " DataBase...........: " + DtoC( dDataBase ) )
			AutoGrLog( " Data / Hora Ínicio.: " + DtoC( Date() )  + " / " + Time() )
			AutoGrLog( " Environment........: " + GetEnvServer()  )
			AutoGrLog( " StartPath..........: " + GetSrvProfString( "StartPath", "" ) )
			AutoGrLog( " RootPath...........: " + GetSrvProfString( "RootPath" , "" ) )
			AutoGrLog( " Versão.............: " + GetVersao(.T.) )
			AutoGrLog( " Usuário TOTVS .....: " + __cUserId + " " +  cUserName )
			AutoGrLog( " Computer Name......: " + GetComputerName() )

			aInfo   := GetUserInfo()
			If ( nPos    := aScan( aInfo,{ |x,y| x[3] == ThreadId() } ) ) > 0
				AutoGrLog( " " )
				AutoGrLog( " Dados Thread" )
				AutoGrLog( " --------------------" )
				AutoGrLog( " Usuário da Rede....: " + aInfo[nPos][1] )
				AutoGrLog( " Estação............: " + aInfo[nPos][2] )
				AutoGrLog( " Programa Inicial...: " + aInfo[nPos][5] )
				AutoGrLog( " Environment........: " + aInfo[nPos][6] )
				AutoGrLog( " Conexão............: " + AllTrim( StrTran( StrTran( aInfo[nPos][7], Chr( 13 ), "" ), Chr( 10 ), "" ) ) )
			EndIf
			AutoGrLog( Replicate( "-", 128 ) )
			AutoGrLog( " " )

			If !lAuto
				AutoGrLog( Replicate( "-", 128 ) )
				AutoGrLog( "Empresa : " + SM0->M0_CODIGO + "/" + SM0->M0_NOME + CRLF )
			EndIf

			oProcess:SetRegua1( 8 )

			//------------------------------------
			// Atualiza o dicionário SX3
			//------------------------------------
			FSAtuSX3()

			oProcess:IncRegua1( "Dicionário de dados" + " - " + SM0->M0_CODIGO + " " + SM0->M0_NOME + " ..." )
			oProcess:IncRegua2( "Atualizando campos/índices" )

			// Alteração física dos arquivos
			__SetX31Mode( .F. )

			If FindFunction(cTCBuild)
				cTopBuild := &cTCBuild.()
			EndIf

			For nX := 1 To Len( aArqUpd )

				If cTopBuild >= "20090811" .AND. TcInternal( 89 ) == "CLOB_SUPPORTED"
					If ( ( aArqUpd[nX] >= "NQ " .AND. aArqUpd[nX] <= "NZZ" ) .OR. ( aArqUpd[nX] >= "O0 " .AND. aArqUpd[nX] <= "NZZ" ) ) .AND.;
						!aArqUpd[nX] $ "NQD,NQF,NQP,NQT"
						TcInternal( 25, "CLOB" )
					EndIf
				EndIf

				If Select( aArqUpd[nX] ) > 0
					dbSelectArea( aArqUpd[nX] )
					dbCloseArea()
				EndIf

				X31UpdTable( aArqUpd[nX] )

				If __GetX31Error()
					Alert( __GetX31Trace() )
					MsgStop( "Ocorreu um erro desconhecido durante a atualização da tabela : " + aArqUpd[nX] + ". Verifique a integridade do dicionário e da tabela.", "ATENÇÃO" )
					AutoGrLog( "Ocorreu um erro desconhecido durante a atualização da estrutura da tabela : " + aArqUpd[nX] )
				EndIf

				If cTopBuild >= "20090811" .AND. TcInternal( 89 ) == "CLOB_SUPPORTED"
					TcInternal( 25, "OFF" )
				EndIf

			Next nX

			//------------------------------------
			// Atualiza o dicionário SX6
			//------------------------------------
			oProcess:IncRegua1( "Dicionário de parâmetros" + " - " + SM0->M0_CODIGO + " " + SM0->M0_NOME + " ..." )
			FSAtuSX6()

			//------------------------------------
			// Atualiza os helps
			//------------------------------------
			oProcess:IncRegua1( "Helps de Campo" + " - " + SM0->M0_CODIGO + " " + SM0->M0_NOME + " ..." )
			FSAtuHlp()

			AutoGrLog( Replicate( "-", 128 ) )
			AutoGrLog( " Data / Hora Final.: " + DtoC( Date() ) + " / " + Time() )
			AutoGrLog( Replicate( "-", 128 ) )

			RpcClearEnv()

		Next nI

		If !lAuto

			cTexto := LeLog()

			Define Font oFont Name "Mono AS" Size 5, 12

			Define MsDialog oDlg Title "Atualização concluida." From 3, 0 to 340, 417 Pixel

			@ 5, 5 Get oMemo Var cTexto Memo Size 200, 145 Of oDlg Pixel
			oMemo:bRClicked := { || AllwaysTrue() }
			oMemo:oFont     := oFont

			Define SButton From 153, 175 Type  1 Action oDlg:End() Enable Of oDlg Pixel // Apaga
			Define SButton From 153, 145 Type 13 Action ( cFile := cGetFile( cMask, "" ), If( cFile == "", .T., ;
			MemoWrite( cFile, cTexto ) ) ) Enable Of oDlg Pixel

			Activate MsDialog oDlg Center

		EndIf

	EndIf

Else

	lRet := .F.

EndIf

Return lRet


//--------------------------------------------------------------------
/*/{Protheus.doc} FSAtuSX3
Função de processamento da gravação do SX3 - Campos

@author TOTVS Protheus
@since  28/06/2018
@obs    Gerado por EXPORDIC - V.5.4.1.3 EFS / Upd. V.4.21.17 EFS
@version 1.0
/*/
//--------------------------------------------------------------------
Static Function FSAtuSX3()
Local aEstrut   := {}
Local aSX3      := {}
Local cAlias    := ""
Local cAliasAtu := ""
Local cMsg      := ""
Local cSeqAtu   := ""
Local cX3Campo  := ""
Local cX3Dado   := ""
Local lTodosNao := .F.
Local lTodosSim := .F.
Local nI        := 0
Local nJ        := 0
Local nOpcA     := 0
Local nPosArq   := 0
Local nPosCpo   := 0
Local nPosOrd   := 0
Local nPosSXG   := 0
Local nPosTam   := 0
Local nPosVld   := 0
Local nSeqAtu   := 0
Local nTamSeek  := Len( SX3->X3_CAMPO )

AutoGrLog( "Ínicio da Atualização" + " SX3" + CRLF )

aEstrut := { { "X3_ARQUIVO", 0 }, { "X3_ORDEM"  , 0 }, { "X3_CAMPO"  , 0 }, { "X3_TIPO"   , 0 }, { "X3_TAMANHO", 0 }, { "X3_DECIMAL", 0 }, { "X3_TITULO" , 0 }, ;
             { "X3_TITSPA" , 0 }, { "X3_TITENG" , 0 }, { "X3_DESCRIC", 0 }, { "X3_DESCSPA", 0 }, { "X3_DESCENG", 0 }, { "X3_PICTURE", 0 }, { "X3_VALID"  , 0 }, ;
             { "X3_USADO"  , 0 }, { "X3_RELACAO", 0 }, { "X3_F3"     , 0 }, { "X3_NIVEL"  , 0 }, { "X3_RESERV" , 0 }, { "X3_CHECK"  , 0 }, { "X3_TRIGGER", 0 }, ;
             { "X3_PROPRI" , 0 }, { "X3_BROWSE" , 0 }, { "X3_VISUAL" , 0 }, { "X3_CONTEXT", 0 }, { "X3_OBRIGAT", 0 }, { "X3_VLDUSER", 0 }, { "X3_CBOX"   , 0 }, ;
             { "X3_CBOXSPA", 0 }, { "X3_CBOXENG", 0 }, { "X3_PICTVAR", 0 }, { "X3_WHEN"   , 0 }, { "X3_INIBRW" , 0 }, { "X3_GRPSXG" , 0 }, { "X3_FOLDER" , 0 }, ;
             { "X3_CONDSQL", 0 }, { "X3_CHKSQL" , 0 }, { "X3_IDXSRV" , 0 }, { "X3_ORTOGRA", 0 }, { "X3_TELA"   , 0 }, { "X3_POSLGT" , 0 }, { "X3_IDXFLD" , 0 }, ;
             { "X3_AGRUP"  , 0 }, { "X3_MODAL"  , 0 }, { "X3_PYME"   , 0 } }

aEval( aEstrut, { |x| x[2] := SX3->( FieldPos( x[1] ) ) } )


//
// Campos Tabela SRV
//
aAdd( aSX3, { ;
	'SRV'																	, ; //X3_ARQUIVO
	'62'																	, ; //X3_ORDEM
	'RV_CTADEB'																, ; //X3_CAMPO
	'C'																		, ; //X3_TIPO
	10																		, ; //X3_TAMANHO
	0																		, ; //X3_DECIMAL
	'Cta Deb. Adm'															, ; //X3_TITULO
	'Cta Deb. Adm'															, ; //X3_TITSPA
	'Cta Deb. Adm'															, ; //X3_TITENG
	'Cta Debito Administracao'												, ; //X3_DESCRIC
	'Cta Debito Administracao'												, ; //X3_DESCSPA
	'Cta Debito Administracao'												, ; //X3_DESCENG
	'!@'																	, ; //X3_PICTURE
	''																		, ; //X3_VALID
	Chr(128) + Chr(128) + Chr(128) + Chr(128) + Chr(128) + ;
	Chr(128) + Chr(128) + Chr(128) + Chr(128) + Chr(128) + ;
	Chr(128) + Chr(128) + Chr(128) + Chr(128) + Chr(160)					, ; //X3_USADO
	''																		, ; //X3_RELACAO
	'CT1'																	, ; //X3_F3
	0																		, ; //X3_NIVEL
	Chr(254) + Chr(192)														, ; //X3_RESERV
	''																		, ; //X3_CHECK
	''																		, ; //X3_TRIGGER
	'U'																		, ; //X3_PROPRI
	'N'																		, ; //X3_BROWSE
	'A'																		, ; //X3_VISUAL
	'R'																		, ; //X3_CONTEXT
	''																		, ; //X3_OBRIGAT
	''																		, ; //X3_VLDUSER
	''																		, ; //X3_CBOX
	''																		, ; //X3_CBOXSPA
	''																		, ; //X3_CBOXENG
	''																		, ; //X3_PICTVAR
	''																		, ; //X3_WHEN
	''																		, ; //X3_INIBRW
	''																		, ; //X3_GRPSXG
	'2'																		, ; //X3_FOLDER
	''																		, ; //X3_CONDSQL
	''																		, ; //X3_CHKSQL
	''																		, ; //X3_IDXSRV
	'N'																		, ; //X3_ORTOGRA
	''																		, ; //X3_TELA
	''																		, ; //X3_POSLGT
	'N'																		, ; //X3_IDXFLD
	''																		, ; //X3_AGRUP
	''																		, ; //X3_MODAL
	''																		} ) //X3_PYME

aAdd( aSX3, { ;
	'SRV'																	, ; //X3_ARQUIVO
	'63'																	, ; //X3_ORDEM
	'RV_CTACRED'															, ; //X3_CAMPO
	'C'																		, ; //X3_TIPO
	10																		, ; //X3_TAMANHO
	0																		, ; //X3_DECIMAL
	'Cta Cred Adm'															, ; //X3_TITULO
	'Cta Cred Adm'															, ; //X3_TITSPA
	'Cta Cred Adm'															, ; //X3_TITENG
	'Cta Credito Administracao'												, ; //X3_DESCRIC
	'Cta Credito Administracao'												, ; //X3_DESCSPA
	'Cta Credito Administracao'												, ; //X3_DESCENG
	'!@'																	, ; //X3_PICTURE
	''																		, ; //X3_VALID
	Chr(128) + Chr(128) + Chr(128) + Chr(128) + Chr(128) + ;
	Chr(128) + Chr(128) + Chr(128) + Chr(128) + Chr(128) + ;
	Chr(128) + Chr(128) + Chr(128) + Chr(128) + Chr(160)					, ; //X3_USADO
	''																		, ; //X3_RELACAO
	'CT1'																	, ; //X3_F3
	0																		, ; //X3_NIVEL
	Chr(254) + Chr(192)														, ; //X3_RESERV
	''																		, ; //X3_CHECK
	''																		, ; //X3_TRIGGER
	'U'																		, ; //X3_PROPRI
	'N'																		, ; //X3_BROWSE
	'A'																		, ; //X3_VISUAL
	'R'																		, ; //X3_CONTEXT
	''																		, ; //X3_OBRIGAT
	''																		, ; //X3_VLDUSER
	''																		, ; //X3_CBOX
	''																		, ; //X3_CBOXSPA
	''																		, ; //X3_CBOXENG
	''																		, ; //X3_PICTVAR
	''																		, ; //X3_WHEN
	''																		, ; //X3_INIBRW
	''																		, ; //X3_GRPSXG
	'2'																		, ; //X3_FOLDER
	''																		, ; //X3_CONDSQL
	''																		, ; //X3_CHKSQL
	''																		, ; //X3_IDXSRV
	'N'																		, ; //X3_ORTOGRA
	''																		, ; //X3_TELA
	''																		, ; //X3_POSLGT
	'N'																		, ; //X3_IDXFLD
	''																		, ; //X3_AGRUP
	''																		, ; //X3_MODAL
	''																		} ) //X3_PYME

aAdd( aSX3, { ;
	'SRV'																	, ; //X3_ARQUIVO
	'64'																	, ; //X3_ORDEM
	'RV_CTADEB2'															, ; //X3_CAMPO
	'C'																		, ; //X3_TIPO
	10																		, ; //X3_TAMANHO
	0																		, ; //X3_DECIMAL
	'Cta Deb Dir'															, ; //X3_TITULO
	'Cta Deb Dir'															, ; //X3_TITSPA
	'Cta Deb Dir'															, ; //X3_TITENG
	'Cta Debito C Direto'													, ; //X3_DESCRIC
	'Cta Debito C Direto'													, ; //X3_DESCSPA
	'Cta Debito C Direto'													, ; //X3_DESCENG
	'!@'																	, ; //X3_PICTURE
	''																		, ; //X3_VALID
	Chr(128) + Chr(128) + Chr(128) + Chr(128) + Chr(128) + ;
	Chr(128) + Chr(128) + Chr(128) + Chr(128) + Chr(128) + ;
	Chr(128) + Chr(128) + Chr(128) + Chr(128) + Chr(160)					, ; //X3_USADO
	''																		, ; //X3_RELACAO
	'CT1'																	, ; //X3_F3
	0																		, ; //X3_NIVEL
	Chr(254) + Chr(192)														, ; //X3_RESERV
	''																		, ; //X3_CHECK
	''																		, ; //X3_TRIGGER
	'U'																		, ; //X3_PROPRI
	'N'																		, ; //X3_BROWSE
	'A'																		, ; //X3_VISUAL
	'R'																		, ; //X3_CONTEXT
	''																		, ; //X3_OBRIGAT
	''																		, ; //X3_VLDUSER
	''																		, ; //X3_CBOX
	''																		, ; //X3_CBOXSPA
	''																		, ; //X3_CBOXENG
	''																		, ; //X3_PICTVAR
	''																		, ; //X3_WHEN
	''																		, ; //X3_INIBRW
	''																		, ; //X3_GRPSXG
	'2'																		, ; //X3_FOLDER
	''																		, ; //X3_CONDSQL
	''																		, ; //X3_CHKSQL
	''																		, ; //X3_IDXSRV
	'N'																		, ; //X3_ORTOGRA
	''																		, ; //X3_TELA
	''																		, ; //X3_POSLGT
	'N'																		, ; //X3_IDXFLD
	''																		, ; //X3_AGRUP
	''																		, ; //X3_MODAL
	''																		} ) //X3_PYME

aAdd( aSX3, { ;
	'SRV'																	, ; //X3_ARQUIVO
	'65'																	, ; //X3_ORDEM
	'RV_CTCRED2'															, ; //X3_CAMPO
	'C'																		, ; //X3_TIPO
	10																		, ; //X3_TAMANHO
	0																		, ; //X3_DECIMAL
	'Cta Cred Dir'															, ; //X3_TITULO
	'Cta Cred Dir'															, ; //X3_TITSPA
	'Cta Cred Dir'															, ; //X3_TITENG
	'Cta Credito C Direto'													, ; //X3_DESCRIC
	'Cta Credito C Direto'													, ; //X3_DESCSPA
	'Cta Credito C Direto'													, ; //X3_DESCENG
	'!@'																	, ; //X3_PICTURE
	''																		, ; //X3_VALID
	Chr(128) + Chr(128) + Chr(128) + Chr(128) + Chr(128) + ;
	Chr(128) + Chr(128) + Chr(128) + Chr(128) + Chr(128) + ;
	Chr(128) + Chr(128) + Chr(128) + Chr(128) + Chr(160)					, ; //X3_USADO
	''																		, ; //X3_RELACAO
	'CT1'																	, ; //X3_F3
	0																		, ; //X3_NIVEL
	Chr(254) + Chr(192)														, ; //X3_RESERV
	''																		, ; //X3_CHECK
	''																		, ; //X3_TRIGGER
	'U'																		, ; //X3_PROPRI
	'N'																		, ; //X3_BROWSE
	'A'																		, ; //X3_VISUAL
	'R'																		, ; //X3_CONTEXT
	''																		, ; //X3_OBRIGAT
	''																		, ; //X3_VLDUSER
	''																		, ; //X3_CBOX
	''																		, ; //X3_CBOXSPA
	''																		, ; //X3_CBOXENG
	''																		, ; //X3_PICTVAR
	''																		, ; //X3_WHEN
	''																		, ; //X3_INIBRW
	''																		, ; //X3_GRPSXG
	'2'																		, ; //X3_FOLDER
	''																		, ; //X3_CONDSQL
	''																		, ; //X3_CHKSQL
	''																		, ; //X3_IDXSRV
	'N'																		, ; //X3_ORTOGRA
	''																		, ; //X3_TELA
	''																		, ; //X3_POSLGT
	'N'																		, ; //X3_IDXFLD
	''																		, ; //X3_AGRUP
	''																		, ; //X3_MODAL
	''																		} ) //X3_PYME

aAdd( aSX3, { ;
	'SRV'																	, ; //X3_ARQUIVO
	'66'																	, ; //X3_ORDEM
	'RV_CTADEB3'															, ; //X3_CAMPO
	'C'																		, ; //X3_TIPO
	10																		, ; //X3_TAMANHO
	0																		, ; //X3_DECIMAL
	'Cta Deb Ind'															, ; //X3_TITULO
	'Cta Deb Ind'															, ; //X3_TITSPA
	'Cta Deb Ind'															, ; //X3_TITENG
	'Cta Debito C Indireto'													, ; //X3_DESCRIC
	'Cta Debito C Indireto'													, ; //X3_DESCSPA
	'Cta Debito C Indireto'													, ; //X3_DESCENG
	'!@'																	, ; //X3_PICTURE
	''																		, ; //X3_VALID
	Chr(128) + Chr(128) + Chr(128) + Chr(128) + Chr(128) + ;
	Chr(128) + Chr(128) + Chr(128) + Chr(128) + Chr(128) + ;
	Chr(128) + Chr(128) + Chr(128) + Chr(128) + Chr(160)					, ; //X3_USADO
	''																		, ; //X3_RELACAO
	'CT1'																	, ; //X3_F3
	0																		, ; //X3_NIVEL
	Chr(254) + Chr(192)														, ; //X3_RESERV
	''																		, ; //X3_CHECK
	''																		, ; //X3_TRIGGER
	'U'																		, ; //X3_PROPRI
	'N'																		, ; //X3_BROWSE
	'A'																		, ; //X3_VISUAL
	'R'																		, ; //X3_CONTEXT
	''																		, ; //X3_OBRIGAT
	''																		, ; //X3_VLDUSER
	''																		, ; //X3_CBOX
	''																		, ; //X3_CBOXSPA
	''																		, ; //X3_CBOXENG
	''																		, ; //X3_PICTVAR
	''																		, ; //X3_WHEN
	''																		, ; //X3_INIBRW
	''																		, ; //X3_GRPSXG
	'2'																		, ; //X3_FOLDER
	''																		, ; //X3_CONDSQL
	''																		, ; //X3_CHKSQL
	''																		, ; //X3_IDXSRV
	'N'																		, ; //X3_ORTOGRA
	''																		, ; //X3_TELA
	''																		, ; //X3_POSLGT
	'N'																		, ; //X3_IDXFLD
	''																		, ; //X3_AGRUP
	''																		, ; //X3_MODAL
	''																		} ) //X3_PYME

aAdd( aSX3, { ;
	'SRV'																	, ; //X3_ARQUIVO
	'67'																	, ; //X3_ORDEM
	'RV_CTCRED3'															, ; //X3_CAMPO
	'C'																		, ; //X3_TIPO
	10																		, ; //X3_TAMANHO
	0																		, ; //X3_DECIMAL
	'Cta Cred Ind'															, ; //X3_TITULO
	'Cta Cred Ind'															, ; //X3_TITSPA
	'Cta Cred Ind'															, ; //X3_TITENG
	'Cta Credito C Indireto'												, ; //X3_DESCRIC
	'Cta Credito C Indireto'												, ; //X3_DESCSPA
	'Cta Credito C Indireto'												, ; //X3_DESCENG
	'!@'																	, ; //X3_PICTURE
	''																		, ; //X3_VALID
	Chr(128) + Chr(128) + Chr(128) + Chr(128) + Chr(128) + ;
	Chr(128) + Chr(128) + Chr(128) + Chr(128) + Chr(128) + ;
	Chr(128) + Chr(128) + Chr(128) + Chr(128) + Chr(160)					, ; //X3_USADO
	''																		, ; //X3_RELACAO
	'CT1'																	, ; //X3_F3
	0																		, ; //X3_NIVEL
	Chr(254) + Chr(192)														, ; //X3_RESERV
	''																		, ; //X3_CHECK
	''																		, ; //X3_TRIGGER
	'U'																		, ; //X3_PROPRI
	'N'																		, ; //X3_BROWSE
	'A'																		, ; //X3_VISUAL
	'R'																		, ; //X3_CONTEXT
	''																		, ; //X3_OBRIGAT
	''																		, ; //X3_VLDUSER
	''																		, ; //X3_CBOX
	''																		, ; //X3_CBOXSPA
	''																		, ; //X3_CBOXENG
	''																		, ; //X3_PICTVAR
	''																		, ; //X3_WHEN
	''																		, ; //X3_INIBRW
	''																		, ; //X3_GRPSXG
	'2'																		, ; //X3_FOLDER
	''																		, ; //X3_CONDSQL
	''																		, ; //X3_CHKSQL
	''																		, ; //X3_IDXSRV
	'N'																		, ; //X3_ORTOGRA
	''																		, ; //X3_TELA
	''																		, ; //X3_POSLGT
	'N'																		, ; //X3_IDXFLD
	''																		, ; //X3_AGRUP
	''																		, ; //X3_MODAL
	''																		} ) //X3_PYME


//
// Atualizando dicionário
//
nPosArq := aScan( aEstrut, { |x| AllTrim( x[1] ) == "X3_ARQUIVO" } )
nPosOrd := aScan( aEstrut, { |x| AllTrim( x[1] ) == "X3_ORDEM"   } )
nPosCpo := aScan( aEstrut, { |x| AllTrim( x[1] ) == "X3_CAMPO"   } )
nPosTam := aScan( aEstrut, { |x| AllTrim( x[1] ) == "X3_TAMANHO" } )
nPosSXG := aScan( aEstrut, { |x| AllTrim( x[1] ) == "X3_GRPSXG"  } )
nPosVld := aScan( aEstrut, { |x| AllTrim( x[1] ) == "X3_VALID"   } )

aSort( aSX3,,, { |x,y| x[nPosArq]+x[nPosOrd]+x[nPosCpo] < y[nPosArq]+y[nPosOrd]+y[nPosCpo] } )

oProcess:SetRegua2( Len( aSX3 ) )

dbSelectArea( "SX3" )
dbSetOrder( 2 )
cAliasAtu := ""

For nI := 1 To Len( aSX3 )

	//
	// Verifica se o campo faz parte de um grupo e ajusta tamanho
	//
	If !Empty( aSX3[nI][nPosSXG] )
		SXG->( dbSetOrder( 1 ) )
		If SXG->( MSSeek( aSX3[nI][nPosSXG] ) )
			If aSX3[nI][nPosTam] <> SXG->XG_SIZE
				aSX3[nI][nPosTam] := SXG->XG_SIZE
				AutoGrLog( "O tamanho do campo " + aSX3[nI][nPosCpo] + " NÃO atualizado e foi mantido em [" + ;
				AllTrim( Str( SXG->XG_SIZE ) ) + "]" + CRLF + ;
				" por pertencer ao grupo de campos [" + SXG->XG_GRUPO + "]" + CRLF )
			EndIf
		EndIf
	EndIf

	SX3->( dbSetOrder( 2 ) )

	If !( aSX3[nI][nPosArq] $ cAlias )
		cAlias += aSX3[nI][nPosArq] + "/"
		aAdd( aArqUpd, aSX3[nI][nPosArq] )
	EndIf

	If !SX3->( dbSeek( PadR( aSX3[nI][nPosCpo], nTamSeek ) ) )

		//
		// Busca ultima ocorrencia do alias
		//
		If ( aSX3[nI][nPosArq] <> cAliasAtu )
			cSeqAtu   := "00"
			cAliasAtu := aSX3[nI][nPosArq]

			dbSetOrder( 1 )
			SX3->( dbSeek( cAliasAtu + "ZZ", .T. ) )
			dbSkip( -1 )

			If ( SX3->X3_ARQUIVO == cAliasAtu )
				cSeqAtu := SX3->X3_ORDEM
			EndIf

			nSeqAtu := Val( RetAsc( cSeqAtu, 3, .F. ) )
		EndIf

		nSeqAtu++
		cSeqAtu := RetAsc( Str( nSeqAtu ), 2, .T. )

		RecLock( "SX3", .T. )
		For nJ := 1 To Len( aSX3[nI] )
			If     nJ == nPosOrd  // Ordem
				SX3->( FieldPut( FieldPos( aEstrut[nJ][1] ), cSeqAtu ) )

			ElseIf aEstrut[nJ][2] > 0
				SX3->( FieldPut( FieldPos( aEstrut[nJ][1] ), aSX3[nI][nJ] ) )

			EndIf
		Next nJ

		dbCommit()
		MsUnLock()

		AutoGrLog( "Criado campo " + aSX3[nI][nPosCpo] )

	EndIf

	oProcess:IncRegua2( "Atualizando Campos de Tabelas (SX3)..." )

Next nI

AutoGrLog( CRLF + "Final da Atualização" + " SX3" + CRLF + Replicate( "-", 128 ) + CRLF )

Return NIL


//--------------------------------------------------------------------
/*/{Protheus.doc} FSAtuSX6
Função de processamento da gravação do SX6 - Parâmetros

@author TOTVS Protheus
@since  28/06/2018
@obs    Gerado por EXPORDIC - V.5.4.1.3 EFS / Upd. V.4.21.17 EFS
@version 1.0
/*/
//--------------------------------------------------------------------
Static Function FSAtuSX6()
Local aEstrut   := {}
Local aSX6      := {}
Local cAlias    := ""
Local cMsg      := ""
Local lContinua := .T.
Local lReclock  := .T.
Local lTodosNao := .F.
Local lTodosSim := .F.
Local nI        := 0
Local nJ        := 0
Local nOpcA     := 0
Local nTamFil   := Len( SX6->X6_FIL )
Local nTamVar   := Len( SX6->X6_VAR )

AutoGrLog( "Ínicio da Atualização" + " SX6" + CRLF )

aEstrut := { "X6_FIL"    , "X6_VAR"    , "X6_TIPO"   , "X6_DESCRIC", "X6_DSCSPA" , "X6_DSCENG" , "X6_DESC1"  , ;
             "X6_DSCSPA1", "X6_DSCENG1", "X6_DESC2"  , "X6_DSCSPA2", "X6_DSCENG2", "X6_CONTEUD", "X6_CONTSPA", ;
             "X6_CONTENG", "X6_PROPRI" , "X6_VALID"  , "X6_INIT"   , "X6_DEFPOR" , "X6_DEFSPA" , "X6_DEFENG" , ;
             "X6_PYME"   }

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'FS_GCTCOT'																, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'Tipo Contrato para cotacao'											, ; //X6_DESCRIC
	'Tipo Contrato para cotizacion'											, ; //X6_DSCSPA
	'Contract type for quotation'											, ; //X6_DSCENG
	''																		, ; //X6_DESC1
	''																		, ; //X6_DSCSPA1
	''																		, ; //X6_DSCENG1
	''																		, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	'001'																	, ; //X6_CONTEUD
	'001'																	, ; //X6_CONTSPA
	'001'																	, ; //X6_CONTENG
	'S'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	'001'																	, ; //X6_DEFPOR
	'001'																	, ; //X6_DEFSPA
	'001'																	, ; //X6_DEFENG
	'S'																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_AB10925'															, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'Modo de retencao do PIS,COFINS e CSLL p/ C.Receber'					, ; //X6_DESCRIC
	'Modo de retenc. del PIS,COFINS y CSLL p/ C. Cobrar'					, ; //X6_DSCSPA
	'PIS, COFINS and CSLL withholding mode for accounts'					, ; //X6_DSCENG
	'1=Verif.retencao pelo valor da nota emitida'							, ; //X6_DESC1
	'1=Verif.retencion por valor de factura emitida'						, ; //X6_DSCSPA1
	'receivable. 1=Check withh.by issued invoice value.'					, ; //X6_DSCENG1
	'2=Verif.retencao p/soma notas emitidas no periodo'						, ; //X6_DESC2
	'2=Verif.retenc. p/suma facturas emitidas en period'					, ; //X6_DSCSPA2
	'2=Check withh.to add invoices issued in period.'						, ; //X6_DSCENG2
	'1'																		, ; //X6_CONTEUD
	'1'																		, ; //X6_CONTSPA
	'1'																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_AC10925'															, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'Controle de permissao para o usuario alterar modo'						, ; //X6_DESCRIC
	'Control de autorizacion para que usuario modifique'					, ; //X6_DSCSPA
	'Permission control for the user to edit the mode'						, ; //X6_DSCENG
	'de retencao na janela "calculo de retencao"'							, ; //X6_DESC1
	'modo retencion en ventana "calculo de retencion"'						, ; //X6_DSCSPA1
	'of withholding in the "withhholding calculation"'						, ; //X6_DSCENG1
	'1=Permite alteracao, 2= Nao permite alteracao'							, ; //X6_DESC2
	'1=Permite modificacion, 2= No permite modificacion'					, ; //X6_DSCSPA2
	'screen (1=Allow edition, 2=Do not allow edition).'						, ; //X6_DSCENG2
	'1'																		, ; //X6_CONTEUD
	'1'																		, ; //X6_CONTSPA
	'1'																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_ALIQCOL'															, ; //X6_VAR
	'L'																		, ; //X6_TIPO
	'Indica se vai aparecer as colunas de aliquota de'						, ; //X6_DESCRIC
	'Indica si apareceran las columnas de alicuota de'						, ; //X6_DSCSPA
	'Indicates whether to disiplay PIS and COFINS rate'						, ; //X6_DSCENG
	'PIS e COFINS no folder "OUTROS VALORES" na'							, ; //X6_DESC1
	'PIS y COFINS en la carpeta "OTROS VALORES" en el'						, ; //X6_DSCSPA1
	'columns in folder "OTHER AMOUNTS" when calculating'					, ; //X6_DSCENG1
	'Apuracao de PIS/COFINS.'												, ; //X6_DESC2
	'Calculo de PIS/COFINS.'												, ; //X6_DSCSPA2
	'PIS/COFINS.'															, ; //X6_DSCENG2
	'F'																		, ; //X6_CONTEUD
	'F'																		, ; //X6_CONTSPA
	'F'																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_APURCOF'															, ; //X6_VAR
	'L'																		, ; //X6_TIPO
	'Define se a alíquota de COFINS deve'									, ; //X6_DESCRIC
	'Define si la alicuota de COFINS debe'									, ; //X6_DSCSPA
	'Defines if COFINS rate must'											, ; //X6_DSCENG
	'permanecer o padrão ou utilizar o conteúdo'							, ; //X6_DESC1
	'permanecer estandar o utilizar el contenido'							, ; //X6_DSCSPA1
	'remain the standard or use content'									, ; //X6_DSCENG1
	'do parâmetro MV_TXCOFIN para Apuração'									, ; //X6_DESC2
	'del parametro MV_TXCOFIN para Calculo'									, ; //X6_DSCSPA2
	'from parameter MV_TXCOFIN Calculation'									, ; //X6_DSCENG2
	'F'																		, ; //X6_CONTEUD
	'F'																		, ; //X6_CONTSPA
	'F'																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_APURPIS'															, ; //X6_VAR
	'L'																		, ; //X6_TIPO
	'Define se a alíquota de PIS deve'										, ; //X6_DESCRIC
	'Define si la alicuta de PIS debe'										, ; //X6_DSCSPA
	'Defines if PIS rate must'												, ; //X6_DSCENG
	'permanecer o padrão ou utilizar o conteúdo'							, ; //X6_DESC1
	'permanecer estandar o utilizar el contenido'							, ; //X6_DSCSPA1
	'remain the standard or use content'									, ; //X6_DSCENG1
	'do parâmetro MV_TXPIS para Apuração'									, ; //X6_DESC2
	'del parametro MV_TXPIS para Calculo'									, ; //X6_DSCSPA2
	'from parameter MV_TXPIS for Calculation'								, ; //X6_DSCENG2
	'F'																		, ; //X6_CONTEUD
	'F'																		, ; //X6_CONTSPA
	'F'																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_ATIVIDA'															, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'Informar o campo da tabela SA1 (Cadastro de Client'					, ; //X6_DESCRIC
	'Informar campo de tabla SA1 (Archivo de Clientes)'						, ; //X6_DSCSPA
	'Enter the SA1 table field (Customer File)'								, ; //X6_DSCENG
	'es) que se refere ao Codigo de Atividade.'								, ; //X6_DESC1
	'que se refiere al Codigo de A tividad.'								, ; //X6_DSCSPA1
	'referring to the Activity Code.'										, ; //X6_DSCENG1
	''																		, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	'SA1->A1_ATIVIDA'														, ; //X6_CONTEUD
	''																		, ; //X6_CONTSPA
	''																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_ATVCIMP'															, ; //X6_VAR
	'L'																		, ; //X6_TIPO
	'Alterar o vencimento dos titulos de impostos (IR,'						, ; //X6_DESCRIC
	'Modificar vencimiento de titulos de impuestos (IR,'					, ; //X6_DSCSPA
	'Change tax bills due date (IR, PIS, COFINS and'						, ; //X6_DSCENG
	'PIS,COFINS e CSLL) quando da baixa do titulo prin-'					, ; //X6_DESC1
	'PIS,COFINS y CSLL) al momento de baja titulo prin-'					, ; //X6_DSCSPA1
	'CSLL) while posting the main bill. T (True)'							, ; //X6_DSCENG1
	'cipal. T (True) = Alterar ou F(False) = Nao Altera'					, ; //X6_DESC2
	'cipal. T (True) = Modif. o F(False) = No Modif.'						, ; //X6_DSCSPA2
	'Edit or F (False) = Do Not Edit'										, ; //X6_DSCENG2
	'F'																		, ; //X6_CONTEUD
	'F'																		, ; //X6_CONTSPA
	'F'																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_AUTOSRC'															, ; //X6_VAR
	'L'																		, ; //X6_TIPO
	'Calcular somente para autônomos que possuam lançam'					, ; //X6_DESCRIC
	'Calcular solo para autonomos que posean registros'						, ; //X6_DSCSPA
	'Only calculate for freelancers that have monthly'						, ; //X6_DSCENG
	'entos mensais. .T. (True) Calcula apenas se possui'					, ; //X6_DESC1
	'mensuales. .T. (True) Calcula solo si posee'							, ; //X6_DSCSPA1
	'entries. .T. (True) Only calculates if assets have'					, ; //X6_DSCENG1
	'r movimento, .F. (False) Calcula todos os ativos.'						, ; //X6_DESC2
	'r movimiento, .F. (False) Calcula todos los activo'					, ; //X6_DSCSPA2
	'transactions., .F. (False) Calculates all assets.'						, ; //X6_DSCENG2
	'.F.'																	, ; //X6_CONTEUD
	'.F.'																	, ; //X6_CONTSPA
	'.F.'																	, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_BP10925'															, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'Define se deve considerar no valor de pagamento,'						, ; //X6_DESCRIC
	'Define si debe considerar en el valor de pago'							, ; //X6_DSCSPA
	'Defines if in the payment amount, the total amount'					, ; //X6_DSCENG
	'o valor bruto da baixa parcial ou o valor liquido'						, ; //X6_DESC1
	'el valor bruto de baja parcial o el valor neto con'					, ; //X6_DSCSPA1
	'of partial posting or the net amount must be consi'					, ; //X6_DSCENG1
	'considerando os impostos.(1=Vl Bruto/2=Vl Liquido)'					, ; //X6_DESC2
	'siderando los impuestos.(1=Vl Bruto/2=Vl Neto)'						, ; //X6_DSCSPA2
	'dering taxes. (1=Gross Amt/2=Net Amt)'									, ; //X6_DSCENG2
	'1'																		, ; //X6_CONTEUD
	'1'																		, ; //X6_CONTSPA
	'1'																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_BS10925'															, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'Indica se o calculo da retencao sera sobre a base'						, ; //X6_DESCRIC
	'Indica si el calculo de retencion es sobre la base'					, ; //X6_DSCSPA
	'Indicates it withholding calculation will be based'					, ; //X6_DSCENG
	'do PIS/COFINS/CSLL ou pelo valor total da duplica'						, ; //X6_DESC1
	'de PIS/COFINS/CSLL o por valor total de Fact Cred.'					, ; //X6_DSCSPA1
	'on PIS/COFINS/CSLL or by the total amount of the'						, ; //X6_DSCENG1
	'ta. 1=Valor da base; 2=Valor total'									, ; //X6_DESC2
	'1=Valor de la base; 2=Valor total'										, ; //X6_DSCSPA2
	'trade bill. 1=Base amount; 2=Total amount'								, ; //X6_DSCENG2
	'1'																		, ; //X6_CONTEUD
	'1'																		, ; //X6_CONTSPA
	'1'																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_BX10925'															, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'Define momento do tratamento da retencäo dos impos'					, ; //X6_DESCRIC
	'Define momento do tratamiento de retencion de impu'					, ; //X6_DSCSPA
	'Defines when the withholding of'										, ; //X6_DSCENG
	'tos Pis Cofins e Csll'													, ; //X6_DESC1
	'estos Pis Cofins y Csll'												, ; //X6_DSCSPA1
	'Pis, Cofins and Csll is calculated'									, ; //X6_DSCENG1
	'1 = Na Baixa ou 2 = Na Emissäo'										, ; //X6_DESC2
	'1 = En la Baja o 2 = En la Emision'									, ; //X6_DSCSPA2
	'1 = During write-off or 2 = During issue'								, ; //X6_DSCENG2
	'2'																		, ; //X6_CONTEUD
	'2'																		, ; //X6_CONTSPA
	'2'																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_CFOPPR'																, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	"Cfop's para geração do Registro Tipo 54 e Registro"					, ; //X6_DESCRIC
	'CFOP para generacion del Archivo Tipo 54 y Archivo'					, ; //X6_DSCSPA
	'CFOPs for generation of Registration Type 54 and'						, ; //X6_DSCENG
	'Tipo 75.'																, ; //X6_DESC1
	'Tipo 75.'																, ; //X6_DSCSPA1
	'Registration Type 75.'													, ; //X6_DSCENG1
	''																		, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	''																		, ; //X6_CONTEUD
	''																		, ; //X6_CONTSPA
	''																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_CIAPCFO'															, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	"Informar os CFOP's de venda para a emissao do"							, ; //X6_DESCRIC
	'Informar los CFOP de venta para la emision del'						, ; //X6_DSCSPA
	'Enter sale CFOPs for generation of CIAP report'						, ; //X6_DSCENG
	'relatorio CIAP modelo C.'												, ; //X6_DESC1
	'informe CIAP modelo C.'												, ; //X6_DSCSPA1
	'model C.'																, ; //X6_DSCENG1
	''																		, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	''																		, ; //X6_CONTEUD
	''																		, ; //X6_CONTSPA
	''																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_CIDADE'																, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'Informe o nome do município em que o contribuinte'						, ; //X6_DESCRIC
	'Informar en este parametro el nombre del municipio'					, ; //X6_DSCSPA
	'Enter the name of the city referring to the Tax'						, ; //X6_DSCENG
	'esta estabelecido.'													, ; //X6_DESC1
	'donde el Contribuyente esta establecido'								, ; //X6_DSCSPA1
	'Payer in this parameter'												, ; //X6_DSCENG1
	''																		, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	'BELO HORIZONTE'														, ; //X6_CONTEUD
	''																		, ; //X6_CONTSPA
	''																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_CNDBCON'															, ; //X6_VAR
	'L'																		, ; //X6_TIPO
	'Banco de conhecimento visivel para todas as reviso'					, ; //X6_DESCRIC
	''																		, ; //X6_DSCSPA
	''																		, ; //X6_DSCENG
	'es da medicao. Obs.: Este parametro e de uso do si'					, ; //X6_DESC1
	''																		, ; //X6_DSCSPA1
	''																		, ; //X6_DSCENG1
	'stema e nao deve ter seu conteudo alterado.'							, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	'.T.'																	, ; //X6_CONTEUD
	'.T.'																	, ; //X6_CONTSPA
	'.T.'																	, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_CNPJAUT'															, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	''																		, ; //X6_DESCRIC
	''																		, ; //X6_DSCSPA
	''																		, ; //X6_DSCENG
	''																		, ; //X6_DESC1
	''																		, ; //X6_DSCSPA1
	''																		, ; //X6_DSCENG1
	''																		, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	'25349440000180'														, ; //X6_CONTEUD
	'25349440000180'														, ; //X6_CONTSPA
	'25349440000180'														, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_CODDP'																, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'Código do município em que o contribuinte está loc'					, ; //X6_DESCRIC
	'Código de município en que el contribuyente está'						, ; //X6_DSCSPA
	'Code of municipality where the taxpayer is'							, ; //X6_DSCENG
	'alizado.'																, ; //X6_DESC1
	'localizado.'															, ; //X6_DSCSPA1
	'located.'																, ; //X6_DSCENG1
	''																		, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	'062'																	, ; //X6_CONTEUD
	'062'																	, ; //X6_CONTSPA
	'062'																	, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_CODISS'																, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'Informar o campo da tabela SF3 (Livros Fiscais) qu'					, ; //X6_DESCRIC
	'Informar campo de tabla SF3 (Libros Fiscales) que'						, ; //X6_DSCSPA
	'Enter the SF3 table field (Tax Records) referring'						, ; //X6_DSCENG
	'e se refere ao Codigo de Servico'										, ; //X6_DESC1
	'se refiere al Codigo de Servicio'										, ; //X6_DSCSPA1
	'to the Service Code'													, ; //X6_DSCENG1
	''																		, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	'SF3->F3_CODISS'														, ; //X6_CONTEUD
	''																		, ; //X6_CONTSPA
	''																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_DEDBCOF'															, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'Indica se retira o valor do ICMS/IPI creditavel da'					, ; //X6_DESCRIC
	'Indica si deduce valor de ICMS/IPI acreditable de'						, ; //X6_DSCSPA
	'Indic. if ded. ICMS/IPI value that can be credited'					, ; //X6_DSCENG
	'base de calculo do COFINS. Conteudos : S-Retira'						, ; //X6_DESC1
	'base de calculo de COFINS. Contenido : S-Deduce'						, ; //X6_DSCSPA1
	'from the COFINS tax basis. Contents: S-Deducts'						, ; //X6_DSCENG1
	'N=Nao retira,I=Ret. apenas ICMS,P=Ret. apenas IPI'						, ; //X6_DESC2
	'N=No deduce,I=Ded. solo ICMS,P=Deduce solo IPI'						, ; //X6_DSCSPA2
	'N=Does not deduct,I=Ded. only ICMS,P=Ded. only IPI'					, ; //X6_DSCENG2
	'I'																		, ; //X6_CONTEUD
	'N'																		, ; //X6_CONTSPA
	'N'																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_DEDBPIS'															, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'Indica se retira o valor do ICMS/IPI creditavel da'					, ; //X6_DESCRIC
	'Indica si retira el valor del ICMS de la base de'						, ; //X6_DSCSPA
	'Ind. if deducts ICMS/IPI value to be credited from'					, ; //X6_DSCENG
	'base de calculo do PIS. Conteudos : S-Retira'							, ; //X6_DESC1
	'calculo del PIS.Contenidos: S-Retira, N-No retira.'					, ; //X6_DSCSPA1
	'the PIS tax basis. Contents : S-Deducts;'								, ; //X6_DSCENG1
	'N=Nao retira,I=Ret. apenas ICMS,P=Ret. apenas IPI'						, ; //X6_DESC2
	'I=Ret. solo ICMS,P=Ret. solo IPI'										, ; //X6_DSCSPA2
	'N=Does not deduct;I=Ded. only ICMS,P=Ded. Only IPI'					, ; //X6_DSCENG2
	'I'																		, ; //X6_CONTEUD
	'N'																		, ; //X6_CONTSPA
	'N'																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_DESCFIN'															, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'Indica se o desconto financeiro sera aplicado inte'					, ; //X6_DESCRIC
	'Indica si el descuento financiero se aplicara'							, ; //X6_DSCSPA
	'It indicates whether the financial deduction is to'					, ; //X6_DSCENG
	'gral ("I") no primeiro pagamento, ou proporcional'						, ; //X6_DESC1
	'integral  ("I") en el primer pago o proporcional'						, ; //X6_DSCSPA1
	'be paid fully (F) on the first payment or'								, ; //X6_DSCENG1
	'("P") ao valor pago en cada parcela.'									, ; //X6_DESC2
	'("P") al valor pagado en cada cuota.'									, ; //X6_DSCSPA2
	'proportional (P) to the amt. paid on each installm'					, ; //X6_DSCENG2
	'I'																		, ; //X6_CONTEUD
	'I'																		, ; //X6_CONTSPA
	'I'																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_FINATFN'															, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'"1" = Fluxo Caixa On-Line,"2" = Fluxo Caixa Off-Li'					, ; //X6_DESCRIC
	'"1" = Flujo Caja On-Line,"2" = Flujo Caja Off-Line'					, ; //X6_DSCSPA
	'"1" = On-Line Cash Flow, "2" = Off-Line Cash'							, ; //X6_DSCENG
	'ne'																	, ; //X6_DESC1
	'.'																		, ; //X6_DSCSPA1
	'Flow'																	, ; //X6_DSCENG1
	''																		, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	'1'																		, ; //X6_CONTEUD
	'1'																		, ; //X6_CONTSPA
	'1'																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_FTPEND'																, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'Parâmetro de endereço do FTP'											, ; //X6_DESCRIC
	'Parametro de direccion del FTP'										, ; //X6_DSCSPA
	'FTP address parameter'													, ; //X6_DSCENG
	'"localhost" - padrão do sistema'										, ; //X6_DESC1
	'"localhost" - estandar del sistema'									, ; //X6_DSCSPA1
	'"localhost" - system default'											, ; //X6_DSCENG1
	''																		, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	'10.10.0.7'																, ; //X6_CONTEUD
	'localhost'																, ; //X6_CONTSPA
	'localhost'																, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_FTPPASS'															, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'Parâmetro do password do FTP'											, ; //X6_DESCRIC
	'Parametro de contrasena del FTP'										, ; //X6_DSCSPA
	'FTP password parameter'												, ; //X6_DSCENG
	'"test@test.com" - padrão do sistema'									, ; //X6_DESC1
	'"test@test.com" - estandar del sistema'								, ; //X6_DSCSPA1
	'"test@test.com" - system default'										, ; //X6_DSCENG1
	''																		, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	'test@test.com'															, ; //X6_CONTEUD
	'test@test.com'															, ; //X6_CONTSPA
	'test@test.com'															, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_FTPPORT'															, ; //X6_VAR
	'N'																		, ; //X6_TIPO
	'Parâmetro de compressão do arquivo'									, ; //X6_DESCRIC
	'Parametro de compresion del archivo'									, ; //X6_DSCSPA
	'File compression parameter'											, ; //X6_DSCENG
	'21 - padrão do sistema'												, ; //X6_DESC1
	'21 - estandar del sistema'												, ; //X6_DSCSPA1
	'21 - system default'													, ; //X6_DSCENG1
	''																		, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	'21'																	, ; //X6_CONTEUD
	'21'																	, ; //X6_CONTSPA
	'21'																	, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_FTPUSER'															, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'Parâmetro do usuário do FTP'											, ; //X6_DESCRIC
	'Parametro del usuario del FTP'											, ; //X6_DSCSPA
	'FTP user parameter'													, ; //X6_DSCENG
	'"Anonymous" - padrão do sistema'										, ; //X6_DESC1
	'"Anonymous" - estandar del sistema'									, ; //X6_DSCSPA1
	'"Anonymous" - system default'											, ; //X6_DSCENG1
	''																		, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	'Anonymous'																, ; //X6_CONTEUD
	'Anonymous'																, ; //X6_CONTSPA
	'Anonymous'																, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_HORANFE'															, ; //X6_VAR
	'L'																		, ; //X6_TIPO
	'Utilizado para gravar o campo F1_HORA no momento'						, ; //X6_DESCRIC
	'Utilizado para grabar el campo F1_HORA en el momen'					, ; //X6_DSCSPA
	'Used to record field F1_HORA in the moment'							, ; //X6_DSCENG
	'da inclusão do documento de entrada.'									, ; //X6_DESC1
	'to de la inclusion del doc. de entrada.'								, ; //X6_DSCSPA1
	'of inclusion of the inflow document.'									, ; //X6_DSCENG1
	''																		, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	'.T.'																	, ; //X6_CONTEUD
	'.T.'																	, ; //X6_CONTSPA
	'.T.'																	, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_INTGFE2'															, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'MODO DE INTEGRAÇÃO ENTRE PROTHEUS E GFE (1=DIRETO'						, ; //X6_DESCRIC
	'MODO INTEGRACION ENTRE PROTHEUS Y GFE (1=DIRECTO'						, ; //X6_DSCSPA
	'MODE OF INTEGRATION BETWEEN PROTHEUS AND GFE'							, ; //X6_DSCENG
	'2= XML)'																, ; //X6_DESC1
	'2= XML)'																, ; //X6_DSCSPA1
	'(1=DIRECT, 2=XML)'														, ; //X6_DSCENG1
	''																		, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	''																		, ; //X6_CONTEUD
	''																		, ; //X6_CONTSPA
	''																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	'1'																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_INTTRAN'															, ; //X6_VAR
	'L'																		, ; //X6_TIPO
	'Habilita a geracao das tags <veicTransp> e <reboqu'					, ; //X6_DESCRIC
	'Habilita a geracao das tags <veicTransp> e <reboqu'					, ; //X6_DSCSPA
	'Habilita a geracao das tags <veicTransp> e <reboqu'					, ; //X6_DSCENG
	'e>, em operacoes internas para NFESEFAZ.'								, ; //X6_DESC1
	'e>, em operacoes internas para NFESEFAZ.'								, ; //X6_DSCSPA1
	'e>, em operacoes internas para NFESEFAZ.'								, ; //X6_DSCENG1
	''																		, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	'.T.'																	, ; //X6_CONTEUD
	'.T.'																	, ; //X6_CONTSPA
	'.T.'																	, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_LIMDMAT'															, ; //X6_VAR
	'N'																		, ; //X6_TIPO
	'Limite de dedução para a licença-maternidade.'							, ; //X6_DESCRIC
	'Limite de deduccion para licencia por maternidad.'						, ; //X6_DSCSPA
	'Deduction threshold for maternity leave.'								, ; //X6_DSCENG
	''																		, ; //X6_DESC1
	''																		, ; //X6_DSCSPA1
	''																		, ; //X6_DSCENG1
	''																		, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	'24500.00'																, ; //X6_CONTEUD
	''																		, ; //X6_CONTSPA
	''																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_MGDI'																, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'Numero da Declaracao de Importacao'									, ; //X6_DESCRIC
	'Numero de la Declaracion de Importacion'								, ; //X6_DSCSPA
	'Import Statement Number'												, ; //X6_DSCENG
	''																		, ; //X6_DESC1
	''																		, ; //X6_DSCSPA1
	''																		, ; //X6_DSCENG1
	''																		, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	''																		, ; //X6_CONTEUD
	''																		, ; //X6_CONTSPA
	''																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_MINCOF'																, ; //X6_VAR
	'N'																		, ; //X6_TIPO
	'Valor base para geracäo de titulos de COFINS pela'						, ; //X6_DESCRIC
	'Valor base para generacion de titulos COFINS por'						, ; //X6_DSCSPA
	'Base amount for generation of COFINS bills through'					, ; //X6_DSCENG
	'rotina de apuracäo do PIS/COFINS. Lei 9.430/96,'						, ; //X6_DESC1
	'rutina de calculo de PIS/COFINS. Lei 9.430/96,'						, ; //X6_DSCSPA1
	'PIS/COFINS calculation routine. Law Nbr. 9,430/96,'					, ; //X6_DSCENG1
	'Art. 68.'																, ; //X6_DESC2
	'Art. 68.'																, ; //X6_DSCSPA2
	'section 68.'															, ; //X6_DSCENG2
	'0'																		, ; //X6_CONTEUD
	'0'																		, ; //X6_CONTSPA
	'0'																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_MINPIS'																, ; //X6_VAR
	'N'																		, ; //X6_TIPO
	'Valor base para geracao de titulos de PIS pela'						, ; //X6_DESCRIC
	'Valor base para generacion de titulos PIS por la'						, ; //X6_DSCSPA
	'Base amount for generation of PIS bills through'						, ; //X6_DSCENG
	'rotina de apuracäo do PIS/COFINS. Lei 9.430/96,'						, ; //X6_DESC1
	'rutina de calculo de PIS/COFINS. Ley 9.430/96,'						, ; //X6_DSCSPA1
	'the PIS/COFINS calculation routine. Law Nbr.'							, ; //X6_DSCENG1
	'Art. 68.'																, ; //X6_DESC2
	'Art. 68.'																, ; //X6_DSCSPA2
	'9,430/96, section 68.'													, ; //X6_DSCENG2
	'0'																		, ; //X6_CONTEUD
	'0'																		, ; //X6_CONTSPA
	'0'																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_MUDATRT'															, ; //X6_VAR
	'L'																		, ; //X6_TIPO
	'Indica se devera alterar o nome físico das tabelas'					, ; //X6_DESCRIC
	'Indica si se modifica el nombre fisico de tablas'						, ; //X6_DSCSPA
	'It indicates if physical name of temporary tables'						, ; //X6_DSCENG
	'temporarias utilizadas nas SPs T=Alterar F=Não'						, ; //X6_DESC1
	'temporarias utilizadas en las SP T=Modifica F=No'						, ; //X6_DSCSPA1
	'used in SPs must be changed.  T=Change F=Do Not'						, ; //X6_DSCENG1
	'Alterar'																, ; //X6_DESC2
	'Modificar'																, ; //X6_DSCSPA2
	'Change'																, ; //X6_DSCENG2
	'.T.'																	, ; //X6_CONTEUD
	''																		, ; //X6_CONTSPA
	''																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_NGREDUT'															, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'Categorias para as quais nao devem ser gerados'						, ; //X6_DESCRIC
	'Categorias para as quais nao devem ser gerados'						, ; //X6_DSCSPA
	'Categorias para as quais nao devem ser gerados'						, ; //X6_DSCENG
	'redutores de IR'														, ; //X6_DESC1
	''																		, ; //X6_DSCSPA1
	''																		, ; //X6_DSCENG1
	''																		, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	''																		, ; //X6_CONTEUD
	''																		, ; //X6_CONTSPA
	''																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_PROCSP'																, ; //X6_VAR
	'L'																		, ; //X6_TIPO
	'Indica se a manutencao de stored procedures sera r'					, ; //X6_DESCRIC
	'Indica se a manutencao de stored procedures sera r'					, ; //X6_DSCSPA
	'Indica se a manutencao de stored procedures sera r'					, ; //X6_DSCENG
	'ealizada por processo (.T.=Sim/.F.=Nao)'								, ; //X6_DESC1
	'ealizada por processo (.T.=Sim/.F.=Nao)'								, ; //X6_DSCSPA1
	'ealizada por processo (.T.=Sim/.F.=Nao)'								, ; //X6_DSCENG1
	''																		, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	'.T.'																	, ; //X6_CONTEUD
	'.T.'																	, ; //X6_CONTSPA
	'.T.'																	, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_RF10925'															, ; //X6_VAR
	'D'																		, ; //X6_TIPO
	'Data de referencia inicial para que os novos proce'					, ; //X6_DESCRIC
	'Fecha de referencia inicial para que nuevos proce-'					, ; //X6_DSCSPA
	'Initial reference date for new procedures about'						, ; //X6_DSCENG
	'dimentos quanto a retencao de PIS/COFINS/CSLL seja'					, ; //X6_DESC1
	'dimientos referentes retencion de PIS/COFINS/CSLL'						, ; //X6_DSCSPA1
	'wittholding concerning  PIS/COFINS/CSLL to be'							, ; //X6_DSCENG1
	'm aplicados.'															, ; //X6_DESC2
	'se apliquen.'															, ; //X6_DSCSPA2
	'applied.'																, ; //X6_DSCENG2
	'01/07/2015'															, ; //X6_CONTEUD
	'15/11/04'																, ; //X6_CONTSPA
	'15/11/04'																, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_RT10925'															, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'Modo padrao de operacao do sistema quanto a reten-'					, ; //X6_DESCRIC
	'Modo estandar de operacion del sistema referente a'					, ; //X6_DSCSPA
	'Operation standard mode of system on witthholding'						, ; //X6_DSCENG
	'cao de PIS/COFINS/CSLL. (1=Calculado p/sistema'						, ; //X6_DESC1
	'retencion de PIS/COFINS/CSLL. (1=Calculado p/sis.'						, ; //X6_DSCSPA1
	'of  PIS/COFINS/CSLL. (1=Calculated f/system,'							, ; //X6_DSCENG1
	'2=Efetua Retencao sempre, 3=Nao efetua retencao)'						, ; //X6_DESC2
	'2=Efectua Retencion siempre, 3=No efectua retenc.)'					, ; //X6_DSCSPA2
	'2=Always withhold, 3=Do not withhold)'									, ; //X6_DSCENG2
	'1'																		, ; //X6_CONTEUD
	'1'																		, ; //X6_CONTSPA
	'1'																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_RTIPESP'															, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'Indica por tipo de imposto financeiro se o valor'						, ; //X6_DESCRIC
	'Indica por tipo de impuesto financiero si el valor'					, ; //X6_DSCSPA
	'Indicates by type of financial tax if amount must'						, ; //X6_DSCENG
	'deve ser considerado na primeira parcela ou'							, ; //X6_DESC1
	'debe considerarse en la primera cuota o'								, ; //X6_DSCSPA1
	'be considered in the first installment or'								, ; //X6_DSCENG1
	'rateado.Posicoes:ISS/IRRF/INSS/CSLL/COF/PIS'							, ; //X6_DESC2
	'prorrateado.Posiciones:ISS/IRRF/INSS/CSLL/COF/PIS'						, ; //X6_DSCSPA2
	'apportioned. Positions: ISS/IRRF/INSS/CSLL/COF/PIS'					, ; //X6_DSCENG2
	'000000'																, ; //X6_CONTEUD
	'000000'																, ; //X6_CONTSPA
	'000000'																, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_RTIPFIN'															, ; //X6_VAR
	'L'																		, ; //X6_TIPO
	'Indica se no documento de saída os impostos finan-'					, ; //X6_DESCRIC
	'Indica si en el documento de salida los impuestos'						, ; //X6_DSCSPA
	'It indicates if, in the outflow document, the'							, ; //X6_DSCENG
	'ceiros devem ser atribuidos a primeira parcela ou'						, ; //X6_DESC1
	'financieros deben atribuirse a la 1a cuota o debe'						, ; //X6_DSCSPA1
	'financial taxes should be assigned to 1st installm'					, ; //X6_DSCENG1
	'rateado em todas as parcelas.'											, ; //X6_DESC2
	'prorratearse en todas las cuotas.'										, ; //X6_DSCSPA2
	'or prorated in all the installments.'									, ; //X6_DSCENG2
	'F'																		, ; //X6_CONTEUD
	'F'																		, ; //X6_CONTSPA
	'F'																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_SPDCOMP'															, ; //X6_VAR
	'L'																		, ; //X6_TIPO
	'Parâmetro de compressão do arquivo'									, ; //X6_DESCRIC
	'Parametro de compresion del archivo'									, ; //X6_DSCSPA
	'File compression parameter'											, ; //X6_DSCENG
	'.T. = Sim / .F. = Não'													, ; //X6_DESC1
	'.T. = Si / .F. = No'													, ; //X6_DSCSPA1
	'T = Yes / F = No'														, ; //X6_DSCENG1
	''																		, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	'.T.'																	, ; //X6_CONTEUD
	'.T.'																	, ; //X6_CONTSPA
	'.T.'																	, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_SPDFDIR'															, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'Diretório do FTP'														, ; //X6_DESCRIC
	'Directorio del FTP'													, ; //X6_DSCSPA
	'FTP diretory'															, ; //X6_DSCENG
	'Ex: "\web\ftpdped"'													, ; //X6_DESC1
	'Ej: "\web\ftpdped"'													, ; //X6_DSCSPA1
	'Ex: "\web\ftpdped"'													, ; //X6_DSCENG1
	''																		, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	'I:\ap_data\web\ftpsped'												, ; //X6_CONTEUD
	'I:\ap_data\web\ftpsped'												, ; //X6_CONTSPA
	'I:\ap_data\web\ftpsped'												, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_SPEDDOW'															, ; //X6_VAR
	'L'																		, ; //X6_TIPO
	'Parâmetro de verificação de download via client'						, ; //X6_DESCRIC
	'Parametro de verificacion de download por client'						, ; //X6_DSCSPA
	'Download per customer checking parameter'								, ; //X6_DSCENG
	'.T. = Sim / .F. = Não'													, ; //X6_DESC1
	'.T. = Si / .F. = No'													, ; //X6_DSCSPA1
	'T = Yes / F = No'														, ; //X6_DSCENG1
	''																		, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	'.T.'																	, ; //X6_CONTEUD
	'.T.'																	, ; //X6_CONTSPA
	'.T.'																	, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_TPALCOF'															, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'Indica como deve ser obtida a aliquota do COFINS'						, ; //X6_DESCRIC
	'Indica como debe obtenerse la alicuota del COFINS'						, ; //X6_DSCSPA
	'Inform how the COFINS will be calculated for reten'					, ; //X6_DSCENG
	'para retencao:1-apenas do cadastro de naturezas;2-'					, ; //X6_DESC1
	'para retencion:1-solo del archivo de modalidades;'						, ; //X6_DSCSPA1
	'-tion : 1-only from the class file; 2-MV_TXCOFIN'						, ; //X6_DSCENG1
	'cadastro de produtos ou de naturezas ou MV_TXCOFIN'					, ; //X6_DESC2
	'2-archivo de productos, modalidades o MV_TXCOFIN'						, ; //X6_DSCSPA2
	'or class or products record.'											, ; //X6_DSCENG2
	'2'																		, ; //X6_CONTEUD
	''																		, ; //X6_CONTSPA
	''																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_TPALPIS'															, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'Indica como deve ser obtida a aliquota do PIS para'					, ; //X6_DESCRIC
	'Indica como debe obenerse alicuota de PIS para'						, ; //X6_DSCSPA
	'Inform how the PIS will be calculated for reten-'						, ; //X6_DSCENG
	'retencao : 1-apenas do cadastro de naturezas;'							, ; //X6_DESC1
	'retencion: 1-solo del archivo de modalidades'							, ; //X6_DSCSPA1
	'tion : 1-only from the class file; 2-MV_TXPIS or'						, ; //X6_DSCENG1
	'2-cadastro de produtos ou de naturezas ou MV_TXPIS'					, ; //X6_DESC2
	'2-archivo de productos, modalidades o MV_TXPIS'						, ; //X6_DSCSPA2
	'2-cadastro de produtos ou de naturezas ou MV_TXPIS'					, ; //X6_DSCENG2
	'2'																		, ; //X6_CONTEUD
	''																		, ; //X6_CONTSPA
	''																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_VC11196'															, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'Determina se fará o cálculo de data dos impostos I'					, ; //X6_DESCRIC
	'Determina si hara el calculop/fecha de impuestos'						, ; //X6_DSCSPA
	'Determines if date will be calculated for Irrf,'						, ; //X6_DSCENG
	'rrf,Pis,Cofins,Csll conforme Lei 11196.1=Calcula o'					, ; //X6_DESC1
	'IRRF,PIS,COFINS,CSLL,segun ley 11196.1=Calcula los'					, ; //X6_DSCSPA1
	',Pis,Cofins,Csll according to Law 11196.1=Calculat'					, ; //X6_DSCENG1
	's vencimentos de acordo com a lei 11.196 2=Default'					, ; //X6_DESC2
	'vencimientos segun la ley 11.196 2=Default'							, ; //X6_DSCSPA2
	'es due dates according to Law 11.196 2=Default'						, ; //X6_DSCENG2
	'1'																		, ; //X6_CONTEUD
	'1'																		, ; //X6_CONTSPA
	'1'																		, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_VL10925'															, ; //X6_VAR
	'N'																		, ; //X6_TIPO
	'Valor maximo de pagamentos no periodo para dispen-'					, ; //X6_DESCRIC
	'Valor maximo de pagos en el periodo para dispensa'						, ; //X6_DSCSPA
	'Maximum value of payments within the period for'						, ; //X6_DSCENG
	'sa da retencao de PIS/COFINS/CSLL'										, ; //X6_DESC1
	'de retencion de PIS/COFINS/CSLL'										, ; //X6_DSCSPA1
	'releasing withholding of PIS/COFINS/CSLL.'								, ; //X6_DSCENG1
	''																		, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	'215,16'																, ; //X6_CONTEUD
	'5000'																	, ; //X6_CONTSPA
	'215,16'																, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'MV_XSEQREM'															, ; //X6_VAR
	'N'																		, ; //X6_TIPO
	'Sequencial utilizado no sispag Santanader'								, ; //X6_DESCRIC
	'Sequencial utilizado no sispag Santanader'								, ; //X6_DSCSPA
	'Sequencial utilizado no sispag Santanader'								, ; //X6_DSCENG
	'atualizado pelo programa U_CNAB033'									, ; //X6_DESC1
	'atualizado pelo programa U_CNAB033'									, ; //X6_DSCSPA1
	'atualizado pelo programa U_CNAB033'									, ; //X6_DSCENG1
	''																		, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	'369'																	, ; //X6_CONTEUD
	'369'																	, ; //X6_CONTSPA
	'369'																	, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

aAdd( aSX6, { ;
	'  '																	, ; //X6_FIL
	'US_CHAPAAF'															, ; //X6_VAR
	'C'																		, ; //X6_TIPO
	'Plaqueta para ativo gerencial'											, ; //X6_DESCRIC
	''																		, ; //X6_DSCSPA
	''																		, ; //X6_DSCENG
	''																		, ; //X6_DESC1
	''																		, ; //X6_DSCSPA1
	''																		, ; //X6_DSCENG1
	''																		, ; //X6_DESC2
	''																		, ; //X6_DSCSPA2
	''																		, ; //X6_DSCENG2
	'G00000'																, ; //X6_CONTEUD
	'G00000'																, ; //X6_CONTSPA
	'G00000'																, ; //X6_CONTENG
	'U'																		, ; //X6_PROPRI
	''																		, ; //X6_VALID
	''																		, ; //X6_INIT
	''																		, ; //X6_DEFPOR
	''																		, ; //X6_DEFSPA
	''																		, ; //X6_DEFENG
	''																		} ) //X6_PYME

//
// Atualizando dicionário
//
oProcess:SetRegua2( Len( aSX6 ) )

dbSelectArea( "SX6" )
dbSetOrder( 1 )

For nI := 1 To Len( aSX6 )
	lContinua := .F.
	lReclock  := .F.

	If !SX6->( dbSeek( PadR( aSX6[nI][1], nTamFil ) + PadR( aSX6[nI][2], nTamVar ) ) )
		lContinua := .T.
		lReclock  := .T.
		AutoGrLog( "Foi incluído o parâmetro " + aSX6[nI][1] + aSX6[nI][2] + " Conteúdo [" + AllTrim( aSX6[nI][13] ) + "]" )
	EndIf

	If lContinua
		If !( aSX6[nI][1] $ cAlias )
			cAlias += aSX6[nI][1] + "/"
		EndIf

		RecLock( "SX6", lReclock )
		For nJ := 1 To Len( aSX6[nI] )
			If FieldPos( aEstrut[nJ] ) > 0
				FieldPut( FieldPos( aEstrut[nJ] ), aSX6[nI][nJ] )
			EndIf
		Next nJ
		dbCommit()
		MsUnLock()
	EndIf

	oProcess:IncRegua2( "Atualizando Arquivos (SX6)..." )

Next nI

AutoGrLog( CRLF + "Final da Atualização" + " SX6" + CRLF + Replicate( "-", 128 ) + CRLF )

Return NIL


//--------------------------------------------------------------------
/*/{Protheus.doc} FSAtuHlp
Função de processamento da gravação dos Helps de Campos

@author TOTVS Protheus
@since  28/06/2018
@obs    Gerado por EXPORDIC - V.5.4.1.3 EFS / Upd. V.4.21.17 EFS
@version 1.0
/*/
//--------------------------------------------------------------------
Static Function FSAtuHlp()
Local aHlpPor   := {}
Local aHlpEng   := {}
Local aHlpSpa   := {}

AutoGrLog( "Ínicio da Atualização" + " " + "Helps de Campos" + CRLF )


oProcess:IncRegua2( "Atualizando Helps de Campos ..." )

//
// Helps Tabela SRV
//
AutoGrLog( CRLF + "Final da Atualização" + " " + "Helps de Campos" + CRLF + Replicate( "-", 128 ) + CRLF )

Return {}


//--------------------------------------------------------------------
/*/{Protheus.doc} EscEmpresa
Função genérica para escolha de Empresa, montada pelo SM0

@return aRet Vetor contendo as seleções feitas.
             Se não for marcada nenhuma o vetor volta vazio

@author Ernani Forastieri
@since  27/09/2004
@version 1.0
/*/
//--------------------------------------------------------------------
Static Function EscEmpresa()

//---------------------------------------------
// Parâmetro  nTipo
// 1 - Monta com Todas Empresas/Filiais
// 2 - Monta só com Empresas
// 3 - Monta só com Filiais de uma Empresa
//
// Parâmetro  aMarcadas
// Vetor com Empresas/Filiais pré marcadas
//
// Parâmetro  cEmpSel
// Empresa que será usada para montar seleção
//---------------------------------------------
Local   aRet      := {}
Local   aSalvAmb  := GetArea()
Local   aSalvSM0  := {}
Local   aVetor    := {}
Local   cMascEmp  := "??"
Local   cVar      := ""
Local   lChk      := .F.
Local   lOk       := .F.
Local   lTeveMarc := .F.
Local   oNo       := LoadBitmap( GetResources(), "LBNO" )
Local   oOk       := LoadBitmap( GetResources(), "LBOK" )
Local   oDlg, oChkMar, oLbx, oMascEmp, oSay
Local   oButDMar, oButInv, oButMarc, oButOk, oButCanc

Local   aMarcadas := {}


If !MyOpenSm0(.F.)
	Return aRet
EndIf


dbSelectArea( "SM0" )
aSalvSM0 := SM0->( GetArea() )
dbSetOrder( 1 )
dbGoTop()

While !SM0->( EOF() )

	If aScan( aVetor, {|x| x[2] == SM0->M0_CODIGO} ) == 0
		aAdd(  aVetor, { aScan( aMarcadas, {|x| x[1] == SM0->M0_CODIGO .and. x[2] == SM0->M0_CODFIL} ) > 0, SM0->M0_CODIGO, SM0->M0_CODFIL, SM0->M0_NOME, SM0->M0_FILIAL } )
	EndIf

	dbSkip()
End

RestArea( aSalvSM0 )

Define MSDialog  oDlg Title "" From 0, 0 To 280, 395 Pixel

oDlg:cToolTip := "Tela para Múltiplas Seleções de Empresas/Filiais"

oDlg:cTitle   := "Selecione a(s) Empresa(s) para Atualização"

@ 10, 10 Listbox  oLbx Var  cVar Fields Header " ", " ", "Empresa" Size 178, 095 Of oDlg Pixel
oLbx:SetArray(  aVetor )
oLbx:bLine := {|| {IIf( aVetor[oLbx:nAt, 1], oOk, oNo ), ;
aVetor[oLbx:nAt, 2], ;
aVetor[oLbx:nAt, 4]}}
oLbx:BlDblClick := { || aVetor[oLbx:nAt, 1] := !aVetor[oLbx:nAt, 1], VerTodos( aVetor, @lChk, oChkMar ), oChkMar:Refresh(), oLbx:Refresh()}
oLbx:cToolTip   :=  oDlg:cTitle
oLbx:lHScroll   := .F. // NoScroll

@ 112, 10 CheckBox oChkMar Var  lChk Prompt "Todos" Message "Marca / Desmarca"+ CRLF + "Todos" Size 40, 007 Pixel Of oDlg;
on Click MarcaTodos( lChk, @aVetor, oLbx )

// Marca/Desmarca por mascara
@ 113, 51 Say   oSay Prompt "Empresa" Size  40, 08 Of oDlg Pixel
@ 112, 80 MSGet oMascEmp Var  cMascEmp Size  05, 05 Pixel Picture "@!"  Valid (  cMascEmp := StrTran( cMascEmp, " ", "?" ), oMascEmp:Refresh(), .T. ) ;
Message "Máscara Empresa ( ?? )"  Of oDlg
oSay:cToolTip := oMascEmp:cToolTip

@ 128, 10 Button oButInv    Prompt "&Inverter"  Size 32, 12 Pixel Action ( InvSelecao( @aVetor, oLbx, @lChk, oChkMar ), VerTodos( aVetor, @lChk, oChkMar ) ) ;
Message "Inverter Seleção" Of oDlg
oButInv:SetCss( CSSBOTAO )
@ 128, 50 Button oButMarc   Prompt "&Marcar"    Size 32, 12 Pixel Action ( MarcaMas( oLbx, aVetor, cMascEmp, .T. ), VerTodos( aVetor, @lChk, oChkMar ) ) ;
Message "Marcar usando" + CRLF + "máscara ( ?? )"    Of oDlg
oButMarc:SetCss( CSSBOTAO )
@ 128, 80 Button oButDMar   Prompt "&Desmarcar" Size 32, 12 Pixel Action ( MarcaMas( oLbx, aVetor, cMascEmp, .F. ), VerTodos( aVetor, @lChk, oChkMar ) ) ;
Message "Desmarcar usando" + CRLF + "máscara ( ?? )" Of oDlg
oButDMar:SetCss( CSSBOTAO )
@ 112, 157  Button oButOk   Prompt "Processar"  Size 32, 12 Pixel Action (  RetSelecao( @aRet, aVetor ), IIf( Len( aRet ) > 0, oDlg:End(), MsgStop( "Ao menos um grupo deve ser selecionado", "UPDEXP" ) ) ) ;
Message "Confirma a seleção e efetua" + CRLF + "o processamento" Of oDlg
oButOk:SetCss( CSSBOTAO )
@ 128, 157  Button oButCanc Prompt "Cancelar"   Size 32, 12 Pixel Action ( IIf( lTeveMarc, aRet :=  aMarcadas, .T. ), oDlg:End() ) ;
Message "Cancela o processamento" + CRLF + "e abandona a aplicação" Of oDlg
oButCanc:SetCss( CSSBOTAO )

Activate MSDialog  oDlg Center

RestArea( aSalvAmb )
dbSelectArea( "SM0" )
dbCloseArea()

Return  aRet


//--------------------------------------------------------------------
/*/{Protheus.doc} MarcaTodos
Função auxiliar para marcar/desmarcar todos os ítens do ListBox ativo

@param lMarca  Contéudo para marca .T./.F.
@param aVetor  Vetor do ListBox
@param oLbx    Objeto do ListBox

@author Ernani Forastieri
@since  27/09/2004
@version 1.0
/*/
//--------------------------------------------------------------------
Static Function MarcaTodos( lMarca, aVetor, oLbx )
Local  nI := 0

For nI := 1 To Len( aVetor )
	aVetor[nI][1] := lMarca
Next nI

oLbx:Refresh()

Return NIL


//--------------------------------------------------------------------
/*/{Protheus.doc} InvSelecao
Função auxiliar para inverter a seleção do ListBox ativo

@param aVetor  Vetor do ListBox
@param oLbx    Objeto do ListBox

@author Ernani Forastieri
@since  27/09/2004
@version 1.0
/*/
//--------------------------------------------------------------------
Static Function InvSelecao( aVetor, oLbx )
Local  nI := 0

For nI := 1 To Len( aVetor )
	aVetor[nI][1] := !aVetor[nI][1]
Next nI

oLbx:Refresh()

Return NIL


//--------------------------------------------------------------------
/*/{Protheus.doc} RetSelecao
Função auxiliar que monta o retorno com as seleções

@param aRet    Array que terá o retorno das seleções (é alterado internamente)
@param aVetor  Vetor do ListBox

@author Ernani Forastieri
@since  27/09/2004
@version 1.0
/*/
//--------------------------------------------------------------------
Static Function RetSelecao( aRet, aVetor )
Local  nI    := 0

aRet := {}
For nI := 1 To Len( aVetor )
	If aVetor[nI][1]
		aAdd( aRet, { aVetor[nI][2] , aVetor[nI][3], aVetor[nI][2] +  aVetor[nI][3] } )
	EndIf
Next nI

Return NIL


//--------------------------------------------------------------------
/*/{Protheus.doc} MarcaMas
Função para marcar/desmarcar usando máscaras

@param oLbx     Objeto do ListBox
@param aVetor   Vetor do ListBox
@param cMascEmp Campo com a máscara (???)
@param lMarDes  Marca a ser atribuída .T./.F.

@author Ernani Forastieri
@since  27/09/2004
@version 1.0
/*/
//--------------------------------------------------------------------
Static Function MarcaMas( oLbx, aVetor, cMascEmp, lMarDes )
Local cPos1 := SubStr( cMascEmp, 1, 1 )
Local cPos2 := SubStr( cMascEmp, 2, 1 )
Local nPos  := oLbx:nAt
Local nZ    := 0

For nZ := 1 To Len( aVetor )
	If cPos1 == "?" .or. SubStr( aVetor[nZ][2], 1, 1 ) == cPos1
		If cPos2 == "?" .or. SubStr( aVetor[nZ][2], 2, 1 ) == cPos2
			aVetor[nZ][1] := lMarDes
		EndIf
	EndIf
Next

oLbx:nAt := nPos
oLbx:Refresh()

Return NIL


//--------------------------------------------------------------------
/*/{Protheus.doc} VerTodos
Função auxiliar para verificar se estão todos marcados ou não

@param aVetor   Vetor do ListBox
@param lChk     Marca do CheckBox do marca todos (referncia)
@param oChkMar  Objeto de CheckBox do marca todos

@author Ernani Forastieri
@since  27/09/2004
@version 1.0
/*/
//--------------------------------------------------------------------
Static Function VerTodos( aVetor, lChk, oChkMar )
Local lTTrue := .T.
Local nI     := 0

For nI := 1 To Len( aVetor )
	lTTrue := IIf( !aVetor[nI][1], .F., lTTrue )
Next nI

lChk := IIf( lTTrue, .T., .F. )
oChkMar:Refresh()

Return NIL


//--------------------------------------------------------------------
/*/{Protheus.doc} MyOpenSM0
Função de processamento abertura do SM0 modo exclusivo

@author TOTVS Protheus
@since  28/06/2018
@obs    Gerado por EXPORDIC - V.5.4.1.3 EFS / Upd. V.4.21.17 EFS
@version 1.0
/*/
//--------------------------------------------------------------------
Static Function MyOpenSM0(lShared)
Local lOpen := .F.
Local nLoop := 0

For nLoop := 1 To 20
	dbUseArea( .T., , "SIGAMAT.EMP", "SM0", lShared, .F. )

	If !Empty( Select( "SM0" ) )
		lOpen := .T.
		dbSetIndex( "SIGAMAT.IND" )
		Exit
	EndIf

	Sleep( 500 )

Next nLoop

If !lOpen
	MsgStop( "Não foi possível a abertura da tabela " + ;
	IIf( lShared, "de empresas (SM0).", "de empresas (SM0) de forma exclusiva." ), "ATENÇÃO" )
EndIf

Return lOpen


//--------------------------------------------------------------------
/*/{Protheus.doc} LeLog
Função de leitura do LOG gerado com limitacao de string

@author TOTVS Protheus
@since  28/06/2018
@obs    Gerado por EXPORDIC - V.5.4.1.3 EFS / Upd. V.4.21.17 EFS
@version 1.0
/*/
//--------------------------------------------------------------------
Static Function LeLog()
Local cRet  := ""
Local cFile := NomeAutoLog()
Local cAux  := ""

FT_FUSE( cFile )
FT_FGOTOP()

While !FT_FEOF()

	cAux := FT_FREADLN()

	If Len( cRet ) + Len( cAux ) < 1048000
		cRet += cAux + CRLF
	Else
		cRet += CRLF
		cRet += Replicate( "=" , 128 ) + CRLF
		cRet += "Tamanho de exibição maxima do LOG alcançado." + CRLF
		cRet += "LOG Completo no arquivo " + cFile + CRLF
		cRet += Replicate( "=" , 128 ) + CRLF
		Exit
	EndIf

	FT_FSKIP()
End

FT_FUSE()

Return cRet


/////////////////////////////////////////////////////////////////////////////
