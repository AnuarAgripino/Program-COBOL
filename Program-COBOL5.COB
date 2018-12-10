      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      ******************************************************************
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
      ******************************************************************
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
      ******************************************************************
       INPUT-OUTPUT SECTION.
      ******************************************************************
       FILE-CONTROL.
      *
           SELECT ENTRADA
                 ASSIGN TO
                    'C:\PastaCobol\Entrada03.txt'
                      ORGANIZATION IS LINE SEQUENTIAL
                      FILE STATUS IS WSN-FS-ENTRADA.
      *
           SELECT SAIDA
                 ASSIGN TO
                    'C:\PastaCobol\Saida03.txt'
                      ORGANIZATION IS LINE SEQUENTIAL
                      FILE STATUS IS WSN-FS-SAIDA.
      *
       DATA DIVISION.
      *
       FILE SECTION.
      *
       FD  ENTRADA.
      *
       01  REG-ENTRADA.
           05 COD-CTA                  PIC 9(04).
           05 TIP-CTA                  PIC X(14).
           05 SALDO                    PIC 9(06)V99.
      *
       FD  SAIDA.
      *
       01  REG-SAIDA                   PIC X(53).
      ******************************************************************
       WORKING-STORAGE SECTION.
      ******************************************************************
       01  WCA-VARIAVEIS.
           05 WSN-FS-ENTRADA           PIC 9(02) VALUE ZEROS.
           05 WSN-FS-SAIDA             PIC 9(02) VALUE ZEROS.
           05 V-TT-CC                  PIC 9(12)V99.
           05 V-TT-CP                  PIC 9(12)V99.
           05 SOMA-TOTAL               PIC 9(15)V99.
      ***********************************************
           05 SLD-MAIOR-CC             PIC 9(15)V99.
           05 SLD-MAIOR-COD-CC         PIC 9(04).
      *
           05 SLD-MAIOR-CP             PIC 9(15)V99.
           05 SLD-MAIOR-COD-CP         PIC 9(04).
      ***********************************************
           05 SLD-MENOR-CC             PIC 9(06)V99 VALUE 999999,99.
           05 SLD-MENOR-COD-CC         PIC 9(04).
      *
           05 SLD-MENOR-CP             PIC 9(06)V99 VALUE 999999,99.
           05 SLD-MENOR-COD-CP         PIC 9(04).
      ***********************************************
       01  WCA-CONSTANTES.
           05 WCA-PRINCIPAL            PIC X(10) VALUE 'PRINCIPAL'.
           05 WCA-INICIO               PIC X(10) VALUE 'INICIO'.
           05 WCA-FINALIZAR            PIC X(10) VALUE 'FINALIZAR'.
           05 WCA-PROCESSO             PIC X(10) VALUE 'PROCESSO'.
           05 WCA-LER-ENTRADA          PIC X(12) VALUE 'LER-ENTRADA'.
           05 WCA-GRAVAR-SAIDA         PIC X(12) VALUE 'GRAVAR-SAIDA'.
      *
           05 CONT-CC                  PIC 9(01) VALUE 1.
           05 CONT-CP                  PIC 9(01) VALUE 1.
      *
           05 QTD-CC                   PIC 9(03) VALUE ZERO.
           05 QTD-CP                   PIC 9(03) VALUE ZERO.
      *
           05 V-TT-CC-M                PIC ZZZ.ZZZ.ZZ9,99.
           05 V-TT-CP-M                PIC ZZZ.ZZZ.ZZ9,99.
      *
       01  WCA-TIP-CTA-SAIDA.
           05 TIP-CTA1                 PIC X(04) VALUE 'C/C'.
           05 TIP-CTA2                 PIC X(04) VALUE 'POUP'.
      *
      ***************    CABECALHOS  ***********************************
       01  WCA-LCAB.
           05 FILLER                  PIC X(01) VALUE '*'.
           05 WCA-LCABC PIC X(50) VALUE ALL '='.
           05 FILLER                  PIC X(01) VALUE '*'.
      *
       01  WCA-DATA.
           05 WCA-ANO                  PIC 9(04).
           05 WCA-MES                  PIC 9(02).
           05 WCA-DIA                  PIC 9(02).
      *
       01  CAB-01.
           03 FILLER                   PIC X(41) VALUE
              '*   RELATORIO MENSAL DO BANCO            '.
           03 CAB-DATA.
                05 CAB-DIA             PIC 9(02).
                05 FILLER              PIC X VALUE '/'.
                05 CAB-MES             PIC 9(02).
                05 FILLER              PIC X VALUE '/'.
                05 CAB-ANO             PIC 9(04).
           03 FILLER                   PIC X(01) VALUE '*'.

       01  CAB-02.
           03 FILLER                   PIC X(53) VALUE
              '*    CLIENTES COM SALDO MAIOR QUE R$ 100.000,00    *'.
       01  CAB-03.
           03 FILLER                   PIC X(53) VALUE
              '  CODIGO      TIPO DA CONTA       SALDO DA CONTA    '.
       01  CAB-04.
           03 FILLER                   PIC X(53) VALUE
              '*               RESUMO DAS CONTAS                  *'.
       01  CAB-05.
           03 FILLER                   PIC X(53) VALUE
              '*               DETALHES DE GERENTE                *'.
      ***************    CABECALHOS  ***********************************
      *
       01  DET-01.
           03  FILLER                  PIC X(03) VALUE SPACES.
           03  DET-CDO-CTA             PIC 9(04).
           03  FILLER                  PIC X(10) VALUE SPACES.
           03  DET-TIP-CTA             PIC X(16).
           03  FILLER                  PIC X(01) VALUE SPACES.
           03  FILLER                  PIC X(04) VALUE 'R$'.
           03  DET-SLD-CTA             PIC ZZZ.ZZ9,99.
      ***********************************************
       01  DET-RC-01.
           03  FILLER                  PIC X(02) VALUE SPACES.
           03 FILLER                   PIC X(28) VALUE
                'TOTAL DE CONTAS CORRENTES = '.
           03 DET-QTD-CC               PIC ZZ9.
      *
       01  DET-RC-02.
           03 FILLER                   PIC X(02) VALUE SPACES.
           03 FILLER                   PIC X(28) VALUE
                'TOTAL DE POUPANCAS        = '.
           03 DET-QTD-CP               PIC ZZ9.
      ***********************************************
       01  DET-RC-03.
           03 FILLER                   PIC X(02) VALUE SPACES.
           03 FILLER                   PIC X(34) VALUE
                'SALDO TOTAL DE CONTA CORRENTE = R$'.
           03 DET-V-TT-CC              PIC ZZZ.ZZZ.ZZ9,99.
      *
       01  DET-RC-04.
           03 FILLER                   PIC X(02) VALUE SPACES.
           03 FILLER                   PIC X(34) VALUE
                'SALDO TOTAL DE POUPANCA = R$      '.
           03 DET-V-TT-CP              PIC ZZZ.ZZZ.ZZ9,99.
      *
       01  DET-RC-05.
           03 FILLER                   PIC X(02) VALUE SPACES.
           03 FILLER                   PIC X(34) VALUE
                'SALDO TOTAL DAS CONTAS = R$      '.
           03 DET-SOMA-TT              PIC ZZZ.ZZZ.ZZ9,99.
      ***********************************************
       01  DET-RC-06.
           03 FILLER                   PIC X(02) VALUE SPACES.
           03 FILLER                   PIC X(41) VALUE
                'CLIENTE COM MAIOR SALDO EM CONTA CORRENTE'.
      *
       01  DET-M-SCC.
           03 FILLER                   PIC X(10) VALUE SPACES.
           03 COD-CC-MA                PIC 9(04).
           03 FILLER                   PIC X(04) VALUE SPACES.
           03 FILLER                   PIC X(02) VALUE 'R$'.
           03 VALOR-CC-MA              PIC ZZZ.ZZZ.ZZ9,99.
      *
       01  DET-RC-07.
           03 FILLER                   PIC X(02) VALUE SPACES.
           03 FILLER                   PIC X(41) VALUE
                'CLIENTE COM MAIOR SALDO EM POUPANCA'.
      *
       01  DET-M-SCP.
           03 FILLER                   PIC X(10) VALUE SPACES.
           03 COD-CP-MA                PIC 9(04).
           03 FILLER                   PIC X(04) VALUE SPACES.
           03 FILLER                   PIC X(02) VALUE 'R$'.
           03 VALOR-CP-MA              PIC ZZZ.ZZZ.ZZ9,99.
      *
       01  DET-RC-08.
           03 FILLER                   PIC X(02) VALUE SPACES.
           03 FILLER                   PIC X(41) VALUE
                'CLIENTE COM MENOR SALDO EM CONTA CORRENTE'.
      *
       01  DET-MN-SCC.
           03 FILLER                   PIC X(10) VALUE SPACES.
           03 COD-CC-MN                PIC 9(04).
           03 FILLER                   PIC X(04) VALUE SPACES.
           03 FILLER                   PIC X(02) VALUE 'R$'.
           03 VALOR-CC-MN              PIC ZZZ.ZZZ.ZZ9,99.
      *
           01  DET-RC-09.
           03 FILLER                   PIC X(02) VALUE SPACES.
           03 FILLER                   PIC X(41) VALUE
                'CLIENTE COM MENOR SALDO EM POUPANCA'.
      *
       01  DET-MN-SCP.
           03 FILLER                   PIC X(10) VALUE SPACES.
           03 COD-CP-MN                PIC 9(04).
           03 FILLER                   PIC X(04) VALUE SPACES.
           03 FILLER                   PIC X(02) VALUE 'R$'.
           03 VALOR-CP-MN              PIC ZZZ.ZZZ.ZZ9,99.
      *
       01  RG-ENT.
           03  ENT-CDO-CTA             PIC 9(04).
           03  ENT-TIP-CTA             PIC X(14).
           03  ENT-SLD-CTA             PIC 9(06)V99.
      *
       01  WSW-SWITCHES.
           05 SW-ENTRADA               PIC X(01) VALUE 'N'.
           88 SW-FIM-NAO                         VALUE 'N'.
           88 SW-FIM-SIM                         VALUE 'S'.
      ******************************************************************
       PROCEDURE DIVISION.
      ******************************************************************
       0000-PRINCIPAL.
      *
           DISPLAY WCA-PRINCIPAL
      *
           PERFORM 1000-INICIO
           THRU 1000-INICIO-FIM
      *
           PERFORM 2000-PROCESSO
           THRU 2000-PROCESSO-FIM
           UNTIL SW-FIM-SIM
      *
           PERFORM 3000-FINALIZAR
           THRU 3000-FINALIZAR-FIM
      *
           STOP RUN
      *
           .
      ******************************************************************
       1000-INICIO.
      ******************************************************************
           DISPLAY WCA-INICIO

           SET SW-FIM-NAO TO TRUE
           OPEN INPUT ENTRADA
           OPEN OUTPUT SAIDA
      *
           PERFORM 1100-LER-ENTRADA
           THRU 1100-LER-ENTRADA-FIM
      *
           PERFORM 1200-CABECALHO
           PERFORM 1200-CABECALHO-FIM
           .
      *
       1000-INICIO-FIM.
           EXIT.
      ******************************************************************
      *1100-LER-ENTRADA - LE A ENTRADA
      ******************************************************************
       1100-LER-ENTRADA.
           READ ENTRADA
              AT END
                 SET SW-FIM-SIM TO TRUE
              END-READ
      *
           .
      *
       1100-LER-ENTRADA-FIM.
           EXIT.
      *
       1200-CABECALHO.
      *
           INITIALIZE REG-SAIDA
      *
           ACCEPT WCA-DATA FROM DATE YYYYMMDD
           DISPLAY WCA-DATA
           MOVE WCA-ANO TO CAB-ANO
           MOVE WCA-MES TO CAB-MES
           MOVE WCA-DIA TO CAB-DIA
      ***************    CABECALHOS  ***********************************
           MOVE WCA-LCAB TO REG-SAIDA
           PERFORM 2100-GRAVA-SAIDA
              THRU 2100-GRAVA-SAIDA-FIM
      *
           MOVE CAB-01 TO REG-SAIDA
           PERFORM 2100-GRAVA-SAIDA
              THRU 2100-GRAVA-SAIDA-FIM
      *
           MOVE WCA-LCAB TO REG-SAIDA
           PERFORM 2100-GRAVA-SAIDA
              THRU 2100-GRAVA-SAIDA-FIM
      ***************    CABECALHOS  ***********************************
           MOVE CAB-02 TO REG-SAIDA
           PERFORM 2100-GRAVA-SAIDA
              THRU 2100-GRAVA-SAIDA-FIM
      *
           MOVE WCA-LCAB TO REG-SAIDA
           PERFORM 2100-GRAVA-SAIDA
              THRU 2100-GRAVA-SAIDA-FIM
      ***************    CABECALHOS  ***********************************
           MOVE CAB-03 TO REG-SAIDA
           PERFORM 2100-GRAVA-SAIDA
              THRU 2100-GRAVA-SAIDA-FIM
      ***************    CABECALHOS  ***********************************
           INITIALIZE REG-SAIDA
      *
           .
      *
       1200-CABECALHO-FIM.
           EXIT.
      *
       2000-PROCESSO.
      *
           DISPLAY WCA-PROCESSO
               ' ' REG-ENTRADA
      *
           MOVE REG-ENTRADA
             TO REG-SAIDA
           INITIALIZE REG-SAIDA
      *
           MOVE REG-ENTRADA TO RG-ENT

           MOVE ENT-CDO-CTA
             TO DET-CDO-CTA
           MOVE ENT-TIP-CTA
             TO DET-TIP-CTA
           MOVE ENT-SLD-CTA
             TO DET-SLD-CTA
      *
           IF TIP-CTA EQUAL 'Conta Corrente'
                MOVE TIP-CTA1 TO DET-TIP-CTA
                ADD CONT-CC TO QTD-CC
      *
                ADD ENT-SLD-CTA TO V-TT-CC
                MOVE V-TT-CC TO V-TT-CC-M
                DISPLAY 'V-TT-CC-M =' V-TT-CC-M
      *
                IF SALDO > SLD-MAIOR-CC
                DISPLAY SLD-MAIOR-CC
                MOVE SALDO TO SLD-MAIOR-CC
                MOVE COD-CTA TO SLD-MAIOR-COD-CC
           END-IF
      *
                IF SALDO < SLD-MENOR-CC
                DISPLAY SLD-MENOR-CC
                MOVE SALDO TO SLD-MENOR-CC
                MOVE COD-CTA TO SLD-MENOR-COD-CC
           END-IF
      ***********************************
           ELSE
                MOVE TIP-CTA2 TO DET-TIP-CTA
                ADD CONT-CP TO QTD-CP
      *
                ADD ENT-SLD-CTA TO V-TT-CP
                MOVE V-TT-CP TO V-TT-CP-M
                DISPLAY 'V-TT-CP-M =' V-TT-CP-M
      *
           IF SALDO > SLD-MAIOR-CP
                DISPLAY SLD-MAIOR-CP
                MOVE SALDO TO SLD-MAIOR-CP
                MOVE COD-CTA TO SLD-MAIOR-COD-CP
           END-IF
      *
           IF SALDO < SLD-MENOR-CP
                DISPLAY SLD-MENOR-CP
                MOVE SALDO TO SLD-MENOR-CP
                MOVE COD-CTA TO SLD-MENOR-COD-CP
           END-IF
      *
           END-IF
      *
           IF SALDO > 100000
                DISPLAY SALDO
                MOVE DET-01 TO REG-SAIDA
                PERFORM 2100-GRAVA-SAIDA
                     THRU 2100-GRAVA-SAIDA-FIM
           END-IF
      *
           PERFORM 1100-LER-ENTRADA
              THRU 1100-LER-ENTRADA-FIM
      *
           .
      *
       2000-PROCESSO-FIM.
           EXIT.
      *
       2100-GRAVA-SAIDA.
      *
           DISPLAY WCA-GRAVAR-SAIDA
           WRITE REG-SAIDA
           .
      *
       2100-GRAVA-SAIDA-FIM.
           EXIT.
      *
       3000-FINALIZAR.
      *
           DISPLAY WCA-FINALIZAR
      ***************    CABECALHO   ***********************************
           MOVE WCA-LCAB TO REG-SAIDA
           PERFORM 2100-GRAVA-SAIDA
              THRU 2100-GRAVA-SAIDA-FIM
      *
           MOVE CAB-04 TO REG-SAIDA
                PERFORM 2100-GRAVA-SAIDA
                     THRU 2100-GRAVA-SAIDA-FIM
      *
           MOVE WCA-LCAB TO REG-SAIDA
                PERFORM 2100-GRAVA-SAIDA
                     THRU 2100-GRAVA-SAIDA-FIM
      ***************    CABECALHO   ***********************************
           MOVE QTD-CC TO DET-QTD-CC
           MOVE DET-RC-01 TO REG-SAIDA
                PERFORM 2100-GRAVA-SAIDA
                     THRU 2100-GRAVA-SAIDA-FIM
      *
           MOVE QTD-CP TO DET-QTD-CP
           MOVE DET-RC-02 TO REG-SAIDA
                PERFORM 2100-GRAVA-SAIDA
                     THRU 2100-GRAVA-SAIDA-FIM
      ******************************///*********************************
           INITIALIZE REG-SAIDA.
                 PERFORM 2100-GRAVA-SAIDA
                     THRU 2100-GRAVA-SAIDA-FIM
      *****************************///**********************************
           MOVE V-TT-CC-M TO DET-V-TT-CC
           MOVE DET-RC-03 TO REG-SAIDA
                PERFORM 2100-GRAVA-SAIDA
                     THRU 2100-GRAVA-SAIDA-FIM
      *
           MOVE V-TT-CP-M TO DET-V-TT-CP
           MOVE DET-RC-04 TO REG-SAIDA
                PERFORM 2100-GRAVA-SAIDA
                     THRU 2100-GRAVA-SAIDA-FIM
      ******************************///*********************************
           INITIALIZE REG-SAIDA.
                 PERFORM 2100-GRAVA-SAIDA
                     THRU 2100-GRAVA-SAIDA-FIM
      *****************************///**********************************
           COMPUTE SOMA-TOTAL = V-TT-CC + V-TT-CP.
           MOVE SOMA-TOTAL TO DET-SOMA-TT
           MOVE DET-RC-05 TO REG-SAIDA
                PERFORM 2100-GRAVA-SAIDA
                     THRU 2100-GRAVA-SAIDA-FIM
      ******************************///*********************************
           INITIALIZE REG-SAIDA.
                 PERFORM 2100-GRAVA-SAIDA
                     THRU 2100-GRAVA-SAIDA-FIM
      *****************************///**********************************
      ***************    CABECALHO   ***********************************
           MOVE WCA-LCAB TO REG-SAIDA
           PERFORM 2100-GRAVA-SAIDA
              THRU 2100-GRAVA-SAIDA-FIM
      *
           MOVE CAB-05 TO REG-SAIDA
                PERFORM 2100-GRAVA-SAIDA
                     THRU 2100-GRAVA-SAIDA-FIM
      *
           MOVE WCA-LCAB TO REG-SAIDA
                PERFORM 2100-GRAVA-SAIDA
                     THRU 2100-GRAVA-SAIDA-FIM
      ***************    CABECALHO   ***********************************
           MOVE DET-RC-06 TO REG-SAIDA
                PERFORM 2100-GRAVA-SAIDA
                     THRU 2100-GRAVA-SAIDA-FIM
      *
           MOVE SLD-MAIOR-COD-CC TO COD-CC-MA
           MOVE SLD-MAIOR-CC TO VALOR-CC-MA
           MOVE DET-M-SCC TO REG-SAIDA
                PERFORM 2100-GRAVA-SAIDA
                     THRU 2100-GRAVA-SAIDA-FIM
      ******************************///*********************************
           INITIALIZE REG-SAIDA.
                 PERFORM 2100-GRAVA-SAIDA
                     THRU 2100-GRAVA-SAIDA-FIM
      *****************************///**********************************
           MOVE DET-RC-07 TO REG-SAIDA
                PERFORM 2100-GRAVA-SAIDA
                     THRU 2100-GRAVA-SAIDA-FIM
      *
           MOVE SLD-MAIOR-COD-CP TO COD-CP-MA
           MOVE SLD-MAIOR-CP TO VALOR-CP-MA
           MOVE DET-M-SCP TO REG-SAIDA
                PERFORM 2100-GRAVA-SAIDA
                     THRU 2100-GRAVA-SAIDA-FIM
      ******************************///*********************************
           INITIALIZE REG-SAIDA.
                 PERFORM 2100-GRAVA-SAIDA
                     THRU 2100-GRAVA-SAIDA-FIM
      *****************************///**********************************
           MOVE DET-RC-08 TO REG-SAIDA
                PERFORM 2100-GRAVA-SAIDA
                     THRU 2100-GRAVA-SAIDA-FIM
      *
           MOVE SLD-MENOR-COD-CC TO COD-CC-MN
           MOVE SLD-MENOR-CC TO VALOR-CC-MN
           MOVE DET-MN-SCC TO REG-SAIDA
                PERFORM 2100-GRAVA-SAIDA
                     THRU 2100-GRAVA-SAIDA-FIM
      ******************************///*********************************
           INITIALIZE REG-SAIDA.
                 PERFORM 2100-GRAVA-SAIDA
                     THRU 2100-GRAVA-SAIDA-FIM
      *****************************///**********************************
           MOVE DET-RC-09 TO REG-SAIDA
                PERFORM 2100-GRAVA-SAIDA
                     THRU 2100-GRAVA-SAIDA-FIM
      *
           MOVE SLD-MENOR-COD-CP TO COD-CP-MN
           MOVE SLD-MENOR-CP TO VALOR-CP-MN
           MOVE DET-MN-SCP TO REG-SAIDA
                PERFORM 2100-GRAVA-SAIDA
                     THRU 2100-GRAVA-SAIDA-FIM
      *
           CLOSE ENTRADA
                 SAIDA
           .
      *
       3000-FINALIZAR-FIM.
           EXIT.
      *
