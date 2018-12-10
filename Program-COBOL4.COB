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
                    'C:\PastaCobol\Entrada02.txt'
                      ORGANIZATION IS LINE SEQUENTIAL
                      FILE STATUS IS WSN-FS-ENTRADA.
      *
           SELECT SAIDA
                 ASSIGN TO
                    'C:\PastaCobol\Saida02.txt'
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
           05 COD-CTA         PIC 9(04).
           05 TIP-CTA         PIC X(16).
           05 SALDO           PIC 9(06)V99.
      *
       FD  SAIDA.
      *
       01  REG-SAIDA                       PIC X(50).
      ******************************************************************
       WORKING-STORAGE SECTION.
      ******************************************************************
       01  WCA-VARIAVEIS.
           05 WSN-FS-ENTRADA               PIC 9(02) VALUE ZEROS.
           05 WSN-FS-SAIDA                 PIC 9(02) VALUE ZEROS.
      *
       01  WCA-CONSTANTES.
           05 WCA-PRINCIPAL            PIC X(10) VALUE 'PRINCIPAL'.
           05 WCA-INICIO               PIC X(10) VALUE 'INICIO'.
           05 WCA-FINALIZAR            PIC X(10) VALUE 'FINALIZAR'.
           05 WCA-PROCESSO             PIC X(10) VALUE 'PROCESSO'.
           05 WCA-LER-ENTRADA          PIC X(12) VALUE 'LER-ENTRADA'.
           05 WCA-GRAVAR-SAIDA         PIC X(12) VALUE 'GRAVAR-SAIDA'.
      *
       01  CAB-01.
           03 FILLER                   PIC X(39) VALUE
              'CODIGO  TIPO DA CONTA   SALDO DA CONTA'.
      *
       01  DET-01.
           03  DET-CDO-CTA             PIC 9(04).
           03  FILLER                  PIC X(04) VALUE SPACES.
           03  DET-TIP-CTA             PIC X(16).
           03  FILLER                  PIC X(04) VALUE 'R$'.
           03  DET-SLD-CTA             PIC ZZZ.ZZ9,99.

      *
       01  RG-ENT.
           03  ENT-CDO-CTA             PIC 9(04).
           03  ENT-TIP-CTA             PIC X(14).
           03  ENT-SLD-CTA             PIC 9(06)V99.
      *
       01  WSW-SWITCHES.
           05 SW-ENTRADA                   PIC X(01) VALUE 'N'.
           88 SW-FIM-NAO                             VALUE 'N'.
           88 SW-FIM-SIM                             VALUE 'S'.
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
           MOVE CAB-01 TO REG-SAIDA
      *
           PERFORM 2100-GRAVA-SAIDA
              THRU 2100-GRAVA-SAIDA-FIM
      *
           INITIALIZE REG-SAIDA
      *
           PERFORM 2100-GRAVA-SAIDA
              THRU 2100-GRAVA-SAIDA-FIM
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
           MOVE DET-01 TO REG-SAIDA
      *
           PERFORM 2100-GRAVA-SAIDA
              THRU 2100-GRAVA-SAIDA-FIM
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
           CLOSE ENTRADA
                 SAIDA
           .
      *
       3000-FINALIZAR-FIM.
           EXIT.
