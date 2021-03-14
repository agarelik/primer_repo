************************************************************************
***   * FO6813 12/07/12 PAMH PROYECTO BUS DE SERVICIO EMPRESARIAL      *
************************************************************************
*IDAPL*BSE
*OBJET*****************************************************************
*OBJET*** OPERACION CONSULTARDESCUENTOLETRAFACTURA                   **
*OBJET*****************************************************************
      *=======================*
       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID. BSEO006.
      *====================*
       ENVIRONMENT DIVISION.
      *====================*
       DATA DIVISION.
      *=============*
       WORKING-STORAGE SECTION.
      *=======================*
       01  WE-ESPECIALES.
           02  WE-BLANCO               PIC X(01)  VALUE SPACES.
           02  WE-RC                   PIC S9(08) COMP VALUE ZEROS.
      *------------------ TABLA DE ERRORES ---------------------------*
       01  WT01-TABLA-MENSAJES.
           02  FILLER                  PIC X(64)  VALUE
                '001*TRANSACCION EXITOSA
      -         '-BSEO006 '.
           02  FILLER                  PIC X(64)  VALUE
                '002*ERROR EN RUTINA
      -         '-BSEO006 '.
           02  FILLER                  PIC X(64)  VALUE
                '003*ERROR EN RUTINA
      -         '-PROCRM29'.
           02  FILLER                  PIC X(64)  VALUE
                '004*ERROR AL LLAMAR PROGRAMA XXXXXXXX
      -         '-BSEO006 '.
           02  FILLER                  PIC X(64)  VALUE
                '005*APLICATIVO DEBE SER $PR
      -         '-BSEO006 '.
           02  FILLER                  PIC X(64)  VALUE
                '006*ENTIDAD DEBE SER 003
      -         '-BSEO006 '.
       01  FILLER  REDEFINES  WT01-TABLA-MENSAJES.
           02  FILLER  OCCURS   6  TIMES.
               04  WT01-COD-MSG        PIC 9(03).
               04  FILLER              PIC X(01).
               04  WT01-TXT-MSG.
                   06  WT01-MSG-DSC    PIC X(51).
                   06  WT01-MSG-PRG    PIC X(09).
      *--- COPY PARA RUTINA CLLOCRM02 ($PR) --------------------------*
           COPY CL02CRM.
      *------------------ COMMAREA -----------------------------------*
           COPY BSECCOM.
      *------------------ ESTRUCTURA DE DATOS DE TRAMAS BSEO006 ------*
           COPY BSEC006I.
           COPY BSEC006O.
      *------------------ VARIABLE DE CICS ---------------------------*
           COPY DFHAID.
           COPY DFHBMSCA.
      *---------------*
       LINKAGE SECTION.
      *---------------*
       01  DFHCOMMAREA          PIC X(16384).
      *------------------*
       PROCEDURE DIVISION.
      *------------------*
           PERFORM  INICIAR-RUTINA.
           PERFORM  PROCESAR-RUTINA.
           PERFORM  TERMINAR-RUTINA.
      *--------------*
       INICIAR-RUTINA.
      *--------------*
           EXEC CICS HANDLE CONDITION  ERROR (ERROR-EN-CICS)
                                       END-EXEC.
           IF EIBCALEN = 0
              PERFORM  BLOQUEAR-INGRESO
           END-IF.
           INSPECT DFHCOMMAREA REPLACING ALL LOW-VALUES BY SPACES.
           MOVE DFHCOMMAREA            TO REG-COMMAREA-BSE.
           MOVE BSE-DATOS              TO REG-BSEC006I.
           MOVE SPACES                 TO BSE-DATOS.
           MOVE SPACES                 TO REG-BSEC006O.
           INITIALIZE                     REG-BSEC006O.
           MOVE 00                     TO BSE-CODIGO-RESPUESTA.
           MOVE WT01-COD-MSG (001)     TO BSE-CODIGO-MENSAJE.
           MOVE WT01-TXT-MSG (001)     TO BSE-DESCR-MENSAJE.
      *---------------*
       PROCESAR-RUTINA.
      *---------------*
      *    PERFORM  VALIDAR-DATOS.
           PERFORM  PROCESAR-CONSULTA.
           PERFORM  ASIGNAR-BSE-DATOS.
      *-------------*
       VALIDAR-DATOS.
      *-------------*
      *--- VALIDAMOS APLICATIVO
           IF BSEC006I-ID-APLI NOT = '$PR'
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (005)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (005)  TO BSE-DESCR-MENSAJE
              PERFORM TERMINAR-RUTINA
           END-IF.
      *--- VALIDAMOS ENTIDAD
           IF BSEC006I-CODENT NOT = '003'
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (006)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (006)  TO BSE-DESCR-MENSAJE
              PERFORM TERMINAR-RUTINA
           END-IF.
      *-----------------*
       PROCESAR-CONSULTA.
      *-----------------*
           INITIALIZE                    CRM-CO-COMMAREA.
           MOVE 029                   TO CRM-CO-NRO-TRAMA.
           MOVE BSEC006I-ID-APLI      TO CRM-CO-ID-APLI.
           MOVE BSEC006I-CODENT       TO CRM-CO-ID-BANCO.
           MOVE BSEC006I-TI-DOCU-PR   TO CRM-CO-TIPDOC.
           MOVE BSEC006I-NU-DOCU-PR   TO CRM-CO-NUMERO.
           EXEC CICS LINK PROGRAM ('CLOCRM02')
                          COMMAREA(CRM-CO-COMMAREA)
                          LENGTH  (LENGTH OF CRM-CO-COMMAREA)
                          RESP    (WE-RC)
           END-EXEC.
           IF WE-RC NOT = DFHRESP(NORMAL)
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (004)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (004)  TO BSE-DESCR-MENSAJE
              MOVE 'CLOCRM02'          TO BSE-DESCR-MENSAJE (26:8)
              PERFORM  TERMINAR-RUTINA
           END-IF.
           EVALUATE  CRM-CO-COD-RETORNO
              WHEN +0000
                    PERFORM ARMAR-TRAMA
              WHEN  OTHER
                    MOVE WT01-COD-MSG (003) TO BSE-CODIGO-MENSAJE
                    MOVE CRM-CO-MENSAJE     TO WT01-MSG-DSC (003)
                    MOVE WT01-TXT-MSG (003) TO BSE-DESCR-MENSAJE
                    IF CRM-CO-MENSAJE (1:19) = 'DOCUMENTO NO EXISTE' OR
                       CRM-CO-MENSAJE (1:18) = 'REGISTRO NO EXISTE'
                       MOVE 01              TO BSE-CODIGO-RESPUESTA
                    ELSE
                       MOVE 16              TO BSE-CODIGO-RESPUESTA
                       PERFORM  TERMINAR-RUTINA
                    END-IF
           END-EVALUATE.
      *-----------*
       ARMAR-TRAMA.
      *-----------*
           MOVE CRM-CO-NUMERO-29       TO BSEC006O-NROINT.
           MOVE CRM-CO-NOM-ACEP        TO BSEC006O-NOMACEP.
           MOVE CRM-CO-DIR-ACEP        TO BSEC006O-DIRACEP.
           MOVE CRM-CO-TIPDOC-IDEN     TO BSEC006O-TIPDOCACEP.
           MOVE CRM-CO-NRODOC-IDEN     TO BSEC006O-NUMDOCACEP.
           MOVE CRM-CO-MNTO-ORIG-29    TO BSEC006O-IMPORI.
           MOVE CRM-CO-SALDO-29        TO BSEC006O-SALACT.
           MOVE CRM-CO-DESEMBOLSO      TO BSEC006O-DESEMBOLSO.
           MOVE CRM-CO-INT-DIFORI      TO BSEC006O-INTDIFORI.
           MOVE CRM-CO-INT-GANADO      TO BSEC006O-INTGANADO.
           MOVE CRM-CO-DESCR-SITUAC-29 TO BSEC006O-STATUS.
           MOVE CRM-CO-DIAS-ORIG       TO BSEC006O-DIASORI.
           MOVE CRM-CO-DIAS-PEN        TO BSEC006O-DIASPEN.
           MOVE CRM-CO-INT-COMPEN      TO BSEC006O-INTCOMP.
           MOVE CRM-CO-INT-MORAT       TO BSEC006O-INTMORA.
           MOVE CRM-CO-FEC-ING-29      TO BSEC006O-FECING.
           MOVE CRM-CO-FEC-VCTO-29     TO BSEC006O-FECVCTO.
           MOVE CRM-CO-FEC-MMTO-29     TO BSEC006O-FECCAMBIO.
           MOVE ZEROS              TO BSEC006O-FECDEVO BSEC006O-FECCANC
           IF CRM-CO-SITUAC-29 = 'C'
              MOVE CRM-CO-FEC-DEVCAN-29     TO BSEC006O-FECCANC
           ELSE
              IF CRM-CO-FEC-DEVCAN-29 > 0
                 MOVE CRM-CO-FEC-DEVCAN-29  TO BSEC006O-FECDEVO
              END-IF
           END-IF.
           MOVE CRM-CO-NUM-RENOV-29         TO BSEC006O-NUMRENO.
      *----------------*
       BLOQUEAR-INGRESO.
      *----------------*
           EXEC CICS SEND TEXT FROM   (WE-BLANCO)
                               LENGTH (01)
                               ERASE
           END-EXEC.
           GOBACK.
      *-------------*
       ERROR-EN-CICS.
      *-------------*
           MOVE 16                     TO BSE-CODIGO-RESPUESTA.
           MOVE WT01-COD-MSG (002)     TO BSE-CODIGO-MENSAJE.
           MOVE WT01-TXT-MSG (002)     TO BSE-DESCR-MENSAJE.
           PERFORM  TERMINAR-RUTINA.
      *-----------------*
       ASIGNAR-BSE-DATOS.
      *-----------------*
           INSPECT REG-BSEC006O REPLACING ALL LOW-VALUES BY SPACES.
           MOVE REG-BSEC006O           TO BSE-DATOS.
      *---------------*
       TERMINAR-RUTINA.
      *---------------*
           MOVE REG-COMMAREA-BSE       TO DFHCOMMAREA.
           EXEC CICS RETURN
           END-EXEC.
           GOBACK.
