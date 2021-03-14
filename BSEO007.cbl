************************************************************************
***   * FO6813 12/07/12 PAMH PROYECTO BUS DE SERVICIO EMPRESARIAL      *
************************************************************************
*IDAPL*BSE
*OBJET*****************************************************************
*OBJET*** OPERACION CONSULTARDOCUMENTOSPORGIRADOR                    **
*OBJET*****************************************************************
      *=======================*
       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID. BSEO007.
      *====================*
       ENVIRONMENT DIVISION.
      *====================*
       DATA DIVISION.
      *=============*
       WORKING-STORAGE SECTION.
      *=======================*
       01  WE-ESPECIALES.
           02  I                       PIC 9(05)       VALUE ZEROS.
           02  K                       PIC 9(05)       VALUE ZEROS.
           02  CONTADOR                PIC 9(05)       VALUE ZEROS.
           02  WE-LINK-MAX             PIC 9(05)       VALUE ZEROS.
           02  WE-NUM-ELEM-FALTAN      PIC 9(05)       VALUE ZEROS.
           02  WE-SECUENCIA            PIC X(10)       VALUE SPACES.
           02  WE-BLANCO               PIC X(01)       VALUE SPACES.
           02  WE-CUNICO               PIC X(14)       VALUE SPACES.
           02  WE-RC                   PIC S9(08) COMP VALUE ZEROS.
      *    ----------------- CAMPOS PARA BIF DEEDIT -----------------*
           02  WE-CANTIDAD.
               03  WE-CANT-ALF         PIC X(18).
               03  WE-CANT-NUM         REDEFINES  WE-CANT-ALF
                                       PIC 9(18).
      *------------------ TABLA DE ERRORES ---------------------------*
       01  WT01-TABLA-MENSAJES.
           02  FILLER                  PIC X(64)  VALUE
                '001*TRANSACCION EXITOSA
      -         '-BSEO007 '.
           02  FILLER                  PIC X(64)  VALUE
                '002*ERROR EN RUTINA
      -         '-BSEO007 '.
           02  FILLER                  PIC X(64)  VALUE
                '003*ERROR EN RUTINA
      -         '-PROCRM30'.
           02  FILLER                  PIC X(64)  VALUE
                '004*ERROR AL LLAMAR PROGRAMA XXXXXXXX
      -         '-BSEO007 '.
           02  FILLER                  PIC X(64)  VALUE
                '005*APLICATIVO DEBE SER $PR
      -         '-BSEO007 '.
           02  FILLER                  PIC X(64)  VALUE
                '006*ENTIDAD DEBE SER 003
      -         '-BSEO007 '.
           02  FILLER                  PIC X(64)  VALUE
                '007*NUMERO SECUENCIA DEBE SER NUMERICO
      -         '-BSEO007 '.
           02  FILLER                  PIC X(64)  VALUE
                '008*OPCION PAGINACION DEBE SER NEXT O PREV
      -         '-BSEO007 '.
           02  FILLER                  PIC X(64)  VALUE
                '009*LA OPERACION DE LA TRAMA NO EXISTE EN TABLA
      -         '-BSEO007 '.
       01  FILLER  REDEFINES  WT01-TABLA-MENSAJES.
           02  FILLER  OCCURS   9  TIMES.
               04  WT01-COD-MSG        PIC 9(03).
               04  FILLER              PIC X(01).
               04  WT01-TXT-MSG.
                   06  WT01-MSG-DSC    PIC X(51).
                   06  WT01-MSG-PRG    PIC X(09).
      *------------------ TABLA DE OPERACIONES -----------------------*
           COPY BSETOPER.
      *------------------ COPY PARA RUTINA CLOCRM02 ($PR) -----------*
           COPY CL02CRM.
      *------------------ COMMAREA -----------------------------------*
           COPY BSECCOM.
      *------------------ ESTRUCTURA DE DATOS DE TRAMAS BSEO007 ------*
           COPY BSEC007I.
           COPY BSEC007O.
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
           MOVE BSE-DATOS              TO REG-BSEC007I.
           MOVE SPACES                 TO BSE-DATOS.
           MOVE SPACES                 TO REG-BSEC007O.
      *    INITIALIZE                     REG-BSEC007O.
           MOVE ZEROS                  TO BSEC007O-CANTIDAD-ACEP.
           MOVE 00                     TO BSE-CODIGO-RESPUESTA.
           MOVE WT01-COD-MSG (001)     TO BSE-CODIGO-MENSAJE.
           MOVE WT01-TXT-MSG (001)     TO BSE-DESCR-MENSAJE.
      *--- BUSCA LA OPERACION DE LA TRAMA INPUT EN LA TABLA OPERACIONES
           MOVE BSE-CODIGO-OPERACION   TO WX-OPERACION-BSETOPER.
           COPY BSELOPER.
           IF WX-SW-FOUND-BSETOPER = 'N'
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (009)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (009)  TO BSE-DESCR-MENSAJE
              PERFORM TERMINAR-RUTINA
           END-IF.
      *---------------*
       PROCESAR-RUTINA.
      *---------------*
      *    PERFORM  VALIDAR-DATOS.
           PERFORM  LLAMAR-CLOCRM02.
           PERFORM  ASIGNAR-BSE-DATOS.
      *-------------*
       VALIDAR-DATOS.
      *-------------*
      *--- VALIDAMOS APLICATIVO
           IF BSEC007I-ID-APLI NOT = '$PR'
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (005)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (005)  TO BSE-DESCR-MENSAJE
              PERFORM TERMINAR-RUTINA
           END-IF.
      *--- VALIDAMOS ENTIDAD
           IF BSEC007I-CODENT NOT = '003'
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (006)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (006)  TO BSE-DESCR-MENSAJE
              PERFORM TERMINAR-RUTINA
           END-IF.
      *--- VALIDAMOS SECUENCIA
           IF BSEC007I-SECUENCIA-PR IS NOT NUMERIC
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (007)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (007)  TO BSE-DESCR-MENSAJE
              PERFORM TERMINAR-RUTINA
           END-IF.
           MOVE BSEC007I-SECUENCIA-PR  TO WE-SECUENCIA.
      *--- VALIDAMOS OPCION NEXT-PREV
           IF BSEC007I-OPCION-NEXT-PREV NOT = 'NEXT' AND 'PREV'
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (008)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (008)  TO BSE-DESCR-MENSAJE
              PERFORM TERMINAR-RUTINA
           END-IF.
      *---------------*
       LLAMAR-CLOCRM02.
      *---------------*
           MOVE 0                      TO K.
           MOVE WX-T-NRO-MAX-OCCURS-TRAMA-OUT TO WE-NUM-ELEM-FALTAN.
           MOVE SPACES                 TO CRM-CO-FLAG-CONT.
           MOVE +0000                  TO CRM-CO-COD-RETORNO.
           MOVE ZEROS                  TO CONTADOR.
           COMPUTE WE-LINK-MAX = WX-T-NRO-MAX-OCCURS-TRAMA-OUT /
                                 WX-T-NRO-MAX-OCCURS-RUTINA
           END-COMPUTE.
           PERFORM UNTIL WE-NUM-ELEM-FALTAN     = 0     OR
                         CRM-CO-FLAG-CONT       = 'U'   OR
                         CRM-CO-COD-RETORNO NOT = +0000 OR
                         CONTADOR              >= WE-LINK-MAX
              ADD  1                   TO CONTADOR
              PERFORM  LINK-CLOCRM02
           END-PERFORM.
      *-------------*
       LINK-CLOCRM02.
      *-------------*
           INITIALIZE                     CRM-CO-COMMAREA.
           MOVE 030                    TO CRM-CO-NRO-TRAMA.
           MOVE BSEC007I-ID-APLI       TO CRM-CO-ID-APLI.
           MOVE BSEC007I-CODENT        TO CRM-CO-ID-BANCO.
           MOVE BSEC007I-TI-DOCU-PR    TO CRM-CO-TIPDOC.
           MOVE BSEC007I-NU-DOCU-PR    TO CRM-CO-NUMERO.
           MOVE BSEC007I-CODIGO-UNICO  TO WE-CANT-ALF.
           EXEC CICS  BIF DEEDIT       FIELD  (WE-CANTIDAD)
                                       LENGTH (14)
                                       END-EXEC.
           MOVE WE-CANT-NUM            TO WE-CUNICO.
           MOVE WE-CUNICO (5:10)       TO CRM-CO-CODUNICO-30.
           MOVE WE-SECUENCIA           TO CRM-CO-SECUEN-30.
           MOVE BSEC007I-OPCION-NEXT-PREV TO CRM-CO-FLAGLEC-30.
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
                    IF CRM-CO-MENSAJE (1:18) = 'REGISTRO NO EXISTE' OR
                       CRM-CO-MENSAJE (1:21) = 'DOCUMENTOS NO EXISTEN'
                       MOVE 01              TO BSE-CODIGO-RESPUESTA
                    ELSE
                       MOVE 16              TO BSE-CODIGO-RESPUESTA
                       PERFORM  TERMINAR-RUTINA
                    END-IF
           END-EVALUATE.
      *-----------*
       ARMAR-TRAMA.
      *-----------*
           MOVE 0                      TO I.
           PERFORM UNTIL ( I >= WX-T-NRO-MAX-OCCURS-TRAMA-OUT ) OR
                         ( I >= CRM-CO-NRO-DCTOS )
              ADD 1                         TO I
              ADD 1                         TO K
              MOVE CRM-CO-PROD-30       (I) TO BSEC007O-PROD      (K)
              MOVE CRM-CO-NUMERO-30     (I) TO BSEC007O-AUTOGEN   (K)
              MOVE CRM-CO-FECVTO-30     (I) TO BSEC007O-FECVCTO   (K)
              MOVE CRM-CO-NOMACEP-30    (I) TO BSEC007O-NOMACEP   (K)
              MOVE CRM-CO-SITUAC-30     (I) TO BSEC007O-SITUACION (K)
              MOVE CRM-CO-SALDO-30      (I) TO BSEC007O-SALACT    (K)
              MOVE CRM-CO-FLAG-RETEN-30 (I) TO BSEC007O-FLAGRET   (K)
           END-PERFORM.
           MOVE CRM-CO-FLAG-CONT       TO BSEC007O-FLAG-CONTINUIDAD.
           MOVE CRM-CO-SECUENC-INI-30  TO BSEC007O-SECUENCIA-PRIMERO.
           MOVE CRM-CO-SECUENC-FIN-30  TO BSEC007O-SECUENCIA-ULTIMO.
           IF BSEC007I-OPCION-NEXT-PREV = 'NEXT'
              MOVE CRM-CO-SECUENC-FIN-30 TO WE-SECUENCIA
           ELSE
              MOVE CRM-CO-SECUENC-INI-30 TO WE-SECUENCIA
           END-IF.
           MOVE K                      TO BSEC007O-CANTIDAD-ACEP.
           COMPUTE WE-NUM-ELEM-FALTAN =
                   WX-T-NRO-MAX-OCCURS-TRAMA-OUT - K
           END-COMPUTE.
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
           COMPUTE BSE-LONGITUD-OUTPUT =
                   WX-LONG-HEADER-BSETOPER        +
                   WX-T-LONG-PARTE-FIJA-TRAMA-OUT +
                  (BSEC007O-CANTIDAD-ACEP         *
                   WX-T-LONG-CADA-OCCUR-TRAMA-OUT)
           END-COMPUTE.
           INSPECT REG-BSEC007O REPLACING ALL LOW-VALUES BY SPACES.
           MOVE REG-BSEC007O           TO BSE-DATOS.
      *---------------*
       TERMINAR-RUTINA.
      *---------------*
           MOVE REG-COMMAREA-BSE       TO DFHCOMMAREA.
           EXEC CICS RETURN
           END-EXEC.
           GOBACK.
