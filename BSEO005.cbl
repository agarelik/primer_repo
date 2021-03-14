************************************************************************
***   * 101022 01/04/13 PAMH AJUSTES VARIOS AL PROGRAMA                *
***   * FO6813 11/07/12 PAMH PROYECTO BUS DE SERVICIO EMPRESARIAL      *
************************************************************************
*IDAPL*BSE
*OBJET*****************************************************************
*OBJET*** OPERACION CONSULTARGRUPOSLIQUIDACION                       **
*OBJET*****************************************************************
      *=======================*
       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID. BSEO005.
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
           02  WE-SECUENCIA            PIC X(24)       VALUE SPACES.
           02  WE-BLANCO               PIC X(01)       VALUE SPACES.
           02  WE-RC                   PIC S9(08) COMP VALUE ZEROS.
      *------------------ TABLA DE ERRORES ---------------------------*
       01  WT01-TABLA-MENSAJES.
           02  FILLER                  PIC X(64)  VALUE
                '001*TRANSACCION EXITOSA
      -         '-BSEO005 '.
           02  FILLER                  PIC X(64)  VALUE
                '002*ERROR AVISAR A SISTEMAS
      -         '-BSEO005 '.
           02  FILLER                  PIC X(64)  VALUE
                '003*RC=XX ERROR EN RUTINA
      -         '-ATCO082 '.
           02  FILLER                  PIC X(64)  VALUE
                '004*ERROR AL LLAMAR PROGRAMA XXXXXXXX
      -         '-BSEO005 '.
           02  FILLER                  PIC X(64)  VALUE
                '005*APLICATIVO DEBE SER SAT
      -         '-BSEO005 '.
           02  FILLER                  PIC X(64)  VALUE
                '006*ENTIDAD DEBE SER NUMERICO
      -         '-BSEO005 '.
           02  FILLER                  PIC X(64)  VALUE
                '007*ENTIDAD DEBE SER 003
      -         '-BSEO005 '.
           02  FILLER                  PIC X(64)  VALUE
                '008*GRUPO LIQUIDACION DEBE SER NUMERICO
      -         '-BSEO005 '.
           02  FILLER                  PIC X(64)  VALUE
                '009*NUMERO SECUENCIA DEBE SER NUMERICO
      -         '-BSEO005 '.
           02  FILLER                  PIC X(64)  VALUE
                '010*OPCION PAGINACION DEBE SER NEXT
      -         '-BSEO005 '.
           02  FILLER                  PIC X(64)  VALUE
                '011*LA OPERACION DE LA TRAMA NO EXISTE EN TABLA
      -         '-BSEO005 '.
       01  FILLER  REDEFINES  WT01-TABLA-MENSAJES.
           02  FILLER  OCCURS  11  TIMES.
               04  WT01-COD-MSG        PIC 9(03).
               04  FILLER              PIC X(01).
               04  WT01-TXT-MSG.
                   06  WT01-MSG-DSC    PIC X(51).
                   06  WT01-MSG-PRG    PIC X(09).
      *------------------ TABLA DE OPERACIONES -----------------------*
           COPY BSETOPER.
      *------------------ COMMAREA -----------------------------------*
           COPY BSECCOM.
      *------------------ ESTRUCTURA DE DATOS DE TRAMAS BSEO005 ------*
           COPY BSEC005I.
           COPY BSEC005O.
      *------------------ RUTINA ATCO082 -----------------------------*
           COPY ATC82CGC.
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
           MOVE BSE-DATOS              TO REG-BSEC005I.
           MOVE SPACES                 TO BSE-DATOS.

           MOVE SPACES                 TO REG-BSEC005O.
           MOVE 'U'                    TO BSEC005O-FLAG-CONTINUIDAD.
           MOVE SPACES                 TO BSEC005O-SECUENCIA-PRIMERO.
           MOVE SPACES                 TO BSEC005O-SECUENCIA-ULTIMO.
           MOVE ZEROS                  TO BSEC005O-CANLIQ.

           MOVE 00                     TO BSE-CODIGO-RESPUESTA.
           MOVE WT01-COD-MSG (001)     TO BSE-CODIGO-MENSAJE.
           MOVE WT01-TXT-MSG (001)     TO BSE-DESCR-MENSAJE.

      *--- BUSCA LA OPERACION DE LA TRAMA INPUT EN LA TABLA OPERACIONES
           MOVE BSE-CODIGO-OPERACION   TO WX-OPERACION-BSETOPER.
           COPY BSELOPER.
           IF WX-SW-FOUND-BSETOPER = 'N'
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (011)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (011)  TO BSE-DESCR-MENSAJE
              PERFORM TERMINAR-RUTINA
           END-IF.
      *---------------*
       PROCESAR-RUTINA.
      *---------------*
           PERFORM  VALIDAR-DATOS.
           PERFORM  LLAMAR-ATCO082.
           PERFORM  ASIGNAR-BSE-DATOS.
      *-------------*
       VALIDAR-DATOS.
      *-------------*
      *--- VALIDAMOS APLICATIVO
           IF BSEC005I-ID-APL  NOT = 'SAT'
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (005)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (005)  TO BSE-DESCR-MENSAJE
              PERFORM TERMINAR-RUTINA
           END-IF.
      *--- VALIDAMOS ENTIDAD
           IF BSEC005I-CODENT IS NOT NUMERIC
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (006)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (006)  TO BSE-DESCR-MENSAJE
              PERFORM TERMINAR-RUTINA
           END-IF.
           IF BSEC005I-CODENT NOT = 003
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (007)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (007)  TO BSE-DESCR-MENSAJE
              PERFORM TERMINAR-RUTINA
           END-IF.
      *--- VALIDAMOS GRUPO LIQUIDACION
           IF BSEC005I-GRUPOLIQ IS NOT NUMERIC
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (008)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (008)  TO BSE-DESCR-MENSAJE
              PERFORM TERMINAR-RUTINA
           END-IF.
      *--- VALIDAMOS SECUENCIA
      *    IF BSEC005I-SECUENCIA-SAT IS NOT NUMERIC
      *       MOVE 16                  TO BSE-CODIGO-RESPUESTA
      *       MOVE WT01-COD-MSG (009)  TO BSE-CODIGO-MENSAJE
      *       MOVE WT01-TXT-MSG (009)  TO BSE-DESCR-MENSAJE
      *       PERFORM TERMINAR-RUTINA
      *    END-IF.
           MOVE BSEC005I-SECUENCIA-SAT TO WE-SECUENCIA.
      *--- VALIDAMOS OPCION NEXT-PREV
           IF BSEC005I-OPCION-NEXT-PREV NOT = 'NEXT'
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (010)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (010)  TO BSE-DESCR-MENSAJE
              PERFORM TERMINAR-RUTINA
           END-IF.
      *--------------*
       LLAMAR-ATCO082.
      *--------------*
           MOVE ZEROS                  TO I.
           MOVE ZEROS                  TO K.
           MOVE ZEROS                  TO CONTADOR.
           COMPUTE WE-LINK-MAX = WX-T-NRO-MAX-OCCURS-TRAMA-OUT /
                                 WX-T-NRO-MAX-OCCURS-RUTINA
           END-COMPUTE.
           COMPUTE WE-NUM-ELEM-FALTAN =
                   WX-T-NRO-MAX-OCCURS-TRAMA-OUT - K
           END-COMPUTE.
           MOVE SPACES                 TO CGC-FLG-CONTINUA.
           MOVE 00                     TO CGC-COD-RESPTA.
           PERFORM UNTIL WE-NUM-ELEM-FALTAN = 0   OR
                         CGC-FLG-CONTINUA   = 'U' OR
                         CGC-COD-RESPTA NOT = 00  OR
                         CONTADOR          >= WE-LINK-MAX
              ADD  1                   TO CONTADOR
              PERFORM  LINK-ATCO082
           END-PERFORM.
      *------------*
       LINK-ATCO082.
      *------------*
           INITIALIZE                     ATC82CGC.
           MOVE BSEC005I-ID-APL        TO CGC-IDE-APP.
           MOVE BSEC005I-CODENT        TO CGC-COD-ENTIDA.
           IF BSEC005I-NU-CNTA-SA NOT NUMERIC
              MOVE ZEROS               TO BSEC005I-NU-CNTA-SA
           END-IF.
           MOVE BSEC005I-ID-DOCU       TO CGC-IDE-DOCUME.
           MOVE BSEC005I-GRUPOLIQ      TO CGC-COD-GRULIQ.
           MOVE WE-SECUENCIA           TO CGC-IDE-SECUENC.
           MOVE BSEC005I-OPCION-NEXT-PREV TO CGC-FLG-LECTURA.
           EXEC CICS LINK PROGRAM  ('ATCO082')
                          COMMAREA (ATC82CGC)
                          LENGTH   (LENGTH OF ATC82CGC)
                          RESP     (WE-RC)
           END-EXEC.
           IF WE-RC NOT = DFHRESP(NORMAL)
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (004)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (004)  TO BSE-DESCR-MENSAJE
              MOVE 'ATCO082 '          TO BSE-DESCR-MENSAJE (26:8)
              PERFORM  TERMINAR-RUTINA
           END-IF.
           EVALUATE CGC-COD-RESPTA
              WHEN 00
                 PERFORM ASIGNA-DATA
              WHEN 10
                 MOVE 01                 TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (003) TO BSE-CODIGO-MENSAJE
                 MOVE CGC-COD-RESPTA     TO WT01-MSG-DSC (003) (4:2)
                 MOVE CGC-MSG-RESPTA     TO WT01-MSG-DSC (003) (7:45)
                 MOVE WT01-TXT-MSG (003) TO BSE-DESCR-MENSAJE
              WHEN OTHER
                 MOVE 16                 TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (003) TO BSE-CODIGO-MENSAJE
                 MOVE CGC-COD-RESPTA     TO WT01-MSG-DSC (003) (4:2)
                 MOVE CGC-MSG-RESPTA     TO WT01-MSG-DSC (003) (7:45)
                 MOVE WT01-TXT-MSG (003) TO BSE-DESCR-MENSAJE
                 PERFORM  TERMINAR-RUTINA
           END-EVALUATE.
      *-----------*
       ASIGNA-DATA.
      *-----------*
           MOVE 0                      TO I.
           PERFORM UNTIL ( K >= WX-T-NRO-MAX-OCCURS-TRAMA-OUT ) OR
                         ( I >= CGC-NUM-ELEMEN )
              ADD  1                   TO I
              ADD  1                   TO K
              MOVE CGC-PTR-IDEREG(I)   TO BSEC005O-SECUENCIA-OCCURS(K)
              MOVE CGC-FEC-CIERRE(I)   TO BSEC005O-FECHACIERRE     (K)
              MOVE CGC-FEC-PAGO  (I)   TO BSEC005O-FECHAPAGO       (K)
           END-PERFORM.
           MOVE CGC-FLG-CONTINUA       TO BSEC005O-FLAG-CONTINUIDAD.
           MOVE CGC-SEC-INICIAL        TO BSEC005O-SECUENCIA-PRIMERO.
           MOVE CGC-SEC-FINAL          TO BSEC005O-SECUENCIA-ULTIMO.
           IF BSEC005I-OPCION-NEXT-PREV = 'NEXT'
              MOVE CGC-SEC-FINAL       TO WE-SECUENCIA
           ELSE
              MOVE CGC-SEC-INICIAL     TO WE-SECUENCIA
           END-IF.
           MOVE K                      TO BSEC005O-CANLIQ.
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
                  (BSEC005O-CANLIQ                *
                   WX-T-LONG-CADA-OCCUR-TRAMA-OUT)
           END-COMPUTE.
           INSPECT REG-BSEC005O REPLACING ALL LOW-VALUES BY SPACES.
           MOVE REG-BSEC005O           TO BSE-DATOS.
      *---------------*
       TERMINAR-RUTINA.
      *---------------*
           MOVE REG-COMMAREA-BSE       TO DFHCOMMAREA.
           EXEC CICS RETURN
           END-EXEC.
           GOBACK.
