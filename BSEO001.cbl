************************************************************************
***   * 100048 04/06/14 PAMH EJRG TARJETA CREDITO CHIP                **
***   * 100043 21/08/13 PAMH INDICADOR DE TARJETAS DE CREDITO ACTIVAS  *
***   * FO6813 10/07/12 PAMH PROYECTO BUS DE SERVICIO EMPRESARIAL      *
************************************************************************
*IDAPL*BSE
*OBJET*****************************************************************
*OBJET*** OPERACION CONSULTARPRODUCTOS                               **
*OBJET*****************************************************************
      *=======================*
       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID. BSEO001.
       AUTHOR. ARIEL GARELIK.
      *====================*
       ENVIRONMENT DIVISION.
      *====================*
       DATA DIVISION.
      *=============*
       WORKING-STORAGE SECTION.
      *=======================*
       01  WE-ESPECIALES.
           02  I                       PIC 9(05)  VALUE ZEROS.
           02  J                       PIC 9(05)  VALUE ZEROS.
           02  K                       PIC 9(05)  VALUE ZEROS.
           02  CONTADOR                PIC 9(05)  VALUE ZEROS.
           02  WE-LINK-MAX             PIC 9(05)  VALUE ZEROS.
           02  WE-NUM-ELEM             PIC 9(05)  VALUE ZEROS.
           02  WE-NUM-ELEM-FALTAN      PIC 9(05)  VALUE ZEROS.
           02  WE-SECUENCIA            PIC 9(08)  VALUE ZEROS.
           02  WE-NEXT-PREV            PIC X(04)  VALUE SPACES.
           02  WE-RC                   PIC S9(08) COMP VALUE ZEROS.
           02  WE-BLANCO               PIC X(01)  VALUE SPACES.
           02  WE-CUNICO               PIC X(10)  VALUE SPACES.
           02  WE-MONEDA               PIC X(02)  VALUE SPACES.
           02  WE-MONEDA-DESC          PIC X(10)  VALUE SPACES.
           02  WE-ACCESO               PIC X(01)  VALUE SPACES.
           02  WE-CR-SRMR011           PIC 9(02)  VALUE ZEROS.
           02  WE-CM-SRMR011           PIC 9(03)  VALUE ZEROS.
           02  WE-DM-SRMR011           PIC X(60)  VALUE SPACES.
           02  WE-CR-BSER004           PIC 9(02)  VALUE ZEROS.
           02  WE-CM-BSER004           PIC 9(03)  VALUE ZEROS.
           02  WE-DM-BSER004           PIC X(60)  VALUE SPACES.
           02  WE-CR-ATCO088           PIC 9(02)  VALUE ZEROS.
           02  WE-CM-ATCO088           PIC 9(03)  VALUE ZEROS.
           02  WE-DM-ATCO088           PIC X(60)  VALUE SPACES.
           02  WE-CR-ATCO070           PIC 9(02)  VALUE ZEROS.
           02  WE-CM-ATCO070           PIC 9(03)  VALUE ZEROS.
           02  WE-DM-ATCO070           PIC X(60)  VALUE SPACES.
      *
       01  WE-MENSAJE.
           02  FILLER                  PIC X(34)  VALUE
               'INVALIDO VALOR DE SECUENCIA INPUT '.
           02  SECUENCIA               PIC X(08)  VALUE SPACES.
      *    ----------------- CAMPOS PARA BIF DEEDIT -----------------*
           02  WE-CANTIDAD.
               03  WE-CANT-ALF         PIC X(18).
               03  WE-CANT-NUM         REDEFINES  WE-CANT-ALF
                                       PIC 9(18).
      *------------------ TABLA DE ERRORES ---------------------------*
       01  WT01-TABLA-MENSAJES.
           02  FILLER                  PIC X(64)  VALUE
                '001*TRANSACCION EXITOSA
      -         '-BSEO001 '.
           02  FILLER                  PIC X(64)  VALUE
                '002*ERROR AVISAR A SISTEMAS
      -         '-BSEO001 '.
           02  FILLER                  PIC X(64)  VALUE
                '003*NUMERO SECUENCIA DEBE SER NUMERICO
      -         '-BSEO001 '.
           02  FILLER                  PIC X(64)  VALUE
                '004*OPCION PAGINACION DEBE SER NEXT
      -         '-BSEO001 '.
           02  FILLER                  PIC X(64)  VALUE
                '005*CODIGO UNICO DEL CLIENTE NO VALIDO
      -         '-BSER004 '.
           02  FILLER                  PIC X(64)  VALUE
                '006*RC=XX ERROR EN RUTINA
      -         '-ATCO070 '.
           02  FILLER                  PIC X(64)  VALUE
                '007*RC=XX ERROR EN RUTINA
      -         '-ATCO088 '.
           02  FILLER                  PIC X(64)  VALUE
                '008*CODIGO DE CLIENTE NO VALIDO
      -         '-SRMR011 '.
           02  FILLER                  PIC X(64)  VALUE
                '009*CODIGO DE CLIENTE NO EXISTE
      -         '-SRMR011 '.
           02  FILLER                  PIC X(64)  VALUE
                '010*NUMERO DE CUENTA NO EXISTE
      -         '-SRMR011 '.
           02  FILLER                  PIC X(64)  VALUE
                '011*ARCHIVO CERRADO
      -         '-SRMR011 '.
           02  FILLER                  PIC X(64)  VALUE
                '012*ERROR CICS EN RUTINA
      -         '-SRMR011 '.
           02  FILLER                  PIC X(64)  VALUE
                '013*ERROR EN RUTINA
      -         '-SRMR011 '.
           02  FILLER                  PIC X(64)  VALUE
                '014*ERROR EN RUTINA
      -         '-BSEOUSU '.
           02  FILLER                  PIC X(64)  VALUE
                '015*USUARIO NO AUTORIZADO A CONSULTAR EMPLEADOS
      -         '-BSEO001 '.
           02  FILLER                  PIC X(64)  VALUE
                '016*INFORMACION DE PRODUCTOS CON ACCESO RESTRINGIDO
      -         '-SCLO039 '.
           02  FILLER                  PIC X(64)  VALUE
                '017*ERROR CICS EN RUTINA SCLO039
      -         '-SCLO039 '.
           02  FILLER                  PIC X(64)  VALUE
                '018*ERROR DB2 EN RUTINA SCLO039
      -         '-SCLO039 '.
           02  FILLER                  PIC X(64)  VALUE
                '019*ERROR EN RUTINA SCLO039
      -         '-SCLO039 '.
           02  FILLER                  PIC X(64)  VALUE
                '020*ERROR AL LLAMAR PROGRAMA XXXXXXXX
      -         '-BSEO001 '.
           02  FILLER                  PIC X(64)  VALUE
                '021*LA OPERACION DE LA TRAMA NO EXISTE EN TABLA
      -         '-BSEO001 '.
       01  FILLER  REDEFINES  WT01-TABLA-MENSAJES.
           02  FILLER  OCCURS  21  TIMES.
               04  WT01-COD-MSG        PIC 9(03).
               04  FILLER              PIC X(01).
               04  WT01-TXT-MSG.
                   06  WT01-MSG-DSC    PIC X(51).
                   06  WT01-MSG-PRG    PIC X(09).
      *------------- TABLA DE MONEDAS --------------------------------*
           COPY CAMWSMON.
      *------------- TABLA DE OPERACIONES ----------------------------*
           COPY BSETOPER.
      *------------- RUTINA SRMR011 (CLIENTES) -----------------------*
       01  WF-COMMAREA-CICS.
           COPY SRMCICS.
      *------------- RUTINA BSEOUSU (SEGURIDAD USUARIO) --------------*
      *    COPY BSECUSU.
      *------------- RUTINA BSER004 (DOCUMENTOS POR CLIENTE) ---------*
       01  WF-COMMAREA-BSER004C.
           COPY BSEC004R.
      *------------- RUTINA ATCO070 (SAT) ----------------------------*
           COPY ATC70CTI.
      *------------- RUTINA ATCO088 (SAT) ----------------------------*
           COPY ATC88TCL.
      *------------------ COMMAREA -----------------------------------*
           COPY BSECCOM.
      *------------------ ESTRUCTURA DE DATOS DE TRAMAS BSEC007 ------*
           COPY BSEC001I.
           COPY BSEC001O.
      *---------------------------------------------------------------*
       01  WE-COMMAREA-SCLO039.
           COPY SCL02CO0.
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
           MOVE BSE-DATOS              TO REG-BSEC001I.
           MOVE SPACES                 TO BSE-DATOS.

           MOVE SPACES                 TO REG-BSEC001O.
           MOVE 'U'                    TO BSEC001O-FLAG-CONTINUIDAD.
           MOVE ZEROS                  TO BSEC001O-SECUENCIA-PRIMERO.
           MOVE ZEROS                  TO BSEC001O-SECUENCIA-ULTIMO.
           MOVE ZEROS                  TO BSEC001O-CANTIDAD-PRODUCTOS.

           MOVE 00                     TO BSE-CODIGO-RESPUESTA.
           MOVE WT01-COD-MSG (001)     TO BSE-CODIGO-MENSAJE.
           MOVE WT01-TXT-MSG (001)     TO BSE-DESCR-MENSAJE.

      *--- BUSCA LA OPERACION DE LA TRAMA INPUT EN LA TABLA OPERACIONES
           MOVE BSE-CODIGO-OPERACION   TO WX-OPERACION-BSETOPER.
           COPY BSELOPER.
           IF WX-SW-FOUND-BSETOPER = 'N'
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (021)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (021)  TO BSE-DESCR-MENSAJE
              PERFORM TERMINAR-RUTINA
           END-IF.
      *---------------*
       PROCESAR-RUTINA.
      *---------------*
           PERFORM VALIDAR-DATOS.
           PERFORM VALIDAR-ACCESO-CLIENTE.
      *--- VERIFICAMOS SECUENCIA
           IF WE-SECUENCIA (1:1) = '0'
              IF WE-SECUENCIA > 0
                 COMPUTE WE-SECUENCIA = WE-SECUENCIA + 1
                 END-COMPUTE
              END-IF
              PERFORM BUSCAR-CLIENTE
              PERFORM BUSCAR-PRODUCTOS
              IF WE-NUM-ELEM-FALTAN = 0 OR
                 K = WX-T-NRO-MAX-OCCURS-TRAMA-OUT
                 MOVE 'C'              TO BSEC001O-FLAG-CONTINUIDAD
              ELSE
                 MOVE 00000000         TO WE-SECUENCIA
                 PERFORM BUSCAR-TARJETAS
              END-IF
           ELSE
              IF WE-SECUENCIA (1:1) = '1'
                 MOVE 0                TO K
                 PERFORM BUSCAR-TARJETAS
              ELSE
                 MOVE 16                 TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (003) TO BSE-CODIGO-MENSAJE
                 MOVE WE-SECUENCIA       TO SECUENCIA
                 MOVE WE-MENSAJE         TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
              END-IF
           END-IF.
      *--- VERIFICAMOS SI EXISTE ALGUN WARNING
           IF WE-CR-SRMR011 = 01
              MOVE WE-CR-SRMR011       TO BSE-CODIGO-RESPUESTA
              MOVE WE-CM-SRMR011       TO BSE-CODIGO-MENSAJE
              MOVE WE-DM-SRMR011       TO BSE-DESCR-MENSAJE
           ELSE
           IF WE-CR-BSER004 = 01
              MOVE WE-CR-BSER004       TO BSE-CODIGO-RESPUESTA
              MOVE WE-CM-BSER004       TO BSE-CODIGO-MENSAJE
              MOVE WE-DM-BSER004       TO BSE-DESCR-MENSAJE
           ELSE
           IF WE-CR-ATCO088 = 01
              MOVE WE-CR-ATCO088       TO BSE-CODIGO-RESPUESTA
              MOVE WE-CM-ATCO088       TO BSE-CODIGO-MENSAJE
              MOVE WE-DM-ATCO088       TO BSE-DESCR-MENSAJE
           ELSE
           IF WE-CR-ATCO070 = 01
              MOVE WE-CR-ATCO070       TO BSE-CODIGO-RESPUESTA
              MOVE WE-CM-ATCO070       TO BSE-CODIGO-MENSAJE
              MOVE WE-DM-ATCO070       TO BSE-DESCR-MENSAJE
           END-IF
           END-IF
           END-IF
           END-IF.
           PERFORM ASIGNAR-BSE-DATOS.
      *-------------*
       VALIDAR-DATOS.
      *-------------*
      *--- ASIGNAMOS CODIGO UNICO
           MOVE BSEC001I-CODIGO-UNICO  TO WE-CANT-ALF.
           EXEC CICS BIF DEEDIT FIELD (WE-CANTIDAD)
                                LENGTH(18)
           END-EXEC.
           MOVE WE-CANT-NUM (9:10)     TO WE-CUNICO.
      *--- VALIDAMOS SECUENCIA
           IF BSEC001I-SECUENCIA IS NOT NUMERIC
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (003)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (003)  TO BSE-DESCR-MENSAJE
              PERFORM TERMINAR-RUTINA
           END-IF.
           MOVE BSEC001I-SECUENCIA     TO WE-SECUENCIA.
      *--- VALIDAMOS OPCION NEXT-PREV
           IF BSEC001I-OPCION-NEXT-PREV NOT = 'NEXT'
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (004)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (004)  TO BSE-DESCR-MENSAJE
              PERFORM TERMINAR-RUTINA
           END-IF.
           MOVE BSEC001I-OPCION-NEXT-PREV TO WE-NEXT-PREV.
      *----------------------*
       VALIDAR-ACCESO-CLIENTE.
      *----------------------*
           INITIALIZE                     WE-COMMAREA-SCLO039.
           MOVE BSE-USER-ID            TO SCL-COD-USUARIO.
           MOVE WE-CUNICO (3:8)        TO SCL-COD-CLIENTE.
           MOVE 'BSEOINIC'             TO SCL-COD-MAPA.
           EXEC CICS LINK PROGRAM ('SCLO039')
                          COMMAREA(WE-COMMAREA-SCLO039)
                          LENGTH  (LENGTH OF WE-COMMAREA-SCLO039)
                          RESP    (WE-RC)
           END-EXEC.
           IF WE-RC NOT = DFHRESP(NORMAL)
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (020)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (020)  TO BSE-DESCR-MENSAJE
              MOVE 'SCLO039 '          TO BSE-DESCR-MENSAJE (26:8)
              PERFORM  TERMINAR-RUTINA
           END-IF.
           IF SCL-CRETORNO = '00'
              CONTINUE
           ELSE
              IF SCL-CRETORNO = '01'
                 MOVE 16                 TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (016) TO BSE-CODIGO-MENSAJE
                 MOVE WT01-TXT-MSG (016) TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
              ELSE
                 IF SCL-CRETORNO = '98'
                    MOVE 16                 TO BSE-CODIGO-RESPUESTA
                    MOVE WT01-COD-MSG (017) TO BSE-CODIGO-MENSAJE
                    MOVE WT01-TXT-MSG (017) TO BSE-DESCR-MENSAJE
                    PERFORM TERMINAR-RUTINA
                 ELSE
                    IF SCL-CRETORNO = '99'
                       MOVE 16                 TO BSE-CODIGO-RESPUESTA
                       MOVE WT01-COD-MSG (018) TO BSE-CODIGO-MENSAJE
                       MOVE WT01-TXT-MSG (018) TO BSE-DESCR-MENSAJE
                       PERFORM TERMINAR-RUTINA
                    ELSE
                       MOVE 16                 TO BSE-CODIGO-RESPUESTA
                       MOVE WT01-COD-MSG (019) TO BSE-CODIGO-MENSAJE
                       MOVE WT01-TXT-MSG (019) TO BSE-DESCR-MENSAJE
                       PERFORM TERMINAR-RUTINA
                    END-IF
                 END-IF
              END-IF
           END-IF.
      *--------------*
       BUSCAR-CLIENTE.
      *--------------*
           INITIALIZE                     WF-COMMAREA-CICS.
           MOVE ' '                    TO RM-ACCION
           MOVE '1'                    TO RM-FUNCTION
           MOVE '0003000000000000'     TO RM-CUST-CTLS-I
           MOVE '0000'                 TO RM-CUST-NBR-I (1:4)
           MOVE WE-CUNICO              TO RM-CUST-NBR-I (5:10)
           EXEC CICS LINK PROGRAM ('SRMR011')
                          COMMAREA(WF-COMMAREA-CICS)
                          LENGTH  (LENGTH OF WF-COMMAREA-CICS)
                          RESP    (WE-RC)
           END-EXEC.
           IF WE-RC NOT = DFHRESP(NORMAL)
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (020)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (020)  TO BSE-DESCR-MENSAJE
              MOVE 'SRMR011 '          TO BSE-DESCR-MENSAJE (26:8)
              PERFORM  TERMINAR-RUTINA
           END-IF.
           EVALUATE RM-RETURN-CODE
              WHEN '00'
                 MOVE 'T'                      TO WE-ACCESO
100048*          IF RMCMRTCS-OFF-EMP-DIR-CD = 'E' OR 'O' OR 'D'
100048*             PERFORM VALIDAR-ACCESO
100048*             IF WE-ACCESO = 'N'
100048*                MOVE 01                 TO BSE-CODIGO-RESPUESTA
100048*                MOVE WT01-COD-MSG (015) TO BSE-CODIGO-MENSAJE
100048*                MOVE WT01-TXT-MSG (015) TO BSE-DESCR-MENSAJE
100048*                PERFORM TERMINAR-RUTINA
100048*             END-IF
100048*          END-IF
              WHEN '01'
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (008)    TO BSE-CODIGO-MENSAJE
                 MOVE WT01-TXT-MSG (008)    TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
              WHEN '02'
                 MOVE 'T'                   TO WE-ACCESO
                 MOVE 01                    TO WE-CR-SRMR011
                 MOVE WT01-COD-MSG (009)    TO WE-CM-SRMR011
                 MOVE WT01-TXT-MSG (009)    TO WE-DM-SRMR011
              WHEN '03'
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (010)    TO BSE-CODIGO-MENSAJE
                 MOVE WT01-TXT-MSG (010)    TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
              WHEN '97'
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (011)    TO BSE-CODIGO-MENSAJE
                 MOVE WT01-TXT-MSG (011)    TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
              WHEN '98'
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 IF RM-RETURN-CODE-NOTOPEN = '97'
                    MOVE WT01-COD-MSG (011) TO BSE-CODIGO-MENSAJE
                    MOVE WT01-TXT-MSG (011) TO BSE-DESCR-MENSAJE
                 ELSE
                    MOVE WT01-COD-MSG (012) TO BSE-CODIGO-MENSAJE
                    MOVE WT01-TXT-MSG (012) TO BSE-DESCR-MENSAJE
                 END-IF
                 PERFORM TERMINAR-RUTINA
              WHEN OTHER
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (013)    TO BSE-CODIGO-MENSAJE
                 MOVE WT01-TXT-MSG (013)    TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
           END-EVALUATE.

100048*--------------*
100048*VALIDAR-ACCESO.
100048*--------------*
100048*    INITIALIZE                     REG-COMMAREA-USU.
100048*    MOVE BSE-USER-ID            TO USU-ID-USUARIO.
100048*    EXEC CICS LINK PROGRAM ('BSEOUSU')
100048*                   COMMAREA(REG-COMMAREA-USU)
100048*                   LENGTH  (LENGTH OF REG-COMMAREA-USU)
100048*                   RESP    (WE-RC)
100048*    END-EXEC.
100048*    IF WE-RC NOT = DFHRESP(NORMAL)
100048*       MOVE 16                  TO BSE-CODIGO-RESPUESTA
100048*       MOVE WT01-COD-MSG (020)  TO BSE-CODIGO-MENSAJE
100048*       MOVE WT01-TXT-MSG (020)  TO BSE-DESCR-MENSAJE
100048*       MOVE 'BSEOUSU '          TO BSE-DESCR-MENSAJE (26:8)
100048*       PERFORM  TERMINAR-RUTINA
100048*    END-IF.
100048*    EVALUATE USU-COD-RETORNO
100048*        WHEN '00'
100048*             EVALUATE RMCMRTCS-OFF-EMP-DIR-CD
100048*                 WHEN 'E' MOVE USU-FLAG-E TO WE-ACCESO
100048*                 WHEN 'O' MOVE USU-FLAG-O TO WE-ACCESO
100048*                 WHEN 'D' MOVE USU-FLAG-D TO WE-ACCESO
100048*             END-EVALUATE
100048*        WHEN '02'
100048*             MOVE 'N'                TO WE-ACCESO
100048*        WHEN OTHER
100048*             MOVE 16                 TO BSE-CODIGO-RESPUESTA
100048*             MOVE WT01-COD-MSG (014) TO BSE-CODIGO-MENSAJE
100048*             MOVE USU-MENSAJE        TO WT01-MSG-DSC (014)
100048*             MOVE WT01-TXT-MSG (014) TO BSE-DESCR-MENSAJE
100048*             PERFORM TERMINAR-RUTINA
100048*    END-EVALUATE.

      *----------------*
       BUSCAR-PRODUCTOS.
      *----------------*
           MOVE 0                      TO K.
           MOVE WX-T-NRO-MAX-OCCURS-TRAMA-OUT TO WE-NUM-ELEM-FALTAN.
           MOVE SPACES                 TO BS4-FLAG-CONTINUIDAD.
           MOVE '00'                   TO BS4-RETURN-CODE.
           MOVE ZEROS                  TO CONTADOR.
           COMPUTE WE-LINK-MAX = WX-T-NRO-MAX-OCCURS-TRAMA-OUT /
                                 WX-T-NRO-MAX-OCCURS-RUTINA
           END-COMPUTE.
           PERFORM UNTIL WE-NUM-ELEM-FALTAN   = 0    OR
                         BS4-FLAG-CONTINUIDAD = 'U'  OR
                         BS4-RETURN-CODE  NOT = '00' OR
                         CONTADOR            >= WE-LINK-MAX
              ADD  1                   TO CONTADOR
              PERFORM  LINK-BSER004
           END-PERFORM.
      *------------*
       LINK-BSER004.
      *------------*
           INITIALIZE                     WF-COMMAREA-BSER004C.
           MOVE WE-CUNICO              TO BS4-CODIGO-UNICO.
           MOVE WE-SECUENCIA           TO BS4-SECUENCIA-INPUT.
           MOVE WE-NEXT-PREV           TO BS4-OPCION-NEXT-PREV.
           MOVE WE-ACCESO              TO BS4-FILTRO.
           EXEC CICS LINK PROGRAM ('BSER004')
                          COMMAREA(WF-COMMAREA-BSER004C)
                          LENGTH  (LENGTH OF WF-COMMAREA-BSER004C)
                          RESP    (WE-RC)
           END-EXEC.
           IF WE-RC NOT = DFHRESP(NORMAL)
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (020)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (020)  TO BSE-DESCR-MENSAJE
              MOVE 'BSER004 '          TO BSE-DESCR-MENSAJE (26:8)
              PERFORM  TERMINAR-RUTINA
           END-IF.
           EVALUATE  BS4-RETURN-CODE
              WHEN '00'
              WHEN '04'
              WHEN '16'
              WHEN '17'
                 PERFORM ASIGNA-BSER004
              WHEN OTHER
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (005)    TO BSE-CODIGO-MENSAJE
                 MOVE BS4-ERROR-MESSAGE     TO WT01-MSG-DSC (005)
                 MOVE WT01-TXT-MSG (005)    TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
           END-EVALUATE.
      *--------------*
       ASIGNA-BSER004.
      *--------------*
           MOVE 0                       TO I.
           PERFORM UNTIL ( K >= WX-T-NRO-MAX-OCCURS-TRAMA-OUT ) OR
                         ( I >= BS4-QTY-REGISTROS )
              ADD  1                    TO I
              ADD  1                    TO K
              MOVE BS4-APLICATIVO  (I)  TO BSEC001O-ID-APLI         (K)
              MOVE BS4-ID-DOCU     (I)  TO BSEC001O-ID-DOCU         (K)
              MOVE BS4-SECUENCIA-OUTPUT (I)
                                        TO BSEC001O-SECUENCIA-OCCURS(K)
              MOVE BS4-PROD-CRM    (I)  TO BSEC001O-CODIGO-PRODUCTO (K)
              MOVE BS4-TIP-PROD-CRM(I)  TO BSEC001O-TIPO-PRODUCTO   (K)
              EVALUATE BS4-TIP-PROD-CRM (I)
                 WHEN '197 ' MOVE 'REF' TO BSEC001O-ESTADO          (K)
                 WHEN '199 ' MOVE 'JUD' TO BSEC001O-ESTADO          (K)
                 WHEN OTHER  MOVE 'VEN' TO BSEC001O-ESTADO          (K)
              END-EVALUATE
              MOVE BS4-SI-DOCU     (I)  TO BSEC001O-SI-DOCU         (K)
              MOVE BS4-SI-CNTA     (I)  TO BSEC001O-SI-CNTA         (K)
              MOVE BS4-SI-DOCU-APLI(I)  TO BSEC001O-SI-DOCU-APLI    (K)
              MOVE SPACES               TO BSEC001O-NUM-DOCU        (K)
              EVALUATE BS4-APLICATIVO  (I)
                 WHEN '$IM'
                    MOVE BS4-CO-TNDA-IM (I)
                                        TO BSEC001O-NUM-DOCU (K) (01:03)
                    MOVE '-'
                                        TO BSEC001O-NUM-DOCU (K) (04:01)
                    MOVE BS4-NU-CNTA-IM (I)
                                        TO BSEC001O-NUM-DOCU (K) (05:10)

                 WHEN '$ST'
                    MOVE BS4-CO-TNDA-ST (I)
                                        TO BSEC001O-NUM-DOCU (K) (01:03)
                    MOVE '-'
                                        TO BSEC001O-NUM-DOCU (K) (04:01)
                    MOVE BS4-NU-CNTA-ST (I) (5:10)
                                        TO BSEC001O-NUM-DOCU (K) (05:10)

                 WHEN OTHER
                    MOVE BS4-NU-DOCU    (I)
                                        TO BSEC001O-NUM-DOCU (K)
              END-EVALUATE
      *------ OBTENEMOS DESCRIPCION DE LA MONEDA
              MOVE BS4-MONEDA (I) (3:2) TO WE-MONEDA
              PERFORM BUSCA-MONEDA
              MOVE WE-MONEDA-DESC       TO BSEC001O-MONEDA-DESC     (K)
           END-PERFORM.
           MOVE BS4-FLAG-CONTINUIDAD    TO BSEC001O-FLAG-CONTINUIDAD.
           MOVE BS4-SECUENCIA-PRIMERO   TO BSEC001O-SECUENCIA-PRIMERO.
           MOVE BS4-SECUENCIA-ULTIMO    TO BSEC001O-SECUENCIA-ULTIMO.
           COMPUTE WE-SECUENCIA = BS4-SECUENCIA-ULTIMO + 1
           END-COMPUTE.
           MOVE K                       TO BSEC001O-CANTIDAD-PRODUCTOS.
           COMPUTE WE-NUM-ELEM-FALTAN =
                   WX-T-NRO-MAX-OCCURS-TRAMA-OUT - K
           END-COMPUTE.
      *---------------*
       BUSCAR-TARJETAS.
      *---------------*
           COMPUTE WE-NUM-ELEM-FALTAN =
                   WX-T-NRO-MAX-OCCURS-TRAMA-OUT - K
           END-COMPUTE.
           MOVE SPACES                 TO TCL-FLG-CONTINUA.
           MOVE 00                     TO TCL-COD-RESPTA.
           MOVE ZEROS                  TO CONTADOR.
           COMPUTE WE-LINK-MAX = WX-T-NRO-MAX-OCCURS-TRAMA-OUT /
                                 WX-T-NRO-MAX-OCCURS-RUTINA
           END-COMPUTE.
           PERFORM UNTIL WE-NUM-ELEM-FALTAN = 0   OR
                         TCL-FLG-CONTINUA   = 'U' OR
                         TCL-COD-RESPTA NOT = 00  OR
                         CONTADOR          >= WE-LINK-MAX
              ADD  1                   TO CONTADOR
              PERFORM  LINK-ATCO088
           END-PERFORM.
      *------------*
       LINK-ATCO088.
      *------------*
           INITIALIZE                     ATC88TCL.
           MOVE  'SAT'                 TO TCL-IDE-APP.
           MOVE  003                   TO TCL-COD-ENTIDA.
           MOVE  WE-CUNICO             TO TCL-COD-UNICO.
           MOVE  WE-SECUENCIA (2:7)    TO TCL-IDE-SECUENC.
           MOVE  WE-NEXT-PREV          TO TCL-FLG-LECTURA.
           EXEC CICS LINK PROGRAM ('ATCO088')
                          COMMAREA(ATC88TCL)
                          LENGTH  (LENGTH OF ATC88TCL)
                          RESP    (WE-RC)
           END-EXEC.
           IF WE-RC NOT = DFHRESP(NORMAL)
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (020)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (020)  TO BSE-DESCR-MENSAJE
              MOVE 'ATCO088 '          TO BSE-DESCR-MENSAJE (26:8)
              PERFORM  TERMINAR-RUTINA
           END-IF.
           EVALUATE TCL-COD-RESPTA
              WHEN 00
                 PERFORM ASIGNA-ATCO088
              WHEN 10
                 MOVE 01                 TO WE-CR-ATCO088
                 MOVE WT01-COD-MSG (007) TO WE-CM-ATCO088
                 MOVE TCL-COD-RESPTA     TO WT01-MSG-DSC (007) (4:2)
                 MOVE TCL-MSG-RESPTA     TO WT01-MSG-DSC (007) (7:45)
                 MOVE WT01-TXT-MSG (007) TO WE-DM-ATCO088
              WHEN OTHER
                 MOVE 16                 TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (007) TO BSE-CODIGO-MENSAJE
                 MOVE TCL-COD-RESPTA     TO WT01-MSG-DSC (007) (4:2)
                 MOVE TCL-MSG-RESPTA     TO WT01-MSG-DSC (007) (7:45)
                 MOVE WT01-TXT-MSG (007) TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
           END-EVALUATE.
      *--------------*
       ASIGNA-ATCO088.
      *--------------*
           IF WE-NUM-ELEM-FALTAN < TCL-NUM-ELEMEN
              MOVE 'C'                  TO BSEC001O-FLAG-CONTINUIDAD
              MOVE WE-NUM-ELEM-FALTAN   TO WE-NUM-ELEM
           ELSE
              MOVE TCL-FLG-CONTINUA     TO BSEC001O-FLAG-CONTINUIDAD
              MOVE TCL-NUM-ELEMEN       TO WE-NUM-ELEM
           END-IF.
           MOVE 0                       TO J.
           PERFORM UNTIL ( K >= WX-T-NRO-MAX-OCCURS-TRAMA-OUT ) OR
                         ( J >= WE-NUM-ELEM )
              ADD  1                    TO J
              ADD  1                    TO K
              IF J = 1
                 MOVE TCL-COD-SECUENC (J) TO BSEC001O-SECUENCIA-PRIMERO
                 MOVE 1             TO BSEC001O-SECUENCIA-PRIMERO (1:1)
              END-IF
              MOVE TCL-COD-SECUENC (J)  TO BSEC001O-SECUENCIA-OCCURS(K)
                                           BSEC001O-SECUENCIA-ULTIMO
                                           WE-SECUENCIA
              MOVE 'SAT'                TO BSEC001O-ID-APLI (K)
              MOVE TCL-NUM-CTATAR  (J)  TO BSEC001O-ID-DOCU (K) (01:07)
              MOVE TCL-PAN         (J)  TO BSEC001O-ID-DOCU (K) (08:16)
              MOVE SPACES               TO BSEC001O-ID-DOCU (K) (24:27)
              MOVE 1             TO BSEC001O-SECUENCIA-OCCURS (K) (1:1)
                                    BSEC001O-SECUENCIA-ULTIMO (1:1)
                                    WE-SECUENCIA (1:1)
              MOVE TCL-COD-PRODUC  (J)  TO BSEC001O-CODIGO-PRODUCTO (K)
              MOVE TCL-TIP-PRODUC  (J)  TO BSEC001O-TIPO-PRODUCTO   (K)
              MOVE '0000000000'         TO BSEC001O-NUM-DOCU (K)
              MOVE TCL-NUM-CTATAR  (J)  TO BSEC001O-NUM-DOCU (K) (4:7)
      *------ FLAG EXTRACASH
              IF TCL-FLAG-EXTRA (J) = 'E'
                MOVE TCL-FLAG-EXTRA (J) TO BSEC001O-NUM-DOCU (K) (11:1)
              END-IF
              EVALUATE TCL-TIP-PRODUC (J)
                 WHEN '197 ' MOVE 'REF' TO BSEC001O-ESTADO          (K)
                 WHEN '199 ' MOVE 'JUD' TO BSEC001O-ESTADO          (K)
                 WHEN OTHER  MOVE 'VEN' TO BSEC001O-ESTADO          (K)
              END-EVALUATE
      *------ OBTENEMOS DESCRIPCION DE LA MONEDA
              MOVE TCL-COD-MONCTA (J) (3:2) TO WE-MONEDA
              PERFORM BUSCA-MONEDA
              MOVE WE-MONEDA-DESC       TO BSEC001O-MONEDA-DESC     (K)
      *------ OBTENEMOS SITUACION DE LAS TARJETAS
              PERFORM BUSCAR-DETALLE-TARJETA
           END-PERFORM.
           MOVE K                       TO BSEC001O-CANTIDAD-PRODUCTOS.
           COMPUTE WE-NUM-ELEM-FALTAN =
                   WX-T-NRO-MAX-OCCURS-TRAMA-OUT - K
           END-COMPUTE.
      *------------*
       BUSCA-MONEDA.
      *------------*
           SET     W1                       TO  1.
           SEARCH  TB-RMONEDAS  AT  END
                   MOVE WE-MONEDA           TO  WE-MONEDA-DESC
           WHEN    TB-CODIGO (W1)  =  WE-MONEDA
                   MOVE TB-NOMBREM (W1)     TO  WE-MONEDA-DESC.
      *----------------------*
       BUSCAR-DETALLE-TARJETA.
      *----------------------*
100043     MOVE 'AC'                   TO BSEC001O-SI-CNTA (K)
           INITIALIZE                     ATC70CTI.
           MOVE 'SAT'                  TO CTI-IDE-APP.
           MOVE 003                    TO CTI-COD-ENTIDA.
           MOVE TCL-NUM-CTATAR (J)     TO CTI-NUM-CTATAR.
           MOVE TCL-PAN        (J)     TO CTI-PAN.
           EXEC CICS LINK PROGRAM ('ATCO070')
                          COMMAREA(ATC70CTI)
                          LENGTH  (LENGTH OF ATC70CTI)
                          RESP    (WE-RC)
           END-EXEC.
           IF WE-RC NOT = DFHRESP(NORMAL)
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (020)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (020)  TO BSE-DESCR-MENSAJE
              MOVE 'ATCO070 '          TO BSE-DESCR-MENSAJE (26:8)
              PERFORM  TERMINAR-RUTINA
           END-IF.
           EVALUATE  CTI-COD-RESPTA
              WHEN 00
100043*--------- ASIGNA INDICADOR DE TARJETA INACTIVA
100043           INSPECT CTI-FEC-BAJCTA  REPLACING ALL X'00' BY X'F0'
100043           INSPECT CTI-FEC-BAJCTA  REPLACING ALL X'40' BY X'F0'
100043           INSPECT CTI-COD-MOTBAJA REPLACING ALL X'00' BY X'F0'
100043           INSPECT CTI-COD-MOTBAJA REPLACING ALL X'40' BY X'F0'
100043           INSPECT CTI-DES-MOTBAJA REPLACING ALL X'00' BY X'40'
100043           IF CTI-FEC-BAJCTA          > 00010101   OR
100043              CTI-COD-MOTBAJA     NOT = '00'       OR
100043              CTI-DES-MOTBAJA     NOT = SPACES
100043              MOVE 'IN'          TO BSEC001O-SI-CNTA (K)
100043           END-IF
      *--------- FLAG CHIP
                 IF CTI-DES-TIPO (15:1) = 'S'
                    MOVE 'S'           TO BSEC001O-NUM-DOCU (K) (12:1)
                 END-IF
              WHEN OTHER
                 MOVE 01                 TO WE-CR-ATCO070
                 MOVE WT01-COD-MSG (006) TO WE-CM-ATCO070
                 MOVE CTI-COD-RESPTA     TO WT01-MSG-DSC (006) (4:2)
                 MOVE CTI-MSG-RESPTA     TO WT01-MSG-DSC (006) (7:45)
                 MOVE WT01-TXT-MSG (006) TO WE-DM-ATCO070
           END-EVALUATE.
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
                  (BSEC001O-CANTIDAD-PRODUCTOS    *
                   WX-T-LONG-CADA-OCCUR-TRAMA-OUT)
           END-COMPUTE.
           INSPECT REG-BSEC001O REPLACING ALL LOW-VALUES BY SPACES.
           MOVE REG-BSEC001O           TO BSE-DATOS.
      *---------------*
       TERMINAR-RUTINA.
      *---------------*
           MOVE REG-COMMAREA-BSE       TO DFHCOMMAREA.
           EXEC CICS RETURN
           END-EXEC.
           GOBACK.
