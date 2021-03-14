************************************************************************
800270* 800072 23/01/18 WCM   BSE ACTUALIZACION SEGMENTO CLIENTE VPC  *
***   * 104231 09/07/14 WCHM SISTEMA DE ATENCION IMAGINE SEGMENTACION *
***   * 102982 29/10/13 MENC CARGA INICIAL RATIONAL                   **
***   * 101022 01/04/13 PAMH AJUSTES VARIOS AL PROGRAMA                *
***   * FO6174 14/01/13 PAMH BUSQUEDA POR DOCUMENTO DE IDENTIDAD       *
***   * FO6813 11/07/12 PAMH PROYECTO BUS DE SERVICIO EMPRESARIAL      *
************************************************************************
*IDAPL*BSE
*OBJET*****************************************************************
*OBJET*** OPERACION CONSULTARLISTACLIENTES                           **
*OBJET*****************************************************************
      *=======================*
       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID. BSEO004.
      *====================*
       ENVIRONMENT DIVISION.
      *====================*
       DATA DIVISION.
      *=============*
       WORKING-STORAGE SECTION.
      *=======================*
       01  WE-ESPECIALES.
           02  I                       PIC 9(05)       VALUE ZEROS.
           02  J                       PIC 9(05)       VALUE ZEROS.
           02  K                       PIC 9(05)       VALUE ZEROS.
           02  WE-CONT                 PIC 9(05)       VALUE ZEROS.
           02  WE-RC                   PIC S9(08) COMP VALUE ZEROS.
           02  SW-OK                   PIC X(01)       VALUE SPACES.
           02  WE-BLANCO               PIC X(01)       VALUE SPACES.
           02  WE-CUNICO               PIC X(10)       VALUE SPACES.
           02  WE-BANCO                PIC X(04)       VALUE SPACES.
           02  WE-MONEDA               PIC X(04)       VALUE SPACES.
           02  WE-OFICINA              PIC X(04)       VALUE SPACES.
           02  WE-PRODUCTO             PIC X(04)       VALUE SPACES.
           02  WE-CUENTA               PIC X(10)       VALUE SPACES.
           02  WE-TARJCRED             PIC X(16)       VALUE SPACES.
           02  WE-TARJDEBI             PIC X(16)       VALUE SPACES.
FO6174     02  WE-TIPDOC               PIC X(01)       VALUE SPACES.
FO6174     02  WE-NUMDOC               PIC X(11)       VALUE SPACES.
      *    ----------------- CAMPOS PARA BIF DEEDIT -----------------*
           02  WE-CANTIDAD.
               03  WE-CANT-ALF         PIC X(18).
               03  WE-CANT-NUM         REDEFINES  WE-CANT-ALF
                                       PIC 9(18).
      *------------------ TABLA DE ERRORES ---------------------------*
       01  WT01-TABLA-MENSAJES.
           02  FILLER                  PIC X(64)  VALUE
                '001*TRANSACCION EXITOSA
      -         '-BSEO004 '.
           02  FILLER                  PIC X(64)  VALUE
                '002*OPCION NO VALIDA
      -         '-BSEO004 '.
           02  FILLER                  PIC X(64)  VALUE
                '003*ERROR CICS EN RUTINA
      -         '-BSEO004 '.
           02  FILLER                  PIC X(64)  VALUE
                '004*CODIGO DE CLIENTE NO VALIDO
      -         '-SRMR011 '.
           02  FILLER                  PIC X(64)  VALUE
                '005*CODIGO DE CLIENTE NO EXISTE
      -         '-SRMR011 '.
           02  FILLER                  PIC X(64)  VALUE
                '006*NUMERO DE CUENTA NO EXISTE
      -         '-SRMR011 '.
           02  FILLER                  PIC X(64)  VALUE
                '007*ARCHIVO CERRADO
      -         '-SRMR011 '.
           02  FILLER                  PIC X(64)  VALUE
                '008*ERROR CICS EN RUTINA
      -         '-SRMR011 '.
           02  FILLER                  PIC X(64)  VALUE
                '009*ERROR EN RUTINA
      -         '-SRMR011 '.
           02  FILLER                  PIC X(64)  VALUE
                '010*CODIGO DE CLIENTE NO VALIDO
      -         '-SRMR007 '.
           02  FILLER                  PIC X(64)  VALUE
                '011*CLIENTE NO TIENE DIRECCION DE CORREO
      -         '-SRMR007 '.
           02  FILLER                  PIC X(64)  VALUE
                '012*ARCHIVO CERRADO
      -         '-SRMR007 '.
           02  FILLER                  PIC X(64)  VALUE
                '013*ERROR CICS EN RUTINA
      -         '-SRMR007 '.
           02  FILLER                  PIC X(64)  VALUE
                '014*ERROR EN RUTINA
      -         '-SRMR007 '.
           02  FILLER                  PIC X(64)  VALUE
                '015*NUMERO DE CUENTA NO VALIDA
      -         '-SRMR013 '.
           02  FILLER                  PIC X(64)  VALUE
                '016*NUMERO DE CUENTA NO EXISTE
      -         '-SRMR013 '.
           02  FILLER                  PIC X(64)  VALUE
                '017*ARCHIVO CERRADO
      -         '-SRMR013 '.
           02  FILLER                  PIC X(64)  VALUE
                '018*ERROR CICS EN RUTINA
      -         '-SRMR013 '.
           02  FILLER                  PIC X(64)  VALUE
                '019*ERROR EN RUTINA
      -         '-SRMR013 '.
           02  FILLER                  PIC X(64)  VALUE
                '020*CUENTA EXISTE PERO SIN CLIENTES
      -         '-SRMR013 '.
           02  FILLER                  PIC X(64)  VALUE
                '021*ERROR EN RUTINA
      -         '-CLOCRM00'.
           02  FILLER                  PIC X(64)  VALUE
                '022*DOCUMENTO EXISTE PERO SIN CLIENTES
      -         '-CLOCRM00'.
           02  FILLER                  PIC X(64)  VALUE
                '023*DOCUMENTO INGRESADO NO ES PAGARE
      -         '-CLOCRM00'.
           02  FILLER                  PIC X(64)  VALUE
                '024*DOCUMENTO INGRESADO NO ES LEASING
      -         '-CLOCRM00'.
           02  FILLER                  PIC X(64)  VALUE
                '025*ERROR EN RUTINA
      -         '-RNCORTC '.
           02  FILLER                  PIC X(64)  VALUE
                '026*TARJETA DE CREDITO EXISTE PERO SIN CLIENTES
      -         '-RNCORTC '.
           02  FILLER                  PIC X(64)  VALUE
                '027*ERROR EN RUTINA
      -         '-BSEOTRJ '.
           02  FILLER                  PIC X(64)  VALUE
                '028*TARJETA DE DEBITO EXISTE PERO SIN CLIENTES
      -         '-BSEOTRJ '.
           02  FILLER                  PIC X(64)  VALUE
                '029*ERROR EN RUTINA
      -         '-BSEOUSU '.
           02  FILLER                  PIC X(64)  VALUE
                '030*USUARIO NO AUTORIZADO A CONSULTAR EMPLEADOS
      -         '-BSEO004 '.
           02  FILLER                  PIC X(64)  VALUE
                '031*SE EXCLUYERON EMPLEADOS TITULARES DE LA CUENTA
      -         '-BSEO004 '.
           02  FILLER                  PIC X(64)  VALUE
                '032*TIPO DE DOCUMENTO INVALIDO
      -         '-BSEO004 '.
           02  FILLER                  PIC X(64)  VALUE
                '033*ERROR AL LLAMAR PROGRAMA XXXXXXXX
      -         '-BSEO004 '.
           02  FILLER                  PIC X(64)  VALUE
                '034*NUMERO SECUENCIA DEBE SER NUMERICO
      -         '-BSEO004 '.
           02  FILLER                  PIC X(64)  VALUE
                '035*OPCION PAGINACION DEBE SER NEXT O PREV
      -         '-BSEO004 '.
           02  FILLER                  PIC X(64)  VALUE
                '036*LA OPERACION DE LA TRAMA NO EXISTE EN TABLA
      -         '-BSEO004 '.
FO6174     02  FILLER                  PIC X(64)  VALUE
FO6174          '037*CONTROLES NO NUMERICOS
FO6174-         '-SRMR031 '.
FO6174     02  FILLER                  PIC X(64)  VALUE
FO6174          '038*CODIGO UNICO NO EXISTE
FO6174-         '-SRMR031 '.
FO6174     02  FILLER                  PIC X(64)  VALUE
FO6174          '039*DOCUMENTO DE IDENTIDAD NO EXISTE
FO6174-         '-SRMR031 '.
FO6174     02  FILLER                  PIC X(64)  VALUE
FO6174          '040*ERROR CICS EN RUTINA
FO6174-         '-SRMR031 '.
FO6174     02  FILLER                  PIC X(64)  VALUE
FO6174          '041*ERROR EN RUTINA
FO6174-         '-SRMR031 '.
FO6174     02  FILLER                  PIC X(64)  VALUE
FO6174          '042*DOCUMENTO DE IDENTIDAD EXISTE PERO SIN CLIENTES
FO6174-         '-SRMR031 '.
       01  FILLER  REDEFINES  WT01-TABLA-MENSAJES.
FO6174     02  FILLER  OCCURS   42  TIMES.
               04  WT01-COD-MSG        PIC 9(03).
               04  FILLER              PIC X(01).
               04  WT01-TXT-MSG.
                   06  WT01-MSG-DSC    PIC X(51).
                   06  WT01-MSG-PRG    PIC X(09).
      *------------- TABLA DE OPERACIONES ----------------------------*
           COPY BSETOPER.
      *------------- RUTINA SRMR011 (CLIENTES) -----------------------*
       01  WF-COMMAREA-CICS.
           COPY SRMCICS.
      *------------- RUTINA BSEOUSU (SEGURIDAD USUARIO) --------------*
      *    COPY BSECUSU.
      *------------- RUTINA SRMR007 (DIRECCIONES) --------------------*
       01  WF-COMMAREA-007C.
           COPY SRMR007C.
      *------------- RUTINA SRMR013 (CUENTAS) ------------------------*
       01  WF-COMMAREA-CRCS.
           COPY SRMCRCS.
FO6174*------------- RUTINA SRMR031 (DOCUMENTO IDENTIDAD) ------------*
FO6174 01  WF-COMMAREA-DOCU.
FO6174     COPY SRMCDOCU.
      *------------- RUTINA CLOCRM00 (LETRA,PAGARE,LEASING,FACTORING -*
           COPY CL01CRM.
      *------------- RUTINA RNCORTC (TARJETA DE CREDITO) -------------*
           COPY RNC01TRC.
      *------------- RUTINA BSEOTRJ (TARJETA DE DEBITO) --------------*
           COPY BSECTRJ.
      *------------------ COMMAREA -----------------------------------*
           COPY BSECCOM.
      *------------------ ESTRUCTURA DE DATOS DE TRAMAS CRMR001 ------*
           COPY BSEC004I.
           COPY BSEC004O.
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
           MOVE BSE-DATOS              TO REG-BSEC004I.
           MOVE SPACES                 TO BSE-DATOS.

           MOVE SPACES                 TO REG-BSEC004O.
           MOVE SPACES                 TO BSEC004O-FLAG-CONTINUIDAD.
           MOVE SPACES                 TO BSEC004O-ULTIMO-REGISTRO.
           MOVE ZEROS                  TO BSEC004O-CANTIDAD-CLIENTES.

           MOVE 00                     TO BSE-CODIGO-RESPUESTA.
           MOVE WT01-COD-MSG (001)     TO BSE-CODIGO-MENSAJE.
           MOVE WT01-TXT-MSG (001)     TO BSE-DESCR-MENSAJE.

      *--- BUSCA LA OPERACION DE LA TRAMA INPUT EN LA TABLA OPERACIONES
           MOVE BSE-CODIGO-OPERACION   TO WX-OPERACION-BSETOPER.
           COPY BSELOPER.
           IF WX-SW-FOUND-BSETOPER = 'N'
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (036)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (036)  TO BSE-DESCR-MENSAJE
              PERFORM TERMINAR-RUTINA
           END-IF.
      *---------------*
       PROCESAR-RUTINA.
      *---------------*
           PERFORM  PROCESAR-CONSULTA.
           PERFORM  ASIGNAR-BSE-DATOS.
      *-----------------*
       PROCESAR-CONSULTA.
      *-----------------*
           EVALUATE BSEC004I-TIPO-DATO-BUSQUEDA
              WHEN  01
                    PERFORM CONSULTA-CODIGO-UNICO
              WHEN  02
                    PERFORM CONSULTA-NUMERO-CUENTA
              WHEN  03
                    PERFORM CONSULTA-DOCUMENTO
              WHEN  04
                    PERFORM CONSULTA-TARJETA-CREDITO
              WHEN  05
                    PERFORM CONSULTA-TARJETA-DEBITO
FO6174        WHEN  07
FO6174              PERFORM CONSULTA-DOCUMENTO-IDENTIDAD
              WHEN  OTHER
                    MOVE 16                 TO BSE-CODIGO-RESPUESTA
                    MOVE WT01-COD-MSG (002) TO BSE-CODIGO-MENSAJE
                    MOVE WT01-TXT-MSG (002) TO BSE-DESCR-MENSAJE
                    PERFORM TERMINAR-RUTINA
           END-EVALUATE.
      *--- VERIFICAMOS SI EXISTE ALGUN WARNING
           IF BSEC004O-CANTIDAD-CLIENTES = 00 AND WE-CONT > 00
              MOVE 01                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (030)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (030)  TO BSE-DESCR-MENSAJE
           END-IF.
           IF BSEC004O-CANTIDAD-CLIENTES > 00 AND WE-CONT > 00
              MOVE 01                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (031)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (031)  TO BSE-DESCR-MENSAJE
           END-IF.
      *---------------------*
       CONSULTA-CODIGO-UNICO.
      *---------------------*
           MOVE BSEC004I-DATO-BUSQUEDA TO WE-CANT-ALF.
           EXEC  CICS  BIF DEEDIT      FIELD  (WE-CANTIDAD)
                                       LENGTH (18)
                                       END-EXEC.
           MOVE WE-CANT-NUM (9:10)     TO WE-CUNICO
           MOVE 1                      TO I.
           PERFORM LLAMAR-SRMR011.
      *----------------------*
       CONSULTA-NUMERO-CUENTA.
      *----------------------*
           MOVE '0003'                        TO WE-BANCO.
           MOVE '00'                          TO WE-MONEDA   (1:2).
           MOVE BSEC004I-DATO-BUSQUEDA (4:2)  TO WE-MONEDA   (3:2).
           MOVE '0'                           TO WE-OFICINA  (1:1).
           MOVE BSEC004I-DATO-BUSQUEDA (6:3)  TO WE-OFICINA  (2:3).
           MOVE '0'                           TO WE-PRODUCTO (1:1).
           MOVE BSEC004I-DATO-BUSQUEDA (1:3)  TO WE-PRODUCTO (2:3).
           MOVE BSEC004I-DATO-BUSQUEDA (9:10) TO WE-CUENTA.
           PERFORM LLAMAR-SRMR013.
      *------------------*
       CONSULTA-DOCUMENTO.
      *------------------*
           EVALUATE BSEC004I-TIPO-DOC-PRODUCTO
              WHEN  01
              WHEN  03
                    PERFORM CONSULTA-LETRA-FACTORING
              WHEN  02
                    PERFORM CONSULTA-PAGARE
              WHEN  04
                    PERFORM CONSULTA-LEASING
              WHEN  OTHER
                    MOVE 16                 TO BSE-CODIGO-RESPUESTA
                    MOVE WT01-COD-MSG (032) TO BSE-CODIGO-MENSAJE
                    MOVE WT01-TXT-MSG (032) TO BSE-DESCR-MENSAJE
                    PERFORM TERMINAR-RUTINA
           END-EVALUATE.
      *------------------------*
       CONSULTA-LETRA-FACTORING.
      *------------------------*
           INITIALIZE REGCL-CRM-NIVEL1.
           MOVE BSEC004I-DATO-BUSQUEDA TO WE-CANT-ALF.
           EXEC  CICS  BIF DEEDIT      FIELD  (WE-CANTIDAD)
                                       LENGTH (18)
                                       END-EXEC.
           MOVE WE-CANT-NUM (11:8)     TO CRM-CL1-NRODOCA.
           MOVE 'PR'                   TO CRM-CL1-TIPO.
           PERFORM LLAMAR-CLOCRM00.
      *---------------*
       CONSULTA-PAGARE.
      *---------------*
           INITIALIZE REGCL-CRM-NIVEL1.
           MOVE BSEC004I-DATO-BUSQUEDA TO WE-CANT-ALF.
           EXEC  CICS  BIF DEEDIT      FIELD  (WE-CANTIDAD)
                                       LENGTH (18)
                                       END-EXEC.
           MOVE WE-CANT-NUM (11:8)     TO CRM-CL1-NRODOCA.
           IF CRM-CL1-NRODOCA (1:1) = '2'
              MOVE 'LS'                TO CRM-CL1-TIPO
           ELSE
              MOVE 'CL'                TO CRM-CL1-TIPO
           END-IF.
           PERFORM LLAMAR-CLOCRM00.
           IF CRM-CL1-RETORNO = '00' OR '04'
              IF CRM-CL1-TIPO NOT = 'PC' AND 'PV' AND 'CL'
                 MOVE 16                  TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (023)  TO BSE-CODIGO-MENSAJE
                 MOVE WT01-TXT-MSG (023)  TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
             END-IF
           END-IF.
      *----------------*
       CONSULTA-LEASING.
      *----------------*
           INITIALIZE REGCL-CRM-NIVEL1.
           MOVE BSEC004I-DATO-BUSQUEDA TO WE-CANT-ALF.
           EXEC  CICS  BIF DEEDIT      FIELD  (WE-CANTIDAD)
                                       LENGTH (18)
                                       END-EXEC.
           MOVE WE-CANT-NUM (11:8)     TO CRM-CL1-NRODOCA.
           MOVE 'LS'                   TO CRM-CL1-TIPO.
           PERFORM LLAMAR-CLOCRM00.
           IF CRM-CL1-RETORNO = '00' OR '04'
              IF CRM-CL1-TIPO NOT = 'LS'
                 MOVE 16                  TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (024)  TO BSE-CODIGO-MENSAJE
                 MOVE WT01-TXT-MSG (024)  TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
              END-IF
           END-IF.
      *------------------------*
       CONSULTA-TARJETA-CREDITO.
      *------------------------*
           MOVE BSEC004I-DATO-BUSQUEDA (1:16) TO WE-TARJCRED.
           PERFORM LLAMAR-RNCORTC.
      *-----------------------*
       CONSULTA-TARJETA-DEBITO.
      *-----------------------*
           MOVE BSEC004I-DATO-BUSQUEDA (1:16) TO WE-TARJDEBI.
           PERFORM LLAMAR-BSEOTRJ.
FO6174*----------------------------*
FO6174 CONSULTA-DOCUMENTO-IDENTIDAD.
FO6174*----------------------------*
FO6174     MOVE BSEC004I-TIPO-DOC-IDENTIDAD (2:1) TO WE-TIPDOC.
FO6174     MOVE BSEC004I-DATO-BUSQUEDA (1:11)     TO WE-NUMDOC.
FO6174     PERFORM LLAMAR-SRMR031.
      *--------------*
       LLAMAR-SRMR011.
      *--------------*
           INITIALIZE WF-COMMAREA-CICS.
           MOVE ' '                TO RM-ACCION      IN WF-COMMAREA-CICS
           MOVE '1'                TO RM-FUNCTION    IN WF-COMMAREA-CICS
           MOVE '0003000000000000' TO RM-CUST-CTLS-I IN WF-COMMAREA-CICS
           MOVE '0000'       TO RM-CUST-NBR-I IN WF-COMMAREA-CICS (1:4)
           MOVE WE-CUNICO    TO RM-CUST-NBR-I IN WF-COMMAREA-CICS (5:10)
           EXEC CICS LINK         PROGRAM ('SRMR011')
                                  COMMAREA(WF-COMMAREA-CICS)
                                  LENGTH  (LENGTH OF WF-COMMAREA-CICS)
                                  RESP    (WE-RC)
           END-EXEC.
           IF WE-RC NOT = DFHRESP(NORMAL)
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (033)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (033)  TO BSE-DESCR-MENSAJE
              MOVE 'SRMR011 '          TO BSE-DESCR-MENSAJE (26:8)
              PERFORM  TERMINAR-RUTINA
           END-IF.
           EVALUATE RM-RETURN-CODE IN WF-COMMAREA-CICS
              WHEN '00'
      *          IF RMCMRTCS-OFF-EMP-DIR-CD IN WF-COMMAREA-CICS
      *                                     = 'E' OR 'O' OR 'D'
      *             PERFORM LLAMAR-BSEOUSU
      *          ELSE
                    MOVE 'S'                TO SW-OK
      *          END-IF
                 IF SW-OK = 'S'
                    PERFORM ASIGNA-DATA
                 ELSE
                    SUBTRACT 1 FROM I
                    ADD      1 TO   WE-CONT
                 END-IF
              WHEN '01'
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (004)    TO BSE-CODIGO-MENSAJE
                 MOVE WT01-TXT-MSG (004)    TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
              WHEN '02'
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (005)    TO BSE-CODIGO-MENSAJE
                 MOVE WT01-TXT-MSG (005)    TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
              WHEN '03'
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (006)    TO BSE-CODIGO-MENSAJE
                 MOVE WT01-TXT-MSG (006)    TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
              WHEN '97'
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (007)    TO BSE-CODIGO-MENSAJE
                 MOVE WT01-TXT-MSG (007)    TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
              WHEN '98'
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 IF RM-RETURN-CODE-NOTOPEN IN WF-COMMAREA-CICS = '97'
                    MOVE WT01-COD-MSG (007) TO BSE-CODIGO-MENSAJE
                    MOVE WT01-TXT-MSG (007) TO BSE-DESCR-MENSAJE
                 ELSE
                    MOVE WT01-COD-MSG (008) TO BSE-CODIGO-MENSAJE
                    MOVE WT01-TXT-MSG (008) TO BSE-DESCR-MENSAJE
                 END-IF
                 PERFORM TERMINAR-RUTINA
              WHEN OTHER
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (009)    TO BSE-CODIGO-MENSAJE
                 MOVE WT01-TXT-MSG (009)    TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
           END-EVALUATE.
      *--------------*
      *LLAMAR-BSEOUSU.
      *--------------*
      *    INITIALIZE REG-COMMAREA-USU.
      *    MOVE BSE-USER-ID       TO USU-ID-USUARIO.
      *    EXEC CICS LINK         PROGRAM ('BSEOUSU')
      *                           COMMAREA(REG-COMMAREA-USU)
      *                           LENGTH  (LENGTH OF REG-COMMAREA-USU)
      *                           RESP    (WE-RC)
      *    END-EXEC.
      *    IF WE-RC NOT = DFHRESP(NORMAL)
      *       MOVE 16                  TO BSE-CODIGO-RESPUESTA
      *       MOVE WT01-COD-MSG (033)  TO BSE-CODIGO-MENSAJE
      *       MOVE WT01-TXT-MSG (033)  TO BSE-DESCR-MENSAJE
      *       MOVE 'BSEOUSU '          TO BSE-DESCR-MENSAJE (26:8)
      *       PERFORM  TERMINAR-RUTINA
      *    END-IF.
      *    EVALUATE USU-COD-RETORNO
      *        WHEN '00'
      *       IF (RMCMRTCS-OFF-EMP-DIR-CD IN WF-COMMAREA-CICS = 'E' AND
      *             USU-FLAG-E = 'N') OR
      *          (RMCMRTCS-OFF-EMP-DIR-CD IN WF-COMMAREA-CICS = 'O' AND
      *             USU-FLAG-O = 'N') OR
      *          (RMCMRTCS-OFF-EMP-DIR-CD IN WF-COMMAREA-CICS = 'D' AND
      *             USU-FLAG-D = 'N')
      *                MOVE 'N'             TO SW-OK
      *             ELSE
      *                MOVE 'S'             TO SW-OK
      *             END-IF
      *        WHEN '02'
      *             MOVE 'N'                TO SW-OK
      *        WHEN OTHER
      *             MOVE 16                 TO BSE-CODIGO-RESPUESTA
      *             MOVE WT01-COD-MSG (029) TO BSE-CODIGO-MENSAJE
      *             MOVE USU-MENSAJE        TO WT01-MSG-DSC (029)
      *             MOVE WT01-TXT-MSG (029) TO BSE-DESCR-MENSAJE
      *             PERFORM TERMINAR-RUTINA
      *    END-EVALUATE.
      *-----------*
       ASIGNA-DATA.
      *-----------*
           MOVE 'U'                TO BSEC004O-FLAG-CONTINUIDAD.
           MOVE SPACES             TO BSEC004O-ULTIMO-REGISTRO.
           MOVE I                  TO BSEC004O-CANTIDAD-CLIENTES.
           MOVE RM-CUST-NBR-I IN WF-COMMAREA-CICS
                                   TO BSEC004O-CODIGO-UNICO       (I).
           MOVE RMCMRTCS-CUST-TYP-CD IN WF-COMMAREA-CICS
                                   TO BSEC004O-TIPO-PERSONA       (I).
           MOVE RMCMRTCS-DT-LST-CUST-CONTACT IN WF-COMMAREA-CICS
                                   TO BSEC004O-ACT-DATOS          (I).
           IF BSEC004O-ACT-DATOS (I) (3:2) > 50
              MOVE 19              TO BSEC004O-ACT-DATOS (I) (1:2)
           ELSE
              IF BSEC004O-ACT-DATOS (I) (3:2) > 00
                 MOVE 20           TO BSEC004O-ACT-DATOS (I) (1:2)
              END-IF
           END-IF.
           IF RMCMRTCS-CUST-TYP-CD IN WF-COMMAREA-CICS = 'P'
              MOVE RMCMRTCS-PATERNAL-NAME IN WF-COMMAREA-CICS
                                   TO BSEC004O-APELLIDO-PATERNO   (I)
              MOVE RMCMRTCS-MATERNAL-NAME IN WF-COMMAREA-CICS
                                   TO BSEC004O-APELLIDO-MATERNO   (I)
              MOVE RMCMRTCS-FIRST-NAME IN WF-COMMAREA-CICS
                                   TO BSEC004O-PRIMER-NOMBRE      (I)
              MOVE RMCMRTCS-SECOND-NAME IN WF-COMMAREA-CICS
                                   TO BSEC004O-SEGUNDO-NOMBRE     (I)
              MOVE SPACES          TO BSEC004O-NOMBRE-EMPRESA     (I)
              MOVE RMCMPECS-GENDER-CD IN WF-COMMAREA-CICS
                                   TO BSEC004O-SEXO               (I)
              MOVE RMCMPECS-MARIT-CD IN WF-COMMAREA-CICS
                                   TO BSEC004O-ESTCIVIL           (I)
      *------ SEGMENTACION PERSONA NATURAL
              MOVE RMCMRTCS-FLAG-CLIE-EXCEP IN WF-COMMAREA-CICS
                                         TO BSEC004O-COD-SEG      (I)
              EVALUATE RMCMRTCS-FLAG-CLIE-EXCEP IN WF-COMMAREA-CICS
                 WHEN '  '
                    MOVE 'NO SEGMENTADO' TO BSEC004O-DES-SEG      (I)
                 WHEN '1 '
                    MOVE 'PREMIUM      ' TO BSEC004O-DES-SEG      (I)
                 WHEN '2 '
                    MOVE 'ORO          ' TO BSEC004O-DES-SEG      (I)
                 WHEN '3 '
                    MOVE 'PLATA        ' TO BSEC004O-DES-SEG      (I)
                 WHEN '4 '
                    MOVE 'BRONCE       ' TO BSEC004O-DES-SEG      (I)
                 WHEN OTHER
                    MOVE RMCMRTCS-FLAG-CLIE-EXCEP IN WF-COMMAREA-CICS
                                         TO BSEC004O-DES-SEG      (I)
              END-EVALUATE
           ELSE
              MOVE SPACES          TO BSEC004O-APELLIDO-PATERNO   (I)
              MOVE SPACES          TO BSEC004O-APELLIDO-MATERNO   (I)
              MOVE SPACES          TO BSEC004O-PRIMER-NOMBRE      (I)
              MOVE SPACES          TO BSEC004O-SEGUNDO-NOMBRE     (I)
              MOVE RMCMRTCS-NAME-LINE-1 IN WF-COMMAREA-CICS
                                   TO BSEC004O-NOMBRE-EMPRESA     (I)
              MOVE SPACES          TO BSEC004O-SEXO               (I)
              MOVE SPACES          TO BSEC004O-ESTCIVIL           (I)
      *------ SEGMENTACION COMERCIAL
              MOVE RMCMRTCS-FLAG-CLIE-EXCEP IN WF-COMMAREA-CICS
                                          TO BSEC004O-COD-SEG     (I)
              EVALUATE RMCMRTCS-FLAG-CLIE-EXCEP IN WF-COMMAREA-CICS
                  WHEN '  '
                     MOVE 'NO SEGMENTADO' TO BSEC004O-DES-SEG     (I)
                  WHEN '0 '
                     MOVE 'GESTOR CERO  ' TO BSEC004O-DES-SEG     (I)
                  WHEN '1 '
800270               MOVE 'PRIORITY     ' TO BSEC004O-DES-SEG     (I)
800270             WHEN '2 '
800270               MOVE 'ESTANDAR     ' TO BSEC004O-DES-SEG     (I)
800270            WHEN '3 '
800270               MOVE 'PASIVERO     ' TO BSEC004O-DES-SEG     (I)
                  WHEN '4 '
800270               MOVE 'SIN POTENCIAL' TO BSEC004O-DES-SEG     (I)
800270            WHEN '5 '
                     MOVE 'CLIENTE      ' TO BSEC004O-DES-SEG     (I)
                  WHEN OTHER
                     MOVE RMCMRTCS-FLAG-CLIE-EXCEP IN WF-COMMAREA-CICS
                                          TO BSEC004O-DES-SEG     (I)
              END-EVALUATE
           END-IF.
104231*----SEGMENTACION IMAGINE
104231     MOVE RMCMRTCS-ID-SEGM-IMG  IN WF-COMMAREA-CICS
104231                             TO BSEC004O-SEGM-IMG           (I).
           MOVE RMCMPECS-DOB IN WF-COMMAREA-CICS
                                   TO BSEC004O-FECHA-NACIMIENTO   (I).
           MOVE RMCMRTCS-TIN-CD IN WF-COMMAREA-CICS
                                   TO BSEC004O-TIPO-DOC-IDENTIDAD (I).
           MOVE RMCMRTCS-TIN IN WF-COMMAREA-CICS
                                   TO BSEC004O-NUME-DOC-IDENTIDAD (I).
           MOVE RMCMRTCS-CUST-QUAL-CD IN WF-COMMAREA-CICS
                                   TO BSEC004O-TIPO-CALIF-CLIENTE (I).
           MOVE SPACES             TO BSEC004O-EMAIL              (I).
           MOVE RMCMRTCS-ADDED-DT IN WF-COMMAREA-CICS
                                   TO BSEC004O-FECHA-CLIENTE-IB   (I).

      *--------- INFORMACION DE DOMICILIOS DEL CLIENTE
           PERFORM LLAMAR-SRMR007.
      *--------------*
       LLAMAR-SRMR007.
      *--------------*
           INITIALIZE WF-COMMAREA-007C.
           MOVE '0003000000000000' TO RM7-CUST-CTLS.
           MOVE '0000'             TO RM7-CUST-NBR (1:4).
           MOVE WE-CUNICO          TO RM7-CUST-NBR (5:10).
           EXEC CICS LINK         PROGRAM ('SRMR007')
                                  COMMAREA(WF-COMMAREA-007C)
                                  LENGTH  (LENGTH OF WF-COMMAREA-007C)
                                  RESP    (WE-RC)
           END-EXEC.
           IF WE-RC NOT = DFHRESP(NORMAL)
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (033)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (033)  TO BSE-DESCR-MENSAJE
              MOVE 'SRMR007 '          TO BSE-DESCR-MENSAJE (26:8)
              PERFORM  TERMINAR-RUTINA
           END-IF.
           EVALUATE RM7-RETURN-CODE
              WHEN '00'
                 MOVE 0                     TO K
                 PERFORM UNTIL ( K >= RM7-QTY-DOMICILIOS )
                    ADD 1 TO K
      *------------ SOLO ASIGNAMOS EL CORREO PERSONAL
                    IF RM7-TIPO-DOMICILIO (K) = 'EMAPER'
                       MOVE RM7-DIRECCION (K) TO BSEC004O-EMAIL (I)
                    END-IF
                 END-PERFORM
              WHEN '01'
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (010)    TO BSE-CODIGO-MENSAJE
                 MOVE WT01-TXT-MSG (010)    TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
              WHEN '02'
                 CONTINUE
              WHEN '97'
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (012)    TO BSE-CODIGO-MENSAJE
                 MOVE WT01-TXT-MSG (012)    TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
              WHEN '98'
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (013)    TO BSE-CODIGO-MENSAJE
                 MOVE WT01-TXT-MSG (013)    TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
              WHEN OTHER
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (014)    TO BSE-CODIGO-MENSAJE
                 MOVE WT01-TXT-MSG (014)    TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
           END-EVALUATE.
      *--------------*
       LLAMAR-SRMR013.
      *--------------*
           INITIALIZE WF-COMMAREA-CRCS.
           MOVE ' '  TO RM-ACCION      IN RM-REL-CUSTOMER-FOR-ACCOUNT.
           MOVE '0'  TO RM-FUNCTION    IN RM-REL-CUSTOMER-FOR-ACCOUNT.
           MOVE 'AC' TO RM-FROM-ENT-CD IN RM-REL-CUSTOMER-FOR-ACCOUNT.
           EVALUATE  WE-PRODUCTO
               WHEN  '0001'
                     MOVE '05'       TO RM-FROM-APPL-CD
                                 IN RM-REL-CUSTOMER-FOR-ACCOUNT
                     MOVE WE-CUENTA  TO RM-ACCT-NBR
                                 IN RM-REL-CUSTOMER-FOR-ACCOUNT (01:10)
                     MOVE SPACES     TO RM-ACCT-NBR
                                 IN RM-REL-CUSTOMER-FOR-ACCOUNT (11:17)
               WHEN  OTHER
                     MOVE '10'       TO RM-FROM-APPL-CD
                                 IN RM-REL-CUSTOMER-FOR-ACCOUNT
                     MOVE '0000'     TO RM-ACCT-NBR
                                 IN RM-REL-CUSTOMER-FOR-ACCOUNT (01:04)
                     MOVE WE-CUENTA  TO RM-ACCT-NBR
                                 IN RM-REL-CUSTOMER-FOR-ACCOUNT (05:10)
                     MOVE SPACES     TO RM-ACCT-NBR
                                 IN RM-REL-CUSTOMER-FOR-ACCOUNT (15:13)
           END-EVALUATE.
           MOVE WE-BANCO            TO RM-ACCT-CTLS
                                 IN RM-REL-CUSTOMER-FOR-ACCOUNT (01:04).
           MOVE WE-MONEDA           TO RM-ACCT-CTLS
                                 IN RM-REL-CUSTOMER-FOR-ACCOUNT (05:04).
           MOVE WE-OFICINA          TO RM-ACCT-CTLS
                                 IN RM-REL-CUSTOMER-FOR-ACCOUNT (09:04).
           MOVE WE-PRODUCTO         TO RM-ACCT-CTLS
                                 IN RM-REL-CUSTOMER-FOR-ACCOUNT (13:04).
           EXEC CICS LINK         PROGRAM ('SRMR013')
                                  COMMAREA(WF-COMMAREA-CRCS)
                                  LENGTH  (LENGTH OF WF-COMMAREA-CRCS)
                                  RESP    (WE-RC)
           END-EXEC.
           IF WE-RC NOT = DFHRESP(NORMAL)
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (033)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (033)  TO BSE-DESCR-MENSAJE
              MOVE 'SRMR013 '          TO BSE-DESCR-MENSAJE (26:8)
              PERFORM  TERMINAR-RUTINA
           END-IF.
           EVALUATE RM-RETURN-CODE IN RM-REL-CUSTOMER-FOR-ACCOUNT
              WHEN '00'
                 MOVE 0 TO I
                 MOVE 1 TO J
                 PERFORM UNTIL ( J > 4)
                    MOVE RMCMACRL-CUST-KEY
                 IN RM-REL-CUSTOMER-FOR-ACCOUNT(J) (21:10) TO WE-CUNICO
                    IF WE-CUNICO > '0000000000'
                       ADD 1 TO I
                       PERFORM LLAMAR-SRMR011
                    END-IF
                    ADD 1 TO J
                 END-PERFORM
                 IF I = 0 AND WE-CONT = 00
                    MOVE 01                 TO BSE-CODIGO-RESPUESTA
                    MOVE WT01-COD-MSG (020) TO BSE-CODIGO-MENSAJE
                    MOVE WT01-TXT-MSG (020) TO BSE-DESCR-MENSAJE
                 END-IF
              WHEN '01'
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (015)    TO BSE-CODIGO-MENSAJE
                 MOVE WT01-TXT-MSG (015)    TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
              WHEN '02'
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (016)    TO BSE-CODIGO-MENSAJE
                 MOVE WT01-TXT-MSG (016)    TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
              WHEN '97'
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (017)    TO BSE-CODIGO-MENSAJE
                 MOVE WT01-TXT-MSG (017)    TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
              WHEN '98'
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 IF RM-RETURN-CODE-NOTOPEN
                    IN RM-REL-CUSTOMER-FOR-ACCOUNT = '97'
                    MOVE WT01-COD-MSG (017) TO BSE-CODIGO-MENSAJE
                    MOVE WT01-TXT-MSG (017) TO BSE-DESCR-MENSAJE
                 ELSE
                    MOVE WT01-COD-MSG (018) TO BSE-CODIGO-MENSAJE
                    MOVE WT01-TXT-MSG (018) TO BSE-DESCR-MENSAJE
                 END-IF
                 PERFORM TERMINAR-RUTINA
              WHEN OTHER
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (019)    TO BSE-CODIGO-MENSAJE
                 MOVE WT01-TXT-MSG (019)    TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
           END-EVALUATE.
      *---------------*
       LLAMAR-CLOCRM00.
      *---------------*
           EXEC CICS LINK         PROGRAM ('CLOCRM00')
                                  COMMAREA(REGCL-CRM-NIVEL1)
                                  LENGTH  (LENGTH OF REGCL-CRM-NIVEL1)
                                  RESP    (WE-RC)
           END-EXEC.
           IF WE-RC NOT = DFHRESP(NORMAL)
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (033)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (033)  TO BSE-DESCR-MENSAJE
              MOVE 'CLOCRM00'          TO BSE-DESCR-MENSAJE (26:8)
              PERFORM  TERMINAR-RUTINA
           END-IF.
           EVALUATE CRM-CL1-RETORNO
              WHEN '00'
              WHEN '04'
                 MOVE 0 TO I
                 MOVE CRM-CL1-CODUNI        TO WE-CUNICO
                 IF WE-CUNICO > '0000000000'
                    ADD 1 TO I
                    PERFORM LLAMAR-SRMR011
                 END-IF
                 IF I = 0 AND WE-CONT = 00
                    MOVE 01                 TO BSE-CODIGO-RESPUESTA
                    MOVE WT01-COD-MSG (022) TO BSE-CODIGO-MENSAJE
                    MOVE WT01-TXT-MSG (022) TO BSE-DESCR-MENSAJE
                 END-IF
              WHEN '23'
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (021)    TO BSE-CODIGO-MENSAJE
                 EVALUATE BSEC004I-TIPO-DOC-PRODUCTO
                    WHEN 01
                       MOVE 'LETRA NO EXISTE'     TO WT01-MSG-DSC (021)
                       MOVE WT01-TXT-MSG (021)    TO BSE-DESCR-MENSAJE
                    WHEN 02
                       MOVE 'PAGARE NO EXISTE'    TO WT01-MSG-DSC (021)
                       MOVE WT01-TXT-MSG (021)    TO BSE-DESCR-MENSAJE
                    WHEN 03
                       MOVE 'FACTORING NO EXISTE' TO WT01-MSG-DSC (021)
                       MOVE WT01-TXT-MSG (021)    TO BSE-DESCR-MENSAJE
                    WHEN 04
                       MOVE 'LEASING NO EXISTE'   TO WT01-MSG-DSC (021)
                       MOVE WT01-TXT-MSG (021)    TO BSE-DESCR-MENSAJE
                    WHEN OTHER
                       MOVE 'DOCUMENTO NO EXISTE' TO WT01-MSG-DSC (021)
                       MOVE WT01-TXT-MSG (021)    TO BSE-DESCR-MENSAJE
                 END-EVALUATE
                 PERFORM TERMINAR-RUTINA
              WHEN OTHER
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (021)    TO BSE-CODIGO-MENSAJE
                 MOVE CRM-CL1-MENSAJE       TO WT01-MSG-DSC (021)
                 MOVE WT01-TXT-MSG (021)    TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
           END-EVALUATE.
      *--------------*
       LLAMAR-RNCORTC.
      *--------------*
           INITIALIZE RNC01TRC.
           MOVE SPACES            TO RNC01TRC.
           MOVE '0003'            TO TRC-CODENT.
           MOVE 'CLI'             TO TRC-FUNCION.
           MOVE WE-TARJCRED       TO TRC-PANCOM-P.
           EXEC CICS LINK         PROGRAM ('RNCORTC')
                                  COMMAREA(RNC01TRC)
                                  LENGTH  (LENGTH OF RNC01TRC)
                                  RESP    (WE-RC)
           END-EXEC.
           IF WE-RC NOT = DFHRESP(NORMAL)
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (033)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (033)  TO BSE-DESCR-MENSAJE
              MOVE 'RNCORTC '          TO BSE-DESCR-MENSAJE (26:8)
              PERFORM  TERMINAR-RUTINA
           END-IF.
           EVALUATE TRC-RC
              WHEN '00'
              WHEN '02'
              WHEN '05'
                 MOVE 0 TO I
                 MOVE TRC-CODCLI (5:10)     TO WE-CUNICO
                 IF WE-CUNICO > '0000000000'
                    ADD 1 TO I
                    PERFORM LLAMAR-SRMR011
                 END-IF
                 IF I = 0 AND WE-CONT = 00
                    MOVE 01                 TO BSE-CODIGO-RESPUESTA
                    MOVE WT01-COD-MSG (026) TO BSE-CODIGO-MENSAJE
                    MOVE WT01-TXT-MSG (026) TO BSE-DESCR-MENSAJE
                 END-IF
              WHEN '03'
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (025)    TO BSE-CODIGO-MENSAJE
                 MOVE 'TARJETA DE CREDITO NO EXISTE'
                                            TO WT01-MSG-DSC (025)
                 MOVE WT01-TXT-MSG (025)    TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
              WHEN '04'
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (025)    TO BSE-CODIGO-MENSAJE
                 MOVE 'CODIGO DE CLIENTE NO EXISTE'
                                            TO WT01-MSG-DSC (025)
                 MOVE WT01-TXT-MSG (025)    TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
              WHEN OTHER
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (025)    TO BSE-CODIGO-MENSAJE
                 MOVE TRC-MESSAGE           TO WT01-MSG-DSC (025)
                 MOVE WT01-TXT-MSG (025)    TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
           END-EVALUATE.
      *--------------*
       LLAMAR-BSEOTRJ.
      *--------------*
           INITIALIZE REG-COMMAREA-TRJ.
           MOVE 'TRJ'             TO TRJ-FUNCION.
           MOVE '1'               TO TRJ-OPCION.
           MOVE WE-TARJDEBI       TO TRJ-NU-TARJ-I.
           EXEC CICS LINK         PROGRAM ('BSEOTRJ')
                                  COMMAREA(REG-COMMAREA-TRJ)
                                  LENGTH  (LENGTH OF REG-COMMAREA-TRJ)
                                  RESP    (WE-RC)
           END-EXEC.
           IF WE-RC NOT = DFHRESP(NORMAL)
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (033)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (033)  TO BSE-DESCR-MENSAJE
              MOVE 'BSEOTRJ '          TO BSE-DESCR-MENSAJE (26:8)
              PERFORM  TERMINAR-RUTINA
           END-IF.
           EVALUATE TRJ-COD-RETORNO
              WHEN 00
                 MOVE 0 TO I
                 MOVE TRJ-ID-CLIE (1) (5:10) TO WE-CUNICO
                 IF WE-CUNICO > '0000000000'
                    ADD 1 TO I
                    PERFORM LLAMAR-SRMR011
                 END-IF
                 IF I = 0 AND WE-CONT = 00
                    MOVE 01                 TO BSE-CODIGO-RESPUESTA
                    MOVE WT01-COD-MSG (028) TO BSE-CODIGO-MENSAJE
                    MOVE WT01-TXT-MSG (028) TO BSE-DESCR-MENSAJE
                 END-IF
              WHEN 03
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (027)    TO BSE-CODIGO-MENSAJE
                 MOVE 'CODIGO DE CLIENTE NO EXISTE'
                                            TO WT01-MSG-DSC (027)
                 MOVE WT01-TXT-MSG (027)    TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
              WHEN 04
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (027)    TO BSE-CODIGO-MENSAJE
                 MOVE 'TARJETA DE DEBITO NO EXISTE'
                                            TO WT01-MSG-DSC (027)
                 MOVE WT01-TXT-MSG (027)    TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
              WHEN OTHER
                 MOVE 16                    TO BSE-CODIGO-RESPUESTA
                 MOVE WT01-COD-MSG (027)    TO BSE-CODIGO-MENSAJE
                 MOVE TRJ-MENSAJE           TO WT01-MSG-DSC (027)
                 MOVE WT01-TXT-MSG (027)    TO BSE-DESCR-MENSAJE
                 PERFORM TERMINAR-RUTINA
           END-EVALUATE.
FO6174*--------------*
FO6174 LLAMAR-SRMR031.
FO6174*--------------*
FO6174     INITIALIZE WF-COMMAREA-DOCU.
FO6174     MOVE 'R'               TO RM-ACCION      IN WF-COMMAREA-DOCU.
FO6174     MOVE '2'               TO RM-FUNCTION    IN WF-COMMAREA-DOCU.
FO6174     MOVE '0003'            TO RM-CTL1-CUST-I IN WF-COMMAREA-DOCU.
FO6174     MOVE '0000'            TO RM-CTL2-CUST-I IN WF-COMMAREA-DOCU.
FO6174     MOVE '0000'            TO RM-CTL3-CUST-I IN WF-COMMAREA-DOCU.
FO6174     MOVE '0000'            TO RM-CTL4-CUST-I IN WF-COMMAREA-DOCU.
FO6174     MOVE WE-TIPDOC         TO RM-TIN-CD      IN WF-COMMAREA-DOCU.
FO6174     MOVE WE-NUMDOC         TO RM-TIN         IN WF-COMMAREA-DOCU.
FO6174     MOVE SPACES            TO RM-CUST-NBR-I  IN WF-COMMAREA-DOCU.
FO6174     EXEC CICS LINK         PROGRAM ('SRMR031')
FO6174                            COMMAREA(WF-COMMAREA-DOCU)
FO6174                            LENGTH  (LENGTH OF WF-COMMAREA-DOCU)
FO6174                            RESP    (WE-RC)
FO6174     END-EXEC.
FO6174     IF WE-RC NOT = DFHRESP(NORMAL)
FO6174        MOVE 16                  TO BSE-CODIGO-RESPUESTA
FO6174        MOVE WT01-COD-MSG (033)  TO BSE-CODIGO-MENSAJE
FO6174        MOVE WT01-TXT-MSG (033)  TO BSE-DESCR-MENSAJE
FO6174        MOVE 'SRMR031 '          TO BSE-DESCR-MENSAJE (26:8)
FO6174        PERFORM  TERMINAR-RUTINA
FO6174     END-IF.
FO6174     EVALUATE RM-RETURN-CODE IN WF-COMMAREA-DOCU
FO6174        WHEN '00'
FO6174           MOVE 0 TO I
FO6174           MOVE RM-CUST-NBR-O IN WF-COMMAREA-DOCU (5:10)
FO6174                                      TO WE-CUNICO
FO6174           IF WE-CUNICO > '0000000000'
FO6174              ADD 1 TO I
FO6174              PERFORM LLAMAR-SRMR011
FO6174           END-IF
FO6174           IF I = 0 AND WE-CONT = 00
FO6174              MOVE 01                 TO BSE-CODIGO-RESPUESTA
FO6174              MOVE WT01-COD-MSG (042) TO BSE-CODIGO-MENSAJE
FO6174              MOVE WT01-TXT-MSG (042) TO BSE-DESCR-MENSAJE
FO6174           END-IF
FO6174        WHEN '01'
FO6174           MOVE 16                    TO BSE-CODIGO-RESPUESTA
FO6174           MOVE WT01-COD-MSG (037)    TO BSE-CODIGO-MENSAJE
FO6174           MOVE WT01-TXT-MSG (037)    TO BSE-DESCR-MENSAJE
FO6174           PERFORM TERMINAR-RUTINA
FO6174        WHEN '02'
FO6174           MOVE 16                    TO BSE-CODIGO-RESPUESTA
FO6174           MOVE WT01-COD-MSG (038)    TO BSE-CODIGO-MENSAJE
FO6174           MOVE WT01-TXT-MSG (038)    TO BSE-DESCR-MENSAJE
FO6174           PERFORM TERMINAR-RUTINA
FO6174        WHEN '03'
FO6174           MOVE 16                    TO BSE-CODIGO-RESPUESTA
FO6174           MOVE WT01-COD-MSG (039)    TO BSE-CODIGO-MENSAJE
FO6174           MOVE WT01-TXT-MSG (039)    TO BSE-DESCR-MENSAJE
FO6174           PERFORM TERMINAR-RUTINA
FO6174        WHEN '98'
FO6174           MOVE 16                    TO BSE-CODIGO-RESPUESTA
FO6174           MOVE WT01-COD-MSG (040)    TO BSE-CODIGO-MENSAJE
FO6174           MOVE WT01-TXT-MSG (040)    TO BSE-DESCR-MENSAJE
FO6174           PERFORM TERMINAR-RUTINA
FO6174        WHEN OTHER
FO6174           MOVE 16                    TO BSE-CODIGO-RESPUESTA
FO6174           MOVE WT01-COD-MSG (041)    TO BSE-CODIGO-MENSAJE
FO6174           MOVE WT01-TXT-MSG (041)    TO BSE-DESCR-MENSAJE
FO6174           PERFORM TERMINAR-RUTINA
FO6174     END-EVALUATE.
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
           MOVE WT01-COD-MSG (003)     TO BSE-CODIGO-MENSAJE.
           MOVE WT01-TXT-MSG (003)     TO BSE-DESCR-MENSAJE.
           PERFORM  TERMINAR-RUTINA.
      *-----------------*
       ASIGNAR-BSE-DATOS.
      *-----------------*
           COMPUTE BSE-LONGITUD-OUTPUT =
                   WX-LONG-HEADER-BSETOPER        +
                   WX-T-LONG-PARTE-FIJA-TRAMA-OUT +
                  (BSEC004O-CANTIDAD-CLIENTES     *
                   WX-T-LONG-CADA-OCCUR-TRAMA-OUT)
           END-COMPUTE.
           INSPECT REG-BSEC004O REPLACING ALL LOW-VALUES BY SPACES.
           MOVE REG-BSEC004O           TO BSE-DATOS.
      *---------------*
       TERMINAR-RUTINA.
      *---------------*
           MOVE REG-COMMAREA-BSE       TO DFHCOMMAREA.
           EXEC CICS RETURN
           END-EXEC.
           GOBACK.
