************************************************************************
***   * FO6813 10/07/12 PAMH PROYECTO BUS DE SERVICIO EMPRESARIAL      *
************************************************************************
*IDAPL*BSE
*OBJET*****************************************************************
*OBJET*** OPERACION CONSULTARCOBRANZAJUDICIALCREDITO                 **
*OBJET*****************************************************************
      *=======================*
       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID. BSEO002.
      *====================*
       ENVIRONMENT DIVISION.
      *====================*
      * no se usa environment division en un programa cics
       DATA DIVISION.
      *=============*
       WORKING-STORAGE SECTION.
      *=======================*
       01  WE-ESPECIALES.
           02  WE-RC                   PIC S9(08) COMP VALUE ZEROS.
           02  WE-BLANCO               PIC X(01)  VALUE SPACES.
      *------------------ TABLA DE ERRORES ---------------------------*
       01  WT01-TABLA-MENSAJES.
           02  FILLER                  PIC X(64)  VALUE
                '001*TRANSACCION EXITOSA
      -         '-BSEO002 '.
           02  FILLER                  PIC X(64)  VALUE
                '002*ERROR EN RUTINA
      -         '-BSEO002 '.
           02  FILLER                  PIC X(64)  VALUE
                '003*ERROR EN RUTINA
      -         '-CLOCRM36'.
           02  FILLER                  PIC X(64)  VALUE
                '004*ERROR AL LLAMAR PROGRAMA XXXXXXXX
      -         '-BSEO002 '.
           02  FILLER                  PIC X(64)  VALUE
                '005*APLICATIVO DEBE SER $CL
      -         '-BSEO002 '.
           02  FILLER                  PIC X(64)  VALUE
                '006*ENTIDAD DEBE SER 003
      -         '-BSEO002 '.
       01  FILLER  REDEFINES  WT01-TABLA-MENSAJES.
           02  FILLER  OCCURS   6  TIMES.
               04  WT01-COD-MSG        PIC 9(03).
               04  FILLER              PIC X(01).
               04  WT01-TXT-MSG.
                   06  WT01-MSG-DSC    PIC X(51).
                   06  WT01-MSG-PRG    PIC X(09).
      *--- COPY PARA RUTINA CLOCRM02 ($CL) ---------------------------*
           COPY CL02CRM.
      *------------------ COMMAREA -----------------------------------*
           COPY BSECCOM.
      *------------------ ESTRUCTURA DE DATOS DE TRAMAS BSEO002 ------*
           COPY BSEC002I.
           COPY BSEC002O.
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
           MOVE BSE-DATOS              TO REG-BSEC002I.
           MOVE SPACES                 TO BSE-DATOS.
           MOVE SPACES                 TO REG-BSEC002O.
           INITIALIZE                     REG-BSEC002O.
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
           IF BSEC002I-ID-APLI NOT = '$CL'
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (005)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (005)  TO BSE-DESCR-MENSAJE
              PERFORM TERMINAR-RUTINA
           END-IF.
      *--- VALIDAMOS ENTIDAD
           IF BSEC002I-CODENT NOT = '003'
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (006)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (006)  TO BSE-DESCR-MENSAJE
              PERFORM TERMINAR-RUTINA
           END-IF.
      *-----------------*
       PROCESAR-CONSULTA.
      *-----------------*
           INITIALIZE                     CRM-CO-COMMAREA.
           MOVE 036                    TO CRM-CO-NRO-TRAMA.
           MOVE BSEC002I-ID-APLI       TO CRM-CO-ID-APLI.
           MOVE BSEC002I-CODENT        TO CRM-CO-ID-BANCO.
           MOVE BSEC002I-TI-DOCU-CL    TO CRM-CO-TIPDOC.
           MOVE BSEC002I-NU-DOCU-CL    TO CRM-CO-NUMERO.
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
                    IF CRM-CO-MENSAJE (1:16) = 'PAGARE NO EXISTE' OR
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
           MOVE CRM-CO-CUENTA-36       TO BSEC002O-CTAAFI.
           MOVE CRM-CO-NUMERO-36       TO BSEC002O-NROBOVEDA.
           MOVE CRM-CO-COD-GL          TO BSEC002O-CODGL.
           MOVE CRM-CO-DESC-GL         TO BSEC002O-DESCODGL.
           MOVE CRM-CO-SITUAC-36       TO BSEC002O-CODSIT.
           MOVE CRM-CO-DESCR-SITUAC-36 TO BSEC002O-DESCODSIT.
           MOVE CRM-CO-SECTORISTA      TO BSEC002O-SECTORISTA.
           MOVE CRM-CO-FEC-ING-36      TO BSEC002O-FECINGRESO.
           MOVE CRM-CO-IMPO-ORIG       TO BSEC002O-IMPORI.
           MOVE CRM-CO-SALACT          TO BSEC002O-SALACT.
           MOVE CRM-CO-INTANT          TO BSEC002O-INTANT.
           MOVE CRM-CO-INTCOM          TO BSEC002O-INTCOM.
           MOVE CRM-CO-INTMOR          TO BSEC002O-INTMOR.
           MOVE CRM-CO-GASTOS          TO BSEC002O-GASTOS.
           MOVE CRM-CO-SEGUROS         TO BSEC002O-SEGUROS.
           MOVE CRM-CO-GASTO-PRDORI    TO BSEC002O-GASPROORI.
           MOVE CRM-CO-HONPRO          TO BSEC002O-HONPRO.
           MOVE CRM-CO-GASPROT         TO BSEC002O-GASPROT.
           MOVE CRM-CO-IGV             TO BSEC002O-IGV.
           MOVE CRM-TASINTCOM          TO BSEC002O-TASINTCOM.
           MOVE CRM-TASINTMOR          TO BSEC002O-TASINTMOR.
           MOVE CRM-TASHONCLI          TO BSEC002O-TASHONCLI.
           MOVE CRM-CO-COD-ESTUDIO     TO BSEC002O-CODESTUDIO.
           MOVE CRM-CO-DESC-ESTUDIO    TO BSEC002O-DESESTUDIO.
           MOVE CRM-CO-PROD-ORIGEN     TO BSEC002O-PRODORIGEN.
           MOVE CRM-CO-FEC-VEN-36      TO BSEC002O-FECVENCIM.
           MOVE CRM-CO-MONEDA-36       TO BSEC002O-SIMMONEDA.
           MOVE CRM-CO-DESCR-MON-36    TO BSEC002O-DESMONEDA.
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
           INSPECT REG-BSEC002O REPLACING ALL LOW-VALUES BY SPACES.
           MOVE REG-BSEC002O           TO BSE-DATOS.
      *---------------*
       TERMINAR-RUTINA.
      *---------------*
           MOVE REG-COMMAREA-BSE       TO DFHCOMMAREA.
           EXEC CICS RETURN
           END-EXEC.
           GOBACK.
