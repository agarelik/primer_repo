************************************************************************
***   * FO6813 11/07/12 PAMH PROYECTO BUS DE SERVICIO EMPRESARIAL      *
************************************************************************
*IDAPL*BSE
*OBJET*****************************************************************
*OBJET*** OPERACION CONSULTARPRELIQCOBRANZAJUDICIALCREDITO           **
*OBJET*****************************************************************
      *=======================*
       IDENTIFICATION DIVISION.
      *=======================*
       PROGRAM-ID. BSEO003.
      *====================*
       ENVIRONMENT DIVISION.
      *====================*
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
      -         '-BSEO003 '.
           02  FILLER                  PIC X(64)  VALUE
                '002*ERROR EN RUTINA
      -         '-BSEO003 '.
           02  FILLER                  PIC X(64)  VALUE
                '003*ERROR EN RUTINA
      -         '-CLOCRM37'.
           02  FILLER                  PIC X(64)  VALUE
                '004*ERROR AL LLAMAR PROGRAMA XXXXXXXX
      -         '-BSEO003 '.
           02  FILLER                  PIC X(64)  VALUE
                '005*APLICATIVO DEBE SER $CL
      -         '-BSEO003 '.
           02  FILLER                  PIC X(64)  VALUE
                '006*ENTIDAD DEBE SER 003
      -         '-BSEO003 '.
       01  FILLER  REDEFINES  WT01-TABLA-MENSAJES.
           02  FILLER  OCCURS   6  TIMES.
               04  WT01-COD-MSG        PIC 9(03).
               04  FILLER              PIC X(01).
               04  WT01-TXT-MSG.
                   06  WT01-MSG-DSC    PIC X(51).
                   06  WT01-MSG-PRG    PIC X(09).
      *--- COPY PARA RUTINA CLLOCRM02 ($CL) --------------------------*
           COPY CL02CRM.
      *------------------ COMMAREA -----------------------------------*
           COPY BSECCOM.
      *------------------ ESTRUCTURA DE DATOS DE TRAMAS BSEO003 ------*
           COPY BSEC003I.
           COPY BSEC003O.
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
           MOVE BSE-DATOS              TO REG-BSEC003I.
           MOVE SPACES                 TO BSE-DATOS.
           MOVE SPACES                 TO REG-BSEC003O.
           INITIALIZE                     REG-BSEC003O.
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
           IF BSEC003I-ID-APLI NOT = '$CL'
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (005)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (005)  TO BSE-DESCR-MENSAJE
              PERFORM TERMINAR-RUTINA
           END-IF.
      *--- VALIDAMOS ENTIDAD
           IF BSEC003I-CODENT NOT = '003'
              MOVE 16                  TO BSE-CODIGO-RESPUESTA
              MOVE WT01-COD-MSG (006)  TO BSE-CODIGO-MENSAJE
              MOVE WT01-TXT-MSG (006)  TO BSE-DESCR-MENSAJE
              PERFORM TERMINAR-RUTINA
           END-IF.
      *-----------------*
       PROCESAR-CONSULTA.
      *-----------------*
           INITIALIZE                     CRM-CO-COMMAREA.
           MOVE 037                    TO CRM-CO-NRO-TRAMA
           MOVE BSEC003I-ID-APLI       TO CRM-CO-ID-APLI
           MOVE BSEC003I-CODENT        TO CRM-CO-ID-BANCO
           MOVE BSEC003I-TI-DOCU-CL    TO CRM-CO-TIPDOC
           MOVE BSEC003I-NU-DOCU-CL    TO CRM-CO-NUMERO
           MOVE BSEC003I-FECPREL       TO CRM-CO-FECHA-PRQ
           MOVE BSEC003I-TIPPREL       TO CRM-CO-TIP-PRLQ
           MOVE BSEC003I-NVOIMP        TO CRM-CO-IMP-PAGO
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
      *----------------*
       ARMAR-TRAMA.
      *----------------*
           MOVE CRM-CO-AMORTI-37       TO BSEC003O-AMORTIZ.
           MOVE CRM-CO-INTVIG-37       TO BSEC003O-INTVIGENTE.
           MOVE CRM-CO-INTMOR-37       TO BSEC003O-INTMORAT.
           MOVE CRM-CO-INTCOM-37       TO BSEC003O-INTCOMPEN.
           MOVE CRM-CO-DIASVIG-37      TO BSEC003O-DIASVIG.
           MOVE CRM-CO-DIASVEN-37      TO BSEC003O-DIASVEN.
           MOVE CRM-CO-GASPRT-37       TO BSEC003O-PROTCOBJ.
           MOVE CRM-CO-GASTOS-37       TO BSEC003O-GASTOS.
           MOVE CRM-CO-HONPRO-37       TO BSEC003O-HONPRO.
           MOVE CRM-CO-IGV-37          TO BSEC003O-IGV.
           MOVE CRM-CO-TOTPAG-37       TO BSEC003O-TOTPAGAR.
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
           MOVE 16                     TO BSE-CODIGO-RESPUESTA
           MOVE WT01-COD-MSG (002)     TO BSE-CODIGO-MENSAJE.
           MOVE WT01-TXT-MSG (002)     TO BSE-DESCR-MENSAJE.
           PERFORM  TERMINAR-RUTINA.
      *-----------------*
       ASIGNAR-BSE-DATOS.
      *-----------------*
           INSPECT REG-BSEC003O REPLACING ALL LOW-VALUES BY SPACES.
           MOVE REG-BSEC003O           TO BSE-DATOS.
      *---------------*
       TERMINAR-RUTINA.
      *---------------*
           MOVE REG-COMMAREA-BSE       TO DFHCOMMAREA.
           EXEC CICS RETURN
           END-EXEC.
           GOBACK.
