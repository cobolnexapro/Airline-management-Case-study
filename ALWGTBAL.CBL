       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALWGTBAL.
       AUTHOR. ENTERPRISE-IBM-I-DEVELOPER.
       DATE-WRITTEN. JUNE 2026.
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-I.
       OBJECT-COMPUTER. IBM-I.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AIRCRAFTPF ASSIGN TO DATABASE-AIRCRAFTPF
                  ORGANIZATION IS INDEXED
                  ACCESS IS RANDOM
                  RECORD KEY IS DB-TAIL-NUM
                  FILE STATUS IS WS-ACF-STATUS.
                  
           SELECT MANIFESTPF ASSIGN TO DATABASE-MANIFESTPF
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS DB-FLT-ID
                  FILE STATUS IS WS-MAN-STATUS.

           SELECT CARGOPF ASSIGN TO DATABASE-CARGOPF
                  ORGANIZATION IS INDEXED
                  ACCESS IS DYNAMIC
                  RECORD KEY IS DB-CARGO-FLT-ID
                  FILE STATUS IS WS-CRG-STATUS.

           SELECT ALWGTBALS ASSIGN TO WORKSTATION-ALWGTBALS
                  ORGANIZATION IS TRANSACTION
                  ACCESS IS DYNAMIC
                  RELATIVE KEY IS WS-SFL-RRN
                  FILE STATUS IS WS-DSP-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  AIRCRAFTPF.
       01  AIRCRAFT-RECORD.
           05 DB-TAIL-NUM          PIC X(6).
           05 DB-AIRCRAFT-TYPE     PIC X(4).
           05 DB-DOW-KG            PIC 9(6)V99. *> Dry Operating Weight
           05 DB-DOW-INDEX         PIC 9(5)V99. *> Base Moment Index
           05 DB-MAC-LEMAC         PIC 9(4)V99. *> Leading Edge MAC
           05 DB-MAC-LENGTH        PIC 9(4)V99.
           05 DB-MTOW-LIMIT        PIC 9(6)V99. *> Max Takeoff Wgt
           05 DB-MZFW-LIMIT        PIC 9(6)V99. *> Max Zero Fuel Wgt
           05 DB-MLAW-LIMIT        PIC 9(6)V99. *> Max Landing Wgt

       FD  MANIFESTPF.
       01  MANIFEST-RECORD.
           05 DB-FLT-ID            PIC X(10).
           05 DB-PAX-ZONE-A-QTY    PIC 9(3).
           05 DB-PAX-ZONE-B-QTY    PIC 9(3).
           05 DB-PAX-ZONE-C-QTY    PIC 9(3).
           05 DB-PAX-ZONE-D-QTY    PIC 9(3).
           05 DB-PAX-ZONE-E-QTY    PIC 9(3).
           05 DB-CREW-QTY          PIC 9(2).

       FD  CARGOPF.
       01  CARGO-RECORD.
           05 DB-CARGO-FLT-ID      PIC X(10).
           05 DB-FWD-HOLD-KG       PIC 9(5)V99.
           05 DB-AFT-HOLD-KG       PIC 9(5)V99.
           05 DB-BULK-HOLD-KG      PIC 9(4)V99.
           05 DB-TAXI-FUEL-KG      PIC 9(4)V99.
           05 DB-TRIP-FUEL-KG      PIC 9(5)V99.
           05 DB-RSV-FUEL-KG       PIC 9(5)V99.

       FD  ALWGTBALS.
       01  DISPLAY-RECORD          PIC X(1920).

       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * SYSTEM AND FILE STATUS FLAGS
      *----------------------------------------------------------------*
       01  WS-FLAGS.
           05 WS-ACF-STATUS        PIC X(2) VALUE '00'.
           05 WS-MAN-STATUS        PIC X(2) VALUE '00'.
           05 WS-CRG-STATUS        PIC X(2) VALUE '00'.
           05 WS-DSP-STATUS        PIC X(2) VALUE '00'.
           05 WS-VALID-FLAG        PIC X(1) VALUE 'Y'.
              88 IS-VALID                   VALUE 'Y'.
              88 IS-INVALID                 VALUE 'N'.
           05 WS-TRIM-FLAG         PIC X(1) VALUE 'Y'.
              88 IN-TRIM                    VALUE 'Y'.
              88 OUT-OF-TRIM                VALUE 'N'.

      *----------------------------------------------------------------*
      * STANDARD WEIGHT DEFINITIONS (WINTER/SUMMER AVERAGES IATA)
      *----------------------------------------------------------------*
       01  WS-STD-WEIGHTS.
           05 WS-ADULT-PAX-KG      PIC 9(2)V99 VALUE 84.00.
           05 WS-CHILD-PAX-KG      PIC 9(2)V99 VALUE 35.00.
           05 WS-CREW-KG           PIC 9(2)V99 VALUE 90.00. *> Includes bags

      *----------------------------------------------------------------*
      * DYNAMIC MOMENT ARMS (METERS FROM DATUM)
      *----------------------------------------------------------------*
       01  WS-MOMENT-ARMS.
           05 WS-ZONE-A-ARM        PIC 9(3)V99.
           05 WS-ZONE-B-ARM        PIC 9(3)V99.
           05 WS-ZONE-C-ARM        PIC 9(3)V99.
           05 WS-ZONE-D-ARM        PIC 9(3)V99.
           05 WS-ZONE-E-ARM        PIC 9(3)V99.
           05 WS-FWD-HOLD-ARM      PIC 9(3)V99.
           05 WS-AFT-HOLD-ARM      PIC 9(3)V99.
           05 WS-BULK-HOLD-ARM     PIC 9(3)V99.
           05 WS-WING-TANK-ARM     PIC 9(3)V99.
           05 WS-CTR-TANK-ARM      PIC 9(3)V99.

      *----------------------------------------------------------------*
      * ACCUMULATORS FOR MASS AND MOMENT (M=W*D)
      *----------------------------------------------------------------*
       01  WS-ACCUMULATORS.
           05 WS-TOTAL-MOMENT      PIC 9(9)V99 VALUE 0.
           05 WS-TOTAL-WEIGHT      PIC 9(7)V99 VALUE 0.
           
           05 WS-ZFW-KG            PIC 9(7)V99 VALUE 0.
           05 WS-ZFW-MOMENT        PIC 9(9)V99 VALUE 0.
           05 WS-ZFW-CG            PIC 9(3)V99 VALUE 0.
           05 WS-ZFW-MAC           PIC S9(3)V99 VALUE 0.
           
           05 WS-TOW-KG            PIC 9(7)V99 VALUE 0.
           05 WS-TOW-MOMENT        PIC 9(9)V99 VALUE 0.
           05 WS-TOW-CG            PIC 9(3)V99 VALUE 0.
           05 WS-TOW-MAC           PIC S9(3)V99 VALUE 0.

           05 WS-LAW-KG            PIC 9(7)V99 VALUE 0.
           05 WS-LAW-MOMENT        PIC 9(9)V99 VALUE 0.
           05 WS-LAW-CG            PIC 9(3)V99 VALUE 0.
           05 WS-LAW-MAC           PIC S9(3)V99 VALUE 0.

           05 WS-TEMP-MOMENT       PIC 9(9)V99 VALUE 0.

      *----------------------------------------------------------------*
      * SCREEN INPUT/OUTPUT FIELDS (MAPPED FROM DDS)
      *----------------------------------------------------------------*
       01  WS-SCREEN-DATA.
           05 SCR-FLT-ID           PIC X(10).
           05 SCR-TAIL-NUM         PIC X(6).
           05 SCR-ACTION           PIC X(1).
           05 SCR-MAC-ZFW-DSP      PIC -99.99.
           05 SCR-MAC-TOW-DSP      PIC -99.99.
           05 SCR-MAC-LAW-DSP      PIC -99.99.
           05 SCR-TRIM-STATUS      PIC X(15).
           05 SCR-ERR-MSG          PIC X(70).

       01  WS-INDICATORS.
           05 IN03                 PIC 1 INDICATOR 3.

       PROCEDURE DIVISION.
       0000-MAIN-LOGIC.
           PERFORM 1000-INITIALIZE.
           PERFORM 2000-PROCESS-SCREEN UNTIL IN03 = B"1".
           PERFORM 9000-TERMINATE.
           STOP RUN.

       1000-INITIALIZE.
           OPEN INPUT AIRCRAFTPF.
           OPEN INPUT MANIFESTPF.
           OPEN INPUT CARGOPF.
           OPEN I-O ALWGTBALS.

       2000-PROCESS-SCREEN.
           WRITE DISPLAY-RECORD FORMAT 'WGTCTL'.
           READ ALWGTBALS RECORD FORMAT 'WGTCTL'.
           
           IF IN03 = B"0" AND SCR-ACTION = '1'
               PERFORM 3000-CALCULATE-LOADSHEET
           END-IF.

       3000-CALCULATE-LOADSHEET.
           MOVE SPACES TO SCR-ERR-MSG.
           PERFORM 4000-FETCH-DATA.
           
           IF IS-VALID
               PERFORM 5000-LOAD-AIRCRAFT-ARMS
               PERFORM 6000-CALCULATE-ZFW
               PERFORM 7000-CALCULATE-TOW
               PERFORM 8000-CALCULATE-LAW
               PERFORM 8500-VALIDATE-ENVELOPE
           END-IF.

       4000-FETCH-DATA.
           MOVE 'Y' TO WS-VALID-FLAG.
           
           MOVE SCR-TAIL-NUM TO DB-TAIL-NUM.
           READ AIRCRAFTPF
               INVALID KEY
                   MOVE 'N' TO WS-VALID-FLAG
                   MOVE 'Aircraft Tail Number not found.' TO SCR-ERR-MSG
                   GO TO 4000-EXIT
           END-READ.

           MOVE SCR-FLT-ID TO DB-FLT-ID.
           READ MANIFESTPF
               INVALID KEY
                   MOVE 'N' TO WS-VALID-FLAG
                   MOVE 'Flight Manifest missing or locked.' TO SCR-ERR-MSG
                   GO TO 4000-EXIT
           END-READ.

           MOVE SCR-FLT-ID TO DB-CARGO-FLT-ID.
           READ CARGOPF
               INVALID KEY
                   MOVE 'N' TO WS-VALID-FLAG
                   MOVE 'Cargo Data missing for Flight.' TO SCR-ERR-MSG
           END-READ.
       4000-EXIT. EXIT.

       5000-LOAD-AIRCRAFT-ARMS.
      * Explicit Aircraft Definition Matrix - Highly sensitive data
           EVALUATE DB-AIRCRAFT-TYPE
               WHEN 'B77W' *> Boeing 777-300ER
                   MOVE 12.50 TO WS-ZONE-A-ARM
                   MOVE 18.20 TO WS-ZONE-B-ARM
                   MOVE 24.80 TO WS-ZONE-C-ARM
                   MOVE 32.10 TO WS-ZONE-D-ARM
                   MOVE 40.50 TO WS-ZONE-E-ARM
                   MOVE 15.00 TO WS-FWD-HOLD-ARM
                   MOVE 35.00 TO WS-AFT-HOLD-ARM
                   MOVE 42.00 TO WS-BULK-HOLD-ARM
                   MOVE 26.50 TO WS-WING-TANK-ARM
                   MOVE 25.00 TO WS-CTR-TANK-ARM
               WHEN 'A359' *> Airbus A350-900
                   MOVE 10.10 TO WS-ZONE-A-ARM
                   MOVE 16.50 TO WS-ZONE-B-ARM
                   MOVE 22.00 TO WS-ZONE-C-ARM
                   MOVE 29.50 TO WS-ZONE-D-ARM
                   MOVE 00.00 TO WS-ZONE-E-ARM *> A359 has 4 main zones
                   MOVE 13.50 TO WS-FWD-HOLD-ARM
                   MOVE 32.00 TO WS-AFT-HOLD-ARM
                   MOVE 38.50 TO WS-BULK-HOLD-ARM
                   MOVE 24.20 TO WS-WING-TANK-ARM
                   MOVE 22.80 TO WS-CTR-TANK-ARM
               WHEN OTHER
                   MOVE 'N' TO WS-VALID-FLAG
                   MOVE 'Unsupported Aircraft Type for Loadsheet.' 
                     TO SCR-ERR-MSG
           END-EVALUATE.

       6000-CALCULATE-ZFW.
      * Step 1: Base Aircraft limits
           MOVE DB-DOW-KG TO WS-ZFW-KG.
           MOVE DB-DOW-INDEX TO WS-ZFW-MOMENT.

      * Step 2: Passenger Weights & Moments
           COMPUTE WS-TEMP-MOMENT = 
               (DB-PAX-ZONE-A-QTY * WS-ADULT-PAX-KG) * WS-ZONE-A-ARM.
           ADD (DB-PAX-ZONE-A-QTY * WS-ADULT-PAX-KG) TO WS-ZFW-KG.
           ADD WS-TEMP-MOMENT TO WS-ZFW-MOMENT.

           COMPUTE WS-TEMP-MOMENT = 
               (DB-PAX-ZONE-B-QTY * WS-ADULT-PAX-KG) * WS-ZONE-B-ARM.
           ADD (DB-PAX-ZONE-B-QTY * WS-ADULT-PAX-KG) TO WS-ZFW-KG.
           ADD WS-TEMP-MOMENT TO WS-ZFW-MOMENT.

           COMPUTE WS-TEMP-MOMENT = 
               (DB-PAX-ZONE-C-QTY * WS-ADULT-PAX-KG) * WS-ZONE-C-ARM.
           ADD (DB-PAX-ZONE-C-QTY * WS-ADULT-PAX-KG) TO WS-ZFW-KG.
           ADD WS-TEMP-MOMENT TO WS-ZFW-MOMENT.

           COMPUTE WS-TEMP-MOMENT = 
               (DB-PAX-ZONE-D-QTY * WS-ADULT-PAX-KG) * WS-ZONE-D-ARM.
           ADD (DB-PAX-ZONE-D-QTY * WS-ADULT-PAX-KG) TO WS-ZFW-KG.
           ADD WS-TEMP-MOMENT TO WS-ZFW-MOMENT.

           COMPUTE WS-TEMP-MOMENT = 
               (DB-PAX-ZONE-E-QTY * WS-ADULT-PAX-KG) * WS-ZONE-E-ARM.
           ADD (DB-PAX-ZONE-E-QTY * WS-ADULT-PAX-KG) TO WS-ZFW-KG.
           ADD WS-TEMP-MOMENT TO WS-ZFW-MOMENT.

      * Step 3: Cargo Holds
           COMPUTE WS-TEMP-MOMENT = DB-FWD-HOLD-KG * WS-FWD-HOLD-ARM.
           ADD DB-FWD-HOLD-KG TO WS-ZFW-KG.
           ADD WS-TEMP-MOMENT TO WS-ZFW-MOMENT.

           COMPUTE WS-TEMP-MOMENT = DB-AFT-HOLD-KG * WS-AFT-HOLD-ARM.
           ADD DB-AFT-HOLD-KG TO WS-ZFW-KG.
           ADD WS-TEMP-MOMENT TO WS-ZFW-MOMENT.

           COMPUTE WS-TEMP-MOMENT = DB-BULK-HOLD-KG * WS-BULK-HOLD-ARM.
           ADD DB-BULK-HOLD-KG TO WS-ZFW-KG.
           ADD WS-TEMP-MOMENT TO WS-ZFW-MOMENT.

      * Step 4: Calculate ZFW CG and %MAC
           DIVIDE WS-ZFW-MOMENT BY WS-ZFW-KG GIVING WS-ZFW-CG.
           PERFORM 8800-CALCULATE-MAC USING WS-ZFW-CG WS-ZFW-MAC.
           MOVE WS-ZFW-MAC TO SCR-MAC-ZFW-DSP.

       7000-CALCULATE-TOW.
      * TOW = ZFW + Takeoff Fuel (Trip + Reserve + Contingency)
      * Note: Taxi fuel is burned before takeoff, so it is omitted from TOW
           COMPUTE WS-TOW-KG = WS-ZFW-KG + DB-TRIP-FUEL-KG + DB-RSV-FUEL-KG.
           
      * Fuel moments (Simulating center tank vs wing tank distribution)
      * If Total fuel > 40,000kg, spillover goes to Center Tank
           IF (DB-TRIP-FUEL-KG + DB-RSV-FUEL-KG) > 40000.00
               COMPUTE WS-TEMP-MOMENT = 
                 (40000.00 * WS-WING-TANK-ARM) +
                 (((DB-TRIP-FUEL-KG + DB-RSV-FUEL-KG) - 40000.00) 
                 * WS-CTR-TANK-ARM)
           ELSE
               COMPUTE WS-TEMP-MOMENT = 
                 (DB-TRIP-FUEL-KG + DB-RSV-FUEL-KG) * WS-WING-TANK-ARM
           END-IF.

           COMPUTE WS-TOW-MOMENT = WS-ZFW-MOMENT + WS-TEMP-MOMENT.
           DIVIDE WS-TOW-MOMENT BY WS-TOW-KG GIVING WS-TOW-CG.
           PERFORM 8800-CALCULATE-MAC USING WS-TOW-CG WS-TOW-MAC.
           MOVE WS-TOW-MAC TO SCR-MAC-TOW-DSP.

       8000-CALCULATE-LAW.
      * LAW = TOW - Trip Fuel Burn
           COMPUTE WS-LAW-KG = WS-TOW-KG - DB-TRIP-FUEL-KG.
           
      * Deduct trip fuel moment (assuming burned from Center first, then wings)
      * Simplified for this simulation.
           COMPUTE WS-TEMP-MOMENT = DB-TRIP-FUEL-KG * WS-CTR-TANK-ARM.
           COMPUTE WS-LAW-MOMENT = WS-TOW-MOMENT - WS-TEMP-MOMENT.
           
           DIVIDE LAW-MOMENT BY WS-LAW-KG GIVING WS-LAW-CG.
           PERFORM 8800-CALCULATE-MAC USING WS-LAW-CG WS-LAW-MAC.
           MOVE WS-LAW-MAC TO SCR-MAC-LAW-DSP.

       8500-VALIDATE-ENVELOPE.
      * Structural Weight Limits Check
           MOVE 'Y' TO WS-TRIM-FLAG.
           MOVE 'TRIM NORMAL' TO SCR-TRIM-STATUS.

           IF WS-ZFW-KG > DB-MZFW-LIMIT
               MOVE 'FATAL: Max Zero Fuel Weight Exceeded.' TO SCR-ERR-MSG
               MOVE 'OUT OF TRIM' TO SCR-TRIM-STATUS
           END-IF.
           IF WS-TOW-KG > DB-MTOW-LIMIT
               MOVE 'FATAL: Max Takeoff Weight Exceeded.' TO SCR-ERR-MSG
               MOVE 'OUT OF TRIM' TO SCR-TRIM-STATUS
           END-IF.
           IF WS-LAW-KG > DB-MLAW-LIMIT
               MOVE 'FATAL: Max Landing Weight Exceeded.' TO SCR-ERR-MSG
               MOVE 'OUT OF TRIM' TO SCR-TRIM-STATUS
           END-IF.

      * Aerodynamic Envelope Check (%MAC must be between 14% and 32% for stability)
           IF WS-TOW-MAC < 14.00
               MOVE 'TRIM EXCEPTION: Nose Heavy. Shift cargo AFT.' 
                 TO SCR-ERR-MSG
               MOVE 'OUT OF TRIM' TO SCR-TRIM-STATUS
           ELSE IF WS-TOW-MAC > 32.00
               MOVE 'TRIM EXCEPTION: Tail Heavy. Shift cargo FWD.' 
                 TO SCR-ERR-MSG
               MOVE 'OUT OF TRIM' TO SCR-TRIM-STATUS
           END-IF.

       8800-CALCULATE-MAC.
      * Aerodynamic formula: %MAC = ((CG - LEMAC) / MAC Length) * 100
      * This translates the physical balance point into a percentage of the wing's chord.
           PROCEDURE USING P-CG P-MAC.
           01 P-CG  PIC 9(3)V99.
           01 P-MAC PIC S9(3)V99.
           
           COMPUTE P-MAC = 
               ((P-CG - DB-MAC-LEMAC) / DB-MAC-LENGTH) * 100.

       9000-TERMINATE.
           CLOSE AIRCRAFTPF.
           CLOSE MANIFESTPF.
           CLOSE CARGOPF.
           CLOSE ALWGTBALS.
