*****************************************************************
      * PROGRAM-ID:  ALRPTGEN                                         *
      * AUTHOR:      IBM i COBOL Reporting Specialist                 *
      * DESCRIPTION: Daily Flight Manifest & Load Factor Generator.   *
      * FEATURES:    - Multi-file relational reading (FLT/RES/BAG).   *
      * - Explicit Line/Page Overflow management.        *
      * - Fixed-Point (COMP-3) Load Factor math.         *
      * - Hierarchical Subtotaling and Grand Totals.     *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALRPTGEN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      * Flight Master Physical File
           SELECT FLT-FILE ASSIGN TO DATABASE-FLIGHTPF
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS FLT-ID
               FILE STATUS IS WS-FLT-STAT.

      * Reservation Physical File (Indexed by Flight ID + Ticket)
           SELECT RES-FILE ASSIGN TO DATABASE-RESERVPF
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS RES-FLT-TICKET
               FILE STATUS IS WS-RES-STAT.

      * Baggage Physical File (Indexed by Ticket)
           SELECT BAG-FILE ASSIGN TO DATABASE-BAGGAGEPF
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BAG-TICKET-NO
               FILE STATUS IS WS-BAG-STAT.

      * Output Printer Stream
           SELECT RPT-FILE ASSIGN TO PRINTER-QPRINT
               ORGANIZATION IS SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  FLT-FILE.
       01  FLT-RECORD.
           05  FLT-ID                   PIC X(10).
           05  FLT-ROUTE                PIC X(10).
           05  FLT-AIRCRAFT-TYPE        PIC X(05).
           05  FLT-CAPACITY             PIC 9(03).
           05  FLT-BASE-FUEL-LBS        PIC 9(06).

       FD  RES-FILE.
       01  RES-RECORD.
           05  RES-FLT-TICKET.
               10  RES-FLT-ID           PIC X(10).
               10  RES-TICKET-NO        PIC X(15).
           05  RES-PASSENGER-NAME       PIC X(30).
           05  RES-CLASS                PIC X(01).
           05  RES-STATUS               PIC X(10).

       FD  BAG-FILE.
       01  BAG-RECORD.
           05  BAG-TICKET-NO            PIC X(15).
           05  BAG-COUNT                PIC 9(02).
           05  BAG-TOTAL-WEIGHT         PIC 9(04).

       FD  RPT-FILE.
       01  PRINT-LINE                   PIC X(132).

       WORKING-STORAGE SECTION.
      * --- FILE STATUS & CONTROL FLAGS ---
       01  WS-FLAGS.
           05  WS-FLT-STAT              PIC X(02) VALUE '00'.
           05  WS-RES-STAT              PIC X(02) VALUE '00'.
           05  WS-BAG-STAT              PIC X(02) VALUE '00'.
           05  WS-EOF-FLT               PIC X(01) VALUE 'N'.
           05  WS-EOF-RES               PIC X(01) VALUE 'N'.
           05  WS-SYSTEM-DATE           PIC X(08).

      * --- PAGE AND LINE COUNTERS ---
       01  WS-PAGINATION.
           05  WS-PAGE-NO               PIC 9(04) COMP VALUE 0.
           05  WS-LINE-COUNT            PIC 9(02) COMP VALUE 99.
           05  WS-MAX-LINES             PIC 9(02) COMP VALUE 55.

      * --- MATH & ACCUMULATORS (FIXED-POINT COMP-3) ---
       01  WS-FLIGHT-TOTALS.
           05  WS-FLT-PASS-CNT          PIC 9(04)     COMP-3 VALUE 0.
           05  WS-FLT-BAG-CNT           PIC 9(04)     COMP-3 VALUE 0.
           05  WS-FLT-BAG-WGT           PIC 9(07)     COMP-3 VALUE 0.
           05  WS-FLT-LOAD-FACT         PIC 9(03)V9999 COMP-3 VALUE 0.
           05  WS-FLT-EST-FUEL          PIC 9(07)     COMP-3 VALUE 0.

       01  WS-GRAND-TOTALS.
           05  WS-GRND-FLT-CNT          PIC 9(04)     COMP-3 VALUE 0.
           05  WS-GRND-PASS-CNT         PIC 9(06)     COMP-3 VALUE 0.
           05  WS-GRND-BAG-WGT          PIC 9(09)     COMP-3 VALUE 0.
           05  WS-GRND-FUEL             PIC 9(09)     COMP-3 VALUE 0.

      * --- REPORT LAYOUT DEFINITIONS ---
       01  RPT-HEADER-1.
           05  FILLER                   PIC X(06) VALUE 'DATE: '.
           05  HDR-DATE                 PIC X(10).
           05  FILLER                   PIC X(40) VALUE SPACES.
           05  FILLER                   PIC X(30) 
               VALUE 'GLOBAL AIRLINES SYSTEM MANIFEST'.
           05  FILLER                   PIC X(34) VALUE SPACES.
           05  FILLER                   PIC X(06) VALUE 'PAGE: '.
           05  HDR-PAGE                 PIC ZZZ9.

       01  RPT-HEADER-2.
           05  FILLER                   PIC X(132) VALUE ALL '='.

       01  RPT-FLIGHT-HEADER.
           05  FILLER                   PIC X(10) VALUE ' FLIGHT:  '.
           05  PRT-FLT-ID               PIC X(10).
           05  FILLER                   PIC X(10) VALUE '  ROUTE:  '.
           05  PRT-FLT-ROUTE            PIC X(10).
           05  FILLER                   PIC X(10) VALUE '  AIRCRAFT: '.
           05  PRT-AIRCRAFT             PIC X(05).
           05  FILLER                   PIC X(12) VALUE '  CAPACITY: '.
           05  PRT-CAPACITY             PIC ZZ9.
           05  FILLER                   PIC X(62) VALUE SPACES.

       01  RPT-COL-HEADERS.
           05  FILLER                   PIC X(02) VALUE SPACES.
           05  FILLER                   PIC X(17) VALUE 'TICKET NUMBER'.
           05  FILLER                   PIC X(32) VALUE 'PASSENGER NAME'.
           05  FILLER                   PIC X(08) VALUE 'CLASS'.
           05  FILLER                   PIC X(12) VALUE 'STATUS'.
           05  FILLER                   PIC X(10) VALUE 'BAG COUNT'.
           05  FILLER                   PIC X(15) VALUE 'BAG WEIGHT(LBS)'.
           05  FILLER                   PIC X(36) VALUE SPACES.

       01  RPT-DETAIL-LINE.
           05  FILLER                   PIC X(02) VALUE SPACES.
           05  PRT-TICKET               PIC X(15).
           05  FILLER                   PIC X(02) VALUE SPACES.
           05  PRT-NAME                 PIC X(30).
           05  FILLER                   PIC X(04) VALUE SPACES.
           05  PRT-CLASS                PIC X(01).
           05  FILLER                   PIC X(05) VALUE SPACES.
           05  PRT-STATUS               PIC X(10).
           05  FILLER                   PIC X(05) VALUE SPACES.
           05  PRT-BAG-CNT              PIC Z9.
           05  FILLER                   PIC X(08) VALUE SPACES.
           05  PRT-BAG-WGT              PIC ZZZ9.
           05  FILLER                   PIC X(44) VALUE SPACES.

       01  RPT-SUBTOTAL-LINE.
           05  FILLER                   PIC X(132) VALUE ALL '-'.

       01  RPT-FLT-TOTALS.
           05  FILLER                   PIC X(10) VALUE ' TOTALS - '.
           05  FILLER                   PIC X(12) VALUE 'PASSENGERS: '.
           05  PRT-TOT-PASS             PIC Z,ZZ9.
           05  FILLER                   PIC X(15) VALUE '   LOAD FACTOR: '.
           05  PRT-LOAD-FACT            PIC ZZ9.99.
           05  FILLER                   PIC X(02) VALUE '% '.
           05  FILLER                   PIC X(14) VALUE '  BAG WEIGHT: '.
           05  PRT-TOT-BAG-WGT          PIC ZZ,ZZ9.
           05  FILLER                   PIC X(05) VALUE ' LBS '.
           05  FILLER                   PIC X(11) VALUE ' EST FUEL: '.
           05  PRT-EST-FUEL             PIC ZZZ,ZZ9.
           05  FILLER                   PIC X(34) VALUE SPACES.

       01  RPT-GRAND-TOTALS.
           05  FILLER                   PIC X(132) VALUE ALL '*'.
       01  RPT-GRAND-DATA.
           05  FILLER                   PIC X(20) VALUE ' GRAND TOTALS >>    '.
           05  FILLER                   PIC X(10) VALUE 'FLIGHTS:  '.
           05  PRT-GRND-FLTS            PIC Z,ZZ9.
           05  FILLER                   PIC X(10) VALUE '    PAX:  '.
           05  PRT-GRND-PAX             PIC ZZZ,ZZ9.
           05  FILLER                   PIC X(15) VALUE '    BAGGAGE:   '.
           05  PRT-GRND-BAGS            PIC ZZ,ZZZ,ZZ9.
           05  FILLER                   PIC X(15) VALUE '    FUEL BURN: '.
           05  PRT-GRND-FUEL            PIC ZZZ,ZZZ,ZZ9.
           05  FILLER                   PIC X(19) VALUE SPACES.

       PROCEDURE DIVISION.
       0000-MAIN.
           PERFORM 1000-INITIALIZATION.
           
           READ FLT-FILE
               AT END MOVE 'Y' TO WS-EOF-FLT
           END-READ.

           PERFORM 2000-PROCESS-FLIGHTS 
               UNTIL WS-EOF-FLT = 'Y'.

           PERFORM 8000-PRINT-GRAND-TOTALS.
           PERFORM 9900-SHUTDOWN.

       1000-INITIALIZATION.
           OPEN INPUT FLT-FILE
           OPEN INPUT RES-FILE
           OPEN INPUT BAG-FILE
           OPEN OUTPUT RPT-FILE.

           ACCEPT WS-SYSTEM-DATE FROM DATE YYYYMMDD.
           STRING WS-SYSTEM-DATE(1:4) '/'
                  WS-SYSTEM-DATE(5:2) '/'
                  WS-SYSTEM-DATE(7:2) DELIMITED BY SIZE 
                  INTO HDR-DATE.

       2000-PROCESS-FLIGHTS.
           PERFORM 2100-RESET-FLIGHT-TOTALS.
           PERFORM 2200-PRINT-FLIGHT-HEADER.

           MOVE 'N' TO WS-EOF-RES.
           MOVE FLT-ID TO RES-FLT-ID.
           MOVE SPACES TO RES-TICKET-NO.
           
           START RES-FILE KEY IS GREATER THAN OR EQUAL TO RES-FLT-TICKET
               INVALID KEY MOVE 'Y' TO WS-EOF-RES
           END-START.

           IF WS-EOF-RES = 'N'
               READ RES-FILE NEXT
                   AT END MOVE 'Y' TO WS-EOF-RES
               END-READ
           END-IF.

           PERFORM 3000-PROCESS-RESERVATIONS 
               UNTIL WS-EOF-RES = 'Y' OR RES-FLT-ID NOT = FLT-ID.

           PERFORM 5000-CALCULATE-LOAD-FACTOR.
           PERFORM 7000-PRINT-SUBTOTALS.
           
           ADD 1 TO WS-GRND-FLT-CNT.

           READ FLT-FILE
               AT END MOVE 'Y' TO WS-EOF-FLT
           END-READ.

       2100-RESET-FLIGHT-TOTALS.
           MOVE 0 TO WS-FLT-PASS-CNT.
           MOVE 0 TO WS-FLT-BAG-CNT.
           MOVE 0 TO WS-FLT-BAG-WGT.
           MOVE 0 TO WS-FLT-LOAD-FACT.
           MOVE 0 TO WS-FLT-EST-FUEL.

       2200-PRINT-FLIGHT-HEADER.
      * Check page break threshold before starting a new flight block
           IF WS-LINE-COUNT > (WS-MAX-LINES - 6)
               PERFORM 9000-PAGE-HEADER
           ELSE
               WRITE PRINT-LINE FROM SPACES
               ADD 1 TO WS-LINE-COUNT
           END-IF.

           MOVE FLT-ID TO PRT-FLT-ID.
           MOVE FLT-ROUTE TO PRT-FLT-ROUTE.
           MOVE FLT-AIRCRAFT-TYPE TO PRT-AIRCRAFT.
           MOVE FLT-CAPACITY TO PRT-CAPACITY.

           WRITE PRINT-LINE FROM RPT-FLIGHT-HEADER.
           WRITE PRINT-LINE FROM RPT-COL-HEADERS.
           WRITE PRINT-LINE FROM RPT-SUBTOTAL-LINE.
           ADD 3 TO WS-LINE-COUNT.

       3000-PROCESS-RESERVATIONS.
           IF RES-STATUS = 'BOARDED' OR RES-STATUS = 'CONFIRMED'
               ADD 1 TO WS-FLT-PASS-CNT
               ADD 1 TO WS-GRND-PASS-CNT
               PERFORM 4000-PROCESS-BAGGAGE
               PERFORM 6000-PRINT-DETAIL
           END-IF.

           READ RES-FILE NEXT
               AT END MOVE 'Y' TO WS-EOF-RES
           END-READ.

       4000-PROCESS-BAGGAGE.
           MOVE RES-TICKET-NO TO BAG-TICKET-NO.
           READ BAG-FILE
               INVALID KEY
                   MOVE 0 TO BAG-COUNT
                   MOVE 0 TO BAG-TOTAL-WEIGHT
           END-READ.

           ADD BAG-COUNT TO WS-FLT-BAG-CNT.
           ADD BAG-TOTAL-WEIGHT TO WS-FLT-BAG-WGT.
           ADD BAG-TOTAL-WEIGHT TO WS-GRND-BAG-WGT.

       5000-CALCULATE-LOAD-FACTOR.
      * Fixed-Point Math Block to avoid rounding errors
      * Formula: (Passengers / Capacity) * 100
           IF FLT-CAPACITY > 0
               COMPUTE WS-FLT-LOAD-FACT = 
                   (WS-FLT-PASS-CNT / FLT-CAPACITY) * 100
           ELSE
               MOVE 0 TO WS-FLT-LOAD-FACT
           END-IF.

      * Fuel Estimation: Base Burn + (0.5 lbs extra per lb of baggage)
           COMPUTE WS-FLT-EST-FUEL = 
               FLT-BASE-FUEL-LBS + (WS-FLT-BAG-WGT * 0.5).
           
           ADD WS-FLT-EST-FUEL TO WS-GRND-FUEL.

       6000-PRINT-DETAIL.
           IF WS-LINE-COUNT > WS-MAX-LINES
               PERFORM 9000-PAGE-HEADER
               WRITE PRINT-LINE FROM RPT-COL-HEADERS
               WRITE PRINT-LINE FROM RPT-SUBTOTAL-LINE
               ADD 2 TO WS-LINE-COUNT
           END-IF.

           MOVE RES-TICKET-NO TO PRT-TICKET.
           MOVE RES-PASSENGER-NAME TO PRT-NAME.
           MOVE RES-CLASS TO PRT-CLASS.
           MOVE RES-STATUS TO PRT-STATUS.
           MOVE BAG-COUNT TO PRT-BAG-CNT.
           MOVE BAG-TOTAL-WEIGHT TO PRT-BAG-WGT.

           WRITE PRINT-LINE FROM RPT-DETAIL-LINE.
           ADD 1 TO WS-LINE-COUNT.

       7000-PRINT-SUBTOTALS.
           MOVE WS-FLT-PASS-CNT TO PRT-TOT-PASS.
           MOVE WS-FLT-LOAD-FACT TO PRT-LOAD-FACT.
           MOVE WS-FLT-BAG-WGT TO PRT-TOT-BAG-WGT.
           MOVE WS-FLT-EST-FUEL TO PRT-EST-FUEL.

           WRITE PRINT-LINE FROM RPT-SUBTOTAL-LINE.
           WRITE PRINT-LINE FROM RPT-FLT-TOTALS.
           WRITE PRINT-LINE FROM RPT-SUBTOTAL-LINE.
           ADD 3 TO WS-LINE-COUNT.

       8000-PRINT-GRAND-TOTALS.
           IF WS-LINE-COUNT > (WS-MAX-LINES - 4)
               PERFORM 9000-PAGE-HEADER
           END-IF.

           MOVE WS-GRND-FLT-CNT TO PRT-GRND-FLTS.
           MOVE WS-GRND-PASS-CNT TO PRT-GRND-PAX.
           MOVE WS-GRND-BAG-WGT TO PRT-GRND-BAGS.
           MOVE WS-GRND-FUEL TO PRT-GRND-FUEL.

           WRITE PRINT-LINE FROM SPACES.
           WRITE PRINT-LINE FROM RPT-GRAND-TOTALS.
           WRITE PRINT-LINE FROM RPT-GRAND-DATA.
           WRITE PRINT-LINE FROM RPT-GRAND-TOTALS.

       9000-PAGE-HEADER.
           ADD 1 TO WS-PAGE-NO.
           MOVE WS-PAGE-NO TO HDR-PAGE.
           
      * EJECT page mechanism for QPRINT (Writing Top of Form character)
           IF WS-PAGE-NO > 1
               WRITE PRINT-LINE FROM SPACES AFTER ADVANCING PAGE
           END-IF.

           WRITE PRINT-LINE FROM RPT-HEADER-1.
           WRITE PRINT-LINE FROM RPT-HEADER-2.
           WRITE PRINT-LINE FROM SPACES.
           MOVE 3 TO WS-LINE-COUNT.

       9900-SHUTDOWN.
           CLOSE FLT-FILE
                 RES-FILE
                 BAG-FILE
                 RPT-FILE.
           GOBACK.
