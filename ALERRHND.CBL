*****************************************************************
      * PROGRAM-ID:  ALERRHND                                         *
      * AUTHOR:      Enterprise IBM i COBOL Engineer                  *
      * DESCRIPTION: Centralized Error Handler & Rollback Coordinator *
      * FEATURES:    - 300+ Code Message Mapping Table                *
      * - Sys-Op Message Queue (QSYSOPR) Integration                  *
      * - Transaction Boundary Management (Rollback/Commit)           *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALERRHND.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * --- SYSTEM MESSAGE QUEUE API VARIABLES ---
       01  WS-MSG-API-VARS.
           05  WS-MSG-ID                PIC X(07) VALUE 'CPF9898'.
           05  WS-MSG-FILE              PIC X(20) VALUE 'QCPFMSG   *LIBL     '.
           05  WS-MSG-DATA              PIC X(128).
           05  WS-MSG-LEN               PIC S9(09) BINARY.
           05  WS-MSG-TYPE              PIC X(10) VALUE '*ESCAPE   '.
           05  WS-STACK-ENTRY           PIC X(10) VALUE '* '.
           05  WS-STACK-COUNT           PIC S9(09) BINARY VALUE 1.
           05  WS-MSG-KEY               PIC X(04).
           05  WS-API-ERR-CODE.
               10  WS-ERR-PROV          PIC S9(09) BINARY VALUE 16.
               10  WS-ERR-AVAIL         PIC S9(09) BINARY VALUE 0.
               10  WS-ERR-ID            PIC X(07).
               10  WS-ERR-RSV           PIC X(01).

      * --- ERROR CODE MAPPING TABLE (Fragment of 300+ codes) ---
      * Codes structured as: 000-099 (File I/O), 100-199 (Validation), 
      * 200-299 (System/Resource)
       01  WS-ERROR-TABLE.
           05  WS-ERR-REC OCCURS 300 TIMES INDEXED BY ERR-IDX.
               10  WS-CODE              PIC 9(03).
               10  WS-SEVERITY          PIC X(01).
               10  WS-TEXT              PIC X(60).

      * --- TEMPORARY VARIABLES ---
       01  WS-WORK.
           05  WS-IDX                   PIC 9(04) BINARY.
           05  WS-FOUND                 PIC X(01).

       LINKAGE SECTION.
       01  LS-ERR-INPUT.
           05  LS-ERR-CODE              PIC 9(03).
           05  LS-PROGRAM-ID            PIC X(10).
           05  LS-TRANS-ID              PIC X(15).
       01  LS-ERR-ACTION.
           05  LS-ROLLBACK-REQ          PIC X(01).
               88 DO-ROLLBACK           VALUE 'Y'.
           05  LS-RETURN-MSG            PIC X(60).

      * ===============================================================
       PROCEDURE DIVISION USING LS-ERR-INPUT LS-ERR-ACTION.
      * ===============================================================
       0000-MAIN.
           PERFORM 1000-INITIALIZE-TABLE.
           PERFORM 2000-FIND-ERROR-CODE.
           
           IF WS-FOUND = 'Y'
               PERFORM 3000-LOG-TO-QSYSOPR
           ELSE
               MOVE 'UNKNOWN SYSTEM ERROR ENCOUNTERED' TO LS-RETURN-MSG
           END-IF.

           PERFORM 4000-MANAGE-TRANSACTION-BOUNDARY.
           
           GOBACK.

      * ===============================================================
       1000-INITIALIZE-TABLE.
      * In production, this would be a loaded flat file or Data Area
      * ===============================================================
           MOVE 001 TO WS-CODE(1). MOVE 'E' TO WS-SEVERITY(1). 
           MOVE 'DB2 RECORD LOCK CONFLICT' TO WS-TEXT(1).
           MOVE 101 TO WS-CODE(101). MOVE 'W' TO WS-SEVERITY(101).
           MOVE 'INVALID PASSENGER DATA FORMAT' TO WS-TEXT(101).
           MOVE 201 TO WS-CODE(201). MOVE 'C' TO WS-SEVERITY(201).
           MOVE 'CRITICAL SYSTEM STORAGE OVERFLOW' TO WS-TEXT(201).
      * ... (Rows 2-300 would be populated here)

      * ===============================================================
       2000-FIND-ERROR-CODE.
      * ===============================================================
           MOVE 'N' TO WS-FOUND.
           SEARCH WS-ERR-REC
               WHEN WS-CODE(ERR-IDX) = LS-ERR-CODE
                   MOVE 'Y' TO WS-FOUND
                   MOVE WS-TEXT(ERR-IDX) TO LS-RETURN-MSG
                   
      * Determine if this error class mandates a rollback
                   IF WS-SEVERITY(ERR-IDX) = 'C' OR 'E'
                       MOVE 'Y' TO LS-ROLLBACK-REQ
                   ELSE
                       MOVE 'N' TO LS-ROLLBACK-REQ
                   END-IF
           END-SEARCH.

      * ===============================================================
       3000-LOG-TO-QSYSOPR.
      * Send alert to the System Operator Message Queue (QSYSOPR)
      * ===============================================================
           STRING 'ALARM: ' LS-PROGRAM-ID ' / ' LS-ERR-CODE ' - ' 
                  LS-RETURN-MSG DELIMITED BY SIZE INTO WS-MSG-DATA.
           MOVE 128 TO WS-MSG-LEN.

           CALL 'QMHSNDPM' USING WS-MSG-ID, WS-MSG-FILE, WS-MSG-DATA,
                                 WS-MSG-LEN, WS-MSG-TYPE, WS-STACK-ENTRY,
                                 WS-STACK-COUNT, WS-MSG-KEY, WS-API-ERR-CODE.

      * ===============================================================
       4000-MANAGE-TRANSACTION-BOUNDARY.
      * Determine rollback scope
      * ===============================================================
           IF DO-ROLLBACK
      * The actual rollback is performed by the calling program 
      * based on this flag, or we signal the commitment control here
               CONTINUE
           ELSE
               COMMIT
           END-IF.
