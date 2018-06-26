       IDENTIFICATION DIVISION.

       PROGRAM-ID.    CNVTDA.

      *TITLE.         SAMEER FILE

      *AUTHOR.        CSC CONTINUUM, INC.

      *SECURITY.      THIS PROGRAM IS THE PROPERTY OF COMPUTER SCIENCES
      *               CORP.  IT MAY NOT BE COPIED IN WHOLE OR IN PART
      *               WITHOUT THE EXPRESS WRITTEN CONSENT OF COMPUTER
      *               SCIENCES CORP.
      *
      *****************************************************************
      *  ENH #  NAME    YEAR  USER DESCRIPTION                        *
      * ------- -----   ---- ---------------------------------------- *
MET001* MET001  SCOTHRO 2017 REPLACED ORIGINAL CNVTDA PROGRAM TO READ *
MET001*                      IN THE GROUP BILL CONVERSION FILE INSTEAD*
MET001*                      OF THE PERSON CONVERSION FILE.           *
MET002* MET002 BSALLADE 2017 RIGHT JUSTIFIED DATA WHEN MOVING GROUP   *
MET002*                      NUMBER FROM 10 BYTE TO 15 BYTE FIELDS    *   
      *****************************************************************
      *
      *    MODULE NARRATIVE.
      *        POPULATE DA TRX FIELDS
      *
      *    INPUTS.
      *        POLICY CFF

      *    CALLING MODULE.

      *    CALLED MODULES.
      *
      *    PROCESSING.

      *    OUTPUTS - DA-TRX-REC

       DATE-WRITTEN.  MAR 17, 2017.
       DATE-COMPILED.

       ENVIRONMENT DIVISION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       01  FILLER                           PIC X(45) VALUE
           '**** CNVTDA  WORKING STORAGE STARTS HERE ****'.

       01  TRX-WORK-AREA.              COPY CIUAX001 SUPPRESS.
       01  DA-TRX-REC REDEFINES        TRX-WORK-AREA.
                                       COPY CIUAXDA1 SUPPRESS.
       01  RDW-WORK-AREA.              COPY CIUAXRDF SUPPRESS.

       01  CONSTANTS.
         05  NINES-DATE                PIC 9(8)
                                       VALUE 99999999.
       01  VCSABEND-CODES   COMP.
           05  DATABASE-READ-ERROR          PIC S9(7) VALUE +11.

       01  VCSABEND-ERRORS.
           05  READ-ERROR            PIC S9(8) COMP VALUE +0361.
           05  WRITE-ERROR           PIC S9(8) COMP VALUE +0362.
           05  OPEN-ERROR            PIC S9(8) COMP VALUE +0363.
           05  CLOSE-ERROR           PIC S9(8) COMP VALUE +0364.
           05  POSITION-ERROR        PIC S9(8) COMP VALUE +0366.
       01  VCSTRX3-COMM-AREA.          COPY CVCSTRX3 SUPPRESS.
       01  QSAM-COMM-AREA.             COPY CIUAXQS1 SUPPRESS.
       01  QSAM-FIELDS.
           05  QSAM-FILENAME                 PIC X(8)  VALUE 'DCOLL'.
           05  QSAM-RECORD.
               10  RDW-LENGTH                PIC S9(4) COMP.
               10  FILLER                    PIC X(02).
               10  DCOLL-TABLE.
                   15 DCOLL-KEY              PIC X(49).
       01  SUB                              PIC S9(4) COMP.
      *01  SUB-POL                          PIC 999.
 
       01  FILLER                           PIC X(43) VALUE
           '**** CNVTDA  WORKING STORAGE ENDS HERE ****'.

       LINKAGE SECTION.
MET00?* ------- GROUP BILLING COPYBOOK
MET00? COPY DTLGRPBL.
MET00? 01  PASS-AREA.                  COPY CUUSXPSS SUPPRESS.

MET00? PROCEDURE DIVISION USING GROUP-BILLING-REC.
MET00?*                         PASS-AREA.

       000000-MAINLINE.
      
           MOVE SPACES TO DA-TRX-REC
           INITIALIZE DA-TRX-REC
           PERFORM 100000-FORMAT-HEADER   THRU 100000-EXIT
           PERFORM 200000-FORMAT-TRX-INFO THRU 200000-EXIT.

       000000-EXIT.
           GOBACK.

       100000-FORMAT-HEADER.
MML517*    MOVE ZEROES
MML517*      TO SUB.
      *         SUB-POL.
           MOVE LENGTH OF      DA-TRX-REC
             TO RECORD-LENGTH-FULL.
           MOVE RECORD-LENGTH-HALF
             TO RDW-LENGTH             IN DA-TRX-REC.

MET00?     MOVE 'BHF'                                                   
             TO COMPANY-CODE           IN DA-TRX-REC.
MET002     MOVE DTL-GROUP-NUMBER       IN GROUP-BILLING-REC (1:7)
MET002       TO MASTER-ID              IN DA-TRX-REC (9:7).
           MOVE 'DA  '
             TO TRX-CODE               IN DA-TRX-REC.

           MOVE 'US'
             TO COUNTRY-CODE           IN DA-TRX-REC
                COUNTRY-OF-ORIGIN      IN DA-TRX-REC
                CITIZENSHIP-COUNTRY    IN DA-TRX-REC.

MET00?*    MOVE ORIGINATOR-CODE        IN PASS-AREA
MET00?     MOVE 'BHFPAS1C'
             TO ORIGINATOR-CODE        IN DA-TRX-REC.

MMLV02     MOVE 'F'
             TO CITIZENSHIP-STATUS     IN DA-TRX-REC.

           MOVE 'Y'
             TO LIST-BILLING-CODE      IN DA-TRX-REC.

           MOVE 'P'
             TO  LIST-BILL-SEQUENCE-CODE IN DA-TRX-REC.

MMLV02     MOVE 01
MMLV02       TO  BILL-EXTRACT-DAY      IN DA-TRX-REC.

MMLV02     MOVE 01
MMLV02       TO  PYMT-DUE-DAY          IN DA-TRX-REC.

           MOVE 'N'
             TO  DEPOSITORY-CODE       IN DA-TRX-REC.

MMLV02     MOVE ZEROS
             TO  LAPSE-DELAY-DAYS      IN DA-TRX-REC.

MMLV02     MOVE 'N'
MMLV02       TO  CONSOLIDATED-BILL-IND IN DA-TRX-REC.

MML518     MOVE SPACES
             TO PRODUCT-CODE           IN DA-TRX-REC.

           MOVE 'E'
             TO LANGUAGE-CODE          IN HEADER-INFO
                                       IN DA-TRX-REC
                LANGUAGE-CODE          IN TRX-INFO
                                       IN DA-TRX-REC.

MET002     MOVE DTL-GROUP-NUMBER       IN GROUP-BILLING-REC (1:7)
MET002       TO  DIRECTORY-ID          IN DA-TRX-REC (9:7).

MMLV01     SET CASE-CLIENT             IN DA-TRX-REC
MMLV01         TO TRUE.

MET00?     MOVE 1985
MMLV01       TO EFFECTIVE-YR           IN DA-TRX-REC.
MMLV01     MOVE 01
MMLV01       TO EFFECTIVE-MO           IN DA-TRX-REC.
MMLV01     MOVE 01
MMLV01       TO EFFECTIVE-DA           IN DA-TRX-REC.
MMLV01*    MOVE EFFECTIVE-DATE         IN DA-TRX-REC
MMLV01*      TO TRX-DATE               IN DA-TRX-REC.
MET00?*    MOVE DTL-ISSUE-YY           IN DTL-RECORD
MET00?*      TO TRX-YR                 IN DA-TRX-REC.
MET00?*    MOVE DTL-ISSUE-MM           IN DTL-RECORD
MET00?*      TO TRX-MO                 IN DA-TRX-REC.
MET00?*    MOVE DTL-ISSUE-DD           IN DTL-RECORD
MET00?*      TO TRX-DA                 IN DA-TRX-REC.

       100000-EXIT.
           EXIT.

       200000-FORMAT-TRX-INFO.

           PERFORM 210000-MOVE-INFO    THRU 210000-EXIT.
           PERFORM 220000-WRITE-TRX    THRU 220000-EXIT.

       200000-EXIT.
           EXIT.

       210000-MOVE-INFO.

MET00?     IF DTL-CORPORATE            IN GROUP-BILLING-REC
MMLV01        MOVE 'E'
MMLV01          TO CLIENT-SEX          IN DA-TRX-REC
MET00?        MOVE DTL-GRP-CORP-NAME   IN GROUP-BILLING-REC 
MMLV01          TO CLIENT-NAME         IN DA-TRX-REC
MET00?*       MOVE DTL-TIN             IN DTL-PERSON-RECORD
MET00?*         TO SOC-SEC-NO          IN DA-TRX-REC
MET00?        MOVE DTL-ADDRESS-LINE1   IN GROUP-BILLING-REC
MMLV01          TO ADDRESS-1           IN DA-TRX-REC
MET00?        MOVE DTL-ADDRESS-LINE2   IN GROUP-BILLING-REC
MMLV01          TO ADDRESS-LINE-2      IN DA-TRX-REC
MET00?*       MOVE DTL-ADDRESS-LINE3   IN GROUP-BILLING-REC 
MET00?*         TO ADDRESS-LINE-3      IN DA-TRX-REC
MET00?        MOVE DTL-CITY            IN GROUP-BILLING-REC  
MMLV01          TO CITY                IN DA-TRX-REC
MET00?        MOVE DTL-STATE           IN GROUP-BILLING-REC   
MMLV01          TO STATE               IN DA-TRX-REC
MET00?        MOVE DTL-ZIP-FIVE        IN GROUP-BILLING-REC
MMLV01          TO ZIP-POSTAL-CODE-5   IN DA-TRX-REC
MET00?        MOVE DTL-ZIP-PLUS-FOUR   IN GROUP-BILLING-REC
MMLV01          TO ZIP-POSTAL-CODE-4   IN DA-TRX-REC
MMLV01     ELSE
MET00?         IF DTL-INDIVIDUAL       IN GROUP-BILLING-REC
MET00?*           MOVE DTL-SEX         IN GROUP-BILLING-REC
MET00?*             TO CLIENT-SEX      IN DA-TRX-REC
MET00?*           MOVE DTL-BIRTH-YY    IN GROUP-BILLING-REC
MET00?*             TO CLIENT-BIRTH-YR IN DA-TRX-REC
MET00?*           MOVE DTL-BIRTH-MM    IN GROUP-BILLING-REC
MET00?*             TO CLIENT-BIRTH-MO IN DA-TRX-REC
MET00?*           MOVE DTL-BIRTH-DD    IN GROUP-BILLING-REC 
MET00?*             TO CLIENT-BIRTH-DA IN DA-TRX-REC
MET00?            MOVE DTL-LAST-NAME   IN GROUP-BILLING-REC
MMLV01              TO  LAST-NAME      IN DA-TRX-REC
MET00?            MOVE DTL-FIRST-NAME  IN GROUP-BILLING-REC
MMLV01              TO FIRST-NAME      IN DA-TRX-REC
MET00?*           MOVE DTL-NAME-PREFIX IN GROUP-BILLING-REC
MET00?*             TO PREFIX          IN DA-TRX-REC
MET00?*           MOVE DTL-NAME-SUFFIX IN GROUP-BILLING-REC
MET00?*             TO  SUFFIX         IN DA-TRX-REC
MET00?*           MOVE DTL-TIN         IN GROUP-BILLING-REC
MET00?*             TO SOC-SEC-NO      IN DA-TRX-REC
MET00?            MOVE DTL-ADDRESS-LINE1 IN GROUP-BILLING-REC
MMLV01              TO ADDRESS-1       IN DA-TRX-REC
MET00?            MOVE DTL-ADDRESS-LINE2 IN GROUP-BILLING-REC
MMLV01              TO ADDRESS-LINE-2  IN DA-TRX-REC
MET00?*           MOVE DTL-ADDRESS-LINE3 IN GROUP-BILLING-REC
MET00?*             TO ADDRESS-LINE-3  IN DA-TRX-REC
MET00?            MOVE DTL-CITY        IN GROUP-BILLING-REC
MMLV01              TO CITY            IN DA-TRX-REC
MET00?            MOVE DTL-STATE       IN GROUP-BILLING-REC
MMLV01              TO STATE           IN DA-TRX-REC
MET00?            MOVE DTL-ZIP-FIVE    IN GROUP-BILLING-REC
MMLV01              TO ZIP-POSTAL-CODE-5 IN DA-TRX-REC
MET00?            MOVE DTL-ZIP-PLUS-FOUR IN GROUP-BILLING-REC
MMLV01              TO ZIP-POSTAL-CODE-4 IN DA-TRX-REC
MMLV01          END-IF
MMLV01     END-IF.
MET00?     IF DTL-CORPORATE            IN GROUP-BILLING-REC
MMLV01        SET CLIENT-IS-CORP       IN DA-TRX-REC
                TO TRUE
MMLV01     ELSE
MMLV01        SET CLIENT-IS-INDIV      IN DA-TRX-REC
                TO TRUE
MMLV01     END-IF.

       210000-EXIT.
           EXIT.
 
       220000-WRITE-TRX.

           MOVE SPACES
             TO RESULTS-CODE     IN    VCSTRX3-COMM-AREA.
           COMPUTE TRX-LENGTH    IN    VCSTRX3-COMM-AREA
                 = RDW-LENGTH    IN    TRX-WORK-AREA - 4.
           SET WRITE-OPERATION   IN    VCSTRX3-COMM-AREA
            TO TRUE.

           CALL 'VCSTRX3'       USING VCSTRX3-COMM-AREA
                                       TRX-WORK-AREA.

           IF NOT SUCCESSFUL     IN    VCSTRX3-COMM-AREA
               MOVE WRITE-ERROR TO VCSABEND-CODE
               PERFORM 999999-CALL-VCSABEND THRU 999999-EXIT.

       220000-EXIT. EXIT.

       999999-CALL-VCSABEND.

           CALL 'VCSABEND'  USING  VCSABEND-CODE.

       999999-EXIT.
           EXIT.

