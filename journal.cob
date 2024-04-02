       IDENTIFICATION DIVISION.
       PROGRAM-ID.    JOURNAL.
      ****************************************************************
      * Simple Journal Program                                       *
      ****************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  IBM-PC.
       OBJECT-COMPUTER.  IBM-PC.
       SPECIAL-NAMES.    CURSOR       IS           CRSPOS.
                         CRT STATUS   IS           CRTSTAT.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JRN-MASTER          ASSIGN       "./JRN"
                                      ORGANIZATION INDEXED
                                      ACCESS       DYNAMIC
                                      RECORD KEY   JRNDK
                                      FILE STATUS  RETJRN.
      ****************************************************************   
       DATA DIVISION.
       FILE SECTION.
       FD   JRN-MASTER
            RECORD CONTAINS           1284 CHARACTERS
            LABEL  RECORDS            ARE  STANDARD
            DATA   RECORD             IS   JRND.
      *Journal File Descriptor
       01          JRND.
           05      JRNDK              PIC  X(10).
           05      JRNDD              PIC  X(1274).
       WORKING-STORAGE SECTION.
      *Journal Data Record
       01          JRN.
           05      JRNKEY.
             10    JRNKYEAR           PIC  X(04).
             10    JRNKMONTH          PIC  X(02).
             10    JRNKDAY            PIC  X(02).
             10    JRNKPAGE           PIC  X(02).
           05      JRNDATA.
             10    JRNUDATE.
               15  JRNUCC             PIC  X(02).
               15  JRNUYY             PIC  X(02).
               15  JRNUMM             PIC  X(02).
               15  JRNUDD             PIC  X(02).
             10    JRNUTIME.
               15  JRNUHR             PIC  X(02).
               15  JRNUMN             PIC  X(02). 
               15  JRNUSC             PIC  X(02).
             10    JRNUUSR            PIC  X(10).
             10    JRNLINE1           PIC  X(78).
             10    JRNLINE2           PIC  X(78).
             10    JRNLINE3           PIC  X(78).
             10    JRNLINE4           PIC  X(78).
             10    JRNLINE5           PIC  X(78).
             10    JRNLINE6           PIC  X(78).
             10    JRNLINE7           PIC  X(78).
             10    JRNLINE8           PIC  X(78).
             10    JRNLINE9           PIC  X(78).
             10    JRNLINE10          PIC  X(78).
             10    JRNLINE11          PIC  X(78).
             10    JRNLINE12          PIC  X(78).
             10    JRNLINE13          PIC  X(78).
             10    JRNLINE14          PIC  X(78).
             10    JRNLINE15          PIC  X(78).
             10    JRNLINE16          PIC  X(78).
      *File Return Codes
       01          RET.
           05      RETJRN             PIC  X(02).
      *Colors
       01          COLORS.
           05      BLACK              PIC 9  VALUE 0.
           05      BLUE               PIC 9  VALUE 1.
           05      GREEN              PIC 9  VALUE 2.
           05      CYAN               PIC 9  VALUE 3.
           05      RED                PIC 9  VALUE 4.
           05      MAGENTA            PIC 9  VALUE 5.
           05      BROWN              PIC 9  VALUE 6.
           05      WHITE              PIC 9  VALUE 7.
           05      GREY               PIC 9  VALUE 8.
           05      LIGHT-BLUE         PIC 9  VALUE 9.
           05      LIGHT-GREEN        PIC 99 VALUE 10.
           05      LIGHT-CYAN         PIC 99 VALUE 11.
           05      LIGHT-RED          PIC 99 VALUE 12.
           05      LIGHT-MAGENTA      PIC 99 VALUE 13.
           05      YELLOW             PIC 99 VALUE 14.
           05      HI-WHITE           PIC 99 VALUE 15.
      *Function Keys
        01         CFKEYS.
          05       SUBMIT             PIC 9(4) VALUE 0000.
          05       CF1                PIC 9(4) VALUE 1001.
          05       CF2                PIC 9(4) VALUE 1002.
          05       CF3                PIC 9(4) VALUE 1003.
          05       CF4                PIC 9(4) VALUE 1004.
          05       CF5                PIC 9(4) VALUE 1005.
          05       CF6                PIC 9(4) VALUE 1006.
          05       CF7                PIC 9(4) VALUE 1007.
          05       CF8                PIC 9(4) VALUE 1008.
          05       CF9                PIC 9(4) VALUE 1009.
          05       CF10               PIC 9(4) VALUE 1010.
          05       CF11               PIC 9(4) VALUE 1011.
          05       CF12               PIC 9(4) VALUE 1012.
          05       CF13               PIC 9(4) VALUE 1013.
          05       CF14               PIC 9(4) VALUE 1014.
          05       CF15               PIC 9(4) VALUE 1015.
          05       CF16               PIC 9(4) VALUE 1016.
          05       CF17               PIC 9(4) VALUE 1017.
          05       CF18               PIC 9(4) VALUE 1018.
          05       CF19               PIC 9(4) VALUE 1019.
          05       CF20               PIC 9(4) VALUE 1020.
          05       CF21               PIC 9(4) VALUE 1021.
          05       CF22               PIC 9(4) VALUE 1022.
          05       CF23               PIC 9(4) VALUE 1023.
          05       CF24               PIC 9(4) VALUE 1024.
      *Cursor Location
        01         CRSPOS             PIC 9(6).
        01         CRSXY              REDEFINES CRSPOS.
          05       CRSPOSY            PIC 999.
          05       CRSPOSX            PIC 999.
      *Screen Status
        01         CRTSTAT            PIC 9(4).
      *Date
        01         NOWD.
          05       NOWDCC             PIC  X(02).
          05       NOWDYY             PIC  X(02).
          05       NOWDMM             PIC  X(02).
          05       NOWDDD             PIC  X(02).
      *Time
        01         NOWT.
          05       NOWTHR             PIC  X(02).
          05       NOWTMN             PIC  X(02).
          05       NOWTSC             PIC  X(02).
          05       NOWTNS             PIC  X(02).
      *Passthrough
        01         PT.
          05       PT-MSG             PIC  X(50).
          05       PT-YEAR            PIC  X(04).
          05       PT-MONTH           PIC  X(02).
          05       PT-DAY             PIC  X(02).
          05       PT-PAGE            PIC  X(02).
          05       PT-LN1             PIC  X(78).
          05       PT-LN2             PIC  X(78).
          05       PT-LN3             PIC  X(78).
          05       PT-LN4             PIC  X(78).
          05       PT-LN5             PIC  X(78).
          05       PT-LN6             PIC  X(78).
          05       PT-LN7             PIC  X(78).
          05       PT-LN8             PIC  X(78).
          05       PT-LN9             PIC  X(78).
          05       PT-LN10            PIC  X(78).
          05       PT-LN11            PIC  X(78).
          05       PT-LN12            PIC  X(78).
          05       PT-LN13            PIC  X(78).
          05       PT-LN14            PIC  X(78).
          05       PT-LN15            PIC  X(78).
          05       PT-LN16            PIC  X(78).
      *New Key
       01          NEWKEY.
          05       NEWKYEAR           PIC  X(04).
          05       NEWKMONTH          PIC  X(02).
          05       NEWKDAY            PIC  X(02).
          05       NEWKPAGE           PIC  X(02).
      * Old Key
       01          OLDKEY.
          05       OLDKYEAR           PIC  X(04).
          05       OLDKMONTH          PIC  X(02).
          05       OLDKDAY            PIC  X(02).
          05       OLDKPAGE           PIC  X(02).
      ****************************************************************
       SCREEN SECTION.
        01         CLEAR-SCREEN.
          05       BLANK SCREEN.
        01         PANEL.
          05       VALUE "Journal"
                   LINE 01 COL 38
                   FOREGROUND-COLOR IS HI-WHITE.
          05       VALUE "Year:"
                   LINE 02 COL 02
                   FOREGROUND-COLOR IS BLUE.
          05       PANEL-YEAR
                   LINE 02 COL 08
                   UNDERLINE
                   FOREGROUND-COLOR IS GREEN
                   PIC X(04) TO PT-YEAR
                   FROM PT-YEAR.
          05       VALUE "Month:"
                   LINE 02 COL 13
                   FOREGROUND-COLOR IS BLUE.
          05       PANEL-MONTH
                   LINE 02 COL 20
                   UNDERLINE
                   FOREGROUND-COLOR IS GREEN
                   PIC X(02) TO PT-MONTH
                   FROM PT-MONTH.
          05       VALUE "Day:"
                   LINE 02 COL 23
                   FOREGROUND-COLOR IS BLUE.
          05       PANEL-DAY
                   LINE 02 COL 28
                   UNDERLINE
                   FOREGROUND-COLOR IS GREEN
                   PIC X(02) TO PT-DAY
                   FROM PT-DAY.
          05       VALUE "Page:"
                   LINE 02 COL 31
                   FOREGROUND-COLOR IS BLUE.
          05       PANEL-PAGE
                   LINE 02 COL 37
                   UNDERLINE
                   FOREGROUND-COLOR IS GREEN
                   PIC X(02) TO PT-PAGE
                   FROM PT-PAGE.
          05       VALUE "_______________________________"
                   LINE 03 COL 01
                   FOREGROUND-COLOR IS BLUE.
          05       VALUE "_______________________________"
                   LINE 03 COL 31
                   FOREGROUND-COLOR IS BLUE.
          05       VALUE "____________________"
                   LINE 03 COL 61
                   FOREGROUND-COLOR IS BLUE.
          05       PANEL-LN1
                   LINE 05 COL 02
                   UNDERLINE
                   FOREGROUND-COLOR IS GREEN
                   PIC X(78) TO PT-LN1
                   FROM PT-LN1.
          05       PANEL-LN2
                   LINE 06 COL 02
                   UNDERLINE
                   FOREGROUND-COLOR IS GREEN
                   PIC X(78) TO PT-LN2
                   FROM PT-LN2.
          05       PANEL-LN3
                   LINE 07 COL 02
                   UNDERLINE
                   FOREGROUND-COLOR IS GREEN
                   PIC X(78) TO PT-LN3
                   FROM PT-LN3.
          05       PANEL-LN4
                   LINE 08 COL 02
                   UNDERLINE
                   FOREGROUND-COLOR IS GREEN
                   PIC X(78) TO PT-LN4
                   FROM PT-LN4.
          05       PANEL-LN5
                   LINE 09 COL 02
                   UNDERLINE
                   FOREGROUND-COLOR IS GREEN
                   PIC X(78) TO PT-LN5
                   FROM PT-LN5.
          05       PANEL-LN6
                   LINE 10 COL 02
                   UNDERLINE
                   FOREGROUND-COLOR IS GREEN
                   PIC X(78) TO PT-LN6
                   FROM PT-LN6.
          05       PANEL-LN7
                   LINE 11 COL 02
                   UNDERLINE
                   FOREGROUND-COLOR IS GREEN
                   PIC X(78) TO PT-LN7
                   FROM PT-LN7.
          05       PANEL-LN8
                   LINE 12 COL 02
                   UNDERLINE
                   FOREGROUND-COLOR IS GREEN
                   PIC X(78) TO PT-LN7
                   FROM PT-LN7.
          05       PANEL-LN9
                   LINE 13 COL 02
                   UNDERLINE
                   FOREGROUND-COLOR IS GREEN
                   PIC X(78) TO PT-LN8
                   FROM PT-LN8.
          05       PANEL-LN10
                   LINE 14 COL 02
                   UNDERLINE
                   FOREGROUND-COLOR IS GREEN
                   PIC X(78) TO PT-LN10
                   FROM PT-LN10.
          05       PANEL-LN11
                   LINE 15 COL 02
                   UNDERLINE
                   FOREGROUND-COLOR IS GREEN
                   PIC X(78) TO PT-LN11
                   FROM PT-LN11.
          05       PANEL-LN12
                   LINE 16 COL 02
                   UNDERLINE
                   FOREGROUND-COLOR IS GREEN
                   PIC X(78) TO PT-LN12
                   FROM PT-LN12.
          05       PANEL-LN13
                   LINE 17 COL 02
                   UNDERLINE
                   FOREGROUND-COLOR IS GREEN
                   PIC X(78) TO PT-LN13
                   FROM PT-LN13.
          05       PANEL-LN14
                   LINE 18 COL 02
                   UNDERLINE
                   FOREGROUND-COLOR IS GREEN
                   PIC X(78) TO PT-LN14
                   FROM PT-LN14.
          05       PANEL-LN15
                   LINE 19 COL 02
                   UNDERLINE
                   FOREGROUND-COLOR IS GREEN
                   PIC X(78) TO PT-LN15
                   FROM PT-LN15.
          05       PANEL-LN16
                   LINE 20 COL 02
                   UNDERLINE
                   FOREGROUND-COLOR IS GREEN
                   PIC X(78) TO PT-LN16
                   FROM PT-LN16.
          05       VALUE "F3=Exit"
                   FOREGROUND-COLOR IS BLACK
                   BACKGROUND-COLOR IS WHITE
                   LINE 23 COL 10.
          05       VALUE "F12=Esc"
                   FOREGROUND-COLOR IS BLACK
                   BACKGROUND-COLOR IS WHITE
                   LINE 23 COL 74.
          05       PANEL-DATE-MM
                   FROM NOWDMM
                   LINE 24 COL 01.
          05       VALUE "/"
                   LINE 24 COL 03.
          05       PANEL-DATE-DD
                   FROM NOWDDD
                   LINE 24 COL 04.
          05       VALUE "/"
                   LINE 24 COL 06.
          05       PANEL-DATE-CC
                   FROM NOWDCC
                   LINE 24 COL 07.
          05       PANEL-DATE-YY
                   FROM NOWDYY
                   LINE 24 COL 09.
          05       PANEL-TIME-HR
                   FROM NOWTHR
                   LINE 24 COL 13.
          05       VALUE ":"
                   LINE 24 COL 15.
          05       PANEL-TIME-MN
                   FROM NOWTMN
                   LINE 24 COL 16.
          05       VALUE ":"
                   LINE 24 COL 18.
          05       PANEL-TIME-SC
                   FROM NOWTSC
                   LINE 24 COL 19.
          05       VALUE "."
                   LINE 24 COL 21.
          05       PANEL-TIME-NS
                   FROM NOWTNS
                   LINE 24 COL 22.
          05       PANEL-MSG
                   FOREGROUND-COLOR IS HI-WHITE
                   FROM PT-MSG
                   LINE 24 COL 26.
          05       PANEL-POSX
                   FOREGROUND-COLOR IS HI-WHITE
                   FROM CRSPOSX
                   LINE 24 COL 74.
          05       VALUE "/"
                   LINE 24 COL 77.
          05       PANEL-POSY
                   FOREGROUND-COLOR IS HI-WHITE
                   FROM CRSPOSY
                   LINE 24 COL 78.
      ****************************************************************
       PROCEDURE DIVISION.
       DECLARATIVES.
       000-ERROR SECTION.
           USE   AFTER STANDARD ERROR PROCEDURE ON JRN-MASTER.
       000-ERRORED.
           CONTINUE.
       END DECLARATIVES.
       000-MAIN.
           PERFORM 999-OPEN           THRU 999-OPEN-EXIT.
       000-LOOP.
           PERFORM 999-TIMESTAMP      THRU 999-TIMESTAMP-EXIT.
           PERFORM 000-INIT           THRU 000-EXIT.
           DISPLAY PANEL.
           ACCEPT  PANEL.

           IF     (CRTSTAT            =    CF3)
               GO                     TO   000-EOJ.
           IF     (CRTSTAT            =    CF12)
               GO                     TO   000-EOJ.

           MOVE    SPACES             TO   PT-MSG    
           MOVE    PT-YEAR            TO   NEWKYEAR
           MOVE    PT-MONTH           TO   NEWKMONTH
           MOVE    PT-DAY             TO   NEWKDAY
           MOVE    PT-PAGE            TO   NEWKPAGE

           IF      NEWKEY             NOT  =  OLDKEY
               PERFORM 100-DISPLAY    THRU 100-EXIT
           ELSE
               PERFORM 300-UPDATE     THRU 300-EXIT.
               
           GO                         TO   000-LOOP.
       000-EOJ.
           GO                         TO   999-CLOSE.

       000-INIT.
           IF     (PT-YEAR            =    SPACES)
               MOVE NOWDCC            TO   PT-YEAR (1:2) 
               MOVE NOWDYY            TO   PT-YEAR (3:2).
           IF     (PT-MONTH           =    SPACES)
               MOVE NOWDMM            TO   PT-MONTH.
           IF     (PT-DAY             =    SPACES)
               MOVE NOWDDD            TO   PT-DAY.
           IF     (PT-PAGE            =    SPACES)
               MOVE '01'              TO   PT-PAGE.
       000-EXIT.
       EXIT.
      ****************************************************************
      * 100 - Display Record                                         *
      ****************************************************************
       100-DISPLAY.
           MOVE    NEWKEY             TO   OLDKEY

           MOVE    NEWKYEAR           TO   JRNKEY
           MOVE    NEWKMONTH          TO   JRNKMONTH
           MOVE    NEWKDAY            TO   JRNKDAY
           MOVE    NEWKPAGE           TO   JRNKPAGE
           MOVE    JRNKEY             TO   JRNDK
           READ    JRN-MASTER
           IF      RETJRN             NOT  =  '00'
               INITIALIZE                  JRND 
               MOVE "Record Not Found. Type Data to Create." TO  PT-MSG.              

           MOVE    JRND               TO   JRN
      *     MOVE    JRNKYEAR           TO   PT-YEAR
      *     MOVE    JRNKMONTH          TO   PT-MONTH
      *     MOVE    JRNKDAY            TO   PT-DAY
      *     MOVE    JRNKPAGE           TO   PT-PAGE
           MOVE    JRNLINE1           TO   PT-LN1
           MOVE    JRNLINE2           TO   PT-LN2
           MOVE    JRNLINE3           TO   PT-LN3
           MOVE    JRNLINE4           TO   PT-LN4
           MOVE    JRNLINE5           TO   PT-LN5
           MOVE    JRNLINE6           TO   PT-LN6
           MOVE    JRNLINE7           TO   PT-LN7
           MOVE    JRNLINE8           TO   PT-LN8
           MOVE    JRNLINE9           TO   PT-LN9
           MOVE    JRNLINE10          TO   PT-LN10
           MOVE    JRNLINE11          TO   PT-LN11
           MOVE    JRNLINE12          TO   PT-LN12
           MOVE    JRNLINE13          TO   PT-LN13
           MOVE    JRNLINE14          TO   PT-LN14
           MOVE    JRNLINE15          TO   PT-LN15
           MOVE    JRNLINE16          TO   PT-LN16.
           
       100-EXIT.
           EXIT.
      ****************************************************************
      * 300 - Update Record                                          *
      ****************************************************************
       300-UPDATE.
           MOVE    NEWKEY             TO   OLDKEY
           
           MOVE    NEWKYEAR           TO   JRNKEY
           MOVE    NEWKMONTH          TO   JRNKMONTH
           MOVE    NEWKDAY            TO   JRNKDAY
           MOVE    NEWKPAGE           TO   JRNKPAGE
           MOVE    JRNKEY             TO   JRNDK
           READ    JRN-MASTER
           IF      RETJRN             =    '00'
               MOVE JRND              TO   JRN
           ELSE
               INITIALIZE                  JRND.

           MOVE    PT-YEAR            TO   JRNKYEAR
           MOVE    PT-MONTH           TO   JRNKMONTH
           MOVE    PT-DAY             TO   JRNKDAY
           MOVE    PT-PAGE            TO   JRNKPAGE
           MOVE    PT-LN1             TO   JRNLINE1
           MOVE    PT-LN2             TO   JRNLINE2
           MOVE    PT-LN3             TO   JRNLINE3
           MOVE    PT-LN4             TO   JRNLINE4
           MOVE    PT-LN5             TO   JRNLINE5
           MOVE    PT-LN6             TO   JRNLINE6
           MOVE    PT-LN7             TO   JRNLINE7
           MOVE    PT-LN8             TO   JRNLINE8
           MOVE    PT-LN9             TO   JRNLINE9
           MOVE    PT-LN10            TO   JRNLINE10
           MOVE    PT-LN11            TO   JRNLINE11
           MOVE    PT-LN12            TO   JRNLINE12
           MOVE    PT-LN13            TO   JRNLINE13
           MOVE    PT-LN14            TO   JRNLINE14
           MOVE    PT-LN15            TO   JRNLINE15
           MOVE    PT-LN16            TO   JRNLINE16

           PERFORM 999-TIMESTAMP      THRU 999-TIMESTAMP-EXIT

           MOVE    NOWDCC             TO   JRNUCC
           MOVE    NOWDYY             TO   JRNUYY
           MOVE    NOWDMM             TO   JRNUMM
           MOVE    NOWDDD             TO   JRNUDD
           MOVE    NOWTHR             TO   JRNUHR
           MOVE    NOWTMN             TO   JRNUMN
           MOVE    NOWTSC             TO   JRNUSC
           MOVE    JRN                TO   JRND

           IF      RETJRN             NOT  =  '00'
               MOVE 'Record Created'  TO   PT-MSG
               WRITE                  JRND
           ELSE
               MOVE 'Record Updated'  TO   PT-MSG
               READ JRN-MASTER
               MOVE JRN               TO   JRND
               REWRITE                JRND.
       300-EXIT.
           EXIT.
      ****************************************************************
      * 999 - Housekeeping and EOJ Routines                          *
      ****************************************************************
       999-TIMESTAMP.
           ACCEPT NOWD                FROM DATE YYYYMMDD
           ACCEPT NOWT                FROM TIME.
       999-TIMESTAMP-EXIT.
           EXIT.
      ****************************************************************
       999-OPEN.
           OPEN I-O SHARING WITH ALL OTHER JRN-MASTER.
      *     IF    (RETJRN              NOT  =  '00' OR '41')
      *         DISPLAY "ERROR: Could not open data file."
      *
      *     STOP RUN.
       999-OPEN-EXIT.
           EXIT.
      ****************************************************************
       999-CLOSE.
           CLOSE                      JRN-MASTER.
           IF    (RETJRN              NOT  =  '00')
               DISPLAY "ERROR: Could not close data file.".
           STOP RUN.

