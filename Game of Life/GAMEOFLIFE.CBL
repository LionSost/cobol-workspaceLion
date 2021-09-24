       IDENTIFICATION DIVISION.
       PROGRAM-ID.  GAMEOFLIFE.
      *****************************************************************
      * Hülle für das Programm
      *****************************************************************       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-START  ASSIGN TO 'input.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
           FILE SECTION.
           FD INPUT-START.
           01 EINGABE.
              05 ZEILE PIC X(170).
       WORKING-STORAGE SECTION.
           01 SPIELFELD.
              05 Y-ACHSE OCCURS 50.
                 07 ZELLE PIC 9 OCCURS 170.
           01 NEUES-SPIELFELD.
              05 NEUE-Y-ACHSE OCCURS 50.
                 07 NEUE-ZELLE PIC 9 OCCURS 170.
           01 AKTUELLE-EINGABE.
              05 AKTUELLE-ZEILE PIC X(170).
           01 DISPLAY-Y PIC 9(4).
           01 DISPLAY-X PIC 9(4).
           01 height1 PIC 9(4) VALUES 50.
           01 width1 PIC 9(4) VALUES 170.
           01 count-neighbour PIC 9.
           01 ENDE PIC X.
           01 ZEILE-EINGABE PIC 99.
           01 SPALTE-EINGABE PIC 999.
       PROCEDURE DIVISION.

           PERFORM start-abfrage
           *>PERFORM start-pos
           PERFORM display_world

           PERFORM 1000 TIMES
              PERFORM next-Generation
              PERFORM display_world
           END-PERFORM

           STOP RUN.

       start-abfrage SECTION.
           DISPLAY "Willkommen beim Game of Life von Jon Conways!"
           DISPLAY "Die Simulation wird in kürze Starten"
           MOVE 1 TO ZEILE-EINGABE
           OPEN INPUT  INPUT-START.
           PERFORM UNTIL ENDE = "Y"
              READ INPUT-START INTO AKTUELLE-EINGABE
                 AT END MOVE "Y" TO ENDE
                 NOT AT END PERFORM eingabe-schreiben
              ADD 1 TO ZEILE-EINGABE
           END-PERFORM
           CLOSE INPUT-START.

       eingabe-schreiben SECTION.
           PERFORM width1 TIMES
              MOVE AKTUELLE-ZEILE(SPALTE-EINGABE:1) TO
                 ZELLE(ZEILE-EINGABE, SPALTE-EINGABE)
              ADD 1 TO SPALTE-EINGABE
           END-PERFORM
           COMPUTE SPALTE-EINGABE = 1
       EXIT.
       

       next-Generation SECTION.
           PERFORM VARYING DISPLAY-Y FROM 1 BY 1 
              UNTIL DISPLAY-Y > height1
              PERFORM VARYING DISPLAY-X FROM 1 BY 1 
                 UNTIL DISPLAY-X > width1
                 PERFORM number-of-neighbours
                 
                 IF ZELLE(DISPLAY-Y, DISPLAY-X) = 1 THEN
                    IF count-neighbour = 2 OR count-neighbour = 3
                    THEN 
                       COMPUTE NEUE-ZELLE(DISPLAY-Y, DISPLAY-X) = 1
                    ELSE
                       COMPUTE NEUE-ZELLE(DISPLAY-Y, DISPLAY-X) = 0
                    END-IF
                 ELSE
                    IF count-neighbour = 3
                    THEN 
                       COMPUTE NEUE-ZELLE(DISPLAY-Y, DISPLAY-X) = 1
                    ELSE
                       COMPUTE NEUE-ZELLE(DISPLAY-Y, DISPLAY-X) = 0
                    END-IF
                 END-IF
                 COMPUTE count-neighbour = 0
             END-PERFORM
           END-PERFORM

           MOVE NEUES-SPIELFELD TO SPIELFELD 
       EXIT.

       number-of-neighbours SECTION.
           IF DISPLAY-Y >= 1 AND DISPLAY-X >= 1 
              AND ZELLE(DISPLAY-Y - 1, DISPLAY-X - 1) = 1 THEN
              ADD 1 TO count-neighbour
           END-IF

           IF DISPLAY-X >= 1 AND ZELLE(DISPLAY-Y, DISPLAY-X - 1) = 1 
           THEN
              ADD 1 TO count-neighbour
           END-IF

           IF DISPLAY-Y >= 1 AND ZELLE(DISPLAY-Y - 1, DISPLAY-X) = 1
           THEN
              ADD 1 TO count-neighbour
           END-IF

           IF DISPLAY-Y >= 1 AND DISPLAY-X < width1
              AND ZELLE(DISPLAY-Y - 1, DISPLAY-X + 1) = 1 THEN
              ADD 1 TO count-neighbour
           END-IF

           IF DISPLAY-X < width1 AND ZELLE(DISPLAY-Y, DISPLAY-X + 1) = 1
           THEN
              ADD 1 TO count-neighbour
           END-IF

           IF DISPLAY-Y < height1 
              AND ZELLE(DISPLAY-Y + 1, DISPLAY-X) = 1 THEN
              ADD 1 TO count-neighbour
           END-IF

           IF DISPLAY-Y < height1  AND DISPLAY-X < width1
              AND ZELLE(DISPLAY-Y + 1, DISPLAY-X + 1) = 1 THEN
              ADD 1 TO count-neighbour
           END-IF

           IF DISPLAY-Y < height1  AND DISPLAY-X >= 1
              AND ZELLE(DISPLAY-Y + 1, DISPLAY-X - 1) = 1 THEN
              ADD 1 TO count-neighbour
           END-IF
       EXIT.



       display_world SECTION.
           PERFORM VARYING DISPLAY-Y FROM 1 BY 1 
              UNTIL DISPLAY-Y > height1
              PERFORM VARYING DISPLAY-X FROM 1 BY 1 
                 UNTIL DISPLAY-X > 50
                 EVALUATE ZELLE(DISPLAY-Y, DISPLAY-X)
                    WHEN 0
                       DISPLAY "  " AT COLUMN DISPLAY-X, LINE DISPLAY-Y 
                       WITH
                          FOREGROUND-COLOR 7,
                          BACKGROUND-COLOR 7
                     WHEN 1
                       DISPLAY "  " AT COLUMN DISPLAY-X LINE DISPLAY-Y 
                       WITH
                          FOREGROUND-COLOR 4,
                          BACKGROUND-COLOR 4  
                     END-EVALUATE
              END-PERFORM
           END-PERFORM 
       EXIT.


       start-pos SECTION.

           PERFORM VARYING DISPLAY-Y FROM 1 BY 1 
              UNTIL DISPLAY-Y > height1
              PERFORM VARYING DISPLAY-X FROM 1 BY 1 
                 UNTIL DISPLAY-X > width1
                  COMPUTE ZELLE(DISPLAY-Y, DISPLAY-X) = 0
                  COMPUTE NEUE-ZELLE(DISPLAY-Y, DISPLAY-X) = 0

             END-PERFORM
           END-PERFORM

           COMPUTE ZELLE(30,50) = 1
           COMPUTE ZELLE(31,51) = 1
           COMPUTE ZELLE(32,51) = 1
           COMPUTE ZELLE(32,50) = 1
           COMPUTE ZELLE(32,49) = 1

       EXIT.