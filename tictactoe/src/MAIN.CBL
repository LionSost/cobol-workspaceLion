       IDENTIFICATION DIVISION.
       PROGRAM-ID.  MAIN.
      *****************************************************************
      * Hülle für das Programm
      *****************************************************************       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
           01 field.
              05 firstrow PIC X(3).
              05 secondrow PIC X(3).
              05 thirdrow PIC X(3).
           01 drawing.
              05 firstrow PIC X(15).
              05 secondrow PIC X(15).
              05 thirdrow PIC X(15).
              05 fourthrow PIC X(15).
              05 fithrow PIC X(15).
           01 1-gewonnen PIC X(4) VALUES "nein".
           01 2-gewonnen PIC X(4) VALUES "nein".
           01 zuege PIC 99.
           01 XO PIC X.
           01 zeile PIC 9.
           01 spalte PIC 9.
       PROCEDURE DIVISION.
           ADD 1 TO zuege
           PERFORM INITIALDRWFIELD
           PERFORM DRAW

           PERFORM UNTIL 1-gewonnen = "ja" OR 2-gewonnen = "ja" OR
           zuege = 10 
              IF FUNCTION MOD(zuege, 2) = 1
              THEN 
                 DISPLAY "Spieler 1 ist dran:"
                 MOVE "X" TO XO
              ELSE  
                 DISPLAY "Spieler 2 ist dran:"
                 MOVE "O" TO XO
              END-IF
              
              PERFORM ZEILENEINGABE
              

              ADD 1 TO zuege
           END-PERFORM

           IF 1-gewonnen = "ja"
           THEN  
              DISPLAY "Spieler 1 hat gewonnen!"
           ELSE  
              IF 2-gewonnen = "ja"
              THEN  
                 DISPLAY "Spieler 2 hat gewonnen!"
              ELSE
                 DISPLAY "Unentschieden"
              END-IF
           END-IF
           STOP RUN.

       ZEILENEINGABE SECTION.
           DISPLAY "Zeile:"
           ACCEPT zeile

           IF zeile IS NOT NUMERIC OR zeile <= 0 OR zeile >= 4
           THEN 
              DISPLAY "Bitte gib eine Zahl zwischen 1 und 3 ein"
              PERFORM ZEILENEINGABE
           END-IF

           PERFORM SPALTENEINGABE
           EXIT.


       SPALTENEINGABE SECTION.
           DISPLAY "Spalte:"
           ACCEPT spalte
           
           IF spalte IS NOT NUMERIC OR spalte <= 0 OR spalte >= 4 
           THEN 
              DISPLAY "Bitte gib eine Zahl zwischen 1 und 3 ein"
              PERFORM SPALTENEINGABE
           END-IF

           IF field()
           EXIT.

       INITIALDRWFIELD SECTION.
           MOVE "   |   |   " TO firstrow IN drawing  
           MOVE "-----------" TO secondrow IN drawing 
           MOVE "   |   |   " TO thirdrow IN drawing
           MOVE "-----------" TO fourthrow IN drawing 
           MOVE "   |   |   " TO fithrow IN drawing
           EXIT.

       DRAW SECTION.
           DISPLAY firstrow IN  drawing
           DISPLAY secondrow IN drawing
           DISPLAY thirdrow IN drawing
           DISPLAY fourthrow IN drawing
           DISPLAY fithrow IN drawing
           EXIT.
