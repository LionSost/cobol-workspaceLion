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
           01 CURRENT-SPACE PIC 999.
           01 CURRENT-FRONTSPACE PIC 999.
           01 CURRENT-LETTER PIC X.
           01 LETTER PIC X.
           01 SIZE-DIA PIC 999.
           01 CURRENT-LOOP PIC 999.
           01 CURRENT-EXPENSE PIC X(300).
           01 LOOPS PIC 999.

       PROCEDURE DIVISION.
           
           PERFORM EINLESEN
           
            MOVE SIZE-DIA TO CURRENT-FRONTSPACE
            ADD 1 TO CURRENT-FRONTSPACE
            ADD 3 TO CURRENT-SPACE
              
           MOVE FUNCTION CHAR(66) TO CURRENT-LETTER
            ADD 1 TO CURRENT-FRONTSPACE

           PERFORM FRONT
           DISPLAY CURRENT-EXPENSE

           ADD -1 TO CURRENT-FRONTSPACE

           
           ADD SIZE-DIA TO  LOOPS
           ADD SIZE-DIA TO  LOOPS
           ADD 1 TO LOOPS
           
           MOVE FUNCTION CHAR(67) TO CURRENT-LETTER
 
           
           PERFORM LOOPS TIMES
              ADD 1 TO CURRENT-LOOP

              PERFORM FRONT
              PERFORM MID 
              
               IF CURRENT-LOOP <= SIZE-DIA THEN
                 ADD 2 TO CURRENT-SPACE
                 ADD -1 TO CURRENT-FRONTSPACE
                 MOVE FUNCTION CHAR(66 + CURRENT-LOOP + 1) 
                 TO CURRENT-LETTER
               ELSE
                 ADD -2 TO CURRENT-SPACE
                 ADD 1 TO CURRENT-FRONTSPACE
                 MOVE FUNCTION CHAR(66 + LOOPS - CURRENT-LOOP) 
                 TO CURRENT-LETTER
               END-IF

               DISPLAY CURRENT-EXPENSE
           END-PERFORM

           MOVE FUNCTION CHAR(66) TO CURRENT-LETTER

           PERFORM FRONT
           DISPLAY CURRENT-EXPENSE


           STOP RUN.



       EINLESEN SECTION.
           DISPLAY "Gib bitte die größe des Diamanten ein:"
           ACCEPT SIZE-DIA

           IF SIZE-DIA IS NOT NUMERIC OR SIZE-DIA <= 0
           THEN 
              DISPLAY "Not a valid Number"
              PERFORM EINLESEN
           END-IF
           EXIT.
           

       FRONT SECTION.
           MOVE "" TO CURRENT-EXPENSE

           MOVE CURRENT-LETTER
           TO CURRENT-EXPENSE(CURRENT-FRONTSPACE : 1)
           EXIT.


       MID SECTION.
          
           MOVE CURRENT-LETTER
           TO CURRENT-EXPENSE(CURRENT-FRONTSPACE + CURRENT-SPACE : 1)
           EXIT.