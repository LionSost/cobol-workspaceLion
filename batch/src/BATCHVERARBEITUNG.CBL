       IDENTIFICATION DIVISION.
       PROGRAM-ID.  BATCHVERARBEITUNG.
      *****************************************************************
      * Hülle für das Programm
      *****************************************************************       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-KONTO ASSIGN TO 'input.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-KONTO ASSIGN TO 'output.txt'
           ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
           FILE SECTION.
           FD INPUT-KONTO.
           01 KONTO-FILE.
              05 BANK-ID PIC X(5).
              05 KONTONUMMER PIC 9(10).
              05 BETRAG PIC Z(07)9.99.
              05 VERWENDUNGSZWECK PIC X(53).

           FD OUTPUT-KONTO.
           01 AUSGABE.
              05 ANFANG PIC X(14).
              05 NUMMER PIC 9(10).
              05 MITTE PIC X(6).
              05 BETRAG PIC Z(15)9.99.
       WORKING-STORAGE SECTION.
           01 AKTUELLE-EINGABE.
              05 AKTUELLE-BANKID PIC 9(5).
              05 AKTUELLE-KONTONUMMER PIC 9(10).
              05 AKTUELLER-BETRAG PIC 9(10).
              05 AKTUELLER-VERWENDUNGSZWECK PIC X(53).
           01 AKTUELLE-AUSGABE.
              05 ANFANG PIC X(14).
              05 NUMMER PIC 9(10).
              05 MITTE PIC X(6) VALUES ":     ".
              05 BETRAG PIC Z(15)9.99.
           01 ENDE PIC X.
           01 ZWISCHENSUMME PIC 9(18).
           01 AKTUELLE-BANK-ID PIC 9(5).
       PROCEDURE DIVISION.
           DISPLAY 'BATCH-VERARBEITUNG'

           OPEN OUTPUT OUTPUT-KONTO.
              OPEN INPUT INPUT-KONTO.
                 PERFORM UNTIL ENDE ="Y"
                    READ INPUT-KONTO INTO AKTUELLE-EINGABE
                       AT END MOVE 
                          "Y" TO ENDE
                          PERFORM SCHREIBE-BANK
                       NOT AT END PERFORM FILE-SCHREIBEN
                    END-READ
                 END-PERFORM
              CLOSE INPUT-KONTO.
           CLOSE OUTPUT-KONTO.
           DISPLAY "FERTIG"
           STOP RUN.



       FILE-SCHREIBEN SECTION.
           IF AKTUELLE-BANK-ID = 00000 THEN
              MOVE AKTUELLE-BANKID IN AKTUELLE-EINGABE 
                TO AKTUELLE-BANK-ID
           END-IF    
           
           IF AKTUELLE-BANK-ID
            = AKTUELLE-BANKID IN AKTUELLE-EINGABE THEN
              PERFORM SCHREIBE-KONTO
           ELSE
              PERFORM SCHREIBE-BANK
              MOVE AKTUELLE-BANKID TO AKTUELLE-BANK-ID
              MOVE 0.00 TO ZWISCHENSUMME 
              PERFORM SCHREIBE-KONTO
           END-IF      
       EXIT.

       SCHREIBE-BANK SECTION.
           MOVE "Total EUR for " TO ANFANG IN AKTUELLE-AUSGABE
           MOVE ZWISCHENSUMME TO BETRAG IN AKTUELLE-AUSGABE
           MOVE AKTUELLE-BANK-ID TO NUMMER IN AKTUELLE-AUSGABE
           PERFORM SCHREIBE-AUSGABE
       EXIT.


       SCHREIBE-KONTO SECTION.
           MOVE "Account: " TO ANFANG IN AKTUELLE-AUSGABE
           MOVE AKTUELLE-KONTONUMMER IN AKTUELLE-EINGABE
              TO NUMMER IN AKTUELLE-AUSGABE
           MOVE AKTUELLER-BETRAG TO BETRAG IN AKTUELLE-AUSGABE
           ADD AKTUELLER-BETRAG TO ZWISCHENSUMME
           PERFORM SCHREIBE-AUSGABE
       EXIT.

       SCHREIBE-AUSGABE SECTION.
           MOVE AKTUELLE-AUSGABE TO AUSGABE
           WRITE AUSGABE FROM AUSGABE
       EXIT.