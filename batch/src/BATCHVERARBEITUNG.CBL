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
              05 KONTONUMMER PIC X(10).
              05 BETRAG PIC Z(08)9.99.
              05 VERWENDUNGSZWECK PIC X(53).

           FD OUTPUT-KONTO.
           01 AUSGABE.
               05 AUSGABE-KONTO.
                 07 ACCOUNT PIC X(9).
                 07 KONTONUMMER PIC X(10).
                 07 DOPPELPUNKT PIC X(6).
                 07 BETRAG PIC 9(11).
              05 AUSGABE-BANK.
                 07 WORTE PIC X(14).
                 07 BANK-ID PIC X(5).
                 07 DOPPELPUNKT PIC X(6).
                 07 SUMME PIC Z(15)9.99. 
       WORKING-STORAGE SECTION.
           01 AKTUELLE-EINGABE.
              05 AKTUELLE-BANKID PIC X(5).
              05 AKTUELLE-KONTONUMMER PIC X(10).
              05 AKTUELLER-BETRAG PIC 9(11).
              05 AKTUELLER-VERWENDUNGSZWECK PIC X(53).
           01 AKTUELLE-AUSGABE.
              05 AUSGABE-KONTO.
                 07 ACCOUNT PIC X(9) VALUES "Account: ".
                 07 KONTONUMMER PIC X(10).
                 07 DOPPELPUNKT PIC X(6) VALUES ":     ".
                 07 BETRAG PIC Z(08)9.99.
              05 AUSGABE-BANK.
                 07 WORTE PIC X(14) VALUES "Total EUR for ".
                 07 BANK-ID PIC X(5).
                 07 DOPPELPUNKT PIC X(6) VALUES ":     ".
                 07 SUMME PIC Z(15)9.99. 
           01 ENDE PIC X.
           01 ZWISCHENSUMME PIC 9(18).
       PROCEDURE DIVISION.
           DISPLAY 'BATCH-VERARBEITUNG'

           OPEN OUTPUT OUTPUT-KONTO.
              OPEN INPUT INPUT-KONTO.
                 PERFORM UNTIL ENDE ="Y"
                    READ INPUT-KONTO INTO AKTUELLE-EINGABE
                       AT END MOVE "Y" TO ENDE
                       NOT AT END PERFORM FILE-SCHREIBEN
                    END-READ
                 END-PERFORM
              CLOSE INPUT-KONTO.
           CLOSE OUTPUT-KONTO.
           DISPLAY "FERTIG"
           STOP RUN.



       FILE-SCHREIBEN SECTION.
           IF BANK-ID IN AUSGABE-BANK IN AKTUELLE-AUSGABE
            = AKTUELLE-BANKID IN AKTUELLE-EINGABE THEN
              MOVE AKTUELLE-KONTONUMMER IN AKTUELLE-EINGABE
                 TO KONTONUMMER IN AUSGABE-KONTO IN AKTUELLE-AUSGABE
              MOVE AKTUELLER-BETRAG TO BETRAG IN AUSGABE-KONTO 
                 IN AKTUELLE-AUSGABE
              ADD AKTUELLER-BETRAG TO ZWISCHENSUMME

              MOVE AUSGABE-KONTO IN AKTUELLE-AUSGABE 
                 TO AUSGABE-KONTO IN AUSGABE
              WRITE AUSGABE FROM AUSGABE
           ELSE
              MOVE ZWISCHENSUMME TO SUMME IN AUSGABE-BANK
                 IN AKTUELLE-AUSGABE
              MOVE AUSGABE-BANK IN AKTUELLE-AUSGABE 
                 TO AUSGABE-BANK IN AUSGABE
              WRITE AUSGABE FROM AUSGABE
              MOVE AKTUELLE-BANKID TO BANK-ID IN AUSGABE-BANK 
                 IN AKTUELLE-AUSGABE
              MOVE 0.00 TO SUMME IN AUSGABE-BANK IN AKTUELLE-AUSGABE
           END-IF      
       EXIT.