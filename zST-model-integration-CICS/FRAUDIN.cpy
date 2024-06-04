        IDENTIFICATION DIVISION.                                     
        PROGRAM-ID. FRAUDIN.                                         
        DATA DIVISION.                                               
        WORKING-STORAGE SECTION.                                     
        01  MODELIN.
           06 Card COMP-2 SYNC.
           06 Month COMP-2 SYNC.
           06 UseXChip-length PIC S9999 COMP-5 SYNC.
           06 UseXChip PIC X(255).
           06 Year COMP-2 SYNC.
           06 XDay COMP-2 SYNC.
           06 Amount COMP-2 SYNC.
           06 MerchantXName COMP-2 SYNC.
           06 MerchantXState-length PIC S9999 COMP-5 SYNC.
           06 MerchantXState PIC X(255).
           06 MerchantXCity-length PIC S9999 COMP-5 SYNC.
           06 MerchantXCity PIC X(255).
           06 User COMP-2 SYNC.
           06 Zip COMP-2 SYNC.
           06 MCC COMP-2 SYNC.
           06 Errors-length PIC S9999 COMP-5 SYNC.
           06 Errors PIC X(255).
           06 XTime COMP-2 SYNC.        
        PROCEDURE DIVISION.  
                STOP RUN. 
