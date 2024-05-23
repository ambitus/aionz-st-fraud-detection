        IDENTIFICATION DIVISION.                         
        PROGRAM-ID. FRAUDOUT.                            
        DATA DIVISION.                                   
        WORKING-STORAGE SECTION.                         
        01  FRAUDOUT.
          03 SCORE-RC                   PIC 9(4) COMP VALUE 0.
          03 SCORE-ERR-ID                  PIC X(8).
          03 SCORE-ERR-MSG                 PIC X(255).
          03 SCORE-ERR-MSG-LEN             PIC S9999 COMP-5 SYNC.
          03 MODELOUT.
             06 probabilityXNoX               COMP-2 SYNC.
             06 probabilityXYesX              COMP-2 SYNC.                      
        PROCEDURE DIVISION.                              
                   STOP RUN.                             
