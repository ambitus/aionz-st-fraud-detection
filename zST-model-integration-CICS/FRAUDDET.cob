       IDENTIFICATION DIVISION.
       PROGRAM-ID. FRAUDDET.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * AUTHOR - EVAN RIVERA *************************************
      *
      * AI ON IBM Z SOLUTION TEMPLATE
      *
      * SAMPLE PROGRAM TO SCORE FRAUD WITHIN CREDIT CARD
      * TRANSACTIONS
      *
      * OUTPUT WILL BE IN FORM OF PROBABILITY(NO/YES)
      ************************************************************

      *Raw Input Data****
      *DATA STRUCTURE OF THE MODEL INPUT
       01  FRAUDIN.
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

      *Scoring Output Variables
      *DATA STRUCTURE OF THE MODEL OUTPUT
      *Probability(No) - scoring for non fraud transaction
      *Probability(Yes) - scoring for fraud transaction
       01  FRAUDOUT.
         03 SCORE-RC                   PIC 9(4) COMP VALUE 0.
         03 SCORE-ERR-ID                  PIC X(8).
         03 SCORE-ERR-MSG                 PIC X(255).
         03 SCORE-ERR-MSG-LEN             PIC S9999 COMP-5 SYNC.
         03 MODELOUT.
             06 probabilityXNoX               COMP-2 SYNC.
             06 probabilityXYesX              COMP-2 SYNC.

      *Typecasting Variables to User friendly Output
      *
       01  CAST-WS.
            06 cast-card                     PIC 9(2).
            06 cast-month                    PIC 9(2).
            06 cast-year                     PIC 9(4).
            06 cast-day                      PIC 9(2).
            06 cast-amount                   PIC 9(5).99.
            06 cast-merchant-name            PIC 9(18).
            06 cast-merchant-city            PIC 9(10).
            06 cast-merchant-state           PIC 9(2).
            06 cast-user                     PIC 9(2).
            06 cast-time                     PIC 9(2).
            06 cast-zip                      PIC 9(5).9.
            06 cast-mcc                      PIC 9(4).
            06 cast-errors                   PIC 9(10).
            06 cast-probabilityXNoX          PIC 9(1).9(16).
            06 cast-probabilityXYesX         PIC 9(1).9(16).

       PROCEDURE DIVISION.

             PERFORM 0500-GET-INPUT
                THRU 0500-GET-INPUT-X.

      *Compute length for string fields

             COMPUTE UseXChip-length =
             FUNCTION LENGTH(UseXChip)

             COMPUTE MerchantXState-length =
             FUNCTION LENGTH(MerchantXState)

             COMPUTE MerchantXCity-length  =
             FUNCTION LENGTH(MerchantXCity)

             COMPUTE Errors-length  =
             FUNCTION LENGTH(Errors)

             PERFORM 1000-CALL-CICS
                THRU 1000-CALL-CICS-X.

             PERFORM 2000-CAST-NUMERIC
                THRU 2000-CAST-NUMERIC-X.

             PERFORM 3000-PUT-OUTPUT
                THRU 3000-PUT-OUTPUT-X.

             EXEC CICS RETURN END-EXEC.
             STOP RUN.

      *---------------
       0500-GET-INPUT.
      *---------------

             MOVE 0                  TO User.
             MOVE 10                 TO Month.
             MOVE 3.72               TO Amount.
             MOVE 13                 TO XDay.
             MOVE 197144152588600136 TO MerchantXName.
      *  Merchant Name is 19 digits in original dataset. Take first 18     
             MOVE 'CA'               TO MerchantXState.
      *      MOVE 2                  TO MerchantXState-length
             MOVE 'Upland'           TO MerchantXCity.
      *      MOVE 6                  TO MerchantXCity-length
             MOVE 2008               TO Year.
             MOVE 91784.0            TO Zip.
             MOVE 3                  TO Card.
             MOVE 'Swipe Transaction' TO UseXChip.
      *      MOVE 17                 TO UseXChip-length.
             MOVE 5300               TO MCC.
             MOVE 'na'               TO Errors.
      *      MOVE 2                  TO Errors-length.
             MOVE 702                TO XTime.

      *-----------------
       0500-GET-INPUT-X.
      *-----------------

      *---------------
       1000-CALL-CICS.
      *---------------

      *     WMLz Model deployment ID - PMML
      *PASS THE DEPLOYMENT ID OF THE MODEL TO SCORING VIA CICS
      *CHANNEL AND CONTAINER ALN_DEPLOYMENT_ID
             EXEC CICS PUT CONTAINER('ALN_DEPLOY_ID') CHANNEL('CHAN')
                  CHAR
                  FROM('DEPLOYMENT_ID')
                  END-EXEC.

      *PASS THE JAVA CLASS NAME OF THE MODEL INPUT TO SCORING VIA
      *CICS CHANNEL AND CONTAINER ALN_INPUT_CLASS
      *     PMML Input Wrapper Class Generated through ALNJCGEN
             EXEC CICS PUT CONTAINER('ALN_INPUT_CLASS') CHANNEL('CHAN')
                  CHAR FROM('FRAUDInPipeWrapper')
                  END-EXEC.

             EXEC CICS PUT CONTAINER('ALN_INPUT_DATA') CHANNEL('CHAN')
                  FROM(FRAUDIN) BIT END-EXEC.

      *PASS THE JAVA CLASS NAME OF THE MODEL OUTPUT TO SCORING VIA
      *CICS CHANNEL AND CONTAINER ALN_OUTPUT_CLASS
      *     PMML Output Wrapper Class Generated through ALNJCGEN
             EXEC CICS PUT CONTAINER('ALN_OUTPUT_CLASS') CHANNEL('CHAN')
                  CHAR FROM('FRAUDOutPipeWrapper')
                  END-EXEC.

      *USE CICS LINK TO CALL THE SCORING PROGRAM ALSCORE TO PERFOR
      *PREDICTION AGAINST THE SPECIFIC INPUT RECORD
      *     Invoke ALNSCORE to do Scoring
             EXEC CICS LINK PROGRAM('ALNSCORE') CHANNEL('CHAN')
                  END-EXEC.

      *GET THE SCORING RESULT BACK VIA CICS CHANNEL AND CONTAINER
      *ALN_OUTPUT_DATA
             EXEC CICS GET CONTAINER('ALN_OUTPUT_DATA') CHANNEL('CHAN')
                  INTO(FRAUDOUT) END-EXEC.

      *-----------------
       1000-CALL-CICS-X.
      *-----------------

      *------------------
       2000-CAST-NUMERIC.
      *------------------

           INITIALIZE CAST-WS.

           MOVE Card                       TO cast-card.
           MOVE Month                      TO cast-month.
           MOVE Year                       TO cast-year.
           MOVE XDay                       TO cast-day.
           MOVE Amount                     TO cast-amount.
           MOVE MerchantXName              TO cast-merchant-name.
           MOVE User                       TO cast-user.
           MOVE Zip                        TO cast-zip.
           MOVE MCC                        TO cast-mcc.
           MOVE XTime                      TO cast-time.
           MOVE probabilityXNoX            TO cast-probabilityXNoX.
           MOVE probabilityXYesX           TO cast-probabilityXYesX.

      *--------------------
       2000-CAST-NUMERIC-X.
      *--------------------

      *----------------
       3000-PUT-OUTPUT.
      *----------------

      *****INPUT FIELDS**********************************
             DISPLAY 'Amount:        :' cast-amount.
             DISPLAY 'Card           :' cast-card.
             DISPLAY 'Day            :' cast-day.
             DISPLAY 'Merchant Name  :' cast-merchant-name.
             DISPLAY 'Merchant City  :' MerchantXCity.
             DISPLAY 'Merchant State :' MerchantXState.
             DISPLAY 'Month          :' cast-month.
             DISPLAY 'Time           :' cast-time.
             DISPLAY 'Use Chip       :' UseXChip.
             DISPLAY 'User           :' cast-user.
             DISPLAY 'Year           :' cast-year.
             DISPLAY 'Zip            :' cast-zip.
             DISPLAY 'MCC            :' cast-mcc.
             DISPLAY 'Errors         :' Errors.
             DISPLAY '                '.

      *****OUTPUT FIELDS*********************************
             DISPLAY 'probability(No):' cast-probabilityXNoX.
             DISPLAY 'probability(Yes):' cast-probabilityXYesX.
             DISPLAY '                '.

      *****RESULT****************************************
             IF  probabilityXNoX > probabilityXYesX
                DISPLAY 'NO FRAUD'
                DISPLAY '                '
             ELSE
                DISPLAY 'FRAUD'
                DISPLAY '                '
             END-IF.
      *------------------
       3000-PUT-OUTPUT-X.
      *------------------
