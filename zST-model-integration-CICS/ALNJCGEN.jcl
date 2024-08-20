//*********************************************************************/
//* JOB NAME = ALNJCGEN                                               */
//*                                                                   */
//*    STATUS = Version 1                                             */
//*                                                                   */
//* FUNCTION = SAMPLE JCL TO GENERATE JAVA HELPER CLASS FOR           */
//*            MODEL INPUT AND OUTPUT WHEN USING CICS-INTEGRATED      */
//*            SCORING SERVER OF MACHINE LEARNING FOR z/OS            */
//*                                                                   */
//*********************************************************************/
//*  NOTE: BEFORE RUNNING THIS JOB, MAKE THE FOLLOWING CHANGES:       */
//*        (1) ADD A VALID JOB CARD                                   */
//*        (3) LOCATE AND SET VALUES TO THE FOLLOWING PARAMETERS:     */
//*            (A) <!IGYHLQ!>                                         */
//*                  TO THE QUALIFIER OF THE COBOL COMPILER           */
//*            (B) <!SRCLIB!>                                         */
//*                  TO THE PDS LIBRARY WHERE THE SOURCE COBOL PGM    */
//*                  IS PLACED                                        */
//*            (C) <!MEMNAM!>                                         */
//*                  TO THE MEMBER NAME OF THE COBOL PROGRAM          */
//*            (D) <!ADATAL!>                                         */
//*                  TO THE PDS LIBRARY WHERE THE ADATA IS PLACED     */
//*            (E) <!java_home_dir!>                                  */
//*                  TO THE JAVA HOME DIRECTORY ON USS                */
//*            (F) <!iml_home_dir_zos!>                               */
//*                  TO THE USS IML_HOME DIRECTORY WHERE MLZ IS       */
//*                  CONFIGURED                                       */
//*            (F) <!install_dir_zos!>                                */
//*                  TO THE USS DIRECTORY WHERE MLZ IS INSTALLED      */
//*            (G) <!region_name!>                                    */
//*                  TO THE CICS REGION NAME WHERE THE SCORING SERVER */
//*                  IS DEPLOYED                                      */
//*            (H) <!java_class_name!>                                */
//*                  TO THE NAME OF THE JAVA CLASS TO BE GENERATED    */
//*  CHANGE ACTIVITY =                                                */
//*    10/26/2017 CREATED                                             */
//*********************************************************************/
//ADATA  EXEC PGM=IGYCRCTL,REGION=300M,
//       PARM=('LIST,MAP,XREF(SHORT),NONUMBER,SOURCE,APOST,ADATA')
//STEPLIB  DD DSN=<!IGYHLQ!>.SIGYCOMP,DISP=SHR
//SYSPRINT DD  SYSOUT=*
//SYSIN    DD  DSN=<!SRCLIB!>(<!MEMNAM!>),DISP=SHR
//SYSLIN   DD  DSN=&&LOADSET,DISP=(MOD,PASS),
//             UNIT=SYSDA,SPACE=(400,(250,100))
//SYSUT1   DD  UNIT=SYSDA,SPACE=(460,(350,100))
//SYSUT2   DD  UNIT=SYSDA,SPACE=(460,(350,100))
//SYSUT3   DD  UNIT=SYSDA,SPACE=(460,(350,100))
//SYSUT4   DD  UNIT=SYSDA,SPACE=(460,(350,100))
//SYSUT5   DD  UNIT=SYSDA,SPACE=(460,(350,100))
//SYSUT6   DD  UNIT=SYSDA,SPACE=(460,(350,100))
//SYSUT7   DD  UNIT=SYSDA,SPACE=(460,(350,100))
//SYSUT8   DD  UNIT=SYSDA,SPACE=(460,(350,100))
//SYSUT9   DD  UNIT=SYSDA,SPACE=(460,(350,100))
//SYSUT10  DD  UNIT=SYSDA,SPACE=(460,(350,100))
//SYSUT11  DD  UNIT=SYSDA,SPACE=(460,(350,100))
//SYSUT12  DD  UNIT=SYSDA,SPACE=(460,(350,100))
//SYSUT13  DD  UNIT=SYSDA,SPACE=(460,(350,100))
//SYSUT14  DD  UNIT=SYSDA,SPACE=(460,(350,100))
//SYSUT15  DD  UNIT=SYSDA,SPACE=(460,(350,100))
//SYSMDECK DD  UNIT=SYSDA,SPACE=(460,(350,100))
//SYSADATA DD DSN=<!ADATAL!>(<!MEMNAM!>),DISP=SHR
//*
//RECGEN EXEC PGM=BPXBATCH,REGION=300M
//STDENV   DD *
JAVAHOME=<!java_home_dir!>
DIR=<!iml_home_dir_zos!>/cics-scoring
REGNAME=<!region_name!>
JCLASS=<!java_class_name!>
ADATAL=<!ADATAL!>
ADATAM=<!MEMNAM!>
//STDOUT   DD   SYSOUT=*
//STDERR   DD   SYSOUT=*
//STDPARM DD *
sh cd <!install_dir_zos!>/cics-scoring/bin;
sh ALNJCGEN $JAVAHOME $DIR $REGNAME $JCLASS $ADATAL $ADATAM;
/*
