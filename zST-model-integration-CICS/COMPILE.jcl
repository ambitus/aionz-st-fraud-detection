//*********************************************************************/
//* JOB NAME = COMPILE                                                */
//*                                                                   */
//*    Licensed Materials - Property of IBM                           */
//*    (C) COPYRIGHT 2017 IBM Corp.  All Rights Reserved.             */
//*                                                                   */
//*    STATUS = Version 1                                             */
//*                                                                   */
//* FUNCTION = SAMPLE JCL TO COMPILE A CICS COBOL PROGRAM.            */
//*                                                                   */
//*********************************************************************/
//*  NOTE: BEFORE RUNNING THIS JOB, MAKE THE FOLLOWING CHANGES:       */
//*        (1) ADD A VALID JOB CARD                                   */
//*        (2) LOCATE AND SET VALUES TO THE FOLLOWING PARAMETERS:     */
//*            (A) <!IGYHLQ!>                                         */
//*                  TO THE QUALIFIER OF THE COBOL COMPILER           */
//*            (B) <!SRCLIB!>                                         */
//*                  TO THE PDS LIBRARY WHERE THE SOURCE COBOL PGM    */
//*                  IS PLACED                                        */
//*            (C) <!MEMNAM!>                                         */
//*                  TO THE MEMBER NAME OF THE COBOL PROGRAM          */
//*            (D) <!ADATAL!>                                         */
//*                  TO THE PDS LIBRARY WHERE THE ADATA IS PLACED     */
//*            (E) <!LOADLIB!>                                        */
//*                  TO THE PDS LIBRARY WHERE THE LOAD MODULE PLACED  */
//*            (F) <!SYSLIB!>                                         */
//*                  TO THE PDS LIBRARY OF SYSTEM DATASETS.           */
//**********************************************************************
//*                                                                     
//COMP1  EXEC PGM=IGYCRCTL,REGION=300M,
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
//LKED   EXEC PGM=IEWL,REGION=4M,                 
//            PARM='LIST,XREF',COND=(4,LT)        
//SYSLIB   DD DISP=SHR,DSN=<!SYSLIB>         
//         DD DISP=SHR,DSN=<!IGYHLQ!>.SDFHLOAD 
//SYSLMOD  DD DISP=SHR,DSN=<!LOADLIB!>(<!MEMNAM!>)
//SYSUT1   DD UNIT=SYSDA,DCB=BLKSIZE=1024,        
//            SPACE=(1024,(200,20))               
//SYSPRINT DD SYSOUT=*                            
//SYSLIN   DD DSN=&&LOADSET,DISP=(OLD,DELETE)     
//         DD DDNAME=SYSIN                        
//SYSIN    DD *                                                              
/*                                                
//*                                               
